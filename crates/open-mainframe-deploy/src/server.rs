//! HTTP server for health, readiness, and Prometheus metrics endpoints.
//!
//! Provides two servers:
//! - Application server on `config.server.port` serving `/health` and `/ready`
//! - Metrics server on `config.server.metrics_port` serving `/metrics`
//!
//! Both servers support graceful shutdown via a tokio `CancellationToken`.

use std::sync::Arc;
use tokio::io::{AsyncReadExt, AsyncWriteExt};
use tokio::net::TcpListener;
use tokio::sync::watch;

use crate::config::ServerConfig;
use crate::health::HealthChecker;
use crate::metrics::MetricsRegistry;

/// Handle to running servers, used for shutdown coordination.
#[derive(Debug)]
pub struct ServerHandle {
    /// Sender to signal shutdown.
    shutdown_tx: watch::Sender<bool>,
    /// Join handle for the application server task.
    app_handle: tokio::task::JoinHandle<()>,
    /// Join handle for the metrics server task.
    metrics_handle: tokio::task::JoinHandle<()>,
}

impl ServerHandle {
    /// Signal both servers to shut down gracefully.
    pub fn shutdown(&self) {
        let _ = self.shutdown_tx.send(true);
    }

    /// Wait for both servers to finish.
    pub async fn wait(self) {
        let _ = self.app_handle.await;
        let _ = self.metrics_handle.await;
    }
}

/// Start both HTTP servers (application + metrics).
///
/// Returns a [`ServerHandle`] that can be used to trigger graceful shutdown.
pub async fn start_servers(
    config: &ServerConfig,
    health: HealthChecker,
    registry: Arc<MetricsRegistry>,
) -> Result<ServerHandle, std::io::Error> {
    let (shutdown_tx, shutdown_rx) = watch::channel(false);

    let app_addr = format!("{}:{}", config.host, config.port);
    let metrics_addr = format!("{}:{}", config.host, config.metrics_port);

    let app_listener = TcpListener::bind(&app_addr).await?;
    let metrics_listener = TcpListener::bind(&metrics_addr).await?;

    let health_clone = health.clone();
    let mut app_rx = shutdown_rx.clone();
    let app_handle = tokio::spawn(async move {
        loop {
            tokio::select! {
                Ok((stream, _)) = app_listener.accept() => {
                    let h = health_clone.clone();
                    tokio::spawn(async move {
                        handle_app_request(stream, &h).await;
                    });
                }
                _ = app_rx.changed() => {
                    break;
                }
            }
        }
    });

    let mut metrics_rx = shutdown_rx;
    let metrics_handle = tokio::spawn(async move {
        loop {
            tokio::select! {
                Ok((stream, _)) = metrics_listener.accept() => {
                    let r = Arc::clone(&registry);
                    tokio::spawn(async move {
                        handle_metrics_request(stream, &r).await;
                    });
                }
                _ = metrics_rx.changed() => {
                    break;
                }
            }
        }
    });

    Ok(ServerHandle {
        shutdown_tx,
        app_handle,
        metrics_handle,
    })
}

/// Handle an application server request (/health, /ready).
async fn handle_app_request(mut stream: tokio::net::TcpStream, health: &HealthChecker) {
    let mut buf = [0u8; 4096];
    let n = match stream.read(&mut buf).await {
        Ok(n) if n > 0 => n,
        _ => return,
    };

    let request = String::from_utf8_lossy(&buf[..n]);
    let path = extract_path(&request);

    let (status, body) = match path {
        "/health" => {
            let status = health.check_liveness();
            let json = serde_json::to_string(&status).unwrap_or_default();
            ("200 OK", json)
        }
        "/ready" => {
            let status = health.check_readiness();
            let json = serde_json::to_string(&status).unwrap_or_default();
            if status.ready {
                ("200 OK", json)
            } else {
                ("503 Service Unavailable", json)
            }
        }
        _ => ("404 Not Found", r#"{"error":"not found"}"#.to_string()),
    };

    let response = format!(
        "HTTP/1.1 {}\r\nContent-Type: application/json\r\nContent-Length: {}\r\nConnection: close\r\n\r\n{}",
        status,
        body.len(),
        body,
    );

    let _ = stream.write_all(response.as_bytes()).await;
}

/// Handle a metrics server request (/metrics).
async fn handle_metrics_request(mut stream: tokio::net::TcpStream, registry: &MetricsRegistry) {
    let mut buf = [0u8; 4096];
    let n = match stream.read(&mut buf).await {
        Ok(n) if n > 0 => n,
        _ => return,
    };

    let request = String::from_utf8_lossy(&buf[..n]);
    let path = extract_path(&request);

    let (status, content_type, body) = match path {
        "/metrics" => {
            let metrics = registry.gather();
            (
                "200 OK",
                "text/plain; version=0.0.4; charset=utf-8",
                metrics,
            )
        }
        _ => (
            "404 Not Found",
            "text/plain",
            "Not Found\n".to_string(),
        ),
    };

    let response = format!(
        "HTTP/1.1 {}\r\nContent-Type: {}\r\nContent-Length: {}\r\nConnection: close\r\n\r\n{}",
        status,
        content_type,
        body.len(),
        body,
    );

    let _ = stream.write_all(response.as_bytes()).await;
}

/// Extract the request path from an HTTP request line.
fn extract_path(request: &str) -> &str {
    // GET /path HTTP/1.1
    let mut parts = request.split_whitespace();
    parts.next(); // method
    parts.next().unwrap_or("/")
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::time::Duration;

    #[test]
    fn test_extract_path() {
        assert_eq!(extract_path("GET /health HTTP/1.1\r\n"), "/health");
        assert_eq!(extract_path("GET /ready HTTP/1.1\r\n"), "/ready");
        assert_eq!(extract_path("GET /metrics HTTP/1.1\r\n"), "/metrics");
        assert_eq!(extract_path("POST /api/test HTTP/1.1\r\n"), "/api/test");
    }

    #[tokio::test]
    async fn test_server_health_endpoint() {
        // Bind to OS-assigned ports
        let app_listener = TcpListener::bind("127.0.0.1:0").await.unwrap();
        let app_port = app_listener.local_addr().unwrap().port();

        let health = HealthChecker::new();
        let health_clone = health.clone();

        let (shutdown_tx, mut shutdown_rx) = watch::channel(false);

        let handle = tokio::spawn(async move {
            tokio::select! {
                Ok((stream, _)) = app_listener.accept() => {
                    handle_app_request(stream, &health_clone).await;
                }
                _ = shutdown_rx.changed() => {}
            }
        });

        // Wait for server to be ready
        tokio::time::sleep(Duration::from_millis(50)).await;

        // Make a request
        let mut stream =
            tokio::net::TcpStream::connect(format!("127.0.0.1:{}", app_port))
                .await
                .unwrap();
        stream
            .write_all(b"GET /health HTTP/1.1\r\nHost: localhost\r\n\r\n")
            .await
            .unwrap();

        let mut response = Vec::new();
        stream.read_to_end(&mut response).await.unwrap();
        let response_str = String::from_utf8_lossy(&response);

        assert!(response_str.contains("200 OK"));
        assert!(response_str.contains("healthy"));

        let _ = shutdown_tx.send(true);
        let _ = handle.await;
    }

    #[tokio::test]
    async fn test_server_readiness_not_ready() {
        let app_listener = TcpListener::bind("127.0.0.1:0").await.unwrap();
        let app_port = app_listener.local_addr().unwrap().port();

        let health = HealthChecker::new();
        // database is not ready by default
        let health_clone = health.clone();

        let (shutdown_tx, mut shutdown_rx) = watch::channel(false);

        let handle = tokio::spawn(async move {
            tokio::select! {
                Ok((stream, _)) = app_listener.accept() => {
                    handle_app_request(stream, &health_clone).await;
                }
                _ = shutdown_rx.changed() => {}
            }
        });

        tokio::time::sleep(Duration::from_millis(50)).await;

        let mut stream =
            tokio::net::TcpStream::connect(format!("127.0.0.1:{}", app_port))
                .await
                .unwrap();
        stream
            .write_all(b"GET /ready HTTP/1.1\r\nHost: localhost\r\n\r\n")
            .await
            .unwrap();

        let mut response = Vec::new();
        stream.read_to_end(&mut response).await.unwrap();
        let response_str = String::from_utf8_lossy(&response);

        assert!(response_str.contains("503 Service Unavailable"));
        assert!(response_str.contains("\"ready\":false"));

        let _ = shutdown_tx.send(true);
        let _ = handle.await;
    }

    #[tokio::test]
    async fn test_server_metrics_endpoint() {
        let metrics_listener = TcpListener::bind("127.0.0.1:0").await.unwrap();
        let metrics_port = metrics_listener.local_addr().unwrap().port();

        let registry = Arc::new(MetricsRegistry::new("test").unwrap());
        // Record a metric
        registry.cobol.record_execution("TESTPROG", true, 50.0);
        let registry_clone = Arc::clone(&registry);

        let (shutdown_tx, mut shutdown_rx) = watch::channel(false);

        let handle = tokio::spawn(async move {
            tokio::select! {
                Ok((stream, _)) = metrics_listener.accept() => {
                    handle_metrics_request(stream, &registry_clone).await;
                }
                _ = shutdown_rx.changed() => {}
            }
        });

        tokio::time::sleep(Duration::from_millis(50)).await;

        let mut stream =
            tokio::net::TcpStream::connect(format!("127.0.0.1:{}", metrics_port))
                .await
                .unwrap();
        stream
            .write_all(b"GET /metrics HTTP/1.1\r\nHost: localhost\r\n\r\n")
            .await
            .unwrap();

        let mut response = Vec::new();
        stream.read_to_end(&mut response).await.unwrap();
        let response_str = String::from_utf8_lossy(&response);

        assert!(response_str.contains("200 OK"));
        assert!(response_str.contains("text/plain"));
        assert!(response_str.contains("cobol_programs_executed"));

        let _ = shutdown_tx.send(true);
        let _ = handle.await;
    }

    #[tokio::test]
    async fn test_server_graceful_shutdown() {
        // Bind to OS-assigned ports, then release so start_servers can rebind
        let app_listener = TcpListener::bind("127.0.0.1:0").await.unwrap();
        let metrics_listener = TcpListener::bind("127.0.0.1:0").await.unwrap();
        let app_port = app_listener.local_addr().unwrap().port();
        let metrics_port = metrics_listener.local_addr().unwrap().port();
        drop(app_listener);
        drop(metrics_listener);

        let config = ServerConfig {
            host: "127.0.0.1".to_string(),
            port: app_port,
            metrics_port,
        };

        let health = HealthChecker::new();
        let registry = Arc::new(MetricsRegistry::new("test_shutdown").unwrap());

        let handle = start_servers(&config, health, registry).await.unwrap();

        // Give servers time to start
        tokio::time::sleep(Duration::from_millis(50)).await;

        // Shutdown
        handle.shutdown();
        // Wait with a timeout
        tokio::time::timeout(Duration::from_secs(2), handle.wait())
            .await
            .expect("servers should shut down within 2 seconds");
    }
}
