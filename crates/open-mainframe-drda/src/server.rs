//! TCP server for DRDA connections.
//!
//! Listens on a TCP port and spawns a task per client connection.
//! Each connection gets its own `RequestHandler` with connection state.

use std::net::SocketAddr;
use std::sync::Arc;

use tokio::io::BufReader;
use tokio::net::TcpListener;

use crate::connection::{ConnectionHandler, DrdaConfig};
use crate::dss::{read_dss, write_dss_chain};
use crate::error::{DrdaError, DrdaResult};
use crate::handler::RequestHandler;

/// Configuration for the DRDA server.
#[derive(Debug, Clone)]
pub struct DrdaServerConfig {
    /// Whether the DRDA server is enabled.
    pub enabled: bool,
    /// Bind host address.
    pub host: String,
    /// Bind port.
    pub port: u16,
    /// Database name to accept.
    pub database: String,
    /// DDF location name.
    pub location: String,
}

impl Default for DrdaServerConfig {
    fn default() -> Self {
        Self {
            enabled: true,
            host: "0.0.0.0".to_string(),
            port: 50000,
            database: "DSN1".to_string(),
            location: "OPENMF".to_string(),
        }
    }
}

/// Authentication function type: validates userid and password.
pub type AuthFn = Arc<dyn Fn(&str, &str) -> bool + Send + Sync>;

/// Start the DRDA server.
///
/// Listens on the configured address and spawns a task per connection.
/// `auth_fn` is called for each SECCHK to validate credentials.
pub async fn start_server(config: DrdaServerConfig, auth_fn: AuthFn) -> DrdaResult<()> {
    let bind_addr = format!("{}:{}", config.host, config.port);
    let listener = TcpListener::bind(&bind_addr).await?;

    tracing::info!(
        addr = %bind_addr,
        database = %config.database,
        "DRDA server listening"
    );

    let config = Arc::new(config);

    loop {
        match listener.accept().await {
            Ok((stream, peer_addr)) => {
                tracing::info!(peer = %peer_addr, "DRDA connection accepted");
                let config = Arc::clone(&config);
                let auth = Arc::clone(&auth_fn);
                tokio::spawn(async move {
                    if let Err(e) = handle_connection(stream, peer_addr, &config, auth).await {
                        match &e {
                            DrdaError::ConnectionClosed => {
                                tracing::info!(peer = %peer_addr, "DRDA connection closed");
                            }
                            DrdaError::AuthFailed(user) => {
                                tracing::warn!(peer = %peer_addr, user = %user, "DRDA authentication failed");
                            }
                            _ => {
                                tracing::error!(peer = %peer_addr, error = %e, "DRDA connection error");
                            }
                        }
                    }
                });
            }
            Err(e) => {
                tracing::error!(error = %e, "Failed to accept DRDA connection");
            }
        }
    }
}

/// Format bytes as hex for debug logging.
fn hex_preview(data: &[u8], max_len: usize) -> String {
    let len = data.len().min(max_len);
    let hex: Vec<String> = data[..len].iter().map(|b| format!("{:02x}", b)).collect();
    let result = hex.join(" ");
    if data.len() > max_len {
        format!("{}...(+{} bytes)", result, data.len() - max_len)
    } else {
        result
    }
}

/// Handle a single DRDA client connection.
async fn handle_connection(
    stream: tokio::net::TcpStream,
    _peer_addr: SocketAddr,
    config: &DrdaServerConfig,
    auth_fn: AuthFn,
) -> DrdaResult<()> {
    let (reader, mut writer) = stream.into_split();
    let mut reader = BufReader::new(reader);

    // Create per-connection handler
    let drda_config = DrdaConfig {
        server_name: config.location.clone(),
        product_id: "SQL11050".to_string(),
        database: config.database.clone(),
        location: config.location.clone(),
    };

    let conn_handler = ConnectionHandler::new(drda_config);
    let auth = Arc::clone(&auth_fn);
    let mut handler = RequestHandler::new(conn_handler, move |user, pass| {
        (auth)(user, pass)
    });

    // Main read loop
    loop {
        // Read one or more chained DSS segments
        let mut segments = Vec::new();
        let first_dss = read_dss(&mut reader).await?;
        tracing::debug!(
            dss_type = first_dss.dss_type,
            chained = first_dss.chained,
            corr_id = first_dss.correlation_id,
            payload_len = first_dss.payload.len(),
            payload_hex = %hex_preview(&first_dss.payload, 64),
            "DRDA recv DSS"
        );
        let mut chained = first_dss.chained;
        segments.push(first_dss);

        // Read any chained segments
        while chained {
            let next_dss = read_dss(&mut reader).await?;
            tracing::debug!(
                dss_type = next_dss.dss_type,
                chained = next_dss.chained,
                corr_id = next_dss.correlation_id,
                payload_len = next_dss.payload.len(),
                "DRDA recv chained DSS"
            );
            chained = next_dss.chained;
            segments.push(next_dss);
        }

        // Process the batch
        let mut responses = match handler.process_dss_batch(&segments) {
            Ok(r) => r,
            Err(e) => {
                tracing::error!(error = %e, "DRDA handler error");
                return Err(e);
            }
        };

        // Send all response segments
        if !responses.is_empty() {
            // Set chaining and same-correlator flags to match Derby behavior.
            //
            // Derby chains ALL response segments in a batch. The chain bit
            // (0x40) indicates more segments follow. The same-correlator bit
            // (0x10) indicates the NEXT chained segment has the same
            // correlation ID as this one.
            //
            // Examples from Derby captures:
            //   EXCSATRD(corr=1) fmt=0x42 → ACCSECRD(corr=2) fmt=0x02
            //   SECCHKRM(corr=1) fmt=0x42 → ACCRDBRM(corr=2) fmt=0x52 → SQLCARD(corr=2) fmt=0x02
            let len = responses.len();
            for i in 0..len {
                if i + 1 < len {
                    responses[i].chained = true;
                    responses[i].same_correlator =
                        responses[i].correlation_id == responses[i + 1].correlation_id;
                } else {
                    responses[i].chained = false;
                    responses[i].same_correlator = false;
                }
            }

            for resp in &responses {
                let fmt = {
                    let mut f = resp.dss_type & 0x0F;
                    if resp.chained { f |= 0x40; }
                    if resp.same_correlator { f |= 0x10; }
                    if resp.continuation { f |= 0x20; }
                    f
                };
                tracing::debug!(
                    dss_type = resp.dss_type,
                    chained = resp.chained,
                    same_correlator = resp.same_correlator,
                    format_byte = format!("0x{:02X}", fmt),
                    corr_id = resp.correlation_id,
                    payload_len = resp.payload.len(),
                    payload_hex = %hex_preview(&resp.payload, 64),
                    "DRDA send DSS"
                );
            }
            write_dss_chain(&mut writer, &responses).await?;
        }
    }
}
