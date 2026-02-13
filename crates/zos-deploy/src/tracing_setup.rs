//! OpenTelemetry tracing and structured logging setup.

use std::env;
use thiserror::Error;
use tracing_subscriber::{
    fmt::{self, format::FmtSpan},
    layer::SubscriberExt,
    util::SubscriberInitExt,
    EnvFilter,
};

/// Tracing configuration.
#[derive(Debug, Clone)]
pub struct TracingConfig {
    /// Log level filter
    pub log_level: String,
    /// Log format
    pub log_format: LogFormat,
    /// OpenTelemetry endpoint (optional)
    pub otel_endpoint: Option<String>,
    /// Service name for tracing
    pub service_name: String,
    /// Tracing sample rate
    pub sample_rate: f64,
}

impl Default for TracingConfig {
    fn default() -> Self {
        Self {
            log_level: "info".to_string(),
            log_format: LogFormat::Json,
            otel_endpoint: None,
            service_name: "zos-clone".to_string(),
            sample_rate: 0.1,
        }
    }
}

impl TracingConfig {
    /// Create from environment variables.
    pub fn from_env() -> Self {
        Self {
            log_level: env::var("ZOS_LOG_LEVEL").unwrap_or_else(|_| "info".to_string()),
            log_format: env::var("ZOS_LOG_FORMAT")
                .map(|f| LogFormat::from_str(&f))
                .unwrap_or(LogFormat::Json),
            otel_endpoint: env::var("OTEL_EXPORTER_OTLP_ENDPOINT").ok(),
            service_name: env::var("OTEL_SERVICE_NAME")
                .unwrap_or_else(|_| "zos-clone".to_string()),
            sample_rate: env::var("OTEL_TRACES_SAMPLER_ARG")
                .ok()
                .and_then(|s| s.parse().ok())
                .unwrap_or(0.1),
        }
    }
}

/// Log format options.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LogFormat {
    /// JSON format for log aggregation
    Json,
    /// Human-readable text format
    Text,
    /// Compact format for development
    Compact,
}

impl LogFormat {
    /// Parse from string.
    pub fn from_str(s: &str) -> Self {
        match s.to_lowercase().as_str() {
            "json" => LogFormat::Json,
            "text" | "pretty" => LogFormat::Text,
            "compact" => LogFormat::Compact,
            _ => LogFormat::Json,
        }
    }
}

/// Tracing initialization errors.
#[derive(Debug, Error)]
pub enum TracingError {
    #[error("Failed to initialize tracing: {0}")]
    InitError(String),

    #[error("Failed to initialize OpenTelemetry: {0}")]
    OtelError(String),

    #[error("Invalid configuration: {0}")]
    ConfigError(String),
}

/// Initialize tracing with default configuration from environment.
pub fn init_tracing() -> Result<(), TracingError> {
    let config = TracingConfig::from_env();
    init_tracing_with_config(&config)
}

/// Initialize tracing with custom configuration.
pub fn init_tracing_with_config(config: &TracingConfig) -> Result<(), TracingError> {
    // Build the env filter
    let env_filter = EnvFilter::try_from_default_env()
        .unwrap_or_else(|_| EnvFilter::new(&config.log_level));

    // Build the subscriber based on format
    match config.log_format {
        LogFormat::Json => {
            let json_layer = fmt::layer()
                .json()
                .with_target(true)
                .with_thread_ids(true)
                .with_thread_names(true)
                .with_file(true)
                .with_line_number(true)
                .with_span_events(FmtSpan::CLOSE);

            tracing_subscriber::registry()
                .with(env_filter)
                .with(json_layer)
                .try_init()
                .map_err(|e| TracingError::InitError(e.to_string()))?;
        }
        LogFormat::Text => {
            let text_layer = fmt::layer()
                .with_target(true)
                .with_thread_ids(false)
                .with_file(true)
                .with_line_number(true)
                .with_span_events(FmtSpan::CLOSE);

            tracing_subscriber::registry()
                .with(env_filter)
                .with(text_layer)
                .try_init()
                .map_err(|e| TracingError::InitError(e.to_string()))?;
        }
        LogFormat::Compact => {
            let compact_layer = fmt::layer()
                .compact()
                .with_target(false)
                .with_thread_ids(false);

            tracing_subscriber::registry()
                .with(env_filter)
                .with(compact_layer)
                .try_init()
                .map_err(|e| TracingError::InitError(e.to_string()))?;
        }
    }

    Ok(())
}

/// Initialize OpenTelemetry tracing (requires tokio runtime).
#[cfg(feature = "otel")]
pub async fn init_otel_tracing(config: &TracingConfig) -> Result<(), TracingError> {
    use opentelemetry::trace::TracerProvider;
    use opentelemetry_otlp::WithExportConfig;
    use opentelemetry_sdk::trace::Sampler;

    let endpoint = config
        .otel_endpoint
        .as_ref()
        .ok_or_else(|| TracingError::ConfigError("OTEL endpoint not configured".to_string()))?;

    // Create OTLP exporter
    let exporter = opentelemetry_otlp::new_exporter()
        .tonic()
        .with_endpoint(endpoint);

    // Create tracer provider
    let tracer_provider = opentelemetry_otlp::new_pipeline()
        .tracing()
        .with_exporter(exporter)
        .with_trace_config(
            opentelemetry_sdk::trace::config()
                .with_sampler(Sampler::TraceIdRatioBased(config.sample_rate))
                .with_resource(opentelemetry_sdk::Resource::new(vec![
                    opentelemetry::KeyValue::new("service.name", config.service_name.clone()),
                ])),
        )
        .install_batch(opentelemetry_sdk::runtime::Tokio)
        .map_err(|e| TracingError::OtelError(e.to_string()))?;

    // Create tracing layer
    let tracer = tracer_provider.tracer(&config.service_name);
    let telemetry_layer = tracing_opentelemetry::layer().with_tracer(tracer);

    // Build env filter
    let env_filter = EnvFilter::try_from_default_env()
        .unwrap_or_else(|_| EnvFilter::new(&config.log_level));

    // Build the subscriber
    let json_layer = fmt::layer()
        .json()
        .with_target(true)
        .with_span_events(FmtSpan::CLOSE);

    tracing_subscriber::registry()
        .with(env_filter)
        .with(json_layer)
        .with(telemetry_layer)
        .try_init()
        .map_err(|e| TracingError::InitError(e.to_string()))?;

    Ok(())
}

/// Shutdown OpenTelemetry tracing.
#[cfg(feature = "otel")]
pub fn shutdown_otel() {
    opentelemetry::global::shutdown_tracer_provider();
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_tracing_config_default() {
        let config = TracingConfig::default();
        assert_eq!(config.log_level, "info");
        assert_eq!(config.log_format, LogFormat::Json);
        assert!(config.otel_endpoint.is_none());
    }

    #[test]
    fn test_log_format_from_str() {
        assert_eq!(LogFormat::from_str("json"), LogFormat::Json);
        assert_eq!(LogFormat::from_str("JSON"), LogFormat::Json);
        assert_eq!(LogFormat::from_str("text"), LogFormat::Text);
        assert_eq!(LogFormat::from_str("pretty"), LogFormat::Text);
        assert_eq!(LogFormat::from_str("compact"), LogFormat::Compact);
        assert_eq!(LogFormat::from_str("unknown"), LogFormat::Json);
    }

    #[test]
    fn test_tracing_config_from_env() {
        // This test just verifies the function doesn't panic
        let _ = TracingConfig::from_env();
    }
}
