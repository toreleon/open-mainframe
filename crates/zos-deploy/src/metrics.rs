//! Prometheus metrics for zOS-clone.

use prometheus::{
    HistogramOpts, HistogramVec, IntCounter, IntCounterVec, IntGauge, IntGaugeVec, Opts, Registry,
};

/// Metrics registry with all application metrics.
#[derive(Debug, Clone)]
pub struct MetricsRegistry {
    /// Prometheus registry
    registry: Registry,
    /// General metrics
    pub general: Metrics,
    /// COBOL metrics
    pub cobol: CobolMetrics,
    /// CICS metrics
    pub cics: CicsMetrics,
    /// IMS metrics
    pub ims: ImsMetrics,
    /// Database metrics
    pub database: DatabaseMetrics,
}

impl MetricsRegistry {
    /// Create a new metrics registry with all metrics initialized.
    pub fn new(prefix: &str) -> Result<Self, prometheus::Error> {
        let registry = Registry::new();

        let general = Metrics::new(prefix, &registry)?;
        let cobol = CobolMetrics::new(prefix, &registry)?;
        let cics = CicsMetrics::new(prefix, &registry)?;
        let ims = ImsMetrics::new(prefix, &registry)?;
        let database = DatabaseMetrics::new(prefix, &registry)?;

        Ok(Self {
            registry,
            general,
            cobol,
            cics,
            ims,
            database,
        })
    }

    /// Get the prometheus registry for export.
    pub fn registry(&self) -> &Registry {
        &self.registry
    }

    /// Gather all metrics as text.
    pub fn gather(&self) -> String {
        use prometheus::Encoder;
        let encoder = prometheus::TextEncoder::new();
        let metric_families = self.registry.gather();
        let mut buffer = Vec::new();
        encoder.encode(&metric_families, &mut buffer).unwrap();
        String::from_utf8(buffer).unwrap()
    }
}

/// General application metrics.
#[derive(Debug, Clone)]
pub struct Metrics {
    /// Total requests
    pub requests_total: IntCounterVec,
    /// Request duration histogram
    pub request_duration: HistogramVec,
    /// Active connections
    pub active_connections: IntGauge,
    /// Errors by type
    pub errors_total: IntCounterVec,
}

impl Metrics {
    /// Create new general metrics.
    pub fn new(prefix: &str, registry: &Registry) -> Result<Self, prometheus::Error> {
        let requests_total = IntCounterVec::new(
            Opts::new(
                format!("{}_requests_total", prefix),
                "Total number of requests",
            ),
            &["method", "path", "status"],
        )?;
        registry.register(Box::new(requests_total.clone()))?;

        let request_duration = HistogramVec::new(
            HistogramOpts::new(
                format!("{}_request_duration_seconds", prefix),
                "Request duration in seconds",
            )
            .buckets(vec![0.001, 0.005, 0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 1.0, 2.5, 5.0, 10.0]),
            &["method", "path"],
        )?;
        registry.register(Box::new(request_duration.clone()))?;

        let active_connections = IntGauge::new(
            format!("{}_active_connections", prefix),
            "Number of active connections",
        )?;
        registry.register(Box::new(active_connections.clone()))?;

        let errors_total = IntCounterVec::new(
            Opts::new(format!("{}_errors_total", prefix), "Total number of errors"),
            &["type"],
        )?;
        registry.register(Box::new(errors_total.clone()))?;

        Ok(Self {
            requests_total,
            request_duration,
            active_connections,
            errors_total,
        })
    }

    /// Record a request.
    pub fn record_request(&self, method: &str, path: &str, status: u16, duration_secs: f64) {
        self.requests_total
            .with_label_values(&[method, path, &status.to_string()])
            .inc();
        self.request_duration
            .with_label_values(&[method, path])
            .observe(duration_secs);
    }

    /// Record an error.
    pub fn record_error(&self, error_type: &str) {
        self.errors_total.with_label_values(&[error_type]).inc();
    }
}

/// COBOL execution metrics.
#[derive(Debug, Clone)]
pub struct CobolMetrics {
    /// Programs executed
    pub programs_executed: IntCounterVec,
    /// Execution duration
    pub execution_duration: HistogramVec,
    /// Compilation time
    pub compilation_time: HistogramVec,
    /// Active programs
    pub active_programs: IntGauge,
}

impl CobolMetrics {
    /// Create new COBOL metrics.
    pub fn new(prefix: &str, registry: &Registry) -> Result<Self, prometheus::Error> {
        let programs_executed = IntCounterVec::new(
            Opts::new(
                format!("{}_cobol_programs_executed_total", prefix),
                "Total COBOL programs executed",
            ),
            &["program", "result"],
        )?;
        registry.register(Box::new(programs_executed.clone()))?;

        let execution_duration = HistogramVec::new(
            HistogramOpts::new(
                format!("{}_cobol_execution_duration_ms", prefix),
                "COBOL program execution duration in milliseconds",
            )
            .buckets(vec![1.0, 5.0, 10.0, 25.0, 50.0, 100.0, 250.0, 500.0, 1000.0, 5000.0]),
            &["program"],
        )?;
        registry.register(Box::new(execution_duration.clone()))?;

        let compilation_time = HistogramVec::new(
            HistogramOpts::new(
                format!("{}_cobol_compilation_time_ms", prefix),
                "COBOL program compilation time in milliseconds",
            )
            .buckets(vec![10.0, 50.0, 100.0, 500.0, 1000.0, 5000.0, 10000.0]),
            &["program"],
        )?;
        registry.register(Box::new(compilation_time.clone()))?;

        let active_programs = IntGauge::new(
            format!("{}_cobol_active_programs", prefix),
            "Number of currently executing COBOL programs",
        )?;
        registry.register(Box::new(active_programs.clone()))?;

        Ok(Self {
            programs_executed,
            execution_duration,
            compilation_time,
            active_programs,
        })
    }

    /// Record a program execution.
    pub fn record_execution(&self, program: &str, success: bool, duration_ms: f64) {
        let result = if success { "success" } else { "failure" };
        self.programs_executed
            .with_label_values(&[program, result])
            .inc();
        self.execution_duration
            .with_label_values(&[program])
            .observe(duration_ms);
    }

    /// Record a compilation.
    pub fn record_compilation(&self, program: &str, duration_ms: f64) {
        self.compilation_time
            .with_label_values(&[program])
            .observe(duration_ms);
    }
}

/// CICS transaction metrics.
#[derive(Debug, Clone)]
pub struct CicsMetrics {
    /// Transactions executed
    pub transactions_total: IntCounterVec,
    /// Transaction duration
    pub transaction_duration: HistogramVec,
    /// Active tasks
    pub active_tasks: IntGauge,
    /// Queue depths
    pub queue_depth: IntGaugeVec,
}

impl CicsMetrics {
    /// Create new CICS metrics.
    pub fn new(prefix: &str, registry: &Registry) -> Result<Self, prometheus::Error> {
        let transactions_total = IntCounterVec::new(
            Opts::new(
                format!("{}_cics_transactions_total", prefix),
                "Total CICS transactions",
            ),
            &["transid", "result"],
        )?;
        registry.register(Box::new(transactions_total.clone()))?;

        let transaction_duration = HistogramVec::new(
            HistogramOpts::new(
                format!("{}_cics_transaction_duration_ms", prefix),
                "CICS transaction duration in milliseconds",
            )
            .buckets(vec![1.0, 5.0, 10.0, 25.0, 50.0, 100.0, 250.0, 500.0, 1000.0]),
            &["transid"],
        )?;
        registry.register(Box::new(transaction_duration.clone()))?;

        let active_tasks = IntGauge::new(
            format!("{}_cics_active_tasks", prefix),
            "Number of active CICS tasks",
        )?;
        registry.register(Box::new(active_tasks.clone()))?;

        let queue_depth = IntGaugeVec::new(
            Opts::new(
                format!("{}_cics_queue_depth", prefix),
                "CICS queue depth by queue type",
            ),
            &["queue_type", "queue_name"],
        )?;
        registry.register(Box::new(queue_depth.clone()))?;

        Ok(Self {
            transactions_total,
            transaction_duration,
            active_tasks,
            queue_depth,
        })
    }

    /// Record a transaction.
    pub fn record_transaction(&self, transid: &str, success: bool, duration_ms: f64) {
        let result = if success { "success" } else { "failure" };
        self.transactions_total
            .with_label_values(&[transid, result])
            .inc();
        self.transaction_duration
            .with_label_values(&[transid])
            .observe(duration_ms);
    }

    /// Update queue depth.
    pub fn set_queue_depth(&self, queue_type: &str, queue_name: &str, depth: i64) {
        self.queue_depth
            .with_label_values(&[queue_type, queue_name])
            .set(depth);
    }
}

/// IMS DL/I metrics.
#[derive(Debug, Clone)]
pub struct ImsMetrics {
    /// DL/I calls
    pub dli_calls_total: IntCounterVec,
    /// DL/I duration
    pub dli_duration: HistogramVec,
    /// Segments retrieved
    pub segments_retrieved: IntCounterVec,
    /// Active PSBs
    pub active_psbs: IntGauge,
}

impl ImsMetrics {
    /// Create new IMS metrics.
    pub fn new(prefix: &str, registry: &Registry) -> Result<Self, prometheus::Error> {
        let dli_calls_total = IntCounterVec::new(
            Opts::new(
                format!("{}_ims_dli_calls_total", prefix),
                "Total IMS DL/I calls",
            ),
            &["call_type", "database", "status"],
        )?;
        registry.register(Box::new(dli_calls_total.clone()))?;

        let dli_duration = HistogramVec::new(
            HistogramOpts::new(
                format!("{}_ims_dli_duration_ms", prefix),
                "IMS DL/I call duration in milliseconds",
            )
            .buckets(vec![0.1, 0.5, 1.0, 2.5, 5.0, 10.0, 25.0, 50.0, 100.0]),
            &["call_type", "database"],
        )?;
        registry.register(Box::new(dli_duration.clone()))?;

        let segments_retrieved = IntCounterVec::new(
            Opts::new(
                format!("{}_ims_segments_retrieved_total", prefix),
                "Total IMS segments retrieved",
            ),
            &["database", "segment"],
        )?;
        registry.register(Box::new(segments_retrieved.clone()))?;

        let active_psbs = IntGauge::new(
            format!("{}_ims_active_psbs", prefix),
            "Number of active PSBs",
        )?;
        registry.register(Box::new(active_psbs.clone()))?;

        Ok(Self {
            dli_calls_total,
            dli_duration,
            segments_retrieved,
            active_psbs,
        })
    }

    /// Record a DL/I call.
    pub fn record_dli_call(
        &self,
        call_type: &str,
        database: &str,
        status: &str,
        duration_ms: f64,
    ) {
        self.dli_calls_total
            .with_label_values(&[call_type, database, status])
            .inc();
        self.dli_duration
            .with_label_values(&[call_type, database])
            .observe(duration_ms);
    }

    /// Record segments retrieved.
    pub fn record_segments(&self, database: &str, segment: &str, count: u64) {
        self.segments_retrieved
            .with_label_values(&[database, segment])
            .inc_by(count);
    }
}

/// Database connection metrics.
#[derive(Debug, Clone)]
pub struct DatabaseMetrics {
    /// Query duration
    pub query_duration: HistogramVec,
    /// Active connections
    pub pool_connections_active: IntGauge,
    /// Idle connections
    pub pool_connections_idle: IntGauge,
    /// Connection errors
    pub connection_errors: IntCounter,
}

impl DatabaseMetrics {
    /// Create new database metrics.
    pub fn new(prefix: &str, registry: &Registry) -> Result<Self, prometheus::Error> {
        let query_duration = HistogramVec::new(
            HistogramOpts::new(
                format!("{}_db_query_duration_ms", prefix),
                "Database query duration in milliseconds",
            )
            .buckets(vec![0.5, 1.0, 2.5, 5.0, 10.0, 25.0, 50.0, 100.0, 250.0, 500.0]),
            &["query_type"],
        )?;
        registry.register(Box::new(query_duration.clone()))?;

        let pool_connections_active = IntGauge::new(
            format!("{}_db_pool_connections_active", prefix),
            "Number of active database connections",
        )?;
        registry.register(Box::new(pool_connections_active.clone()))?;

        let pool_connections_idle = IntGauge::new(
            format!("{}_db_pool_connections_idle", prefix),
            "Number of idle database connections",
        )?;
        registry.register(Box::new(pool_connections_idle.clone()))?;

        let connection_errors = IntCounter::new(
            format!("{}_db_connection_errors_total", prefix),
            "Total database connection errors",
        )?;
        registry.register(Box::new(connection_errors.clone()))?;

        Ok(Self {
            query_duration,
            pool_connections_active,
            pool_connections_idle,
            connection_errors,
        })
    }

    /// Record a query.
    pub fn record_query(&self, query_type: &str, duration_ms: f64) {
        self.query_duration
            .with_label_values(&[query_type])
            .observe(duration_ms);
    }

    /// Update pool statistics.
    pub fn update_pool_stats(&self, active: i64, idle: i64) {
        self.pool_connections_active.set(active);
        self.pool_connections_idle.set(idle);
    }

    /// Record a connection error.
    pub fn record_connection_error(&self) {
        self.connection_errors.inc();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_metrics_registry_creation() {
        let registry = MetricsRegistry::new("test").unwrap();
        assert!(!registry.gather().is_empty());
    }

    #[test]
    fn test_general_metrics() {
        let registry = Registry::new();
        let metrics = Metrics::new("test", &registry).unwrap();

        metrics.record_request("GET", "/api/test", 200, 0.123);
        metrics.record_error("connection_timeout");

        // Verify counters incremented
        assert_eq!(
            metrics
                .requests_total
                .with_label_values(&["GET", "/api/test", "200"])
                .get(),
            1
        );
    }

    #[test]
    fn test_cobol_metrics() {
        let registry = Registry::new();
        let metrics = CobolMetrics::new("test", &registry).unwrap();

        metrics.record_execution("TESTPROG", true, 50.0);
        metrics.record_compilation("TESTPROG", 100.0);

        assert_eq!(
            metrics
                .programs_executed
                .with_label_values(&["TESTPROG", "success"])
                .get(),
            1
        );
    }

    #[test]
    fn test_cics_metrics() {
        let registry = Registry::new();
        let metrics = CicsMetrics::new("test", &registry).unwrap();

        metrics.record_transaction("INQY", true, 25.0);
        metrics.set_queue_depth("TS", "TESTQ", 5);

        assert_eq!(
            metrics
                .transactions_total
                .with_label_values(&["INQY", "success"])
                .get(),
            1
        );
        assert_eq!(
            metrics
                .queue_depth
                .with_label_values(&["TS", "TESTQ"])
                .get(),
            5
        );
    }

    #[test]
    fn test_ims_metrics() {
        let registry = Registry::new();
        let metrics = ImsMetrics::new("test", &registry).unwrap();

        metrics.record_dli_call("GU", "CUSTDB", "", 5.0);
        metrics.record_segments("CUSTDB", "CUSTOMER", 1);

        assert_eq!(
            metrics
                .dli_calls_total
                .with_label_values(&["GU", "CUSTDB", ""])
                .get(),
            1
        );
    }

    #[test]
    fn test_database_metrics() {
        let registry = Registry::new();
        let metrics = DatabaseMetrics::new("test", &registry).unwrap();

        metrics.record_query("SELECT", 2.5);
        metrics.update_pool_stats(5, 3);
        metrics.record_connection_error();

        assert_eq!(metrics.pool_connections_active.get(), 5);
        assert_eq!(metrics.pool_connections_idle.get(), 3);
        assert_eq!(metrics.connection_errors.get(), 1);
    }
}
