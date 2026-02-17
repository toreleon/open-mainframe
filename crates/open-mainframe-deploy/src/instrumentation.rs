//! Runtime integration hooks for automatic metrics instrumentation.
//!
//! Provides middleware-style wrappers that instrument COBOL, CICS, and IMS
//! runtime operations with Prometheus metrics. Instrumentation is opt-in
//! via the [`InstrumentedRuntime`] trait and builder-style constructors.
//!
//! # Design
//!
//! Each runtime gains an `Arc<MetricsRegistry>` via `.with_metrics(registry)`.
//! The instrumentation wraps operations to: record start time, execute the
//! operation, then record duration and result. This adds microseconds of
//! overhead per operation when metrics are enabled, and zero overhead when
//! they are disabled.

use std::sync::Arc;
use std::time::Instant;

use crate::metrics::MetricsRegistry;

/// Trait for runtimes that support optional metrics instrumentation.
///
/// Implementing this trait enables a runtime to accept a `MetricsRegistry`
/// and report execution metrics automatically.
pub trait InstrumentedRuntime {
    /// Attach a metrics registry to this runtime for automatic instrumentation.
    fn with_metrics(self, registry: Arc<MetricsRegistry>) -> Self;

    /// Check if metrics instrumentation is enabled.
    fn has_metrics(&self) -> bool;
}

/// COBOL runtime instrumentation wrapper.
///
/// Wraps COBOL program execution with metrics recording:
/// - `programs_executed` counter incremented per call
/// - `execution_duration` histogram records each call's duration
/// - `compilation_time` histogram records compilation durations
/// - `active_programs` gauge tracks concurrent executions
#[derive(Debug, Clone)]
pub struct CobolInstrumentation {
    registry: Arc<MetricsRegistry>,
}

impl CobolInstrumentation {
    /// Create a new COBOL instrumentation wrapper.
    pub fn new(registry: Arc<MetricsRegistry>) -> Self {
        Self { registry }
    }

    /// Record the start of a program execution.
    ///
    /// Returns a [`ProgramExecGuard`] that records the duration when dropped.
    pub fn begin_execution(&self, program: &str) -> ProgramExecGuard {
        self.registry.cobol.active_programs.inc();
        ProgramExecGuard {
            registry: Arc::clone(&self.registry),
            program: program.to_string(),
            start: Instant::now(),
            finished: false,
        }
    }

    /// Record a completed program execution directly.
    pub fn record_execution(&self, program: &str, success: bool, duration_ms: f64) {
        self.registry
            .cobol
            .record_execution(program, success, duration_ms);
    }

    /// Record a compilation duration.
    pub fn record_compilation(&self, program: &str, duration_ms: f64) {
        self.registry
            .cobol
            .record_compilation(program, duration_ms);
    }
}

/// RAII guard for tracking program execution duration.
///
/// When dropped, records the execution duration and decrements the
/// active programs gauge.
#[derive(Debug)]
pub struct ProgramExecGuard {
    registry: Arc<MetricsRegistry>,
    program: String,
    start: Instant,
    finished: bool,
}

impl ProgramExecGuard {
    /// Complete the execution with a success/failure result.
    pub fn finish(mut self, success: bool) {
        self.finished = true;
        let duration_ms = self.start.elapsed().as_secs_f64() * 1000.0;
        self.registry
            .cobol
            .record_execution(&self.program, success, duration_ms);
        self.registry.cobol.active_programs.dec();
    }
}

impl Drop for ProgramExecGuard {
    fn drop(&mut self) {
        if !self.finished {
            self.registry.cobol.active_programs.dec();
        }
    }
}

/// CICS transaction instrumentation wrapper.
///
/// Wraps CICS command execution with metrics recording:
/// - `transactions_total` counter per transaction ID
/// - `transaction_duration` histogram per transaction
/// - `active_tasks` gauge for concurrent tasks
/// - `queue_depth` gauge for TS/TD queue monitoring
#[derive(Debug, Clone)]
pub struct CicsInstrumentation {
    registry: Arc<MetricsRegistry>,
}

impl CicsInstrumentation {
    /// Create a new CICS instrumentation wrapper.
    pub fn new(registry: Arc<MetricsRegistry>) -> Self {
        Self { registry }
    }

    /// Record the start of a CICS transaction.
    ///
    /// Returns a [`TransactionGuard`] that records duration when completed.
    pub fn begin_transaction(&self, transid: &str) -> TransactionGuard {
        self.registry.cics.active_tasks.inc();
        TransactionGuard {
            registry: Arc::clone(&self.registry),
            transid: transid.to_string(),
            start: Instant::now(),
            finished: false,
        }
    }

    /// Record a CICS command execution directly.
    pub fn record_command(
        &self,
        transid: &str,
        _command: &str,
        success: bool,
        duration_ms: f64,
    ) {
        self.registry
            .cics
            .record_transaction(transid, success, duration_ms);
    }

    /// Update a queue depth metric.
    pub fn update_queue_depth(&self, queue_type: &str, queue_name: &str, depth: i64) {
        self.registry
            .cics
            .set_queue_depth(queue_type, queue_name, depth);
    }
}

/// RAII guard for tracking CICS transaction duration.
#[derive(Debug)]
pub struct TransactionGuard {
    registry: Arc<MetricsRegistry>,
    transid: String,
    start: Instant,
    finished: bool,
}

impl TransactionGuard {
    /// Complete the transaction with a success/failure result.
    pub fn finish(mut self, success: bool) {
        self.finished = true;
        let duration_ms = self.start.elapsed().as_secs_f64() * 1000.0;
        self.registry
            .cics
            .record_transaction(&self.transid, success, duration_ms);
        self.registry.cics.active_tasks.dec();
    }
}

impl Drop for TransactionGuard {
    fn drop(&mut self) {
        if !self.finished {
            self.registry.cics.active_tasks.dec();
        }
    }
}

/// IMS DL/I instrumentation wrapper.
///
/// Wraps IMS DL/I calls with metrics recording:
/// - `dli_calls_total` counter per call type and database
/// - `dli_duration` histogram per call
/// - `active_psbs` gauge for concurrent PSBs
#[derive(Debug, Clone)]
pub struct ImsInstrumentation {
    registry: Arc<MetricsRegistry>,
}

impl ImsInstrumentation {
    /// Create a new IMS instrumentation wrapper.
    pub fn new(registry: Arc<MetricsRegistry>) -> Self {
        Self { registry }
    }

    /// Record the start of a PSB schedule.
    pub fn psb_scheduled(&self) {
        self.registry.ims.active_psbs.inc();
    }

    /// Record the end of a PSB (TERM call).
    pub fn psb_terminated(&self) {
        self.registry.ims.active_psbs.dec();
    }

    /// Record a DL/I call with timing.
    pub fn record_dli_call(
        &self,
        call_type: &str,
        database: &str,
        status: &str,
        duration_ms: f64,
    ) {
        self.registry
            .ims
            .record_dli_call(call_type, database, status, duration_ms);
    }

    /// Record segments retrieved.
    pub fn record_segments(&self, database: &str, segment: &str, count: u64) {
        self.registry.ims.record_segments(database, segment, count);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::MetricsRegistry;

    fn test_registry() -> Arc<MetricsRegistry> {
        Arc::new(MetricsRegistry::new("test_instr").unwrap())
    }

    #[test]
    fn test_cobol_instrumentation_record() {
        let registry = test_registry();
        let instr = CobolInstrumentation::new(Arc::clone(&registry));

        instr.record_execution("TESTPROG", true, 50.0);

        assert_eq!(
            registry
                .cobol
                .programs_executed
                .with_label_values(&["TESTPROG", "success"])
                .get(),
            1
        );
    }

    #[test]
    fn test_cobol_exec_guard_finish() {
        let registry = test_registry();
        let instr = CobolInstrumentation::new(Arc::clone(&registry));

        assert_eq!(registry.cobol.active_programs.get(), 0);

        let guard = instr.begin_execution("PROG1");
        assert_eq!(registry.cobol.active_programs.get(), 1);

        guard.finish(true);
        assert_eq!(registry.cobol.active_programs.get(), 0);
        assert_eq!(
            registry
                .cobol
                .programs_executed
                .with_label_values(&["PROG1", "success"])
                .get(),
            1
        );
    }

    #[test]
    fn test_cobol_exec_guard_drop() {
        let registry = test_registry();
        let instr = CobolInstrumentation::new(Arc::clone(&registry));

        {
            let _guard = instr.begin_execution("PROG2");
            assert_eq!(registry.cobol.active_programs.get(), 1);
        }
        // Guard dropped without finish() — active_programs should still decrement
        assert_eq!(registry.cobol.active_programs.get(), 0);
    }

    #[test]
    fn test_cobol_compilation_metrics() {
        let registry = test_registry();
        let instr = CobolInstrumentation::new(Arc::clone(&registry));

        instr.record_compilation("COMPILE1", 200.0);
        // No panic = metric recorded successfully
    }

    #[test]
    fn test_cics_instrumentation_record() {
        let registry = test_registry();
        let instr = CicsInstrumentation::new(Arc::clone(&registry));

        instr.record_command("INQY", "LINK", true, 25.0);

        assert_eq!(
            registry
                .cics
                .transactions_total
                .with_label_values(&["INQY", "success"])
                .get(),
            1
        );
    }

    #[test]
    fn test_cics_transaction_guard() {
        let registry = test_registry();
        let instr = CicsInstrumentation::new(Arc::clone(&registry));

        assert_eq!(registry.cics.active_tasks.get(), 0);

        let guard = instr.begin_transaction("TXN1");
        assert_eq!(registry.cics.active_tasks.get(), 1);

        guard.finish(true);
        assert_eq!(registry.cics.active_tasks.get(), 0);
        assert_eq!(
            registry
                .cics
                .transactions_total
                .with_label_values(&["TXN1", "success"])
                .get(),
            1
        );
    }

    #[test]
    fn test_cics_queue_depth() {
        let registry = test_registry();
        let instr = CicsInstrumentation::new(Arc::clone(&registry));

        instr.update_queue_depth("TS", "MYQUEUE", 10);

        assert_eq!(
            registry
                .cics
                .queue_depth
                .with_label_values(&["TS", "MYQUEUE"])
                .get(),
            10
        );
    }

    #[test]
    fn test_ims_instrumentation() {
        let registry = test_registry();
        let instr = ImsInstrumentation::new(Arc::clone(&registry));

        instr.psb_scheduled();
        assert_eq!(registry.ims.active_psbs.get(), 1);

        instr.record_dli_call("GU", "CUSTDB", "", 5.0);
        assert_eq!(
            registry
                .ims
                .dli_calls_total
                .with_label_values(&["GU", "CUSTDB", ""])
                .get(),
            1
        );

        instr.record_segments("CUSTDB", "CUSTOMER", 3);
        assert_eq!(
            registry
                .ims
                .segments_retrieved
                .with_label_values(&["CUSTDB", "CUSTOMER"])
                .get(),
            3
        );

        instr.psb_terminated();
        assert_eq!(registry.ims.active_psbs.get(), 0);
    }

    #[test]
    fn test_multiple_concurrent_executions() {
        let registry = test_registry();
        let instr = CobolInstrumentation::new(Arc::clone(&registry));

        let g1 = instr.begin_execution("PROG1");
        let g2 = instr.begin_execution("PROG2");
        let g3 = instr.begin_execution("PROG3");

        assert_eq!(registry.cobol.active_programs.get(), 3);

        g1.finish(true);
        assert_eq!(registry.cobol.active_programs.get(), 2);

        g2.finish(false);
        assert_eq!(registry.cobol.active_programs.get(), 1);

        g3.finish(true);
        assert_eq!(registry.cobol.active_programs.get(), 0);

        assert_eq!(
            registry
                .cobol
                .programs_executed
                .with_label_values(&["PROG2", "failure"])
                .get(),
            1
        );
    }
}
