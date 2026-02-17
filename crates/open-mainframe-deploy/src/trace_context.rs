//! Distributed trace context propagation for CICS transactions.
//!
//! Attaches OpenTelemetry-style span context to CICS task execution,
//! creating child spans for LINK, XCTL, and EXEC SQL operations.
//! Derives trace IDs from EIBTASKN for deterministic correlation.

use std::collections::HashMap;
use std::time::Instant;

/// A trace ID derived from EIBTASKN for deterministic transaction tracing.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TraceId(pub String);

impl TraceId {
    /// Create a trace ID from a CICS EIBTASKN value.
    pub fn from_eibtaskn(task_num: u32) -> Self {
        // Pad to 32 hex chars for W3C trace context compatibility
        Self(format!("{:032x}", task_num))
    }

    /// Get the hex string representation.
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

/// A span ID for a single operation within a trace.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SpanId(pub String);

impl SpanId {
    /// Create a span ID from a program name and sequence number.
    pub fn new(program: &str, seq: u16) -> Self {
        // 16 hex chars for W3C span ID format
        let hash = simple_hash(program) ^ (seq as u64);
        Self(format!("{:016x}", hash))
    }
}

/// Simple hash for deterministic span IDs (not cryptographic).
fn simple_hash(s: &str) -> u64 {
    let mut h: u64 = 0xcbf2_9ce4_8422_2325;
    for b in s.bytes() {
        h ^= b as u64;
        h = h.wrapping_mul(0x0100_0000_01b3);
    }
    h
}

/// Type of operation that creates a span.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SpanKind {
    /// CICS LINK to another program.
    CicsLink,
    /// CICS XCTL to another program.
    CicsXctl,
    /// EXEC SQL query execution.
    SqlQuery,
    /// Top-level transaction execution.
    Transaction,
    /// Generic program execution.
    Program,
}

impl SpanKind {
    /// Display label for the span kind.
    pub fn label(&self) -> &'static str {
        match self {
            SpanKind::CicsLink => "CICS LINK",
            SpanKind::CicsXctl => "CICS XCTL",
            SpanKind::SqlQuery => "SQL Query",
            SpanKind::Transaction => "Transaction",
            SpanKind::Program => "Program",
        }
    }
}

/// A single span representing an operation within a trace.
#[derive(Debug, Clone)]
pub struct TraceSpan {
    /// Span identifier.
    pub span_id: SpanId,
    /// Parent span ID (None for root).
    pub parent_id: Option<SpanId>,
    /// Operation name (program name, SQL text, etc.).
    pub operation: String,
    /// Kind of operation.
    pub kind: SpanKind,
    /// Start time.
    pub start: Instant,
    /// Duration in microseconds (set when span ends).
    pub duration_us: Option<u64>,
    /// Additional attributes.
    pub attributes: HashMap<String, String>,
}

/// Transaction trace context that accumulates spans.
#[derive(Debug)]
pub struct TransactionTrace {
    /// Trace ID (derived from EIBTASKN).
    pub trace_id: TraceId,
    /// Transaction ID (e.g., "COSG").
    pub transid: String,
    /// EIBTASKN value.
    pub eibtaskn: u32,
    /// All spans in this trace.
    pub spans: Vec<TraceSpan>,
    /// Span sequence counter.
    seq: u16,
    /// Currently active span index.
    active_span: Option<usize>,
}

impl TransactionTrace {
    /// Create a new transaction trace from a CICS task.
    pub fn new(eibtaskn: u32, transid: &str) -> Self {
        Self {
            trace_id: TraceId::from_eibtaskn(eibtaskn),
            transid: transid.to_string(),
            eibtaskn,
            spans: Vec::new(),
            seq: 0,
            active_span: None,
        }
    }

    /// Start a new root span for the transaction.
    pub fn start_transaction_span(&mut self, program: &str) -> usize {
        self.start_span(program, SpanKind::Transaction, None)
    }

    /// Start a child span for a LINK operation.
    pub fn start_link_span(&mut self, target_program: &str) -> usize {
        let parent = self.active_span.map(|i| self.spans[i].span_id.clone());
        self.start_span(target_program, SpanKind::CicsLink, parent)
    }

    /// Start a child span for an XCTL operation.
    pub fn start_xctl_span(&mut self, target_program: &str) -> usize {
        let parent = self.active_span.map(|i| self.spans[i].span_id.clone());
        self.start_span(target_program, SpanKind::CicsXctl, parent)
    }

    /// Start a child span for a SQL query.
    pub fn start_sql_span(&mut self, sql_text: &str) -> usize {
        let parent = self.active_span.map(|i| self.spans[i].span_id.clone());
        let idx = self.start_span(sql_text, SpanKind::SqlQuery, parent);
        self.spans[idx]
            .attributes
            .insert("db.statement".to_string(), sql_text.to_string());
        idx
    }

    /// End a span, recording its duration.
    pub fn end_span(&mut self, span_idx: usize) {
        if let Some(span) = self.spans.get_mut(span_idx) {
            span.duration_us = Some(span.start.elapsed().as_micros() as u64);
        }
        // Restore parent as active
        if self.active_span == Some(span_idx) {
            self.active_span = self
                .spans
                .get(span_idx)
                .and_then(|s| s.parent_id.as_ref())
                .and_then(|pid| self.spans.iter().position(|s| s.span_id == *pid));
        }
    }

    /// Get all completed spans.
    pub fn completed_spans(&self) -> Vec<&TraceSpan> {
        self.spans.iter().filter(|s| s.duration_us.is_some()).collect()
    }

    /// Get span count.
    pub fn span_count(&self) -> usize {
        self.spans.len()
    }

    fn start_span(
        &mut self,
        operation: &str,
        kind: SpanKind,
        parent_id: Option<SpanId>,
    ) -> usize {
        self.seq += 1;
        let span = TraceSpan {
            span_id: SpanId::new(operation, self.seq),
            parent_id,
            operation: operation.to_string(),
            kind,
            start: Instant::now(),
            duration_us: None,
            attributes: HashMap::new(),
        };
        let idx = self.spans.len();
        self.spans.push(span);
        self.active_span = Some(idx);
        // Add EIBTASKN as an attribute
        self.spans[idx]
            .attributes
            .insert("cics.eibtaskn".to_string(), self.eibtaskn.to_string());
        idx
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_trace_id_from_eibtaskn() {
        let tid = TraceId::from_eibtaskn(12345);
        assert_eq!(tid.as_str().len(), 32);
        assert!(tid.as_str().ends_with("3039")); // 0x3039 = 12345
    }

    #[test]
    fn test_transaction_trace_spans() {
        let mut trace = TransactionTrace::new(100, "COSG");
        let root = trace.start_transaction_span("COSGN00C");
        let link = trace.start_link_span("COSGN00P");
        let sql = trace.start_sql_span("SELECT NAME FROM CUSTOMER");

        assert_eq!(trace.span_count(), 3);

        // SQL span should have db.statement attribute
        assert_eq!(
            trace.spans[sql].attributes.get("db.statement").unwrap(),
            "SELECT NAME FROM CUSTOMER"
        );

        // All spans should have EIBTASKN attribute
        for span in &trace.spans {
            assert_eq!(span.attributes.get("cics.eibtaskn").unwrap(), "100");
        }

        // End spans
        trace.end_span(sql);
        trace.end_span(link);
        trace.end_span(root);

        assert_eq!(trace.completed_spans().len(), 3);
    }

    #[test]
    fn test_span_parent_chain() {
        let mut trace = TransactionTrace::new(1, "TEST");
        let root = trace.start_transaction_span("PROG-A");
        let child = trace.start_link_span("PROG-B");
        let grandchild = trace.start_link_span("PROG-C");

        // grandchild's parent should be child
        assert_eq!(
            trace.spans[grandchild].parent_id.as_ref().unwrap(),
            &trace.spans[child].span_id
        );
        // child's parent should be root
        assert_eq!(
            trace.spans[child].parent_id.as_ref().unwrap(),
            &trace.spans[root].span_id
        );
        // root has no parent
        assert!(trace.spans[root].parent_id.is_none());
    }

    #[test]
    fn test_span_kind_labels() {
        assert_eq!(SpanKind::CicsLink.label(), "CICS LINK");
        assert_eq!(SpanKind::SqlQuery.label(), "SQL Query");
        assert_eq!(SpanKind::Transaction.label(), "Transaction");
    }

    #[test]
    fn test_xctl_span() {
        let mut trace = TransactionTrace::new(42, "MENU");
        trace.start_transaction_span("MAINPROG");
        let xctl = trace.start_xctl_span("TARGET");
        assert_eq!(trace.spans[xctl].kind, SpanKind::CicsXctl);
        assert_eq!(trace.spans[xctl].operation, "TARGET");
    }
}
