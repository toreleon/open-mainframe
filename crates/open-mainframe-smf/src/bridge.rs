//! Observability Bridge.
//!
//! Bridges SMF records to modern observability formats:
//! - SMF -> Prometheus metrics (export SMF data as Prometheus-format metrics)
//! - SMF -> OpenTelemetry spans (convert job lifecycles to traces)
//! - Prometheus -> SMF reverse bridge (ingest Prometheus metrics as SMF user records)

use crate::record::{extend_padded, push_u64, read_padded, read_u16, read_u64, SmfRecord};
use std::collections::HashMap;

// ---------------------------------------------------------------------------
//  Prometheus metric types
// ---------------------------------------------------------------------------

/// Prometheus metric type.
#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum MetricType {
    /// A counter (monotonically increasing).
    Counter,
    /// A gauge (can go up and down).
    Gauge,
    /// A histogram.
    Histogram,
}

/// A Prometheus-format metric.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct PrometheusMetric {
    /// Metric name (e.g., "smf_job_cpu_seconds").
    pub name: String,
    /// Metric help text.
    pub help: String,
    /// Metric type.
    pub metric_type: MetricType,
    /// Metric value.
    pub value: f64,
    /// Labels (key-value pairs).
    pub labels: HashMap<String, String>,
}

impl PrometheusMetric {
    /// Create a new metric.
    pub fn new(name: &str, value: f64, metric_type: MetricType) -> Self {
        Self {
            name: name.to_string(),
            help: String::new(),
            metric_type,
            value,
            labels: HashMap::new(),
        }
    }

    /// Add a label.
    pub fn with_label(mut self, key: &str, value: &str) -> Self {
        self.labels.insert(key.to_string(), value.to_string());
        self
    }

    /// Set help text.
    pub fn with_help(mut self, help: &str) -> Self {
        self.help = help.to_string();
        self
    }

    /// Format as Prometheus text exposition format.
    pub fn to_prometheus_text(&self) -> String {
        let mut output = String::new();
        if !self.help.is_empty() {
            output.push_str(&format!("# HELP {} {}\n", self.name, self.help));
        }
        let type_str = match self.metric_type {
            MetricType::Counter => "counter",
            MetricType::Gauge => "gauge",
            MetricType::Histogram => "histogram",
        };
        output.push_str(&format!("# TYPE {} {}\n", self.name, type_str));

        if self.labels.is_empty() {
            output.push_str(&format!("{} {}\n", self.name, self.value));
        } else {
            let labels: Vec<String> = self
                .labels
                .iter()
                .map(|(k, v)| format!("{}=\"{}\"", k, v))
                .collect();
            output.push_str(&format!(
                "{}{{{}}} {}\n",
                self.name,
                labels.join(","),
                self.value,
            ));
        }
        output
    }
}

// ---------------------------------------------------------------------------
//  SMF -> Prometheus bridge
// ---------------------------------------------------------------------------

/// Converts SMF records to Prometheus metrics.
pub struct SmfToPrometheus;

impl SmfToPrometheus {
    /// Extract metrics from a Type 30 job accounting record.
    pub fn from_type30(record: &SmfRecord) -> Vec<PrometheusMetric> {
        let mut metrics = Vec::new();

        if record.header.record_type != 30 || record.data.len() < 52 {
            return metrics;
        }

        // Parse job name (offset 2..10) and service class (offset 44..52).
        let job_name = read_padded(&record.data, 2, 8);
        let service_class = read_padded(&record.data, 44, 8);

        // CPU time at offset 52 (8 bytes, microseconds).
        if record.data.len() >= 60 {
            let cpu_us = read_u64(&record.data, 52);
            let cpu_secs = cpu_us as f64 / 1_000_000.0;
            metrics.push(
                PrometheusMetric::new("smf_job_cpu_seconds", cpu_secs, MetricType::Gauge)
                    .with_label("job_name", &job_name)
                    .with_label("service_class", &service_class)
                    .with_help("CPU time consumed by job in seconds"),
            );
        }

        // Elapsed time at offset 68 (8 bytes, microseconds).
        if record.data.len() >= 76 {
            let elapsed_us = read_u64(&record.data, 68);
            let elapsed_secs = elapsed_us as f64 / 1_000_000.0;
            metrics.push(
                PrometheusMetric::new("smf_job_elapsed_seconds", elapsed_secs, MetricType::Gauge)
                    .with_label("job_name", &job_name)
                    .with_help("Elapsed wall-clock time for job in seconds"),
            );
        }

        metrics
    }

    /// Extract metrics from a Type 72 workload activity record.
    pub fn from_type72(record: &SmfRecord) -> Vec<PrometheusMetric> {
        let mut metrics = Vec::new();

        if record.header.record_type != 72 || record.data.len() < 12 {
            return metrics;
        }

        let sc_count = read_u16(&record.data, 0) as usize;
        let mut offset = 12; // After header (count + reserved + interval).

        for _ in 0..sc_count {
            if offset + 36 > record.data.len() {
                break;
            }

            let name = read_padded(&record.data, offset, 8);
            let pi = read_u16(&record.data, offset + 24);
            let pi_float = pi as f64 / 100.0;

            metrics.push(
                PrometheusMetric::new(
                    "smf_wlm_performance_index",
                    pi_float,
                    MetricType::Gauge,
                )
                .with_label("service_class", &name)
                .with_help("WLM Performance Index (1.0 = at goal)"),
            );

            offset += 36; // Size per service class entry.
        }

        metrics
    }
}

// ---------------------------------------------------------------------------
//  OpenTelemetry span representation
// ---------------------------------------------------------------------------

/// A simplified OpenTelemetry span for SMF job lifecycle tracing.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct OtelSpan {
    /// Trace ID.
    pub trace_id: String,
    /// Span ID.
    pub span_id: String,
    /// Parent span ID (empty for root spans).
    pub parent_span_id: String,
    /// Operation name.
    pub operation: String,
    /// Start time (microseconds since epoch or relative).
    pub start_time_us: u64,
    /// End time (microseconds).
    pub end_time_us: u64,
    /// Attributes.
    pub attributes: HashMap<String, String>,
}

impl OtelSpan {
    /// Duration in microseconds.
    pub fn duration_us(&self) -> u64 {
        self.end_time_us.saturating_sub(self.start_time_us)
    }
}

/// Converts Type 30 job lifecycle records to OpenTelemetry spans.
pub struct SmfToOtel;

impl SmfToOtel {
    /// Convert a set of Type 30 records (from a lifecycle collector) into spans.
    pub fn from_type30_lifecycle(records: &[SmfRecord]) -> Vec<OtelSpan> {
        let mut spans = Vec::new();

        if records.is_empty() {
            return spans;
        }

        // Use the first record's job name as trace context.
        let trace_id = format!("smf-{:016x}", records.len() as u64);
        let root_span_id = format!("root-{:08x}", 0u32);

        // Create a root span from initiation to termination.
        let first_time = records.first().map(|r| r.header.time as u64).unwrap_or(0);
        let last_time = records.last().map(|r| r.header.time as u64).unwrap_or(0);

        let job_name = if !records.is_empty() && records[0].data.len() >= 10 {
            String::from_utf8_lossy(&records[0].data[2..10])
                .trim_end()
                .to_string()
        } else {
            "unknown".to_string()
        };

        spans.push(OtelSpan {
            trace_id: trace_id.clone(),
            span_id: root_span_id.clone(),
            parent_span_id: String::new(),
            operation: format!("job:{}", job_name),
            start_time_us: first_time * 10_000, // hundredths to us
            end_time_us: last_time * 10_000,
            attributes: {
                let mut attrs = HashMap::new();
                attrs.insert("smf.record_type".to_string(), "30".to_string());
                attrs.insert("job.name".to_string(), job_name.clone());
                attrs
            },
        });

        // Create child spans for each step/interval record.
        for (i, rec) in records.iter().enumerate().skip(1) {
            let span_id = format!("span-{:08x}", i as u32);
            let subtype = if rec.data.len() >= 2 {
                u16::from_be_bytes([rec.data[0], rec.data[1]])
            } else {
                0
            };
            let op = match subtype {
                2 => "interval".to_string(),
                3 => "step:termination".to_string(),
                4 => "job:termination".to_string(),
                _ => format!("subtype:{}", subtype),
            };

            spans.push(OtelSpan {
                trace_id: trace_id.clone(),
                span_id,
                parent_span_id: root_span_id.clone(),
                operation: op,
                start_time_us: rec.header.time as u64 * 10_000,
                end_time_us: rec.header.time as u64 * 10_000,
                attributes: HashMap::new(),
            });
        }

        spans
    }
}

// ---------------------------------------------------------------------------
//  Prometheus -> SMF reverse bridge
// ---------------------------------------------------------------------------

/// Converts Prometheus metrics to SMF user records (Type 200).
pub struct PrometheusToSmf;

impl PrometheusToSmf {
    /// User record type for Prometheus-sourced records.
    pub const USER_RECORD_TYPE: u8 = 200;

    /// Convert a Prometheus metric to an SMF user record.
    pub fn to_smf_record(metric: &PrometheusMetric) -> SmfRecord {
        let mut data = Vec::new();

        // Metric name (32 bytes).
        extend_padded(&mut data, &metric.name, 32);

        // Metric value as f64 bits (8 bytes).
        push_u64(&mut data, metric.value.to_bits());

        // Metric type (1 byte).
        data.push(match metric.metric_type {
            MetricType::Counter => 1,
            MetricType::Gauge => 2,
            MetricType::Histogram => 3,
        });

        // Label count (1 byte).
        data.push(metric.labels.len().min(255) as u8);

        // Padding (2 bytes).
        data.push(0);
        data.push(0);

        // Labels: key(16) + value(32) each.
        for (k, v) in &metric.labels {
            extend_padded(&mut data, k, 16);
            extend_padded(&mut data, v, 32);
        }

        SmfRecord::new(Self::USER_RECORD_TYPE, data)
    }
}

// ---------------------------------------------------------------------------
//  Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::type30::Type30Record;

    #[test]
    fn test_prometheus_metric_text() {
        let metric = PrometheusMetric::new("smf_job_cpu_seconds", 1.5, MetricType::Gauge)
            .with_label("job_name", "MYJOB")
            .with_help("CPU time in seconds");
        let text = metric.to_prometheus_text();
        assert!(text.contains("# HELP smf_job_cpu_seconds CPU time in seconds"));
        assert!(text.contains("# TYPE smf_job_cpu_seconds gauge"));
        assert!(text.contains("job_name=\"MYJOB\""));
        assert!(text.contains("1.5"));
    }

    #[test]
    fn test_prometheus_metric_no_labels() {
        let metric = PrometheusMetric::new("simple_metric", 42.0, MetricType::Counter);
        let text = metric.to_prometheus_text();
        assert!(text.contains("simple_metric 42"));
    }

    #[test]
    fn test_smf_to_prometheus_type30() {
        let rec = Type30Record::job_termination("PAYROLL", "JOB001", "PRODBTCH", 2_000_000, 5_000_000, 0);
        let smf = rec.to_record();
        let metrics = SmfToPrometheus::from_type30(&smf);
        assert!(!metrics.is_empty());

        let cpu_metric = metrics.iter().find(|m| m.name == "smf_job_cpu_seconds");
        assert!(cpu_metric.is_some());
        let cpu = cpu_metric.unwrap();
        assert!((cpu.value - 2.0).abs() < 0.01);
        assert_eq!(cpu.labels.get("job_name").unwrap(), "PAYROLL");
    }

    #[test]
    fn test_smf_to_prometheus_wrong_type() {
        let rec = SmfRecord::new(4, vec![0; 50]);
        let metrics = SmfToPrometheus::from_type30(&rec);
        assert!(metrics.is_empty());
    }

    #[test]
    fn test_smf_to_otel_spans() {
        let mut records = Vec::new();
        let init = Type30Record::job_initiation("MYJOB", "J001", "A", 1, 360000);
        let mut init_smf = init.to_record();
        init_smf.header.set_time(1, 0, 0, 0);
        records.push(init_smf);

        let step = Type30Record::step_termination("MYJOB", "J001", "STEP1", "PGM1", 0, 500, 1000);
        let mut step_smf = step.to_record();
        step_smf.header.set_time(1, 30, 0, 0);
        records.push(step_smf);

        let term = Type30Record::job_termination("MYJOB", "J001", "SVC1", 2000, 5000, 0);
        let mut term_smf = term.to_record();
        term_smf.header.set_time(2, 0, 0, 0);
        records.push(term_smf);

        let spans = SmfToOtel::from_type30_lifecycle(&records);
        assert!(spans.len() >= 3);
        assert!(spans[0].operation.contains("MYJOB"));
        assert_eq!(spans[0].parent_span_id, "");
        assert!(!spans[1].parent_span_id.is_empty());
    }

    #[test]
    fn test_smf_to_otel_empty() {
        let spans = SmfToOtel::from_type30_lifecycle(&[]);
        assert!(spans.is_empty());
    }

    #[test]
    fn test_prometheus_to_smf_reverse() {
        let metric = PrometheusMetric::new("app_requests_total", 1500.0, MetricType::Counter)
            .with_label("method", "GET")
            .with_label("status", "200");

        let smf = PrometheusToSmf::to_smf_record(&metric);
        assert_eq!(smf.header.record_type, 200);
        assert!(!smf.data.is_empty());

        // Metric name at 0..32.
        let name = String::from_utf8_lossy(&smf.data[0..32])
            .trim_end()
            .to_string();
        assert_eq!(name, "app_requests_total");

        // Metric value at 32..40 (f64 bits).
        let bits = u64::from_be_bytes([
            smf.data[32], smf.data[33], smf.data[34], smf.data[35],
            smf.data[36], smf.data[37], smf.data[38], smf.data[39],
        ]);
        let val = f64::from_bits(bits);
        assert!((val - 1500.0).abs() < 0.01);
    }

    #[test]
    fn test_otel_span_duration() {
        let span = OtelSpan {
            trace_id: "t1".to_string(),
            span_id: "s1".to_string(),
            parent_span_id: String::new(),
            operation: "test".to_string(),
            start_time_us: 1000,
            end_time_us: 5000,
            attributes: HashMap::new(),
        };
        assert_eq!(span.duration_us(), 4000);
    }

    #[test]
    fn test_metric_type_variants() {
        let c = PrometheusMetric::new("c", 1.0, MetricType::Counter);
        let g = PrometheusMetric::new("g", 2.0, MetricType::Gauge);
        let h = PrometheusMetric::new("h", 3.0, MetricType::Histogram);
        assert!(c.to_prometheus_text().contains("counter"));
        assert!(g.to_prometheus_text().contains("gauge"));
        assert!(h.to_prometheus_text().contains("histogram"));
    }
}
