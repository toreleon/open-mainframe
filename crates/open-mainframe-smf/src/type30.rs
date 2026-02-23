//! Type 30 — Job Accounting Records.
//!
//! Enhanced Type 30 record support with explicit subtypes for the complete
//! job lifecycle:
//! - Subtype 1: Job initiation
//! - Subtype 2: Interval record (periodic during execution)
//! - Subtype 3: Step termination
//! - Subtype 4: Job termination
//! - Subtype 5: Redirected output
//!
//! Includes WLM service class integration and service unit tracking.

use crate::record::{extend_padded, push_u16, push_u32, push_u64, SmfRecord};

// ---------------------------------------------------------------------------
//  Subtype enum
// ---------------------------------------------------------------------------

/// Type 30 subtypes for the job lifecycle.
#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum Type30Subtype {
    /// Subtype 1: Job initiation.
    JobInitiation,
    /// Subtype 2: Interval record.
    IntervalRecord,
    /// Subtype 3: Step termination.
    StepTermination,
    /// Subtype 4: Job termination.
    JobTermination,
    /// Subtype 5: Redirected output.
    RedirectedOutput,
}

impl Type30Subtype {
    /// Get the numeric subtype code.
    pub fn code(&self) -> u16 {
        match self {
            Type30Subtype::JobInitiation => 1,
            Type30Subtype::IntervalRecord => 2,
            Type30Subtype::StepTermination => 3,
            Type30Subtype::JobTermination => 4,
            Type30Subtype::RedirectedOutput => 5,
        }
    }

    /// Construct from numeric code.
    pub fn from_code(code: u16) -> Option<Self> {
        match code {
            1 => Some(Type30Subtype::JobInitiation),
            2 => Some(Type30Subtype::IntervalRecord),
            3 => Some(Type30Subtype::StepTermination),
            4 => Some(Type30Subtype::JobTermination),
            5 => Some(Type30Subtype::RedirectedOutput),
            _ => None,
        }
    }
}

// ---------------------------------------------------------------------------
//  Type 30 Record
// ---------------------------------------------------------------------------

/// Enhanced Type 30 record — job accounting.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct Type30Record {
    /// Subtype.
    pub subtype: Type30Subtype,
    /// Job name (8 chars).
    pub job_name: String,
    /// Job ID (8 chars).
    pub job_id: String,
    /// Step name (8 chars).
    pub step_name: String,
    /// Program name (8 chars).
    pub program_name: String,
    /// User ID (8 chars).
    pub user_id: String,
    /// Job class (1 char).
    pub job_class: String,
    /// Job priority.
    pub priority: u8,
    /// WLM service class (8 chars).
    pub service_class: String,
    /// CPU time in microseconds.
    pub cpu_time_us: u64,
    /// SRB time in microseconds.
    pub srb_time_us: u64,
    /// Elapsed time in microseconds.
    pub elapsed_time_us: u64,
    /// EXCP count (I/O operations).
    pub excp_count: u32,
    /// Service units consumed.
    pub service_units: u64,
    /// Completion code (return code or abend).
    pub completion_code: u16,
    /// Start time (hundredths of seconds since midnight).
    pub start_time: u32,
    /// End time (hundredths of seconds since midnight).
    pub end_time: u32,
}

impl Default for Type30Record {
    fn default() -> Self {
        Self {
            subtype: Type30Subtype::JobTermination,
            job_name: String::new(),
            job_id: String::new(),
            step_name: String::new(),
            program_name: String::new(),
            user_id: String::new(),
            job_class: String::new(),
            priority: 0,
            service_class: String::new(),
            cpu_time_us: 0,
            srb_time_us: 0,
            elapsed_time_us: 0,
            excp_count: 0,
            service_units: 0,
            completion_code: 0,
            start_time: 0,
            end_time: 0,
        }
    }
}

impl Type30Record {
    /// Create a job initiation record (subtype 1).
    pub fn job_initiation(
        job_name: &str,
        job_id: &str,
        job_class: &str,
        priority: u8,
        start_time: u32,
    ) -> Self {
        Self {
            subtype: Type30Subtype::JobInitiation,
            job_name: job_name.to_string(),
            job_id: job_id.to_string(),
            job_class: job_class.to_string(),
            priority,
            start_time,
            ..Default::default()
        }
    }

    /// Create an interval record (subtype 2).
    pub fn interval(
        job_name: &str,
        job_id: &str,
        cpu_time_us: u64,
        excp_count: u32,
        service_units: u64,
    ) -> Self {
        Self {
            subtype: Type30Subtype::IntervalRecord,
            job_name: job_name.to_string(),
            job_id: job_id.to_string(),
            cpu_time_us,
            excp_count,
            service_units,
            ..Default::default()
        }
    }

    /// Create a step termination record (subtype 3).
    pub fn step_termination(
        job_name: &str,
        job_id: &str,
        step_name: &str,
        program_name: &str,
        completion_code: u16,
        cpu_time_us: u64,
        elapsed_time_us: u64,
    ) -> Self {
        Self {
            subtype: Type30Subtype::StepTermination,
            job_name: job_name.to_string(),
            job_id: job_id.to_string(),
            step_name: step_name.to_string(),
            program_name: program_name.to_string(),
            completion_code,
            cpu_time_us,
            elapsed_time_us,
            ..Default::default()
        }
    }

    /// Create a job termination record (subtype 4).
    pub fn job_termination(
        job_name: &str,
        job_id: &str,
        service_class: &str,
        cpu_time_us: u64,
        elapsed_time_us: u64,
        completion_code: u16,
    ) -> Self {
        Self {
            subtype: Type30Subtype::JobTermination,
            job_name: job_name.to_string(),
            job_id: job_id.to_string(),
            service_class: service_class.to_string(),
            cpu_time_us,
            elapsed_time_us,
            completion_code,
            ..Default::default()
        }
    }

    /// Convert to a generic SMF record.
    pub fn to_record(&self) -> SmfRecord {
        let mut data = Vec::new();

        push_u16(&mut data, self.subtype.code());
        extend_padded(&mut data, &self.job_name, 8);
        extend_padded(&mut data, &self.job_id, 8);
        extend_padded(&mut data, &self.step_name, 8);
        extend_padded(&mut data, &self.program_name, 8);
        extend_padded(&mut data, &self.user_id, 8);
        extend_padded(&mut data, &self.job_class, 1);
        data.push(self.priority);
        extend_padded(&mut data, &self.service_class, 8);
        push_u64(&mut data, self.cpu_time_us);
        push_u64(&mut data, self.srb_time_us);
        push_u64(&mut data, self.elapsed_time_us);
        push_u32(&mut data, self.excp_count);
        push_u64(&mut data, self.service_units);
        push_u16(&mut data, self.completion_code);
        push_u32(&mut data, self.start_time);
        push_u32(&mut data, self.end_time);

        let mut record = SmfRecord::new(30, data);
        record.header.subtype = self.subtype.code();
        record
    }
}

// ---------------------------------------------------------------------------
//  Job lifecycle collector
// ---------------------------------------------------------------------------

/// Collects Type 30 records across the job lifecycle.
#[derive(Debug, Default)]
pub struct JobLifecycleCollector {
    records: Vec<Type30Record>,
}

impl JobLifecycleCollector {
    /// Create a new collector.
    pub fn new() -> Self {
        Self {
            records: Vec::new(),
        }
    }

    /// Record job initiation.
    pub fn record_initiation(&mut self, record: Type30Record) {
        debug_assert_eq!(record.subtype, Type30Subtype::JobInitiation);
        self.records.push(record);
    }

    /// Record an interval.
    pub fn record_interval(&mut self, record: Type30Record) {
        debug_assert_eq!(record.subtype, Type30Subtype::IntervalRecord);
        self.records.push(record);
    }

    /// Record step termination.
    pub fn record_step(&mut self, record: Type30Record) {
        debug_assert_eq!(record.subtype, Type30Subtype::StepTermination);
        self.records.push(record);
    }

    /// Record job termination.
    pub fn record_termination(&mut self, record: Type30Record) {
        debug_assert_eq!(record.subtype, Type30Subtype::JobTermination);
        self.records.push(record);
    }

    /// Get all collected records.
    pub fn records(&self) -> &[Type30Record] {
        &self.records
    }

    /// Convert all records to generic SMF records.
    pub fn to_smf_records(&self) -> Vec<SmfRecord> {
        self.records.iter().map(|r| r.to_record()).collect()
    }

    /// Check if the lifecycle is complete (has initiation and termination).
    pub fn is_complete(&self) -> bool {
        let has_init = self
            .records
            .iter()
            .any(|r| r.subtype == Type30Subtype::JobInitiation);
        let has_term = self
            .records
            .iter()
            .any(|r| r.subtype == Type30Subtype::JobTermination);
        has_init && has_term
    }
}

// ---------------------------------------------------------------------------
//  Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_subtype_codes() {
        assert_eq!(Type30Subtype::JobInitiation.code(), 1);
        assert_eq!(Type30Subtype::IntervalRecord.code(), 2);
        assert_eq!(Type30Subtype::StepTermination.code(), 3);
        assert_eq!(Type30Subtype::JobTermination.code(), 4);
        assert_eq!(Type30Subtype::RedirectedOutput.code(), 5);
    }

    #[test]
    fn test_subtype_from_code() {
        assert_eq!(Type30Subtype::from_code(1), Some(Type30Subtype::JobInitiation));
        assert_eq!(Type30Subtype::from_code(4), Some(Type30Subtype::JobTermination));
        assert_eq!(Type30Subtype::from_code(99), None);
    }

    #[test]
    fn test_job_initiation() {
        let rec = Type30Record::job_initiation("PAYROLL", "JOB00001", "A", 5, 360000);
        assert_eq!(rec.subtype, Type30Subtype::JobInitiation);
        assert_eq!(rec.job_name, "PAYROLL");
        assert_eq!(rec.priority, 5);
        assert_eq!(rec.start_time, 360000);
    }

    #[test]
    fn test_interval_record() {
        let rec = Type30Record::interval("PAYROLL", "JOB00001", 500_000, 150, 10000);
        assert_eq!(rec.subtype, Type30Subtype::IntervalRecord);
        assert_eq!(rec.cpu_time_us, 500_000);
        assert_eq!(rec.excp_count, 150);
        assert_eq!(rec.service_units, 10000);
    }

    #[test]
    fn test_step_termination() {
        let rec = Type30Record::step_termination(
            "PAYROLL",
            "JOB00001",
            "STEP01",
            "IEFBR14",
            0,
            100_000,
            200_000,
        );
        assert_eq!(rec.subtype, Type30Subtype::StepTermination);
        assert_eq!(rec.step_name, "STEP01");
        assert_eq!(rec.program_name, "IEFBR14");
        assert_eq!(rec.completion_code, 0);
    }

    #[test]
    fn test_job_termination() {
        let rec = Type30Record::job_termination(
            "PAYROLL",
            "JOB00001",
            "BATCH_HI",
            1_000_000,
            5_000_000,
            0,
        );
        assert_eq!(rec.subtype, Type30Subtype::JobTermination);
        assert_eq!(rec.service_class, "BATCH_HI");
    }

    #[test]
    fn test_to_record() {
        let rec = Type30Record::job_initiation("MYJOB", "JOB00100", "A", 3, 0);
        let smf = rec.to_record();
        assert_eq!(smf.header.record_type, 30);
        assert_eq!(smf.header.subtype, 1);
        assert!(!smf.data.is_empty());
    }

    #[test]
    fn test_wlm_service_class_in_record() {
        let rec = Type30Record::job_termination(
            "PAYROLL",
            "JOB00001",
            "BATCH_HI",
            0,
            0,
            0,
        );
        let smf = rec.to_record();
        // Service class is at offset: 2+8+8+8+8+8+1+1 = 44, length 8.
        let sc = String::from_utf8_lossy(&smf.data[44..52])
            .trim_end()
            .to_string();
        assert_eq!(sc, "BATCH_HI");
    }

    #[test]
    fn test_lifecycle_collector() {
        let mut collector = JobLifecycleCollector::new();

        collector.record_initiation(Type30Record::job_initiation(
            "MYJOB", "J001", "A", 1, 0,
        ));
        assert!(!collector.is_complete());

        collector.record_interval(Type30Record::interval("MYJOB", "J001", 1000, 10, 100));

        collector.record_step(Type30Record::step_termination(
            "MYJOB", "J001", "STEP1", "PGM1", 0, 500, 1000,
        ));

        collector.record_termination(Type30Record::job_termination(
            "MYJOB", "J001", "PRODBTCH", 2000, 5000, 0,
        ));
        assert!(collector.is_complete());

        let records = collector.records();
        assert_eq!(records.len(), 4);
        assert_eq!(records[0].subtype, Type30Subtype::JobInitiation);
        assert_eq!(records[1].subtype, Type30Subtype::IntervalRecord);
        assert_eq!(records[2].subtype, Type30Subtype::StepTermination);
        assert_eq!(records[3].subtype, Type30Subtype::JobTermination);
    }

    #[test]
    fn test_lifecycle_to_smf_records() {
        let mut collector = JobLifecycleCollector::new();
        collector.record_initiation(Type30Record::job_initiation(
            "JOB1", "J001", "A", 1, 0,
        ));
        collector.record_termination(Type30Record::job_termination(
            "JOB1", "J001", "SVC1", 0, 0, 0,
        ));
        let smf_records = collector.to_smf_records();
        assert_eq!(smf_records.len(), 2);
        assert_eq!(smf_records[0].header.subtype, 1);
        assert_eq!(smf_records[1].header.subtype, 4);
    }
}
