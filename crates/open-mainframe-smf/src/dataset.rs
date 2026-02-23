//! Types 14/15/17/18 — Dataset Activity Records.
//!
//! Tracks dataset lifecycle events:
//! - Type 14: Dataset input (read / close after input)
//! - Type 15: Dataset output (write / close after output)
//! - Type 17: Dataset scratch (delete)
//! - Type 18: Dataset rename

use crate::record::{extend_padded, push_u32, push_u64, SmfRecord};

// ---------------------------------------------------------------------------
//  Dataset activity type
// ---------------------------------------------------------------------------

/// The type of dataset activity.
#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum DatasetActivityType {
    /// Type 14: Input (read).
    Input,
    /// Type 15: Output (write).
    Output,
    /// Type 17: Scratch (delete).
    Scratch,
    /// Type 18: Rename.
    Rename,
}

impl DatasetActivityType {
    /// Get the SMF record type code.
    pub fn record_type(&self) -> u8 {
        match self {
            DatasetActivityType::Input => 14,
            DatasetActivityType::Output => 15,
            DatasetActivityType::Scratch => 17,
            DatasetActivityType::Rename => 18,
        }
    }

    /// Construct from record type code.
    pub fn from_record_type(code: u8) -> Option<Self> {
        match code {
            14 => Some(DatasetActivityType::Input),
            15 => Some(DatasetActivityType::Output),
            17 => Some(DatasetActivityType::Scratch),
            18 => Some(DatasetActivityType::Rename),
            _ => None,
        }
    }
}

// ---------------------------------------------------------------------------
//  Type 14/15 — Dataset I/O records
// ---------------------------------------------------------------------------

/// Type 14/15 record — dataset input/output activity.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct DatasetIoRecord {
    /// Activity type (Input or Output).
    pub activity_type: DatasetActivityType,
    /// Dataset name (up to 44 characters).
    pub dataset_name: String,
    /// Volume serial (6 characters).
    pub volser: String,
    /// Job name.
    pub job_name: String,
    /// Step name.
    pub step_name: String,
    /// EXCP count (I/O operations).
    pub excp_count: u32,
    /// Device type.
    pub device_type: String,
    /// Bytes read or written.
    pub byte_count: u64,
}

impl Default for DatasetIoRecord {
    fn default() -> Self {
        Self {
            activity_type: DatasetActivityType::Input,
            dataset_name: String::new(),
            volser: String::new(),
            job_name: String::new(),
            step_name: String::new(),
            excp_count: 0,
            device_type: String::new(),
            byte_count: 0,
        }
    }
}

impl DatasetIoRecord {
    /// Create a Type 14 (input) record.
    pub fn input(dsn: &str, volser: &str, job_name: &str, excp_count: u32) -> Self {
        Self {
            activity_type: DatasetActivityType::Input,
            dataset_name: dsn.to_string(),
            volser: volser.to_string(),
            job_name: job_name.to_string(),
            excp_count,
            ..Default::default()
        }
    }

    /// Create a Type 15 (output) record.
    pub fn output(dsn: &str, volser: &str, job_name: &str, excp_count: u32) -> Self {
        Self {
            activity_type: DatasetActivityType::Output,
            dataset_name: dsn.to_string(),
            volser: volser.to_string(),
            job_name: job_name.to_string(),
            excp_count,
            ..Default::default()
        }
    }

    /// Convert to a generic SMF record.
    pub fn to_record(&self) -> SmfRecord {
        let mut data = Vec::new();
        extend_padded(&mut data, &self.dataset_name, 44);
        extend_padded(&mut data, &self.volser, 6);
        extend_padded(&mut data, &self.job_name, 8);
        extend_padded(&mut data, &self.step_name, 8);
        push_u32(&mut data, self.excp_count);
        extend_padded(&mut data, &self.device_type, 8);
        push_u64(&mut data, self.byte_count);

        SmfRecord::new(self.activity_type.record_type(), data)
    }
}

// ---------------------------------------------------------------------------
//  Type 17 — Dataset Scratch (Delete)
// ---------------------------------------------------------------------------

/// Type 17 record — dataset scratch (deletion).
#[derive(Debug, Clone, Default, serde::Serialize, serde::Deserialize)]
pub struct DatasetScratchRecord {
    /// Dataset name.
    pub dataset_name: String,
    /// Volume serial.
    pub volser: String,
    /// Job name that performed the delete.
    pub job_name: String,
    /// Deletion time (hundredths of seconds since midnight).
    pub deletion_time: u32,
}

impl DatasetScratchRecord {
    /// Create a new scratch record.
    pub fn new(dsn: &str, volser: &str, job_name: &str, deletion_time: u32) -> Self {
        Self {
            dataset_name: dsn.to_string(),
            volser: volser.to_string(),
            job_name: job_name.to_string(),
            deletion_time,
        }
    }

    /// Convert to a generic SMF record.
    pub fn to_record(&self) -> SmfRecord {
        let mut data = Vec::new();
        extend_padded(&mut data, &self.dataset_name, 44);
        extend_padded(&mut data, &self.volser, 6);
        extend_padded(&mut data, &self.job_name, 8);
        push_u32(&mut data, self.deletion_time);

        SmfRecord::new(17, data)
    }
}

// ---------------------------------------------------------------------------
//  Type 18 — Dataset Rename
// ---------------------------------------------------------------------------

/// Type 18 record — dataset rename.
#[derive(Debug, Clone, Default, serde::Serialize, serde::Deserialize)]
pub struct DatasetRenameRecord {
    /// Old dataset name.
    pub old_name: String,
    /// New dataset name.
    pub new_name: String,
    /// Volume serial.
    pub volser: String,
    /// Job name that performed the rename.
    pub job_name: String,
}

impl DatasetRenameRecord {
    /// Create a new rename record.
    pub fn new(old_name: &str, new_name: &str, volser: &str, job_name: &str) -> Self {
        Self {
            old_name: old_name.to_string(),
            new_name: new_name.to_string(),
            volser: volser.to_string(),
            job_name: job_name.to_string(),
        }
    }

    /// Convert to a generic SMF record.
    pub fn to_record(&self) -> SmfRecord {
        let mut data = Vec::new();
        extend_padded(&mut data, &self.old_name, 44);
        extend_padded(&mut data, &self.new_name, 44);
        extend_padded(&mut data, &self.volser, 6);
        extend_padded(&mut data, &self.job_name, 8);

        SmfRecord::new(18, data)
    }
}

// ---------------------------------------------------------------------------
//  Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_activity_type_codes() {
        assert_eq!(DatasetActivityType::Input.record_type(), 14);
        assert_eq!(DatasetActivityType::Output.record_type(), 15);
        assert_eq!(DatasetActivityType::Scratch.record_type(), 17);
        assert_eq!(DatasetActivityType::Rename.record_type(), 18);
    }

    #[test]
    fn test_activity_type_from_code() {
        assert_eq!(
            DatasetActivityType::from_record_type(14),
            Some(DatasetActivityType::Input)
        );
        assert_eq!(
            DatasetActivityType::from_record_type(15),
            Some(DatasetActivityType::Output)
        );
        assert_eq!(DatasetActivityType::from_record_type(30), None);
    }

    #[test]
    fn test_type14_input_record() {
        let rec = DatasetIoRecord::input("MY.DATA", "VOL001", "MYJOB", 150);
        assert_eq!(rec.activity_type, DatasetActivityType::Input);
        assert_eq!(rec.dataset_name, "MY.DATA");
        assert_eq!(rec.volser, "VOL001");
        assert_eq!(rec.excp_count, 150);

        let smf = rec.to_record();
        assert_eq!(smf.header.record_type, 14);
    }

    #[test]
    fn test_type15_output_record() {
        let rec = DatasetIoRecord::output("MY.DATA", "VOL001", "MYJOB", 200);
        assert_eq!(rec.activity_type, DatasetActivityType::Output);

        let smf = rec.to_record();
        assert_eq!(smf.header.record_type, 15);
    }

    #[test]
    fn test_type17_scratch_record() {
        let rec = DatasetScratchRecord::new("MY.DATA", "VOL001", "MYJOB", 360000);
        assert_eq!(rec.dataset_name, "MY.DATA");
        assert_eq!(rec.deletion_time, 360000);

        let smf = rec.to_record();
        assert_eq!(smf.header.record_type, 17);
        assert!(!smf.data.is_empty());
    }

    #[test]
    fn test_type18_rename_record() {
        let rec = DatasetRenameRecord::new("MY.DATA", "MY.NEWDATA", "VOL001", "MYJOB");
        assert_eq!(rec.old_name, "MY.DATA");
        assert_eq!(rec.new_name, "MY.NEWDATA");

        let smf = rec.to_record();
        assert_eq!(smf.header.record_type, 18);
        assert!(!smf.data.is_empty());
    }

    #[test]
    fn test_dataset_io_with_byte_count() {
        let mut rec = DatasetIoRecord::input("BIG.FILE", "VOL002", "LOADJOB", 5000);
        rec.byte_count = 1_000_000;
        let smf = rec.to_record();
        assert_eq!(smf.header.record_type, 14);
        // Verify data includes byte count at the end.
        assert!(smf.data.len() > 74); // 44+6+8+8+4+8+8 = 86
    }

    #[test]
    fn test_rename_old_new_in_record() {
        let rec = DatasetRenameRecord::new("OLD.DSN", "NEW.DSN", "VOL001", "RENAMEJB");
        let smf = rec.to_record();
        // Old name at offset 0..44, new name at 44..88.
        let old = String::from_utf8_lossy(&smf.data[0..44])
            .trim_end()
            .to_string();
        let new = String::from_utf8_lossy(&smf.data[44..88])
            .trim_end()
            .to_string();
        assert_eq!(old, "OLD.DSN");
        assert_eq!(new, "NEW.DSN");
    }

    #[test]
    fn test_scratch_deletion_time() {
        let rec = DatasetScratchRecord::new("TEMP.DATA", "TMP001", "CLEANUP", 5220000);
        let smf = rec.to_record();
        // Deletion time at offset 44+6+8=58, 4 bytes.
        let t = u32::from_be_bytes([smf.data[58], smf.data[59], smf.data[60], smf.data[61]]);
        assert_eq!(t, 5220000);
    }
}
