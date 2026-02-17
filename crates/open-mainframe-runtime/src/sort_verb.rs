//! COBOL SORT and MERGE verb runtime integration.
//!
//! Bridges the COBOL SORT/MERGE verbs to the `open-mainframe-sort` crate's
//! `SortEngine`. Supports two usage patterns:
//!
//! 1. **SORT USING/GIVING** — file-to-file sort with automatic I/O
//! 2. **SORT with INPUT/OUTPUT PROCEDURE** — custom record processing via
//!    RELEASE (feed records) and RETURN (retrieve sorted records)
//!
//! # Example
//!
//! ```no_run
//! use open_mainframe_runtime::sort_verb::{SortVerb, SortKeySpec, SortKeyOrder};
//!
//! // SORT USING/GIVING
//! let keys = vec![
//!     SortKeySpec { offset: 0, length: 10, order: SortKeyOrder::Ascending },
//! ];
//! let sorter = SortVerb::new(keys);
//! let stats = sorter.sort_using_giving("/tmp/input.dat", "/tmp/output.dat").unwrap();
//! assert!(stats.output_records > 0 || stats.input_records == 0);
//! ```

use std::path::Path;

use open_mainframe_sort::{DataType, SortEngine, SortField, SortOrder, SortSpec};

/// Sort key ordering direction.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SortKeyOrder {
    /// ASCENDING KEY
    Ascending,
    /// DESCENDING KEY
    Descending,
}

/// A single key specification from the SORT statement.
///
/// Maps to the COBOL `ON ASCENDING/DESCENDING KEY data-name` clause.
/// The offset and length correspond to the field's position and size
/// within the sort-work record (SD).
#[derive(Debug, Clone)]
pub struct SortKeySpec {
    /// Byte offset within the record (0-based).
    pub offset: usize,
    /// Length of the key field in bytes.
    pub length: usize,
    /// Sort direction.
    pub order: SortKeyOrder,
}

/// Statistics returned after a sort or merge operation.
#[derive(Debug, Default)]
pub struct SortResult {
    /// Number of input records read.
    pub input_records: usize,
    /// Number of output records written.
    pub output_records: usize,
}

/// COBOL SORT verb runtime.
///
/// Supports both USING/GIVING (file-based) and INPUT/OUTPUT PROCEDURE
/// (in-memory RELEASE/RETURN) patterns.
pub struct SortVerb {
    /// Key specifications from the SORT statement.
    keys: Vec<SortKeySpec>,
    /// Records released by INPUT PROCEDURE (for RELEASE verb).
    released_records: Vec<Vec<u8>>,
    /// Sorted records waiting for OUTPUT PROCEDURE (for RETURN verb).
    sorted_records: Vec<Vec<u8>>,
    /// Current position in sorted_records for RETURN.
    return_index: usize,
    /// Whether the sort has been executed (records are available for RETURN).
    sort_executed: bool,
}

impl SortVerb {
    /// Creates a new SORT verb handler with the given key specifications.
    pub fn new(keys: Vec<SortKeySpec>) -> Self {
        Self {
            keys,
            released_records: Vec::new(),
            sorted_records: Vec::new(),
            return_index: 0,
            sort_executed: false,
        }
    }

    /// Builds a `SortSpec` from the key specifications.
    fn build_sort_spec(&self) -> SortSpec {
        let mut spec = SortSpec::new();
        for key in &self.keys {
            let order = match key.order {
                SortKeyOrder::Ascending => SortOrder::Ascending,
                SortKeyOrder::Descending => SortOrder::Descending,
            };
            // SortField uses 1-based position
            spec = spec.add_field(SortField::new(
                key.offset + 1,
                key.length,
                DataType::Character,
                order,
            ));
        }
        spec
    }

    // -----------------------------------------------------------------
    // Story 506.1: SORT USING/GIVING
    // -----------------------------------------------------------------

    /// Execute SORT USING/GIVING — sort records from input file to output file.
    ///
    /// This corresponds to:
    /// ```cobol
    /// SORT SORT-FILE ON ASCENDING KEY field-1
    ///     USING input-file
    ///     GIVING output-file.
    /// ```
    pub fn sort_using_giving<P: AsRef<Path>>(
        &self,
        input: P,
        output: P,
    ) -> Result<SortResult, SortVerbError> {
        let spec = self.build_sort_spec();
        let engine = SortEngine::new(spec);
        let stats = engine
            .sort_file(input, output)
            .map_err(SortVerbError::SortEngine)?;
        Ok(SortResult {
            input_records: stats.input_records,
            output_records: stats.output_records,
        })
    }

    /// Execute MERGE USING/GIVING — merge pre-sorted files into output file.
    ///
    /// This corresponds to:
    /// ```cobol
    /// MERGE MERGE-FILE ON ASCENDING KEY field-1
    ///     USING file-1 file-2
    ///     GIVING output-file.
    /// ```
    pub fn merge_using_giving<P: AsRef<Path>>(
        &self,
        inputs: &[P],
        output: P,
    ) -> Result<SortResult, SortVerbError> {
        if inputs.is_empty() {
            return Err(SortVerbError::NoInputFiles);
        }
        let spec = self.build_sort_spec();
        let engine = SortEngine::new(spec);
        let stats = engine
            .merge_files(inputs, output)
            .map_err(SortVerbError::SortEngine)?;
        Ok(SortResult {
            input_records: stats.input_records,
            output_records: stats.output_records,
        })
    }

    // -----------------------------------------------------------------
    // Story 506.2: SORT with INPUT/OUTPUT PROCEDURE
    // -----------------------------------------------------------------

    /// RELEASE a record to the sort work file (INPUT PROCEDURE).
    ///
    /// Called from an INPUT PROCEDURE section to feed records into the sort.
    /// After all records are released, call [`execute_sort`](Self::execute_sort)
    /// to sort them and make them available via [`return_record`](Self::return_record).
    pub fn release(&mut self, record: Vec<u8>) {
        self.released_records.push(record);
    }

    /// Execute the sort on all released records.
    ///
    /// Must be called after the INPUT PROCEDURE has finished releasing
    /// records and before the OUTPUT PROCEDURE begins retrieving them.
    pub fn execute_sort(&mut self) {
        let spec = self.build_sort_spec();
        self.sorted_records = std::mem::take(&mut self.released_records);
        self.sorted_records
            .sort_by(|a, b| spec.compare(a, b));
        self.return_index = 0;
        self.sort_executed = true;
    }

    /// RETURN a record from the sort work file (OUTPUT PROCEDURE).
    ///
    /// Called from an OUTPUT PROCEDURE section to retrieve sorted records.
    /// Returns `None` when all records have been returned (AT END condition).
    ///
    /// This corresponds to:
    /// ```cobol
    /// RETURN SORT-FILE INTO work-record
    ///     AT END SET end-of-sort TO TRUE.
    /// ```
    pub fn return_record(&mut self) -> Option<Vec<u8>> {
        if !self.sort_executed {
            return None;
        }
        if self.return_index < self.sorted_records.len() {
            let record = self.sorted_records[self.return_index].clone();
            self.return_index += 1;
            Some(record)
        } else {
            None
        }
    }

    /// Returns the number of records released so far (before sort).
    pub fn released_count(&self) -> usize {
        self.released_records.len()
    }

    /// Returns the number of sorted records available (after sort).
    pub fn sorted_count(&self) -> usize {
        self.sorted_records.len()
    }

    /// Returns the number of records already returned via RETURN.
    pub fn returned_count(&self) -> usize {
        self.return_index
    }

    /// Reset the sort verb for reuse.
    pub fn reset(&mut self) {
        self.released_records.clear();
        self.sorted_records.clear();
        self.return_index = 0;
        self.sort_executed = false;
    }
}

/// Errors from SORT/MERGE verb execution.
#[derive(Debug)]
pub enum SortVerbError {
    /// Error from the underlying sort engine.
    SortEngine(open_mainframe_sort::SortError),
    /// No input files provided for MERGE.
    NoInputFiles,
    /// I/O error during file operations.
    IoError(std::io::Error),
}

impl std::fmt::Display for SortVerbError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SortVerbError::SortEngine(e) => write!(f, "Sort engine error: {}", e),
            SortVerbError::NoInputFiles => write!(f, "No input files provided for MERGE"),
            SortVerbError::IoError(e) => write!(f, "I/O error: {}", e),
        }
    }
}

impl std::error::Error for SortVerbError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            SortVerbError::SortEngine(e) => Some(e),
            SortVerbError::IoError(e) => Some(e),
            SortVerbError::NoInputFiles => None,
        }
    }
}

impl From<std::io::Error> for SortVerbError {
    fn from(e: std::io::Error) -> Self {
        SortVerbError::IoError(e)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use std::sync::atomic::{AtomicUsize, Ordering};

    static COUNTER: AtomicUsize = AtomicUsize::new(0);

    fn temp_path(label: &str) -> std::path::PathBuf {
        let n = COUNTER.fetch_add(1, Ordering::SeqCst);
        std::env::temp_dir().join(format!("sort_verb_{}_{}", label, n))
    }

    fn cleanup(p: &Path) {
        let _ = fs::remove_file(p);
    }

    // -----------------------------------------------------------------
    // Story 506.1: SORT USING/GIVING tests
    // -----------------------------------------------------------------

    #[test]
    fn test_sort_using_giving_ascending() {
        let input = temp_path("in");
        let output = temp_path("out");

        fs::write(&input, "Charlie\nAlpha..\nBravo..\n").unwrap();

        let keys = vec![SortKeySpec {
            offset: 0,
            length: 7,
            order: SortKeyOrder::Ascending,
        }];
        let sorter = SortVerb::new(keys);
        let result = sorter.sort_using_giving(&input, &output).unwrap();

        assert_eq!(result.input_records, 3);
        assert_eq!(result.output_records, 3);

        let content = fs::read_to_string(&output).unwrap();
        assert_eq!(content, "Alpha..\nBravo..\nCharlie\n");

        cleanup(&input);
        cleanup(&output);
    }

    #[test]
    fn test_sort_using_giving_descending() {
        let input = temp_path("in_d");
        let output = temp_path("out_d");

        fs::write(&input, "A\nC\nB\n").unwrap();

        let keys = vec![SortKeySpec {
            offset: 0,
            length: 1,
            order: SortKeyOrder::Descending,
        }];
        let sorter = SortVerb::new(keys);
        sorter.sort_using_giving(&input, &output).unwrap();

        let content = fs::read_to_string(&output).unwrap();
        assert_eq!(content, "C\nB\nA\n");

        cleanup(&input);
        cleanup(&output);
    }

    #[test]
    fn test_sort_using_giving_multiple_keys() {
        let input = temp_path("in_mk");
        let output = temp_path("out_mk");

        // Records: 2-char department + 3-char name
        // Sort by dept ASC, then name DESC
        fs::write(&input, "ITBOB\nHRACE\nITANN\nHRDAM\n").unwrap();

        let keys = vec![
            SortKeySpec { offset: 0, length: 2, order: SortKeyOrder::Ascending },
            SortKeySpec { offset: 2, length: 3, order: SortKeyOrder::Descending },
        ];
        let sorter = SortVerb::new(keys);
        sorter.sort_using_giving(&input, &output).unwrap();

        let content = fs::read_to_string(&output).unwrap();
        // HR first (ASC), then within HR: DAM before ACE (DESC)
        // IT second, then within IT: BOB before ANN (DESC)
        assert_eq!(content, "HRDAM\nHRACE\nITBOB\nITANN\n");

        cleanup(&input);
        cleanup(&output);
    }

    #[test]
    fn test_sort_using_giving_empty_file() {
        let input = temp_path("in_e");
        let output = temp_path("out_e");

        fs::write(&input, "").unwrap();

        let keys = vec![SortKeySpec {
            offset: 0,
            length: 5,
            order: SortKeyOrder::Ascending,
        }];
        let sorter = SortVerb::new(keys);
        let result = sorter.sort_using_giving(&input, &output).unwrap();

        assert_eq!(result.input_records, 0);
        assert_eq!(result.output_records, 0);

        cleanup(&input);
        cleanup(&output);
    }

    #[test]
    fn test_sort_using_giving_file_not_found() {
        let keys = vec![SortKeySpec {
            offset: 0,
            length: 5,
            order: SortKeyOrder::Ascending,
        }];
        let sorter = SortVerb::new(keys);
        let result = sorter.sort_using_giving("/nonexistent/path", "/tmp/out");
        assert!(result.is_err());
    }

    #[test]
    fn test_merge_using_giving() {
        let in1 = temp_path("merge_in1");
        let in2 = temp_path("merge_in2");
        let output = temp_path("merge_out");

        // Two pre-sorted files
        fs::write(&in1, "Alpha\nDelta\n").unwrap();
        fs::write(&in2, "Bravo\nCharl\n").unwrap();

        let keys = vec![SortKeySpec {
            offset: 0,
            length: 5,
            order: SortKeyOrder::Ascending,
        }];
        let sorter = SortVerb::new(keys);
        let result = sorter
            .merge_using_giving(&[&in1, &in2], &output)
            .unwrap();

        assert_eq!(result.output_records, 4);

        let content = fs::read_to_string(&output).unwrap();
        assert_eq!(content, "Alpha\nBravo\nCharl\nDelta\n");

        cleanup(&in1);
        cleanup(&in2);
        cleanup(&output);
    }

    #[test]
    fn test_merge_no_inputs() {
        let keys = vec![SortKeySpec {
            offset: 0,
            length: 1,
            order: SortKeyOrder::Ascending,
        }];
        let sorter = SortVerb::new(keys);
        let empty: Vec<&str> = vec![];
        let result = sorter.merge_using_giving(&empty, "/tmp/out");
        assert!(matches!(result, Err(SortVerbError::NoInputFiles)));
    }

    // -----------------------------------------------------------------
    // Story 506.2: INPUT/OUTPUT PROCEDURE tests
    // -----------------------------------------------------------------

    #[test]
    fn test_release_and_return() {
        let keys = vec![SortKeySpec {
            offset: 0,
            length: 5,
            order: SortKeyOrder::Ascending,
        }];
        let mut sorter = SortVerb::new(keys);

        // INPUT PROCEDURE: RELEASE records
        sorter.release(b"Delta".to_vec());
        sorter.release(b"Alpha".to_vec());
        sorter.release(b"Charl".to_vec());
        sorter.release(b"Bravo".to_vec());

        assert_eq!(sorter.released_count(), 4);

        // Execute the sort
        sorter.execute_sort();
        assert_eq!(sorter.sorted_count(), 4);

        // OUTPUT PROCEDURE: RETURN records
        assert_eq!(sorter.return_record(), Some(b"Alpha".to_vec()));
        assert_eq!(sorter.return_record(), Some(b"Bravo".to_vec()));
        assert_eq!(sorter.return_record(), Some(b"Charl".to_vec()));
        assert_eq!(sorter.return_record(), Some(b"Delta".to_vec()));
        // AT END
        assert_eq!(sorter.return_record(), None);

        assert_eq!(sorter.returned_count(), 4);
    }

    #[test]
    fn test_release_descending() {
        let keys = vec![SortKeySpec {
            offset: 0,
            length: 1,
            order: SortKeyOrder::Descending,
        }];
        let mut sorter = SortVerb::new(keys);

        sorter.release(b"A".to_vec());
        sorter.release(b"C".to_vec());
        sorter.release(b"B".to_vec());
        sorter.execute_sort();

        assert_eq!(sorter.return_record(), Some(b"C".to_vec()));
        assert_eq!(sorter.return_record(), Some(b"B".to_vec()));
        assert_eq!(sorter.return_record(), Some(b"A".to_vec()));
        assert_eq!(sorter.return_record(), None);
    }

    #[test]
    fn test_return_before_sort_returns_none() {
        let keys = vec![SortKeySpec {
            offset: 0,
            length: 1,
            order: SortKeyOrder::Ascending,
        }];
        let mut sorter = SortVerb::new(keys);
        sorter.release(b"X".to_vec());

        // Calling RETURN before execute_sort should return None
        assert_eq!(sorter.return_record(), None);
    }

    #[test]
    fn test_release_empty_then_sort() {
        let keys = vec![SortKeySpec {
            offset: 0,
            length: 1,
            order: SortKeyOrder::Ascending,
        }];
        let mut sorter = SortVerb::new(keys);

        sorter.execute_sort();
        assert_eq!(sorter.sorted_count(), 0);
        assert_eq!(sorter.return_record(), None);
    }

    #[test]
    fn test_reset() {
        let keys = vec![SortKeySpec {
            offset: 0,
            length: 1,
            order: SortKeyOrder::Ascending,
        }];
        let mut sorter = SortVerb::new(keys);

        sorter.release(b"B".to_vec());
        sorter.release(b"A".to_vec());
        sorter.execute_sort();
        assert_eq!(sorter.return_record(), Some(b"A".to_vec()));

        sorter.reset();
        assert_eq!(sorter.released_count(), 0);
        assert_eq!(sorter.sorted_count(), 0);
        assert_eq!(sorter.returned_count(), 0);
        assert_eq!(sorter.return_record(), None);
    }

    #[test]
    fn test_input_procedure_filter_pattern() {
        // Simulate a typical INPUT PROCEDURE that filters records:
        // Only RELEASE records where department = "IT"
        let keys = vec![SortKeySpec {
            offset: 2,
            length: 5,
            order: SortKeyOrder::Ascending,
        }];
        let mut sorter = SortVerb::new(keys);

        let all_records = vec![
            b"ITSMITH".to_vec(),
            b"HRJONES".to_vec(),
            b"ITADAMS".to_vec(),
            b"HRBROWN".to_vec(),
        ];

        // INPUT PROCEDURE: filter and RELEASE
        for rec in &all_records {
            if &rec[0..2] == b"IT" {
                sorter.release(rec.clone());
            }
        }

        assert_eq!(sorter.released_count(), 2);

        sorter.execute_sort();

        // OUTPUT PROCEDURE: RETURN sorted IT records
        assert_eq!(sorter.return_record(), Some(b"ITADAMS".to_vec()));
        assert_eq!(sorter.return_record(), Some(b"ITSMITH".to_vec()));
        assert_eq!(sorter.return_record(), None);
    }

    #[test]
    fn test_output_procedure_transform_pattern() {
        // Simulate OUTPUT PROCEDURE that transforms sorted records
        let keys = vec![SortKeySpec {
            offset: 0,
            length: 3,
            order: SortKeyOrder::Ascending,
        }];
        let mut sorter = SortVerb::new(keys);

        sorter.release(b"CCC".to_vec());
        sorter.release(b"AAA".to_vec());
        sorter.release(b"BBB".to_vec());
        sorter.execute_sort();

        // OUTPUT PROCEDURE: collect and transform
        let mut results = Vec::new();
        while let Some(rec) = sorter.return_record() {
            // Transform: prepend record number
            let transformed = format!("{:02}-{}", results.len() + 1, String::from_utf8_lossy(&rec));
            results.push(transformed);
        }

        assert_eq!(results, vec!["01-AAA", "02-BBB", "03-CCC"]);
    }

    #[test]
    fn test_sort_verb_error_display() {
        let err = SortVerbError::NoInputFiles;
        assert_eq!(format!("{}", err), "No input files provided for MERGE");

        let io_err = SortVerbError::IoError(std::io::Error::new(
            std::io::ErrorKind::NotFound,
            "file not found",
        ));
        assert!(format!("{}", io_err).contains("I/O error"));
    }

    #[test]
    fn test_sort_key_order() {
        assert_eq!(SortKeyOrder::Ascending, SortKeyOrder::Ascending);
        assert_ne!(SortKeyOrder::Ascending, SortKeyOrder::Descending);
    }

    #[test]
    fn test_sort_result_default() {
        let result = SortResult::default();
        assert_eq!(result.input_records, 0);
        assert_eq!(result.output_records, 0);
    }
}
