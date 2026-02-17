//! Sort engine implementation.
//!
//! Supports both in-memory and disk-based external sort.
//! When the input exceeds `max_memory_records`, the engine automatically
//! switches to external sort: divide input into sorted runs written as
//! temporary files, then k-way merge them using a min-heap.

use std::cmp::Ordering;
use std::collections::BinaryHeap;
use std::fs::{self, File, OpenOptions};
use std::io::{BufRead, BufReader, BufWriter, Read, Write};
use std::path::{Path, PathBuf};

use crate::error::SortError;
use crate::fields::SortSpec;
use crate::filter::FilterSpec;
use crate::reformat::OutrecSpec;

/// Default maximum records to hold in memory for sorting.
const DEFAULT_MAX_MEMORY_RECORDS: usize = 100_000;

/// Sort engine for processing records.
pub struct SortEngine {
    /// Sort specification.
    sort_spec: Option<SortSpec>,
    /// INCLUDE filter.
    include: Option<FilterSpec>,
    /// OMIT filter.
    omit: Option<FilterSpec>,
    /// INREC reformatting.
    inrec: Option<OutrecSpec>,
    /// OUTREC reformatting.
    outrec: Option<OutrecSpec>,
    /// SUM fields for duplicate handling.
    sum_fields: Option<Vec<(usize, usize, crate::fields::DataType)>>,
    /// Copy mode (no sorting).
    copy_mode: bool,
    /// Fixed record length (0 = variable/line-based).
    record_length: usize,
    /// Maximum records to hold in memory before switching to external sort.
    max_memory_records: usize,
}

impl SortEngine {
    /// Creates a new sort engine with a sort specification.
    pub fn new(sort_spec: SortSpec) -> Self {
        Self {
            sort_spec: Some(sort_spec),
            include: None,
            omit: None,
            inrec: None,
            outrec: None,
            sum_fields: None,
            copy_mode: false,
            record_length: 0,
            max_memory_records: DEFAULT_MAX_MEMORY_RECORDS,
        }
    }

    /// Creates a sort engine for copy mode (no sorting).
    pub fn copy() -> Self {
        Self {
            sort_spec: None,
            include: None,
            omit: None,
            inrec: None,
            outrec: None,
            sum_fields: None,
            copy_mode: true,
            record_length: 0,
            max_memory_records: DEFAULT_MAX_MEMORY_RECORDS,
        }
    }

    /// Sets the INCLUDE filter.
    pub fn with_include(mut self, filter: FilterSpec) -> Self {
        self.include = Some(filter);
        self
    }

    /// Sets the OMIT filter.
    pub fn with_omit(mut self, filter: FilterSpec) -> Self {
        self.omit = Some(filter);
        self
    }

    /// Sets INREC reformatting.
    pub fn with_inrec(mut self, spec: OutrecSpec) -> Self {
        self.inrec = Some(spec);
        self
    }

    /// Sets OUTREC reformatting.
    pub fn with_outrec(mut self, spec: OutrecSpec) -> Self {
        self.outrec = Some(spec);
        self
    }

    /// Sets SUM fields.
    pub fn with_sum(mut self, fields: Vec<(usize, usize, crate::fields::DataType)>) -> Self {
        self.sum_fields = Some(fields);
        self
    }

    /// Sets fixed record length.
    pub fn with_record_length(mut self, length: usize) -> Self {
        self.record_length = length;
        self
    }

    /// Sets the maximum number of records to hold in memory.
    ///
    /// When the input exceeds this limit, the engine automatically
    /// switches to disk-based external sort with k-way merge.
    pub fn with_max_memory_records(mut self, max: usize) -> Self {
        self.max_memory_records = max;
        self
    }

    /// Sorts a file.
    ///
    /// Automatically uses in-memory sort for small datasets and
    /// disk-based external sort for datasets exceeding `max_memory_records`.
    pub fn sort_file<P: AsRef<Path>>(&self, input: P, output: P) -> Result<SortStats, SortError> {
        let input_path = input.as_ref();
        let output_path = output.as_ref();

        // Read and filter records in a streaming fashion for external sort.
        // First, determine total count by reading all records.
        let mut records = self.read_records(input_path)?;
        let input_count = records.len();

        // Apply filters
        if let Some(ref filter) = self.include {
            records.retain(|r| filter.should_include(r));
        }
        if let Some(ref filter) = self.omit {
            records.retain(|r| filter.should_include(r));
        }
        let after_filter = records.len();

        // Apply INREC
        if let Some(ref inrec) = self.inrec {
            records = records.into_iter().map(|r| inrec.reformat(&r)).collect();
        }

        // Decide: in-memory vs external sort
        let use_external = !self.copy_mode
            && self.sort_spec.is_some()
            && after_filter > self.max_memory_records;

        if use_external {
            // External sort: sorted runs + k-way merge
            let result = self.external_sort(records, output_path)?;
            return Ok(SortStats {
                input_records: input_count,
                filtered_records: input_count - after_filter,
                summed_records: result.summed,
                output_records: result.output,
            });
        }

        // In-memory sort path (original behavior)
        if !self.copy_mode {
            if let Some(ref spec) = self.sort_spec {
                records.sort_by(|a, b| spec.compare(a, b));
            }
        }

        // Apply SUM for duplicates
        if let Some(ref sum_fields) = self.sum_fields {
            records = self.apply_sum(&records, sum_fields);
        }
        let after_sum = records.len();

        // Apply OUTREC
        if let Some(ref outrec) = self.outrec {
            records = records.into_iter().map(|r| outrec.reformat(&r)).collect();
        }

        // Write output
        self.write_records(output_path, &records)?;

        Ok(SortStats {
            input_records: input_count,
            filtered_records: input_count - after_filter,
            summed_records: after_filter - after_sum,
            output_records: after_sum,
        })
    }

    /// Reads records from a file.
    fn read_records(&self, path: &Path) -> Result<Vec<Vec<u8>>, SortError> {
        let file = File::open(path)?;
        let mut records = Vec::new();

        if self.record_length > 0 {
            // Fixed-length records
            let mut reader = BufReader::new(file);
            let mut buffer = vec![0u8; self.record_length];

            loop {
                match reader.read_exact(&mut buffer) {
                    Ok(()) => records.push(buffer.clone()),
                    Err(e) if e.kind() == std::io::ErrorKind::UnexpectedEof => break,
                    Err(e) => return Err(e.into()),
                }
            }
        } else {
            // Line-based records
            let reader = BufReader::new(file);
            for line in reader.lines() {
                records.push(line?.into_bytes());
            }
        }

        Ok(records)
    }

    /// Writes records to a file.
    fn write_records(&self, path: &Path, records: &[Vec<u8>]) -> Result<(), SortError> {
        let file = OpenOptions::new()
            .write(true)
            .create(true)
            .truncate(true)
            .open(path)?;
        let mut writer = BufWriter::new(file);

        if self.record_length > 0 {
            // Fixed-length records
            for record in records {
                writer.write_all(record)?;
            }
        } else {
            // Line-based records
            for record in records {
                writer.write_all(record)?;
                writer.write_all(b"\n")?;
            }
        }

        writer.flush()?;
        Ok(())
    }

    /// Apply SUM processing for duplicate keys.
    fn apply_sum(
        &self,
        records: &[Vec<u8>],
        _sum_fields: &[(usize, usize, crate::fields::DataType)],
    ) -> Vec<Vec<u8>> {
        if records.is_empty() {
            return Vec::new();
        }

        let sort_spec = match &self.sort_spec {
            Some(spec) => spec,
            None => return records.to_vec(),
        };

        let mut result = Vec::new();
        let mut current_key_record: Option<Vec<u8>> = None;

        for record in records {
            match &current_key_record {
                None => {
                    current_key_record = Some(record.clone());
                }
                Some(prev) => {
                    if sort_spec.compare(prev, record) == std::cmp::Ordering::Equal {
                        // Duplicate key - for now, just keep first (FIELDS=NONE behavior)
                        // TODO: Implement actual summation for numeric fields
                    } else {
                        result.push(prev.clone());
                        current_key_record = Some(record.clone());
                    }
                }
            }
        }

        if let Some(last) = current_key_record {
            result.push(last);
        }

        result
    }

    // -----------------------------------------------------------------------
    // External Sort (Epic 800)
    // -----------------------------------------------------------------------

    /// Perform disk-based external sort: divide into sorted runs, then k-way merge.
    fn external_sort(
        &self,
        records: Vec<Vec<u8>>,
        output_path: &Path,
    ) -> Result<ExternalSortResult, SortError> {
        let sort_spec = self.sort_spec.as_ref().unwrap();
        let total_records = records.len();

        // Phase 1: Generate sorted runs
        let mut run_files = TempRunFiles::new();
        let mut chunk_start = 0;

        while chunk_start < total_records {
            let chunk_end = (chunk_start + self.max_memory_records).min(total_records);
            let mut chunk: Vec<Vec<u8>> = records[chunk_start..chunk_end].to_vec();
            chunk.sort_by(|a, b| sort_spec.compare(a, b));

            let run_path = run_files.create_run()?;
            self.write_run(&run_path, &chunk)?;

            chunk_start = chunk_end;
        }

        // Phase 2: K-way merge with min-heap
        let merged_count = self.merge_runs_to_output(
            &run_files.paths,
            output_path,
            sort_spec,
        )?;

        // run_files is dropped here, cleaning up temp files via Drop

        Ok(ExternalSortResult {
            summed: 0,
            output: merged_count,
        })
    }

    /// Write a sorted chunk (run) to a temporary file.
    fn write_run(&self, path: &Path, records: &[Vec<u8>]) -> Result<(), SortError> {
        let file = File::create(path)?;
        let mut writer = BufWriter::new(file);

        for record in records {
            // Write length-prefixed records for reliable deserialization
            let len = record.len() as u32;
            writer.write_all(&len.to_le_bytes())?;
            writer.write_all(record)?;
        }

        writer.flush()?;
        Ok(())
    }

    /// Read one length-prefixed record from a reader.
    fn read_run_record<R: Read>(reader: &mut R) -> Result<Option<Vec<u8>>, SortError> {
        let mut len_buf = [0u8; 4];
        match reader.read_exact(&mut len_buf) {
            Ok(()) => {}
            Err(e) if e.kind() == std::io::ErrorKind::UnexpectedEof => return Ok(None),
            Err(e) => return Err(e.into()),
        }

        let len = u32::from_le_bytes(len_buf) as usize;
        let mut record = vec![0u8; len];
        reader.read_exact(&mut record)?;
        Ok(Some(record))
    }

    /// K-way merge sorted run files into the final output using a min-heap.
    fn merge_runs_to_output(
        &self,
        run_paths: &[PathBuf],
        output_path: &Path,
        sort_spec: &SortSpec,
    ) -> Result<usize, SortError> {
        let mut readers: Vec<BufReader<File>> = run_paths
            .iter()
            .map(|p| Ok(BufReader::new(File::open(p)?)))
            .collect::<Result<Vec<_>, SortError>>()?;

        // Initialize the min-heap with one record from each run
        let mut heap: BinaryHeap<HeapEntry> = BinaryHeap::new();

        for (i, reader) in readers.iter_mut().enumerate() {
            if let Some(record) = Self::read_run_record(reader)? {
                heap.push(HeapEntry {
                    record,
                    run_index: i,
                    sort_spec,
                });
            }
        }

        let output_file = OpenOptions::new()
            .write(true)
            .create(true)
            .truncate(true)
            .open(output_path)?;
        let mut writer = BufWriter::new(output_file);
        let mut output_count = 0;

        // SUM state for duplicate handling during merge
        let has_sum = self.sum_fields.is_some();
        let mut prev_record: Option<Vec<u8>> = None;

        while let Some(entry) = heap.pop() {
            let record = entry.record;
            let run_idx = entry.run_index;

            // Read next record from the same run
            if let Some(next) = Self::read_run_record(&mut readers[run_idx])? {
                heap.push(HeapEntry {
                    record: next,
                    run_index: run_idx,
                    sort_spec,
                });
            }

            // Apply SUM deduplication
            if has_sum {
                if let Some(ref prev) = prev_record {
                    if sort_spec.compare(prev, &record) == Ordering::Equal {
                        continue; // Duplicate — skip
                    }
                }
                prev_record = Some(record.clone());
            }

            // Apply OUTREC
            let output_rec = if let Some(ref outrec) = self.outrec {
                outrec.reformat(&record)
            } else {
                record
            };

            if self.record_length > 0 {
                writer.write_all(&output_rec)?;
            } else {
                writer.write_all(&output_rec)?;
                writer.write_all(b"\n")?;
            }
            output_count += 1;
        }

        writer.flush()?;
        Ok(output_count)
    }

    /// Merges multiple pre-sorted files.
    pub fn merge_files<P: AsRef<Path>>(
        &self,
        inputs: &[P],
        output: P,
    ) -> Result<SortStats, SortError> {
        if inputs.is_empty() {
            return Err(SortError::MissingInput("No input files for merge".to_string()));
        }

        let sort_spec = self.sort_spec.as_ref().ok_or_else(|| {
            SortError::MissingInput("Sort specification required for merge".to_string())
        })?;

        // Open all input files
        let mut readers: Vec<_> = inputs
            .iter()
            .map(|p| {
                let file = File::open(p.as_ref())?;
                Ok(BufReader::new(file))
            })
            .collect::<Result<Vec<_>, SortError>>()?;

        // Read first record from each
        let mut current_records: Vec<Option<Vec<u8>>> = readers
            .iter_mut()
            .map(|r| {
                let mut line = String::new();
                match r.read_line(&mut line) {
                    Ok(0) => None,
                    Ok(_) => Some(line.trim_end().as_bytes().to_vec()),
                    Err(_) => None,
                }
            })
            .collect();

        let output_file = OpenOptions::new()
            .write(true)
            .create(true)
            .truncate(true)
            .open(output.as_ref())?;
        let mut writer = BufWriter::new(output_file);

        let mut output_count = 0;

        loop {
            // Find the smallest record
            let mut min_idx: Option<usize> = None;
            for (i, rec) in current_records.iter().enumerate() {
                if let Some(ref record) = rec {
                    match min_idx {
                        None => min_idx = Some(i),
                        Some(idx) => {
                            if let Some(ref min_rec) = current_records[idx] {
                                if sort_spec.compare(record, min_rec) == std::cmp::Ordering::Less {
                                    min_idx = Some(i);
                                }
                            }
                        }
                    }
                }
            }

            match min_idx {
                None => break, // All inputs exhausted
                Some(idx) => {
                    // Write the record
                    let record = current_records[idx].take().unwrap();

                    // Apply OUTREC if specified
                    let output_rec = if let Some(ref outrec) = self.outrec {
                        outrec.reformat(&record)
                    } else {
                        record
                    };

                    writer.write_all(&output_rec)?;
                    writer.write_all(b"\n")?;
                    output_count += 1;

                    // Read next record from this input
                    let mut line = String::new();
                    current_records[idx] = match readers[idx].read_line(&mut line) {
                        Ok(0) => None,
                        Ok(_) => Some(line.trim_end().as_bytes().to_vec()),
                        Err(_) => None,
                    };
                }
            }
        }

        writer.flush()?;

        Ok(SortStats {
            input_records: 0, // Unknown without reading all files
            filtered_records: 0,
            summed_records: 0,
            output_records: output_count,
        })
    }
}

/// Statistics from a sort operation.
#[derive(Debug, Default)]
pub struct SortStats {
    /// Number of input records.
    pub input_records: usize,
    /// Number of records filtered out.
    pub filtered_records: usize,
    /// Number of records removed by SUM.
    pub summed_records: usize,
    /// Number of output records.
    pub output_records: usize,
}

/// Internal result from external sort.
struct ExternalSortResult {
    summed: usize,
    output: usize,
}

/// Manages temporary sorted run files with automatic cleanup.
struct TempRunFiles {
    paths: Vec<PathBuf>,
    counter: usize,
}

impl TempRunFiles {
    fn new() -> Self {
        Self {
            paths: Vec::new(),
            counter: 0,
        }
    }

    /// Create a new temporary run file and return its path.
    fn create_run(&mut self) -> Result<PathBuf, SortError> {
        let path = std::env::temp_dir().join(format!(
            "omsort_run_{}_{}_{}.tmp",
            std::process::id(),
            self.counter,
            std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap_or_default()
                .as_nanos()
        ));
        self.counter += 1;
        self.paths.push(path.clone());
        Ok(path)
    }
}

impl Drop for TempRunFiles {
    fn drop(&mut self) {
        for path in &self.paths {
            let _ = fs::remove_file(path);
        }
    }
}

/// Entry in the min-heap for k-way merge.
///
/// Implements Ord to produce a min-heap (BinaryHeap is a max-heap by default,
/// so we reverse the comparison).
struct HeapEntry<'a> {
    record: Vec<u8>,
    run_index: usize,
    sort_spec: &'a SortSpec,
}

impl<'a> PartialEq for HeapEntry<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.sort_spec.compare(&self.record, &other.record) == Ordering::Equal
    }
}

impl<'a> Eq for HeapEntry<'a> {}

impl<'a> PartialOrd for HeapEntry<'a> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<'a> Ord for HeapEntry<'a> {
    fn cmp(&self, other: &Self) -> Ordering {
        // Reverse ordering for min-heap behavior
        other
            .sort_spec
            .compare(&other.record, &self.record)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::fields::{DataType, SortField, SortOrder};
    use std::fs;
    use std::path::PathBuf;
    use std::sync::atomic::{AtomicUsize, Ordering as AtomicOrdering};

    static TEST_COUNTER: AtomicUsize = AtomicUsize::new(0);

    fn test_path(name: &str) -> PathBuf {
        let count = TEST_COUNTER.fetch_add(1, AtomicOrdering::SeqCst);
        std::env::temp_dir().join(format!("sort_test_{}_{}", name, count))
    }

    fn cleanup(path: &Path) {
        let _ = fs::remove_file(path);
    }

    #[test]
    fn test_sort_simple() {
        let input_path = test_path("input.dat");
        let output_path = test_path("output.dat");

        // Create input file with consistent field widths
        fs::write(&input_path, "Charlie\nAlpha..\nBravo..\nDelta..\n").unwrap();

        // Sort on first 7 characters
        let spec = SortSpec::new()
            .add_field(SortField::new(1, 7, DataType::Character, SortOrder::Ascending));
        let engine = SortEngine::new(spec);
        let stats = engine.sort_file(&input_path, &output_path).unwrap();

        assert_eq!(stats.input_records, 4);
        assert_eq!(stats.output_records, 4);

        // Verify output
        let output = fs::read_to_string(&output_path).unwrap();
        assert_eq!(output, "Alpha..\nBravo..\nCharlie\nDelta..\n");

        cleanup(&input_path);
        cleanup(&output_path);
    }

    #[test]
    fn test_sort_descending() {
        let input_path = test_path("input_desc.dat");
        let output_path = test_path("output_desc.dat");

        fs::write(&input_path, "A\nC\nB\n").unwrap();

        let spec = SortSpec::new()
            .add_field(SortField::new(1, 1, DataType::Character, SortOrder::Descending));
        let engine = SortEngine::new(spec);
        engine.sort_file(&input_path, &output_path).unwrap();

        let output = fs::read_to_string(&output_path).unwrap();
        assert_eq!(output, "C\nB\nA\n");

        cleanup(&input_path);
        cleanup(&output_path);
    }

    #[test]
    fn test_copy_mode() {
        let input_path = test_path("input_copy.dat");
        let output_path = test_path("output_copy.dat");

        fs::write(&input_path, "Three\nOne\nTwo\n").unwrap();

        let engine = SortEngine::copy();
        engine.sort_file(&input_path, &output_path).unwrap();

        // Should preserve original order
        let output = fs::read_to_string(&output_path).unwrap();
        assert_eq!(output, "Three\nOne\nTwo\n");

        cleanup(&input_path);
        cleanup(&output_path);
    }

    #[test]
    fn test_with_filter() {
        let input_path = test_path("input_filter.dat");
        let output_path = test_path("output_filter.dat");

        fs::write(&input_path, "NY123\nCA456\nNY789\nTX000\n").unwrap();

        let spec = SortSpec::new()
            .add_field(SortField::new(1, 5, DataType::Character, SortOrder::Ascending));

        let filter = crate::filter::FilterSpec {
            filter_type: crate::filter::FilterType::Include,
            conditions: vec![crate::filter::Condition {
                position: 1,
                length: 2,
                data_type: DataType::Character,
                op: crate::filter::CompareOp::Eq,
                value: b"NY".to_vec(),
            }],
            logic: None,
        };

        let engine = SortEngine::new(spec).with_include(filter);
        let stats = engine.sort_file(&input_path, &output_path).unwrap();

        assert_eq!(stats.input_records, 4);
        assert_eq!(stats.filtered_records, 2);
        assert_eq!(stats.output_records, 2);

        let output = fs::read_to_string(&output_path).unwrap();
        assert_eq!(output, "NY123\nNY789\n");

        cleanup(&input_path);
        cleanup(&output_path);
    }

    #[test]
    fn test_with_outrec() {
        let input_path = test_path("input_outrec.dat");
        let output_path = test_path("output_outrec.dat");

        fs::write(&input_path, "HelloWorld\nAlphaBravo\n").unwrap();

        let spec = SortSpec::new()
            .add_field(SortField::new(1, 5, DataType::Character, SortOrder::Ascending));

        let outrec = OutrecSpec::new()
            .add_field(crate::reformat::OutrecField::Field { position: 6, length: 5 })
            .add_field(crate::reformat::OutrecField::Literal(b"-".to_vec()))
            .add_field(crate::reformat::OutrecField::Field { position: 1, length: 5 });

        let engine = SortEngine::new(spec).with_outrec(outrec);
        engine.sort_file(&input_path, &output_path).unwrap();

        let output = fs::read_to_string(&output_path).unwrap();
        assert_eq!(output, "Bravo-Alpha\nWorld-Hello\n");

        cleanup(&input_path);
        cleanup(&output_path);
    }

    // -----------------------------------------------------------------------
    // External Sort Tests (Epic 800)
    // -----------------------------------------------------------------------

    #[test]
    fn test_external_sort_small_chunk_size() {
        let input_path = test_path("ext_input.dat");
        let output_path = test_path("ext_output.dat");

        // 10 records, chunk size 3 → 4 sorted runs → k-way merge
        let mut input = String::new();
        for c in ['J', 'D', 'G', 'B', 'I', 'E', 'A', 'H', 'C', 'F'] {
            input.push(c);
            input.push('\n');
        }
        fs::write(&input_path, &input).unwrap();

        let spec = SortSpec::new()
            .add_field(SortField::new(1, 1, DataType::Character, SortOrder::Ascending));
        let engine = SortEngine::new(spec).with_max_memory_records(3);
        let stats = engine.sort_file(&input_path, &output_path).unwrap();

        assert_eq!(stats.input_records, 10);
        assert_eq!(stats.output_records, 10);

        let output = fs::read_to_string(&output_path).unwrap();
        assert_eq!(output, "A\nB\nC\nD\nE\nF\nG\nH\nI\nJ\n");

        cleanup(&input_path);
        cleanup(&output_path);
    }

    #[test]
    fn test_external_sort_with_exact_chunk_boundary() {
        let input_path = test_path("ext_exact.dat");
        let output_path = test_path("ext_exact_out.dat");

        // 6 records, chunk size 3 → exactly 2 runs
        fs::write(&input_path, "F\nD\nE\nC\nA\nB\n").unwrap();

        let spec = SortSpec::new()
            .add_field(SortField::new(1, 1, DataType::Character, SortOrder::Ascending));
        let engine = SortEngine::new(spec).with_max_memory_records(3);
        let stats = engine.sort_file(&input_path, &output_path).unwrap();

        assert_eq!(stats.output_records, 6);

        let output = fs::read_to_string(&output_path).unwrap();
        assert_eq!(output, "A\nB\nC\nD\nE\nF\n");

        cleanup(&input_path);
        cleanup(&output_path);
    }

    #[test]
    fn test_external_sort_descending() {
        let input_path = test_path("ext_desc.dat");
        let output_path = test_path("ext_desc_out.dat");

        fs::write(&input_path, "A\nE\nC\nD\nB\n").unwrap();

        let spec = SortSpec::new()
            .add_field(SortField::new(1, 1, DataType::Character, SortOrder::Descending));
        let engine = SortEngine::new(spec).with_max_memory_records(2);
        let stats = engine.sort_file(&input_path, &output_path).unwrap();

        assert_eq!(stats.output_records, 5);

        let output = fs::read_to_string(&output_path).unwrap();
        assert_eq!(output, "E\nD\nC\nB\nA\n");

        cleanup(&input_path);
        cleanup(&output_path);
    }

    #[test]
    fn test_small_dataset_uses_in_memory_sort() {
        let input_path = test_path("small_input.dat");
        let output_path = test_path("small_output.dat");

        // 3 records with max_memory_records=5 → should use in-memory sort
        fs::write(&input_path, "C\nA\nB\n").unwrap();

        let spec = SortSpec::new()
            .add_field(SortField::new(1, 1, DataType::Character, SortOrder::Ascending));
        let engine = SortEngine::new(spec).with_max_memory_records(5);
        let stats = engine.sort_file(&input_path, &output_path).unwrap();

        assert_eq!(stats.output_records, 3);
        let output = fs::read_to_string(&output_path).unwrap();
        assert_eq!(output, "A\nB\nC\n");

        cleanup(&input_path);
        cleanup(&output_path);
    }

    #[test]
    fn test_external_sort_with_filter() {
        let input_path = test_path("ext_filter.dat");
        let output_path = test_path("ext_filter_out.dat");

        // Filter + external sort: only include records starting with 'A'
        fs::write(
            &input_path,
            "A3\nB1\nA1\nB2\nA4\nB3\nA2\nB4\n",
        )
        .unwrap();

        let spec = SortSpec::new()
            .add_field(SortField::new(1, 2, DataType::Character, SortOrder::Ascending));

        let filter = crate::filter::FilterSpec {
            filter_type: crate::filter::FilterType::Include,
            conditions: vec![crate::filter::Condition {
                position: 1,
                length: 1,
                data_type: DataType::Character,
                op: crate::filter::CompareOp::Eq,
                value: b"A".to_vec(),
            }],
            logic: None,
        };

        let engine = SortEngine::new(spec)
            .with_include(filter)
            .with_max_memory_records(2);
        let stats = engine.sort_file(&input_path, &output_path).unwrap();

        assert_eq!(stats.input_records, 8);
        assert_eq!(stats.filtered_records, 4);
        assert_eq!(stats.output_records, 4);

        let output = fs::read_to_string(&output_path).unwrap();
        assert_eq!(output, "A1\nA2\nA3\nA4\n");

        cleanup(&input_path);
        cleanup(&output_path);
    }

    #[test]
    fn test_temp_run_files_cleanup() {
        let paths: Vec<PathBuf>;
        {
            let mut runs = TempRunFiles::new();
            let p1 = runs.create_run().unwrap();
            let p2 = runs.create_run().unwrap();
            // Create the actual files
            fs::write(&p1, b"run1").unwrap();
            fs::write(&p2, b"run2").unwrap();
            assert!(p1.exists());
            assert!(p2.exists());
            paths = runs.paths.clone();
        }
        // After drop, files should be cleaned up
        for p in &paths {
            assert!(!p.exists(), "Temp file was not cleaned up: {:?}", p);
        }
    }
}
