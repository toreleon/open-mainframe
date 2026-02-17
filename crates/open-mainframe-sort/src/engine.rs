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

/// Record format for input/output.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RecordFormat {
    /// Line-based text records (newline-delimited).
    LineBased,
    /// Fixed-length binary records.
    Fixed(usize),
    /// Variable-length records with 4-byte RDW (Record Descriptor Word).
    VariableBlocked,
}

/// Format conversion for OUTFIL.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FormatConversion {
    /// No conversion.
    None,
    /// Fixed-to-variable: remove trailing spaces, add RDW.
    Ftov,
    /// Variable-to-fixed: pad/truncate to fixed length, strip RDW.
    Vtof(usize),
}

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
    /// Record format for input.
    record_format: RecordFormat,
    /// Format conversion for output.
    format_conversion: FormatConversion,
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
            record_format: RecordFormat::LineBased,
            format_conversion: FormatConversion::None,
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
            record_format: RecordFormat::LineBased,
            format_conversion: FormatConversion::None,
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

    /// Sets fixed record length (also sets format to Fixed).
    pub fn with_record_length(mut self, length: usize) -> Self {
        self.record_length = length;
        if length > 0 {
            self.record_format = RecordFormat::Fixed(length);
        }
        self
    }

    /// Sets the record format.
    pub fn with_record_format(mut self, format: RecordFormat) -> Self {
        self.record_format = format;
        if let RecordFormat::Fixed(len) = format {
            self.record_length = len;
        }
        self
    }

    /// Sets format conversion for output.
    pub fn with_format_conversion(mut self, conversion: FormatConversion) -> Self {
        self.format_conversion = conversion;
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

        match self.record_format {
            RecordFormat::Fixed(len) => {
                let mut reader = BufReader::new(file);
                let mut buffer = vec![0u8; len];
                loop {
                    match reader.read_exact(&mut buffer) {
                        Ok(()) => records.push(buffer.clone()),
                        Err(e) if e.kind() == std::io::ErrorKind::UnexpectedEof => break,
                        Err(e) => return Err(e.into()),
                    }
                }
            }
            RecordFormat::VariableBlocked => {
                let mut reader = BufReader::new(file);
                loop {
                    // Read 4-byte RDW: bytes 0-1 = record length (including RDW), bytes 2-3 = 0
                    let mut rdw = [0u8; 4];
                    match reader.read_exact(&mut rdw) {
                        Ok(()) => {}
                        Err(e) if e.kind() == std::io::ErrorKind::UnexpectedEof => break,
                        Err(e) => return Err(e.into()),
                    }
                    let rec_len = u16::from_be_bytes([rdw[0], rdw[1]]) as usize;
                    if rec_len < 4 {
                        break; // Invalid RDW
                    }
                    let data_len = rec_len - 4;
                    let mut buffer = vec![0u8; data_len];
                    reader.read_exact(&mut buffer)?;
                    records.push(buffer);
                }
            }
            RecordFormat::LineBased => {
                let reader = BufReader::new(file);
                for line in reader.lines() {
                    records.push(line?.into_bytes());
                }
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

        for record in records {
            let output = self.apply_format_conversion(record);
            match self.output_format() {
                RecordFormat::Fixed(len) => {
                    // Pad or truncate to fixed length
                    if output.len() >= len {
                        writer.write_all(&output[..len])?;
                    } else {
                        writer.write_all(&output)?;
                        writer.write_all(&vec![b' '; len - output.len()])?;
                    }
                }
                RecordFormat::VariableBlocked => {
                    let rec_len = (output.len() + 4) as u16;
                    writer.write_all(&rec_len.to_be_bytes())?;
                    writer.write_all(&[0u8; 2])?; // Reserved bytes
                    writer.write_all(&output)?;
                }
                RecordFormat::LineBased => {
                    writer.write_all(&output)?;
                    writer.write_all(b"\n")?;
                }
            }
        }

        writer.flush()?;
        Ok(())
    }

    /// Apply format conversion to a record for output.
    fn apply_format_conversion(&self, record: &[u8]) -> Vec<u8> {
        match self.format_conversion {
            FormatConversion::None => record.to_vec(),
            FormatConversion::Ftov => {
                // Fixed-to-variable: strip trailing spaces
                let trimmed_len = record.iter().rposition(|&b| b != b' ')
                    .map_or(0, |pos| pos + 1);
                record[..trimmed_len].to_vec()
            }
            FormatConversion::Vtof(fixed_len) => {
                // Variable-to-fixed: pad or truncate
                let mut output = Vec::with_capacity(fixed_len);
                let copy_len = record.len().min(fixed_len);
                output.extend_from_slice(&record[..copy_len]);
                if output.len() < fixed_len {
                    output.resize(fixed_len, b' ');
                }
                output
            }
        }
    }

    /// Determine output record format based on conversion.
    fn output_format(&self) -> RecordFormat {
        match self.format_conversion {
            FormatConversion::None => self.record_format,
            FormatConversion::Ftov => RecordFormat::VariableBlocked,
            FormatConversion::Vtof(len) => RecordFormat::Fixed(len),
        }
    }

    /// Apply SUM processing for duplicate keys.
    ///
    /// When `sum_fields` is empty (FIELDS=NONE), duplicates are simply removed.
    /// When `sum_fields` contains field definitions, numeric fields are accumulated
    /// across records with identical sort keys.
    fn apply_sum(
        &self,
        records: &[Vec<u8>],
        sum_fields: &[(usize, usize, crate::fields::DataType)],
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
            match &mut current_key_record {
                None => {
                    current_key_record = Some(record.clone());
                }
                Some(prev) => {
                    if sort_spec.compare(prev, record) == std::cmp::Ordering::Equal {
                        // Duplicate key — accumulate numeric SUM fields
                        if !sum_fields.is_empty() {
                            accumulate_sum_fields(prev, record, sum_fields);
                        }
                        // If sum_fields is empty (FIELDS=NONE), just skip (dedup)
                    } else {
                        result.push(std::mem::replace(prev, record.clone()));
                    }
                }
            }
        }

        if let Some(last) = current_key_record {
            result.push(last);
        }

        result
    }

    /// Apply SUM during external sort merge.
    ///
    /// Returns (output record, was_accumulated) so the caller knows whether
    /// the record was merged into the accumulator or is a new key.
    fn accumulate_or_emit(
        &self,
        accumulator: &mut Option<Vec<u8>>,
        record: &[u8],
        sort_spec: &SortSpec,
        sum_fields: &[(usize, usize, crate::fields::DataType)],
    ) -> Option<Vec<u8>> {
        match accumulator {
            None => {
                *accumulator = Some(record.to_vec());
                None
            }
            Some(ref mut acc) => {
                if sort_spec.compare(acc, record) == Ordering::Equal {
                    // Duplicate — accumulate
                    if !sum_fields.is_empty() {
                        accumulate_sum_fields(acc, record, sum_fields);
                    }
                    None
                } else {
                    // New key — emit old, start new accumulator
                    let emit = std::mem::replace(acc, record.to_vec());
                    Some(emit)
                }
            }
        }
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
        let sum_fields_ref = self.sum_fields.as_deref().unwrap_or(&[]);
        let has_sum = self.sum_fields.is_some();
        let mut accumulator: Option<Vec<u8>> = None;

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

            // Apply SUM accumulation or pass through
            if has_sum {
                if let Some(emit) = self.accumulate_or_emit(
                    &mut accumulator,
                    &record,
                    sort_spec,
                    sum_fields_ref,
                ) {
                    let output_rec = if let Some(ref outrec) = self.outrec {
                        outrec.reformat(&emit)
                    } else {
                        emit
                    };
                    if self.record_length > 0 {
                        writer.write_all(&output_rec)?;
                    } else {
                        writer.write_all(&output_rec)?;
                        writer.write_all(b"\n")?;
                    }
                    output_count += 1;
                }
            } else {
                // No SUM — write directly
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
        }

        // Flush final accumulated record
        if has_sum {
            if let Some(last) = accumulator {
                let output_rec = if let Some(ref outrec) = self.outrec {
                    outrec.reformat(&last)
                } else {
                    last
                };
                if self.record_length > 0 {
                    writer.write_all(&output_rec)?;
                } else {
                    writer.write_all(&output_rec)?;
                    writer.write_all(b"\n")?;
                }
                output_count += 1;
            }
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

/// Accumulate SUM fields from `source` into `target`.
///
/// For each SUM field, extract the numeric value from `source`,
/// add it to the value in `target`, and write the result back to `target`.
fn accumulate_sum_fields(
    target: &mut [u8],
    source: &[u8],
    sum_fields: &[(usize, usize, crate::fields::DataType)],
) {
    use crate::fields::{extract_numeric, pack_numeric};

    for &(position, length, data_type) in sum_fields {
        let start = position.saturating_sub(1);
        let end = start + length;

        // Extract values from both records
        let target_val = if end <= target.len() {
            extract_numeric(&target[start..end], data_type)
        } else {
            0
        };

        let source_val = if end <= source.len() {
            extract_numeric(&source[start..end], data_type)
        } else {
            0
        };

        // Accumulate and write back
        let sum = target_val.saturating_add(source_val);
        if end <= target.len() {
            pack_numeric(sum, &mut target[start..end], data_type);
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

    // -----------------------------------------------------------------------
    // SUM Tests (Epic 803)
    // -----------------------------------------------------------------------

    #[test]
    fn test_sum_fields_none_dedup() {
        let input_path = test_path("sum_none_in.dat");
        let output_path = test_path("sum_none_out.dat");

        // Three records with key "AAA", two with "BBB"
        fs::write(&input_path, "AAA100\nAAA200\nAAA300\nBBB400\nBBB500\n").unwrap();

        let spec = SortSpec::new()
            .add_field(SortField::new(1, 3, DataType::Character, SortOrder::Ascending));
        let engine = SortEngine::new(spec).with_sum(vec![]);
        let stats = engine.sort_file(&input_path, &output_path).unwrap();

        assert_eq!(stats.output_records, 2);
        assert_eq!(stats.summed_records, 3);

        let output = fs::read_to_string(&output_path).unwrap();
        let lines: Vec<&str> = output.lines().collect();
        assert_eq!(lines.len(), 2);
        assert_eq!(&lines[0][..3], "AAA");
        assert_eq!(&lines[1][..3], "BBB");

        cleanup(&input_path);
        cleanup(&output_path);
    }

    #[test]
    fn test_sum_packed_decimal_accumulation() {
        let input_path = test_path("sum_pd_in.dat");
        let output_path = test_path("sum_pd_out.dat");

        // Fixed-length records: 3-byte key + 3-byte packed decimal
        // 3-byte PD: 5 digit positions (3*2 - 1 = 5)
        // PD 100 = 0x00 0x10 0x0C, PD 200 = 0x00 0x20 0x0C, PD 300 = 0x00 0x30 0x0C
        let mut records: Vec<u8> = Vec::new();
        // Record 1: AAA + PD 100
        records.extend_from_slice(b"AAA");
        records.extend_from_slice(&[0x00, 0x10, 0x0C]);
        // Record 2: AAA + PD 200
        records.extend_from_slice(b"AAA");
        records.extend_from_slice(&[0x00, 0x20, 0x0C]);
        // Record 3: AAA + PD 300
        records.extend_from_slice(b"AAA");
        records.extend_from_slice(&[0x00, 0x30, 0x0C]);
        // Record 4: BBB + PD 400
        records.extend_from_slice(b"BBB");
        records.extend_from_slice(&[0x00, 0x40, 0x0C]);

        fs::write(&input_path, &records).unwrap();

        let spec = SortSpec::new()
            .add_field(SortField::new(1, 3, DataType::Character, SortOrder::Ascending));

        let engine = SortEngine::new(spec)
            .with_sum(vec![(4, 3, DataType::PackedDecimal)])
            .with_record_length(6);

        let stats = engine.sort_file(&input_path, &output_path).unwrap();
        assert_eq!(stats.output_records, 2);

        // Read the output
        let output = fs::read(&output_path).unwrap();
        assert_eq!(output.len(), 12); // 2 records × 6 bytes

        // First record: AAA + sum of 100+200+300 = 600 packed
        assert_eq!(&output[0..3], b"AAA");
        let sum_val = crate::fields::extract_numeric(&output[3..6], DataType::PackedDecimal);
        assert_eq!(sum_val, 600);

        // Second record: BBB + 400
        assert_eq!(&output[6..9], b"BBB");
        let val2 = crate::fields::extract_numeric(&output[9..12], DataType::PackedDecimal);
        assert_eq!(val2, 400);

        cleanup(&input_path);
        cleanup(&output_path);
    }

    #[test]
    fn test_sum_binary_accumulation() {
        let input_path = test_path("sum_bi_in.dat");
        let output_path = test_path("sum_bi_out.dat");

        // Fixed-length records: 2-byte key + 4-byte binary
        let mut records: Vec<u8> = Vec::new();
        // Record 1: "AB" + binary 1000
        records.extend_from_slice(b"AB");
        records.extend_from_slice(&1000i32.to_be_bytes());
        // Record 2: "AB" + binary 2000
        records.extend_from_slice(b"AB");
        records.extend_from_slice(&2000i32.to_be_bytes());
        // Record 3: "CD" + binary 500
        records.extend_from_slice(b"CD");
        records.extend_from_slice(&500i32.to_be_bytes());

        fs::write(&input_path, &records).unwrap();

        let spec = SortSpec::new()
            .add_field(SortField::new(1, 2, DataType::Character, SortOrder::Ascending));

        let engine = SortEngine::new(spec)
            .with_sum(vec![(3, 4, DataType::Binary)])
            .with_record_length(6);

        let stats = engine.sort_file(&input_path, &output_path).unwrap();
        assert_eq!(stats.output_records, 2);

        let output = fs::read(&output_path).unwrap();
        assert_eq!(output.len(), 12);

        // AB: 1000 + 2000 = 3000
        let sum = crate::fields::extract_numeric(&output[2..6], DataType::Binary);
        assert_eq!(sum, 3000);

        // CD: 500
        let val = crate::fields::extract_numeric(&output[8..12], DataType::Binary);
        assert_eq!(val, 500);

        cleanup(&input_path);
        cleanup(&output_path);
    }

    #[test]
    fn test_sum_multiple_fields() {
        let input_path = test_path("sum_multi_in.dat");
        let output_path = test_path("sum_multi_out.dat");

        // 2-byte key + 4-byte binary (field1) + 4-byte binary (field2)
        let mut records: Vec<u8> = Vec::new();
        // Record 1: "AA" + 10 + 100
        records.extend_from_slice(b"AA");
        records.extend_from_slice(&10i32.to_be_bytes());
        records.extend_from_slice(&100i32.to_be_bytes());
        // Record 2: "AA" + 20 + 200
        records.extend_from_slice(b"AA");
        records.extend_from_slice(&20i32.to_be_bytes());
        records.extend_from_slice(&200i32.to_be_bytes());

        fs::write(&input_path, &records).unwrap();

        let spec = SortSpec::new()
            .add_field(SortField::new(1, 2, DataType::Character, SortOrder::Ascending));

        let engine = SortEngine::new(spec)
            .with_sum(vec![
                (3, 4, DataType::Binary),
                (7, 4, DataType::Binary),
            ])
            .with_record_length(10);

        let stats = engine.sort_file(&input_path, &output_path).unwrap();
        assert_eq!(stats.output_records, 1);

        let output = fs::read(&output_path).unwrap();
        let f1 = crate::fields::extract_numeric(&output[2..6], DataType::Binary);
        let f2 = crate::fields::extract_numeric(&output[6..10], DataType::Binary);
        assert_eq!(f1, 30);
        assert_eq!(f2, 300);

        cleanup(&input_path);
        cleanup(&output_path);
    }

    #[test]
    fn test_sum_external_sort_accumulation() {
        let input_path = test_path("sum_ext_in.dat");
        let output_path = test_path("sum_ext_out.dat");

        // 2-byte key + 4-byte binary, max_memory_records=2 to force external sort
        let mut records: Vec<u8> = Vec::new();
        // 4 records with key "AA" and 4 with key "BB"
        for val in [10i32, 20, 30, 40] {
            records.extend_from_slice(b"AA");
            records.extend_from_slice(&val.to_be_bytes());
        }
        for val in [100i32, 200] {
            records.extend_from_slice(b"BB");
            records.extend_from_slice(&val.to_be_bytes());
        }

        fs::write(&input_path, &records).unwrap();

        let spec = SortSpec::new()
            .add_field(SortField::new(1, 2, DataType::Character, SortOrder::Ascending));

        let engine = SortEngine::new(spec)
            .with_sum(vec![(3, 4, DataType::Binary)])
            .with_record_length(6)
            .with_max_memory_records(2);

        let stats = engine.sort_file(&input_path, &output_path).unwrap();
        assert_eq!(stats.output_records, 2);

        let output = fs::read(&output_path).unwrap();
        // AA: 10+20+30+40 = 100
        let aa_sum = crate::fields::extract_numeric(&output[2..6], DataType::Binary);
        assert_eq!(aa_sum, 100);
        // BB: 100+200 = 300
        let bb_sum = crate::fields::extract_numeric(&output[8..12], DataType::Binary);
        assert_eq!(bb_sum, 300);

        cleanup(&input_path);
        cleanup(&output_path);
    }

    // -----------------------------------------------------------------------
    // VB Record / FTOV / VTOF Tests (Epic 807)
    // -----------------------------------------------------------------------

    #[test]
    fn test_vb_read_write_sort() {
        let input_path = test_path("vb_input.dat");
        let output_path = test_path("vb_output.dat");

        // Write VB records: RDW (4 bytes) + data
        let mut input_data = Vec::new();
        // Record "Charlie" (7 bytes) → RDW = 11
        let rec1 = b"Charlie";
        let rdw1 = (rec1.len() as u16 + 4).to_be_bytes();
        input_data.extend_from_slice(&rdw1);
        input_data.extend_from_slice(&[0, 0]);
        input_data.extend_from_slice(rec1);

        // Record "Alpha" (5 bytes) → RDW = 9
        let rec2 = b"Alpha";
        let rdw2 = (rec2.len() as u16 + 4).to_be_bytes();
        input_data.extend_from_slice(&rdw2);
        input_data.extend_from_slice(&[0, 0]);
        input_data.extend_from_slice(rec2);

        // Record "Bravo" (5 bytes) → RDW = 9
        let rec3 = b"Bravo";
        let rdw3 = (rec3.len() as u16 + 4).to_be_bytes();
        input_data.extend_from_slice(&rdw3);
        input_data.extend_from_slice(&[0, 0]);
        input_data.extend_from_slice(rec3);

        fs::write(&input_path, &input_data).unwrap();

        let spec = SortSpec::new()
            .add_field(SortField::new(1, 7, DataType::Character, SortOrder::Ascending));
        let engine = SortEngine::new(spec)
            .with_record_format(RecordFormat::VariableBlocked);
        let stats = engine.sort_file(&input_path, &output_path).unwrap();

        assert_eq!(stats.input_records, 3);
        assert_eq!(stats.output_records, 3);

        // Read output VB records
        let output = fs::read(&output_path).unwrap();
        let mut pos = 0;
        let mut sorted_records = Vec::new();
        while pos < output.len() {
            let rec_len = u16::from_be_bytes([output[pos], output[pos + 1]]) as usize;
            let data = &output[pos + 4..pos + rec_len];
            sorted_records.push(String::from_utf8_lossy(data).to_string());
            pos += rec_len;
        }

        assert_eq!(sorted_records, vec!["Alpha", "Bravo", "Charlie"]);

        cleanup(&input_path);
        cleanup(&output_path);
    }

    #[test]
    fn test_vtof_conversion() {
        let input_path = test_path("vtof_input.dat");
        let output_path = test_path("vtof_output.dat");

        // Write VB records of varying length
        let mut input_data = Vec::new();
        for rec in [b"Short" as &[u8], b"Medium Data", b"Longer Data Here"] {
            let rdw = (rec.len() as u16 + 4).to_be_bytes();
            input_data.extend_from_slice(&rdw);
            input_data.extend_from_slice(&[0, 0]);
            input_data.extend_from_slice(rec);
        }
        fs::write(&input_path, &input_data).unwrap();

        let engine = SortEngine::copy()
            .with_record_format(RecordFormat::VariableBlocked)
            .with_format_conversion(FormatConversion::Vtof(20));
        let stats = engine.sort_file(&input_path, &output_path).unwrap();

        assert_eq!(stats.output_records, 3);

        // Output should be fixed 20-byte records
        let output = fs::read(&output_path).unwrap();
        assert_eq!(output.len(), 60); // 3 records × 20 bytes

        assert_eq!(&output[0..5], b"Short");
        assert_eq!(&output[5..20], &vec![b' '; 15]); // padded
        assert_eq!(&output[20..31], b"Medium Data");
        assert_eq!(&output[40..56], b"Longer Data Here");

        cleanup(&input_path);
        cleanup(&output_path);
    }

    #[test]
    fn test_ftov_conversion() {
        let input_path = test_path("ftov_input.dat");
        let output_path = test_path("ftov_output.dat");

        // Write fixed 20-byte records with trailing spaces
        let mut input_data = Vec::new();
        let rec1 = format!("{:<20}", "Hello");
        let rec2 = format!("{:<20}", "World Record");
        input_data.extend_from_slice(rec1.as_bytes());
        input_data.extend_from_slice(rec2.as_bytes());
        fs::write(&input_path, &input_data).unwrap();

        let engine = SortEngine::copy()
            .with_record_format(RecordFormat::Fixed(20))
            .with_format_conversion(FormatConversion::Ftov);
        let stats = engine.sort_file(&input_path, &output_path).unwrap();

        assert_eq!(stats.output_records, 2);

        // Read output VB records
        let output = fs::read(&output_path).unwrap();
        let mut pos = 0;
        let mut records = Vec::new();
        while pos < output.len() {
            let rec_len = u16::from_be_bytes([output[pos], output[pos + 1]]) as usize;
            let data = &output[pos + 4..pos + rec_len];
            records.push(String::from_utf8_lossy(data).to_string());
            pos += rec_len;
        }

        assert_eq!(records[0], "Hello"); // trailing spaces stripped
        assert_eq!(records[1], "World Record"); // trailing spaces stripped
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
