//! EZ-105: SORT & Utilities for Easytrieve Plus.
//!
//! Provides SORT by fields, MATCH between files on key fields,
//! and BEFORE/AFTER control break processing.

use std::cmp::Ordering;
use std::collections::HashMap;

use miette::Diagnostic;
use thiserror::Error;

use crate::fileio::EzRecord;

// ---------------------------------------------------------------------------
// Errors
// ---------------------------------------------------------------------------

/// Errors during sort and utility operations.
#[derive(Debug, Error, Diagnostic)]
pub enum SortError {
    /// Sort key field not found.
    #[error("sort key field '{field}' not found at position {position} with length {length}")]
    KeyFieldNotFound {
        /// Field name.
        field: String,
        /// Expected position.
        position: usize,
        /// Expected length.
        length: usize,
    },
    /// Match key mismatch.
    #[error("match key field '{field}' not defined in file")]
    MatchKeyNotFound {
        /// Field name.
        field: String,
    },
}

// ---------------------------------------------------------------------------
// Sort key
// ---------------------------------------------------------------------------

/// Direction for sort operations.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SortOrder {
    /// Ascending sort order.
    Ascending,
    /// Descending sort order.
    Descending,
}

/// A sort key definition.
#[derive(Debug, Clone)]
pub struct SortKey {
    /// Field name.
    pub field_name: String,
    /// Start position within record (0-based).
    pub position: usize,
    /// Field length in bytes.
    pub length: usize,
    /// Sort direction.
    pub order: SortOrder,
}

impl SortKey {
    /// Create a new sort key.
    pub fn new(field_name: &str, position: usize, length: usize, order: SortOrder) -> Self {
        Self {
            field_name: field_name.to_string(),
            position,
            length,
            order,
        }
    }

    /// Extract the key value from a record.
    pub fn extract(&self, record: &EzRecord) -> Vec<u8> {
        let end = std::cmp::min(self.position + self.length, record.data.len());
        if self.position >= record.data.len() {
            return vec![b' '; self.length];
        }
        record.data[self.position..end].to_vec()
    }
}

// ---------------------------------------------------------------------------
// Sort
// ---------------------------------------------------------------------------

/// Easytrieve SORT operation on a set of records.
///
/// Sorts records by one or more key fields in ascending or descending order.
#[derive(Debug)]
pub struct EzSort {
    /// Sort keys in priority order.
    pub keys: Vec<SortKey>,
}

impl EzSort {
    /// Create a new sort operation.
    pub fn new() -> Self {
        Self { keys: Vec::new() }
    }

    /// Add a sort key.
    pub fn add_key(&mut self, key: SortKey) {
        self.keys.push(key);
    }

    /// Sort the given records in place.
    pub fn sort(&self, records: &mut [EzRecord]) {
        records.sort_by(|a, b| self.compare_records(a, b));
    }

    /// Sort records and return a new sorted vector.
    pub fn sorted(&self, records: &[EzRecord]) -> Vec<EzRecord> {
        let mut sorted = records.to_vec();
        self.sort(&mut sorted);
        sorted
    }

    /// Compare two records by the sort keys.
    fn compare_records(&self, a: &EzRecord, b: &EzRecord) -> Ordering {
        for key in &self.keys {
            let ka = key.extract(a);
            let kb = key.extract(b);
            let cmp = ka.cmp(&kb);
            if cmp != Ordering::Equal {
                return match key.order {
                    SortOrder::Ascending => cmp,
                    SortOrder::Descending => cmp.reverse(),
                };
            }
        }
        Ordering::Equal
    }
}

impl Default for EzSort {
    fn default() -> Self {
        Self::new()
    }
}

// ---------------------------------------------------------------------------
// Match
// ---------------------------------------------------------------------------

/// Match status for records.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MatchStatus {
    /// Record matched in both files.
    Matched,
    /// Record found only in file 1.
    File1Only,
    /// Record found only in file 2.
    File2Only,
}

/// Matched record pair.
#[derive(Debug, Clone)]
pub struct MatchedPair {
    /// Match status.
    pub status: MatchStatus,
    /// Record from file 1 (if present).
    pub record1: Option<EzRecord>,
    /// Record from file 2 (if present).
    pub record2: Option<EzRecord>,
}

/// Easytrieve MATCH operation between two files.
///
/// Matches records between two sorted files on common key fields,
/// similar to a SQL full outer join.
#[derive(Debug)]
pub struct EzMatch {
    /// Key field position in file 1 (0-based).
    pub key1_position: usize,
    /// Key field position in file 2 (0-based).
    pub key2_position: usize,
    /// Key field length.
    pub key_length: usize,
}

impl EzMatch {
    /// Create a new match operation.
    pub fn new(key1_position: usize, key2_position: usize, key_length: usize) -> Self {
        Self {
            key1_position,
            key2_position,
            key_length,
        }
    }

    /// Extract key from a record at the given position.
    fn extract_key(&self, record: &EzRecord, position: usize) -> Vec<u8> {
        let end = std::cmp::min(position + self.key_length, record.data.len());
        if position >= record.data.len() {
            return vec![b' '; self.key_length];
        }
        record.data[position..end].to_vec()
    }

    /// Perform the match between two sorted record sets.
    ///
    /// Both input record sets must be sorted by their respective key fields.
    pub fn execute(
        &self,
        records1: &[EzRecord],
        records2: &[EzRecord],
    ) -> Vec<MatchedPair> {
        let mut result = Vec::new();
        let mut i = 0;
        let mut j = 0;

        while i < records1.len() && j < records2.len() {
            let key1 = self.extract_key(&records1[i], self.key1_position);
            let key2 = self.extract_key(&records2[j], self.key2_position);

            match key1.cmp(&key2) {
                Ordering::Equal => {
                    result.push(MatchedPair {
                        status: MatchStatus::Matched,
                        record1: Some(records1[i].clone()),
                        record2: Some(records2[j].clone()),
                    });
                    i += 1;
                    j += 1;
                }
                Ordering::Less => {
                    result.push(MatchedPair {
                        status: MatchStatus::File1Only,
                        record1: Some(records1[i].clone()),
                        record2: None,
                    });
                    i += 1;
                }
                Ordering::Greater => {
                    result.push(MatchedPair {
                        status: MatchStatus::File2Only,
                        record1: None,
                        record2: Some(records2[j].clone()),
                    });
                    j += 1;
                }
            }
        }

        // Remaining from file 1
        while i < records1.len() {
            result.push(MatchedPair {
                status: MatchStatus::File1Only,
                record1: Some(records1[i].clone()),
                record2: None,
            });
            i += 1;
        }

        // Remaining from file 2
        while j < records2.len() {
            result.push(MatchedPair {
                status: MatchStatus::File2Only,
                record1: None,
                record2: Some(records2[j].clone()),
            });
            j += 1;
        }

        result
    }
}

// ---------------------------------------------------------------------------
// Control break
// ---------------------------------------------------------------------------

/// Callback kind for control break processing.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BreakTiming {
    /// Execute before the first record of a new group.
    Before,
    /// Execute after the last record of a group.
    After,
}

/// Control break event with accumulated data.
#[derive(Debug, Clone)]
pub struct BreakEvent {
    /// Timing of the break.
    pub timing: BreakTiming,
    /// Field name that triggered the break.
    pub field_name: String,
    /// Value of the break field.
    pub break_value: String,
    /// Accumulated values for the group.
    pub accumulators: HashMap<String, f64>,
    /// Count of records in the group.
    pub record_count: usize,
}

/// Easytrieve control break processor for BEFORE/AFTER processing.
///
/// Tracks sort key changes and generates break events with accumulated data.
#[derive(Debug)]
pub struct EzControlBreak {
    /// Break field name.
    pub field_name: String,
    /// Previous value.
    previous_value: Option<String>,
    /// Accumulators for the current group.
    accumulators: HashMap<String, f64>,
    /// Record count for the current group.
    record_count: usize,
    /// Collected break events.
    events: Vec<BreakEvent>,
}

impl EzControlBreak {
    /// Create a new control break processor.
    pub fn new(field_name: &str) -> Self {
        Self {
            field_name: field_name.to_string(),
            previous_value: None,
            accumulators: HashMap::new(),
            record_count: 0,
            events: Vec::new(),
        }
    }

    /// Process a record with the given field value.
    ///
    /// Returns break events if a control break occurred.
    pub fn process(&mut self, current_value: &str) -> Vec<BreakEvent> {
        let mut fired = Vec::new();

        if let Some(prev) = &self.previous_value {
            if prev != current_value {
                // AFTER event for the previous group
                let after_event = BreakEvent {
                    timing: BreakTiming::After,
                    field_name: self.field_name.clone(),
                    break_value: prev.clone(),
                    accumulators: self.accumulators.clone(),
                    record_count: self.record_count,
                };
                fired.push(after_event);
                self.events.push(fired.last().unwrap().clone());

                // Reset for new group
                self.accumulators.clear();
                self.record_count = 0;

                // BEFORE event for the new group
                let before_event = BreakEvent {
                    timing: BreakTiming::Before,
                    field_name: self.field_name.clone(),
                    break_value: current_value.to_string(),
                    accumulators: HashMap::new(),
                    record_count: 0,
                };
                fired.push(before_event);
                self.events.push(fired.last().unwrap().clone());
            }
        } else {
            // First record — BEFORE event
            let before_event = BreakEvent {
                timing: BreakTiming::Before,
                field_name: self.field_name.clone(),
                break_value: current_value.to_string(),
                accumulators: HashMap::new(),
                record_count: 0,
            };
            fired.push(before_event);
            self.events.push(fired.last().unwrap().clone());
        }

        self.previous_value = Some(current_value.to_string());
        self.record_count += 1;

        fired
    }

    /// Accumulate a value for a field within the current group.
    pub fn accumulate(&mut self, field: &str, value: f64) {
        let acc = self.accumulators.entry(field.to_string()).or_insert(0.0);
        *acc += value;
    }

    /// Finalize processing and generate the final AFTER event.
    pub fn finalize(&mut self) -> Option<BreakEvent> {
        if let Some(prev) = &self.previous_value {
            let event = BreakEvent {
                timing: BreakTiming::After,
                field_name: self.field_name.clone(),
                break_value: prev.clone(),
                accumulators: self.accumulators.clone(),
                record_count: self.record_count,
            };
            self.events.push(event.clone());
            Some(event)
        } else {
            None
        }
    }

    /// Get all break events that have been generated.
    pub fn events(&self) -> &[BreakEvent] {
        &self.events
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    fn make_record(key: &str, data: &str) -> EzRecord {
        let mut rec = EzRecord::new(40);
        let key_bytes = key.as_bytes();
        let data_bytes = data.as_bytes();
        let key_len = std::cmp::min(key_bytes.len(), 10);
        rec.data[0..key_len].copy_from_slice(&key_bytes[..key_len]);
        let data_len = std::cmp::min(data_bytes.len(), 30);
        rec.data[10..10 + data_len].copy_from_slice(&data_bytes[..data_len]);
        rec
    }

    #[test]
    fn test_sort_ascending() {
        let mut records = vec![
            make_record("C", "Charlie"),
            make_record("A", "Alice"),
            make_record("B", "Bob"),
        ];

        let mut sorter = EzSort::new();
        sorter.add_key(SortKey::new("KEY", 0, 1, SortOrder::Ascending));
        sorter.sort(&mut records);

        assert_eq!(records[0].data[0], b'A');
        assert_eq!(records[1].data[0], b'B');
        assert_eq!(records[2].data[0], b'C');
    }

    #[test]
    fn test_sort_descending() {
        let mut records = vec![
            make_record("A", "Alice"),
            make_record("C", "Charlie"),
            make_record("B", "Bob"),
        ];

        let mut sorter = EzSort::new();
        sorter.add_key(SortKey::new("KEY", 0, 1, SortOrder::Descending));
        sorter.sort(&mut records);

        assert_eq!(records[0].data[0], b'C');
        assert_eq!(records[1].data[0], b'B');
        assert_eq!(records[2].data[0], b'A');
    }

    #[test]
    fn test_sort_multiple_keys() {
        let mut records = vec![
            make_record("A2", "Second A"),
            make_record("B1", "First B"),
            make_record("A1", "First A"),
        ];

        let mut sorter = EzSort::new();
        sorter.add_key(SortKey::new("KEY1", 0, 1, SortOrder::Ascending));
        sorter.add_key(SortKey::new("KEY2", 1, 1, SortOrder::Ascending));
        sorter.sort(&mut records);

        assert_eq!(&records[0].data[0..2], b"A1");
        assert_eq!(&records[1].data[0..2], b"A2");
        assert_eq!(&records[2].data[0..2], b"B1");
    }

    #[test]
    fn test_sorted_returns_new_vec() {
        let records = vec![
            make_record("B", "Bob"),
            make_record("A", "Alice"),
        ];

        let mut sorter = EzSort::new();
        sorter.add_key(SortKey::new("KEY", 0, 1, SortOrder::Ascending));
        let sorted = sorter.sorted(&records);

        assert_eq!(sorted[0].data[0], b'A');
        assert_eq!(records[0].data[0], b'B'); // Original unchanged
    }

    #[test]
    fn test_match_all_matched() {
        let records1 = vec![
            make_record("A", "File1-A"),
            make_record("B", "File1-B"),
        ];
        let records2 = vec![
            make_record("A", "File2-A"),
            make_record("B", "File2-B"),
        ];

        let matcher = EzMatch::new(0, 0, 1);
        let result = matcher.execute(&records1, &records2);

        assert_eq!(result.len(), 2);
        assert_eq!(result[0].status, MatchStatus::Matched);
        assert_eq!(result[1].status, MatchStatus::Matched);
    }

    #[test]
    fn test_match_unmatched_records() {
        let records1 = vec![
            make_record("A", "File1-A"),
            make_record("C", "File1-C"),
        ];
        let records2 = vec![
            make_record("B", "File2-B"),
            make_record("C", "File2-C"),
        ];

        let matcher = EzMatch::new(0, 0, 1);
        let result = matcher.execute(&records1, &records2);

        assert_eq!(result.len(), 3);
        assert_eq!(result[0].status, MatchStatus::File1Only); // A
        assert_eq!(result[1].status, MatchStatus::File2Only); // B
        assert_eq!(result[2].status, MatchStatus::Matched); // C
    }

    #[test]
    fn test_match_empty_files() {
        let matcher = EzMatch::new(0, 0, 1);
        let result = matcher.execute(&[], &[]);
        assert!(result.is_empty());
    }

    #[test]
    fn test_match_one_empty() {
        let records1 = vec![make_record("A", "only")];
        let matcher = EzMatch::new(0, 0, 1);
        let result = matcher.execute(&records1, &[]);
        assert_eq!(result.len(), 1);
        assert_eq!(result[0].status, MatchStatus::File1Only);
    }

    #[test]
    fn test_control_break_basic() {
        let mut cb = EzControlBreak::new("DEPT");

        let events = cb.process("SALES");
        assert_eq!(events.len(), 1); // BEFORE for first group
        assert_eq!(events[0].timing, BreakTiming::Before);

        let events = cb.process("SALES");
        assert!(events.is_empty()); // Same group, no break

        let events = cb.process("IT");
        assert_eq!(events.len(), 2); // AFTER for SALES, BEFORE for IT
        assert_eq!(events[0].timing, BreakTiming::After);
        assert_eq!(events[0].break_value, "SALES");
        assert_eq!(events[1].timing, BreakTiming::Before);
        assert_eq!(events[1].break_value, "IT");
    }

    #[test]
    fn test_control_break_accumulation() {
        let mut cb = EzControlBreak::new("DEPT");

        cb.process("SALES");
        cb.accumulate("SALARY", 50000.0);
        cb.accumulate("SALARY", 60000.0);

        let events = cb.process("IT");
        assert_eq!(events[0].timing, BreakTiming::After);
        let total = events[0].accumulators.get("SALARY").copied().unwrap_or(0.0);
        assert!((total - 110000.0).abs() < f64::EPSILON);
    }

    #[test]
    fn test_control_break_finalize() {
        let mut cb = EzControlBreak::new("DEPT");
        cb.process("SALES");
        cb.accumulate("SALARY", 50000.0);

        let final_event = cb.finalize().unwrap();
        assert_eq!(final_event.timing, BreakTiming::After);
        assert_eq!(final_event.break_value, "SALES");
        assert_eq!(final_event.record_count, 1);
    }

    #[test]
    fn test_control_break_finalize_empty() {
        let mut cb = EzControlBreak::new("DEPT");
        assert!(cb.finalize().is_none());
    }

    #[test]
    fn test_control_break_events() {
        let mut cb = EzControlBreak::new("DEPT");
        cb.process("A");
        cb.process("B");
        cb.finalize();
        assert!(cb.events().len() >= 3); // BEFORE A, AFTER A, BEFORE B, AFTER B
    }
}
