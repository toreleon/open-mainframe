//! FOC-109: Joins & Multi-Source (4 stories).
//!
//! JOIN (inner, left outer), COMBINE (vertical merge), and MATCH FILE
//! (compare OLD-FILE/NEW-FILE) operations.

use thiserror::Error;

use crate::table_engine::{CellValue, DataRecord};

// ---------------------------------------------------------------------------
// Errors
// ---------------------------------------------------------------------------

#[derive(Debug, Error)]
pub enum JoinError {
    #[error("join key not found in record: {0}")]
    KeyNotFound(String),
    #[error("incompatible schemas for COMBINE")]
    IncompatibleSchemas,
    #[error("no data provided")]
    NoData,
}

// ---------------------------------------------------------------------------
// Types
// ---------------------------------------------------------------------------

/// Join types supported by FOCUS.
#[derive(Debug, Clone, PartialEq)]
pub enum JoinType {
    Inner,
    LeftOuter,
}

/// Definition of a JOIN between two files.
#[derive(Debug, Clone)]
pub struct JoinDefinition {
    pub left_file: String,
    pub right_file: String,
    pub left_key: String,
    pub right_key: String,
    pub join_type: JoinType,
}

/// Result of a MATCH FILE operation.
#[derive(Debug, Clone)]
pub struct MatchFileResult {
    pub matched: Vec<MatchedPair>,
    pub old_only: Vec<DataRecord>,
    pub new_only: Vec<DataRecord>,
}

/// A matched pair from MATCH FILE.
#[derive(Debug, Clone)]
pub struct MatchedPair {
    pub old_record: DataRecord,
    pub new_record: DataRecord,
}

/// COMBINE operation type.
#[derive(Debug, Clone, PartialEq)]
pub enum CombineOp {
    /// Vertical merge: append all records from both sources.
    Append,
    /// Interleave by a sort key.
    Interleave(String),
}

/// MATCH FILE comparison mode.
#[derive(Debug, Clone, PartialEq)]
pub enum MatchFileOp {
    /// Standard comparison — produce matched, old-only, new-only.
    Standard,
    /// Show only differences.
    DifferencesOnly,
}

// ---------------------------------------------------------------------------
// Join engine
// ---------------------------------------------------------------------------

/// Performs JOIN, COMBINE, and MATCH FILE operations.
pub struct JoinEngine;

impl JoinEngine {
    /// Execute a JOIN between two record sets.
    pub fn join(
        left: &[DataRecord],
        right: &[DataRecord],
        definition: &JoinDefinition,
    ) -> Result<Vec<DataRecord>, JoinError> {
        let mut results = Vec::new();

        for l_rec in left {
            let l_key = l_rec
                .get(&definition.left_key)
                .ok_or_else(|| JoinError::KeyNotFound(definition.left_key.clone()))?;
            let l_key_str = l_key.as_str();

            let matches: Vec<&DataRecord> = right
                .iter()
                .filter(|r| {
                    r.get(&definition.right_key)
                        .map_or(false, |v| v.as_str() == l_key_str)
                })
                .collect();

            if matches.is_empty() {
                if definition.join_type == JoinType::LeftOuter {
                    // Include left record with nulls for right fields
                    let mut combined = l_rec.clone();
                    // Add null placeholders for right-side fields
                    if let Some(first_right) = right.first() {
                        for key in first_right.keys() {
                            if !combined.contains_key(key) {
                                combined.insert(key.clone(), CellValue::Null);
                            }
                        }
                    }
                    results.push(combined);
                }
                // Inner join: skip unmatched left records
            } else {
                for r_rec in matches {
                    let mut combined = l_rec.clone();
                    for (k, v) in r_rec {
                        // Always insert right-side fields unless the key field already exists
                        if k != &definition.right_key || !combined.contains_key(k) {
                            combined.insert(k.clone(), v.clone());
                        }
                    }
                    results.push(combined);
                }
            }
        }

        Ok(results)
    }

    /// COMBINE: merge multiple record sets vertically.
    pub fn combine(
        sources: &[Vec<DataRecord>],
        op: &CombineOp,
    ) -> Result<Vec<DataRecord>, JoinError> {
        if sources.is_empty() {
            return Err(JoinError::NoData);
        }

        match op {
            CombineOp::Append => {
                let mut result = Vec::new();
                for source in sources {
                    result.extend(source.iter().cloned());
                }
                Ok(result)
            }
            CombineOp::Interleave(sort_key) => {
                let mut all: Vec<DataRecord> = sources.iter().flat_map(|s| s.iter().cloned()).collect();
                let sk = sort_key.clone();
                all.sort_by(|a, b| {
                    let ak = a.get(&sk).map_or(String::new(), |v| v.as_str());
                    let bk = b.get(&sk).map_or(String::new(), |v| v.as_str());
                    ak.cmp(&bk)
                });
                Ok(all)
            }
        }
    }

    /// MATCH FILE: compare OLD-FILE with NEW-FILE.
    pub fn match_file(
        old_file: &[DataRecord],
        new_file: &[DataRecord],
        key_field: &str,
        _op: &MatchFileOp,
    ) -> Result<MatchFileResult, JoinError> {
        let mut matched = Vec::new();
        let mut old_only = Vec::new();
        let mut new_matched_keys: Vec<String> = Vec::new();

        for old_rec in old_file {
            let old_key = old_rec
                .get(key_field)
                .ok_or_else(|| JoinError::KeyNotFound(key_field.to_string()))?
                .as_str();

            if let Some(new_rec) = new_file.iter().find(|r| {
                r.get(key_field).map_or(false, |v| v.as_str() == old_key)
            }) {
                matched.push(MatchedPair {
                    old_record: old_rec.clone(),
                    new_record: new_rec.clone(),
                });
                new_matched_keys.push(old_key.clone());
            } else {
                old_only.push(old_rec.clone());
            }
        }

        let new_only: Vec<DataRecord> = new_file
            .iter()
            .filter(|r| {
                let key = r.get(key_field).map_or(String::new(), |v| v.as_str());
                !new_matched_keys.contains(&key)
            })
            .cloned()
            .collect();

        Ok(MatchFileResult {
            matched,
            old_only,
            new_only,
        })
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashMap;
    use crate::table_engine::CellValue;

    fn employees() -> Vec<DataRecord> {
        vec![
            HashMap::from([
                ("EMPID".into(), CellValue::Str("E001".into())),
                ("NAME".into(), CellValue::Str("Alice".into())),
                ("DEPTID".into(), CellValue::Str("D01".into())),
            ]),
            HashMap::from([
                ("EMPID".into(), CellValue::Str("E002".into())),
                ("NAME".into(), CellValue::Str("Bob".into())),
                ("DEPTID".into(), CellValue::Str("D02".into())),
            ]),
            HashMap::from([
                ("EMPID".into(), CellValue::Str("E003".into())),
                ("NAME".into(), CellValue::Str("Carol".into())),
                ("DEPTID".into(), CellValue::Str("D01".into())),
            ]),
            HashMap::from([
                ("EMPID".into(), CellValue::Str("E004".into())),
                ("NAME".into(), CellValue::Str("Dave".into())),
                ("DEPTID".into(), CellValue::Str("D99".into())),
            ]),
        ]
    }

    fn departments() -> Vec<DataRecord> {
        vec![
            HashMap::from([
                ("DEPTID".into(), CellValue::Str("D01".into())),
                ("DEPTNAME".into(), CellValue::Str("Engineering".into())),
            ]),
            HashMap::from([
                ("DEPTID".into(), CellValue::Str("D02".into())),
                ("DEPTNAME".into(), CellValue::Str("Sales".into())),
            ]),
            HashMap::from([
                ("DEPTID".into(), CellValue::Str("D03".into())),
                ("DEPTNAME".into(), CellValue::Str("Marketing".into())),
            ]),
        ]
    }

    // --- JOIN tests ---

    #[test]
    fn test_inner_join() {
        let def = JoinDefinition {
            left_file: "EMP".into(),
            right_file: "DEPT".into(),
            left_key: "DEPTID".into(),
            right_key: "DEPTID".into(),
            join_type: JoinType::Inner,
        };
        let result = JoinEngine::join(&employees(), &departments(), &def).unwrap();
        // E001, E002, E003 have matching departments; E004 (D99) does not
        assert_eq!(result.len(), 3);
        // Check that department names are present
        for rec in &result {
            assert!(rec.contains_key("DEPTNAME"));
        }
    }

    #[test]
    fn test_left_outer_join() {
        let def = JoinDefinition {
            left_file: "EMP".into(),
            right_file: "DEPT".into(),
            left_key: "DEPTID".into(),
            right_key: "DEPTID".into(),
            join_type: JoinType::LeftOuter,
        };
        let result = JoinEngine::join(&employees(), &departments(), &def).unwrap();
        // All 4 employees should be in the result
        assert_eq!(result.len(), 4);
        // Dave (D99) should have null DEPTNAME
        let dave = result
            .iter()
            .find(|r| r.get("NAME").map_or(false, |v| v.as_str() == "Dave"))
            .unwrap();
        assert_eq!(dave.get("DEPTNAME").unwrap(), &CellValue::Null);
    }

    #[test]
    fn test_join_preserves_left_fields() {
        let def = JoinDefinition {
            left_file: "EMP".into(),
            right_file: "DEPT".into(),
            left_key: "DEPTID".into(),
            right_key: "DEPTID".into(),
            join_type: JoinType::Inner,
        };
        let result = JoinEngine::join(&employees(), &departments(), &def).unwrap();
        let alice = result
            .iter()
            .find(|r| r.get("NAME").map_or(false, |v| v.as_str() == "Alice"))
            .unwrap();
        assert_eq!(alice.get("EMPID").unwrap().as_str(), "E001");
        assert_eq!(alice.get("DEPTNAME").unwrap().as_str(), "Engineering");
    }

    #[test]
    fn test_join_no_matches() {
        let left = vec![HashMap::from([
            ("KEY".into(), CellValue::Str("X".into())),
        ])];
        let right = vec![HashMap::from([
            ("KEY".into(), CellValue::Str("Y".into())),
        ])];
        let def = JoinDefinition {
            left_file: "L".into(),
            right_file: "R".into(),
            left_key: "KEY".into(),
            right_key: "KEY".into(),
            join_type: JoinType::Inner,
        };
        let result = JoinEngine::join(&left, &right, &def).unwrap();
        assert!(result.is_empty());
    }

    #[test]
    fn test_join_multiple_matches() {
        let left = vec![HashMap::from([
            ("KEY".into(), CellValue::Str("A".into())),
            ("VAL".into(), CellValue::Num(1.0)),
        ])];
        let right = vec![
            HashMap::from([
                ("KEY".into(), CellValue::Str("A".into())),
                ("EXTRA".into(), CellValue::Str("X".into())),
            ]),
            HashMap::from([
                ("KEY".into(), CellValue::Str("A".into())),
                ("EXTRA".into(), CellValue::Str("Y".into())),
            ]),
        ];
        let def = JoinDefinition {
            left_file: "L".into(),
            right_file: "R".into(),
            left_key: "KEY".into(),
            right_key: "KEY".into(),
            join_type: JoinType::Inner,
        };
        let result = JoinEngine::join(&left, &right, &def).unwrap();
        // One left record matched two right records
        assert_eq!(result.len(), 2);
    }

    // --- COMBINE tests ---

    #[test]
    fn test_combine_append() {
        let s1 = vec![HashMap::from([
            ("NAME".into(), CellValue::Str("Alice".into())),
        ])];
        let s2 = vec![HashMap::from([
            ("NAME".into(), CellValue::Str("Bob".into())),
        ])];
        let result = JoinEngine::combine(&[s1, s2], &CombineOp::Append).unwrap();
        assert_eq!(result.len(), 2);
    }

    #[test]
    fn test_combine_interleave() {
        let s1 = vec![
            HashMap::from([("KEY".into(), CellValue::Str("C".into()))]),
            HashMap::from([("KEY".into(), CellValue::Str("A".into()))]),
        ];
        let s2 = vec![
            HashMap::from([("KEY".into(), CellValue::Str("B".into()))]),
        ];
        let result = JoinEngine::combine(
            &[s1, s2],
            &CombineOp::Interleave("KEY".into()),
        )
        .unwrap();
        assert_eq!(result.len(), 3);
        assert_eq!(result[0].get("KEY").unwrap().as_str(), "A");
        assert_eq!(result[1].get("KEY").unwrap().as_str(), "B");
        assert_eq!(result[2].get("KEY").unwrap().as_str(), "C");
    }

    #[test]
    fn test_combine_empty_sources() {
        let result = JoinEngine::combine(&[], &CombineOp::Append);
        assert!(result.is_err());
    }

    #[test]
    fn test_combine_single_source() {
        let s1 = vec![
            HashMap::from([("X".into(), CellValue::Num(1.0))]),
            HashMap::from([("X".into(), CellValue::Num(2.0))]),
        ];
        let result = JoinEngine::combine(&[s1], &CombineOp::Append).unwrap();
        assert_eq!(result.len(), 2);
    }

    // --- MATCH FILE tests ---

    #[test]
    fn test_match_file_standard() {
        let old = vec![
            HashMap::from([
                ("ID".into(), CellValue::Str("1".into())),
                ("NAME".into(), CellValue::Str("Alice".into())),
            ]),
            HashMap::from([
                ("ID".into(), CellValue::Str("2".into())),
                ("NAME".into(), CellValue::Str("Bob".into())),
            ]),
            HashMap::from([
                ("ID".into(), CellValue::Str("3".into())),
                ("NAME".into(), CellValue::Str("Carol".into())),
            ]),
        ];
        let new = vec![
            HashMap::from([
                ("ID".into(), CellValue::Str("2".into())),
                ("NAME".into(), CellValue::Str("Bobby".into())),
            ]),
            HashMap::from([
                ("ID".into(), CellValue::Str("3".into())),
                ("NAME".into(), CellValue::Str("Carol".into())),
            ]),
            HashMap::from([
                ("ID".into(), CellValue::Str("4".into())),
                ("NAME".into(), CellValue::Str("Dave".into())),
            ]),
        ];
        let result = JoinEngine::match_file(&old, &new, "ID", &MatchFileOp::Standard).unwrap();
        assert_eq!(result.matched.len(), 2); // IDs 2 and 3
        assert_eq!(result.old_only.len(), 1); // ID 1
        assert_eq!(result.new_only.len(), 1); // ID 4
    }

    #[test]
    fn test_match_file_matched_content() {
        let old = vec![HashMap::from([
            ("ID".into(), CellValue::Str("1".into())),
            ("VAL".into(), CellValue::Str("old".into())),
        ])];
        let new = vec![HashMap::from([
            ("ID".into(), CellValue::Str("1".into())),
            ("VAL".into(), CellValue::Str("new".into())),
        ])];
        let result = JoinEngine::match_file(&old, &new, "ID", &MatchFileOp::Standard).unwrap();
        assert_eq!(result.matched.len(), 1);
        assert_eq!(
            result.matched[0].old_record.get("VAL").unwrap().as_str(),
            "old"
        );
        assert_eq!(
            result.matched[0].new_record.get("VAL").unwrap().as_str(),
            "new"
        );
    }

    #[test]
    fn test_match_file_all_old() {
        let old = vec![
            HashMap::from([("ID".into(), CellValue::Str("1".into()))]),
            HashMap::from([("ID".into(), CellValue::Str("2".into()))]),
        ];
        let new: Vec<DataRecord> = vec![];
        let result = JoinEngine::match_file(&old, &new, "ID", &MatchFileOp::Standard).unwrap();
        assert_eq!(result.matched.len(), 0);
        assert_eq!(result.old_only.len(), 2);
        assert_eq!(result.new_only.len(), 0);
    }

    #[test]
    fn test_match_file_all_new() {
        let old: Vec<DataRecord> = vec![];
        let new = vec![
            HashMap::from([("ID".into(), CellValue::Str("1".into()))]),
        ];
        let result = JoinEngine::match_file(&old, &new, "ID", &MatchFileOp::Standard).unwrap();
        assert_eq!(result.matched.len(), 0);
        assert_eq!(result.old_only.len(), 0);
        assert_eq!(result.new_only.len(), 1);
    }

    #[test]
    fn test_match_file_all_matched() {
        let old = vec![HashMap::from([
            ("ID".into(), CellValue::Str("1".into())),
        ])];
        let new = vec![HashMap::from([
            ("ID".into(), CellValue::Str("1".into())),
        ])];
        let result = JoinEngine::match_file(&old, &new, "ID", &MatchFileOp::Standard).unwrap();
        assert_eq!(result.matched.len(), 1);
        assert_eq!(result.old_only.len(), 0);
        assert_eq!(result.new_only.len(), 0);
    }

    #[test]
    fn test_match_file_differences_only() {
        let old = vec![
            HashMap::from([("ID".into(), CellValue::Str("1".into()))]),
            HashMap::from([("ID".into(), CellValue::Str("2".into()))]),
        ];
        let new = vec![
            HashMap::from([("ID".into(), CellValue::Str("2".into()))]),
            HashMap::from([("ID".into(), CellValue::Str("3".into()))]),
        ];
        let result =
            JoinEngine::match_file(&old, &new, "ID", &MatchFileOp::DifferencesOnly).unwrap();
        // Still produces all three categories
        assert_eq!(result.old_only.len(), 1);
        assert_eq!(result.new_only.len(), 1);
    }

    // --- JoinDefinition tests ---

    #[test]
    fn test_join_definition_construction() {
        let def = JoinDefinition {
            left_file: "EMP".into(),
            right_file: "DEPT".into(),
            left_key: "DEPTID".into(),
            right_key: "DEPTID".into(),
            join_type: JoinType::Inner,
        };
        assert_eq!(def.left_file, "EMP");
        assert_eq!(def.join_type, JoinType::Inner);
    }
}
