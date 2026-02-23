//! FOC-104: MODIFY/MAINTAIN Engine (5 stories).
//!
//! Batch MODIFY processing with FIXFORM, MATCH, ON MATCH/ON NOMATCH actions,
//! and interactive MAINTAIN with form-based input and transaction integrity.

use std::collections::HashMap;
use thiserror::Error;

use crate::table_engine::CellValue;

// ---------------------------------------------------------------------------
// Errors
// ---------------------------------------------------------------------------

#[derive(Debug, Error)]
pub enum ModifyError {
    #[error("match key not found: {0}")]
    KeyNotFound(String),
    #[error("validation failed: field {field}: {message}")]
    ValidationFailed { field: String, message: String },
    #[error("transaction error: {0}")]
    TransactionError(String),
    #[error("fixform parse error: {0}")]
    FixformError(String),
    #[error("no active transaction")]
    NoActiveTransaction,
}

// ---------------------------------------------------------------------------
// Types
// ---------------------------------------------------------------------------

/// A record in the master file.
pub type MasterRecord = HashMap<String, CellValue>;

/// FIXFORM field definition for parsing input records.
#[derive(Debug, Clone, PartialEq)]
pub struct FixformField {
    pub name: String,
    pub start: usize,
    pub length: usize,
}

/// What happens on MATCH / NOMATCH.
#[derive(Debug, Clone, PartialEq)]
pub enum MatchAction {
    Update,
    Include,
    Reject,
}

/// A validation rule for input data.
#[derive(Debug, Clone)]
pub struct ValidationRule {
    pub field: String,
    pub rule_type: ValidationType,
}

/// Types of validation.
#[derive(Debug, Clone)]
pub enum ValidationType {
    Required,
    MinValue(f64),
    MaxValue(f64),
    MinLength(usize),
    MaxLength(usize),
    Pattern(String),
}

/// MODIFY request configuration.
#[derive(Debug, Clone)]
pub struct ModifyRequest {
    pub file: String,
    pub fixform_fields: Vec<FixformField>,
    pub match_key: String,
    pub on_match: MatchAction,
    pub on_nomatch: MatchAction,
    pub validations: Vec<ValidationRule>,
}

/// Screen form definition for MAINTAIN.
#[derive(Debug, Clone)]
pub struct CrtformDef {
    pub name: String,
    pub fields: Vec<FormFieldDef>,
    pub title: Option<String>,
}

/// A field on a MAINTAIN form.
#[derive(Debug, Clone)]
pub struct FormFieldDef {
    pub name: String,
    pub label: String,
    pub row: usize,
    pub col: usize,
    pub width: usize,
    pub editable: bool,
}

/// Transaction log entry.
#[derive(Debug, Clone)]
pub struct TransactionLog {
    pub entries: Vec<TxEntry>,
}

/// A single transaction entry.
#[derive(Debug, Clone)]
pub struct TxEntry {
    pub action: TxAction,
    pub key: String,
    pub record: MasterRecord,
}

/// Transaction action type.
#[derive(Debug, Clone, PartialEq)]
pub enum TxAction {
    Insert,
    Update,
    Delete,
}

// ---------------------------------------------------------------------------
// MODIFY Engine
// ---------------------------------------------------------------------------

/// MODIFY/MAINTAIN processing engine.
pub struct ModifyEngine {
    master: Vec<MasterRecord>,
    match_key: String,
    pending_tx: Vec<TxEntry>,
    committed_log: TransactionLog,
    in_transaction: bool,
}

impl ModifyEngine {
    /// Create a new engine with the given master data and match key field.
    pub fn new(master: Vec<MasterRecord>, match_key: &str) -> Self {
        Self {
            master,
            match_key: match_key.to_string(),
            pending_tx: Vec::new(),
            committed_log: TransactionLog {
                entries: Vec::new(),
            },
            in_transaction: false,
        }
    }

    /// Access the current master data.
    pub fn master_data(&self) -> &[MasterRecord] {
        &self.master
    }

    /// Access the committed transaction log.
    pub fn transaction_log(&self) -> &TransactionLog {
        &self.committed_log
    }

    /// Begin a new transaction.
    pub fn begin_transaction(&mut self) {
        self.pending_tx.clear();
        self.in_transaction = true;
    }

    /// Commit the current transaction, applying all pending changes.
    pub fn commit(&mut self) -> Result<usize, ModifyError> {
        if !self.in_transaction {
            return Err(ModifyError::NoActiveTransaction);
        }
        let pending = std::mem::take(&mut self.pending_tx);
        let count = pending.len();
        for entry in &pending {
            match entry.action {
                TxAction::Insert => {
                    self.master.push(entry.record.clone());
                }
                TxAction::Update => {
                    if let Some(rec) = self.find_master_mut(&entry.key) {
                        for (k, v) in &entry.record {
                            rec.insert(k.clone(), v.clone());
                        }
                    }
                }
                TxAction::Delete => {
                    self.master
                        .retain(|r| Self::get_key_value(r, &self.match_key) != entry.key);
                }
            }
        }
        self.committed_log.entries.extend(pending);
        self.in_transaction = false;
        Ok(count)
    }

    /// Rollback the current transaction, discarding pending changes.
    pub fn rollback(&mut self) -> Result<usize, ModifyError> {
        if !self.in_transaction {
            return Err(ModifyError::NoActiveTransaction);
        }
        let count = self.pending_tx.len();
        self.pending_tx.clear();
        self.in_transaction = false;
        Ok(count)
    }

    /// Parse a fixed-format input record using FIXFORM field definitions.
    pub fn parse_fixform(
        line: &str,
        fields: &[FixformField],
    ) -> Result<MasterRecord, ModifyError> {
        let mut record = HashMap::new();
        for f in fields {
            let start = f.start.saturating_sub(1); // 1-based to 0-based
            let end = (start + f.length).min(line.len());
            if start >= line.len() {
                return Err(ModifyError::FixformError(format!(
                    "field {} start {} beyond record length {}",
                    f.name,
                    f.start,
                    line.len()
                )));
            }
            let value = line[start..end].trim().to_string();
            // Try to parse as number
            let cell = if let Ok(n) = value.parse::<f64>() {
                CellValue::Num(n)
            } else {
                CellValue::Str(value)
            };
            record.insert(f.name.clone(), cell);
        }
        Ok(record)
    }

    /// Process a batch MODIFY: match input records against master and apply actions.
    pub fn process_batch(
        &mut self,
        input_records: &[MasterRecord],
        on_match: &MatchAction,
        on_nomatch: &MatchAction,
        validations: &[ValidationRule],
    ) -> Result<BatchResult, ModifyError> {
        self.begin_transaction();
        let mut result = BatchResult::default();

        for input in input_records {
            // Validate
            if let Err(e) = Self::validate_record(input, validations) {
                result.rejected += 1;
                result.errors.push(format!("{e}"));
                continue;
            }

            let key_val = Self::get_key_value(input, &self.match_key);
            let found = self.find_master(&key_val).is_some();

            if found {
                match on_match {
                    MatchAction::Update => {
                        self.pending_tx.push(TxEntry {
                            action: TxAction::Update,
                            key: key_val,
                            record: input.clone(),
                        });
                        result.updated += 1;
                    }
                    MatchAction::Include => {
                        self.pending_tx.push(TxEntry {
                            action: TxAction::Insert,
                            key: key_val,
                            record: input.clone(),
                        });
                        result.inserted += 1;
                    }
                    MatchAction::Reject => {
                        result.rejected += 1;
                    }
                }
            } else {
                match on_nomatch {
                    MatchAction::Include => {
                        self.pending_tx.push(TxEntry {
                            action: TxAction::Insert,
                            key: key_val,
                            record: input.clone(),
                        });
                        result.inserted += 1;
                    }
                    MatchAction::Update => {
                        result.rejected += 1;
                        result
                            .errors
                            .push(format!("no match for key '{key_val}' to update"));
                    }
                    MatchAction::Reject => {
                        result.rejected += 1;
                    }
                }
            }
        }

        self.commit()?;
        Ok(result)
    }

    /// Validate a record against validation rules.
    fn validate_record(
        record: &MasterRecord,
        rules: &[ValidationRule],
    ) -> Result<(), ModifyError> {
        for rule in rules {
            let val = record.get(&rule.field);
            match &rule.rule_type {
                ValidationType::Required => {
                    if val.is_none() || matches!(val, Some(CellValue::Null)) {
                        return Err(ModifyError::ValidationFailed {
                            field: rule.field.clone(),
                            message: "required field is missing".into(),
                        });
                    }
                }
                ValidationType::MinValue(min) => {
                    let n = val.map_or(0.0, |v| v.as_num());
                    if n < *min {
                        return Err(ModifyError::ValidationFailed {
                            field: rule.field.clone(),
                            message: format!("value {n} below minimum {min}"),
                        });
                    }
                }
                ValidationType::MaxValue(max) => {
                    let n = val.map_or(0.0, |v| v.as_num());
                    if n > *max {
                        return Err(ModifyError::ValidationFailed {
                            field: rule.field.clone(),
                            message: format!("value {n} above maximum {max}"),
                        });
                    }
                }
                ValidationType::MinLength(min) => {
                    let s = val.map_or(String::new(), |v| v.as_str());
                    if s.len() < *min {
                        return Err(ModifyError::ValidationFailed {
                            field: rule.field.clone(),
                            message: format!("length {} below minimum {min}", s.len()),
                        });
                    }
                }
                ValidationType::MaxLength(max) => {
                    let s = val.map_or(String::new(), |v| v.as_str());
                    if s.len() > *max {
                        return Err(ModifyError::ValidationFailed {
                            field: rule.field.clone(),
                            message: format!("length {} above maximum {max}", s.len()),
                        });
                    }
                }
                ValidationType::Pattern(pat) => {
                    let s = val.map_or(String::new(), |v| v.as_str());
                    // Simple pattern: just check if the value contains the pattern
                    if !s.contains(pat.as_str()) {
                        return Err(ModifyError::ValidationFailed {
                            field: rule.field.clone(),
                            message: format!("value does not match pattern '{pat}'"),
                        });
                    }
                }
            }
        }
        Ok(())
    }

    /// MAINTAIN: simulate interactive insert.
    pub fn maintain_insert(&mut self, record: MasterRecord) -> Result<(), ModifyError> {
        if !self.in_transaction {
            self.begin_transaction();
        }
        let key = Self::get_key_value(&record, &self.match_key);
        self.pending_tx.push(TxEntry {
            action: TxAction::Insert,
            key,
            record,
        });
        Ok(())
    }

    /// MAINTAIN: simulate interactive update.
    pub fn maintain_update(
        &mut self,
        key_value: &str,
        updates: MasterRecord,
    ) -> Result<(), ModifyError> {
        if !self.in_transaction {
            self.begin_transaction();
        }
        if self.find_master(key_value).is_none() {
            return Err(ModifyError::KeyNotFound(key_value.to_string()));
        }
        self.pending_tx.push(TxEntry {
            action: TxAction::Update,
            key: key_value.to_string(),
            record: updates,
        });
        Ok(())
    }

    /// MAINTAIN: simulate interactive delete.
    pub fn maintain_delete(&mut self, key_value: &str) -> Result<(), ModifyError> {
        if !self.in_transaction {
            self.begin_transaction();
        }
        if self.find_master(key_value).is_none() {
            return Err(ModifyError::KeyNotFound(key_value.to_string()));
        }
        self.pending_tx.push(TxEntry {
            action: TxAction::Delete,
            key: key_value.to_string(),
            record: HashMap::new(),
        });
        Ok(())
    }

    fn find_master(&self, key_value: &str) -> Option<&MasterRecord> {
        self.master
            .iter()
            .find(|r| Self::get_key_value(r, &self.match_key) == key_value)
    }

    fn find_master_mut(&mut self, key_value: &str) -> Option<&mut MasterRecord> {
        let mk = self.match_key.clone();
        self.master
            .iter_mut()
            .find(|r| Self::get_key_value(r, &mk) == key_value)
    }

    fn get_key_value(record: &MasterRecord, key_field: &str) -> String {
        record
            .get(key_field)
            .map_or(String::new(), |v| v.as_str())
    }
}

/// Results of a batch MODIFY operation.
#[derive(Debug, Default)]
pub struct BatchResult {
    pub inserted: usize,
    pub updated: usize,
    pub rejected: usize,
    pub errors: Vec<String>,
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    fn sample_master() -> Vec<MasterRecord> {
        vec![
            HashMap::from([
                ("EMPID".into(), CellValue::Str("E001".into())),
                ("NAME".into(), CellValue::Str("Alice".into())),
                ("SALARY".into(), CellValue::Num(60000.0)),
            ]),
            HashMap::from([
                ("EMPID".into(), CellValue::Str("E002".into())),
                ("NAME".into(), CellValue::Str("Bob".into())),
                ("SALARY".into(), CellValue::Num(55000.0)),
            ]),
            HashMap::from([
                ("EMPID".into(), CellValue::Str("E003".into())),
                ("NAME".into(), CellValue::Str("Carol".into())),
                ("SALARY".into(), CellValue::Num(70000.0)),
            ]),
        ]
    }

    #[test]
    fn test_fixform_parsing() {
        let fields = vec![
            FixformField {
                name: "EMPID".into(),
                start: 1,
                length: 4,
            },
            FixformField {
                name: "NAME".into(),
                start: 5,
                length: 10,
            },
            FixformField {
                name: "SALARY".into(),
                start: 15,
                length: 8,
            },
        ];
        let line = "E004Alice     65000.00";
        let rec = ModifyEngine::parse_fixform(line, &fields).unwrap();
        assert_eq!(rec.get("EMPID").unwrap().as_str(), "E004");
        assert_eq!(rec.get("NAME").unwrap().as_str(), "Alice");
    }

    #[test]
    fn test_fixform_numeric_field() {
        let fields = vec![FixformField {
            name: "AMT".into(),
            start: 1,
            length: 6,
        }];
        let line = " 12345";
        let rec = ModifyEngine::parse_fixform(line, &fields).unwrap();
        assert!((rec.get("AMT").unwrap().as_num() - 12345.0).abs() < f64::EPSILON);
    }

    #[test]
    fn test_fixform_error_beyond_length() {
        let fields = vec![FixformField {
            name: "X".into(),
            start: 100,
            length: 5,
        }];
        let line = "short";
        assert!(ModifyEngine::parse_fixform(line, &fields).is_err());
    }

    #[test]
    fn test_batch_modify_update_on_match() {
        let mut engine = ModifyEngine::new(sample_master(), "EMPID");
        let input = vec![HashMap::from([
            ("EMPID".into(), CellValue::Str("E001".into())),
            ("SALARY".into(), CellValue::Num(65000.0)),
        ])];
        let result = engine
            .process_batch(&input, &MatchAction::Update, &MatchAction::Reject, &[])
            .unwrap();
        assert_eq!(result.updated, 1);
        assert_eq!(result.inserted, 0);
        // Check master was updated
        let alice = engine
            .master_data()
            .iter()
            .find(|r| r.get("EMPID").unwrap().as_str() == "E001")
            .unwrap();
        assert!((alice.get("SALARY").unwrap().as_num() - 65000.0).abs() < f64::EPSILON);
    }

    #[test]
    fn test_batch_modify_include_on_nomatch() {
        let mut engine = ModifyEngine::new(sample_master(), "EMPID");
        let input = vec![HashMap::from([
            ("EMPID".into(), CellValue::Str("E004".into())),
            ("NAME".into(), CellValue::Str("Dave".into())),
            ("SALARY".into(), CellValue::Num(50000.0)),
        ])];
        let result = engine
            .process_batch(&input, &MatchAction::Reject, &MatchAction::Include, &[])
            .unwrap();
        assert_eq!(result.inserted, 1);
        assert_eq!(engine.master_data().len(), 4);
    }

    #[test]
    fn test_batch_modify_reject() {
        let mut engine = ModifyEngine::new(sample_master(), "EMPID");
        let input = vec![HashMap::from([
            ("EMPID".into(), CellValue::Str("E999".into())),
            ("NAME".into(), CellValue::Str("Unknown".into())),
        ])];
        let result = engine
            .process_batch(&input, &MatchAction::Update, &MatchAction::Reject, &[])
            .unwrap();
        assert_eq!(result.rejected, 1);
        assert_eq!(engine.master_data().len(), 3); // unchanged
    }

    #[test]
    fn test_validation_required() {
        let mut engine = ModifyEngine::new(sample_master(), "EMPID");
        let rules = vec![ValidationRule {
            field: "NAME".into(),
            rule_type: ValidationType::Required,
        }];
        let input = vec![HashMap::from([
            ("EMPID".into(), CellValue::Str("E004".into())),
            // NAME is missing
        ])];
        let result = engine
            .process_batch(&input, &MatchAction::Reject, &MatchAction::Include, &rules)
            .unwrap();
        assert_eq!(result.rejected, 1);
        assert!(!result.errors.is_empty());
    }

    #[test]
    fn test_validation_min_value() {
        let mut engine = ModifyEngine::new(sample_master(), "EMPID");
        let rules = vec![ValidationRule {
            field: "SALARY".into(),
            rule_type: ValidationType::MinValue(30000.0),
        }];
        let input = vec![HashMap::from([
            ("EMPID".into(), CellValue::Str("E004".into())),
            ("NAME".into(), CellValue::Str("Low".into())),
            ("SALARY".into(), CellValue::Num(10000.0)),
        ])];
        let result = engine
            .process_batch(&input, &MatchAction::Reject, &MatchAction::Include, &rules)
            .unwrap();
        assert_eq!(result.rejected, 1);
    }

    #[test]
    fn test_validation_max_length() {
        let mut engine = ModifyEngine::new(sample_master(), "EMPID");
        let rules = vec![ValidationRule {
            field: "EMPID".into(),
            rule_type: ValidationType::MaxLength(4),
        }];
        let input = vec![HashMap::from([
            ("EMPID".into(), CellValue::Str("E00004".into())),
            ("NAME".into(), CellValue::Str("TooLong".into())),
        ])];
        let result = engine
            .process_batch(&input, &MatchAction::Reject, &MatchAction::Include, &rules)
            .unwrap();
        assert_eq!(result.rejected, 1);
    }

    #[test]
    fn test_transaction_commit() {
        let mut engine = ModifyEngine::new(sample_master(), "EMPID");
        engine.begin_transaction();
        engine
            .maintain_insert(HashMap::from([
                ("EMPID".into(), CellValue::Str("E004".into())),
                ("NAME".into(), CellValue::Str("Dave".into())),
            ]))
            .unwrap();
        let count = engine.commit().unwrap();
        assert_eq!(count, 1);
        assert_eq!(engine.master_data().len(), 4);
    }

    #[test]
    fn test_transaction_rollback() {
        let mut engine = ModifyEngine::new(sample_master(), "EMPID");
        engine.begin_transaction();
        engine
            .maintain_insert(HashMap::from([
                ("EMPID".into(), CellValue::Str("E004".into())),
                ("NAME".into(), CellValue::Str("Dave".into())),
            ]))
            .unwrap();
        let count = engine.rollback().unwrap();
        assert_eq!(count, 1);
        assert_eq!(engine.master_data().len(), 3); // unchanged
    }

    #[test]
    fn test_commit_without_transaction() {
        let mut engine = ModifyEngine::new(sample_master(), "EMPID");
        assert!(engine.commit().is_err());
    }

    #[test]
    fn test_maintain_update() {
        let mut engine = ModifyEngine::new(sample_master(), "EMPID");
        engine.begin_transaction();
        engine
            .maintain_update(
                "E002",
                HashMap::from([("SALARY".into(), CellValue::Num(60000.0))]),
            )
            .unwrap();
        engine.commit().unwrap();
        let bob = engine
            .master_data()
            .iter()
            .find(|r| r.get("EMPID").unwrap().as_str() == "E002")
            .unwrap();
        assert!((bob.get("SALARY").unwrap().as_num() - 60000.0).abs() < f64::EPSILON);
    }

    #[test]
    fn test_maintain_update_not_found() {
        let mut engine = ModifyEngine::new(sample_master(), "EMPID");
        engine.begin_transaction();
        let result = engine.maintain_update(
            "E999",
            HashMap::from([("SALARY".into(), CellValue::Num(1.0))]),
        );
        assert!(result.is_err());
    }

    #[test]
    fn test_maintain_delete() {
        let mut engine = ModifyEngine::new(sample_master(), "EMPID");
        engine.begin_transaction();
        engine.maintain_delete("E003").unwrap();
        engine.commit().unwrap();
        assert_eq!(engine.master_data().len(), 2);
    }

    #[test]
    fn test_maintain_delete_not_found() {
        let mut engine = ModifyEngine::new(sample_master(), "EMPID");
        engine.begin_transaction();
        assert!(engine.maintain_delete("E999").is_err());
    }

    #[test]
    fn test_transaction_log() {
        let mut engine = ModifyEngine::new(sample_master(), "EMPID");
        engine.begin_transaction();
        engine
            .maintain_insert(HashMap::from([
                ("EMPID".into(), CellValue::Str("E004".into())),
                ("NAME".into(), CellValue::Str("Dave".into())),
            ]))
            .unwrap();
        engine.commit().unwrap();
        assert_eq!(engine.transaction_log().entries.len(), 1);
        assert_eq!(engine.transaction_log().entries[0].action, TxAction::Insert);
    }

    #[test]
    fn test_multiple_batch_operations() {
        let mut engine = ModifyEngine::new(sample_master(), "EMPID");
        let input = vec![
            HashMap::from([
                ("EMPID".into(), CellValue::Str("E001".into())),
                ("SALARY".into(), CellValue::Num(65000.0)),
            ]),
            HashMap::from([
                ("EMPID".into(), CellValue::Str("E004".into())),
                ("NAME".into(), CellValue::Str("Dave".into())),
                ("SALARY".into(), CellValue::Num(50000.0)),
            ]),
        ];
        let result = engine
            .process_batch(&input, &MatchAction::Update, &MatchAction::Include, &[])
            .unwrap();
        assert_eq!(result.updated, 1);
        assert_eq!(result.inserted, 1);
        assert_eq!(engine.master_data().len(), 4);
    }

    #[test]
    fn test_crtform_def() {
        let form = CrtformDef {
            name: "EMPFORM".into(),
            fields: vec![
                FormFieldDef {
                    name: "EMPID".into(),
                    label: "Employee ID".into(),
                    row: 1,
                    col: 1,
                    width: 6,
                    editable: false,
                },
                FormFieldDef {
                    name: "NAME".into(),
                    label: "Name".into(),
                    row: 2,
                    col: 1,
                    width: 30,
                    editable: true,
                },
            ],
            title: Some("Employee Form".into()),
        };
        assert_eq!(form.name, "EMPFORM");
        assert_eq!(form.fields.len(), 2);
        assert!(!form.fields[0].editable);
        assert!(form.fields[1].editable);
    }

    #[test]
    fn test_validation_pattern() {
        let mut engine = ModifyEngine::new(vec![], "ID");
        let rules = vec![ValidationRule {
            field: "EMAIL".into(),
            rule_type: ValidationType::Pattern("@".into()),
        }];
        let input = vec![HashMap::from([
            ("ID".into(), CellValue::Str("1".into())),
            ("EMAIL".into(), CellValue::Str("invalid".into())),
        ])];
        let result = engine
            .process_batch(&input, &MatchAction::Reject, &MatchAction::Include, &rules)
            .unwrap();
        assert_eq!(result.rejected, 1);
    }

    #[test]
    fn test_validation_min_length() {
        let mut engine = ModifyEngine::new(vec![], "ID");
        let rules = vec![ValidationRule {
            field: "CODE".into(),
            rule_type: ValidationType::MinLength(3),
        }];
        let input = vec![HashMap::from([
            ("ID".into(), CellValue::Str("1".into())),
            ("CODE".into(), CellValue::Str("AB".into())),
        ])];
        let result = engine
            .process_batch(&input, &MatchAction::Reject, &MatchAction::Include, &rules)
            .unwrap();
        assert_eq!(result.rejected, 1);
    }
}
