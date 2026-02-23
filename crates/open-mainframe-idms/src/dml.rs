//! IDMS-102: Navigational DML Engine (7 stories).
//!
//! Implements the IDMS Data Manipulation Language operations: FIND, GET,
//! STORE, MODIFY, ERASE, CONNECT, and DISCONNECT.  These operate on
//! record instances within the context of a run-unit, using currency
//! indicators to track the current position.

use std::collections::HashMap;

use crate::codasyl::{CodasylSchema, FieldType};
use crate::currency::CurrencyTable;

// ---------------------------------------------------------------------------
//  Usage mode for READY
// ---------------------------------------------------------------------------

/// Area usage mode for the READY statement.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UsageMode {
    /// Read-only access, shared with other run-units.
    Retrieval,
    /// Read-write access, shared with other retrieval users.
    Update,
    /// Read-write access, exclusive to this run-unit.
    Exclusive,
    /// Read-only access, no other updates allowed.
    ProtectedRetrieval,
    /// Read-write access, no other updates allowed.
    ProtectedUpdate,
}

// ---------------------------------------------------------------------------
//  Find mode
// ---------------------------------------------------------------------------

/// Mode used for FIND operations.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FindMode {
    /// FIND FIRST record-name WITHIN set-name.
    First,
    /// FIND LAST record-name WITHIN set-name.
    Last,
    /// FIND NEXT record-name WITHIN set-name.
    Next,
    /// FIND PRIOR record-name WITHIN set-name.
    Prior,
    /// FIND record-name WITHIN set-name USING CALC.
    Calc,
    /// FIND record-name WITHIN area-name.
    WithinArea,
    /// FIND record-name BY DBKEY.
    Dbkey(u64),
}

// ---------------------------------------------------------------------------
//  Record instance
// ---------------------------------------------------------------------------

/// A stored record instance with field values and a database key.
#[derive(Debug, Clone, PartialEq)]
pub struct RecordInstance {
    /// Database key (unique identifier for this instance).
    pub dbkey: u64,
    /// Record type name.
    pub record_type: String,
    /// Field values keyed by field name.
    pub fields: HashMap<String, FieldValue>,
}

/// Runtime value of a record field.
#[derive(Debug, Clone, PartialEq)]
pub enum FieldValue {
    /// Character string.
    Str(String),
    /// 32-bit integer.
    Int(i32),
    /// 64-bit integer.
    Long(i64),
    /// Decimal stored as string representation.
    Decimal(String),
    /// Binary data.
    Binary(Vec<u8>),
    /// Null / not set.
    Null,
}

impl RecordInstance {
    /// Create a new record instance.
    pub fn new(dbkey: u64, record_type: &str) -> Self {
        Self {
            dbkey,
            record_type: record_type.to_uppercase(),
            fields: HashMap::new(),
        }
    }

    /// Set a field value.
    pub fn set_field(&mut self, name: &str, value: FieldValue) {
        self.fields.insert(name.to_uppercase(), value);
    }

    /// Get a field value.
    pub fn get_field(&self, name: &str) -> Option<&FieldValue> {
        self.fields.get(&name.to_uppercase())
    }
}

// ---------------------------------------------------------------------------
//  DML status codes
// ---------------------------------------------------------------------------

/// Status codes returned by DML operations (modelled after IDMS status codes).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StatusCode {
    /// Successful completion (0000).
    Success,
    /// End of set (0307).
    EndOfSet,
    /// Record not found (0326).
    RecordNotFound,
    /// Duplicate record (1207).
    DuplicateRecord,
    /// Currency not established (0306).
    NoCurrency,
    /// Area not open (0069).
    AreaNotOpen,
}

impl StatusCode {
    /// IDMS-style four-digit code.
    pub fn code(self) -> &'static str {
        match self {
            Self::Success => "0000",
            Self::EndOfSet => "0307",
            Self::RecordNotFound => "0326",
            Self::DuplicateRecord => "1207",
            Self::NoCurrency => "0306",
            Self::AreaNotOpen => "0069",
        }
    }
}

// ---------------------------------------------------------------------------
//  DML result
// ---------------------------------------------------------------------------

/// Result of a DML operation.
#[derive(Debug, Clone)]
pub struct DmlResult {
    /// Status code.
    pub status: StatusCode,
    /// Optional database key of the affected record.
    pub dbkey: Option<u64>,
    /// Optional record instance (populated on GET).
    pub record: Option<RecordInstance>,
}

impl DmlResult {
    /// Create a success result.
    pub fn success() -> Self {
        Self {
            status: StatusCode::Success,
            dbkey: None,
            record: None,
        }
    }

    /// Create a success result with a dbkey.
    pub fn success_with_dbkey(dbkey: u64) -> Self {
        Self {
            status: StatusCode::Success,
            dbkey: Some(dbkey),
            record: None,
        }
    }

    /// Create a failure result.
    pub fn failure(status: StatusCode) -> Self {
        Self {
            status,
            dbkey: None,
            record: None,
        }
    }
}

// ---------------------------------------------------------------------------
//  DML engine
// ---------------------------------------------------------------------------

/// The navigational DML engine.
///
/// Operates on an in-memory set of record instances, maintaining currency
/// indicators and supporting FIND/GET/STORE/MODIFY/ERASE/CONNECT/DISCONNECT.
#[derive(Debug)]
pub struct DmlEngine {
    /// Reference schema name.
    schema_name: String,
    /// All stored records, keyed by dbkey.
    records: HashMap<u64, RecordInstance>,
    /// Set memberships: set_name -> owner_dbkey -> ordered member dbkeys.
    set_members: HashMap<String, HashMap<u64, Vec<u64>>>,
    /// Currency table.
    pub currency: CurrencyTable,
    /// Next dbkey to assign.
    next_dbkey: u64,
    /// Open areas with their usage modes.
    open_areas: HashMap<String, UsageMode>,
    /// Whether a run-unit is bound.
    bound: bool,
    /// Subschema name for the bound run-unit.
    subschema_name: Option<String>,
}

impl DmlEngine {
    /// Create a new DML engine for the given schema.
    pub fn new(schema: &CodasylSchema) -> Self {
        Self {
            schema_name: schema.name.clone(),
            records: HashMap::new(),
            set_members: HashMap::new(),
            currency: CurrencyTable::new(),
            next_dbkey: 1,
            open_areas: HashMap::new(),
            bound: false,
            subschema_name: None,
        }
    }

    /// Return the schema name this engine was created for.
    pub fn schema_name(&self) -> &str {
        &self.schema_name
    }

    /// BIND RUN-UNIT: establish a connection to the database.
    pub fn bind_run_unit(&mut self, subschema: &str) -> DmlResult {
        self.bound = true;
        self.subschema_name = Some(subschema.to_uppercase());
        self.currency.reset();
        DmlResult::success()
    }

    /// Check whether a run-unit is bound.
    pub fn is_bound(&self) -> bool {
        self.bound
    }

    /// READY: open an area for access with a usage mode.
    pub fn ready(&mut self, area: &str, mode: UsageMode) -> DmlResult {
        let upper = area.to_uppercase();
        self.open_areas.insert(upper, mode);
        DmlResult::success()
    }

    /// Open an area for access (convenience, defaults to Update mode).
    pub fn open_area(&mut self, area: &str) {
        let upper = area.to_uppercase();
        self.open_areas.entry(upper).or_insert(UsageMode::Update);
    }

    /// Check if an area is open.
    pub fn is_area_open(&self, area: &str) -> bool {
        self.open_areas.contains_key(&area.to_uppercase())
    }

    /// Return the usage mode of an open area.
    pub fn area_usage_mode(&self, area: &str) -> Option<UsageMode> {
        self.open_areas.get(&area.to_uppercase()).copied()
    }

    /// FINISH: close all areas and reset currency.
    pub fn finish(&mut self) -> DmlResult {
        self.open_areas.clear();
        self.currency.reset();
        self.bound = false;
        self.subschema_name = None;
        DmlResult::success()
    }

    /// COMMIT: commit current transaction (keep run-unit bound).
    pub fn commit(&mut self) -> DmlResult {
        // In a real system, this would write journal records.
        // Here we just acknowledge the commit.
        DmlResult::success()
    }

    /// ROLLBACK: rollback current transaction (keep run-unit bound).
    pub fn rollback(&mut self) -> DmlResult {
        // In a real system, this would restore before-images.
        self.currency.reset();
        DmlResult::success()
    }

    /// ACCEPT: retrieve the current database key.
    pub fn accept_dbkey(&self) -> DmlResult {
        match self.currency.current_of_run_unit() {
            Some(dbkey) => DmlResult::success_with_dbkey(dbkey),
            None => DmlResult::failure(StatusCode::NoCurrency),
        }
    }

    /// IF: test if the current record is a member of a set occurrence.
    pub fn if_member(&self, set_name: &str, owner_dbkey: u64) -> bool {
        let set_upper = set_name.to_uppercase();
        let current = match self.currency.current_of_run_unit() {
            Some(dbk) => dbk,
            None => return false,
        };
        self.set_members
            .get(&set_upper)
            .and_then(|owners| owners.get(&owner_dbkey))
            .map_or(false, |members| members.contains(&current))
    }

    /// IF: test if the current record is an owner of a set occurrence.
    pub fn if_owner(&self, set_name: &str) -> bool {
        let set_upper = set_name.to_uppercase();
        let current = match self.currency.current_of_run_unit() {
            Some(dbk) => dbk,
            None => return false,
        };
        self.set_members
            .get(&set_upper)
            .map_or(false, |owners| owners.contains_key(&current))
    }

    /// OBTAIN: combined FIND + GET (locate and retrieve data in one operation).
    pub fn obtain(
        &mut self,
        record_type: &str,
        set_name: &str,
        owner_dbkey: u64,
        mode: &FindMode,
    ) -> DmlResult {
        let find_result = self.find(record_type, set_name, owner_dbkey, mode);
        if find_result.status != StatusCode::Success {
            return find_result;
        }
        self.get()
    }

    /// FIND OWNER: navigate to the owner record of a set occurrence.
    pub fn find_owner(&mut self, set_name: &str) -> DmlResult {
        let set_upper = set_name.to_uppercase();
        let current = match self.currency.current_of_run_unit() {
            Some(dbk) => dbk,
            None => return DmlResult::failure(StatusCode::NoCurrency),
        };

        // Search all set occurrences for the one containing the current record.
        if let Some(owner_map) = self.set_members.get(&set_upper) {
            for (&owner_dbkey, members) in owner_map {
                if members.contains(&current) || owner_dbkey == current {
                    // Found the owner.
                    if self.records.contains_key(&owner_dbkey) {
                        self.currency.set_current_of_run_unit(owner_dbkey);
                        if let Some(rec) = self.records.get(&owner_dbkey) {
                            self.currency
                                .set_current_of_record(&rec.record_type, owner_dbkey);
                        }
                        self.currency.set_current_of_set(&set_upper, owner_dbkey);
                        return DmlResult::success_with_dbkey(owner_dbkey);
                    }
                }
            }
        }
        DmlResult::failure(StatusCode::RecordNotFound)
    }

    /// STORE: store a new record, assigning a dbkey.
    pub fn store(
        &mut self,
        record_type: &str,
        fields: HashMap<String, FieldValue>,
        _schema: &CodasylSchema,
    ) -> DmlResult {
        let rt = record_type.to_uppercase();
        let dbkey = self.next_dbkey;
        self.next_dbkey += 1;
        let mut inst = RecordInstance::new(dbkey, &rt);
        for (k, v) in fields {
            inst.set_field(&k, v);
        }
        self.records.insert(dbkey, inst);
        self.currency.set_current_of_run_unit(dbkey);
        self.currency.set_current_of_record(&rt, dbkey);
        DmlResult::success_with_dbkey(dbkey)
    }

    /// GET: retrieve the current record (populates record data from currency).
    pub fn get(&self) -> DmlResult {
        match self.currency.current_of_run_unit() {
            Some(dbkey) => {
                if let Some(inst) = self.records.get(&dbkey) {
                    DmlResult {
                        status: StatusCode::Success,
                        dbkey: Some(dbkey),
                        record: Some(inst.clone()),
                    }
                } else {
                    DmlResult::failure(StatusCode::RecordNotFound)
                }
            }
            None => DmlResult::failure(StatusCode::NoCurrency),
        }
    }

    /// FIND: locate a record using the specified mode.
    pub fn find(
        &mut self,
        record_type: &str,
        set_name: &str,
        owner_dbkey: u64,
        mode: &FindMode,
    ) -> DmlResult {
        let set_upper = set_name.to_uppercase();
        let rt_upper = record_type.to_uppercase();

        let members = match self
            .set_members
            .get(&set_upper)
            .and_then(|m| m.get(&owner_dbkey))
        {
            Some(m) if !m.is_empty() => m,
            _ => return DmlResult::failure(StatusCode::EndOfSet),
        };

        // Filter members to only the requested record type.
        let typed_members: Vec<u64> = members
            .iter()
            .copied()
            .filter(|dbk| {
                self.records
                    .get(dbk)
                    .map_or(false, |r| r.record_type == rt_upper)
            })
            .collect();

        if typed_members.is_empty() {
            return DmlResult::failure(StatusCode::EndOfSet);
        }

        let dbkey = match mode {
            FindMode::First => typed_members[0],
            FindMode::Last => *typed_members.last().unwrap(),
            FindMode::Next => {
                let cur = self.currency.current_of_set(&set_upper);
                match cur {
                    Some(cur_dbkey) => {
                        let pos = typed_members.iter().position(|&d| d == cur_dbkey);
                        match pos {
                            Some(p) if p + 1 < typed_members.len() => typed_members[p + 1],
                            _ => return DmlResult::failure(StatusCode::EndOfSet),
                        }
                    }
                    None => typed_members[0],
                }
            }
            FindMode::Prior => {
                let cur = self.currency.current_of_set(&set_upper);
                match cur {
                    Some(cur_dbkey) => {
                        let pos = typed_members.iter().position(|&d| d == cur_dbkey);
                        match pos {
                            Some(p) if p > 0 => typed_members[p - 1],
                            _ => return DmlResult::failure(StatusCode::EndOfSet),
                        }
                    }
                    None => *typed_members.last().unwrap(),
                }
            }
            FindMode::Dbkey(dbk) => {
                if typed_members.contains(dbk) {
                    *dbk
                } else {
                    return DmlResult::failure(StatusCode::RecordNotFound);
                }
            }
            FindMode::Calc | FindMode::WithinArea => {
                // Simplified: return first match.
                typed_members[0]
            }
        };

        self.currency.set_current_of_run_unit(dbkey);
        self.currency.set_current_of_record(&rt_upper, dbkey);
        self.currency.set_current_of_set(&set_upper, dbkey);
        DmlResult::success_with_dbkey(dbkey)
    }

    /// MODIFY: update fields of the current record.
    pub fn modify(&mut self, updates: HashMap<String, FieldValue>) -> DmlResult {
        match self.currency.current_of_run_unit() {
            Some(dbkey) => {
                if let Some(inst) = self.records.get_mut(&dbkey) {
                    for (k, v) in updates {
                        inst.set_field(&k, v);
                    }
                    DmlResult::success_with_dbkey(dbkey)
                } else {
                    DmlResult::failure(StatusCode::RecordNotFound)
                }
            }
            None => DmlResult::failure(StatusCode::NoCurrency),
        }
    }

    /// ERASE: remove the current record.
    pub fn erase(&mut self) -> DmlResult {
        match self.currency.current_of_run_unit() {
            Some(dbkey) => {
                if self.records.remove(&dbkey).is_some() {
                    // Remove from all set memberships.
                    for owner_map in self.set_members.values_mut() {
                        for members in owner_map.values_mut() {
                            members.retain(|&d| d != dbkey);
                        }
                    }
                    self.currency.clear_run_unit();
                    DmlResult::success_with_dbkey(dbkey)
                } else {
                    DmlResult::failure(StatusCode::RecordNotFound)
                }
            }
            None => DmlResult::failure(StatusCode::NoCurrency),
        }
    }

    /// CONNECT: add the current record as a member of a set occurrence.
    pub fn connect(&mut self, set_name: &str, owner_dbkey: u64) -> DmlResult {
        match self.currency.current_of_run_unit() {
            Some(dbkey) => {
                let set_upper = set_name.to_uppercase();
                let members = self
                    .set_members
                    .entry(set_upper.clone())
                    .or_default()
                    .entry(owner_dbkey)
                    .or_default();
                if members.contains(&dbkey) {
                    return DmlResult::failure(StatusCode::DuplicateRecord);
                }
                members.push(dbkey);
                self.currency.set_current_of_set(&set_upper, dbkey);
                DmlResult::success_with_dbkey(dbkey)
            }
            None => DmlResult::failure(StatusCode::NoCurrency),
        }
    }

    /// DISCONNECT: remove the current record from a set occurrence.
    pub fn disconnect(&mut self, set_name: &str, owner_dbkey: u64) -> DmlResult {
        match self.currency.current_of_run_unit() {
            Some(dbkey) => {
                let set_upper = set_name.to_uppercase();
                if let Some(owner_map) = self.set_members.get_mut(&set_upper) {
                    if let Some(members) = owner_map.get_mut(&owner_dbkey) {
                        members.retain(|&d| d != dbkey);
                        return DmlResult::success_with_dbkey(dbkey);
                    }
                }
                DmlResult::failure(StatusCode::RecordNotFound)
            }
            None => DmlResult::failure(StatusCode::NoCurrency),
        }
    }

    /// Return the number of stored records.
    pub fn record_count(&self) -> usize {
        self.records.len()
    }

    /// Validate a field value against a field type (basic type check).
    pub fn validate_field(value: &FieldValue, field_type: &FieldType) -> bool {
        matches!(
            (value, field_type),
            (FieldValue::Str(_), FieldType::Char(_))
                | (FieldValue::Int(_), FieldType::Int)
                | (FieldValue::Long(_), FieldType::Long)
                | (FieldValue::Decimal(_), FieldType::Decimal(_, _))
                | (FieldValue::Binary(_), FieldType::Binary(_))
                | (FieldValue::Null, _)
        )
    }
}

// ---------------------------------------------------------------------------
//  Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::codasyl::{CodasylSchema, RecordType, SetOrder, SetType};

    fn make_schema() -> CodasylSchema {
        let mut schema = CodasylSchema::new("TESTSCHM");
        let dept = RecordType::new(100, "DEPARTMENT", "DEPT-AREA");
        let emp = RecordType::new(200, "EMPLOYEE", "EMP-AREA");
        schema.add_record(dept).unwrap();
        schema.add_record(emp).unwrap();
        let mut set = SetType::new("DEPT-EMP", "DEPARTMENT", SetOrder::Last);
        set.add_member("EMPLOYEE");
        schema.add_set(set).unwrap();
        schema
    }

    #[test]
    fn store_and_get() {
        let schema = make_schema();
        let mut engine = DmlEngine::new(&schema);
        let mut fields = HashMap::new();
        fields.insert("DEPT-ID".into(), FieldValue::Int(10));
        let result = engine.store("DEPARTMENT", fields, &schema);
        assert_eq!(result.status, StatusCode::Success);
        assert!(result.dbkey.is_some());

        let get_result = engine.get();
        assert_eq!(get_result.status, StatusCode::Success);
        let rec = get_result.record.unwrap();
        assert_eq!(rec.get_field("DEPT-ID"), Some(&FieldValue::Int(10)));
    }

    #[test]
    fn find_in_set() {
        let schema = make_schema();
        let mut engine = DmlEngine::new(&schema);

        // Store owner.
        let mut dept_fields = HashMap::new();
        dept_fields.insert("DEPT-ID".into(), FieldValue::Int(1));
        let dept_res = engine.store("DEPARTMENT", dept_fields, &schema);
        let owner_dbkey = dept_res.dbkey.unwrap();

        // Store two employees and connect them.
        let mut emp1_fields = HashMap::new();
        emp1_fields.insert("EMP-ID".into(), FieldValue::Int(100));
        let emp1_res = engine.store("EMPLOYEE", emp1_fields, &schema);
        let emp1_dbkey = emp1_res.dbkey.unwrap();
        engine.connect("DEPT-EMP", owner_dbkey);

        let mut emp2_fields = HashMap::new();
        emp2_fields.insert("EMP-ID".into(), FieldValue::Int(200));
        engine.store("EMPLOYEE", emp2_fields, &schema);
        engine.connect("DEPT-EMP", owner_dbkey);

        // Find first.
        let r = engine.find("EMPLOYEE", "DEPT-EMP", owner_dbkey, &FindMode::First);
        assert_eq!(r.status, StatusCode::Success);
        assert_eq!(r.dbkey, Some(emp1_dbkey));

        // Find next.
        let r2 = engine.find("EMPLOYEE", "DEPT-EMP", owner_dbkey, &FindMode::Next);
        assert_eq!(r2.status, StatusCode::Success);
    }

    #[test]
    fn modify_current() {
        let schema = make_schema();
        let mut engine = DmlEngine::new(&schema);
        let mut fields = HashMap::new();
        fields.insert("DEPT-ID".into(), FieldValue::Int(5));
        engine.store("DEPARTMENT", fields, &schema);

        let mut updates = HashMap::new();
        updates.insert("DEPT-ID".into(), FieldValue::Int(99));
        let r = engine.modify(updates);
        assert_eq!(r.status, StatusCode::Success);

        let get = engine.get();
        let rec = get.record.unwrap();
        assert_eq!(rec.get_field("DEPT-ID"), Some(&FieldValue::Int(99)));
    }

    #[test]
    fn erase_current() {
        let schema = make_schema();
        let mut engine = DmlEngine::new(&schema);
        let mut fields = HashMap::new();
        fields.insert("DEPT-ID".into(), FieldValue::Int(5));
        engine.store("DEPARTMENT", fields, &schema);
        assert_eq!(engine.record_count(), 1);

        let r = engine.erase();
        assert_eq!(r.status, StatusCode::Success);
        assert_eq!(engine.record_count(), 0);
    }

    #[test]
    fn connect_and_disconnect() {
        let schema = make_schema();
        let mut engine = DmlEngine::new(&schema);

        let mut dept_fields = HashMap::new();
        dept_fields.insert("DEPT-ID".into(), FieldValue::Int(1));
        let dept_res = engine.store("DEPARTMENT", dept_fields, &schema);
        let owner_dbkey = dept_res.dbkey.unwrap();

        let mut emp_fields = HashMap::new();
        emp_fields.insert("EMP-ID".into(), FieldValue::Int(100));
        engine.store("EMPLOYEE", emp_fields, &schema);
        let emp_dbkey = engine.currency.current_of_run_unit().unwrap();

        let cr = engine.connect("DEPT-EMP", owner_dbkey);
        assert_eq!(cr.status, StatusCode::Success);

        // Duplicate connect fails.
        let cr2 = engine.connect("DEPT-EMP", owner_dbkey);
        assert_eq!(cr2.status, StatusCode::DuplicateRecord);

        // Disconnect.
        let dr = engine.disconnect("DEPT-EMP", owner_dbkey);
        assert_eq!(dr.status, StatusCode::Success);
        assert_eq!(dr.dbkey, Some(emp_dbkey));
    }

    #[test]
    fn status_codes() {
        assert_eq!(StatusCode::Success.code(), "0000");
        assert_eq!(StatusCode::EndOfSet.code(), "0307");
        assert_eq!(StatusCode::RecordNotFound.code(), "0326");
    }

    #[test]
    fn get_no_currency() {
        let schema = make_schema();
        let engine = DmlEngine::new(&schema);
        let r = engine.get();
        assert_eq!(r.status, StatusCode::NoCurrency);
    }

    #[test]
    fn validate_field_types() {
        use crate::codasyl::FieldType;
        assert!(DmlEngine::validate_field(
            &FieldValue::Int(1),
            &FieldType::Int
        ));
        assert!(DmlEngine::validate_field(
            &FieldValue::Str("hi".into()),
            &FieldType::Char(10)
        ));
        assert!(!DmlEngine::validate_field(
            &FieldValue::Int(1),
            &FieldType::Char(10)
        ));
        assert!(DmlEngine::validate_field(
            &FieldValue::Null,
            &FieldType::Int
        ));
    }

    #[test]
    fn open_area() {
        let schema = make_schema();
        let mut engine = DmlEngine::new(&schema);
        assert!(!engine.is_area_open("EMP-AREA"));
        engine.open_area("EMP-AREA");
        assert!(engine.is_area_open("emp-area"));
    }

    #[test]
    fn find_by_dbkey() {
        let schema = make_schema();
        let mut engine = DmlEngine::new(&schema);

        let mut dept_fields = HashMap::new();
        dept_fields.insert("DEPT-ID".into(), FieldValue::Int(1));
        let dept_res = engine.store("DEPARTMENT", dept_fields, &schema);
        let owner_dbkey = dept_res.dbkey.unwrap();

        let mut emp_fields = HashMap::new();
        emp_fields.insert("EMP-ID".into(), FieldValue::Int(100));
        let emp_res = engine.store("EMPLOYEE", emp_fields, &schema);
        let emp_dbkey = emp_res.dbkey.unwrap();
        engine.connect("DEPT-EMP", owner_dbkey);

        let r = engine.find(
            "EMPLOYEE",
            "DEPT-EMP",
            owner_dbkey,
            &FindMode::Dbkey(emp_dbkey),
        );
        assert_eq!(r.status, StatusCode::Success);
    }
}
