//! IDMS-103: Currency Indicators (4 stories).
//!
//! Currency indicators track the "current position" in the database for
//! a run-unit.  IDMS maintains:
//!
//! - **Current of run-unit** -- the last record accessed
//! - **Current of record type** -- the last record of each type accessed
//! - **Current of set type** -- the last record within each set accessed
//! - **Current of area** -- the last record in each area accessed

use std::collections::HashMap;

// ---------------------------------------------------------------------------
//  Currency update rules
// ---------------------------------------------------------------------------

/// Describes which currency indicators to update after a DML operation.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CurrencyUpdate {
    /// Update all applicable currency indicators.
    All,
    /// Do not update currency of the specified set (SUPPRESS).
    SuppressSet(String),
    /// Do not update any currency indicators (GET without currency change).
    None,
}

// ---------------------------------------------------------------------------
//  Currency table
// ---------------------------------------------------------------------------

/// A snapshot of all currency indicators, used for save/restore.
#[derive(Debug, Clone)]
struct CurrencySnapshot {
    run_unit: Option<u64>,
    record_type: HashMap<String, u64>,
    set_type: HashMap<String, u64>,
    area: HashMap<String, u64>,
}

/// Tracks all currency indicators for a run-unit.
///
/// Each indicator stores the database key (dbkey) of the record that is
/// "current" for a particular context.  Supports save/restore via an
/// internal stack for nested operations.
#[derive(Debug, Clone, Default)]
pub struct CurrencyTable {
    /// Current of run-unit.
    run_unit: Option<u64>,
    /// Current of record type (record-type-name -> dbkey).
    record_type: HashMap<String, u64>,
    /// Current of set type (set-type-name -> dbkey).
    set_type: HashMap<String, u64>,
    /// Current of area (area-name -> dbkey).
    area: HashMap<String, u64>,
    /// Stack for save/restore of currency state.
    stack: Vec<CurrencySnapshot>,
}

impl CurrencyTable {
    /// Create a new, empty currency table.
    pub fn new() -> Self {
        Self::default()
    }

    // -- run unit --

    /// Get current of run-unit.
    pub fn current_of_run_unit(&self) -> Option<u64> {
        self.run_unit
    }

    /// Set current of run-unit.
    pub fn set_current_of_run_unit(&mut self, dbkey: u64) {
        self.run_unit = Some(dbkey);
    }

    /// Clear current of run-unit.
    pub fn clear_run_unit(&mut self) {
        self.run_unit = None;
    }

    // -- record type --

    /// Get current of a record type.
    pub fn current_of_record(&self, record_type: &str) -> Option<u64> {
        self.record_type.get(&record_type.to_uppercase()).copied()
    }

    /// Set current of a record type.
    pub fn set_current_of_record(&mut self, record_type: &str, dbkey: u64) {
        self.record_type
            .insert(record_type.to_uppercase(), dbkey);
    }

    /// Clear current of a record type.
    pub fn clear_record(&mut self, record_type: &str) {
        self.record_type.remove(&record_type.to_uppercase());
    }

    // -- set type --

    /// Get current of a set type.
    pub fn current_of_set(&self, set_type: &str) -> Option<u64> {
        self.set_type.get(&set_type.to_uppercase()).copied()
    }

    /// Set current of a set type.
    pub fn set_current_of_set(&mut self, set_type: &str, dbkey: u64) {
        self.set_type.insert(set_type.to_uppercase(), dbkey);
    }

    /// Clear current of a set type.
    pub fn clear_set(&mut self, set_type: &str) {
        self.set_type.remove(&set_type.to_uppercase());
    }

    // -- area --

    /// Get current of an area.
    pub fn current_of_area(&self, area: &str) -> Option<u64> {
        self.area.get(&area.to_uppercase()).copied()
    }

    /// Set current of an area.
    pub fn set_current_of_area(&mut self, area: &str, dbkey: u64) {
        self.area.insert(area.to_uppercase(), dbkey);
    }

    /// Clear current of an area.
    pub fn clear_area(&mut self, area: &str) {
        self.area.remove(&area.to_uppercase());
    }

    /// Apply a currency update rule, updating all indicators except those
    /// suppressed.
    pub fn apply_update(
        &mut self,
        rule: &CurrencyUpdate,
        dbkey: u64,
        record_type: &str,
        set_name: Option<&str>,
        area_name: Option<&str>,
    ) {
        match rule {
            CurrencyUpdate::None => {}
            CurrencyUpdate::All => {
                self.set_current_of_run_unit(dbkey);
                self.set_current_of_record(record_type, dbkey);
                if let Some(sn) = set_name {
                    self.set_current_of_set(sn, dbkey);
                }
                if let Some(an) = area_name {
                    self.set_current_of_area(an, dbkey);
                }
            }
            CurrencyUpdate::SuppressSet(suppressed) => {
                self.set_current_of_run_unit(dbkey);
                self.set_current_of_record(record_type, dbkey);
                if let Some(sn) = set_name {
                    if !sn.eq_ignore_ascii_case(suppressed) {
                        self.set_current_of_set(sn, dbkey);
                    }
                }
                if let Some(an) = area_name {
                    self.set_current_of_area(an, dbkey);
                }
            }
        }
    }

    /// Save all currency indicators onto the internal stack.
    pub fn save(&mut self) {
        self.stack.push(CurrencySnapshot {
            run_unit: self.run_unit,
            record_type: self.record_type.clone(),
            set_type: self.set_type.clone(),
            area: self.area.clone(),
        });
    }

    /// Restore currency indicators from the last save.
    /// Returns `true` if a snapshot was restored, `false` if the stack was empty.
    pub fn restore(&mut self) -> bool {
        if let Some(snapshot) = self.stack.pop() {
            self.run_unit = snapshot.run_unit;
            self.record_type = snapshot.record_type;
            self.set_type = snapshot.set_type;
            self.area = snapshot.area;
            true
        } else {
            false
        }
    }

    /// Return the depth of the currency save stack.
    pub fn stack_depth(&self) -> usize {
        self.stack.len()
    }

    /// Reset all currency indicators.
    pub fn reset(&mut self) {
        self.run_unit = None;
        self.record_type.clear();
        self.set_type.clear();
        self.area.clear();
        self.stack.clear();
    }
}

// ---------------------------------------------------------------------------
//  Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn run_unit_currency() {
        let mut ct = CurrencyTable::new();
        assert!(ct.current_of_run_unit().is_none());
        ct.set_current_of_run_unit(42);
        assert_eq!(ct.current_of_run_unit(), Some(42));
        ct.clear_run_unit();
        assert!(ct.current_of_run_unit().is_none());
    }

    #[test]
    fn record_type_currency() {
        let mut ct = CurrencyTable::new();
        ct.set_current_of_record("EMPLOYEE", 10);
        assert_eq!(ct.current_of_record("employee"), Some(10));
        ct.clear_record("EMPLOYEE");
        assert!(ct.current_of_record("EMPLOYEE").is_none());
    }

    #[test]
    fn set_type_currency() {
        let mut ct = CurrencyTable::new();
        ct.set_current_of_set("DEPT-EMP", 20);
        assert_eq!(ct.current_of_set("dept-emp"), Some(20));
        ct.clear_set("DEPT-EMP");
        assert!(ct.current_of_set("DEPT-EMP").is_none());
    }

    #[test]
    fn area_currency() {
        let mut ct = CurrencyTable::new();
        ct.set_current_of_area("EMP-AREA", 30);
        assert_eq!(ct.current_of_area("emp-area"), Some(30));
        ct.clear_area("EMP-AREA");
        assert!(ct.current_of_area("EMP-AREA").is_none());
    }

    #[test]
    fn apply_update_all() {
        let mut ct = CurrencyTable::new();
        ct.apply_update(
            &CurrencyUpdate::All,
            50,
            "EMPLOYEE",
            Some("DEPT-EMP"),
            Some("EMP-AREA"),
        );
        assert_eq!(ct.current_of_run_unit(), Some(50));
        assert_eq!(ct.current_of_record("EMPLOYEE"), Some(50));
        assert_eq!(ct.current_of_set("DEPT-EMP"), Some(50));
        assert_eq!(ct.current_of_area("EMP-AREA"), Some(50));
    }

    #[test]
    fn apply_update_suppress_set() {
        let mut ct = CurrencyTable::new();
        ct.apply_update(
            &CurrencyUpdate::SuppressSet("DEPT-EMP".into()),
            50,
            "EMPLOYEE",
            Some("DEPT-EMP"),
            Some("EMP-AREA"),
        );
        assert_eq!(ct.current_of_run_unit(), Some(50));
        assert_eq!(ct.current_of_record("EMPLOYEE"), Some(50));
        assert!(ct.current_of_set("DEPT-EMP").is_none());
        assert_eq!(ct.current_of_area("EMP-AREA"), Some(50));
    }

    #[test]
    fn apply_update_none() {
        let mut ct = CurrencyTable::new();
        ct.apply_update(&CurrencyUpdate::None, 50, "EMPLOYEE", None, None);
        assert!(ct.current_of_run_unit().is_none());
    }

    #[test]
    fn reset_clears_all() {
        let mut ct = CurrencyTable::new();
        ct.set_current_of_run_unit(1);
        ct.set_current_of_record("EMPLOYEE", 2);
        ct.set_current_of_set("DEPT-EMP", 3);
        ct.set_current_of_area("EMP-AREA", 4);
        ct.reset();
        assert!(ct.current_of_run_unit().is_none());
        assert!(ct.current_of_record("EMPLOYEE").is_none());
        assert!(ct.current_of_set("DEPT-EMP").is_none());
        assert!(ct.current_of_area("EMP-AREA").is_none());
    }
}
