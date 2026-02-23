//! IDMS-107: ADS/Online 4GL (4 stories).
//!
//! Application Development System (ADS) provides a fourth-generation
//! language for building online IDMS-DC applications.  ADS applications
//! are defined as *dialogs* consisting of maps and process blocks that
//! execute premap and response logic.

use std::collections::HashMap;

// ---------------------------------------------------------------------------
//  ADS process
// ---------------------------------------------------------------------------

/// A process block in an ADS dialog.
///
/// Process blocks contain procedural logic that manipulates database
/// records and map fields.  Each block has a name and a list of
/// statements (represented here as strings).
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct AdsProcess {
    /// Process name.
    pub name: String,
    /// Process statements (simplified representation).
    pub statements: Vec<String>,
    /// Local variables.
    pub variables: HashMap<String, String>,
}

impl AdsProcess {
    /// Create a new empty process.
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_uppercase(),
            statements: Vec::new(),
            variables: HashMap::new(),
        }
    }

    /// Add a statement to the process.
    pub fn add_statement(&mut self, stmt: &str) {
        self.statements.push(stmt.to_string());
    }

    /// Set a local variable.
    pub fn set_variable(&mut self, name: &str, value: &str) {
        self.variables
            .insert(name.to_uppercase(), value.to_string());
    }

    /// Get a local variable.
    pub fn get_variable(&self, name: &str) -> Option<&str> {
        self.variables
            .get(&name.to_uppercase())
            .map(String::as_str)
    }

    /// Execute the process (simplified: returns statements joined).
    pub fn execute(&self) -> String {
        self.statements.join("; ")
    }
}

// ---------------------------------------------------------------------------
//  ADS map
// ---------------------------------------------------------------------------

/// An ADS map -- a screen definition with fields that can be bound
/// to database record fields.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct AdsMap {
    /// Map name.
    pub name: String,
    /// Map fields: field name -> (display label, bound record.field or empty).
    pub fields: Vec<AdsMapField>,
}

/// A single field in an ADS map.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct AdsMapField {
    /// Field name.
    pub name: String,
    /// Display label.
    pub label: String,
    /// Bound database field (record.field) if any.
    pub binding: Option<String>,
    /// Current value.
    pub value: String,
}

impl AdsMap {
    /// Create a new map with no fields.
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_uppercase(),
            fields: Vec::new(),
        }
    }

    /// Add a field to the map.
    pub fn add_field(&mut self, name: &str, label: &str, binding: Option<&str>) {
        self.fields.push(AdsMapField {
            name: name.to_uppercase(),
            label: label.to_string(),
            binding: binding.map(|b| b.to_uppercase()),
            value: String::new(),
        });
    }

    /// Set a field value by name.
    pub fn set_value(&mut self, name: &str, value: &str) -> bool {
        let upper = name.to_uppercase();
        for field in &mut self.fields {
            if field.name == upper {
                field.value = value.to_string();
                return true;
            }
        }
        false
    }

    /// Get a field value by name.
    pub fn get_value(&self, name: &str) -> Option<&str> {
        let upper = name.to_uppercase();
        self.fields
            .iter()
            .find(|f| f.name == upper)
            .map(|f| f.value.as_str())
    }

    /// Return the number of fields.
    pub fn field_count(&self) -> usize {
        self.fields.len()
    }
}

// ---------------------------------------------------------------------------
//  ADS dialog
// ---------------------------------------------------------------------------

/// An ADS dialog -- the top-level component of an ADS application.
///
/// A dialog ties together a map and process blocks that execute
/// premap (before display) and response (after user input) logic.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct AdsDialog {
    /// Dialog name.
    pub name: String,
    /// Associated map name.
    pub map_name: String,
    /// Premap process (executes before map display).
    pub premap: Option<AdsProcess>,
    /// Response process (executes after user input).
    pub response: Option<AdsProcess>,
    /// Associated subschema name.
    pub subschema: Option<String>,
}

impl AdsDialog {
    /// Create a new dialog.
    pub fn new(name: &str, map_name: &str) -> Self {
        Self {
            name: name.to_uppercase(),
            map_name: map_name.to_uppercase(),
            premap: None,
            response: None,
            subschema: None,
        }
    }

    /// Set the premap process.
    pub fn set_premap(&mut self, process: AdsProcess) {
        self.premap = Some(process);
    }

    /// Set the response process.
    pub fn set_response(&mut self, process: AdsProcess) {
        self.response = Some(process);
    }

    /// Set the subschema binding.
    pub fn set_subschema(&mut self, name: &str) {
        self.subschema = Some(name.to_uppercase());
    }

    /// Execute the premap process (if any).
    pub fn run_premap(&self) -> Option<String> {
        self.premap.as_ref().map(AdsProcess::execute)
    }

    /// Execute the response process (if any).
    pub fn run_response(&self) -> Option<String> {
        self.response.as_ref().map(AdsProcess::execute)
    }
}

// ---------------------------------------------------------------------------
//  Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn process_statements() {
        let mut proc = AdsProcess::new("PREMAP1");
        proc.add_statement("FIND FIRST EMPLOYEE WITHIN DEPT-EMP");
        proc.add_statement("GET EMPLOYEE");
        assert_eq!(proc.statements.len(), 2);
        let output = proc.execute();
        assert!(output.contains("FIND FIRST"));
    }

    #[test]
    fn process_variables() {
        let mut proc = AdsProcess::new("RESP1");
        proc.set_variable("STATUS", "OK");
        assert_eq!(proc.get_variable("status"), Some("OK"));
        assert!(proc.get_variable("NONEXISTENT").is_none());
    }

    #[test]
    fn map_fields() {
        let mut map = AdsMap::new("EMPMAP");
        map.add_field("EMP-ID", "Employee ID", Some("EMPLOYEE.EMP-ID"));
        map.add_field("EMP-NAME", "Employee Name", None);
        assert_eq!(map.field_count(), 2);

        assert!(map.set_value("EMP-ID", "12345"));
        assert_eq!(map.get_value("emp-id"), Some("12345"));
        assert!(!map.set_value("NONEXISTENT", "val"));
    }

    #[test]
    fn map_binding() {
        let mut map = AdsMap::new("EMPMAP");
        map.add_field("EMP-ID", "ID", Some("EMPLOYEE.EMP-ID"));
        let field = &map.fields[0];
        assert_eq!(field.binding.as_deref(), Some("EMPLOYEE.EMP-ID"));
    }

    #[test]
    fn dialog_creation() {
        let mut dialog = AdsDialog::new("EMPDLG", "EMPMAP");
        assert_eq!(dialog.name, "EMPDLG");
        assert_eq!(dialog.map_name, "EMPMAP");

        let mut premap = AdsProcess::new("PREMAP");
        premap.add_statement("FIND FIRST EMPLOYEE");
        dialog.set_premap(premap);

        let mut response = AdsProcess::new("RESPONSE");
        response.add_statement("MODIFY EMPLOYEE");
        dialog.set_response(response);

        dialog.set_subschema("EMPSUB");

        assert!(dialog.run_premap().is_some());
        assert!(dialog.run_response().is_some());
        assert_eq!(dialog.subschema.as_deref(), Some("EMPSUB"));
    }

    #[test]
    fn dialog_no_processes() {
        let dialog = AdsDialog::new("EMPTYDLG", "EMPTYMAP");
        assert!(dialog.run_premap().is_none());
        assert!(dialog.run_response().is_none());
    }
}
