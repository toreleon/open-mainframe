//! Runtime SQLCA (SQL Communication Area) management.
//!
//! Provides the runtime representation of SQLCA for tracking
//! SQL execution status and error information.

/// SQL Communication Area for runtime status tracking.
#[derive(Debug, Clone)]
pub struct Sqlca {
    /// SQL return code
    sqlcode: i32,
    /// Error message length
    sqlerrml: i16,
    /// Error message text
    sqlerrmc: String,
    /// Error state info
    sqlerrd: [i32; 6],
    /// Warning flags
    sqlwarn: [char; 11],
    /// SQL state (5 characters)
    sqlstate: String,
}

impl Sqlca {
    /// Success code.
    pub const SUCCESS: i32 = 0;
    /// Not found code.
    pub const NOT_FOUND: i32 = 100;
    /// Duplicate key code.
    pub const DUPLICATE_KEY: i32 = -803;
    /// Null value without indicator.
    pub const NULL_VALUE: i32 = -305;
    /// Too many rows for SELECT INTO.
    pub const TOO_MANY_ROWS: i32 = -811;
    /// Cursor not open.
    pub const CURSOR_NOT_OPEN: i32 = -501;
    /// Cursor already open.
    pub const CURSOR_ALREADY_OPEN: i32 = -502;
    /// Deadlock or timeout.
    pub const DEADLOCK: i32 = -911;
    /// Connection error.
    pub const CONNECTION_ERROR: i32 = -30081;

    /// Create a new SQLCA with success status.
    pub fn new() -> Self {
        Self {
            sqlcode: 0,
            sqlerrml: 0,
            sqlerrmc: String::new(),
            sqlerrd: [0; 6],
            sqlwarn: [' '; 11],
            sqlstate: "00000".to_string(),
        }
    }

    /// Reset SQLCA to initial state.
    pub fn reset(&mut self) {
        self.sqlcode = 0;
        self.sqlerrml = 0;
        self.sqlerrmc.clear();
        self.sqlerrd = [0; 6];
        self.sqlwarn = [' '; 11];
        self.sqlstate = "00000".to_string();
    }

    /// Get the SQLCODE value.
    pub fn sqlcode(&self) -> i32 {
        self.sqlcode
    }

    /// Set SQLCODE and related fields.
    pub fn set_sqlcode(&mut self, code: i32) {
        self.sqlcode = code;
        self.sqlstate = Self::code_to_state(code);
    }

    /// Set success status.
    pub fn set_success(&mut self) {
        self.set_sqlcode(Self::SUCCESS);
    }

    /// Set not found status.
    pub fn set_not_found(&mut self) {
        self.set_sqlcode(Self::NOT_FOUND);
    }

    /// Set error with message.
    pub fn set_error(&mut self, code: i32, message: &str) {
        self.sqlcode = code;
        self.sqlerrmc = message.chars().take(70).collect();
        self.sqlerrml = self.sqlerrmc.len() as i16;
        self.sqlstate = Self::code_to_state(code);
    }

    /// Get the error message.
    pub fn error_message(&self) -> &str {
        &self.sqlerrmc
    }

    /// Get SQLERRD values.
    pub fn sqlerrd(&self) -> &[i32; 6] {
        &self.sqlerrd
    }

    /// Set SQLERRD[2] (rows affected).
    pub fn set_rows_affected(&mut self, count: i32) {
        self.sqlerrd[2] = count;
    }

    /// Get rows affected (SQLERRD[2]).
    pub fn rows_affected(&self) -> i32 {
        self.sqlerrd[2]
    }

    /// Get SQLSTATE.
    pub fn sqlstate(&self) -> &str {
        &self.sqlstate
    }

    /// Check if last operation was successful.
    pub fn is_success(&self) -> bool {
        self.sqlcode == Self::SUCCESS
    }

    /// Check if no data was found.
    pub fn is_not_found(&self) -> bool {
        self.sqlcode == Self::NOT_FOUND
    }

    /// Check if an error occurred.
    pub fn is_error(&self) -> bool {
        self.sqlcode < 0
    }

    /// Set warning flag.
    pub fn set_warning(&mut self, index: usize, flag: char) {
        if index < 11 {
            self.sqlwarn[index] = flag;
            self.sqlwarn[0] = 'W'; // SQLWARN0 indicates warnings exist
        }
    }

    /// Check if any warnings.
    pub fn has_warnings(&self) -> bool {
        self.sqlwarn[0] == 'W'
    }

    /// Convert SQLCODE to SQLSTATE.
    fn code_to_state(code: i32) -> String {
        match code {
            0 => "00000".to_string(),      // Success
            100 => "02000".to_string(),    // Not found
            -803 => "23505".to_string(),   // Unique violation
            -305 => "22002".to_string(),   // Null value
            -811 => "21000".to_string(),   // Cardinality violation
            -501 => "24501".to_string(),   // Cursor not open
            -502 => "24502".to_string(),   // Cursor already open
            -911 => "40001".to_string(),   // Serialization failure
            _ if code < 0 => "HY000".to_string(), // General error
            _ => "00000".to_string(),
        }
    }
}

impl Default for Sqlca {
    fn default() -> Self {
        Self::new()
    }
}

/// Builder for SQLCA (for testing and simulation).
pub struct SqlcaBuilder {
    sqlca: Sqlca,
}

impl SqlcaBuilder {
    /// Create a new builder.
    pub fn new() -> Self {
        Self {
            sqlca: Sqlca::new(),
        }
    }

    /// Set SQLCODE.
    pub fn sqlcode(mut self, code: i32) -> Self {
        self.sqlca.set_sqlcode(code);
        self
    }

    /// Set error message.
    pub fn error(mut self, code: i32, message: &str) -> Self {
        self.sqlca.set_error(code, message);
        self
    }

    /// Set rows affected.
    pub fn rows_affected(mut self, count: i32) -> Self {
        self.sqlca.set_rows_affected(count);
        self
    }

    /// Build the SQLCA.
    pub fn build(self) -> Sqlca {
        self.sqlca
    }
}

impl Default for SqlcaBuilder {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_sqlca_default() {
        let sqlca = Sqlca::new();
        assert_eq!(sqlca.sqlcode(), 0);
        assert!(sqlca.is_success());
        assert!(!sqlca.is_error());
    }

    #[test]
    fn test_sqlca_not_found() {
        let mut sqlca = Sqlca::new();
        sqlca.set_not_found();
        assert_eq!(sqlca.sqlcode(), 100);
        assert!(sqlca.is_not_found());
        assert_eq!(sqlca.sqlstate(), "02000");
    }

    #[test]
    fn test_sqlca_error() {
        let mut sqlca = Sqlca::new();
        sqlca.set_error(-803, "Duplicate key violation");
        assert!(sqlca.is_error());
        assert_eq!(sqlca.sqlcode(), -803);
        assert_eq!(sqlca.error_message(), "Duplicate key violation");
        assert_eq!(sqlca.sqlstate(), "23505");
    }

    #[test]
    fn test_sqlca_rows_affected() {
        let mut sqlca = Sqlca::new();
        sqlca.set_rows_affected(42);
        assert_eq!(sqlca.rows_affected(), 42);
    }

    #[test]
    fn test_sqlca_warnings() {
        let mut sqlca = Sqlca::new();
        assert!(!sqlca.has_warnings());
        sqlca.set_warning(1, 'W');
        assert!(sqlca.has_warnings());
    }

    #[test]
    fn test_sqlca_builder() {
        let sqlca = SqlcaBuilder::new()
            .sqlcode(100)
            .rows_affected(0)
            .build();

        assert!(sqlca.is_not_found());
        assert_eq!(sqlca.rows_affected(), 0);
    }

    #[test]
    fn test_sqlca_reset() {
        let mut sqlca = Sqlca::new();
        sqlca.set_error(-803, "Error");
        sqlca.reset();
        assert!(sqlca.is_success());
        assert!(sqlca.error_message().is_empty());
    }
}
