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
    /// Name not found (table, view, etc.).
    pub const NAME_NOT_FOUND: i32 = -204;
    /// Syntax error.
    pub const SYNTAX_ERROR: i32 = -104;
    /// Resource unavailable.
    pub const RESOURCE_UNAVAILABLE: i32 = -904;
    /// Authorization failure.
    pub const AUTHORIZATION_FAILURE: i32 = -551;
    /// Check constraint violation.
    pub const CHECK_VIOLATION: i32 = -545;
    /// Foreign key violation.
    pub const FK_VIOLATION: i32 = -530;
    /// Numeric overflow / data exception.
    pub const DATA_EXCEPTION: i32 = -302;

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

    /// Set SQLCA from a PostgreSQL SQLSTATE error code.
    ///
    /// Maps PostgreSQL 5-character SQLSTATE codes to DB2 SQLCODE values.
    pub fn set_from_pg_state(&mut self, pg_state: &str, message: &str) {
        let sqlcode = Self::pg_state_to_sqlcode(pg_state);
        self.set_error(sqlcode, message);
        // Preserve the original PG SQLSTATE
        self.sqlstate = pg_state.to_string();
    }

    /// Map a PostgreSQL SQLSTATE to a DB2 SQLCODE.
    pub fn pg_state_to_sqlcode(pg_state: &str) -> i32 {
        match pg_state {
            // Successful completion
            "00000" => Self::SUCCESS,
            // No data
            "02000" => Self::NOT_FOUND,
            // Unique violation
            "23505" => Self::DUPLICATE_KEY,
            // Not-null violation
            "23502" => Self::NULL_VALUE,
            // Check violation
            "23514" => Self::CHECK_VIOLATION,
            // Foreign key violation
            "23503" => Self::FK_VIOLATION,
            // Undefined table (42P01)
            "42P01" => Self::NAME_NOT_FOUND,
            // Undefined column (42703)
            "42703" => Self::NAME_NOT_FOUND,
            // Syntax error (42601)
            "42601" => Self::SYNTAX_ERROR,
            // Insufficient privilege (42501)
            "42501" => Self::AUTHORIZATION_FAILURE,
            // Serialization failure / deadlock
            "40001" => Self::DEADLOCK,
            "40P01" => Self::DEADLOCK,
            // Numeric value out of range
            "22003" => Self::DATA_EXCEPTION,
            // String data right truncation
            "22001" => Self::DATA_EXCEPTION,
            // Connection failure
            "08000" | "08001" | "08003" | "08006" => Self::CONNECTION_ERROR,
            // Cardinality violation
            "21000" => Self::TOO_MANY_ROWS,
            // Class 22: data exception (general)
            s if s.starts_with("22") => Self::DATA_EXCEPTION,
            // Class 23: integrity constraint
            s if s.starts_with("23") => Self::DUPLICATE_KEY,
            // Class 08: connection
            s if s.starts_with("08") => Self::CONNECTION_ERROR,
            // Class 42: syntax/access
            s if s.starts_with("42") => Self::SYNTAX_ERROR,
            // Everything else
            _ => -999, // Unknown error
        }
    }

    /// Convert SQLCODE to SQLSTATE.
    fn code_to_state(code: i32) -> String {
        match code {
            0 => "00000".to_string(),       // Success
            100 => "02000".to_string(),     // Not found
            -803 => "23505".to_string(),    // Unique violation
            -305 => "22002".to_string(),    // Null value
            -811 => "21000".to_string(),    // Cardinality violation
            -501 => "24501".to_string(),    // Cursor not open
            -502 => "24502".to_string(),    // Cursor already open
            -911 => "40001".to_string(),    // Serialization failure
            -204 => "42704".to_string(),    // Name not found
            -104 => "42601".to_string(),    // Syntax error
            -904 => "57011".to_string(),    // Resource unavailable
            -551 => "42501".to_string(),    // Authorization failure
            -545 => "23514".to_string(),    // Check violation
            -530 => "23503".to_string(),    // FK violation
            -302 => "22003".to_string(),    // Data exception
            -30081 => "08001".to_string(),  // Connection error
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

    // --- PostgreSQL Error Mapping Tests (Story 302.3) ---

    #[test]
    fn test_pg_unique_violation_to_duplicate_key() {
        let code = Sqlca::pg_state_to_sqlcode("23505");
        assert_eq!(code, Sqlca::DUPLICATE_KEY); // -803
    }

    #[test]
    fn test_pg_undefined_table_to_name_not_found() {
        let code = Sqlca::pg_state_to_sqlcode("42P01");
        assert_eq!(code, Sqlca::NAME_NOT_FOUND); // -204
    }

    #[test]
    fn test_pg_undefined_column_to_name_not_found() {
        let code = Sqlca::pg_state_to_sqlcode("42703");
        assert_eq!(code, Sqlca::NAME_NOT_FOUND);
    }

    #[test]
    fn test_pg_deadlock_to_deadlock() {
        assert_eq!(Sqlca::pg_state_to_sqlcode("40001"), Sqlca::DEADLOCK);
        assert_eq!(Sqlca::pg_state_to_sqlcode("40P01"), Sqlca::DEADLOCK);
    }

    #[test]
    fn test_pg_connection_failure() {
        assert_eq!(Sqlca::pg_state_to_sqlcode("08001"), Sqlca::CONNECTION_ERROR);
        assert_eq!(Sqlca::pg_state_to_sqlcode("08006"), Sqlca::CONNECTION_ERROR);
    }

    #[test]
    fn test_pg_syntax_error() {
        assert_eq!(Sqlca::pg_state_to_sqlcode("42601"), Sqlca::SYNTAX_ERROR);
    }

    #[test]
    fn test_pg_no_data() {
        assert_eq!(Sqlca::pg_state_to_sqlcode("02000"), Sqlca::NOT_FOUND);
    }

    #[test]
    fn test_pg_success() {
        assert_eq!(Sqlca::pg_state_to_sqlcode("00000"), Sqlca::SUCCESS);
    }

    #[test]
    fn test_set_from_pg_state() {
        let mut sqlca = Sqlca::new();
        sqlca.set_from_pg_state("23505", "duplicate key value violates unique constraint");
        assert_eq!(sqlca.sqlcode(), Sqlca::DUPLICATE_KEY);
        assert_eq!(sqlca.sqlstate(), "23505");
        assert!(sqlca.error_message().contains("duplicate key"));
    }

    #[test]
    fn test_pg_check_violation() {
        assert_eq!(Sqlca::pg_state_to_sqlcode("23514"), Sqlca::CHECK_VIOLATION);
    }

    #[test]
    fn test_pg_fk_violation() {
        assert_eq!(Sqlca::pg_state_to_sqlcode("23503"), Sqlca::FK_VIOLATION);
    }

    #[test]
    fn test_pg_data_exception_class() {
        // Any 22xxx should map to DATA_EXCEPTION
        assert_eq!(Sqlca::pg_state_to_sqlcode("22003"), Sqlca::DATA_EXCEPTION);
        assert_eq!(Sqlca::pg_state_to_sqlcode("22001"), Sqlca::DATA_EXCEPTION);
    }

    #[test]
    fn test_code_to_state_new_codes() {
        let mut sqlca = Sqlca::new();
        sqlca.set_sqlcode(Sqlca::NAME_NOT_FOUND);
        assert_eq!(sqlca.sqlstate(), "42704");

        sqlca.set_sqlcode(Sqlca::SYNTAX_ERROR);
        assert_eq!(sqlca.sqlstate(), "42601");

        sqlca.set_sqlcode(Sqlca::CONNECTION_ERROR);
        assert_eq!(sqlca.sqlstate(), "08001");
    }
}
