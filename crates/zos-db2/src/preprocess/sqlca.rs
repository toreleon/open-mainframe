//! SQLCA (SQL Communication Area) generation for COBOL.
//!
//! Generates the SQLCA copybook that COBOL programs use to check
//! SQL return codes and error information.

/// Generate SQLCA copybook for COBOL.
pub fn generate_sqlca_copybook() -> String {
    r#"      *----------------------------------------------------------------*
      * SQLCA - SQL COMMUNICATION AREA
      *----------------------------------------------------------------*
       01  SQLCA.
           05  SQLCAID           PIC X(8) VALUE 'SQLCA   '.
           05  SQLCABC           PIC S9(9) COMP-5 VALUE 136.
           05  SQLCODE           PIC S9(9) COMP-5.
           05  SQLERRM.
               10  SQLERRML      PIC S9(4) COMP-5.
               10  SQLERRMC      PIC X(70).
           05  SQLERRP           PIC X(8).
           05  SQLERRD           OCCURS 6 TIMES
                                 PIC S9(9) COMP-5.
           05  SQLWARN.
               10  SQLWARN0      PIC X.
               10  SQLWARN1      PIC X.
               10  SQLWARN2      PIC X.
               10  SQLWARN3      PIC X.
               10  SQLWARN4      PIC X.
               10  SQLWARN5      PIC X.
               10  SQLWARN6      PIC X.
               10  SQLWARN7      PIC X.
               10  SQLWARN8      PIC X.
               10  SQLWARN9      PIC X.
               10  SQLWARNA      PIC X.
           05  SQLSTATE          PIC X(5).
      *----------------------------------------------------------------*
      * SQLCODE VALUES:
      *   0     - SUCCESSFUL EXECUTION
      *   100   - NOT FOUND (NO DATA)
      *   < 0   - ERROR OCCURRED
      *
      * COMMON SQLCODE VALUES:
      *   -117  - STATEMENT HAS WRONG NUMBER OF VALUES
      *   -180  - INVALID DATE/TIME VALUE
      *   -181  - INVALID DATE/TIME VALUE FOR COLUMN
      *   -305  - NULL VALUE IN HOST VARIABLE
      *   -501  - CURSOR NOT OPEN
      *   -502  - CURSOR ALREADY OPEN
      *   -803  - DUPLICATE KEY
      *   -811  - TOO MANY ROWS RETURNED
      *   -904  - RESOURCE UNAVAILABLE
      *   -911  - DEADLOCK OR TIMEOUT
      *----------------------------------------------------------------*
"#
    .to_string()
}

/// Generate SQLCODE condition names for easier checking.
pub fn generate_sqlcode_conditions() -> String {
    r#"      *----------------------------------------------------------------*
      * SQLCODE CONDITION NAMES
      *----------------------------------------------------------------*
       01  SQL-STATUS.
           05  SQL-SUCCESS       PIC S9(9) COMP-5 VALUE 0.
           05  SQL-NOT-FOUND     PIC S9(9) COMP-5 VALUE 100.
           05  SQL-DUP-KEY       PIC S9(9) COMP-5 VALUE -803.
           05  SQL-NULL-VALUE    PIC S9(9) COMP-5 VALUE -305.
           05  SQL-TOO-MANY-ROWS PIC S9(9) COMP-5 VALUE -811.
           05  SQL-DEADLOCK      PIC S9(9) COMP-5 VALUE -911.
"#
    .to_string()
}

/// SQL return codes.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SqlCode {
    /// Successful execution
    Success = 0,
    /// No data found
    NotFound = 100,
    /// Duplicate key
    DuplicateKey = -803,
    /// Null value without indicator
    NullValue = -305,
    /// Too many rows for SELECT INTO
    TooManyRows = -811,
    /// Cursor not open
    CursorNotOpen = -501,
    /// Cursor already open
    CursorAlreadyOpen = -502,
    /// Deadlock or timeout
    Deadlock = -911,
    /// Resource unavailable
    ResourceUnavailable = -904,
    /// Invalid date/time
    InvalidDateTime = -180,
    /// Connection error
    ConnectionError = -30081,
}

impl SqlCode {
    /// Get the SQLCODE value.
    pub fn code(&self) -> i32 {
        *self as i32
    }

    /// Get a description of the SQLCODE.
    pub fn description(&self) -> &'static str {
        match self {
            SqlCode::Success => "Successful execution",
            SqlCode::NotFound => "No data found",
            SqlCode::DuplicateKey => "Duplicate key violation",
            SqlCode::NullValue => "Null value without indicator variable",
            SqlCode::TooManyRows => "Too many rows for SELECT INTO",
            SqlCode::CursorNotOpen => "Cursor not open",
            SqlCode::CursorAlreadyOpen => "Cursor already open",
            SqlCode::Deadlock => "Deadlock or timeout",
            SqlCode::ResourceUnavailable => "Resource unavailable",
            SqlCode::InvalidDateTime => "Invalid date/time value",
            SqlCode::ConnectionError => "Connection error",
        }
    }

    /// Check if the SQLCODE indicates success.
    pub fn is_success(&self) -> bool {
        matches!(self, SqlCode::Success)
    }

    /// Check if the SQLCODE indicates no data.
    pub fn is_not_found(&self) -> bool {
        matches!(self, SqlCode::NotFound)
    }

    /// Check if the SQLCODE indicates an error.
    pub fn is_error(&self) -> bool {
        self.code() < 0
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_generate_sqlca() {
        let sqlca = generate_sqlca_copybook();
        assert!(sqlca.contains("01  SQLCA."));
        assert!(sqlca.contains("SQLCODE"));
        assert!(sqlca.contains("SQLERRM"));
        assert!(sqlca.contains("SQLSTATE"));
    }

    #[test]
    fn test_generate_conditions() {
        let conditions = generate_sqlcode_conditions();
        assert!(conditions.contains("SQL-SUCCESS"));
        assert!(conditions.contains("SQL-NOT-FOUND"));
        assert!(conditions.contains("SQL-DUP-KEY"));
    }

    #[test]
    fn test_sqlcode_values() {
        assert_eq!(SqlCode::Success.code(), 0);
        assert_eq!(SqlCode::NotFound.code(), 100);
        assert_eq!(SqlCode::DuplicateKey.code(), -803);
    }

    #[test]
    fn test_sqlcode_checks() {
        assert!(SqlCode::Success.is_success());
        assert!(!SqlCode::Success.is_error());
        assert!(SqlCode::NotFound.is_not_found());
        assert!(SqlCode::DuplicateKey.is_error());
    }
}
