//! COBOL Intrinsic Functions.
//!
//! Implements COBOL-2014 intrinsic functions for runtime evaluation.
//! These functions are used by the generated code at runtime.

mod string;
mod numeric;
mod datetime;

pub use string::*;
pub use numeric::*;
pub use datetime::*;

/// Function category for intrinsic functions.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FunctionCategory {
    /// String manipulation functions
    String,
    /// Numeric/mathematical functions
    Numeric,
    /// Date and time functions
    DateTime,
    /// General functions
    General,
}

/// Function result type.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FunctionResultType {
    /// Returns alphanumeric string
    Alphanumeric,
    /// Returns national string (UTF-16)
    National,
    /// Returns numeric value
    Numeric,
    /// Returns integer
    Integer,
}

/// Intrinsic function definition.
#[derive(Debug, Clone)]
pub struct IntrinsicFunction {
    /// Function name
    pub name: &'static str,
    /// Category
    pub category: FunctionCategory,
    /// Result type
    pub result_type: FunctionResultType,
    /// Minimum number of arguments
    pub min_args: usize,
    /// Maximum number of arguments (None = unlimited)
    pub max_args: Option<usize>,
    /// Description
    pub description: &'static str,
}

/// Registry of all intrinsic functions.
pub static INTRINSIC_FUNCTIONS: &[IntrinsicFunction] = &[
    // String functions
    IntrinsicFunction {
        name: "TRIM",
        category: FunctionCategory::String,
        result_type: FunctionResultType::Alphanumeric,
        min_args: 1,
        max_args: Some(2),
        description: "Remove leading and/or trailing spaces",
    },
    IntrinsicFunction {
        name: "SUBSTITUTE",
        category: FunctionCategory::String,
        result_type: FunctionResultType::Alphanumeric,
        min_args: 3,
        max_args: None,
        description: "Replace substrings",
    },
    IntrinsicFunction {
        name: "CONCATENATE",
        category: FunctionCategory::String,
        result_type: FunctionResultType::Alphanumeric,
        min_args: 1,
        max_args: None,
        description: "Join strings together",
    },
    IntrinsicFunction {
        name: "LENGTH",
        category: FunctionCategory::String,
        result_type: FunctionResultType::Integer,
        min_args: 1,
        max_args: Some(1),
        description: "Get string length",
    },
    IntrinsicFunction {
        name: "REVERSE",
        category: FunctionCategory::String,
        result_type: FunctionResultType::Alphanumeric,
        min_args: 1,
        max_args: Some(1),
        description: "Reverse string",
    },
    IntrinsicFunction {
        name: "UPPER-CASE",
        category: FunctionCategory::String,
        result_type: FunctionResultType::Alphanumeric,
        min_args: 1,
        max_args: Some(1),
        description: "Convert to uppercase",
    },
    IntrinsicFunction {
        name: "LOWER-CASE",
        category: FunctionCategory::String,
        result_type: FunctionResultType::Alphanumeric,
        min_args: 1,
        max_args: Some(1),
        description: "Convert to lowercase",
    },
    IntrinsicFunction {
        name: "DISPLAY-OF",
        category: FunctionCategory::String,
        result_type: FunctionResultType::Alphanumeric,
        min_args: 1,
        max_args: Some(2),
        description: "Convert national to alphanumeric (UTF-16 to UTF-8)",
    },
    IntrinsicFunction {
        name: "NATIONAL-OF",
        category: FunctionCategory::String,
        result_type: FunctionResultType::National,
        min_args: 1,
        max_args: Some(2),
        description: "Convert alphanumeric to national (UTF-8 to UTF-16)",
    },
    // Numeric functions
    IntrinsicFunction {
        name: "E",
        category: FunctionCategory::Numeric,
        result_type: FunctionResultType::Numeric,
        min_args: 0,
        max_args: Some(0),
        description: "Euler's number (2.71828...)",
    },
    IntrinsicFunction {
        name: "PI",
        category: FunctionCategory::Numeric,
        result_type: FunctionResultType::Numeric,
        min_args: 0,
        max_args: Some(0),
        description: "Pi (3.14159...)",
    },
    IntrinsicFunction {
        name: "EXP",
        category: FunctionCategory::Numeric,
        result_type: FunctionResultType::Numeric,
        min_args: 1,
        max_args: Some(1),
        description: "e raised to power",
    },
    IntrinsicFunction {
        name: "EXP10",
        category: FunctionCategory::Numeric,
        result_type: FunctionResultType::Numeric,
        min_args: 1,
        max_args: Some(1),
        description: "10 raised to power",
    },
    IntrinsicFunction {
        name: "LOG",
        category: FunctionCategory::Numeric,
        result_type: FunctionResultType::Numeric,
        min_args: 1,
        max_args: Some(1),
        description: "Natural logarithm",
    },
    IntrinsicFunction {
        name: "LOG10",
        category: FunctionCategory::Numeric,
        result_type: FunctionResultType::Numeric,
        min_args: 1,
        max_args: Some(1),
        description: "Base-10 logarithm",
    },
    IntrinsicFunction {
        name: "SQRT",
        category: FunctionCategory::Numeric,
        result_type: FunctionResultType::Numeric,
        min_args: 1,
        max_args: Some(1),
        description: "Square root",
    },
    IntrinsicFunction {
        name: "ABS",
        category: FunctionCategory::Numeric,
        result_type: FunctionResultType::Numeric,
        min_args: 1,
        max_args: Some(1),
        description: "Absolute value",
    },
    IntrinsicFunction {
        name: "SIN",
        category: FunctionCategory::Numeric,
        result_type: FunctionResultType::Numeric,
        min_args: 1,
        max_args: Some(1),
        description: "Sine",
    },
    IntrinsicFunction {
        name: "COS",
        category: FunctionCategory::Numeric,
        result_type: FunctionResultType::Numeric,
        min_args: 1,
        max_args: Some(1),
        description: "Cosine",
    },
    IntrinsicFunction {
        name: "TAN",
        category: FunctionCategory::Numeric,
        result_type: FunctionResultType::Numeric,
        min_args: 1,
        max_args: Some(1),
        description: "Tangent",
    },
    IntrinsicFunction {
        name: "MIN",
        category: FunctionCategory::Numeric,
        result_type: FunctionResultType::Numeric,
        min_args: 1,
        max_args: None,
        description: "Minimum value",
    },
    IntrinsicFunction {
        name: "MAX",
        category: FunctionCategory::Numeric,
        result_type: FunctionResultType::Numeric,
        min_args: 1,
        max_args: None,
        description: "Maximum value",
    },
    IntrinsicFunction {
        name: "SUM",
        category: FunctionCategory::Numeric,
        result_type: FunctionResultType::Numeric,
        min_args: 1,
        max_args: None,
        description: "Sum of values",
    },
    IntrinsicFunction {
        name: "MEAN",
        category: FunctionCategory::Numeric,
        result_type: FunctionResultType::Numeric,
        min_args: 1,
        max_args: None,
        description: "Arithmetic mean",
    },
    IntrinsicFunction {
        name: "MEDIAN",
        category: FunctionCategory::Numeric,
        result_type: FunctionResultType::Numeric,
        min_args: 1,
        max_args: None,
        description: "Median value",
    },
    IntrinsicFunction {
        name: "VARIANCE",
        category: FunctionCategory::Numeric,
        result_type: FunctionResultType::Numeric,
        min_args: 1,
        max_args: None,
        description: "Variance",
    },
    IntrinsicFunction {
        name: "STANDARD-DEVIATION",
        category: FunctionCategory::Numeric,
        result_type: FunctionResultType::Numeric,
        min_args: 1,
        max_args: None,
        description: "Standard deviation",
    },
    IntrinsicFunction {
        name: "MOD",
        category: FunctionCategory::Numeric,
        result_type: FunctionResultType::Integer,
        min_args: 2,
        max_args: Some(2),
        description: "Modulo operation",
    },
    IntrinsicFunction {
        name: "REM",
        category: FunctionCategory::Numeric,
        result_type: FunctionResultType::Numeric,
        min_args: 2,
        max_args: Some(2),
        description: "Remainder",
    },
    IntrinsicFunction {
        name: "INTEGER",
        category: FunctionCategory::Numeric,
        result_type: FunctionResultType::Integer,
        min_args: 1,
        max_args: Some(1),
        description: "Truncate to integer",
    },
    IntrinsicFunction {
        name: "INTEGER-PART",
        category: FunctionCategory::Numeric,
        result_type: FunctionResultType::Integer,
        min_args: 1,
        max_args: Some(1),
        description: "Integer part",
    },
    // Inverse trigonometric functions (implementations exist in numeric.rs)
    IntrinsicFunction {
        name: "ACOS",
        category: FunctionCategory::Numeric,
        result_type: FunctionResultType::Numeric,
        min_args: 1,
        max_args: Some(1),
        description: "Arc cosine",
    },
    IntrinsicFunction {
        name: "ASIN",
        category: FunctionCategory::Numeric,
        result_type: FunctionResultType::Numeric,
        min_args: 1,
        max_args: Some(1),
        description: "Arc sine",
    },
    IntrinsicFunction {
        name: "ATAN",
        category: FunctionCategory::Numeric,
        result_type: FunctionResultType::Numeric,
        min_args: 1,
        max_args: Some(1),
        description: "Arc tangent",
    },
    IntrinsicFunction {
        name: "SIGN",
        category: FunctionCategory::Numeric,
        result_type: FunctionResultType::Integer,
        min_args: 1,
        max_args: Some(1),
        description: "Sign of number (-1, 0, or 1)",
    },
    IntrinsicFunction {
        name: "FACTORIAL",
        category: FunctionCategory::Numeric,
        result_type: FunctionResultType::Integer,
        min_args: 1,
        max_args: Some(1),
        description: "Factorial (n!)",
    },
    IntrinsicFunction {
        name: "RANDOM",
        category: FunctionCategory::Numeric,
        result_type: FunctionResultType::Numeric,
        min_args: 0,
        max_args: Some(1),
        description: "Pseudo-random number between 0 and 1",
    },
    IntrinsicFunction {
        name: "ORD-MIN",
        category: FunctionCategory::Numeric,
        result_type: FunctionResultType::Integer,
        min_args: 1,
        max_args: None,
        description: "Ordinal position of minimum value",
    },
    IntrinsicFunction {
        name: "ORD-MAX",
        category: FunctionCategory::Numeric,
        result_type: FunctionResultType::Integer,
        min_args: 1,
        max_args: None,
        description: "Ordinal position of maximum value",
    },
    IntrinsicFunction {
        name: "RANGE",
        category: FunctionCategory::Numeric,
        result_type: FunctionResultType::Numeric,
        min_args: 1,
        max_args: None,
        description: "Difference between maximum and minimum values",
    },
    IntrinsicFunction {
        name: "MIDRANGE",
        category: FunctionCategory::Numeric,
        result_type: FunctionResultType::Numeric,
        min_args: 1,
        max_args: None,
        description: "Mean of minimum and maximum values",
    },
    // Financial functions
    IntrinsicFunction {
        name: "ANNUITY",
        category: FunctionCategory::Numeric,
        result_type: FunctionResultType::Numeric,
        min_args: 2,
        max_args: Some(2),
        description: "Ratio of annuity paid at given rate for given periods",
    },
    IntrinsicFunction {
        name: "PRESENT-VALUE",
        category: FunctionCategory::Numeric,
        result_type: FunctionResultType::Numeric,
        min_args: 2,
        max_args: None,
        description: "Present value of future amounts at discount rate",
    },
    // Character-handling functions (implementations exist in string.rs)
    IntrinsicFunction {
        name: "BYTE-LENGTH",
        category: FunctionCategory::String,
        result_type: FunctionResultType::Integer,
        min_args: 1,
        max_args: Some(1),
        description: "Length in bytes",
    },
    IntrinsicFunction {
        name: "CHAR",
        category: FunctionCategory::String,
        result_type: FunctionResultType::Alphanumeric,
        min_args: 1,
        max_args: Some(1),
        description: "Character for ordinal position",
    },
    IntrinsicFunction {
        name: "ORD",
        category: FunctionCategory::String,
        result_type: FunctionResultType::Integer,
        min_args: 1,
        max_args: Some(1),
        description: "Ordinal position of character",
    },
    IntrinsicFunction {
        name: "NUMVAL",
        category: FunctionCategory::String,
        result_type: FunctionResultType::Numeric,
        min_args: 1,
        max_args: Some(1),
        description: "Numeric value of alphanumeric string",
    },
    IntrinsicFunction {
        name: "NUMVAL-C",
        category: FunctionCategory::String,
        result_type: FunctionResultType::Numeric,
        min_args: 1,
        max_args: Some(2),
        description: "Numeric value of currency-formatted string",
    },
    IntrinsicFunction {
        name: "NUMVAL-F",
        category: FunctionCategory::String,
        result_type: FunctionResultType::Numeric,
        min_args: 1,
        max_args: Some(1),
        description: "Numeric value of floating-point string",
    },
    IntrinsicFunction {
        name: "TEST-NUMVAL",
        category: FunctionCategory::String,
        result_type: FunctionResultType::Integer,
        min_args: 1,
        max_args: Some(1),
        description: "Test if string is valid for NUMVAL (0=valid)",
    },
    IntrinsicFunction {
        name: "TEST-NUMVAL-C",
        category: FunctionCategory::String,
        result_type: FunctionResultType::Integer,
        min_args: 1,
        max_args: Some(2),
        description: "Test if string is valid for NUMVAL-C (0=valid)",
    },
    IntrinsicFunction {
        name: "TEST-NUMVAL-F",
        category: FunctionCategory::String,
        result_type: FunctionResultType::Integer,
        min_args: 1,
        max_args: Some(1),
        description: "Test if string is valid for NUMVAL-F (0=valid)",
    },
    IntrinsicFunction {
        name: "HEX-OF",
        category: FunctionCategory::String,
        result_type: FunctionResultType::Alphanumeric,
        min_args: 1,
        max_args: Some(1),
        description: "Hexadecimal representation of string",
    },
    IntrinsicFunction {
        name: "HEX-TO-CHAR",
        category: FunctionCategory::String,
        result_type: FunctionResultType::Alphanumeric,
        min_args: 1,
        max_args: Some(1),
        description: "Character string from hexadecimal",
    },
    IntrinsicFunction {
        name: "BIT-OF",
        category: FunctionCategory::String,
        result_type: FunctionResultType::Alphanumeric,
        min_args: 1,
        max_args: Some(1),
        description: "Bit representation of string",
    },
    IntrinsicFunction {
        name: "BIT-TO-CHAR",
        category: FunctionCategory::String,
        result_type: FunctionResultType::Alphanumeric,
        min_args: 1,
        max_args: Some(1),
        description: "Character string from bit representation",
    },
    IntrinsicFunction {
        name: "UUID4",
        category: FunctionCategory::General,
        result_type: FunctionResultType::Alphanumeric,
        min_args: 0,
        max_args: Some(0),
        description: "Generate RFC 4122 UUID v4",
    },
    IntrinsicFunction {
        name: "CONTENT-OF",
        category: FunctionCategory::General,
        result_type: FunctionResultType::Alphanumeric,
        min_args: 1,
        max_args: Some(2),
        description: "Content of environment variable or file",
    },
    // DateTime functions
    IntrinsicFunction {
        name: "CURRENT-DATE",
        category: FunctionCategory::DateTime,
        result_type: FunctionResultType::Alphanumeric,
        min_args: 0,
        max_args: Some(0),
        description: "Current date and time (21 chars)",
    },
    IntrinsicFunction {
        name: "WHEN-COMPILED",
        category: FunctionCategory::DateTime,
        result_type: FunctionResultType::Alphanumeric,
        min_args: 0,
        max_args: Some(0),
        description: "Compilation date and time",
    },
    IntrinsicFunction {
        name: "DATE-OF-INTEGER",
        category: FunctionCategory::DateTime,
        result_type: FunctionResultType::Integer,
        min_args: 1,
        max_args: Some(1),
        description: "Convert integer date to YYYYMMDD",
    },
    IntrinsicFunction {
        name: "INTEGER-OF-DATE",
        category: FunctionCategory::DateTime,
        result_type: FunctionResultType::Integer,
        min_args: 1,
        max_args: Some(1),
        description: "Convert YYYYMMDD to integer date",
    },
    IntrinsicFunction {
        name: "DAY-OF-INTEGER",
        category: FunctionCategory::DateTime,
        result_type: FunctionResultType::Integer,
        min_args: 1,
        max_args: Some(1),
        description: "Day number from integer date",
    },
    IntrinsicFunction {
        name: "INTEGER-OF-DAY",
        category: FunctionCategory::DateTime,
        result_type: FunctionResultType::Integer,
        min_args: 1,
        max_args: Some(1),
        description: "Integer date from YYYYDDD",
    },
    IntrinsicFunction {
        name: "DATE-TO-YYYYMMDD",
        category: FunctionCategory::DateTime,
        result_type: FunctionResultType::Integer,
        min_args: 2,
        max_args: Some(3),
        description: "Convert date to 4-digit year YYYYMMDD format",
    },
    IntrinsicFunction {
        name: "YEAR-TO-YYYY",
        category: FunctionCategory::DateTime,
        result_type: FunctionResultType::Integer,
        min_args: 2,
        max_args: Some(3),
        description: "Convert 2-digit year to 4-digit year",
    },
    IntrinsicFunction {
        name: "DAY-TO-YYYYDDD",
        category: FunctionCategory::DateTime,
        result_type: FunctionResultType::Integer,
        min_args: 2,
        max_args: Some(3),
        description: "Convert Julian day to 4-digit year YYYYDDD format",
    },
    IntrinsicFunction {
        name: "COMBINED-DATETIME",
        category: FunctionCategory::DateTime,
        result_type: FunctionResultType::Numeric,
        min_args: 2,
        max_args: Some(2),
        description: "Combine integer date and seconds past midnight",
    },
    IntrinsicFunction {
        name: "SECONDS-PAST-MIDNIGHT",
        category: FunctionCategory::DateTime,
        result_type: FunctionResultType::Numeric,
        min_args: 0,
        max_args: Some(0),
        description: "Seconds since midnight",
    },
    IntrinsicFunction {
        name: "TEST-DATE-YYYYMMDD",
        category: FunctionCategory::DateTime,
        result_type: FunctionResultType::Integer,
        min_args: 1,
        max_args: Some(1),
        description: "Test if YYYYMMDD date is valid (0=valid)",
    },
    IntrinsicFunction {
        name: "TEST-DAY-YYYYDDD",
        category: FunctionCategory::DateTime,
        result_type: FunctionResultType::Integer,
        min_args: 1,
        max_args: Some(1),
        description: "Test if YYYYDDD Julian date is valid (0=valid)",
    },
];

/// Look up an intrinsic function by name.
pub fn lookup_function(name: &str) -> Option<&'static IntrinsicFunction> {
    let upper = name.to_uppercase();
    INTRINSIC_FUNCTIONS.iter().find(|f| f.name == upper)
}

/// Check if a name is an intrinsic function.
pub fn is_intrinsic_function(name: &str) -> bool {
    lookup_function(name).is_some()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lookup_function() {
        assert!(lookup_function("TRIM").is_some());
        assert!(lookup_function("trim").is_some());
        assert!(lookup_function("CONCATENATE").is_some());
        assert!(lookup_function("UNKNOWN").is_none());
    }

    #[test]
    fn test_function_registry() {
        let trim = lookup_function("TRIM").unwrap();
        assert_eq!(trim.name, "TRIM");
        assert_eq!(trim.category, FunctionCategory::String);
        assert_eq!(trim.min_args, 1);
        assert_eq!(trim.max_args, Some(2));
    }

    #[test]
    fn test_numeric_functions() {
        assert!(lookup_function("SIN").is_some());
        assert!(lookup_function("COS").is_some());
        assert!(lookup_function("LOG10").is_some());
    }

    #[test]
    fn test_previously_orphaned_trig_functions() {
        assert!(lookup_function("ACOS").is_some());
        assert!(lookup_function("ASIN").is_some());
        assert!(lookup_function("ATAN").is_some());
    }

    #[test]
    fn test_previously_orphaned_numeric_functions() {
        assert!(lookup_function("SIGN").is_some());
        assert!(lookup_function("FACTORIAL").is_some());
        assert!(lookup_function("RANDOM").is_some());
        assert!(lookup_function("ORD-MIN").is_some());
        assert!(lookup_function("ORD-MAX").is_some());
        assert!(lookup_function("RANGE").is_some());
        assert!(lookup_function("MIDRANGE").is_some());
    }

    #[test]
    fn test_financial_functions() {
        let ann = lookup_function("ANNUITY").unwrap();
        assert_eq!(ann.min_args, 2);
        assert_eq!(ann.max_args, Some(2));

        let pv = lookup_function("PRESENT-VALUE").unwrap();
        assert_eq!(pv.min_args, 2);
        assert_eq!(pv.max_args, None);
    }

    #[test]
    fn test_previously_orphaned_string_functions() {
        assert!(lookup_function("BYTE-LENGTH").is_some());
        assert!(lookup_function("CHAR").is_some());
        assert!(lookup_function("ORD").is_some());
        assert!(lookup_function("NUMVAL").is_some());
        assert!(lookup_function("NUMVAL-C").is_some());
    }

    #[test]
    fn test_new_string_functions() {
        assert!(lookup_function("NUMVAL-F").is_some());
        assert!(lookup_function("TEST-NUMVAL").is_some());
        assert!(lookup_function("TEST-NUMVAL-C").is_some());
        assert!(lookup_function("TEST-NUMVAL-F").is_some());
        assert!(lookup_function("HEX-OF").is_some());
        assert!(lookup_function("HEX-TO-CHAR").is_some());
        assert!(lookup_function("BIT-OF").is_some());
        assert!(lookup_function("BIT-TO-CHAR").is_some());
    }

    #[test]
    fn test_general_functions() {
        assert!(lookup_function("UUID4").is_some());
        assert!(lookup_function("CONTENT-OF").is_some());
    }

    #[test]
    fn test_previously_orphaned_datetime_functions() {
        assert!(lookup_function("DATE-TO-YYYYMMDD").is_some());
        assert!(lookup_function("YEAR-TO-YYYY").is_some());
    }

    #[test]
    fn test_new_datetime_functions() {
        assert!(lookup_function("DAY-TO-YYYYDDD").is_some());
        assert!(lookup_function("COMBINED-DATETIME").is_some());
        assert!(lookup_function("SECONDS-PAST-MIDNIGHT").is_some());
        assert!(lookup_function("TEST-DATE-YYYYMMDD").is_some());
        assert!(lookup_function("TEST-DAY-YYYYDDD").is_some());
    }

    #[test]
    fn test_total_function_count() {
        // We should have 64+ functions registered now
        assert!(
            INTRINSIC_FUNCTIONS.len() >= 64,
            "Expected at least 64 registered functions, got {}",
            INTRINSIC_FUNCTIONS.len()
        );
    }
}
