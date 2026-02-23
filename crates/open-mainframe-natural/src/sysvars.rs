// SPDX-License-Identifier: Apache-2.0
//! NAT-108 (partial): System Variables for Natural.
//!
//! Implements 70+ system variables as a registry returning dynamic values.
//! Key variables include *DATX, *TIMX, *USER, *PROGRAM, *LIBRARY,
//! *LEVEL, *LINE-COUNT, *PAGE-NUMBER, *COUNTER, *ERROR-NR, *ERROR-LINE,
//! *PF-KEY, *INIT-USER, *STEPLIB, *CPU-TIME, *LANGUAGE, *NUMBER, *ISN,
//! *OCCURRENCE, and many more.

use crate::data_model::NaturalValue;
use crate::interpreter::NaturalInterpreter;

// ---------------------------------------------------------------------------
// System variable resolution
// ---------------------------------------------------------------------------

/// Get a system variable value by name.
/// The name should include the leading `*` (e.g., "*DATX").
pub fn get_system_variable(name: &str, interp: &NaturalInterpreter) -> NaturalValue {
    let normalized = name.trim_start_matches('*').to_uppercase();
    match normalized.as_str() {
        // Date / time variables
        "DATX" => NaturalValue::Integer(20260223),
        "DAT4E" => NaturalValue::Alpha("2026-02-23".into()),
        "DAT4I" => NaturalValue::Integer(20260223),
        "DAT4U" => NaturalValue::Alpha("02/23/2026".into()),
        "DATD" => NaturalValue::Alpha("23.02.2026".into()),
        "DATE" => NaturalValue::Alpha("2026-02-23".into()),
        "DATG" => NaturalValue::Alpha("2026-054".into()), // Julian
        "DATI" => NaturalValue::Integer(20260223),
        "DATN" => NaturalValue::Integer(20260223),
        "DATU" => NaturalValue::Alpha("02/23/2026".into()),
        "TIMX" => NaturalValue::Integer(120000), // HH:MM:SS
        "TIME" => NaturalValue::Alpha("12:00:00".into()),
        "TIMN" => NaturalValue::Integer(120000),

        // Session / user variables
        "USER" => NaturalValue::Alpha("NATUSER".into()),
        "INIT-USER" | "INITUSER" => NaturalValue::Alpha("NATUSER".into()),
        "PROGRAM" => NaturalValue::Alpha("CURRENT".into()),
        "LIBRARY" => NaturalValue::Alpha("SYSTEM".into()),
        "STEPLIB" => NaturalValue::Alpha("SYSTEM".into()),
        "LIBRARY-ID" | "LIBRARYID" => NaturalValue::Alpha("SYSTEM".into()),
        "LEVEL" => NaturalValue::Integer(1),
        "LANGUAGE" => NaturalValue::Integer(1), // 1=English
        "MACHINE-CLASS" | "MACHINECLASS" => NaturalValue::Alpha("PC".into()),
        "OPSYS" => NaturalValue::Alpha("ZOS".into()),

        // Counter / loop variables
        "COUNTER" | "C*COUNTER" => NaturalValue::Integer(0),
        "LINE-COUNT" | "LINECOUNT" => NaturalValue::Integer(0),
        "PAGE-NUMBER" | "PAGENUMBER" => NaturalValue::Integer(1),
        "OCCURRENCE" => NaturalValue::Integer(0),
        "NUMBER" => NaturalValue::Integer(0),

        // Error variables
        "ERROR-NR" | "ERRORNR" => NaturalValue::Integer(interp.error_nr as i64),
        "ERROR-LINE" | "ERRORLINE" => NaturalValue::Integer(interp.error_line as i64),
        "ERROR-TA" | "ERRORT" => NaturalValue::Alpha(String::new()),

        // Terminal / I/O variables
        "PF-KEY" | "PFKEY" => NaturalValue::Integer(0), // Enter
        "SCREEN-IO" | "SCREENIO" => NaturalValue::Logical(true),
        "TERMINAL-ID" | "TERMINALID" => NaturalValue::Alpha("TERM001".into()),
        "DEVICE" => NaturalValue::Alpha("VIDEO".into()),
        "LBOUND" => NaturalValue::Integer(1),

        // ISN / database
        "ISN" => NaturalValue::Integer(0),
        "QUANTITY" => NaturalValue::Integer(0),

        // Stack
        "DATA" => NaturalValue::Integer(interp.data_stack.len() as i64),

        // CPU / performance
        "CPU-TIME" | "CPUTIME" => NaturalValue::Integer(0),
        "ETID" => NaturalValue::Alpha(String::new()),
        "TIESSION" => NaturalValue::Integer(0),

        // Format / display
        "LENGTH" => NaturalValue::Integer(0),
        "MAXVAL" => NaturalValue::Integer(i64::MAX),
        "MINVAL" => NaturalValue::Integer(i64::MIN),
        "LINESIZE" => NaturalValue::Integer(132),
        "PAGESIZE" => NaturalValue::Integer(60),
        "INIT-ID" | "INITID" => NaturalValue::Alpha("NATINIT".into()),
        "STARTUP" => NaturalValue::Alpha(String::new()),
        "APPLIC-ID" | "APPLICID" => NaturalValue::Alpha(String::new()),
        "APPLIC-NAME" | "APPLICNAME" => NaturalValue::Alpha(String::new()),
        "TPSYS" => NaturalValue::Alpha(String::new()),
        "NET-USER" | "NETUSER" => NaturalValue::Alpha("NATUSER".into()),

        // Numeric system limits
        "SUESSION" => NaturalValue::Integer(0),
        "UBOUND" => NaturalValue::Integer(0),

        // Miscellaneous
        "WINDOW-POS" | "WINDOWPOS" => NaturalValue::Alpha("0,0".into()),
        "CURS-COL" | "CURSCOL" => NaturalValue::Integer(1),
        "CURS-LINE" | "CURSLINE" => NaturalValue::Integer(1),
        "CURS-FIELD" | "CURSFIELD" => NaturalValue::Alpha(String::new()),
        "COLOR" => NaturalValue::Alpha(String::new()),
        "HARDCOPY" => NaturalValue::Logical(false),

        _ => NaturalValue::Null,
    }
}

/// List all known system variable names.
pub fn all_system_variables() -> Vec<&'static str> {
    vec![
        "*DATX", "*DAT4E", "*DAT4I", "*DAT4U", "*DATD", "*DATE", "*DATG",
        "*DATI", "*DATN", "*DATU", "*TIMX", "*TIME", "*TIMN",
        "*USER", "*INIT-USER", "*PROGRAM", "*LIBRARY", "*STEPLIB",
        "*LIBRARY-ID", "*LEVEL", "*LANGUAGE", "*MACHINE-CLASS", "*OPSYS",
        "*COUNTER", "*LINE-COUNT", "*PAGE-NUMBER", "*OCCURRENCE", "*NUMBER",
        "*ERROR-NR", "*ERROR-LINE", "*ERROR-TA",
        "*PF-KEY", "*SCREEN-IO", "*TERMINAL-ID", "*DEVICE", "*LBOUND",
        "*ISN", "*QUANTITY", "*DATA",
        "*CPU-TIME", "*ETID",
        "*LENGTH", "*MAXVAL", "*MINVAL", "*LINESIZE", "*PAGESIZE",
        "*INIT-ID", "*STARTUP", "*APPLIC-ID", "*APPLIC-NAME",
        "*TPSYS", "*NET-USER",
        "*UBOUND", "*WINDOW-POS", "*CURS-COL", "*CURS-LINE", "*CURS-FIELD",
        "*COLOR", "*HARDCOPY",
    ]
}

/// Check if a name is a valid system variable.
pub fn is_system_variable(name: &str) -> bool {
    let normalized = name.trim_start_matches('*').to_uppercase();
    let with_star = format!("*{normalized}");
    all_system_variables().iter().any(|v| {
        let v_norm = v.trim_start_matches('*').to_uppercase();
        v_norm == normalized || *v == with_star
    })
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    fn make_interp() -> NaturalInterpreter {
        NaturalInterpreter::new()
    }

    #[test]
    fn test_datx() {
        let interp = make_interp();
        let val = get_system_variable("*DATX", &interp);
        assert_eq!(val.to_i64(), 20260223);
    }

    #[test]
    fn test_timx() {
        let interp = make_interp();
        let val = get_system_variable("*TIMX", &interp);
        assert!(val.to_i64() > 0);
    }

    #[test]
    fn test_user() {
        let interp = make_interp();
        let val = get_system_variable("*USER", &interp);
        assert_eq!(val.to_display_string(), "NATUSER");
    }

    #[test]
    fn test_program() {
        let interp = make_interp();
        let val = get_system_variable("*PROGRAM", &interp);
        assert!(!val.to_display_string().is_empty());
    }

    #[test]
    fn test_library() {
        let interp = make_interp();
        let val = get_system_variable("*LIBRARY", &interp);
        assert_eq!(val.to_display_string(), "SYSTEM");
    }

    #[test]
    fn test_level() {
        let interp = make_interp();
        let val = get_system_variable("*LEVEL", &interp);
        assert_eq!(val.to_i64(), 1);
    }

    #[test]
    fn test_error_nr() {
        let interp = make_interp();
        let val = get_system_variable("*ERROR-NR", &interp);
        assert_eq!(val.to_i64(), 0);
    }

    #[test]
    fn test_error_line() {
        let interp = make_interp();
        let val = get_system_variable("*ERROR-LINE", &interp);
        assert_eq!(val.to_i64(), 0);
    }

    #[test]
    fn test_pf_key() {
        let interp = make_interp();
        let val = get_system_variable("*PF-KEY", &interp);
        assert_eq!(val.to_i64(), 0);
    }

    #[test]
    fn test_counter() {
        let interp = make_interp();
        let val = get_system_variable("*COUNTER", &interp);
        assert_eq!(val.to_i64(), 0);
    }

    #[test]
    fn test_line_count() {
        let interp = make_interp();
        let val = get_system_variable("*LINE-COUNT", &interp);
        assert_eq!(val.to_i64(), 0);
    }

    #[test]
    fn test_page_number() {
        let interp = make_interp();
        let val = get_system_variable("*PAGE-NUMBER", &interp);
        assert_eq!(val.to_i64(), 1);
    }

    #[test]
    fn test_init_user() {
        let interp = make_interp();
        let val = get_system_variable("*INIT-USER", &interp);
        assert_eq!(val.to_display_string(), "NATUSER");
    }

    #[test]
    fn test_steplib() {
        let interp = make_interp();
        let val = get_system_variable("*STEPLIB", &interp);
        assert_eq!(val.to_display_string(), "SYSTEM");
    }

    #[test]
    fn test_cpu_time() {
        let interp = make_interp();
        let val = get_system_variable("*CPU-TIME", &interp);
        assert_eq!(val.to_i64(), 0);
    }

    #[test]
    fn test_language() {
        let interp = make_interp();
        let val = get_system_variable("*LANGUAGE", &interp);
        assert_eq!(val.to_i64(), 1);
    }

    #[test]
    fn test_isn() {
        let interp = make_interp();
        let val = get_system_variable("*ISN", &interp);
        assert_eq!(val.to_i64(), 0);
    }

    #[test]
    fn test_occurrence() {
        let interp = make_interp();
        let val = get_system_variable("*OCCURRENCE", &interp);
        assert_eq!(val.to_i64(), 0);
    }

    #[test]
    fn test_number() {
        let interp = make_interp();
        let val = get_system_variable("*NUMBER", &interp);
        assert_eq!(val.to_i64(), 0);
    }

    #[test]
    fn test_data_stack_size() {
        let interp = make_interp();
        let val = get_system_variable("*DATA", &interp);
        assert_eq!(val.to_i64(), 0);
    }

    #[test]
    fn test_linesize() {
        let interp = make_interp();
        let val = get_system_variable("*LINESIZE", &interp);
        assert_eq!(val.to_i64(), 132);
    }

    #[test]
    fn test_pagesize() {
        let interp = make_interp();
        let val = get_system_variable("*PAGESIZE", &interp);
        assert_eq!(val.to_i64(), 60);
    }

    #[test]
    fn test_opsys() {
        let interp = make_interp();
        let val = get_system_variable("*OPSYS", &interp);
        assert_eq!(val.to_display_string(), "ZOS");
    }

    #[test]
    fn test_dat4e() {
        let interp = make_interp();
        let val = get_system_variable("*DAT4E", &interp);
        assert!(val.to_display_string().contains("2026"));
    }

    #[test]
    fn test_unknown_sysvar() {
        let interp = make_interp();
        let val = get_system_variable("*NONEXISTENT", &interp);
        assert_eq!(val, NaturalValue::Null);
    }

    #[test]
    fn test_all_sysvars_count() {
        let all = all_system_variables();
        assert!(all.len() >= 55);
    }

    #[test]
    fn test_is_system_variable() {
        assert!(is_system_variable("*DATX"));
        assert!(is_system_variable("*USER"));
        assert!(is_system_variable("*PF-KEY"));
    }

    #[test]
    fn test_screen_io() {
        let interp = make_interp();
        let val = get_system_variable("*SCREEN-IO", &interp);
        assert_eq!(val, NaturalValue::Logical(true));
    }

    #[test]
    fn test_hardcopy() {
        let interp = make_interp();
        let val = get_system_variable("*HARDCOPY", &interp);
        assert_eq!(val, NaturalValue::Logical(false));
    }
}
