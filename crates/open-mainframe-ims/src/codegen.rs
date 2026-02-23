//! IMS-TM110: EXEC DLI Code Generation.
//!
//! Generates COBOL DL/I call source code for:
//! - CBLTDLI calls (classic interface)
//! - AIBTDLI calls (AIB-based interface)
//! - DIB (DL/I Interface Block) copybook generation

// ---------------------------------------------------------------------------
// CBLTDLI generation
// ---------------------------------------------------------------------------

/// Generate a COBOL `CALL 'CBLTDLI'` statement.
///
/// # Arguments
/// * `function` -- DL/I function code (e.g., "GU", "GN", "ISRT").
/// * `pcb` -- PCB variable name in the COBOL program.
/// * `ssa` -- optional list of SSA variable names.
///
/// # Returns
/// A string containing the generated COBOL CALL statement.
pub fn generate_cbltdli_call(function: &str, pcb: &str, ssa: &[&str]) -> String {
    let mut lines = Vec::new();
    lines.push("           CALL 'CBLTDLI' USING".to_string());
    lines.push(format!("               {}", function));
    lines.push(format!("               {}", pcb));
    lines.push("               IO-AREA".to_string());
    for (i, s) in ssa.iter().enumerate() {
        if i == ssa.len() - 1 {
            lines.push(format!("               {}.", s));
        } else {
            lines.push(format!("               {}", s));
        }
    }
    if ssa.is_empty() {
        // Terminate the last USING parameter with a period
        let last = lines.last_mut().unwrap();
        if !last.ends_with('.') {
            last.push('.');
        }
    }
    lines.join("\n")
}

// ---------------------------------------------------------------------------
// DIB copybook generation
// ---------------------------------------------------------------------------

/// Generate a COBOL copybook for the DL/I Interface Block (DIB).
///
/// The DIB is the area where IMS returns status information after each
/// DL/I call. This copybook defines the standard fields.
pub fn generate_dib_copybook() -> String {
    [
        "      *----------------------------------------------------------------*",
        "      * DL/I INTERFACE BLOCK (DIB) COPYBOOK                            *",
        "      *----------------------------------------------------------------*",
        "       01  DIB.",
        "           05  DIB-STATUS-CODE        PIC XX.",
        "           05  DIB-PROC-OPT           PIC X(4).",
        "           05  DIB-SEG-NAME           PIC X(8).",
        "           05  DIB-SEG-LEVEL          PIC XX.",
        "           05  DIB-KEY-FEEDBACK       PIC X(256).",
        "           05  DIB-KEY-FEEDBACK-LEN   PIC S9(8) COMP.",
        "           05  DIB-NUM-SEGS-RETURNED  PIC S9(8) COMP.",
    ]
    .join("\n")
}

// ---------------------------------------------------------------------------
// AIBTDLI generation
// ---------------------------------------------------------------------------

/// Generate a COBOL `CALL 'AIBTDLI'` statement.
///
/// The AIB (Application Interface Block) based interface is the modern
/// alternative to CBLTDLI, providing richer feedback.
///
/// # Arguments
/// * `function` -- DL/I function code.
/// * `aib` -- AIB variable name.
/// * `pcb` -- PCB variable name (referenced through the AIB).
/// * `ssa` -- optional SSA variable names.
pub fn generate_aibtdli_call(function: &str, aib: &str, pcb: &str, ssa: &[&str]) -> String {
    let _ = pcb; // PCB is referenced within the AIB structure
    let mut lines = Vec::new();
    lines.push("           CALL 'AIBTDLI' USING".to_string());
    lines.push(format!("               {}", function));
    lines.push(format!("               {}", aib));
    lines.push("               IO-AREA".to_string());
    for (i, s) in ssa.iter().enumerate() {
        if i == ssa.len() - 1 {
            lines.push(format!("               {}.", s));
        } else {
            lines.push(format!("               {}", s));
        }
    }
    if ssa.is_empty() {
        let last = lines.last_mut().unwrap();
        if !last.ends_with('.') {
            last.push('.');
        }
    }
    lines.join("\n")
}

/// Generate a COBOL copybook for the AIB (Application Interface Block).
pub fn generate_aib_copybook() -> String {
    [
        "      *----------------------------------------------------------------*",
        "      * APPLICATION INTERFACE BLOCK (AIB) COPYBOOK                     *",
        "      *----------------------------------------------------------------*",
        "       01  AIB.",
        "           05  AIB-ID                 PIC X(8)  VALUE 'DFSAIB  '.",
        "           05  AIB-LEN               PIC S9(8) COMP VALUE +264.",
        "           05  AIB-SUB-FUNC          PIC X(8).",
        "           05  AIB-RSRC-NAME         PIC X(8).",
        "           05  AIB-RSRC-TYPE         PIC X(8).",
        "           05  AIB-RETURN-CODE       PIC S9(8) COMP.",
        "           05  AIB-REASON-CODE       PIC S9(8) COMP.",
        "           05  AIB-STATUS-CODE       PIC XX.",
        "           05  FILLER                PIC X(218).",
    ]
    .join("\n")
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_cbltdli_gu_with_ssa() {
        let code = generate_cbltdli_call("GU-FUNC", "CUST-PCB", &["CUST-SSA"]);
        assert!(code.contains("CALL 'CBLTDLI'"));
        assert!(code.contains("GU-FUNC"));
        assert!(code.contains("CUST-PCB"));
        assert!(code.contains("IO-AREA"));
        assert!(code.contains("CUST-SSA."));
    }

    #[test]
    fn test_cbltdli_no_ssa() {
        let code = generate_cbltdli_call("GN-FUNC", "DB-PCB", &[]);
        assert!(code.contains("CALL 'CBLTDLI'"));
        assert!(code.contains("GN-FUNC"));
        assert!(code.contains("DB-PCB"));
        // Should end with a period
        assert!(code.contains('.'));
    }

    #[test]
    fn test_cbltdli_multiple_ssa() {
        let code = generate_cbltdli_call("GU-FUNC", "PCB1", &["SSA1", "SSA2", "SSA3"]);
        assert!(code.contains("SSA1"));
        assert!(code.contains("SSA2"));
        assert!(code.contains("SSA3."));
        // Only the last SSA should have a period
        let ssa1_count = code.matches("SSA1.").count();
        assert_eq!(ssa1_count, 0);
    }

    #[test]
    fn test_dib_copybook() {
        let copybook = generate_dib_copybook();
        assert!(copybook.contains("DIB"));
        assert!(copybook.contains("DIB-STATUS-CODE"));
        assert!(copybook.contains("DIB-SEG-NAME"));
        assert!(copybook.contains("DIB-KEY-FEEDBACK"));
        assert!(copybook.contains("PIC XX"));
    }

    #[test]
    fn test_aibtdli_call() {
        let code = generate_aibtdli_call("GU-FUNC", "MY-AIB", "MY-PCB", &["SSA1"]);
        assert!(code.contains("CALL 'AIBTDLI'"));
        assert!(code.contains("GU-FUNC"));
        assert!(code.contains("MY-AIB"));
        assert!(code.contains("SSA1."));
    }

    #[test]
    fn test_aibtdli_no_ssa() {
        let code = generate_aibtdli_call("GN-FUNC", "AIB1", "PCB1", &[]);
        assert!(code.contains("CALL 'AIBTDLI'"));
        assert!(code.contains('.'));
    }

    #[test]
    fn test_aib_copybook() {
        let copybook = generate_aib_copybook();
        assert!(copybook.contains("AIB"));
        assert!(copybook.contains("AIB-ID"));
        assert!(copybook.contains("AIB-RETURN-CODE"));
        assert!(copybook.contains("AIB-REASON-CODE"));
        assert!(copybook.contains("AIB-STATUS-CODE"));
        assert!(copybook.contains("DFSAIB"));
    }

    #[test]
    fn test_cbltdli_isrt() {
        let code = generate_cbltdli_call("ISRT-FUNC", "IO-PCB", &["MSG-SSA"]);
        assert!(code.contains("CALL 'CBLTDLI'"));
        assert!(code.contains("ISRT-FUNC"));
        assert!(code.contains("IO-PCB"));
    }

    #[test]
    fn test_dib_has_num_segs() {
        let copybook = generate_dib_copybook();
        assert!(copybook.contains("DIB-NUM-SEGS-RETURNED"));
    }
}
