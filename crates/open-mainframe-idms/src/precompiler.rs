//! IDMS-104: COBOL DML Precompiler (4 stories).
//!
//! Parses `EXEC IDMS ... END-EXEC` statements embedded in COBOL source
//! code and transforms them into standard COBOL CALL statements targeting
//! the IDMS runtime (`IDMSCONN`).  Also generates SUBSCHEMA-CTRL copybook
//! entries for the working-storage section.

// ---------------------------------------------------------------------------
//  Precompiler
// ---------------------------------------------------------------------------

/// COBOL DML precompiler for IDMS.
///
/// Scans COBOL source for `EXEC IDMS` blocks, replaces them with CALL
/// statements, and tracks the subschema/schema bindings required.
#[derive(Debug)]
pub struct DmlPrecompiler {
    /// Subschema name referenced by the program.
    pub subschema_name: Option<String>,
    /// Schema name referenced by the program.
    pub schema_name: Option<String>,
    /// Collected DML statements (raw text between EXEC IDMS ... END-EXEC).
    pub dml_statements: Vec<String>,
}

impl DmlPrecompiler {
    /// Create a new precompiler instance.
    pub fn new() -> Self {
        Self {
            subschema_name: None,
            schema_name: None,
            dml_statements: Vec::new(),
        }
    }

    /// Process COBOL source, returning the transformed source.
    ///
    /// Each `EXEC IDMS ... END-EXEC` block is replaced with a CALL to
    /// `IDMSCONN` passing the DML function code.
    pub fn process(&mut self, source: &str) -> String {
        let mut output = String::with_capacity(source.len());
        let mut lines = source.lines().peekable();

        while let Some(line) = lines.next() {
            let trimmed = line.trim();

            // Detect EXEC IDMS.
            if let Some(rest) = strip_prefix_ci(trimmed, "EXEC IDMS") {
                let rest = rest.trim();

                // Check for single-line EXEC IDMS ... END-EXEC.
                if let Some(dml_text) = strip_suffix_ci(rest, "END-EXEC") {
                    let dml = dml_text.trim().trim_end_matches('.');
                    self.dml_statements.push(dml.to_string());
                    self.detect_bind(dml);
                    let call = self.generate_call(dml);
                    output.push_str(&call);
                    output.push('\n');
                    continue;
                }

                // Multi-line: accumulate until END-EXEC.
                let mut dml_buf = rest.to_string();
                for next_line in lines.by_ref() {
                    let next_trimmed = next_line.trim();
                    if let Some(before) = strip_suffix_ci(next_trimmed, "END-EXEC") {
                        if !before.trim().is_empty() {
                            dml_buf.push(' ');
                            dml_buf.push_str(before.trim());
                        }
                        break;
                    }
                    dml_buf.push(' ');
                    dml_buf.push_str(next_trimmed);
                }
                let dml = dml_buf.trim().trim_end_matches('.').to_string();
                self.dml_statements.push(dml.clone());
                self.detect_bind(&dml);
                let call = self.generate_call(&dml);
                output.push_str(&call);
                output.push('\n');
            } else {
                output.push_str(line);
                output.push('\n');
            }
        }

        output
    }

    /// Generate a SUBSCHEMA-CTRL copybook block for WORKING-STORAGE.
    ///
    /// This is the standard IDMS control block that must appear in
    /// any program that issues DML calls.
    pub fn generate_subschema_ctrl(&self) -> String {
        let sub = self
            .subschema_name
            .as_deref()
            .unwrap_or("UNKNOWN");
        let sch = self.schema_name.as_deref().unwrap_or("UNKNOWN");
        format!(
            "\
       01  SUBSCHEMA-CTRL.
           05  PROGRAM-NAME     PIC X(8)  VALUE SPACES.
           05  ERROR-STATUS     PIC X(4)  VALUE '1400'.
           05  DBKEY            PIC S9(8) COMP.
           05  RECORD-NAME      PIC X(16) VALUE SPACES.
           05  AREA-NAME        PIC X(16) VALUE SPACES.
           05  SUBSCHEMA-NAME   PIC X(8)  VALUE '{sub}'.
           05  SCHEMA-NAME      PIC X(8)  VALUE '{sch}'."
        )
    }

    // -- internal --

    fn detect_bind(&mut self, dml: &str) {
        let upper = dml.to_uppercase();
        if upper.starts_with("BIND RUN-UNIT") {
            // BIND RUN-UNIT FOR subschema OF schema
            let tokens: Vec<&str> = upper.split_whitespace().collect();
            for (i, tok) in tokens.iter().enumerate() {
                if *tok == "FOR" && i + 1 < tokens.len() {
                    self.subschema_name = Some(tokens[i + 1].to_string());
                }
                if *tok == "OF" && i + 1 < tokens.len() {
                    self.schema_name = Some(tokens[i + 1].to_string());
                }
            }
        }
    }

    fn generate_call(&self, dml: &str) -> String {
        let func = Self::extract_function(dml);
        format!(
            "           CALL 'IDMSCONN' USING SUBSCHEMA-CTRL\n\
             *>         DML: {func}"
        )
    }

    fn extract_function(dml: &str) -> &str {
        dml.split_whitespace().next().unwrap_or("UNKNOWN")
    }
}

impl Default for DmlPrecompiler {
    fn default() -> Self {
        Self::new()
    }
}

// ---------------------------------------------------------------------------
//  Helpers
// ---------------------------------------------------------------------------

fn strip_prefix_ci<'a>(s: &'a str, prefix: &str) -> Option<&'a str> {
    if s.len() >= prefix.len()
        && s[..prefix.len()].eq_ignore_ascii_case(prefix)
    {
        Some(&s[prefix.len()..])
    } else {
        None
    }
}

fn strip_suffix_ci<'a>(s: &'a str, suffix: &str) -> Option<&'a str> {
    if s.len() >= suffix.len()
        && s[s.len() - suffix.len()..].eq_ignore_ascii_case(suffix)
    {
        Some(&s[..s.len() - suffix.len()])
    } else {
        None
    }
}

// ---------------------------------------------------------------------------
//  Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn single_line_exec_idms() {
        let source = "\
       PROCEDURE DIVISION.
           EXEC IDMS BIND RUN-UNIT FOR EMPSUB OF EMPSCHM END-EXEC
           EXEC IDMS FIND FIRST EMPLOYEE WITHIN DEPT-EMP END-EXEC
           STOP RUN.
";
        let mut pc = DmlPrecompiler::new();
        let output = pc.process(source);
        assert!(output.contains("CALL 'IDMSCONN'"));
        assert_eq!(pc.dml_statements.len(), 2);
        assert_eq!(pc.subschema_name.as_deref(), Some("EMPSUB"));
        assert_eq!(pc.schema_name.as_deref(), Some("EMPSCHM"));
    }

    #[test]
    fn multi_line_exec_idms() {
        let source = "\
           EXEC IDMS
               FIND NEXT EMPLOYEE
               WITHIN DEPT-EMP
           END-EXEC
";
        let mut pc = DmlPrecompiler::new();
        let output = pc.process(source);
        assert!(output.contains("CALL 'IDMSCONN'"));
        assert_eq!(pc.dml_statements.len(), 1);
        assert!(pc.dml_statements[0].contains("FIND NEXT EMPLOYEE"));
    }

    #[test]
    fn subschema_ctrl_generation() {
        let mut pc = DmlPrecompiler::new();
        pc.subschema_name = Some("EMPSUB".into());
        pc.schema_name = Some("EMPSCHM".into());
        let ctrl = pc.generate_subschema_ctrl();
        assert!(ctrl.contains("SUBSCHEMA-CTRL"));
        assert!(ctrl.contains("EMPSUB"));
        assert!(ctrl.contains("EMPSCHM"));
        assert!(ctrl.contains("ERROR-STATUS"));
    }

    #[test]
    fn no_exec_idms_passthrough() {
        let source = "       DISPLAY 'HELLO'.\n       STOP RUN.\n";
        let mut pc = DmlPrecompiler::new();
        let output = pc.process(source);
        assert!(output.contains("DISPLAY 'HELLO'"));
        assert_eq!(pc.dml_statements.len(), 0);
    }

    #[test]
    fn default_subschema_ctrl() {
        let pc = DmlPrecompiler::new();
        let ctrl = pc.generate_subschema_ctrl();
        assert!(ctrl.contains("UNKNOWN"));
    }

    #[test]
    fn precompiler_default_trait() {
        let pc = DmlPrecompiler::default();
        assert!(pc.subschema_name.is_none());
    }
}
