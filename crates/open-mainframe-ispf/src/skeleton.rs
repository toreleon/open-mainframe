//! ISPF file tailoring — skeleton processing with variable substitution and control statements.
//!
//! Services:
//! - **FTOPEN** — begin file tailoring session
//! - **FTINCL** — process a skeleton file with variable substitution
//! - **FTCLOSE** — end session and produce output
//!
//! Control statements within skeletons:
//! - `)SEL cond` / `)ENDSEL` — conditional inclusion
//! - `)DOT tablename` / `)ENDDOT` — iterate over ISPF table rows
//! - `)SET var = value` — set a variable
//! - `)CM comment` — comment (ignored)
//! - `)IM skelname` — imbed (include) another skeleton
//! - `)NB` — no-blank: suppress blank line

use std::collections::HashMap;

use crate::dialog::IspfVarPools;
use crate::table::TableManager;

// ---------------------------------------------------------------------------
//  File tailoring engine
// ---------------------------------------------------------------------------

/// The ISPF file tailoring engine.
#[derive(Debug)]
pub struct FileTailor {
    /// Skeleton library: name → skeleton lines.
    skeletons: HashMap<String, Vec<String>>,
    /// Whether a tailoring session is open.
    session_open: bool,
    /// Accumulated output lines for the current session.
    output: Vec<String>,
}

impl Default for FileTailor {
    fn default() -> Self {
        Self::new()
    }
}

impl FileTailor {
    pub fn new() -> Self {
        Self {
            skeletons: HashMap::new(),
            session_open: false,
            output: Vec::new(),
        }
    }

    /// Register a skeleton in the library.
    pub fn load_skeleton(&mut self, name: &str, lines: Vec<String>) {
        self.skeletons.insert(name.to_uppercase(), lines);
    }

    /// FTOPEN — begin a file tailoring session.
    pub fn ftopen(&mut self) -> i32 {
        if self.session_open {
            return 8; // Already open.
        }
        self.session_open = true;
        self.output.clear();
        0
    }

    /// FTINCL — process a skeleton with variable substitution.
    pub fn ftincl(
        &mut self,
        skel_name: &str,
        vars: &IspfVarPools,
        tables: &mut TableManager,
    ) -> i32 {
        if !self.session_open {
            return 16; // No session open.
        }
        let upper = skel_name.to_uppercase();
        let lines = match self.skeletons.get(&upper) {
            Some(l) => l.clone(),
            None => return 8, // Skeleton not found.
        };

        self.process_lines(&lines, vars, tables);
        0
    }

    /// FTCLOSE — end the session and return the output.
    pub fn ftclose(&mut self) -> (i32, Vec<String>) {
        if !self.session_open {
            return (16, Vec::new());
        }
        self.session_open = false;
        let output = std::mem::take(&mut self.output);
        (0, output)
    }

    /// Get the current output (without closing).
    pub fn current_output(&self) -> &[String] {
        &self.output
    }

    // -----------------------------------------------------------------------
    //  Internal processing
    // -----------------------------------------------------------------------

    fn process_lines(
        &mut self,
        lines: &[String],
        vars: &IspfVarPools,
        tables: &mut TableManager,
    ) {
        let mut i = 0;
        while i < lines.len() {
            let line = &lines[i];
            let trimmed = line.trim();
            let upper = trimmed.to_uppercase();

            if upper.starts_with(")CM") {
                // Comment — skip.
                i += 1;
                continue;
            }

            if upper.starts_with(")NB") {
                // No-blank: just skip (suppresses an empty line).
                i += 1;
                continue;
            }

            if let Some(rest) = upper.strip_prefix(")SET ") {
                self.process_set(rest, vars);
                i += 1;
                continue;
            }

            if let Some(rest) = upper.strip_prefix(")SEL ") {
                // Find matching )ENDSEL.
                let (body, end_idx) = collect_block(lines, i + 1, ")ENDSEL");
                if self.eval_sel_condition(rest, vars) {
                    self.process_lines(&body, vars, tables);
                }
                i = end_idx + 1;
                continue;
            }

            if let Some(rest) = upper.strip_prefix(")DOT ") {
                let table_name = rest.trim().to_string();
                let (body, end_idx) = collect_block(lines, i + 1, ")ENDDOT");
                self.process_dot(&table_name, &body, vars, tables);
                i = end_idx + 1;
                continue;
            }

            if let Some(rest) = upper.strip_prefix(")IM ") {
                let skel_name = rest.trim();
                if let Some(sub_lines) = self.skeletons.get(skel_name).cloned() {
                    self.process_lines(&sub_lines, vars, tables);
                }
                i += 1;
                continue;
            }

            // Regular line — substitute variables and output.
            let substituted = substitute_line(line, vars);
            self.output.push(substituted);
            i += 1;
        }
    }

    fn process_set(&mut self, rest: &str, vars: &IspfVarPools) {
        // )SET VAR = VALUE
        // Note: we can't mutate vars since it's borrowed immutably here.
        // In real ISPF, )SET modifies the variable pool. We store in output
        // as a metadata marker for now. Real implementation would need &mut vars.
        let _ = rest;
        let _ = vars;
        // )SET is a no-op in this read-only context — callers should
        // pre-set variables before calling ftincl.
    }

    fn eval_sel_condition(&self, cond_str: &str, vars: &IspfVarPools) -> bool {
        // Simple condition parser: "&VAR = VALUE" or "&VAR NE VALUE"
        let cond = cond_str.trim();

        // Replace &var references with values.
        let resolved = substitute_line(cond, vars);
        let resolved = resolved.trim();

        // Parse: LHS OP RHS
        let ops = ["NE", "EQ", ">=", "<=", "!=", "=", ">", "<"];
        for op in &ops {
            if let Some(pos) = resolved.find(op) {
                let lhs = resolved[..pos].trim().trim_matches('\'');
                let rhs = resolved[pos + op.len()..].trim().trim_matches('\'');
                return match *op {
                    "=" | "EQ" => lhs == rhs,
                    "!=" | "NE" => lhs != rhs,
                    ">" => lhs > rhs,
                    "<" => lhs < rhs,
                    ">=" => lhs >= rhs,
                    "<=" => lhs <= rhs,
                    _ => false,
                };
            }
        }

        // If no operator, treat as truthy (non-empty, non-zero).
        !resolved.is_empty() && resolved != "0"
    }

    fn process_dot(
        &mut self,
        table_name: &str,
        body: &[String],
        vars: &IspfVarPools,
        tables: &mut TableManager,
    ) {
        let upper = table_name.to_uppercase();

        // Position to top of table and iterate.
        tables.tbtop(&upper);

        let mut count = 0usize;
        loop {
            let (rc, row) = tables.tbskip(&upper, 1);
            if rc != 0 {
                break;
            }
            count += 1;

            if let Some(row_data) = row {
                // Create a temporary overlay: substitute row columns + ZCNT.
                let overlay = DotOverlay {
                    row: row_data,
                    count,
                };
                for line in body {
                    let substituted = substitute_line_with_overlay(line, vars, &overlay);
                    self.output.push(substituted);
                }
            }
        }
    }
}

/// Overlay for )DOT iteration — provides row columns and ZCNT.
struct DotOverlay {
    row: HashMap<String, String>,
    count: usize,
}

// ---------------------------------------------------------------------------
//  Variable substitution
// ---------------------------------------------------------------------------

/// Substitute `&var` references in a line with variable values.
fn substitute_line(line: &str, vars: &IspfVarPools) -> String {
    let mut result = String::with_capacity(line.len());
    let chars: Vec<char> = line.chars().collect();
    let mut i = 0;

    while i < chars.len() {
        if chars[i] == '&' {
            let start = i + 1;
            let mut end = start;
            while end < chars.len() && (chars[end].is_ascii_alphanumeric() || chars[end] == '_') {
                end += 1;
            }
            if end > start {
                let var_name: String = chars[start..end].iter().collect();
                let val = vars.get(&var_name).unwrap_or_default();
                result.push_str(&val);
                // Skip trailing period (ISPF concatenation marker).
                if end < chars.len() && chars[end] == '.' {
                    end += 1;
                }
                i = end;
            } else {
                result.push('&');
                i += 1;
            }
        } else {
            result.push(chars[i]);
            i += 1;
        }
    }

    result
}

/// Substitute with a DOT overlay (row variables + ZCNT take priority).
fn substitute_line_with_overlay(line: &str, vars: &IspfVarPools, overlay: &DotOverlay) -> String {
    let mut result = String::with_capacity(line.len());
    let chars: Vec<char> = line.chars().collect();
    let mut i = 0;

    while i < chars.len() {
        if chars[i] == '&' {
            let start = i + 1;
            let mut end = start;
            while end < chars.len() && (chars[end].is_ascii_alphanumeric() || chars[end] == '_') {
                end += 1;
            }
            if end > start {
                let var_name: String = chars[start..end].iter().collect();
                let upper = var_name.to_uppercase();
                // Check overlay first: row columns, then ZCNT.
                let val = if upper == "ZCNT" {
                    overlay.count.to_string()
                } else if let Some(v) = overlay.row.get(&upper) {
                    v.clone()
                } else {
                    vars.get(&var_name).unwrap_or_default()
                };
                result.push_str(&val);
                if end < chars.len() && chars[end] == '.' {
                    end += 1;
                }
                i = end;
            } else {
                result.push('&');
                i += 1;
            }
        } else {
            result.push(chars[i]);
            i += 1;
        }
    }

    result
}

/// Collect lines until a matching end marker (e.g., )ENDSEL, )ENDDOT).
/// Returns (collected body lines, index of end marker line).
fn collect_block(lines: &[String], start: usize, end_marker: &str) -> (Vec<String>, usize) {
    let mut body = Vec::new();
    let mut depth = 0;
    let marker_upper = end_marker.to_uppercase();
    // Determine the matching start marker for nesting.
    let start_marker = if marker_upper == ")ENDSEL" { ")SEL " } else { ")DOT " };

    for (i, line) in lines.iter().enumerate().skip(start) {
        let upper = line.trim().to_uppercase();
        if upper.starts_with(start_marker) {
            depth += 1;
            body.push(line.clone());
        } else if upper.starts_with(&marker_upper) {
            if depth == 0 {
                return (body, i);
            }
            depth -= 1;
            body.push(line.clone());
        } else {
            body.push(line.clone());
        }
    }

    // If no matching end marker, return everything up to the end.
    (body, lines.len().saturating_sub(1))
}

// ---------------------------------------------------------------------------
//  Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    fn make_vars(pairs: &[(&str, &str)]) -> IspfVarPools {
        let mut vars = IspfVarPools::default();
        for (k, v) in pairs {
            vars.set(k, v.to_string());
        }
        vars
    }

    #[test]
    fn test_ftopen_ftclose() {
        let mut ft = FileTailor::new();
        assert_eq!(ft.ftopen(), 0);
        let (rc, output) = ft.ftclose();
        assert_eq!(rc, 0);
        assert!(output.is_empty());
    }

    #[test]
    fn test_ftopen_double() {
        let mut ft = FileTailor::new();
        ft.ftopen();
        assert_eq!(ft.ftopen(), 8); // Already open.
    }

    #[test]
    fn test_ftclose_without_open() {
        let mut ft = FileTailor::new();
        let (rc, _) = ft.ftclose();
        assert_eq!(rc, 16);
    }

    #[test]
    fn test_ftincl_not_found() {
        let mut ft = FileTailor::new();
        ft.ftopen();
        let mut tables = TableManager::new();
        let vars = IspfVarPools::default();
        assert_eq!(ft.ftincl("NOSKEL", &vars, &mut tables), 8);
    }

    #[test]
    fn test_basic_variable_substitution() {
        let mut ft = FileTailor::new();
        ft.load_skeleton("JCLSKEL", vec![
            "//&JOBNAME JOB &ACCT,'&PGMR'".to_string(),
            "//STEP01 EXEC PGM=IEFBR14".to_string(),
        ]);

        let vars = make_vars(&[("JOBNAME", "MYJOB"), ("ACCT", "12345"), ("PGMR", "SMITH")]);
        let mut tables = TableManager::new();

        ft.ftopen();
        let rc = ft.ftincl("JCLSKEL", &vars, &mut tables);
        assert_eq!(rc, 0);

        let (rc, output) = ft.ftclose();
        assert_eq!(rc, 0);
        assert_eq!(output.len(), 2);
        assert_eq!(output[0], "//MYJOB JOB 12345,'SMITH'");
        assert_eq!(output[1], "//STEP01 EXEC PGM=IEFBR14");
    }

    #[test]
    fn test_sel_true() {
        let mut ft = FileTailor::new();
        ft.load_skeleton("S", vec![
            ")SEL &OPT = 1".to_string(),
            "INCLUDED LINE".to_string(),
            ")ENDSEL".to_string(),
            "AFTER SEL".to_string(),
        ]);

        let vars = make_vars(&[("OPT", "1")]);
        let mut tables = TableManager::new();

        ft.ftopen();
        ft.ftincl("S", &vars, &mut tables);
        let (_, output) = ft.ftclose();
        assert_eq!(output, vec!["INCLUDED LINE", "AFTER SEL"]);
    }

    #[test]
    fn test_sel_false() {
        let mut ft = FileTailor::new();
        ft.load_skeleton("S", vec![
            ")SEL &OPT = 1".to_string(),
            "SHOULD NOT APPEAR".to_string(),
            ")ENDSEL".to_string(),
            "AFTER SEL".to_string(),
        ]);

        let vars = make_vars(&[("OPT", "2")]);
        let mut tables = TableManager::new();

        ft.ftopen();
        ft.ftincl("S", &vars, &mut tables);
        let (_, output) = ft.ftclose();
        assert_eq!(output, vec!["AFTER SEL"]);
    }

    #[test]
    fn test_sel_ne() {
        let mut ft = FileTailor::new();
        ft.load_skeleton("S", vec![
            ")SEL &OPT NE 1".to_string(),
            "INCLUDED".to_string(),
            ")ENDSEL".to_string(),
        ]);

        let vars = make_vars(&[("OPT", "2")]);
        let mut tables = TableManager::new();

        ft.ftopen();
        ft.ftincl("S", &vars, &mut tables);
        let (_, output) = ft.ftclose();
        assert_eq!(output, vec!["INCLUDED"]);
    }

    #[test]
    fn test_dot_loop() {
        let mut ft = FileTailor::new();
        ft.load_skeleton("S", vec![
            "HEADER".to_string(),
            ")DOT MYTAB".to_string(),
            "//DD&ZCNT DD DSN=&DSN,DISP=SHR".to_string(),
            ")ENDDOT".to_string(),
            "FOOTER".to_string(),
        ]);

        let vars = IspfVarPools::default();
        let mut tables = TableManager::new();
        tables.tbcreate("MYTAB", &[], &["DSN".into()], true);

        let make_row = |dsn: &str| -> HashMap<String, String> {
            [("DSN".to_string(), dsn.to_string())].into_iter().collect()
        };
        tables.tbadd("MYTAB", &make_row("SYS1.MACLIB"));
        tables.tbadd("MYTAB", &make_row("SYS1.LINKLIB"));
        tables.tbadd("MYTAB", &make_row("USER.LOAD"));

        ft.ftopen();
        ft.ftincl("S", &vars, &mut tables);
        let (_, output) = ft.ftclose();

        assert_eq!(output.len(), 5);
        assert_eq!(output[0], "HEADER");
        assert_eq!(output[1], "//DD1 DD DSN=SYS1.MACLIB,DISP=SHR");
        assert_eq!(output[2], "//DD2 DD DSN=SYS1.LINKLIB,DISP=SHR");
        assert_eq!(output[3], "//DD3 DD DSN=USER.LOAD,DISP=SHR");
        assert_eq!(output[4], "FOOTER");
    }

    #[test]
    fn test_dot_empty_table() {
        let mut ft = FileTailor::new();
        ft.load_skeleton("S", vec![
            ")DOT EMPTY".to_string(),
            "SHOULD NOT APPEAR".to_string(),
            ")ENDDOT".to_string(),
        ]);

        let vars = IspfVarPools::default();
        let mut tables = TableManager::new();
        tables.tbcreate("EMPTY", &[], &["V".into()], true);

        ft.ftopen();
        ft.ftincl("S", &vars, &mut tables);
        let (_, output) = ft.ftclose();
        assert!(output.is_empty());
    }

    #[test]
    fn test_comment_and_noblank() {
        let mut ft = FileTailor::new();
        ft.load_skeleton("S", vec![
            ")CM This is a comment".to_string(),
            "LINE1".to_string(),
            ")NB".to_string(),
            "LINE2".to_string(),
        ]);

        let vars = IspfVarPools::default();
        let mut tables = TableManager::new();

        ft.ftopen();
        ft.ftincl("S", &vars, &mut tables);
        let (_, output) = ft.ftclose();
        assert_eq!(output, vec!["LINE1", "LINE2"]);
    }

    #[test]
    fn test_imbed_skeleton() {
        let mut ft = FileTailor::new();
        ft.load_skeleton("MAIN", vec![
            "BEFORE".to_string(),
            ")IM SUB".to_string(),
            "AFTER".to_string(),
        ]);
        ft.load_skeleton("SUB", vec![
            "INNER LINE".to_string(),
        ]);

        let vars = IspfVarPools::default();
        let mut tables = TableManager::new();

        ft.ftopen();
        ft.ftincl("MAIN", &vars, &mut tables);
        let (_, output) = ft.ftclose();
        assert_eq!(output, vec!["BEFORE", "INNER LINE", "AFTER"]);
    }

    #[test]
    fn test_nested_sel() {
        let mut ft = FileTailor::new();
        ft.load_skeleton("S", vec![
            ")SEL &A = Y".to_string(),
            "OUTER".to_string(),
            ")SEL &B = Y".to_string(),
            "INNER".to_string(),
            ")ENDSEL".to_string(),
            ")ENDSEL".to_string(),
        ]);

        let vars = make_vars(&[("A", "Y"), ("B", "Y")]);
        let mut tables = TableManager::new();

        ft.ftopen();
        ft.ftincl("S", &vars, &mut tables);
        let (_, output) = ft.ftclose();
        assert_eq!(output, vec!["OUTER", "INNER"]);
    }

    #[test]
    fn test_variable_with_period_concat() {
        let mut ft = FileTailor::new();
        ft.load_skeleton("S", vec![
            "&PREFIX..DATA.SET".to_string(),
        ]);

        let vars = make_vars(&[("PREFIX", "USER01")]);
        let mut tables = TableManager::new();

        ft.ftopen();
        ft.ftincl("S", &vars, &mut tables);
        let (_, output) = ft.ftclose();
        assert_eq!(output[0], "USER01.DATA.SET");
    }

    #[test]
    fn test_multiple_ftincl() {
        let mut ft = FileTailor::new();
        ft.load_skeleton("A", vec!["LINE A".to_string()]);
        ft.load_skeleton("B", vec!["LINE B".to_string()]);

        let vars = IspfVarPools::default();
        let mut tables = TableManager::new();

        ft.ftopen();
        ft.ftincl("A", &vars, &mut tables);
        ft.ftincl("B", &vars, &mut tables);
        let (_, output) = ft.ftclose();
        assert_eq!(output, vec!["LINE A", "LINE B"]);
    }
}
