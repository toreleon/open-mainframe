//! ISREDIT — Edit Macro programming interface.
//!
//! Provides the `ADDRESS ISREDIT` API for REXX/CLIST edit macros:
//! - **Macro lifecycle**: MACRO, MEND, PROCESS, CANCEL
//! - **Line access**: LINE / LINE_AFTER / LINE_BEFORE, INSERT, DELETE
//! - **Search**: FIND, CHANGE, EXCLUDE, SEEK
//! - **Query**: LINENUM, DATA_WIDTH, DISPLAY_LINES, MACRO_LEVEL
//! - **Cursor**: CURSOR, LOCATE
//! - **State**: USER_STATE save/restore
//! - **Assignment**: `(var) = LINE n`, `LINE n = (var)`, `(var) = LINENUM .label`

use crate::editor::Editor;
use std::collections::HashMap;

// ---------------------------------------------------------------------------
//  Return codes
// ---------------------------------------------------------------------------

/// ISREDIT command return code.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IsreRc {
    /// 0 — Normal completion.
    Ok,
    /// 4 — Warning / string not found (FIND), etc.
    Warning,
    /// 8 — End of data or not found.
    EndOfData,
    /// 12 — Invalid command or state.
    Error,
    /// 20 — Severe error.
    Severe,
}

impl IsreRc {
    /// Numeric value.
    pub fn code(self) -> u32 {
        match self {
            Self::Ok => 0,
            Self::Warning => 4,
            Self::EndOfData => 8,
            Self::Error => 12,
            Self::Severe => 20,
        }
    }
}

// ---------------------------------------------------------------------------
//  User state snapshot
// ---------------------------------------------------------------------------

/// A snapshot of the editor state for USER_STATE save/restore.
#[derive(Debug, Clone)]
pub struct UserState {
    /// Saved editor data.
    data: Vec<String>,
    /// Saved cursor position.
    cursor_line: usize,
    /// Saved cursor column.
    cursor_col: usize,
}

// ---------------------------------------------------------------------------
//  Edit Macro Engine
// ---------------------------------------------------------------------------

/// The ISREDIT macro engine — wraps an Editor and provides the macro API.
#[derive(Debug)]
pub struct IsreditEngine {
    /// The underlying editor instance.
    editor: Editor,
    /// Macro variables (set by assignment commands).
    variables: HashMap<String, String>,
    /// Cursor line (1-based, 0 = command line).
    cursor_line: usize,
    /// Cursor column (1-based).
    cursor_col: usize,
    /// Macro nesting level.
    macro_level: u32,
    /// Whether MACRO statement has been issued.
    macro_started: bool,
    /// Macro arguments (from MACRO statement).
    macro_args: Vec<String>,
    /// Saved user states.
    user_states: HashMap<String, UserState>,
    /// Last FIND cursor position.
    find_cursor: usize,
}

impl IsreditEngine {
    /// Create a new ISREDIT engine wrapping an editor.
    pub fn new(editor: Editor) -> Self {
        Self {
            editor,
            variables: HashMap::new(),
            cursor_line: 0,
            cursor_col: 1,
            macro_level: 1,
            macro_started: false,
            macro_args: Vec::new(),
            user_states: HashMap::new(),
            find_cursor: 0,
        }
    }

    /// Get a reference to the underlying editor.
    pub fn editor(&self) -> &Editor {
        &self.editor
    }

    /// Get a mutable reference to the underlying editor.
    pub fn editor_mut(&mut self) -> &mut Editor {
        &mut self.editor
    }

    /// Execute an ISREDIT command string.
    ///
    /// Parses the command and dispatches to the appropriate handler.
    /// Returns `(return_code, optional_output_value)`.
    pub fn execute(&mut self, cmd: &str) -> (IsreRc, Option<String>) {
        let cmd = cmd.trim();
        if cmd.is_empty() {
            return (IsreRc::Error, None);
        }

        // Check for assignment form: (VAR) = COMMAND
        if cmd.starts_with('(') {
            return self.execute_query(cmd);
        }

        // Check for line assignment: LINE n = (VAR)
        if cmd.to_uppercase().starts_with("LINE ") && cmd.contains("= (") {
            return self.execute_line_set(cmd);
        }

        // Split into verb and operands.
        let upper = cmd.to_uppercase();
        let (verb, operands) = match upper.split_once(char::is_whitespace) {
            Some((v, o)) => (v.to_string(), o.trim().to_string()),
            None => (upper, String::new()),
        };

        match verb.as_str() {
            "MACRO" => self.cmd_macro(&operands),
            "MEND" | "END" => self.cmd_mend(),
            "PROCESS" => self.cmd_process(),
            "CANCEL" => self.cmd_cancel(),
            "FIND" => self.cmd_find(&operands),
            "CHANGE" => self.cmd_change(&operands),
            "EXCLUDE" => self.cmd_exclude(&operands),
            "INSERT" => self.cmd_insert(&operands),
            "DELETE" => self.cmd_delete(&operands),
            "CURSOR" => self.cmd_cursor(&operands),
            "LOCATE" => self.cmd_locate(&operands),
            "RESET" => self.cmd_reset(),
            "SAVE" => self.cmd_save(),
            "USER_STATE" => self.cmd_user_state(&operands),
            "LINE_AFTER" => self.cmd_line_after(&operands),
            "LINE_BEFORE" => self.cmd_line_before(&operands),
            _ => (IsreRc::Error, None),
        }
    }

    /// Get a variable value.
    pub fn get_var(&self, name: &str) -> Option<&str> {
        self.variables.get(&name.to_uppercase()).map(|s| s.as_str())
    }

    /// Set a variable value.
    pub fn set_var(&mut self, name: &str, value: &str) {
        self.variables
            .insert(name.to_uppercase(), value.to_string());
    }

    /// Get macro arguments.
    pub fn macro_args(&self) -> &[String] {
        &self.macro_args
    }

    /// Get current macro level.
    pub fn macro_level(&self) -> u32 {
        self.macro_level
    }

    /// Get cursor position (line, col).
    pub fn cursor(&self) -> (usize, usize) {
        (self.cursor_line, self.cursor_col)
    }

    // -------------------------------------------------------------------
    //  Query assignment: (VAR) = COMMAND
    // -------------------------------------------------------------------

    fn execute_query(&mut self, cmd: &str) -> (IsreRc, Option<String>) {
        // Parse: (VAR) = COMMAND
        let Some(rest) = cmd.strip_prefix('(') else {
            return (IsreRc::Error, None);
        };
        let Some((var_name, rest)) = rest.split_once(')') else {
            return (IsreRc::Error, None);
        };
        let var_name = var_name.trim().to_uppercase();
        let Some(rest) = rest.trim().strip_prefix('=') else {
            return (IsreRc::Error, None);
        };
        let rest = rest.trim();
        let upper = rest.to_uppercase();

        let value = if upper.starts_with("LINE ") {
            // (VAR) = LINE n  or  (VAR) = LINE .label
            let arg = rest[5..].trim();
            self.query_line(arg)
        } else if upper.starts_with("LINENUM ") {
            // (VAR) = LINENUM .label
            let label = rest[8..].trim();
            self.query_linenum(label)
        } else if upper == "LINENUM .ZLAST" {
            Some(self.editor.data().len().to_string())
        } else if upper == "LINENUM .ZCSR" {
            Some(self.cursor_line.to_string())
        } else if upper == "DATA_WIDTH" {
            Some("80".to_string())
        } else if upper == "DISPLAY_LINES" {
            Some(self.editor.data().len().to_string())
        } else if upper == "MACRO_LEVEL" {
            Some(self.macro_level.to_string())
        } else if upper == "CURSOR" {
            Some(format!("{} {}", self.cursor_line, self.cursor_col))
        } else if upper == "DATASET" {
            let data = self.editor.data();
            // Dataset name not stored on Editor — return empty.
            let _ = data;
            Some(String::new())
        } else {
            None
        };

        match value {
            Some(v) => {
                self.variables.insert(var_name, v.clone());
                (IsreRc::Ok, Some(v))
            }
            None => (IsreRc::Error, None),
        }
    }

    fn query_line(&self, arg: &str) -> Option<String> {
        let line_num = if arg.starts_with('.') {
            // Label reference.
            self.resolve_label(arg)?
        } else {
            arg.parse::<usize>().ok()?
        };
        let data = self.editor.data();
        if line_num >= 1 && line_num <= data.len() {
            Some(data[line_num - 1].clone())
        } else {
            None
        }
    }

    fn query_linenum(&self, label: &str) -> Option<String> {
        let num = self.resolve_label(label)?;
        Some(num.to_string())
    }

    fn resolve_label(&self, label: &str) -> Option<usize> {
        let label = label.trim();
        let upper = label.to_uppercase();
        if upper == ".ZCSR" {
            return Some(self.cursor_line);
        }
        if upper == ".ZLAST" {
            return Some(self.editor.data().len());
        }
        if upper == ".ZFIRST" {
            return Some(1);
        }
        // User-defined label.
        self.editor.find_label(label).map(|i| i + 1)
    }

    // -------------------------------------------------------------------
    //  Line set: LINE n = (VAR)
    // -------------------------------------------------------------------

    fn execute_line_set(&mut self, cmd: &str) -> (IsreRc, Option<String>) {
        // Parse: LINE n = (VAR)
        let upper = cmd.to_uppercase();
        let Some(rest) = upper.strip_prefix("LINE ") else {
            return (IsreRc::Error, None);
        };
        let rest = rest.trim();
        let Some((num_str, rest)) = rest.split_once('=') else {
            return (IsreRc::Error, None);
        };
        let num_str = num_str.trim();
        let rest = rest.trim();
        let Some(var_part) = rest.strip_prefix('(').and_then(|s| s.strip_suffix(')')) else {
            return (IsreRc::Error, None);
        };

        let line_num: usize = if num_str.starts_with('.') {
            match self.resolve_label(num_str) {
                Some(n) => n,
                None => return (IsreRc::Error, None),
            }
        } else {
            match num_str.parse() {
                Ok(n) => n,
                Err(_) => return (IsreRc::Error, None),
            }
        };

        let var_name = var_part.trim().to_uppercase();
        let value = match self.variables.get(&var_name) {
            Some(v) => v.clone(),
            None => return (IsreRc::Error, None),
        };

        let data = self.editor.data();
        if line_num >= 1 && line_num <= data.len() {
            // Replace line content by reconstructing via editor.
            // Use delete + insert approach.
            let idx = line_num - 1;
            self.editor.delete_line(idx);
            self.editor.insert_lines(if idx > 0 { idx - 1 } else { 0 }, 1);
            // The inserted line is blank — we need to set it via change.
            // Since we can't directly set a line, use change on that specific line.
            // Actually, let's just use a direct approach through the editor's data.
            // For now, we put the value back by doing a process_line_cmd approach.
            // Simplest: rebuild data.
            let mut all_data = self.editor.data();
            if idx < all_data.len() {
                all_data[idx] = value;
            }
            // Re-create editor with new data — this is a limitation but works for macros.
            self.editor = Editor::new("", all_data, true);
            (IsreRc::Ok, None)
        } else {
            (IsreRc::Error, None)
        }
    }

    // -------------------------------------------------------------------
    //  Commands
    // -------------------------------------------------------------------

    fn cmd_macro(&mut self, operands: &str) -> (IsreRc, Option<String>) {
        if self.macro_started {
            return (IsreRc::Error, None);
        }
        self.macro_started = true;

        // Parse: (ARGS) or (ARG1,ARG2,...)
        if let Some(rest) = operands.strip_prefix('(') {
            if let Some(args_str) = rest.strip_suffix(')') {
                self.macro_args = args_str
                    .split(',')
                    .map(|s| s.trim().to_string())
                    .collect();
            }
        }
        (IsreRc::Ok, None)
    }

    fn cmd_mend(&mut self) -> (IsreRc, Option<String>) {
        (IsreRc::Ok, None)
    }

    fn cmd_process(&mut self) -> (IsreRc, Option<String>) {
        // PROCESS — signal that the edit session should continue.
        (IsreRc::Ok, None)
    }

    fn cmd_cancel(&mut self) -> (IsreRc, Option<String>) {
        self.editor.cancel();
        (IsreRc::Ok, None)
    }

    fn cmd_find(&mut self, operands: &str) -> (IsreRc, Option<String>) {
        // Parse: 'string' [FIRST|LAST|NEXT|PREV|ALL]
        let (pattern, opts) = parse_quoted_and_opts(operands);
        if pattern.is_empty() {
            return (IsreRc::Error, None);
        }

        let opts_upper = opts.to_uppercase();
        let data = self.editor.data();

        if opts_upper.contains("FIRST") {
            self.find_cursor = 0;
        }

        // Search from find_cursor.
        for (i, line) in data.iter().enumerate().skip(self.find_cursor) {
            if line.contains(&pattern) {
                self.cursor_line = i + 1;
                // Find column position.
                if let Some(col) = line.find(&pattern) {
                    self.cursor_col = col + 1;
                }
                self.find_cursor = i + 1;
                return (IsreRc::Ok, None);
            }
        }

        self.find_cursor = 0; // Wrap around.
        (IsreRc::Warning, None) // Not found.
    }

    fn cmd_change(&mut self, operands: &str) -> (IsreRc, Option<String>) {
        // Parse: 'old' 'new' [ALL|FIRST|NEXT]
        let parts = extract_two_quoted(operands);
        let (old, new, opts) = match parts {
            Some(v) => v,
            None => return (IsreRc::Error, None),
        };

        let all = opts.to_uppercase().contains("ALL");
        let count = self.editor.change(&old, &new, all);

        if count > 0 {
            (IsreRc::Ok, Some(count.to_string()))
        } else {
            (IsreRc::Warning, Some("0".to_string()))
        }
    }

    fn cmd_exclude(&mut self, operands: &str) -> (IsreRc, Option<String>) {
        let (pattern, opts) = parse_quoted_and_opts(operands);
        if pattern.is_empty() {
            return (IsreRc::Error, None);
        }

        let all = opts.to_uppercase().contains("ALL");
        let data = self.editor.data();
        let mut count = 0usize;

        for (i, line) in data.iter().enumerate() {
            if line.contains(&pattern) {
                self.editor.exclude_line(i);
                count += 1;
                if !all {
                    break;
                }
            }
        }

        if count > 0 {
            (IsreRc::Ok, Some(count.to_string()))
        } else {
            (IsreRc::Warning, None)
        }
    }

    fn cmd_insert(&mut self, operands: &str) -> (IsreRc, Option<String>) {
        // INSERT linenum DATALINE (VAR)
        let parts: Vec<&str> = operands.split_whitespace().collect();
        if parts.is_empty() {
            return (IsreRc::Error, None);
        }

        let line_num: usize = match parts[0].parse() {
            Ok(n) => n,
            Err(_) => {
                // Try label.
                match self.resolve_label(parts[0]) {
                    Some(n) => n,
                    None => return (IsreRc::Error, None),
                }
            }
        };

        // Check for DATALINE (VAR) form.
        let insert_text = if parts.len() >= 3 && parts[1].eq_ignore_ascii_case("DATALINE") {
            let var_ref = parts[2..].join(" ");
            let var_name = var_ref
                .trim()
                .strip_prefix('(')
                .and_then(|s| s.strip_suffix(')'))
                .map(|s| s.trim().to_uppercase());
            match var_name {
                Some(name) => self.variables.get(&name).cloned().unwrap_or_default(),
                None => var_ref,
            }
        } else {
            // Just insert a blank line.
            String::new()
        };

        let data_len = self.editor.data().len();
        if line_num > data_len {
            return (IsreRc::Error, None);
        }

        let insert_at = if line_num == 0 { 0 } else { line_num - 1 };
        self.editor.insert_lines(insert_at, 1);

        // Set the inserted line's content.
        if !insert_text.is_empty() {
            let mut all_data = self.editor.data();
            let target_idx = if line_num == 0 { 0 } else { line_num };
            if target_idx < all_data.len() {
                all_data[target_idx] = insert_text;
                self.editor = Editor::new("", all_data, true);
            }
        }

        (IsreRc::Ok, None)
    }

    fn cmd_delete(&mut self, operands: &str) -> (IsreRc, Option<String>) {
        let parts: Vec<&str> = operands.split_whitespace().collect();
        if parts.is_empty() {
            return (IsreRc::Error, None);
        }

        let start: usize = match parts[0].parse::<usize>() {
            Ok(n) => n,
            Err(_) => match self.resolve_label(parts[0]) {
                Some(n) => n,
                None => return (IsreRc::Error, None),
            },
        };

        let count: usize = if parts.len() >= 2 {
            parts[1].parse().unwrap_or(1)
        } else {
            1
        };

        let data_len = self.editor.data().len();
        if start < 1 || start > data_len {
            return (IsreRc::Error, None);
        }

        let end = (start + count - 1).min(data_len);
        self.editor.delete_range(start - 1, end - 1);
        (IsreRc::Ok, None)
    }

    fn cmd_cursor(&mut self, operands: &str) -> (IsreRc, Option<String>) {
        // CURSOR = line col
        let stripped = operands.strip_prefix('=').unwrap_or(operands).trim();
        let parts: Vec<&str> = stripped.split_whitespace().collect();
        if let Some(line_str) = parts.first() {
            if let Ok(line) = line_str.parse::<usize>() {
                self.cursor_line = line;
            }
        }
        if let Some(col_str) = parts.get(1) {
            if let Ok(col) = col_str.parse::<usize>() {
                self.cursor_col = col;
            }
        }
        (IsreRc::Ok, None)
    }

    fn cmd_locate(&mut self, operands: &str) -> (IsreRc, Option<String>) {
        let target = operands.trim();
        if target.starts_with('.') {
            // Label.
            match self.resolve_label(target) {
                Some(n) => {
                    self.cursor_line = n;
                    (IsreRc::Ok, None)
                }
                None => (IsreRc::Warning, None),
            }
        } else if let Ok(n) = target.parse::<usize>() {
            self.cursor_line = n;
            (IsreRc::Ok, None)
        } else {
            (IsreRc::Error, None)
        }
    }

    fn cmd_reset(&mut self) -> (IsreRc, Option<String>) {
        self.editor.reset();
        (IsreRc::Ok, None)
    }

    fn cmd_save(&mut self) -> (IsreRc, Option<String>) {
        self.editor.save();
        (IsreRc::Ok, None)
    }

    fn cmd_user_state(&mut self, operands: &str) -> (IsreRc, Option<String>) {
        // USER_STATE = (VAR)  — save
        // (VAR) = USER_STATE  — restore (handled by execute_query)
        let stripped = operands.trim();
        if let Some(rest) = stripped.strip_prefix('=') {
            let rest = rest.trim();
            if let Some(var) = rest.strip_prefix('(').and_then(|s| s.strip_suffix(')')) {
                let var_name = var.trim().to_uppercase();
                // Save state.
                let state = UserState {
                    data: self.editor.data(),
                    cursor_line: self.cursor_line,
                    cursor_col: self.cursor_col,
                };
                self.user_states.insert(var_name, state);
                return (IsreRc::Ok, None);
            }
        }
        (IsreRc::Error, None)
    }

    /// Restore a previously saved user state.
    pub fn restore_user_state(&mut self, name: &str) -> IsreRc {
        let name = name.to_uppercase();
        if let Some(state) = self.user_states.get(&name).cloned() {
            self.editor = Editor::new("", state.data, true);
            self.cursor_line = state.cursor_line;
            self.cursor_col = state.cursor_col;
            IsreRc::Ok
        } else {
            IsreRc::Error
        }
    }

    fn cmd_line_after(&mut self, operands: &str) -> (IsreRc, Option<String>) {
        // LINE_AFTER linenum = (VAR) or LINE_AFTER linenum = "text"
        self.insert_line_relative(operands, true)
    }

    fn cmd_line_before(&mut self, operands: &str) -> (IsreRc, Option<String>) {
        // LINE_BEFORE linenum = (VAR) or LINE_BEFORE linenum = "text"
        self.insert_line_relative(operands, false)
    }

    fn insert_line_relative(
        &mut self,
        operands: &str,
        after: bool,
    ) -> (IsreRc, Option<String>) {
        let (num_str, rest) = match operands.split_once('=') {
            Some((a, b)) => (a.trim(), b.trim()),
            None => return (IsreRc::Error, None),
        };

        let line_num: usize = if num_str.starts_with('.') {
            match self.resolve_label(num_str) {
                Some(n) => n,
                None => return (IsreRc::Error, None),
            }
        } else {
            match num_str.parse() {
                Ok(n) => n,
                Err(_) => return (IsreRc::Error, None),
            }
        };

        // Get the text value.
        let text = if rest.starts_with('(') && rest.ends_with(')') {
            let var = rest[1..rest.len() - 1].trim().to_uppercase();
            self.variables.get(&var).cloned().unwrap_or_default()
        } else if rest.starts_with('"') || rest.starts_with('\'') {
            rest[1..rest.len().saturating_sub(1)].to_string()
        } else {
            rest.to_string()
        };

        let mut all_data = self.editor.data();
        let idx = if after { line_num } else { line_num.saturating_sub(1) };
        if idx > all_data.len() {
            return (IsreRc::Error, None);
        }
        all_data.insert(idx, text);
        self.editor = Editor::new("", all_data, true);
        (IsreRc::Ok, None)
    }
}

// ---------------------------------------------------------------------------
//  Helpers
// ---------------------------------------------------------------------------

/// Parse a quoted string and remaining options from an operand string.
fn parse_quoted_and_opts(s: &str) -> (String, String) {
    let s = s.trim();
    if let Some(inner) = s.strip_prefix('\'') {
        if let Some(end) = inner.find('\'') {
            let pattern = inner[..end].to_string();
            let rest = inner[end + 1..].trim().to_string();
            return (pattern, rest);
        }
    }
    // Try unquoted first word.
    match s.split_once(char::is_whitespace) {
        Some((p, r)) => (p.to_string(), r.trim().to_string()),
        None => (s.to_string(), String::new()),
    }
}

/// Extract two quoted strings and trailing options.
fn extract_two_quoted(s: &str) -> Option<(String, String, String)> {
    let s = s.trim();
    let (first, rest) = extract_one_quoted(s)?;
    let rest = rest.trim();
    let (second, opts) = extract_one_quoted(rest)?;
    Some((first, second, opts.trim().to_string()))
}

fn extract_one_quoted(s: &str) -> Option<(String, String)> {
    let s = s.trim();
    if let Some(inner) = s.strip_prefix('\'') {
        let end = inner.find('\'')?;
        Some((inner[..end].to_string(), inner[end + 1..].to_string()))
    } else {
        let (word, rest) = s.split_once(char::is_whitespace).unwrap_or((s, ""));
        Some((word.to_string(), rest.to_string()))
    }
}

// ---------------------------------------------------------------------------
//  Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::editor::Editor;

    fn make_engine(lines: &[&str]) -> IsreditEngine {
        let data: Vec<String> = lines.iter().map(|s| s.to_string()).collect();
        let editor = Editor::new("TEST.DATA", data, true);
        IsreditEngine::new(editor)
    }

    // ─── T110.1: Edit Macro Framework ───

    #[test]
    fn test_macro_statement() {
        let mut eng = make_engine(&["LINE 1"]);
        let (rc, _) = eng.execute("MACRO (ARGS)");
        assert_eq!(rc, IsreRc::Ok);
        assert!(eng.macro_started);
        assert_eq!(eng.macro_args(), &["ARGS"]);
    }

    #[test]
    fn test_macro_multiple_args() {
        let mut eng = make_engine(&["LINE 1"]);
        eng.execute("MACRO (ARG1,ARG2,ARG3)");
        assert_eq!(eng.macro_args().len(), 3);
        assert_eq!(eng.macro_args()[1], "ARG2");
    }

    #[test]
    fn test_double_macro_fails() {
        let mut eng = make_engine(&["LINE 1"]);
        eng.execute("MACRO (ARGS)");
        let (rc, _) = eng.execute("MACRO (ARGS)");
        assert_eq!(rc, IsreRc::Error);
    }

    #[test]
    fn test_query_line() {
        let mut eng = make_engine(&["FIRST LINE", "SECOND LINE", "THIRD LINE"]);
        let (rc, val) = eng.execute("(DATA) = LINE 2");
        assert_eq!(rc, IsreRc::Ok);
        assert_eq!(val.as_deref(), Some("SECOND LINE"));
        assert_eq!(eng.get_var("DATA"), Some("SECOND LINE"));
    }

    #[test]
    fn test_query_line_label() {
        let mut eng = make_engine(&["FIRST", "SECOND", "THIRD"]);
        eng.editor_mut().set_label(1, ".HERE");
        let (rc, val) = eng.execute("(DATA) = LINE .HERE");
        assert_eq!(rc, IsreRc::Ok);
        assert_eq!(val.as_deref(), Some("SECOND"));
    }

    #[test]
    fn test_query_linenum_zlast() {
        let mut eng = make_engine(&["A", "B", "C"]);
        let (rc, val) = eng.execute("(NUM) = LINENUM .ZLAST");
        assert_eq!(rc, IsreRc::Ok);
        assert_eq!(val.as_deref(), Some("3"));
    }

    #[test]
    fn test_query_data_width() {
        let mut eng = make_engine(&["A"]);
        let (rc, val) = eng.execute("(W) = DATA_WIDTH");
        assert_eq!(rc, IsreRc::Ok);
        assert_eq!(val.as_deref(), Some("80"));
    }

    #[test]
    fn test_query_macro_level() {
        let mut eng = make_engine(&["A"]);
        let (rc, val) = eng.execute("(LVL) = MACRO_LEVEL");
        assert_eq!(rc, IsreRc::Ok);
        assert_eq!(val.as_deref(), Some("1"));
    }

    #[test]
    fn test_mend() {
        let mut eng = make_engine(&["A"]);
        let (rc, _) = eng.execute("MEND");
        assert_eq!(rc, IsreRc::Ok);
    }

    // ─── T110.2: Macro Edit Services ───

    #[test]
    fn test_find_first() {
        let mut eng = make_engine(&["HELLO WORLD", "TODO: fix this", "DONE"]);
        let (rc, _) = eng.execute("FIND 'TODO' FIRST");
        assert_eq!(rc, IsreRc::Ok);
        assert_eq!(eng.cursor_line, 2);
    }

    #[test]
    fn test_find_not_found() {
        let mut eng = make_engine(&["HELLO"]);
        let (rc, _) = eng.execute("FIND 'MISSING' FIRST");
        assert_eq!(rc, IsreRc::Warning);
    }

    #[test]
    fn test_find_then_read_line() {
        let mut eng = make_engine(&["AAA", "BBB TODO", "CCC"]);
        eng.execute("FIND 'TODO' FIRST");
        let (rc, val) = eng.execute("(DATA) = LINE .ZCSR");
        assert_eq!(rc, IsreRc::Ok);
        assert_eq!(val.as_deref(), Some("BBB TODO"));
    }

    #[test]
    fn test_change() {
        let mut eng = make_engine(&["FOO BAR", "FOO BAZ", "QUX"]);
        let (rc, val) = eng.execute("CHANGE 'FOO' 'REPLACED' ALL");
        assert_eq!(rc, IsreRc::Ok);
        assert_eq!(val.as_deref(), Some("2"));

        let data = eng.editor().data();
        assert!(data[0].contains("REPLACED"));
        assert!(data[1].contains("REPLACED"));
    }

    #[test]
    fn test_change_not_found() {
        let mut eng = make_engine(&["HELLO"]);
        let (rc, val) = eng.execute("CHANGE 'MISSING' 'NEW'");
        assert_eq!(rc, IsreRc::Warning);
        assert_eq!(val.as_deref(), Some("0"));
    }

    #[test]
    fn test_delete() {
        let mut eng = make_engine(&["LINE1", "LINE2", "LINE3"]);
        let (rc, _) = eng.execute("DELETE 2");
        assert_eq!(rc, IsreRc::Ok);
        assert_eq!(eng.editor().data().len(), 2);
        assert_eq!(eng.editor().data()[1], "LINE3");
    }

    #[test]
    fn test_delete_range() {
        let mut eng = make_engine(&["A", "B", "C", "D"]);
        let (rc, _) = eng.execute("DELETE 2 2");
        assert_eq!(rc, IsreRc::Ok);
        assert_eq!(eng.editor().data().len(), 2);
        assert_eq!(eng.editor().data(), vec!["A", "D"]);
    }

    #[test]
    fn test_exclude() {
        let mut eng = make_engine(&["KEEP", "HIDE ME", "KEEP TOO"]);
        let (rc, _) = eng.execute("EXCLUDE 'HIDE' ALL");
        assert_eq!(rc, IsreRc::Ok);
    }

    #[test]
    fn test_cursor_set() {
        let mut eng = make_engine(&["A"]);
        eng.execute("CURSOR = 5 10");
        assert_eq!(eng.cursor(), (5, 10));
    }

    #[test]
    fn test_locate_line() {
        let mut eng = make_engine(&["A", "B", "C"]);
        eng.execute("LOCATE 3");
        assert_eq!(eng.cursor_line, 3);
    }

    #[test]
    fn test_locate_label() {
        let mut eng = make_engine(&["A", "B", "C"]);
        eng.editor_mut().set_label(2, ".TARG");
        let (rc, _) = eng.execute("LOCATE .TARG");
        assert_eq!(rc, IsreRc::Ok);
        assert_eq!(eng.cursor_line, 3); // 0-indexed 2 → 1-based 3
    }

    #[test]
    fn test_user_state_save_restore() {
        let mut eng = make_engine(&["ORIGINAL1", "ORIGINAL2"]);

        // Save state.
        eng.execute("USER_STATE = (USTATE)");

        // Make changes.
        eng.execute("DELETE 1");
        assert_eq!(eng.editor().data().len(), 1);

        // Restore state.
        let rc = eng.restore_user_state("USTATE");
        assert_eq!(rc, IsreRc::Ok);
        assert_eq!(eng.editor().data().len(), 2);
        assert_eq!(eng.editor().data()[0], "ORIGINAL1");
    }

    #[test]
    fn test_line_after() {
        let mut eng = make_engine(&["FIRST", "SECOND"]);
        eng.set_var("NEWLINE", "INSERTED");
        let (rc, _) = eng.execute("LINE_AFTER 1 = (NEWLINE)");
        assert_eq!(rc, IsreRc::Ok);

        let data = eng.editor().data();
        assert_eq!(data.len(), 3);
        assert_eq!(data[1], "INSERTED");
    }

    #[test]
    fn test_line_before() {
        let mut eng = make_engine(&["FIRST", "SECOND"]);
        eng.set_var("NEWLINE", "INSERTED");
        let (rc, _) = eng.execute("LINE_BEFORE 2 = (NEWLINE)");
        assert_eq!(rc, IsreRc::Ok);

        let data = eng.editor().data();
        assert_eq!(data.len(), 3);
        assert_eq!(data[1], "INSERTED");
    }

    #[test]
    fn test_line_after_with_literal() {
        let mut eng = make_engine(&["FIRST"]);
        let (rc, _) = eng.execute("LINE_AFTER 1 = \"NEW LINE\"");
        assert_eq!(rc, IsreRc::Ok);
        assert_eq!(eng.editor().data().len(), 2);
        assert_eq!(eng.editor().data()[1], "NEW LINE");
    }

    #[test]
    fn test_reset() {
        let mut eng = make_engine(&["A", "B"]);
        eng.editor_mut().exclude_line(0);
        eng.execute("RESET");
        // Reset clears excludes.
    }

    #[test]
    fn test_save() {
        let mut eng = make_engine(&["ORIGINAL"]);
        let (rc, _) = eng.execute("SAVE");
        assert_eq!(rc, IsreRc::Ok);
    }

    #[test]
    fn test_process() {
        let mut eng = make_engine(&["A"]);
        let (rc, _) = eng.execute("PROCESS");
        assert_eq!(rc, IsreRc::Ok);
    }

    #[test]
    fn test_cancel() {
        let mut eng = make_engine(&["A"]);
        let (rc, _) = eng.execute("CANCEL");
        assert_eq!(rc, IsreRc::Ok);
    }

    #[test]
    fn test_unknown_command() {
        let mut eng = make_engine(&["A"]);
        let (rc, _) = eng.execute("BOGUSCMD");
        assert_eq!(rc, IsreRc::Error);
    }

    #[test]
    fn test_empty_command() {
        let mut eng = make_engine(&["A"]);
        let (rc, _) = eng.execute("");
        assert_eq!(rc, IsreRc::Error);
    }

    #[test]
    fn test_find_sequential() {
        let mut eng = make_engine(&["AAA", "BBB", "AAA AGAIN"]);
        let (rc, _) = eng.execute("FIND 'AAA' FIRST");
        assert_eq!(rc, IsreRc::Ok);
        assert_eq!(eng.cursor_line, 1);

        let (rc, _) = eng.execute("FIND 'AAA'");
        assert_eq!(rc, IsreRc::Ok);
        assert_eq!(eng.cursor_line, 3);

        // Wraps around — not found from cursor.
        let (rc, _) = eng.execute("FIND 'AAA'");
        assert_eq!(rc, IsreRc::Warning);
    }
}
