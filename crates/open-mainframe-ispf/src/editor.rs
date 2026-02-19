//! ISPF editor — line-based editor with line commands, primary commands, and edit profiles.
//!
//! Services:
//! - **Core**: EDIT/VIEW dataset, SAVE, CANCEL (PF3/PF12)
//! - **Line commands**: I, D, C, M, R, CC/MM/DD, >, <, X, S, LC, UC
//! - **Primary commands**: FIND, CHANGE, SORT, SUBMIT, COPY, RESET, HEX, COLS
//! - **Profiles**: CAPS, NULLS, TABS, BOUNDS, NUMBER, STATS
//! - **Undo/Redo**: change tracking with full undo stack

use std::collections::HashMap;

// ---------------------------------------------------------------------------
//  Editor data structures
// ---------------------------------------------------------------------------

/// A single editor line.
#[derive(Debug, Clone)]
pub struct EditorLine {
    /// The line data.
    pub data: String,
    /// Whether this line is excluded from display.
    pub excluded: bool,
    /// Line label (e.g., .A, .B).
    pub label: Option<String>,
    /// Whether this line has been modified.
    pub modified: bool,
}

impl EditorLine {
    fn new(data: String) -> Self {
        Self {
            data,
            excluded: false,
            label: None,
            modified: false,
        }
    }
}

/// Edit profile — per-type settings.
#[derive(Debug, Clone)]
pub struct EditProfile {
    /// Profile name (e.g., "COBOL", "JCL", "DATA").
    pub name: String,
    /// CAPS mode — uppercase input.
    pub caps: bool,
    /// NULLS mode — allow null characters in fields.
    pub nulls: bool,
    /// TABS mode — expand tabs.
    pub tabs: bool,
    /// Edit bounds (left column, right column), 1-based.
    pub bounds: (usize, usize),
    /// NUMBER mode — show line numbers.
    pub number: bool,
    /// STATS mode — track modification statistics.
    pub stats: bool,
    /// HEX display mode.
    pub hex: bool,
    /// AUTONUM — auto-renumber on save.
    pub autonum: bool,
    /// RECOVERY mode — track changes for undo.
    pub recovery: bool,
}

impl Default for EditProfile {
    fn default() -> Self {
        Self {
            name: "DEFAULT".to_string(),
            caps: false,
            nulls: true,
            tabs: false,
            bounds: (1, 80),
            number: false,
            stats: true,
            hex: false,
            autonum: false,
            recovery: true,
        }
    }
}

impl EditProfile {
    /// Create a COBOL profile.
    pub fn cobol() -> Self {
        Self {
            name: "COBOL".to_string(),
            caps: true,
            nulls: true,
            tabs: true,
            bounds: (7, 72),
            number: true,
            stats: true,
            hex: false,
            autonum: false,
            recovery: true,
        }
    }

    /// Create a JCL profile.
    pub fn jcl() -> Self {
        Self {
            name: "JCL".to_string(),
            caps: true,
            nulls: true,
            tabs: false,
            bounds: (1, 72),
            number: true,
            stats: true,
            hex: false,
            autonum: false,
            recovery: true,
        }
    }
}

/// An undo snapshot.
#[derive(Debug, Clone)]
struct UndoEntry {
    lines: Vec<EditorLine>,
    cursor_line: usize,
    description: String,
}

/// Find/change result.
#[derive(Debug, Clone)]
pub struct FindResult {
    /// Number of matches found.
    pub count: usize,
    /// Line indices where matches were found (0-based).
    pub line_indices: Vec<usize>,
}

/// Pending block command state.
#[derive(Debug, Clone)]
struct BlockPending {
    cmd: BlockCmd,
    start: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum BlockCmd {
    Copy,
    Move,
    Delete,
}

// ---------------------------------------------------------------------------
//  Editor
// ---------------------------------------------------------------------------

/// The ISPF line editor.
#[derive(Debug)]
pub struct Editor {
    /// Dataset name being edited.
    pub dataset: String,
    /// The editor lines.
    pub lines: Vec<EditorLine>,
    /// Current cursor line (0-based).
    pub cursor: usize,
    /// Active edit profile.
    pub profile: EditProfile,
    /// Whether data has been modified.
    pub modified: bool,
    /// Original data (for CANCEL).
    original: Vec<String>,
    /// Undo stack.
    undo_stack: Vec<UndoEntry>,
    /// Redo stack.
    redo_stack: Vec<UndoEntry>,
    /// Profile library.
    profiles: HashMap<String, EditProfile>,
    /// Pending block command (first CC/MM/DD).
    block_pending: Option<BlockPending>,
    /// Last FIND string.
    last_find: Option<String>,
    /// Edit mode: true = EDIT, false = VIEW.
    pub edit_mode: bool,
}

impl Editor {
    /// Create a new editor session.
    pub fn new(dataset: &str, data: Vec<String>, edit_mode: bool) -> Self {
        let lines: Vec<EditorLine> = data.iter().map(|s| EditorLine::new(s.clone())).collect();
        let original = data;

        let mut profiles = HashMap::new();
        profiles.insert("DEFAULT".to_string(), EditProfile::default());
        profiles.insert("COBOL".to_string(), EditProfile::cobol());
        profiles.insert("JCL".to_string(), EditProfile::jcl());

        // Auto-detect profile from dataset name.
        let profile = detect_profile(dataset, &profiles);

        Self {
            dataset: dataset.to_string(),
            lines,
            cursor: 0,
            profile,
            modified: false,
            original,
            undo_stack: Vec::new(),
            redo_stack: Vec::new(),
            profiles,
            block_pending: None,
            last_find: None,
            edit_mode,
        }
    }

    /// Get the current data as a vector of strings.
    pub fn data(&self) -> Vec<String> {
        self.lines.iter().map(|l| l.data.clone()).collect()
    }

    /// Get visible lines (non-excluded) with excluded-line summaries.
    pub fn visible_lines(&self) -> Vec<VisibleLine> {
        let mut result = Vec::new();
        let mut i = 0;
        while i < self.lines.len() {
            if self.lines[i].excluded {
                let start = i;
                while i < self.lines.len() && self.lines[i].excluded {
                    i += 1;
                }
                result.push(VisibleLine::ExcludedBlock {
                    start_index: start,
                    count: i - start,
                });
            } else {
                result.push(VisibleLine::Data {
                    index: i,
                    line: &self.lines[i],
                });
                i += 1;
            }
        }
        result
    }

    // -----------------------------------------------------------------------
    //  Save / Cancel
    // -----------------------------------------------------------------------

    /// SAVE — return the current data (caller persists it).
    pub fn save(&mut self) -> Vec<String> {
        self.modified = false;
        self.data()
    }

    /// CANCEL — discard changes and return original data.
    pub fn cancel(&mut self) -> Vec<String> {
        self.lines = self.original.iter().map(|s| EditorLine::new(s.clone())).collect();
        self.modified = false;
        self.original.clone()
    }

    /// END (PF3) — save if modified, then exit.
    pub fn end_edit(&mut self) -> (bool, Vec<String>) {
        if self.modified {
            (true, self.save())
        } else {
            (false, self.data())
        }
    }

    // -----------------------------------------------------------------------
    //  Line commands
    // -----------------------------------------------------------------------

    fn push_undo(&mut self, desc: &str) {
        if self.profile.recovery {
            self.undo_stack.push(UndoEntry {
                lines: self.lines.clone(),
                cursor_line: self.cursor,
                description: desc.to_string(),
            });
            self.redo_stack.clear();
        }
    }

    /// Insert `count` blank lines after line `after` (0-based).
    pub fn insert_lines(&mut self, after: usize, count: usize) {
        self.push_undo("insert");
        let pos = (after + 1).min(self.lines.len());
        for _ in 0..count {
            let mut line = EditorLine::new(String::new());
            line.modified = true;
            self.lines.insert(pos, line);
        }
        self.modified = true;
    }

    /// Delete line at index.
    pub fn delete_line(&mut self, index: usize) {
        if index < self.lines.len() {
            self.push_undo("delete");
            self.lines.remove(index);
            self.modified = true;
        }
    }

    /// Delete lines in range [start, end] inclusive.
    pub fn delete_range(&mut self, start: usize, end: usize) {
        if start <= end && end < self.lines.len() {
            self.push_undo("delete block");
            self.lines.drain(start..=end);
            self.modified = true;
        }
    }

    /// Repeat line at index `count` times.
    pub fn repeat_line(&mut self, index: usize, count: usize) {
        if index < self.lines.len() {
            self.push_undo("repeat");
            let line = self.lines[index].clone();
            for i in 0..count {
                let mut dup = line.clone();
                dup.modified = true;
                self.lines.insert(index + 1 + i, dup);
            }
            self.modified = true;
        }
    }

    /// Copy lines [start, end] after target line.
    pub fn copy_range(&mut self, start: usize, end: usize, after: usize) {
        if start <= end && end < self.lines.len() {
            self.push_undo("copy");
            let copied: Vec<EditorLine> = self.lines[start..=end].iter().cloned().map(|mut l| {
                l.modified = true;
                l
            }).collect();
            let insert_pos = (after + 1).min(self.lines.len());
            for (i, line) in copied.into_iter().enumerate() {
                self.lines.insert(insert_pos + i, line);
            }
            self.modified = true;
        }
    }

    /// Move lines [start, end] after target line.
    pub fn move_range(&mut self, start: usize, end: usize, after: usize) {
        if start <= end && end < self.lines.len() {
            self.push_undo("move");
            let moved: Vec<EditorLine> = self.lines.drain(start..=end).collect();
            let insert_pos = if after >= start {
                (after - (end - start)).min(self.lines.len())
            } else {
                (after + 1).min(self.lines.len())
            };
            for (i, line) in moved.into_iter().enumerate() {
                self.lines.insert(insert_pos + i, line);
            }
            self.modified = true;
        }
    }

    /// Shift line right by `count` spaces.
    pub fn shift_right(&mut self, index: usize, count: usize) {
        if index < self.lines.len() {
            self.push_undo("shift right");
            let spaces = " ".repeat(count);
            self.lines[index].data = format!("{spaces}{}", self.lines[index].data);
            self.lines[index].modified = true;
            self.modified = true;
        }
    }

    /// Shift line left by `count` spaces.
    pub fn shift_left(&mut self, index: usize, count: usize) {
        if index < self.lines.len() {
            self.push_undo("shift left");
            let data = &self.lines[index].data;
            let stripped = if count >= data.len() {
                String::new()
            } else {
                let leading = data.len() - data.trim_start().len();
                let remove = count.min(leading);
                data[remove..].to_string()
            };
            self.lines[index].data = stripped;
            self.lines[index].modified = true;
            self.modified = true;
        }
    }

    /// Exclude line from display.
    pub fn exclude_line(&mut self, index: usize) {
        if index < self.lines.len() {
            self.lines[index].excluded = true;
        }
    }

    /// Exclude range [start, end].
    pub fn exclude_range(&mut self, start: usize, end: usize) {
        for i in start..=end.min(self.lines.len().saturating_sub(1)) {
            self.lines[i].excluded = true;
        }
    }

    /// Show (un-exclude) line.
    pub fn show_line(&mut self, index: usize) {
        if index < self.lines.len() {
            self.lines[index].excluded = false;
        }
    }

    /// Uppercase a line.
    pub fn uppercase_line(&mut self, index: usize) {
        if index < self.lines.len() {
            self.push_undo("uppercase");
            self.lines[index].data = self.lines[index].data.to_uppercase();
            self.lines[index].modified = true;
            self.modified = true;
        }
    }

    /// Lowercase a line.
    pub fn lowercase_line(&mut self, index: usize) {
        if index < self.lines.len() {
            self.push_undo("lowercase");
            self.lines[index].data = self.lines[index].data.to_lowercase();
            self.lines[index].modified = true;
            self.modified = true;
        }
    }

    /// Set a line label (e.g., .A).
    pub fn set_label(&mut self, index: usize, label: &str) {
        if index < self.lines.len() {
            self.lines[index].label = Some(label.to_uppercase());
        }
    }

    /// Find line by label. Returns 0-based index.
    pub fn find_label(&self, label: &str) -> Option<usize> {
        let upper = label.to_uppercase();
        self.lines.iter().position(|l| l.label.as_deref() == Some(&upper))
    }

    // -----------------------------------------------------------------------
    //  Line command processor
    // -----------------------------------------------------------------------

    /// Process a line command string on the given line index.
    /// Returns Ok(()) on success, Err(message) on error.
    pub fn process_line_cmd(&mut self, index: usize, cmd: &str) -> Result<(), String> {
        let upper = cmd.trim().to_uppercase();

        // Label assignment: .A through .Z.
        if upper.starts_with('.') && upper.len() == 2 {
            self.set_label(index, &upper);
            return Ok(());
        }

        // Parse count suffix: e.g., I5, D3, R2, >2, <3.
        let (base, count) = parse_cmd_count(&upper);

        match base {
            "I" => { self.insert_lines(index, count); Ok(()) }
            "D" => {
                if count == 1 {
                    self.delete_line(index);
                } else {
                    let end = (index + count - 1).min(self.lines.len() - 1);
                    self.delete_range(index, end);
                }
                Ok(())
            }
            "R" => { self.repeat_line(index, count); Ok(()) }
            ">" => { self.shift_right(index, count); Ok(()) }
            "<" => { self.shift_left(index, count); Ok(()) }
            "X" => {
                let end = (index + count - 1).min(self.lines.len().saturating_sub(1));
                self.exclude_range(index, end);
                Ok(())
            }
            "S" => { self.show_line(index); Ok(()) }
            "UC" => { self.uppercase_line(index); Ok(()) }
            "LC" => { self.lowercase_line(index); Ok(()) }
            "C" => {
                // Single-line copy — needs A/B target.
                self.copy_range(index, index, index);
                Ok(())
            }
            "CC" => {
                // Block copy start/end.
                if let Some(pending) = &self.block_pending {
                    if pending.cmd == BlockCmd::Copy {
                        let start = pending.start;
                        let end = index;
                        self.block_pending = None;
                        self.copy_range(start, end, end);
                        return Ok(());
                    }
                }
                self.block_pending = Some(BlockPending { cmd: BlockCmd::Copy, start: index });
                Ok(())
            }
            "DD" => {
                if let Some(pending) = &self.block_pending {
                    if pending.cmd == BlockCmd::Delete {
                        let start = pending.start;
                        let end = index;
                        self.block_pending = None;
                        self.delete_range(start, end);
                        return Ok(());
                    }
                }
                self.block_pending = Some(BlockPending { cmd: BlockCmd::Delete, start: index });
                Ok(())
            }
            "MM" => {
                if let Some(pending) = &self.block_pending {
                    if pending.cmd == BlockCmd::Move {
                        let start = pending.start;
                        let end = index;
                        self.block_pending = None;
                        self.move_range(start, end, end);
                        return Ok(());
                    }
                }
                self.block_pending = Some(BlockPending { cmd: BlockCmd::Move, start: index });
                Ok(())
            }
            "A" => {
                // After — target for pending copy/move block.
                if let Some(pending) = self.block_pending.take() {
                    match pending.cmd {
                        BlockCmd::Copy => self.copy_range(pending.start, pending.start, index),
                        BlockCmd::Move => self.move_range(pending.start, pending.start, index),
                        _ => {}
                    }
                }
                Ok(())
            }
            "B" => {
                // Before — target for pending copy/move.
                if let Some(pending) = self.block_pending.take() {
                    let target = if index > 0 { index - 1 } else { 0 };
                    match pending.cmd {
                        BlockCmd::Copy => self.copy_range(pending.start, pending.start, target),
                        BlockCmd::Move => self.move_range(pending.start, pending.start, target),
                        _ => {}
                    }
                }
                Ok(())
            }
            _ => Err(format!("Unknown line command: {cmd}")),
        }
    }

    // -----------------------------------------------------------------------
    //  Primary commands
    // -----------------------------------------------------------------------

    /// FIND — search for a string. Returns match count and positions.
    pub fn find(&mut self, pattern: &str, all: bool) -> FindResult {
        self.last_find = Some(pattern.to_string());
        let mut result = FindResult { count: 0, line_indices: Vec::new() };

        for (i, line) in self.lines.iter().enumerate() {
            let matches = line.data.matches(pattern).count();
            if matches > 0 {
                result.count += matches;
                result.line_indices.push(i);
                if !all {
                    self.cursor = i;
                    return result;
                }
            }
        }

        result
    }

    /// FIND with case-insensitive search.
    pub fn find_nocase(&mut self, pattern: &str, all: bool) -> FindResult {
        let lower_pat = pattern.to_lowercase();
        self.last_find = Some(pattern.to_string());
        let mut result = FindResult { count: 0, line_indices: Vec::new() };

        for (i, line) in self.lines.iter().enumerate() {
            let lower_data = line.data.to_lowercase();
            let matches = lower_data.matches(&lower_pat).count();
            if matches > 0 {
                result.count += matches;
                result.line_indices.push(i);
                if !all {
                    self.cursor = i;
                    return result;
                }
            }
        }

        result
    }

    /// CHANGE — replace occurrences. Returns count of changes made.
    pub fn change(&mut self, old: &str, new: &str, all: bool) -> usize {
        self.push_undo("change");
        let mut total = 0;

        for line in &mut self.lines {
            if line.data.contains(old) {
                if all {
                    let count = line.data.matches(old).count();
                    line.data = line.data.replace(old, new);
                    line.modified = true;
                    total += count;
                } else if total == 0 {
                    line.data = line.data.replacen(old, new, 1);
                    line.modified = true;
                    total = 1;
                    if !all { break; }
                }
            }
        }

        if total > 0 {
            self.modified = true;
        }
        total
    }

    /// SORT — sort all visible lines.
    pub fn sort(&mut self, ascending: bool) {
        self.push_undo("sort");
        if ascending {
            self.lines.sort_by(|a, b| a.data.cmp(&b.data));
        } else {
            self.lines.sort_by(|a, b| b.data.cmp(&a.data));
        }
        self.modified = true;
    }

    /// RESET — un-exclude all lines, clear labels, clear pending commands.
    pub fn reset(&mut self) {
        for line in &mut self.lines {
            line.excluded = false;
        }
        self.block_pending = None;
    }

    /// COLS — return a column ruler string.
    pub fn cols(&self) -> String {
        let width = self.profile.bounds.1;
        let mut ruler = String::with_capacity(width);
        for i in 1..=width {
            if i % 10 == 0 {
                ruler.push_str(&format!("{}", (i / 10) % 10));
            } else if i % 5 == 0 {
                ruler.push('+');
            } else {
                ruler.push('-');
            }
        }
        ruler
    }

    /// HEX — get hex representation of a line.
    pub fn hex_display(&self, index: usize) -> Option<(String, String)> {
        if index >= self.lines.len() {
            return None;
        }
        let data = &self.lines[index].data;
        let mut high = String::with_capacity(data.len());
        let mut low = String::with_capacity(data.len());
        for byte in data.bytes() {
            high.push(char::from(b"0123456789ABCDEF"[(byte >> 4) as usize]));
            low.push(char::from(b"0123456789ABCDEF"[(byte & 0x0F) as usize]));
        }
        Some((high, low))
    }

    /// PROFILE — switch to a named profile.
    pub fn set_profile(&mut self, name: &str) {
        let upper = name.to_uppercase();
        if let Some(p) = self.profiles.get(&upper) {
            self.profile = p.clone();
        }
    }

    /// BOUNDS — set edit bounds.
    pub fn set_bounds(&mut self, left: usize, right: usize) {
        self.profile.bounds = (left, right);
    }

    /// CAPS ON/OFF.
    pub fn set_caps(&mut self, on: bool) {
        self.profile.caps = on;
    }

    /// HEX ON/OFF.
    pub fn set_hex(&mut self, on: bool) {
        self.profile.hex = on;
    }

    /// NUMBER ON/OFF.
    pub fn set_number(&mut self, on: bool) {
        self.profile.number = on;
    }

    // -----------------------------------------------------------------------
    //  Undo / Redo
    // -----------------------------------------------------------------------

    /// UNDO — restore previous state. Returns true if undo was performed.
    pub fn undo(&mut self) -> bool {
        if let Some(entry) = self.undo_stack.pop() {
            self.redo_stack.push(UndoEntry {
                lines: self.lines.clone(),
                cursor_line: self.cursor,
                description: entry.description.clone(),
            });
            self.lines = entry.lines;
            self.cursor = entry.cursor_line;
            self.modified = true;
            true
        } else {
            false
        }
    }

    /// REDO — re-apply undone change. Returns true if redo was performed.
    pub fn redo(&mut self) -> bool {
        if let Some(entry) = self.redo_stack.pop() {
            self.undo_stack.push(UndoEntry {
                lines: self.lines.clone(),
                cursor_line: self.cursor,
                description: entry.description.clone(),
            });
            self.lines = entry.lines;
            self.cursor = entry.cursor_line;
            self.modified = true;
            true
        } else {
            false
        }
    }

    // -----------------------------------------------------------------------
    //  Primary command processor
    // -----------------------------------------------------------------------

    /// Process a primary command string.
    /// Returns Ok(message) on success, Err(message) on error.
    pub fn process_primary_cmd(&mut self, cmd: &str) -> Result<String, String> {
        let trimmed = cmd.trim();
        let upper = trimmed.to_uppercase();
        let parts: Vec<&str> = upper.split_whitespace().collect();

        match parts.first().copied() {
            Some("FIND") | Some("F") => {
                let pattern = extract_quoted(&upper, 1);
                let all = parts.last().copied() == Some("ALL");
                let result = self.find(&pattern, all);
                if result.count > 0 {
                    Ok(format!("CHARS '{}' FOUND - {} OCCURRENCE(S)", pattern, result.count))
                } else {
                    Err(format!("CHARS '{}' NOT FOUND", pattern))
                }
            }
            Some("CHANGE") | Some("CHG") | Some("C") => {
                let (old, new) = extract_two_quoted(&upper);
                let all = parts.last().copied() == Some("ALL");
                let count = self.change(&old, &new, all);
                Ok(format!("CHARS '{}' CHANGED TO '{}' - {} OCCURRENCE(S)", old, new, count))
            }
            Some("SORT") => {
                let desc = parts.get(1).copied() == Some("D");
                self.sort(!desc);
                Ok("SORT COMPLETED".to_string())
            }
            Some("RESET") | Some("RES") => {
                self.reset();
                Ok("RESET COMPLETED".to_string())
            }
            Some("SAVE") => {
                self.save();
                Ok("SAVED".to_string())
            }
            Some("CANCEL") | Some("CAN") => {
                self.cancel();
                Ok("CANCELLED".to_string())
            }
            Some("UNDO") => {
                if self.undo() {
                    Ok("UNDO COMPLETED".to_string())
                } else {
                    Err("NOTHING TO UNDO".to_string())
                }
            }
            Some("REDO") => {
                if self.redo() {
                    Ok("REDO COMPLETED".to_string())
                } else {
                    Err("NOTHING TO REDO".to_string())
                }
            }
            Some("PROFILE") => {
                if let Some(name) = parts.get(1) {
                    self.set_profile(name);
                    Ok(format!("PROFILE {name} SET"))
                } else {
                    Ok(format!("PROFILE {} ACTIVE", self.profile.name))
                }
            }
            Some("BOUNDS") => {
                if parts.len() >= 3 {
                    let left = parts[1].parse::<usize>().unwrap_or(1);
                    let right = parts[2].parse::<usize>().unwrap_or(80);
                    self.set_bounds(left, right);
                    Ok(format!("BOUNDS SET {left} {right}"))
                } else {
                    Ok(format!("BOUNDS {} {}", self.profile.bounds.0, self.profile.bounds.1))
                }
            }
            Some("CAPS") => {
                let on = parts.get(1).copied() != Some("OFF");
                self.set_caps(on);
                Ok(format!("CAPS {}", if on { "ON" } else { "OFF" }))
            }
            Some("HEX") => {
                let on = parts.get(1).copied() != Some("OFF");
                self.set_hex(on);
                Ok(format!("HEX {}", if on { "ON" } else { "OFF" }))
            }
            Some("NUMBER") | Some("NUM") => {
                let on = parts.get(1).copied() != Some("OFF");
                self.set_number(on);
                Ok(format!("NUMBER {}", if on { "ON" } else { "OFF" }))
            }
            Some("COLS") => {
                Ok(self.cols())
            }
            Some("SUBMIT") | Some("SUB") => {
                Ok("SUBMITTED".to_string())
            }
            _ => Err(format!("UNKNOWN COMMAND: {}", parts.first().copied().unwrap_or(""))),
        }
    }
}

/// A visible line — either actual data or an excluded-block summary.
#[derive(Debug)]
pub enum VisibleLine<'a> {
    Data { index: usize, line: &'a EditorLine },
    ExcludedBlock { start_index: usize, count: usize },
}

// ---------------------------------------------------------------------------
//  Helpers
// ---------------------------------------------------------------------------

/// Parse a line command and optional count: "I5" → ("I", 5), "D" → ("D", 1).
fn parse_cmd_count(cmd: &str) -> (&str, usize) {
    // Two-char commands first: CC, DD, MM, UC, LC.
    for prefix in &["CC", "DD", "MM", "UC", "LC"] {
        if cmd == *prefix {
            return (prefix, 1);
        }
    }

    // Single-char commands with optional count.
    for prefix in &["I", "D", "R", "C", "M", ">", "<", "X", "S", "A", "B"] {
        if let Some(rest) = cmd.strip_prefix(prefix) {
            let count = rest.parse::<usize>().unwrap_or(1).max(1);
            return (prefix, count);
        }
    }

    (cmd, 1)
}

/// Detect profile from dataset name.
fn detect_profile(dataset: &str, profiles: &HashMap<String, EditProfile>) -> EditProfile {
    let upper = dataset.to_uppercase();
    if upper.contains("COBOL") || upper.contains("COB") || upper.contains("CBL") {
        profiles.get("COBOL").cloned().unwrap_or_default()
    } else if upper.contains("JCL") || upper.contains("CNTL") {
        profiles.get("JCL").cloned().unwrap_or_default()
    } else {
        profiles.get("DEFAULT").cloned().unwrap_or_default()
    }
}

/// Extract a quoted string from a command at word position `pos`.
fn extract_quoted(cmd: &str, _pos: usize) -> String {
    // Find first quoted string.
    if let Some(start) = cmd.find('\'') {
        if let Some(end) = cmd[start + 1..].find('\'') {
            return cmd[start + 1..start + 1 + end].to_string();
        }
    }
    // Fallback: second word.
    cmd.split_whitespace().nth(1).unwrap_or("").to_string()
}

/// Extract two quoted strings from a command.
fn extract_two_quoted(cmd: &str) -> (String, String) {
    let mut strings = Vec::new();
    let mut chars = cmd.chars().peekable();
    while let Some(&c) = chars.peek() {
        if c == '\'' {
            chars.next(); // skip opening quote
            let mut s = String::new();
            while let Some(&c2) = chars.peek() {
                if c2 == '\'' {
                    chars.next();
                    break;
                }
                s.push(c2);
                chars.next();
            }
            strings.push(s);
        } else {
            chars.next();
        }
    }

    let first = strings.first().cloned().unwrap_or_default();
    let second = strings.get(1).cloned().unwrap_or_default();
    (first, second)
}

// ---------------------------------------------------------------------------
//  Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    fn sample_data() -> Vec<String> {
        vec![
            "LINE ONE".to_string(),
            "LINE TWO".to_string(),
            "LINE THREE".to_string(),
            "LINE FOUR".to_string(),
            "LINE FIVE".to_string(),
        ]
    }

    #[test]
    fn test_editor_new() {
        let ed = Editor::new("USER01.DATA", sample_data(), true);
        assert_eq!(ed.lines.len(), 5);
        assert!(ed.edit_mode);
        assert!(!ed.modified);
    }

    #[test]
    fn test_insert_lines() {
        let mut ed = Editor::new("T", sample_data(), true);
        ed.insert_lines(1, 3);
        assert_eq!(ed.lines.len(), 8);
        assert!(ed.lines[2].data.is_empty());
        assert!(ed.modified);
    }

    #[test]
    fn test_delete_line() {
        let mut ed = Editor::new("T", sample_data(), true);
        ed.delete_line(2);
        assert_eq!(ed.lines.len(), 4);
        assert_eq!(ed.lines[2].data, "LINE FOUR");
    }

    #[test]
    fn test_delete_range() {
        let mut ed = Editor::new("T", sample_data(), true);
        ed.delete_range(1, 3);
        assert_eq!(ed.lines.len(), 2);
        assert_eq!(ed.lines[0].data, "LINE ONE");
        assert_eq!(ed.lines[1].data, "LINE FIVE");
    }

    #[test]
    fn test_repeat_line() {
        let mut ed = Editor::new("T", sample_data(), true);
        ed.repeat_line(0, 2);
        assert_eq!(ed.lines.len(), 7);
        assert_eq!(ed.lines[1].data, "LINE ONE");
        assert_eq!(ed.lines[2].data, "LINE ONE");
    }

    #[test]
    fn test_copy_range() {
        let mut ed = Editor::new("T", sample_data(), true);
        ed.copy_range(0, 1, 4);
        assert_eq!(ed.lines.len(), 7);
        assert_eq!(ed.lines[5].data, "LINE ONE");
        assert_eq!(ed.lines[6].data, "LINE TWO");
    }

    #[test]
    fn test_shift_right() {
        let mut ed = Editor::new("T", sample_data(), true);
        ed.shift_right(0, 3);
        assert_eq!(ed.lines[0].data, "   LINE ONE");
    }

    #[test]
    fn test_shift_left() {
        let mut ed = Editor::new("T", vec!["   INDENTED".to_string()], true);
        ed.shift_left(0, 2);
        assert_eq!(ed.lines[0].data, " INDENTED");
    }

    #[test]
    fn test_exclude_and_show() {
        let mut ed = Editor::new("T", sample_data(), true);
        ed.exclude_range(1, 3);
        let visible = ed.visible_lines();
        // Should have: line 0, excluded block (1-3), line 4.
        assert_eq!(visible.len(), 3);
        assert!(matches!(visible[1], VisibleLine::ExcludedBlock { count: 3, .. }));

        ed.show_line(2);
        assert!(!ed.lines[2].excluded);
    }

    #[test]
    fn test_uppercase_lowercase() {
        let mut ed = Editor::new("T", vec!["Hello World".to_string()], true);
        ed.uppercase_line(0);
        assert_eq!(ed.lines[0].data, "HELLO WORLD");
        ed.lowercase_line(0);
        assert_eq!(ed.lines[0].data, "hello world");
    }

    #[test]
    fn test_find() {
        let mut ed = Editor::new("T", sample_data(), true);
        let result = ed.find("THREE", true);
        assert_eq!(result.count, 1);
        assert_eq!(result.line_indices, vec![2]);
    }

    #[test]
    fn test_find_all() {
        let mut ed = Editor::new("T", sample_data(), true);
        let result = ed.find("LINE", true);
        assert_eq!(result.count, 5);
        assert_eq!(result.line_indices.len(), 5);
    }

    #[test]
    fn test_change() {
        let mut ed = Editor::new("T", sample_data(), true);
        let count = ed.change("LINE", "ROW", true);
        assert_eq!(count, 5);
        assert_eq!(ed.lines[0].data, "ROW ONE");
    }

    #[test]
    fn test_change_single() {
        let mut ed = Editor::new("T", sample_data(), true);
        let count = ed.change("LINE", "ROW", false);
        assert_eq!(count, 1);
        assert_eq!(ed.lines[0].data, "ROW ONE");
        assert_eq!(ed.lines[1].data, "LINE TWO"); // Unchanged.
    }

    #[test]
    fn test_sort_ascending() {
        let mut ed = Editor::new("T", vec!["C".into(), "A".into(), "B".into()], true);
        ed.sort(true);
        assert_eq!(ed.lines[0].data, "A");
        assert_eq!(ed.lines[1].data, "B");
        assert_eq!(ed.lines[2].data, "C");
    }

    #[test]
    fn test_sort_descending() {
        let mut ed = Editor::new("T", vec!["A".into(), "C".into(), "B".into()], true);
        ed.sort(false);
        assert_eq!(ed.lines[0].data, "C");
        assert_eq!(ed.lines[1].data, "B");
        assert_eq!(ed.lines[2].data, "A");
    }

    #[test]
    fn test_undo_redo() {
        let mut ed = Editor::new("T", sample_data(), true);
        ed.delete_line(0);
        assert_eq!(ed.lines.len(), 4);

        assert!(ed.undo());
        assert_eq!(ed.lines.len(), 5);
        assert_eq!(ed.lines[0].data, "LINE ONE");

        assert!(ed.redo());
        assert_eq!(ed.lines.len(), 4);
    }

    #[test]
    fn test_undo_nothing() {
        let mut ed = Editor::new("T", sample_data(), true);
        assert!(!ed.undo());
    }

    #[test]
    fn test_save_and_cancel() {
        let mut ed = Editor::new("T", sample_data(), true);
        ed.delete_line(0);
        assert!(ed.modified);

        let saved = ed.save();
        assert_eq!(saved.len(), 4);
        assert!(!ed.modified);

        // Now modify again and cancel.
        ed.delete_line(0);
        let orig = ed.cancel();
        assert_eq!(orig.len(), 5);
        assert_eq!(orig[0], "LINE ONE");
    }

    #[test]
    fn test_end_edit_modified() {
        let mut ed = Editor::new("T", sample_data(), true);
        ed.delete_line(0);
        let (was_modified, data) = ed.end_edit();
        assert!(was_modified);
        assert_eq!(data.len(), 4);
    }

    #[test]
    fn test_end_edit_unmodified() {
        let mut ed = Editor::new("T", sample_data(), true);
        let (was_modified, data) = ed.end_edit();
        assert!(!was_modified);
        assert_eq!(data.len(), 5);
    }

    #[test]
    fn test_hex_display() {
        let ed = Editor::new("T", vec!["AB".to_string()], true);
        let (high, low) = ed.hex_display(0).unwrap();
        assert_eq!(high, "44");
        assert_eq!(low, "12");
    }

    #[test]
    fn test_cols() {
        let ed = Editor::new("T", sample_data(), true);
        let ruler = ed.cols();
        assert!(!ruler.is_empty());
        assert!(ruler.len() == ed.profile.bounds.1);
    }

    #[test]
    fn test_profile_cobol() {
        let ed = Editor::new("USER01.COBOL(MYPROG)", sample_data(), true);
        assert_eq!(ed.profile.name, "COBOL");
        assert!(ed.profile.caps);
        assert_eq!(ed.profile.bounds, (7, 72));
    }

    #[test]
    fn test_profile_jcl() {
        let ed = Editor::new("USER01.JCL(MYJOB)", sample_data(), true);
        assert_eq!(ed.profile.name, "JCL");
        assert_eq!(ed.profile.bounds, (1, 72));
    }

    #[test]
    fn test_set_label_find() {
        let mut ed = Editor::new("T", sample_data(), true);
        ed.set_label(2, ".A");
        assert_eq!(ed.find_label(".A"), Some(2));
    }

    #[test]
    fn test_process_line_cmd_i5() {
        let mut ed = Editor::new("T", sample_data(), true);
        ed.process_line_cmd(2, "I5").unwrap();
        assert_eq!(ed.lines.len(), 10);
    }

    #[test]
    fn test_process_line_cmd_d3() {
        let mut ed = Editor::new("T", sample_data(), true);
        ed.process_line_cmd(1, "D3").unwrap();
        assert_eq!(ed.lines.len(), 2);
        assert_eq!(ed.lines[0].data, "LINE ONE");
        assert_eq!(ed.lines[1].data, "LINE FIVE");
    }

    #[test]
    fn test_process_line_cmd_label() {
        let mut ed = Editor::new("T", sample_data(), true);
        ed.process_line_cmd(3, ".B").unwrap();
        assert_eq!(ed.find_label(".B"), Some(3));
    }

    #[test]
    fn test_primary_cmd_find() {
        let mut ed = Editor::new("T", sample_data(), true);
        let result = ed.process_primary_cmd("FIND 'THREE' ALL");
        assert!(result.is_ok());
        assert!(result.unwrap().contains("1 OCCURRENCE"));
    }

    #[test]
    fn test_primary_cmd_change() {
        let mut ed = Editor::new("T", sample_data(), true);
        let result = ed.process_primary_cmd("CHANGE 'LINE' 'ROW' ALL");
        assert!(result.is_ok());
        assert_eq!(ed.lines[0].data, "ROW ONE");
    }

    #[test]
    fn test_primary_cmd_reset() {
        let mut ed = Editor::new("T", sample_data(), true);
        ed.exclude_range(0, 4);
        ed.process_primary_cmd("RESET").unwrap();
        assert!(ed.lines.iter().all(|l| !l.excluded));
    }

    #[test]
    fn test_primary_cmd_profile() {
        let mut ed = Editor::new("T", sample_data(), true);
        let result = ed.process_primary_cmd("PROFILE COBOL");
        assert!(result.is_ok());
        assert_eq!(ed.profile.name, "COBOL");
    }

    #[test]
    fn test_move_range() {
        let mut ed = Editor::new("T", sample_data(), true);
        // Move lines 0-1 after line 3.
        ed.move_range(0, 1, 3);
        assert_eq!(ed.lines[0].data, "LINE THREE");
        assert_eq!(ed.lines[1].data, "LINE FOUR");
        assert_eq!(ed.lines[2].data, "LINE ONE");
        assert_eq!(ed.lines[3].data, "LINE TWO");
    }
}
