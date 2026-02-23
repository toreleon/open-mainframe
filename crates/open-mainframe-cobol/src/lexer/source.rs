//! Source file handling and COBOL format detection.
//!
//! This module handles reading source files, detecting encoding,
//! and parsing the COBOL fixed/free format structure.

use std::fs;
use std::path::{Path, PathBuf};

use crate::error::CobolError;
use crate::lexer::span::{FileId, Span};
use open_mainframe_lang_core::preprocess;

/// Result type for source operations.
pub type Result<T> = std::result::Result<T, CobolError>;

/// COBOL source format.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum SourceFormat {
    /// Traditional fixed format (columns 1-6: sequence, 7: indicator, 8-72: code, 73-80: ignored).
    #[default]
    Fixed,
    /// Free format (no column restrictions).
    Free,
}

/// Column indicator values in fixed format.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Indicator {
    /// Normal code line (space or no indicator).
    Normal,
    /// Comment line ('*' or '/').
    Comment,
    /// Continuation line ('-').
    Continuation,
    /// Debug line ('D' or 'd').
    Debug,
}

impl Indicator {
    /// Parse an indicator character.
    pub fn from_char(ch: char) -> Self {
        match ch {
            '*' | '/' => Indicator::Comment,
            '-' => Indicator::Continuation,
            'D' | 'd' => Indicator::Debug,
            _ => Indicator::Normal,
        }
    }
}

/// A processed line of COBOL source.
#[derive(Debug, Clone)]
pub struct SourceLine {
    /// The original line number (1-indexed).
    pub line_number: u32,
    /// The sequence number area (columns 1-6 in fixed format).
    pub sequence: String,
    /// The indicator character.
    pub indicator: Indicator,
    /// The code content (Area A + Area B in fixed format).
    pub content: String,
    /// Starting byte offset in original source.
    pub start_offset: u32,
    /// Byte offset where content starts (after indicator).
    pub content_offset: u32,
}

impl SourceLine {
    /// Check if this line is a comment.
    pub fn is_comment(&self) -> bool {
        self.indicator == Indicator::Comment
    }

    /// Check if this line is a continuation.
    pub fn is_continuation(&self) -> bool {
        self.indicator == Indicator::Continuation
    }

    /// Check if this line is a debug line.
    pub fn is_debug(&self) -> bool {
        self.indicator == Indicator::Debug
    }

    /// Get the content trimmed of trailing whitespace.
    pub fn trimmed_content(&self) -> &str {
        self.content.trim_end()
    }
}

/// A source file loaded into memory.
#[derive(Debug, Clone)]
pub struct SourceFile {
    /// Unique identifier for this file.
    pub id: FileId,
    /// The file path (if loaded from disk).
    pub path: Option<PathBuf>,
    /// The raw source text (converted to UTF-8).
    pub text: String,
    /// The detected/configured source format.
    pub format: SourceFormat,
    /// Processed lines.
    pub lines: Vec<SourceLine>,
}

impl SourceFile {
    /// Create a source file from text.
    pub fn from_text(id: FileId, text: String, format: SourceFormat) -> Self {
        // Always normalize line endings to prevent offset drift
        let text = preprocess::normalize_line_endings(&text);
        let lines = parse_lines(&text, format);
        Self {
            id,
            path: None,
            text,
            format,
            lines,
        }
    }

    /// Load a source file from disk.
    pub fn from_path(id: FileId, path: &Path, format: SourceFormat) -> Result<Self> {
        let text = fs::read_to_string(path).map_err(|e| CobolError::IoError {
            message: format!("Failed to read {}: {}", path.display(), e),
        })?;

        // Normalize line endings using the canonical core implementation
        let text = preprocess::normalize_line_endings(&text);
        let lines = parse_lines(&text, format);

        Ok(Self {
            id,
            path: Some(path.to_path_buf()),
            text,
            format,
            lines,
        })
    }

    /// Get a span for a line.
    pub fn line_span(&self, line_idx: usize) -> Span {
        if let Some(line) = self.lines.get(line_idx) {
            Span::new(
                self.id,
                line.content_offset,
                line.content_offset + line.content.len() as u32,
            )
        } else {
            Span::dummy()
        }
    }

    /// Get the source text for a span.
    pub fn span_text(&self, span: Span) -> &str {
        let start = span.start as usize;
        let end = (span.end as usize).min(self.text.len());
        if start < self.text.len() {
            &self.text[start..end]
        } else {
            ""
        }
    }

    /// Get the file name for error messages.
    pub fn file_name(&self) -> &str {
        self.path
            .as_ref()
            .and_then(|p| p.file_name())
            .and_then(|n| n.to_str())
            .unwrap_or("<source>")
    }
}

/// Parse source text into processed lines.
///
/// The input text **must** already be normalized (no `\r` bytes). This is
/// guaranteed when called via [`SourceFile::from_text`] or
/// [`SourceFile::from_path`], both of which normalize first.
///
/// Line offsets are computed using pointer arithmetic against the source
/// string, avoiding the `offset += line.len() + 1` pattern that drifts
/// on `\r\n` inputs.
fn parse_lines(text: &str, format: SourceFormat) -> Vec<SourceLine> {
    let mut lines = Vec::new();
    let base = text.as_ptr() as usize;
    let mut line_number: u32 = 1;

    for line in text.lines() {
        // Derive the true byte offset via pointer arithmetic.
        let offset = (line.as_ptr() as usize - base) as u32;

        let (sequence, indicator, content, content_offset) = match format {
            SourceFormat::Fixed => parse_fixed_line(line, offset),
            SourceFormat::Free => parse_free_line(line, offset),
        };

        lines.push(SourceLine {
            line_number,
            sequence,
            indicator,
            content,
            start_offset: offset,
            content_offset,
        });

        line_number += 1;
    }

    lines
}

/// Parse a fixed-format COBOL line.
///
/// Fixed format layout:
/// - Columns 1-6: Sequence number area (ignored)
/// - Column 7: Indicator area
/// - Columns 8-72: Areas A and B (actual code)
/// - Columns 73-80: Identification area (ignored)
fn parse_fixed_line(line: &str, start_offset: u32) -> (String, Indicator, String, u32) {
    // Handle short lines
    if line.len() < 7 {
        // Line too short to have indicator
        let content = if line.len() > 6 {
            line[6..].to_string()
        } else {
            String::new()
        };
        return (
            line.chars().take(6).collect(),
            Indicator::Normal,
            content,
            start_offset + line.len().min(7) as u32,
        );
    }

    let chars: Vec<char> = line.chars().collect();

    // Extract sequence number (cols 1-6)
    let sequence: String = chars.iter().take(6).collect();

    // Extract indicator (col 7)
    let indicator_char = chars.get(6).copied().unwrap_or(' ');
    let indicator = Indicator::from_char(indicator_char);

    // Extract code area (cols 8-72)
    // In modern practice, we allow longer lines but only process through col 72 in strict mode
    let code_start = 7; // 0-indexed position of column 8
    let code_end = chars.len().min(72); // Don't exceed column 72

    let content: String = if code_start < chars.len() {
        chars[code_start..code_end].iter().collect()
    } else {
        String::new()
    };

    // Content starts at column 8 (offset 7)
    let content_offset = start_offset + 7;

    (sequence, indicator, content, content_offset)
}

/// Parse a free-format COBOL line.
fn parse_free_line(line: &str, start_offset: u32) -> (String, Indicator, String, u32) {
    let trimmed = line.trim_start();
    let leading_spaces = line.len() - trimmed.len();

    // In free format, '*>' starts a comment
    let (indicator, content) = if trimmed.starts_with("*>") {
        (Indicator::Comment, trimmed.to_string())
    } else {
        (Indicator::Normal, line.to_string())
    };

    (
        String::new(),
        indicator,
        content,
        start_offset + leading_spaces as u32,
    )
}

/// Source file manager that tracks all loaded files.
#[derive(Debug, Default)]
pub struct SourceManager {
    /// All loaded source files.
    files: Vec<SourceFile>,
}

impl SourceManager {
    /// Create a new source manager.
    pub fn new() -> Self {
        Self::default()
    }

    /// Add a source file from text.
    pub fn add_text(&mut self, text: String, format: SourceFormat) -> FileId {
        let id = FileId(self.files.len() as u32);
        self.files.push(SourceFile::from_text(id, text, format));
        id
    }

    /// Add a source file from a path.
    pub fn add_file(&mut self, path: &Path, format: SourceFormat) -> Result<FileId> {
        let id = FileId(self.files.len() as u32);
        let file = SourceFile::from_path(id, path, format)?;
        self.files.push(file);
        Ok(id)
    }

    /// Get a source file by ID.
    pub fn get(&self, id: FileId) -> Option<&SourceFile> {
        self.files.get(id.0 as usize)
    }

    /// Get a mutable source file by ID.
    pub fn get_mut(&mut self, id: FileId) -> Option<&mut SourceFile> {
        self.files.get_mut(id.0 as usize)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_fixed_format_parsing() {
        let source = "000100 IDENTIFICATION DIVISION.                                        HELLO";
        let lines = parse_lines(source, SourceFormat::Fixed);

        assert_eq!(lines.len(), 1);
        assert_eq!(lines[0].sequence, "000100");
        assert_eq!(lines[0].indicator, Indicator::Normal);
        // Content is from col 8 to col 72
        assert!(lines[0].content.starts_with("IDENTIFICATION DIVISION."));
    }

    #[test]
    fn test_comment_line() {
        let source = "      * This is a comment";
        let lines = parse_lines(source, SourceFormat::Fixed);

        assert_eq!(lines.len(), 1);
        assert_eq!(lines[0].indicator, Indicator::Comment);
    }

    #[test]
    fn test_continuation_line() {
        let source = "      -    continuation";
        let lines = parse_lines(source, SourceFormat::Fixed);

        assert_eq!(lines.len(), 1);
        assert_eq!(lines[0].indicator, Indicator::Continuation);
    }

    #[test]
    fn test_free_format_comment() {
        let source = "*> This is a free format comment";
        let lines = parse_lines(source, SourceFormat::Free);

        assert_eq!(lines.len(), 1);
        assert_eq!(lines[0].indicator, Indicator::Comment);
    }

    #[test]
    fn test_normalize_line_endings() {
        assert_eq!(preprocess::normalize_line_endings("a\r\nb\r\nc"), "a\nb\nc");
        assert_eq!(preprocess::normalize_line_endings("a\rb\rc"), "a\nb\nc");
        assert_eq!(preprocess::normalize_line_endings("a\nb\nc"), "a\nb\nc");
    }

    #[test]
    fn test_source_manager() {
        let mut manager = SourceManager::new();

        let id1 = manager.add_text("IDENTIFICATION DIVISION.".to_string(), SourceFormat::Free);
        let id2 = manager.add_text("PROGRAM-ID. TEST.".to_string(), SourceFormat::Free);

        assert_eq!(id1, FileId(0));
        assert_eq!(id2, FileId(1));

        let file1 = manager.get(id1).unwrap();
        assert!(file1.text.contains("IDENTIFICATION"));
    }
}
