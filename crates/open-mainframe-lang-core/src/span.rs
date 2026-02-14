//! Source location tracking for error reporting.
//!
//! Every token and AST node includes a [`Span`] that tracks its position
//! in the source code. This enables precise error messages with source context.
//!
//! These types are shared across all language crates (`open-mainframe-cobol`,
//! `open-mainframe-jcl`, etc.) so that tooling can operate uniformly.

use std::ops::Range;

/// Unique identifier for a source file.
///
/// Used to distinguish tokens from different files (main source vs copybooks/includes).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct FileId(pub u32);

impl FileId {
    /// The default file ID for the main source file.
    pub const MAIN: FileId = FileId(0);
}

/// A location in source code, representing a contiguous range of bytes.
///
/// Spans are used throughout the compiler pipeline to track where each token
/// and AST node originated in the source. This enables:
/// - Precise error messages pointing to the exact problematic code
/// - Source-level debugging information
/// - IDE features like go-to-definition
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct Span {
    /// The source file this span belongs to.
    pub file: FileId,
    /// Byte offset of the start of this span (0-indexed).
    pub start: u32,
    /// Byte offset of the end of this span (exclusive).
    pub end: u32,
}

impl Span {
    /// Create a new span.
    pub fn new(file: FileId, start: u32, end: u32) -> Self {
        Self { file, start, end }
    }

    /// Create a span in the main source file.
    pub fn main(start: u32, end: u32) -> Self {
        Self::new(FileId::MAIN, start, end)
    }

    /// Create an empty span at a position.
    pub fn point(file: FileId, pos: u32) -> Self {
        Self::new(file, pos, pos)
    }

    /// Create a dummy span for synthesized nodes.
    pub fn dummy() -> Self {
        Self::default()
    }

    /// Get the length of this span in bytes.
    pub fn len(&self) -> usize {
        (self.end - self.start) as usize
    }

    /// Check if this span is empty.
    pub fn is_empty(&self) -> bool {
        self.start == self.end
    }

    /// Extend this span to include another span.
    pub fn extend(self, other: Span) -> Self {
        debug_assert_eq!(self.file, other.file, "Cannot extend span across files");
        Self {
            file: self.file,
            start: self.start.min(other.start),
            end: self.end.max(other.end),
        }
    }

    /// Convert to a byte range.
    pub fn to_range(&self) -> Range<usize> {
        (self.start as usize)..(self.end as usize)
    }
}

/// Resolved location information for display purposes.
///
/// Contains line/column information computed from a [`Span`] and the source text.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Location {
    /// Line number (1-indexed).
    pub line: u32,
    /// Column number (1-indexed).
    pub column: u32,
    /// The file name or path.
    pub file_name: String,
}

impl Location {
    /// Create a new location.
    pub fn new(file_name: impl Into<String>, line: u32, column: u32) -> Self {
        Self {
            file_name: file_name.into(),
            line,
            column,
        }
    }
}

/// Compute line and column from a byte offset in source text.
///
/// Returns `(line, column)` where both are 1-indexed.
pub fn offset_to_line_col(source: &str, offset: usize) -> (u32, u32) {
    let mut line = 1;
    let mut col = 1;

    for (i, ch) in source.char_indices() {
        if i >= offset {
            break;
        }
        if ch == '\n' {
            line += 1;
            col = 1;
        } else {
            col += 1;
        }
    }

    (line, col)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_span_creation() {
        let span = Span::new(FileId(1), 10, 20);
        assert_eq!(span.file, FileId(1));
        assert_eq!(span.start, 10);
        assert_eq!(span.end, 20);
        assert_eq!(span.len(), 10);
    }

    #[test]
    fn test_span_main() {
        let span = Span::main(5, 15);
        assert_eq!(span.file, FileId::MAIN);
        assert_eq!(span.start, 5);
        assert_eq!(span.end, 15);
    }

    #[test]
    fn test_span_point() {
        let span = Span::point(FileId(2), 42);
        assert_eq!(span.start, 42);
        assert_eq!(span.end, 42);
        assert!(span.is_empty());
    }

    #[test]
    fn test_span_dummy() {
        let span = Span::dummy();
        assert_eq!(span.file, FileId(0));
        assert_eq!(span.start, 0);
        assert_eq!(span.end, 0);
    }

    #[test]
    fn test_span_extend() {
        let span1 = Span::main(10, 20);
        let span2 = Span::main(15, 30);
        let extended = span1.extend(span2);
        assert_eq!(extended.start, 10);
        assert_eq!(extended.end, 30);
    }

    #[test]
    fn test_span_to_range() {
        let span = Span::main(10, 20);
        assert_eq!(span.to_range(), 10..20);
    }

    #[test]
    fn test_offset_to_line_col() {
        let source = "line 1\nline 2\nline 3";
        assert_eq!(offset_to_line_col(source, 0), (1, 1));
        assert_eq!(offset_to_line_col(source, 5), (1, 6));
        assert_eq!(offset_to_line_col(source, 7), (2, 1));
        assert_eq!(offset_to_line_col(source, 14), (3, 1));
    }

    #[test]
    fn test_offset_to_line_col_empty() {
        let source = "";
        assert_eq!(offset_to_line_col(source, 0), (1, 1));
    }

    #[test]
    fn test_location_new() {
        let loc = Location::new("test.cbl", 10, 5);
        assert_eq!(loc.file_name, "test.cbl");
        assert_eq!(loc.line, 10);
        assert_eq!(loc.column, 5);
    }

    #[test]
    fn test_file_id_main() {
        assert_eq!(FileId::MAIN, FileId(0));
    }
}
