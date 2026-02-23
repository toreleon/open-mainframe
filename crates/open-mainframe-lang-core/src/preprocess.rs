//! Source preprocessing for all OpenMainframe language compilers.
//!
//! Every language lexer should pass raw source through [`PreprocessedSource::new()`]
//! before tokenization. This guarantees:
//! - Line endings are normalized to `\n` (handles `\r\n`, `\r`, and `\n`)
//! - Byte offsets in [`LineIndex`] are accurate regardless of the original
//!   line terminator style
//!
//! # The Problem This Solves
//!
//! Rust's [`str::lines()`] transparently strips both `\r\n` and `\n`, but
//! reports the same `line.len()` either way. Code that accumulates offsets
//! via `offset += line.len() + 1` silently drifts by one byte per `\r\n`
//! line, eventually producing garbage spans.
//!
//! [`PreprocessedSource`] normalizes first, then builds a line index from
//! actual byte positions in the normalized text — no assumptions about
//! terminator width.

/// Result of preprocessing a source string.
///
/// Holds the normalized text and a precomputed line-offset index. Language
/// crates should store the [`PreprocessedSource`] and use its [`LineIndex`]
/// whenever they need byte offsets for spans.
#[derive(Debug, Clone)]
pub struct PreprocessedSource {
    /// Source text with all line endings converted to `\n`.
    pub text: String,
    /// Precomputed line offset index built from the normalized text.
    pub line_index: LineIndex,
}

impl PreprocessedSource {
    /// Preprocess raw source text.
    ///
    /// 1. Normalizes all line endings (`\r\n` and bare `\r`) to `\n`.
    /// 2. Builds a [`LineIndex`] from the normalized text.
    pub fn new(raw: &str) -> Self {
        let text = normalize_line_endings(raw);
        let line_index = LineIndex::new(&text);
        Self { text, line_index }
    }

    /// Preprocess source that is already known to have Unix line endings.
    ///
    /// Skips normalization. Use this only when the caller can guarantee
    /// that the input contains no `\r` bytes (e.g., it was already
    /// normalized or was generated programmatically).
    pub fn from_unix(text: String) -> Self {
        let line_index = LineIndex::new(&text);
        Self { text, line_index }
    }

    /// Get the line (0-indexed) and column (0-indexed) for a byte offset.
    ///
    /// Returns `(line, column)` both 0-indexed. Use [`offset_to_line_col_1`]
    /// for 1-indexed values suitable for user-facing diagnostics.
    pub fn offset_to_line_col_0(&self, offset: u32) -> (u32, u32) {
        self.line_index.offset_to_line_col_0(offset)
    }

    /// Get the line (1-indexed) and column (1-indexed) for a byte offset.
    pub fn offset_to_line_col_1(&self, offset: u32) -> (u32, u32) {
        let (line, col) = self.offset_to_line_col_0(offset);
        (line + 1, col + 1)
    }
}

/// Precomputed byte-offset index for each line in a source string.
///
/// Built by scanning the normalized text once. All offsets are derived from
/// actual byte positions — never from length accumulation with assumed
/// terminator sizes.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LineIndex {
    /// Byte offset of the start of each line.
    /// `offsets[i]` is the byte offset where line `i` (0-indexed) begins.
    offsets: Vec<u32>,
}

impl LineIndex {
    /// Build a line index from (already-normalized) source text.
    ///
    /// The text **must** use `\n` as its only line terminator. Call
    /// [`normalize_line_endings`] first if you are unsure.
    pub fn new(text: &str) -> Self {
        let mut offsets = Vec::new();
        // Line 0 always starts at byte 0.
        offsets.push(0);

        for (i, byte) in text.as_bytes().iter().enumerate() {
            if *byte == b'\n' {
                // The next line starts at the byte right after the `\n`.
                offsets.push((i + 1) as u32);
            }
        }

        Self { offsets }
    }

    /// Number of lines in the source.
    pub fn line_count(&self) -> usize {
        self.offsets.len()
    }

    /// Byte offset where the given line (0-indexed) starts.
    ///
    /// Returns `None` if `line` is out of range.
    pub fn line_start(&self, line: usize) -> Option<u32> {
        self.offsets.get(line).copied()
    }

    /// Convert a byte offset to a 0-indexed `(line, column)` pair.
    pub fn offset_to_line_col_0(&self, offset: u32) -> (u32, u32) {
        // Binary search: find the last line whose start is <= offset.
        let line = match self.offsets.binary_search(&offset) {
            Ok(exact) => exact,
            Err(insert_point) => insert_point.saturating_sub(1),
        };
        let col = offset - self.offsets[line];
        (line as u32, col)
    }

    /// Iterator over `(line_index, start_offset)` pairs.
    pub fn iter(&self) -> impl Iterator<Item = (usize, u32)> + '_ {
        self.offsets.iter().enumerate().map(|(i, &o)| (i, o))
    }
}

/// Normalize line endings to Unix style (`\n`).
///
/// Converts `\r\n` (Windows) and bare `\r` (old Mac) to `\n`.
/// Pure `\n` lines are left untouched.
///
/// This is the canonical normalization function for the entire workspace.
/// Language crates should call this (via [`PreprocessedSource::new`]) instead
/// of rolling their own.
pub fn normalize_line_endings(text: &str) -> String {
    // Fast path: if there are no `\r` bytes, the text is already normalized.
    if !text.as_bytes().contains(&b'\r') {
        return text.to_string();
    }

    let mut out = String::with_capacity(text.len());
    let bytes = text.as_bytes();
    let mut i = 0;

    while i < bytes.len() {
        if bytes[i] == b'\r' {
            out.push('\n');
            // Skip the `\n` in a `\r\n` pair.
            if i + 1 < bytes.len() && bytes[i + 1] == b'\n' {
                i += 1;
            }
        } else {
            out.push(bytes[i] as char);
        }
        i += 1;
    }

    out
}

#[cfg(test)]
mod tests {
    use super::*;

    // ---------------------------------------------------------------
    // normalize_line_endings
    // ---------------------------------------------------------------

    #[test]
    fn normalize_unix_unchanged() {
        assert_eq!(normalize_line_endings("a\nb\nc"), "a\nb\nc");
    }

    #[test]
    fn normalize_windows_crlf() {
        assert_eq!(normalize_line_endings("a\r\nb\r\nc"), "a\nb\nc");
    }

    #[test]
    fn normalize_old_mac_cr() {
        assert_eq!(normalize_line_endings("a\rb\rc"), "a\nb\nc");
    }

    #[test]
    fn normalize_mixed() {
        assert_eq!(normalize_line_endings("a\r\nb\rc\nd"), "a\nb\nc\nd");
    }

    #[test]
    fn normalize_empty() {
        assert_eq!(normalize_line_endings(""), "");
    }

    #[test]
    fn normalize_no_newlines() {
        assert_eq!(normalize_line_endings("hello"), "hello");
    }

    #[test]
    fn normalize_trailing_crlf() {
        assert_eq!(normalize_line_endings("a\r\n"), "a\n");
    }

    // ---------------------------------------------------------------
    // LineIndex
    // ---------------------------------------------------------------

    #[test]
    fn line_index_single_line() {
        let idx = LineIndex::new("hello");
        assert_eq!(idx.line_count(), 1);
        assert_eq!(idx.line_start(0), Some(0));
        assert_eq!(idx.line_start(1), None);
    }

    #[test]
    fn line_index_three_lines() {
        //              01234 5 6789ab c defgh
        let text = "line1\nline2\nline3";
        let idx = LineIndex::new(text);

        assert_eq!(idx.line_count(), 3);
        assert_eq!(idx.line_start(0), Some(0));  // "line1" starts at 0
        assert_eq!(idx.line_start(1), Some(6));  // "line2" starts at 6
        assert_eq!(idx.line_start(2), Some(12)); // "line3" starts at 12
    }

    #[test]
    fn line_index_trailing_newline() {
        let text = "a\nb\n";
        let idx = LineIndex::new(text);

        // Three lines: "a", "b", and the empty line after the trailing \n
        assert_eq!(idx.line_count(), 3);
        assert_eq!(idx.line_start(0), Some(0));
        assert_eq!(idx.line_start(1), Some(2));
        assert_eq!(idx.line_start(2), Some(4));
    }

    #[test]
    fn line_index_empty() {
        let idx = LineIndex::new("");
        assert_eq!(idx.line_count(), 1); // Even empty text has "line 0"
        assert_eq!(idx.line_start(0), Some(0));
    }

    // ---------------------------------------------------------------
    // offset_to_line_col
    // ---------------------------------------------------------------

    #[test]
    fn offset_to_line_col_basic() {
        let text = "line1\nline2\nline3";
        let idx = LineIndex::new(text);

        // First char of line 0
        assert_eq!(idx.offset_to_line_col_0(0), (0, 0));
        // Last char of line 0 ('1' at offset 4)
        assert_eq!(idx.offset_to_line_col_0(4), (0, 4));
        // The '\n' at offset 5 — still considered part of line 0
        assert_eq!(idx.offset_to_line_col_0(5), (0, 5));
        // First char of line 1
        assert_eq!(idx.offset_to_line_col_0(6), (1, 0));
        // First char of line 2
        assert_eq!(idx.offset_to_line_col_0(12), (2, 0));
    }

    // ---------------------------------------------------------------
    // PreprocessedSource (integration)
    // ---------------------------------------------------------------

    #[test]
    fn preprocessed_crlf_offsets() {
        // Simulates a Windows-originated file.
        let raw = "//JOB1 JOB\r\n//STEP1 EXEC PGM=TEST\r\n//DD1 DD *\r\n";
        let pp = PreprocessedSource::new(raw);

        // After normalization the text should use \n only.
        assert!(!pp.text.contains('\r'));

        // Line starts should be accurate.
        assert_eq!(pp.line_index.line_start(0), Some(0));
        assert_eq!(pp.line_index.line_start(1), Some(11)); // "//JOB1 JOB\n" = 11 bytes
        assert_eq!(pp.line_index.line_start(2), Some(33)); // + "//STEP1 EXEC PGM=TEST\n" = 22 bytes

        // 1-indexed for diagnostics
        assert_eq!(pp.offset_to_line_col_1(0), (1, 1));
        assert_eq!(pp.offset_to_line_col_1(11), (2, 1));
    }

    #[test]
    fn preprocessed_unix_offsets() {
        let raw = "line one\nline two\nline three";
        let pp = PreprocessedSource::new(raw);

        assert_eq!(pp.line_index.line_start(0), Some(0));
        assert_eq!(pp.line_index.line_start(1), Some(9));
        assert_eq!(pp.line_index.line_start(2), Some(18));

        assert_eq!(pp.offset_to_line_col_1(9), (2, 1));
        assert_eq!(pp.offset_to_line_col_1(18), (3, 1));
    }

    #[test]
    fn preprocessed_from_unix_skips_normalization() {
        let text = "a\nb\nc".to_string();
        let pp = PreprocessedSource::from_unix(text.clone());
        assert_eq!(pp.text, text);
        assert_eq!(pp.line_index.line_count(), 3);
    }

    #[test]
    fn preprocessed_offset_to_line_col_boundary() {
        // Ensure the binary search handles the exact-match case (offset == line start).
        let pp = PreprocessedSource::new("abc\ndef\nghi");
        assert_eq!(pp.offset_to_line_col_0(0), (0, 0)); // 'a'
        assert_eq!(pp.offset_to_line_col_0(3), (0, 3)); // '\n'
        assert_eq!(pp.offset_to_line_col_0(4), (1, 0)); // 'd'
        assert_eq!(pp.offset_to_line_col_0(8), (2, 0)); // 'g'
    }

    #[test]
    fn preprocessed_cobol_with_windows_endings() {
        // Realistic COBOL fixed-format source with \r\n
        let raw = "000100 IDENTIFICATION DIVISION.                                        \r\n\
                   000200 PROGRAM-ID. HELLO.                                                \r\n\
                   000300 PROCEDURE DIVISION.                                                \r\n";
        let pp = PreprocessedSource::new(raw);

        // All \r removed
        assert!(!pp.text.contains('\r'));

        // 3 content lines + 1 trailing empty line
        assert_eq!(pp.line_index.line_count(), 4);

        // Verify line starts are based on actual byte positions
        assert_eq!(pp.line_index.line_start(0), Some(0));
        let line0_len = pp.text.lines().next().unwrap().len() as u32 + 1; // +1 for \n
        assert_eq!(pp.line_index.line_start(1), Some(line0_len));
    }

    #[test]
    fn preprocessed_jcl_inline_data_offsets() {
        // Realistic JCL with inline data — the original failing scenario
        let raw = "//JOB1    JOB CLASS=A\r\n\
                   //STEP1   EXEC PGM=SORT\r\n\
                   //SYSIN   DD *\r\n\
                   SORT FIELDS=(1,10,CH,A)\r\n\
                   /*\r\n";
        let pp = PreprocessedSource::new(raw);

        // Verify we can slice the inline data line correctly
        let line3_start = pp.line_index.line_start(3).unwrap() as usize;
        let line3_text = &pp.text[line3_start..];
        let line3_end = line3_text.find('\n').unwrap_or(line3_text.len());
        assert_eq!(&line3_text[..line3_end], "SORT FIELDS=(1,10,CH,A)");
    }
}
