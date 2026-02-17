//! INREC/OUTREC record reformatting.

/// A field in an OUTREC/INREC specification (BUILD syntax).
#[derive(Debug, Clone)]
pub enum OutrecField {
    /// Copy from input record (position, length).
    Field { position: usize, length: usize },
    /// Insert literal bytes.
    Literal(Vec<u8>),
    /// Insert spaces.
    Spaces(usize),
    /// Insert zeros.
    Zeros(usize),
    /// Sequence number (width in digits, ZD format).
    SeqNum { width: usize },
    /// Record count placeholder (width in digits, filled at end).
    Count { width: usize },
    /// Numeric field with EDIT mask.
    Edit {
        position: usize,
        length: usize,
        data_type: crate::fields::DataType,
        mask: String,
    },
}

impl OutrecField {
    /// Returns the output length of this field.
    pub fn output_length(&self) -> usize {
        match self {
            OutrecField::Field { length, .. } => *length,
            OutrecField::Literal(bytes) => bytes.len(),
            OutrecField::Spaces(n) => *n,
            OutrecField::Zeros(n) => *n,
            OutrecField::SeqNum { width } => *width,
            OutrecField::Count { width } => *width,
            OutrecField::Edit { mask, .. } => mask.len(),
        }
    }

    /// Applies this field to an input record, writing to output.
    ///
    /// For stateful fields (SeqNum, Count), use `apply_stateful` instead.
    pub fn apply(&self, input: &[u8], output: &mut Vec<u8>) {
        self.apply_stateful(input, output, 0, 0);
    }

    /// Applies this field with stateful context (sequence number, record count).
    pub fn apply_stateful(&self, input: &[u8], output: &mut Vec<u8>, seq: u64, count: u64) {
        match self {
            OutrecField::Field { position, length } => {
                let start = position.saturating_sub(1);
                let end = (start + length).min(input.len());

                if start < input.len() {
                    output.extend_from_slice(&input[start..end]);
                    // Pad with spaces if field extends beyond input
                    let written = end - start;
                    if written < *length {
                        output.extend(std::iter::repeat(b' ').take(length - written));
                    }
                } else {
                    // Field is entirely beyond input - fill with spaces
                    output.extend(std::iter::repeat(b' ').take(*length));
                }
            }
            OutrecField::Literal(bytes) => {
                output.extend_from_slice(bytes);
            }
            OutrecField::Spaces(n) => {
                output.extend(std::iter::repeat(b' ').take(*n));
            }
            OutrecField::Zeros(n) => {
                output.extend(std::iter::repeat(b'0').take(*n));
            }
            OutrecField::SeqNum { width } => {
                let formatted = format!("{:0>width$}", seq, width = *width);
                output.extend_from_slice(formatted.as_bytes());
            }
            OutrecField::Count { width } => {
                let formatted = format!("{:0>width$}", count, width = *width);
                output.extend_from_slice(formatted.as_bytes());
            }
            OutrecField::Edit { position, length, data_type, mask } => {
                let start = position.saturating_sub(1);
                let end = (start + length).min(input.len());
                let value = if start < input.len() {
                    crate::fields::extract_numeric(&input[start..end], *data_type)
                } else {
                    0
                };
                let edited = apply_edit_mask(value, mask);
                output.extend_from_slice(edited.as_bytes());
            }
        }
    }
}

/// Complete OUTREC/INREC specification (BUILD syntax).
#[derive(Debug, Clone, Default)]
pub struct OutrecSpec {
    /// Fields in output order (BUILD).
    pub fields: Vec<OutrecField>,
    /// OVERLAY fields (modify in-place at specific positions).
    pub overlay: Vec<OverlaySpec>,
    /// FINDREP find-and-replace specifications.
    pub findrep: Vec<FindRepSpec>,
}

/// An overlay specification — modify a specific position in-place.
#[derive(Debug, Clone)]
pub struct OverlaySpec {
    /// Target output position (1-based).
    pub position: usize,
    /// The field data to write at that position.
    pub field: OutrecField,
}

/// FINDREP specification — find and replace byte patterns.
#[derive(Debug, Clone)]
pub struct FindRepSpec {
    /// The pattern to find.
    pub find: Vec<u8>,
    /// The replacement bytes.
    pub replace: Vec<u8>,
}

impl OutrecSpec {
    /// Creates a new empty specification.
    pub fn new() -> Self {
        Self {
            fields: Vec::new(),
            overlay: Vec::new(),
            findrep: Vec::new(),
        }
    }

    /// Adds a BUILD field.
    pub fn add_field(mut self, field: OutrecField) -> Self {
        self.fields.push(field);
        self
    }

    /// Adds an OVERLAY field.
    pub fn add_overlay(mut self, position: usize, field: OutrecField) -> Self {
        self.overlay.push(OverlaySpec { position, field });
        self
    }

    /// Adds a FINDREP specification.
    pub fn add_findrep(mut self, find: Vec<u8>, replace: Vec<u8>) -> Self {
        self.findrep.push(FindRepSpec { find, replace });
        self
    }

    /// Returns the total output record length (BUILD only).
    pub fn output_length(&self) -> usize {
        self.fields.iter().map(|f| f.output_length()).sum()
    }

    /// Reformats an input record according to the specification.
    ///
    /// Processing order: BUILD (if any fields), then OVERLAY, then FINDREP.
    pub fn reformat(&self, input: &[u8]) -> Vec<u8> {
        self.reformat_stateful(input, 0, 0)
    }

    /// Reformats with stateful context (sequence number, record count).
    pub fn reformat_stateful(&self, input: &[u8], seq: u64, count: u64) -> Vec<u8> {
        // Phase 1: BUILD — if fields are specified, construct new record
        let mut output = if !self.fields.is_empty() {
            let mut buf = Vec::with_capacity(self.output_length());
            for field in &self.fields {
                field.apply_stateful(input, &mut buf, seq, count);
            }
            buf
        } else {
            input.to_vec()
        };

        // Phase 2: OVERLAY — modify specific positions in-place
        for overlay in &self.overlay {
            let pos = overlay.position.saturating_sub(1);
            let mut overlay_data = Vec::new();
            overlay.field.apply_stateful(&output, &mut overlay_data, seq, count);
            let needed = pos + overlay_data.len();
            if needed > output.len() {
                output.resize(needed, b' ');
            }
            output[pos..pos + overlay_data.len()].copy_from_slice(&overlay_data);
        }

        // Phase 3: FINDREP — find and replace patterns
        for fr in &self.findrep {
            output = find_and_replace(&output, &fr.find, &fr.replace);
        }

        output
    }

    /// Returns true if this has any processing to do.
    pub fn is_valid(&self) -> bool {
        !self.fields.is_empty() || !self.overlay.is_empty() || !self.findrep.is_empty()
    }
}

/// Apply a DFSORT EDIT mask to a numeric value.
///
/// EDIT mask characters:
/// - `I` — insert a digit from the value (suppress leading zeros)
/// - `T` — insert a digit (always shown)
/// - `.` `,` `/` `-` — insert literal separator
///
/// Example: EDIT=(IIIIIII.TT) with value 12345 → "   123.45"
fn apply_edit_mask(value: i64, mask: &str) -> String {
    let is_negative = value < 0;
    let abs_val = value.unsigned_abs();
    let val_str = format!("{}", abs_val);

    // Count digit positions in mask (I and T characters)
    let digit_positions: usize = mask.chars().filter(|c| *c == 'I' || *c == 'T').count();

    // Right-justify value digits
    let padding = digit_positions.saturating_sub(val_str.len());
    let mut digits: Vec<char> = vec!['0'; padding];
    digits.extend(val_str.chars());

    let mut result = String::new();
    let mut digit_idx = 0;
    let mut found_nonzero = false;

    for mc in mask.chars() {
        match mc {
            'I' => {
                let d = digits.get(digit_idx).copied().unwrap_or('0');
                digit_idx += 1;
                if d != '0' {
                    found_nonzero = true;
                }
                if found_nonzero {
                    result.push(d);
                } else {
                    result.push(' ');
                }
            }
            'T' => {
                let d = digits.get(digit_idx).copied().unwrap_or('0');
                digit_idx += 1;
                found_nonzero = true;
                result.push(d);
            }
            other => {
                // Separator — only show if we have digits on either side
                if found_nonzero {
                    result.push(other);
                } else {
                    // Check if any remaining digits are non-zero
                    let remaining_nonzero = digits[digit_idx..].iter().any(|&c| c != '0');
                    if remaining_nonzero {
                        result.push(other);
                    } else {
                        result.push(' ');
                    }
                }
            }
        }
    }

    if is_negative {
        result.push('-');
    }

    result
}

/// Find and replace all occurrences of `find` in `data` with `replace`.
fn find_and_replace(data: &[u8], find: &[u8], replace: &[u8]) -> Vec<u8> {
    if find.is_empty() {
        return data.to_vec();
    }

    let mut result = Vec::new();
    let mut i = 0;

    while i < data.len() {
        if i + find.len() <= data.len() && &data[i..i + find.len()] == find {
            result.extend_from_slice(replace);
            i += find.len();
        } else {
            result.push(data[i]);
            i += 1;
        }
    }

    result
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_field_copy() {
        let field = OutrecField::Field { position: 1, length: 5 };
        let mut output = Vec::new();
        field.apply(b"Hello World", &mut output);
        assert_eq!(output, b"Hello");
    }

    #[test]
    fn test_field_copy_middle() {
        let field = OutrecField::Field { position: 7, length: 5 };
        let mut output = Vec::new();
        field.apply(b"Hello World", &mut output);
        assert_eq!(output, b"World");
    }

    #[test]
    fn test_field_copy_pad() {
        let field = OutrecField::Field { position: 10, length: 5 };
        let mut output = Vec::new();
        field.apply(b"Hello World", &mut output);
        assert_eq!(output, b"ld   "); // "ld" + 3 spaces
    }

    #[test]
    fn test_literal() {
        let field = OutrecField::Literal(b"***".to_vec());
        let mut output = Vec::new();
        field.apply(b"anything", &mut output);
        assert_eq!(output, b"***");
    }

    #[test]
    fn test_spaces() {
        let field = OutrecField::Spaces(3);
        let mut output = Vec::new();
        field.apply(b"anything", &mut output);
        assert_eq!(output, b"   ");
    }

    #[test]
    fn test_outrec_spec() {
        let spec = OutrecSpec::new()
            .add_field(OutrecField::Field { position: 1, length: 5 })
            .add_field(OutrecField::Literal(b"-".to_vec()))
            .add_field(OutrecField::Field { position: 7, length: 5 });

        let result = spec.reformat(b"Hello World");
        assert_eq!(result, b"Hello-World");
    }

    #[test]
    fn test_outrec_reorder() {
        // Take positions 11-15, then 1-5
        let spec = OutrecSpec::new()
            .add_field(OutrecField::Field { position: 7, length: 5 })
            .add_field(OutrecField::Field { position: 1, length: 5 });

        let result = spec.reformat(b"Hello World");
        assert_eq!(result, b"WorldHello");
    }

    // -----------------------------------------------------------------------
    // Epic 804: BUILD/OVERLAY/FINDREP Tests
    // -----------------------------------------------------------------------

    #[test]
    fn test_seqnum_field() {
        let field = OutrecField::SeqNum { width: 6 };
        let mut output = Vec::new();
        field.apply_stateful(b"", &mut output, 42, 0);
        assert_eq!(output, b"000042");
    }

    #[test]
    fn test_count_field() {
        let field = OutrecField::Count { width: 4 };
        let mut output = Vec::new();
        field.apply_stateful(b"", &mut output, 0, 100);
        assert_eq!(output, b"0100");
    }

    #[test]
    fn test_edit_mask_basic() {
        // EDIT=(IIIIIIII.TT) has 8 I-digits + dot + 2 T-digits = 10 digit slots
        // 12345 → digits 0000012345 → I slots suppress leading zeros
        let result = apply_edit_mask(12345, "IIIIIIII.TT");
        assert_eq!(result, "     123.45");
    }

    #[test]
    fn test_edit_mask_zero() {
        // III.TT has 3 I-digits + 2 T-digits = 5 digit slots
        // Value 0 → digits 00000 → all I's suppressed, T's show zeros
        let result = apply_edit_mask(0, "III.TT");
        assert_eq!(result, "    00");
    }

    #[test]
    fn test_edit_mask_negative() {
        // IIII.TT has 4 I-digits + 2 T-digits = 6 digit slots
        // -42 → digits 000042 → I's: "   " (suppress), separator shown, T's: "42", then '-'
        let result = apply_edit_mask(-42, "IIII.TT");
        assert_eq!(result, "    .42-");
    }

    #[test]
    fn test_edit_field_in_build() {
        let spec = OutrecSpec::new()
            .add_field(OutrecField::Field { position: 1, length: 3 })
            .add_field(OutrecField::Edit {
                position: 4,
                length: 4,
                data_type: crate::fields::DataType::Binary,
                mask: "IIII.TT".to_string(),
            });

        // Record: "ABC" + binary 12345 (4 bytes)
        let mut record = b"ABC".to_vec();
        record.extend_from_slice(&12345i32.to_be_bytes());

        let result = spec.reformat(&record);
        assert_eq!(&result[0..3], b"ABC");
        // Binary 12345 → EDIT "IIII.TT" → " 123.45"
        assert_eq!(std::str::from_utf8(&result[3..]).unwrap(), " 123.45");
    }

    #[test]
    fn test_overlay_literal() {
        let spec = OutrecSpec::new()
            .add_overlay(1, OutrecField::Literal(b"X".to_vec()))
            .add_overlay(5, OutrecField::Literal(b"YZ".to_vec()));

        let result = spec.reformat(b"ABCDEFGH");
        assert_eq!(&result[0..1], b"X");
        assert_eq!(&result[1..4], b"BCD");
        assert_eq!(&result[4..6], b"YZ");
        assert_eq!(&result[6..8], b"GH");
    }

    #[test]
    fn test_overlay_with_seqnum() {
        let spec = OutrecSpec::new()
            .add_overlay(6, OutrecField::SeqNum { width: 4 });

        let result = spec.reformat_stateful(b"HELLO", 7, 0);
        assert_eq!(&result[0..5], b"HELLO");
        assert_eq!(&result[5..9], b"0007");
    }

    #[test]
    fn test_overlay_extends_record() {
        let spec = OutrecSpec::new()
            .add_overlay(8, OutrecField::Literal(b"END".to_vec()));

        let result = spec.reformat(b"SHORT");
        assert_eq!(result.len(), 10);
        assert_eq!(&result[0..5], b"SHORT");
        assert_eq!(&result[7..10], b"END");
    }

    #[test]
    fn test_findrep_single() {
        let spec = OutrecSpec::new()
            .add_findrep(b"OLD".to_vec(), b"NEW".to_vec());

        let result = spec.reformat(b"THE OLD WAY IS OLD");
        assert_eq!(result, b"THE NEW WAY IS NEW");
    }

    #[test]
    fn test_findrep_different_length() {
        let spec = OutrecSpec::new()
            .add_findrep(b"ABC".to_vec(), b"XY".to_vec());

        let result = spec.reformat(b"ABCDEF");
        assert_eq!(result, b"XYDEF");
    }

    #[test]
    fn test_findrep_no_match() {
        let spec = OutrecSpec::new()
            .add_findrep(b"ZZZ".to_vec(), b"AAA".to_vec());

        let result = spec.reformat(b"HELLO WORLD");
        assert_eq!(result, b"HELLO WORLD");
    }

    #[test]
    fn test_findrep_multiple_patterns() {
        let spec = OutrecSpec::new()
            .add_findrep(b"FOO".to_vec(), b"BAR".to_vec())
            .add_findrep(b"BAZ".to_vec(), b"QUX".to_vec());

        let result = spec.reformat(b"FOO AND BAZ");
        assert_eq!(result, b"BAR AND QUX");
    }

    #[test]
    fn test_build_then_overlay_then_findrep() {
        let spec = OutrecSpec::new()
            .add_field(OutrecField::Field { position: 1, length: 10 })
            .add_overlay(1, OutrecField::Literal(b"X".to_vec()))
            .add_findrep(b"World".to_vec(), b"Earth".to_vec());

        let result = spec.reformat(b"Hello World");
        // BUILD: "Hello Worl" (first 10)
        // OVERLAY pos 1: "Xello Worl"
        // FINDREP: no "World" match (it's "Worl"), so "Xello Worl"
        assert_eq!(result, b"Xello Worl");
    }

    #[test]
    fn test_build_with_seqnum_stateful() {
        let spec = OutrecSpec::new()
            .add_field(OutrecField::Field { position: 1, length: 5 })
            .add_field(OutrecField::SeqNum { width: 4 });

        let r1 = spec.reformat_stateful(b"HELLO", 1, 0);
        assert_eq!(r1, b"HELLO0001");

        let r2 = spec.reformat_stateful(b"WORLD", 2, 0);
        assert_eq!(r2, b"WORLD0002");
    }

    #[test]
    fn test_is_valid() {
        assert!(!OutrecSpec::new().is_valid());

        assert!(OutrecSpec::new()
            .add_field(OutrecField::Literal(b"X".to_vec()))
            .is_valid());

        assert!(OutrecSpec::new()
            .add_overlay(1, OutrecField::Literal(b"X".to_vec()))
            .is_valid());

        assert!(OutrecSpec::new()
            .add_findrep(b"A".to_vec(), b"B".to_vec())
            .is_valid());
    }

    #[test]
    fn test_find_and_replace_helper() {
        assert_eq!(find_and_replace(b"AABAA", b"AA", b"X"), b"XBX");
        assert_eq!(find_and_replace(b"ABC", b"", b"X"), b"ABC");
        assert_eq!(find_and_replace(b"", b"A", b"B"), b"");
    }
}
