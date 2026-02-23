//! DFSORT symbols and built-in functions.
//!
//! Provides convenience constructors for DFSORT symbol fields:
//! - `SEQNUM` — sequence number
//! - `DATE1` through `DATE4` — current date formats
//! - `COUNT` — record count
//!
//! These map to [`OutrecField`] variants for use in OUTREC/INREC/BUILD specs.

use crate::reformat::OutrecField;

/// Create a SEQNUM field (sequence number in ZD format).
///
/// `OUTREC=(1,20,SEQNUM,8,ZD)` → `seqnum(8)`
pub fn seqnum(width: usize) -> OutrecField {
    OutrecField::SeqNum { width }
}

/// Create a DATE1 field (MM/DD/YYYY with specified separator).
///
/// `DATE1=(MD4/)` → `date1('/')`
pub fn date1(separator: char) -> OutrecField {
    OutrecField::Date1 { separator }
}

/// Create a DATE2 field (DD/MM/YYYY with specified separator).
///
/// `DATE2=(DM4/)` → `date2('/')`
pub fn date2(separator: char) -> OutrecField {
    OutrecField::Date2 { separator }
}

/// Create a DATE3 field (YYYY/MM/DD with specified separator).
///
/// `DATE3=(Y4MD/)` → `date3('/')`
pub fn date3(separator: char) -> OutrecField {
    OutrecField::Date3 { separator }
}

/// Create a DATE4 field (YYYYMMDD, no separator).
///
/// `DATE4` → `date4()`
pub fn date4() -> OutrecField {
    OutrecField::Date4
}

/// Create a COUNT field (record count, formatted to width).
///
/// `COUNT=(width,ZD)` → `count(width)`
pub fn count(width: usize) -> OutrecField {
    OutrecField::Count { width }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_seqnum_symbol() {
        let field = seqnum(8);
        assert_eq!(field.output_length(), 8);
        let mut output = Vec::new();
        field.apply_stateful(b"", &mut output, 42, 0);
        assert_eq!(output, b"00000042");
    }

    #[test]
    fn test_date1_symbol() {
        let field = date1('/');
        assert_eq!(field.output_length(), 10);
        let mut output = Vec::new();
        field.apply(b"", &mut output);
        let text = String::from_utf8(output).unwrap();
        // Format: MM/DD/YYYY
        assert_eq!(text.len(), 10);
        assert_eq!(text.as_bytes()[2], b'/');
        assert_eq!(text.as_bytes()[5], b'/');
    }

    #[test]
    fn test_date2_symbol() {
        let field = date2('-');
        let mut output = Vec::new();
        field.apply(b"", &mut output);
        let text = String::from_utf8(output).unwrap();
        // Format: DD-MM-YYYY
        assert_eq!(text.len(), 10);
        assert_eq!(text.as_bytes()[2], b'-');
        assert_eq!(text.as_bytes()[5], b'-');
    }

    #[test]
    fn test_date3_symbol() {
        let field = date3('.');
        let mut output = Vec::new();
        field.apply(b"", &mut output);
        let text = String::from_utf8(output).unwrap();
        // Format: YYYY.MM.DD
        assert_eq!(text.len(), 10);
        assert_eq!(text.as_bytes()[4], b'.');
        assert_eq!(text.as_bytes()[7], b'.');
    }

    #[test]
    fn test_date4_symbol() {
        let field = date4();
        assert_eq!(field.output_length(), 8);
        let mut output = Vec::new();
        field.apply(b"", &mut output);
        let text = String::from_utf8(output).unwrap();
        // Format: YYYYMMDD — all digits, no separators
        assert_eq!(text.len(), 8);
        assert!(text.chars().all(|c| c.is_ascii_digit()));
    }

    #[test]
    fn test_count_symbol() {
        let field = count(6);
        assert_eq!(field.output_length(), 6);
        let mut output = Vec::new();
        field.apply_stateful(b"", &mut output, 0, 100);
        assert_eq!(output, b"000100");
    }

    #[test]
    fn test_date_in_outrec_spec() {
        use crate::reformat::OutrecSpec;
        let spec = OutrecSpec::new()
            .add_field(OutrecField::Field { position: 1, length: 5 })
            .add_field(date1('/'));

        let result = spec.reformat(b"HELLO");
        // 5 bytes of data + 10 bytes of date
        assert_eq!(result.len(), 15);
        assert_eq!(&result[0..5], b"HELLO");
        // Date portion is MM/DD/YYYY
        assert_eq!(result[7], b'/');
        assert_eq!(result[10], b'/');
    }
}
