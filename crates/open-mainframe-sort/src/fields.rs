//! Sort field definitions and specifications.

use std::cmp::Ordering;

/// Data types for sort fields.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DataType {
    /// Character (EBCDIC/ASCII string comparison).
    Character,
    /// Zoned decimal (signed numeric in character form).
    ZonedDecimal,
    /// Packed decimal (BCD format).
    PackedDecimal,
    /// Binary integer (signed).
    Binary,
    /// Fixed-point integer (unsigned binary).
    FixedPoint,
}

impl DataType {
    /// Parse data type from DFSORT format code.
    pub fn from_code(code: &str) -> Option<Self> {
        match code.to_uppercase().as_str() {
            "CH" | "A" => Some(DataType::Character),
            "ZD" => Some(DataType::ZonedDecimal),
            "PD" => Some(DataType::PackedDecimal),
            "BI" => Some(DataType::Binary),
            "FI" => Some(DataType::FixedPoint),
            _ => None,
        }
    }

    /// Returns the DFSORT format code.
    pub fn code(&self) -> &'static str {
        match self {
            DataType::Character => "CH",
            DataType::ZonedDecimal => "ZD",
            DataType::PackedDecimal => "PD",
            DataType::Binary => "BI",
            DataType::FixedPoint => "FI",
        }
    }
}

/// Sort order for a field.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SortOrder {
    /// Ascending order (A).
    Ascending,
    /// Descending order (D).
    Descending,
}

impl SortOrder {
    /// Parse sort order from DFSORT code.
    pub fn from_code(code: &str) -> Option<Self> {
        match code.to_uppercase().as_str() {
            "A" => Some(SortOrder::Ascending),
            "D" => Some(SortOrder::Descending),
            _ => None,
        }
    }

    /// Returns the DFSORT code.
    pub fn code(&self) -> &'static str {
        match self {
            SortOrder::Ascending => "A",
            SortOrder::Descending => "D",
        }
    }
}

/// A single sort field specification.
#[derive(Debug, Clone)]
pub struct SortField {
    /// Starting position (1-based, like DFSORT).
    pub position: usize,
    /// Length in bytes.
    pub length: usize,
    /// Data type for comparison.
    pub data_type: DataType,
    /// Sort order.
    pub order: SortOrder,
}

impl SortField {
    /// Creates a new sort field.
    pub fn new(position: usize, length: usize, data_type: DataType, order: SortOrder) -> Self {
        Self {
            position,
            length,
            data_type,
            order,
        }
    }

    /// Extracts the field value from a record.
    /// Position is 1-based (DFSORT convention).
    pub fn extract<'a>(&self, record: &'a [u8]) -> Option<&'a [u8]> {
        let start = self.position.checked_sub(1)?;
        let end = start.checked_add(self.length)?;
        if end <= record.len() {
            Some(&record[start..end])
        } else {
            None
        }
    }

    /// Compares two field values according to data type.
    pub fn compare(&self, a: &[u8], b: &[u8]) -> Ordering {
        let cmp = match self.data_type {
            DataType::Character => a.cmp(b),
            DataType::ZonedDecimal => compare_zoned_decimal(a, b),
            DataType::PackedDecimal => compare_packed_decimal(a, b),
            DataType::Binary | DataType::FixedPoint => compare_binary(a, b),
        };

        match self.order {
            SortOrder::Ascending => cmp,
            SortOrder::Descending => cmp.reverse(),
        }
    }
}

/// Extract numeric value from bytes according to data type.
pub(crate) fn extract_numeric(data: &[u8], data_type: DataType) -> i64 {
    match data_type {
        DataType::Character => {
            // Interpret character digits as numeric
            let s = std::str::from_utf8(data).unwrap_or("0");
            s.trim().parse::<i64>().unwrap_or(0)
        }
        DataType::ZonedDecimal => parse_zoned_decimal(data),
        DataType::PackedDecimal => parse_packed_decimal(data),
        DataType::Binary | DataType::FixedPoint => parse_binary(data),
    }
}

/// Pack a numeric value into bytes according to data type.
pub(crate) fn pack_numeric(value: i64, target: &mut [u8], data_type: DataType) {
    match data_type {
        DataType::Character => {
            let s = format!("{}", value);
            let bytes = s.as_bytes();
            // Right-justify in field
            let start = target.len().saturating_sub(bytes.len());
            for b in target.iter_mut().take(start) {
                *b = b'0';
            }
            for (i, &byte) in bytes.iter().enumerate() {
                if start + i < target.len() {
                    target[start + i] = byte;
                }
            }
        }
        DataType::ZonedDecimal => pack_zoned_decimal(value, target),
        DataType::PackedDecimal => pack_packed_decimal(value, target),
        DataType::Binary | DataType::FixedPoint => pack_binary(value, target),
    }
}

/// Pack a zoned decimal value into bytes.
fn pack_zoned_decimal(value: i64, target: &mut [u8]) {
    let is_negative = value < 0;
    let abs_val = value.unsigned_abs();

    let mut digits = Vec::new();
    let mut v = abs_val;
    if v == 0 {
        digits.push(0u8);
    } else {
        while v > 0 {
            digits.push((v % 10) as u8);
            v /= 10;
        }
    }
    digits.reverse();

    let len = target.len();
    let start = len.saturating_sub(digits.len());
    for (i, byte) in target.iter_mut().enumerate() {
        let digit = if i >= start {
            digits[i - start]
        } else {
            0
        };
        if i == len - 1 {
            let sign = if is_negative { 0xD0 } else { 0xF0 };
            *byte = sign | digit;
        } else {
            *byte = 0xF0 | digit;
        }
    }
}

/// Pack a packed decimal value into bytes.
fn pack_packed_decimal(value: i64, target: &mut [u8]) {
    if target.is_empty() {
        return;
    }
    let is_negative = value < 0;
    let abs_val = value.unsigned_abs();

    // Total digit capacity: (len * 2) - 1 (last nibble is sign)
    let total_digits = target.len() * 2 - 1;
    let mut digits = vec![0u8; total_digits];
    let mut v = abs_val;
    for d in digits.iter_mut().rev() {
        *d = (v % 10) as u8;
        v /= 10;
    }

    let mut digit_idx = 0;
    let last_idx = target.len() - 1;
    for (i, byte) in target.iter_mut().enumerate() {
        if i < last_idx {
            let high = digits.get(digit_idx).copied().unwrap_or(0);
            let low = digits.get(digit_idx + 1).copied().unwrap_or(0);
            *byte = (high << 4) | low;
            digit_idx += 2;
        } else {
            let high = digits.get(digit_idx).copied().unwrap_or(0);
            let sign = if is_negative { 0x0D } else { 0x0C };
            *byte = (high << 4) | sign;
        }
    }
}

/// Pack a binary value into bytes (big-endian).
fn pack_binary(value: i64, target: &mut [u8]) {
    let bytes = value.to_be_bytes();
    let start = 8usize.saturating_sub(target.len());
    for (i, b) in target.iter_mut().enumerate() {
        *b = bytes[start + i];
    }
}

/// Compare zoned decimal values.
fn compare_zoned_decimal(a: &[u8], b: &[u8]) -> Ordering {
    let a_val = parse_zoned_decimal(a);
    let b_val = parse_zoned_decimal(b);
    a_val.cmp(&b_val)
}

/// Parse a zoned decimal value.
fn parse_zoned_decimal(data: &[u8]) -> i64 {
    if data.is_empty() {
        return 0;
    }

    let mut value: i64 = 0;
    let mut negative = false;

    for (i, &byte) in data.iter().enumerate() {
        // Extract digit (low nibble)
        let digit = (byte & 0x0F) as i64;

        // Check sign in last byte's high nibble
        if i == data.len() - 1 {
            let sign = byte >> 4;
            // 0xD = negative, 0xB = negative, others = positive
            negative = sign == 0x0D || sign == 0x0B;
        }

        value = value * 10 + digit;
    }

    if negative {
        -value
    } else {
        value
    }
}

/// Compare packed decimal values.
fn compare_packed_decimal(a: &[u8], b: &[u8]) -> Ordering {
    let a_val = parse_packed_decimal(a);
    let b_val = parse_packed_decimal(b);
    a_val.cmp(&b_val)
}

/// Parse a packed decimal value.
fn parse_packed_decimal(data: &[u8]) -> i64 {
    if data.is_empty() {
        return 0;
    }

    let mut value: i64 = 0;

    for (i, &byte) in data.iter().enumerate() {
        if i == data.len() - 1 {
            // Last byte: high nibble is last digit, low nibble is sign
            let digit = (byte >> 4) as i64;
            value = value * 10 + digit;

            let sign = byte & 0x0F;
            // 0x0D = negative, 0x0B = negative
            if sign == 0x0D || sign == 0x0B {
                value = -value;
            }
        } else {
            // Other bytes: two digits per byte
            let high = (byte >> 4) as i64;
            let low = (byte & 0x0F) as i64;
            value = value * 100 + high * 10 + low;
        }
    }

    value
}

/// Compare binary values (big-endian signed).
fn compare_binary(a: &[u8], b: &[u8]) -> Ordering {
    let a_val = parse_binary(a);
    let b_val = parse_binary(b);
    a_val.cmp(&b_val)
}

/// Parse a binary value (big-endian signed).
fn parse_binary(data: &[u8]) -> i64 {
    if data.is_empty() {
        return 0;
    }

    // Determine if negative (sign bit set)
    let negative = data[0] & 0x80 != 0;

    let mut value: i64 = 0;
    for &byte in data {
        value = (value << 8) | (byte as i64);
    }

    // Sign extend if negative
    if negative {
        let bits = data.len() * 8;
        let mask = !((1i64 << bits) - 1);
        value |= mask;
    }

    value
}

/// Complete sort specification with multiple fields.
#[derive(Debug, Clone, Default)]
pub struct SortSpec {
    /// Sort fields in priority order.
    pub fields: Vec<SortField>,
}

impl SortSpec {
    /// Creates an empty sort specification.
    pub fn new() -> Self {
        Self { fields: Vec::new() }
    }

    /// Adds a sort field.
    pub fn add_field(mut self, field: SortField) -> Self {
        self.fields.push(field);
        self
    }

    /// Compares two records according to all sort fields.
    pub fn compare(&self, a: &[u8], b: &[u8]) -> Ordering {
        for field in &self.fields {
            let a_val = field.extract(a);
            let b_val = field.extract(b);

            match (a_val, b_val) {
                (Some(av), Some(bv)) => {
                    let cmp = field.compare(av, bv);
                    if cmp != Ordering::Equal {
                        return cmp;
                    }
                }
                (None, Some(_)) => return Ordering::Less,
                (Some(_), None) => return Ordering::Greater,
                (None, None) => continue,
            }
        }
        Ordering::Equal
    }

    /// Returns true if this is a valid (non-empty) sort specification.
    pub fn is_valid(&self) -> bool {
        !self.fields.is_empty()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_data_type_from_code() {
        assert_eq!(DataType::from_code("CH"), Some(DataType::Character));
        assert_eq!(DataType::from_code("ch"), Some(DataType::Character));
        assert_eq!(DataType::from_code("ZD"), Some(DataType::ZonedDecimal));
        assert_eq!(DataType::from_code("PD"), Some(DataType::PackedDecimal));
        assert_eq!(DataType::from_code("BI"), Some(DataType::Binary));
        assert_eq!(DataType::from_code("FI"), Some(DataType::FixedPoint));
        assert_eq!(DataType::from_code("XX"), None);
    }

    #[test]
    fn test_sort_order_from_code() {
        assert_eq!(SortOrder::from_code("A"), Some(SortOrder::Ascending));
        assert_eq!(SortOrder::from_code("D"), Some(SortOrder::Descending));
        assert_eq!(SortOrder::from_code("X"), None);
    }

    #[test]
    fn test_field_extract() {
        let field = SortField::new(1, 5, DataType::Character, SortOrder::Ascending);
        let record = b"Hello World";
        assert_eq!(field.extract(record), Some(&b"Hello"[..]));

        let field2 = SortField::new(7, 5, DataType::Character, SortOrder::Ascending);
        assert_eq!(field2.extract(record), Some(&b"World"[..]));

        // Out of bounds
        let field3 = SortField::new(10, 5, DataType::Character, SortOrder::Ascending);
        assert_eq!(field3.extract(record), None);
    }

    #[test]
    fn test_character_compare() {
        let field = SortField::new(1, 5, DataType::Character, SortOrder::Ascending);
        assert_eq!(field.compare(b"AAAAA", b"BBBBB"), Ordering::Less);
        assert_eq!(field.compare(b"BBBBB", b"AAAAA"), Ordering::Greater);
        assert_eq!(field.compare(b"AAAAA", b"AAAAA"), Ordering::Equal);

        let field_desc = SortField::new(1, 5, DataType::Character, SortOrder::Descending);
        assert_eq!(field_desc.compare(b"AAAAA", b"BBBBB"), Ordering::Greater);
    }

    #[test]
    fn test_zoned_decimal_parse() {
        // "123" with positive sign (0xF in last byte)
        let data = [0xF1, 0xF2, 0xF3]; // 1, 2, 3+
        assert_eq!(parse_zoned_decimal(&data), 123);

        // "123" with negative sign (0xD in last byte)
        let data_neg = [0xF1, 0xF2, 0xD3]; // 1, 2, 3-
        assert_eq!(parse_zoned_decimal(&data_neg), -123);
    }

    #[test]
    fn test_packed_decimal_parse() {
        // 123+ = 0x12 0x3C
        let data = [0x12, 0x3C];
        assert_eq!(parse_packed_decimal(&data), 123);

        // 123- = 0x12 0x3D
        let data_neg = [0x12, 0x3D];
        assert_eq!(parse_packed_decimal(&data_neg), -123);
    }

    #[test]
    fn test_binary_parse() {
        // 256 in big-endian
        let data = [0x01, 0x00];
        assert_eq!(parse_binary(&data), 256);

        // -1 in 2-byte signed
        let data_neg = [0xFF, 0xFF];
        assert_eq!(parse_binary(&data_neg), -1);
    }

    #[test]
    fn test_extract_pack_roundtrip_packed_decimal() {
        let mut buf = [0u8; 3];
        pack_packed_decimal(12345, &mut buf);
        assert_eq!(parse_packed_decimal(&buf), 12345);

        pack_packed_decimal(-99, &mut buf);
        assert_eq!(parse_packed_decimal(&buf), -99);
    }

    #[test]
    fn test_extract_pack_roundtrip_zoned_decimal() {
        let mut buf = [0u8; 4];
        pack_zoned_decimal(1234, &mut buf);
        assert_eq!(parse_zoned_decimal(&buf), 1234);

        pack_zoned_decimal(-56, &mut buf);
        assert_eq!(parse_zoned_decimal(&buf), -56);
    }

    #[test]
    fn test_extract_pack_roundtrip_binary() {
        let mut buf = [0u8; 4];
        pack_binary(42, &mut buf);
        assert_eq!(parse_binary(&buf), 42);

        pack_binary(-1, &mut buf);
        assert_eq!(parse_binary(&buf), -1);
    }

    #[test]
    fn test_extract_numeric_dispatch() {
        // Packed decimal
        let pd = [0x12, 0x3C]; // 123+
        assert_eq!(extract_numeric(&pd, DataType::PackedDecimal), 123);

        // Binary
        let bi = [0x00, 0x00, 0x01, 0x00]; // 256
        assert_eq!(extract_numeric(&bi, DataType::Binary), 256);
    }

    #[test]
    fn test_sort_spec_compare() {
        let spec = SortSpec::new()
            .add_field(SortField::new(1, 5, DataType::Character, SortOrder::Ascending))
            .add_field(SortField::new(6, 5, DataType::Character, SortOrder::Ascending));

        let a = b"AAAAA11111";
        let b = b"AAAAA22222";
        let c = b"BBBBB00000";

        assert_eq!(spec.compare(a, b), Ordering::Less);
        assert_eq!(spec.compare(a, c), Ordering::Less);
        assert_eq!(spec.compare(c, a), Ordering::Greater);
    }
}
