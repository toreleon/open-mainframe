//! COBOL type system definitions.
//!
//! COBOL has a complex type system with categories that determine
//! what operations are valid and how data is stored.

use crate::ast::{PictureCategory, Usage};

/// Category of a COBOL type for semantic analysis.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TypeCategory {
    /// Group item (no PICTURE, contains subordinates).
    Group,
    /// Alphabetic (PIC A).
    Alphabetic,
    /// Alphanumeric (PIC X).
    Alphanumeric,
    /// Alphanumeric-edited.
    AlphanumericEdited,
    /// Numeric (PIC 9).
    Numeric,
    /// Numeric-edited.
    NumericEdited,
    /// Index data item.
    Index,
    /// Pointer data item.
    Pointer,
    /// Condition name (level 88).
    ConditionName,
}

impl TypeCategory {
    /// Check if this type is numeric (can participate in arithmetic).
    pub fn is_numeric(&self) -> bool {
        matches!(self, TypeCategory::Numeric | TypeCategory::NumericEdited)
    }

    /// Check if this type is alphanumeric.
    pub fn is_alphanumeric(&self) -> bool {
        matches!(
            self,
            TypeCategory::Alphanumeric
                | TypeCategory::AlphanumericEdited
                | TypeCategory::Alphabetic
        )
    }

    /// Check if this category can receive a MOVE from another category.
    pub fn can_receive_move_from(&self, source: TypeCategory) -> bool {
        match self {
            TypeCategory::Group => true, // Groups accept anything
            TypeCategory::Alphabetic => source.is_alphanumeric(),
            TypeCategory::Alphanumeric | TypeCategory::AlphanumericEdited => true,
            TypeCategory::Numeric => {
                // Numeric can receive numeric or alphanumeric (with conversion)
                source.is_numeric() || source.is_alphanumeric()
            }
            TypeCategory::NumericEdited => source.is_numeric() || source.is_alphanumeric(),
            TypeCategory::Index | TypeCategory::Pointer => {
                *self == source || source == TypeCategory::Numeric
            }
            TypeCategory::ConditionName => false, // Cannot MOVE to condition names
        }
    }
}

impl From<PictureCategory> for TypeCategory {
    fn from(pic: PictureCategory) -> Self {
        match pic {
            PictureCategory::Alphabetic => TypeCategory::Alphabetic,
            PictureCategory::Alphanumeric => TypeCategory::Alphanumeric,
            PictureCategory::AlphanumericEdited => TypeCategory::AlphanumericEdited,
            PictureCategory::Numeric => TypeCategory::Numeric,
            PictureCategory::NumericEdited => TypeCategory::NumericEdited,
            PictureCategory::Utf8 => TypeCategory::Alphanumeric,
            PictureCategory::National => TypeCategory::Alphanumeric,
        }
    }
}

/// Complete type information for a COBOL data item.
#[derive(Debug, Clone, PartialEq)]
pub struct CobolType {
    /// The category of this type.
    pub category: TypeCategory,
    /// Size in bytes.
    pub size: u32,
    /// Decimal positions (for numeric types).
    pub decimal_positions: u32,
    /// Storage usage (DISPLAY, BINARY, PACKED-DECIMAL, etc.).
    pub usage: Usage,
    /// Whether the item is signed.
    pub is_signed: bool,
    /// Whether the item is a group (contains subordinates).
    pub is_group: bool,
    /// Number of occurrences (1 for non-tables).
    pub occurs: u32,
}

impl Default for CobolType {
    fn default() -> Self {
        Self {
            category: TypeCategory::Alphanumeric,
            size: 1,
            decimal_positions: 0,
            usage: Usage::Display,
            is_signed: false,
            is_group: false,
            occurs: 1,
        }
    }
}

impl CobolType {
    /// Create a new group type.
    pub fn group(size: u32) -> Self {
        Self {
            category: TypeCategory::Group,
            size,
            decimal_positions: 0,
            usage: Usage::Display,
            is_signed: false,
            is_group: true,
            occurs: 1,
        }
    }

    /// Create a new alphanumeric type.
    pub fn alphanumeric(size: u32) -> Self {
        Self {
            category: TypeCategory::Alphanumeric,
            size,
            decimal_positions: 0,
            usage: Usage::Display,
            is_signed: false,
            is_group: false,
            occurs: 1,
        }
    }

    /// Create a new numeric type.
    pub fn numeric(size: u32, decimal_positions: u32, is_signed: bool, usage: Usage) -> Self {
        Self {
            category: TypeCategory::Numeric,
            size,
            decimal_positions,
            usage,
            is_signed,
            is_group: false,
            occurs: 1,
        }
    }

    /// Create an index type.
    pub fn index() -> Self {
        Self {
            category: TypeCategory::Index,
            size: 4, // Typically 4 bytes
            decimal_positions: 0,
            usage: Usage::Index,
            is_signed: false,
            is_group: false,
            occurs: 1,
        }
    }

    /// Create a condition name type.
    pub fn condition_name() -> Self {
        Self {
            category: TypeCategory::ConditionName,
            size: 0,
            decimal_positions: 0,
            usage: Usage::Display,
            is_signed: false,
            is_group: false,
            occurs: 1,
        }
    }

    /// Get the total storage size including occurrences.
    pub fn total_size(&self) -> u32 {
        self.size * self.occurs
    }

    /// Check if this type can be used in arithmetic operations.
    pub fn is_arithmetic_compatible(&self) -> bool {
        self.category.is_numeric()
    }

    /// Calculate storage size based on PICTURE and USAGE.
    pub fn calculate_storage_size(
        pic_size: u32,
        _decimal_positions: u32,
        usage: Usage,
        _is_signed: bool,
    ) -> u32 {
        match usage {
            Usage::Display => {
                // Each digit/character is one byte
                // Sign may add one byte if separate
                pic_size
            }
            Usage::Binary | Usage::Comp5 => {
                // Binary storage depends on digit count
                let digits = pic_size;
                if digits <= 4 {
                    2 // Halfword
                } else if digits <= 9 {
                    4 // Fullword
                } else {
                    8 // Doubleword
                }
            }
            Usage::PackedDecimal => {
                // (digits + 1) / 2, rounded up, + 1 for sign
                (pic_size + 2) / 2
            }
            Usage::Comp1 => 4,   // Single precision float
            Usage::Comp2 => 8,   // Double precision float
            Usage::Pointer | Usage::FunctionPointer | Usage::ProcedurePointer => 8, // 64-bit pointer
            Usage::Index => 4,   // Index size
            Usage::National => pic_size * 2, // National uses UTF-16 (2 bytes per char)
            Usage::Utf8 => pic_size * 4,     // UTF-8 worst-case: 4 bytes per character
            Usage::Display1 => pic_size * 2, // DBCS: 2 bytes per character
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_type_category_is_numeric() {
        assert!(TypeCategory::Numeric.is_numeric());
        assert!(TypeCategory::NumericEdited.is_numeric());
        assert!(!TypeCategory::Alphanumeric.is_numeric());
        assert!(!TypeCategory::Group.is_numeric());
    }

    #[test]
    fn test_move_compatibility() {
        // Alphanumeric can receive from anything
        assert!(TypeCategory::Alphanumeric.can_receive_move_from(TypeCategory::Numeric));
        assert!(TypeCategory::Alphanumeric.can_receive_move_from(TypeCategory::Alphabetic));

        // Numeric can receive from numeric or alphanumeric
        assert!(TypeCategory::Numeric.can_receive_move_from(TypeCategory::Numeric));
        assert!(TypeCategory::Numeric.can_receive_move_from(TypeCategory::Alphanumeric));

        // Alphabetic only receives from alphanumeric types
        assert!(TypeCategory::Alphabetic.can_receive_move_from(TypeCategory::Alphabetic));
        assert!(!TypeCategory::Alphabetic.can_receive_move_from(TypeCategory::Numeric));
    }

    #[test]
    fn test_storage_size_calculation() {
        // DISPLAY: one byte per digit
        assert_eq!(
            CobolType::calculate_storage_size(5, 0, Usage::Display, false),
            5
        );

        // COMP/BINARY: 2/4/8 bytes based on digits
        assert_eq!(
            CobolType::calculate_storage_size(4, 0, Usage::Binary, true),
            2
        );
        assert_eq!(
            CobolType::calculate_storage_size(9, 0, Usage::Binary, true),
            4
        );
        assert_eq!(
            CobolType::calculate_storage_size(18, 0, Usage::Binary, true),
            8
        );

        // COMP-3: (digits + 2) / 2
        assert_eq!(
            CobolType::calculate_storage_size(5, 0, Usage::PackedDecimal, true),
            3
        );
        assert_eq!(
            CobolType::calculate_storage_size(7, 2, Usage::PackedDecimal, true),
            4
        );
    }
}
