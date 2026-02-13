//! COBOL-2014 Extended Type System.
//!
//! Implements additional data types and features from COBOL-2014 standard:
//! - BOOLEAN type
//! - FUNCTION-POINTER type
//! - TYPEDEF support
//! - National (UTF-16) type
//! - Floating-point types (FLOAT-SHORT, FLOAT-LONG)
//! - Exception handling (RAISE/RESUME)
//! - FREE statement for memory management

use std::collections::HashMap;

/// COBOL-2014 extended type categories.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExtendedType {
    /// Boolean type (single bit conceptually, 1 byte storage)
    Boolean,
    /// Function pointer
    FunctionPointer,
    /// Procedure pointer
    ProcedurePointer,
    /// Object reference
    ObjectReference(Option<String>), // Optional class name
    /// National (UTF-16) string
    National(usize), // Character count
    /// IEEE 754 single precision float (4 bytes)
    FloatShort,
    /// IEEE 754 double precision float (8 bytes)
    FloatLong,
    /// IEEE 754 extended precision (16 bytes)
    FloatExtended,
    /// User-defined type (TYPEDEF)
    UserDefined(String),
}

impl ExtendedType {
    /// Get the storage size in bytes.
    pub fn storage_size(&self) -> usize {
        match self {
            ExtendedType::Boolean => 1,
            ExtendedType::FunctionPointer => 8,
            ExtendedType::ProcedurePointer => 8,
            ExtendedType::ObjectReference(_) => 8,
            ExtendedType::National(chars) => chars * 2, // UTF-16: 2 bytes per char
            ExtendedType::FloatShort => 4,
            ExtendedType::FloatLong => 8,
            ExtendedType::FloatExtended => 16,
            ExtendedType::UserDefined(_) => 0, // Resolved from typedef
        }
    }

    /// Check if this type is numeric.
    pub fn is_numeric(&self) -> bool {
        matches!(
            self,
            ExtendedType::FloatShort | ExtendedType::FloatLong | ExtendedType::FloatExtended
        )
    }

    /// Check if this is a pointer type.
    pub fn is_pointer(&self) -> bool {
        matches!(
            self,
            ExtendedType::FunctionPointer
                | ExtendedType::ProcedurePointer
                | ExtendedType::ObjectReference(_)
        )
    }
}

/// Boolean value in COBOL-2014.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BooleanValue {
    /// B"0" - false
    False,
    /// B"1" - true
    True,
}

impl BooleanValue {
    /// Create from a boolean.
    pub fn from_bool(b: bool) -> Self {
        if b {
            BooleanValue::True
        } else {
            BooleanValue::False
        }
    }

    /// Convert to bool.
    pub fn to_bool(self) -> bool {
        matches!(self, BooleanValue::True)
    }

    /// Parse from COBOL boolean literal.
    pub fn parse(s: &str) -> Option<Self> {
        match s.to_uppercase().as_str() {
            "B\"0\"" | "B'0'" | "FALSE" => Some(BooleanValue::False),
            "B\"1\"" | "B'1'" | "TRUE" => Some(BooleanValue::True),
            _ => None,
        }
    }

    /// Convert to COBOL literal.
    pub fn to_literal(self) -> &'static str {
        match self {
            BooleanValue::False => "B\"0\"",
            BooleanValue::True => "B\"1\"",
        }
    }
}

impl Default for BooleanValue {
    fn default() -> Self {
        BooleanValue::False
    }
}

/// Type definition (TYPEDEF).
#[derive(Debug, Clone, PartialEq)]
pub struct TypeDef {
    /// Name of the type
    pub name: String,
    /// Base type definition
    pub base_type: TypeDefBase,
    /// Whether this is a STRONG typedef (strict type checking)
    pub is_strong: bool,
}

/// Base definition for a TYPEDEF.
#[derive(Debug, Clone, PartialEq)]
pub enum TypeDefBase {
    /// Based on a PICTURE clause
    Picture {
        picture: String,
        usage: Option<String>,
    },
    /// Based on another type
    Reference(String),
    /// Based on a level-01 structure
    Structure(Vec<TypeDefField>),
}

/// A field within a TYPEDEF structure.
#[derive(Debug, Clone, PartialEq)]
pub struct TypeDefField {
    /// Level number
    pub level: u8,
    /// Field name
    pub name: String,
    /// Picture clause
    pub picture: Option<String>,
    /// Usage
    pub usage: Option<String>,
}

/// Registry for user-defined types.
#[derive(Debug, Default)]
pub struct TypeDefRegistry {
    /// Registered type definitions
    types: HashMap<String, TypeDef>,
}

impl TypeDefRegistry {
    /// Create a new registry.
    pub fn new() -> Self {
        Self {
            types: HashMap::new(),
        }
    }

    /// Register a type definition.
    pub fn register(&mut self, typedef: TypeDef) {
        self.types.insert(typedef.name.to_uppercase(), typedef);
    }

    /// Look up a type definition.
    pub fn lookup(&self, name: &str) -> Option<&TypeDef> {
        self.types.get(&name.to_uppercase())
    }

    /// Check if a type is defined.
    pub fn is_defined(&self, name: &str) -> bool {
        self.types.contains_key(&name.to_uppercase())
    }
}

/// COBOL-2014 Exception Conditions.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExceptionCondition {
    // EC-ARGUMENT conditions
    ArgumentError,
    ArgumentFunctionTrim,
    ArgumentFunctionLog,
    ArgumentFunctionInspect,

    // EC-BOUND conditions
    BoundError,
    BoundRefMod,
    BoundSubscript,
    BoundTableLimit,

    // EC-DATA conditions
    DataError,
    DataIncompleteLine,
    DataConversion,

    // EC-FLOW conditions
    FlowError,
    FlowGlobalGoback,
    FlowIpc,

    // EC-I-O conditions
    IoError,
    IoAtEnd,
    IoEndOfPage,
    IoInvalidKey,
    IoPermError,

    // EC-OO conditions
    OoError,
    OoConformance,
    OoNull,
    OoUniversal,

    // EC-OVERFLOW conditions
    OverflowError,
    OverflowString,
    OverflowUnstring,

    // EC-PROGRAM conditions
    ProgramError,
    ProgramArgMismatch,
    ProgramCancelActive,
    ProgramNotFound,
    ProgramRecursive,

    // EC-RANGE conditions
    RangeError,
    RangeInvalidTime,
    RangeOdoInvalid,

    // EC-SIZE conditions
    SizeError,
    SizeExponentiation,
    SizeTruncation,
    SizeZeroDivide,

    // EC-STORAGE conditions
    StorageError,
    StorageNotAlloc,
    StorageNotAvail,

    // EC-USER conditions
    UserException(String),

    // All conditions
    All,
}

impl ExceptionCondition {
    /// Parse from COBOL exception name.
    pub fn parse(name: &str) -> Option<Self> {
        let upper = name.to_uppercase();
        match upper.as_str() {
            "EC-ARGUMENT" => Some(Self::ArgumentError),
            "EC-ARGUMENT-FUNCTION-TRIM" => Some(Self::ArgumentFunctionTrim),
            "EC-ARGUMENT-FUNCTION-LOG" => Some(Self::ArgumentFunctionLog),
            "EC-BOUND" => Some(Self::BoundError),
            "EC-BOUND-REF-MOD" => Some(Self::BoundRefMod),
            "EC-BOUND-SUBSCRIPT" => Some(Self::BoundSubscript),
            "EC-DATA" => Some(Self::DataError),
            "EC-DATA-CONVERSION" => Some(Self::DataConversion),
            "EC-FLOW" => Some(Self::FlowError),
            "EC-I-O" => Some(Self::IoError),
            "EC-I-O-AT-END" => Some(Self::IoAtEnd),
            "EC-I-O-INVALID-KEY" => Some(Self::IoInvalidKey),
            "EC-OO" => Some(Self::OoError),
            "EC-OO-NULL" => Some(Self::OoNull),
            "EC-OVERFLOW" => Some(Self::OverflowError),
            "EC-OVERFLOW-STRING" => Some(Self::OverflowString),
            "EC-PROGRAM" => Some(Self::ProgramError),
            "EC-PROGRAM-NOT-FOUND" => Some(Self::ProgramNotFound),
            "EC-RANGE" => Some(Self::RangeError),
            "EC-SIZE" => Some(Self::SizeError),
            "EC-SIZE-ZERO-DIVIDE" => Some(Self::SizeZeroDivide),
            "EC-STORAGE" => Some(Self::StorageError),
            "EC-STORAGE-NOT-ALLOC" => Some(Self::StorageNotAlloc),
            "EC-ALL" => Some(Self::All),
            _ if upper.starts_with("EC-USER-") => {
                Some(Self::UserException(upper[8..].to_string()))
            }
            _ => None,
        }
    }

    /// Get the exception name.
    pub fn name(&self) -> String {
        match self {
            Self::ArgumentError => "EC-ARGUMENT".to_string(),
            Self::ArgumentFunctionTrim => "EC-ARGUMENT-FUNCTION-TRIM".to_string(),
            Self::ArgumentFunctionLog => "EC-ARGUMENT-FUNCTION-LOG".to_string(),
            Self::ArgumentFunctionInspect => "EC-ARGUMENT-FUNCTION-INSPECT".to_string(),
            Self::BoundError => "EC-BOUND".to_string(),
            Self::BoundRefMod => "EC-BOUND-REF-MOD".to_string(),
            Self::BoundSubscript => "EC-BOUND-SUBSCRIPT".to_string(),
            Self::BoundTableLimit => "EC-BOUND-TABLE-LIMIT".to_string(),
            Self::DataError => "EC-DATA".to_string(),
            Self::DataIncompleteLine => "EC-DATA-INCOMPLETE-LINE".to_string(),
            Self::DataConversion => "EC-DATA-CONVERSION".to_string(),
            Self::FlowError => "EC-FLOW".to_string(),
            Self::FlowGlobalGoback => "EC-FLOW-GLOBAL-GOBACK".to_string(),
            Self::FlowIpc => "EC-FLOW-IPC".to_string(),
            Self::IoError => "EC-I-O".to_string(),
            Self::IoAtEnd => "EC-I-O-AT-END".to_string(),
            Self::IoEndOfPage => "EC-I-O-END-OF-PAGE".to_string(),
            Self::IoInvalidKey => "EC-I-O-INVALID-KEY".to_string(),
            Self::IoPermError => "EC-I-O-PERM".to_string(),
            Self::OoError => "EC-OO".to_string(),
            Self::OoConformance => "EC-OO-CONFORMANCE".to_string(),
            Self::OoNull => "EC-OO-NULL".to_string(),
            Self::OoUniversal => "EC-OO-UNIVERSAL".to_string(),
            Self::OverflowError => "EC-OVERFLOW".to_string(),
            Self::OverflowString => "EC-OVERFLOW-STRING".to_string(),
            Self::OverflowUnstring => "EC-OVERFLOW-UNSTRING".to_string(),
            Self::ProgramError => "EC-PROGRAM".to_string(),
            Self::ProgramArgMismatch => "EC-PROGRAM-ARG-MISMATCH".to_string(),
            Self::ProgramCancelActive => "EC-PROGRAM-CANCEL-ACTIVE".to_string(),
            Self::ProgramNotFound => "EC-PROGRAM-NOT-FOUND".to_string(),
            Self::ProgramRecursive => "EC-PROGRAM-RECURSIVE".to_string(),
            Self::RangeError => "EC-RANGE".to_string(),
            Self::RangeInvalidTime => "EC-RANGE-INVALID-TIME".to_string(),
            Self::RangeOdoInvalid => "EC-RANGE-ODO-INVALID".to_string(),
            Self::SizeError => "EC-SIZE".to_string(),
            Self::SizeExponentiation => "EC-SIZE-EXPONENTIATION".to_string(),
            Self::SizeTruncation => "EC-SIZE-TRUNCATION".to_string(),
            Self::SizeZeroDivide => "EC-SIZE-ZERO-DIVIDE".to_string(),
            Self::StorageError => "EC-STORAGE".to_string(),
            Self::StorageNotAlloc => "EC-STORAGE-NOT-ALLOC".to_string(),
            Self::StorageNotAvail => "EC-STORAGE-NOT-AVAIL".to_string(),
            Self::UserException(name) => format!("EC-USER-{}", name),
            Self::All => "EC-ALL".to_string(),
        }
    }
}

/// Memory management for ALLOCATE/FREE statements.
#[derive(Debug, Default)]
pub struct MemoryAllocator {
    /// Allocated pointers and their sizes
    allocations: HashMap<usize, usize>,
    /// Next allocation address (simplified)
    next_addr: usize,
}

impl MemoryAllocator {
    /// Create a new allocator.
    pub fn new() -> Self {
        Self {
            allocations: HashMap::new(),
            next_addr: 0x10000, // Start at some base address
        }
    }

    /// Allocate memory.
    pub fn allocate(&mut self, size: usize) -> usize {
        let addr = self.next_addr;
        self.next_addr += size;
        // Align to 8 bytes
        self.next_addr = (self.next_addr + 7) & !7;
        self.allocations.insert(addr, size);
        addr
    }

    /// Free memory.
    pub fn free(&mut self, addr: usize) -> Result<(), MemoryError> {
        if self.allocations.remove(&addr).is_some() {
            Ok(())
        } else {
            Err(MemoryError::NotAllocated)
        }
    }

    /// Check if an address is allocated.
    pub fn is_allocated(&self, addr: usize) -> bool {
        self.allocations.contains_key(&addr)
    }

    /// Get allocation size.
    pub fn get_size(&self, addr: usize) -> Option<usize> {
        self.allocations.get(&addr).copied()
    }
}

/// Memory operation errors.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MemoryError {
    /// Address was not allocated
    NotAllocated,
    /// No memory available
    NotAvailable,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_extended_type_size() {
        assert_eq!(ExtendedType::Boolean.storage_size(), 1);
        assert_eq!(ExtendedType::FunctionPointer.storage_size(), 8);
        assert_eq!(ExtendedType::National(10).storage_size(), 20);
        assert_eq!(ExtendedType::FloatShort.storage_size(), 4);
        assert_eq!(ExtendedType::FloatLong.storage_size(), 8);
    }

    #[test]
    fn test_boolean_value() {
        assert_eq!(BooleanValue::parse("B\"1\""), Some(BooleanValue::True));
        assert_eq!(BooleanValue::parse("B\"0\""), Some(BooleanValue::False));
        assert_eq!(BooleanValue::parse("TRUE"), Some(BooleanValue::True));

        assert!(BooleanValue::True.to_bool());
        assert!(!BooleanValue::False.to_bool());
    }

    #[test]
    fn test_typedef_registry() {
        let mut registry = TypeDefRegistry::new();

        let typedef = TypeDef {
            name: "CUSTOMER-ID".to_string(),
            base_type: TypeDefBase::Picture {
                picture: "X(10)".to_string(),
                usage: None,
            },
            is_strong: false,
        };

        registry.register(typedef);

        assert!(registry.is_defined("CUSTOMER-ID"));
        assert!(registry.is_defined("customer-id")); // Case insensitive
        assert!(!registry.is_defined("UNKNOWN"));

        let looked_up = registry.lookup("CUSTOMER-ID").unwrap();
        assert_eq!(looked_up.name, "CUSTOMER-ID");
    }

    #[test]
    fn test_exception_parse() {
        assert_eq!(
            ExceptionCondition::parse("EC-SIZE"),
            Some(ExceptionCondition::SizeError)
        );
        assert_eq!(
            ExceptionCondition::parse("EC-SIZE-ZERO-DIVIDE"),
            Some(ExceptionCondition::SizeZeroDivide)
        );
        assert_eq!(
            ExceptionCondition::parse("EC-USER-CUSTOM"),
            Some(ExceptionCondition::UserException("CUSTOM".to_string()))
        );
    }

    #[test]
    fn test_memory_allocator() {
        let mut alloc = MemoryAllocator::new();

        let addr1 = alloc.allocate(100);
        let addr2 = alloc.allocate(200);

        assert!(alloc.is_allocated(addr1));
        assert!(alloc.is_allocated(addr2));
        assert_eq!(alloc.get_size(addr1), Some(100));

        assert!(alloc.free(addr1).is_ok());
        assert!(!alloc.is_allocated(addr1));

        assert_eq!(alloc.free(addr1), Err(MemoryError::NotAllocated));
    }

    #[test]
    fn test_extended_type_is_numeric() {
        assert!(ExtendedType::FloatShort.is_numeric());
        assert!(ExtendedType::FloatLong.is_numeric());
        assert!(!ExtendedType::Boolean.is_numeric());
        assert!(!ExtendedType::National(5).is_numeric());
    }

    #[test]
    fn test_extended_type_is_pointer() {
        assert!(ExtendedType::FunctionPointer.is_pointer());
        assert!(ExtendedType::ProcedurePointer.is_pointer());
        assert!(ExtendedType::ObjectReference(None).is_pointer());
        assert!(!ExtendedType::Boolean.is_pointer());
    }
}
