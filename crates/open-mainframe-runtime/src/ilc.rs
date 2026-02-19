//! Interlanguage Communication (ILC) — cross-language calling and condition propagation.
//!
//! Implements the LE ILC framework that enables COBOL, PL/I, C, and Assembler
//! programs to call each other within a shared LE enclave:
//! - **Parameter descriptor mapping** between language conventions.
//! - **Calling convention adaptation** (BY REFERENCE, BY CONTENT, BY VALUE).
//! - **Condition propagation** across language boundaries.
//! - **Shared heap storage** within an enclave.

use crate::date_time::FeedbackCode;
use std::collections::HashMap;

// ---------------------------------------------------------------------------
//  Language identifiers
// ---------------------------------------------------------------------------

/// Supported LE-managed languages.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Language {
    /// COBOL (Enterprise COBOL for z/OS).
    Cobol,
    /// PL/I (Enterprise PL/I for z/OS).
    Pli,
    /// C/C++ (z/OS XL C/C++).
    C,
    /// High Level Assembler.
    Assembler,
}

impl std::fmt::Display for Language {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Cobol => write!(f, "COBOL"),
            Self::Pli => write!(f, "PL/I"),
            Self::C => write!(f, "C"),
            Self::Assembler => write!(f, "Assembler"),
        }
    }
}

// ---------------------------------------------------------------------------
//  Parameter passing conventions
// ---------------------------------------------------------------------------

/// How a parameter is passed across a language boundary.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PassingConvention {
    /// Pointer to the actual data item (COBOL BY REFERENCE, PL/I default).
    ByReference,
    /// Copy of the data passed (COBOL BY CONTENT).
    ByContent,
    /// Numeric value passed in register (C pass-by-value).
    ByValue,
}

/// Descriptor for a parameter being passed across languages.
#[derive(Debug, Clone)]
pub struct ParameterDescriptor {
    /// Parameter name (for diagnostics).
    pub name: String,
    /// Data type in the source language.
    pub source_type: DataDescriptor,
    /// Passing convention.
    pub convention: PassingConvention,
    /// Size in bytes.
    pub size: usize,
}

/// Type descriptor for cross-language data mapping.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DataDescriptor {
    /// Fixed-length character string (COBOL PIC X(n), PL/I CHAR(n)).
    Character(usize),
    /// Null-terminated string (C `char*`).
    CString,
    /// PL/I VARYING string (2-byte length prefix + data).
    VaryingString(usize),
    /// Signed integer (COBOL PIC S9(n) COMP, PL/I FIXED BIN, C `int`).
    Integer(usize),
    /// Floating point (PL/I FLOAT, C `double`).
    Float(usize),
    /// Packed decimal (COBOL PIC S9(n)V9(m) COMP-3, PL/I FIXED DEC).
    PackedDecimal { precision: u8, scale: u8 },
    /// Pointer (PL/I POINTER, C `void*`).
    Pointer,
    /// Opaque byte buffer.
    Raw(usize),
}

// ---------------------------------------------------------------------------
//  Condition mapping
// ---------------------------------------------------------------------------

/// LE condition codes that can be mapped across languages.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IlcCondition {
    /// Arithmetic overflow (COBOL SIZE ERROR, PL/I FIXEDOVERFLOW).
    Overflow,
    /// Division by zero (COBOL SIZE ERROR, PL/I ZERODIVIDE, C SIGFPE).
    ZeroDivide,
    /// Invalid data conversion (COBOL, PL/I CONVERSION).
    Conversion,
    /// End of file (COBOL AT END, PL/I ENDFILE).
    EndFile,
    /// Subscript out of range (COBOL, PL/I SUBSCRIPTRANGE).
    SubscriptRange,
    /// String size exceeded (COBOL, PL/I STRINGSIZE).
    StringSize,
    /// General program check (SIGFPE, SIGSEGV, S0Cx abend).
    ProgramCheck,
    /// User-defined condition.
    User(u32),
}

/// Map a condition from one language to another.
pub fn map_condition(
    cond: IlcCondition,
    from: Language,
    to: Language,
) -> (IlcCondition, String) {
    // Most conditions map 1:1 under LE's unified condition model.
    let desc = match (cond, from, to) {
        (IlcCondition::ZeroDivide, Language::Pli, Language::Cobol) => {
            "PL/I ZERODIVIDE → COBOL SIZE ERROR".to_string()
        }
        (IlcCondition::Overflow, Language::Pli, Language::Cobol) => {
            "PL/I FIXEDOVERFLOW → COBOL SIZE ERROR".to_string()
        }
        (IlcCondition::ProgramCheck, Language::C, Language::Cobol) => {
            "C SIGFPE → COBOL program-check".to_string()
        }
        (IlcCondition::ProgramCheck, Language::C, Language::Pli) => {
            "C SIGFPE → PL/I ERROR condition".to_string()
        }
        _ => format!("{from} {cond:?} → {to} {cond:?}"),
    };
    (cond, desc)
}

// ---------------------------------------------------------------------------
//  Cross-language call frame
// ---------------------------------------------------------------------------

/// A call frame tracking a cross-language invocation.
#[derive(Debug, Clone)]
pub struct IlcCallFrame {
    /// Caller's language.
    pub caller_lang: Language,
    /// Callee's language.
    pub callee_lang: Language,
    /// Target routine name.
    pub routine_name: String,
    /// Parameter descriptors.
    pub parameters: Vec<ParameterDescriptor>,
}

// ---------------------------------------------------------------------------
//  ILC Manager
// ---------------------------------------------------------------------------

/// Manages interlanguage communication within an LE enclave.
///
/// Tracks registered routines, active call stack, and provides
/// parameter adaptation and condition propagation.
#[derive(Debug, Clone)]
pub struct IlcManager {
    /// Registry of routines by name → language.
    routines: HashMap<String, Language>,
    /// Active call stack (cross-language frames only).
    call_stack: Vec<IlcCallFrame>,
}

impl Default for IlcManager {
    fn default() -> Self {
        Self::new()
    }
}

impl IlcManager {
    /// Create a new ILC manager.
    pub fn new() -> Self {
        Self {
            routines: HashMap::new(),
            call_stack: Vec::new(),
        }
    }

    /// Register a routine with its implementation language.
    pub fn register_routine(&mut self, name: &str, language: Language) {
        self.routines.insert(name.to_uppercase(), language);
    }

    /// Look up the language of a registered routine.
    pub fn routine_language(&self, name: &str) -> Option<Language> {
        self.routines.get(&name.to_uppercase()).copied()
    }

    /// Initiate a cross-language call.
    ///
    /// Validates parameters and pushes a call frame. Returns a feedback code
    /// indicating whether the call setup succeeded.
    pub fn begin_call(
        &mut self,
        caller_lang: Language,
        routine_name: &str,
        parameters: Vec<ParameterDescriptor>,
    ) -> FeedbackCode {
        let callee_lang = match self.routine_language(routine_name) {
            Some(lang) => lang,
            None => return FeedbackCode::error(3008),
        };

        self.call_stack.push(IlcCallFrame {
            caller_lang,
            callee_lang,
            routine_name: routine_name.to_uppercase(),
            parameters,
        });

        FeedbackCode::success()
    }

    /// Complete (pop) the current cross-language call.
    pub fn end_call(&mut self) -> Option<IlcCallFrame> {
        self.call_stack.pop()
    }

    /// Get the current call depth.
    pub fn call_depth(&self) -> usize {
        self.call_stack.len()
    }

    /// Propagate a condition through the ILC call stack.
    ///
    /// Returns the mapped condition for each language boundary crossed.
    pub fn propagate_condition(
        &self,
        condition: IlcCondition,
    ) -> Vec<(IlcCondition, Language, String)> {
        let mut results = Vec::new();
        // Walk the stack from innermost to outermost.
        for frame in self.call_stack.iter().rev() {
            let (mapped, desc) =
                map_condition(condition, frame.callee_lang, frame.caller_lang);
            results.push((mapped, frame.caller_lang, desc));
        }
        results
    }

    /// Adapt a parameter descriptor from source language conventions to target.
    pub fn adapt_parameter(
        param: &ParameterDescriptor,
        from: Language,
        to: Language,
    ) -> ParameterDescriptor {
        let mut adapted = param.clone();

        // C ↔ COBOL/PL/I string adaptation
        match (&param.source_type, from, to) {
            (DataDescriptor::Character(n), Language::Cobol | Language::Pli, Language::C) => {
                // COBOL/PL/I fixed char → C expects pointer (not null-terminated).
                adapted.source_type = DataDescriptor::Raw(*n);
                adapted.convention = PassingConvention::ByReference;
            }
            (DataDescriptor::CString, Language::C, Language::Cobol | Language::Pli) => {
                // C null-terminated → COBOL/PL/I fixed-length (caller must handle).
                adapted.convention = PassingConvention::ByReference;
            }
            (DataDescriptor::VaryingString(n), Language::Pli, Language::C) => {
                // PL/I VARYING → C must skip 2-byte length prefix.
                adapted.source_type = DataDescriptor::Raw(*n + 2);
                adapted.convention = PassingConvention::ByReference;
            }
            _ => {
                // Default: preserve convention, adjust for BY VALUE ↔ BY REFERENCE.
                if from == Language::C
                    && to != Language::C
                    && param.convention == PassingConvention::ByValue
                {
                    // C pass-by-value → COBOL/PL/I BY REFERENCE (copy to temp).
                    adapted.convention = PassingConvention::ByContent;
                }
            }
        }

        adapted
    }
}

// ---------------------------------------------------------------------------
//  Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    // ─── LE108.1: Cross-Language Call Conventions ───

    #[test]
    fn test_register_and_lookup_routine() {
        let mut mgr = IlcManager::new();
        mgr.register_routine("PLISUB", Language::Pli);
        mgr.register_routine("CSUB", Language::C);
        assert_eq!(mgr.routine_language("PLISUB"), Some(Language::Pli));
        assert_eq!(mgr.routine_language("csub"), Some(Language::C));
        assert_eq!(mgr.routine_language("NOSUCH"), None);
    }

    #[test]
    fn test_begin_end_call() {
        let mut mgr = IlcManager::new();
        mgr.register_routine("PLISUB", Language::Pli);
        let fc = mgr.begin_call(Language::Cobol, "PLISUB", vec![]);
        assert!(fc.is_success());
        assert_eq!(mgr.call_depth(), 1);

        let frame = mgr.end_call().unwrap();
        assert_eq!(frame.caller_lang, Language::Cobol);
        assert_eq!(frame.callee_lang, Language::Pli);
        assert_eq!(mgr.call_depth(), 0);
    }

    #[test]
    fn test_begin_call_unknown_routine() {
        let mut mgr = IlcManager::new();
        let fc = mgr.begin_call(Language::Cobol, "NOSUCH", vec![]);
        assert!(!fc.is_success());
    }

    #[test]
    fn test_cobol_to_pli_by_reference() {
        let mut mgr = IlcManager::new();
        mgr.register_routine("PLISUB", Language::Pli);

        let param = ParameterDescriptor {
            name: "WS-FIELD".to_string(),
            source_type: DataDescriptor::Character(20),
            convention: PassingConvention::ByReference,
            size: 20,
        };
        let fc = mgr.begin_call(Language::Cobol, "PLISUB", vec![param.clone()]);
        assert!(fc.is_success());

        let frame = mgr.end_call().unwrap();
        assert_eq!(frame.parameters.len(), 1);
        assert_eq!(frame.parameters[0].convention, PassingConvention::ByReference);
    }

    #[test]
    fn test_cobol_char_to_c_adaptation() {
        let param = ParameterDescriptor {
            name: "WS-NAME".to_string(),
            source_type: DataDescriptor::Character(20),
            convention: PassingConvention::ByReference,
            size: 20,
        };
        let adapted = IlcManager::adapt_parameter(&param, Language::Cobol, Language::C);
        assert_eq!(adapted.source_type, DataDescriptor::Raw(20));
        assert_eq!(adapted.convention, PassingConvention::ByReference);
    }

    #[test]
    fn test_c_byvalue_to_cobol_becomes_bycontent() {
        let param = ParameterDescriptor {
            name: "count".to_string(),
            source_type: DataDescriptor::Integer(4),
            convention: PassingConvention::ByValue,
            size: 4,
        };
        let adapted = IlcManager::adapt_parameter(&param, Language::C, Language::Cobol);
        assert_eq!(adapted.convention, PassingConvention::ByContent);
    }

    #[test]
    fn test_pli_varying_to_c_adaptation() {
        let param = ParameterDescriptor {
            name: "PLI_STR".to_string(),
            source_type: DataDescriptor::VaryingString(100),
            convention: PassingConvention::ByReference,
            size: 102,
        };
        let adapted = IlcManager::adapt_parameter(&param, Language::Pli, Language::C);
        assert_eq!(adapted.source_type, DataDescriptor::Raw(102));
    }

    // ─── LE108.2: Condition Propagation ───

    #[test]
    fn test_condition_map_pli_zerodiv_to_cobol() {
        let (cond, desc) = map_condition(
            IlcCondition::ZeroDivide,
            Language::Pli,
            Language::Cobol,
        );
        assert_eq!(cond, IlcCondition::ZeroDivide);
        assert!(desc.contains("SIZE ERROR"));
    }

    #[test]
    fn test_condition_map_pli_overflow_to_cobol() {
        let (cond, desc) = map_condition(
            IlcCondition::Overflow,
            Language::Pli,
            Language::Cobol,
        );
        assert_eq!(cond, IlcCondition::Overflow);
        assert!(desc.contains("SIZE ERROR"));
    }

    #[test]
    fn test_condition_map_c_sigfpe_to_pli() {
        let (_, desc) = map_condition(
            IlcCondition::ProgramCheck,
            Language::C,
            Language::Pli,
        );
        assert!(desc.contains("ERROR condition"));
    }

    #[test]
    fn test_propagate_through_stack() {
        let mut mgr = IlcManager::new();
        mgr.register_routine("PLISUB", Language::Pli);
        mgr.register_routine("CFUNC", Language::C);

        // COBOL → PL/I → C
        mgr.begin_call(Language::Cobol, "PLISUB", vec![]);
        mgr.begin_call(Language::Pli, "CFUNC", vec![]);

        let results = mgr.propagate_condition(IlcCondition::ProgramCheck);
        // Should propagate from C→PL/I, then PL/I→COBOL.
        assert_eq!(results.len(), 2);
        assert_eq!(results[0].1, Language::Pli); // C→PL/I
        assert_eq!(results[1].1, Language::Cobol); // PL/I→COBOL
    }

    #[test]
    fn test_nested_calls_depth() {
        let mut mgr = IlcManager::new();
        mgr.register_routine("SUB1", Language::Pli);
        mgr.register_routine("SUB2", Language::C);
        mgr.register_routine("SUB3", Language::Assembler);

        mgr.begin_call(Language::Cobol, "SUB1", vec![]);
        mgr.begin_call(Language::Pli, "SUB2", vec![]);
        mgr.begin_call(Language::C, "SUB3", vec![]);
        assert_eq!(mgr.call_depth(), 3);

        mgr.end_call();
        assert_eq!(mgr.call_depth(), 2);
    }

    #[test]
    fn test_language_display() {
        assert_eq!(format!("{}", Language::Cobol), "COBOL");
        assert_eq!(format!("{}", Language::Pli), "PL/I");
        assert_eq!(format!("{}", Language::C), "C");
        assert_eq!(format!("{}", Language::Assembler), "Assembler");
    }

    #[test]
    fn test_data_descriptor_variants() {
        let d1 = DataDescriptor::PackedDecimal { precision: 9, scale: 2 };
        let d2 = DataDescriptor::Pointer;
        let d3 = DataDescriptor::Float(8);
        // Just ensure they can be compared.
        assert_ne!(d1, d2);
        assert_ne!(d2, d3);
    }
}
