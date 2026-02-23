//! Runtime library for OpenMainframe COBOL execution.
//!
//! This crate provides the runtime library that compiled COBOL programs
//! link against. It includes:
//!
//! - Console I/O (DISPLAY, ACCEPT)
//! - Decimal arithmetic with IBM-compatible precision
//! - String manipulation (STRING, UNSTRING, INSPECT)
//! - Value representation for COBOL data types
//!
//! # Example
//!
//! ```
//! use open_mainframe_runtime::{display_to_writer, CobolValue};
//!
//! let mut output = Vec::new();
//! let values = vec![CobolValue::alphanumeric("HELLO WORLD")];
//! display_to_writer(&values, &mut output, false).unwrap();
//! assert_eq!(String::from_utf8(output).unwrap(), "HELLO WORLD\n");
//! ```

pub mod abend;
pub mod bits;
pub mod condition;
pub mod date_time;
pub mod decimal;
pub mod enclave;
pub mod error;
pub mod heap;
pub mod file_io;
pub mod ilc;
pub mod interpreter;
pub mod io;
pub mod locale;
pub mod math;
pub mod message;
pub mod numeric_editing;
pub mod options;
pub mod sort_verb;
pub mod storage;
pub mod string;
pub mod value;

pub use abend::{
    AbendCode, AbendInfo, AbendRegistry, AbendRegistryEntry, DumpType, FormattedDump,
    MachineDump, MdumpHeader, RegisterSet, SnapDump, StackFrame, StepCompletion,
    SystemAreas, ABEND_REGISTRY,
};
pub use date_time::{
    cee3dly, ceedate, ceedatm, ceedays, ceedywk, ceegmt, ceegmto, ceeisec, ceeloct,
    ceeseci, ceesecs, date_of_integer, date_to_lilian, day_of_integer, integer_of_date,
    integer_of_day, is_leap_year, lilian_to_date, CenturyWindow, FeedbackCode,
};
pub use decimal::{add, add_to, compute, divide, multiply, power, subtract_from};
pub use decimal::{ArithmeticResult, RoundingMode};
pub use error::RuntimeError;
pub use io::{accept, accept_from_reader, display, display_to_writer};
pub use io::{AcceptSource, DisplayOptions, DisplayTarget};
pub use numeric_editing::format_numeric;
pub use storage::{StorageFormat, StoredNumeric};
pub use string::{
    inspect_converting, inspect_replacing, inspect_tallying, string_concat, unstring,
};
pub use string::{
    InspectMode, ReplacingClause, StringDelimiter, StringSource, TallyingClause, UnstringDelimiter,
    UnstringFieldResult, UnstringTarget,
};
pub use math::{
    call_le_math, cees_abs, cees_acs, cees_asn, cees_at2, cees_ath, cees_atn, cees_cos,
    cees_csh, cees_ctn, cees_exp, cees_lg1, cees_log, cees_mod, cees_nin, cees_nwn, cees_sgn,
    cees_sin, cees_snh, cees_sqt, cees_tan, cees_tnh, cees_xpx, Precision, RandomState,
};
pub use message::{
    ceedcod, ceemget, ceemsg, ceemout, ceencod, ConditionToken, MessageCatalog,
};
pub use options::{OptionSource, OptionValue, RuntimeOptions};
pub use ilc::{
    DataDescriptor, IlcCallFrame, IlcCondition, IlcManager, Language, ParameterDescriptor,
    PassingConvention,
};
pub use bits::{ceesiclr, ceesiset, ceesishf, ceesitst};
pub use locale::{LocaleConventions, LocaleDefinition, LocaleManager};
pub use value::{CobolValue, NumericValue};
