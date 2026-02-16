//! CBL/PROCESS statement parsing and compiler options.
//!
//! This module implements Pass 0 of the preprocessor pipeline:
//! parsing CBL or PROCESS statements that appear on the first line(s)
//! of a COBOL source file to set compiler options.
//!
//! Per IBM Enterprise COBOL v6.4, the CBL/PROCESS statement must
//! appear before the IDENTIFICATION DIVISION and specifies compiler
//! options in the form: `CBL OPT1(val),OPT2(val),...`

use std::fmt;

/// Arithmetic mode for numeric computations.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ArithMode {
    /// Compatible mode — up to 18-digit intermediate precision (default).
    Compat,
    /// Extended mode — up to 31-digit intermediate precision.
    Extend,
}

impl Default for ArithMode {
    fn default() -> Self {
        ArithMode::Compat
    }
}

impl fmt::Display for ArithMode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ArithMode::Compat => write!(f, "COMPAT"),
            ArithMode::Extend => write!(f, "EXTEND"),
        }
    }
}

/// Truncation mode for BINARY/COMP fields.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TruncMode {
    /// Standard truncation — truncate to PIC size (default).
    Std,
    /// Optimized truncation — truncate to PIC size, compiler may optimize.
    Opt,
    /// Binary truncation — use full binary field range (no PIC truncation).
    Bin,
}

impl Default for TruncMode {
    fn default() -> Self {
        TruncMode::Std
    }
}

impl fmt::Display for TruncMode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TruncMode::Std => write!(f, "STD"),
            TruncMode::Opt => write!(f, "OPT"),
            TruncMode::Bin => write!(f, "BIN"),
        }
    }
}

/// NUMPROC option — controls sign processing.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NumProc {
    /// No special numeric processing; sign is normalized on each operation.
    Nopfd,
    /// Preferred sign — compiler assumes valid signs (performance optimization).
    Pfd,
    /// Migration mode — NUMPROC(MIG).
    Mig,
}

impl Default for NumProc {
    fn default() -> Self {
        NumProc::Nopfd
    }
}

impl fmt::Display for NumProc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            NumProc::Nopfd => write!(f, "NOPFD"),
            NumProc::Pfd => write!(f, "PFD"),
            NumProc::Mig => write!(f, "MIG"),
        }
    }
}

/// NSYMBOL option — controls the meaning of the N literal prefix.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NSymbol {
    /// N literals are DBCS (default).
    Dbcs,
    /// N literals are national (UTF-16).
    National,
}

impl Default for NSymbol {
    fn default() -> Self {
        NSymbol::Dbcs
    }
}

impl fmt::Display for NSymbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            NSymbol::Dbcs => write!(f, "DBCS"),
            NSymbol::National => write!(f, "NATIONAL"),
        }
    }
}

/// INTDATE option — controls the date intrinsic function behavior.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IntDate {
    /// ANSI date functions (default).
    Ansi,
    /// Lilian date functions.
    Lilian,
}

impl Default for IntDate {
    fn default() -> Self {
        IntDate::Ansi
    }
}

impl fmt::Display for IntDate {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            IntDate::Ansi => write!(f, "ANSI"),
            IntDate::Lilian => write!(f, "LILIAN"),
        }
    }
}

/// Compiler options parsed from CBL/PROCESS statement.
///
/// All options default to IBM Enterprise COBOL v6.4 defaults.
#[derive(Debug, Clone)]
pub struct CompilerOptions {
    /// Arithmetic precision mode.
    pub arith: ArithMode,
    /// Binary field truncation mode.
    pub trunc: TruncMode,
    /// EBCDIC code page for the source.
    pub codepage: u16,
    /// Numeric processing mode.
    pub numproc: NumProc,
    /// N literal symbol interpretation.
    pub nsymbol: NSymbol,
    /// Date intrinsic function mode.
    pub intdate: IntDate,
    /// Maximum digits for numeric precision based on ARITH.
    /// COMPAT=18, EXTEND=31.
    pub max_digits: u8,
}

impl Default for CompilerOptions {
    fn default() -> Self {
        Self {
            arith: ArithMode::Compat,
            trunc: TruncMode::Std,
            codepage: 1140, // IBM-1140 (US EBCDIC with euro)
            numproc: NumProc::Nopfd,
            nsymbol: NSymbol::Dbcs,
            intdate: IntDate::Ansi,
            max_digits: 18,
        }
    }
}

impl CompilerOptions {
    /// Create compiler options with all defaults.
    pub fn new() -> Self {
        Self::default()
    }

    /// Update max_digits based on current arith mode.
    fn sync_max_digits(&mut self) {
        self.max_digits = match self.arith {
            ArithMode::Compat => 18,
            ArithMode::Extend => 31,
        };
    }
}

/// Errors from parsing CBL/PROCESS statements.
#[derive(Debug, Clone)]
pub struct CompilerOptionError {
    /// Description of the error.
    pub message: String,
    /// The option text that caused the error, if available.
    pub option_text: Option<String>,
}

impl fmt::Display for CompilerOptionError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(ref opt) = self.option_text {
            write!(f, "Invalid compiler option '{}': {}", opt, self.message)
        } else {
            write!(f, "Compiler option error: {}", self.message)
        }
    }
}

/// Parse CBL/PROCESS statements from source text.
///
/// This is Pass 0 of the preprocessor pipeline. It:
/// 1. Checks if the first non-blank line starts with CBL or PROCESS
/// 2. Parses all comma-separated options
/// 3. Returns the CompilerOptions and the remaining source text
///
/// CBL/PROCESS can span multiple lines (continuation lines start with CBL/PROCESS).
pub fn parse_cbl_process(source: &str) -> (CompilerOptions, String, Vec<CompilerOptionError>) {
    let mut options = CompilerOptions::new();
    let mut errors = Vec::new();
    let mut remaining_lines = Vec::new();
    let mut in_cbl_process = true;
    let mut found_cbl = false;

    for line in source.lines() {
        let trimmed = line.trim();

        if in_cbl_process {
            let upper = trimmed.to_uppercase();

            // Check if line starts with CBL or PROCESS
            if upper.starts_with("CBL ") || upper.starts_with("CBL\t") || upper == "CBL" {
                found_cbl = true;
                let opts_text = trimmed[3..].trim();
                if !opts_text.is_empty() {
                    parse_options_list(opts_text, &mut options, &mut errors);
                }
                continue;
            } else if upper.starts_with("PROCESS ") || upper.starts_with("PROCESS\t") || upper == "PROCESS" {
                found_cbl = true;
                let opts_text = trimmed[7..].trim();
                if !opts_text.is_empty() {
                    parse_options_list(opts_text, &mut options, &mut errors);
                }
                continue;
            } else if found_cbl {
                // Previous line was CBL/PROCESS but this one isn't — stop scanning
                in_cbl_process = false;
                remaining_lines.push(line.to_string());
            } else if trimmed.is_empty() {
                // Blank lines before CBL/PROCESS are kept
                remaining_lines.push(line.to_string());
                // But don't stop looking — CBL can appear after blanks
                continue;
            } else {
                // Non-CBL/PROCESS content — no CBL/PROCESS statement present
                in_cbl_process = false;
                remaining_lines.push(line.to_string());
            }
        } else {
            remaining_lines.push(line.to_string());
        }
    }

    // Sync derived values
    options.sync_max_digits();

    let remaining = remaining_lines.join("\n");
    (options, remaining, errors)
}

/// Parse a comma-separated list of compiler options.
fn parse_options_list(text: &str, options: &mut CompilerOptions, errors: &mut Vec<CompilerOptionError>) {
    // Split on commas, handling optional spaces
    for opt in text.split(',') {
        let opt = opt.trim();
        if opt.is_empty() {
            continue;
        }
        parse_single_option(opt, options, errors);
    }
}

/// Parse a single compiler option like `ARITH(EXTEND)` or `TRUNC(BIN)`.
fn parse_single_option(opt: &str, options: &mut CompilerOptions, errors: &mut Vec<CompilerOptionError>) {
    let upper = opt.trim().to_uppercase();

    // Handle options with parenthesized values: NAME(VALUE)
    if let Some(paren_start) = upper.find('(') {
        let name = &upper[..paren_start];
        let rest = &upper[paren_start + 1..];
        let value = if let Some(paren_end) = rest.find(')') {
            &rest[..paren_end]
        } else {
            errors.push(CompilerOptionError {
                message: "Missing closing parenthesis".into(),
                option_text: Some(opt.to_string()),
            });
            return;
        };

        match name {
            "ARITH" | "AR" => match value {
                "COMPAT" | "C" => options.arith = ArithMode::Compat,
                "EXTEND" | "E" => options.arith = ArithMode::Extend,
                _ => errors.push(CompilerOptionError {
                    message: format!("Invalid ARITH value '{}', expected COMPAT or EXTEND", value),
                    option_text: Some(opt.to_string()),
                }),
            },
            "TRUNC" => match value {
                "STD" => options.trunc = TruncMode::Std,
                "OPT" => options.trunc = TruncMode::Opt,
                "BIN" => options.trunc = TruncMode::Bin,
                _ => errors.push(CompilerOptionError {
                    message: format!("Invalid TRUNC value '{}', expected STD, OPT, or BIN", value),
                    option_text: Some(opt.to_string()),
                }),
            },
            "CODEPAGE" | "CP" => {
                match value.parse::<u16>() {
                    Ok(cp) => options.codepage = cp,
                    Err(_) => errors.push(CompilerOptionError {
                        message: format!("Invalid CODEPAGE value '{}', expected numeric", value),
                        option_text: Some(opt.to_string()),
                    }),
                }
            }
            "NUMPROC" => match value {
                "NOPFD" => options.numproc = NumProc::Nopfd,
                "PFD" => options.numproc = NumProc::Pfd,
                "MIG" => options.numproc = NumProc::Mig,
                _ => errors.push(CompilerOptionError {
                    message: format!("Invalid NUMPROC value '{}', expected NOPFD, PFD, or MIG", value),
                    option_text: Some(opt.to_string()),
                }),
            },
            "NSYMBOL" | "NS" => match value {
                "DBCS" => options.nsymbol = NSymbol::Dbcs,
                "NATIONAL" | "NAT" => options.nsymbol = NSymbol::National,
                _ => errors.push(CompilerOptionError {
                    message: format!("Invalid NSYMBOL value '{}', expected DBCS or NATIONAL", value),
                    option_text: Some(opt.to_string()),
                }),
            },
            "INTDATE" => match value {
                "ANSI" => options.intdate = IntDate::Ansi,
                "LILIAN" => options.intdate = IntDate::Lilian,
                _ => errors.push(CompilerOptionError {
                    message: format!("Invalid INTDATE value '{}', expected ANSI or LILIAN", value),
                    option_text: Some(opt.to_string()),
                }),
            },
            _ => {
                // Unknown option with value — skip with warning
                errors.push(CompilerOptionError {
                    message: format!("Unrecognized compiler option '{}'", name),
                    option_text: Some(opt.to_string()),
                });
            }
        }
    } else {
        // Options without parentheses (flag-style options)
        // For now, just record as unrecognized since the main options use parens
        match upper.as_str() {
            "NOARITH" => options.arith = ArithMode::Compat,
            _ => {
                errors.push(CompilerOptionError {
                    message: format!("Unrecognized compiler option '{}'", upper),
                    option_text: Some(opt.to_string()),
                });
            }
        }
    }
}

/// Get the maximum numeric precision digits for the current options.
pub fn max_precision_digits(options: &CompilerOptions) -> u8 {
    options.max_digits
}

/// Check if a binary value should be truncated to PIC size.
pub fn should_truncate_to_pic(options: &CompilerOptions) -> bool {
    matches!(options.trunc, TruncMode::Std | TruncMode::Opt)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_default_options() {
        let opts = CompilerOptions::default();
        assert_eq!(opts.arith, ArithMode::Compat);
        assert_eq!(opts.trunc, TruncMode::Std);
        assert_eq!(opts.codepage, 1140);
        assert_eq!(opts.numproc, NumProc::Nopfd);
        assert_eq!(opts.nsymbol, NSymbol::Dbcs);
        assert_eq!(opts.intdate, IntDate::Ansi);
        assert_eq!(opts.max_digits, 18);
    }

    #[test]
    fn test_parse_cbl_arith_trunc() {
        let source = "CBL ARITH(EXTEND),TRUNC(BIN)\n       IDENTIFICATION DIVISION.\n";
        let (opts, remaining, errors) = parse_cbl_process(source);
        assert!(errors.is_empty(), "Unexpected errors: {:?}", errors);
        assert_eq!(opts.arith, ArithMode::Extend);
        assert_eq!(opts.trunc, TruncMode::Bin);
        assert_eq!(opts.max_digits, 31);
        assert!(remaining.contains("IDENTIFICATION DIVISION"));
        assert!(!remaining.to_uppercase().contains("CBL"));
    }

    #[test]
    fn test_parse_process_keyword() {
        let source = "PROCESS CODEPAGE(1140),NUMPROC(PFD)\n       IDENTIFICATION DIVISION.\n";
        let (opts, remaining, errors) = parse_cbl_process(source);
        assert!(errors.is_empty(), "Unexpected errors: {:?}", errors);
        assert_eq!(opts.codepage, 1140);
        assert_eq!(opts.numproc, NumProc::Pfd);
        assert!(remaining.contains("IDENTIFICATION DIVISION"));
    }

    #[test]
    fn test_no_cbl_process() {
        let source = "       IDENTIFICATION DIVISION.\n       PROGRAM-ID. HELLO.\n";
        let (opts, remaining, errors) = parse_cbl_process(source);
        assert!(errors.is_empty());
        assert_eq!(opts.arith, ArithMode::Compat); // default
        assert_eq!(opts.trunc, TruncMode::Std); // default
        assert!(remaining.contains("IDENTIFICATION DIVISION"));
    }

    #[test]
    fn test_multiple_options() {
        let source = "CBL ARITH(EXTEND),TRUNC(OPT),NSYMBOL(NATIONAL),INTDATE(LILIAN)\n       ID DIVISION.\n";
        let (opts, remaining, errors) = parse_cbl_process(source);
        assert!(errors.is_empty(), "Unexpected errors: {:?}", errors);
        assert_eq!(opts.arith, ArithMode::Extend);
        assert_eq!(opts.trunc, TruncMode::Opt);
        assert_eq!(opts.nsymbol, NSymbol::National);
        assert_eq!(opts.intdate, IntDate::Lilian);
        assert_eq!(opts.max_digits, 31);
        assert!(remaining.contains("ID DIVISION"));
    }

    #[test]
    fn test_abbreviated_options() {
        let source = "CBL AR(E),CP(819),NS(NAT)\n       ID DIVISION.\n";
        let (opts, remaining, errors) = parse_cbl_process(source);
        assert!(errors.is_empty(), "Unexpected errors: {:?}", errors);
        assert_eq!(opts.arith, ArithMode::Extend);
        assert_eq!(opts.codepage, 819);
        assert_eq!(opts.nsymbol, NSymbol::National);
        assert!(remaining.contains("ID DIVISION"));
    }

    #[test]
    fn test_invalid_option_value() {
        let source = "CBL ARITH(INVALID)\n       ID DIVISION.\n";
        let (opts, _remaining, errors) = parse_cbl_process(source);
        assert_eq!(errors.len(), 1);
        assert!(errors[0].message.contains("Invalid ARITH value"));
        // Default should remain
        assert_eq!(opts.arith, ArithMode::Compat);
    }

    #[test]
    fn test_unknown_option() {
        let source = "CBL FOOBAR(XYZ)\n       ID DIVISION.\n";
        let (_, _remaining, errors) = parse_cbl_process(source);
        assert_eq!(errors.len(), 1);
        assert!(errors[0].message.contains("Unrecognized"));
    }

    #[test]
    fn test_missing_paren() {
        let source = "CBL ARITH(EXTEND\n       ID DIVISION.\n";
        let (_, _remaining, errors) = parse_cbl_process(source);
        assert_eq!(errors.len(), 1);
        assert!(errors[0].message.contains("Missing closing parenthesis"));
    }

    #[test]
    fn test_case_insensitive() {
        let source = "cbl arith(extend),trunc(bin)\n       ID DIVISION.\n";
        let (opts, _, errors) = parse_cbl_process(source);
        assert!(errors.is_empty(), "Unexpected errors: {:?}", errors);
        assert_eq!(opts.arith, ArithMode::Extend);
        assert_eq!(opts.trunc, TruncMode::Bin);
    }

    #[test]
    fn test_trunc_std_truncates() {
        let opts = CompilerOptions { trunc: TruncMode::Std, ..Default::default() };
        assert!(should_truncate_to_pic(&opts));
    }

    #[test]
    fn test_trunc_bin_no_truncate() {
        let opts = CompilerOptions { trunc: TruncMode::Bin, ..Default::default() };
        assert!(!should_truncate_to_pic(&opts));
    }

    #[test]
    fn test_arith_compat_precision() {
        let opts = CompilerOptions::default();
        assert_eq!(max_precision_digits(&opts), 18);
    }

    #[test]
    fn test_arith_extend_precision() {
        let (opts, _, _) = parse_cbl_process("CBL ARITH(EXTEND)\n       ID DIVISION.\n");
        assert_eq!(max_precision_digits(&opts), 31);
    }

    #[test]
    fn test_blank_lines_before_cbl() {
        let source = "\n\nCBL TRUNC(BIN)\n       ID DIVISION.\n";
        let (opts, remaining, errors) = parse_cbl_process(source);
        assert!(errors.is_empty());
        assert_eq!(opts.trunc, TruncMode::Bin);
        assert!(remaining.contains("ID DIVISION"));
    }

    #[test]
    fn test_display_traits() {
        assert_eq!(ArithMode::Compat.to_string(), "COMPAT");
        assert_eq!(ArithMode::Extend.to_string(), "EXTEND");
        assert_eq!(TruncMode::Std.to_string(), "STD");
        assert_eq!(TruncMode::Opt.to_string(), "OPT");
        assert_eq!(TruncMode::Bin.to_string(), "BIN");
        assert_eq!(NumProc::Nopfd.to_string(), "NOPFD");
        assert_eq!(NumProc::Pfd.to_string(), "PFD");
        assert_eq!(NumProc::Mig.to_string(), "MIG");
        assert_eq!(NSymbol::Dbcs.to_string(), "DBCS");
        assert_eq!(NSymbol::National.to_string(), "NATIONAL");
        assert_eq!(IntDate::Ansi.to_string(), "ANSI");
        assert_eq!(IntDate::Lilian.to_string(), "LILIAN");
    }
}
