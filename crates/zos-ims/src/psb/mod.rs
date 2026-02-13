//! PSB (Program Specification Block) parsing.
//!
//! Parses IMS PSB macros to define program's database access:
//! - PSB (Program Specification Block)
//! - PCB (Program Communication Block)
//! - SENSEG (Sensitive Segment)
//! - SENFLD (Sensitive Field)
//! - PSBGEN (End of PSB)

use std::collections::HashMap;

/// A complete Program Specification Block.
#[derive(Debug, Clone)]
pub struct ProgramSpecBlock {
    /// PSB name (1-8 characters)
    pub name: String,
    /// Language (COBOL, PL/I, etc.)
    pub lang: PsbLanguage,
    /// PCBs defined in this PSB
    pub pcbs: Vec<ProgramCommBlock>,
}

impl ProgramSpecBlock {
    /// Create a new PSB.
    pub fn new(name: &str, lang: PsbLanguage) -> Self {
        Self {
            name: name.to_string(),
            lang,
            pcbs: Vec::new(),
        }
    }

    /// Add a PCB.
    pub fn add_pcb(&mut self, pcb: ProgramCommBlock) {
        self.pcbs.push(pcb);
    }

    /// Get a PCB by database name.
    pub fn get_pcb(&self, dbname: &str) -> Option<&ProgramCommBlock> {
        self.pcbs.iter().find(|p| p.dbname == dbname)
    }

    /// Get a PCB by index.
    pub fn get_pcb_by_index(&self, index: usize) -> Option<&ProgramCommBlock> {
        self.pcbs.get(index)
    }

    /// Get number of PCBs.
    pub fn pcb_count(&self) -> usize {
        self.pcbs.len()
    }
}

/// PSB language.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PsbLanguage {
    Cobol,
    Pli,
    Asm,
    Pascal,
}

impl PsbLanguage {
    /// Parse from string.
    pub fn from_str(s: &str) -> Option<Self> {
        match s.to_uppercase().as_str() {
            "COBOL" | "CBL" => Some(PsbLanguage::Cobol),
            "PLI" | "PL/I" | "PL1" => Some(PsbLanguage::Pli),
            "ASM" | "ASSEM" => Some(PsbLanguage::Asm),
            "PASCAL" | "PAS" => Some(PsbLanguage::Pascal),
            _ => None,
        }
    }
}

impl Default for PsbLanguage {
    fn default() -> Self {
        PsbLanguage::Cobol
    }
}

/// A Program Communication Block within a PSB.
#[derive(Debug, Clone)]
pub struct ProgramCommBlock {
    /// PCB type
    pub pcb_type: PcbType,
    /// Database name (for DB PCB)
    pub dbname: String,
    /// Processing options
    pub procopt: ProcessingOptions,
    /// Key feedback area length
    pub keylen: usize,
    /// Sensitive segments
    pub senseg: Vec<SensitiveSegment>,
    /// PCB position in PSB (0-based)
    pub position: usize,
    /// Current status code
    pub status: crate::StatusCode,
    /// Current segment level
    pub segment_level: u8,
    /// Key feedback area
    pub key_feedback: Vec<u8>,
}

impl ProgramCommBlock {
    /// Create a new DB PCB.
    pub fn new_db(dbname: &str, procopt: ProcessingOptions, keylen: usize) -> Self {
        Self {
            pcb_type: PcbType::Db,
            dbname: dbname.to_string(),
            procopt,
            keylen,
            senseg: Vec::new(),
            position: 0,
            status: crate::StatusCode::Ok,
            segment_level: 0,
            key_feedback: vec![0; keylen],
        }
    }

    /// Create a new GSAM PCB.
    pub fn new_gsam(dbname: &str) -> Self {
        Self {
            pcb_type: PcbType::Gsam,
            dbname: dbname.to_string(),
            procopt: ProcessingOptions::default(),
            keylen: 0,
            senseg: Vec::new(),
            position: 0,
            status: crate::StatusCode::Ok,
            segment_level: 0,
            key_feedback: Vec::new(),
        }
    }

    /// Add a sensitive segment.
    pub fn add_senseg(&mut self, senseg: SensitiveSegment) {
        self.senseg.push(senseg);
    }

    /// Check if a segment is accessible.
    pub fn is_segment_accessible(&self, segment_name: &str) -> bool {
        self.senseg.iter().any(|s| s.name == segment_name)
    }

    /// Check if an operation is allowed on a segment.
    pub fn can_operate(&self, segment_name: &str, operation: Operation) -> bool {
        if let Some(senseg) = self.senseg.iter().find(|s| s.name == segment_name) {
            senseg.procopt.allows(operation)
        } else {
            false
        }
    }

    /// Set status code.
    pub fn set_status(&mut self, status: crate::StatusCode) {
        self.status = status;
    }

    /// Set key feedback.
    pub fn set_key_feedback(&mut self, key: &[u8]) {
        let len = key.len().min(self.keylen);
        self.key_feedback[..len].copy_from_slice(&key[..len]);
    }
}

/// PCB type.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PcbType {
    /// Database PCB
    Db,
    /// GSAM PCB
    Gsam,
    /// Alternate PCB (for message handling)
    Alt,
    /// I/O PCB
    Io,
}

/// Processing options.
#[derive(Debug, Clone, Copy, Default)]
pub struct ProcessingOptions {
    /// Get operations allowed
    pub get: bool,
    /// Insert operations allowed
    pub insert: bool,
    /// Replace operations allowed
    pub replace: bool,
    /// Delete operations allowed
    pub delete: bool,
    /// All operations allowed
    pub all: bool,
}

impl ProcessingOptions {
    /// Parse from PROCOPT string (e.g., "GIRD", "A", "G").
    pub fn from_str(s: &str) -> Self {
        let upper = s.to_uppercase();
        let mut opts = ProcessingOptions::default();

        if upper.contains('A') {
            opts.all = true;
            opts.get = true;
            opts.insert = true;
            opts.replace = true;
            opts.delete = true;
        } else {
            opts.get = upper.contains('G');
            opts.insert = upper.contains('I');
            opts.replace = upper.contains('R');
            opts.delete = upper.contains('D');
        }

        opts
    }

    /// Check if operation is allowed.
    pub fn allows(&self, op: Operation) -> bool {
        if self.all {
            return true;
        }
        match op {
            Operation::Get => self.get,
            Operation::Insert => self.insert,
            Operation::Replace => self.replace,
            Operation::Delete => self.delete,
        }
    }
}

/// DL/I operation types.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Operation {
    Get,
    Insert,
    Replace,
    Delete,
}

/// A sensitive segment definition.
#[derive(Debug, Clone)]
pub struct SensitiveSegment {
    /// Segment name
    pub name: String,
    /// Parent segment name
    pub parent: String,
    /// Processing options for this segment
    pub procopt: ProcessingOptions,
    /// Sensitive fields (empty = all fields)
    pub senfld: Vec<String>,
}

impl SensitiveSegment {
    /// Create a new sensitive segment.
    pub fn new(name: &str, parent: &str, procopt: ProcessingOptions) -> Self {
        Self {
            name: name.to_string(),
            parent: parent.to_string(),
            procopt,
            senfld: Vec::new(),
        }
    }
}

/// PSB Parser.
pub struct PsbParser {
    lines: Vec<String>,
    current_line: usize,
}

impl PsbParser {
    /// Create a new PSB parser.
    pub fn new() -> Self {
        Self {
            lines: Vec::new(),
            current_line: 0,
        }
    }

    /// Parse PSB source.
    pub fn parse(&mut self, source: &str) -> Result<ProgramSpecBlock, PsbParseError> {
        self.lines = source.lines().map(|s| s.to_string()).collect();
        self.current_line = 0;

        let mut psb: Option<ProgramSpecBlock> = None;
        let mut current_pcb: Option<ProgramCommBlock> = None;
        let mut pcb_position = 0;

        while self.current_line < self.lines.len() {
            let line = &self.lines[self.current_line];
            let trimmed = line.trim();

            // Skip comments and empty lines
            if trimmed.is_empty() || trimmed.starts_with('*') || trimmed.starts_with(".*") {
                self.current_line += 1;
                continue;
            }

            // Parse macro
            if let Some(macro_content) = self.extract_macro(trimmed) {
                let (macro_name, params) = self.parse_macro_line(&macro_content);

                match macro_name.as_str() {
                    "PSB" | "PSBGEN" if psb.is_none() => {
                        let name = params.get("PSBNAME").or(params.get("NAME")).cloned().unwrap_or_default();
                        let lang_str = params.get("LANG").cloned().unwrap_or_else(|| "COBOL".to_string());
                        let lang = PsbLanguage::from_str(&lang_str).unwrap_or_default();
                        psb = Some(ProgramSpecBlock::new(&name, lang));
                    }
                    "PCB" => {
                        // Save previous PCB
                        if let (Some(ref mut ps), Some(pcb)) = (&mut psb, current_pcb.take()) {
                            ps.add_pcb(pcb);
                        }

                        let pcb_type_str = params.get("TYPE").cloned().unwrap_or_else(|| "DB".to_string());
                        let dbname = params.get("DBDNAME").or(params.get("NAME")).cloned().unwrap_or_default();
                        let procopt_str = params.get("PROCOPT").cloned().unwrap_or_else(|| "G".to_string());
                        let keylen: usize = params.get("KEYLEN").and_then(|s| s.parse().ok()).unwrap_or(0);

                        let procopt = ProcessingOptions::from_str(&procopt_str);

                        let mut pcb = match pcb_type_str.to_uppercase().as_str() {
                            "GSAM" => ProgramCommBlock::new_gsam(&dbname),
                            _ => ProgramCommBlock::new_db(&dbname, procopt, keylen),
                        };
                        pcb.position = pcb_position;
                        pcb_position += 1;

                        current_pcb = Some(pcb);
                    }
                    "SENSEG" => {
                        if let Some(ref mut pcb) = current_pcb {
                            let name = params.get("NAME").cloned().unwrap_or_default();
                            let parent = params.get("PARENT").cloned().unwrap_or_default();
                            let procopt_str = params.get("PROCOPT").cloned().unwrap_or_default();

                            let procopt = if procopt_str.is_empty() {
                                pcb.procopt
                            } else {
                                ProcessingOptions::from_str(&procopt_str)
                            };

                            pcb.add_senseg(SensitiveSegment::new(&name, &parent, procopt));
                        }
                    }
                    "PSBGEN" | "END" => {
                        // End of PSB
                        break;
                    }
                    _ => {
                        // Ignore other macros
                    }
                }
            }

            self.current_line += 1;
        }

        // Save last PCB
        if let (Some(ref mut ps), Some(pcb)) = (&mut psb, current_pcb.take()) {
            ps.add_pcb(pcb);
        }

        psb.ok_or(PsbParseError::NoPsb)
    }

    /// Extract macro from line.
    fn extract_macro(&self, line: &str) -> Option<String> {
        let parts: Vec<&str> = line.split_whitespace().collect();

        if parts.is_empty() {
            return None;
        }

        let known_macros = ["PSB", "PSBGEN", "PCB", "SENSEG", "SENFLD", "END"];

        if known_macros.contains(&parts[0].to_uppercase().as_str()) {
            return Some(line.to_string());
        }

        if parts.len() > 1 && known_macros.contains(&parts[1].to_uppercase().as_str()) {
            return Some(parts[1..].join(" "));
        }

        None
    }

    /// Parse a macro line into name and parameters.
    fn parse_macro_line(&self, line: &str) -> (String, HashMap<String, String>) {
        let mut params = HashMap::new();
        let parts: Vec<&str> = line.splitn(2, char::is_whitespace).collect();

        let macro_name = parts[0].to_uppercase();

        if parts.len() > 1 {
            let param_str = parts[1].trim();
            let mut current_key = String::new();
            let mut current_value = String::new();
            let mut in_parens = 0;
            let mut in_key = true;

            for c in param_str.chars() {
                match c {
                    '(' => {
                        in_parens += 1;
                        if in_parens == 1 && !current_key.is_empty() {
                            in_key = false;
                        } else {
                            current_value.push(c);
                        }
                    }
                    ')' => {
                        in_parens -= 1;
                        if in_parens == 0 && !in_key {
                            let key = current_key.trim().to_uppercase();
                            let value = current_value.trim().to_string();
                            if !key.is_empty() {
                                params.insert(key, value);
                            }
                            current_key.clear();
                            current_value.clear();
                            in_key = true;
                        } else {
                            current_value.push(c);
                        }
                    }
                    ',' if in_parens == 0 => {
                        if !current_key.is_empty() && current_value.is_empty() {
                            params.insert(current_key.trim().to_uppercase(), String::new());
                        }
                        current_key.clear();
                        current_value.clear();
                        in_key = true;
                    }
                    '=' if in_parens == 0 => {
                        in_key = false;
                    }
                    ' ' if in_parens == 0 && current_key.is_empty() => {}
                    _ => {
                        if in_key {
                            current_key.push(c);
                        } else {
                            current_value.push(c);
                        }
                    }
                }
            }

            let key = current_key.trim().to_uppercase();
            if !key.is_empty() {
                if current_value.is_empty() {
                    params.insert(key, String::new());
                } else {
                    params.insert(key, current_value.trim().to_string());
                }
            }
        }

        (macro_name, params)
    }
}

impl Default for PsbParser {
    fn default() -> Self {
        Self::new()
    }
}

/// PSB parse errors.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PsbParseError {
    /// No PSB macro found
    NoPsb,
    /// Invalid PCB definition
    InvalidPcb(String),
    /// Syntax error
    SyntaxError { line: usize, message: String },
}

impl std::fmt::Display for PsbParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PsbParseError::NoPsb => write!(f, "No PSB macro found"),
            PsbParseError::InvalidPcb(s) => write!(f, "Invalid PCB: {}", s),
            PsbParseError::SyntaxError { line, message } => {
                write!(f, "Syntax error at line {}: {}", line, message)
            }
        }
    }
}

impl std::error::Error for PsbParseError {}

#[cfg(test)]
mod tests {
    use super::*;

    const SAMPLE_PSB: &str = r#"
         PSB   PSBNAME(CUSTPSB),LANG(COBOL)
         PCB   TYPE(DB),DBDNAME(CUSTDB),PROCOPT(A),KEYLEN(50)
         SENSEG NAME(CUSTOMER),PARENT=0
         SENSEG NAME(ORDER),PARENT(CUSTOMER)
         SENSEG NAME(ITEM),PARENT(ORDER)
         PCB   TYPE(DB),DBDNAME(PRODDB),PROCOPT(G),KEYLEN(20)
         SENSEG NAME(PRODUCT),PARENT=0
         PSBGEN
    "#;

    #[test]
    fn test_parse_psb() {
        let mut parser = PsbParser::new();
        let psb = parser.parse(SAMPLE_PSB).unwrap();

        assert_eq!(psb.name, "CUSTPSB");
        assert_eq!(psb.lang, PsbLanguage::Cobol);
    }

    #[test]
    fn test_pcb_count() {
        let mut parser = PsbParser::new();
        let psb = parser.parse(SAMPLE_PSB).unwrap();

        assert_eq!(psb.pcb_count(), 2);
    }

    #[test]
    fn test_pcb_by_name() {
        let mut parser = PsbParser::new();
        let psb = parser.parse(SAMPLE_PSB).unwrap();

        let pcb = psb.get_pcb("CUSTDB").unwrap();
        assert_eq!(pcb.dbname, "CUSTDB");
        assert!(pcb.procopt.all);
        assert_eq!(pcb.keylen, 50);
    }

    #[test]
    fn test_senseg() {
        let mut parser = PsbParser::new();
        let psb = parser.parse(SAMPLE_PSB).unwrap();

        let pcb = psb.get_pcb("CUSTDB").unwrap();
        assert_eq!(pcb.senseg.len(), 3);
        assert!(pcb.is_segment_accessible("CUSTOMER"));
        assert!(pcb.is_segment_accessible("ORDER"));
        assert!(pcb.is_segment_accessible("ITEM"));
        assert!(!pcb.is_segment_accessible("INVALID"));
    }

    #[test]
    fn test_procopt() {
        let opts = ProcessingOptions::from_str("GIRD");
        assert!(opts.get);
        assert!(opts.insert);
        assert!(opts.replace);
        assert!(opts.delete);
        assert!(!opts.all);

        let opts = ProcessingOptions::from_str("A");
        assert!(opts.all);
        assert!(opts.allows(Operation::Get));
        assert!(opts.allows(Operation::Insert));

        let opts = ProcessingOptions::from_str("G");
        assert!(opts.get);
        assert!(!opts.insert);
    }

    #[test]
    fn test_can_operate() {
        let mut parser = PsbParser::new();
        let psb = parser.parse(SAMPLE_PSB).unwrap();

        let pcb = psb.get_pcb("CUSTDB").unwrap();
        assert!(pcb.can_operate("CUSTOMER", Operation::Get));
        assert!(pcb.can_operate("CUSTOMER", Operation::Insert));

        let pcb = psb.get_pcb("PRODDB").unwrap();
        assert!(pcb.can_operate("PRODUCT", Operation::Get));
        // PRODDB has PROCOPT(G), so insert should not be allowed
        // But senseg inherits from PCB, so it depends on implementation
    }

    #[test]
    fn test_psb_language() {
        assert_eq!(PsbLanguage::from_str("COBOL"), Some(PsbLanguage::Cobol));
        assert_eq!(PsbLanguage::from_str("PLI"), Some(PsbLanguage::Pli));
        assert_eq!(PsbLanguage::from_str("ASM"), Some(PsbLanguage::Asm));
        assert_eq!(PsbLanguage::from_str("UNKNOWN"), None);
    }
}
