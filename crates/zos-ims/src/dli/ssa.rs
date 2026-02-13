//! SSA (Segment Search Argument) parsing.
//!
//! SSAs specify which segments to access in DL/I calls.
//! Format: segname*cmdcodes(qualification)
//!
//! Examples:
//! - CUSTOMER (unqualified)
//! - CUSTOMER*D (with command code D)
//! - CUSTOMER(CUSTNO  = 12345) (qualified)
//! - CUSTOMER*D(CUSTNO  >= 10000) (qualified with command code)

use crate::{ImsError, ImsResult};

/// A Segment Search Argument.
#[derive(Debug, Clone)]
pub struct Ssa {
    /// Segment name (8 characters)
    pub segment_name: String,
    /// Command codes
    pub command_codes: Vec<CommandCode>,
    /// Qualification (optional)
    pub qualification: Option<SsaQualification>,
}

impl Ssa {
    /// Create an unqualified SSA.
    pub fn unqualified(segment_name: &str) -> Self {
        Self {
            segment_name: segment_name.to_string(),
            command_codes: Vec::new(),
            qualification: None,
        }
    }

    /// Create a qualified SSA.
    pub fn qualified(segment_name: &str, field: &str, op: SsaOperator, value: &[u8]) -> Self {
        Self {
            segment_name: segment_name.to_string(),
            command_codes: Vec::new(),
            qualification: Some(SsaQualification {
                field_name: field.to_string(),
                operator: op,
                value: value.to_vec(),
            }),
        }
    }

    /// Add a command code.
    pub fn with_command_code(mut self, code: CommandCode) -> Self {
        self.command_codes.push(code);
        self
    }

    /// Parse an SSA from string.
    ///
    /// Format: segname*cmdcodes(field  op value)
    pub fn parse(s: &str) -> ImsResult<Self> {
        let s = s.trim();

        if s.is_empty() {
            return Err(ImsError::InvalidSsa("Empty SSA".to_string()));
        }

        // Split at command code marker
        let (before_qual, qual_part) = if let Some(paren_pos) = s.find('(') {
            (&s[..paren_pos], Some(&s[paren_pos..]))
        } else {
            (s, None)
        };

        // Parse segment name and command codes
        let (segment_name, command_codes) = if let Some(star_pos) = before_qual.find('*') {
            let name = before_qual[..star_pos].trim();
            let codes_str = &before_qual[star_pos + 1..];
            let codes: Vec<CommandCode> = codes_str
                .chars()
                .filter_map(CommandCode::from_char)
                .collect();
            (name.to_string(), codes)
        } else {
            (before_qual.trim().to_string(), Vec::new())
        };

        // Validate segment name
        if segment_name.len() > 8 {
            return Err(ImsError::InvalidSsa(format!(
                "Segment name too long: {}",
                segment_name
            )));
        }

        // Parse qualification
        let qualification = if let Some(qual_str) = qual_part {
            Some(parse_qualification(qual_str)?)
        } else {
            None
        };

        Ok(Self {
            segment_name,
            command_codes,
            qualification,
        })
    }

    /// Check if SSA is qualified.
    pub fn is_qualified(&self) -> bool {
        self.qualification.is_some()
    }

    /// Check if SSA has a specific command code.
    pub fn has_command_code(&self, code: CommandCode) -> bool {
        self.command_codes.contains(&code)
    }

    /// Convert to SSA string.
    pub fn to_string(&self) -> String {
        let mut result = self.segment_name.clone();

        if !self.command_codes.is_empty() {
            result.push('*');
            for code in &self.command_codes {
                result.push(code.to_char());
            }
        }

        if let Some(ref qual) = self.qualification {
            result.push_str(&format!(
                "({}{}{})",
                qual.field_name,
                qual.operator.to_str(),
                String::from_utf8_lossy(&qual.value)
            ));
        }

        result
    }
}

/// Parse qualification part of SSA.
fn parse_qualification(s: &str) -> ImsResult<SsaQualification> {
    // Remove parentheses
    let s = s.trim();
    let s = if s.starts_with('(') && s.ends_with(')') {
        &s[1..s.len() - 1]
    } else {
        s
    };

    // Find operator
    let operators = [">=", "<=", "!=", "<>", "=", ">", "<"];

    for op_str in &operators {
        if let Some(pos) = s.find(op_str) {
            let field = s[..pos].trim();
            let value = s[pos + op_str.len()..].trim();

            let operator = SsaOperator::from_str(op_str)
                .ok_or_else(|| ImsError::InvalidSsa(format!("Unknown operator: {}", op_str)))?;

            // Remove quotes from value if present
            let value = value.trim_matches('\'').trim_matches('"');

            return Ok(SsaQualification {
                field_name: field.to_string(),
                operator,
                value: value.as_bytes().to_vec(),
            });
        }
    }

    Err(ImsError::InvalidSsa(format!(
        "No operator found in qualification: {}",
        s
    )))
}

/// SSA qualification (field comparison).
#[derive(Debug, Clone)]
pub struct SsaQualification {
    /// Field name
    pub field_name: String,
    /// Comparison operator
    pub operator: SsaOperator,
    /// Comparison value
    pub value: Vec<u8>,
}

impl SsaQualification {
    /// Check if a field value matches this qualification.
    pub fn matches(&self, field_value: &[u8]) -> bool {
        match self.operator {
            SsaOperator::Eq => field_value == self.value.as_slice(),
            SsaOperator::Ne => field_value != self.value.as_slice(),
            SsaOperator::Gt => field_value > self.value.as_slice(),
            SsaOperator::Ge => field_value >= self.value.as_slice(),
            SsaOperator::Lt => field_value < self.value.as_slice(),
            SsaOperator::Le => field_value <= self.value.as_slice(),
        }
    }
}

/// SSA comparison operators.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SsaOperator {
    /// Equal
    Eq,
    /// Not equal
    Ne,
    /// Greater than
    Gt,
    /// Greater than or equal
    Ge,
    /// Less than
    Lt,
    /// Less than or equal
    Le,
}

impl SsaOperator {
    /// Parse from string.
    pub fn from_str(s: &str) -> Option<Self> {
        match s {
            "=" | "EQ" => Some(SsaOperator::Eq),
            "!=" | "<>" | "NE" => Some(SsaOperator::Ne),
            ">" | "GT" => Some(SsaOperator::Gt),
            ">=" | "GE" => Some(SsaOperator::Ge),
            "<" | "LT" => Some(SsaOperator::Lt),
            "<=" | "LE" => Some(SsaOperator::Le),
            _ => None,
        }
    }

    /// Convert to string.
    pub fn to_str(&self) -> &'static str {
        match self {
            SsaOperator::Eq => " = ",
            SsaOperator::Ne => " != ",
            SsaOperator::Gt => " > ",
            SsaOperator::Ge => " >= ",
            SsaOperator::Lt => " < ",
            SsaOperator::Le => " <= ",
        }
    }
}

/// SSA command codes.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CommandCode {
    /// D - Path call (retrieve segments along path)
    D,
    /// F - First occurrence
    F,
    /// L - Last occurrence
    L,
    /// N - Path call, ignore this segment's data
    N,
    /// P - Set parentage at this level
    P,
    /// Q - Enqueue segment for exclusive use
    Q,
    /// U - Maintain position at this level
    U,
    /// V - Maintain position at this level and all above
    V,
    /// - (minus) - Null command code
    Null,
}

impl CommandCode {
    /// Parse from character.
    pub fn from_char(c: char) -> Option<Self> {
        match c.to_ascii_uppercase() {
            'D' => Some(CommandCode::D),
            'F' => Some(CommandCode::F),
            'L' => Some(CommandCode::L),
            'N' => Some(CommandCode::N),
            'P' => Some(CommandCode::P),
            'Q' => Some(CommandCode::Q),
            'U' => Some(CommandCode::U),
            'V' => Some(CommandCode::V),
            '-' => Some(CommandCode::Null),
            _ => None,
        }
    }

    /// Convert to character.
    pub fn to_char(&self) -> char {
        match self {
            CommandCode::D => 'D',
            CommandCode::F => 'F',
            CommandCode::L => 'L',
            CommandCode::N => 'N',
            CommandCode::P => 'P',
            CommandCode::Q => 'Q',
            CommandCode::U => 'U',
            CommandCode::V => 'V',
            CommandCode::Null => '-',
        }
    }

    /// Get description.
    pub fn description(&self) -> &'static str {
        match self {
            CommandCode::D => "Path call",
            CommandCode::F => "First occurrence",
            CommandCode::L => "Last occurrence",
            CommandCode::N => "Path call ignore",
            CommandCode::P => "Set parentage",
            CommandCode::Q => "Enqueue",
            CommandCode::U => "Maintain position",
            CommandCode::V => "Maintain position at all levels",
            CommandCode::Null => "Null",
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_unqualified() {
        let ssa = Ssa::parse("CUSTOMER").unwrap();
        assert_eq!(ssa.segment_name, "CUSTOMER");
        assert!(!ssa.is_qualified());
        assert!(ssa.command_codes.is_empty());
    }

    #[test]
    fn test_parse_with_command_code() {
        let ssa = Ssa::parse("CUSTOMER*D").unwrap();
        assert_eq!(ssa.segment_name, "CUSTOMER");
        assert!(ssa.has_command_code(CommandCode::D));
    }

    #[test]
    fn test_parse_multiple_command_codes() {
        let ssa = Ssa::parse("ORDER*DP").unwrap();
        assert_eq!(ssa.segment_name, "ORDER");
        assert!(ssa.has_command_code(CommandCode::D));
        assert!(ssa.has_command_code(CommandCode::P));
    }

    #[test]
    fn test_parse_qualified() {
        let ssa = Ssa::parse("CUSTOMER(CUSTNO  = 12345)").unwrap();
        assert_eq!(ssa.segment_name, "CUSTOMER");
        assert!(ssa.is_qualified());

        let qual = ssa.qualification.unwrap();
        assert_eq!(qual.field_name, "CUSTNO");
        assert_eq!(qual.operator, SsaOperator::Eq);
        assert_eq!(qual.value, b"12345");
    }

    #[test]
    fn test_parse_qualified_with_command_code() {
        let ssa = Ssa::parse("CUSTOMER*D(CUSTNO  >= 10000)").unwrap();
        assert_eq!(ssa.segment_name, "CUSTOMER");
        assert!(ssa.has_command_code(CommandCode::D));

        let qual = ssa.qualification.unwrap();
        assert_eq!(qual.operator, SsaOperator::Ge);
    }

    #[test]
    fn test_operator_parse() {
        assert_eq!(SsaOperator::from_str("="), Some(SsaOperator::Eq));
        assert_eq!(SsaOperator::from_str(">="), Some(SsaOperator::Ge));
        assert_eq!(SsaOperator::from_str("<="), Some(SsaOperator::Le));
        assert_eq!(SsaOperator::from_str("!="), Some(SsaOperator::Ne));
        assert_eq!(SsaOperator::from_str("<>"), Some(SsaOperator::Ne));
    }

    #[test]
    fn test_qualification_matches() {
        let qual = SsaQualification {
            field_name: "AMOUNT".to_string(),
            operator: SsaOperator::Ge,
            value: b"100".to_vec(),
        };

        assert!(qual.matches(b"100"));
        assert!(qual.matches(b"200"));
        assert!(!qual.matches(b"050"));
    }

    #[test]
    fn test_command_code_parse() {
        assert_eq!(CommandCode::from_char('D'), Some(CommandCode::D));
        assert_eq!(CommandCode::from_char('d'), Some(CommandCode::D));
        assert_eq!(CommandCode::from_char('P'), Some(CommandCode::P));
        assert_eq!(CommandCode::from_char('X'), None);
    }

    #[test]
    fn test_ssa_to_string() {
        let ssa = Ssa::unqualified("CUSTOMER");
        assert_eq!(ssa.to_string(), "CUSTOMER");

        let ssa = Ssa::unqualified("ORDER").with_command_code(CommandCode::D);
        assert_eq!(ssa.to_string(), "ORDER*D");

        let ssa = Ssa::qualified("ITEM", "ITEMNO", SsaOperator::Eq, b"ABC");
        assert_eq!(ssa.to_string(), "ITEM(ITEMNO = ABC)");
    }

    #[test]
    fn test_builder_pattern() {
        let ssa = Ssa::unqualified("CUSTOMER")
            .with_command_code(CommandCode::D)
            .with_command_code(CommandCode::P);

        assert_eq!(ssa.command_codes.len(), 2);
    }
}
