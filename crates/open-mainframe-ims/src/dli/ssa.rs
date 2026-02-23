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
                additional_clauses: Vec::new(),
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
    #[allow(clippy::inherent_to_string)]
    pub fn to_string(&self) -> String {
        let mut result = self.segment_name.clone();

        if !self.command_codes.is_empty() {
            result.push('*');
            for code in &self.command_codes {
                result.push(code.to_char());
            }
        }

        if let Some(ref qual) = self.qualification {
            result.push('(');
            result.push_str(&format!(
                "{}{}{}",
                qual.field_name,
                qual.operator.to_str(),
                String::from_utf8_lossy(&qual.value)
            ));
            for (connector, clause) in &qual.additional_clauses {
                let conn_str = match connector {
                    BooleanConnector::And => " *AND ",
                    BooleanConnector::Or => " *OR ",
                };
                result.push_str(&format!(
                    "{}{}{}{}",
                    conn_str,
                    clause.field_name,
                    clause.operator.to_str(),
                    String::from_utf8_lossy(&clause.value)
                ));
            }
            result.push(')');
        }

        result
    }
}

/// Parse a single qualification clause from a string.
fn parse_single_clause(s: &str) -> ImsResult<QualificationClause> {
    let s = s.trim();
    let operators = [">=", "<=", "!=", "<>", "=", ">", "<"];

    for op_str in &operators {
        if let Some(pos) = s.find(op_str) {
            let field = s[..pos].trim();
            let value = s[pos + op_str.len()..].trim();

            let operator = SsaOperator::from_str(op_str)
                .ok_or_else(|| ImsError::InvalidSsa(format!("Unknown operator: {}", op_str)))?;

            let value = value.trim_matches('\'').trim_matches('"');

            return Ok(QualificationClause {
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

/// Parse qualification part of SSA (supports *AND and *OR connectors).
fn parse_qualification(s: &str) -> ImsResult<SsaQualification> {
    // Remove parentheses
    let s = s.trim();
    let s = if s.starts_with('(') && s.ends_with(')') {
        &s[1..s.len() - 1]
    } else {
        s
    };

    // Split on *AND and *OR connectors
    // We need to find these tokens and split the string
    let mut parts: Vec<(Option<BooleanConnector>, &str)> = Vec::new();
    let mut remaining = s;

    // Extract the first clause (no connector)
    if let Some(and_pos) = remaining.find("*AND") {
        parts.push((None, &remaining[..and_pos]));
        remaining = &remaining[and_pos..];
    } else if let Some(or_pos) = remaining.find("*OR") {
        parts.push((None, &remaining[..or_pos]));
        remaining = &remaining[or_pos..];
    } else {
        // Simple qualification — no connectors
        let clause = parse_single_clause(s)?;
        return Ok(SsaQualification {
            field_name: clause.field_name,
            operator: clause.operator,
            value: clause.value,
            additional_clauses: Vec::new(),
        });
    }

    // Parse remaining connectors and clauses
    while !remaining.is_empty() {
        let remaining_trimmed = remaining.trim();
        if let Some(after) = remaining_trimmed.strip_prefix("*AND") {
            // Find next connector
            let next_and = after.find("*AND");
            let next_or = after.find("*OR");
            let end = match (next_and, next_or) {
                (Some(a), Some(o)) => a.min(o),
                (Some(a), None) => a,
                (None, Some(o)) => o,
                (None, None) => after.len(),
            };
            parts.push((Some(BooleanConnector::And), &after[..end]));
            remaining = &after[end..];
        } else if let Some(after) = remaining_trimmed.strip_prefix("*OR") {
            let next_and = after.find("*AND");
            let next_or = after.find("*OR");
            let end = match (next_and, next_or) {
                (Some(a), Some(o)) => a.min(o),
                (Some(a), None) => a,
                (None, Some(o)) => o,
                (None, None) => after.len(),
            };
            parts.push((Some(BooleanConnector::Or), &after[..end]));
            remaining = &after[end..];
        } else {
            break;
        }
    }

    // Parse all clauses
    if parts.is_empty() {
        return Err(ImsError::InvalidSsa("Empty qualification".to_string()));
    }

    let first_clause = parse_single_clause(parts[0].1)?;
    let mut additional = Vec::new();

    for &(connector, clause_str) in &parts[1..] {
        let connector = connector.ok_or_else(|| {
            ImsError::InvalidSsa("Missing connector".to_string())
        })?;
        let clause = parse_single_clause(clause_str)?;
        additional.push((connector, clause));
    }

    Ok(SsaQualification {
        field_name: first_clause.field_name,
        operator: first_clause.operator,
        value: first_clause.value,
        additional_clauses: additional,
    })
}

/// Boolean connector for compound qualifications.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BooleanConnector {
    /// AND has higher precedence than OR
    And,
    /// OR has lower precedence
    Or,
}

/// A single qualification clause (field op value).
#[derive(Debug, Clone)]
pub struct QualificationClause {
    /// Field name
    pub field_name: String,
    /// Comparison operator
    pub operator: SsaOperator,
    /// Comparison value
    pub value: Vec<u8>,
}

impl QualificationClause {
    /// Check if a field value matches this clause.
    pub fn matches_field(&self, field_value: &[u8]) -> bool {
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

/// SSA qualification (field comparison, possibly compound).
#[derive(Debug, Clone)]
pub struct SsaQualification {
    /// Field name (primary clause for backward compatibility)
    pub field_name: String,
    /// Comparison operator (primary clause)
    pub operator: SsaOperator,
    /// Comparison value (primary clause)
    pub value: Vec<u8>,
    /// Additional clauses connected by boolean operators.
    /// Each entry is (connector, clause). The first clause is the primary fields above.
    pub additional_clauses: Vec<(BooleanConnector, QualificationClause)>,
}

impl SsaQualification {
    /// Check if a field value matches this qualification (single-field, backward compat).
    pub fn matches(&self, field_value: &[u8]) -> bool {
        if self.additional_clauses.is_empty() {
            return self.primary_matches(field_value);
        }
        // For compound qualifications, use matches_multi
        // Fall back to primary match if only one field is involved
        self.primary_matches(field_value)
    }

    /// Check the primary (first) clause.
    fn primary_matches(&self, field_value: &[u8]) -> bool {
        match self.operator {
            SsaOperator::Eq => field_value == self.value.as_slice(),
            SsaOperator::Ne => field_value != self.value.as_slice(),
            SsaOperator::Gt => field_value > self.value.as_slice(),
            SsaOperator::Ge => field_value >= self.value.as_slice(),
            SsaOperator::Lt => field_value < self.value.as_slice(),
            SsaOperator::Le => field_value <= self.value.as_slice(),
        }
    }

    /// Check if a set of field values matches this compound qualification.
    ///
    /// The `fields` callback returns the value for a given field name.
    /// Evaluates with AND having higher precedence than OR (IBM IMS behavior).
    pub fn matches_multi<F>(&self, get_field: F) -> bool
    where
        F: Fn(&str) -> Option<Vec<u8>>,
    {
        // Build all clauses: primary + additional
        let primary = QualificationClause {
            field_name: self.field_name.clone(),
            operator: self.operator,
            value: self.value.clone(),
        };

        if self.additional_clauses.is_empty() {
            let val = get_field(&primary.field_name);
            return val.as_ref().map(|v| primary.matches_field(v)).unwrap_or(false);
        }

        // Evaluate with AND precedence > OR:
        // Split into OR-groups of AND-connected clauses
        // e.g., A AND B OR C AND D => (A AND B) OR (C AND D)
        let mut or_groups: Vec<Vec<&QualificationClause>> = Vec::new();
        let mut current_and_group: Vec<&QualificationClause> = vec![&primary];

        for (connector, clause) in &self.additional_clauses {
            match connector {
                BooleanConnector::And => {
                    current_and_group.push(clause);
                }
                BooleanConnector::Or => {
                    or_groups.push(current_and_group);
                    current_and_group = vec![clause];
                }
            }
        }
        or_groups.push(current_and_group);

        // Any OR group must be fully satisfied (all ANDs within it)
        or_groups.iter().any(|and_group| {
            and_group.iter().all(|clause| {
                let val = get_field(&clause.field_name);
                val.as_ref().map(|v| clause.matches_field(v)).unwrap_or(false)
            })
        })
    }

    /// Get the number of clauses (1 = simple, >1 = compound).
    pub fn clause_count(&self) -> usize {
        1 + self.additional_clauses.len()
    }

    /// Check if this is a compound (boolean) qualification.
    pub fn is_compound(&self) -> bool {
        !self.additional_clauses.is_empty()
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
    #[allow(clippy::should_implement_trait)]
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
            additional_clauses: Vec::new(),
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

    // -----------------------------------------------------------------------
    // Epic 408: Boolean SSA Qualification Tests
    // -----------------------------------------------------------------------

    #[test]
    fn test_parse_and_qualification() {
        let ssa = Ssa::parse("CUSTOMER(CUSTNO >= 10000 *AND CUSTNO <= 20000)").unwrap();
        let qual = ssa.qualification.unwrap();

        assert_eq!(qual.field_name, "CUSTNO");
        assert_eq!(qual.operator, SsaOperator::Ge);
        assert_eq!(qual.value, b"10000");
        assert_eq!(qual.additional_clauses.len(), 1);
        assert_eq!(qual.additional_clauses[0].0, BooleanConnector::And);
        assert_eq!(qual.additional_clauses[0].1.field_name, "CUSTNO");
        assert_eq!(qual.additional_clauses[0].1.operator, SsaOperator::Le);
        assert_eq!(qual.additional_clauses[0].1.value, b"20000");
    }

    #[test]
    fn test_parse_or_qualification() {
        let ssa = Ssa::parse("ORDER(STATUS = OPEN *OR STATUS = PENDING)").unwrap();
        let qual = ssa.qualification.unwrap();

        assert_eq!(qual.field_name, "STATUS");
        assert_eq!(qual.value, b"OPEN");
        assert_eq!(qual.additional_clauses.len(), 1);
        assert_eq!(qual.additional_clauses[0].0, BooleanConnector::Or);
        assert_eq!(qual.additional_clauses[0].1.value, b"PENDING");
    }

    #[test]
    fn test_parse_mixed_and_or() {
        let ssa = Ssa::parse("ITEM(QTY > 0 *AND QTY < 100 *OR STATUS = SPECIAL)").unwrap();
        let qual = ssa.qualification.unwrap();

        assert_eq!(qual.clause_count(), 3);
        assert!(qual.is_compound());
        assert_eq!(qual.additional_clauses[0].0, BooleanConnector::And);
        assert_eq!(qual.additional_clauses[1].0, BooleanConnector::Or);
    }

    #[test]
    fn test_matches_multi_and() {
        let qual = SsaQualification {
            field_name: "CUSTNO".to_string(),
            operator: SsaOperator::Ge,
            value: b"10000".to_vec(),
            additional_clauses: vec![
                (BooleanConnector::And, QualificationClause {
                    field_name: "CUSTNO".to_string(),
                    operator: SsaOperator::Le,
                    value: b"20000".to_vec(),
                }),
            ],
        };

        // 15000 is between 10000 and 20000
        assert!(qual.matches_multi(|f| {
            if f == "CUSTNO" { Some(b"15000".to_vec()) } else { None }
        }));

        // 05000 is below range
        assert!(!qual.matches_multi(|f| {
            if f == "CUSTNO" { Some(b"05000".to_vec()) } else { None }
        }));

        // 25000 is above range
        assert!(!qual.matches_multi(|f| {
            if f == "CUSTNO" { Some(b"25000".to_vec()) } else { None }
        }));
    }

    #[test]
    fn test_matches_multi_or() {
        let qual = SsaQualification {
            field_name: "STATUS".to_string(),
            operator: SsaOperator::Eq,
            value: b"OPEN".to_vec(),
            additional_clauses: vec![
                (BooleanConnector::Or, QualificationClause {
                    field_name: "STATUS".to_string(),
                    operator: SsaOperator::Eq,
                    value: b"PENDING".to_vec(),
                }),
            ],
        };

        assert!(qual.matches_multi(|f| {
            if f == "STATUS" { Some(b"OPEN".to_vec()) } else { None }
        }));
        assert!(qual.matches_multi(|f| {
            if f == "STATUS" { Some(b"PENDING".to_vec()) } else { None }
        }));
        assert!(!qual.matches_multi(|f| {
            if f == "STATUS" { Some(b"CLOSED".to_vec()) } else { None }
        }));
    }

    #[test]
    fn test_matches_multi_and_or_precedence() {
        // A=1 AND B=2 OR C=3 => (A=1 AND B=2) OR (C=3)
        let qual = SsaQualification {
            field_name: "A".to_string(),
            operator: SsaOperator::Eq,
            value: b"1".to_vec(),
            additional_clauses: vec![
                (BooleanConnector::And, QualificationClause {
                    field_name: "B".to_string(),
                    operator: SsaOperator::Eq,
                    value: b"2".to_vec(),
                }),
                (BooleanConnector::Or, QualificationClause {
                    field_name: "C".to_string(),
                    operator: SsaOperator::Eq,
                    value: b"3".to_vec(),
                }),
            ],
        };

        // A=1, B=2 => first AND group matches
        assert!(qual.matches_multi(|f| match f {
            "A" => Some(b"1".to_vec()),
            "B" => Some(b"2".to_vec()),
            "C" => Some(b"9".to_vec()),
            _ => None,
        }));

        // C=3 => second OR group matches
        assert!(qual.matches_multi(|f| match f {
            "A" => Some(b"9".to_vec()),
            "B" => Some(b"9".to_vec()),
            "C" => Some(b"3".to_vec()),
            _ => None,
        }));

        // A=1, B=9, C=9 => neither group matches
        assert!(!qual.matches_multi(|f| match f {
            "A" => Some(b"1".to_vec()),
            "B" => Some(b"9".to_vec()),
            "C" => Some(b"9".to_vec()),
            _ => None,
        }));
    }

    #[test]
    fn test_compound_to_string() {
        let ssa = Ssa::parse("CUSTOMER(CUSTNO >= 10000 *AND CUSTNO <= 20000)").unwrap();
        let s = ssa.to_string();
        assert!(s.contains("*AND"));
        assert!(s.contains("CUSTNO"));
    }

    #[test]
    fn test_simple_qualification_not_compound() {
        let ssa = Ssa::parse("CUSTOMER(CUSTNO = 12345)").unwrap();
        let qual = ssa.qualification.unwrap();
        assert!(!qual.is_compound());
        assert_eq!(qual.clause_count(), 1);
    }
}
