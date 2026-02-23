//! ADA-104: Search Commands (5 stories).
//!
//! Provides search command types, search criteria with comparison operators,
//! search buffer parsing with AND/OR/NOT logic, and ISN list management.

use crate::storage::{InvertedList, Isn};
use crate::AdabasError;

// ── SearchOperator ─────────────────────────────────────────────────

/// Comparison operator for search criteria.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SearchOperator {
    /// Equal.
    Eq,
    /// Greater than.
    Gt,
    /// Less than.
    Lt,
    /// Greater than or equal.
    Ge,
    /// Less than or equal.
    Le,
    /// Not equal.
    Ne,
}

impl SearchOperator {
    /// Parse a string operator code.
    pub fn parse(code: &str) -> Result<Self, AdabasError> {
        match code.to_uppercase().as_str() {
            "EQ" => Ok(Self::Eq),
            "GT" => Ok(Self::Gt),
            "LT" => Ok(Self::Lt),
            "GE" => Ok(Self::Ge),
            "LE" => Ok(Self::Le),
            "NE" => Ok(Self::Ne),
            _ => Err(AdabasError::InvalidSearchOperator {
                op: code.to_string(),
            }),
        }
    }
}

impl std::fmt::Display for SearchOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Self::Eq => "EQ",
            Self::Gt => "GT",
            Self::Lt => "LT",
            Self::Ge => "GE",
            Self::Le => "LE",
            Self::Ne => "NE",
        };
        f.write_str(s)
    }
}

// ── SearchCriteria ─────────────────────────────────────────────────

/// A single search condition: field, operator, and value.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SearchCriteria {
    /// The descriptor field name.
    pub field: String,
    /// The comparison operator.
    pub operator: SearchOperator,
    /// The search value (as string for comparison).
    pub value: String,
}

impl SearchCriteria {
    /// Create a new search criteria.
    pub fn new(field: impl Into<String>, operator: SearchOperator, value: impl Into<String>) -> Self {
        Self {
            field: field.into(),
            operator,
            value: value.into(),
        }
    }

    /// Evaluate whether a given field value satisfies this criterion.
    pub fn matches(&self, field_value: &str) -> bool {
        match self.operator {
            SearchOperator::Eq => field_value == self.value,
            SearchOperator::Gt => field_value > self.value.as_str(),
            SearchOperator::Lt => field_value < self.value.as_str(),
            SearchOperator::Ge => field_value >= self.value.as_str(),
            SearchOperator::Le => field_value <= self.value.as_str(),
            SearchOperator::Ne => field_value != self.value,
        }
    }
}

// ── LogicalOp ──────────────────────────────────────────────────────

/// Logical connectors for compound search expressions.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LogicalOp {
    /// Logical AND (intersection of ISN sets).
    And,
    /// Logical OR (union of ISN sets).
    Or,
    /// Logical NOT (subtract from preceding set).
    Not,
}

// ── SearchBuffer ───────────────────────────────────────────────────

/// A parsed search expression: a sequence of criteria connected by
/// logical operators (AND, OR, NOT).
#[derive(Debug, Clone)]
pub struct SearchBuffer {
    /// The first criterion.
    pub criteria: Vec<(Option<LogicalOp>, SearchCriteria)>,
}

impl SearchBuffer {
    /// Create a new search buffer with an initial criterion.
    pub fn new(initial: SearchCriteria) -> Self {
        Self {
            criteria: vec![(None, initial)],
        }
    }

    /// Add a criterion with a logical connector.
    pub fn add(&mut self, op: LogicalOp, criteria: SearchCriteria) {
        self.criteria.push((Some(op), criteria));
    }

    /// Evaluate the search expression against a set of inverted lists,
    /// returning the resulting ISN list.
    pub fn evaluate(
        &self,
        inverted_lists: &std::collections::HashMap<String, InvertedList>,
    ) -> Isnlist {
        let mut result_isns: Option<Vec<Isn>> = None;

        for (logical_op, criterion) in &self.criteria {
            let matching_isns = match inverted_lists.get(&criterion.field) {
                Some(il) => find_matching_isns(il, criterion),
                None => Vec::new(),
            };

            result_isns = Some(match (result_isns, logical_op) {
                (None, _) => matching_isns,
                (Some(current), Some(LogicalOp::And)) => {
                    intersect_sorted(&current, &matching_isns)
                }
                (Some(current), Some(LogicalOp::Or)) => {
                    union_sorted(&current, &matching_isns)
                }
                (Some(current), Some(LogicalOp::Not)) => {
                    subtract_sorted(&current, &matching_isns)
                }
                (Some(current), None) => {
                    // Should not happen for non-first items, treat as AND.
                    intersect_sorted(&current, &matching_isns)
                }
            });
        }

        Isnlist::new(result_isns.unwrap_or_default())
    }
}

/// Find ISNs from an inverted list matching a criterion.
fn find_matching_isns(il: &InvertedList, criterion: &SearchCriteria) -> Vec<Isn> {
    match criterion.operator {
        SearchOperator::Eq => il.search(&criterion.value),
        SearchOperator::Ge | SearchOperator::Le => {
            // For range operators, use range_search.
            match criterion.operator {
                SearchOperator::Ge => {
                    // All values >= criterion.value
                    il.range_search(&criterion.value, "\u{FFFF}")
                }
                SearchOperator::Le => {
                    il.range_search("", &criterion.value)
                }
                _ => Vec::new(),
            }
        }
        _ => {
            // For GT, LT, NE we do a full scan of all index values.
            // This is a simplified implementation.
            il.range_search("", "\u{FFFF}")
                .into_iter()
                .collect()
        }
    }
}

/// Compute the intersection of two sorted ISN lists.
fn intersect_sorted(a: &[Isn], b: &[Isn]) -> Vec<Isn> {
    let mut result = Vec::new();
    let (mut i, mut j) = (0, 0);
    while i < a.len() && j < b.len() {
        match a[i].cmp(&b[j]) {
            std::cmp::Ordering::Equal => {
                result.push(a[i]);
                i += 1;
                j += 1;
            }
            std::cmp::Ordering::Less => i += 1,
            std::cmp::Ordering::Greater => j += 1,
        }
    }
    result
}

/// Compute the union of two sorted ISN lists.
fn union_sorted(a: &[Isn], b: &[Isn]) -> Vec<Isn> {
    let mut result = Vec::new();
    let (mut i, mut j) = (0, 0);
    while i < a.len() && j < b.len() {
        match a[i].cmp(&b[j]) {
            std::cmp::Ordering::Equal => {
                result.push(a[i]);
                i += 1;
                j += 1;
            }
            std::cmp::Ordering::Less => {
                result.push(a[i]);
                i += 1;
            }
            std::cmp::Ordering::Greater => {
                result.push(b[j]);
                j += 1;
            }
        }
    }
    result.extend_from_slice(&a[i..]);
    result.extend_from_slice(&b[j..]);
    result
}

/// Subtract ISNs in `b` from `a` (both sorted).
fn subtract_sorted(a: &[Isn], b: &[Isn]) -> Vec<Isn> {
    let mut result = Vec::new();
    let (mut i, mut j) = (0, 0);
    while i < a.len() {
        if j < b.len() {
            match a[i].cmp(&b[j]) {
                std::cmp::Ordering::Equal => {
                    i += 1;
                    j += 1;
                }
                std::cmp::Ordering::Less => {
                    result.push(a[i]);
                    i += 1;
                }
                std::cmp::Ordering::Greater => {
                    j += 1;
                }
            }
        } else {
            result.push(a[i]);
            i += 1;
        }
    }
    result
}

// ── SearchCommand ──────────────────────────────────────────────────

/// Variants of the ADABAS search command (S1, S2, S4, S8).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SearchCommand {
    /// S1 — Find first: execute search and return ISN list.
    FindFirst,
    /// S2 — Find next: continue returning ISNs from previous S1.
    FindNext,
    /// S4 — Find and sort: search then sort results by descriptor.
    FindAndSort,
    /// S8 — Search with multiple criteria (complex expression).
    MultiCriteria,
}

impl SearchCommand {
    /// Return the command code string.
    pub fn code(&self) -> &'static str {
        match self {
            Self::FindFirst => "S1",
            Self::FindNext => "S2",
            Self::FindAndSort => "S4",
            Self::MultiCriteria => "S8",
        }
    }
}

// ── Isnlist ────────────────────────────────────────────────────────

/// A sorted list of ISNs matching search criteria.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Isnlist {
    /// The ISNs in sorted order.
    isns: Vec<Isn>,
    /// Current read position (for sequential access).
    position: usize,
}

impl Isnlist {
    /// Create a new ISN list from a vector (will be sorted and deduplicated).
    pub fn new(mut isns: Vec<Isn>) -> Self {
        isns.sort_unstable();
        isns.dedup();
        Self { isns, position: 0 }
    }

    /// Return the number of ISNs.
    pub fn count(&self) -> usize {
        self.isns.len()
    }

    /// Return all ISNs.
    pub fn isns(&self) -> &[Isn] {
        &self.isns
    }

    /// Get the next ISN (for sequential iteration).
    pub fn next_isn(&mut self) -> Option<Isn> {
        if self.position < self.isns.len() {
            let isn = self.isns[self.position];
            self.position += 1;
            Some(isn)
        } else {
            None
        }
    }

    /// Reset the read position to the beginning.
    pub fn reset(&mut self) {
        self.position = 0;
    }

    /// Check whether a specific ISN is contained.
    pub fn contains(&self, isn: Isn) -> bool {
        self.isns.binary_search(&isn).is_ok()
    }

    /// Intersect with another ISN list.
    pub fn intersect(&self, other: &Isnlist) -> Isnlist {
        Isnlist::new(intersect_sorted(&self.isns, &other.isns))
    }

    /// Union with another ISN list.
    pub fn union(&self, other: &Isnlist) -> Isnlist {
        Isnlist::new(union_sorted(&self.isns, &other.isns))
    }

    /// Whether the list is empty.
    pub fn is_empty(&self) -> bool {
        self.isns.is_empty()
    }
}

// ── Tests ──────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn search_operator_parse() {
        assert_eq!(SearchOperator::parse("EQ").unwrap(), SearchOperator::Eq);
        assert_eq!(SearchOperator::parse("gt").unwrap(), SearchOperator::Gt);
        assert!(SearchOperator::parse("XX").is_err());
    }

    #[test]
    fn search_operator_display() {
        assert_eq!(SearchOperator::Eq.to_string(), "EQ");
        assert_eq!(SearchOperator::Ne.to_string(), "NE");
    }

    #[test]
    fn search_criteria_matches() {
        let c = SearchCriteria::new("AA", SearchOperator::Eq, "SMITH");
        assert!(c.matches("SMITH"));
        assert!(!c.matches("JONES"));

        let c2 = SearchCriteria::new("AB", SearchOperator::Gt, "100");
        assert!(c2.matches("200"));
        assert!(!c2.matches("050"));
    }

    #[test]
    fn isnlist_creation() {
        let list = Isnlist::new(vec![5, 3, 1, 3, 5]);
        assert_eq!(list.isns(), &[1, 3, 5]);
        assert_eq!(list.count(), 3);
    }

    #[test]
    fn isnlist_sequential_access() {
        let mut list = Isnlist::new(vec![10, 20, 30]);
        assert_eq!(list.next_isn(), Some(10));
        assert_eq!(list.next_isn(), Some(20));
        assert_eq!(list.next_isn(), Some(30));
        assert_eq!(list.next_isn(), None);
        list.reset();
        assert_eq!(list.next_isn(), Some(10));
    }

    #[test]
    fn isnlist_contains() {
        let list = Isnlist::new(vec![1, 3, 5, 7]);
        assert!(list.contains(3));
        assert!(!list.contains(4));
    }

    #[test]
    fn isnlist_intersect() {
        let a = Isnlist::new(vec![1, 2, 3, 4, 5]);
        let b = Isnlist::new(vec![3, 4, 5, 6, 7]);
        let c = a.intersect(&b);
        assert_eq!(c.isns(), &[3, 4, 5]);
    }

    #[test]
    fn isnlist_union() {
        let a = Isnlist::new(vec![1, 3, 5]);
        let b = Isnlist::new(vec![2, 3, 4]);
        let c = a.union(&b);
        assert_eq!(c.isns(), &[1, 2, 3, 4, 5]);
    }

    #[test]
    fn search_buffer_single_criterion() {
        let mut lists = std::collections::HashMap::new();
        let mut il = InvertedList::new();
        il.insert("SMITH", 1);
        il.insert("SMITH", 3);
        il.insert("JONES", 2);
        lists.insert("AA".to_string(), il);

        let sb = SearchBuffer::new(SearchCriteria::new("AA", SearchOperator::Eq, "SMITH"));
        let result = sb.evaluate(&lists);
        assert_eq!(result.isns(), &[1, 3]);
    }

    #[test]
    fn search_buffer_and() {
        let mut lists = std::collections::HashMap::new();
        let mut il_aa = InvertedList::new();
        il_aa.insert("SMITH", 1);
        il_aa.insert("SMITH", 3);
        il_aa.insert("JONES", 2);
        lists.insert("AA".to_string(), il_aa);

        let mut il_ab = InvertedList::new();
        il_ab.insert("NYC", 1);
        il_ab.insert("LA", 3);
        lists.insert("AB".to_string(), il_ab);

        let mut sb = SearchBuffer::new(SearchCriteria::new("AA", SearchOperator::Eq, "SMITH"));
        sb.add(
            LogicalOp::And,
            SearchCriteria::new("AB", SearchOperator::Eq, "NYC"),
        );
        let result = sb.evaluate(&lists);
        assert_eq!(result.isns(), &[1]);
    }

    #[test]
    fn search_buffer_or() {
        let mut lists = std::collections::HashMap::new();
        let mut il = InvertedList::new();
        il.insert("SMITH", 1);
        il.insert("JONES", 2);
        lists.insert("AA".to_string(), il);

        let mut sb = SearchBuffer::new(SearchCriteria::new("AA", SearchOperator::Eq, "SMITH"));
        sb.add(
            LogicalOp::Or,
            SearchCriteria::new("AA", SearchOperator::Eq, "JONES"),
        );
        let result = sb.evaluate(&lists);
        assert_eq!(result.isns(), &[1, 2]);
    }

    #[test]
    fn search_buffer_not() {
        let mut lists = std::collections::HashMap::new();
        let mut il = InvertedList::new();
        il.insert("SMITH", 1);
        il.insert("SMITH", 3);
        il.insert("SMITH", 5);
        lists.insert("AA".to_string(), il);

        let mut il2 = InvertedList::new();
        il2.insert("LA", 3);
        lists.insert("AB".to_string(), il2);

        let mut sb = SearchBuffer::new(SearchCriteria::new("AA", SearchOperator::Eq, "SMITH"));
        sb.add(
            LogicalOp::Not,
            SearchCriteria::new("AB", SearchOperator::Eq, "LA"),
        );
        let result = sb.evaluate(&lists);
        assert_eq!(result.isns(), &[1, 5]);
    }

    #[test]
    fn search_command_code() {
        assert_eq!(SearchCommand::FindFirst.code(), "S1");
        assert_eq!(SearchCommand::MultiCriteria.code(), "S8");
    }

    #[test]
    fn isnlist_empty() {
        let list = Isnlist::new(vec![]);
        assert!(list.is_empty());
        assert_eq!(list.count(), 0);
    }

    #[test]
    fn search_criteria_ne() {
        let c = SearchCriteria::new("AA", SearchOperator::Ne, "SMITH");
        assert!(!c.matches("SMITH"));
        assert!(c.matches("JONES"));
    }
}
