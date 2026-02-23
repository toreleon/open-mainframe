//! # ACS (Automatic Class Selection) Routine Interpreter
//!
//! Implements the ACS routine scripting language used by SMS for automatic
//! class assignment at dataset allocation time.
//!
//! ## Language Elements
//!
//! - **FILTLIST** — named pattern lists for dataset name matching
//! - **SELECT/WHEN/OTHERWISE/END** — conditional logic
//! - **SET** — variable assignment (e.g., `SET &STORCLAS = 'SCFAST'`)
//! - **System Variables** — &DSN, &DSTYPE, &HLQ, &JOBNAME, etc.
//!
//! ## Routine Types
//!
//! Four routines are invoked in order at allocation:
//! 1. DATACLAS — assigns data class
//! 2. STORCLAS — assigns storage class
//! 3. MGMTCLAS — assigns management class
//! 4. STORGRP — assigns storage group

use std::collections::HashMap;

// ─────────────────────── ACS Context ───────────────────────

/// System variables available in ACS routines.
///
/// These are set by the system before ACS routine execution based on
/// the allocation request context.
#[derive(Debug, Clone, Default)]
pub struct AcsContext {
    /// Dataset name being allocated.
    pub dsn: String,
    /// Dataset type (VSAM, PDS, PS, HFS, etc.).
    pub dstype: String,
    /// High-level qualifier of the DSN.
    pub hlq: String,
    /// Job name.
    pub jobname: String,
    /// Step name.
    pub stepname: String,
    /// Unit specified in JCL.
    pub unit: String,
    /// DATACLAS from JCL (if specified).
    pub jcl_dataclas: Option<String>,
    /// STORCLAS from JCL (if specified).
    pub jcl_storclas: Option<String>,
    /// MGMTCLAS from JCL (if specified).
    pub jcl_mgmtclas: Option<String>,
}

impl AcsContext {
    /// Create a new ACS context for the given dataset name.
    pub fn new(dsn: &str) -> Self {
        let hlq = dsn
            .split('.')
            .next()
            .unwrap_or("")
            .to_uppercase();
        Self {
            dsn: dsn.to_uppercase(),
            hlq,
            ..Default::default()
        }
    }

    /// Builder: set dataset type.
    pub fn with_dstype(mut self, dstype: &str) -> Self {
        self.dstype = dstype.to_uppercase();
        self
    }

    /// Builder: set job name.
    pub fn with_jobname(mut self, jobname: &str) -> Self {
        self.jobname = jobname.to_uppercase();
        self
    }

    /// Builder: set step name.
    pub fn with_stepname(mut self, stepname: &str) -> Self {
        self.stepname = stepname.to_uppercase();
        self
    }

    /// Get the value of a system variable by name.
    pub fn get_var(&self, name: &str) -> String {
        match name.to_uppercase().as_str() {
            "&DSN" => self.dsn.clone(),
            "&DSTYPE" => self.dstype.clone(),
            "&HLQ" => self.hlq.clone(),
            "&JOBNAME" => self.jobname.clone(),
            "&STEPNAME" => self.stepname.clone(),
            "&UNIT" => self.unit.clone(),
            _ => String::new(),
        }
    }
}

// ─────────────────────── FILTLIST ───────────────────────

/// A named list of dataset name patterns for matching.
///
/// Patterns use `*` for single-qualifier wildcard and `**` for
/// multi-qualifier wildcard.
#[derive(Debug, Clone)]
pub struct FiltList {
    /// FILTLIST name.
    pub name: String,
    /// Include patterns.
    pub patterns: Vec<String>,
}

impl FiltList {
    /// Create a new FILTLIST with the given name and patterns.
    pub fn new(name: &str, patterns: Vec<String>) -> Self {
        Self {
            name: name.to_uppercase(),
            patterns: patterns.iter().map(|p| p.to_uppercase()).collect(),
        }
    }

    /// Check if a dataset name matches any pattern in this FILTLIST.
    pub fn matches(&self, dsn: &str) -> bool {
        let dsn_upper = dsn.to_uppercase();
        self.patterns.iter().any(|pat| pattern_match(pat, &dsn_upper))
    }
}

/// Match a dataset name against a pattern.
///
/// - `*` matches a single qualifier (no dots)
/// - `**` matches zero or more qualifiers (including dots)
fn pattern_match(pattern: &str, dsn: &str) -> bool {
    let pat_parts: Vec<&str> = pattern.split('.').collect();
    let dsn_parts: Vec<&str> = dsn.split('.').collect();
    match_parts(&pat_parts, &dsn_parts)
}

fn match_parts(pat: &[&str], dsn: &[&str]) -> bool {
    if pat.is_empty() {
        return dsn.is_empty();
    }
    if pat[0] == "**" {
        // ** matches zero or more qualifiers
        if match_parts(&pat[1..], dsn) {
            return true;
        }
        if !dsn.is_empty() {
            return match_parts(pat, &dsn[1..]);
        }
        return false;
    }
    if dsn.is_empty() {
        return false;
    }
    if pat[0] == "*" || pat[0] == dsn[0] {
        return match_parts(&pat[1..], &dsn[1..]);
    }
    false
}

// ─────────────────────── ACS Statements ───────────────────────

/// An ACS routine statement.
#[derive(Debug, Clone)]
pub enum AcsStatement {
    /// FILTLIST definition.
    FiltList(FiltList),
    /// SELECT block with WHEN conditions.
    Select {
        cases: Vec<WhenCase>,
        otherwise: Option<Vec<AcsStatement>>,
    },
    /// SET assignment: `SET &var = 'value'`
    Set { variable: String, value: String },
}

/// A WHEN case in a SELECT block.
#[derive(Debug, Clone)]
pub struct WhenCase {
    /// Condition to evaluate.
    pub condition: AcsCondition,
    /// Statements to execute if condition is true.
    pub body: Vec<AcsStatement>,
}

/// A condition in a WHEN clause.
#[derive(Debug, Clone)]
pub enum AcsCondition {
    /// Variable equals a string literal: `&DSN = 'SYS1.PARMLIB'`
    Equals { variable: String, value: String },
    /// Variable matches a FILTLIST: `&DSN = &FILTNAME`
    MatchesFiltList { variable: String, filtlist_name: String },
    /// Always true (for OTHERWISE-like WHEN).
    Always,
}

// ─────────────────────── ACS Routine Type ───────────────────────

/// The type of ACS routine (determines which construct is being assigned).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AcsRoutineType {
    /// Assigns DATACLAS.
    DataClas,
    /// Assigns STORCLAS.
    StorClas,
    /// Assigns MGMTCLAS.
    MgmtClas,
    /// Assigns STORGRP.
    StorGrp,
}

impl AcsRoutineType {
    /// The output variable name for this routine type.
    pub fn output_variable(&self) -> &str {
        match self {
            Self::DataClas => "&DATACLAS",
            Self::StorClas => "&STORCLAS",
            Self::MgmtClas => "&MGMTCLAS",
            Self::StorGrp => "&STORGRP",
        }
    }
}

// ─────────────────────── ACS Routine ───────────────────────

/// A parsed ACS routine.
#[derive(Debug, Clone)]
pub struct AcsRoutine {
    /// Routine type.
    pub routine_type: AcsRoutineType,
    /// Routine name.
    pub name: String,
    /// Statements in the routine body.
    pub statements: Vec<AcsStatement>,
}

/// Result of executing ACS routines.
#[derive(Debug, Clone, Default)]
pub struct AcsResult {
    /// Assigned data class (None if not assigned).
    pub dataclas: Option<String>,
    /// Assigned storage class.
    pub storclas: Option<String>,
    /// Assigned management class.
    pub mgmtclas: Option<String>,
    /// Assigned storage group.
    pub storgrp: Option<String>,
}

// ─────────────────────── ACS Engine ───────────────────────

/// ACS routine execution engine.
///
/// Holds registered routines and FILTLISTs. When allocation occurs,
/// all four routine types are invoked in order.
#[derive(Debug, Clone, Default)]
pub struct AcsEngine {
    /// Registered routines by type.
    routines: Vec<AcsRoutine>,
    /// Global FILTLISTs (shared across routines).
    filtlists: HashMap<String, FiltList>,
}

impl AcsEngine {
    /// Create a new ACS engine.
    pub fn new() -> Self {
        Self::default()
    }

    /// Register a FILTLIST.
    pub fn add_filtlist(&mut self, fl: FiltList) {
        self.filtlists.insert(fl.name.clone(), fl);
    }

    /// Register a routine.
    pub fn add_routine(&mut self, routine: AcsRoutine) {
        self.routines.push(routine);
    }

    /// Execute all ACS routines for the given allocation context.
    ///
    /// Routines are executed in order: DATACLAS → STORCLAS → MGMTCLAS → STORGRP.
    /// JCL-specified values take precedence over ACS assignments.
    pub fn execute(&self, ctx: &AcsContext) -> AcsResult {
        let mut result = AcsResult::default();
        let mut vars: HashMap<String, String> = HashMap::new();

        // Collect FILTLISTs from routines too
        let mut local_filtlists: HashMap<String, FiltList> = self.filtlists.clone();

        // Execute routines in type order
        let order = [
            AcsRoutineType::DataClas,
            AcsRoutineType::StorClas,
            AcsRoutineType::MgmtClas,
            AcsRoutineType::StorGrp,
        ];

        for rtype in &order {
            for routine in &self.routines {
                if routine.routine_type == *rtype {
                    execute_statements(&routine.statements, ctx, &mut vars, &mut local_filtlists);
                }
            }
        }

        // Extract assignments — JCL overrides ACS
        result.dataclas = ctx
            .jcl_dataclas
            .clone()
            .or_else(|| vars.get("&DATACLAS").cloned());
        result.storclas = ctx
            .jcl_storclas
            .clone()
            .or_else(|| vars.get("&STORCLAS").cloned());
        result.mgmtclas = ctx
            .jcl_mgmtclas
            .clone()
            .or_else(|| vars.get("&MGMTCLAS").cloned());
        result.storgrp = vars.get("&STORGRP").cloned();

        result
    }
}

fn execute_statements(
    stmts: &[AcsStatement],
    ctx: &AcsContext,
    vars: &mut HashMap<String, String>,
    filtlists: &mut HashMap<String, FiltList>,
) {
    for stmt in stmts {
        match stmt {
            AcsStatement::FiltList(fl) => {
                filtlists.insert(fl.name.clone(), fl.clone());
            }
            AcsStatement::Set { variable, value } => {
                vars.insert(variable.to_uppercase(), value.clone());
            }
            AcsStatement::Select { cases, otherwise } => {
                let mut matched = false;
                for case in cases {
                    if evaluate_condition(&case.condition, ctx, vars, filtlists) {
                        execute_statements(&case.body, ctx, vars, filtlists);
                        matched = true;
                        break;
                    }
                }
                if !matched {
                    if let Some(other) = otherwise {
                        execute_statements(other, ctx, vars, filtlists);
                    }
                }
            }
        }
    }
}

fn evaluate_condition(
    cond: &AcsCondition,
    ctx: &AcsContext,
    vars: &HashMap<String, String>,
    filtlists: &HashMap<String, FiltList>,
) -> bool {
    match cond {
        AcsCondition::Always => true,
        AcsCondition::Equals { variable, value } => {
            let var_val = resolve_variable(variable, ctx, vars);
            var_val.eq_ignore_ascii_case(value)
        }
        AcsCondition::MatchesFiltList {
            variable,
            filtlist_name,
        } => {
            let var_val = resolve_variable(variable, ctx, vars);
            // Strip & prefix for FILTLIST lookup (FILTLIST names don't have &)
            let fl_name = filtlist_name
                .strip_prefix('&')
                .unwrap_or(filtlist_name)
                .to_uppercase();
            if let Some(fl) = filtlists.get(&fl_name) {
                fl.matches(&var_val)
            } else {
                false
            }
        }
    }
}

fn resolve_variable(name: &str, ctx: &AcsContext, vars: &HashMap<String, String>) -> String {
    let upper = name.to_uppercase();
    // Check system variables first
    let sys_val = ctx.get_var(&upper);
    if !sys_val.is_empty() {
        return sys_val;
    }
    // Check user variables
    vars.get(&upper).cloned().unwrap_or_default()
}

// ─────────────────────── Simple Parser ───────────────────────

/// Parse a simple ACS routine source into statements.
///
/// Supports a subset of the ACS language:
/// - `FILTLIST name INCLUDE('pattern','pattern',...)`
/// - `SELECT`
/// - `WHEN (&var = 'value')` or `WHEN (&var = &filtlist)`
/// - `SET &var = 'value'`
/// - `OTHERWISE`
/// - `END`
pub fn parse_acs_source(source: &str) -> Vec<AcsStatement> {
    let lines: Vec<String> = source
        .lines()
        .map(|l| l.trim().to_string())
        .filter(|l| !l.is_empty() && !l.starts_with("/*"))
        .collect();

    let mut pos = 0;
    parse_block(&lines, &mut pos)
}

fn parse_block(lines: &[String], pos: &mut usize) -> Vec<AcsStatement> {
    let mut stmts = Vec::new();

    while *pos < lines.len() {
        let line = &lines[*pos];
        let upper = line.to_uppercase();

        if upper.starts_with("FILTLIST") {
            if let Some(fl) = parse_filtlist_line(line) {
                stmts.push(AcsStatement::FiltList(fl));
            }
            *pos += 1;
        } else if upper.starts_with("SET") {
            if let Some((var, val)) = parse_set_line(line) {
                stmts.push(AcsStatement::Set {
                    variable: var,
                    value: val,
                });
            }
            *pos += 1;
        } else if upper.starts_with("SELECT") {
            *pos += 1;
            let (cases, otherwise) = parse_select_body(lines, pos);
            stmts.push(AcsStatement::Select { cases, otherwise });
        } else if upper.starts_with("END") || upper.starts_with("OTHERWISE") || upper.starts_with("WHEN") {
            // These are handled by the caller
            break;
        } else {
            *pos += 1;
        }
    }

    stmts
}

fn parse_select_body(
    lines: &[String],
    pos: &mut usize,
) -> (Vec<WhenCase>, Option<Vec<AcsStatement>>) {
    let mut cases = Vec::new();
    let mut otherwise = None;

    while *pos < lines.len() {
        let upper = lines[*pos].to_uppercase();

        if upper.starts_with("WHEN") {
            let cond = parse_when_condition(&lines[*pos]);
            *pos += 1;
            let body = parse_block(lines, pos);
            cases.push(WhenCase {
                condition: cond,
                body,
            });
        } else if upper.starts_with("OTHERWISE") {
            *pos += 1;
            let body = parse_block(lines, pos);
            otherwise = Some(body);
        } else if upper.starts_with("END") {
            *pos += 1;
            break;
        } else {
            *pos += 1;
        }
    }

    (cases, otherwise)
}

fn parse_when_condition(line: &str) -> AcsCondition {
    // WHEN (&var = 'value') or WHEN (&var = &filtlist)
    let content = line.trim();
    // Strip WHEN and parentheses
    let inner = content
        .strip_prefix("WHEN")
        .or_else(|| content.strip_prefix("when"))
        .or_else(|| content.strip_prefix("When"))
        .unwrap_or(content)
        .trim();
    let inner = inner
        .strip_prefix('(')
        .unwrap_or(inner)
        .strip_suffix(')')
        .unwrap_or(inner)
        .trim();

    // Split on =
    if let Some(eq_pos) = inner.find('=') {
        let left = inner[..eq_pos].trim().to_uppercase();
        let right = inner[eq_pos + 1..].trim();

        if let Some(val) = right.strip_prefix('\'').and_then(|r| r.strip_suffix('\'')) {
            AcsCondition::Equals {
                variable: left,
                value: val.to_uppercase(),
            }
        } else if right.starts_with('&') {
            AcsCondition::MatchesFiltList {
                variable: left,
                filtlist_name: right.to_uppercase(),
            }
        } else {
            AcsCondition::Equals {
                variable: left,
                value: right.to_uppercase(),
            }
        }
    } else {
        AcsCondition::Always
    }
}

fn parse_filtlist_line(line: &str) -> Option<FiltList> {
    // FILTLIST name INCLUDE('pat1','pat2',...)
    let upper = line.to_uppercase();
    let parts: Vec<&str> = upper.split_whitespace().collect();
    if parts.len() < 3 {
        return None;
    }

    let name = parts[1].to_string();
    // Find the INCLUDE(...) part
    let include_pos = upper.find("INCLUDE")?;
    let rest = &upper[include_pos..];
    let start = rest.find('(')?;
    let end = rest.rfind(')')?;
    let patterns_str = &rest[start + 1..end];

    let patterns: Vec<String> = patterns_str
        .split(',')
        .map(|p| p.trim().trim_matches('\'').to_string())
        .filter(|p| !p.is_empty())
        .collect();

    Some(FiltList::new(&name, patterns))
}

fn parse_set_line(line: &str) -> Option<(String, String)> {
    // SET &var = 'value'
    let content = line.trim();
    let rest = content
        .strip_prefix("SET")
        .or_else(|| content.strip_prefix("set"))
        .or_else(|| content.strip_prefix("Set"))?
        .trim();

    let eq_pos = rest.find('=')?;
    let var = rest[..eq_pos].trim().to_uppercase();
    let val = rest[eq_pos + 1..].trim();
    let val = val
        .strip_prefix('\'')
        .and_then(|v| v.strip_suffix('\''))
        .unwrap_or(val)
        .to_uppercase();

    Some((var, val))
}

// ─────────────────────── Tests ───────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    // ─── DFSMS-101.1: ACS Routine Parser ───

    #[test]
    fn test_parse_filtlist() {
        let source = "FILTLIST PRODDSNS INCLUDE('PROD.**','PROD1.*')";
        let stmts = parse_acs_source(source);
        assert_eq!(stmts.len(), 1);
        if let AcsStatement::FiltList(fl) = &stmts[0] {
            assert_eq!(fl.name, "PRODDSNS");
            assert_eq!(fl.patterns.len(), 2);
        } else {
            panic!("Expected FiltList");
        }
    }

    #[test]
    fn test_parse_set() {
        let source = "SET &STORCLAS = 'SCFAST'";
        let stmts = parse_acs_source(source);
        assert_eq!(stmts.len(), 1);
        if let AcsStatement::Set { variable, value } = &stmts[0] {
            assert_eq!(variable, "&STORCLAS");
            assert_eq!(value, "SCFAST");
        } else {
            panic!("Expected Set");
        }
    }

    #[test]
    fn test_parse_select_when() {
        let source = r#"SELECT
WHEN (&HLQ = 'SYS1')
  SET &STORCLAS = 'SCFAST'
WHEN (&HLQ = 'TEST')
  SET &STORCLAS = 'SCSTD'
OTHERWISE
  SET &STORCLAS = 'SCDEFAULT'
END"#;
        let stmts = parse_acs_source(source);
        assert_eq!(stmts.len(), 1);
        if let AcsStatement::Select { cases, otherwise } = &stmts[0] {
            assert_eq!(cases.len(), 2);
            assert!(otherwise.is_some());
        } else {
            panic!("Expected Select");
        }
    }

    #[test]
    fn test_comments_ignored() {
        let source = "/* This is a comment */\nSET &STORCLAS = 'SC1'";
        let stmts = parse_acs_source(source);
        assert_eq!(stmts.len(), 1);
    }

    // ─── DFSMS-101.2: FILTLIST Pattern Matching ───

    #[test]
    fn test_filtlist_single_star() {
        let fl = FiltList::new("TEST", vec!["PROD.*".to_string()]);
        assert!(fl.matches("PROD.DATA"));
        assert!(!fl.matches("PROD.DATA.FILE")); // * only matches one qualifier
        assert!(!fl.matches("TEST.DATA"));
    }

    #[test]
    fn test_filtlist_double_star() {
        let fl = FiltList::new("TEST", vec!["PROD.**".to_string()]);
        assert!(fl.matches("PROD.DATA"));
        assert!(fl.matches("PROD.DATA.FILE"));
        assert!(fl.matches("PROD.A.B.C.D"));
        assert!(!fl.matches("TEST.DATA"));
    }

    #[test]
    fn test_filtlist_exact() {
        let fl = FiltList::new("TEST", vec!["SYS1.PARMLIB".to_string()]);
        assert!(fl.matches("SYS1.PARMLIB"));
        assert!(!fl.matches("SYS1.MACLIB"));
    }

    #[test]
    fn test_filtlist_multiple_patterns() {
        let fl = FiltList::new("TEST", vec!["PROD.**".to_string(), "PROD1.*".to_string()]);
        assert!(fl.matches("PROD.DATA.FILE"));
        assert!(fl.matches("PROD1.DATA"));
        assert!(!fl.matches("TEST.DATA"));
    }

    #[test]
    fn test_filtlist_case_insensitive() {
        let fl = FiltList::new("TEST", vec!["PROD.**".to_string()]);
        assert!(fl.matches("prod.data.file"));
    }

    // ─── DFSMS-101.3: SELECT/WHEN/DO/END Logic ───

    #[test]
    fn test_select_when_matches() {
        let mut engine = AcsEngine::new();

        let routine = AcsRoutine {
            routine_type: AcsRoutineType::StorClas,
            name: "STORCLAS".to_string(),
            statements: vec![AcsStatement::Select {
                cases: vec![
                    WhenCase {
                        condition: AcsCondition::Equals {
                            variable: "&HLQ".to_string(),
                            value: "SYS1".to_string(),
                        },
                        body: vec![AcsStatement::Set {
                            variable: "&STORCLAS".to_string(),
                            value: "SCFAST".to_string(),
                        }],
                    },
                    WhenCase {
                        condition: AcsCondition::Equals {
                            variable: "&HLQ".to_string(),
                            value: "TEST".to_string(),
                        },
                        body: vec![AcsStatement::Set {
                            variable: "&STORCLAS".to_string(),
                            value: "SCSTD".to_string(),
                        }],
                    },
                ],
                otherwise: Some(vec![AcsStatement::Set {
                    variable: "&STORCLAS".to_string(),
                    value: "SCDEFAULT".to_string(),
                }]),
            }],
        };

        engine.add_routine(routine);

        // SYS1 → SCFAST
        let ctx = AcsContext::new("SYS1.PARMLIB");
        let result = engine.execute(&ctx);
        assert_eq!(result.storclas.as_deref(), Some("SCFAST"));

        // TEST → SCSTD
        let ctx = AcsContext::new("TEST.DATA");
        let result = engine.execute(&ctx);
        assert_eq!(result.storclas.as_deref(), Some("SCSTD"));

        // OTHER → SCDEFAULT
        let ctx = AcsContext::new("PROD.DATA");
        let result = engine.execute(&ctx);
        assert_eq!(result.storclas.as_deref(), Some("SCDEFAULT"));
    }

    #[test]
    fn test_select_with_filtlist() {
        let mut engine = AcsEngine::new();

        engine.add_filtlist(FiltList::new(
            "PRODDSNS",
            vec!["PROD.**".to_string(), "PROD1.*".to_string()],
        ));

        let routine = AcsRoutine {
            routine_type: AcsRoutineType::StorClas,
            name: "STORCLAS".to_string(),
            statements: vec![AcsStatement::Select {
                cases: vec![WhenCase {
                    condition: AcsCondition::MatchesFiltList {
                        variable: "&DSN".to_string(),
                        filtlist_name: "&PRODDSNS".to_string(),
                    },
                    body: vec![AcsStatement::Set {
                        variable: "&STORCLAS".to_string(),
                        value: "SCFAST".to_string(),
                    }],
                }],
                otherwise: Some(vec![AcsStatement::Set {
                    variable: "&STORCLAS".to_string(),
                    value: "SCSTD".to_string(),
                }]),
            }],
        };

        engine.add_routine(routine);

        let ctx = AcsContext::new("PROD.DATA.FILE");
        let result = engine.execute(&ctx);
        assert_eq!(result.storclas.as_deref(), Some("SCFAST"));

        let ctx = AcsContext::new("TEST.DATA");
        let result = engine.execute(&ctx);
        assert_eq!(result.storclas.as_deref(), Some("SCSTD"));
    }

    // ─── DFSMS-101.4: System Variables ───

    #[test]
    fn test_system_variables() {
        let ctx = AcsContext::new("MY.DATA.FILE")
            .with_dstype("PS")
            .with_jobname("MYJOB");

        assert_eq!(ctx.get_var("&DSN"), "MY.DATA.FILE");
        assert_eq!(ctx.get_var("&DSTYPE"), "PS");
        assert_eq!(ctx.get_var("&HLQ"), "MY");
        assert_eq!(ctx.get_var("&JOBNAME"), "MYJOB");
    }

    #[test]
    fn test_hlq_extraction() {
        let ctx = AcsContext::new("SYS1.PARMLIB");
        assert_eq!(ctx.hlq, "SYS1");

        let ctx = AcsContext::new("PROD.DATA.FILE.BACKUP");
        assert_eq!(ctx.hlq, "PROD");
    }

    // ─── DFSMS-101.5: Four Routine Types ───

    #[test]
    fn test_four_routine_types() {
        let mut engine = AcsEngine::new();

        // DATACLAS routine
        engine.add_routine(AcsRoutine {
            routine_type: AcsRoutineType::DataClas,
            name: "DATACLAS".to_string(),
            statements: vec![AcsStatement::Set {
                variable: "&DATACLAS".to_string(),
                value: "DCSTD80".to_string(),
            }],
        });

        // STORCLAS routine
        engine.add_routine(AcsRoutine {
            routine_type: AcsRoutineType::StorClas,
            name: "STORCLAS".to_string(),
            statements: vec![AcsStatement::Set {
                variable: "&STORCLAS".to_string(),
                value: "SCFAST".to_string(),
            }],
        });

        // MGMTCLAS routine
        engine.add_routine(AcsRoutine {
            routine_type: AcsRoutineType::MgmtClas,
            name: "MGMTCLAS".to_string(),
            statements: vec![AcsStatement::Set {
                variable: "&MGMTCLAS".to_string(),
                value: "MCPROD".to_string(),
            }],
        });

        // STORGRP routine
        engine.add_routine(AcsRoutine {
            routine_type: AcsRoutineType::StorGrp,
            name: "STORGRP".to_string(),
            statements: vec![AcsStatement::Set {
                variable: "&STORGRP".to_string(),
                value: "SGPROD".to_string(),
            }],
        });

        let ctx = AcsContext::new("MY.DATA");
        let result = engine.execute(&ctx);

        assert_eq!(result.dataclas.as_deref(), Some("DCSTD80"));
        assert_eq!(result.storclas.as_deref(), Some("SCFAST"));
        assert_eq!(result.mgmtclas.as_deref(), Some("MCPROD"));
        assert_eq!(result.storgrp.as_deref(), Some("SGPROD"));
    }

    #[test]
    fn test_routine_no_set_returns_none() {
        let engine = AcsEngine::new(); // No routines
        let ctx = AcsContext::new("MY.DATA");
        let result = engine.execute(&ctx);

        assert!(result.dataclas.is_none());
        assert!(result.storclas.is_none());
        assert!(result.mgmtclas.is_none());
        assert!(result.storgrp.is_none());
    }

    #[test]
    fn test_jcl_overrides_acs() {
        let mut engine = AcsEngine::new();
        engine.add_routine(AcsRoutine {
            routine_type: AcsRoutineType::DataClas,
            name: "DATACLAS".to_string(),
            statements: vec![AcsStatement::Set {
                variable: "&DATACLAS".to_string(),
                value: "DCACS".to_string(),
            }],
        });

        let mut ctx = AcsContext::new("MY.DATA");
        ctx.jcl_dataclas = Some("DCJCL".to_string());

        let result = engine.execute(&ctx);
        assert_eq!(result.dataclas.as_deref(), Some("DCJCL")); // JCL wins
    }

    // ─── DFSMS-101.6: ACS-to-Allocation Integration ───

    #[test]
    fn test_acs_assigns_dataclas() {
        let mut engine = AcsEngine::new();

        engine.add_filtlist(FiltList::new("SYSDSNS", vec!["SYS1.**".to_string()]));

        engine.add_routine(AcsRoutine {
            routine_type: AcsRoutineType::DataClas,
            name: "DATACLAS".to_string(),
            statements: vec![AcsStatement::Select {
                cases: vec![WhenCase {
                    condition: AcsCondition::MatchesFiltList {
                        variable: "&DSN".to_string(),
                        filtlist_name: "&SYSDSNS".to_string(),
                    },
                    body: vec![AcsStatement::Set {
                        variable: "&DATACLAS".to_string(),
                        value: "DCSYS".to_string(),
                    }],
                }],
                otherwise: Some(vec![AcsStatement::Set {
                    variable: "&DATACLAS".to_string(),
                    value: "DCSTD80".to_string(),
                }]),
            }],
        });

        let ctx = AcsContext::new("SYS1.PARMLIB");
        let result = engine.execute(&ctx);
        assert_eq!(result.dataclas.as_deref(), Some("DCSYS"));

        let ctx = AcsContext::new("MY.DATA");
        let result = engine.execute(&ctx);
        assert_eq!(result.dataclas.as_deref(), Some("DCSTD80"));
    }

    // ─── DFSMS-101.7: Full ACS Tests ───

    #[test]
    fn test_complex_acs_scenario() {
        let mut engine = AcsEngine::new();

        engine.add_filtlist(FiltList::new(
            "PRODDSNS",
            vec!["PROD.**".to_string()],
        ));
        engine.add_filtlist(FiltList::new(
            "TESTDSNS",
            vec!["TEST.**".to_string(), "DEV.**".to_string()],
        ));
        engine.add_filtlist(FiltList::new(
            "SYSDSNS",
            vec!["SYS1.**".to_string(), "SYS2.**".to_string()],
        ));

        // STORCLAS routine with 5 WHEN conditions
        engine.add_routine(AcsRoutine {
            routine_type: AcsRoutineType::StorClas,
            name: "STORCLAS".to_string(),
            statements: vec![AcsStatement::Select {
                cases: vec![
                    WhenCase {
                        condition: AcsCondition::MatchesFiltList {
                            variable: "&DSN".to_string(),
                            filtlist_name: "&SYSDSNS".to_string(),
                        },
                        body: vec![AcsStatement::Set {
                            variable: "&STORCLAS".to_string(),
                            value: "SCSYS".to_string(),
                        }],
                    },
                    WhenCase {
                        condition: AcsCondition::MatchesFiltList {
                            variable: "&DSN".to_string(),
                            filtlist_name: "&PRODDSNS".to_string(),
                        },
                        body: vec![AcsStatement::Set {
                            variable: "&STORCLAS".to_string(),
                            value: "SCPROD".to_string(),
                        }],
                    },
                    WhenCase {
                        condition: AcsCondition::MatchesFiltList {
                            variable: "&DSN".to_string(),
                            filtlist_name: "&TESTDSNS".to_string(),
                        },
                        body: vec![AcsStatement::Set {
                            variable: "&STORCLAS".to_string(),
                            value: "SCTEST".to_string(),
                        }],
                    },
                    WhenCase {
                        condition: AcsCondition::Equals {
                            variable: "&DSTYPE".to_string(),
                            value: "VSAM".to_string(),
                        },
                        body: vec![AcsStatement::Set {
                            variable: "&STORCLAS".to_string(),
                            value: "SCVSAM".to_string(),
                        }],
                    },
                ],
                otherwise: Some(vec![AcsStatement::Set {
                    variable: "&STORCLAS".to_string(),
                    value: "SCDEFAULT".to_string(),
                }]),
            }],
        });

        // Test 10 dataset names
        let test_cases = vec![
            ("SYS1.PARMLIB", "SCSYS"),
            ("SYS2.MACLIB", "SCSYS"),
            ("PROD.DATA.FILE", "SCPROD"),
            ("PROD.BACKUP.DAILY", "SCPROD"),
            ("TEST.UNIT.DATA", "SCTEST"),
            ("DEV.SANDBOX.FILE", "SCTEST"),
            ("USER.PERSONAL", "SCDEFAULT"),
            ("MISC.DATA", "SCDEFAULT"),
        ];

        for (dsn, expected) in test_cases {
            let ctx = AcsContext::new(dsn);
            let result = engine.execute(&ctx);
            assert_eq!(
                result.storclas.as_deref(),
                Some(expected),
                "DSN={dsn} expected STORCLAS={expected}"
            );
        }

        // VSAM type override
        let ctx = AcsContext::new("USER.VSAM.KSDS").with_dstype("VSAM");
        let result = engine.execute(&ctx);
        assert_eq!(result.storclas.as_deref(), Some("SCVSAM"));
    }

    #[test]
    fn test_parse_and_execute() {
        let source = r#"FILTLIST PRODDSNS INCLUDE('PROD.**')
SELECT
WHEN (&DSN = &PRODDSNS)
  SET &STORCLAS = 'SCFAST'
OTHERWISE
  SET &STORCLAS = 'SCDEFAULT'
END"#;

        let stmts = parse_acs_source(source);

        let mut engine = AcsEngine::new();
        // Extract FILTLISTs and build a routine from parsed statements
        let mut routine_stmts = Vec::new();
        for stmt in stmts {
            match &stmt {
                AcsStatement::FiltList(fl) => engine.add_filtlist(fl.clone()),
                _ => routine_stmts.push(stmt),
            }
        }

        engine.add_routine(AcsRoutine {
            routine_type: AcsRoutineType::StorClas,
            name: "STORCLAS".to_string(),
            statements: routine_stmts,
        });

        let ctx = AcsContext::new("PROD.DATA.FILE");
        let result = engine.execute(&ctx);
        assert_eq!(result.storclas.as_deref(), Some("SCFAST"));

        let ctx = AcsContext::new("TEST.DATA");
        let result = engine.execute(&ctx);
        assert_eq!(result.storclas.as_deref(), Some("SCDEFAULT"));
    }

    #[test]
    fn test_routine_type_output_variables() {
        assert_eq!(AcsRoutineType::DataClas.output_variable(), "&DATACLAS");
        assert_eq!(AcsRoutineType::StorClas.output_variable(), "&STORCLAS");
        assert_eq!(AcsRoutineType::MgmtClas.output_variable(), "&MGMTCLAS");
        assert_eq!(AcsRoutineType::StorGrp.output_variable(), "&STORGRP");
    }
}
