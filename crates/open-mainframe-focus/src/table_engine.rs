//! FOC-102: TABLE Request Engine (6 stories).
//!
//! Executes TABLE requests against in-memory data producing report output
//! with PRINT (detail), SUM (aggregation), BY (group-by), ACROSS (pivot),
//! WHERE (filter), and COMPUTE (virtual fields).

use std::collections::HashMap;
use thiserror::Error;

use crate::parser::{CompOp, ComputedField, Expr, TableRequest, TableVerb, WhereClause};

// ---------------------------------------------------------------------------
// Errors
// ---------------------------------------------------------------------------

#[derive(Debug, Error)]
pub enum TableError {
    #[error("field not found: {0}")]
    FieldNotFound(String),
    #[error("type mismatch: {0}")]
    TypeMismatch(String),
    #[error("evaluation error: {0}")]
    EvalError(String),
}

// ---------------------------------------------------------------------------
// Data representation
// ---------------------------------------------------------------------------

/// A single cell value in the report.
#[derive(Debug, Clone, PartialEq)]
pub enum CellValue {
    Str(String),
    Num(f64),
    Null,
}

impl CellValue {
    pub fn as_num(&self) -> f64 {
        match self {
            CellValue::Num(n) => *n,
            CellValue::Str(s) => s.parse().unwrap_or(0.0),
            CellValue::Null => 0.0,
        }
    }

    pub fn as_str(&self) -> String {
        match self {
            CellValue::Str(s) => s.clone(),
            CellValue::Num(n) => format!("{n}"),
            CellValue::Null => String::new(),
        }
    }
}

/// A data record — maps field names to values.
pub type DataRecord = HashMap<String, CellValue>;

/// A single row in the report output.
#[derive(Debug, Clone, PartialEq)]
pub struct ReportRow {
    pub row_type: RowType,
    pub cells: Vec<(String, CellValue)>,
}

/// Type of row in the output.
#[derive(Debug, Clone, PartialEq)]
pub enum RowType {
    Heading,
    Detail,
    Subtotal,
    Total,
    Footing,
    Subfoot,
}

/// Complete report output.
#[derive(Debug, Clone)]
pub struct ReportOutput {
    pub heading: Option<String>,
    pub footing: Option<String>,
    pub subfoot: Option<String>,
    pub columns: Vec<String>,
    pub rows: Vec<ReportRow>,
}

// ---------------------------------------------------------------------------
// Table engine
// ---------------------------------------------------------------------------

/// Aggregation operation for TABLE verbs.
#[derive(Debug, Clone, PartialEq)]
enum AggOp {
    Sum,
    Count,
    Avg,
    Max,
    Min,
    Pct,
    First,
    Last,
}

/// Executes TABLE requests against in-memory data.
pub struct TableEngine;

impl TableEngine {
    /// Execute a TABLE request against the given data records.
    pub fn execute(
        request: &TableRequest,
        data: &[DataRecord],
    ) -> Result<ReportOutput, TableError> {
        // 1. Filter with WHERE clauses
        let filtered = Self::apply_where(data, &request.where_clauses)?;

        // 2. Apply COMPUTE fields
        let enriched = Self::apply_computes(filtered, &request.computes)?;

        // 3. Determine output columns
        let mut columns: Vec<String> = request.by_dims.clone();
        columns.extend(request.fields.clone());
        for c in &request.computes {
            columns.push(c.name.clone());
        }

        // 4. Execute based on verb
        let rows = match request.verb {
            TableVerb::Print => Self::execute_print(&enriched, &columns, &request.by_dims)?,
            TableVerb::Sum => Self::execute_agg(
                &enriched, &request.fields, &request.computes,
                &request.by_dims, &request.across_dims, AggOp::Sum,
            )?,
            TableVerb::Count => Self::execute_agg(
                &enriched, &request.fields, &request.computes,
                &request.by_dims, &request.across_dims, AggOp::Count,
            )?,
            TableVerb::Avg => Self::execute_agg(
                &enriched, &request.fields, &request.computes,
                &request.by_dims, &request.across_dims, AggOp::Avg,
            )?,
            TableVerb::Max => Self::execute_agg(
                &enriched, &request.fields, &request.computes,
                &request.by_dims, &request.across_dims, AggOp::Max,
            )?,
            TableVerb::Min => Self::execute_agg(
                &enriched, &request.fields, &request.computes,
                &request.by_dims, &request.across_dims, AggOp::Min,
            )?,
            TableVerb::Pct => Self::execute_agg(
                &enriched, &request.fields, &request.computes,
                &request.by_dims, &request.across_dims, AggOp::Pct,
            )?,
            TableVerb::First => Self::execute_agg(
                &enriched, &request.fields, &request.computes,
                &request.by_dims, &request.across_dims, AggOp::First,
            )?,
            TableVerb::Last => Self::execute_agg(
                &enriched, &request.fields, &request.computes,
                &request.by_dims, &request.across_dims, AggOp::Last,
            )?,
        };

        Ok(ReportOutput {
            heading: request.heading.clone(),
            footing: request.footing.clone(),
            subfoot: request.subfoot.clone(),
            columns,
            rows,
        })
    }

    /// Filter records by WHERE clauses.
    fn apply_where(
        data: &[DataRecord],
        wheres: &[WhereClause],
    ) -> Result<Vec<DataRecord>, TableError> {
        let mut result = Vec::new();
        for rec in data {
            let mut passes = true;
            for wh in wheres {
                let val = rec
                    .get(&wh.field)
                    .cloned()
                    .unwrap_or(CellValue::Null);
                let cmp_val = Self::eval_expr_simple(&wh.value);
                if !Self::compare(&val, &wh.op, &cmp_val) {
                    passes = false;
                    break;
                }
            }
            if passes {
                result.push(rec.clone());
            }
        }
        Ok(result)
    }

    /// Add computed fields to each record.
    fn apply_computes(
        mut data: Vec<DataRecord>,
        computes: &[ComputedField],
    ) -> Result<Vec<DataRecord>, TableError> {
        for rec in &mut data {
            for comp in computes {
                let val = Self::eval_expr(&comp.expr, rec)?;
                rec.insert(comp.name.clone(), val);
            }
        }
        Ok(data)
    }

    /// PRINT verb: detail listing — one output row per input record.
    fn execute_print(
        data: &[DataRecord],
        columns: &[String],
        by_dims: &[String],
    ) -> Result<Vec<ReportRow>, TableError> {
        let mut rows = Vec::new();

        if by_dims.is_empty() {
            // Simple detail listing
            for rec in data {
                let cells: Vec<(String, CellValue)> = columns
                    .iter()
                    .map(|c| {
                        let v = rec.get(c).cloned().unwrap_or(CellValue::Null);
                        (c.clone(), v)
                    })
                    .collect();
                rows.push(ReportRow {
                    row_type: RowType::Detail,
                    cells,
                });
            }
        } else {
            // Group by dimension — sorted detail
            let groups = Self::group_by(data, by_dims);
            for (_key, group_recs) in &groups {
                for rec in group_recs {
                    let cells: Vec<(String, CellValue)> = columns
                        .iter()
                        .map(|c| {
                            let v = rec.get(c).cloned().unwrap_or(CellValue::Null);
                            (c.clone(), v)
                        })
                        .collect();
                    rows.push(ReportRow {
                        row_type: RowType::Detail,
                        cells,
                    });
                }
            }
        }

        Ok(rows)
    }

    /// Aggregation operation type.
    fn aggregate_values(values: &[f64], op: &AggOp, grand_total: f64) -> f64 {
        match op {
            AggOp::Sum => values.iter().sum(),
            AggOp::Count => values.len() as f64,
            AggOp::Avg => {
                if values.is_empty() {
                    0.0
                } else {
                    values.iter().sum::<f64>() / values.len() as f64
                }
            }
            AggOp::Max => values.iter().cloned().fold(f64::NEG_INFINITY, f64::max),
            AggOp::Min => values.iter().cloned().fold(f64::INFINITY, f64::min),
            AggOp::Pct => {
                let sum: f64 = values.iter().sum();
                if grand_total.abs() < f64::EPSILON {
                    0.0
                } else {
                    (sum / grand_total) * 100.0
                }
            }
            AggOp::First => values.first().copied().unwrap_or(0.0),
            AggOp::Last => values.last().copied().unwrap_or(0.0),
        }
    }

    /// Generic aggregation verb: SUM, COUNT, AVG, MAX, MIN, PCT, FIRST, LAST.
    fn execute_agg(
        data: &[DataRecord],
        fields: &[String],
        computes: &[ComputedField],
        by_dims: &[String],
        across_dims: &[String],
        agg_op: AggOp,
    ) -> Result<Vec<ReportRow>, TableError> {
        let mut rows = Vec::new();
        let measure_fields: Vec<String> = fields
            .iter()
            .chain(computes.iter().map(|c| &c.name))
            .cloned()
            .collect();

        // Compute grand total for PCT calculations
        let grand_total_for_pct: f64 = measure_fields.iter().map(|f| {
            data.iter().map(|r| r.get(f).map_or(0.0, |v| v.as_num())).sum::<f64>()
        }).sum();

        if by_dims.is_empty() && across_dims.is_empty() {
            // Grand total
            let mut cells = Vec::new();
            for f in &measure_fields {
                let values: Vec<f64> = data
                    .iter()
                    .map(|r| r.get(f).map_or(0.0, |v| v.as_num()))
                    .collect();
                let result = Self::aggregate_values(&values, &agg_op, grand_total_for_pct);
                cells.push((f.clone(), CellValue::Num(result)));
            }
            rows.push(ReportRow {
                row_type: RowType::Total,
                cells,
            });
        } else if across_dims.is_empty() {
            // BY grouping only
            let groups = Self::group_by(data, by_dims);
            let mut grand_totals: Vec<f64> = vec![0.0; measure_fields.len()];

            for (key, group_recs) in &groups {
                let mut cells = Vec::new();
                for (i, dim) in by_dims.iter().enumerate() {
                    let dim_val = key.get(i).cloned().unwrap_or_default();
                    cells.push((dim.clone(), CellValue::Str(dim_val)));
                }
                for (j, f) in measure_fields.iter().enumerate() {
                    let values: Vec<f64> = group_recs
                        .iter()
                        .map(|r| r.get(f).map_or(0.0, |v| v.as_num()))
                        .collect();
                    let result = Self::aggregate_values(&values, &agg_op, grand_total_for_pct);
                    grand_totals[j] += values.iter().sum::<f64>();
                    cells.push((f.clone(), CellValue::Num(result)));
                }
                rows.push(ReportRow {
                    row_type: RowType::Subtotal,
                    cells,
                });
            }

            // Grand total row
            let mut total_cells = Vec::new();
            for dim in by_dims {
                total_cells.push((dim.clone(), CellValue::Str("TOTAL".to_string())));
            }
            for (j, f) in measure_fields.iter().enumerate() {
                total_cells.push((f.clone(), CellValue::Num(grand_totals[j])));
            }
            rows.push(ReportRow {
                row_type: RowType::Total,
                cells: total_cells,
            });
        } else {
            // ACROSS (pivot) — cross-tabulation
            let across_field = &across_dims[0];
            let mut across_values: Vec<String> = Vec::new();
            for rec in data {
                let v = rec.get(across_field).map_or(String::new(), |v| v.as_str());
                if !across_values.contains(&v) {
                    across_values.push(v);
                }
            }

            let groups = Self::group_by(data, by_dims);
            for (key, group_recs) in &groups {
                let mut cells = Vec::new();
                for (i, dim) in by_dims.iter().enumerate() {
                    let dim_val = key.get(i).cloned().unwrap_or_default();
                    cells.push((dim.clone(), CellValue::Str(dim_val)));
                }
                for av in &across_values {
                    let matching: Vec<&DataRecord> = group_recs
                        .iter()
                        .filter(|r| {
                            r.get(across_field).map_or(false, |v| v.as_str() == *av)
                        })
                        .collect();
                    for f in &measure_fields {
                        let values: Vec<f64> = matching
                            .iter()
                            .map(|r| r.get(f).map_or(0.0, |v| v.as_num()))
                            .collect();
                        let result = Self::aggregate_values(&values, &agg_op, grand_total_for_pct);
                        let col_name = format!("{f}_{av}");
                        cells.push((col_name, CellValue::Num(result)));
                    }
                }
                rows.push(ReportRow {
                    row_type: RowType::Subtotal,
                    cells,
                });
            }
        }

        Ok(rows)
    }

    /// Group records by dimension fields.
    fn group_by(data: &[DataRecord], dims: &[String]) -> Vec<(Vec<String>, Vec<DataRecord>)> {
        let mut map: Vec<(Vec<String>, Vec<DataRecord>)> = Vec::new();
        for rec in data {
            let key: Vec<String> = dims
                .iter()
                .map(|d| rec.get(d).map_or(String::new(), |v| v.as_str()))
                .collect();
            if let Some(entry) = map.iter_mut().find(|(k, _)| *k == key) {
                entry.1.push(rec.clone());
            } else {
                map.push((key, vec![rec.clone()]));
            }
        }
        map
    }

    fn compare(left: &CellValue, op: &CompOp, right: &CellValue) -> bool {
        let ln = left.as_num();
        let rn = right.as_num();
        // Try numeric comparison first, fall back to string
        let (use_num, ls, rs) = match (left, right) {
            (CellValue::Num(_), _) | (_, CellValue::Num(_)) => (true, String::new(), String::new()),
            _ => (false, left.as_str(), right.as_str()),
        };

        if use_num {
            match op {
                CompOp::Eq => (ln - rn).abs() < f64::EPSILON,
                CompOp::Ne => (ln - rn).abs() >= f64::EPSILON,
                CompOp::Gt => ln > rn,
                CompOp::Lt => ln < rn,
                CompOp::Ge => ln >= rn,
                CompOp::Le => ln <= rn,
            }
        } else {
            match op {
                CompOp::Eq => ls == rs,
                CompOp::Ne => ls != rs,
                CompOp::Gt => ls > rs,
                CompOp::Lt => ls < rs,
                CompOp::Ge => ls >= rs,
                CompOp::Le => ls <= rs,
            }
        }
    }

    fn eval_expr_simple(expr: &Expr) -> CellValue {
        match expr {
            Expr::Literal(n) => CellValue::Num(*n),
            Expr::StringLiteral(s) => CellValue::Str(s.clone()),
            Expr::Field(f) => CellValue::Str(f.clone()),
            Expr::Var(v) => CellValue::Str(v.clone()),
            _ => CellValue::Null,
        }
    }

    fn eval_expr(expr: &Expr, rec: &DataRecord) -> Result<CellValue, TableError> {
        match expr {
            Expr::Literal(n) => Ok(CellValue::Num(*n)),
            Expr::StringLiteral(s) => Ok(CellValue::Str(s.clone())),
            Expr::Field(name) => Ok(rec.get(name).cloned().unwrap_or(CellValue::Null)),
            Expr::Var(name) => Ok(rec.get(name).cloned().unwrap_or(CellValue::Null)),
            Expr::BinOp { left, op, right } => {
                let l = Self::eval_expr(left, rec)?.as_num();
                let r = Self::eval_expr(right, rec)?.as_num();
                let result = match op {
                    '+' => l + r,
                    '-' => l - r,
                    '*' => l * r,
                    '/' => {
                        if r.abs() < f64::EPSILON {
                            0.0
                        } else {
                            l / r
                        }
                    }
                    _ => {
                        return Err(TableError::EvalError(format!("unknown op: {op}")));
                    }
                };
                Ok(CellValue::Num(result))
            }
            Expr::FunctionCall { name, .. } => Err(TableError::EvalError(format!(
                "function calls not supported in table engine: {name}"
            ))),
        }
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::{CompOp, ComputedField, Expr, TableRequest, TableVerb, WhereClause};

    fn sample_data() -> Vec<DataRecord> {
        vec![
            HashMap::from([
                ("NAME".into(), CellValue::Str("Alice".into())),
                ("SALARY".into(), CellValue::Num(60000.0)),
                ("DEPT".into(), CellValue::Str("ENG".into())),
                ("YEAR".into(), CellValue::Str("2024".into())),
            ]),
            HashMap::from([
                ("NAME".into(), CellValue::Str("Bob".into())),
                ("SALARY".into(), CellValue::Num(55000.0)),
                ("DEPT".into(), CellValue::Str("ENG".into())),
                ("YEAR".into(), CellValue::Str("2025".into())),
            ]),
            HashMap::from([
                ("NAME".into(), CellValue::Str("Carol".into())),
                ("SALARY".into(), CellValue::Num(70000.0)),
                ("DEPT".into(), CellValue::Str("SALES".into())),
                ("YEAR".into(), CellValue::Str("2024".into())),
            ]),
            HashMap::from([
                ("NAME".into(), CellValue::Str("Dave".into())),
                ("SALARY".into(), CellValue::Num(50000.0)),
                ("DEPT".into(), CellValue::Str("SALES".into())),
                ("YEAR".into(), CellValue::Str("2025".into())),
            ]),
            HashMap::from([
                ("NAME".into(), CellValue::Str("Eve".into())),
                ("SALARY".into(), CellValue::Num(80000.0)),
                ("DEPT".into(), CellValue::Str("ENG".into())),
                ("YEAR".into(), CellValue::Str("2024".into())),
            ]),
        ]
    }

    fn make_table_request(
        verb: TableVerb,
        fields: Vec<&str>,
        by_dims: Vec<&str>,
        across_dims: Vec<&str>,
    ) -> TableRequest {
        TableRequest {
            file: "TEST".into(),
            verb,
            fields: fields.into_iter().map(String::from).collect(),
            by_dims: by_dims.into_iter().map(String::from).collect(),
            across_dims: across_dims.into_iter().map(String::from).collect(),
            where_clauses: Vec::new(),
            computes: Vec::new(),
            heading: None,
            footing: None,
            subfoot: None,
        }
    }

    #[test]
    fn test_print_simple() {
        let req = make_table_request(TableVerb::Print, vec!["NAME", "SALARY"], vec![], vec![]);
        let output = TableEngine::execute(&req, &sample_data()).unwrap();
        assert_eq!(output.rows.len(), 5);
        assert!(output.rows.iter().all(|r| r.row_type == RowType::Detail));
    }

    #[test]
    fn test_print_columns() {
        let req = make_table_request(TableVerb::Print, vec!["NAME"], vec![], vec![]);
        let output = TableEngine::execute(&req, &sample_data()).unwrap();
        assert_eq!(output.columns, vec!["NAME"]);
        assert_eq!(output.rows[0].cells[0].1, CellValue::Str("Alice".into()));
    }

    #[test]
    fn test_sum_grand_total() {
        let req = make_table_request(TableVerb::Sum, vec!["SALARY"], vec![], vec![]);
        let output = TableEngine::execute(&req, &sample_data()).unwrap();
        assert_eq!(output.rows.len(), 1);
        assert_eq!(output.rows[0].row_type, RowType::Total);
        let total = output.rows[0].cells[0].1.as_num();
        assert!((total - 315000.0).abs() < f64::EPSILON);
    }

    #[test]
    fn test_sum_by_dept() {
        let req = make_table_request(TableVerb::Sum, vec!["SALARY"], vec!["DEPT"], vec![]);
        let output = TableEngine::execute(&req, &sample_data()).unwrap();
        // Two groups + grand total
        assert_eq!(output.rows.len(), 3);
        // Find ENG subtotal
        let eng_row = output.rows.iter().find(|r| {
            r.cells.iter().any(|(_, v)| v.as_str() == "ENG")
        }).unwrap();
        let eng_salary = eng_row.cells.iter().find(|(k, _)| k == "SALARY").unwrap();
        assert!((eng_salary.1.as_num() - 195000.0).abs() < f64::EPSILON);
    }

    #[test]
    fn test_sum_by_with_grand_total() {
        let req = make_table_request(TableVerb::Sum, vec!["SALARY"], vec!["DEPT"], vec![]);
        let output = TableEngine::execute(&req, &sample_data()).unwrap();
        let total_row = output.rows.last().unwrap();
        assert_eq!(total_row.row_type, RowType::Total);
        let grand = total_row.cells.iter().find(|(k, _)| k == "SALARY").unwrap();
        assert!((grand.1.as_num() - 315000.0).abs() < f64::EPSILON);
    }

    #[test]
    fn test_across_pivot() {
        let req = make_table_request(
            TableVerb::Sum,
            vec!["SALARY"],
            vec!["DEPT"],
            vec!["YEAR"],
        );
        let output = TableEngine::execute(&req, &sample_data()).unwrap();
        assert!(!output.rows.is_empty());
        // Check that pivoted columns exist
        let first = &output.rows[0];
        let col_names: Vec<&str> = first.cells.iter().map(|(k, _)| k.as_str()).collect();
        // Should have DEPT, plus SALARY_2024 and/or SALARY_2025
        assert!(col_names.iter().any(|c| c.starts_with("SALARY_")));
    }

    #[test]
    fn test_where_filter_gt() {
        let mut req = make_table_request(TableVerb::Print, vec!["NAME", "SALARY"], vec![], vec![]);
        req.where_clauses.push(WhereClause {
            field: "SALARY".into(),
            op: CompOp::Gt,
            value: Expr::Literal(60000.0),
        });
        let output = TableEngine::execute(&req, &sample_data()).unwrap();
        // Only Carol (70000) and Eve (80000) pass
        assert_eq!(output.rows.len(), 2);
    }

    #[test]
    fn test_where_filter_eq() {
        let mut req = make_table_request(TableVerb::Print, vec!["NAME"], vec![], vec![]);
        req.where_clauses.push(WhereClause {
            field: "SALARY".into(),
            op: CompOp::Eq,
            value: Expr::Literal(55000.0),
        });
        let output = TableEngine::execute(&req, &sample_data()).unwrap();
        assert_eq!(output.rows.len(), 1);
        assert_eq!(output.rows[0].cells[0].1, CellValue::Str("Bob".into()));
    }

    #[test]
    fn test_where_filter_le() {
        let mut req = make_table_request(TableVerb::Print, vec!["NAME"], vec![], vec![]);
        req.where_clauses.push(WhereClause {
            field: "SALARY".into(),
            op: CompOp::Le,
            value: Expr::Literal(55000.0),
        });
        let output = TableEngine::execute(&req, &sample_data()).unwrap();
        assert_eq!(output.rows.len(), 2); // Bob (55000), Dave (50000)
    }

    #[test]
    fn test_compute_field() {
        let mut req = make_table_request(TableVerb::Print, vec!["NAME", "SALARY"], vec![], vec![]);
        req.computes.push(ComputedField {
            name: "BONUS".into(),
            expr: Expr::BinOp {
                left: Box::new(Expr::Field("SALARY".into())),
                op: '*',
                right: Box::new(Expr::Literal(0.1)),
            },
        });
        let output = TableEngine::execute(&req, &sample_data()).unwrap();
        assert!(output.columns.contains(&"BONUS".to_string()));
        // Alice bonus = 60000 * 0.1 = 6000
        let alice_row = &output.rows[0];
        let bonus = alice_row.cells.iter().find(|(k, _)| k == "BONUS").unwrap();
        assert!((bonus.1.as_num() - 6000.0).abs() < f64::EPSILON);
    }

    #[test]
    fn test_compute_division_by_zero() {
        let mut req = make_table_request(TableVerb::Print, vec!["NAME"], vec![], vec![]);
        req.computes.push(ComputedField {
            name: "RATIO".into(),
            expr: Expr::BinOp {
                left: Box::new(Expr::Field("SALARY".into())),
                op: '/',
                right: Box::new(Expr::Literal(0.0)),
            },
        });
        let output = TableEngine::execute(&req, &sample_data()).unwrap();
        let ratio = output.rows[0].cells.iter().find(|(k, _)| k == "RATIO").unwrap();
        assert!((ratio.1.as_num()).abs() < f64::EPSILON); // division by zero gives 0
    }

    #[test]
    fn test_heading_footing() {
        let mut req = make_table_request(TableVerb::Print, vec!["NAME"], vec![], vec![]);
        req.heading = Some("Report Title".into());
        req.footing = Some("End of Report".into());
        let output = TableEngine::execute(&req, &sample_data()).unwrap();
        assert_eq!(output.heading.as_deref(), Some("Report Title"));
        assert_eq!(output.footing.as_deref(), Some("End of Report"));
    }

    #[test]
    fn test_subfoot() {
        let mut req = make_table_request(TableVerb::Print, vec!["NAME"], vec![], vec![]);
        req.subfoot = Some("Sub-footer text".into());
        let output = TableEngine::execute(&req, &sample_data()).unwrap();
        assert_eq!(output.subfoot.as_deref(), Some("Sub-footer text"));
    }

    #[test]
    fn test_print_by_dimension() {
        let req = make_table_request(TableVerb::Print, vec!["NAME", "SALARY"], vec!["DEPT"], vec![]);
        let output = TableEngine::execute(&req, &sample_data()).unwrap();
        // All 5 records but grouped by DEPT
        assert_eq!(output.rows.len(), 5);
    }

    #[test]
    fn test_empty_data() {
        let req = make_table_request(TableVerb::Print, vec!["NAME"], vec![], vec![]);
        let output = TableEngine::execute(&req, &[]).unwrap();
        assert!(output.rows.is_empty());
    }

    #[test]
    fn test_cell_value_as_str() {
        assert_eq!(CellValue::Num(42.0).as_str(), "42");
        assert_eq!(CellValue::Str("hello".into()).as_str(), "hello");
        assert_eq!(CellValue::Null.as_str(), "");
    }

    #[test]
    fn test_cell_value_as_num() {
        assert!((CellValue::Num(42.0).as_num() - 42.0).abs() < f64::EPSILON);
        assert!((CellValue::Str("3.14".into()).as_num() - 3.14).abs() < 0.001);
        assert!((CellValue::Null.as_num()).abs() < f64::EPSILON);
    }
}
