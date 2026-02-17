//! SQL statement executor.
//!
//! Provides runtime execution of SQL statements against PostgreSQL.

use crate::preprocess::{HostVariableUsage, SqlStatementType};
use crate::runtime::{Sqlca, SqlTranslator};
use crate::Db2Result;
use std::collections::HashMap;

/// A host variable for runtime execution.
#[derive(Debug, Clone)]
pub struct RuntimeHostVariable {
    /// Variable name (without colon prefix)
    pub name: String,
    /// Optional indicator variable name
    pub indicator: Option<String>,
    /// Whether used as input or output
    pub usage: HostVariableUsage,
}

/// A SQL statement prepared for runtime execution.
#[derive(Debug, Clone)]
pub struct RuntimeStatement {
    /// Statement number
    pub number: usize,
    /// The SQL text
    pub sql: String,
    /// Statement type
    pub stmt_type: SqlStatementType,
    /// Host variables used in this statement
    pub host_variables: Vec<RuntimeHostVariable>,
}

/// Value that can be bound to a host variable.
#[derive(Debug, Clone, PartialEq)]
pub enum SqlValue {
    /// NULL value
    Null,
    /// String value
    String(String),
    /// Integer value
    Integer(i64),
    /// Floating point value
    Float(f64),
    /// Boolean value
    Boolean(bool),
    /// Binary data
    Binary(Vec<u8>),
}

impl SqlValue {
    /// Check if value is null.
    pub fn is_null(&self) -> bool {
        matches!(self, SqlValue::Null)
    }

    /// Get as string, returning None if null.
    pub fn as_string(&self) -> Option<&str> {
        match self {
            SqlValue::String(s) => Some(s),
            _ => None,
        }
    }

    /// Get as i64, returning None if null or wrong type.
    pub fn as_integer(&self) -> Option<i64> {
        match self {
            SqlValue::Integer(i) => Some(*i),
            _ => None,
        }
    }

    /// Get as f64, returning None if null or wrong type.
    pub fn as_float(&self) -> Option<f64> {
        match self {
            SqlValue::Float(f) => Some(*f),
            SqlValue::Integer(i) => Some(*i as f64),
            _ => None,
        }
    }
}

impl From<&str> for SqlValue {
    fn from(s: &str) -> Self {
        SqlValue::String(s.to_string())
    }
}

impl From<String> for SqlValue {
    fn from(s: String) -> Self {
        SqlValue::String(s)
    }
}

impl From<i32> for SqlValue {
    fn from(i: i32) -> Self {
        SqlValue::Integer(i as i64)
    }
}

impl From<i64> for SqlValue {
    fn from(i: i64) -> Self {
        SqlValue::Integer(i)
    }
}

impl From<f64> for SqlValue {
    fn from(f: f64) -> Self {
        SqlValue::Float(f)
    }
}

/// Result row from a query.
#[derive(Debug, Clone, Default)]
pub struct SqlRow {
    /// Column values by index
    columns: Vec<SqlValue>,
    /// Column names to index mapping
    column_names: HashMap<String, usize>,
}

impl SqlRow {
    /// Create a new empty row.
    pub fn new() -> Self {
        Self::default()
    }

    /// Add a column value.
    pub fn add_column(&mut self, name: &str, value: SqlValue) {
        let index = self.columns.len();
        self.column_names.insert(name.to_uppercase(), index);
        self.columns.push(value);
    }

    /// Get column by index.
    pub fn get(&self, index: usize) -> Option<&SqlValue> {
        self.columns.get(index)
    }

    /// Get column by name.
    pub fn get_by_name(&self, name: &str) -> Option<&SqlValue> {
        self.column_names
            .get(&name.to_uppercase())
            .and_then(|&idx| self.columns.get(idx))
    }

    /// Get number of columns.
    pub fn len(&self) -> usize {
        self.columns.len()
    }

    /// Check if row is empty.
    pub fn is_empty(&self) -> bool {
        self.columns.is_empty()
    }
}

/// A dynamically prepared SQL statement.
#[derive(Debug, Clone)]
pub struct PreparedDynamic {
    /// Statement name (e.g., "STMT1").
    pub name: String,
    /// The SQL text as supplied to PREPARE.
    pub sql: String,
    /// Translated SQL (PostgreSQL dialect).
    pub translated_sql: String,
    /// Number of parameter markers (?).
    pub param_count: usize,
}

/// Column description returned by DESCRIBE.
#[derive(Debug, Clone)]
pub struct DescribeColumn {
    /// Column name.
    pub name: String,
    /// Column data type.
    pub data_type: String,
    /// Column length / precision.
    pub length: usize,
    /// Scale (for DECIMAL).
    pub scale: usize,
    /// Whether the column is nullable.
    pub nullable: bool,
}

/// Execution mode for the SQL executor.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExecutorMode {
    /// Mock mode — no database connection, results simulated.
    Mock,
    /// Live mode — executes against a real PostgreSQL database.
    Live,
}

/// SQL executor for running statements.
pub struct SqlExecutor {
    /// SQL translator
    translator: SqlTranslator,
    /// Current SQLCA
    sqlca: Sqlca,
    /// Execution mode (mock or live)
    mode: ExecutorMode,
    /// Mock results for testing
    mock_results: Vec<SqlRow>,
    /// Registry of dynamically prepared statements (PREPARE/EXECUTE).
    prepared_dynamic: HashMap<String, PreparedDynamic>,
}

impl SqlExecutor {
    /// Create a new executor in mock mode.
    pub fn new() -> Self {
        Self {
            translator: SqlTranslator::new(),
            sqlca: Sqlca::new(),
            mode: ExecutorMode::Mock,
            mock_results: Vec::new(),
            prepared_dynamic: HashMap::new(),
        }
    }

    /// Create a new executor in live mode.
    pub fn new_live() -> Self {
        Self {
            translator: SqlTranslator::new(),
            sqlca: Sqlca::new(),
            mode: ExecutorMode::Live,
            mock_results: Vec::new(),
            prepared_dynamic: HashMap::new(),
        }
    }

    /// Get the current execution mode.
    pub fn mode(&self) -> ExecutorMode {
        self.mode
    }

    /// Set the execution mode.
    pub fn set_mode(&mut self, mode: ExecutorMode) {
        self.mode = mode;
    }

    /// Get the current SQLCA.
    pub fn sqlca(&self) -> &Sqlca {
        &self.sqlca
    }

    /// Get mutable SQLCA.
    pub fn sqlca_mut(&mut self) -> &mut Sqlca {
        &mut self.sqlca
    }

    /// Set mock results for testing.
    pub fn set_mock_results(&mut self, results: Vec<SqlRow>) {
        self.mock_results = results;
    }

    /// Execute a SELECT INTO statement.
    ///
    /// Returns a map of output host variable names to their values.
    pub fn execute_select_into(
        &mut self,
        statement: &RuntimeStatement,
        input_values: &HashMap<String, SqlValue>,
    ) -> Db2Result<HashMap<String, SqlValue>> {
        self.sqlca.reset();

        // Validate statement type
        if statement.stmt_type != SqlStatementType::SelectInto {
            self.sqlca.set_error(-104, "Not a SELECT INTO statement");
            return Ok(HashMap::new());
        }

        // Translate SQL
        let _pg_sql = self.translator.translate(&statement.sql);

        // In mock mode, return mock results
        if self.mode == ExecutorMode::Mock {
            return self.execute_mock_select_into(statement, input_values);
        }

        // Live mode — requires connection to be provided externally
        // In live mode without a connection handle, set resource unavailable
        self.sqlca.set_error(
            Sqlca::RESOURCE_UNAVAILABLE,
            "Live execution requires a connection — use execute_select_into_with_connection",
        );
        Ok(HashMap::new())
    }

    /// Execute SELECT INTO in mock mode.
    fn execute_mock_select_into(
        &mut self,
        statement: &RuntimeStatement,
        _input_values: &HashMap<String, SqlValue>,
    ) -> Db2Result<HashMap<String, SqlValue>> {
        let mut result = HashMap::new();

        if self.mock_results.is_empty() {
            // No data found
            self.sqlca.set_not_found();
            return Ok(result);
        }

        if self.mock_results.len() > 1 {
            // Too many rows
            self.sqlca.set_error(Sqlca::TOO_MANY_ROWS, "Too many rows for SELECT INTO");
            return Ok(result);
        }

        // Get output host variables
        let output_vars: Vec<&RuntimeHostVariable> = statement
            .host_variables
            .iter()
            .filter(|v| v.usage == HostVariableUsage::Output)
            .collect();

        let row = &self.mock_results[0];

        // Map row columns to host variables
        for (i, var) in output_vars.iter().enumerate() {
            if let Some(value) = row.get(i) {
                result.insert(var.name.clone(), value.clone());
            }
        }

        self.sqlca.set_success();
        self.sqlca.set_rows_affected(1);

        Ok(result)
    }

    /// Execute an INSERT statement.
    ///
    /// Returns the number of rows inserted.
    pub fn execute_insert(
        &mut self,
        statement: &RuntimeStatement,
        input_values: &HashMap<String, SqlValue>,
    ) -> Db2Result<i32> {
        self.sqlca.reset();

        // Validate statement type
        if statement.stmt_type != SqlStatementType::Insert {
            self.sqlca.set_error(-104, "Not an INSERT statement");
            return Ok(0);
        }

        // Translate SQL
        let _pg_sql = self.translator.translate(&statement.sql);

        // In mock mode, simulate success
        if self.mode == ExecutorMode::Mock {
            // Check for duplicate key simulation
            if input_values.contains_key("__simulate_duplicate") {
                self.sqlca.set_error(Sqlca::DUPLICATE_KEY, "Duplicate key value");
                return Ok(0);
            }

            self.sqlca.set_success();
            self.sqlca.set_rows_affected(1);
            return Ok(1);
        }

        // Live mode — requires connection
        self.sqlca.set_error(
            Sqlca::RESOURCE_UNAVAILABLE,
            "Live execution requires a connection",
        );
        Ok(self.sqlca.rows_affected())
    }

    /// Execute an UPDATE statement.
    ///
    /// Returns the number of rows updated.
    pub fn execute_update(
        &mut self,
        statement: &RuntimeStatement,
        input_values: &HashMap<String, SqlValue>,
    ) -> Db2Result<i32> {
        self.sqlca.reset();

        // Validate statement type
        if statement.stmt_type != SqlStatementType::Update {
            self.sqlca.set_error(-104, "Not an UPDATE statement");
            return Ok(0);
        }

        // Translate SQL
        let _pg_sql = self.translator.translate(&statement.sql);

        // In mock mode, simulate based on input
        if self.mode == ExecutorMode::Mock {
            let rows = input_values
                .get("__mock_rows_affected")
                .and_then(|v| v.as_integer())
                .unwrap_or(1) as i32;

            if rows == 0 {
                self.sqlca.set_not_found();
            } else {
                self.sqlca.set_success();
            }
            self.sqlca.set_rows_affected(rows);
            return Ok(rows);
        }

        // Live mode — requires connection
        self.sqlca.set_error(
            Sqlca::RESOURCE_UNAVAILABLE,
            "Live execution requires a connection",
        );
        Ok(self.sqlca.rows_affected())
    }

    /// Execute a DELETE statement.
    ///
    /// Returns the number of rows deleted.
    pub fn execute_delete(
        &mut self,
        statement: &RuntimeStatement,
        input_values: &HashMap<String, SqlValue>,
    ) -> Db2Result<i32> {
        self.sqlca.reset();

        // Validate statement type
        if statement.stmt_type != SqlStatementType::Delete {
            self.sqlca.set_error(-104, "Not a DELETE statement");
            return Ok(0);
        }

        // Translate SQL
        let _pg_sql = self.translator.translate(&statement.sql);

        // In mock mode, simulate based on input
        if self.mode == ExecutorMode::Mock {
            let rows = input_values
                .get("__mock_rows_affected")
                .and_then(|v| v.as_integer())
                .unwrap_or(1) as i32;

            if rows == 0 {
                self.sqlca.set_not_found();
            } else {
                self.sqlca.set_success();
            }
            self.sqlca.set_rows_affected(rows);
            return Ok(rows);
        }

        // Live mode — requires connection
        self.sqlca.set_error(
            Sqlca::RESOURCE_UNAVAILABLE,
            "Live execution requires a connection",
        );
        Ok(self.sqlca.rows_affected())
    }

    /// Prepare a dynamic SQL statement.
    ///
    /// Parses `PREPARE <name> FROM <sql_text>` — the SQL text is provided
    /// at runtime (typically from a COBOL host variable).  The prepared
    /// statement is stored in the internal registry and can be later
    /// executed via `execute_prepared()`.
    pub fn prepare(
        &mut self,
        stmt_name: &str,
        sql_text: &str,
    ) -> Db2Result<()> {
        self.sqlca.reset();

        let name = stmt_name.to_uppercase();

        // Count parameter markers (?)
        let param_count = sql_text.chars().filter(|&c| c == '?').count();

        // Translate SQL
        let translated = self.translator.translate(sql_text);

        let prepared = PreparedDynamic {
            name: name.clone(),
            sql: sql_text.to_string(),
            translated_sql: translated,
            param_count,
        };

        self.prepared_dynamic.insert(name, prepared);
        self.sqlca.set_success();
        Ok(())
    }

    /// Execute a previously prepared dynamic SQL statement.
    ///
    /// `params` supplies the values for parameter markers (?) in the order
    /// they appear in the prepared SQL.
    pub fn execute_prepared(
        &mut self,
        stmt_name: &str,
        params: &[SqlValue],
    ) -> Db2Result<i32> {
        self.sqlca.reset();

        let name = stmt_name.to_uppercase();

        let prepared = match self.prepared_dynamic.get(&name) {
            Some(p) => p.clone(),
            None => {
                self.sqlca.set_error(-518, &format!("Statement {} has not been prepared", name));
                return Ok(0);
            }
        };

        // Validate parameter count
        if params.len() != prepared.param_count {
            self.sqlca.set_error(
                -313,
                &format!(
                    "Number of host variables ({}) does not match parameter markers ({})",
                    params.len(),
                    prepared.param_count
                ),
            );
            return Ok(0);
        }

        // In mock mode, simulate execution
        if self.mode == ExecutorMode::Mock {
            self.sqlca.set_success();
            self.sqlca.set_rows_affected(1);
            return Ok(1);
        }

        // Live mode — requires connection
        self.sqlca.set_error(
            Sqlca::RESOURCE_UNAVAILABLE,
            "Live execution requires a connection",
        );
        Ok(self.sqlca.rows_affected())
    }

    /// Execute a dynamic SQL statement immediately (EXECUTE IMMEDIATE).
    ///
    /// Combines PREPARE + EXECUTE in a single step.  Suited for DDL and
    /// other one-shot statements that need no parameter markers.
    pub fn execute_immediate(
        &mut self,
        sql_text: &str,
    ) -> Db2Result<i32> {
        self.sqlca.reset();

        // Translate SQL
        let _pg_sql = self.translator.translate(sql_text);

        // In mock mode, simulate success
        if self.mode == ExecutorMode::Mock {
            self.sqlca.set_success();
            return Ok(0);
        }

        // Live mode — requires connection
        self.sqlca.set_error(
            Sqlca::RESOURCE_UNAVAILABLE,
            "Live execution requires a connection",
        );
        Ok(0)
    }

    /// Describe a prepared statement's result columns.
    ///
    /// Returns column metadata (name, type, length, nullability) for the
    /// result set of a previously prepared SELECT statement.
    pub fn describe(
        &mut self,
        stmt_name: &str,
    ) -> Db2Result<Vec<DescribeColumn>> {
        self.sqlca.reset();

        let name = stmt_name.to_uppercase();

        let prepared = match self.prepared_dynamic.get(&name) {
            Some(p) => p.clone(),
            None => {
                self.sqlca.set_error(-518, &format!("Statement {} has not been prepared", name));
                return Ok(Vec::new());
            }
        };

        // In mock mode, derive columns from the SQL text (best-effort parse)
        if self.mode == ExecutorMode::Mock {
            let columns = self.mock_describe_columns(&prepared.sql);
            self.sqlca.set_success();
            return Ok(columns);
        }

        // Live mode — requires connection
        self.sqlca.set_error(
            Sqlca::RESOURCE_UNAVAILABLE,
            "Live execution requires a connection",
        );
        Ok(Vec::new())
    }

    /// Get a reference to a prepared dynamic statement.
    pub fn get_prepared(&self, stmt_name: &str) -> Option<&PreparedDynamic> {
        self.prepared_dynamic.get(&stmt_name.to_uppercase())
    }

    /// Best-effort extraction of column names from a SELECT statement.
    fn mock_describe_columns(&self, sql: &str) -> Vec<DescribeColumn> {
        let upper = sql.trim().to_uppercase();
        if !upper.starts_with("SELECT") {
            return Vec::new();
        }

        // Extract column list between SELECT and FROM
        let from_pos = match upper.find(" FROM ") {
            Some(p) => p,
            None => return Vec::new(),
        };

        let select_len = "SELECT".len();
        let col_part = &sql[select_len..from_pos];

        if col_part.trim() == "*" {
            return vec![DescribeColumn {
                name: "*".to_string(),
                data_type: "VARCHAR".to_string(),
                length: 0,
                scale: 0,
                nullable: true,
            }];
        }

        col_part
            .split(',')
            .map(|c| {
                let col_name = c.split_whitespace().last().unwrap_or("?");
                DescribeColumn {
                    name: col_name.to_uppercase(),
                    data_type: "VARCHAR".to_string(),
                    length: 255,
                    scale: 0,
                    nullable: true,
                }
            })
            .collect()
    }

    /// Bind host variable values to a SQL statement.
    ///
    /// Replaces :VARNAME with $1, $2, etc. and returns the parameter order.
    pub fn bind_parameters(
        &self,
        sql: &str,
        host_variables: &[RuntimeHostVariable],
        values: &HashMap<String, SqlValue>,
    ) -> (String, Vec<SqlValue>) {
        let mut result = sql.to_string();
        let mut params = Vec::new();
        let mut param_num = 1;

        // Get input variables in order of appearance
        let input_vars: Vec<&RuntimeHostVariable> = host_variables
            .iter()
            .filter(|v| v.usage == HostVariableUsage::Input)
            .collect();

        for var in input_vars {
            let placeholder = format!(":{}", var.name);
            if result.contains(&placeholder) {
                let pg_param = format!("${}", param_num);
                result = result.replace(&placeholder, &pg_param);

                // Get value for this variable
                let value = values
                    .get(&var.name)
                    .cloned()
                    .unwrap_or(SqlValue::Null);
                params.push(value);
                param_num += 1;
            }
        }

        (result, params)
    }

    /// Get a reference to the SQL translator.
    pub fn translator(&self) -> &SqlTranslator {
        &self.translator
    }
}

// -----------------------------------------------------------------------
// Result-to-COBOL mapping utilities (Story 302.2)
// -----------------------------------------------------------------------

/// Map a PostgreSQL result row to COBOL host variables.
///
/// Applies COBOL-style formatting:
/// - String values: left-justified, padded with spaces to `target_len`
/// - Decimal values: converted to string with correct precision/scale
/// - Integer values: stored as-is
/// - NULL values: detected via indicator variables
pub fn map_row_to_host_variables(
    row: &SqlRow,
    output_vars: &[RuntimeHostVariable],
    target_lengths: &HashMap<String, usize>,
) -> HashMap<String, SqlValue> {
    let mut result = HashMap::new();
    let mut indicators = HashMap::new();

    for (i, var) in output_vars.iter().enumerate() {
        if let Some(value) = row.get(i) {
            if value.is_null() {
                // NULL handling — leave host var unchanged, set indicator = -1
                if let Some(ref ind_name) = var.indicator {
                    indicators.insert(ind_name.clone(), SqlValue::Integer(-1));
                }
            } else {
                // Apply COBOL formatting
                let formatted = if let Some(&len) = target_lengths.get(&var.name) {
                    cobol_format_value(value, len)
                } else {
                    value.clone()
                };
                result.insert(var.name.clone(), formatted);

                // Indicator = 0 for non-NULL
                if let Some(ref ind_name) = var.indicator {
                    indicators.insert(ind_name.clone(), SqlValue::Integer(0));
                }
            }
        } else {
            // Column not present — treat as NULL
            if let Some(ref ind_name) = var.indicator {
                indicators.insert(ind_name.clone(), SqlValue::Integer(-1));
            }
        }
    }

    // Merge indicators into result
    result.extend(indicators);
    result
}

/// Format a `SqlValue` for COBOL host variable storage.
///
/// Applies COBOL display rules:
/// - PIC X(n): left-justify, pad with spaces on the right
/// - PIC S9(n)V9(m) COMP-3: decimal value stored with correct precision
/// - Integer types: stored as-is
pub fn cobol_format_value(value: &SqlValue, target_len: usize) -> SqlValue {
    match value {
        SqlValue::String(s) => {
            // Left-justify and pad with spaces
            let formatted = cobol_pad_string(s, target_len);
            SqlValue::String(formatted)
        }
        // Numeric values don't need padding
        _ => value.clone(),
    }
}

/// Left-justify a string and pad with spaces to the target length (COBOL PIC X).
pub fn cobol_pad_string(s: &str, target_len: usize) -> String {
    if s.len() >= target_len {
        // Truncate to target length
        s[..target_len].to_string()
    } else {
        // Pad with spaces on the right
        format!("{:<width$}", s, width = target_len)
    }
}

/// Convert a decimal value to COBOL packed decimal representation.
///
/// For PIC S9(p-s)V9(s) COMP-3, the value is scaled and stored as an integer.
pub fn cobol_format_decimal(value: f64, precision: u8, scale: u8) -> SqlValue {
    let factor = 10_f64.powi(scale as i32);
    let scaled = (value * factor).round() as i64;

    // Verify it fits within precision
    let max_val = 10_i64.pow(precision as u32) - 1;
    if scaled.abs() > max_val {
        // Overflow — return the truncated value
        SqlValue::Integer(if scaled > 0 { max_val } else { -max_val })
    } else {
        SqlValue::Integer(scaled)
    }
}

/// Convert an `SqlValue` to a COBOL-compatible string for display.
///
/// Converts numeric types to their string representation suitable
/// for COBOL display fields.
pub fn sql_value_to_display(value: &SqlValue) -> String {
    match value {
        SqlValue::Null => String::new(),
        SqlValue::String(s) => s.clone(),
        SqlValue::Integer(i) => i.to_string(),
        SqlValue::Float(f) => format!("{:.2}", f),
        SqlValue::Boolean(b) => if *b { "1" } else { "0" }.to_string(),
        SqlValue::Binary(b) => {
            use std::fmt::Write;
            b.iter().fold(String::new(), |mut acc, byte| {
                let _ = write!(acc, "{:02X}", byte);
                acc
            })
        }
    }
}

impl Default for SqlExecutor {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn create_select_into_statement() -> RuntimeStatement {
        RuntimeStatement {
            number: 1,
            sql: "SELECT NAME, AGE INTO :WS-NAME, :WS-AGE FROM EMPLOYEE WHERE ID = :WS-ID".to_string(),
            stmt_type: SqlStatementType::SelectInto,
            host_variables: vec![
                RuntimeHostVariable {
                    name: "WS-NAME".to_string(),
                    indicator: None,
                    usage: HostVariableUsage::Output,
                },
                RuntimeHostVariable {
                    name: "WS-AGE".to_string(),
                    indicator: None,
                    usage: HostVariableUsage::Output,
                },
                RuntimeHostVariable {
                    name: "WS-ID".to_string(),
                    indicator: None,
                    usage: HostVariableUsage::Input,
                },
            ],
        }
    }

    fn create_insert_statement() -> RuntimeStatement {
        RuntimeStatement {
            number: 1,
            sql: "INSERT INTO EMPLOYEE (ID, NAME) VALUES (:WS-ID, :WS-NAME)".to_string(),
            stmt_type: SqlStatementType::Insert,
            host_variables: vec![
                RuntimeHostVariable {
                    name: "WS-ID".to_string(),
                    indicator: None,
                    usage: HostVariableUsage::Input,
                },
                RuntimeHostVariable {
                    name: "WS-NAME".to_string(),
                    indicator: None,
                    usage: HostVariableUsage::Input,
                },
            ],
        }
    }

    fn create_update_statement() -> RuntimeStatement {
        RuntimeStatement {
            number: 1,
            sql: "UPDATE EMPLOYEE SET NAME = :WS-NAME WHERE ID = :WS-ID".to_string(),
            stmt_type: SqlStatementType::Update,
            host_variables: vec![
                RuntimeHostVariable {
                    name: "WS-NAME".to_string(),
                    indicator: None,
                    usage: HostVariableUsage::Input,
                },
                RuntimeHostVariable {
                    name: "WS-ID".to_string(),
                    indicator: None,
                    usage: HostVariableUsage::Input,
                },
            ],
        }
    }

    fn create_delete_statement() -> RuntimeStatement {
        RuntimeStatement {
            number: 1,
            sql: "DELETE FROM EMPLOYEE WHERE ID = :WS-ID".to_string(),
            stmt_type: SqlStatementType::Delete,
            host_variables: vec![RuntimeHostVariable {
                name: "WS-ID".to_string(),
                indicator: None,
                usage: HostVariableUsage::Input,
            }],
        }
    }

    #[test]
    fn test_select_into_success() {
        let mut executor = SqlExecutor::new();
        let statement = create_select_into_statement();

        // Set mock result
        let mut row = SqlRow::new();
        row.add_column("NAME", SqlValue::String("John Doe".to_string()));
        row.add_column("AGE", SqlValue::Integer(30));
        executor.set_mock_results(vec![row]);

        let mut input = HashMap::new();
        input.insert("WS-ID".to_string(), SqlValue::Integer(1));

        let result = executor.execute_select_into(&statement, &input).unwrap();

        assert!(executor.sqlca().is_success());
        assert_eq!(result.get("WS-NAME").unwrap().as_string(), Some("John Doe"));
        assert_eq!(result.get("WS-AGE").unwrap().as_integer(), Some(30));
    }

    #[test]
    fn test_select_into_not_found() {
        let mut executor = SqlExecutor::new();
        let statement = create_select_into_statement();

        // No mock results - simulates no data found
        let input = HashMap::new();
        let result = executor.execute_select_into(&statement, &input).unwrap();

        assert!(executor.sqlca().is_not_found());
        assert!(result.is_empty());
    }

    #[test]
    fn test_select_into_too_many_rows() {
        let mut executor = SqlExecutor::new();
        let statement = create_select_into_statement();

        // Multiple mock results
        let mut row1 = SqlRow::new();
        row1.add_column("NAME", SqlValue::String("John".to_string()));
        let mut row2 = SqlRow::new();
        row2.add_column("NAME", SqlValue::String("Jane".to_string()));
        executor.set_mock_results(vec![row1, row2]);

        let input = HashMap::new();
        let _result = executor.execute_select_into(&statement, &input).unwrap();

        assert!(executor.sqlca().is_error());
        assert_eq!(executor.sqlca().sqlcode(), Sqlca::TOO_MANY_ROWS);
    }

    #[test]
    fn test_insert_success() {
        let mut executor = SqlExecutor::new();
        let statement = create_insert_statement();

        let mut input = HashMap::new();
        input.insert("WS-ID".to_string(), SqlValue::Integer(1));
        input.insert("WS-NAME".to_string(), SqlValue::String("John".to_string()));

        let rows = executor.execute_insert(&statement, &input).unwrap();

        assert!(executor.sqlca().is_success());
        assert_eq!(rows, 1);
    }

    #[test]
    fn test_insert_duplicate_key() {
        let mut executor = SqlExecutor::new();
        let statement = create_insert_statement();

        let mut input = HashMap::new();
        input.insert("__simulate_duplicate".to_string(), SqlValue::Boolean(true));

        let rows = executor.execute_insert(&statement, &input).unwrap();

        assert!(executor.sqlca().is_error());
        assert_eq!(executor.sqlca().sqlcode(), Sqlca::DUPLICATE_KEY);
        assert_eq!(rows, 0);
    }

    #[test]
    fn test_update_success() {
        let mut executor = SqlExecutor::new();
        let statement = create_update_statement();

        let mut input = HashMap::new();
        input.insert("WS-ID".to_string(), SqlValue::Integer(1));
        input.insert("WS-NAME".to_string(), SqlValue::String("Updated".to_string()));

        let rows = executor.execute_update(&statement, &input).unwrap();

        assert!(executor.sqlca().is_success());
        assert_eq!(rows, 1);
    }

    #[test]
    fn test_update_not_found() {
        let mut executor = SqlExecutor::new();
        let statement = create_update_statement();

        let mut input = HashMap::new();
        input.insert("__mock_rows_affected".to_string(), SqlValue::Integer(0));

        let rows = executor.execute_update(&statement, &input).unwrap();

        assert!(executor.sqlca().is_not_found());
        assert_eq!(rows, 0);
    }

    #[test]
    fn test_delete_success() {
        let mut executor = SqlExecutor::new();
        let statement = create_delete_statement();

        let mut input = HashMap::new();
        input.insert("WS-ID".to_string(), SqlValue::Integer(1));

        let rows = executor.execute_delete(&statement, &input).unwrap();

        assert!(executor.sqlca().is_success());
        assert_eq!(rows, 1);
    }

    #[test]
    fn test_delete_multiple_rows() {
        let mut executor = SqlExecutor::new();
        let statement = create_delete_statement();

        let mut input = HashMap::new();
        input.insert("__mock_rows_affected".to_string(), SqlValue::Integer(5));

        let rows = executor.execute_delete(&statement, &input).unwrap();

        assert!(executor.sqlca().is_success());
        assert_eq!(rows, 5);
    }

    #[test]
    fn test_bind_parameters() {
        let executor = SqlExecutor::new();
        let sql = "SELECT * FROM T WHERE ID = :WS-ID AND NAME = :WS-NAME";
        let vars = vec![
            RuntimeHostVariable {
                name: "WS-ID".to_string(),
                indicator: None,
                usage: HostVariableUsage::Input,
            },
            RuntimeHostVariable {
                name: "WS-NAME".to_string(),
                indicator: None,
                usage: HostVariableUsage::Input,
            },
        ];
        let mut values = HashMap::new();
        values.insert("WS-ID".to_string(), SqlValue::Integer(1));
        values.insert("WS-NAME".to_string(), SqlValue::String("John".to_string()));

        let (bound_sql, params) = executor.bind_parameters(sql, &vars, &values);

        assert!(bound_sql.contains("$1"));
        assert!(bound_sql.contains("$2"));
        assert!(!bound_sql.contains(":WS-ID"));
        assert_eq!(params.len(), 2);
    }

    #[test]
    fn test_sql_value_conversions() {
        let v1: SqlValue = "hello".into();
        assert_eq!(v1.as_string(), Some("hello"));

        let v2: SqlValue = 42i32.into();
        assert_eq!(v2.as_integer(), Some(42));

        let v3: SqlValue = 3.14f64.into();
        assert_eq!(v3.as_float(), Some(3.14));

        let v4 = SqlValue::Null;
        assert!(v4.is_null());
    }

    #[test]
    fn test_sql_row() {
        let mut row = SqlRow::new();
        row.add_column("NAME", SqlValue::String("Test".to_string()));
        row.add_column("VALUE", SqlValue::Integer(100));

        assert_eq!(row.len(), 2);
        assert_eq!(row.get(0).unwrap().as_string(), Some("Test"));
        assert_eq!(row.get_by_name("VALUE").unwrap().as_integer(), Some(100));
        assert_eq!(row.get_by_name("name").unwrap().as_string(), Some("Test")); // Case insensitive
    }

    // --- Dynamic SQL Tests (Epic 300) ---

    #[test]
    fn test_prepare_and_execute() {
        let mut executor = SqlExecutor::new();

        // PREPARE
        executor
            .prepare("STMT1", "SELECT * FROM EMP WHERE DEPT = ?")
            .unwrap();
        assert!(executor.sqlca().is_success());

        // Verify prepared statement
        let prep = executor.get_prepared("STMT1").unwrap();
        assert_eq!(prep.name, "STMT1");
        assert_eq!(prep.param_count, 1);

        // EXECUTE with correct parameter count
        let rows = executor
            .execute_prepared("STMT1", &[SqlValue::String("D01".to_string())])
            .unwrap();
        assert!(executor.sqlca().is_success());
        assert_eq!(rows, 1);
    }

    #[test]
    fn test_prepare_overwrites_existing() {
        let mut executor = SqlExecutor::new();

        executor.prepare("S1", "SELECT * FROM T WHERE A = ?").unwrap();
        assert_eq!(executor.get_prepared("S1").unwrap().param_count, 1);

        executor.prepare("S1", "SELECT * FROM T WHERE A = ? AND B = ?").unwrap();
        assert_eq!(executor.get_prepared("S1").unwrap().param_count, 2);
    }

    #[test]
    fn test_execute_not_prepared() {
        let mut executor = SqlExecutor::new();

        let rows = executor.execute_prepared("NOPE", &[]).unwrap();
        assert!(executor.sqlca().is_error());
        assert_eq!(executor.sqlca().sqlcode(), -518);
        assert_eq!(rows, 0);
    }

    #[test]
    fn test_execute_wrong_param_count() {
        let mut executor = SqlExecutor::new();

        executor.prepare("S1", "SELECT * FROM T WHERE A = ? AND B = ?").unwrap();

        // Too few parameters
        let rows = executor
            .execute_prepared("S1", &[SqlValue::Integer(1)])
            .unwrap();
        assert!(executor.sqlca().is_error());
        assert_eq!(executor.sqlca().sqlcode(), -313);
        assert_eq!(rows, 0);
    }

    #[test]
    fn test_execute_no_params() {
        let mut executor = SqlExecutor::new();

        executor.prepare("S1", "SELECT * FROM T").unwrap();
        let rows = executor.execute_prepared("S1", &[]).unwrap();
        assert!(executor.sqlca().is_success());
        assert_eq!(rows, 1);
    }

    #[test]
    fn test_execute_immediate_success() {
        let mut executor = SqlExecutor::new();

        let rows = executor
            .execute_immediate("CREATE TABLE TEMP1 (COL1 INTEGER)")
            .unwrap();
        assert!(executor.sqlca().is_success());
        assert_eq!(rows, 0); // DDL returns 0 rows
    }

    #[test]
    fn test_execute_immediate_drop() {
        let mut executor = SqlExecutor::new();

        let rows = executor
            .execute_immediate("DROP TABLE TEMP1")
            .unwrap();
        assert!(executor.sqlca().is_success());
        assert_eq!(rows, 0);
    }

    #[test]
    fn test_describe_prepared_select() {
        let mut executor = SqlExecutor::new();

        executor
            .prepare("S1", "SELECT NAME, AGE, DEPT FROM EMP WHERE ID = ?")
            .unwrap();

        let columns = executor.describe("S1").unwrap();
        assert!(executor.sqlca().is_success());
        assert_eq!(columns.len(), 3);
        assert_eq!(columns[0].name, "NAME");
        assert_eq!(columns[1].name, "AGE");
        assert_eq!(columns[2].name, "DEPT");
    }

    #[test]
    fn test_describe_not_prepared() {
        let mut executor = SqlExecutor::new();

        let columns = executor.describe("NOPE").unwrap();
        assert!(executor.sqlca().is_error());
        assert_eq!(executor.sqlca().sqlcode(), -518);
        assert!(columns.is_empty());
    }

    #[test]
    fn test_describe_select_star() {
        let mut executor = SqlExecutor::new();

        executor.prepare("S1", "SELECT * FROM EMP").unwrap();
        let columns = executor.describe("S1").unwrap();
        assert_eq!(columns.len(), 1);
        assert_eq!(columns[0].name, "*");
    }

    #[test]
    fn test_prepare_case_insensitive() {
        let mut executor = SqlExecutor::new();

        executor.prepare("stmt1", "SELECT 1 FROM DUAL").unwrap();
        assert!(executor.get_prepared("STMT1").is_some());
        assert!(executor.get_prepared("stmt1").is_some());
    }

    #[test]
    fn test_prepare_sql_translation() {
        let mut executor = SqlExecutor::new();

        executor
            .prepare("S1", "SELECT * FROM T FETCH FIRST 5 ROWS ONLY")
            .unwrap();
        let prep = executor.get_prepared("S1").unwrap();
        // DB2 FETCH FIRST should be translated to LIMIT
        assert!(prep.translated_sql.contains("LIMIT 5"));
    }

    // --- Story 302.1: Executor Mode Tests ---

    #[test]
    fn test_executor_default_mode_is_mock() {
        let executor = SqlExecutor::new();
        assert_eq!(executor.mode(), ExecutorMode::Mock);
    }

    #[test]
    fn test_executor_live_mode_creation() {
        let executor = SqlExecutor::new_live();
        assert_eq!(executor.mode(), ExecutorMode::Live);
    }

    #[test]
    fn test_executor_mode_switch() {
        let mut executor = SqlExecutor::new();
        assert_eq!(executor.mode(), ExecutorMode::Mock);

        executor.set_mode(ExecutorMode::Live);
        assert_eq!(executor.mode(), ExecutorMode::Live);

        executor.set_mode(ExecutorMode::Mock);
        assert_eq!(executor.mode(), ExecutorMode::Mock);
    }

    #[test]
    fn test_live_mode_select_without_connection_returns_error() {
        let mut executor = SqlExecutor::new_live();
        let statement = create_select_into_statement();

        let input = HashMap::new();
        let _result = executor.execute_select_into(&statement, &input).unwrap();

        assert!(executor.sqlca().is_error());
        assert_eq!(executor.sqlca().sqlcode(), Sqlca::RESOURCE_UNAVAILABLE);
    }

    #[test]
    fn test_live_mode_insert_without_connection_returns_error() {
        let mut executor = SqlExecutor::new_live();
        let statement = create_insert_statement();

        let input = HashMap::new();
        let _rows = executor.execute_insert(&statement, &input).unwrap();

        assert!(executor.sqlca().is_error());
        assert_eq!(executor.sqlca().sqlcode(), Sqlca::RESOURCE_UNAVAILABLE);
    }

    #[test]
    fn test_live_mode_update_without_connection_returns_error() {
        let mut executor = SqlExecutor::new_live();
        let statement = create_update_statement();

        let input = HashMap::new();
        let _rows = executor.execute_update(&statement, &input).unwrap();

        assert!(executor.sqlca().is_error());
        assert_eq!(executor.sqlca().sqlcode(), Sqlca::RESOURCE_UNAVAILABLE);
    }

    #[test]
    fn test_live_mode_delete_without_connection_returns_error() {
        let mut executor = SqlExecutor::new_live();
        let statement = create_delete_statement();

        let input = HashMap::new();
        let _rows = executor.execute_delete(&statement, &input).unwrap();

        assert!(executor.sqlca().is_error());
        assert_eq!(executor.sqlca().sqlcode(), Sqlca::RESOURCE_UNAVAILABLE);
    }

    #[test]
    fn test_live_mode_execute_immediate_without_connection() {
        let mut executor = SqlExecutor::new_live();

        let _rows = executor.execute_immediate("CREATE TABLE T (A INT)").unwrap();
        assert!(executor.sqlca().is_error());
        assert_eq!(executor.sqlca().sqlcode(), Sqlca::RESOURCE_UNAVAILABLE);
    }

    #[test]
    fn test_live_mode_describe_without_connection() {
        let mut executor = SqlExecutor::new_live();
        executor.prepare("S1", "SELECT * FROM T").unwrap();

        let cols = executor.describe("S1").unwrap();
        assert!(executor.sqlca().is_error());
        assert_eq!(executor.sqlca().sqlcode(), Sqlca::RESOURCE_UNAVAILABLE);
        assert!(cols.is_empty());
    }

    // --- Story 302.2: Result-to-COBOL Mapping Tests ---

    #[test]
    fn test_cobol_pad_string_shorter_than_target() {
        let result = cobol_pad_string("Hello", 10);
        assert_eq!(result, "Hello     ");
        assert_eq!(result.len(), 10);
    }

    #[test]
    fn test_cobol_pad_string_exact_length() {
        let result = cobol_pad_string("Hello", 5);
        assert_eq!(result, "Hello");
    }

    #[test]
    fn test_cobol_pad_string_truncation() {
        let result = cobol_pad_string("Hello World", 5);
        assert_eq!(result, "Hello");
    }

    #[test]
    fn test_cobol_pad_string_empty() {
        let result = cobol_pad_string("", 10);
        assert_eq!(result, "          ");
        assert_eq!(result.len(), 10);
    }

    #[test]
    fn test_cobol_format_value_string() {
        let value = SqlValue::String("Test".to_string());
        let formatted = cobol_format_value(&value, 10);
        if let SqlValue::String(s) = formatted {
            assert_eq!(s, "Test      ");
            assert_eq!(s.len(), 10);
        } else {
            panic!("Expected String value");
        }
    }

    #[test]
    fn test_cobol_format_value_integer_unchanged() {
        let value = SqlValue::Integer(42);
        let formatted = cobol_format_value(&value, 10);
        assert_eq!(formatted, SqlValue::Integer(42));
    }

    #[test]
    fn test_cobol_format_decimal_basic() {
        // PIC S9(7)V99 — precision=9, scale=2
        let result = cobol_format_decimal(1234.56, 9, 2);
        assert_eq!(result, SqlValue::Integer(123456));
    }

    #[test]
    fn test_cobol_format_decimal_negative() {
        let result = cobol_format_decimal(-99.99, 5, 2);
        assert_eq!(result, SqlValue::Integer(-9999));
    }

    #[test]
    fn test_cobol_format_decimal_rounding() {
        // 1.235 * 100 = 123.5, rounds to 124
        let result = cobol_format_decimal(1.235, 5, 2);
        assert_eq!(result, SqlValue::Integer(124));
    }

    #[test]
    fn test_cobol_format_decimal_overflow() {
        // PIC S9(3)V99 — max 999.99 = 99999 scaled
        let result = cobol_format_decimal(9999.99, 5, 2);
        // 9999.99 * 100 = 999999 > 99999, should clamp
        assert_eq!(result, SqlValue::Integer(99999));
    }

    #[test]
    fn test_map_row_to_host_variables_basic() {
        let mut row = SqlRow::new();
        row.add_column("NAME", SqlValue::String("JOHN DOE".to_string()));
        row.add_column("SALARY", SqlValue::Float(75000.50));

        let vars = vec![
            RuntimeHostVariable {
                name: "WS-NAME".to_string(),
                indicator: None,
                usage: HostVariableUsage::Output,
            },
            RuntimeHostVariable {
                name: "WS-SALARY".to_string(),
                indicator: None,
                usage: HostVariableUsage::Output,
            },
        ];

        let mut target_lengths = HashMap::new();
        target_lengths.insert("WS-NAME".to_string(), 20);

        let result = map_row_to_host_variables(&row, &vars, &target_lengths);

        // Name should be padded to 20 chars
        if let SqlValue::String(name) = result.get("WS-NAME").unwrap() {
            assert_eq!(name.len(), 20);
            assert!(name.starts_with("JOHN DOE"));
        } else {
            panic!("Expected String");
        }

        // Salary has no target length, should be unchanged
        assert_eq!(
            result.get("WS-SALARY").unwrap(),
            &SqlValue::Float(75000.50)
        );
    }

    #[test]
    fn test_map_row_to_host_variables_with_null_indicator() {
        let mut row = SqlRow::new();
        row.add_column("NAME", SqlValue::Null);

        let vars = vec![RuntimeHostVariable {
            name: "WS-NAME".to_string(),
            indicator: Some("WS-NAME-IND".to_string()),
            usage: HostVariableUsage::Output,
        }];

        let target_lengths = HashMap::new();
        let result = map_row_to_host_variables(&row, &vars, &target_lengths);

        // WS-NAME should NOT be in result (NULL → leave unchanged)
        assert!(!result.contains_key("WS-NAME"));

        // Indicator should be -1
        assert_eq!(
            result.get("WS-NAME-IND").unwrap(),
            &SqlValue::Integer(-1)
        );
    }

    #[test]
    fn test_map_row_to_host_variables_with_non_null_indicator() {
        let mut row = SqlRow::new();
        row.add_column("NAME", SqlValue::String("ALICE".to_string()));

        let vars = vec![RuntimeHostVariable {
            name: "WS-NAME".to_string(),
            indicator: Some("WS-NAME-IND".to_string()),
            usage: HostVariableUsage::Output,
        }];

        let mut target_lengths = HashMap::new();
        target_lengths.insert("WS-NAME".to_string(), 10);

        let result = map_row_to_host_variables(&row, &vars, &target_lengths);

        // Indicator should be 0 for non-NULL
        assert_eq!(
            result.get("WS-NAME-IND").unwrap(),
            &SqlValue::Integer(0)
        );

        // Value should be present and padded
        if let SqlValue::String(name) = result.get("WS-NAME").unwrap() {
            assert_eq!(name, "ALICE     ");
        } else {
            panic!("Expected String");
        }
    }

    #[test]
    fn test_sql_value_to_display() {
        assert_eq!(sql_value_to_display(&SqlValue::Null), "");
        assert_eq!(sql_value_to_display(&SqlValue::String("ABC".to_string())), "ABC");
        assert_eq!(sql_value_to_display(&SqlValue::Integer(42)), "42");
        assert_eq!(sql_value_to_display(&SqlValue::Float(3.14)), "3.14");
        assert_eq!(sql_value_to_display(&SqlValue::Boolean(true)), "1");
        assert_eq!(sql_value_to_display(&SqlValue::Boolean(false)), "0");
        assert_eq!(
            sql_value_to_display(&SqlValue::Binary(vec![0xAB, 0xCD])),
            "ABCD"
        );
    }

    #[test]
    fn test_varchar_to_pic_x_left_justified_padded() {
        // AC: Given VARCHAR(100) with 50 chars, host var PIC X(100)
        // → left-justified, padded with spaces
        let value = SqlValue::String("A".repeat(50));
        let formatted = cobol_format_value(&value, 100);
        if let SqlValue::String(s) = formatted {
            assert_eq!(s.len(), 100);
            assert!(s.starts_with(&"A".repeat(50)));
            assert!(s.ends_with(&" ".repeat(50)));
        } else {
            panic!("Expected String");
        }
    }

    #[test]
    fn test_decimal_to_comp3_conversion() {
        // AC: Given DECIMAL(9,2) column, host var PIC S9(7)V99 COMP-3
        // → value correctly converted and stored
        let result = cobol_format_decimal(12345.67, 9, 2);
        assert_eq!(result, SqlValue::Integer(1234567));
    }
}
