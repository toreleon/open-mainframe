//! SYS-115: DB2 Operational Utilities.
//!
//! This module provides simulated DB2 operational utilities:
//!
//! - [`Dsntep2`] — Dynamic SQL processor for SELECT, DML, and DDL.
//! - [`DclgenUtil`] — Declaration generator producing COBOL copybooks,
//!   SQL DECLARE TABLE, and host variable declarations.
//! - [`LoadUtility`] — LOAD data into a table from fixed-format or
//!   delimited input records.
//! - [`UnloadUtility`] — UNLOAD table rows to fixed-width or delimited
//!   output records.

use std::collections::HashMap;
use thiserror::Error;

// ---------------------------------------------------------------------------
// Errors
// ---------------------------------------------------------------------------

/// Errors specific to operational utility execution.
#[derive(Error, Debug)]
pub enum OpsError {
    /// SQL syntax or semantic error.
    #[error("SQL error: {0}")]
    SqlError(String),

    /// Table not found.
    #[error("Table '{0}' not found")]
    TableNotFound(String),

    /// Column not found.
    #[error("Column '{column}' not found in table '{table}'")]
    ColumnNotFound {
        /// Table name.
        table: String,
        /// Column name.
        column: String,
    },

    /// Invalid input data format.
    #[error("Data format error at record {record}: {message}")]
    DataFormatError {
        /// 1-based record number.
        record: usize,
        /// Description of the problem.
        message: String,
    },

    /// Column count mismatch between schema and data.
    #[error("Column count mismatch: expected {expected}, got {got}")]
    ColumnCountMismatch {
        /// Expected number of columns.
        expected: usize,
        /// Actual number of columns.
        got: usize,
    },
}

/// Result alias for operational utilities.
pub type OpsResult<T> = Result<T, OpsError>;

// ---------------------------------------------------------------------------
// Shared table schema used across utilities
// ---------------------------------------------------------------------------

/// A column definition used by the operational utilities.
#[derive(Debug, Clone)]
pub struct OpsColumnDef {
    /// Column name.
    pub name: String,
    /// DB2 data type string (e.g., "INTEGER", "VARCHAR(50)", "DECIMAL(10,2)").
    pub data_type: String,
    /// Whether the column allows NULLs.
    pub nullable: bool,
}

/// An in-memory table definition used by utilities.
#[derive(Debug, Clone)]
pub struct OpsTableDef {
    /// Table name.
    pub name: String,
    /// Column definitions, in order.
    pub columns: Vec<OpsColumnDef>,
}

/// An in-memory table (schema + rows) used by the simulated utilities.
#[derive(Debug, Clone)]
pub struct OpsTable {
    /// Table definition (schema).
    pub def: OpsTableDef,
    /// Rows stored as `Vec<HashMap<column_name, value_string>>`.
    pub rows: Vec<HashMap<String, String>>,
}

impl OpsTable {
    /// Create a new empty table from a definition.
    pub fn new(def: OpsTableDef) -> Self {
        Self {
            def,
            rows: Vec::new(),
        }
    }

    /// Insert a row (column-name -> value mapping).
    pub fn insert_row(&mut self, row: HashMap<String, String>) {
        self.rows.push(row);
    }

    /// Get column names in definition order.
    pub fn column_names(&self) -> Vec<&str> {
        self.def.columns.iter().map(|c| c.name.as_str()).collect()
    }
}

// ---------------------------------------------------------------------------
// Dsntep2 — SYS-115 story 1
// ---------------------------------------------------------------------------

/// Result of executing SQL through DSNTEP2.
#[derive(Debug, Clone)]
pub enum Dsntep2Result {
    /// SELECT result: column names + row data.
    Rows {
        /// Column names.
        columns: Vec<String>,
        /// Row data (each row is a vec of string values).
        rows: Vec<Vec<String>>,
    },
    /// DML result (INSERT/UPDATE/DELETE).
    RowCount {
        /// Number of rows affected.
        count: usize,
    },
    /// DDL result (CREATE/ALTER/DROP).
    DdlSuccess {
        /// Description of what was done.
        message: String,
    },
}

/// DSNTEP2 — Dynamic SQL processor.
///
/// Takes SQL text and executes it against an in-memory table store,
/// returning structured results.
#[derive(Debug, Default)]
pub struct Dsntep2 {
    /// In-memory tables keyed by upper-case table name.
    tables: HashMap<String, OpsTable>,
}

impl Dsntep2 {
    /// Create a new DSNTEP2 instance.
    pub fn new() -> Self {
        Self::default()
    }

    /// Register a table in the in-memory store.
    pub fn register_table(&mut self, table: OpsTable) {
        self.tables
            .insert(table.def.name.to_uppercase(), table);
    }

    /// Execute a SQL statement and return the result.
    ///
    /// Supported forms (simplified parsing):
    /// - `SELECT col1, col2 FROM table` / `SELECT * FROM table`
    /// - `SELECT col1, col2 FROM table WHERE col = 'value'`
    /// - `INSERT INTO table (col1, col2) VALUES ('v1', 'v2')`
    /// - `UPDATE table SET col = 'value' WHERE col2 = 'value2'`
    /// - `DELETE FROM table WHERE col = 'value'`
    /// - `CREATE TABLE name (col1 TYPE, ...)`
    /// - `ALTER TABLE ...`
    /// - `DROP TABLE name`
    pub fn execute(&mut self, sql: &str) -> OpsResult<Dsntep2Result> {
        let trimmed = sql.trim();
        let upper = trimmed.to_uppercase();
        let first_word = upper.split_whitespace().next().unwrap_or("");

        match first_word {
            "SELECT" => self.execute_select(trimmed, &upper),
            "INSERT" => self.execute_insert(trimmed, &upper),
            "UPDATE" => self.execute_update(trimmed, &upper),
            "DELETE" => self.execute_delete(&upper),
            "CREATE" => self.execute_create(&upper),
            "ALTER" => Ok(Dsntep2Result::DdlSuccess {
                message: format!("ALTER executed: {}", trimmed),
            }),
            "DROP" => self.execute_drop(&upper),
            _ => Err(OpsError::SqlError(format!(
                "Unsupported SQL statement: {}",
                first_word
            ))),
        }
    }

    /// Execute a SELECT statement.
    fn execute_select(&self, _sql: &str, upper: &str) -> OpsResult<Dsntep2Result> {
        // Parse: SELECT cols FROM table [WHERE col = 'val']
        let from_pos = upper
            .find(" FROM ")
            .ok_or_else(|| OpsError::SqlError("Missing FROM clause".to_string()))?;
        let cols_part = &upper[7..from_pos]; // after "SELECT "
        let after_from = &upper[from_pos + 6..]; // after " FROM "

        // Extract table name (first word after FROM)
        let table_name = after_from
            .split_whitespace()
            .next()
            .ok_or_else(|| OpsError::SqlError("Missing table name after FROM".to_string()))?;

        let table = self
            .tables
            .get(table_name)
            .ok_or_else(|| OpsError::TableNotFound(table_name.to_string()))?;

        // Determine column list
        let select_cols: Vec<String> = if cols_part.trim() == "*" {
            table.column_names().iter().map(|s| s.to_string()).collect()
        } else {
            cols_part
                .split(',')
                .map(|c| c.trim().to_string())
                .collect()
        };

        // Parse optional WHERE clause
        let where_filter = parse_where_clause(after_from);

        // Filter rows
        let filtered: Vec<&HashMap<String, String>> = table
            .rows
            .iter()
            .filter(|row| {
                if let Some((ref col, ref val)) = where_filter {
                    row.get(col.as_str()).map(|v| v == val).unwrap_or(false)
                } else {
                    true
                }
            })
            .collect();

        // Project columns
        let result_rows: Vec<Vec<String>> = filtered
            .iter()
            .map(|row| {
                select_cols
                    .iter()
                    .map(|c| row.get(c.as_str()).cloned().unwrap_or_default())
                    .collect()
            })
            .collect();

        Ok(Dsntep2Result::Rows {
            columns: select_cols,
            rows: result_rows,
        })
    }

    /// Execute an INSERT statement.
    fn execute_insert(&mut self, _sql: &str, upper: &str) -> OpsResult<Dsntep2Result> {
        // Parse: INSERT INTO table (cols) VALUES (vals)
        let into_pos = upper
            .find("INTO ")
            .ok_or_else(|| OpsError::SqlError("Missing INTO keyword".to_string()))?;
        let after_into = &upper[into_pos + 5..];

        // Table name
        let paren_or_space = after_into
            .find(['(', ' '])
            .unwrap_or(after_into.len());
        let table_name = after_into[..paren_or_space].trim().to_string();

        let table = self
            .tables
            .get_mut(&table_name)
            .ok_or_else(|| OpsError::TableNotFound(table_name.clone()))?;

        // Parse column list
        let col_start = after_into
            .find('(')
            .ok_or_else(|| OpsError::SqlError("Missing column list".to_string()))?;
        let col_end = after_into
            .find(')')
            .ok_or_else(|| OpsError::SqlError("Missing closing paren for columns".to_string()))?;
        let cols: Vec<String> = after_into[col_start + 1..col_end]
            .split(',')
            .map(|c| c.trim().to_string())
            .collect();

        // Parse VALUES
        let values_pos = after_into
            .find("VALUES")
            .ok_or_else(|| OpsError::SqlError("Missing VALUES keyword".to_string()))?;
        let after_values = &after_into[values_pos + 6..];
        let val_start = after_values
            .find('(')
            .ok_or_else(|| OpsError::SqlError("Missing values paren".to_string()))?;
        let val_end = after_values
            .rfind(')')
            .ok_or_else(|| OpsError::SqlError("Missing closing values paren".to_string()))?;
        let vals: Vec<String> = after_values[val_start + 1..val_end]
            .split(',')
            .map(|v| v.trim().trim_matches('\'').to_string())
            .collect();

        if cols.len() != vals.len() {
            return Err(OpsError::ColumnCountMismatch {
                expected: cols.len(),
                got: vals.len(),
            });
        }

        let mut row = HashMap::new();
        for (col, val) in cols.iter().zip(vals.iter()) {
            row.insert(col.clone(), val.clone());
        }
        table.insert_row(row);

        Ok(Dsntep2Result::RowCount { count: 1 })
    }

    /// Execute an UPDATE statement.
    fn execute_update(&mut self, _sql: &str, upper: &str) -> OpsResult<Dsntep2Result> {
        // Parse: UPDATE table SET col = 'val' [WHERE col2 = 'val2']
        let table_name = upper
            .split_whitespace()
            .nth(1)
            .ok_or_else(|| OpsError::SqlError("Missing table name".to_string()))?
            .to_string();

        let table = self
            .tables
            .get_mut(&table_name)
            .ok_or_else(|| OpsError::TableNotFound(table_name.clone()))?;

        // Parse SET clause
        let set_pos = upper
            .find(" SET ")
            .ok_or_else(|| OpsError::SqlError("Missing SET clause".to_string()))?;
        let after_set = &upper[set_pos + 5..];

        // Find WHERE or end
        let where_pos = after_set.find(" WHERE ");
        let set_clause = if let Some(wp) = where_pos {
            &after_set[..wp]
        } else {
            after_set
        };

        // Parse col = 'val' from SET
        let set_eq = set_clause
            .find('=')
            .ok_or_else(|| OpsError::SqlError("Missing = in SET".to_string()))?;
        let set_col = set_clause[..set_eq].trim().to_string();
        let set_val = set_clause[set_eq + 1..]
            .trim()
            .trim_matches('\'')
            .to_string();

        // Parse WHERE
        let where_filter = if let Some(wp) = where_pos {
            parse_where_clause(&after_set[wp..])
        } else {
            None
        };

        let mut count = 0usize;
        for row in &mut table.rows {
            let matches = if let Some((ref wcol, ref wval)) = where_filter {
                row.get(wcol.as_str()).map(|v| v == wval).unwrap_or(false)
            } else {
                true
            };
            if matches {
                row.insert(set_col.clone(), set_val.clone());
                count += 1;
            }
        }

        Ok(Dsntep2Result::RowCount { count })
    }

    /// Execute a DELETE statement.
    fn execute_delete(&mut self, upper: &str) -> OpsResult<Dsntep2Result> {
        // Parse: DELETE FROM table [WHERE col = 'val']
        let from_pos = upper
            .find(" FROM ")
            .ok_or_else(|| OpsError::SqlError("Missing FROM in DELETE".to_string()))?;
        let after_from = &upper[from_pos + 6..];

        let table_name = after_from
            .split_whitespace()
            .next()
            .ok_or_else(|| OpsError::SqlError("Missing table name".to_string()))?
            .to_string();

        let table = self
            .tables
            .get_mut(&table_name)
            .ok_or_else(|| OpsError::TableNotFound(table_name.clone()))?;

        let where_filter = parse_where_clause(after_from);

        let before = table.rows.len();
        if let Some((ref wcol, ref wval)) = where_filter {
            table
                .rows
                .retain(|row| row.get(wcol.as_str()).map(|v| v != wval).unwrap_or(true));
        } else {
            table.rows.clear();
        }
        let count = before - table.rows.len();

        Ok(Dsntep2Result::RowCount { count })
    }

    /// Execute a CREATE TABLE statement.
    fn execute_create(&mut self, upper: &str) -> OpsResult<Dsntep2Result> {
        // Parse: CREATE TABLE name (col1 type1, col2 type2, ...)
        let words: Vec<&str> = upper.split_whitespace().collect();
        if words.len() < 3 || words[1] != "TABLE" {
            return Err(OpsError::SqlError(
                "Invalid CREATE statement".to_string(),
            ));
        }

        let table_name = words[2].trim_end_matches('(').to_string();

        // Parse column definitions between parens
        let paren_start = upper
            .find('(')
            .ok_or_else(|| OpsError::SqlError("Missing column definitions".to_string()))?;
        let paren_end = upper
            .rfind(')')
            .ok_or_else(|| OpsError::SqlError("Missing closing paren".to_string()))?;
        let cols_str = &upper[paren_start + 1..paren_end];

        let mut columns = Vec::new();
        for col_def_str in cols_str.split(',') {
            let parts: Vec<&str> = col_def_str.split_whitespace().collect();
            if parts.len() >= 2 {
                let nullable = !col_def_str.contains("NOT NULL");
                columns.push(OpsColumnDef {
                    name: parts[0].to_string(),
                    data_type: parts[1].to_string(),
                    nullable,
                });
            }
        }

        let def = OpsTableDef {
            name: table_name.clone(),
            columns,
        };
        self.tables.insert(table_name.clone(), OpsTable::new(def));

        Ok(Dsntep2Result::DdlSuccess {
            message: format!("Table {} created", table_name),
        })
    }

    /// Execute a DROP TABLE statement.
    fn execute_drop(&mut self, upper: &str) -> OpsResult<Dsntep2Result> {
        let words: Vec<&str> = upper.split_whitespace().collect();
        if words.len() < 3 || words[1] != "TABLE" {
            return Err(OpsError::SqlError(
                "Invalid DROP statement".to_string(),
            ));
        }
        let table_name = words[2].to_string();
        if self.tables.remove(&table_name).is_some() {
            Ok(Dsntep2Result::DdlSuccess {
                message: format!("Table {} dropped", table_name),
            })
        } else {
            Err(OpsError::TableNotFound(table_name))
        }
    }

}

/// Parse a simple WHERE col = 'val' clause. Returns None if no WHERE.
fn parse_where_clause(text: &str) -> Option<(String, String)> {
    let where_pos = text.find("WHERE ")?;
    let after_where = &text[where_pos + 6..];
    let eq_pos = after_where.find('=')?;
    let col = after_where[..eq_pos].trim().to_string();
    let val = after_where[eq_pos + 1..]
        .trim()
        .trim_matches('\'')
        .trim()
        .to_string();
    Some((col, val))
}

// ---------------------------------------------------------------------------
// DclgenUtil — SYS-115 story 2
// ---------------------------------------------------------------------------

/// DCLGEN (Declaration Generator) utility.
///
/// Given a table definition, generates:
/// 1. A COBOL level-01 copybook structure.
/// 2. An SQL DECLARE TABLE statement.
/// 3. Host variable declarations.
#[derive(Debug)]
pub struct DclgenUtil;

/// Output from the DCLGEN utility.
#[derive(Debug, Clone)]
pub struct DclgenOutput {
    /// COBOL copybook (level-01 structure).
    pub cobol_copybook: String,
    /// SQL DECLARE TABLE statement.
    pub sql_declare: String,
    /// Host variable declarations section.
    pub host_variables: String,
}

impl DclgenUtil {
    /// Generate all three output sections for the given table.
    pub fn generate(table: &OpsTableDef) -> DclgenOutput {
        DclgenOutput {
            cobol_copybook: Self::generate_cobol_copybook(table),
            sql_declare: Self::generate_sql_declare(table),
            host_variables: Self::generate_host_variables(table),
        }
    }

    /// Generate a COBOL level-01 copybook structure.
    fn generate_cobol_copybook(table: &OpsTableDef) -> String {
        let struct_name = format!("DCL{}", table.name.to_uppercase().replace(' ', "-"));
        let mut out = String::new();

        out.push_str("      ******************************************************************\n");
        out.push_str(&format!(
            "      * DCLGEN TABLE({})\n",
            table.name.to_uppercase()
        ));
        out.push_str("      ******************************************************************\n");
        out.push_str(&format!("       01  {}.\n", struct_name));

        for col in &table.columns {
            let cobol_name = col.name.to_uppercase().replace('_', "-");
            let pic = Self::db2_type_to_pic(&col.data_type);
            out.push_str(&format!("           10  {} {}.\n", cobol_name, pic));
        }

        out
    }

    /// Generate an SQL DECLARE TABLE statement.
    fn generate_sql_declare(table: &OpsTableDef) -> String {
        let mut out = String::new();
        out.push_str(&format!(
            "       EXEC SQL DECLARE {} TABLE\n",
            table.name.to_uppercase()
        ));
        out.push_str("       (\n");

        for (i, col) in table.columns.iter().enumerate() {
            let nullable = if col.nullable { "" } else { " NOT NULL" };
            let comma = if i < table.columns.len() - 1 { "," } else { "" };
            out.push_str(&format!(
                "           {} {}{}{}\n",
                col.name.to_uppercase(),
                col.data_type.to_uppercase(),
                nullable,
                comma
            ));
        }

        out.push_str("       ) END-EXEC.\n");
        out
    }

    /// Generate host variable declarations.
    fn generate_host_variables(table: &OpsTableDef) -> String {
        let struct_name = format!("H-{}", table.name.to_uppercase().replace(' ', "-"));
        let mut out = String::new();

        out.push_str(&format!("       01  {}.\n", struct_name));

        for col in &table.columns {
            let cobol_name = col.name.to_uppercase().replace('_', "-");
            let pic = Self::db2_type_to_pic(&col.data_type);
            out.push_str(&format!("           10  H-{} {}.\n", cobol_name, pic));
        }

        out
    }

    /// Map a DB2 type string to a COBOL PIC clause.
    fn db2_type_to_pic(data_type: &str) -> String {
        let upper = data_type.to_uppercase();

        // Handle parameterized types like VARCHAR(50), DECIMAL(10,2), CHAR(20)
        let base = if let Some(p) = upper.find('(') {
            upper[..p].trim()
        } else {
            upper.trim()
        };

        match base {
            "INTEGER" | "INT" => "PIC S9(9) COMP".to_string(),
            "SMALLINT" => "PIC S9(4) COMP".to_string(),
            "BIGINT" => "PIC S9(18) COMP".to_string(),
            "DECIMAL" | "NUMERIC" => {
                // Parse precision and scale
                if let Some(start) = upper.find('(') {
                    if let Some(end) = upper.find(')') {
                        let params = &upper[start + 1..end];
                        let parts: Vec<&str> = params.split(',').collect();
                        let prec: usize = parts
                            .first()
                            .and_then(|s| s.trim().parse().ok())
                            .unwrap_or(18);
                        let scale: usize = parts
                            .get(1)
                            .and_then(|s| s.trim().parse().ok())
                            .unwrap_or(0);
                        if scale > 0 {
                            let int_digits = prec.saturating_sub(scale);
                            return format!("PIC S9({})V9({}) COMP-3", int_digits, scale);
                        } else {
                            return format!("PIC S9({}) COMP-3", prec);
                        }
                    }
                }
                "PIC S9(18) COMP-3".to_string()
            }
            "VARCHAR" | "CHARACTER VARYING" => {
                let len = Self::extract_length(&upper).unwrap_or(255);
                format!("PIC X({})", len)
            }
            "CHAR" | "CHARACTER" => {
                let len = Self::extract_length(&upper).unwrap_or(1);
                format!("PIC X({})", len)
            }
            "DATE" => "PIC X(10)".to_string(),
            "TIME" => "PIC X(8)".to_string(),
            "TIMESTAMP" => "PIC X(26)".to_string(),
            _ => {
                let len = Self::extract_length(&upper).unwrap_or(255);
                format!("PIC X({})", len)
            }
        }
    }

    /// Extract the length from a parameterized type like VARCHAR(50).
    fn extract_length(upper: &str) -> Option<usize> {
        let start = upper.find('(')?;
        let end = upper.find(')')?;
        let inner = &upper[start + 1..end];
        // For types like DECIMAL(10,2), take the first number
        let first = inner.split(',').next()?;
        first.trim().parse().ok()
    }
}

// ---------------------------------------------------------------------------
// LoadUtility — SYS-115 story 3
// ---------------------------------------------------------------------------

/// Data format for LOAD/UNLOAD operations.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DataFormat {
    /// Fixed-width fields. Each column occupies a fixed number of characters.
    FixedWidth,
    /// Delimited fields separated by a delimiter character.
    Delimited,
}

/// LOAD utility — loads data from text records into a table.
#[derive(Debug)]
pub struct LoadUtility {
    /// Data format.
    pub format: DataFormat,
    /// Delimiter character (only used when format is Delimited).
    pub delimiter: char,
    /// Fixed-width column widths (only used when format is FixedWidth).
    /// Must match the number of columns in the target table.
    pub field_widths: Vec<usize>,
}

impl LoadUtility {
    /// Create a new LOAD utility configured for delimited input.
    pub fn delimited(delimiter: char) -> Self {
        Self {
            format: DataFormat::Delimited,
            delimiter,
            field_widths: Vec::new(),
        }
    }

    /// Create a new LOAD utility configured for fixed-width input.
    pub fn fixed_width(widths: Vec<usize>) -> Self {
        Self {
            format: DataFormat::FixedWidth,
            delimiter: ',',
            field_widths: widths,
        }
    }

    /// Load data from input records into the table.
    ///
    /// Each element of `records` is one input line/record.
    /// Returns the number of rows loaded.
    pub fn load(&self, table: &mut OpsTable, records: &[&str]) -> OpsResult<usize> {
        let col_names = table.column_names().iter().map(|s| s.to_string()).collect::<Vec<_>>();
        let mut count = 0usize;

        for (i, &record) in records.iter().enumerate() {
            let values = match self.format {
                DataFormat::Delimited => self.parse_delimited(record),
                DataFormat::FixedWidth => self.parse_fixed_width(record, i + 1)?,
            };

            if values.len() != col_names.len() {
                return Err(OpsError::ColumnCountMismatch {
                    expected: col_names.len(),
                    got: values.len(),
                });
            }

            let mut row = HashMap::new();
            for (name, val) in col_names.iter().zip(values.iter()) {
                row.insert(name.clone(), val.clone());
            }
            table.insert_row(row);
            count += 1;
        }

        Ok(count)
    }

    /// Parse a delimited record into field values.
    fn parse_delimited(&self, record: &str) -> Vec<String> {
        record
            .split(self.delimiter)
            .map(|f| f.trim().to_string())
            .collect()
    }

    /// Parse a fixed-width record into field values.
    fn parse_fixed_width(&self, record: &str, record_num: usize) -> OpsResult<Vec<String>> {
        let mut values = Vec::new();
        let mut pos = 0;

        for (col_idx, &width) in self.field_widths.iter().enumerate() {
            if pos + width > record.len() {
                return Err(OpsError::DataFormatError {
                    record: record_num,
                    message: format!(
                        "Record too short for column {} (need pos {}+{}, have {})",
                        col_idx + 1,
                        pos,
                        width,
                        record.len()
                    ),
                });
            }
            let field = record[pos..pos + width].trim().to_string();
            values.push(field);
            pos += width;
        }

        Ok(values)
    }
}

// ---------------------------------------------------------------------------
// UnloadUtility — SYS-115 story 4
// ---------------------------------------------------------------------------

/// UNLOAD utility — writes table rows to text records.
#[derive(Debug)]
pub struct UnloadUtility {
    /// Output data format.
    pub format: DataFormat,
    /// Delimiter character (for Delimited format).
    pub delimiter: char,
    /// Fixed-width column widths (for FixedWidth format).
    pub field_widths: Vec<usize>,
}

impl UnloadUtility {
    /// Create an UNLOAD utility configured for delimited output.
    pub fn delimited(delimiter: char) -> Self {
        Self {
            format: DataFormat::Delimited,
            delimiter,
            field_widths: Vec::new(),
        }
    }

    /// Create an UNLOAD utility configured for fixed-width output.
    pub fn fixed_width(widths: Vec<usize>) -> Self {
        Self {
            format: DataFormat::FixedWidth,
            delimiter: ',',
            field_widths: widths,
        }
    }

    /// Unload all rows from the table into text records.
    ///
    /// Returns a vector of formatted record strings.
    pub fn unload(&self, table: &OpsTable) -> OpsResult<Vec<String>> {
        let col_names = table.column_names().iter().map(|s| s.to_string()).collect::<Vec<_>>();
        let mut records = Vec::new();

        for row in &table.rows {
            let values: Vec<String> = col_names
                .iter()
                .map(|c| row.get(c.as_str()).cloned().unwrap_or_default())
                .collect();

            let record = match self.format {
                DataFormat::Delimited => self.format_delimited(&values),
                DataFormat::FixedWidth => self.format_fixed_width(&values)?,
            };
            records.push(record);
        }

        Ok(records)
    }

    /// Format a row as a delimited record.
    fn format_delimited(&self, values: &[String]) -> String {
        values.join(&self.delimiter.to_string())
    }

    /// Format a row as a fixed-width record.
    fn format_fixed_width(&self, values: &[String]) -> OpsResult<String> {
        let mut record = String::new();

        for (i, val) in values.iter().enumerate() {
            let width = self.field_widths.get(i).copied().unwrap_or(20);
            if val.len() > width {
                record.push_str(&val[..width]);
            } else {
                record.push_str(val);
                // Pad with spaces
                for _ in 0..width - val.len() {
                    record.push(' ');
                }
            }
        }

        Ok(record)
    }
}

// ---------------------------------------------------------------------------
// Tests — SYS-115 story 5
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    // -- Helpers -----------------------------------------------------------

    fn employee_table_def() -> OpsTableDef {
        OpsTableDef {
            name: "EMPLOYEE".to_string(),
            columns: vec![
                OpsColumnDef {
                    name: "EMPNO".to_string(),
                    data_type: "INTEGER".to_string(),
                    nullable: false,
                },
                OpsColumnDef {
                    name: "NAME".to_string(),
                    data_type: "VARCHAR(50)".to_string(),
                    nullable: false,
                },
                OpsColumnDef {
                    name: "SALARY".to_string(),
                    data_type: "DECIMAL(10,2)".to_string(),
                    nullable: true,
                },
            ],
        }
    }

    fn employee_table_with_data() -> OpsTable {
        let def = employee_table_def();
        let mut table = OpsTable::new(def);
        let mut row1 = HashMap::new();
        row1.insert("EMPNO".to_string(), "100".to_string());
        row1.insert("NAME".to_string(), "ALICE".to_string());
        row1.insert("SALARY".to_string(), "50000.00".to_string());
        table.insert_row(row1);

        let mut row2 = HashMap::new();
        row2.insert("EMPNO".to_string(), "200".to_string());
        row2.insert("NAME".to_string(), "BOB".to_string());
        row2.insert("SALARY".to_string(), "60000.00".to_string());
        table.insert_row(row2);

        table
    }

    // -- DSNTEP2 tests -----------------------------------------------------

    #[test]
    fn test_dsntep2_select_all() {
        let mut dsn = Dsntep2::new();
        dsn.register_table(employee_table_with_data());

        let result = dsn.execute("SELECT * FROM EMPLOYEE").unwrap();
        match result {
            Dsntep2Result::Rows { columns, rows } => {
                assert_eq!(columns.len(), 3);
                assert_eq!(rows.len(), 2);
                assert_eq!(rows[0][1], "ALICE");
                assert_eq!(rows[1][1], "BOB");
            }
            _ => panic!("Expected Rows result"),
        }
    }

    #[test]
    fn test_dsntep2_select_with_where() {
        let mut dsn = Dsntep2::new();
        dsn.register_table(employee_table_with_data());

        let result = dsn
            .execute("SELECT NAME, SALARY FROM EMPLOYEE WHERE EMPNO = '100'")
            .unwrap();
        match result {
            Dsntep2Result::Rows { columns, rows } => {
                assert_eq!(columns, vec!["NAME", "SALARY"]);
                assert_eq!(rows.len(), 1);
                assert_eq!(rows[0][0], "ALICE");
            }
            _ => panic!("Expected Rows result"),
        }
    }

    #[test]
    fn test_dsntep2_select_no_rows() {
        let mut dsn = Dsntep2::new();
        dsn.register_table(employee_table_with_data());

        let result = dsn
            .execute("SELECT * FROM EMPLOYEE WHERE EMPNO = '999'")
            .unwrap();
        match result {
            Dsntep2Result::Rows { rows, .. } => {
                assert!(rows.is_empty());
            }
            _ => panic!("Expected Rows result"),
        }
    }

    #[test]
    fn test_dsntep2_insert() {
        let mut dsn = Dsntep2::new();
        dsn.register_table(OpsTable::new(employee_table_def()));

        let result = dsn
            .execute("INSERT INTO EMPLOYEE (EMPNO, NAME, SALARY) VALUES ('300', 'CAROL', '70000.00')")
            .unwrap();
        match result {
            Dsntep2Result::RowCount { count } => assert_eq!(count, 1),
            _ => panic!("Expected RowCount result"),
        }

        // Verify via SELECT
        let sel = dsn.execute("SELECT * FROM EMPLOYEE").unwrap();
        match sel {
            Dsntep2Result::Rows { rows, .. } => assert_eq!(rows.len(), 1),
            _ => panic!("Expected Rows result"),
        }
    }

    #[test]
    fn test_dsntep2_update() {
        let mut dsn = Dsntep2::new();
        dsn.register_table(employee_table_with_data());

        let result = dsn
            .execute("UPDATE EMPLOYEE SET SALARY = '99999.00' WHERE NAME = 'ALICE'")
            .unwrap();
        match result {
            Dsntep2Result::RowCount { count } => assert_eq!(count, 1),
            _ => panic!("Expected RowCount result"),
        }

        // Verify
        let sel = dsn
            .execute("SELECT SALARY FROM EMPLOYEE WHERE NAME = 'ALICE'")
            .unwrap();
        match sel {
            Dsntep2Result::Rows { rows, .. } => {
                assert_eq!(rows[0][0], "99999.00");
            }
            _ => panic!("Expected Rows result"),
        }
    }

    #[test]
    fn test_dsntep2_delete() {
        let mut dsn = Dsntep2::new();
        dsn.register_table(employee_table_with_data());

        let result = dsn
            .execute("DELETE FROM EMPLOYEE WHERE NAME = 'BOB'")
            .unwrap();
        match result {
            Dsntep2Result::RowCount { count } => assert_eq!(count, 1),
            _ => panic!("Expected RowCount result"),
        }

        let sel = dsn.execute("SELECT * FROM EMPLOYEE").unwrap();
        match sel {
            Dsntep2Result::Rows { rows, .. } => assert_eq!(rows.len(), 1),
            _ => panic!("Expected Rows result"),
        }
    }

    #[test]
    fn test_dsntep2_delete_all() {
        let mut dsn = Dsntep2::new();
        dsn.register_table(employee_table_with_data());

        let result = dsn.execute("DELETE FROM EMPLOYEE").unwrap();
        match result {
            Dsntep2Result::RowCount { count } => assert_eq!(count, 2),
            _ => panic!("Expected RowCount result"),
        }
    }

    #[test]
    fn test_dsntep2_create_table() {
        let mut dsn = Dsntep2::new();

        let result = dsn
            .execute("CREATE TABLE DEPARTMENT (DEPTNO INTEGER NOT NULL, DNAME VARCHAR(40))")
            .unwrap();
        match result {
            Dsntep2Result::DdlSuccess { message } => {
                assert!(message.contains("DEPARTMENT"));
            }
            _ => panic!("Expected DdlSuccess result"),
        }

        // Verify table exists by inserting
        dsn.execute("INSERT INTO DEPARTMENT (DEPTNO, DNAME) VALUES ('10', 'SALES')")
            .unwrap();
    }

    #[test]
    fn test_dsntep2_drop_table() {
        let mut dsn = Dsntep2::new();
        dsn.register_table(employee_table_with_data());

        let result = dsn.execute("DROP TABLE EMPLOYEE").unwrap();
        match result {
            Dsntep2Result::DdlSuccess { message } => {
                assert!(message.contains("EMPLOYEE"));
            }
            _ => panic!("Expected DdlSuccess result"),
        }

        // Now SELECT should fail
        let err = dsn.execute("SELECT * FROM EMPLOYEE").unwrap_err();
        assert!(matches!(err, OpsError::TableNotFound(_)));
    }

    #[test]
    fn test_dsntep2_alter_table() {
        let mut dsn = Dsntep2::new();
        let result = dsn
            .execute("ALTER TABLE EMPLOYEE ADD COLUMN DEPT INTEGER")
            .unwrap();
        match result {
            Dsntep2Result::DdlSuccess { message } => {
                assert!(message.contains("ALTER"));
            }
            _ => panic!("Expected DdlSuccess result"),
        }
    }

    #[test]
    fn test_dsntep2_table_not_found() {
        let mut dsn = Dsntep2::new();
        let err = dsn.execute("SELECT * FROM NONEXIST").unwrap_err();
        assert!(matches!(err, OpsError::TableNotFound(_)));
    }

    #[test]
    fn test_dsntep2_unsupported_sql() {
        let mut dsn = Dsntep2::new();
        let err = dsn.execute("GRANT SELECT ON TABLE T TO PUBLIC").unwrap_err();
        assert!(matches!(err, OpsError::SqlError(_)));
    }

    // -- DclgenUtil tests --------------------------------------------------

    #[test]
    fn test_dclgen_cobol_copybook() {
        let def = employee_table_def();
        let output = DclgenUtil::generate(&def);

        assert!(output.cobol_copybook.contains("DCLEMPLOYEE"));
        assert!(output.cobol_copybook.contains("EMPNO"));
        assert!(output.cobol_copybook.contains("PIC S9(9) COMP")); // INTEGER
        assert!(output.cobol_copybook.contains("PIC X(50)")); // VARCHAR(50)
        assert!(output.cobol_copybook.contains("PIC S9(8)V9(2) COMP-3")); // DECIMAL(10,2)
    }

    #[test]
    fn test_dclgen_sql_declare() {
        let def = employee_table_def();
        let output = DclgenUtil::generate(&def);

        assert!(output.sql_declare.contains("EXEC SQL DECLARE EMPLOYEE TABLE"));
        assert!(output.sql_declare.contains("EMPNO INTEGER NOT NULL"));
        assert!(output.sql_declare.contains("NAME VARCHAR(50) NOT NULL"));
        assert!(output.sql_declare.contains("SALARY DECIMAL(10,2)"));
        assert!(output.sql_declare.contains("END-EXEC"));
    }

    #[test]
    fn test_dclgen_host_variables() {
        let def = employee_table_def();
        let output = DclgenUtil::generate(&def);

        assert!(output.host_variables.contains("H-EMPLOYEE"));
        assert!(output.host_variables.contains("H-EMPNO"));
        assert!(output.host_variables.contains("H-NAME"));
        assert!(output.host_variables.contains("H-SALARY"));
    }

    #[test]
    fn test_dclgen_various_types() {
        let def = OpsTableDef {
            name: "TYPES_TEST".to_string(),
            columns: vec![
                OpsColumnDef {
                    name: "COL_SMALL".to_string(),
                    data_type: "SMALLINT".to_string(),
                    nullable: false,
                },
                OpsColumnDef {
                    name: "COL_BIG".to_string(),
                    data_type: "BIGINT".to_string(),
                    nullable: false,
                },
                OpsColumnDef {
                    name: "COL_CHAR".to_string(),
                    data_type: "CHAR(20)".to_string(),
                    nullable: false,
                },
                OpsColumnDef {
                    name: "COL_DATE".to_string(),
                    data_type: "DATE".to_string(),
                    nullable: true,
                },
                OpsColumnDef {
                    name: "COL_TS".to_string(),
                    data_type: "TIMESTAMP".to_string(),
                    nullable: true,
                },
                OpsColumnDef {
                    name: "COL_TIME".to_string(),
                    data_type: "TIME".to_string(),
                    nullable: true,
                },
            ],
        };
        let output = DclgenUtil::generate(&def);

        assert!(output.cobol_copybook.contains("PIC S9(4) COMP")); // SMALLINT
        assert!(output.cobol_copybook.contains("PIC S9(18) COMP")); // BIGINT
        assert!(output.cobol_copybook.contains("PIC X(20)")); // CHAR(20)
        assert!(output.cobol_copybook.contains("PIC X(10)")); // DATE
        assert!(output.cobol_copybook.contains("PIC X(26)")); // TIMESTAMP
        assert!(output.cobol_copybook.contains("PIC X(8)")); // TIME
    }

    // -- LoadUtility tests -------------------------------------------------

    #[test]
    fn test_load_delimited() {
        let def = employee_table_def();
        let mut table = OpsTable::new(def);

        let loader = LoadUtility::delimited(',');
        let records = vec!["100,ALICE,50000.00", "200,BOB,60000.00"];

        let count = loader.load(&mut table, &records).unwrap();
        assert_eq!(count, 2);
        assert_eq!(table.rows.len(), 2);
        assert_eq!(table.rows[0].get("NAME").unwrap(), "ALICE");
        assert_eq!(table.rows[1].get("EMPNO").unwrap(), "200");
    }

    #[test]
    fn test_load_fixed_width() {
        let def = employee_table_def();
        let mut table = OpsTable::new(def);

        let loader = LoadUtility::fixed_width(vec![5, 10, 10]);
        let records = vec![
            "100  ALICE     50000.00  ",
            "200  BOB       60000.00  ",
        ];

        let count = loader.load(&mut table, &records).unwrap();
        assert_eq!(count, 2);
        assert_eq!(table.rows[0].get("EMPNO").unwrap(), "100");
        assert_eq!(table.rows[0].get("NAME").unwrap(), "ALICE");
    }

    #[test]
    fn test_load_column_count_mismatch() {
        let def = employee_table_def();
        let mut table = OpsTable::new(def);

        let loader = LoadUtility::delimited(',');
        let records = vec!["100,ALICE"]; // Missing SALARY

        let err = loader.load(&mut table, &records).unwrap_err();
        assert!(matches!(err, OpsError::ColumnCountMismatch { .. }));
    }

    #[test]
    fn test_load_fixed_width_record_too_short() {
        let def = employee_table_def();
        let mut table = OpsTable::new(def);

        let loader = LoadUtility::fixed_width(vec![5, 10, 10]);
        let records = vec!["100"]; // Too short

        let err = loader.load(&mut table, &records).unwrap_err();
        assert!(matches!(err, OpsError::DataFormatError { .. }));
    }

    // -- UnloadUtility tests -----------------------------------------------

    #[test]
    fn test_unload_delimited() {
        let table = employee_table_with_data();
        let unloader = UnloadUtility::delimited('|');

        let records = unloader.unload(&table).unwrap();
        assert_eq!(records.len(), 2);
        assert!(records[0].contains('|'));
        assert!(records[0].contains("100"));
        assert!(records[0].contains("ALICE"));
    }

    #[test]
    fn test_unload_fixed_width() {
        let table = employee_table_with_data();
        let unloader = UnloadUtility::fixed_width(vec![5, 10, 12]);

        let records = unloader.unload(&table).unwrap();
        assert_eq!(records.len(), 2);
        // Each record should be 5 + 10 + 12 = 27 chars
        assert_eq!(records[0].len(), 27);
        assert!(records[0].starts_with("100  ")); // 5 chars padded
    }

    // -- LOAD/UNLOAD round-trip tests --------------------------------------

    #[test]
    fn test_load_unload_delimited_roundtrip() {
        let def = employee_table_def();
        let mut table = OpsTable::new(def.clone());

        // Load
        let loader = LoadUtility::delimited(',');
        let input = vec!["100,ALICE,50000.00", "200,BOB,60000.00"];
        loader.load(&mut table, &input).unwrap();

        // Unload
        let unloader = UnloadUtility::delimited(',');
        let output = unloader.unload(&table).unwrap();

        assert_eq!(output.len(), 2);

        // Reload into a fresh table
        let mut table2 = OpsTable::new(def);
        let output_refs: Vec<&str> = output.iter().map(|s| s.as_str()).collect();
        loader.load(&mut table2, &output_refs).unwrap();

        assert_eq!(table2.rows.len(), 2);
        assert_eq!(table2.rows[0].get("NAME").unwrap(), "ALICE");
        assert_eq!(table2.rows[1].get("SALARY").unwrap(), "60000.00");
    }

    #[test]
    fn test_load_unload_fixed_width_roundtrip() {
        let def = employee_table_def();
        let mut table = OpsTable::new(def.clone());

        let widths = vec![6, 12, 12];

        // Load
        let loader = LoadUtility::fixed_width(widths.clone());
        let input = vec![
            "100   ALICE       50000.00    ",
            "200   BOB         60000.00    ",
        ];
        loader.load(&mut table, &input).unwrap();

        // Unload
        let unloader = UnloadUtility::fixed_width(widths.clone());
        let output = unloader.unload(&table).unwrap();
        assert_eq!(output.len(), 2);

        // Each record should be exactly 6 + 12 + 12 = 30 chars
        for rec in &output {
            assert_eq!(rec.len(), 30);
        }

        // Reload
        let mut table2 = OpsTable::new(def);
        let output_refs: Vec<&str> = output.iter().map(|s| s.as_str()).collect();
        let loader2 = LoadUtility::fixed_width(widths);
        loader2.load(&mut table2, &output_refs).unwrap();

        assert_eq!(table2.rows.len(), 2);
        assert_eq!(table2.rows[0].get("EMPNO").unwrap(), "100");
        assert_eq!(table2.rows[1].get("NAME").unwrap(), "BOB");
    }

    #[test]
    fn test_unload_empty_table() {
        let table = OpsTable::new(employee_table_def());
        let unloader = UnloadUtility::delimited(',');
        let records = unloader.unload(&table).unwrap();
        assert!(records.is_empty());
    }

    // -- Integration: DSNTEP2 + LOAD/UNLOAD --------------------------------

    #[test]
    fn test_dsntep2_insert_then_unload() {
        let mut dsn = Dsntep2::new();
        dsn.register_table(OpsTable::new(employee_table_def()));

        dsn.execute("INSERT INTO EMPLOYEE (EMPNO, NAME, SALARY) VALUES ('300', 'CAROL', '70000.00')")
            .unwrap();
        dsn.execute("INSERT INTO EMPLOYEE (EMPNO, NAME, SALARY) VALUES ('400', 'DAVE', '80000.00')")
            .unwrap();

        // Grab the table reference for unload
        let table = dsn.tables.get("EMPLOYEE").unwrap();
        let unloader = UnloadUtility::delimited(',');
        let records = unloader.unload(table).unwrap();
        assert_eq!(records.len(), 2);
    }
}
