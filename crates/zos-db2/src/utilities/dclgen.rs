//! DCLGEN utility for generating COBOL copybooks from database tables.
//!
//! Generates COBOL data structures matching PostgreSQL table schemas.

use crate::{Db2Error, Db2Result};
use std::path::Path;

/// Column information from database.
#[derive(Debug, Clone)]
pub struct ColumnInfo {
    /// Column name
    pub name: String,
    /// Data type
    pub data_type: String,
    /// Maximum length (for char types)
    pub max_length: Option<i32>,
    /// Numeric precision
    pub precision: Option<i32>,
    /// Numeric scale
    pub scale: Option<i32>,
    /// Whether column is nullable
    pub nullable: bool,
}

/// Table information for DCLGEN.
#[derive(Debug, Clone)]
pub struct TableInfo {
    /// Schema name
    pub schema: String,
    /// Table name
    pub table: String,
    /// Column information
    pub columns: Vec<ColumnInfo>,
}

/// DCLGEN output options.
#[derive(Debug, Clone)]
pub struct DclgenOptions {
    /// Output structure name (default: derived from table name)
    pub structure_name: Option<String>,
    /// Add null indicators
    pub null_indicators: bool,
    /// Column name prefix
    pub prefix: Option<String>,
    /// Language (COBOL or PL/I)
    pub language: DclgenLanguage,
}

impl Default for DclgenOptions {
    fn default() -> Self {
        Self {
            structure_name: None,
            null_indicators: true,
            prefix: None,
            language: DclgenLanguage::Cobol,
        }
    }
}

/// Target language for DCLGEN.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DclgenLanguage {
    /// COBOL copybook
    Cobol,
    /// PL/I include
    Pli,
}

/// DCLGEN utility.
pub struct Dclgen {
    options: DclgenOptions,
}

impl Dclgen {
    /// Create a new DCLGEN instance.
    pub fn new() -> Self {
        Self {
            options: DclgenOptions::default(),
        }
    }

    /// Create with options.
    pub fn with_options(options: DclgenOptions) -> Self {
        Self { options }
    }

    /// Generate COBOL copybook from table information.
    pub fn generate(&self, table: &TableInfo) -> Db2Result<String> {
        match self.options.language {
            DclgenLanguage::Cobol => self.generate_cobol(table),
            DclgenLanguage::Pli => self.generate_pli(table),
        }
    }

    /// Generate COBOL copybook.
    fn generate_cobol(&self, table: &TableInfo) -> Db2Result<String> {
        let mut output = String::new();

        // Header comment
        output.push_str("      ******************************************************************\n");
        output.push_str(&format!(
            "      * DCLGEN TABLE({}.{})\n",
            table.schema, table.table
        ));
        output.push_str("      *        LIBRARY()                                              \n");
        output.push_str("      *        LANGUAGE(COBOL)                                        \n");
        output.push_str("      ******************************************************************\n");

        // Structure name
        let struct_name = self
            .options
            .structure_name
            .clone()
            .unwrap_or_else(|| format!("DCL{}", table.table.to_uppercase()));

        // Main structure
        output.push_str(&format!("       01  {}.\n", struct_name));

        // Generate column declarations
        for col in &table.columns {
            let cobol_name = self.to_cobol_name(&col.name);
            let pic_clause = self.to_cobol_pic(col);

            output.push_str(&format!("           10  {} {}.\n", cobol_name, pic_clause));
        }

        // Generate null indicators if requested
        if self.options.null_indicators {
            output.push_str("      ******************************************************************\n");
            output.push_str("      * NULL INDICATORS\n");
            output.push_str("      ******************************************************************\n");

            let ind_name = format!("{}-IND", struct_name);
            output.push_str(&format!("       01  {}.\n", ind_name));

            for col in &table.columns {
                if col.nullable {
                    let cobol_name = self.to_cobol_name(&col.name);
                    output.push_str(&format!(
                        "           10  {}-IND PIC S9(4) COMP.\n",
                        cobol_name
                    ));
                }
            }
        }

        Ok(output)
    }

    /// Generate PL/I include (stub).
    fn generate_pli(&self, table: &TableInfo) -> Db2Result<String> {
        let mut output = String::new();

        output.push_str(&format!(
            "/* DCLGEN TABLE({}.{}) */\n",
            table.schema, table.table
        ));
        output.push_str("/* LANGUAGE(PLI)          */\n");

        let struct_name = self
            .options
            .structure_name
            .clone()
            .unwrap_or_else(|| format!("DCL{}", table.table.to_uppercase()));

        output.push_str(&format!("DCL 1 {},\n", struct_name));

        for (i, col) in table.columns.iter().enumerate() {
            let pli_name = col.name.to_uppercase();
            let pli_type = self.to_pli_type(col);
            let comma = if i < table.columns.len() - 1 { "," } else { ";" };

            output.push_str(&format!("      5 {} {}{}\n", pli_name, pli_type, comma));
        }

        Ok(output)
    }

    /// Convert column name to COBOL-compatible name.
    fn to_cobol_name(&self, name: &str) -> String {
        let mut result = name.to_uppercase().replace('_', "-");

        // Add prefix if specified
        if let Some(ref prefix) = self.options.prefix {
            result = format!("{}-{}", prefix, result);
        }

        // Ensure it starts with a letter
        if result.chars().next().map(|c| c.is_ascii_digit()).unwrap_or(false) {
            result = format!("F-{}", result);
        }

        // Truncate to 30 characters (COBOL limit)
        if result.len() > 30 {
            result.truncate(30);
        }

        result
    }

    /// Convert database type to COBOL PIC clause.
    fn to_cobol_pic(&self, col: &ColumnInfo) -> String {
        let data_type = col.data_type.to_uppercase();

        match data_type.as_str() {
            "INTEGER" | "INT" | "INT4" => "PIC S9(9) COMP".to_string(),
            "SMALLINT" | "INT2" => "PIC S9(4) COMP".to_string(),
            "BIGINT" | "INT8" => "PIC S9(18) COMP".to_string(),
            "DECIMAL" | "NUMERIC" => {
                let prec = col.precision.unwrap_or(18);
                let scale = col.scale.unwrap_or(0);
                if scale > 0 {
                    format!("PIC S9({})V9({}) COMP-3", prec - scale, scale)
                } else {
                    format!("PIC S9({}) COMP-3", prec)
                }
            }
            "REAL" | "FLOAT4" => "COMP-1".to_string(),
            "DOUBLE PRECISION" | "FLOAT8" | "FLOAT" => "COMP-2".to_string(),
            "CHAR" | "CHARACTER" => {
                let len = col.max_length.unwrap_or(1);
                format!("PIC X({})", len)
            }
            "VARCHAR" | "CHARACTER VARYING" | "TEXT" => {
                let len = col.max_length.unwrap_or(255);
                format!("PIC X({})", len)
            }
            "DATE" => "PIC X(10)".to_string(),
            "TIME" => "PIC X(8)".to_string(),
            "TIMESTAMP" => "PIC X(26)".to_string(),
            "BYTEA" => {
                let len = col.max_length.unwrap_or(255);
                format!("PIC X({})", len)
            }
            "BOOLEAN" | "BOOL" => "PIC X(1)".to_string(),
            _ => {
                // Default to character
                let len = col.max_length.unwrap_or(255);
                format!("PIC X({})", len)
            }
        }
    }

    /// Convert database type to PL/I type.
    fn to_pli_type(&self, col: &ColumnInfo) -> String {
        let data_type = col.data_type.to_uppercase();

        match data_type.as_str() {
            "INTEGER" | "INT" | "INT4" => "FIXED BIN(31)".to_string(),
            "SMALLINT" | "INT2" => "FIXED BIN(15)".to_string(),
            "BIGINT" | "INT8" => "FIXED BIN(63)".to_string(),
            "DECIMAL" | "NUMERIC" => {
                let prec = col.precision.unwrap_or(18);
                let scale = col.scale.unwrap_or(0);
                format!("FIXED DEC({},{})", prec, scale)
            }
            "REAL" | "FLOAT4" => "FLOAT BIN(21)".to_string(),
            "DOUBLE PRECISION" | "FLOAT8" | "FLOAT" => "FLOAT BIN(53)".to_string(),
            "CHAR" | "CHARACTER" => {
                let len = col.max_length.unwrap_or(1);
                format!("CHAR({})", len)
            }
            "VARCHAR" | "CHARACTER VARYING" | "TEXT" => {
                let len = col.max_length.unwrap_or(255);
                format!("CHAR({}) VARYING", len)
            }
            _ => {
                let len = col.max_length.unwrap_or(255);
                format!("CHAR({})", len)
            }
        }
    }

    /// Write generated copybook to file.
    pub fn write_to_file(&self, table: &TableInfo, path: &Path) -> Db2Result<()> {
        let content = self.generate(table)?;
        std::fs::write(path, content).map_err(Db2Error::Io)?;
        Ok(())
    }
}

impl Default for Dclgen {
    fn default() -> Self {
        Self::new()
    }
}

/// Parse PostgreSQL type string into ColumnInfo.
pub fn parse_pg_type(type_str: &str) -> (String, Option<i32>, Option<i32>, Option<i32>) {
    let upper = type_str.to_uppercase();
    let trimmed = upper.trim();

    // Handle parameterized types
    if let Some(paren_pos) = trimmed.find('(') {
        let base_type = trimmed[..paren_pos].to_string();
        let params = &trimmed[paren_pos + 1..trimmed.len() - 1];

        let parts: Vec<&str> = params.split(',').collect();
        let first: Option<i32> = parts.first().and_then(|s| s.trim().parse().ok());
        let second: Option<i32> = parts.get(1).and_then(|s| s.trim().parse().ok());

        if base_type == "NUMERIC" || base_type == "DECIMAL" {
            (base_type, None, first, second)
        } else {
            (base_type, first, None, None)
        }
    } else {
        (trimmed.to_string(), None, None, None)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn create_test_table() -> TableInfo {
        TableInfo {
            schema: "PUBLIC".to_string(),
            table: "CUSTOMER".to_string(),
            columns: vec![
                ColumnInfo {
                    name: "CUST_ID".to_string(),
                    data_type: "INTEGER".to_string(),
                    max_length: None,
                    precision: None,
                    scale: None,
                    nullable: false,
                },
                ColumnInfo {
                    name: "CUST_NAME".to_string(),
                    data_type: "VARCHAR".to_string(),
                    max_length: Some(50),
                    precision: None,
                    scale: None,
                    nullable: true,
                },
                ColumnInfo {
                    name: "BALANCE".to_string(),
                    data_type: "DECIMAL".to_string(),
                    max_length: None,
                    precision: Some(10),
                    scale: Some(2),
                    nullable: true,
                },
                ColumnInfo {
                    name: "CREATED_DATE".to_string(),
                    data_type: "DATE".to_string(),
                    max_length: None,
                    precision: None,
                    scale: None,
                    nullable: false,
                },
            ],
        }
    }

    #[test]
    fn test_generate_cobol() {
        let dclgen = Dclgen::new();
        let table = create_test_table();

        let result = dclgen.generate(&table).unwrap();

        assert!(result.contains("DCLCUSTOMER"));
        assert!(result.contains("CUST-ID"));
        assert!(result.contains("CUST-NAME"));
        assert!(result.contains("BALANCE"));
        assert!(result.contains("PIC S9(9) COMP")); // INTEGER
        assert!(result.contains("PIC X(50)")); // VARCHAR(50)
        assert!(result.contains("PIC S9(8)V9(2) COMP-3")); // DECIMAL(10,2)
    }

    #[test]
    fn test_null_indicators() {
        let dclgen = Dclgen::new();
        let table = create_test_table();

        let result = dclgen.generate(&table).unwrap();

        assert!(result.contains("NULL INDICATORS"));
        assert!(result.contains("CUST-NAME-IND"));
        assert!(result.contains("BALANCE-IND"));
        // CUST_ID and CREATED_DATE are not nullable, so no indicators
    }

    #[test]
    fn test_no_null_indicators() {
        let options = DclgenOptions {
            null_indicators: false,
            ..Default::default()
        };
        let dclgen = Dclgen::with_options(options);
        let table = create_test_table();

        let result = dclgen.generate(&table).unwrap();

        assert!(!result.contains("NULL INDICATORS"));
    }

    #[test]
    fn test_custom_structure_name() {
        let options = DclgenOptions {
            structure_name: Some("WS-CUSTOMER-REC".to_string()),
            ..Default::default()
        };
        let dclgen = Dclgen::with_options(options);
        let table = create_test_table();

        let result = dclgen.generate(&table).unwrap();

        assert!(result.contains("WS-CUSTOMER-REC"));
    }

    #[test]
    fn test_column_prefix() {
        let options = DclgenOptions {
            prefix: Some("WS".to_string()),
            ..Default::default()
        };
        let dclgen = Dclgen::with_options(options);
        let table = create_test_table();

        let result = dclgen.generate(&table).unwrap();

        assert!(result.contains("WS-CUST-ID"));
        assert!(result.contains("WS-CUST-NAME"));
    }

    #[test]
    fn test_pli_generation() {
        let options = DclgenOptions {
            language: DclgenLanguage::Pli,
            ..Default::default()
        };
        let dclgen = Dclgen::with_options(options);
        let table = create_test_table();

        let result = dclgen.generate(&table).unwrap();

        assert!(result.contains("DCL 1 DCLCUSTOMER"));
        assert!(result.contains("FIXED BIN(31)")); // INTEGER
        assert!(result.contains("CHAR(50) VARYING")); // VARCHAR(50)
    }

    #[test]
    fn test_parse_pg_type() {
        let (base, len, prec, _scale) = parse_pg_type("VARCHAR(100)");
        assert_eq!(base, "VARCHAR");
        assert_eq!(len, Some(100));
        assert_eq!(prec, None);

        let (base, _, prec, scale) = parse_pg_type("NUMERIC(10,2)");
        assert_eq!(base, "NUMERIC");
        assert_eq!(prec, Some(10));
        assert_eq!(scale, Some(2));

        let (base, _, _, _) = parse_pg_type("INTEGER");
        assert_eq!(base, "INTEGER");
    }

    #[test]
    fn test_cobol_name_conversion() {
        let dclgen = Dclgen::new();

        // Test underscore to hyphen conversion
        let table = TableInfo {
            schema: "PUBLIC".to_string(),
            table: "TEST".to_string(),
            columns: vec![ColumnInfo {
                name: "my_column_name".to_string(),
                data_type: "INTEGER".to_string(),
                max_length: None,
                precision: None,
                scale: None,
                nullable: false,
            }],
        };

        let result = dclgen.generate(&table).unwrap();
        assert!(result.contains("MY-COLUMN-NAME"));
    }
}
