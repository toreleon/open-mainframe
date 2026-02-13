//! DBRM (Database Request Module) generation.
//!
//! The DBRM contains SQL statements extracted from a COBOL program,
//! used by the BIND process to create access paths.

use crate::{Db2Error, Db2Result};
use super::{SqlStatement, HostVariable};
use serde::{Deserialize, Serialize};
use std::io::Write;
use std::path::Path;

/// Database Request Module containing extracted SQL.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Dbrm {
    /// Program name
    pub program_name: String,
    /// Timestamp when created
    pub timestamp: String,
    /// SQL statements
    pub statements: Vec<DbrmStatement>,
    /// Collection ID for binding
    pub collection_id: Option<String>,
}

/// A SQL statement in the DBRM.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DbrmStatement {
    /// Statement number
    pub number: usize,
    /// SQL text
    pub sql: String,
    /// Statement type
    pub stmt_type: String,
    /// Host variables used
    pub host_variables: Vec<DbrmHostVariable>,
    /// Source line number
    pub source_line: usize,
}

/// Host variable in DBRM.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DbrmHostVariable {
    /// Variable name
    pub name: String,
    /// Indicator variable name
    pub indicator: Option<String>,
    /// Usage (input/output)
    pub usage: String,
}

impl Dbrm {
    /// Create a new DBRM.
    pub fn new(program_name: &str) -> Self {
        Self {
            program_name: program_name.to_string(),
            timestamp: chrono_lite_timestamp(),
            statements: Vec::new(),
            collection_id: None,
        }
    }

    /// Add statements from preprocessing result.
    pub fn add_statements(
        &mut self,
        sql_statements: &[SqlStatement],
        host_variables: &[HostVariable],
    ) {
        for stmt in sql_statements {
            // Get host variables for this statement
            let vars: Vec<DbrmHostVariable> = host_variables
                .iter()
                .filter(|v| v.statement_number == stmt.number)
                .map(|v| DbrmHostVariable {
                    name: v.name.clone(),
                    indicator: v.indicator.clone(),
                    usage: format!("{:?}", v.usage),
                })
                .collect();

            self.statements.push(DbrmStatement {
                number: stmt.number,
                sql: stmt.sql.clone(),
                stmt_type: format!("{:?}", stmt.stmt_type),
                host_variables: vars,
                source_line: stmt.start_line,
            });
        }
    }

    /// Set the collection ID for binding.
    pub fn set_collection_id(&mut self, id: &str) {
        self.collection_id = Some(id.to_string());
    }

    /// Write DBRM to a file.
    pub fn write_to_file(&self, path: &Path) -> Db2Result<()> {
        let json = serde_json::to_string_pretty(self)
            .map_err(|e| Db2Error::Io(std::io::Error::new(
                std::io::ErrorKind::Other,
                e.to_string(),
            )))?;

        let mut file = std::fs::File::create(path)?;
        file.write_all(json.as_bytes())?;
        Ok(())
    }

    /// Read DBRM from a file.
    pub fn read_from_file(path: &Path) -> Db2Result<Self> {
        let content = std::fs::read_to_string(path)?;
        let dbrm: Dbrm = serde_json::from_str(&content)
            .map_err(|e| Db2Error::Io(std::io::Error::new(
                std::io::ErrorKind::InvalidData,
                e.to_string(),
            )))?;
        Ok(dbrm)
    }

    /// Generate human-readable listing.
    pub fn to_listing(&self) -> String {
        let mut output = String::new();

        output.push_str(&format!(
            "DBRM LISTING FOR PROGRAM: {}\n",
            self.program_name
        ));
        output.push_str(&format!("CREATED: {}\n", self.timestamp));
        if let Some(ref coll) = self.collection_id {
            output.push_str(&format!("COLLECTION: {}\n", coll));
        }
        output.push_str(&"=".repeat(72));
        output.push('\n');
        output.push('\n');

        for stmt in &self.statements {
            output.push_str(&format!(
                "STATEMENT {:03} (LINE {}) - {}\n",
                stmt.number, stmt.source_line, stmt.stmt_type
            ));
            output.push_str(&"-".repeat(72));
            output.push('\n');
            output.push_str(&stmt.sql);
            output.push('\n');

            if !stmt.host_variables.is_empty() {
                output.push_str("\nHOST VARIABLES:\n");
                for var in &stmt.host_variables {
                    output.push_str(&format!("  {:20} {}", var.name, var.usage));
                    if let Some(ref ind) = var.indicator {
                        output.push_str(&format!(" (INDICATOR: {})", ind));
                    }
                    output.push('\n');
                }
            }
            output.push('\n');
        }

        output.push_str(&format!(
            "TOTAL STATEMENTS: {}\n",
            self.statements.len()
        ));

        output
    }
}

/// Simple timestamp without external dependency.
fn chrono_lite_timestamp() -> String {
    // Use a simple format since we don't have chrono
    "2026-02-13T00:00:00Z".to_string()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::preprocess::{HostVariableUsage, SqlStatement, SqlStatementType};

    #[test]
    fn test_dbrm_creation() {
        let dbrm = Dbrm::new("TESTPROG");
        assert_eq!(dbrm.program_name, "TESTPROG");
        assert!(dbrm.statements.is_empty());
    }

    #[test]
    fn test_add_statements() {
        let mut dbrm = Dbrm::new("TESTPROG");

        let statements = vec![SqlStatement {
            number: 1,
            sql: "SELECT NAME INTO :WS-NAME FROM CUST WHERE ID = :WS-ID".to_string(),
            start_line: 10,
            end_line: 12,
            stmt_type: SqlStatementType::SelectInto,
        }];

        let host_vars = vec![
            HostVariable {
                name: "WS-NAME".to_string(),
                indicator: None,
                statement_number: 1,
                usage: HostVariableUsage::Output,
            },
            HostVariable {
                name: "WS-ID".to_string(),
                indicator: None,
                statement_number: 1,
                usage: HostVariableUsage::Input,
            },
        ];

        dbrm.add_statements(&statements, &host_vars);

        assert_eq!(dbrm.statements.len(), 1);
        assert_eq!(dbrm.statements[0].host_variables.len(), 2);
    }

    #[test]
    fn test_dbrm_listing() {
        let mut dbrm = Dbrm::new("TESTPROG");
        dbrm.set_collection_id("MYCOLL");

        let statements = vec![SqlStatement {
            number: 1,
            sql: "SELECT * FROM T".to_string(),
            start_line: 10,
            end_line: 10,
            stmt_type: SqlStatementType::Other,
        }];

        dbrm.add_statements(&statements, &[]);

        let listing = dbrm.to_listing();
        assert!(listing.contains("TESTPROG"));
        assert!(listing.contains("MYCOLL"));
        assert!(listing.contains("STATEMENT 001"));
    }

    #[test]
    fn test_dbrm_file_roundtrip() {
        let mut dbrm = Dbrm::new("TESTPROG");
        dbrm.add_statements(
            &[SqlStatement {
                number: 1,
                sql: "SELECT 1".to_string(),
                start_line: 1,
                end_line: 1,
                stmt_type: SqlStatementType::Other,
            }],
            &[],
        );

        let temp_dir = std::env::temp_dir();
        let temp_file = temp_dir.join("test_dbrm.json");

        dbrm.write_to_file(&temp_file).unwrap();
        let loaded = Dbrm::read_from_file(&temp_file).unwrap();

        assert_eq!(loaded.program_name, "TESTPROG");
        assert_eq!(loaded.statements.len(), 1);

        std::fs::remove_file(&temp_file).ok();
    }
}
