//! SQL execution handlers for DRDA.
//!
//! Routes SQL-related DDM commands to the appropriate DB2 executor:
//! - EXCSQLIMM (Execute Immediate) — DML/DDL statements
//! - OPNQRY (Open Query) — SELECT queries
//! - CNTQRY (Continue Query) — fetch more rows
//! - CLSQRY (Close Query) — close result set
//! - PRPSQLSTT (Prepare) — prepare a statement
//! - EXCSQLSTT (Execute) — execute a prepared statement
//! - RDBCMM (Commit) / RDBRLLBCK (Rollback)

use std::collections::HashMap;

use crate::code_points::*;
use crate::ddm::DdmObject;
use crate::dss::DssSegment;
use crate::error::DrdaResult;
use crate::response::{self, ColumnDesc, ColumnValue, FdocaType};

/// Tracks open queries (cursors) for a connection.
#[derive(Debug)]
pub struct QueryState {
    /// Open cursors: query ID → result set.
    cursors: HashMap<u64, QueryCursor>,
    /// Next cursor ID.
    next_cursor_id: u64,
}

/// An open query cursor with its result set.
#[derive(Debug)]
#[allow(dead_code)]
struct QueryCursor {
    /// Column descriptions.
    columns: Vec<ColumnDesc>,
    /// Remaining rows to return.
    rows: Vec<Vec<ColumnValue>>,
    /// Current position in the result set.
    position: usize,
}

impl QueryState {
    /// Create a new empty query state.
    pub fn new() -> Self {
        Self {
            cursors: HashMap::new(),
            next_cursor_id: 1,
        }
    }

    /// Handle EXCSQLIMM (Execute Immediate SQL).
    ///
    /// For DML/DDL statements that don't return a result set.
    pub fn handle_excsqlimm(
        &mut self,
        request: &DdmObject,
        correlation_id: u16,
    ) -> DrdaResult<Vec<DssSegment>> {
        // Extract SQL text from the request's parameters or companion SQLSTT object
        let sql = self.extract_sql_text(request)?;
        tracing::info!(sql = %sql, "EXCSQLIMM");

        // Parse SQL to determine type and generate mock response
        let sql_upper = sql.trim().to_uppercase();

        if sql_upper.starts_with("SELECT") {
            // SELECT via EXCSQLIMM — some clients do this for single-row queries
            return self.handle_select_immediate(&sql, correlation_id);
        }

        // DML/DDL — return success SQLCARD
        let rows_affected = if sql_upper.starts_with("INSERT")
            || sql_upper.starts_with("UPDATE")
            || sql_upper.starts_with("DELETE")
        {
            1
        } else {
            0
        };

        let sqlcard = response::build_sqlcard_success(rows_affected);
        Ok(vec![response::reply_dss(correlation_id, &sqlcard)])
    }

    /// Handle SELECT via EXCSQLIMM (single-row immediate query).
    fn handle_select_immediate(
        &mut self,
        sql: &str,
        correlation_id: u16,
    ) -> DrdaResult<Vec<DssSegment>> {
        let (columns, rows) = self.generate_mock_results(sql);

        if rows.is_empty() {
            let sqlcard = response::build_sqlcard_not_found();
            return Ok(vec![response::reply_dss(correlation_id, &sqlcard)]);
        }

        // Build SQLDARD + QRYDTA + SQLCARD
        let sqldard = response::build_sqldard(&columns);
        let qrydta = response::build_qrydta(&rows);
        let sqlcard = response::build_sqlcard_success(rows.len() as i32);

        Ok(vec![
            response::reply_dss_chained(correlation_id, &sqldard),
            response::object_dss(correlation_id, &qrydta),
            response::reply_dss(correlation_id, &sqlcard),
        ])
    }

    /// Handle OPNQRY (Open Query — SELECT with cursor).
    pub fn handle_opnqry(
        &mut self,
        request: &DdmObject,
        sql_object: Option<&DdmObject>,
        correlation_id: u16,
    ) -> DrdaResult<Vec<DssSegment>> {
        // Extract SQL from companion SQLSTT or from within OPNQRY params
        let sql = if let Some(sql_obj) = sql_object {
            String::from_utf8_lossy(&sql_obj.payload)
                .trim_end_matches('\0')
                .trim()
                .to_string()
        } else {
            self.extract_sql_text(request)?
        };

        tracing::info!(sql = %sql, "OPNQRY");

        let (columns, rows) = self.generate_mock_results(&sql);

        // Create cursor
        let cursor_id = self.next_cursor_id;
        self.next_cursor_id += 1;

        let row_count = rows.len();

        // Build initial response with first batch of rows
        let qrydsc = response::build_qrydsc(&columns);
        let qrydta = response::build_qrydta(&rows);
        let opnqryrm = response::build_opnqryrm();
        let sqlcard = response::build_sqlcard_success(0);

        // If we returned all rows, send ENDQRYRM; otherwise keep cursor open
        let cursor = QueryCursor {
            columns,
            rows: Vec::new(), // all rows already sent
            position: row_count,
        };
        self.cursors.insert(cursor_id, cursor);

        let mut segments = vec![
            response::reply_dss_chained(correlation_id, &opnqryrm),
            response::reply_dss_chained(correlation_id, &qrydsc),
        ];

        if row_count > 0 {
            segments.push(response::object_dss(correlation_id, &qrydta));
        }

        // Send ENDQRYRM + SQLCARD to signal end of result set
        let endqryrm = response::build_endqryrm();
        segments.push(response::reply_dss_chained(correlation_id, &endqryrm));
        segments.push(response::reply_dss(correlation_id, &sqlcard));

        Ok(segments)
    }

    /// Handle CNTQRY (Continue Query — fetch more rows).
    pub fn handle_cntqry(
        &mut self,
        _request: &DdmObject,
        correlation_id: u16,
    ) -> DrdaResult<Vec<DssSegment>> {
        // Since we send all data in OPNQRY, just return end-of-query
        let endqryrm = response::build_endqryrm();
        let sqlcard = response::build_sqlcard_not_found();

        Ok(vec![
            response::reply_dss_chained(correlation_id, &endqryrm),
            response::reply_dss(correlation_id, &sqlcard),
        ])
    }

    /// Handle CLSQRY (Close Query).
    pub fn handle_clsqry(
        &mut self,
        _request: &DdmObject,
        correlation_id: u16,
    ) -> DrdaResult<Vec<DssSegment>> {
        // Remove cursor (we might not know which one, just clear all for simplicity)
        self.cursors.clear();

        let sqlcard = response::build_sqlcard_success(0);
        Ok(vec![response::reply_dss(correlation_id, &sqlcard)])
    }

    /// Handle PRPSQLSTT (Prepare SQL Statement).
    pub fn handle_prpsqlstt(
        &mut self,
        request: &DdmObject,
        correlation_id: u16,
    ) -> DrdaResult<Vec<DssSegment>> {
        let sql = self.extract_sql_text(request)?;
        tracing::info!(sql = %sql, "PRPSQLSTT");

        // Return SQLDARD describing the prepared statement
        let sql_upper = sql.trim().to_uppercase();
        if sql_upper.starts_with("SELECT") {
            let (columns, _) = self.generate_mock_results(&sql);
            let sqldard = response::build_sqldard(&columns);
            let sqlcard = response::build_sqlcard_success(0);
            Ok(vec![
                response::reply_dss_chained(correlation_id, &sqldard),
                response::reply_dss(correlation_id, &sqlcard),
            ])
        } else {
            let sqlcard = response::build_sqlcard_success(0);
            Ok(vec![response::reply_dss(correlation_id, &sqlcard)])
        }
    }

    /// Handle EXCSQLSTT (Execute prepared SQL Statement).
    pub fn handle_excsqlstt(
        &mut self,
        _request: &DdmObject,
        correlation_id: u16,
    ) -> DrdaResult<Vec<DssSegment>> {
        // Execute prepared statement — treat like EXCSQLIMM
        let sqlcard = response::build_sqlcard_success(0);
        Ok(vec![response::reply_dss(correlation_id, &sqlcard)])
    }

    /// Handle RDBCMM (Commit).
    pub fn handle_rdbcmm(
        &mut self,
        correlation_id: u16,
    ) -> DrdaResult<Vec<DssSegment>> {
        tracing::debug!("RDBCMM (commit)");
        let enduowrm = response::build_enduowrm();
        let sqlcard = response::build_sqlcard_success(0);
        Ok(vec![
            response::reply_dss_chained(correlation_id, &enduowrm),
            response::reply_dss(correlation_id, &sqlcard),
        ])
    }

    /// Handle RDBRLLBCK (Rollback).
    pub fn handle_rollback(
        &mut self,
        correlation_id: u16,
    ) -> DrdaResult<Vec<DssSegment>> {
        tracing::debug!("RDBRLLBCK (rollback)");
        let enduowrm = response::build_enduowrm();
        let sqlcard = response::build_sqlcard_success(0);
        Ok(vec![
            response::reply_dss_chained(correlation_id, &enduowrm),
            response::reply_dss(correlation_id, &sqlcard),
        ])
    }

    /// Extract SQL text from a DDM object.
    ///
    /// SQL can be in an SQLSTT parameter within the request or as the
    /// entire payload if it's a standalone SQLSTT object.
    fn extract_sql_text(&self, request: &DdmObject) -> DrdaResult<String> {
        // First try to find SQLSTT as a nested parameter
        if let Some(sql) = request.get_string_param(SQLSTT)? {
            if !sql.is_empty() {
                return Ok(sql);
            }
        }

        // If the object itself is SQLSTT, the payload is the SQL text
        if request.code_point == SQLSTT {
            let s = String::from_utf8_lossy(&request.payload)
                .trim_end_matches('\0')
                .trim()
                .to_string();
            if !s.is_empty() {
                return Ok(s);
            }
        }

        // Try the raw payload as SQL text (some clients send it directly)
        let s = String::from_utf8_lossy(&request.payload)
            .trim_end_matches('\0')
            .trim()
            .to_string();
        if !s.is_empty() {
            return Ok(s);
        }

        Ok("SELECT 1 FROM SYSIBM.SYSDUMMY1".to_string())
    }

    /// Generate mock query results based on the SQL text.
    ///
    /// Provides meaningful responses for common system catalog queries
    /// that DB2 clients/tools typically execute.
    fn generate_mock_results(&self, sql: &str) -> (Vec<ColumnDesc>, Vec<Vec<ColumnValue>>) {
        let sql_upper = sql.trim().to_uppercase();

        // SYSIBM.SYSDUMMY1 — standard DB2 single-row table
        if sql_upper.contains("SYSIBM.SYSDUMMY1") || sql_upper.contains("DUAL") {
            return self.mock_sysdummy1(&sql_upper);
        }

        // SYSIBM.SYSTABLES — table catalog
        if sql_upper.contains("SYSIBM.SYSTABLES") || sql_upper.contains("SYSCAT.TABLES") {
            return self.mock_systables();
        }

        // SYSIBM.SYSCOLUMNS — column catalog
        if sql_upper.contains("SYSIBM.SYSCOLUMNS") || sql_upper.contains("SYSCAT.COLUMNS") {
            return self.mock_syscolumns();
        }

        // Current schema / special registers
        if sql_upper.contains("CURRENT SCHEMA")
            || sql_upper.contains("CURRENT_SCHEMA")
            || sql_upper.contains("CURRENT SERVER")
        {
            return self.mock_special_register(&sql_upper);
        }

        // Default: return a single-column result with a message
        let columns = vec![ColumnDesc {
            name: "RESULT".to_string(),
            data_type: FdocaType::Varchar,
            length: 128,
            precision: 0,
            scale: 0,
            nullable: false,
        }];

        let rows = vec![vec![ColumnValue::Varchar(
            "SQL executed successfully".to_string(),
        )]];

        (columns, rows)
    }

    fn mock_sysdummy1(&self, sql_upper: &str) -> (Vec<ColumnDesc>, Vec<Vec<ColumnValue>>) {
        // Handle "SELECT 1 FROM SYSIBM.SYSDUMMY1" and similar
        if sql_upper.contains("SELECT 1") {
            let columns = vec![ColumnDesc {
                name: "1".to_string(),
                data_type: FdocaType::Integer,
                length: 4,
                precision: 0,
                scale: 0,
                nullable: false,
            }];
            let rows = vec![vec![ColumnValue::Integer(1)]];
            return (columns, rows);
        }

        let columns = vec![ColumnDesc {
            name: "IBMREQD".to_string(),
            data_type: FdocaType::FixedChar,
            length: 1,
            precision: 0,
            scale: 0,
            nullable: false,
        }];
        let rows = vec![vec![ColumnValue::FixedChar("Y".to_string(), 1)]];
        (columns, rows)
    }

    fn mock_systables(&self) -> (Vec<ColumnDesc>, Vec<Vec<ColumnValue>>) {
        let columns = vec![
            ColumnDesc {
                name: "TABSCHEMA".to_string(),
                data_type: FdocaType::Varchar,
                length: 128,
                precision: 0,
                scale: 0,
                nullable: false,
            },
            ColumnDesc {
                name: "TABNAME".to_string(),
                data_type: FdocaType::Varchar,
                length: 128,
                precision: 0,
                scale: 0,
                nullable: false,
            },
            ColumnDesc {
                name: "TYPE".to_string(),
                data_type: FdocaType::FixedChar,
                length: 1,
                precision: 0,
                scale: 0,
                nullable: false,
            },
        ];

        let rows = vec![
            vec![
                ColumnValue::Varchar("IBMUSER".to_string()),
                ColumnValue::Varchar("ACCTDAT".to_string()),
                ColumnValue::FixedChar("T".to_string(), 1),
            ],
            vec![
                ColumnValue::Varchar("IBMUSER".to_string()),
                ColumnValue::Varchar("CARDDAT".to_string()),
                ColumnValue::FixedChar("T".to_string(), 1),
            ],
            vec![
                ColumnValue::Varchar("IBMUSER".to_string()),
                ColumnValue::Varchar("CUSTDAT".to_string()),
                ColumnValue::FixedChar("T".to_string(), 1),
            ],
            vec![
                ColumnValue::Varchar("IBMUSER".to_string()),
                ColumnValue::Varchar("TRANSACT".to_string()),
                ColumnValue::FixedChar("T".to_string(), 1),
            ],
        ];

        (columns, rows)
    }

    fn mock_syscolumns(&self) -> (Vec<ColumnDesc>, Vec<Vec<ColumnValue>>) {
        let columns = vec![
            ColumnDesc {
                name: "TABSCHEMA".to_string(),
                data_type: FdocaType::Varchar,
                length: 128,
                precision: 0,
                scale: 0,
                nullable: false,
            },
            ColumnDesc {
                name: "TABNAME".to_string(),
                data_type: FdocaType::Varchar,
                length: 128,
                precision: 0,
                scale: 0,
                nullable: false,
            },
            ColumnDesc {
                name: "COLNAME".to_string(),
                data_type: FdocaType::Varchar,
                length: 128,
                precision: 0,
                scale: 0,
                nullable: false,
            },
            ColumnDesc {
                name: "TYPENAME".to_string(),
                data_type: FdocaType::Varchar,
                length: 30,
                precision: 0,
                scale: 0,
                nullable: false,
            },
        ];

        let rows = vec![vec![
            ColumnValue::Varchar("IBMUSER".to_string()),
            ColumnValue::Varchar("ACCTDAT".to_string()),
            ColumnValue::Varchar("ACCTNO".to_string()),
            ColumnValue::Varchar("VARCHAR".to_string()),
        ]];

        (columns, rows)
    }

    fn mock_special_register(
        &self,
        sql_upper: &str,
    ) -> (Vec<ColumnDesc>, Vec<Vec<ColumnValue>>) {
        let name = if sql_upper.contains("CURRENT SERVER") {
            "CURRENT SERVER"
        } else {
            "CURRENT SCHEMA"
        };

        let columns = vec![ColumnDesc {
            name: name.to_string(),
            data_type: FdocaType::Varchar,
            length: 128,
            precision: 0,
            scale: 0,
            nullable: false,
        }];

        let rows = vec![vec![ColumnValue::Varchar("DSN1".to_string())]];

        (columns, rows)
    }
}
