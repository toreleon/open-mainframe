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

/// Tracks open queries (cursors) and prepared statements for a connection.
#[derive(Debug)]
pub struct QueryState {
    /// Open cursors: query ID → result set.
    cursors: HashMap<u64, QueryCursor>,
    /// Next cursor ID.
    next_cursor_id: u64,
    /// Most recently prepared SQL text (from PRPSQLSTT).
    last_prepared_sql: Option<String>,
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
            last_prepared_sql: None,
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
        Ok(vec![response::object_dss(correlation_id, &sqlcard)])
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
            return Ok(vec![response::object_dss(correlation_id, &sqlcard)]);
        }

        // Build SQLDARD + QRYDTA + SQLCARD
        let sqldard = response::build_sqldard(&columns);
        let qrydta = response::build_qrydta(&rows, &columns);
        let sqlcard = response::build_sqlcard_success(rows.len() as i32);

        Ok(vec![
            response::object_dss(correlation_id, &sqldard),
            response::object_dss(correlation_id, &qrydta),
            response::object_dss(correlation_id, &sqlcard),
        ])
    }

    /// Handle OPNQRY (Open Query — SELECT with cursor).
    ///
    /// OPNQRY usually follows a PRPSQLSTT that already prepared the SQL.
    /// The OPNQRY itself carries a PKGNAMCSN (package name) that references
    /// the prepared statement, not the SQL text directly.
    pub fn handle_opnqry(
        &mut self,
        request: &DdmObject,
        sql_object: Option<&DdmObject>,
        correlation_id: u16,
    ) -> DrdaResult<Vec<DssSegment>> {
        // Extract SQL: companion SQLSTT → last prepared SQL → fallback
        let sql = if let Some(sql_obj) = sql_object {
            extract_sql_from_sqlstt(&sql_obj.payload)
        } else if let Some(ref prepared) = self.last_prepared_sql {
            prepared.clone()
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
        let qrydta = response::build_qrydta(&rows, &columns);
        let opnqryrm = response::build_opnqryrm();

        // If we returned all rows, send ENDQRYRM; otherwise keep cursor open
        let cursor = QueryCursor {
            columns,
            rows: Vec::new(), // all rows already sent
            position: row_count,
        };
        self.cursors.insert(cursor_id, cursor);

        // Response format:
        //   OPNQRYRM (Reply, chained)
        //   → QRYDSC (Object, chained)
        //   → QRYDTA (Object, chained)
        //   → ENDQRYRM (Reply, chained)
        //   → SQLCARD (Object, last)
        let endqryrm = response::build_endqryrm();
        let sqlcard = response::build_sqlcard_not_found();

        let mut segments = vec![
            response::reply_dss_chained_same_corr(correlation_id, &opnqryrm),
        ];

        if row_count > 0 {
            segments.push(response::object_dss_chained_same_corr(correlation_id, &qrydsc));
            segments.push(response::object_dss_chained_same_corr(correlation_id, &qrydta));
            segments.push(response::reply_dss_chained_same_corr(correlation_id, &endqryrm));
            segments.push(response::object_dss(correlation_id, &sqlcard));
        } else {
            segments.push(response::object_dss_chained_same_corr(correlation_id, &qrydsc));
            segments.push(response::reply_dss_chained_same_corr(correlation_id, &endqryrm));
            segments.push(response::object_dss(correlation_id, &sqlcard));
        }

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
            response::reply_dss_chained_same_corr(correlation_id, &endqryrm),
            response::object_dss(correlation_id, &sqlcard),
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
        Ok(vec![response::object_dss(correlation_id, &sqlcard)])
    }

    /// Handle PRPSQLSTT (Prepare SQL Statement).
    ///
    /// The SQL text is typically in a companion SQLSTT object sent as a
    /// chained DDM object alongside PRPSQLSTT.
    ///
    /// Response format (matching Derby):
    /// - SQLDARD (Reply DSS) + SQLCARD (Object DSS) for SELECT
    /// - SQLCARD (Object DSS) for non-SELECT
    pub fn handle_prpsqlstt(
        &mut self,
        request: &DdmObject,
        sql_object: Option<&DdmObject>,
        correlation_id: u16,
    ) -> DrdaResult<Vec<DssSegment>> {
        // Extract SQL from companion SQLSTT object first, then from request
        let sql = if let Some(sql_obj) = sql_object {
            extract_sql_from_sqlstt(&sql_obj.payload)
        } else {
            self.extract_sql_text(request)?
        };

        tracing::info!(sql = %sql, "PRPSQLSTT");

        // Save for later OPNQRY/EXCSQLSTT that reference this prepared statement
        self.last_prepared_sql = Some(sql.clone());

        // Return SQLDARD describing the prepared statement
        // SQLCARD after PRPSQLSTT must be Object DSS (type=3), not Reply DSS
        let sql_upper = sql.trim().to_uppercase();
        if sql_upper.starts_with("SELECT") {
            // For SELECT: send SQLDARD (Reply DSS) + SQLCARD (Object DSS)
            // Both at the same correlation ID to complete the PRPSQLSTT response.
            // Without the SQLCARD, the client sees the next command's reply
            // (e.g., OPNQRYRM) as an invalid code point for PRPSQLSTT.
            let (columns, _) = self.generate_mock_results(&sql);
            let sqldard = response::build_sqldard(&columns);
            let sqlcard = response::build_sqlcard_success(0);
            Ok(vec![
                response::reply_dss(correlation_id, &sqldard),
                response::object_dss(correlation_id, &sqlcard),
            ])
        } else {
            let sqlcard = response::build_sqlcard_success(0);
            Ok(vec![response::object_dss(correlation_id, &sqlcard)])
        }
    }

    /// Handle EXCSQLSTT (Execute prepared SQL Statement).
    pub fn handle_excsqlstt(
        &mut self,
        request: &DdmObject,
        sql_object: Option<&DdmObject>,
        correlation_id: u16,
    ) -> DrdaResult<Vec<DssSegment>> {
        // Try companion SQLSTT, then last prepared SQL
        let sql = if let Some(sql_obj) = sql_object {
            extract_sql_from_sqlstt(&sql_obj.payload)
        } else if let Some(ref prepared) = self.last_prepared_sql {
            prepared.clone()
        } else {
            self.extract_sql_text(request)?
        };

        tracing::info!(sql = %sql, "EXCSQLSTT");

        // Execute the prepared SQL
        let sql_upper = sql.trim().to_uppercase();
        if sql_upper.starts_with("SELECT") {
            return self.handle_select_immediate(&sql, correlation_id);
        }

        let rows_affected = if sql_upper.starts_with("INSERT")
            || sql_upper.starts_with("UPDATE")
            || sql_upper.starts_with("DELETE")
        {
            1
        } else {
            0
        };

        let sqlcard = response::build_sqlcard_success(rows_affected);
        Ok(vec![response::object_dss(correlation_id, &sqlcard)])
    }

    /// Handle EXCSQLSET (Execute SQL SET statement — special registers).
    pub fn handle_excsqlset(
        &mut self,
        _request: &DdmObject,
        sql_object: Option<&DdmObject>,
        correlation_id: u16,
    ) -> DrdaResult<Vec<DssSegment>> {
        let sql = if let Some(sql_obj) = sql_object {
            extract_sql_from_sqlstt(&sql_obj.payload)
        } else {
            String::new()
        };

        tracing::info!(sql = %sql, "EXCSQLSET");

        // SET statements (SET CURRENT SCHEMA, etc.) — just acknowledge success
        let sqlcard = response::build_sqlcard_success(0);
        Ok(vec![response::object_dss(correlation_id, &sqlcard)])
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
            response::reply_dss_chained_same_corr(correlation_id, &enduowrm),
            response::object_dss(correlation_id, &sqlcard),
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
            response::reply_dss_chained_same_corr(correlation_id, &enduowrm),
            response::object_dss(correlation_id, &sqlcard),
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

        // Current schema / special registers — check first, before SYSDUMMY1
        if sql_upper.contains("CURRENT SCHEMA")
            || sql_upper.contains("CURRENT_SCHEMA")
            || sql_upper.contains("CURRENT SERVER")
        {
            return self.mock_special_register(&sql_upper);
        }

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
            data_type: FdocaType::Varchar,
            length: 1,
            precision: 0,
            scale: 0,
            nullable: false,
        }];
        let rows = vec![vec![ColumnValue::Varchar("Y".to_string())]];
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
                data_type: FdocaType::Varchar,
                length: 8,
                precision: 0,
                scale: 0,
                nullable: false,
            },
        ];

        let rows = vec![
            vec![
                ColumnValue::Varchar("IBMUSER".to_string()),
                ColumnValue::Varchar("ACCTDAT".to_string()),
                ColumnValue::Varchar("T".to_string()),
            ],
            vec![
                ColumnValue::Varchar("IBMUSER".to_string()),
                ColumnValue::Varchar("CARDDAT".to_string()),
                ColumnValue::Varchar("T".to_string()),
            ],
            vec![
                ColumnValue::Varchar("IBMUSER".to_string()),
                ColumnValue::Varchar("CUSTDAT".to_string()),
                ColumnValue::Varchar("T".to_string()),
            ],
            vec![
                ColumnValue::Varchar("IBMUSER".to_string()),
                ColumnValue::Varchar("TRANSACT".to_string()),
                ColumnValue::Varchar("T".to_string()),
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

}

/// Extract SQL text from an SQLSTT object payload.
///
/// The SQLSTT payload in DRDA can have several formats:
/// 1. Length-prefixed: [null_ind(1)] [length(4 BE)] [sql_text] [trailer(1)]
/// 2. Raw: just the SQL text bytes
///
/// We detect the format by checking if the first bytes look like a length prefix.
fn extract_sql_from_sqlstt(payload: &[u8]) -> String {
    if payload.is_empty() {
        return String::new();
    }

    // Check if payload starts with a null indicator + 4-byte length prefix.
    // Format: [00] [00 00 LL LL] [SQL text...] [FF]
    // The first byte 0x00 = not null indicator, then 4-byte big-endian length.
    if payload.len() >= 5 {
        let possible_null_ind = payload[0];
        let possible_len =
            u32::from_be_bytes([payload[1], payload[2], payload[3], payload[4]]) as usize;

        // Heuristic: if first byte is 0x00 (null indicator) and the 4-byte length
        // matches the remaining payload (possibly with a 1-byte trailer), this is
        // a length-prefixed SQLSTT.
        if possible_null_ind == 0x00
            && possible_len > 0
            && possible_len <= payload.len() - 5
        {
            let sql_bytes = &payload[5..5 + possible_len];
            let sql = String::from_utf8_lossy(sql_bytes)
                .trim_end_matches('\0')
                .trim()
                .to_string();
            if !sql.is_empty() {
                return sql;
            }
        }
    }

    // Fallback: treat entire payload as raw SQL text
    String::from_utf8_lossy(payload)
        .trim_end_matches('\0')
        .trim_end_matches('\u{FFFD}')
        .trim()
        .to_string()
}

impl QueryState {
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
