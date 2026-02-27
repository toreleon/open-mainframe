//! Top-level DRDA request dispatcher.
//!
//! Routes incoming DDM objects to the appropriate handler based on code point
//! and connection state.

use crate::code_points::*;
use crate::connection::{ConnectionHandler, ConnectionState};
use crate::ddm::parse_ddm_list;
use crate::dss::DssSegment;
use crate::error::{DrdaError, DrdaResult};
use crate::sql_handler::QueryState;

/// Per-connection request handler that owns connection and query state.
pub struct RequestHandler {
    /// Connection handshake handler.
    pub conn: ConnectionHandler,
    /// SQL query state (open cursors, etc.).
    pub query: QueryState,
    /// Authentication function context — userid/password validator.
    auth_validator: Box<dyn Fn(&str, &str) -> bool + Send>,
}

impl RequestHandler {
    /// Create a new request handler with the given auth validator.
    pub fn new<F>(conn: ConnectionHandler, auth_fn: F) -> Self
    where
        F: Fn(&str, &str) -> bool + Send + 'static,
    {
        Self {
            conn,
            query: QueryState::new(),
            auth_validator: Box::new(auth_fn),
        }
    }

    /// Process a DSS segment and return response segments to send back.
    pub fn process_dss(&mut self, dss: &DssSegment) -> DrdaResult<Vec<DssSegment>> {
        let payload = &dss.payload;
        if payload.is_empty() {
            return Ok(vec![]);
        }

        // Parse all DDM objects from the DSS payload
        let objects = parse_ddm_list(payload)?;
        if objects.is_empty() {
            return Ok(vec![]);
        }

        let correlation_id = dss.correlation_id;
        let mut all_responses = Vec::new();

        // Find the primary command object (first non-parameter DDM)
        let primary = &objects[0];

        // Some commands come with companion SQLSTT objects in chained DSS
        // For OPNQRY and EXCSQLIMM, SQL might be in a companion SQLSTT
        let sql_object = objects.iter().find(|o| o.code_point == SQLSTT);

        match primary.code_point {
            // ── Connection handshake ─────────────────────────
            EXCSAT | ACCSEC | SECCHK | ACCRDB => {
                // Handle all handshake commands that might be in a single DSS
                for obj in &objects {
                    match obj.code_point {
                        EXCSAT | ACCSEC | ACCRDB => {
                            let responses = self.conn.process_handshake(
                                obj.code_point,
                                obj,
                                correlation_id,
                                |_, _| true, // placeholder, real auth below
                            )?;
                            all_responses.extend(responses);
                        }
                        SECCHK => {
                            let auth_fn = &self.auth_validator;
                            let userid = obj.get_string_param(USRID)?.unwrap_or_default();
                            let password = obj.get_string_param(PASSWORD)?.unwrap_or_default();
                            let authenticated = (auth_fn)(&userid, &password);

                            let responses = self.conn.handle_secchk(
                                obj,
                                correlation_id,
                                |_, _| authenticated,
                            )?;
                            all_responses.extend(responses);
                        }
                        _ => {} // skip nested parameters
                    }
                }
            }

            // ── SQL execution (requires Ready state) ─────────
            EXCSQLIMM => {
                self.ensure_ready()?;
                let responses = self.query.handle_excsqlimm(primary, correlation_id)?;
                all_responses.extend(responses);
            }

            OPNQRY => {
                self.ensure_ready()?;
                let responses =
                    self.query
                        .handle_opnqry(primary, sql_object, correlation_id)?;
                all_responses.extend(responses);
            }

            CNTQRY => {
                self.ensure_ready()?;
                let responses = self.query.handle_cntqry(primary, correlation_id)?;
                all_responses.extend(responses);
            }

            CLSQRY => {
                self.ensure_ready()?;
                let responses = self.query.handle_clsqry(primary, correlation_id)?;
                all_responses.extend(responses);
            }

            PRPSQLSTT => {
                self.ensure_ready()?;
                let responses = self.query.handle_prpsqlstt(primary, correlation_id)?;
                all_responses.extend(responses);
            }

            EXCSQLSTT => {
                self.ensure_ready()?;
                let responses = self.query.handle_excsqlstt(primary, correlation_id)?;
                all_responses.extend(responses);
            }

            RDBCMM => {
                self.ensure_ready()?;
                let responses = self.query.handle_rdbcmm(correlation_id)?;
                all_responses.extend(responses);
            }

            RDBRLLBCK => {
                self.ensure_ready()?;
                let responses = self.query.handle_rollback(correlation_id)?;
                all_responses.extend(responses);
            }

            _ => {
                tracing::warn!(
                    code_point = format!("0x{:04X}", primary.code_point),
                    "Unsupported DRDA code point"
                );
                // Return CMDNSPRM (Command Not Supported)
                let cmdnsprm = crate::response::build_cmdchkrm("Unsupported command");
                all_responses.push(crate::response::reply_dss(correlation_id, &cmdnsprm));
            }
        }

        Ok(all_responses)
    }

    /// Process a batch of chained DSS segments (multiple DDM objects across DSS boundaries).
    /// This handles cases where SQL text is sent in a separate chained DSS from the command.
    pub fn process_dss_batch(&mut self, segments: &[DssSegment]) -> DrdaResult<Vec<DssSegment>> {
        if segments.is_empty() {
            return Ok(vec![]);
        }

        // If there's only one segment, process it directly
        if segments.len() == 1 {
            return self.process_dss(&segments[0]);
        }

        // Merge all payloads from chained segments into a combined DDM list
        let mut combined_payload = bytes::BytesMut::new();
        let correlation_id = segments[0].correlation_id;

        for seg in segments {
            combined_payload.extend_from_slice(&seg.payload);
        }

        // Create a virtual combined segment
        let combined = DssSegment {
            dss_type: segments[0].dss_type,
            chained: false,
            continuation: false,
            correlation_id,
            payload: combined_payload,
        };

        self.process_dss(&combined)
    }

    /// Ensure the connection is in the Ready state for SQL execution.
    fn ensure_ready(&self) -> DrdaResult<()> {
        if *self.conn.state() != ConnectionState::Ready {
            return Err(DrdaError::Protocol(format!(
                "Connection not ready for SQL (state: {})",
                self.conn.state()
            )));
        }
        Ok(())
    }
}
