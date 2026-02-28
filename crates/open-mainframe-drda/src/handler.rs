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

    /// Process a single DSS segment and return response segments.
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

        // The primary command is the first DDM object
        let primary = &objects[0];

        // Find companion SQLSTT if present
        let sql_object = objects.iter().find(|o| o.code_point == SQLSTT);

        tracing::debug!(
            code_point = format!("0x{:04X}", primary.code_point),
            corr_id = correlation_id,
            num_objects = objects.len(),
            "Processing DDM command"
        );

        match primary.code_point {
            // ── Connection handshake commands ─────────────────
            EXCSAT => {
                let responses = self.conn.handle_excsat(primary, correlation_id)?;
                all_responses.extend(responses);
            }

            ACCSEC => {
                let responses = self.conn.handle_accsec(primary, correlation_id)?;
                all_responses.extend(responses);
            }

            SECCHK => {
                let auth_fn = &self.auth_validator;

                if self.conn.secmec() == SECMEC_EUSRIDPWD {
                    // EUSRIDPWD: credentials are encrypted in SECTKN params.
                    // Connection handler decrypts them and passes to auth_fn.
                    let responses = self.conn.handle_secchk(
                        primary,
                        correlation_id,
                        |userid, password| {
                            tracing::debug!(
                                userid = %userid,
                                "SECCHK EUSRIDPWD authentication"
                            );
                            (auth_fn)(userid, password)
                        },
                    )?;
                    all_responses.extend(responses);
                } else {
                    // Plain text USRIDPWD: extract from raw bytes, decode EBCDIC.
                    let userid_bytes = primary.get_raw_param(USRID)?.unwrap_or_default();
                    let password_bytes = primary.get_raw_param(PASSWORD)?.unwrap_or_default();

                    let userid_decoded = decode_ebcdic_bytes(&userid_bytes);
                    let password_decoded = decode_ebcdic_bytes(&password_bytes);

                    let authenticated = (auth_fn)(&userid_decoded, &password_decoded);

                    tracing::debug!(
                        userid_decoded = %userid_decoded,
                        authenticated = authenticated,
                        "SECCHK authentication"
                    );

                    let responses = self.conn.handle_secchk(
                        primary,
                        correlation_id,
                        |_, _| authenticated,
                    )?;
                    all_responses.extend(responses);
                }
            }

            ACCRDB => {
                let responses = self.conn.handle_accrdb(primary, correlation_id)?;
                all_responses.extend(responses);
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
                let responses =
                    self.query
                        .handle_prpsqlstt(primary, sql_object, correlation_id)?;
                all_responses.extend(responses);
            }

            EXCSQLSTT => {
                self.ensure_ready()?;
                let responses =
                    self.query
                        .handle_excsqlstt(primary, sql_object, correlation_id)?;
                all_responses.extend(responses);
            }

            EXCSQLSET => {
                self.ensure_ready()?;
                let responses = self.query.handle_excsqlset(primary, sql_object, correlation_id)?;
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
                let cmdnsprm = crate::response::build_cmdchkrm("Unsupported command");
                all_responses.push(crate::response::reply_dss(correlation_id, &cmdnsprm));
            }
        }

        Ok(all_responses)
    }

    /// Process a batch of DSS segments. Each segment is processed individually
    /// with its own correlation ID, except for same-correlator chains which are
    /// merged before processing.
    pub fn process_dss_batch(&mut self, segments: &[DssSegment]) -> DrdaResult<Vec<DssSegment>> {
        if segments.is_empty() {
            return Ok(vec![]);
        }

        let mut all_responses = Vec::new();

        // Group segments by correlation ID for chained same-correlator sequences.
        // Segments with different correlation IDs are processed independently.
        let mut i = 0;
        while i < segments.len() {
            let seg = &segments[i];
            let corr_id = seg.correlation_id;

            // Collect any continuation segments with the same correlation ID
            let mut combined_payload = bytes::BytesMut::new();
            combined_payload.extend_from_slice(&seg.payload);
            let dss_type = seg.dss_type;

            let mut j = i + 1;
            while j < segments.len() && segments[j].correlation_id == corr_id {
                combined_payload.extend_from_slice(&segments[j].payload);
                j += 1;
            }

            // Create a virtual segment with merged payload (or just use the single segment)
            let virtual_seg = DssSegment {
                dss_type,
                chained: false,
                same_correlator: false,
                continuation: false,
                correlation_id: corr_id,
                payload: combined_payload,
            };

            let responses = self.process_dss(&virtual_seg)?;
            all_responses.extend(responses);

            i = j;
        }

        Ok(all_responses)
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

/// Public wrapper for EBCDIC decoding, usable from other modules.
pub fn decode_ebcdic_bytes_pub(bytes: &[u8]) -> String {
    decode_ebcdic_bytes(bytes)
}

/// Decode raw bytes that may be EBCDIC cp037/cp500 encoded.
///
/// If the bytes are valid printable ASCII, returns them as-is.
/// Otherwise, converts from EBCDIC to ASCII.
fn decode_ebcdic_bytes(bytes: &[u8]) -> String {
    if bytes.is_empty() {
        return String::new();
    }

    // Check if it looks like already valid ASCII
    let is_ascii_printable = bytes.iter().all(|&b| b >= 0x20 && b < 0x7F);
    if is_ascii_printable {
        return String::from_utf8_lossy(bytes).trim().to_string();
    }

    // EBCDIC cp037/cp500 decoding
    let decoded: String = bytes.iter().map(|&b| ebcdic_to_ascii(b) as char).collect();
    decoded.trim().to_string()
}

/// Convert a single EBCDIC cp037 byte to ASCII.
fn ebcdic_to_ascii(b: u8) -> u8 {
    // EBCDIC cp037 to ASCII mapping (common characters)
    match b {
        0x40 => b' ',  // space
        0x4B => b'.', 0x4C => b'<', 0x4D => b'(', 0x4E => b'+',
        0x50 => b'&', 0x5A => b'!', 0x5B => b'$', 0x5C => b'*',
        0x5D => b')', 0x5E => b';', 0x60 => b'-', 0x61 => b'/',
        0x6B => b',', 0x6C => b'%', 0x6D => b'_', 0x6E => b'>',
        0x6F => b'?', 0x7A => b':', 0x7B => b'#', 0x7C => b'@',
        0x7D => b'\'', 0x7E => b'=', 0x7F => b'"',
        // Lowercase letters a-i
        0x81..=0x89 => b'a' + (b - 0x81),
        // Lowercase letters j-r
        0x91..=0x99 => b'j' + (b - 0x91),
        // Lowercase letters s-z
        0xA2..=0xA9 => b's' + (b - 0xA2),
        // Uppercase letters A-I
        0xC1..=0xC9 => b'A' + (b - 0xC1),
        // Uppercase letters J-R
        0xD1..=0xD9 => b'J' + (b - 0xD1),
        // Uppercase letters S-Z
        0xE2..=0xE9 => b'S' + (b - 0xE2),
        // Digits 0-9
        0xF0..=0xF9 => b'0' + (b - 0xF0),
        // Null
        0x00 => 0x00,
        // Default: return as-is
        _ => b,
    }
}
