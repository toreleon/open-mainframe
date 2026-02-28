//! DRDA connection handshake state machine.
//!
//! Manages the 4-phase DRDA connection setup:
//! 1. EXCSAT → EXCSATRD (exchange server attributes)
//! 2. ACCSEC → ACCSECRD (negotiate security mechanism)
//! 3. SECCHK → SECCHKRM (authenticate user)
//! 4. ACCRDB → ACCRDBRM (access database)
//!
//! After completing all 4 phases, the connection enters the Ready state
//! for SQL execution.

use crate::code_points::*;
use crate::ddm::DdmObject;
use crate::dss::DssSegment;
use crate::error::{DrdaError, DrdaResult};
use crate::response;
use crate::secmec9::EusridpwdState;

/// Connection state during the handshake process.
#[derive(Debug, Clone, PartialEq)]
pub enum ConnectionState {
    /// Waiting for EXCSAT.
    Initial,
    /// EXCSAT completed, waiting for ACCSEC.
    Exchanged,
    /// ACCSEC completed, waiting for SECCHK.
    SecurityNegotiated,
    /// SECCHK completed, waiting for ACCRDB.
    Authenticated,
    /// ACCRDB completed, ready for SQL execution.
    Ready,
}

impl std::fmt::Display for ConnectionState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ConnectionState::Initial => write!(f, "Initial"),
            ConnectionState::Exchanged => write!(f, "Exchanged"),
            ConnectionState::SecurityNegotiated => write!(f, "SecurityNegotiated"),
            ConnectionState::Authenticated => write!(f, "Authenticated"),
            ConnectionState::Ready => write!(f, "Ready"),
        }
    }
}

/// Configuration for the DRDA connection handler.
#[derive(Debug, Clone)]
pub struct DrdaConfig {
    /// Server name reported in EXCSATRD.
    pub server_name: String,
    /// Product identifier.
    pub product_id: String,
    /// Database name to accept.
    pub database: String,
    /// Location name (DDF location).
    pub location: String,
}

/// Manages the connection handshake state.
pub struct ConnectionHandler {
    state: ConnectionState,
    config: DrdaConfig,
    /// Authenticated user ID (set after SECCHK).
    pub userid: Option<String>,
    /// Authenticated database name (set after ACCRDB).
    pub database: Option<String>,
    /// Negotiated security mechanism.
    secmec: u16,
    /// DH key exchange state for EUSRIDPWD (SECMEC 0x0009).
    eusridpwd_state: Option<EusridpwdState>,
}

impl ConnectionHandler {
    /// Create a new connection handler.
    pub fn new(config: DrdaConfig) -> Self {
        Self {
            state: ConnectionState::Initial,
            config,
            userid: None,
            database: None,
            secmec: SECMEC_USRIDPWD,
            eusridpwd_state: None,
        }
    }

    /// Get the current connection state.
    pub fn state(&self) -> &ConnectionState {
        &self.state
    }

    /// Check if the connection is ready for SQL execution.
    pub fn is_ready(&self) -> bool {
        self.state == ConnectionState::Ready
    }

    /// Get the negotiated security mechanism.
    pub fn secmec(&self) -> u16 {
        self.secmec
    }

    /// Handle an EXCSAT (Exchange Server Attributes) request.
    pub fn handle_excsat(
        &mut self,
        _request: &DdmObject,
        correlation_id: u16,
    ) -> DrdaResult<Vec<DssSegment>> {
        // We accept EXCSAT in Initial state, or re-EXCSAT after connection
        let excsatrd = response::build_excsatrd(&self.config.server_name, &self.config.product_id);

        if self.state == ConnectionState::Initial {
            self.state = ConnectionState::Exchanged;
        }

        Ok(vec![response::reply_dss(correlation_id, &excsatrd)])
    }

    /// Handle an ACCSEC (Access Security) request.
    pub fn handle_accsec(
        &mut self,
        request: &DdmObject,
        correlation_id: u16,
    ) -> DrdaResult<Vec<DssSegment>> {
        // Read the requested security mechanism
        if let Some(secmec) = request.get_u16_param(SECMEC)? {
            self.secmec = secmec;
        }

        // We support USRIDPWD (0x0003), USRONLY (0x0004), and EUSRIDPWD (0x0009).
        let accepted_secmec = match self.secmec {
            SECMEC_USRIDPWD | SECMEC_USRONLY => self.secmec,
            SECMEC_EUSRIDPWD => SECMEC_EUSRIDPWD,
            _ => SECMEC_USRIDPWD, // downgrade to plain user/password
        };
        self.secmec = accepted_secmec;

        // Log all incoming ACCSEC params for debugging
        if let Ok(params) = request.parse_params() {
            for p in &params {
                tracing::debug!(
                    code_point = format!("0x{:04X}", p.code_point),
                    payload_len = p.payload.len(),
                    "ACCSEC param"
                );
            }
        }

        let accsecrd = if accepted_secmec == SECMEC_EUSRIDPWD {
            // Extract client's DH public key from SECTKN
            let client_public_key = request.get_raw_param(SECTKN)?.unwrap_or_default();

            tracing::debug!(
                client_sectkn_len = client_public_key.len(),
                client_sectkn_hex = %client_public_key.iter().map(|b| format!("{:02x}", b)).collect::<Vec<_>>().join(""),
                "EUSRIDPWD: received client DH public key"
            );

            // Create DH state and generate server keypair
            let state = EusridpwdState::new(&client_public_key);
            let server_public = state.server_public;
            self.eusridpwd_state = Some(state);

            tracing::debug!(
                server_sectkn_hex = %server_public.iter().map(|b| format!("{:02x}", b)).collect::<Vec<_>>().join(""),
                "EUSRIDPWD: generated server DH public key"
            );

            // Respond with ACCSECRD including server's DH public key as SECTKN
            response::build_accsecrd_with_token(SECMEC_EUSRIDPWD, &server_public)
        } else {
            response::build_accsecrd(accepted_secmec)
        };

        if self.state == ConnectionState::Exchanged || self.state == ConnectionState::Initial {
            self.state = ConnectionState::SecurityNegotiated;
        }

        Ok(vec![response::reply_dss(correlation_id, &accsecrd)])
    }

    /// Handle a SECCHK (Security Check) request.
    ///
    /// The `auth_fn` validates the userid/password against RACF.
    /// Returns true if authentication succeeded.
    pub fn handle_secchk<F>(
        &mut self,
        request: &DdmObject,
        correlation_id: u16,
        auth_fn: F,
    ) -> DrdaResult<Vec<DssSegment>>
    where
        F: FnOnce(&str, &str) -> bool,
    {
        let (userid, password) = if self.secmec == SECMEC_EUSRIDPWD {
            // EUSRIDPWD: credentials are DES-encrypted in SECTKN parameters.
            // ibm_db sends two SECTKN params: encrypted userid and encrypted password.
            let sectkn_list = request.get_all_raw_params(SECTKN)?;

            if sectkn_list.len() < 2 {
                return Err(DrdaError::Protocol(format!(
                    "EUSRIDPWD SECCHK requires 2 SECTKN params, got {}",
                    sectkn_list.len()
                )));
            }

            let state = self.eusridpwd_state.as_ref().ok_or_else(|| {
                DrdaError::Protocol("EUSRIDPWD state not initialized (missing ACCSEC?)".to_string())
            })?;

            let encrypted_userid = &sectkn_list[0];
            let encrypted_password = &sectkn_list[1];

            tracing::debug!(
                enc_userid_len = encrypted_userid.len(),
                enc_password_len = encrypted_password.len(),
                "EUSRIDPWD: decrypting credentials"
            );

            let (userid_bytes, password_bytes) =
                state.decrypt_credentials(encrypted_userid, encrypted_password)?;

            // Credentials are in EBCDIC cp500 — decode to ASCII
            let userid = crate::handler::decode_ebcdic_bytes_pub(&userid_bytes);
            let password = crate::handler::decode_ebcdic_bytes_pub(&password_bytes);

            tracing::debug!(
                userid = %userid,
                "EUSRIDPWD: credentials decrypted"
            );

            (userid, password)
        } else {
            // Plain text USRIDPWD: extract USRID and PASSWORD directly
            let userid = request.get_string_param(USRID)?.unwrap_or_default();
            let password = request.get_string_param(PASSWORD)?.unwrap_or_default();
            (userid, password)
        };

        // Authenticate
        let authenticated = auth_fn(&userid, &password);

        if authenticated {
            self.userid = Some(userid.clone());
            self.state = ConnectionState::Authenticated;
            let secchkrm = response::build_secchkrm(0x00); // success
            Ok(vec![response::reply_dss(correlation_id, &secchkrm)])
        } else {
            let _secchkrm = response::build_secchkrm(0x0E); // invalid credentials
            Err(DrdaError::AuthFailed(userid))
        }
    }

    /// Handle an ACCRDB (Access RDB) request.
    pub fn handle_accrdb(
        &mut self,
        request: &DdmObject,
        correlation_id: u16,
    ) -> DrdaResult<Vec<DssSegment>> {
        // Log all incoming ACCRDB params for debugging
        if let Ok(params) = request.parse_params() {
            for p in &params {
                tracing::debug!(
                    code_point = format!("0x{:04X}", p.code_point),
                    payload_len = p.payload.len(),
                    payload_hex = %p.payload.iter().map(|b| format!("{:02x}", b)).collect::<Vec<_>>().join(""),
                    "ACCRDB param"
                );
            }
        }

        // Extract RDB name — may be EBCDIC encoded, decode from raw bytes
        let rdbnam_bytes = request.get_raw_param(RDBNAM)?.unwrap_or_default();
        let rdbnam = crate::handler::decode_ebcdic_bytes_pub(&rdbnam_bytes).to_uppercase();

        // Check if database name matches our config
        let expected = self.config.database.to_uppercase();
        if rdbnam != expected && !rdbnam.is_empty() {
            // Accept any database name — we're an emulator
            tracing::info!(
                requested = %rdbnam,
                configured = %expected,
                "Accepting non-standard database name"
            );
        }

        self.database = Some(rdbnam.clone());
        self.state = ConnectionState::Ready;

        let db = if rdbnam.is_empty() { &expected } else { &rdbnam };

        // Build ACCRDBRM + PBSD (piggy-backed session data)
        // This matches Apache Derby's response format:
        //   ACCRDBRM (Reply DSS, chained) → PBSD (Reply DSS, end of chain)
        // Note: Derby sends PBSD (not SQLCARD) after ACCRDBRM.
        let accrdbrm = response::build_accrdbrm(db);

        // Extract userid for schema name (default to database name)
        let schema = self.userid.as_deref().unwrap_or(db);
        let pbsd = response::build_pbsd(schema);

        Ok(vec![
            response::reply_dss_chained_same_corr(correlation_id, &accrdbrm),
            response::object_dss(correlation_id, &pbsd),
        ])
    }

    /// Process a handshake DDM object based on current state.
    /// Returns response DSS segments to send back.
    ///
    /// `auth_fn` is called during SECCHK to validate credentials.
    pub fn process_handshake<F>(
        &mut self,
        code_point: u16,
        request: &DdmObject,
        correlation_id: u16,
        auth_fn: F,
    ) -> DrdaResult<Vec<DssSegment>>
    where
        F: FnOnce(&str, &str) -> bool,
    {
        match code_point {
            EXCSAT => self.handle_excsat(request, correlation_id),
            ACCSEC => self.handle_accsec(request, correlation_id),
            SECCHK => self.handle_secchk(request, correlation_id, auth_fn),
            ACCRDB => self.handle_accrdb(request, correlation_id),
            _ => Err(DrdaError::UnexpectedCodePoint(
                code_point,
                self.state.to_string(),
            )),
        }
    }
}
