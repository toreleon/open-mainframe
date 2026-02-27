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

        // We support USRIDPWD (0x0003) and USRONLY (0x0004)
        let accepted_secmec = match self.secmec {
            SECMEC_USRIDPWD | SECMEC_USRONLY => self.secmec,
            _ => SECMEC_USRIDPWD, // default to user/password
        };

        let accsecrd = response::build_accsecrd(accepted_secmec);

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
        // Extract USRID and PASSWORD from SECCHK parameters
        let userid = request
            .get_string_param(USRID)?
            .unwrap_or_default();
        let password = request
            .get_string_param(PASSWORD)?
            .unwrap_or_default();

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
        // Extract RDB name
        let rdbnam = request
            .get_string_param(RDBNAM)?
            .unwrap_or_default()
            .to_uppercase();

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

        // Build ACCRDBRM + SQLCARD success
        let accrdbrm = response::build_accrdbrm(
            if rdbnam.is_empty() { &expected } else { &rdbnam },
        );
        let sqlcard = response::build_sqlcard_success(0);

        Ok(vec![
            response::reply_dss_chained(correlation_id, &accrdbrm),
            response::reply_dss(correlation_id, &sqlcard),
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
