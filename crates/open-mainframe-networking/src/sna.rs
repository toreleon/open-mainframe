//! SNA LU Type Support (NET-101).
//!
//! Provides SNA Logical Unit session types: LU 0 (unformatted),
//! LU 1/3 (printers), LU 2 (3270 terminal), and BIND parameter negotiation.

use thiserror::Error;

use crate::vtam::LuType;

// ---------------------------------------------------------------------------
// Errors
// ---------------------------------------------------------------------------

#[derive(Debug, Error)]
pub enum SnaError {
    #[error("unsupported 3270 command: 0x{0:02X}")]
    Unsupported3270Command(u8),
    #[error("invalid RU size: {0} (max 32767)")]
    InvalidRuSize(u16),
    #[error("BIND negotiation failed: {0}")]
    BindFailed(String),
    #[error("SCS control code 0x{0:02X} not recognised")]
    UnknownScsCode(u8),
}

// ---------------------------------------------------------------------------
// NET-101.1 — LU Type 2 (3270 Terminal)
// ---------------------------------------------------------------------------

/// 3270 data stream commands supported by LU 2 sessions.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Command3270 {
    Write,
    EraseWrite,
    EraseWriteAlternate,
    ReadBuffer,
    ReadModified,
    EraseAllUnprotected,
}

impl Command3270 {
    /// Parse a 3270 command byte.
    pub fn from_byte(byte: u8) -> Result<Self, SnaError> {
        match byte {
            0xF1 => Ok(Self::Write),
            0xF5 => Ok(Self::EraseWrite),
            0x7E => Ok(Self::EraseWriteAlternate),
            0xF2 => Ok(Self::ReadBuffer),
            0xF6 => Ok(Self::ReadModified),
            0x6F => Ok(Self::EraseAllUnprotected),
            _ => Err(SnaError::Unsupported3270Command(byte)),
        }
    }

    /// Convert to command byte.
    pub fn to_byte(self) -> u8 {
        match self {
            Self::Write => 0xF1,
            Self::EraseWrite => 0xF5,
            Self::EraseWriteAlternate => 0x7E,
            Self::ReadBuffer => 0xF2,
            Self::ReadModified => 0xF6,
            Self::EraseAllUnprotected => 0x6F,
        }
    }
}

/// An LU 2 (3270) session supporting structured data streams.
#[derive(Debug)]
pub struct Lu2Session {
    pub lu_name: String,
    pub screen_rows: u16,
    pub screen_cols: u16,
    buffer: Vec<u8>,
}

impl Lu2Session {
    pub fn new(lu_name: impl Into<String>, rows: u16, cols: u16) -> Self {
        Self {
            lu_name: lu_name.into(),
            screen_rows: rows,
            screen_cols: cols,
            buffer: vec![0; (rows as usize) * (cols as usize)],
        }
    }

    /// Process a 3270 data-stream command.
    pub fn process_command(&mut self, cmd: Command3270, data: &[u8]) -> Vec<u8> {
        match cmd {
            Command3270::Write => {
                let len = data.len().min(self.buffer.len());
                self.buffer[..len].copy_from_slice(&data[..len]);
                Vec::new()
            }
            Command3270::EraseWrite => {
                self.buffer.fill(0);
                let len = data.len().min(self.buffer.len());
                self.buffer[..len].copy_from_slice(&data[..len]);
                Vec::new()
            }
            Command3270::EraseWriteAlternate => {
                self.buffer.fill(0);
                Vec::new()
            }
            Command3270::ReadBuffer => self.buffer.clone(),
            Command3270::ReadModified => {
                // Return non-zero positions as modified
                self.buffer.iter().copied().filter(|&b| b != 0).collect()
            }
            Command3270::EraseAllUnprotected => {
                self.buffer.fill(0);
                Vec::new()
            }
        }
    }
}

// ---------------------------------------------------------------------------
// NET-101.2 — LU Type 0 (Unformatted)
// ---------------------------------------------------------------------------

/// An LU 0 session for raw byte-stream exchange.
#[derive(Debug)]
pub struct Lu0Session {
    pub lu_name: String,
    outbound: Vec<u8>,
    inbound: Vec<u8>,
}

impl Lu0Session {
    pub fn new(lu_name: impl Into<String>) -> Self {
        Self {
            lu_name: lu_name.into(),
            outbound: Vec::new(),
            inbound: Vec::new(),
        }
    }

    /// Send raw bytes on the session.
    pub fn send(&mut self, data: &[u8]) {
        self.outbound.extend_from_slice(data);
    }

    /// Queue inbound data (simulating partner sending).
    pub fn queue_inbound(&mut self, data: &[u8]) {
        self.inbound.extend_from_slice(data);
    }

    /// Receive inbound data.
    pub fn receive(&mut self) -> Vec<u8> {
        std::mem::take(&mut self.inbound)
    }

    /// Drain outbound data (simulating network transmission).
    pub fn drain_outbound(&mut self) -> Vec<u8> {
        std::mem::take(&mut self.outbound)
    }
}

// ---------------------------------------------------------------------------
// NET-101.3 — LU Type 1/3 (SCS/DSC Printers)
// ---------------------------------------------------------------------------

/// SCS (SNA Character String) control codes for LU 1 printer sessions.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ScsControlCode {
    NewLine,
    CarriageReturn,
    FormFeed,
    Backspace,
    HorizontalTab,
    SetHorizontalFormat,
    SetVerticalFormat,
    TransparentData,
}

impl ScsControlCode {
    /// Parse an SCS control byte.
    pub fn from_byte(byte: u8) -> Result<Self, SnaError> {
        match byte {
            0x15 => Ok(Self::NewLine),
            0x0D => Ok(Self::CarriageReturn),
            0x0C => Ok(Self::FormFeed),
            0x16 => Ok(Self::Backspace),
            0x05 => Ok(Self::HorizontalTab),
            0x2B => Ok(Self::SetHorizontalFormat),
            0x11 => Ok(Self::SetVerticalFormat),
            0x35 => Ok(Self::TransparentData),
            _ => Err(SnaError::UnknownScsCode(byte)),
        }
    }
}

/// An LU 1 printer session supporting SCS control codes.
#[derive(Debug)]
pub struct Lu1PrinterSession {
    pub lu_name: String,
    pub output: Vec<u8>,
    pub page_count: u32,
}

impl Lu1PrinterSession {
    pub fn new(lu_name: impl Into<String>) -> Self {
        Self {
            lu_name: lu_name.into(),
            output: Vec::new(),
            page_count: 0,
        }
    }

    /// Process print data with embedded SCS control codes.
    pub fn process_print_data(&mut self, data: &[u8]) {
        for &byte in data {
            if byte == 0x0C {
                // Form feed = new page
                self.page_count += 1;
            }
            self.output.push(byte);
        }
    }

    /// Drain the accumulated output.
    pub fn drain_output(&mut self) -> Vec<u8> {
        std::mem::take(&mut self.output)
    }
}

// ---------------------------------------------------------------------------
// NET-101.3b — LU Type 3 (DSC/3270 Printer)
// ---------------------------------------------------------------------------

/// An LU 3 printer session supporting 3270 printer data streams (DSC).
#[derive(Debug)]
pub struct Lu3PrinterSession {
    pub lu_name: String,
    pub output: Vec<u8>,
    pub page_count: u32,
    pub max_print_line: u16,
}

impl Lu3PrinterSession {
    pub fn new(lu_name: impl Into<String>) -> Self {
        Self {
            lu_name: lu_name.into(),
            output: Vec::new(),
            page_count: 0,
            max_print_line: 132,
        }
    }

    /// Process a 3270 printer data stream (Write command + data).
    pub fn process_print_command(&mut self, cmd: Command3270, data: &[u8]) {
        match cmd {
            Command3270::Write | Command3270::EraseWrite => {
                self.output.extend_from_slice(data);
            }
            Command3270::EraseWriteAlternate | Command3270::EraseAllUnprotected => {
                self.page_count += 1;
                self.output.clear();
            }
            _ => {}
        }
    }

    /// Drain the accumulated print output.
    pub fn drain_output(&mut self) -> Vec<u8> {
        std::mem::take(&mut self.output)
    }
}

// ---------------------------------------------------------------------------
// NET-101.4 — Session BIND Parameters
// ---------------------------------------------------------------------------

/// BIND parameters controlling session characteristics.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BindParameters {
    pub lu_type: LuType,
    pub max_ru_size: u16,
    pub pacing_count: u8,
    pub crypto_supported: bool,
    pub bracket_protocol: bool,
}

impl BindParameters {
    pub fn new(lu_type: LuType, max_ru_size: u16) -> Result<Self, SnaError> {
        if max_ru_size > 32767 {
            return Err(SnaError::InvalidRuSize(max_ru_size));
        }
        Ok(Self {
            lu_type,
            max_ru_size,
            pacing_count: 1,
            crypto_supported: false,
            bracket_protocol: true,
        })
    }

    /// Negotiate BIND parameters between primary and secondary LU.
    pub fn negotiate(
        primary: &BindParameters,
        secondary: &BindParameters,
    ) -> Result<BindParameters, SnaError> {
        if primary.lu_type != secondary.lu_type {
            return Err(SnaError::BindFailed(format!(
                "LU type mismatch: {:?} vs {:?}",
                primary.lu_type, secondary.lu_type
            )));
        }
        Ok(BindParameters {
            lu_type: primary.lu_type,
            max_ru_size: primary.max_ru_size.min(secondary.max_ru_size),
            pacing_count: primary.pacing_count.max(secondary.pacing_count),
            crypto_supported: primary.crypto_supported && secondary.crypto_supported,
            bracket_protocol: primary.bracket_protocol && secondary.bracket_protocol,
        })
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    // NET-101.1 tests
    #[test]
    fn lu2_3270_write_and_read() {
        let mut session = Lu2Session::new("TERM0001", 24, 80);
        let data = b"Hello 3270";
        session.process_command(Command3270::Write, data);
        let buf = session.process_command(Command3270::ReadBuffer, &[]);
        assert_eq!(&buf[..data.len()], data);
    }

    #[test]
    fn lu2_erase_write() {
        let mut session = Lu2Session::new("TERM0001", 24, 80);
        session.process_command(Command3270::Write, b"Old data");
        session.process_command(Command3270::EraseWrite, b"New");
        let buf = session.process_command(Command3270::ReadBuffer, &[]);
        assert_eq!(&buf[..3], b"New");
        assert_eq!(buf[3], 0); // rest erased
    }

    #[test]
    fn command_3270_roundtrip() {
        for cmd in [
            Command3270::Write,
            Command3270::EraseWrite,
            Command3270::ReadBuffer,
            Command3270::ReadModified,
            Command3270::EraseAllUnprotected,
            Command3270::EraseWriteAlternate,
        ] {
            let byte = cmd.to_byte();
            let parsed = Command3270::from_byte(byte).unwrap();
            assert_eq!(parsed, cmd);
        }
    }

    #[test]
    fn unsupported_3270_command() {
        assert!(Command3270::from_byte(0xFF).is_err());
    }

    // NET-101.2 tests
    #[test]
    fn lu0_raw_exchange() {
        let mut session = Lu0Session::new("LU0PEER");
        session.send(b"outbound data");
        session.queue_inbound(b"inbound data");
        let out = session.drain_outbound();
        let inb = session.receive();
        assert_eq!(out, b"outbound data");
        assert_eq!(inb, b"inbound data");
    }

    #[test]
    fn lu0_empty_receive() {
        let mut session = Lu0Session::new("LU0PEER");
        assert!(session.receive().is_empty());
    }

    // NET-101.3 tests
    #[test]
    fn lu1_printer_scs_processing() {
        let mut printer = Lu1PrinterSession::new("PRT001");
        // Send data with form feeds
        printer.process_print_data(&[0x48, 0x65, 0x6C, 0x0C, 0x6C, 0x6F, 0x0C]);
        assert_eq!(printer.page_count, 2);
        let output = printer.drain_output();
        assert_eq!(output.len(), 7);
    }

    #[test]
    fn scs_control_code_parsing() {
        assert_eq!(
            ScsControlCode::from_byte(0x15).unwrap(),
            ScsControlCode::NewLine
        );
        assert_eq!(
            ScsControlCode::from_byte(0x0D).unwrap(),
            ScsControlCode::CarriageReturn
        );
        assert_eq!(
            ScsControlCode::from_byte(0x0C).unwrap(),
            ScsControlCode::FormFeed
        );
        assert!(ScsControlCode::from_byte(0xFF).is_err());
    }

    // NET-101.4 tests
    #[test]
    fn bind_parameters_creation() {
        let bp = BindParameters::new(LuType::Lu2, 3840).unwrap();
        assert_eq!(bp.max_ru_size, 3840);
        assert_eq!(bp.lu_type, LuType::Lu2);
    }

    #[test]
    fn bind_parameters_invalid_ru_size() {
        assert!(BindParameters::new(LuType::Lu2, 40000).is_err());
    }

    #[test]
    fn bind_negotiation_success() {
        let primary = BindParameters::new(LuType::Lu2, 3840).unwrap();
        let secondary = BindParameters::new(LuType::Lu2, 2048).unwrap();
        let result = BindParameters::negotiate(&primary, &secondary).unwrap();
        assert_eq!(result.max_ru_size, 2048); // min of both
    }

    #[test]
    fn bind_negotiation_lu_type_mismatch() {
        let primary = BindParameters::new(LuType::Lu2, 3840).unwrap();
        let secondary = BindParameters::new(LuType::Lu0, 2048).unwrap();
        assert!(BindParameters::negotiate(&primary, &secondary).is_err());
    }

    // NET-101.3b — LU 3 tests
    #[test]
    fn lu3_printer_write() {
        let mut printer = Lu3PrinterSession::new("PRT3270");
        printer.process_print_command(Command3270::Write, b"Report line 1");
        assert_eq!(printer.output, b"Report line 1");
    }

    #[test]
    fn lu3_printer_erase_write() {
        let mut printer = Lu3PrinterSession::new("PRT3270");
        printer.process_print_command(Command3270::Write, b"old data");
        printer.process_print_command(Command3270::EraseWrite, b"new data");
        assert_eq!(printer.output, b"old datanew data");
    }

    #[test]
    fn lu3_printer_page_eject() {
        let mut printer = Lu3PrinterSession::new("PRT3270");
        printer.process_print_command(Command3270::Write, b"page 1");
        printer.process_print_command(Command3270::EraseAllUnprotected, &[]);
        assert_eq!(printer.page_count, 1);
        assert!(printer.output.is_empty());
    }

    #[test]
    fn lu3_printer_drain() {
        let mut printer = Lu3PrinterSession::new("PRT3270");
        printer.process_print_command(Command3270::Write, b"data");
        let out = printer.drain_output();
        assert_eq!(out, b"data");
        assert!(printer.output.is_empty());
    }
}
