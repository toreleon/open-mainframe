//! Execute Interface Block (EIB).
//!
//! The EIB contains information about the current CICS environment
//! and is passed to each CICS program.

use crate::CicsResponse;

/// Execute Interface Block.
#[derive(Debug, Clone)]
pub struct Eib {
    /// Time of day (0HHMMSS+ packed decimal format, stored as u32)
    pub eibtime: u32,
    /// Date (0CYYDDD+ packed decimal format, stored as u32)
    pub eibdate: u32,
    /// Transaction ID (4 characters)
    pub eibtrnid: [u8; 4],
    /// Task number
    pub eibtaskn: u32,
    /// Terminal ID (4 characters)
    pub eibtrmid: [u8; 4],
    /// File name (8 characters)
    pub eibfn: [u8; 8],
    /// Resource type code
    pub eibrcode: [u8; 6],
    /// Data set name (8 characters)
    pub eibds: [u8; 8],
    /// Request ID (8 characters)
    pub eibreqid: [u8; 8],
    /// Resource name (8 characters)
    pub eibrsrce: [u8; 8],
    /// Sync point indicator
    pub eibsync: u8,
    /// Free indicator
    pub eibfree: u8,
    /// Receive indicator
    pub eibrecv: u8,
    /// Send indicator
    pub eibsend: u8,
    /// Attention ID (key pressed)
    pub eibaid: u8,
    /// End of chain indicator
    pub eibeoc: u8,
    /// FMH received indicator
    pub eibfmh: u8,
    /// Commarea indicator
    pub eibcompl: u8,
    /// Signal received indicator
    pub eibsig: u8,
    /// Confirmation indicator
    pub eibconf: u8,
    /// Error indicator
    pub eiberr: u8,
    /// Error code
    pub eiberrcd: [u8; 4],
    /// No handle indicator
    pub eibnodat: u8,
    /// Response code
    pub eibresp: u32,
    /// Response code 2
    pub eibresp2: u32,
    /// Rollback indicator
    pub eibrldbk: u8,
    /// Cursor position
    pub eibcposn: u16,
    /// Call type
    pub eibcalen: u16,
}

impl Default for Eib {
    fn default() -> Self {
        Self::new()
    }
}

impl Eib {
    /// Create a new EIB with default values.
    pub fn new() -> Self {
        Self {
            eibtime: 0,
            eibdate: 0,
            eibtrnid: [b' '; 4],
            eibtaskn: 0,
            eibtrmid: [b' '; 4],
            eibfn: [b' '; 8],
            eibrcode: [0; 6],
            eibds: [b' '; 8],
            eibreqid: [b' '; 8],
            eibrsrce: [b' '; 8],
            eibsync: 0,
            eibfree: 0,
            eibrecv: 1,
            eibsend: 0,
            eibaid: 0,
            eibeoc: 0,
            eibfmh: 0,
            eibcompl: 0,
            eibsig: 0,
            eibconf: 0,
            eiberr: 0,
            eiberrcd: [0; 4],
            eibnodat: 0,
            eibresp: 0,
            eibresp2: 0,
            eibrldbk: 0,
            eibcposn: 0,
            eibcalen: 0,
        }
    }

    /// Set transaction ID.
    pub fn set_transaction_id(&mut self, id: &str) {
        let bytes = id.as_bytes();
        self.eibtrnid = [b' '; 4];
        for (i, &b) in bytes.iter().take(4).enumerate() {
            self.eibtrnid[i] = b;
        }
    }

    /// Get transaction ID as string.
    pub fn transaction_id(&self) -> String {
        String::from_utf8_lossy(&self.eibtrnid).trim_end().to_string()
    }

    /// Set terminal ID.
    pub fn set_terminal_id(&mut self, id: &str) {
        let bytes = id.as_bytes();
        self.eibtrmid = [b' '; 4];
        for (i, &b) in bytes.iter().take(4).enumerate() {
            self.eibtrmid[i] = b;
        }
    }

    /// Get terminal ID as string.
    pub fn terminal_id(&self) -> String {
        String::from_utf8_lossy(&self.eibtrmid).trim_end().to_string()
    }

    /// Set time (in 0HHMMSS format).
    pub fn set_time(&mut self, hour: u8, minute: u8, second: u8) {
        // Pack as 0HHMMSS
        self.eibtime = (hour as u32) * 10000 + (minute as u32) * 100 + (second as u32);
    }

    /// Set date (in 0CYYDDD format).
    pub fn set_date(&mut self, year: u16, day_of_year: u16) {
        // Format: 0CYYDDD where C = century (0 = 1900s, 1 = 2000s)
        let century = if year >= 2000 { 1u32 } else { 0 };
        let yy = (year % 100) as u32;
        self.eibdate = century * 1000000 + yy * 1000 + (day_of_year as u32);
    }

    /// Set response code.
    pub fn set_response(&mut self, resp: CicsResponse) {
        self.eibresp = resp as u32;
    }

    /// Set response code 2.
    pub fn set_response2(&mut self, resp2: u32) {
        self.eibresp2 = resp2;
    }

    /// Set COMMAREA length.
    pub fn set_commarea_length(&mut self, len: u16) {
        self.eibcalen = len;
    }

    /// Set attention ID (key pressed).
    pub fn set_aid(&mut self, aid: u8) {
        self.eibaid = aid;
    }

    /// Set cursor position.
    pub fn set_cursor_position(&mut self, pos: u16) {
        self.eibcposn = pos;
    }

    /// Set file name.
    pub fn set_filename(&mut self, name: &str) {
        let bytes = name.as_bytes();
        self.eibds = [b' '; 8];
        for (i, &b) in bytes.iter().take(8).enumerate() {
            self.eibds[i] = b;
        }
    }

    /// Reset for new command.
    pub fn reset_for_command(&mut self) {
        self.eibresp = 0;
        self.eibresp2 = 0;
        self.eiberr = 0;
        self.eiberrcd = [0; 4];
    }
}

/// Attention Identifier (AID) values.
#[allow(dead_code)]
pub mod aid {
    /// No AID
    pub const NO_AID: u8 = 0x60;
    /// ENTER key
    pub const ENTER: u8 = 0x7D;
    /// PF1
    pub const PF1: u8 = 0xF1;
    /// PF2
    pub const PF2: u8 = 0xF2;
    /// PF3
    pub const PF3: u8 = 0xF3;
    /// PF4
    pub const PF4: u8 = 0xF4;
    /// PF5
    pub const PF5: u8 = 0xF5;
    /// PF6
    pub const PF6: u8 = 0xF6;
    /// PF7
    pub const PF7: u8 = 0xF7;
    /// PF8
    pub const PF8: u8 = 0xF8;
    /// PF9
    pub const PF9: u8 = 0xF9;
    /// PF10
    pub const PF10: u8 = 0x7A;
    /// PF11
    pub const PF11: u8 = 0x7B;
    /// PF12
    pub const PF12: u8 = 0x7C;
    /// PA1
    pub const PA1: u8 = 0x6C;
    /// PA2
    pub const PA2: u8 = 0x6E;
    /// PA3
    pub const PA3: u8 = 0x6B;
    /// CLEAR key
    pub const CLEAR: u8 = 0x6D;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_eib_default() {
        let eib = Eib::new();
        assert_eq!(eib.eibresp, 0);
        assert_eq!(eib.eibcalen, 0);
    }

    #[test]
    fn test_set_transaction_id() {
        let mut eib = Eib::new();
        eib.set_transaction_id("MENU");
        assert_eq!(eib.transaction_id(), "MENU");
    }

    #[test]
    fn test_set_terminal_id() {
        let mut eib = Eib::new();
        eib.set_terminal_id("T001");
        assert_eq!(eib.terminal_id(), "T001");
    }

    #[test]
    fn test_set_time() {
        let mut eib = Eib::new();
        eib.set_time(14, 30, 45);
        assert_eq!(eib.eibtime, 143045);
    }

    #[test]
    fn test_set_date() {
        let mut eib = Eib::new();
        eib.set_date(2026, 44); // Feb 13, 2026 is day 44
        // Expected: 1026044 (century 1, year 26, day 044)
        assert_eq!(eib.eibdate, 1026044);
    }

    #[test]
    fn test_set_response() {
        let mut eib = Eib::new();
        eib.set_response(CicsResponse::Notfnd);
        assert_eq!(eib.eibresp, 13);
    }

    #[test]
    fn test_set_commarea_length() {
        let mut eib = Eib::new();
        eib.set_commarea_length(100);
        assert_eq!(eib.eibcalen, 100);
    }

    #[test]
    fn test_reset_for_command() {
        let mut eib = Eib::new();
        eib.set_response(CicsResponse::Notfnd);
        eib.set_response2(1);
        eib.eiberr = 1;

        eib.reset_for_command();

        assert_eq!(eib.eibresp, 0);
        assert_eq!(eib.eibresp2, 0);
        assert_eq!(eib.eiberr, 0);
    }
}
