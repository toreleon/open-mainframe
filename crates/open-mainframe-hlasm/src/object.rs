//! Object code generation for HLASM.
//!
//! Generates traditional 80-byte object deck format:
//! - **ESD** (External Symbol Dictionary) records
//! - **TXT** (Text) records containing machine code
//! - **RLD** (Relocation Dictionary) records for address constants
//! - **END** record marking the assembly end
//!
//! Also supports:
//! - AMODE/RMODE attributes
//! - ENTRY/EXTRN/WXTRN external symbol management
//! - Minimal GOFF (Generalized Object File Format) support

use std::collections::HashMap;

// ---------------------------------------------------------------------------
//  AMODE / RMODE
// ---------------------------------------------------------------------------

/// Addressing mode.
#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum Amode {
    /// 24-bit addressing (below the line).
    A24,
    /// 31-bit addressing (below the bar).
    A31,
    /// 64-bit addressing (above the bar).
    A64,
    /// Any addressing mode.
    Any,
}

/// Residency mode.
#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum Rmode {
    /// Must reside below 16MB.
    R24,
    /// May reside below 2GB.
    R31,
    /// May reside anywhere below 2GB.
    Any,
    /// May reside above 2GB (with AMODE 64).
    R64,
}

// ---------------------------------------------------------------------------
//  External Symbol Dictionary (ESD)
// ---------------------------------------------------------------------------

/// Type of ESD item.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EsdType {
    /// Control Section (SD).
    Sd,
    /// External Reference (ER).
    Er,
    /// Weak External Reference (WX).
    Wx,
    /// Label Definition / Entry Point (LD).
    Ld,
    /// Common section (CM).
    Cm,
    /// Dummy section (not stored in ESD, just for reference).
    Dsect,
}

/// An ESD item (external symbol).
#[derive(Debug, Clone)]
pub struct EsdItem {
    /// External symbol ID (1-based index).
    pub esdid: u16,
    /// Symbol name (up to 8 characters in OBJ, longer in GOFF).
    pub name: String,
    /// ESD type.
    pub esd_type: EsdType,
    /// Address within the module.
    pub address: u32,
    /// Length (for SD/CM sections).
    pub length: u32,
    /// AMODE attribute.
    pub amode: Option<Amode>,
    /// RMODE attribute.
    pub rmode: Option<Rmode>,
    /// Owner ESDID for LD items.
    pub owner: Option<u16>,
}

// ---------------------------------------------------------------------------
//  Text record
// ---------------------------------------------------------------------------

/// A TXT record — a chunk of object code.
#[derive(Debug, Clone)]
pub struct TxtRecord {
    /// ESDID of the containing section.
    pub esdid: u16,
    /// Address offset within the section.
    pub address: u32,
    /// Object code bytes (up to 56 bytes per record in OBJ format).
    pub data: Vec<u8>,
}

// ---------------------------------------------------------------------------
//  Relocation Dictionary (RLD)
// ---------------------------------------------------------------------------

/// A relocation entry.
#[derive(Debug, Clone)]
pub struct RldEntry {
    /// ESDID of the relocation target (the address constant references this).
    pub r_esdid: u16,
    /// ESDID of the section containing the address constant.
    pub p_esdid: u16,
    /// Offset of the address constant within the section.
    pub address: u32,
    /// Length of the address constant in bytes (typically 4 for A-type, 8 for AD-type).
    pub length: u8,
    /// Whether this is a negative relocation (V-type constants).
    pub negative: bool,
}

// ---------------------------------------------------------------------------
//  Object module
// ---------------------------------------------------------------------------

/// A complete object module produced by the assembler.
#[derive(Debug)]
pub struct ObjectModule {
    /// Module name (from first CSECT or START).
    pub name: String,
    /// ESD items.
    pub esd_items: Vec<EsdItem>,
    /// ESD items by name for quick lookup.
    esd_by_name: HashMap<String, u16>,
    /// Next ESDID to assign.
    next_esdid: u16,
    /// Text records.
    pub txt_records: Vec<TxtRecord>,
    /// Relocation entries.
    pub rld_entries: Vec<RldEntry>,
    /// Entry point ESDID (from END statement).
    pub entry_esdid: Option<u16>,
    /// Entry point address.
    pub entry_address: Option<u32>,
    /// Current text buffer being accumulated.
    current_txt: Vec<u8>,
    /// Current text buffer ESDID.
    current_txt_esdid: u16,
    /// Current text buffer start address.
    current_txt_addr: u32,
}

impl Default for ObjectModule {
    fn default() -> Self {
        Self::new("")
    }
}

impl ObjectModule {
    /// Create a new object module.
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_uppercase(),
            esd_items: Vec::new(),
            esd_by_name: HashMap::new(),
            next_esdid: 1,
            txt_records: Vec::new(),
            rld_entries: Vec::new(),
            entry_esdid: None,
            entry_address: None,
            current_txt: Vec::new(),
            current_txt_esdid: 0,
            current_txt_addr: 0,
        }
    }

    // -- ESD management ---------------------------------------------------

    /// Define a control section (CSECT).
    pub fn define_csect(&mut self, name: &str) -> u16 {
        let upper = name.to_uppercase();
        if let Some(&esdid) = self.esd_by_name.get(&upper) {
            return esdid;
        }
        let esdid = self.next_esdid;
        self.next_esdid += 1;
        let item = EsdItem {
            esdid,
            name: upper.clone(),
            esd_type: EsdType::Sd,
            address: 0,
            length: 0,
            amode: None,
            rmode: None,
            owner: None,
        };
        self.esd_items.push(item);
        self.esd_by_name.insert(upper, esdid);
        if self.name.is_empty() {
            self.name = name.to_uppercase();
        }
        esdid
    }

    /// Declare an external reference (EXTRN).
    pub fn define_extrn(&mut self, name: &str) -> u16 {
        let upper = name.to_uppercase();
        if let Some(&esdid) = self.esd_by_name.get(&upper) {
            return esdid;
        }
        let esdid = self.next_esdid;
        self.next_esdid += 1;
        let item = EsdItem {
            esdid,
            name: upper.clone(),
            esd_type: EsdType::Er,
            address: 0,
            length: 0,
            amode: None,
            rmode: None,
            owner: None,
        };
        self.esd_items.push(item);
        self.esd_by_name.insert(upper, esdid);
        esdid
    }

    /// Declare a weak external reference (WXTRN).
    pub fn define_wxtrn(&mut self, name: &str) -> u16 {
        let upper = name.to_uppercase();
        if let Some(&esdid) = self.esd_by_name.get(&upper) {
            return esdid;
        }
        let esdid = self.next_esdid;
        self.next_esdid += 1;
        let item = EsdItem {
            esdid,
            name: upper.clone(),
            esd_type: EsdType::Wx,
            address: 0,
            length: 0,
            amode: None,
            rmode: None,
            owner: None,
        };
        self.esd_items.push(item);
        self.esd_by_name.insert(upper, esdid);
        esdid
    }

    /// Declare an entry point (ENTRY).
    pub fn define_entry(&mut self, name: &str, address: u32, owner_esdid: u16) -> u16 {
        let upper = name.to_uppercase();
        if let Some(&esdid) = self.esd_by_name.get(&upper) {
            return esdid;
        }
        let esdid = self.next_esdid;
        self.next_esdid += 1;
        let item = EsdItem {
            esdid,
            name: upper.clone(),
            esd_type: EsdType::Ld,
            address,
            length: 0,
            amode: None,
            rmode: None,
            owner: Some(owner_esdid),
        };
        self.esd_items.push(item);
        self.esd_by_name.insert(upper, esdid);
        esdid
    }

    /// Set AMODE for an ESD item.
    pub fn set_amode(&mut self, esdid: u16, amode: Amode) {
        if let Some(item) = self.esd_items.iter_mut().find(|e| e.esdid == esdid) {
            item.amode = Some(amode);
        }
    }

    /// Set RMODE for an ESD item.
    pub fn set_rmode(&mut self, esdid: u16, rmode: Rmode) {
        if let Some(item) = self.esd_items.iter_mut().find(|e| e.esdid == esdid) {
            item.rmode = Some(rmode);
        }
    }

    /// Update the length of a section.
    pub fn set_section_length(&mut self, esdid: u16, length: u32) {
        if let Some(item) = self.esd_items.iter_mut().find(|e| e.esdid == esdid) {
            item.length = length;
        }
    }

    /// Look up an ESD item by name.
    pub fn lookup_esd(&self, name: &str) -> Option<&EsdItem> {
        self.esd_by_name
            .get(&name.to_uppercase())
            .and_then(|&esdid| self.esd_items.iter().find(|e| e.esdid == esdid))
    }

    // -- Text emission ----------------------------------------------------

    /// Emit object code bytes.
    pub fn emit_text(&mut self, esdid: u16, address: u32, data: &[u8]) {
        // Flush if changing section or if buffer would exceed 56 bytes.
        if !self.current_txt.is_empty()
            && (self.current_txt_esdid != esdid
                || self.current_txt_addr + self.current_txt.len() as u32 != address
                || self.current_txt.len() + data.len() > 56)
        {
            self.flush_txt();
        }

        if self.current_txt.is_empty() {
            self.current_txt_esdid = esdid;
            self.current_txt_addr = address;
        }

        self.current_txt.extend_from_slice(data);
    }

    /// Flush the current text buffer into a TXT record.
    pub fn flush_txt(&mut self) {
        if !self.current_txt.is_empty() {
            self.txt_records.push(TxtRecord {
                esdid: self.current_txt_esdid,
                address: self.current_txt_addr,
                data: std::mem::take(&mut self.current_txt),
            });
        }
    }

    // -- RLD emission -----------------------------------------------------

    /// Add a relocation entry.
    pub fn add_rld(&mut self, r_esdid: u16, p_esdid: u16, address: u32, length: u8, negative: bool) {
        self.rld_entries.push(RldEntry {
            r_esdid,
            p_esdid,
            address,
            length,
            negative,
        });
    }

    // -- END record -------------------------------------------------------

    /// Set the entry point (from END statement).
    pub fn set_entry(&mut self, esdid: u16, address: u32) {
        self.entry_esdid = Some(esdid);
        self.entry_address = Some(address);
    }

    // -- OBJ format generation --------------------------------------------

    /// Generate the complete object deck as 80-byte records.
    pub fn generate_obj(&mut self) -> Vec<[u8; 80]> {
        self.flush_txt();
        let mut records = Vec::new();

        // ESD records.
        for item in &self.esd_items {
            records.push(format_esd_record(item));
        }

        // TXT records.
        for txt in &self.txt_records {
            records.push(format_txt_record(txt));
        }

        // RLD records.
        for rld in &self.rld_entries {
            records.push(format_rld_record(rld));
        }

        // END record.
        records.push(format_end_record(self.entry_address, self.entry_esdid));

        records
    }

    /// Generate the object module as a byte vector (all 80-byte records concatenated).
    pub fn generate_obj_bytes(&mut self) -> Vec<u8> {
        let records = self.generate_obj();
        let mut bytes = Vec::with_capacity(records.len() * 80);
        for rec in &records {
            bytes.extend_from_slice(rec);
        }
        bytes
    }

    /// Total object code size across all TXT records.
    pub fn total_text_size(&self) -> usize {
        self.txt_records.iter().map(|t| t.data.len()).sum::<usize>() + self.current_txt.len()
    }
}

// ---------------------------------------------------------------------------
//  OBJ record formatting
// ---------------------------------------------------------------------------

/// Format an ESD record (80 bytes).
///
/// Layout:
/// - Col 1: X'02' (ESD)
/// - Col 2-4: "ESD"
/// - Col 5-10: blank
/// - Col 11-12: ESD ID (halfword)
/// - Col 13-14: blank
/// - Col 15-16: item count (1)
/// - Col 17-24: symbol name (8 chars, padded)
/// - Col 25: ESD type code
/// - Col 26-28: address (3 bytes)
/// - Col 29: alignment / AMODE
/// - Col 30-32: length (3 bytes)
/// - Col 33-72: blank
/// - Col 73-80: sequence
fn format_esd_record(item: &EsdItem) -> [u8; 80] {
    let mut rec = [0x40u8; 80]; // EBCDIC spaces

    rec[0] = 0x02; // ESD indicator
    // "ESD" in EBCDIC: E=0xC5, S=0xE2, D=0xC4
    rec[1] = 0xC5;
    rec[2] = 0xE2;
    rec[3] = 0xC4;

    // ESDID at cols 11-12 (bytes 10-11).
    rec[10] = (item.esdid >> 8) as u8;
    rec[11] = item.esdid as u8;

    // Item count = 1 at cols 15-16 (bytes 14-15).
    rec[14] = 0;
    rec[15] = 1;

    // Symbol name at cols 17-24 (bytes 16-23), 8 chars EBCDIC padded.
    let name_bytes = ascii_to_ebcdic_padded(&item.name, 8);
    rec[16..24].copy_from_slice(&name_bytes);

    // Type code at col 25 (byte 24).
    rec[24] = match item.esd_type {
        EsdType::Sd => 0x00,
        EsdType::Er => 0x02,
        EsdType::Wx => 0x0A,
        EsdType::Ld => 0x01,
        EsdType::Cm => 0x05,
        EsdType::Dsect => 0x00, // Not normally in ESD.
    };

    // Address at cols 26-28 (bytes 25-27), 3 bytes.
    rec[25] = (item.address >> 16) as u8;
    rec[26] = (item.address >> 8) as u8;
    rec[27] = item.address as u8;

    // AMODE at col 29 (byte 28).
    rec[28] = match item.amode {
        Some(Amode::A24) => 0x01,
        Some(Amode::A31) => 0x02,
        Some(Amode::A64) => 0x04,
        Some(Amode::Any) => 0x03,
        None => 0x00,
    };

    // Length at cols 30-32 (bytes 29-31), 3 bytes.
    rec[29] = (item.length >> 16) as u8;
    rec[30] = (item.length >> 8) as u8;
    rec[31] = item.length as u8;

    // Sequence field cols 73-80 (bytes 72-79).
    let seq = ascii_to_ebcdic_padded("ESD     ", 8);
    rec[72..80].copy_from_slice(&seq);

    rec
}

/// Format a TXT record (80 bytes).
///
/// Layout:
/// - Col 1: X'02' (TXT)
/// - Col 2-4: "TXT"
/// - Col 5: blank
/// - Col 6-8: address (3 bytes)
/// - Col 9-10: blank
/// - Col 11-12: byte count (halfword)
/// - Col 13-14: blank
/// - Col 15-16: ESDID (halfword)
/// - Col 17-72: object code (up to 56 bytes)
/// - Col 73-80: sequence
fn format_txt_record(txt: &TxtRecord) -> [u8; 80] {
    let mut rec = [0x40u8; 80]; // EBCDIC spaces

    rec[0] = 0x02; // TXT indicator
    // "TXT" in EBCDIC: T=0xE3, X=0xE7, T=0xE3
    rec[1] = 0xE3;
    rec[2] = 0xE7;
    rec[3] = 0xE3;

    // Address at cols 6-8 (bytes 5-7).
    rec[5] = (txt.address >> 16) as u8;
    rec[6] = (txt.address >> 8) as u8;
    rec[7] = txt.address as u8;

    // Byte count at cols 11-12 (bytes 10-11).
    let len = txt.data.len() as u16;
    rec[10] = (len >> 8) as u8;
    rec[11] = len as u8;

    // ESDID at cols 15-16 (bytes 14-15).
    rec[14] = (txt.esdid >> 8) as u8;
    rec[15] = txt.esdid as u8;

    // Object code at cols 17-72 (bytes 16-71), up to 56 bytes.
    let copy_len = txt.data.len().min(56);
    rec[16..16 + copy_len].copy_from_slice(&txt.data[..copy_len]);

    // Sequence.
    let seq = ascii_to_ebcdic_padded("TXT     ", 8);
    rec[72..80].copy_from_slice(&seq);

    rec
}

/// Format an RLD record (80 bytes).
///
/// Layout:
/// - Col 1: X'02' (RLD)
/// - Col 2-4: "RLD"
/// - Col 5-10: blank
/// - Col 11-12: data length (halfword)
/// - Col 13-16: blank
/// - Col 17+: RLD data entries (8 bytes each)
/// - Col 73-80: sequence
fn format_rld_record(rld: &RldEntry) -> [u8; 80] {
    let mut rec = [0x40u8; 80]; // EBCDIC spaces

    rec[0] = 0x02;
    // "RLD" in EBCDIC: R=0xD9, L=0xD3, D=0xC4
    rec[1] = 0xD9;
    rec[2] = 0xD3;
    rec[3] = 0xC4;

    // Data length at cols 11-12 (bytes 10-11) = 8 (one entry).
    rec[10] = 0;
    rec[11] = 8;

    // RLD entry at cols 17-24 (bytes 16-23):
    // Bytes 0-1: R-ESDID (target)
    // Bytes 2-3: P-ESDID (position)
    // Byte 4: flag byte (length-1 in bits 0-1, sign in bit 2)
    // Bytes 5-7: address (3 bytes)
    rec[16] = (rld.r_esdid >> 8) as u8;
    rec[17] = rld.r_esdid as u8;
    rec[18] = (rld.p_esdid >> 8) as u8;
    rec[19] = rld.p_esdid as u8;

    let len_code = ((rld.length - 1) & 0x03) << 6;
    let sign_bit = if rld.negative { 0x02 } else { 0x00 };
    rec[20] = len_code | sign_bit;

    rec[21] = (rld.address >> 16) as u8;
    rec[22] = (rld.address >> 8) as u8;
    rec[23] = rld.address as u8;

    // Sequence.
    let seq = ascii_to_ebcdic_padded("RLD     ", 8);
    rec[72..80].copy_from_slice(&seq);

    rec
}

/// Format an END record (80 bytes).
///
/// Layout:
/// - Col 1: X'02' (END)
/// - Col 2-4: "END"
/// - Col 5: blank
/// - Col 6-8: entry address (3 bytes)
/// - Col 9-14: blank
/// - Col 15-16: entry ESDID (halfword)
/// - Col 17-72: blank
/// - Col 73-80: sequence
fn format_end_record(entry_address: Option<u32>, entry_esdid: Option<u16>) -> [u8; 80] {
    let mut rec = [0x40u8; 80]; // EBCDIC spaces

    rec[0] = 0x02;
    // "END" in EBCDIC: E=0xC5, N=0xD5, D=0xC4
    rec[1] = 0xC5;
    rec[2] = 0xD5;
    rec[3] = 0xC4;

    if let Some(addr) = entry_address {
        rec[5] = (addr >> 16) as u8;
        rec[6] = (addr >> 8) as u8;
        rec[7] = addr as u8;
    }

    if let Some(esdid) = entry_esdid {
        rec[14] = (esdid >> 8) as u8;
        rec[15] = esdid as u8;
    }

    // Sequence.
    let seq = ascii_to_ebcdic_padded("END     ", 8);
    rec[72..80].copy_from_slice(&seq);

    rec
}

// ---------------------------------------------------------------------------
//  GOFF (Generalized Object File Format) — minimal support
// ---------------------------------------------------------------------------

/// GOFF record types.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum GoffRecordType {
    /// Header.
    Hdr,
    /// External Symbol Dictionary.
    Esd,
    /// Text.
    Txt,
    /// Relocation.
    Rld,
    /// End.
    End,
}

/// A minimal GOFF record.
#[derive(Debug, Clone)]
pub struct GoffRecord {
    /// Record type.
    pub record_type: GoffRecordType,
    /// Raw data bytes.
    pub data: Vec<u8>,
}

/// Generate a minimal GOFF header record.
pub fn goff_header() -> GoffRecord {
    let mut data = vec![0u8; 80];
    // GOFF header: byte 0 = 0x03 (PTV), high nibble = record type 0 (HDR).
    data[0] = 0x03;
    data[1] = 0x00; // HDR record type indicator.
    // Architecture level.
    data[4] = 0x01; // GOFF version 1.
    GoffRecord {
        record_type: GoffRecordType::Hdr,
        data,
    }
}

/// Generate a minimal GOFF end record.
pub fn goff_end() -> GoffRecord {
    let mut data = vec![0u8; 80];
    data[0] = 0x03;
    data[1] = 0x04; // END record type.
    GoffRecord {
        record_type: GoffRecordType::End,
        data,
    }
}

// ---------------------------------------------------------------------------
//  EBCDIC helper
// ---------------------------------------------------------------------------

/// Convert ASCII string to EBCDIC bytes, padded to `len` with EBCDIC spaces (0x40).
fn ascii_to_ebcdic_padded(s: &str, len: usize) -> Vec<u8> {
    let mut result = vec![0x40u8; len];
    for (i, ch) in s.bytes().enumerate() {
        if i >= len {
            break;
        }
        result[i] = ascii_to_ebcdic_byte(ch);
    }
    result
}

/// Simple ASCII to EBCDIC single-byte conversion for printable characters.
fn ascii_to_ebcdic_byte(b: u8) -> u8 {
    match b {
        b' ' => 0x40,
        b'0'..=b'9' => 0xF0 + (b - b'0'),
        b'A'..=b'I' => 0xC1 + (b - b'A'),
        b'J'..=b'R' => 0xD1 + (b - b'J'),
        b'S'..=b'Z' => 0xE2 + (b - b'S'),
        b'a'..=b'i' => 0x81 + (b - b'a'),
        b'j'..=b'r' => 0x91 + (b - b'j'),
        b's'..=b'z' => 0xA2 + (b - b's'),
        b'.' => 0x4B,
        b'(' => 0x4D,
        b')' => 0x5D,
        b'+' => 0x4E,
        b'-' => 0x60,
        b'*' => 0x5C,
        b'/' => 0x61,
        b',' => 0x6B,
        b'\'' => 0x7D,
        b'=' => 0x7E,
        b'&' => 0x50,
        b'@' => 0x7C,
        b'#' => 0x7B,
        b'$' => 0x5B,
        _ => 0x40, // Default to space.
    }
}

// ---------------------------------------------------------------------------
//  Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_define_csect() {
        let mut obj = ObjectModule::new("TEST");
        let esdid = obj.define_csect("MYCSECT");
        assert_eq!(esdid, 1);
        let item = obj.lookup_esd("MYCSECT").unwrap();
        assert_eq!(item.esd_type, EsdType::Sd);
        assert_eq!(item.name, "MYCSECT");
    }

    #[test]
    fn test_define_csect_idempotent() {
        let mut obj = ObjectModule::new("TEST");
        let id1 = obj.define_csect("MYCSECT");
        let id2 = obj.define_csect("MYCSECT");
        assert_eq!(id1, id2);
        assert_eq!(obj.esd_items.len(), 1);
    }

    #[test]
    fn test_define_extrn() {
        let mut obj = ObjectModule::new("TEST");
        let esdid = obj.define_extrn("EXTFUNC");
        assert_eq!(esdid, 1);
        let item = obj.lookup_esd("EXTFUNC").unwrap();
        assert_eq!(item.esd_type, EsdType::Er);
    }

    #[test]
    fn test_define_wxtrn() {
        let mut obj = ObjectModule::new("TEST");
        let esdid = obj.define_wxtrn("WEAKREF");
        assert_eq!(esdid, 1);
        let item = obj.lookup_esd("WEAKREF").unwrap();
        assert_eq!(item.esd_type, EsdType::Wx);
    }

    #[test]
    fn test_define_entry() {
        let mut obj = ObjectModule::new("TEST");
        let csect = obj.define_csect("MYCSECT");
        let entry = obj.define_entry("MYENTRY", 0x100, csect);
        assert_eq!(entry, 2);
        let item = obj.lookup_esd("MYENTRY").unwrap();
        assert_eq!(item.esd_type, EsdType::Ld);
        assert_eq!(item.address, 0x100);
        assert_eq!(item.owner, Some(csect));
    }

    #[test]
    fn test_set_amode_rmode() {
        let mut obj = ObjectModule::new("TEST");
        let csect = obj.define_csect("MYCSECT");
        obj.set_amode(csect, Amode::A31);
        obj.set_rmode(csect, Rmode::Any);
        let item = obj.lookup_esd("MYCSECT").unwrap();
        assert_eq!(item.amode, Some(Amode::A31));
        assert_eq!(item.rmode, Some(Rmode::Any));
    }

    #[test]
    fn test_emit_text() {
        let mut obj = ObjectModule::new("TEST");
        let csect = obj.define_csect("CODE");
        obj.emit_text(csect, 0, &[0x18, 0x35]); // LR 3,5
        obj.emit_text(csect, 2, &[0x1A, 0x12]); // AR 1,2
        obj.flush_txt();
        assert_eq!(obj.txt_records.len(), 1);
        assert_eq!(obj.txt_records[0].data, vec![0x18, 0x35, 0x1A, 0x12]);
    }

    #[test]
    fn test_emit_text_flush_on_gap() {
        let mut obj = ObjectModule::new("TEST");
        let csect = obj.define_csect("CODE");
        obj.emit_text(csect, 0, &[0x18, 0x35]);
        obj.emit_text(csect, 100, &[0x1A, 0x12]); // Non-contiguous
        obj.flush_txt();
        assert_eq!(obj.txt_records.len(), 2);
    }

    #[test]
    fn test_add_rld() {
        let mut obj = ObjectModule::new("TEST");
        let csect = obj.define_csect("CODE");
        let extrn = obj.define_extrn("EXTFUNC");
        obj.add_rld(extrn, csect, 0x10, 4, false);
        assert_eq!(obj.rld_entries.len(), 1);
        assert_eq!(obj.rld_entries[0].r_esdid, extrn);
        assert_eq!(obj.rld_entries[0].p_esdid, csect);
    }

    #[test]
    fn test_set_entry() {
        let mut obj = ObjectModule::new("TEST");
        let csect = obj.define_csect("CODE");
        obj.set_entry(csect, 0);
        assert_eq!(obj.entry_esdid, Some(csect));
        assert_eq!(obj.entry_address, Some(0));
    }

    #[test]
    fn test_generate_obj() {
        let mut obj = ObjectModule::new("TEST");
        let csect = obj.define_csect("CODE");
        obj.set_section_length(csect, 4);
        obj.emit_text(csect, 0, &[0x18, 0x35, 0x1A, 0x12]);
        obj.set_entry(csect, 0);

        let records = obj.generate_obj();
        // Should have: 1 ESD + 1 TXT + 0 RLD + 1 END = 3 records.
        assert_eq!(records.len(), 3);

        // Check ESD record type indicator.
        assert_eq!(records[0][0], 0x02);
        assert_eq!(records[0][1], 0xC5); // 'E' in EBCDIC

        // Check TXT record.
        assert_eq!(records[1][0], 0x02);
        assert_eq!(records[1][1], 0xE3); // 'T' in EBCDIC

        // Check END record.
        assert_eq!(records[2][0], 0x02);
        assert_eq!(records[2][1], 0xC5); // 'E' in EBCDIC
    }

    #[test]
    fn test_generate_obj_bytes() {
        let mut obj = ObjectModule::new("TEST");
        let csect = obj.define_csect("CODE");
        obj.emit_text(csect, 0, &[0x07, 0xFE]); // BR 14
        obj.set_entry(csect, 0);

        let bytes = obj.generate_obj_bytes();
        // 1 ESD + 1 TXT + 1 END = 3 records × 80 bytes = 240.
        assert_eq!(bytes.len(), 240);
    }

    #[test]
    fn test_total_text_size() {
        let mut obj = ObjectModule::new("TEST");
        let csect = obj.define_csect("CODE");
        obj.emit_text(csect, 0, &[0x18, 0x35]);
        assert_eq!(obj.total_text_size(), 2);
        obj.flush_txt();
        assert_eq!(obj.total_text_size(), 2);
    }

    #[test]
    fn test_obj_with_rld() {
        let mut obj = ObjectModule::new("TEST");
        let csect = obj.define_csect("CODE");
        let extrn = obj.define_extrn("EXTFUNC");
        obj.emit_text(csect, 0, &[0x00, 0x00, 0x00, 0x00]); // A(EXTFUNC) placeholder
        obj.add_rld(extrn, csect, 0, 4, false);
        obj.set_entry(csect, 0);

        let records = obj.generate_obj();
        // 2 ESD + 1 TXT + 1 RLD + 1 END = 5 records.
        assert_eq!(records.len(), 5);
    }

    #[test]
    fn test_esd_record_format() {
        let item = EsdItem {
            esdid: 1,
            name: "MYCSECT".to_string(),
            esd_type: EsdType::Sd,
            address: 0,
            length: 0x100,
            amode: Some(Amode::A31),
            rmode: Some(Rmode::Any),
            owner: None,
        };
        let rec = format_esd_record(&item);

        // Check ESDID at bytes 10-11.
        assert_eq!(rec[10], 0x00);
        assert_eq!(rec[11], 0x01);

        // Check type code at byte 24.
        assert_eq!(rec[24], 0x00); // SD type

        // Check AMODE at byte 28.
        assert_eq!(rec[28], 0x02); // AMODE 31

        // Check length at bytes 29-31.
        assert_eq!(rec[29], 0x00);
        assert_eq!(rec[30], 0x01);
        assert_eq!(rec[31], 0x00); // 0x100
    }

    #[test]
    fn test_ascii_to_ebcdic() {
        assert_eq!(ascii_to_ebcdic_byte(b'A'), 0xC1);
        assert_eq!(ascii_to_ebcdic_byte(b'Z'), 0xE9);
        assert_eq!(ascii_to_ebcdic_byte(b'0'), 0xF0);
        assert_eq!(ascii_to_ebcdic_byte(b'9'), 0xF9);
        assert_eq!(ascii_to_ebcdic_byte(b' '), 0x40);
    }

    #[test]
    fn test_goff_header_and_end() {
        let hdr = goff_header();
        assert_eq!(hdr.record_type, GoffRecordType::Hdr);
        assert_eq!(hdr.data[0], 0x03);

        let end = goff_end();
        assert_eq!(end.record_type, GoffRecordType::End);
        assert_eq!(end.data[1], 0x04);
    }

    #[test]
    fn test_module_default_name() {
        let mut obj = ObjectModule::new("");
        obj.define_csect("FIRST");
        assert_eq!(obj.name, "FIRST");
    }

    #[test]
    fn test_section_length_update() {
        let mut obj = ObjectModule::new("TEST");
        let csect = obj.define_csect("CODE");
        obj.set_section_length(csect, 1024);
        let item = obj.lookup_esd("CODE").unwrap();
        assert_eq!(item.length, 1024);
    }
}
