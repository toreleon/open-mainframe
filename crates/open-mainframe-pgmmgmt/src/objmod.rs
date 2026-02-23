//! # Object Module Format Parser
//!
//! Parses z/OS object module format (ESD/TXT/RLD/END records)
//! and load module format (CESD, text records).
//!
//! ## Object Module Record Types
//! - **ESD** — External Symbol Dictionary
//! - **TXT** — Text (code/data)
//! - **RLD** — Relocation Dictionary
//! - **END** — End of module

use std::collections::HashMap;

use crate::binder::{AdconType, EsdEntry, EsdType, ObjectModule, RldEntry, TextRecord};

// ─────────────────────── Record Type Identification ───────────────────────

/// Object module record type identifier.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RecordType {
    /// ESD — External Symbol Dictionary.
    Esd,
    /// TXT — Text record.
    Txt,
    /// RLD — Relocation Dictionary.
    Rld,
    /// END — End of module.
    End,
    /// SYM — Symbol table (ignored).
    Sym,
}

impl RecordType {
    /// Parse record type from the 3-byte identifier (columns 2-4).
    pub fn from_bytes(bytes: &[u8]) -> Option<Self> {
        match bytes {
            b"ESD" => Some(RecordType::Esd),
            b"TXT" => Some(RecordType::Txt),
            b"RLD" => Some(RecordType::Rld),
            b"END" => Some(RecordType::End),
            b"SYM" => Some(RecordType::Sym),
            _ => None,
        }
    }

    /// To 3-byte identifier.
    pub fn as_bytes(self) -> &'static [u8; 3] {
        match self {
            RecordType::Esd => b"ESD",
            RecordType::Txt => b"TXT",
            RecordType::Rld => b"RLD",
            RecordType::End => b"END",
            RecordType::Sym => b"SYM",
        }
    }
}

// ─────────────────────── Object Module Parser ───────────────────────

/// Object module parse error.
#[derive(Debug, Clone, thiserror::Error)]
pub enum ParseError {
    /// Invalid record type.
    #[error("INVALID RECORD TYPE AT OFFSET {offset}")]
    InvalidRecordType { offset: usize },
    /// Truncated record.
    #[error("TRUNCATED RECORD — EXPECTED {expected} BYTES, GOT {actual}")]
    TruncatedRecord { expected: usize, actual: usize },
    /// Invalid ESD entry.
    #[error("INVALID ESD ENTRY — {reason}")]
    InvalidEsd { reason: String },
    /// Missing END record.
    #[error("MISSING END RECORD")]
    MissingEnd,
}

/// A raw object record (before full parsing).
#[derive(Debug, Clone)]
pub struct RawRecord {
    /// Record type.
    pub record_type: RecordType,
    /// Raw data.
    pub data: Vec<u8>,
}

/// Parse ESD type code to EsdType.
fn parse_esd_type(code: u8) -> Option<EsdType> {
    match code {
        0x00 => Some(EsdType::SectionDef),
        0x01 => Some(EsdType::LabelDef),
        0x02 => Some(EsdType::ExternalRef),
        0x0A => Some(EsdType::WeakExternalRef),
        _ => None,
    }
}

/// Serialize EsdType to code byte.
fn esd_type_code(t: EsdType) -> u8 {
    match t {
        EsdType::SectionDef => 0x00,
        EsdType::LabelDef => 0x01,
        EsdType::ExternalRef => 0x02,
        EsdType::WeakExternalRef => 0x0A,
    }
}

/// Parse a symbol name from EBCDIC-like bytes (we use ASCII for simplicity).
fn parse_symbol_name(bytes: &[u8]) -> String {
    let s = std::str::from_utf8(bytes).unwrap_or("");
    s.trim_end().to_string()
}

/// Pad symbol name to 8 bytes.
fn pad_symbol_name(name: &str) -> [u8; 8] {
    let mut buf = [b' '; 8];
    let bytes = name.as_bytes();
    let len = bytes.len().min(8);
    buf[..len].copy_from_slice(&bytes[..len]);
    buf
}

/// Parse an ESD record's data into ESD entries.
///
/// Each ESD item is 16 bytes:
/// - Bytes 0-7: Symbol name (8 chars)
/// - Byte 8: Type code
/// - Bytes 9-11: Address (3 bytes)
/// - Byte 12: Alignment/flags
/// - Bytes 13-15: Length (3 bytes, for SD) / ESDID (for ER)
pub fn parse_esd_record(data: &[u8], base_esdid: &mut u16) -> Result<Vec<EsdEntry>, ParseError> {
    let mut entries = Vec::new();

    if data.len() < 16 {
        return Ok(entries);
    }

    let mut pos = 0;
    while pos + 16 <= data.len() {
        let name = parse_symbol_name(&data[pos..pos + 8]);
        let type_code = data[pos + 8];

        let esd_type = parse_esd_type(type_code).ok_or_else(|| ParseError::InvalidEsd {
            reason: format!("unknown type code {type_code:#04X}"),
        })?;

        let offset = u32::from_be_bytes([0, data[pos + 9], data[pos + 10], data[pos + 11]]);
        let length = u32::from_be_bytes([0, data[pos + 13], data[pos + 14], data[pos + 15]]);

        let esdid = *base_esdid;
        *base_esdid += 1;

        entries.push(EsdEntry {
            name,
            esd_type,
            esdid,
            offset,
            length,
        });

        pos += 16;
    }

    Ok(entries)
}

/// Serialize ESD entries into binary data.
pub fn serialize_esd_entries(entries: &[EsdEntry]) -> Vec<u8> {
    let mut data = Vec::with_capacity(entries.len() * 16);

    for e in entries {
        data.extend_from_slice(&pad_symbol_name(&e.name));
        data.push(esd_type_code(e.esd_type));
        let offset_bytes = e.offset.to_be_bytes();
        data.extend_from_slice(&offset_bytes[1..4]);
        data.push(0x00); // alignment/flags
        let length_bytes = e.length.to_be_bytes();
        data.extend_from_slice(&length_bytes[1..4]);
    }

    data
}

/// Parse a TXT record.
///
/// - Bytes 0-2: Address (3 bytes) — offset in section
/// - Bytes 3-4: Reserved
/// - Bytes 5-6: Byte count
/// - Bytes 7-8: ESDID
/// - Bytes 9+: Data
pub fn parse_txt_record(data: &[u8]) -> Result<TextRecord, ParseError> {
    if data.len() < 9 {
        return Err(ParseError::TruncatedRecord {
            expected: 9,
            actual: data.len(),
        });
    }

    let offset = u32::from_be_bytes([0, data[0], data[1], data[2]]);
    let byte_count = u16::from_be_bytes([data[5], data[6]]) as usize;
    let esdid = u16::from_be_bytes([data[7], data[8]]);

    let text_start = 9;
    let text_end = (text_start + byte_count).min(data.len());
    let text_data = data[text_start..text_end].to_vec();

    Ok(TextRecord {
        esdid,
        offset,
        data: text_data,
    })
}

/// Serialize a TXT record.
pub fn serialize_txt_record(txt: &TextRecord) -> Vec<u8> {
    let mut data = Vec::new();

    let offset_bytes = txt.offset.to_be_bytes();
    data.extend_from_slice(&offset_bytes[1..4]);
    data.extend_from_slice(&[0x00, 0x00]); // reserved
    let count = txt.data.len() as u16;
    data.extend_from_slice(&count.to_be_bytes());
    data.extend_from_slice(&txt.esdid.to_be_bytes());
    data.extend_from_slice(&txt.data);

    data
}

/// Parse an RLD record.
///
/// Each RLD item is 8 bytes:
/// - Bytes 0-1: Relocation ESDID (target)
/// - Bytes 2-3: Position ESDID (where adcon lives)
/// - Byte 4: Flags (adcon type + length)
/// - Bytes 5-7: Address (offset of adcon)
pub fn parse_rld_record(data: &[u8]) -> Result<Vec<RldEntry>, ParseError> {
    let mut entries = Vec::new();
    let mut pos = 0;

    while pos + 8 <= data.len() {
        let r_esdid = u16::from_be_bytes([data[pos], data[pos + 1]]);
        let p_esdid = u16::from_be_bytes([data[pos + 2], data[pos + 3]]);
        let flags = data[pos + 4];
        let offset = u32::from_be_bytes([0, data[pos + 5], data[pos + 6], data[pos + 7]]);

        let adcon_type = if flags & 0x80 != 0 {
            AdconType::VType
        } else {
            AdconType::AType
        };

        let adcon_len = ((flags >> 4) & 0x03) + 1; // 0→1, 1→2, 2→3, 3→4

        entries.push(RldEntry {
            r_esdid,
            p_esdid,
            offset,
            adcon_type,
            adcon_len,
        });

        pos += 8;
    }

    Ok(entries)
}

/// Serialize RLD entries.
pub fn serialize_rld_entries(entries: &[RldEntry]) -> Vec<u8> {
    let mut data = Vec::new();

    for e in entries {
        data.extend_from_slice(&e.r_esdid.to_be_bytes());
        data.extend_from_slice(&e.p_esdid.to_be_bytes());

        let type_bit = if e.adcon_type == AdconType::VType {
            0x80
        } else {
            0x00
        };
        let len_bits = (e.adcon_len.saturating_sub(1) & 0x03) << 4;
        data.push(type_bit | len_bits);

        let offset_bytes = e.offset.to_be_bytes();
        data.extend_from_slice(&offset_bytes[1..4]);
    }

    data
}

/// Parse a complete object module from a sequence of raw records.
pub fn parse_object_module(
    name: &str,
    records: &[RawRecord],
) -> Result<ObjectModule, ParseError> {
    let mut esd_entries = Vec::new();
    let mut text_records = Vec::new();
    let mut rld_entries = Vec::new();
    let mut has_end = false;
    let mut next_esdid: u16 = 1;

    for record in records {
        match record.record_type {
            RecordType::Esd => {
                let entries = parse_esd_record(&record.data, &mut next_esdid)?;
                esd_entries.extend(entries);
            }
            RecordType::Txt => {
                let txt = parse_txt_record(&record.data)?;
                text_records.push(txt);
            }
            RecordType::Rld => {
                let entries = parse_rld_record(&record.data)?;
                rld_entries.extend(entries);
            }
            RecordType::End => {
                has_end = true;
            }
            RecordType::Sym => {
                // Ignored.
            }
        }
    }

    if !has_end {
        return Err(ParseError::MissingEnd);
    }

    Ok(ObjectModule {
        name: name.to_string(),
        esd_entries,
        text_records,
        rld_entries,
    })
}

// ─────────────────────── Load Module Format ───────────────────────

/// CESD entry (Composite External Symbol Dictionary) — used in load modules.
#[derive(Debug, Clone)]
pub struct CesdEntry {
    /// Symbol name.
    pub name: String,
    /// Type (section def, external ref, etc.).
    pub esd_type: EsdType,
    /// Address in the load module.
    pub address: u32,
    /// Length (for sections).
    pub length: u32,
}

/// A parsed load module (from disk format).
#[derive(Debug, Clone)]
pub struct ParsedLoadModule {
    /// Module name.
    pub name: String,
    /// CESD entries.
    pub cesd: Vec<CesdEntry>,
    /// Text segments.
    pub text_segments: Vec<LoadModuleText>,
    /// Entry point address.
    pub entry_point: u32,
    /// AMODE bits.
    pub amode_bits: u8,
    /// RMODE bit.
    pub rmode_any: bool,
    /// Aliases.
    pub aliases: Vec<String>,
}

/// A text segment in a load module.
#[derive(Debug, Clone)]
pub struct LoadModuleText {
    /// Load address.
    pub address: u32,
    /// Data.
    pub data: Vec<u8>,
}

/// Parse a load module from its binary representation.
///
/// Simplified format:
/// - Header (16 bytes): magic "LMOD", entry_point (4), amode (1), rmode (1), cesd_count (2), text_count (2), alias_count (2), reserved (2)
/// - CESD entries (24 bytes each): name (8), type (1), pad (3), address (4), length (4), pad (4)
/// - Text segments: address (4) + length (4) + data
/// - Aliases: length (1) + name bytes
pub fn parse_load_module(name: &str, data: &[u8]) -> Result<ParsedLoadModule, ParseError> {
    if data.len() < 16 {
        return Err(ParseError::TruncatedRecord {
            expected: 16,
            actual: data.len(),
        });
    }

    if &data[0..4] != b"LMOD" {
        return Err(ParseError::InvalidRecordType { offset: 0 });
    }

    let entry_point = u32::from_be_bytes(data[4..8].try_into().unwrap());
    let amode_bits = data[8];
    let rmode_any = data[9] != 0;
    let cesd_count = u16::from_be_bytes(data[10..12].try_into().unwrap()) as usize;
    let text_count = u16::from_be_bytes(data[12..14].try_into().unwrap()) as usize;
    let alias_count = u16::from_be_bytes(data[14..16].try_into().unwrap()) as usize;

    let mut pos = 16;

    // Parse CESD entries.
    let mut cesd = Vec::with_capacity(cesd_count);
    for _ in 0..cesd_count {
        if pos + 24 > data.len() {
            return Err(ParseError::TruncatedRecord {
                expected: pos + 24,
                actual: data.len(),
            });
        }

        let sym_name = parse_symbol_name(&data[pos..pos + 8]);
        let esd_type = parse_esd_type(data[pos + 8]).unwrap_or(EsdType::SectionDef);
        let address = u32::from_be_bytes(data[pos + 12..pos + 16].try_into().unwrap());
        let length = u32::from_be_bytes(data[pos + 16..pos + 20].try_into().unwrap());

        cesd.push(CesdEntry {
            name: sym_name,
            esd_type,
            address,
            length,
        });

        pos += 24;
    }

    // Parse text segments.
    let mut text_segments = Vec::with_capacity(text_count);
    for _ in 0..text_count {
        if pos + 8 > data.len() {
            return Err(ParseError::TruncatedRecord {
                expected: pos + 8,
                actual: data.len(),
            });
        }

        let address = u32::from_be_bytes(data[pos..pos + 4].try_into().unwrap());
        let length = u32::from_be_bytes(data[pos + 4..pos + 8].try_into().unwrap()) as usize;
        pos += 8;

        if pos + length > data.len() {
            return Err(ParseError::TruncatedRecord {
                expected: pos + length,
                actual: data.len(),
            });
        }

        text_segments.push(LoadModuleText {
            address,
            data: data[pos..pos + length].to_vec(),
        });
        pos += length;
    }

    // Parse aliases.
    let mut aliases = Vec::with_capacity(alias_count);
    for _ in 0..alias_count {
        if pos >= data.len() {
            break;
        }
        let len = data[pos] as usize;
        pos += 1;
        if pos + len > data.len() {
            break;
        }
        let alias_name = parse_symbol_name(&data[pos..pos + len]);
        aliases.push(alias_name);
        pos += len;
    }

    Ok(ParsedLoadModule {
        name: name.to_string(),
        cesd,
        text_segments,
        entry_point,
        amode_bits,
        rmode_any,
        aliases,
    })
}

/// Serialize a load module to binary format.
pub fn serialize_load_module(module: &ParsedLoadModule) -> Vec<u8> {
    let mut data = Vec::new();

    // Header.
    data.extend_from_slice(b"LMOD");
    data.extend_from_slice(&module.entry_point.to_be_bytes());
    data.push(module.amode_bits);
    data.push(if module.rmode_any { 1 } else { 0 });
    data.extend_from_slice(&(module.cesd.len() as u16).to_be_bytes());
    data.extend_from_slice(&(module.text_segments.len() as u16).to_be_bytes());
    data.extend_from_slice(&(module.aliases.len() as u16).to_be_bytes());

    // CESD entries.
    for entry in &module.cesd {
        data.extend_from_slice(&pad_symbol_name(&entry.name));
        data.push(esd_type_code(entry.esd_type));
        data.extend_from_slice(&[0x00, 0x00, 0x00]); // pad
        data.extend_from_slice(&entry.address.to_be_bytes());
        data.extend_from_slice(&entry.length.to_be_bytes());
        data.extend_from_slice(&[0x00, 0x00, 0x00, 0x00]); // pad
    }

    // Text segments.
    for seg in &module.text_segments {
        data.extend_from_slice(&seg.address.to_be_bytes());
        data.extend_from_slice(&(seg.data.len() as u32).to_be_bytes());
        data.extend_from_slice(&seg.data);
    }

    // Aliases.
    for alias in &module.aliases {
        let bytes = alias.as_bytes();
        data.push(bytes.len() as u8);
        data.extend_from_slice(bytes);
    }

    data
}

/// Convert a ParsedLoadModule into a binder LoadModule for execution.
pub fn to_load_module(parsed: &ParsedLoadModule) -> crate::binder::LoadModule {
    // Combine all text segments into contiguous memory.
    let max_addr = parsed
        .text_segments
        .iter()
        .map(|s| s.address as usize + s.data.len())
        .max()
        .unwrap_or(0);

    let mut text = vec![0u8; max_addr];
    for seg in &parsed.text_segments {
        let start = seg.address as usize;
        let end = start + seg.data.len();
        if end <= text.len() {
            text[start..end].copy_from_slice(&seg.data);
        }
    }

    let mut symbols = HashMap::new();
    for entry in &parsed.cesd {
        symbols.insert(
            entry.name.clone(),
            crate::binder::ResolvedSymbol {
                address: entry.address,
                module: parsed.name.clone(),
                is_section: entry.esd_type == EsdType::SectionDef,
            },
        );
    }

    crate::binder::LoadModule {
        name: parsed.name.clone(),
        entry_point: parsed.entry_point,
        text,
        symbols,
        aliases: parsed.aliases.clone(),
    }
}

// ─────────────────────── Tests ───────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    // ─── SYS-105.1: Object Module Parser ───

    #[test]
    fn test_record_type_identification() {
        assert_eq!(RecordType::from_bytes(b"ESD"), Some(RecordType::Esd));
        assert_eq!(RecordType::from_bytes(b"TXT"), Some(RecordType::Txt));
        assert_eq!(RecordType::from_bytes(b"RLD"), Some(RecordType::Rld));
        assert_eq!(RecordType::from_bytes(b"END"), Some(RecordType::End));
        assert_eq!(RecordType::from_bytes(b"SYM"), Some(RecordType::Sym));
        assert_eq!(RecordType::from_bytes(b"XXX"), None);
    }

    #[test]
    fn test_parse_esd_record() {
        let mut data = Vec::new();
        // Symbol: "MAIN    " (8 bytes)
        data.extend_from_slice(b"MAIN    ");
        // Type: SD (0x00)
        data.push(0x00);
        // Address: 0x000000
        data.extend_from_slice(&[0x00, 0x00, 0x00]);
        // Alignment flag
        data.push(0x00);
        // Length: 0x000010 = 16
        data.extend_from_slice(&[0x00, 0x00, 0x10]);

        let mut esdid = 1;
        let entries = parse_esd_record(&data, &mut esdid).unwrap();
        assert_eq!(entries.len(), 1);
        assert_eq!(entries[0].name, "MAIN");
        assert_eq!(entries[0].esd_type, EsdType::SectionDef);
        assert_eq!(entries[0].length, 16);
        assert_eq!(entries[0].esdid, 1);
        assert_eq!(esdid, 2);
    }

    #[test]
    fn test_esd_roundtrip() {
        let entries = vec![
            EsdEntry {
                name: "CSECT1".into(),
                esd_type: EsdType::SectionDef,
                esdid: 1,
                offset: 0,
                length: 256,
            },
            EsdEntry {
                name: "EXTERN".into(),
                esd_type: EsdType::ExternalRef,
                esdid: 2,
                offset: 0,
                length: 0,
            },
        ];

        let data = serialize_esd_entries(&entries);
        let mut esdid = 1;
        let parsed = parse_esd_record(&data, &mut esdid).unwrap();

        assert_eq!(parsed.len(), 2);
        assert_eq!(parsed[0].name, "CSECT1");
        assert_eq!(parsed[0].esd_type, EsdType::SectionDef);
        assert_eq!(parsed[0].length, 256);
        assert_eq!(parsed[1].name, "EXTERN");
        assert_eq!(parsed[1].esd_type, EsdType::ExternalRef);
    }

    // ─── SYS-105.2: Text Record Processing ───

    #[test]
    fn test_parse_txt_record() {
        let mut data = Vec::new();
        // Address: 0x000008
        data.extend_from_slice(&[0x00, 0x00, 0x08]);
        // Reserved
        data.extend_from_slice(&[0x00, 0x00]);
        // Byte count: 4
        data.extend_from_slice(&[0x00, 0x04]);
        // ESDID: 1
        data.extend_from_slice(&[0x00, 0x01]);
        // Data: 4 bytes
        data.extend_from_slice(&[0x47, 0xF0, 0x00, 0x10]);

        let txt = parse_txt_record(&data).unwrap();
        assert_eq!(txt.offset, 8);
        assert_eq!(txt.esdid, 1);
        assert_eq!(txt.data, vec![0x47, 0xF0, 0x00, 0x10]);
    }

    #[test]
    fn test_txt_roundtrip() {
        let original = TextRecord {
            esdid: 3,
            offset: 0x100,
            data: vec![0xDE, 0xAD, 0xBE, 0xEF],
        };

        let serialized = serialize_txt_record(&original);
        let parsed = parse_txt_record(&serialized).unwrap();

        assert_eq!(parsed.esdid, original.esdid);
        assert_eq!(parsed.offset, original.offset);
        assert_eq!(parsed.data, original.data);
    }

    // ─── SYS-105.3: Relocation Dictionary Processing ───

    #[test]
    fn test_parse_rld_record() {
        let mut data = Vec::new();
        // R-ESDID: 2
        data.extend_from_slice(&[0x00, 0x02]);
        // P-ESDID: 1
        data.extend_from_slice(&[0x00, 0x01]);
        // Flags: V-type, 4-byte (0x80 | 0x30 = 0xB0)
        data.push(0xB0);
        // Offset: 0x000004
        data.extend_from_slice(&[0x00, 0x00, 0x04]);

        let entries = parse_rld_record(&data).unwrap();
        assert_eq!(entries.len(), 1);
        assert_eq!(entries[0].r_esdid, 2);
        assert_eq!(entries[0].p_esdid, 1);
        assert_eq!(entries[0].adcon_type, AdconType::VType);
        assert_eq!(entries[0].adcon_len, 4);
        assert_eq!(entries[0].offset, 4);
    }

    #[test]
    fn test_rld_atype_3byte() {
        let mut data = Vec::new();
        data.extend_from_slice(&[0x00, 0x01]); // R-ESDID
        data.extend_from_slice(&[0x00, 0x01]); // P-ESDID
        data.push(0x20); // A-type, 3-byte (len bits = 0x20 → (2) → 2+1=3)
        data.extend_from_slice(&[0x00, 0x00, 0x10]); // Offset

        let entries = parse_rld_record(&data).unwrap();
        assert_eq!(entries[0].adcon_type, AdconType::AType);
        assert_eq!(entries[0].adcon_len, 3);
    }

    #[test]
    fn test_rld_roundtrip() {
        let entries = vec![RldEntry {
            r_esdid: 2,
            p_esdid: 1,
            offset: 0x100,
            adcon_type: AdconType::VType,
            adcon_len: 4,
        }];

        let data = serialize_rld_entries(&entries);
        let parsed = parse_rld_record(&data).unwrap();

        assert_eq!(parsed.len(), 1);
        assert_eq!(parsed[0].r_esdid, 2);
        assert_eq!(parsed[0].p_esdid, 1);
        assert_eq!(parsed[0].adcon_type, AdconType::VType);
        assert_eq!(parsed[0].adcon_len, 4);
        assert_eq!(parsed[0].offset, 0x100);
    }

    // ─── SYS-105.4: Load Module Format Parser ───

    #[test]
    fn test_load_module_roundtrip() {
        let original = ParsedLoadModule {
            name: "TESTMOD".into(),
            cesd: vec![CesdEntry {
                name: "MAIN".into(),
                esd_type: EsdType::SectionDef,
                address: 0,
                length: 8,
            }],
            text_segments: vec![LoadModuleText {
                address: 0,
                data: vec![0x47, 0xF0, 0x00, 0x08, 0x07, 0xFE, 0x00, 0x00],
            }],
            entry_point: 0,
            amode_bits: 31,
            rmode_any: true,
            aliases: vec!["ALIAS1".into()],
        };

        let serialized = serialize_load_module(&original);
        let parsed = parse_load_module("TESTMOD", &serialized).unwrap();

        assert_eq!(parsed.name, "TESTMOD");
        assert_eq!(parsed.entry_point, 0);
        assert_eq!(parsed.amode_bits, 31);
        assert!(parsed.rmode_any);
        assert_eq!(parsed.cesd.len(), 1);
        assert_eq!(parsed.cesd[0].name, "MAIN");
        assert_eq!(parsed.text_segments.len(), 1);
        assert_eq!(parsed.text_segments[0].data.len(), 8);
        assert_eq!(parsed.aliases, vec!["ALIAS1"]);
    }

    #[test]
    fn test_load_module_invalid_magic() {
        let data = vec![0x00; 16];
        let result = parse_load_module("BAD", &data);
        assert!(result.is_err());
    }

    // ─── SYS-105.5: ALIAS Entry Points ───

    #[test]
    fn test_aliases_in_load_module() {
        let module = ParsedLoadModule {
            name: "MYPROG".into(),
            cesd: Vec::new(),
            text_segments: vec![LoadModuleText {
                address: 0,
                data: vec![0x07, 0xFE],
            }],
            entry_point: 0,
            amode_bits: 31,
            rmode_any: false,
            aliases: vec!["ALIAS1".into(), "ALIAS2".into(), "ALIAS3".into()],
        };

        let serialized = serialize_load_module(&module);
        let parsed = parse_load_module("MYPROG", &serialized).unwrap();
        assert_eq!(parsed.aliases.len(), 3);
        assert_eq!(parsed.aliases[0], "ALIAS1");
        assert_eq!(parsed.aliases[1], "ALIAS2");
        assert_eq!(parsed.aliases[2], "ALIAS3");
    }

    #[test]
    fn test_to_load_module_conversion() {
        let parsed = ParsedLoadModule {
            name: "CONV".into(),
            cesd: vec![CesdEntry {
                name: "MAIN".into(),
                esd_type: EsdType::SectionDef,
                address: 0,
                length: 4,
            }],
            text_segments: vec![LoadModuleText {
                address: 0,
                data: vec![0x07, 0xFE, 0x00, 0x00],
            }],
            entry_point: 0,
            amode_bits: 31,
            rmode_any: true,
            aliases: vec!["A1".into()],
        };

        let lm = to_load_module(&parsed);
        assert_eq!(lm.name, "CONV");
        assert_eq!(lm.entry_point, 0);
        assert_eq!(lm.text, vec![0x07, 0xFE, 0x00, 0x00]);
        assert!(lm.symbols.contains_key("MAIN"));
        assert_eq!(lm.aliases, vec!["A1"]);
    }

    // ─── SYS-105.6: Integration Tests ───

    #[test]
    fn test_full_object_module_parse() {
        let records = vec![
            RawRecord {
                record_type: RecordType::Esd,
                data: serialize_esd_entries(&[
                    EsdEntry {
                        name: "MAIN".into(),
                        esd_type: EsdType::SectionDef,
                        esdid: 1,
                        offset: 0,
                        length: 8,
                    },
                    EsdEntry {
                        name: "SUB".into(),
                        esd_type: EsdType::ExternalRef,
                        esdid: 2,
                        offset: 0,
                        length: 0,
                    },
                ]),
            },
            RawRecord {
                record_type: RecordType::Txt,
                data: serialize_txt_record(&TextRecord {
                    esdid: 1,
                    offset: 0,
                    data: vec![0x58, 0x10, 0xF0, 0x04, 0x00, 0x00, 0x00, 0x00],
                }),
            },
            RawRecord {
                record_type: RecordType::Rld,
                data: serialize_rld_entries(&[RldEntry {
                    r_esdid: 2,
                    p_esdid: 1,
                    offset: 4,
                    adcon_type: AdconType::VType,
                    adcon_len: 4,
                }]),
            },
            RawRecord {
                record_type: RecordType::End,
                data: Vec::new(),
            },
        ];

        let module = parse_object_module("TESTOBJ", &records).unwrap();
        assert_eq!(module.name, "TESTOBJ");
        assert_eq!(module.esd_entries.len(), 2);
        assert_eq!(module.text_records.len(), 1);
        assert_eq!(module.rld_entries.len(), 1);
    }

    #[test]
    fn test_missing_end_record() {
        let records = vec![RawRecord {
            record_type: RecordType::Esd,
            data: serialize_esd_entries(&[EsdEntry {
                name: "X".into(),
                esd_type: EsdType::SectionDef,
                esdid: 1,
                offset: 0,
                length: 4,
            }]),
        }];

        let result = parse_object_module("NOEND", &records);
        assert!(matches!(result, Err(ParseError::MissingEnd)));
    }

    #[test]
    fn test_parse_bind_execute_roundtrip() {
        use crate::binder::Binder;

        // Create object records.
        let records = vec![
            RawRecord {
                record_type: RecordType::Esd,
                data: serialize_esd_entries(&[EsdEntry {
                    name: "HELLO".into(),
                    esd_type: EsdType::SectionDef,
                    esdid: 1,
                    offset: 0,
                    length: 4,
                }]),
            },
            RawRecord {
                record_type: RecordType::Txt,
                data: serialize_txt_record(&TextRecord {
                    esdid: 1,
                    offset: 0,
                    data: vec![0x07, 0xFE, 0x00, 0x00],
                }),
            },
            RawRecord {
                record_type: RecordType::End,
                data: Vec::new(),
            },
        ];

        // Parse.
        let obj = parse_object_module("HELLO", &records).unwrap();
        assert_eq!(obj.esd_entries.len(), 1);

        // Bind.
        let mut binder = Binder::new("HELLO");
        binder.add_module(obj);
        binder.set_entry("HELLO");
        let load_mod = binder.bind().unwrap();

        assert_eq!(load_mod.text[..2], [0x07, 0xFE]);
    }

    #[test]
    fn test_multiple_text_segments() {
        let module = ParsedLoadModule {
            name: "MULTI".into(),
            cesd: vec![
                CesdEntry { name: "SEG1".into(), esd_type: EsdType::SectionDef, address: 0, length: 4 },
                CesdEntry { name: "SEG2".into(), esd_type: EsdType::SectionDef, address: 8, length: 4 },
            ],
            text_segments: vec![
                LoadModuleText { address: 0, data: vec![0x11, 0x22, 0x33, 0x44] },
                LoadModuleText { address: 8, data: vec![0x55, 0x66, 0x77, 0x88] },
            ],
            entry_point: 0,
            amode_bits: 31,
            rmode_any: true,
            aliases: Vec::new(),
        };

        let lm = to_load_module(&module);
        assert_eq!(lm.text.len(), 12);
        assert_eq!(&lm.text[0..4], &[0x11, 0x22, 0x33, 0x44]);
        assert_eq!(&lm.text[8..12], &[0x55, 0x66, 0x77, 0x88]);
    }
}
