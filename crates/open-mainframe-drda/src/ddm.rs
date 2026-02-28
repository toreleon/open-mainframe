//! DDM (Distributed Data Management) object parsing and building.
//!
//! DDM objects are the fundamental data units within DRDA DSS segments.
//! They follow a TLV (Type-Length-Value) format:
//!
//! ```text
//! Offset  Len  Field
//! 0       2    Total length (big-endian, includes the 4-byte header)
//! 2       2    Code point (big-endian, identifies the object type)
//! 4       N    Payload (may contain nested DDM parameters)
//! ```

use bytes::{Buf, BufMut, BytesMut};

use crate::error::{DrdaError, DrdaResult};

/// Minimum DDM header length.
pub const DDM_HEADER_LEN: usize = 4;

/// A parsed DDM object.
#[derive(Debug, Clone)]
pub struct DdmObject {
    /// Code point identifying this DDM object type.
    pub code_point: u16,
    /// Raw payload bytes (may contain nested DDM parameters).
    pub payload: Vec<u8>,
}

impl DdmObject {
    /// Create a new DDM object with the given code point and payload.
    pub fn new(code_point: u16, payload: Vec<u8>) -> Self {
        Self {
            code_point,
            payload,
        }
    }

    /// Create an empty DDM object (header only, no payload).
    pub fn empty(code_point: u16) -> Self {
        Self {
            code_point,
            payload: Vec::new(),
        }
    }

    /// Parse nested DDM parameters from this object's payload.
    pub fn parse_params(&self) -> DrdaResult<Vec<DdmObject>> {
        parse_ddm_params(&self.payload)
    }

    /// Find a parameter by code point within this object's payload.
    pub fn find_param(&self, code_point: u16) -> DrdaResult<Option<DdmObject>> {
        let params = self.parse_params()?;
        Ok(params.into_iter().find(|p| p.code_point == code_point))
    }

    /// Get a parameter's payload as a UTF-8 string.
    pub fn get_string_param(&self, code_point: u16) -> DrdaResult<Option<String>> {
        if let Some(param) = self.find_param(code_point)? {
            let s = String::from_utf8_lossy(&param.payload).to_string();
            Ok(Some(s.trim_end_matches('\0').trim().to_string()))
        } else {
            Ok(None)
        }
    }

    /// Get a parameter's raw payload bytes.
    pub fn get_raw_param(&self, code_point: u16) -> DrdaResult<Option<Vec<u8>>> {
        if let Some(param) = self.find_param(code_point)? {
            Ok(Some(param.payload))
        } else {
            Ok(None)
        }
    }

    /// Get all parameters with the given code point (for repeated params like SECTKN).
    pub fn get_all_raw_params(&self, code_point: u16) -> DrdaResult<Vec<Vec<u8>>> {
        let params = self.parse_params()?;
        Ok(params
            .into_iter()
            .filter(|p| p.code_point == code_point)
            .map(|p| p.payload)
            .collect())
    }

    /// Get a parameter's payload as a u16 value.
    pub fn get_u16_param(&self, code_point: u16) -> DrdaResult<Option<u16>> {
        if let Some(param) = self.find_param(code_point)? {
            if param.payload.len() >= 2 {
                let val = u16::from_be_bytes([param.payload[0], param.payload[1]]);
                Ok(Some(val))
            } else {
                Ok(None)
            }
        } else {
            Ok(None)
        }
    }

    /// Serialize this DDM object to bytes.
    pub fn serialize(&self) -> BytesMut {
        let total_len = DDM_HEADER_LEN + self.payload.len();
        let mut buf = BytesMut::with_capacity(total_len);
        buf.put_u16(total_len as u16);
        buf.put_u16(self.code_point);
        buf.extend_from_slice(&self.payload);
        buf
    }

    /// Total serialized length including header.
    pub fn total_len(&self) -> usize {
        DDM_HEADER_LEN + self.payload.len()
    }
}

/// Parse the first DDM object from a byte slice.
/// Returns the object and the number of bytes consumed.
pub fn parse_ddm(data: &[u8]) -> DrdaResult<(DdmObject, usize)> {
    if data.len() < DDM_HEADER_LEN {
        return Err(DrdaError::InvalidDdm(format!(
            "DDM data too short: {} bytes, need at least {}",
            data.len(),
            DDM_HEADER_LEN
        )));
    }

    let mut cursor = &data[..];
    let length = cursor.get_u16() as usize;
    let code_point = cursor.get_u16();

    if length < DDM_HEADER_LEN {
        return Err(DrdaError::InvalidDdm(format!(
            "DDM length {} is less than header size {}",
            length, DDM_HEADER_LEN
        )));
    }

    if length > data.len() {
        return Err(DrdaError::InvalidDdm(format!(
            "DDM length {} exceeds available data {}",
            length,
            data.len()
        )));
    }

    let payload_len = length - DDM_HEADER_LEN;
    let payload = data[DDM_HEADER_LEN..DDM_HEADER_LEN + payload_len].to_vec();

    Ok((DdmObject { code_point, payload }, length))
}

/// Parse all DDM objects from a byte slice (e.g., DSS payload or nested params).
pub fn parse_ddm_list(data: &[u8]) -> DrdaResult<Vec<DdmObject>> {
    let mut objects = Vec::new();
    let mut offset = 0;

    while offset < data.len() {
        if data.len() - offset < DDM_HEADER_LEN {
            break;
        }
        let (obj, consumed) = parse_ddm(&data[offset..])?;
        objects.push(obj);
        offset += consumed;
    }

    Ok(objects)
}

/// Parse nested DDM parameters from a payload.
/// Same as parse_ddm_list but with a clearer name for nested usage.
pub fn parse_ddm_params(data: &[u8]) -> DrdaResult<Vec<DdmObject>> {
    parse_ddm_list(data)
}

/// Builder for constructing DDM objects with nested parameters.
pub struct DdmBuilder {
    code_point: u16,
    payload: BytesMut,
}

impl DdmBuilder {
    /// Create a new builder for the given code point.
    pub fn new(code_point: u16) -> Self {
        Self {
            code_point,
            payload: BytesMut::new(),
        }
    }

    /// Add a nested DDM parameter with raw bytes.
    pub fn add_param(mut self, code_point: u16, data: &[u8]) -> Self {
        let len = (DDM_HEADER_LEN + data.len()) as u16;
        self.payload.put_u16(len);
        self.payload.put_u16(code_point);
        self.payload.extend_from_slice(data);
        self
    }

    /// Add a nested DDM parameter with a string value (UTF-8/ASCII).
    pub fn add_string_param(self, code_point: u16, value: &str) -> Self {
        self.add_param(code_point, value.as_bytes())
    }

    /// Add a nested DDM parameter with a string value encoded in EBCDIC cp500.
    /// DRDA handshake messages (EXCSAT/EXCSATRD) require EBCDIC encoding.
    pub fn add_ebcdic_param(self, code_point: u16, value: &str) -> Self {
        let ebcdic_bytes: Vec<u8> = value.bytes().map(ascii_to_ebcdic_cp500).collect();
        self.add_param(code_point, &ebcdic_bytes)
    }

    /// Add a nested DDM parameter with a u16 value.
    pub fn add_u16_param(self, code_point: u16, value: u16) -> Self {
        self.add_param(code_point, &value.to_be_bytes())
    }

    /// Add a nested DDM parameter with a u32 value.
    pub fn add_u32_param(self, code_point: u16, value: u32) -> Self {
        self.add_param(code_point, &value.to_be_bytes())
    }

    /// Add a pre-built DDM object as a nested parameter.
    pub fn add_object(mut self, obj: &DdmObject) -> Self {
        let serialized = obj.serialize();
        self.payload.extend_from_slice(&serialized);
        self
    }

    /// Add raw bytes directly to the payload (no TLV wrapping).
    pub fn add_raw(mut self, data: &[u8]) -> Self {
        self.payload.extend_from_slice(data);
        self
    }

    /// Build the final DDM object.
    pub fn build(self) -> DdmObject {
        DdmObject {
            code_point: self.code_point,
            payload: self.payload.to_vec(),
        }
    }

    /// Build and serialize to bytes.
    pub fn build_bytes(self) -> BytesMut {
        self.build().serialize()
    }
}

/// Convert a single ASCII byte to EBCDIC cp500.
/// Used for encoding strings in DRDA handshake messages.
pub fn ascii_to_ebcdic_cp500(b: u8) -> u8 {
    match b {
        b' ' => 0x40,
        b'.' => 0x4B, b'<' => 0x4C, b'(' => 0x4D, b'+' => 0x4E,
        b'&' => 0x50, b'!' => 0x5A, b'$' => 0x5B, b'*' => 0x5C,
        b')' => 0x5D, b';' => 0x5E, b'-' => 0x60, b'/' => 0x61,
        b',' => 0x6B, b'%' => 0x6C, b'_' => 0x6D, b'>' => 0x6E,
        b'?' => 0x6F, b':' => 0x7A, b'#' => 0x7B, b'@' => 0x7C,
        b'\'' => 0x7D, b'=' => 0x7E, b'"' => 0x7F,
        // Lowercase a-i
        b'a'..=b'i' => 0x81 + (b - b'a'),
        // Lowercase j-r
        b'j'..=b'r' => 0x91 + (b - b'j'),
        // Lowercase s-z
        b's'..=b'z' => 0xA2 + (b - b's'),
        // Uppercase A-I
        b'A'..=b'I' => 0xC1 + (b - b'A'),
        // Uppercase J-R
        b'J'..=b'R' => 0xD1 + (b - b'J'),
        // Uppercase S-Z
        b'S'..=b'Z' => 0xE2 + (b - b'S'),
        // Digits 0-9
        b'0'..=b'9' => 0xF0 + (b - b'0'),
        // Null
        0x00 => 0x00,
        // Default: pass through
        _ => b,
    }
}
