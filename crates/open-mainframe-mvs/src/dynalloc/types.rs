//! DYNALLOC type definitions — SVC 99 request/response structures.

use serde::{Deserialize, Serialize};

/// SVC 99 verb codes.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[repr(u8)]
pub enum DynallocVerb {
    /// Allocate a dataset to a DDname.
    Allocate = 0x01,
    /// Unallocate (free) a DDname.
    Unallocate = 0x02,
    /// Concatenate multiple datasets under one DDname.
    Concatenate = 0x03,
    /// Break a concatenation into individual DD entries.
    Deconcatenate = 0x04,
    /// Remove in-use attribute.
    RemoveInUse = 0x05,
    /// Mark allocation as non-permanent.
    MarkNonPerm = 0x06,
    /// Information retrieval.
    InfoRetrieval = 0x07,
}

/// Text unit keys (IBM-defined numeric values).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[repr(u16)]
pub enum TextUnitKey {
    /// Dataset name.
    DalDsnam = 0x0001,
    /// DD name.
    DalDdnam = 0x0002,
    /// Member name.
    DalMembr = 0x0003,
    /// Dataset status (OLD/NEW/SHR/MOD).
    DalStats = 0x0004,
    /// Normal disposition.
    DalNdisp = 0x0005,
    /// Conditional disposition.
    DalCdisp = 0x0006,
    /// Space type (TRK/CYL/BLK).
    DalSpace = 0x000A,
    /// Primary allocation.
    DalPrime = 0x000B,
    /// Secondary allocation.
    DalSecnd = 0x000C,
    /// Directory blocks (PDS).
    DalDir = 0x000D,
    /// Volume serial.
    DalVlser = 0x0010,
    /// Unit name.
    DalUnit = 0x0015,
    /// Block size.
    DalBlksz = 0x0030,
    /// Dataset organization.
    DalDsorg = 0x003C,
    /// Logical record length.
    DalLrecl = 0x0042,
    /// Record format.
    DalRecfm = 0x0049,
    /// Return DDname.
    DalRtddn = 0x0056,
    /// Return dataset name.
    DalRtdsn = 0x0057,
    /// Return dataset organization.
    DalRtorg = 0x0059,
}

/// A single text unit in the SVC 99 parameter list.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TextUnit {
    /// Text unit key identifying the parameter type.
    pub key: TextUnitKey,
    /// Parameter values (can be multi-valued).
    pub values: Vec<Vec<u8>>,
}

impl TextUnit {
    /// Create a text unit with a single string value.
    pub fn string(key: TextUnitKey, value: &str) -> Self {
        Self {
            key,
            values: vec![value.as_bytes().to_vec()],
        }
    }

    /// Create a text unit with a single byte value.
    pub fn byte(key: TextUnitKey, value: u8) -> Self {
        Self {
            key,
            values: vec![vec![value]],
        }
    }

    /// Get the first value as a UTF-8 string, if possible.
    pub fn first_string(&self) -> Option<String> {
        self.values
            .first()
            .and_then(|v| String::from_utf8(v.clone()).ok())
    }

    /// Get the first value as a single byte.
    pub fn first_byte(&self) -> Option<u8> {
        self.values.first().and_then(|v| v.first().copied())
    }
}

/// SVC 99 Request Block.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DynallocRequest {
    /// Verb code (allocate, unallocate, etc.).
    pub verb: DynallocVerb,
    /// Request flags.
    pub flags: u16,
    /// Text units carrying request parameters.
    pub text_units: Vec<TextUnit>,
}

impl DynallocRequest {
    /// Find the first text unit with the given key.
    pub fn find_text_unit(&self, key: TextUnitKey) -> Option<&TextUnit> {
        self.text_units.iter().find(|tu| tu.key == key)
    }

    /// Find all text units with the given key.
    pub fn find_all_text_units(&self, key: TextUnitKey) -> Vec<&TextUnit> {
        self.text_units.iter().filter(|tu| tu.key == key).collect()
    }
}

/// SVC 99 Response.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DynallocResponse {
    /// Error code (0 = success).
    pub error_code: u16,
    /// Informational reason code.
    pub info_code: u16,
    /// Return text units (e.g., assigned DDname).
    pub return_text_units: Vec<TextUnit>,
}

impl DynallocResponse {
    /// Create a successful response.
    pub fn success() -> Self {
        Self {
            error_code: 0,
            info_code: 0,
            return_text_units: Vec::new(),
        }
    }

    /// Create a successful response with return text units.
    pub fn success_with(text_units: Vec<TextUnit>) -> Self {
        Self {
            error_code: 0,
            info_code: 0,
            return_text_units: text_units,
        }
    }

    /// Create an error response.
    pub fn error(error_code: u16, info_code: u16) -> Self {
        Self {
            error_code,
            info_code,
            return_text_units: Vec::new(),
        }
    }

    /// Whether the request succeeded.
    pub fn is_success(&self) -> bool {
        self.error_code == 0
    }
}

/// Dataset status (DISP first sub-parameter).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum DatasetStatus {
    /// Existing dataset, exclusive access.
    Old,
    /// New dataset to be created.
    New,
    /// Existing dataset, shared access.
    Shr,
    /// Existing dataset, extend mode.
    Mod,
}

impl DatasetStatus {
    /// Decode from SVC 99 byte value.
    pub fn from_byte(b: u8) -> Option<Self> {
        match b {
            0x01 => Some(Self::Old),
            0x02 => Some(Self::Mod),
            0x04 => Some(Self::New),
            0x08 => Some(Self::Shr),
            _ => None,
        }
    }

    /// Encode to SVC 99 byte value.
    pub fn to_byte(self) -> u8 {
        match self {
            Self::Old => 0x01,
            Self::Mod => 0x02,
            Self::New => 0x04,
            Self::Shr => 0x08,
        }
    }
}

/// Disposition (DISP normal/conditional sub-parameters).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum Disposition {
    /// Keep the dataset.
    Keep,
    /// Delete the dataset.
    Delete,
    /// Catalog the dataset.
    Catlg,
    /// Uncatalog the dataset.
    Uncatlg,
    /// Pass to the next job step.
    Pass,
}

impl Disposition {
    /// Decode from SVC 99 byte value.
    pub fn from_byte(b: u8) -> Option<Self> {
        match b {
            0x01 => Some(Self::Delete),
            0x02 => Some(Self::Keep),
            0x04 => Some(Self::Catlg),
            0x08 => Some(Self::Uncatlg),
            0x10 => Some(Self::Pass),
            _ => None,
        }
    }
}

/// DD table entry — maps a DDname to one or more datasets.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DdEntry {
    /// The DD name (1-8 characters, uppercase).
    pub ddname: String,
    /// Primary dataset name.
    pub dsname: String,
    /// Dataset status.
    pub status: DatasetStatus,
    /// Normal disposition.
    pub disposition: Disposition,
    /// Concatenated dataset names (additional datasets after the primary).
    pub concatenation: Vec<String>,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn text_unit_string_roundtrip() {
        let tu = TextUnit::string(TextUnitKey::DalDsnam, "MY.DATASET");
        assert_eq!(tu.first_string().unwrap(), "MY.DATASET");
    }

    #[test]
    fn text_unit_byte_roundtrip() {
        let tu = TextUnit::byte(TextUnitKey::DalStats, 0x08);
        assert_eq!(tu.first_byte().unwrap(), 0x08);
    }

    #[test]
    fn dataset_status_byte_roundtrip() {
        for status in [DatasetStatus::Old, DatasetStatus::New, DatasetStatus::Shr, DatasetStatus::Mod] {
            let byte = status.to_byte();
            assert_eq!(DatasetStatus::from_byte(byte), Some(status));
        }
    }

    #[test]
    fn disposition_from_byte() {
        assert_eq!(Disposition::from_byte(0x01), Some(Disposition::Delete));
        assert_eq!(Disposition::from_byte(0x02), Some(Disposition::Keep));
        assert_eq!(Disposition::from_byte(0x04), Some(Disposition::Catlg));
        assert_eq!(Disposition::from_byte(0xFF), None);
    }

    #[test]
    fn dynalloc_response_success() {
        let resp = DynallocResponse::success();
        assert!(resp.is_success());
        assert_eq!(resp.error_code, 0);
    }

    #[test]
    fn dynalloc_response_error() {
        let resp = DynallocResponse::error(0x0438, 0x0002);
        assert!(!resp.is_success());
        assert_eq!(resp.error_code, 0x0438);
    }

    #[test]
    fn dynalloc_request_find_text_unit() {
        let req = DynallocRequest {
            verb: DynallocVerb::Allocate,
            flags: 0,
            text_units: vec![
                TextUnit::string(TextUnitKey::DalDsnam, "MY.DATA"),
                TextUnit::string(TextUnitKey::DalDdnam, "DD01"),
            ],
        };
        let tu = req.find_text_unit(TextUnitKey::DalDsnam).unwrap();
        assert_eq!(tu.first_string().unwrap(), "MY.DATA");
        assert!(req.find_text_unit(TextUnitKey::DalMembr).is_none());
    }

    #[test]
    fn dynalloc_verb_serialization() {
        let verb = DynallocVerb::Allocate;
        let json = serde_json::to_string(&verb).unwrap();
        let deserialized: DynallocVerb = serde_json::from_str(&json).unwrap();
        assert_eq!(verb, deserialized);
    }
}
