//! Machine instruction encoding for z/Architecture.
//!
//! Covers the core instruction formats:
//! - E (2-byte, no operands)
//! - I (2-byte, immediate)
//! - RR (2-byte, register-register)
//! - RRE (4-byte, extended register-register)
//! - RRF (4-byte, register-register with extra fields)
//! - RX (4-byte, register-indexed storage)
//! - RXY (6-byte, register-indexed long displacement)
//! - RS (4-byte, register-storage)
//! - RSY (6-byte, register-storage long displacement)
//! - RI (4-byte, register-immediate)
//! - RIE (6-byte, register-immediate extended)
//! - RIL (6-byte, register-immediate long)
//! - SI (4-byte, storage-immediate)
//! - SIY (6-byte, storage-immediate long displacement)
//! - SIL (6-byte, storage-immediate long)
//! - S (4-byte, storage operand only)
//! - SS (6-byte, storage-storage — 1 or 2 length variants)
//! - SSE (6-byte, storage-storage extended)
//! - SSF (6-byte, storage-storage with register)
//!
//! Each instruction mnemonic is mapped to its format and opcode, then encoded
//! from parsed operand fields into a byte vector.

use std::collections::HashMap;
use std::fmt;

// ---------------------------------------------------------------------------
//  Instruction format descriptors
// ---------------------------------------------------------------------------

/// The encoding format of a z/Architecture machine instruction.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
pub enum InsnFormat {
    /// 2-byte, no operands.
    E,
    /// 2-byte, immediate operand I2.
    I,
    /// 2-byte, R1-R2 register-register.
    RR,
    /// 4-byte, extended opcode RR.
    RRE,
    /// 4-byte, RRF-a: R1,R2,R3 (optional M4).
    RRFa,
    /// 4-byte, RRF-b: R1,R2,R3,M4.
    RRFb,
    /// 4-byte, RRF-c: R1,R2,M3.
    RRFc,
    /// 4-byte, R1,X2,B2,D2 register-indexed storage.
    RX,
    /// 4-byte, RXE — 6-byte extended indexed.
    RXE,
    /// 6-byte, R1,X2,B2,DH2,DL2 long displacement.
    RXY,
    /// 4-byte, R1,R3,B2,D2 register-storage.
    RS,
    /// 6-byte, R1,R3,B2,DH2,DL2 register-storage long displacement.
    RSY,
    /// 4-byte, R1,I2 register-immediate (16-bit).
    RI,
    /// 6-byte, register-immediate extended (multiple sub-formats).
    RIEa,
    /// RIE-b: R1,R2,M3,I4(16-bit relative).
    RIEb,
    /// RIE-c: R1,I2(8-bit),M3,I4(16-bit relative).
    RIEc,
    /// RIE-d: R1,I2(16-bit imm).
    RIEd,
    /// RIE-f: R1,R2,I3,I4,I5.
    RIEf,
    /// 6-byte, R1,I2(32-bit) register-immediate long.
    RIL,
    /// 4-byte, D1(B1),I2 storage-immediate.
    SI,
    /// 6-byte, D1(B1),I2 storage-immediate long displacement.
    SIY,
    /// 6-byte, D1(B1),I2(16-bit) storage-immediate long.
    SIL,
    /// 4-byte, D1(B1) storage operand only.
    S,
    /// 6-byte, D1(L,B1),D2(B2) storage-storage (one length).
    SSa,
    /// 6-byte, D1(L1,B1),D2(L2,B2) storage-storage (two lengths).
    SSb,
    /// 6-byte, D1(B1),D2(B2) storage-storage extended.
    SSE,
    /// 6-byte, D1(B1),D2(B2),R3 storage-storage with register.
    SSF,

    // ─── Vector facility formats (z13+) ───

    /// 6-byte, VRR-a: V1,V2,M3,M4,M5.
    VRRa,
    /// 6-byte, VRR-b: V1,V2,V3,M4,M5.
    VRRb,
    /// 6-byte, VRR-c: V1,V2,V3,M4,M5,M6.
    VRRc,
    /// 6-byte, VRR-d: V1,V2,V3,V4,M5.
    VRRd,
    /// 6-byte, VRR-e: V1,V2,V3,V4,M5,M6.
    VRRe,
    /// 6-byte, VRI-a: V1,I2,M3.
    VRIa,
    /// 6-byte, VRI-b: V1,I2,I3,M4.
    VRIb,
    /// 6-byte, VRI-c: V1,V3,I2,M4.
    VRIc,
    /// 6-byte, VRS-a: V1,V3,D2(B2),M4.
    VRSa,
    /// 6-byte, VRS-b: V1,R3,D2(B2),M4.
    VRSb,
    /// 6-byte, VRS-c: R1,V3,D2(B2),M4.
    VRSc,
    /// 6-byte, VRV: V1,V2,D2(B2),M3.
    VRV,
    /// 6-byte, VRX: V1,D2(X2,B2),M3.
    VRX,
    /// 6-byte, VSI: V1,D2(B2),I3.
    VSI,
}

impl InsnFormat {
    /// Instruction length in bytes.
    pub fn length(self) -> usize {
        match self {
            InsnFormat::E | InsnFormat::I | InsnFormat::RR => 2,
            InsnFormat::RRE | InsnFormat::RRFa | InsnFormat::RRFb | InsnFormat::RRFc
            | InsnFormat::RX | InsnFormat::RS | InsnFormat::RI | InsnFormat::SI
            | InsnFormat::S => 4,
            InsnFormat::RXE | InsnFormat::RXY | InsnFormat::RSY | InsnFormat::RIEa
            | InsnFormat::RIEb | InsnFormat::RIEc | InsnFormat::RIEd | InsnFormat::RIEf
            | InsnFormat::RIL | InsnFormat::SIY | InsnFormat::SIL | InsnFormat::SSa
            | InsnFormat::SSb | InsnFormat::SSE | InsnFormat::SSF
            | InsnFormat::VRRa | InsnFormat::VRRb | InsnFormat::VRRc
            | InsnFormat::VRRd | InsnFormat::VRRe
            | InsnFormat::VRIa | InsnFormat::VRIb | InsnFormat::VRIc
            | InsnFormat::VRSa | InsnFormat::VRSb | InsnFormat::VRSc
            | InsnFormat::VRV | InsnFormat::VRX | InsnFormat::VSI => 6,
        }
    }
}

// ---------------------------------------------------------------------------
//  Instruction definition
// ---------------------------------------------------------------------------

/// Definition of a machine instruction (opcode + format).
#[derive(Debug, Clone)]
pub struct InsnDef {
    /// Mnemonic (uppercase).
    pub mnemonic: String,
    /// Primary opcode byte(s). For 2-byte opcodes, byte 0 is high and byte 1 is low.
    pub opcode: u16,
    /// Whether this is a 2-byte opcode (bits in positions 0-7 and 40-47 of 6-byte insns).
    pub opcode2: Option<u8>,
    /// Instruction format.
    pub format: InsnFormat,
    /// Optional extended branch condition code mask (for mnemonics like BE, BNE, etc.).
    pub cond_mask: Option<u8>,
}

// ---------------------------------------------------------------------------
//  Operand fields for encoding
// ---------------------------------------------------------------------------

/// Operand fields parsed from source to pass to the encoder.
#[derive(Debug, Clone, Default)]
pub struct InsnOperands {
    /// Register fields.
    pub r1: u8,
    pub r2: u8,
    pub r3: u8,
    pub m3: u8,
    pub m4: u8,
    /// Index register.
    pub x2: u8,
    /// Base register.
    pub b1: u8,
    pub b2: u8,
    /// Displacement (12-bit or 20-bit depending on format).
    pub d1: u32,
    pub d2: u32,
    /// Immediate value (various widths).
    pub i2: i64,
    /// Length fields for SS.
    pub l1: u8,
    pub l2: u8,
    /// Single length for SSa.
    pub length: u8,
    /// Vector register fields (0-31).
    pub v1: u8,
    pub v2: u8,
    pub v3: u8,
    pub v4: u8,
    /// Mask fields for vector instructions.
    pub m3_v: u8,
    pub m4_v: u8,
    pub m5: u8,
    pub m6: u8,
    /// Immediate fields for VRI.
    pub i2_v: u16,
    pub i3_v: u8,
}

// ---------------------------------------------------------------------------
//  Encoding error
// ---------------------------------------------------------------------------

/// Error during instruction encoding.
#[derive(Debug, Clone, thiserror::Error)]
pub enum EncodeError {
    #[error("Unknown mnemonic: {0}")]
    UnknownMnemonic(String),
    #[error("Displacement out of range: {0}")]
    DisplacementRange(i64),
    #[error("Immediate out of range: {0}")]
    ImmediateRange(i64),
    #[error("Encoding error: {0}")]
    Other(String),
}

// ---------------------------------------------------------------------------
//  Instruction catalog
// ---------------------------------------------------------------------------

/// The instruction catalog holds definitions for all known mnemonics.
#[derive(Debug)]
pub struct InsnCatalog {
    instructions: HashMap<String, InsnDef>,
}

impl Default for InsnCatalog {
    fn default() -> Self {
        Self::new()
    }
}

impl InsnCatalog {
    /// Build the catalog with core z/Architecture instructions.
    pub fn new() -> Self {
        let mut catalog = Self {
            instructions: HashMap::new(),
        };
        catalog.register_rr_instructions();
        catalog.register_rre_instructions();
        catalog.register_rx_instructions();
        catalog.register_rxy_instructions();
        catalog.register_rs_instructions();
        catalog.register_rsy_instructions();
        catalog.register_ri_instructions();
        catalog.register_ril_instructions();
        catalog.register_si_instructions();
        catalog.register_siy_instructions();
        catalog.register_sil_instructions();
        catalog.register_ss_instructions();
        catalog.register_s_instructions();
        catalog.register_e_instructions();
        catalog.register_extended_branches();
        catalog.register_vector_instructions();
        catalog
    }

    fn add(&mut self, mnemonic: &str, opcode: u16, opcode2: Option<u8>, format: InsnFormat, cond_mask: Option<u8>) {
        self.instructions.insert(mnemonic.to_uppercase(), InsnDef {
            mnemonic: mnemonic.to_uppercase(),
            opcode,
            opcode2,
            format,
            cond_mask,
        });
    }

    // -- RR format (opcode 1 byte at bits 0-7) --------------------------------
    fn register_rr_instructions(&mut self) {
        let f = InsnFormat::RR;
        self.add("LR",   0x18, None, f, None);
        self.add("CR",   0x19, None, f, None);
        self.add("AR",   0x1A, None, f, None);
        self.add("SR",   0x1B, None, f, None);
        self.add("MR",   0x1C, None, f, None);
        self.add("DR",   0x1D, None, f, None);
        self.add("ALR",  0x1E, None, f, None);
        self.add("SLR",  0x1F, None, f, None);
        self.add("LPDR", 0x20, None, f, None);
        self.add("LNDR", 0x21, None, f, None);
        self.add("LTDR", 0x22, None, f, None);
        self.add("LCDR", 0x23, None, f, None);
        self.add("HDR",  0x24, None, f, None);
        self.add("LDXR", 0x25, None, f, None);
        self.add("LDR",  0x28, None, f, None);
        self.add("CDR",  0x29, None, f, None);
        self.add("ADR",  0x2A, None, f, None);
        self.add("SDR",  0x2B, None, f, None);
        self.add("MDR",  0x2C, None, f, None);
        self.add("DDR",  0x2D, None, f, None);
        self.add("LER",  0x38, None, f, None);
        self.add("CER",  0x39, None, f, None);
        self.add("AER",  0x3A, None, f, None);
        self.add("SER",  0x3B, None, f, None);
        self.add("MER",  0x3C, None, f, None);
        self.add("DER",  0x3D, None, f, None);
        self.add("LPR",  0x10, None, f, None);
        self.add("LNR",  0x11, None, f, None);
        self.add("LTR",  0x12, None, f, None);
        self.add("LCR",  0x13, None, f, None);
        self.add("NR",   0x14, None, f, None);
        self.add("CLR",  0x15, None, f, None);
        self.add("OR",   0x16, None, f, None);
        self.add("XR",   0x17, None, f, None);
        self.add("BALR", 0x05, None, f, None);
        self.add("BCTR", 0x06, None, f, None);
        self.add("BCR",  0x07, None, f, None);
        self.add("BASR", 0x0D, None, f, None);
        self.add("SPM",  0x04, None, f, None);
        self.add("SVC",  0x0A, None, f, None); // technically I format but encoded similarly
    }

    // -- RRE format (opcode 2 bytes at bits 0-15) -----------------------------
    fn register_rre_instructions(&mut self) {
        let f = InsnFormat::RRE;
        self.add("LGR",  0xB904, None, f, None);
        self.add("LGFR", 0xB914, None, f, None);
        self.add("AGR",  0xB908, None, f, None);
        self.add("SGR",  0xB909, None, f, None);
        self.add("MSGR", 0xB90C, None, f, None);
        self.add("DSGR", 0xB90D, None, f, None);
        self.add("CGR",  0xB920, None, f, None);
        self.add("CLGR", 0xB921, None, f, None);
        self.add("NGR",  0xB980, None, f, None);
        self.add("OGR",  0xB981, None, f, None);
        self.add("XGR",  0xB982, None, f, None);
        self.add("LTGR", 0xB902, None, f, None);
        self.add("LCGR", 0xB903, None, f, None);
        self.add("LPGR", 0xB900, None, f, None);
        self.add("LNGR", 0xB901, None, f, None);
        self.add("ALGR", 0xB90A, None, f, None);
        self.add("SLGR", 0xB90B, None, f, None);
        self.add("LLGFR", 0xB916, None, f, None);
        self.add("FLOGR", 0xB983, None, f, None);
    }

    // -- RX format (opcode 1 byte at bits 0-7) --------------------------------
    fn register_rx_instructions(&mut self) {
        let f = InsnFormat::RX;
        self.add("L",    0x58, None, f, None);
        self.add("ST",   0x50, None, f, None);
        self.add("A",    0x5A, None, f, None);
        self.add("S",    0x5B, None, f, None);   // Subtract (RX)
        self.add("M",    0x5C, None, f, None);
        self.add("D",    0x5D, None, f, None);
        self.add("C",    0x59, None, f, None);
        self.add("CL",   0x55, None, f, None);
        self.add("AL",   0x5E, None, f, None);
        self.add("SL",   0x5F, None, f, None);
        self.add("N",    0x54, None, f, None);
        self.add("O",    0x56, None, f, None);
        self.add("X",    0x57, None, f, None);
        self.add("LA",   0x41, None, f, None);
        self.add("LH",   0x48, None, f, None);
        self.add("STH",  0x40, None, f, None);
        self.add("IC",   0x43, None, f, None);
        self.add("STC",  0x42, None, f, None);
        self.add("AH",   0x4A, None, f, None);
        self.add("SH",   0x4B, None, f, None);
        self.add("MH",   0x4C, None, f, None);
        self.add("CH",   0x49, None, f, None);
        self.add("CVB",  0x4F, None, f, None);
        self.add("CVD",  0x4E, None, f, None);
        self.add("BC",   0x47, None, f, None);
        self.add("BAL",  0x45, None, f, None);
        self.add("BCT",  0x46, None, f, None);
        self.add("BAS",  0x4D, None, f, None);
        self.add("EX",   0x44, None, f, None);
        self.add("LE",   0x78, None, f, None);
        self.add("LD",   0x68, None, f, None);
        self.add("STE",  0x70, None, f, None);
        self.add("STD",  0x60, None, f, None);
        self.add("AE",   0x7A, None, f, None);
        self.add("SE",   0x7B, None, f, None);
        self.add("ME",   0x7C, None, f, None);
        self.add("DE",   0x7D, None, f, None);
        self.add("CE",   0x79, None, f, None);
        self.add("AD",   0x6A, None, f, None);
        self.add("SD",   0x6B, None, f, None);
        self.add("MD",   0x6C, None, f, None);
        self.add("DD",   0x6D, None, f, None);
        self.add("CD",   0x69, None, f, None);
    }

    // -- RXY format (opcode split: byte0 at 0-7, byte5 at 40-47) --------
    fn register_rxy_instructions(&mut self) {
        let f = InsnFormat::RXY;
        self.add("LG",    0xE3, Some(0x04), f, None);
        self.add("STG",   0xE3, Some(0x24), f, None);
        self.add("AG",    0xE3, Some(0x08), f, None);
        self.add("SG",    0xE3, Some(0x09), f, None);
        self.add("MSG",   0xE3, Some(0x0C), f, None);
        self.add("DSG",   0xE3, Some(0x0D), f, None);
        self.add("CG",    0xE3, Some(0x20), f, None);
        self.add("CLG",   0xE3, Some(0x21), f, None);
        self.add("LY",    0xE3, Some(0x58), f, None);
        self.add("STY",   0xE3, Some(0x50), f, None);
        self.add("AY",    0xE3, Some(0x5A), f, None);
        self.add("SY",    0xE3, Some(0x5B), f, None);
        self.add("CY",    0xE3, Some(0x59), f, None);
        self.add("ALY",   0xE3, Some(0x5E), f, None);
        self.add("SLY",   0xE3, Some(0x5F), f, None);
        self.add("NY",    0xE3, Some(0x54), f, None);
        self.add("OY",    0xE3, Some(0x56), f, None);
        self.add("XY",    0xE3, Some(0x57), f, None);
        self.add("LAY",   0xE3, Some(0x71), f, None);
        self.add("LGF",   0xE3, Some(0x14), f, None);
        self.add("LLGF",  0xE3, Some(0x16), f, None);
        self.add("LHY",   0xE3, Some(0x78), f, None);
        self.add("STHY",  0xE3, Some(0x70), f, None);
        self.add("ICY",   0xE3, Some(0x73), f, None);
        self.add("STCY",  0xE3, Some(0x72), f, None);
        self.add("NG",    0xE3, Some(0x80), f, None);
        self.add("OG",    0xE3, Some(0x81), f, None);
        self.add("XG",    0xE3, Some(0x82), f, None);
        self.add("ALG",   0xE3, Some(0x0A), f, None);
        self.add("SLG",   0xE3, Some(0x0B), f, None);
    }

    // -- RS format (opcode 1 byte at bits 0-7) --------------------------------
    fn register_rs_instructions(&mut self) {
        let f = InsnFormat::RS;
        self.add("BXH",  0x86, None, f, None);
        self.add("BXLE", 0x87, None, f, None);
        self.add("LM",   0x98, None, f, None);
        self.add("STM",  0x90, None, f, None);
        self.add("SLA",  0x8B, None, f, None);
        self.add("SRA",  0x8A, None, f, None);
        self.add("SLL",  0x89, None, f, None);
        self.add("SRL",  0x88, None, f, None);
        self.add("ICM",  0xBF, None, f, None);
        self.add("STCM", 0xBE, None, f, None);
        self.add("CLM",  0xBD, None, f, None);
        self.add("CS",   0xBA, None, f, None);
        self.add("CDS",  0xBB, None, f, None);
    }

    // -- RSY format (opcode split: byte0 at 0-7, byte5 at 40-47) --------
    fn register_rsy_instructions(&mut self) {
        let f = InsnFormat::RSY;
        self.add("LMG",   0xEB, Some(0x04), f, None);
        self.add("STMG",  0xEB, Some(0x24), f, None);
        self.add("BXHG",  0xEB, Some(0x44), f, None);
        self.add("BXLEG", 0xEB, Some(0x45), f, None);
        self.add("SRAG",  0xEB, Some(0x0A), f, None);
        self.add("SLAG",  0xEB, Some(0x0B), f, None);
        self.add("SRLG",  0xEB, Some(0x0C), f, None);
        self.add("SLLG",  0xEB, Some(0x0D), f, None);
        self.add("CSY",   0xEB, Some(0x14), f, None);
        self.add("ICMH",  0xEB, Some(0x80), f, None);
        self.add("STCMH", 0xEB, Some(0x2C), f, None);
    }

    // -- RI format (opcode split: byte0 at 0-7, nibble at 12-15) --------
    fn register_ri_instructions(&mut self) {
        let f = InsnFormat::RI;
        self.add("AHI",   0xA70A, None, f, None);
        self.add("MHI",   0xA70C, None, f, None);
        self.add("CHI",   0xA70E, None, f, None);
        self.add("AGHI",  0xA70B, None, f, None);
        self.add("CGHI",  0xA70F, None, f, None);
        self.add("TMHH",  0xA702, None, f, None);
        self.add("TMHL",  0xA703, None, f, None);
        self.add("TMLH",  0xA700, None, f, None);
        self.add("TMLL",  0xA701, None, f, None);
        self.add("LHI",   0xA708, None, f, None);
        self.add("LGHI",  0xA709, None, f, None);
        self.add("BRC",   0xA704, None, f, None);
    }

    // -- RIL format (opcode split: byte0 at 0-7, nibble at 12-15) --------
    fn register_ril_instructions(&mut self) {
        let f = InsnFormat::RIL;
        self.add("BRCL",   0xC004, None, f, None);
        self.add("BRASL",  0xC005, None, f, None);
        self.add("AFI",    0xC209, None, f, None);
        self.add("AGFI",   0xC208, None, f, None);
        self.add("CFI",    0xC20D, None, f, None);
        self.add("CGFI",   0xC20C, None, f, None);
        self.add("LGFI",   0xC001, None, f, None);
        self.add("NIHF",   0xC00A, None, f, None);
        self.add("NILF",   0xC00B, None, f, None);
        self.add("OIHF",   0xC00C, None, f, None);
        self.add("OILF",   0xC00D, None, f, None);
        self.add("XIHF",   0xC006, None, f, None);
        self.add("XILF",   0xC007, None, f, None);
        self.add("LARL",   0xC000, None, f, None);
        self.add("LLIHF",  0xC00E, None, f, None);
        self.add("LLILF",  0xC00F, None, f, None);
        self.add("IIHF",   0xC008, None, f, None);
        self.add("IILF",   0xC009, None, f, None);
    }

    // -- SI format (opcode 1 byte at bits 0-7) --------------------------------
    fn register_si_instructions(&mut self) {
        let f = InsnFormat::SI;
        self.add("MVI",  0x92, None, f, None);
        self.add("CLI",  0x95, None, f, None);
        self.add("NI",   0x94, None, f, None);
        self.add("OI",   0x96, None, f, None);
        self.add("XI",   0x97, None, f, None);
        self.add("TM",   0x91, None, f, None);
    }

    // -- SIY format (opcode split: byte0 at 0-7, byte5 at 40-47) --------
    fn register_siy_instructions(&mut self) {
        let f = InsnFormat::SIY;
        self.add("MVIY", 0xEB, Some(0x52), f, None);
        self.add("CLIY", 0xEB, Some(0x55), f, None);
        self.add("NIY",  0xEB, Some(0x54), f, None);
        self.add("OIY",  0xEB, Some(0x56), f, None);
        self.add("XIY",  0xEB, Some(0x57), f, None);
        self.add("TMY",  0xEB, Some(0x51), f, None);
    }

    // -- SIL format (opcode 2 bytes at bits 0-15) -----------------------------
    fn register_sil_instructions(&mut self) {
        let f = InsnFormat::SIL;
        self.add("MVHHI", 0xE544, None, f, None);
        self.add("MVHI",  0xE54C, None, f, None);
        self.add("MVGHI", 0xE548, None, f, None);
        self.add("CHHSI", 0xE554, None, f, None);
        self.add("CHSI",  0xE55C, None, f, None);
        self.add("CGHSI", 0xE558, None, f, None);
        self.add("CLHHSI", 0xE555, None, f, None);
        self.add("CLFHSI", 0xE55D, None, f, None);
        self.add("CLGHSI", 0xE559, None, f, None);
    }

    // -- SS format (opcode 1 byte at bits 0-7) --------------------------------
    fn register_ss_instructions(&mut self) {
        // SSa: single length
        self.add("MVC",   0xD2, None, InsnFormat::SSa, None);
        self.add("CLC",   0xD5, None, InsnFormat::SSa, None);
        self.add("XC",    0xD7, None, InsnFormat::SSa, None);
        self.add("OC",    0xD6, None, InsnFormat::SSa, None);
        self.add("NC",    0xD4, None, InsnFormat::SSa, None);
        self.add("TR",    0xDC, None, InsnFormat::SSa, None);
        self.add("TRT",   0xDD, None, InsnFormat::SSa, None);
        self.add("MVN",   0xD1, None, InsnFormat::SSa, None);
        self.add("MVZ",   0xD3, None, InsnFormat::SSa, None);
        self.add("ED",    0xDE, None, InsnFormat::SSa, None);
        self.add("EDMK",  0xDF, None, InsnFormat::SSa, None);
        // SSb: two lengths
        self.add("PACK",  0xF2, None, InsnFormat::SSb, None);
        self.add("UNPK",  0xF3, None, InsnFormat::SSb, None);
        self.add("AP",    0xFA, None, InsnFormat::SSb, None);
        self.add("SP",    0xFB, None, InsnFormat::SSb, None);
        self.add("MP",    0xFC, None, InsnFormat::SSb, None);
        self.add("DP",    0xFD, None, InsnFormat::SSb, None);
        self.add("ZAP",   0xF8, None, InsnFormat::SSb, None);
        self.add("CP",    0xF9, None, InsnFormat::SSb, None);
        self.add("MVO",   0xF1, None, InsnFormat::SSb, None);
        self.add("SRP",   0xF0, None, InsnFormat::SSb, None);
    }

    // -- S format (opcode 2 bytes at bits 0-15) --------------------------------
    fn register_s_instructions(&mut self) {
        let f = InsnFormat::S;
        self.add("LPSW",  0x8200, None, f, None);
        self.add("STCK",  0xB205, None, f, None);
        self.add("STCKE", 0xB278, None, f, None);
        self.add("IPK",   0xB20B, None, f, None);
    }

    // -- E format (opcode 2 bytes, no operands) --------------------------------
    fn register_e_instructions(&mut self) {
        let f = InsnFormat::E;
        self.add("PR",    0x0101, None, f, None);
        self.add("SAM24", 0x010C, None, f, None);
        self.add("SAM31", 0x010D, None, f, None);
        self.add("SAM64", 0x010E, None, f, None);
        self.add("TRAP2", 0x01FF, None, f, None);
    }

    // -- Extended branch mnemonics (map to BC/BCR/BRC/BRCL with condition mask) -
    fn register_extended_branches(&mut self) {
        // BC-based (RX format, opcode 0x47)
        let bc_pairs: &[(&str, u8)] = &[
            ("B",   15), ("BR",  15),
            ("BH",   2), ("BL",   4), ("BE",   8), ("BNE",  7),
            ("BNH", 13), ("BNL", 11), ("BZ",   8), ("BNZ",  7),
            ("BO",   1), ("BNO", 14), ("BP",   2), ("BM",   4),
            ("NOP",  0), ("NOPR", 0),
        ];
        for &(mn, mask) in bc_pairs {
            if mn.ends_with('R') || mn == "NOPR" {
                // BCR-based (RR format)
                self.add(mn, 0x07, None, InsnFormat::RR, Some(mask));
            } else {
                // BC-based (RX format)
                self.add(mn, 0x47, None, InsnFormat::RX, Some(mask));
            }
        }

        // BRC-based relative branches (RI format, opcode A704)
        let brc_pairs: &[(&str, u8)] = &[
            ("J",   15), ("JH",   2), ("JL",   4), ("JE",   8),
            ("JNE",  7), ("JNH", 13), ("JNL", 11), ("JZ",   8),
            ("JNZ",  7), ("JO",   1), ("JNO", 14), ("JP",   2),
            ("JM",   4), ("JNOP", 0),
        ];
        for &(mn, mask) in brc_pairs {
            self.add(mn, 0xA704, None, InsnFormat::RI, Some(mask));
        }

        // BRCL-based relative branches (RIL format, opcode C004)
        let brcl_pairs: &[(&str, u8)] = &[
            ("JLU",  15), ("JLNOP", 0),
        ];
        for &(mn, mask) in brcl_pairs {
            self.add(mn, 0xC004, None, InsnFormat::RIL, Some(mask));
        }
    }

    // -- Vector facility instructions (z13+) ----------------------------------
    fn register_vector_instructions(&mut self) {
        // VRR-a: opcode(8) | V1(4) V2(4) | ___ | M5(4) M4(4) | M3(4) _(4) | opcode2(8)
        //   V1,V2[,M3[,M4[,M5]]]
        self.add("VCLZ",  0xE7, Some(0x53), InsnFormat::VRRa, None); // count leading zeros
        self.add("VCTZ",  0xE7, Some(0x52), InsnFormat::VRRa, None); // count trailing zeros
        self.add("VLC",   0xE7, Some(0xDE), InsnFormat::VRRa, None); // complement
        self.add("VLP",   0xE7, Some(0xDF), InsnFormat::VRRa, None); // load positive
        self.add("VPOPCT",0xE7, Some(0x50), InsnFormat::VRRa, None); // population count
        self.add("VSEG",  0xE7, Some(0x5F), InsnFormat::VRRa, None); // sign extend
        self.add("VUPH",  0xE7, Some(0xD7), InsnFormat::VRRa, None); // unpack high
        self.add("VUPL",  0xE7, Some(0xD6), InsnFormat::VRRa, None); // unpack low
        self.add("VUPLH", 0xE7, Some(0xD5), InsnFormat::VRRa, None); // unpack logical high
        self.add("VUPLL", 0xE7, Some(0xD4), InsnFormat::VRRa, None); // unpack logical low

        // VRR-b: opcode(8) | V1(4) V2(4) | V3(4) _(4) | _(4) M5(4) | _(4) M4(4) | opcode2(8)
        //   V1,V2,V3[,M4[,M5]]
        self.add("VFS",   0xE7, Some(0xE2), InsnFormat::VRRb, None); // FP subtract
        self.add("VFA",   0xE7, Some(0xE3), InsnFormat::VRRb, None); // FP add

        // VRR-c: opcode(8) | V1(4) V2(4) | V3(4) _(4) | M6(4) M5(4) | M4(4) _(4) | opcode2(8)
        //   V1,V2,V3[,M4[,M5[,M6]]]
        self.add("VA",    0xE7, Some(0xF3), InsnFormat::VRRc, None); // add
        self.add("VS",    0xE7, Some(0xF7), InsnFormat::VRRc, None); // subtract
        self.add("VN",    0xE7, Some(0x68), InsnFormat::VRRc, None); // AND
        self.add("VO",    0xE7, Some(0x6A), InsnFormat::VRRc, None); // OR
        self.add("VX",    0xE7, Some(0x6D), InsnFormat::VRRc, None); // XOR
        self.add("VNN",   0xE7, Some(0x6E), InsnFormat::VRRc, None); // NAND
        self.add("VNO",   0xE7, Some(0x6B), InsnFormat::VRRc, None); // NOR
        self.add("VNX",   0xE7, Some(0x6C), InsnFormat::VRRc, None); // NOT XOR
        self.add("VOC",   0xE7, Some(0x6F), InsnFormat::VRRc, None); // OR with complement
        self.add("VAVG",  0xE7, Some(0xF2), InsnFormat::VRRc, None); // average
        self.add("VAVGL", 0xE7, Some(0xF0), InsnFormat::VRRc, None); // average logical
        self.add("VCEQ",  0xE7, Some(0xF8), InsnFormat::VRRc, None); // compare equal
        self.add("VCH",   0xE7, Some(0xFB), InsnFormat::VRRc, None); // compare high
        self.add("VCHL",  0xE7, Some(0xF9), InsnFormat::VRRc, None); // compare high logical
        self.add("VMX",   0xE7, Some(0xFF), InsnFormat::VRRc, None); // maximum
        self.add("VMXL",  0xE7, Some(0xFD), InsnFormat::VRRc, None); // maximum logical
        self.add("VMN",   0xE7, Some(0xFE), InsnFormat::VRRc, None); // minimum
        self.add("VMNL",  0xE7, Some(0xFC), InsnFormat::VRRc, None); // minimum logical
        self.add("VMH",   0xE7, Some(0xA3), InsnFormat::VRRc, None); // multiply high
        self.add("VMLH",  0xE7, Some(0xA1), InsnFormat::VRRc, None); // multiply logical high
        self.add("VML",   0xE7, Some(0xA2), InsnFormat::VRRc, None); // multiply low
        self.add("VME",   0xE7, Some(0xA6), InsnFormat::VRRc, None); // multiply even
        self.add("VMLE",  0xE7, Some(0xA4), InsnFormat::VRRc, None); // multiply logical even
        self.add("VMO",   0xE7, Some(0xA7), InsnFormat::VRRc, None); // multiply odd
        self.add("VMLO",  0xE7, Some(0xA5), InsnFormat::VRRc, None); // multiply logical odd
        self.add("VMRH",  0xE7, Some(0x61), InsnFormat::VRRc, None); // merge high
        self.add("VMRL",  0xE7, Some(0x60), InsnFormat::VRRc, None); // merge low
        self.add("VPKS",  0xE7, Some(0x97), InsnFormat::VRRc, None); // pack signed
        self.add("VPKLS", 0xE7, Some(0x95), InsnFormat::VRRc, None); // pack logical signed

        // VRR-d: opcode(8) | V1(4) V2(4) | V3(4) M5(4) | _(4) _(4) | V4(4) _(4) | opcode2(8)
        self.add("VSEL",  0xE7, Some(0x8D), InsnFormat::VRRd, None); // select

        // VRR-e: opcode(8) | V1(4) V2(4) | V3(4) M6(4) | _(4) M5(4) | V4(4) _(4) | opcode2(8)
        self.add("VFMA",  0xE7, Some(0x8F), InsnFormat::VRRe, None); // FP multiply and add
        self.add("VFMS",  0xE7, Some(0x8E), InsnFormat::VRRe, None); // FP multiply and subtract

        // VRX: opcode(8) | V1(4) X2(4) | B2(4) D2(12) | M3(4) _(4) | opcode2(8)
        self.add("VL",    0xE7, Some(0x06), InsnFormat::VRX, None); // load
        self.add("VST",   0xE7, Some(0x0E), InsnFormat::VRX, None); // store
        self.add("VLBB",  0xE7, Some(0x07), InsnFormat::VRX, None); // load to block boundary
        self.add("VLEB",  0xE7, Some(0x00), InsnFormat::VRX, None); // load element (byte)
        self.add("VLEH",  0xE7, Some(0x01), InsnFormat::VRX, None); // load element (halfword)
        self.add("VLEF",  0xE7, Some(0x03), InsnFormat::VRX, None); // load element (fullword)
        self.add("VLEG",  0xE7, Some(0x02), InsnFormat::VRX, None); // load element (doubleword)

        // VRS-a: opcode(8) | V1(4) V3(4) | B2(4) D2(12) | M4(4) _(4) | opcode2(8)
        self.add("VERLL", 0xE7, Some(0x33), InsnFormat::VRSa, None); // element rotate left logical
        self.add("VESL",  0xE7, Some(0x30), InsnFormat::VRSa, None); // element shift left
        self.add("VESRA", 0xE7, Some(0x3A), InsnFormat::VRSa, None); // element shift right arith
        self.add("VESRL", 0xE7, Some(0x38), InsnFormat::VRSa, None); // element shift right logical
        self.add("VLM",   0xE7, Some(0x36), InsnFormat::VRSa, None); // load multiple
        self.add("VSTM",  0xE7, Some(0x3E), InsnFormat::VRSa, None); // store multiple

        // VRS-b: opcode(8) | V1(4) R3(4) | B2(4) D2(12) | M4(4) _(4) | opcode2(8)
        self.add("VLVG",  0xE7, Some(0x22), InsnFormat::VRSb, None); // load VR from GR element

        // VRS-c: opcode(8) | R1(4) V3(4) | B2(4) D2(12) | M4(4) _(4) | opcode2(8)
        self.add("VLGV",  0xE7, Some(0x21), InsnFormat::VRSc, None); // load GR from VR element

        // VRI-a: opcode(8) | V1(4) _(4) | I2(16) | M3(4) _(4) | opcode2(8)
        self.add("VREPI", 0xE7, Some(0x45), InsnFormat::VRIa, None); // replicate immediate
        self.add("VLEI",  0xE7, Some(0x40), InsnFormat::VRIa, None); // load element immediate (approx)

        // VRI-c: opcode(8) | V1(4) V3(4) | I2(16) | M4(4) _(4) | opcode2(8)
        self.add("VREP",  0xE7, Some(0x4D), InsnFormat::VRIc, None); // replicate

        // String instructions (VRR-b)
        self.add("VFAE",  0xE7, Some(0x82), InsnFormat::VRRb, None); // find any element equal
        self.add("VFEE",  0xE7, Some(0x80), InsnFormat::VRRb, None); // find element equal
        self.add("VFENE", 0xE7, Some(0x81), InsnFormat::VRRb, None); // find element not equal
        self.add("VISTR", 0xE7, Some(0x5C), InsnFormat::VRRa, None); // isolate string

        // VRR-c type — additional
        self.add("VSCBI", 0xE7, Some(0xF5), InsnFormat::VRRc, None); // subtract with borrow indication
        self.add("VACC",  0xE7, Some(0xF1), InsnFormat::VRRc, None); // add with carry
        self.add("VFM",   0xE7, Some(0xE7), InsnFormat::VRRb, None); // FP multiply
        self.add("VFD",   0xE7, Some(0xE5), InsnFormat::VRRb, None); // FP divide
    }

    /// Look up an instruction definition by mnemonic.
    pub fn lookup(&self, mnemonic: &str) -> Option<&InsnDef> {
        self.instructions.get(&mnemonic.to_uppercase())
    }

    /// Number of registered instructions.
    pub fn len(&self) -> usize {
        self.instructions.len()
    }

    /// Whether the catalog is empty.
    pub fn is_empty(&self) -> bool {
        self.instructions.is_empty()
    }
}

// ---------------------------------------------------------------------------
//  Instruction encoder
// ---------------------------------------------------------------------------

/// Encode an instruction given its definition and operand fields.
pub fn encode_instruction(def: &InsnDef, ops: &InsnOperands) -> Result<Vec<u8>, EncodeError> {
    match def.format {
        InsnFormat::E => encode_e(def),
        InsnFormat::I => encode_i(def, ops),
        InsnFormat::RR => encode_rr(def, ops),
        InsnFormat::RRE => encode_rre(def, ops),
        InsnFormat::RRFa | InsnFormat::RRFb | InsnFormat::RRFc => encode_rrf(def, ops),
        InsnFormat::RX => encode_rx(def, ops),
        InsnFormat::RXE => encode_rxe(def, ops),
        InsnFormat::RXY => encode_rxy(def, ops),
        InsnFormat::RS => encode_rs(def, ops),
        InsnFormat::RSY => encode_rsy(def, ops),
        InsnFormat::RI => encode_ri(def, ops),
        InsnFormat::RIEa | InsnFormat::RIEb | InsnFormat::RIEc
        | InsnFormat::RIEd | InsnFormat::RIEf => encode_rie(def, ops),
        InsnFormat::RIL => encode_ril(def, ops),
        InsnFormat::SI => encode_si(def, ops),
        InsnFormat::SIY => encode_siy(def, ops),
        InsnFormat::SIL => encode_sil(def, ops),
        InsnFormat::S => encode_s(def, ops),
        InsnFormat::SSa => encode_ssa(def, ops),
        InsnFormat::SSb => encode_ssb(def, ops),
        InsnFormat::SSE => encode_sse(def, ops),
        InsnFormat::SSF => encode_ssf(def, ops),
        InsnFormat::VRRa => encode_vrra(def, ops),
        InsnFormat::VRRb => encode_vrrb(def, ops),
        InsnFormat::VRRc => encode_vrrc(def, ops),
        InsnFormat::VRRd => encode_vrrd(def, ops),
        InsnFormat::VRRe => encode_vrre(def, ops),
        InsnFormat::VRIa => encode_vria(def, ops),
        InsnFormat::VRIb => encode_vrib(def, ops),
        InsnFormat::VRIc => encode_vric(def, ops),
        InsnFormat::VRSa => encode_vrsa(def, ops),
        InsnFormat::VRSb => encode_vrsb(def, ops),
        InsnFormat::VRSc => encode_vrsc(def, ops),
        InsnFormat::VRV => encode_vrv(def, ops),
        InsnFormat::VRX => encode_vrx(def, ops),
        InsnFormat::VSI => encode_vsi(def, ops),
    }
}

// E format: [opcode(16)]
fn encode_e(def: &InsnDef) -> Result<Vec<u8>, EncodeError> {
    let op = def.opcode;
    Ok(vec![(op >> 8) as u8, op as u8])
}

// I format: [opcode(8) | I2(8)]
fn encode_i(def: &InsnDef, ops: &InsnOperands) -> Result<Vec<u8>, EncodeError> {
    Ok(vec![def.opcode as u8, ops.i2 as u8])
}

// RR format: [opcode(8) | R1(4) R2(4)]
fn encode_rr(def: &InsnDef, ops: &InsnOperands) -> Result<Vec<u8>, EncodeError> {
    let r1 = if let Some(mask) = def.cond_mask { mask } else { ops.r1 };
    Ok(vec![def.opcode as u8, (r1 << 4) | (ops.r2 & 0x0F)])
}

// RRE format: [opcode(16) | 0000(8) | R1(4) R2(4)]
fn encode_rre(def: &InsnDef, ops: &InsnOperands) -> Result<Vec<u8>, EncodeError> {
    let op = def.opcode;
    Ok(vec![
        (op >> 8) as u8,
        op as u8,
        0x00,
        (ops.r1 << 4) | (ops.r2 & 0x0F),
    ])
}

// RRF format: [opcode(16) | R3(4) M4(4) | R1(4) R2(4)]
fn encode_rrf(def: &InsnDef, ops: &InsnOperands) -> Result<Vec<u8>, EncodeError> {
    let op = def.opcode;
    Ok(vec![
        (op >> 8) as u8,
        op as u8,
        (ops.r3 << 4) | (ops.m4 & 0x0F),
        (ops.r1 << 4) | (ops.r2 & 0x0F),
    ])
}

// RX format: [opcode(8) | R1(4) X2(4) | B2(4) D2(12)]
fn encode_rx(def: &InsnDef, ops: &InsnOperands) -> Result<Vec<u8>, EncodeError> {
    let r1 = if let Some(mask) = def.cond_mask { mask } else { ops.r1 };
    let d = ops.d2;
    if d > 0xFFF {
        return Err(EncodeError::DisplacementRange(d as i64));
    }
    Ok(vec![
        def.opcode as u8,
        (r1 << 4) | (ops.x2 & 0x0F),
        (ops.b2 << 4) | ((d >> 8) as u8 & 0x0F),
        d as u8,
    ])
}

// RXE format: [opcode(8) | R1(4) X2(4) | B2(4) D2(12) | M3(4) 0(4) | opcode2(8)]
fn encode_rxe(def: &InsnDef, ops: &InsnOperands) -> Result<Vec<u8>, EncodeError> {
    let d = ops.d2;
    if d > 0xFFF {
        return Err(EncodeError::DisplacementRange(d as i64));
    }
    let op2 = def.opcode2.unwrap_or(0);
    Ok(vec![
        def.opcode as u8,
        (ops.r1 << 4) | (ops.x2 & 0x0F),
        (ops.b2 << 4) | ((d >> 8) as u8 & 0x0F),
        d as u8,
        (ops.m3 << 4),
        op2,
    ])
}

// RXY format: [opcode(8) | R1(4) X2(4) | B2(4) DL2(12) | DH2(8) | opcode2(8)]
fn encode_rxy(def: &InsnDef, ops: &InsnOperands) -> Result<Vec<u8>, EncodeError> {
    // 20-bit signed displacement: DH(8) || DL(12)
    let d = ops.d2 as i32;
    let dl = (d & 0xFFF) as u16;
    let dh = ((d >> 12) & 0xFF) as u8;
    let op2 = def.opcode2.unwrap_or(0);
    Ok(vec![
        def.opcode as u8,
        (ops.r1 << 4) | (ops.x2 & 0x0F),
        (ops.b2 << 4) | ((dl >> 8) as u8 & 0x0F),
        dl as u8,
        dh,
        op2,
    ])
}

// RS format: [opcode(8) | R1(4) R3(4) | B2(4) D2(12)]
fn encode_rs(def: &InsnDef, ops: &InsnOperands) -> Result<Vec<u8>, EncodeError> {
    let d = ops.d2;
    if d > 0xFFF {
        return Err(EncodeError::DisplacementRange(d as i64));
    }
    Ok(vec![
        def.opcode as u8,
        (ops.r1 << 4) | (ops.r3 & 0x0F),
        (ops.b2 << 4) | ((d >> 8) as u8 & 0x0F),
        d as u8,
    ])
}

// RSY format: [opcode(8) | R1(4) R3(4) | B2(4) DL2(12) | DH2(8) | opcode2(8)]
fn encode_rsy(def: &InsnDef, ops: &InsnOperands) -> Result<Vec<u8>, EncodeError> {
    let d = ops.d2 as i32;
    let dl = (d & 0xFFF) as u16;
    let dh = ((d >> 12) & 0xFF) as u8;
    let op2 = def.opcode2.unwrap_or(0);
    Ok(vec![
        def.opcode as u8,
        (ops.r1 << 4) | (ops.r3 & 0x0F),
        (ops.b2 << 4) | ((dl >> 8) as u8 & 0x0F),
        dl as u8,
        dh,
        op2,
    ])
}

// RI format: [opcode(8) | R1(4) op2(4) | I2(16)]
//   opcode field is byte0, op2 nibble is bits 12-15.
fn encode_ri(def: &InsnDef, ops: &InsnOperands) -> Result<Vec<u8>, EncodeError> {
    let op = def.opcode;
    let byte0 = (op >> 8) as u8;
    let op2_nibble = (op & 0x0F) as u8;
    let r1 = if let Some(mask) = def.cond_mask { mask } else { ops.r1 };
    let i2 = ops.i2 as u16;
    Ok(vec![
        byte0,
        (r1 << 4) | op2_nibble,
        (i2 >> 8) as u8,
        i2 as u8,
    ])
}

// RIE format (simplified — uses RIEd for now): [opcode(8) | R1(4) R3(4) | I2(16) | 00 | opcode2(8)]
fn encode_rie(def: &InsnDef, ops: &InsnOperands) -> Result<Vec<u8>, EncodeError> {
    let op2 = def.opcode2.unwrap_or(0);
    let i2 = ops.i2 as u16;
    Ok(vec![
        def.opcode as u8,
        (ops.r1 << 4) | (ops.r3 & 0x0F),
        (i2 >> 8) as u8,
        i2 as u8,
        0x00,
        op2,
    ])
}

// RIL format: [opcode(8) | R1(4) op2(4) | I2(32)]
fn encode_ril(def: &InsnDef, ops: &InsnOperands) -> Result<Vec<u8>, EncodeError> {
    let op = def.opcode;
    let byte0 = (op >> 8) as u8;
    let op2_nibble = (op & 0x0F) as u8;
    let r1 = if let Some(mask) = def.cond_mask { mask } else { ops.r1 };
    let i2 = ops.i2 as u32;
    Ok(vec![
        byte0,
        (r1 << 4) | op2_nibble,
        (i2 >> 24) as u8,
        (i2 >> 16) as u8,
        (i2 >> 8) as u8,
        i2 as u8,
    ])
}

// SI format: [opcode(8) | I2(8) | B1(4) D1(12)]
fn encode_si(def: &InsnDef, ops: &InsnOperands) -> Result<Vec<u8>, EncodeError> {
    let d = ops.d1;
    if d > 0xFFF {
        return Err(EncodeError::DisplacementRange(d as i64));
    }
    Ok(vec![
        def.opcode as u8,
        ops.i2 as u8,
        (ops.b1 << 4) | ((d >> 8) as u8 & 0x0F),
        d as u8,
    ])
}

// SIY format: [opcode(8) | I2(8) | B1(4) DL1(12) | DH1(8) | opcode2(8)]
fn encode_siy(def: &InsnDef, ops: &InsnOperands) -> Result<Vec<u8>, EncodeError> {
    let d = ops.d1 as i32;
    let dl = (d & 0xFFF) as u16;
    let dh = ((d >> 12) & 0xFF) as u8;
    let op2 = def.opcode2.unwrap_or(0);
    Ok(vec![
        def.opcode as u8,
        ops.i2 as u8,
        (ops.b1 << 4) | ((dl >> 8) as u8 & 0x0F),
        dl as u8,
        dh,
        op2,
    ])
}

// SIL format: [opcode(16) | B1(4) D1(12) | I2(16)]
fn encode_sil(def: &InsnDef, ops: &InsnOperands) -> Result<Vec<u8>, EncodeError> {
    let op = def.opcode;
    let d = ops.d1;
    if d > 0xFFF {
        return Err(EncodeError::DisplacementRange(d as i64));
    }
    let i2 = ops.i2 as u16;
    Ok(vec![
        (op >> 8) as u8,
        op as u8,
        (ops.b1 << 4) | ((d >> 8) as u8 & 0x0F),
        d as u8,
        (i2 >> 8) as u8,
        i2 as u8,
    ])
}

// S format: [opcode(16) | B2(4) D2(12)]
fn encode_s(def: &InsnDef, ops: &InsnOperands) -> Result<Vec<u8>, EncodeError> {
    let op = def.opcode;
    let d = ops.d2;
    if d > 0xFFF {
        return Err(EncodeError::DisplacementRange(d as i64));
    }
    Ok(vec![
        (op >> 8) as u8,
        op as u8,
        (ops.b2 << 4) | ((d >> 8) as u8 & 0x0F),
        d as u8,
    ])
}

// SSa format: [opcode(8) | L(8) | B1(4) D1(12) | B2(4) D2(12)]
fn encode_ssa(def: &InsnDef, ops: &InsnOperands) -> Result<Vec<u8>, EncodeError> {
    let d1 = ops.d1;
    let d2 = ops.d2;
    if d1 > 0xFFF || d2 > 0xFFF {
        return Err(EncodeError::DisplacementRange(d1.max(d2) as i64));
    }
    Ok(vec![
        def.opcode as u8,
        ops.length,
        (ops.b1 << 4) | ((d1 >> 8) as u8 & 0x0F),
        d1 as u8,
        (ops.b2 << 4) | ((d2 >> 8) as u8 & 0x0F),
        d2 as u8,
    ])
}

// SSb format: [opcode(8) | L1(4) L2(4) | B1(4) D1(12) | B2(4) D2(12)]
fn encode_ssb(def: &InsnDef, ops: &InsnOperands) -> Result<Vec<u8>, EncodeError> {
    let d1 = ops.d1;
    let d2 = ops.d2;
    if d1 > 0xFFF || d2 > 0xFFF {
        return Err(EncodeError::DisplacementRange(d1.max(d2) as i64));
    }
    Ok(vec![
        def.opcode as u8,
        (ops.l1 << 4) | (ops.l2 & 0x0F),
        (ops.b1 << 4) | ((d1 >> 8) as u8 & 0x0F),
        d1 as u8,
        (ops.b2 << 4) | ((d2 >> 8) as u8 & 0x0F),
        d2 as u8,
    ])
}

// SSE format: [opcode(16) | B1(4) D1(12) | B2(4) D2(12)]
fn encode_sse(def: &InsnDef, ops: &InsnOperands) -> Result<Vec<u8>, EncodeError> {
    let op = def.opcode;
    let d1 = ops.d1;
    let d2 = ops.d2;
    if d1 > 0xFFF || d2 > 0xFFF {
        return Err(EncodeError::DisplacementRange(d1.max(d2) as i64));
    }
    Ok(vec![
        (op >> 8) as u8,
        op as u8,
        (ops.b1 << 4) | ((d1 >> 8) as u8 & 0x0F),
        d1 as u8,
        (ops.b2 << 4) | ((d2 >> 8) as u8 & 0x0F),
        d2 as u8,
    ])
}

// SSF format: [opcode(8) | R3(4) op2(4) | B1(4) D1(12) | B2(4) D2(12)]
fn encode_ssf(def: &InsnDef, ops: &InsnOperands) -> Result<Vec<u8>, EncodeError> {
    let d1 = ops.d1;
    let d2 = ops.d2;
    if d1 > 0xFFF || d2 > 0xFFF {
        return Err(EncodeError::DisplacementRange(d1.max(d2) as i64));
    }
    let op2_nibble = def.opcode2.unwrap_or(0) & 0x0F;
    Ok(vec![
        def.opcode as u8,
        (ops.r3 << 4) | op2_nibble,
        (ops.b1 << 4) | ((d1 >> 8) as u8 & 0x0F),
        d1 as u8,
        (ops.b2 << 4) | ((d2 >> 8) as u8 & 0x0F),
        d2 as u8,
    ])
}

// ---------------------------------------------------------------------------
//  Vector instruction encoders
// ---------------------------------------------------------------------------

/// Helper: RXB (register extension bits) for vector registers V1..V4.
/// Bit 0 = high bit of V1, bit 1 = high bit of V2, bit 2 = high bit of V3, bit 3 = high bit of V4.
fn rxb(v1: u8, v2: u8, v3: u8, v4: u8) -> u8 {
    let b0 = if v1 > 15 { 0x08 } else { 0 };
    let b1 = if v2 > 15 { 0x04 } else { 0 };
    let b2 = if v3 > 15 { 0x02 } else { 0 };
    let b3 = if v4 > 15 { 0x01 } else { 0 };
    b0 | b1 | b2 | b3
}

// VRR-a: [op(8) | V1(4) V2(4) | _(8) | M5(4) M4(4) | M3(4) RXB(4) | op2(8)]
fn encode_vrra(def: &InsnDef, ops: &InsnOperands) -> Result<Vec<u8>, EncodeError> {
    let op2 = def.opcode2.unwrap_or(0);
    let rxb_val = rxb(ops.v1, ops.v2, 0, 0);
    Ok(vec![
        def.opcode as u8,
        ((ops.v1 & 0x0F) << 4) | (ops.v2 & 0x0F),
        0x00,
        (ops.m5 << 4) | (ops.m4_v & 0x0F),
        (ops.m3_v << 4) | rxb_val,
        op2,
    ])
}

// VRR-b: [op(8) | V1(4) V2(4) | V3(4) _(4) | _(4) M5(4) | _(4) M4(4) RXB(4) | op2(8)]
fn encode_vrrb(def: &InsnDef, ops: &InsnOperands) -> Result<Vec<u8>, EncodeError> {
    let op2 = def.opcode2.unwrap_or(0);
    let rxb_val = rxb(ops.v1, ops.v2, ops.v3, 0);
    Ok(vec![
        def.opcode as u8,
        ((ops.v1 & 0x0F) << 4) | (ops.v2 & 0x0F),
        ((ops.v3 & 0x0F) << 4),
        ops.m5 & 0x0F,
        (ops.m4_v << 4) | rxb_val,
        op2,
    ])
}

// VRR-c: [op(8) | V1(4) V2(4) | V3(4) _(4) | M6(4) M5(4) | M4(4) RXB(4) | op2(8)]
fn encode_vrrc(def: &InsnDef, ops: &InsnOperands) -> Result<Vec<u8>, EncodeError> {
    let op2 = def.opcode2.unwrap_or(0);
    let rxb_val = rxb(ops.v1, ops.v2, ops.v3, 0);
    Ok(vec![
        def.opcode as u8,
        ((ops.v1 & 0x0F) << 4) | (ops.v2 & 0x0F),
        ((ops.v3 & 0x0F) << 4),
        (ops.m6 << 4) | (ops.m5 & 0x0F),
        (ops.m4_v << 4) | rxb_val,
        op2,
    ])
}

// VRR-d: [op(8) | V1(4) V2(4) | V3(4) M5(4) | _(8) | V4(4) RXB(4) | op2(8)]
fn encode_vrrd(def: &InsnDef, ops: &InsnOperands) -> Result<Vec<u8>, EncodeError> {
    let op2 = def.opcode2.unwrap_or(0);
    let rxb_val = rxb(ops.v1, ops.v2, ops.v3, ops.v4);
    Ok(vec![
        def.opcode as u8,
        ((ops.v1 & 0x0F) << 4) | (ops.v2 & 0x0F),
        ((ops.v3 & 0x0F) << 4) | (ops.m5 & 0x0F),
        0x00,
        ((ops.v4 & 0x0F) << 4) | rxb_val,
        op2,
    ])
}

// VRR-e: [op(8) | V1(4) V2(4) | V3(4) M6(4) | _(4) M5(4) | V4(4) RXB(4) | op2(8)]
fn encode_vrre(def: &InsnDef, ops: &InsnOperands) -> Result<Vec<u8>, EncodeError> {
    let op2 = def.opcode2.unwrap_or(0);
    let rxb_val = rxb(ops.v1, ops.v2, ops.v3, ops.v4);
    Ok(vec![
        def.opcode as u8,
        ((ops.v1 & 0x0F) << 4) | (ops.v2 & 0x0F),
        ((ops.v3 & 0x0F) << 4) | (ops.m6 & 0x0F),
        ops.m5 & 0x0F,
        ((ops.v4 & 0x0F) << 4) | rxb_val,
        op2,
    ])
}

// VRI-a: [op(8) | V1(4) _(4) | I2(16) | M3(4) RXB(4) | op2(8)]
fn encode_vria(def: &InsnDef, ops: &InsnOperands) -> Result<Vec<u8>, EncodeError> {
    let op2 = def.opcode2.unwrap_or(0);
    let rxb_val = rxb(ops.v1, 0, 0, 0);
    Ok(vec![
        def.opcode as u8,
        (ops.v1 & 0x0F) << 4,
        (ops.i2_v >> 8) as u8,
        ops.i2_v as u8,
        (ops.m3_v << 4) | rxb_val,
        op2,
    ])
}

// VRI-b: [op(8) | V1(4) _(4) | I2(8) I3(8) | M4(4) RXB(4) | op2(8)]
fn encode_vrib(def: &InsnDef, ops: &InsnOperands) -> Result<Vec<u8>, EncodeError> {
    let op2 = def.opcode2.unwrap_or(0);
    let rxb_val = rxb(ops.v1, 0, 0, 0);
    Ok(vec![
        def.opcode as u8,
        (ops.v1 & 0x0F) << 4,
        ops.i2_v as u8,
        ops.i3_v,
        (ops.m4_v << 4) | rxb_val,
        op2,
    ])
}

// VRI-c: [op(8) | V1(4) V3(4) | I2(16) | M4(4) RXB(4) | op2(8)]
fn encode_vric(def: &InsnDef, ops: &InsnOperands) -> Result<Vec<u8>, EncodeError> {
    let op2 = def.opcode2.unwrap_or(0);
    let rxb_val = rxb(ops.v1, 0, ops.v3, 0);
    Ok(vec![
        def.opcode as u8,
        ((ops.v1 & 0x0F) << 4) | (ops.v3 & 0x0F),
        (ops.i2_v >> 8) as u8,
        ops.i2_v as u8,
        (ops.m4_v << 4) | rxb_val,
        op2,
    ])
}

// VRS-a: [op(8) | V1(4) V3(4) | B2(4) D2(12) | M4(4) RXB(4) | op2(8)]
fn encode_vrsa(def: &InsnDef, ops: &InsnOperands) -> Result<Vec<u8>, EncodeError> {
    let d = ops.d2;
    if d > 0xFFF {
        return Err(EncodeError::DisplacementRange(d as i64));
    }
    let op2 = def.opcode2.unwrap_or(0);
    let rxb_val = rxb(ops.v1, 0, ops.v3, 0);
    Ok(vec![
        def.opcode as u8,
        ((ops.v1 & 0x0F) << 4) | (ops.v3 & 0x0F),
        (ops.b2 << 4) | ((d >> 8) as u8 & 0x0F),
        d as u8,
        (ops.m4_v << 4) | rxb_val,
        op2,
    ])
}

// VRS-b: [op(8) | V1(4) R3(4) | B2(4) D2(12) | M4(4) RXB(4) | op2(8)]
fn encode_vrsb(def: &InsnDef, ops: &InsnOperands) -> Result<Vec<u8>, EncodeError> {
    let d = ops.d2;
    if d > 0xFFF {
        return Err(EncodeError::DisplacementRange(d as i64));
    }
    let op2 = def.opcode2.unwrap_or(0);
    let rxb_val = rxb(ops.v1, 0, 0, 0);
    Ok(vec![
        def.opcode as u8,
        ((ops.v1 & 0x0F) << 4) | (ops.r3 & 0x0F),
        (ops.b2 << 4) | ((d >> 8) as u8 & 0x0F),
        d as u8,
        (ops.m4_v << 4) | rxb_val,
        op2,
    ])
}

// VRS-c: [op(8) | R1(4) V3(4) | B2(4) D2(12) | M4(4) RXB(4) | op2(8)]
fn encode_vrsc(def: &InsnDef, ops: &InsnOperands) -> Result<Vec<u8>, EncodeError> {
    let d = ops.d2;
    if d > 0xFFF {
        return Err(EncodeError::DisplacementRange(d as i64));
    }
    let op2 = def.opcode2.unwrap_or(0);
    let rxb_val = rxb(0, 0, ops.v3, 0);
    Ok(vec![
        def.opcode as u8,
        (ops.r1 << 4) | (ops.v3 & 0x0F),
        (ops.b2 << 4) | ((d >> 8) as u8 & 0x0F),
        d as u8,
        (ops.m4_v << 4) | rxb_val,
        op2,
    ])
}

// VRV: [op(8) | V1(4) V2(4) | B2(4) D2(12) | M3(4) RXB(4) | op2(8)]
fn encode_vrv(def: &InsnDef, ops: &InsnOperands) -> Result<Vec<u8>, EncodeError> {
    let d = ops.d2;
    if d > 0xFFF {
        return Err(EncodeError::DisplacementRange(d as i64));
    }
    let op2 = def.opcode2.unwrap_or(0);
    let rxb_val = rxb(ops.v1, ops.v2, 0, 0);
    Ok(vec![
        def.opcode as u8,
        ((ops.v1 & 0x0F) << 4) | (ops.v2 & 0x0F),
        (ops.b2 << 4) | ((d >> 8) as u8 & 0x0F),
        d as u8,
        (ops.m3_v << 4) | rxb_val,
        op2,
    ])
}

// VRX: [op(8) | V1(4) X2(4) | B2(4) D2(12) | M3(4) RXB(4) | op2(8)]
fn encode_vrx(def: &InsnDef, ops: &InsnOperands) -> Result<Vec<u8>, EncodeError> {
    let d = ops.d2;
    if d > 0xFFF {
        return Err(EncodeError::DisplacementRange(d as i64));
    }
    let op2 = def.opcode2.unwrap_or(0);
    let rxb_val = rxb(ops.v1, 0, 0, 0);
    Ok(vec![
        def.opcode as u8,
        ((ops.v1 & 0x0F) << 4) | (ops.x2 & 0x0F),
        (ops.b2 << 4) | ((d >> 8) as u8 & 0x0F),
        d as u8,
        (ops.m3_v << 4) | rxb_val,
        op2,
    ])
}

// VSI: [op(8) | I3(8) | B2(4) D2(12) | V1(4) RXB(4) | op2(8)]
fn encode_vsi(def: &InsnDef, ops: &InsnOperands) -> Result<Vec<u8>, EncodeError> {
    let d = ops.d2;
    if d > 0xFFF {
        return Err(EncodeError::DisplacementRange(d as i64));
    }
    let op2 = def.opcode2.unwrap_or(0);
    let rxb_val = rxb(ops.v1, 0, 0, 0);
    Ok(vec![
        def.opcode as u8,
        ops.i3_v,
        (ops.b2 << 4) | ((d >> 8) as u8 & 0x0F),
        d as u8,
        ((ops.v1 & 0x0F) << 4) | rxb_val,
        op2,
    ])
}

// ---------------------------------------------------------------------------
//  Display for InsnFormat
// ---------------------------------------------------------------------------

impl fmt::Display for InsnFormat {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

// ---------------------------------------------------------------------------
//  Display for EncodeError (already via thiserror, but Token Display needed)
// ---------------------------------------------------------------------------

// Token Display is already defined in lexer.rs; EncodeError Display via thiserror.

// ---------------------------------------------------------------------------
//  Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_catalog_size() {
        let cat = InsnCatalog::new();
        // We registered 200+ instructions; check minimum.
        assert!(cat.len() > 200, "Catalog has {} instructions", cat.len());
    }

    #[test]
    fn test_lookup() {
        let cat = InsnCatalog::new();
        let lr = cat.lookup("LR").unwrap();
        assert_eq!(lr.opcode, 0x18);
        assert_eq!(lr.format, InsnFormat::RR);
    }

    #[test]
    fn test_lookup_case_insensitive() {
        let cat = InsnCatalog::new();
        assert!(cat.lookup("lr").is_some());
        assert!(cat.lookup("Lr").is_some());
    }

    #[test]
    fn test_encode_e() {
        let cat = InsnCatalog::new();
        let def = cat.lookup("PR").unwrap();
        let ops = InsnOperands::default();
        let bytes = encode_instruction(def, &ops).unwrap();
        assert_eq!(bytes, vec![0x01, 0x01]);
    }

    #[test]
    fn test_encode_rr_lr() {
        let cat = InsnCatalog::new();
        let def = cat.lookup("LR").unwrap();
        let ops = InsnOperands { r1: 3, r2: 5, ..Default::default() };
        let bytes = encode_instruction(def, &ops).unwrap();
        // LR: opcode 0x18, R1=3, R2=5 → [18, 35]
        assert_eq!(bytes, vec![0x18, 0x35]);
    }

    #[test]
    fn test_encode_rr_ar() {
        let cat = InsnCatalog::new();
        let def = cat.lookup("AR").unwrap();
        let ops = InsnOperands { r1: 1, r2: 2, ..Default::default() };
        let bytes = encode_instruction(def, &ops).unwrap();
        assert_eq!(bytes, vec![0x1A, 0x12]);
    }

    #[test]
    fn test_encode_rre_lgr() {
        let cat = InsnCatalog::new();
        let def = cat.lookup("LGR").unwrap();
        let ops = InsnOperands { r1: 4, r2: 6, ..Default::default() };
        let bytes = encode_instruction(def, &ops).unwrap();
        // LGR: opcode B904, 00, R1=4 R2=6 → [B9, 04, 00, 46]
        assert_eq!(bytes, vec![0xB9, 0x04, 0x00, 0x46]);
    }

    #[test]
    fn test_encode_rx_l() {
        let cat = InsnCatalog::new();
        let def = cat.lookup("L").unwrap();
        let ops = InsnOperands {
            r1: 3, x2: 0, b2: 12, d2: 0x100,
            ..Default::default()
        };
        let bytes = encode_instruction(def, &ops).unwrap();
        // L R3,X'100'(0,12): opcode 58, R1=3 X2=0, B2=C D2=100 → [58, 30, C1, 00]
        assert_eq!(bytes, vec![0x58, 0x30, 0xC1, 0x00]);
    }

    #[test]
    fn test_encode_rx_bc_branch() {
        let cat = InsnCatalog::new();
        let def = cat.lookup("B").unwrap();
        let ops = InsnOperands {
            x2: 0, b2: 12, d2: 0x200,
            ..Default::default()
        };
        let bytes = encode_instruction(def, &ops).unwrap();
        // B is BC 15: opcode 47, M1=F X2=0, B2=C D2=200 → [47, F0, C2, 00]
        assert_eq!(bytes, vec![0x47, 0xF0, 0xC2, 0x00]);
    }

    #[test]
    fn test_encode_rx_be() {
        let cat = InsnCatalog::new();
        let def = cat.lookup("BE").unwrap();
        let ops = InsnOperands {
            x2: 0, b2: 12, d2: 0x050,
            ..Default::default()
        };
        let bytes = encode_instruction(def, &ops).unwrap();
        // BE = BC 8: [47, 80, C0, 50]
        assert_eq!(bytes, vec![0x47, 0x80, 0xC0, 0x50]);
    }

    #[test]
    fn test_encode_rxy_lg() {
        let cat = InsnCatalog::new();
        let def = cat.lookup("LG").unwrap();
        let ops = InsnOperands {
            r1: 5, x2: 0, b2: 13, d2: 0x100,
            ..Default::default()
        };
        let bytes = encode_instruction(def, &ops).unwrap();
        // LG R5,X'100'(0,13): opcode E3, R1=5 X2=0, B2=D DL=100, DH=00, op2=04
        assert_eq!(bytes, vec![0xE3, 0x50, 0xD1, 0x00, 0x00, 0x04]);
    }

    #[test]
    fn test_encode_rs_lm() {
        let cat = InsnCatalog::new();
        let def = cat.lookup("LM").unwrap();
        let ops = InsnOperands {
            r1: 14, r3: 12, b2: 13, d2: 12,
            ..Default::default()
        };
        let bytes = encode_instruction(def, &ops).unwrap();
        // LM 14,12,12(13): opcode 98, R1=E R3=C, B2=D D2=00C → [98, EC, D0, 0C]
        assert_eq!(bytes, vec![0x98, 0xEC, 0xD0, 0x0C]);
    }

    #[test]
    fn test_encode_rsy_lmg() {
        let cat = InsnCatalog::new();
        let def = cat.lookup("LMG").unwrap();
        let ops = InsnOperands {
            r1: 14, r3: 12, b2: 13, d2: 0x100,
            ..Default::default()
        };
        let bytes = encode_instruction(def, &ops).unwrap();
        // LMG 14,12,X'100'(13): [EB, EC, D1, 00, 00, 04]
        assert_eq!(bytes, vec![0xEB, 0xEC, 0xD1, 0x00, 0x00, 0x04]);
    }

    #[test]
    fn test_encode_ri_ahi() {
        let cat = InsnCatalog::new();
        let def = cat.lookup("AHI").unwrap();
        let ops = InsnOperands {
            r1: 1, i2: 100,
            ..Default::default()
        };
        let bytes = encode_instruction(def, &ops).unwrap();
        // AHI R1,100: opcode A7, R1=1 op2=A, I2=0064 → [A7, 1A, 00, 64]
        assert_eq!(bytes, vec![0xA7, 0x1A, 0x00, 0x64]);
    }

    #[test]
    fn test_encode_ri_brc_j() {
        let cat = InsnCatalog::new();
        let def = cat.lookup("J").unwrap();
        let ops = InsnOperands {
            i2: 10, // relative offset
            ..Default::default()
        };
        let bytes = encode_instruction(def, &ops).unwrap();
        // J (BRC 15,offset): opcode A7, M1=F op2=4, I2=000A → [A7, F4, 00, 0A]
        assert_eq!(bytes, vec![0xA7, 0xF4, 0x00, 0x0A]);
    }

    #[test]
    fn test_encode_ril_brasl() {
        let cat = InsnCatalog::new();
        let def = cat.lookup("BRASL").unwrap();
        let ops = InsnOperands {
            r1: 14, i2: 0x1000,
            ..Default::default()
        };
        let bytes = encode_instruction(def, &ops).unwrap();
        // BRASL R14,offset: opcode C0, R1=E op2=5, I2(32)=00001000
        assert_eq!(bytes, vec![0xC0, 0xE5, 0x00, 0x00, 0x10, 0x00]);
    }

    #[test]
    fn test_encode_si_mvi() {
        let cat = InsnCatalog::new();
        let def = cat.lookup("MVI").unwrap();
        let ops = InsnOperands {
            i2: 0xC1, // EBCDIC 'A'
            b1: 12, d1: 0x200,
            ..Default::default()
        };
        let bytes = encode_instruction(def, &ops).unwrap();
        // MVI X'200'(12),X'C1': opcode 92, I2=C1, B1=C D1=200 → [92, C1, C2, 00]
        assert_eq!(bytes, vec![0x92, 0xC1, 0xC2, 0x00]);
    }

    #[test]
    fn test_encode_siy_mviy() {
        let cat = InsnCatalog::new();
        let def = cat.lookup("MVIY").unwrap();
        let ops = InsnOperands {
            i2: 0x40,
            b1: 9, d1: 0x100,
            ..Default::default()
        };
        let bytes = encode_instruction(def, &ops).unwrap();
        // MVIY X'100'(9),X'40': [EB, 40, 91, 00, 00, 52]
        assert_eq!(bytes, vec![0xEB, 0x40, 0x91, 0x00, 0x00, 0x52]);
    }

    #[test]
    fn test_encode_ssa_mvc() {
        let cat = InsnCatalog::new();
        let def = cat.lookup("MVC").unwrap();
        let ops = InsnOperands {
            length: 19, // L-1 = 19 (20 bytes)
            b1: 12, d1: 0x200,
            b2: 12, d2: 0x300,
            ..Default::default()
        };
        let bytes = encode_instruction(def, &ops).unwrap();
        // MVC X'200'(20,12),X'300'(12): opcode D2, L=13, B1=C D1=200, B2=C D2=300
        assert_eq!(bytes, vec![0xD2, 19, 0xC2, 0x00, 0xC3, 0x00]);
    }

    #[test]
    fn test_encode_ssb_ap() {
        let cat = InsnCatalog::new();
        let def = cat.lookup("AP").unwrap();
        let ops = InsnOperands {
            l1: 7, l2: 3,
            b1: 10, d1: 0x100,
            b2: 10, d2: 0x200,
            ..Default::default()
        };
        let bytes = encode_instruction(def, &ops).unwrap();
        // AP X'100'(8,10),X'200'(4,10): opcode FA, L1=7 L2=3, B1=A D1=100, B2=A D2=200
        assert_eq!(bytes, vec![0xFA, 0x73, 0xA1, 0x00, 0xA2, 0x00]);
    }

    #[test]
    fn test_encode_s_stck() {
        let cat = InsnCatalog::new();
        let def = cat.lookup("STCK").unwrap();
        let ops = InsnOperands {
            b2: 13, d2: 0x200,
            ..Default::default()
        };
        let bytes = encode_instruction(def, &ops).unwrap();
        // STCK X'200'(13): opcode B205, B2=D D2=200 → [B2, 05, D2, 00]
        assert_eq!(bytes, vec![0xB2, 0x05, 0xD2, 0x00]);
    }

    #[test]
    fn test_displacement_out_of_range() {
        let cat = InsnCatalog::new();
        let def = cat.lookup("L").unwrap();
        let ops = InsnOperands {
            r1: 1, b2: 12, d2: 0x1000, // 4096, max is 4095
            ..Default::default()
        };
        assert!(encode_instruction(def, &ops).is_err());
    }

    #[test]
    fn test_insn_format_length() {
        assert_eq!(InsnFormat::E.length(), 2);
        assert_eq!(InsnFormat::RR.length(), 2);
        assert_eq!(InsnFormat::RX.length(), 4);
        assert_eq!(InsnFormat::RRE.length(), 4);
        assert_eq!(InsnFormat::RS.length(), 4);
        assert_eq!(InsnFormat::SI.length(), 4);
        assert_eq!(InsnFormat::RXY.length(), 6);
        assert_eq!(InsnFormat::RSY.length(), 6);
        assert_eq!(InsnFormat::RIL.length(), 6);
        assert_eq!(InsnFormat::SSa.length(), 6);
        assert_eq!(InsnFormat::SSb.length(), 6);
    }

    #[test]
    fn test_extended_branch_br() {
        let cat = InsnCatalog::new();
        let def = cat.lookup("BR").unwrap();
        assert_eq!(def.format, InsnFormat::RR);
        assert_eq!(def.cond_mask, Some(15));
        let ops = InsnOperands { r2: 14, ..Default::default() };
        let bytes = encode_instruction(def, &ops).unwrap();
        // BR R14 = BCR 15,14 → [07, FE]
        assert_eq!(bytes, vec![0x07, 0xFE]);
    }

    #[test]
    fn test_encode_sil_mvhi() {
        let cat = InsnCatalog::new();
        let def = cat.lookup("MVHI").unwrap();
        let ops = InsnOperands {
            b1: 13, d1: 0x100,
            i2: 42,
            ..Default::default()
        };
        let bytes = encode_instruction(def, &ops).unwrap();
        // MVHI X'100'(13),42: opcode E54C, B1=D D1=100, I2=002A → [E5, 4C, D1, 00, 00, 2A]
        assert_eq!(bytes, vec![0xE5, 0x4C, 0xD1, 0x00, 0x00, 0x2A]);
    }

    #[test]
    fn test_unknown_mnemonic() {
        let cat = InsnCatalog::new();
        assert!(cat.lookup("INVALID").is_none());
    }

    #[test]
    fn test_encode_nop() {
        let cat = InsnCatalog::new();
        let def = cat.lookup("NOP").unwrap();
        let ops = InsnOperands {
            x2: 0, b2: 0, d2: 0,
            ..Default::default()
        };
        let bytes = encode_instruction(def, &ops).unwrap();
        // NOP = BC 0,0 → [47, 00, 00, 00]
        assert_eq!(bytes, vec![0x47, 0x00, 0x00, 0x00]);
    }

    #[test]
    fn test_encode_nopr() {
        let cat = InsnCatalog::new();
        let def = cat.lookup("NOPR").unwrap();
        let ops = InsnOperands { r2: 0, ..Default::default() };
        let bytes = encode_instruction(def, &ops).unwrap();
        // NOPR = BCR 0,0 → [07, 00]
        assert_eq!(bytes, vec![0x07, 0x00]);
    }
}
