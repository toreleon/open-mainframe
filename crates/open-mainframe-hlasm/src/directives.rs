//! HLASM assembler directives.
//!
//! Covers section control, data definition, symbol management, listing
//! control, and external linkage directives.

use std::fmt;

// ---------------------------------------------------------------------------
//  Directive catalog
// ---------------------------------------------------------------------------

/// Known HLASM assembler directive opcodes.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Directive {
    // ─── Section control ───
    Csect,
    Dsect,
    Rsect,
    Com,
    Dxd,
    Start,
    End,
    Loctr,

    // ─── Data definition ───
    Dc,
    Ds,
    Ccw,
    Ccw0,
    Ccw1,

    // ─── Symbol & addressing ───
    Equ,
    Using,
    Drop,
    Org,
    Push,
    Pop,
    Opsyn,

    // ─── External linkage ───
    Entry,
    Extrn,
    Wxtrn,

    // ─── Source management ───
    Copy,
    Ltorg,
    Cnop,
    Amode,
    Rmode,

    // ─── Listing control ───
    Print,
    Title,
    Eject,
    Space,
    Punch,
    Repro,

    // ─── Conditional assembly / control ───
    Acontrol,
    Adata,
    Ainsert,
    Exitctl,
    Xattr,
    Cattr,
    Alias,

    // ─── Macro-related ───
    Macro,
    Mend,
    Mexit,
    Mnote,

    // ─── Conditional assembly ───
    Lcla,
    Lclb,
    Lclc,
    Gbla,
    Gblb,
    Gblc,
    Seta,
    Setb,
    Setc,
    Aif,
    Ago,
    Anop,
    Actr,
}

impl Directive {
    /// Try to parse a directive from an opcode string.
    pub fn from_opcode(opcode: &str) -> Option<Self> {
        match opcode.to_uppercase().as_str() {
            "CSECT" => Some(Self::Csect),
            "DSECT" => Some(Self::Dsect),
            "RSECT" => Some(Self::Rsect),
            "COM" => Some(Self::Com),
            "DXD" => Some(Self::Dxd),
            "START" => Some(Self::Start),
            "END" => Some(Self::End),
            "LOCTR" => Some(Self::Loctr),
            "DC" => Some(Self::Dc),
            "DS" => Some(Self::Ds),
            "CCW" => Some(Self::Ccw),
            "CCW0" => Some(Self::Ccw0),
            "CCW1" => Some(Self::Ccw1),
            "EQU" => Some(Self::Equ),
            "USING" => Some(Self::Using),
            "DROP" => Some(Self::Drop),
            "ORG" => Some(Self::Org),
            "PUSH" => Some(Self::Push),
            "POP" => Some(Self::Pop),
            "OPSYN" => Some(Self::Opsyn),
            "ENTRY" => Some(Self::Entry),
            "EXTRN" => Some(Self::Extrn),
            "WXTRN" => Some(Self::Wxtrn),
            "COPY" => Some(Self::Copy),
            "LTORG" => Some(Self::Ltorg),
            "CNOP" => Some(Self::Cnop),
            "AMODE" => Some(Self::Amode),
            "RMODE" => Some(Self::Rmode),
            "PRINT" => Some(Self::Print),
            "TITLE" => Some(Self::Title),
            "EJECT" => Some(Self::Eject),
            "SPACE" => Some(Self::Space),
            "PUNCH" => Some(Self::Punch),
            "REPRO" => Some(Self::Repro),
            "ACONTROL" => Some(Self::Acontrol),
            "ADATA" => Some(Self::Adata),
            "AINSERT" => Some(Self::Ainsert),
            "EXITCTL" => Some(Self::Exitctl),
            "XATTR" => Some(Self::Xattr),
            "CATTR" => Some(Self::Cattr),
            "ALIAS" => Some(Self::Alias),
            "MACRO" => Some(Self::Macro),
            "MEND" => Some(Self::Mend),
            "MEXIT" => Some(Self::Mexit),
            "MNOTE" => Some(Self::Mnote),
            "LCLA" => Some(Self::Lcla),
            "LCLB" => Some(Self::Lclb),
            "LCLC" => Some(Self::Lclc),
            "GBLA" => Some(Self::Gbla),
            "GBLB" => Some(Self::Gblb),
            "GBLC" => Some(Self::Gblc),
            "SETA" => Some(Self::Seta),
            "SETB" => Some(Self::Setb),
            "SETC" => Some(Self::Setc),
            "AIF" => Some(Self::Aif),
            "AGO" => Some(Self::Ago),
            "ANOP" => Some(Self::Anop),
            "ACTR" => Some(Self::Actr),
            _ => None,
        }
    }

    /// Whether this directive is a machine-time directive (vs. conditional-assembly).
    pub fn is_machine_time(&self) -> bool {
        matches!(
            self,
            Self::Csect
                | Self::Dsect
                | Self::Rsect
                | Self::Com
                | Self::Dxd
                | Self::Start
                | Self::End
                | Self::Loctr
                | Self::Dc
                | Self::Ds
                | Self::Ccw
                | Self::Ccw0
                | Self::Ccw1
                | Self::Equ
                | Self::Using
                | Self::Drop
                | Self::Org
                | Self::Push
                | Self::Pop
                | Self::Opsyn
                | Self::Entry
                | Self::Extrn
                | Self::Wxtrn
                | Self::Copy
                | Self::Ltorg
                | Self::Cnop
                | Self::Amode
                | Self::Rmode
                | Self::Print
                | Self::Title
                | Self::Eject
                | Self::Space
                | Self::Punch
                | Self::Repro
                | Self::Acontrol
                | Self::Adata
                | Self::Ainsert
                | Self::Exitctl
                | Self::Xattr
                | Self::Cattr
                | Self::Alias
        )
    }
}

impl fmt::Display for Directive {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            Self::Csect => "CSECT",
            Self::Dsect => "DSECT",
            Self::Rsect => "RSECT",
            Self::Com => "COM",
            Self::Dxd => "DXD",
            Self::Start => "START",
            Self::End => "END",
            Self::Loctr => "LOCTR",
            Self::Dc => "DC",
            Self::Ds => "DS",
            Self::Ccw => "CCW",
            Self::Ccw0 => "CCW0",
            Self::Ccw1 => "CCW1",
            Self::Equ => "EQU",
            Self::Using => "USING",
            Self::Drop => "DROP",
            Self::Org => "ORG",
            Self::Push => "PUSH",
            Self::Pop => "POP",
            Self::Opsyn => "OPSYN",
            Self::Entry => "ENTRY",
            Self::Extrn => "EXTRN",
            Self::Wxtrn => "WXTRN",
            Self::Copy => "COPY",
            Self::Ltorg => "LTORG",
            Self::Cnop => "CNOP",
            Self::Amode => "AMODE",
            Self::Rmode => "RMODE",
            Self::Print => "PRINT",
            Self::Title => "TITLE",
            Self::Eject => "EJECT",
            Self::Space => "SPACE",
            Self::Punch => "PUNCH",
            Self::Repro => "REPRO",
            Self::Acontrol => "ACONTROL",
            Self::Adata => "ADATA",
            Self::Ainsert => "AINSERT",
            Self::Exitctl => "EXITCTL",
            Self::Xattr => "XATTR",
            Self::Cattr => "CATTR",
            Self::Alias => "ALIAS",
            Self::Macro => "MACRO",
            Self::Mend => "MEND",
            Self::Mexit => "MEXIT",
            Self::Mnote => "MNOTE",
            Self::Lcla => "LCLA",
            Self::Lclb => "LCLB",
            Self::Lclc => "LCLC",
            Self::Gbla => "GBLA",
            Self::Gblb => "GBLB",
            Self::Gblc => "GBLC",
            Self::Seta => "SETA",
            Self::Setb => "SETB",
            Self::Setc => "SETC",
            Self::Aif => "AIF",
            Self::Ago => "AGO",
            Self::Anop => "ANOP",
            Self::Actr => "ACTR",
        };
        write!(f, "{s}")
    }
}

// ---------------------------------------------------------------------------
//  DC / DS type codes
// ---------------------------------------------------------------------------

/// A DC/DS type code.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DcType {
    /// Address constant (A).
    A,
    /// Address double (AD).
    Ad,
    /// Binary (B).
    B,
    /// Character (C).
    C,
    /// Character (ASCII variant, CA).
    Ca,
    /// Character (EBCDIC variant, CE).
    Ce,
    /// Long floating-point (D / DH / DB / DD).
    D,
    /// Short floating-point (E / EH / EB / ED).
    E,
    /// Fullword fixed-point (F / FD).
    F,
    /// Graphic DBCS (G).
    G,
    /// Halfword fixed-point (H).
    H,
    /// Packed decimal (P).
    P,
    /// External dummy section reference (Q / DXD offset).
    Q,
    /// Relocatable address (R).
    R,
    /// S-type address constant (S).
    S,
    /// V-type external address constant (V).
    V,
    /// Hexadecimal (X).
    X,
    /// Address halfword (Y).
    Y,
    /// Zoned decimal (Z).
    Z,
}

impl DcType {
    /// Parse a type code character.
    pub fn from_char(ch: char) -> Option<Self> {
        match ch.to_ascii_uppercase() {
            'A' => Some(Self::A),
            'B' => Some(Self::B),
            'C' => Some(Self::C),
            'D' => Some(Self::D),
            'E' => Some(Self::E),
            'F' => Some(Self::F),
            'G' => Some(Self::G),
            'H' => Some(Self::H),
            'P' => Some(Self::P),
            'Q' => Some(Self::Q),
            'R' => Some(Self::R),
            'S' => Some(Self::S),
            'V' => Some(Self::V),
            'X' => Some(Self::X),
            'Y' => Some(Self::Y),
            'Z' => Some(Self::Z),
            _ => None,
        }
    }

    /// Default length in bytes for this type.
    pub fn default_length(self) -> u32 {
        match self {
            Self::A | Self::Ad => 4,
            Self::B => 1,
            Self::C | Self::Ca | Self::Ce => 1,
            Self::D => 8,
            Self::E => 4,
            Self::F => 4,
            Self::G => 2,
            Self::H => 2,
            Self::P => 1,
            Self::Q => 4,
            Self::R => 4,
            Self::S => 2,
            Self::V => 4,
            Self::X => 1,
            Self::Y => 2,
            Self::Z => 1,
        }
    }

    /// Default alignment for this type.
    pub fn alignment(self) -> u32 {
        match self {
            Self::A | Self::Ad | Self::V | Self::Q | Self::R => 4,
            Self::F => 4,
            Self::H | Self::Y | Self::S => 2,
            Self::D => 8,
            Self::E => 4,
            _ => 1,
        }
    }

    /// The type attribute character for the symbol table.
    pub fn type_attr(self) -> char {
        match self {
            Self::A | Self::Ad => 'A',
            Self::B => 'B',
            Self::C | Self::Ca | Self::Ce => 'C',
            Self::D => 'D',
            Self::E => 'E',
            Self::F => 'F',
            Self::G => 'G',
            Self::H => 'H',
            Self::P => 'P',
            Self::Q => 'Q',
            Self::R => 'R',
            Self::S => 'S',
            Self::V => 'V',
            Self::X => 'X',
            Self::Y => 'Y',
            Self::Z => 'Z',
        }
    }
}

/// A parsed DC/DS operand.
#[derive(Debug, Clone)]
pub struct DcOperand {
    /// Duplication factor (default 1).
    pub duplication: u32,
    /// Type code.
    pub dc_type: DcType,
    /// Explicit length modifier (overrides default).
    pub length: Option<u32>,
    /// Scale modifier.
    pub scale: Option<i32>,
    /// Exponent modifier.
    pub exponent: Option<i32>,
    /// The nominal value string (between quotes, or address expression).
    pub value: String,
}

/// Parse a DC/DS operand string like `3F'100'`, `CL80' '`, `XL4'FF'`.
pub fn parse_dc_operand(operand: &str) -> Option<DcOperand> {
    let s = operand.trim();
    if s.is_empty() {
        return None;
    }

    let chars: Vec<char> = s.chars().collect();
    let mut i = 0;

    // Duplication factor.
    let mut duplication = 1u32;
    if i < chars.len() && chars[i].is_ascii_digit() {
        let start = i;
        while i < chars.len() && chars[i].is_ascii_digit() {
            i += 1;
        }
        duplication = s[start..i].parse().unwrap_or(1);
    }

    // Type code.
    if i >= chars.len() {
        return None;
    }
    let type_char = chars[i];
    let dc_type = DcType::from_char(type_char)?;
    i += 1;

    // Check for two-char type codes: AD, CA, CE, DB, DD, DH, EB, ED, EH, FD.
    if i < chars.len() && chars[i].is_ascii_alphabetic() && chars[i] != 'L' {
        // Two-char variant; skip the second char for now.
        i += 1;
    }

    // Length modifier: L followed by digits.
    let mut length = None;
    if i < chars.len() && chars[i].to_ascii_uppercase() == 'L' {
        i += 1;
        let start = i;
        while i < chars.len() && chars[i].is_ascii_digit() {
            i += 1;
        }
        if start < i {
            length = s[start..i].parse::<u32>().ok();
        }
    }

    // Scale modifier: S followed by digits.
    let mut scale = None;
    if i < chars.len() && chars[i].to_ascii_uppercase() == 'S' {
        i += 1;
        let start = i;
        if i < chars.len() && (chars[i] == '-' || chars[i] == '+') {
            i += 1;
        }
        while i < chars.len() && chars[i].is_ascii_digit() {
            i += 1;
        }
        if start < i {
            scale = s[start..i].parse::<i32>().ok();
        }
    }

    // Exponent modifier: E followed by digits.
    let mut exponent = None;
    if i < chars.len() && chars[i].to_ascii_uppercase() == 'E' && dc_type != DcType::E {
        i += 1;
        let start = i;
        if i < chars.len() && (chars[i] == '-' || chars[i] == '+') {
            i += 1;
        }
        while i < chars.len() && chars[i].is_ascii_digit() {
            i += 1;
        }
        if start < i {
            exponent = s[start..i].parse::<i32>().ok();
        }
    }

    // Nominal value in quotes.
    let mut value = String::new();
    if i < chars.len() && chars[i] == '\'' {
        i += 1; // skip opening quote
        while i < chars.len() {
            if chars[i] == '\'' {
                // Check for doubled quote (escaped).
                if i + 1 < chars.len() && chars[i + 1] == '\'' {
                    value.push('\'');
                    i += 2;
                } else {
                    // closing quote — i is not used after this
                    break;
                }
            } else {
                value.push(chars[i]);
                i += 1;
            }
        }
    } else if i < chars.len() && chars[i] == '(' {
        // Address expression in parens: A(expr).
        let start = i;
        let mut depth = 0;
        while i < chars.len() {
            if chars[i] == '(' {
                depth += 1;
            } else if chars[i] == ')' {
                depth -= 1;
                if depth == 0 {
                    i += 1;
                    break;
                }
            }
            i += 1;
        }
        value = s[start..i].to_string();
    }

    Some(DcOperand {
        duplication,
        dc_type,
        length,
        scale,
        exponent,
        value,
    })
}

/// Compute the total byte length of a DC/DS operand.
pub fn dc_byte_length(op: &DcOperand) -> u32 {
    let unit_len = op.length.unwrap_or_else(|| {
        if op.dc_type == DcType::C || op.dc_type == DcType::Ca || op.dc_type == DcType::Ce {
            // Character type: length = length of value string.
            if op.value.is_empty() { 1 } else { op.value.len() as u32 }
        } else if op.dc_type == DcType::X {
            // Hex: 2 hex digits = 1 byte.
            let hex_len = op.value.len() as u32;
            (hex_len + 1) / 2
        } else if op.dc_type == DcType::B {
            let bit_len = op.value.len() as u32;
            (bit_len + 7) / 8
        } else if op.dc_type == DcType::P || op.dc_type == DcType::Z {
            // Packed/Zoned: digits + sign.
            let digits = op.value.len() as u32;
            if op.dc_type == DcType::P {
                (digits + 2) / 2
            } else {
                digits.max(1)
            }
        } else {
            op.dc_type.default_length()
        }
    });
    op.duplication * unit_len
}

// ---------------------------------------------------------------------------
//  USING / DROP tracking
// ---------------------------------------------------------------------------

/// A USING association (base register <-> addressability range).
#[derive(Debug, Clone)]
pub struct UsingEntry {
    /// The label or address establishing addressability.
    pub base_address: i64,
    /// The base register (0-15).
    pub register: u8,
    /// Optional label qualifier (for labeled USING).
    pub label: Option<String>,
    /// Range end (base + 4096 for ordinary USING).
    pub range_end: i64,
}

/// USING/DROP state manager.
#[derive(Debug, Default)]
pub struct UsingTable {
    entries: Vec<UsingEntry>,
    /// Stack for PUSH/POP USING.
    stack: Vec<Vec<UsingEntry>>,
}

impl UsingTable {
    pub fn new() -> Self {
        Self::default()
    }

    /// Establish a USING: `USING addr,reg`.
    pub fn establish(&mut self, base_address: i64, register: u8, label: Option<String>) {
        // Remove any existing USING for this register.
        self.entries.retain(|e| e.register != register || e.label != label);
        self.entries.push(UsingEntry {
            base_address,
            register,
            label,
            range_end: base_address + 4096,
        });
    }

    /// Drop a USING for a register.
    pub fn drop_register(&mut self, register: u8) {
        self.entries.retain(|e| e.register != register);
    }

    /// Drop all USINGs.
    pub fn drop_all(&mut self) {
        self.entries.clear();
    }

    /// Resolve a symbol address to a base-displacement pair.
    /// Returns `(base_register, displacement)` or `None` if no USING covers the address.
    pub fn resolve(&self, address: i64) -> Option<(u8, u32)> {
        // Find the best (smallest displacement) USING that covers the address.
        let mut best: Option<(u8, u32)> = None;
        for entry in &self.entries {
            if address >= entry.base_address && address < entry.range_end {
                let disp = (address - entry.base_address) as u32;
                if best.map_or(true, |(_, d)| disp < d) {
                    best = Some((entry.register, disp));
                }
            }
        }
        best
    }

    /// PUSH current USING state.
    pub fn push(&mut self) {
        self.stack.push(self.entries.clone());
    }

    /// POP saved USING state.
    pub fn pop(&mut self) {
        if let Some(saved) = self.stack.pop() {
            self.entries = saved;
        }
    }

    /// List current USING entries.
    pub fn entries(&self) -> &[UsingEntry] {
        &self.entries
    }
}

// ---------------------------------------------------------------------------
//  Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_directive_from_opcode() {
        assert_eq!(Directive::from_opcode("CSECT"), Some(Directive::Csect));
        assert_eq!(Directive::from_opcode("dc"), Some(Directive::Dc));
        assert_eq!(Directive::from_opcode("EQU"), Some(Directive::Equ));
        assert_eq!(Directive::from_opcode("USING"), Some(Directive::Using));
        assert_eq!(Directive::from_opcode("DROP"), Some(Directive::Drop));
        assert_eq!(Directive::from_opcode("LTORG"), Some(Directive::Ltorg));
        assert_eq!(Directive::from_opcode("NOSUCH"), None);
    }

    #[test]
    fn test_directive_is_machine_time() {
        assert!(Directive::Csect.is_machine_time());
        assert!(Directive::Dc.is_machine_time());
        assert!(!Directive::Seta.is_machine_time());
        assert!(!Directive::Aif.is_machine_time());
    }

    #[test]
    fn test_directive_display() {
        assert_eq!(format!("{}", Directive::Csect), "CSECT");
        assert_eq!(format!("{}", Directive::Dc), "DC");
    }

    #[test]
    fn test_dc_type_from_char() {
        assert_eq!(DcType::from_char('F'), Some(DcType::F));
        assert_eq!(DcType::from_char('c'), Some(DcType::C));
        assert_eq!(DcType::from_char('P'), Some(DcType::P));
        assert_eq!(DcType::from_char('!'), None);
    }

    #[test]
    fn test_dc_type_default_length() {
        assert_eq!(DcType::F.default_length(), 4);
        assert_eq!(DcType::H.default_length(), 2);
        assert_eq!(DcType::D.default_length(), 8);
        assert_eq!(DcType::C.default_length(), 1);
    }

    #[test]
    fn test_dc_type_alignment() {
        assert_eq!(DcType::F.alignment(), 4);
        assert_eq!(DcType::D.alignment(), 8);
        assert_eq!(DcType::H.alignment(), 2);
        assert_eq!(DcType::C.alignment(), 1);
    }

    #[test]
    fn test_parse_dc_fullword() {
        let op = parse_dc_operand("F'100'").unwrap();
        assert_eq!(op.dc_type, DcType::F);
        assert_eq!(op.duplication, 1);
        assert_eq!(op.value, "100");
        assert_eq!(op.length, None);
    }

    #[test]
    fn test_parse_dc_with_duplication() {
        let op = parse_dc_operand("3F'0'").unwrap();
        assert_eq!(op.duplication, 3);
        assert_eq!(op.dc_type, DcType::F);
        assert_eq!(op.value, "0");
    }

    #[test]
    fn test_parse_dc_character_with_length() {
        let op = parse_dc_operand("CL80' '").unwrap();
        assert_eq!(op.dc_type, DcType::C);
        assert_eq!(op.length, Some(80));
        assert_eq!(op.value, " ");
    }

    #[test]
    fn test_parse_dc_hex() {
        let op = parse_dc_operand("XL4'FFFFFFFF'").unwrap();
        assert_eq!(op.dc_type, DcType::X);
        assert_eq!(op.length, Some(4));
        assert_eq!(op.value, "FFFFFFFF");
    }

    #[test]
    fn test_parse_dc_address() {
        let op = parse_dc_operand("A(MYENTRY)").unwrap();
        assert_eq!(op.dc_type, DcType::A);
        assert_eq!(op.value, "(MYENTRY)");
    }

    #[test]
    fn test_parse_dc_packed() {
        let op = parse_dc_operand("PL3'123'").unwrap();
        assert_eq!(op.dc_type, DcType::P);
        assert_eq!(op.length, Some(3));
        assert_eq!(op.value, "123");
    }

    #[test]
    fn test_dc_byte_length_fullword() {
        let op = parse_dc_operand("F'100'").unwrap();
        assert_eq!(dc_byte_length(&op), 4);
    }

    #[test]
    fn test_dc_byte_length_with_duplication() {
        let op = parse_dc_operand("3F'0'").unwrap();
        assert_eq!(dc_byte_length(&op), 12);
    }

    #[test]
    fn test_dc_byte_length_character() {
        let op = parse_dc_operand("CL80' '").unwrap();
        assert_eq!(dc_byte_length(&op), 80);
    }

    #[test]
    fn test_dc_byte_length_hex() {
        let op = parse_dc_operand("X'FF'").unwrap();
        assert_eq!(dc_byte_length(&op), 1);
    }

    #[test]
    fn test_using_table_establish_and_resolve() {
        let mut ut = UsingTable::new();
        ut.establish(0x1000, 12, None);
        let (reg, disp) = ut.resolve(0x1100).unwrap();
        assert_eq!(reg, 12);
        assert_eq!(disp, 0x100);
    }

    #[test]
    fn test_using_table_out_of_range() {
        let mut ut = UsingTable::new();
        ut.establish(0x1000, 12, None);
        // Address 0x2000 is beyond base + 4096.
        assert!(ut.resolve(0x2000).is_none());
    }

    #[test]
    fn test_using_table_best_fit() {
        let mut ut = UsingTable::new();
        ut.establish(0x1000, 12, None);
        ut.establish(0x1800, 11, None);
        // Address 0x1900 is closer to R11's base (0x100) than R12's (0x900).
        let (reg, disp) = ut.resolve(0x1900).unwrap();
        assert_eq!(reg, 11);
        assert_eq!(disp, 0x100);
    }

    #[test]
    fn test_using_table_drop() {
        let mut ut = UsingTable::new();
        ut.establish(0x1000, 12, None);
        ut.drop_register(12);
        assert!(ut.resolve(0x1100).is_none());
    }

    #[test]
    fn test_using_table_push_pop() {
        let mut ut = UsingTable::new();
        ut.establish(0x1000, 12, None);
        ut.push();
        ut.drop_register(12);
        assert!(ut.resolve(0x1100).is_none());
        ut.pop();
        assert!(ut.resolve(0x1100).is_some());
    }

    #[test]
    fn test_using_table_drop_all() {
        let mut ut = UsingTable::new();
        ut.establish(0x1000, 12, None);
        ut.establish(0x2000, 11, None);
        ut.drop_all();
        assert!(ut.entries().is_empty());
    }
}
