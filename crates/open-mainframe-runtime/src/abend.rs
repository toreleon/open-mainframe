//! # ABEND Code Framework & Dump Support
//!
//! Implements the z/OS ABEND diagnostic system:
//!
//! - **System ABEND codes** — S0Cx, S013, S222, S322, S806, S913, SB37, etc.
//! - **User ABEND codes** — U0000–U4095 with optional reason codes
//! - **Dump types** — SYSUDUMP (formatted), SYSABEND (extended), SYSMDUMP (binary)
//! - **SNAP dumps** — Non-terminating point-in-time diagnostic capture
//! - **Diagnostic messages** — IBM IEA/CSV convention messages

use std::collections::BTreeMap;
use std::fmt;

// ─────────────────────── ABEND Code ───────────────────────

/// An ABEND code — either system (Sxxx) or user (Unnnn).
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AbendCode {
    /// System ABEND (0x000–0xFFF, displayed as S0C1, S013, etc.).
    System(u16),
    /// User ABEND (0–4095).
    User(u16),
}

impl AbendCode {
    /// Create a system ABEND from a hex code.
    pub fn system(code: u16) -> Self {
        AbendCode::System(code)
    }

    /// Create a user ABEND. Panics if code > 4095.
    pub fn user(code: u16) -> Self {
        assert!(code <= 4095, "User ABEND code must be 0-4095, got {code}");
        AbendCode::User(code)
    }

    /// Format as z/OS display string (e.g., "S0C1", "U1234").
    pub fn display_code(&self) -> String {
        match self {
            AbendCode::System(code) => format!("S{code:03X}"),
            AbendCode::User(code) => format!("U{code:04}"),
        }
    }

    /// Whether this is a system ABEND.
    pub fn is_system(&self) -> bool {
        matches!(self, AbendCode::System(_))
    }

    /// Whether this is a user ABEND.
    pub fn is_user(&self) -> bool {
        matches!(self, AbendCode::User(_))
    }
}

impl fmt::Display for AbendCode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.display_code())
    }
}

// ─────────────────────── ABEND Information ───────────────────────

/// Complete ABEND event information.
#[derive(Debug, Clone)]
pub struct AbendInfo {
    /// ABEND code.
    pub code: AbendCode,
    /// Optional reason code.
    pub reason: Option<u32>,
    /// Description of the ABEND.
    pub description: String,
    /// Recovery hint.
    pub recovery_hint: String,
    /// Whether a dump was requested.
    pub dump_requested: bool,
    /// Step name where ABEND occurred.
    pub step_name: Option<String>,
    /// Program name where ABEND occurred.
    pub program_name: Option<String>,
}

impl AbendInfo {
    /// Create a new ABEND info from the registry.
    pub fn from_registry(code: AbendCode, reason: Option<u32>) -> Self {
        let (desc, hint) = ABEND_REGISTRY
            .lookup(&code)
            .map(|e| (e.description.to_string(), e.recovery_hint.to_string()))
            .unwrap_or_else(|| {
                (
                    format!("Unknown ABEND code {code}"),
                    "Check system documentation".to_string(),
                )
            });
        Self {
            code,
            reason,
            description: desc,
            recovery_hint: hint,
            dump_requested: true,
            step_name: None,
            program_name: None,
        }
    }
}

// ─────────────────────── System ABEND Registry ───────────────────────

/// A registered ABEND code entry.
#[derive(Debug, Clone)]
pub struct AbendRegistryEntry {
    /// ABEND code.
    pub code: AbendCode,
    /// Short description.
    pub description: &'static str,
    /// Recovery hint.
    pub recovery_hint: &'static str,
    /// IBM message ID associated with this ABEND.
    pub message_id: &'static str,
}

/// The system ABEND code registry.
pub struct AbendRegistry {
    entries: &'static [AbendRegistryEntry],
}

impl AbendRegistry {
    /// Look up an ABEND code in the registry.
    pub fn lookup(&self, code: &AbendCode) -> Option<&AbendRegistryEntry> {
        self.entries.iter().find(|e| e.code == *code)
    }

    /// Get all registered entries.
    pub fn all_entries(&self) -> &[AbendRegistryEntry] {
        self.entries
    }

    /// Number of registered codes.
    pub fn count(&self) -> usize {
        self.entries.len()
    }
}

/// Global ABEND registry with 30+ system ABEND codes.
pub static ABEND_REGISTRY: AbendRegistry = AbendRegistry {
    entries: &[
        // S0Cx — Program interruptions
        AbendRegistryEntry { code: AbendCode::System(0x0C1), description: "Operation exception — invalid instruction", recovery_hint: "Check program logic for invalid opcodes or branch to data", message_id: "IEA995I" },
        AbendRegistryEntry { code: AbendCode::System(0x0C2), description: "Privileged operation exception", recovery_hint: "Program attempted a privileged instruction in problem state", message_id: "IEA995I" },
        AbendRegistryEntry { code: AbendCode::System(0x0C3), description: "Execute exception — target is another EXECUTE", recovery_hint: "Remove nested EXECUTE instructions", message_id: "IEA995I" },
        AbendRegistryEntry { code: AbendCode::System(0x0C4), description: "Protection exception — invalid memory access", recovery_hint: "Check pointer validity, array bounds, and storage allocation", message_id: "IEA995I" },
        AbendRegistryEntry { code: AbendCode::System(0x0C5), description: "Addressing exception — address beyond real storage", recovery_hint: "Check for uninitialized or corrupted address pointers", message_id: "IEA995I" },
        AbendRegistryEntry { code: AbendCode::System(0x0C6), description: "Specification exception — invalid instruction operand", recovery_hint: "Check alignment requirements and operand specifications", message_id: "IEA995I" },
        AbendRegistryEntry { code: AbendCode::System(0x0C7), description: "Data exception — invalid packed decimal data", recovery_hint: "Initialize numeric fields, check for non-numeric data in packed fields", message_id: "IEA995I" },
        AbendRegistryEntry { code: AbendCode::System(0x0C8), description: "Fixed-point overflow exception", recovery_hint: "Check for arithmetic overflow in binary computations", message_id: "IEA995I" },
        AbendRegistryEntry { code: AbendCode::System(0x0C9), description: "Fixed-point divide exception — division by zero", recovery_hint: "Validate divisor is non-zero before division", message_id: "IEA995I" },
        AbendRegistryEntry { code: AbendCode::System(0x0CA), description: "Decimal overflow exception", recovery_hint: "Increase field size or check computation logic", message_id: "IEA995I" },
        AbendRegistryEntry { code: AbendCode::System(0x0CB), description: "Decimal divide exception — division by zero", recovery_hint: "Validate divisor is non-zero before decimal division", message_id: "IEA995I" },
        AbendRegistryEntry { code: AbendCode::System(0x0CC), description: "Exponent overflow exception", recovery_hint: "Check floating-point computations for overflow", message_id: "IEA995I" },
        AbendRegistryEntry { code: AbendCode::System(0x0CD), description: "Exponent underflow exception", recovery_hint: "Check floating-point computations for underflow", message_id: "IEA995I" },
        AbendRegistryEntry { code: AbendCode::System(0x0CE), description: "Significance exception", recovery_hint: "Floating-point significance lost in computation", message_id: "IEA995I" },
        AbendRegistryEntry { code: AbendCode::System(0x0CF), description: "Floating-point divide exception", recovery_hint: "Validate divisor is non-zero before floating-point division", message_id: "IEA995I" },

        // Data management ABENDs
        AbendRegistryEntry { code: AbendCode::System(0x013), description: "DCB open error — conflicting or missing DCB parameters", recovery_hint: "Check DD statement, RECFM, LRECL, BLKSIZE compatibility", message_id: "IEC141I" },
        AbendRegistryEntry { code: AbendCode::System(0x001), description: "I/O error — wait-state PSW in I/O operation", recovery_hint: "Check device status and SYNAD exit routine", message_id: "IEC020I" },
        AbendRegistryEntry { code: AbendCode::System(0x002), description: "I/O error — invalid SVC or bad SRB", recovery_hint: "Check system integrity and module levels", message_id: "IEA995I" },

        // Space ABENDs
        AbendRegistryEntry { code: AbendCode::System(0xB37), description: "End of volume — no space on volume and no more volumes", recovery_hint: "Increase primary/secondary allocation or add volumes", message_id: "IEC030I" },
        AbendRegistryEntry { code: AbendCode::System(0xD37), description: "End of volume — no secondary allocation specified", recovery_hint: "Add secondary allocation in SPACE parameter", message_id: "IEC031I" },
        AbendRegistryEntry { code: AbendCode::System(0xE37), description: "End of volume — maximum extents exceeded (16)", recovery_hint: "Use larger primary allocation or compress the dataset", message_id: "IEC032I" },

        // Job management ABENDs
        AbendRegistryEntry { code: AbendCode::System(0x222), description: "Operator CANCEL command issued", recovery_hint: "Job was canceled by operator or authorized user", message_id: "IEF450I" },
        AbendRegistryEntry { code: AbendCode::System(0x322), description: "Time limit exceeded — CPU or wait time exceeded", recovery_hint: "Increase TIME parameter on JOB/EXEC or optimize program", message_id: "IEF352I" },
        AbendRegistryEntry { code: AbendCode::System(0x122), description: "Region exceeded — insufficient storage for step", recovery_hint: "Increase REGION parameter on JOB/EXEC statement", message_id: "IEF352I" },
        AbendRegistryEntry { code: AbendCode::System(0x622), description: "Output limit exceeded — SYSOUT lines/pages exceeded", recovery_hint: "Increase OUTLIM or LINES parameter", message_id: "IEF450I" },

        // Module/load ABENDs
        AbendRegistryEntry { code: AbendCode::System(0x806), description: "Module not found — LOAD, LINK, or ATTACH failed", recovery_hint: "Verify module name and library concatenation (STEPLIB/JOBLIB)", message_id: "CSV003I" },
        AbendRegistryEntry { code: AbendCode::System(0x804), description: "Insufficient storage for GETMAIN/STORAGE OBTAIN", recovery_hint: "Increase REGION size or reduce storage requests", message_id: "IEA995I" },

        // Security ABENDs
        AbendRegistryEntry { code: AbendCode::System(0x913), description: "RACF authorization failure — access denied", recovery_hint: "Verify RACF profile permissions (READ/UPDATE/ALTER)", message_id: "ICH408I" },

        // Subsystem ABENDs
        AbendRegistryEntry { code: AbendCode::System(0x0D3), description: "VSAM logical error", recovery_hint: "Check VSAM cluster status and file disposition", message_id: "IEC070I" },
        AbendRegistryEntry { code: AbendCode::System(0x213), description: "Dataset not found on volume", recovery_hint: "Verify dataset name and volume serial in DD/catalog", message_id: "IEC143I" },
        AbendRegistryEntry { code: AbendCode::System(0x413), description: "OPEN failed — conflicting DISP or ENQ conflict", recovery_hint: "Check DISP parameter and verify no conflicting jobs hold the dataset", message_id: "IEC144I" },
        AbendRegistryEntry { code: AbendCode::System(0x613), description: "CLOSE failed for output dataset", recovery_hint: "Check I/O errors and device status", message_id: "IEC147I" },
        AbendRegistryEntry { code: AbendCode::System(0x714), description: "Catalog error during LOCATE", recovery_hint: "Run IDCAMS EXAMINE on the catalog", message_id: "IEC161I" },
        AbendRegistryEntry { code: AbendCode::System(0x80A), description: "Insufficient storage for GETMAIN above 16MB line", recovery_hint: "Increase REGION or use AMODE(31) for more storage", message_id: "IEA995I" },
    ],
};

// ─────────────────────── Register Set ───────────────────────

/// Simulated z/Architecture general-purpose register set (R0–R15).
#[derive(Debug, Clone)]
pub struct RegisterSet {
    /// General purpose registers R0–R15.
    pub gprs: [u64; 16],
    /// Program Status Word.
    pub psw: u64,
}

impl RegisterSet {
    /// Create a zeroed register set.
    pub fn new() -> Self {
        Self {
            gprs: [0; 16],
            psw: 0,
        }
    }
}

impl Default for RegisterSet {
    fn default() -> Self {
        Self::new()
    }
}

// ─────────────────────── Call Stack ───────────────────────

/// A call stack frame for traceback.
#[derive(Debug, Clone)]
pub struct StackFrame {
    /// Module name.
    pub module: String,
    /// Offset within module.
    pub offset: u32,
    /// Entry point name.
    pub entry_point: String,
}

// ─────────────────────── Dump Types ───────────────────────

/// Type of dump to produce.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DumpType {
    /// SYSUDUMP — formatted user dump.
    Sysudump,
    /// SYSABEND — extended dump with system areas.
    Sysabend,
    /// SYSMDUMP — unformatted machine-readable dump.
    Sysmdump,
}

/// A formatted dump output.
#[derive(Debug, Clone)]
pub struct FormattedDump {
    /// Dump type.
    pub dump_type: DumpType,
    /// ABEND code and reason.
    pub abend_code: String,
    /// Reason code.
    pub reason_code: Option<u32>,
    /// Register contents.
    pub registers: RegisterSet,
    /// Call stack traceback.
    pub traceback: Vec<StackFrame>,
    /// User storage areas (address → bytes).
    pub user_storage: BTreeMap<u64, Vec<u8>>,
    /// System areas (for SYSABEND).
    pub system_areas: Option<SystemAreas>,
    /// Diagnostic messages.
    pub messages: Vec<String>,
}

/// System areas included in SYSABEND dumps.
#[derive(Debug, Clone)]
pub struct SystemAreas {
    /// TCB (Task Control Block) contents.
    pub tcb: Vec<u8>,
    /// Recovery routine chain entries.
    pub estae_chain: Vec<String>,
    /// System trace entries.
    pub trace_entries: Vec<String>,
}

impl FormattedDump {
    /// Generate a SYSUDUMP.
    pub fn sysudump(info: &AbendInfo, regs: &RegisterSet, stack: &[StackFrame]) -> Self {
        let mut messages = Vec::new();
        messages.push(format!(
            "IEA995I SYMPTOM DUMP OUTPUT — ABEND={} REASON={:08X}",
            info.code,
            info.reason.unwrap_or(0)
        ));
        if let Some(pgm) = &info.program_name {
            messages.push(format!("IEA995I MODULE={pgm}"));
        }
        messages.push(format!("IEA995I {}", info.description));

        Self {
            dump_type: DumpType::Sysudump,
            abend_code: info.code.display_code(),
            reason_code: info.reason,
            registers: regs.clone(),
            traceback: stack.to_vec(),
            user_storage: BTreeMap::new(),
            system_areas: None,
            messages,
        }
    }

    /// Generate a SYSABEND (includes system areas).
    pub fn sysabend(
        info: &AbendInfo,
        regs: &RegisterSet,
        stack: &[StackFrame],
        sys_areas: SystemAreas,
    ) -> Self {
        let mut dump = Self::sysudump(info, regs, stack);
        dump.dump_type = DumpType::Sysabend;
        dump.system_areas = Some(sys_areas);
        dump
    }

    /// Add a user storage area to the dump.
    pub fn add_storage(&mut self, address: u64, data: Vec<u8>) {
        self.user_storage.insert(address, data);
    }

    /// Format the dump as text.
    pub fn format(&self) -> String {
        let mut out = String::new();

        // Header
        out.push_str(&format!(
            "=== {} DUMP ===\n",
            match self.dump_type {
                DumpType::Sysudump => "SYSUDUMP",
                DumpType::Sysabend => "SYSABEND",
                DumpType::Sysmdump => "SYSMDUMP",
            }
        ));
        out.push_str(&format!(
            "ABEND CODE: {} REASON: {:08X}\n\n",
            self.abend_code,
            self.reason_code.unwrap_or(0)
        ));

        // Messages
        for msg in &self.messages {
            out.push_str(msg);
            out.push('\n');
        }
        out.push('\n');

        // Registers
        out.push_str("GENERAL PURPOSE REGISTERS:\n");
        for i in (0..16).step_by(4) {
            out.push_str(&format!(
                "  R{:<2}={:016X}  R{:<2}={:016X}  R{:<2}={:016X}  R{:<2}={:016X}\n",
                i,
                self.registers.gprs[i],
                i + 1,
                self.registers.gprs[i + 1],
                i + 2,
                self.registers.gprs[i + 2],
                i + 3,
                self.registers.gprs[i + 3]
            ));
        }
        out.push_str(&format!("  PSW={:016X}\n\n", self.registers.psw));

        // Traceback
        if !self.traceback.is_empty() {
            out.push_str("TRACEBACK:\n");
            for (i, frame) in self.traceback.iter().enumerate() {
                out.push_str(&format!(
                    "  #{}: {} + {:04X} ({})\n",
                    i, frame.module, frame.offset, frame.entry_point
                ));
            }
            out.push('\n');
        }

        // System areas (SYSABEND only)
        if let Some(sys) = &self.system_areas {
            out.push_str("SYSTEM AREAS:\n");
            out.push_str(&format!("  TCB: {} bytes\n", sys.tcb.len()));
            if !sys.estae_chain.is_empty() {
                out.push_str("  ESTAE CHAIN:\n");
                for entry in &sys.estae_chain {
                    out.push_str(&format!("    {entry}\n"));
                }
            }
            if !sys.trace_entries.is_empty() {
                out.push_str("  SYSTEM TRACE:\n");
                for entry in &sys.trace_entries {
                    out.push_str(&format!("    {entry}\n"));
                }
            }
            out.push('\n');
        }

        // User storage
        if !self.user_storage.is_empty() {
            out.push_str("USER STORAGE:\n");
            for (addr, data) in &self.user_storage {
                out.push_str(&format!("  {:016X}: ", addr));
                for (i, byte) in data.iter().enumerate() {
                    if i > 0 && i % 16 == 0 {
                        out.push_str(&format!("\n  {:016X}: ", addr + i as u64));
                    }
                    out.push_str(&format!("{byte:02X} "));
                }
                out.push('\n');
            }
        }

        out
    }
}

// ─────────────────────── SYSMDUMP (Binary) ───────────────────────

/// Binary machine dump header.
#[derive(Debug, Clone)]
pub struct MdumpHeader {
    /// Dump title.
    pub title: String,
    /// Date string (YYYY-MM-DD).
    pub date: String,
    /// Time string (HH:MM:SS).
    pub time: String,
    /// ABEND code.
    pub abend_code: String,
    /// Reason code.
    pub reason_code: u32,
}

/// An unformatted machine dump (SYSMDUMP).
#[derive(Debug, Clone)]
pub struct MachineDump {
    /// Dump header.
    pub header: MdumpHeader,
    /// Raw storage sections (address → bytes).
    pub sections: BTreeMap<u64, Vec<u8>>,
    /// Register state.
    pub registers: RegisterSet,
}

impl MachineDump {
    /// Create a SYSMDUMP.
    pub fn new(info: &AbendInfo, regs: &RegisterSet) -> Self {
        Self {
            header: MdumpHeader {
                title: format!("DUMP FROM {} ABEND={}", info.program_name.as_deref().unwrap_or("UNKNOWN"), info.code),
                date: "2026-02-23".to_string(),
                time: "00:00:00".to_string(),
                abend_code: info.code.display_code(),
                reason_code: info.reason.unwrap_or(0),
            },
            sections: BTreeMap::new(),
            registers: regs.clone(),
        }
    }

    /// Add a storage section.
    pub fn add_section(&mut self, address: u64, data: Vec<u8>) {
        self.sections.insert(address, data);
    }

    /// Serialize to binary format.
    pub fn serialize(&self) -> Vec<u8> {
        let mut out = Vec::new();

        // Magic
        out.extend_from_slice(b"MDMP");
        // Version
        out.push(1);

        // Title (length-prefixed)
        let title_bytes = self.header.title.as_bytes();
        out.extend_from_slice(&(title_bytes.len() as u32).to_be_bytes());
        out.extend_from_slice(title_bytes);

        // ABEND code (length-prefixed)
        let code_bytes = self.header.abend_code.as_bytes();
        out.extend_from_slice(&(code_bytes.len() as u32).to_be_bytes());
        out.extend_from_slice(code_bytes);

        // Reason code
        out.extend_from_slice(&self.header.reason_code.to_be_bytes());

        // Registers (16 x 8 bytes + PSW)
        for gpr in &self.registers.gprs {
            out.extend_from_slice(&gpr.to_be_bytes());
        }
        out.extend_from_slice(&self.registers.psw.to_be_bytes());

        // Section count
        out.extend_from_slice(&(self.sections.len() as u32).to_be_bytes());

        // Sections
        for (addr, data) in &self.sections {
            out.extend_from_slice(&addr.to_be_bytes());
            out.extend_from_slice(&(data.len() as u32).to_be_bytes());
            out.extend_from_slice(data);
        }

        out
    }

    /// Deserialize from binary format.
    pub fn deserialize(bytes: &[u8]) -> Option<Self> {
        if bytes.len() < 5 || &bytes[0..4] != b"MDMP" {
            return None;
        }
        let mut pos = 5; // skip magic + version

        // Title
        if pos + 4 > bytes.len() { return None; }
        let title_len = u32::from_be_bytes(bytes[pos..pos + 4].try_into().ok()?) as usize;
        pos += 4;
        if pos + title_len > bytes.len() { return None; }
        let title = String::from_utf8(bytes[pos..pos + title_len].to_vec()).ok()?;
        pos += title_len;

        // ABEND code
        if pos + 4 > bytes.len() { return None; }
        let code_len = u32::from_be_bytes(bytes[pos..pos + 4].try_into().ok()?) as usize;
        pos += 4;
        if pos + code_len > bytes.len() { return None; }
        let abend_code = String::from_utf8(bytes[pos..pos + code_len].to_vec()).ok()?;
        pos += code_len;

        // Reason code
        if pos + 4 > bytes.len() { return None; }
        let reason_code = u32::from_be_bytes(bytes[pos..pos + 4].try_into().ok()?);
        pos += 4;

        // Registers
        let mut gprs = [0u64; 16];
        for gpr in &mut gprs {
            if pos + 8 > bytes.len() { return None; }
            *gpr = u64::from_be_bytes(bytes[pos..pos + 8].try_into().ok()?);
            pos += 8;
        }
        if pos + 8 > bytes.len() { return None; }
        let psw = u64::from_be_bytes(bytes[pos..pos + 8].try_into().ok()?);
        pos += 8;

        // Section count
        if pos + 4 > bytes.len() { return None; }
        let section_count = u32::from_be_bytes(bytes[pos..pos + 4].try_into().ok()?) as usize;
        pos += 4;

        let mut sections = BTreeMap::new();
        for _ in 0..section_count {
            if pos + 12 > bytes.len() { return None; }
            let addr = u64::from_be_bytes(bytes[pos..pos + 8].try_into().ok()?);
            pos += 8;
            let data_len = u32::from_be_bytes(bytes[pos..pos + 4].try_into().ok()?) as usize;
            pos += 4;
            if pos + data_len > bytes.len() { return None; }
            sections.insert(addr, bytes[pos..pos + data_len].to_vec());
            pos += data_len;
        }

        Some(Self {
            header: MdumpHeader {
                title,
                date: String::new(),
                time: String::new(),
                abend_code,
                reason_code,
            },
            sections,
            registers: RegisterSet { gprs, psw },
        })
    }
}

// ─────────────────────── SNAP Dump ───────────────────────

/// A SNAP dump — non-terminating point-in-time capture.
#[derive(Debug, Clone)]
pub struct SnapDump {
    /// SNAP ID for identification.
    pub id: u32,
    /// Register state at time of SNAP.
    pub registers: RegisterSet,
    /// Storage ranges captured.
    pub storage_ranges: Vec<(u64, Vec<u8>)>,
    /// Save area contents.
    pub save_area: Vec<u8>,
}

impl SnapDump {
    /// Create a SNAP dump.
    pub fn capture(id: u32, regs: &RegisterSet) -> Self {
        Self {
            id,
            registers: regs.clone(),
            storage_ranges: Vec::new(),
            save_area: Vec::new(),
        }
    }

    /// Add a storage range to capture.
    pub fn add_range(&mut self, from: u64, data: Vec<u8>) {
        self.storage_ranges.push((from, data));
    }

    /// Set the save area.
    pub fn set_save_area(&mut self, data: Vec<u8>) {
        self.save_area = data;
    }

    /// Format the SNAP dump.
    pub fn format(&self) -> String {
        let mut out = String::new();
        out.push_str(&format!("=== SNAP DUMP ID={:04} ===\n\n", self.id));

        // Registers
        out.push_str("REGISTERS:\n");
        for i in (0..16).step_by(4) {
            out.push_str(&format!(
                "  R{:<2}={:016X}  R{:<2}={:016X}  R{:<2}={:016X}  R{:<2}={:016X}\n",
                i,
                self.registers.gprs[i],
                i + 1,
                self.registers.gprs[i + 1],
                i + 2,
                self.registers.gprs[i + 2],
                i + 3,
                self.registers.gprs[i + 3]
            ));
        }
        out.push('\n');

        // Storage ranges
        for (addr, data) in &self.storage_ranges {
            out.push_str(&format!("STORAGE FROM {:016X} LEN {:08X}:\n", addr, data.len()));
            for chunk in data.chunks(16) {
                out.push_str("  ");
                for byte in chunk {
                    out.push_str(&format!("{byte:02X} "));
                }
                out.push('\n');
            }
        }

        out
    }
}

// ─────────────────────── Diagnostic Messages ───────────────────────

/// Format an IEA995I symptom dump message.
pub fn format_symptom_dump(info: &AbendInfo) -> String {
    let mut lines = Vec::new();
    lines.push("IEA995I SYMPTOM DUMP OUTPUT".to_string());
    lines.push(format!(
        "  SYSTEM COMPLETION CODE={} REASON CODE={:08X}",
        info.code,
        info.reason.unwrap_or(0)
    ));
    if let Some(pgm) = &info.program_name {
        lines.push(format!("  MODULE={pgm}"));
    }
    if let Some(step) = &info.step_name {
        lines.push(format!("  STEP={step}"));
    }
    lines.push(format!("  {}", info.description));
    lines.push(format!("  RECOVERY: {}", info.recovery_hint));
    lines.join("\n")
}

/// Format a CSV003I module-not-found message.
pub fn format_csv003i(module_name: &str) -> String {
    format!("CSV003I MODULE {module_name} NOT FOUND")
}

/// Format a step completion message with ABEND info.
pub fn format_step_completion(step_name: &str, info: &AbendInfo) -> String {
    format!(
        "IEF472I {step_name} - COMPLETION CODE - SYSTEM={} REASON={:08X}",
        info.code,
        info.reason.unwrap_or(0)
    )
}

// ─────────────────────── JCL Integration ───────────────────────

/// Step completion result for JCL conditional logic.
#[derive(Debug, Clone)]
pub struct StepCompletion {
    /// Step name.
    pub step_name: String,
    /// Whether step ABENDed.
    pub abended: bool,
    /// ABEND code (if abended).
    pub abend_code: Option<AbendCode>,
    /// Return code (if normal completion).
    pub return_code: Option<u16>,
    /// Reason code (if abended).
    pub reason_code: Option<u32>,
}

impl StepCompletion {
    /// Create a normal completion.
    pub fn normal(step_name: &str, rc: u16) -> Self {
        Self {
            step_name: step_name.to_string(),
            abended: false,
            abend_code: None,
            return_code: Some(rc),
            reason_code: None,
        }
    }

    /// Create an ABEND completion.
    pub fn abend(step_name: &str, code: AbendCode, reason: Option<u32>) -> Self {
        Self {
            step_name: step_name.to_string(),
            abended: true,
            abend_code: Some(code),
            return_code: None,
            reason_code: reason,
        }
    }

    /// Check if this step matches a COND test.
    /// COND=(code,op) — true means SKIP the step.
    pub fn cond_test(&self, code: u16, op: &str) -> bool {
        let rc = self.return_code.unwrap_or(0);
        match op {
            "EQ" => rc == code,
            "NE" => rc != code,
            "GT" => rc > code,
            "GE" => rc >= code,
            "LT" => rc < code,
            "LE" => rc <= code,
            _ => false,
        }
    }

    /// Format as job log line.
    pub fn job_log_line(&self) -> String {
        if self.abended {
            let code = self.abend_code.as_ref().map(|c| c.display_code()).unwrap_or_default();
            format!(
                "IEF472I {} - COMPLETION CODE - SYSTEM={} REASON={:08X}",
                self.step_name,
                code,
                self.reason_code.unwrap_or(0)
            )
        } else {
            format!(
                "IEF142I {} - STEP WAS EXECUTED - COND CODE {:04}",
                self.step_name,
                self.return_code.unwrap_or(0)
            )
        }
    }
}

// ─────────────────────── Tests ───────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    // ─── SYS-111.1: System ABEND Code Registry ───

    #[test]
    fn test_registry_has_30_plus_codes() {
        assert!(ABEND_REGISTRY.count() >= 30);
    }

    #[test]
    fn test_registry_lookup_s0c1() {
        let entry = ABEND_REGISTRY.lookup(&AbendCode::System(0x0C1)).unwrap();
        assert!(entry.description.contains("Operation exception"));
        assert_eq!(entry.message_id, "IEA995I");
    }

    #[test]
    fn test_registry_lookup_s0c4() {
        let entry = ABEND_REGISTRY.lookup(&AbendCode::System(0x0C4)).unwrap();
        assert!(entry.description.contains("Protection exception"));
    }

    #[test]
    fn test_registry_lookup_s0c7() {
        let entry = ABEND_REGISTRY.lookup(&AbendCode::System(0x0C7)).unwrap();
        assert!(entry.description.contains("Data exception"));
    }

    #[test]
    fn test_registry_lookup_s013() {
        let entry = ABEND_REGISTRY.lookup(&AbendCode::System(0x013)).unwrap();
        assert!(entry.description.contains("DCB"));
    }

    #[test]
    fn test_registry_lookup_s322() {
        let entry = ABEND_REGISTRY.lookup(&AbendCode::System(0x322)).unwrap();
        assert!(entry.description.contains("Time limit"));
    }

    #[test]
    fn test_registry_lookup_s806() {
        let entry = ABEND_REGISTRY.lookup(&AbendCode::System(0x806)).unwrap();
        assert!(entry.description.contains("Module not found"));
        assert_eq!(entry.message_id, "CSV003I");
    }

    #[test]
    fn test_registry_lookup_s913() {
        let entry = ABEND_REGISTRY.lookup(&AbendCode::System(0x913)).unwrap();
        assert!(entry.description.contains("RACF"));
    }

    #[test]
    fn test_registry_lookup_sb37() {
        let entry = ABEND_REGISTRY.lookup(&AbendCode::System(0xB37)).unwrap();
        assert!(entry.description.contains("volume"));
    }

    #[test]
    fn test_abend_code_display() {
        assert_eq!(AbendCode::System(0x0C1).display_code(), "S0C1");
        assert_eq!(AbendCode::System(0x0C4).display_code(), "S0C4");
        assert_eq!(AbendCode::System(0x013).display_code(), "S013");
        assert_eq!(AbendCode::System(0x322).display_code(), "S322");
        assert_eq!(AbendCode::System(0xB37).display_code(), "SB37");
    }

    #[test]
    fn test_registry_unknown_code() {
        assert!(ABEND_REGISTRY.lookup(&AbendCode::System(0xFFF)).is_none());
    }

    // ─── SYS-111.2: User ABEND Code Support ───

    #[test]
    fn test_user_abend_code() {
        let code = AbendCode::user(1234);
        assert_eq!(code.display_code(), "U1234");
        assert!(code.is_user());
        assert!(!code.is_system());
    }

    #[test]
    fn test_user_abend_with_reason() {
        let info = AbendInfo::from_registry(AbendCode::user(100), Some(0x0008));
        assert_eq!(info.code, AbendCode::User(100));
        assert_eq!(info.reason, Some(0x0008));
    }

    #[test]
    #[should_panic(expected = "User ABEND code must be 0-4095")]
    fn test_user_abend_max_4095() {
        AbendCode::user(4096);
    }

    #[test]
    fn test_user_abend_boundary() {
        let code = AbendCode::user(4095);
        assert_eq!(code.display_code(), "U4095");
        let code = AbendCode::user(0);
        assert_eq!(code.display_code(), "U0000");
    }

    // ─── SYS-111.3: JCL Step Completion Integration ───

    #[test]
    fn test_step_completion_normal() {
        let step = StepCompletion::normal("STEP1", 0);
        assert!(!step.abended);
        assert_eq!(step.return_code, Some(0));
        assert!(step.job_log_line().contains("COND CODE 0000"));
    }

    #[test]
    fn test_step_completion_abend() {
        let step = StepCompletion::abend("STEP2", AbendCode::System(0x0C4), Some(0x0010));
        assert!(step.abended);
        assert_eq!(step.abend_code.as_ref().unwrap().display_code(), "S0C4");
        assert!(step.job_log_line().contains("S0C4"));
    }

    #[test]
    fn test_cond_test() {
        let step = StepCompletion::normal("STEP1", 4);
        assert!(step.cond_test(0, "NE"));   // 4 != 0 → true
        assert!(!step.cond_test(0, "EQ"));  // 4 == 0 → false
        assert!(step.cond_test(4, "EQ"));   // 4 == 4 → true
        assert!(step.cond_test(3, "GT"));   // 4 > 3 → true
        assert!(step.cond_test(4, "GE"));   // 4 >= 4 → true
        assert!(step.cond_test(5, "LT"));   // 4 < 5 → true
        assert!(step.cond_test(4, "LE"));   // 4 <= 4 → true
    }

    // ─── SYS-111.4: SYSUDUMP ───

    #[test]
    fn test_sysudump_format() {
        let info = AbendInfo::from_registry(AbendCode::System(0x0C4), Some(0x0010));
        let mut regs = RegisterSet::new();
        regs.gprs[1] = 0x00FF_0000;
        regs.gprs[13] = 0x0000_1000;
        regs.psw = 0x0705_0000_0000_1234;

        let stack = vec![
            StackFrame { module: "MYPROG".into(), offset: 0x100, entry_point: "MAIN".into() },
            StackFrame { module: "CEEMAIN".into(), offset: 0x050, entry_point: "CEESTART".into() },
        ];

        let dump = FormattedDump::sysudump(&info, &regs, &stack);
        assert_eq!(dump.dump_type, DumpType::Sysudump);

        let text = dump.format();
        assert!(text.contains("SYSUDUMP"));
        assert!(text.contains("S0C4"));
        assert!(text.contains("0000000000FF0000"));
        assert!(text.contains("MYPROG"));
        assert!(text.contains("TRACEBACK"));
    }

    #[test]
    fn test_sysudump_messages() {
        let info = AbendInfo::from_registry(AbendCode::System(0x0C7), None);
        let regs = RegisterSet::new();
        let dump = FormattedDump::sysudump(&info, &regs, &[]);
        assert!(dump.messages.iter().any(|m| m.contains("IEA995I")));
    }

    // ─── SYS-111.5: SYSABEND ───

    #[test]
    fn test_sysabend_includes_system_areas() {
        let info = AbendInfo::from_registry(AbendCode::System(0x0C4), None);
        let regs = RegisterSet::new();
        let sys_areas = SystemAreas {
            tcb: vec![0; 128],
            estae_chain: vec!["ESTAE1 @ 0x1000".into()],
            trace_entries: vec!["SVC 13 AT 0x2000".into()],
        };

        let dump = FormattedDump::sysabend(&info, &regs, &[], sys_areas);
        assert_eq!(dump.dump_type, DumpType::Sysabend);
        assert!(dump.system_areas.is_some());

        let text = dump.format();
        assert!(text.contains("SYSABEND"));
        assert!(text.contains("ESTAE CHAIN"));
        assert!(text.contains("SYSTEM TRACE"));
        assert!(text.contains("TCB: 128 bytes"));
    }

    // ─── SYS-111.6: SYSMDUMP ───

    #[test]
    fn test_sysmdump_serialize_deserialize() {
        let info = AbendInfo::from_registry(AbendCode::System(0x0C4), Some(0x0010));
        let mut regs = RegisterSet::new();
        regs.gprs[0] = 0xDEADBEEF;
        regs.gprs[15] = 0xCAFEBABE;
        regs.psw = 0x0705_0000_0000_1234;

        let mut dump = MachineDump::new(&info, &regs);
        dump.add_section(0x1000, vec![0x47, 0x00, 0x10, 0x00]);
        dump.add_section(0x2000, vec![0xF0; 256]);

        let bytes = dump.serialize();
        assert_eq!(&bytes[0..4], b"MDMP");

        let restored = MachineDump::deserialize(&bytes).unwrap();
        assert_eq!(restored.header.abend_code, "S0C4");
        assert_eq!(restored.header.reason_code, 0x0010);
        assert_eq!(restored.registers.gprs[0], 0xDEADBEEF);
        assert_eq!(restored.registers.gprs[15], 0xCAFEBABE);
        assert_eq!(restored.registers.psw, 0x0705_0000_0000_1234);
        assert_eq!(restored.sections.len(), 2);
        assert_eq!(restored.sections[&0x1000], vec![0x47, 0x00, 0x10, 0x00]);
        assert_eq!(restored.sections[&0x2000].len(), 256);
    }

    #[test]
    fn test_sysmdump_invalid_magic() {
        assert!(MachineDump::deserialize(b"XXXX").is_none());
        assert!(MachineDump::deserialize(b"").is_none());
    }

    // ─── SYS-111.7: SNAP Dump ───

    #[test]
    fn test_snap_capture() {
        let mut regs = RegisterSet::new();
        regs.gprs[1] = 0x1234;
        regs.gprs[15] = 0xFFFF;

        let mut snap = SnapDump::capture(1, &regs);
        snap.add_range(0x1000, vec![0x47, 0x00, 0x10, 0x00]);
        snap.set_save_area(vec![0; 72]);

        assert_eq!(snap.id, 1);
        assert_eq!(snap.registers.gprs[1], 0x1234);
        assert_eq!(snap.storage_ranges.len(), 1);
        assert_eq!(snap.save_area.len(), 72);
    }

    #[test]
    fn test_snap_format() {
        let regs = RegisterSet::new();
        let mut snap = SnapDump::capture(42, &regs);
        snap.add_range(0x1000, vec![0xAB; 32]);

        let text = snap.format();
        assert!(text.contains("SNAP DUMP ID=0042"));
        assert!(text.contains("REGISTERS:"));
        assert!(text.contains("AB"));
    }

    #[test]
    fn test_multiple_snaps_different_ids() {
        let regs = RegisterSet::new();
        let snap1 = SnapDump::capture(1, &regs);
        let snap2 = SnapDump::capture(2, &regs);
        assert_ne!(snap1.id, snap2.id);
    }

    // ─── SYS-111.8: Diagnostic Messages & Integration ───

    #[test]
    fn test_symptom_dump_message() {
        let mut info = AbendInfo::from_registry(AbendCode::System(0x0C4), Some(0x0010));
        info.program_name = Some("MYPROG".into());
        info.step_name = Some("STEP1".into());

        let msg = format_symptom_dump(&info);
        assert!(msg.contains("IEA995I"));
        assert!(msg.contains("S0C4"));
        assert!(msg.contains("00000010"));
        assert!(msg.contains("MODULE=MYPROG"));
        assert!(msg.contains("STEP=STEP1"));
    }

    #[test]
    fn test_csv003i_message() {
        let msg = format_csv003i("BADMOD");
        assert_eq!(msg, "CSV003I MODULE BADMOD NOT FOUND");
    }

    #[test]
    fn test_step_completion_message() {
        let info = AbendInfo::from_registry(AbendCode::System(0x322), None);
        let msg = format_step_completion("RUNSTEP", &info);
        assert!(msg.contains("IEF472I"));
        assert!(msg.contains("RUNSTEP"));
        assert!(msg.contains("S322"));
    }

    #[test]
    fn test_three_step_jcl_scenario() {
        // Simulate 3-step JCL job
        let step1 = StepCompletion::normal("STEP1", 0);
        let step2 = StepCompletion::abend("STEP2", AbendCode::System(0x0C7), None);
        let step3 = StepCompletion::normal("STEP3", 0);

        // Job log
        let log1 = step1.job_log_line();
        let log2 = step2.job_log_line();
        let log3 = step3.job_log_line();

        assert!(log1.contains("COND CODE 0000"));
        assert!(log2.contains("S0C7"));
        assert!(log3.contains("COND CODE 0000"));

        // Conditional logic: IF (STEP2.ABEND) — should be true
        assert!(step2.abended);

        // COND=(0,NE) on STEP3 based on STEP1 — 0 == 0, so NE is false → don't skip
        assert!(!step1.cond_test(0, "NE"));
    }

    #[test]
    fn test_sysudump_with_storage() {
        let info = AbendInfo::from_registry(AbendCode::user(100), None);
        let regs = RegisterSet::new();
        let mut dump = FormattedDump::sysudump(&info, &regs, &[]);
        dump.add_storage(0x0000_1000, vec![0x41; 32]);
        dump.add_storage(0x0000_2000, vec![0x42; 16]);

        let text = dump.format();
        assert!(text.contains("USER STORAGE"));
        assert!(text.contains("0000000000001000"));
    }
}
