---
version: 'v5.0'
planningGroup: 'PG-22'
technology: 'System Initialization & PARMLIB Configuration'
date: '2026-02-21'
status: 'complete'
inputDocuments:
  - 'prd-syscfg-v5.0.md'
---

# Architecture: System Initialization & PARMLIB Configuration

## 1. Crate Strategy

**New crate:** `open-mainframe-parmlib`

Rationale: PARMLIB is a cross-cutting configuration framework used by every subsystem. A dedicated crate provides the member discovery, parsing framework, symbol substitution, and initialization orchestration that all other crates depend on. This avoids circular dependencies — subsystems register their member parsers with the framework rather than the framework importing every subsystem.

## 2. Module Layout

```
crates/open-mainframe-parmlib/src/
├── lib.rs              # Crate root, public API, ParmlibConfig
├── discovery.rs        # PARMLIB concatenation and member lookup
├── symbols.rs          # IEASYMxx parser, system symbol table, substitution engine
├── ieasys.rs           # IEASYSxx parser — master system parameters
├── members/
│   ├── mod.rs          # Member parser registry and trait
│   ├── lnklst.rs       # LNKLSTxx — linklist configuration
│   ├── prog.rs         # PROGxx — APF, linklist, LPA management
│   ├── consol.rs       # CONSOLxx — console definitions
│   ├── commnd.rs       # COMMNDxx — IPL commands
│   ├── ikjtso.rs       # IKJTSOxx — TSO parameters
│   └── alloc.rs        # ALLOCxx — allocation defaults
├── init.rs             # System initialization sequence orchestrator
├── operator.rs         # SET/SETPROG command handlers
└── error.rs            # ParmlibError types
```

## 3. Key Types

```rust
/// PARMLIB concatenation — ordered list of dataset locations
pub struct ParmlibConcat {
    pub datasets: Vec<PathBuf>,  // Directories representing PARMLIB PDSes
}

/// System symbol table (from IEASYMxx)
pub struct SymbolTable {
    symbols: HashMap<String, String>,  // e.g., "SYSNAME" → "SYS1"
}

impl SymbolTable {
    /// Substitute all &symbol. references in text
    pub fn substitute(&self, text: &str) -> String;
}

/// Master system parameters (from IEASYSxx)
pub struct SystemParameters {
    pub sysname: String,
    pub sysplex: String,
    pub lnklst_suffix: String,    // LNKLSTxx suffix
    pub lpa_suffix: String,
    pub smf_suffix: String,
    pub consol_suffix: String,
    pub commnd_suffix: String,
    pub ikjtso_suffix: String,
    pub alloc_suffix: String,
    pub prog_suffix: String,
    pub maxuser: u32,
    pub page_datasets: Vec<String>,
    pub csa_size: MemorySize,
    pub sqa_size: MemorySize,
    pub custom: HashMap<String, String>,  // Additional key=value pairs
}

/// Trait for subsystem-specific member parsers
pub trait ParmlibMemberParser: Send + Sync {
    /// Member base name (e.g., "SMFPRM", "BPXPRM")
    fn member_name(&self) -> &str;

    /// Parse raw member content with symbol substitution applied
    fn parse(&self, content: &str) -> Result<Box<dyn std::any::Any + Send>, ParmlibError>;
}

/// Member parser registry
pub struct ParmlibRegistry {
    parsers: HashMap<String, Box<dyn ParmlibMemberParser>>,
}

/// Linklist entry (from LNKLSTxx)
pub struct LinklistEntry {
    pub dsname: String,
    pub volume: Option<String>,
}

/// Console definition (from CONSOLxx)
pub struct ConsoleDefinition {
    pub name: String,
    pub unit: Option<String>,
    pub authority: ConsoleAuthority,
    pub routcodes: Vec<u16>,
    pub hardcopy: bool,
}

pub enum ConsoleAuthority {
    Master,
    All,
    Info,
    Io,
    Sys,
    Cons,
}

/// IPL command (from COMMNDxx)
pub struct IplCommand {
    pub command: String,
    pub phase: IplPhase,
}

pub enum IplPhase {
    DuringNip,   // Issued during NIP processing
    AfterNip,    // Issued after NIP completes
}

/// APF entry (from PROGxx)
pub struct ApfEntry {
    pub dsname: String,
    pub volume: Option<String>,
    pub sms_managed: bool,
}

/// TSO parameters (from IKJTSOxx)
pub struct TsoParameters {
    pub auth_commands: Vec<String>,
    pub auth_programs: Vec<String>,
    pub auth_call_libs: Vec<String>,
    pub logon_proc: String,
    pub send_params: SendParameters,
    pub help_datasets: Vec<String>,
}

/// System initialization orchestrator
pub struct SystemInitializer {
    parmlib: ParmlibConcat,
    symbols: SymbolTable,
    params: SystemParameters,
    registry: ParmlibRegistry,
}

impl SystemInitializer {
    /// Execute the full initialization sequence
    pub fn initialize(&mut self) -> Result<InitResult, ParmlibError>;
}
```

## 4. Design Decisions

### DD-5.0-CFG-01: Filesystem-Backed PARMLIB
**Decision:** PARMLIB datasets are represented as directories on the native filesystem. Each member is a file within the directory. This maps cleanly to the existing dataset crate's PDS model while allowing easy editing of configuration with standard tools. The concatenation order mirrors z/OS behavior — first match wins.

### DD-5.0-CFG-02: Registry Pattern for Member Parsers
**Decision:** Subsystem crates register their own PARMLIB member parsers with the central registry via the `ParmlibMemberParser` trait. This avoids the PARMLIB crate needing to import every subsystem crate. For example, the SMF crate registers `SmfPrmParser` which handles SMFPRMxx semantics. The PARMLIB crate only handles framework members (IEASYSxx, IEASYMxx, LNKLSTxx, PROGxx, CONSOLxx, COMMNDxx, IKJTSOxx, ALLOCxx).

### DD-5.0-CFG-03: Two-Phase Symbol Substitution
**Decision:** Symbol substitution happens in two phases: (1) IEASYMxx is read first to populate the symbol table with static definitions, (2) all subsequent member reads pass through the substitution engine before parsing. This matches real z/OS behavior where symbols like `&SYSNAME.` and `&SYSPLEX.` are resolved at read time.

### DD-5.0-CFG-04: Coexistence with zosmf.toml
**Decision:** The PARMLIB framework coexists with the existing zosmf.toml configuration. PARMLIB provides z/OS-compatible configuration for subsystems that need it; zosmf.toml continues to provide OpenMainframe-specific settings (HTTP port, TLS, etc.). Over time, subsystems may migrate their configuration from zosmf.toml into PARMLIB members for full z/OS fidelity.
