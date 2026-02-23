//! # CICS System Programming (INQUIRE/SET)
//!
//! Query and modify CICS resource definitions at runtime:
//! - INQUIRE/SET PROGRAM
//! - INQUIRE/SET TRANSACTION
//! - INQUIRE/SET FILE
//! - INQUIRE/SET SYSTEM

use std::collections::HashMap;

// ─────────────────────── Resource Status ───────────────────────

/// Resource enable/disable status.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ResourceStatus {
    Enabled,
    Disabled,
}

/// Programming language.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ProgramLanguage {
    Cobol,
    Assembler,
    Pli,
    C,
    Java,
}

impl ProgramLanguage {
    /// Parse from string.
    pub fn parse_str(s: &str) -> Option<Self> {
        match s.to_uppercase().as_str() {
            "COBOL" => Some(Self::Cobol),
            "ASSEMBLER" | "ASM" => Some(Self::Assembler),
            "PLI" | "PL/I" => Some(Self::Pli),
            "C" => Some(Self::C),
            "JAVA" => Some(Self::Java),
            _ => None,
        }
    }

    /// To string.
    pub fn as_str(&self) -> &str {
        match self {
            Self::Cobol => "COBOL",
            Self::Assembler => "ASSEMBLER",
            Self::Pli => "PLI",
            Self::C => "C",
            Self::Java => "JAVA",
        }
    }
}

/// File open status.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OpenStatus {
    Open,
    Closed,
    Closing,
}

// ─────────────────────── Program Definition ───────────────────────

/// A CICS program definition.
#[derive(Debug, Clone)]
pub struct ProgramDef {
    /// Program name.
    pub name: String,
    /// Status.
    pub status: ResourceStatus,
    /// Language.
    pub language: ProgramLanguage,
    /// Use count.
    pub use_count: u32,
    /// Whether NEWCOPY is pending.
    pub newcopy_pending: bool,
}

// ─────────────────────── Transaction Definition ───────────────────────

/// A CICS transaction definition.
#[derive(Debug, Clone)]
pub struct TransactionDef {
    /// Transaction ID (4 chars).
    pub tranid: String,
    /// Associated program.
    pub program: String,
    /// Status.
    pub status: ResourceStatus,
    /// Priority.
    pub priority: u8,
}

// ─────────────────────── File Definition ───────────────────────

/// A CICS file definition.
#[derive(Debug, Clone)]
pub struct FileDef {
    /// File name.
    pub name: String,
    /// Dataset name.
    pub dsname: String,
    /// Open status.
    pub open_status: OpenStatus,
    /// Enable status.
    pub status: ResourceStatus,
    /// Read-only flag.
    pub read_only: bool,
}

// ─────────────────────── System Settings ───────────────────────

/// CICS system-level settings.
#[derive(Debug, Clone)]
pub struct SystemSettings {
    /// Maximum concurrent tasks.
    pub max_tasks: u32,
    /// Current active task count.
    pub active_tasks: u32,
    /// System ID (4 chars).
    pub sysid: String,
    /// CICS region name.
    pub applid: String,
    /// Maximum open TCBs.
    pub max_open_tcbs: u32,
}

impl SystemSettings {
    /// Create default system settings.
    pub fn new(sysid: &str) -> Self {
        Self {
            max_tasks: 100,
            active_tasks: 0,
            sysid: sysid.to_uppercase(),
            applid: sysid.to_uppercase(),
            max_open_tcbs: 50,
        }
    }
}

// ─────────────────────── System Programming Interface ───────────────────────

/// CICS System Programming Interface for INQUIRE/SET.
#[derive(Debug)]
pub struct SystemProgrammingInterface {
    programs: HashMap<String, ProgramDef>,
    transactions: HashMap<String, TransactionDef>,
    files: HashMap<String, FileDef>,
    system: SystemSettings,
}

impl SystemProgrammingInterface {
    /// Create a new SPI.
    pub fn new(sysid: &str) -> Self {
        Self {
            programs: HashMap::new(),
            transactions: HashMap::new(),
            files: HashMap::new(),
            system: SystemSettings::new(sysid),
        }
    }

    // ─── Program operations ───

    /// Install a program definition.
    pub fn install_program(&mut self, def: ProgramDef) {
        self.programs.insert(def.name.to_uppercase(), def);
    }

    /// INQUIRE PROGRAM.
    pub fn inquire_program(&self, name: &str) -> Result<&ProgramDef, SpiError> {
        self.programs
            .get(&name.to_uppercase())
            .ok_or_else(|| SpiError::ProgramNotFound(name.to_uppercase()))
    }

    /// SET PROGRAM STATUS.
    pub fn set_program_status(
        &mut self,
        name: &str,
        status: ResourceStatus,
    ) -> Result<(), SpiError> {
        let upper = name.to_uppercase();
        let prog = self
            .programs
            .get_mut(&upper)
            .ok_or(SpiError::ProgramNotFound(upper))?;
        prog.status = status;
        Ok(())
    }

    /// SET PROGRAM NEWCOPY.
    pub fn set_program_newcopy(&mut self, name: &str) -> Result<(), SpiError> {
        let upper = name.to_uppercase();
        let prog = self
            .programs
            .get_mut(&upper)
            .ok_or(SpiError::ProgramNotFound(upper))?;
        prog.newcopy_pending = true;
        Ok(())
    }

    // ─── Transaction operations ───

    /// Install a transaction definition.
    pub fn install_transaction(&mut self, def: TransactionDef) {
        self.transactions.insert(def.tranid.to_uppercase(), def);
    }

    /// INQUIRE TRANSACTION.
    pub fn inquire_transaction(&self, tranid: &str) -> Result<&TransactionDef, SpiError> {
        self.transactions
            .get(&tranid.to_uppercase())
            .ok_or_else(|| SpiError::TransactionNotFound(tranid.to_uppercase()))
    }

    /// SET TRANSACTION STATUS.
    pub fn set_transaction_status(
        &mut self,
        tranid: &str,
        status: ResourceStatus,
    ) -> Result<(), SpiError> {
        let upper = tranid.to_uppercase();
        let txn = self
            .transactions
            .get_mut(&upper)
            .ok_or(SpiError::TransactionNotFound(upper))?;
        txn.status = status;
        Ok(())
    }

    // ─── File operations ───

    /// Install a file definition.
    pub fn install_file(&mut self, def: FileDef) {
        self.files.insert(def.name.to_uppercase(), def);
    }

    /// INQUIRE FILE.
    pub fn inquire_file(&self, name: &str) -> Result<&FileDef, SpiError> {
        self.files
            .get(&name.to_uppercase())
            .ok_or_else(|| SpiError::FileNotFound(name.to_uppercase()))
    }

    /// SET FILE STATUS (enable/disable).
    pub fn set_file_status(
        &mut self,
        name: &str,
        status: ResourceStatus,
    ) -> Result<(), SpiError> {
        let upper = name.to_uppercase();
        let file = self
            .files
            .get_mut(&upper)
            .ok_or(SpiError::FileNotFound(upper))?;
        file.status = status;
        Ok(())
    }

    /// SET FILE OPEN/CLOSE.
    pub fn set_file_open_status(
        &mut self,
        name: &str,
        open: OpenStatus,
    ) -> Result<(), SpiError> {
        let upper = name.to_uppercase();
        let file = self
            .files
            .get_mut(&upper)
            .ok_or(SpiError::FileNotFound(upper))?;
        file.open_status = open;
        Ok(())
    }

    // ─── System operations ───

    /// INQUIRE SYSTEM.
    pub fn inquire_system(&self) -> &SystemSettings {
        &self.system
    }

    /// SET SYSTEM MAXTASKS.
    pub fn set_max_tasks(&mut self, max: u32) {
        self.system.max_tasks = max;
    }

    /// SET SYSTEM MAX OPEN TCBS.
    pub fn set_max_open_tcbs(&mut self, max: u32) {
        self.system.max_open_tcbs = max;
    }

    /// Update active task count (called internally).
    pub fn update_active_tasks(&mut self, count: u32) {
        self.system.active_tasks = count;
    }
}

/// System Programming Interface errors.
#[derive(Debug, thiserror::Error)]
pub enum SpiError {
    #[error("program not found: {0}")]
    ProgramNotFound(String),
    #[error("transaction not found: {0}")]
    TransactionNotFound(String),
    #[error("file not found: {0}")]
    FileNotFound(String),
}

// ─────────────────────── Tests ───────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    fn setup_spi() -> SystemProgrammingInterface {
        let mut spi = SystemProgrammingInterface::new("CICA");

        spi.install_program(ProgramDef {
            name: "EMPPROG".to_string(),
            status: ResourceStatus::Enabled,
            language: ProgramLanguage::Cobol,
            use_count: 0,
            newcopy_pending: false,
        });

        spi.install_transaction(TransactionDef {
            tranid: "EMPT".to_string(),
            program: "EMPPROG".to_string(),
            status: ResourceStatus::Enabled,
            priority: 5,
        });

        spi.install_file(FileDef {
            name: "CUSTFILE".to_string(),
            dsname: "PROD.CUST.VSAM".to_string(),
            open_status: OpenStatus::Open,
            status: ResourceStatus::Enabled,
            read_only: false,
        });

        spi
    }

    // ─── CICS-211.1: INQUIRE PROGRAM ───

    #[test]
    fn test_inquire_program() {
        let spi = setup_spi();
        let prog = spi.inquire_program("EMPPROG").unwrap();
        assert_eq!(prog.status, ResourceStatus::Enabled);
        assert_eq!(prog.language, ProgramLanguage::Cobol);
    }

    #[test]
    fn test_inquire_program_not_found() {
        let spi = setup_spi();
        assert!(spi.inquire_program("NOSUCH").is_err());
    }

    // ─── CICS-211.2: INQUIRE TRANSACTION ───

    #[test]
    fn test_inquire_transaction() {
        let spi = setup_spi();
        let txn = spi.inquire_transaction("EMPT").unwrap();
        assert_eq!(txn.program, "EMPPROG");
        assert_eq!(txn.status, ResourceStatus::Enabled);
    }

    #[test]
    fn test_inquire_transaction_not_found() {
        let spi = setup_spi();
        assert!(spi.inquire_transaction("XXXX").is_err());
    }

    // ─── CICS-211.3: INQUIRE FILE ───

    #[test]
    fn test_inquire_file() {
        let spi = setup_spi();
        let file = spi.inquire_file("CUSTFILE").unwrap();
        assert_eq!(file.dsname, "PROD.CUST.VSAM");
        assert_eq!(file.open_status, OpenStatus::Open);
    }

    #[test]
    fn test_inquire_file_not_found() {
        let spi = setup_spi();
        assert!(spi.inquire_file("NOSUCH").is_err());
    }

    // ─── CICS-211.4: SET PROGRAM/TRANSACTION/FILE ───

    #[test]
    fn test_set_program_newcopy() {
        let mut spi = setup_spi();
        spi.set_program_newcopy("EMPPROG").unwrap();
        let prog = spi.inquire_program("EMPPROG").unwrap();
        assert!(prog.newcopy_pending);
    }

    #[test]
    fn test_set_program_disabled() {
        let mut spi = setup_spi();
        spi.set_program_status("EMPPROG", ResourceStatus::Disabled)
            .unwrap();
        let prog = spi.inquire_program("EMPPROG").unwrap();
        assert_eq!(prog.status, ResourceStatus::Disabled);
    }

    #[test]
    fn test_set_file_disabled() {
        let mut spi = setup_spi();
        spi.set_file_status("CUSTFILE", ResourceStatus::Disabled)
            .unwrap();
        let file = spi.inquire_file("CUSTFILE").unwrap();
        assert_eq!(file.status, ResourceStatus::Disabled);
    }

    #[test]
    fn test_set_file_closed() {
        let mut spi = setup_spi();
        spi.set_file_open_status("CUSTFILE", OpenStatus::Closed)
            .unwrap();
        let file = spi.inquire_file("CUSTFILE").unwrap();
        assert_eq!(file.open_status, OpenStatus::Closed);
    }

    #[test]
    fn test_set_transaction_disabled() {
        let mut spi = setup_spi();
        spi.set_transaction_status("EMPT", ResourceStatus::Disabled)
            .unwrap();
        let txn = spi.inquire_transaction("EMPT").unwrap();
        assert_eq!(txn.status, ResourceStatus::Disabled);
    }

    // ─── CICS-211.5: INQUIRE/SET SYSTEM ───

    #[test]
    fn test_inquire_system() {
        let spi = setup_spi();
        let sys = spi.inquire_system();
        assert_eq!(sys.sysid, "CICA");
        assert_eq!(sys.max_tasks, 100);
    }

    #[test]
    fn test_set_system_maxtasks() {
        let mut spi = setup_spi();
        spi.set_max_tasks(200);
        let sys = spi.inquire_system();
        assert_eq!(sys.max_tasks, 200);
    }

    // ─── CICS-211.6: System Programming Integration Tests ───

    #[test]
    fn test_full_spi_workflow() {
        let mut spi = SystemProgrammingInterface::new("CICB");

        // Install resources.
        spi.install_program(ProgramDef {
            name: "PGM1".to_string(),
            status: ResourceStatus::Enabled,
            language: ProgramLanguage::Cobol,
            use_count: 0,
            newcopy_pending: false,
        });
        spi.install_program(ProgramDef {
            name: "PGM2".to_string(),
            status: ResourceStatus::Enabled,
            language: ProgramLanguage::Assembler,
            use_count: 0,
            newcopy_pending: false,
        });
        spi.install_transaction(TransactionDef {
            tranid: "TRN1".to_string(),
            program: "PGM1".to_string(),
            status: ResourceStatus::Enabled,
            priority: 10,
        });
        spi.install_file(FileDef {
            name: "FILE1".to_string(),
            dsname: "PROD.DATA".to_string(),
            open_status: OpenStatus::Open,
            status: ResourceStatus::Enabled,
            read_only: false,
        });

        // Query all.
        assert_eq!(
            spi.inquire_program("PGM1").unwrap().language,
            ProgramLanguage::Cobol
        );
        assert_eq!(
            spi.inquire_program("PGM2").unwrap().language,
            ProgramLanguage::Assembler
        );
        assert_eq!(
            spi.inquire_transaction("TRN1").unwrap().program,
            "PGM1"
        );

        // Modify.
        spi.set_program_newcopy("PGM1").unwrap();
        spi.set_file_status("FILE1", ResourceStatus::Disabled)
            .unwrap();
        spi.set_max_tasks(500);

        // Verify.
        assert!(spi.inquire_program("PGM1").unwrap().newcopy_pending);
        assert_eq!(
            spi.inquire_file("FILE1").unwrap().status,
            ResourceStatus::Disabled
        );
        assert_eq!(spi.inquire_system().max_tasks, 500);
    }

    #[test]
    fn test_language_parsing() {
        assert_eq!(ProgramLanguage::parse_str("COBOL"), Some(ProgramLanguage::Cobol));
        assert_eq!(ProgramLanguage::parse_str("ASM"), Some(ProgramLanguage::Assembler));
        assert_eq!(ProgramLanguage::parse_str("PLI"), Some(ProgramLanguage::Pli));
        assert_eq!(ProgramLanguage::parse_str("UNKNOWN"), None);
    }
}
