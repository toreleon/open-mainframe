//! # PDSE Extensions — Program Objects & Member Generations
//!
//! Extends PDS with PDSE-specific features:
//!
//! - **Program Objects** — AMODE/RMODE addressing mode attributes
//! - **Member Generations** — Version history with MAXGENS
//! - **Extended Attributes (EATTR)** — Large member and directory support
//! - **Enhanced Directory** — Class descriptors, deferred classes, alias info

use std::collections::{HashMap, VecDeque};

// ─────────────────────── Addressing Modes ───────────────────────

/// Addressing mode (AMODE) for program objects.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Amode {
    /// 24-bit addressing (below the line).
    Amode24,
    /// 31-bit addressing.
    Amode31,
    /// 64-bit addressing.
    Amode64,
    /// Any addressing mode (24 or 31).
    AmodeAny,
}

impl Amode {
    /// Display string.
    pub fn as_str(self) -> &'static str {
        match self {
            Amode::Amode24 => "24",
            Amode::Amode31 => "31",
            Amode::Amode64 => "64",
            Amode::AmodeAny => "ANY",
        }
    }
}

/// Residency mode (RMODE) for program objects.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Rmode {
    /// Must reside below 16MB line.
    Rmode24,
    /// Can reside anywhere in 31-bit address space.
    RmodeAny,
}

impl Rmode {
    /// Display string.
    pub fn as_str(self) -> &'static str {
        match self {
            Rmode::Rmode24 => "24",
            Rmode::RmodeAny => "ANY",
        }
    }
}

// ─────────────────────── Program Object ───────────────────────

/// A program object stored in a PDSE load library.
#[derive(Debug, Clone)]
pub struct ProgramObject {
    /// Member name (up to 8 characters).
    pub name: String,
    /// Addressing mode.
    pub amode: Amode,
    /// Residency mode.
    pub rmode: Rmode,
    /// Entry point offset.
    pub entry_point: u32,
    /// Program text/data.
    pub text: Vec<u8>,
    /// Class descriptors (section names).
    pub class_descriptors: Vec<String>,
    /// Deferred class names.
    pub deferred_classes: Vec<String>,
    /// Alias names for this program object.
    pub aliases: Vec<String>,
}

impl ProgramObject {
    /// Create a new program object.
    pub fn new(name: &str, amode: Amode, rmode: Rmode, text: Vec<u8>) -> Self {
        Self {
            name: name.to_uppercase(),
            amode,
            rmode,
            entry_point: 0,
            text,
            class_descriptors: Vec::new(),
            deferred_classes: Vec::new(),
            aliases: Vec::new(),
        }
    }

    /// Set the entry point offset.
    pub fn with_entry_point(mut self, offset: u32) -> Self {
        self.entry_point = offset;
        self
    }

    /// Add a class descriptor.
    pub fn with_class(mut self, class_name: &str) -> Self {
        self.class_descriptors.push(class_name.to_string());
        self
    }

    /// Add an alias.
    pub fn with_alias(mut self, alias: &str) -> Self {
        self.aliases.push(alias.to_uppercase());
        self
    }

    /// Size of the program object.
    pub fn size(&self) -> usize {
        self.text.len()
    }
}

// ─────────────────────── Member Generation ───────────────────────

/// A member generation (version snapshot).
#[derive(Debug, Clone)]
pub struct MemberGeneration {
    /// Generation number (0 = current, -1 = previous, etc.).
    pub generation: i32,
    /// Content at this generation.
    pub data: Vec<u8>,
    /// Timestamp of this generation.
    pub saved_at: u64,
}

/// Member generation manager for a single member.
#[derive(Debug, Clone)]
struct GenerationHistory {
    /// Maximum generations to retain (MAXGENS).
    max_gens: u32,
    /// Previous generations (most recent first).
    generations: VecDeque<Vec<u8>>,
    /// Current (active) version.
    current: Vec<u8>,
    /// Save counter for timestamps.
    save_count: u64,
}

impl GenerationHistory {
    fn new(max_gens: u32, initial_data: Vec<u8>) -> Self {
        Self {
            max_gens,
            generations: VecDeque::new(),
            current: initial_data,
            save_count: 1,
        }
    }

    /// Save a new version, pushing current to generations.
    fn save(&mut self, new_data: Vec<u8>) {
        // Push current to front of generations
        self.generations.push_front(self.current.clone());

        // Enforce MAXGENS limit
        while self.generations.len() > self.max_gens as usize {
            self.generations.pop_back();
        }

        self.current = new_data;
        self.save_count += 1;
    }

    /// Get a specific generation.
    /// 0 = current, -1 = previous, -2 = two saves ago, etc.
    fn get_generation(&self, gen: i32) -> Option<&Vec<u8>> {
        if gen == 0 {
            Some(&self.current)
        } else {
            let idx = (-gen - 1) as usize;
            self.generations.get(idx)
        }
    }

    /// Number of available generations (excluding current).
    fn generation_count(&self) -> usize {
        self.generations.len()
    }
}

// ─────────────────────── Extended Attributes ───────────────────────

/// PDSE extended attributes mode.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Eattr {
    /// No extended attributes (standard PDSE limits).
    No,
    /// Optional extended attributes — large members and directories.
    Opt,
}

// ─────────────────────── PDSE ───────────────────────

/// PDSE (Partitioned Data Set Extended) with program objects and generations.
#[derive(Debug)]
pub struct Pdse {
    /// PDSE name.
    pub name: String,
    /// Maximum member generations (MAXGENS parameter).
    pub max_gens: u32,
    /// Extended attribute mode.
    pub eattr: Eattr,
    /// Program objects indexed by member name.
    program_objects: HashMap<String, ProgramObject>,
    /// Member generation histories.
    member_gens: HashMap<String, GenerationHistory>,
    /// Data members (non-program-object content).
    data_members: HashMap<String, Vec<u8>>,
}

impl Pdse {
    /// Create a new PDSE.
    pub fn new(name: &str, max_gens: u32, eattr: Eattr) -> Self {
        Self {
            name: name.to_uppercase(),
            max_gens,
            eattr,
            program_objects: HashMap::new(),
            member_gens: HashMap::new(),
            data_members: HashMap::new(),
        }
    }

    /// Store a program object in the PDSE.
    pub fn store_program_object(&mut self, pgm: ProgramObject) {
        let name = pgm.name.clone();
        self.program_objects.insert(name, pgm);
    }

    /// Get a program object by name.
    pub fn get_program_object(&self, name: &str) -> Option<&ProgramObject> {
        self.program_objects.get(&name.to_uppercase())
    }

    /// List all program objects with their AMODE/RMODE.
    pub fn list_program_objects(&self) -> Vec<(&str, &str, &str)> {
        let mut result: Vec<_> = self
            .program_objects
            .values()
            .map(|p| (p.name.as_str(), p.amode.as_str(), p.rmode.as_str()))
            .collect();
        result.sort_by_key(|(name, _, _)| *name);
        result
    }

    /// Save a data member (with generation tracking).
    pub fn save_member(&mut self, name: &str, data: Vec<u8>) {
        let key = name.to_uppercase();

        if let Some(history) = self.member_gens.get_mut(&key) {
            history.save(data.clone());
        } else {
            self.member_gens
                .insert(key.clone(), GenerationHistory::new(self.max_gens, data.clone()));
        }

        self.data_members.insert(key, data);
    }

    /// Read a member at a specific generation.
    /// Generation 0 = current, -1 = previous, etc.
    pub fn read_member_generation(&self, name: &str, generation: i32) -> Option<&Vec<u8>> {
        let key = name.to_uppercase();
        let history = self.member_gens.get(&key)?;
        history.get_generation(generation)
    }

    /// Read current member data.
    pub fn read_member(&self, name: &str) -> Option<&Vec<u8>> {
        self.data_members.get(&name.to_uppercase())
    }

    /// Get the number of available generations for a member.
    pub fn generation_count(&self, name: &str) -> usize {
        self.member_gens
            .get(&name.to_uppercase())
            .map(|h| h.generation_count())
            .unwrap_or(0)
    }

    /// Total member count (data + program objects).
    pub fn member_count(&self) -> usize {
        let mut names: std::collections::HashSet<&str> = std::collections::HashSet::new();
        for k in self.data_members.keys() {
            names.insert(k.as_str());
        }
        for k in self.program_objects.keys() {
            names.insert(k.as_str());
        }
        names.len()
    }

    /// Check if EATTR=OPT allows large members.
    pub fn supports_large_members(&self) -> bool {
        self.eattr == Eattr::Opt
    }

    /// Check if EATTR=OPT allows large directory.
    pub fn supports_large_directory(&self) -> bool {
        self.eattr == Eattr::Opt
    }

    /// Maximum member size based on EATTR setting.
    pub fn max_member_size(&self) -> usize {
        match self.eattr {
            Eattr::No => 15 * 1024 * 1024, // 15 MB standard limit
            Eattr::Opt => usize::MAX,       // No limit
        }
    }

    /// Maximum directory entries based on EATTR setting.
    pub fn max_directory_entries(&self) -> usize {
        match self.eattr {
            Eattr::No => 65_535,
            Eattr::Opt => usize::MAX,
        }
    }
}

// ─────────────────────── Tests ───────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    // ─── DFSMS-108.1: Program Object Format ───

    #[test]
    fn test_program_object_amode_rmode() {
        let pgm = ProgramObject::new("MYPROG", Amode::Amode31, Rmode::RmodeAny, vec![0x47; 100]);

        assert_eq!(pgm.name, "MYPROG");
        assert_eq!(pgm.amode, Amode::Amode31);
        assert_eq!(pgm.rmode, Rmode::RmodeAny);
        assert_eq!(pgm.size(), 100);
    }

    #[test]
    fn test_program_object_in_pdse() {
        let mut pdse = Pdse::new("MY.LOADLIB", 0, Eattr::No);

        pdse.store_program_object(ProgramObject::new(
            "PROG1",
            Amode::Amode31,
            Rmode::RmodeAny,
            vec![0x47; 50],
        ));
        pdse.store_program_object(ProgramObject::new(
            "PROG2",
            Amode::Amode24,
            Rmode::Rmode24,
            vec![0x07; 30],
        ));

        let pgm1 = pdse.get_program_object("PROG1").unwrap();
        assert_eq!(pgm1.amode.as_str(), "31");
        assert_eq!(pgm1.rmode.as_str(), "ANY");

        let pgm2 = pdse.get_program_object("PROG2").unwrap();
        assert_eq!(pgm2.amode.as_str(), "24");
        assert_eq!(pgm2.rmode.as_str(), "24");
    }

    #[test]
    fn test_list_program_objects() {
        let mut pdse = Pdse::new("MY.LOADLIB", 0, Eattr::No);

        pdse.store_program_object(ProgramObject::new(
            "BPROG",
            Amode::Amode64,
            Rmode::RmodeAny,
            vec![],
        ));
        pdse.store_program_object(ProgramObject::new(
            "APROG",
            Amode::Amode31,
            Rmode::Rmode24,
            vec![],
        ));

        let list = pdse.list_program_objects();
        assert_eq!(list.len(), 2);
        assert_eq!(list[0], ("APROG", "31", "24"));
        assert_eq!(list[1], ("BPROG", "64", "ANY"));
    }

    #[test]
    fn test_amode_variants() {
        assert_eq!(Amode::Amode24.as_str(), "24");
        assert_eq!(Amode::Amode31.as_str(), "31");
        assert_eq!(Amode::Amode64.as_str(), "64");
        assert_eq!(Amode::AmodeAny.as_str(), "ANY");
    }

    // ─── DFSMS-108.2: Member Generations ───

    #[test]
    fn test_member_generations_maxgens_5() {
        let mut pdse = Pdse::new("MY.SOURCE", 5, Eattr::No);

        // Save member A 6 times
        for i in 0..6 {
            pdse.save_member("A", format!("version {}", i + 1).into_bytes());
        }

        // Current should be version 6
        let current = pdse.read_member("A").unwrap();
        assert_eq!(current, b"version 6");

        // Should have 5 previous generations
        assert_eq!(pdse.generation_count("A"), 5);

        // Generation -1 = version 5
        let gen_m1 = pdse.read_member_generation("A", -1).unwrap();
        assert_eq!(gen_m1, b"version 5");

        // Generation -2 = version 4
        let gen_m2 = pdse.read_member_generation("A", -2).unwrap();
        assert_eq!(gen_m2, b"version 4");

        // Generation -5 = version 1 (oldest kept)
        let gen_m5 = pdse.read_member_generation("A", -5).unwrap();
        assert_eq!(gen_m5, b"version 1");

        // Generation 0 = current (version 6)
        let gen_0 = pdse.read_member_generation("A", 0).unwrap();
        assert_eq!(gen_0, b"version 6");
    }

    #[test]
    fn test_maxgens_enforced() {
        let mut pdse = Pdse::new("MY.SOURCE", 3, Eattr::No);

        // Save 5 times with MAXGENS=3
        for i in 0..5 {
            pdse.save_member("B", format!("v{}", i + 1).into_bytes());
        }

        // Current = v5, generations = v4, v3, v2 (v1 deleted)
        assert_eq!(pdse.read_member("B").unwrap(), b"v5");
        assert_eq!(pdse.generation_count("B"), 3);

        assert_eq!(pdse.read_member_generation("B", -1).unwrap(), b"v4");
        assert_eq!(pdse.read_member_generation("B", -3).unwrap(), b"v2");

        // v1 should not be available
        assert!(pdse.read_member_generation("B", -4).is_none());
    }

    #[test]
    fn test_maxgens_zero() {
        let mut pdse = Pdse::new("MY.SOURCE", 0, Eattr::No);

        pdse.save_member("C", b"v1".to_vec());
        pdse.save_member("C", b"v2".to_vec());

        // No generations kept
        assert_eq!(pdse.generation_count("C"), 0);
        assert_eq!(pdse.read_member("C").unwrap(), b"v2");
        assert!(pdse.read_member_generation("C", -1).is_none());
    }

    // ─── DFSMS-108.3: Extended Attributes ───

    #[test]
    fn test_eattr_opt_large_members() {
        let pdse = Pdse::new("MY.LARGE", 0, Eattr::Opt);
        assert!(pdse.supports_large_members());
        assert_eq!(pdse.max_member_size(), usize::MAX);
    }

    #[test]
    fn test_eattr_no_standard_limits() {
        let pdse = Pdse::new("MY.STD", 0, Eattr::No);
        assert!(!pdse.supports_large_members());
        assert_eq!(pdse.max_member_size(), 15 * 1024 * 1024);
        assert_eq!(pdse.max_directory_entries(), 65_535);
    }

    #[test]
    fn test_eattr_opt_large_directory() {
        let pdse = Pdse::new("MY.BIGDIR", 0, Eattr::Opt);
        assert!(pdse.supports_large_directory());
        assert_eq!(pdse.max_directory_entries(), usize::MAX);
    }

    // ─── DFSMS-108.4: Directory Enhancements ───

    #[test]
    fn test_program_object_class_descriptors() {
        let pgm = ProgramObject::new("CLSPROG", Amode::Amode31, Rmode::RmodeAny, vec![0; 100])
            .with_class("B_TEXT")
            .with_class("B_IDRL")
            .with_entry_point(0x100);

        assert_eq!(pgm.class_descriptors, vec!["B_TEXT", "B_IDRL"]);
        assert_eq!(pgm.entry_point, 0x100);
    }

    #[test]
    fn test_program_object_aliases() {
        let pgm = ProgramObject::new("MAINPGM", Amode::Amode31, Rmode::RmodeAny, vec![])
            .with_alias("ALIAS1")
            .with_alias("ALIAS2");

        assert_eq!(pgm.aliases, vec!["ALIAS1", "ALIAS2"]);
    }

    #[test]
    fn test_program_object_deferred_class() {
        let mut pgm = ProgramObject::new("DEFPGM", Amode::Amode31, Rmode::RmodeAny, vec![]);
        pgm.deferred_classes.push("C_DEFERRED".to_string());

        assert_eq!(pgm.deferred_classes, vec!["C_DEFERRED"]);
    }

    // ─── DFSMS-108.5: Integration Tests ───

    #[test]
    fn test_pdse_maxgens_3_with_5_saves() {
        let mut pdse = Pdse::new("TEST.PDSE", 3, Eattr::No);

        pdse.save_member("MEMBER", b"save1".to_vec());
        pdse.save_member("MEMBER", b"save2".to_vec());
        pdse.save_member("MEMBER", b"save3".to_vec());
        pdse.save_member("MEMBER", b"save4".to_vec());
        pdse.save_member("MEMBER", b"save5".to_vec());

        // Current = save5, generations = save4, save3, save2
        assert_eq!(pdse.read_member("MEMBER").unwrap(), b"save5");
        assert_eq!(pdse.generation_count("MEMBER"), 3);
        assert_eq!(
            pdse.read_member_generation("MEMBER", -1).unwrap(),
            b"save4"
        );
        assert_eq!(
            pdse.read_member_generation("MEMBER", -2).unwrap(),
            b"save3"
        );
        assert_eq!(
            pdse.read_member_generation("MEMBER", -3).unwrap(),
            b"save2"
        );
        // save1 is gone
        assert!(pdse.read_member_generation("MEMBER", -4).is_none());
    }

    #[test]
    fn test_program_objects_all_amodes() {
        let mut pdse = Pdse::new("LOAD.LIB", 0, Eattr::No);

        let modes = [
            ("PGM24", Amode::Amode24, Rmode::Rmode24),
            ("PGM31", Amode::Amode31, Rmode::Rmode24),
            ("PGM64", Amode::Amode64, Rmode::RmodeAny),
            ("PGMANY", Amode::AmodeAny, Rmode::RmodeAny),
        ];

        for (name, amode, rmode) in &modes {
            pdse.store_program_object(ProgramObject::new(name, *amode, *rmode, vec![0; 10]));
        }

        for (name, amode, rmode) in &modes {
            let pgm = pdse.get_program_object(name).unwrap();
            assert_eq!(pgm.amode, *amode);
            assert_eq!(pgm.rmode, *rmode);
        }
    }

    #[test]
    fn test_mixed_data_and_program_objects() {
        let mut pdse = Pdse::new("MIXED.LIB", 2, Eattr::Opt);

        // Store program object
        pdse.store_program_object(ProgramObject::new(
            "PROG1",
            Amode::Amode31,
            Rmode::RmodeAny,
            vec![0x47, 0x00],
        ));

        // Store data members
        pdse.save_member("SOURCE1", b"IDENTIFICATION DIVISION.".to_vec());
        pdse.save_member("SOURCE2", b"DATA DIVISION.".to_vec());

        assert_eq!(pdse.member_count(), 3);
        assert!(pdse.get_program_object("PROG1").is_some());
        assert!(pdse.read_member("SOURCE1").is_some());
    }
}
