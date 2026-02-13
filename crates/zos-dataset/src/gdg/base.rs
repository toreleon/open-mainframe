//! GDG base definition and management.

use std::fs::{self, File, OpenOptions};
use std::io::{BufRead, BufReader, BufWriter, Read, Seek, SeekFrom, Write};
use std::path::{Path, PathBuf};

use crate::error::DatasetError;
use crate::gdg::generation::{GdgGeneration, GenerationNumber};

/// Information about a GDG for LISTCAT-style output.
#[derive(Debug, Clone)]
pub struct GdgListInfo {
    /// Base name.
    pub name: String,
    /// Generation limit.
    pub limit: u8,
    /// Scratch mode.
    pub scratch: bool,
    /// Empty allowed.
    pub empty: bool,
    /// Number of generations.
    pub generation_count: usize,
    /// Generation details.
    pub generations: Vec<GdgGenerationInfo>,
}

/// Information about a single generation.
#[derive(Debug, Clone)]
pub struct GdgGenerationInfo {
    /// Full dataset name.
    pub name: String,
    /// Generation number.
    pub number: GenerationNumber,
    /// Creation timestamp (Unix seconds).
    pub created: u64,
    /// Whether the file exists.
    pub exists: bool,
}

/// GDG base options.
#[derive(Debug, Clone)]
pub struct GdgOptions {
    /// Maximum number of generations to retain (1-255).
    pub limit: u8,
    /// If true, delete old generations when rolling off. If false, just uncatalog.
    pub scratch: bool,
    /// If true, allow empty GDG (no generations). If false, GDG must have at least one.
    pub empty: bool,
    /// If true, old generations are pushed off immediately. If false, at job end.
    pub noempty: bool,
    /// Model DCB dataset name for new generations.
    pub model_dcb: Option<String>,
}

impl Default for GdgOptions {
    fn default() -> Self {
        Self {
            limit: 255,
            scratch: true,
            empty: true,
            noempty: false,
            model_dcb: None,
        }
    }
}

/// Header for GDG base catalog file.
#[repr(C)]
struct GdgHeader {
    /// Magic bytes: "GDG1".
    magic: [u8; 4],
    /// Version (1).
    version: u8,
    /// Limit (1-255).
    limit: u8,
    /// Flags: bit 0 = scratch, bit 1 = empty, bit 2 = noempty.
    flags: u8,
    /// Reserved.
    _reserved: u8,
    /// Number of generations.
    generation_count: u32,
    /// Next generation number to assign.
    next_generation: u16,
    /// Reserved.
    _reserved2: [u8; 50],
}

impl GdgHeader {
    const MAGIC: [u8; 4] = *b"GDG1";
    const SIZE: usize = 64;

    fn new(options: &GdgOptions) -> Self {
        let mut flags = 0u8;
        if options.scratch {
            flags |= 0x01;
        }
        if options.empty {
            flags |= 0x02;
        }
        if options.noempty {
            flags |= 0x04;
        }

        Self {
            magic: Self::MAGIC,
            version: 1,
            limit: options.limit,
            flags,
            _reserved: 0,
            generation_count: 0,
            next_generation: 1,
            _reserved2: [0; 50],
        }
    }

    fn to_bytes(&self) -> [u8; Self::SIZE] {
        let mut bytes = [0u8; Self::SIZE];
        bytes[0..4].copy_from_slice(&self.magic);
        bytes[4] = self.version;
        bytes[5] = self.limit;
        bytes[6] = self.flags;
        bytes[7] = self._reserved;
        bytes[8..12].copy_from_slice(&self.generation_count.to_le_bytes());
        bytes[12..14].copy_from_slice(&self.next_generation.to_le_bytes());
        bytes
    }

    fn from_bytes(bytes: &[u8]) -> Result<Self, DatasetError> {
        if bytes.len() < Self::SIZE {
            return Err(DatasetError::InvalidFormat {
                message: "GDG header too short".to_string(),
            });
        }

        let magic: [u8; 4] = bytes[0..4].try_into().unwrap();
        if magic != Self::MAGIC {
            return Err(DatasetError::InvalidFormat {
                message: "Invalid GDG magic bytes".to_string(),
            });
        }

        Ok(Self {
            magic,
            version: bytes[4],
            limit: bytes[5],
            flags: bytes[6],
            _reserved: bytes[7],
            generation_count: u32::from_le_bytes(bytes[8..12].try_into().unwrap()),
            next_generation: u16::from_le_bytes(bytes[12..14].try_into().unwrap()),
            _reserved2: [0; 50],
        })
    }

    fn is_scratch(&self) -> bool {
        self.flags & 0x01 != 0
    }

    fn is_empty(&self) -> bool {
        self.flags & 0x02 != 0
    }

    #[allow(dead_code)]
    fn is_noempty(&self) -> bool {
        self.flags & 0x04 != 0
    }
}

/// Generation entry in the catalog.
#[repr(C)]
struct GenerationEntry {
    /// Generation number (1-9999).
    generation: u16,
    /// Version (0-99).
    version: u8,
    /// Status: 0 = active, 1 = deleted.
    status: u8,
    /// Creation timestamp (Unix seconds).
    created: u64,
    /// Path length.
    path_len: u16,
    /// Reserved.
    _reserved: [u8; 6],
    // Variable: path bytes follow.
}

impl GenerationEntry {
    const FIXED_SIZE: usize = 20;

    fn write<W: Write>(&self, writer: &mut W, path: &str) -> Result<(), DatasetError> {
        writer.write_all(&self.generation.to_le_bytes())?;
        writer.write_all(&[self.version, self.status])?;
        writer.write_all(&self.created.to_le_bytes())?;

        let path_bytes = path.as_bytes();
        let path_len = path_bytes.len() as u16;
        writer.write_all(&path_len.to_le_bytes())?;
        writer.write_all(&[0u8; 6])?; // Reserved
        writer.write_all(path_bytes)?;

        Ok(())
    }

    fn read<R: BufRead>(reader: &mut R) -> Result<(Self, String), DatasetError> {
        let mut fixed = [0u8; Self::FIXED_SIZE];
        reader.read_exact(&mut fixed)?;

        let generation = u16::from_le_bytes(fixed[0..2].try_into().unwrap());
        let version = fixed[2];
        let status = fixed[3];
        let created = u64::from_le_bytes(fixed[4..12].try_into().unwrap());
        let path_len = u16::from_le_bytes(fixed[12..14].try_into().unwrap());

        let mut path_bytes = vec![0u8; path_len as usize];
        reader.read_exact(&mut path_bytes)?;
        let path = String::from_utf8(path_bytes).map_err(|_| DatasetError::InvalidFormat {
            message: "Invalid UTF-8 in generation path".to_string(),
        })?;

        Ok((
            Self {
                generation,
                version,
                status,
                created,
                path_len,
                _reserved: [0; 6],
            },
            path,
        ))
    }
}

/// GDG base entry.
#[derive(Debug)]
pub struct GdgBase {
    /// Base dataset name.
    name: String,
    /// Path to the GDG catalog file (.gdg).
    catalog_path: PathBuf,
    /// Base directory for generation files.
    data_dir: PathBuf,
    /// Options.
    options: GdgOptions,
    /// Cached generations.
    generations: Vec<GdgGeneration>,
}

impl GdgBase {
    /// Create a new GDG base.
    pub fn create(
        name: &str,
        base_dir: impl AsRef<Path>,
        options: GdgOptions,
    ) -> Result<Self, DatasetError> {
        let name = name.to_uppercase();
        let base_dir = base_dir.as_ref();

        // Create directory structure: base_dir/NAME (replace dots with /)
        let data_dir = Self::name_to_dir(&name, base_dir);
        fs::create_dir_all(&data_dir).map_err(|e| DatasetError::IoError {
            message: format!("Failed to create GDG directory: {}", e),
        })?;

        // Catalog file: data_dir/.gdg
        let catalog_path = data_dir.join(".gdg");

        if catalog_path.exists() {
            return Err(DatasetError::AlreadyExists {
                name: name.clone(),
            });
        }

        // Write header
        let header = GdgHeader::new(&options);
        let file = File::create(&catalog_path)?;
        let mut writer = BufWriter::new(file);
        writer.write_all(&header.to_bytes())?;
        writer.flush()?;

        Ok(Self {
            name,
            catalog_path,
            data_dir,
            options,
            generations: Vec::new(),
        })
    }

    /// Open an existing GDG base.
    pub fn open(name: &str, base_dir: impl AsRef<Path>) -> Result<Self, DatasetError> {
        let name = name.to_uppercase();
        let base_dir = base_dir.as_ref();

        let data_dir = Self::name_to_dir(&name, base_dir);
        let catalog_path = data_dir.join(".gdg");

        if !catalog_path.exists() {
            return Err(DatasetError::NotFound {
                name: name.clone(),
            });
        }

        let mut file = File::open(&catalog_path)?;
        let mut header_bytes = [0u8; GdgHeader::SIZE];
        file.read_exact(&mut header_bytes)?;
        let header = GdgHeader::from_bytes(&header_bytes)?;

        let options = GdgOptions {
            limit: header.limit,
            scratch: header.is_scratch(),
            empty: header.is_empty(),
            noempty: false,
            model_dcb: None,
        };

        // Read generations
        let mut generations = Vec::new();
        let mut reader = BufReader::new(file);

        for _ in 0..header.generation_count {
            let (entry, path) = GenerationEntry::read(&mut reader)?;
            if entry.status == 0 {
                // Active generation
                let gen_num = GenerationNumber::new(entry.generation, entry.version)?;
                generations.push(GdgGeneration::with_timestamp(
                    &name,
                    gen_num,
                    PathBuf::from(path),
                    entry.created,
                ));
            }
        }

        generations.sort_by_key(|a| a.number());

        Ok(Self {
            name,
            catalog_path,
            data_dir,
            options,
            generations,
        })
    }

    /// Convert dataset name to directory path.
    fn name_to_dir(name: &str, base_dir: &Path) -> PathBuf {
        let mut path = base_dir.to_path_buf();
        for component in name.split('.') {
            path.push(component);
        }
        path
    }

    /// Get the GDG base name.
    pub fn name(&self) -> &str {
        &self.name
    }

    /// Get the options.
    pub fn options(&self) -> &GdgOptions {
        &self.options
    }

    /// Get all generations (oldest first).
    pub fn generations(&self) -> &[GdgGeneration] {
        &self.generations
    }

    /// Get the current (newest) generation.
    pub fn current(&self) -> Option<&GdgGeneration> {
        self.generations.last()
    }

    /// Get generation count.
    pub fn generation_count(&self) -> usize {
        self.generations.len()
    }

    /// Get generation by relative number.
    /// - 0 = current
    /// - -1 = previous
    /// - +1 = next (new generation, returns None)
    pub fn relative_generation(&self, relative: i32) -> Option<&GdgGeneration> {
        if relative > 0 {
            // New generation reference - handled by new_generation()
            return None;
        }

        let count = self.generations.len() as i32;
        let index = count - 1 + relative;

        if index >= 0 && index < count {
            Some(&self.generations[index as usize])
        } else {
            None
        }
    }

    /// Get generation by absolute number.
    pub fn absolute_generation(&self, number: GenerationNumber) -> Option<&GdgGeneration> {
        self.generations.iter().find(|g| g.number() == number)
    }

    /// Create a new generation.
    pub fn new_generation(&mut self) -> Result<GdgGeneration, DatasetError> {
        // Read current header
        let mut file = OpenOptions::new()
            .read(true)
            .write(true)
            .open(&self.catalog_path)?;

        let mut header_bytes = [0u8; GdgHeader::SIZE];
        file.read_exact(&mut header_bytes)?;
        let mut header = GdgHeader::from_bytes(&header_bytes)?;

        // Create new generation
        let gen_num = GenerationNumber::new(header.next_generation, 0)?;
        let gen_file = format!("{}.dat", gen_num.to_suffix());
        let gen_path = self.data_dir.join(&gen_file);

        let generation = GdgGeneration::new(&self.name, gen_num, &gen_path);

        // Check limit and perform rolloff
        let rolloffs = self.calculate_rolloff();

        // Append new generation entry
        file.seek(SeekFrom::End(0))?;
        let entry = GenerationEntry {
            generation: gen_num.generation,
            version: gen_num.version,
            status: 0,
            created: generation.created(),
            path_len: 0,
            _reserved: [0; 6],
        };
        entry.write(&mut file, gen_path.to_str().unwrap_or(""))?;

        // Update header
        header.next_generation = gen_num.generation.saturating_add(1);
        header.generation_count += 1;

        file.seek(SeekFrom::Start(0))?;
        file.write_all(&header.to_bytes())?;
        file.flush()?;

        // Perform rolloff
        for rolled in rolloffs {
            self.remove_generation(rolled)?;
        }

        // Add to cached list
        self.generations.push(generation.clone());

        Ok(generation)
    }

    /// Calculate which generations to roll off.
    fn calculate_rolloff(&self) -> Vec<GenerationNumber> {
        let limit = self.options.limit as usize;
        if self.generations.len() < limit {
            return Vec::new();
        }

        // Roll off oldest generations to make room
        let to_remove = self.generations.len() + 1 - limit;
        self.generations
            .iter()
            .take(to_remove)
            .map(|g| g.number())
            .collect()
    }

    /// Remove a generation (delete or uncatalog).
    fn remove_generation(&mut self, number: GenerationNumber) -> Result<(), DatasetError> {
        // Find and remove from list
        let idx = self.generations.iter().position(|g| g.number() == number);
        if let Some(idx) = idx {
            let gen = self.generations.remove(idx);

            // Delete file if scratch mode
            if self.options.scratch && gen.path().exists() {
                fs::remove_file(gen.path()).ok();
            }
        }

        // Mark as deleted in catalog (rewrite would be cleaner but this is simpler)
        // For now, we just keep it in-memory accurate

        Ok(())
    }

    /// Get LISTCAT-style information about this GDG.
    pub fn list_info(&self) -> GdgListInfo {
        GdgListInfo {
            name: self.name.clone(),
            limit: self.options.limit,
            scratch: self.options.scratch,
            empty: self.options.empty,
            generation_count: self.generations.len(),
            generations: self.generations.iter().map(|g| GdgGenerationInfo {
                name: g.name(),
                number: g.number(),
                created: g.created(),
                exists: g.exists(),
            }).collect(),
        }
    }

    /// Delete a specific generation.
    pub fn delete_generation(&mut self, number: GenerationNumber) -> Result<(), DatasetError> {
        let idx = self.generations.iter().position(|g| g.number() == number);
        match idx {
            Some(idx) => {
                let gen = &self.generations[idx];
                // Delete file
                if gen.path().exists() {
                    fs::remove_file(gen.path())?;
                }
                // Remove from list
                self.generations.remove(idx);
                Ok(())
            }
            None => Err(DatasetError::NotFound {
                name: format!("{}.{}", self.name, number),
            }),
        }
    }

    /// Delete the entire GDG and all generations.
    pub fn delete(self) -> Result<(), DatasetError> {
        // Delete all generation files
        for gen in &self.generations {
            if gen.path().exists() {
                fs::remove_file(gen.path()).ok();
            }
        }

        // Delete catalog file
        if self.catalog_path.exists() {
            fs::remove_file(&self.catalog_path)?;
        }

        // Try to remove directory (if empty)
        fs::remove_dir(&self.data_dir).ok();

        Ok(())
    }

    /// Resolve a dataset reference to a generation.
    /// Handles: BASE(0), BASE(-1), BASE(+1), BASE.GxxxxVyy
    pub fn resolve(&self, reference: &str) -> Result<Option<GdgGeneration>, DatasetError> {
        let reference = reference.to_uppercase();

        // Check for absolute reference (BASE.GxxxxVyy)
        if reference.starts_with(&self.name) {
            let suffix = reference.strip_prefix(&self.name).unwrap_or("");
            if let Some(gen_suffix) = suffix.strip_prefix('.') {
                if gen_suffix.starts_with('G') {
                    let number = GenerationNumber::parse(gen_suffix)?;
                    return Ok(self.absolute_generation(number).cloned());
                }
            }
        }

        // Check for relative reference BASE(n)
        if let Some(paren_start) = reference.find('(') {
            if reference.ends_with(')') {
                let rel_str = &reference[paren_start + 1..reference.len() - 1];
                let relative: i32 = rel_str.parse().map_err(|_| {
                    DatasetError::InvalidParameter(format!(
                        "Invalid relative generation: {}",
                        rel_str
                    ))
                })?;
                return Ok(self.relative_generation(relative).cloned());
            }
        }

        // Default to current generation
        Ok(self.current().cloned())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::{AtomicUsize, Ordering};

    static TEST_COUNTER: AtomicUsize = AtomicUsize::new(0);

    fn test_dir() -> PathBuf {
        let count = TEST_COUNTER.fetch_add(1, Ordering::SeqCst);
        std::env::temp_dir().join(format!("gdg_test_{}", count))
    }

    fn cleanup(path: &Path) {
        let _ = fs::remove_dir_all(path);
    }

    #[test]
    fn test_gdg_base_create() {
        let dir = test_dir();
        cleanup(&dir);

        let base = GdgBase::create("MY.GDG.BASE", &dir, GdgOptions::default()).unwrap();

        assert_eq!(base.name(), "MY.GDG.BASE");
        assert_eq!(base.generation_count(), 0);
        assert!(base.catalog_path.exists());

        base.delete().unwrap();
        cleanup(&dir);
    }

    #[test]
    fn test_gdg_base_create_duplicate() {
        let dir = test_dir();
        cleanup(&dir);

        let _base = GdgBase::create("MY.GDG.BASE", &dir, GdgOptions::default()).unwrap();
        let result = GdgBase::create("MY.GDG.BASE", &dir, GdgOptions::default());

        assert!(result.is_err());
        cleanup(&dir);
    }

    #[test]
    fn test_gdg_base_open() {
        let dir = test_dir();
        cleanup(&dir);

        // Create
        let base = GdgBase::create(
            "MY.GDG.BASE",
            &dir,
            GdgOptions {
                limit: 10,
                scratch: true,
                ..Default::default()
            },
        )
        .unwrap();
        drop(base);

        // Open
        let base = GdgBase::open("MY.GDG.BASE", &dir).unwrap();
        assert_eq!(base.options().limit, 10);
        assert!(base.options().scratch);

        base.delete().unwrap();
        cleanup(&dir);
    }

    #[test]
    fn test_gdg_new_generation() {
        let dir = test_dir();
        cleanup(&dir);

        let mut base = GdgBase::create("MY.GDG.BASE", &dir, GdgOptions::default()).unwrap();

        let gen1 = base.new_generation().unwrap();
        assert_eq!(gen1.name(), "MY.GDG.BASE.G0001V00");
        assert_eq!(base.generation_count(), 1);

        let gen2 = base.new_generation().unwrap();
        assert_eq!(gen2.name(), "MY.GDG.BASE.G0002V00");
        assert_eq!(base.generation_count(), 2);

        // Current should be gen2
        assert_eq!(base.current().unwrap().name(), "MY.GDG.BASE.G0002V00");

        base.delete().unwrap();
        cleanup(&dir);
    }

    #[test]
    fn test_gdg_relative_generation() {
        let dir = test_dir();
        cleanup(&dir);

        let mut base = GdgBase::create("MY.GDG.BASE", &dir, GdgOptions::default()).unwrap();

        let _gen1 = base.new_generation().unwrap();
        let _gen2 = base.new_generation().unwrap();
        let _gen3 = base.new_generation().unwrap();

        // Relative references
        assert_eq!(
            base.relative_generation(0).unwrap().name(),
            "MY.GDG.BASE.G0003V00"
        );
        assert_eq!(
            base.relative_generation(-1).unwrap().name(),
            "MY.GDG.BASE.G0002V00"
        );
        assert_eq!(
            base.relative_generation(-2).unwrap().name(),
            "MY.GDG.BASE.G0001V00"
        );
        assert!(base.relative_generation(-3).is_none());
        assert!(base.relative_generation(1).is_none()); // +1 = new generation

        base.delete().unwrap();
        cleanup(&dir);
    }

    #[test]
    fn test_gdg_limit_rolloff() {
        let dir = test_dir();
        cleanup(&dir);

        let mut base = GdgBase::create(
            "MY.GDG.BASE",
            &dir,
            GdgOptions {
                limit: 3,
                scratch: true,
                ..Default::default()
            },
        )
        .unwrap();

        // Create 4 generations - first should roll off
        let gen1 = base.new_generation().unwrap();
        let _gen2 = base.new_generation().unwrap();
        let _gen3 = base.new_generation().unwrap();

        // Create a file for gen1 to verify scratch
        fs::write(gen1.path(), "test").unwrap();
        assert!(gen1.path().exists());

        // This should trigger rolloff of gen1
        let _gen4 = base.new_generation().unwrap();

        assert_eq!(base.generation_count(), 3);
        assert_eq!(
            base.relative_generation(-2).unwrap().name(),
            "MY.GDG.BASE.G0002V00"
        );
        // Gen1 should be deleted (scratch mode)
        assert!(!gen1.path().exists());

        base.delete().unwrap();
        cleanup(&dir);
    }

    #[test]
    fn test_gdg_resolve() {
        let dir = test_dir();
        cleanup(&dir);

        let mut base = GdgBase::create("MY.GDG.BASE", &dir, GdgOptions::default()).unwrap();

        let _gen1 = base.new_generation().unwrap();
        let _gen2 = base.new_generation().unwrap();

        // Absolute
        let resolved = base.resolve("MY.GDG.BASE.G0001V00").unwrap().unwrap();
        assert_eq!(resolved.name(), "MY.GDG.BASE.G0001V00");

        // Relative
        let resolved = base.resolve("MY.GDG.BASE(0)").unwrap().unwrap();
        assert_eq!(resolved.name(), "MY.GDG.BASE.G0002V00");

        let resolved = base.resolve("MY.GDG.BASE(-1)").unwrap().unwrap();
        assert_eq!(resolved.name(), "MY.GDG.BASE.G0001V00");

        base.delete().unwrap();
        cleanup(&dir);
    }

    #[test]
    fn test_gdg_persistence() {
        let dir = test_dir();
        cleanup(&dir);

        // Create and add generations
        {
            let mut base = GdgBase::create("MY.GDG.BASE", &dir, GdgOptions::default()).unwrap();
            base.new_generation().unwrap();
            base.new_generation().unwrap();
        }

        // Reopen and verify
        {
            let base = GdgBase::open("MY.GDG.BASE", &dir).unwrap();
            assert_eq!(base.generation_count(), 2);
            assert_eq!(base.current().unwrap().name(), "MY.GDG.BASE.G0002V00");
            base.delete().unwrap();
        }

        cleanup(&dir);
    }

    #[test]
    fn test_gdg_list_info() {
        let dir = test_dir();
        cleanup(&dir);

        let mut base = GdgBase::create(
            "MY.GDG.BASE",
            &dir,
            GdgOptions {
                limit: 5,
                scratch: true,
                empty: true,
                ..Default::default()
            },
        )
        .unwrap();

        base.new_generation().unwrap();
        base.new_generation().unwrap();

        let info = base.list_info();
        assert_eq!(info.name, "MY.GDG.BASE");
        assert_eq!(info.limit, 5);
        assert!(info.scratch);
        assert!(info.empty);
        assert_eq!(info.generation_count, 2);
        assert_eq!(info.generations.len(), 2);
        assert_eq!(info.generations[0].name, "MY.GDG.BASE.G0001V00");
        assert_eq!(info.generations[1].name, "MY.GDG.BASE.G0002V00");

        base.delete().unwrap();
        cleanup(&dir);
    }

    #[test]
    fn test_gdg_delete_generation() {
        let dir = test_dir();
        cleanup(&dir);

        let mut base = GdgBase::create("MY.GDG.BASE", &dir, GdgOptions::default()).unwrap();

        let gen1 = base.new_generation().unwrap();
        let gen2 = base.new_generation().unwrap();

        // Create files
        fs::write(gen1.path(), "gen1").unwrap();
        fs::write(gen2.path(), "gen2").unwrap();

        // Delete gen1
        let gen1_num = gen1.number();
        base.delete_generation(gen1_num).unwrap();

        assert_eq!(base.generation_count(), 1);
        assert!(!gen1.path().exists());
        assert!(gen2.path().exists());

        // Current should be gen2
        assert_eq!(base.current().unwrap().name(), "MY.GDG.BASE.G0002V00");

        // Try to delete nonexistent generation
        let result = base.delete_generation(gen1_num);
        assert!(result.is_err());

        base.delete().unwrap();
        cleanup(&dir);
    }
}
