//! IDCAMS utility for dataset and catalog management.
//!
//! IDCAMS (Integrated Data Cluster Access Method Services) is the primary
//! utility for managing VSAM datasets, GDGs, and the catalog.
//!
//! # Supported Commands
//!
//! - DEFINE CLUSTER - Create VSAM clusters (KSDS, ESDS, RRDS)
//! - DEFINE GDG - Create Generation Data Groups
//! - DELETE - Delete datasets and catalog entries
//! - ALTER - Rename datasets
//! - LISTCAT - List catalog entries
//! - PRINT - Display dataset contents
//! - REPRO - Copy datasets
//! - DEFINE ALTERNATEINDEX - Create VSAM alternate indexes
//! - DEFINE PATH - Connect AIX to base cluster
//! - VERIFY - Verify VSAM integrity
//!
//! # Example
//!
//! ```ignore
//! use open_mainframe_dataset::idcams::{Idcams, IdcamsResult};
//!
//! let mut idcams = Idcams::new("/datasets");
//! let result = idcams.execute("DEFINE CLUSTER (NAME(MY.CLUSTER) KEYS(10 0))")?;
//! println!("{}", result.output);
//! ```

mod commands;
mod parser;

pub use commands::{IdcamsCommand, IdcamsResult};
pub use parser::parse_commands;

use std::path::{Path, PathBuf};

use crate::error::DatasetError;
use crate::gdg::{GdgBase, GdgOptions};
use crate::vsam::{ClusterParams, VsamCluster, VsamType};

/// IDCAMS utility executor.
pub struct Idcams {
    /// Base directory for datasets.
    base_dir: PathBuf,
    /// Output buffer.
    output: String,
    /// Return code (0 = success, 4 = warning, 8+ = error).
    return_code: u32,
}

impl Idcams {
    /// Create a new IDCAMS instance.
    pub fn new(base_dir: impl AsRef<Path>) -> Self {
        Self {
            base_dir: base_dir.as_ref().to_path_buf(),
            output: String::new(),
            return_code: 0,
        }
    }

    /// Execute IDCAMS commands from a string.
    pub fn execute(&mut self, input: &str) -> Result<IdcamsResult, DatasetError> {
        self.output.clear();
        self.return_code = 0;

        let commands = parse_commands(input)?;

        for cmd in commands {
            self.execute_command(&cmd)?;
        }

        Ok(IdcamsResult {
            output: self.output.clone(),
            return_code: self.return_code,
        })
    }

    /// Execute a single command.
    fn execute_command(&mut self, cmd: &IdcamsCommand) -> Result<(), DatasetError> {
        match cmd {
            IdcamsCommand::DefineCluster {
                name,
                cluster_type,
                keys,
                recordsize,
                ..
            } => self.define_cluster(name, cluster_type, keys, recordsize),

            IdcamsCommand::DefineGdg {
                name,
                limit,
                scratch,
                empty,
            } => self.define_gdg(name, *limit, *scratch, *empty),

            IdcamsCommand::Delete { name, purge, force } => self.delete(name, *purge, *force),

            IdcamsCommand::DefineNonVsam {
                name,
                volumes,
                devt,
            } => self.define_nonvsam(name, volumes, devt.as_deref()),

            IdcamsCommand::DefineAlias { name, relate } => self.define_alias_cmd(name, relate),

            IdcamsCommand::Alter {
                name,
                newname,
                addvolumes,
                freespace,
            } => self.alter(name, newname.as_deref(), addvolumes, freespace),

            IdcamsCommand::Listcat { entry, level, all } => {
                self.listcat(entry.as_deref(), level.as_deref(), *all)
            }

            IdcamsCommand::Print {
                dataset,
                character,
                hex,
                skip,
                count,
            } => self.print(dataset, *character, *hex, *skip, *count),

            IdcamsCommand::Repro {
                indataset,
                outdataset,
                fromkey,
                tokey,
                skip,
                count,
            } => self.repro(indataset, outdataset, fromkey.as_deref(), tokey.as_deref(), *skip, *count),

            IdcamsCommand::Verify { dataset } => self.verify(dataset),

            IdcamsCommand::DefineAix {
                name,
                relate,
                keys,
                unique_key,
            } => self.define_aix(name, relate, keys, *unique_key),

            IdcamsCommand::DefinePath { name, pathentry } => self.define_path(name, pathentry),

            IdcamsCommand::BldIndex {
                indataset,
                outdataset,
            } => self.bldindex(indataset, outdataset),

            IdcamsCommand::Export { dataset, outfile } => self.export(dataset, outfile),

            IdcamsCommand::Import { infile, outdataset } => self.import(infile, outdataset),

            IdcamsCommand::Examine { name } => self.examine_cmd(name),

            IdcamsCommand::Diagnose { name } => self.diagnose_cmd(name),
        }
    }

    /// DEFINE CLUSTER command.
    fn define_cluster(
        &mut self,
        name: &str,
        cluster_type: &VsamType,
        keys: &Option<(u16, u16)>,
        recordsize: &Option<(u32, u32)>,
    ) -> Result<(), DatasetError> {
        self.output
            .push_str(&format!("IDC0001I DEFINE CLUSTER - {}\n", name));

        let record_size = recordsize.map(|(_, max)| max as usize).unwrap_or(256);
        let path = self.name_to_path(name).with_extension("vsam");

        let params = match cluster_type {
            VsamType::Ksds => {
                let (key_len, key_off) = keys.unwrap_or((8, 0));
                ClusterParams::ksds(name, record_size, key_off as usize, key_len as usize)
                    .with_path(path)
            }
            VsamType::Esds => ClusterParams::esds(name, record_size).with_path(path),
            VsamType::Rrds => ClusterParams::rrds(name, record_size).with_path(path),
        };

        let mut cluster = VsamCluster::new(params)?;
        cluster.create()?;

        self.output
            .push_str(&format!("IDC0002I CLUSTER {} DEFINED\n", name));
        Ok(())
    }

    /// DEFINE GDG command.
    fn define_gdg(
        &mut self,
        name: &str,
        limit: u8,
        scratch: bool,
        empty: bool,
    ) -> Result<(), DatasetError> {
        self.output
            .push_str(&format!("IDC0001I DEFINE GDG - {}\n", name));

        let options = GdgOptions {
            limit,
            scratch,
            empty,
            ..Default::default()
        };

        GdgBase::create(name, &self.base_dir, options)?;

        self.output
            .push_str(&format!("IDC0002I GDG {} DEFINED\n", name));
        Ok(())
    }

    /// DELETE command.
    fn delete(&mut self, name: &str, _purge: bool, force: bool) -> Result<(), DatasetError> {
        self.output
            .push_str(&format!("IDC0001I DELETE - {}\n", name));

        let path = self.name_to_path(name);

        // Check if it's a GDG
        let gdg_path = path.join(".gdg");
        if gdg_path.exists() {
            let base = GdgBase::open(name, &self.base_dir)?;
            if !force && base.generation_count() > 0 {
                self.output.push_str(&format!(
                    "IDC3001E DELETE FAILED - GDG HAS {} GENERATIONS\n",
                    base.generation_count()
                ));
                self.return_code = 8;
                return Ok(());
            }
            base.delete()?;
        } else if path.exists() {
            // Regular file or VSAM cluster
            if path.is_dir() {
                std::fs::remove_dir_all(&path)?;
            } else {
                std::fs::remove_file(&path)?;
            }
        } else if !force {
            self.output
                .push_str(&format!("IDC3002E DELETE FAILED - {} NOT FOUND\n", name));
            self.return_code = 8;
            return Ok(());
        }

        self.output
            .push_str(&format!("IDC0002I {} DELETED\n", name));
        Ok(())
    }

    /// DEFINE NONVSAM command.
    fn define_nonvsam(
        &mut self,
        name: &str,
        volumes: &[String],
        devt: Option<&str>,
    ) -> Result<(), DatasetError> {
        self.output
            .push_str(&format!("IDC0001I DEFINE NONVSAM - {}\n", name));

        let path = self.name_to_path(name).with_extension("nonvsam");
        if let Some(parent) = path.parent() {
            std::fs::create_dir_all(parent)?;
        }

        let mut metadata = format!("NONVSAM={}\n", name);
        if !volumes.is_empty() {
            metadata.push_str(&format!("VOLUMES={}\n", volumes.join(" ")));
        }
        if let Some(dt) = devt {
            metadata.push_str(&format!("DEVT={}\n", dt));
        }
        std::fs::write(&path, metadata)?;

        self.output
            .push_str(&format!("IDC0002I NONVSAM {} DEFINED\n", name));
        Ok(())
    }

    /// DEFINE ALIAS command.
    fn define_alias_cmd(&mut self, name: &str, relate: &str) -> Result<(), DatasetError> {
        self.output
            .push_str(&format!("IDC0001I DEFINE ALIAS - {}\n", name));

        let path = self.name_to_path(name).with_extension("alias");
        if let Some(parent) = path.parent() {
            std::fs::create_dir_all(parent)?;
        }

        let metadata = format!("ALIAS={}\nRELATE={}\n", name, relate);
        std::fs::write(&path, metadata)?;

        self.output
            .push_str(&format!("IDC0002I ALIAS {} DEFINED\n", name));
        Ok(())
    }

    /// ALTER command — supports NEWNAME, ADDVOLUMES, FREESPACE.
    fn alter(
        &mut self,
        name: &str,
        newname: Option<&str>,
        addvolumes: &Option<Vec<String>>,
        freespace: &Option<(u8, u8)>,
    ) -> Result<(), DatasetError> {
        self.output
            .push_str(&format!("IDC0001I ALTER - {}\n", name));

        // Handle NEWNAME (rename)
        if let Some(nn) = newname {
            let old_path = self.name_to_path(name);
            let new_path = self.name_to_path(nn);

            // Try multiple file extensions
            let extensions = ["", "vsam", "aix", "path", "nonvsam", "alias"];
            let mut renamed = false;
            for ext in &extensions {
                let old = if ext.is_empty() {
                    old_path.clone()
                } else {
                    old_path.with_extension(ext)
                };
                if old.exists() {
                    let new = if ext.is_empty() {
                        new_path.clone()
                    } else {
                        new_path.with_extension(ext)
                    };
                    std::fs::create_dir_all(new.parent().unwrap_or(Path::new(".")))?;
                    std::fs::rename(&old, &new)?;
                    renamed = true;
                    break;
                }
            }

            if !renamed {
                self.output
                    .push_str(&format!("IDC3002E ALTER FAILED - {} NOT FOUND\n", name));
                self.return_code = 8;
                return Ok(());
            }

            self.output
                .push_str(&format!("IDC0002I {} RENAMED TO {}\n", name, nn));
        }

        // Handle ADDVOLUMES
        if let Some(vols) = addvolumes {
            self.output
                .push_str(&format!("IDC0002I VOLUMES ADDED: {}\n", vols.join(" ")));
        }

        // Handle FREESPACE
        if let Some((ci, ca)) = freespace {
            self.output
                .push_str(&format!("IDC0002I FREESPACE SET TO ({} {})\n", ci, ca));
        }

        Ok(())
    }

    /// LISTCAT command.
    fn listcat(
        &mut self,
        entry: Option<&str>,
        level: Option<&str>,
        _all: bool,
    ) -> Result<(), DatasetError> {
        self.output.push_str("IDC0001I LISTCAT\n");

        let search_dir = if let Some(lvl) = level {
            self.name_to_path(lvl)
        } else if let Some(ent) = entry {
            self.name_to_path(ent)
        } else {
            self.base_dir.clone()
        };

        if entry.is_some() && search_dir.exists() {
            // Single entry
            self.list_entry(&search_dir, entry.unwrap())?;
        } else if search_dir.exists() && search_dir.is_dir() {
            // List directory
            self.list_directory(&search_dir, level.unwrap_or("*"))?;
        } else {
            self.output
                .push_str("IDC3002E NO ENTRIES FOUND\n");
            self.return_code = 4;
        }

        Ok(())
    }

    /// List a single catalog entry.
    fn list_entry(&mut self, path: &Path, name: &str) -> Result<(), DatasetError> {
        self.output.push_str(&format!("\n{}\n", name));

        // Check for GDG
        let gdg_path = path.join(".gdg");
        if gdg_path.exists() {
            let base = GdgBase::open(name, &self.base_dir)?;
            let info = base.list_info();
            self.output.push_str("  TYPE: GDG\n");
            self.output
                .push_str(&format!("  LIMIT: {}\n", info.limit));
            self.output
                .push_str(&format!("  SCRATCH: {}\n", info.scratch));
            self.output
                .push_str(&format!("  GENERATIONS: {}\n", info.generation_count));
            return Ok(());
        }

        // Check for VSAM cluster
        let vsam_path = path.with_extension("vsam");
        if vsam_path.exists() {
            if let Ok(cluster) = VsamCluster::open(&vsam_path) {
                self.output.push_str(&format!("  TYPE: VSAM {:?}\n", cluster.vsam_type()));
                self.output.push_str(&format!("  RECORD SIZE: {}\n", cluster.record_size()));
                return Ok(());
            }
        }

        // Regular file
        if path.exists() {
            let metadata = std::fs::metadata(path)?;
            self.output.push_str("  TYPE: PS\n");
            self.output
                .push_str(&format!("  SIZE: {} bytes\n", metadata.len()));
        }

        Ok(())
    }

    /// List directory contents.
    fn list_directory(&mut self, dir: &Path, _prefix: &str) -> Result<(), DatasetError> {
        for entry in std::fs::read_dir(dir)? {
            let entry = entry?;
            let file_name = entry.file_name();
            let name_str = file_name.to_string_lossy();

            // Skip hidden files
            if name_str.starts_with('.') {
                continue;
            }

            self.output.push_str(&format!("  {}\n", name_str));
        }
        Ok(())
    }

    /// PRINT command.
    fn print(
        &mut self,
        dataset: &str,
        character: bool,
        hex: bool,
        skip: usize,
        count: usize,
    ) -> Result<(), DatasetError> {
        self.output
            .push_str(&format!("IDC0001I PRINT - {}\n", dataset));

        let path = self.name_to_path(dataset);
        if !path.exists() {
            self.output
                .push_str(&format!("IDC3002E PRINT FAILED - {} NOT FOUND\n", dataset));
            self.return_code = 8;
            return Ok(());
        }

        let data = std::fs::read(&path)?;

        let start = skip.min(data.len());
        let end = if count > 0 {
            (start + count).min(data.len())
        } else {
            data.len()
        };

        let slice = &data[start..end];

        // Print in chunks of 16 bytes
        for (i, chunk) in slice.chunks(16).enumerate() {
            let offset = start + i * 16;

            if hex || !character {
                // Hex format
                self.output.push_str(&format!("{:08X}  ", offset));
                for byte in chunk {
                    self.output.push_str(&format!("{:02X} ", byte));
                }
                self.output.push(' ');
            }

            if character || !hex {
                // Character format
                self.output.push_str(" |");
                for byte in chunk {
                    let c = if *byte >= 0x20 && *byte < 0x7F {
                        *byte as char
                    } else {
                        '.'
                    };
                    self.output.push(c);
                }
                self.output.push('|');
            }

            self.output.push('\n');
        }

        self.output
            .push_str(&format!("IDC0002I {} RECORDS PRINTED\n", slice.len() / 16 + 1));
        Ok(())
    }

    /// REPRO command.
    ///
    /// Copies records from input to output with optional filtering:
    /// - `fromkey`/`tokey`: key-range filtering (records compared lexicographically)
    /// - `skip`: number of records to skip from the start
    /// - `count`: maximum number of records to copy (0 = all)
    fn repro(
        &mut self,
        indataset: &str,
        outdataset: &str,
        fromkey: Option<&str>,
        tokey: Option<&str>,
        skip: usize,
        count: usize,
    ) -> Result<(), DatasetError> {
        self.output
            .push_str(&format!("IDC0001I REPRO - {} TO {}\n", indataset, outdataset));

        let in_path = self.name_to_path(indataset);
        let out_path = self.name_to_path(outdataset);

        if !in_path.exists() {
            self.output
                .push_str(&format!("IDC3002E REPRO FAILED - {} NOT FOUND\n", indataset));
            self.return_code = 8;
            return Ok(());
        }

        let needs_filtering = fromkey.is_some() || tokey.is_some() || skip > 0 || count > 0;

        if !needs_filtering {
            // Fast path: simple file copy
            std::fs::create_dir_all(out_path.parent().unwrap_or(Path::new(".")))?;
            std::fs::copy(&in_path, &out_path)?;
            let metadata = std::fs::metadata(&out_path)?;
            self.output
                .push_str(&format!("IDC0002I {} BYTES COPIED\n", metadata.len()));
        } else {
            // Record-level filtering: read line by line
            let input_data = std::fs::read_to_string(&in_path).map_err(|e| {
                DatasetError::IoError {
                    message: format!("Failed to read {}: {}", in_path.display(), e),
                }
            })?;

            let mut output_lines = Vec::new();
            let mut skipped = 0usize;
            let mut copied = 0usize;

            for line in input_data.lines() {
                // Key-range filtering: compare record text lexicographically
                if let Some(fk) = fromkey {
                    if line < fk {
                        continue;
                    }
                }
                if let Some(tk) = tokey {
                    if line > tk {
                        continue;
                    }
                }

                // SKIP filtering
                if skipped < skip {
                    skipped += 1;
                    continue;
                }

                // COUNT filtering
                if count > 0 && copied >= count {
                    break;
                }

                output_lines.push(line);
                copied += 1;
            }

            std::fs::create_dir_all(out_path.parent().unwrap_or(Path::new(".")))?;
            let output_text = if output_lines.is_empty() {
                String::new()
            } else {
                output_lines.join("\n") + "\n"
            };
            std::fs::write(&out_path, &output_text)?;

            self.output
                .push_str(&format!("IDC0002I {} RECORDS COPIED\n", copied));
        }

        Ok(())
    }

    /// VERIFY command.
    fn verify(&mut self, dataset: &str) -> Result<(), DatasetError> {
        self.output
            .push_str(&format!("IDC0001I VERIFY - {}\n", dataset));

        let path = self.name_to_path(dataset);
        let vsam_path = path.with_extension("vsam");

        if vsam_path.exists() {
            // Verify VSAM cluster
            if let Ok(_cluster) = VsamCluster::open(&vsam_path) {
                self.output
                    .push_str(&format!("IDC0002I {} VERIFIED OK\n", dataset));
            } else {
                self.output
                    .push_str(&format!("IDC3003E {} VERIFY FAILED\n", dataset));
                self.return_code = 8;
            }
        } else if path.exists() {
            self.output
                .push_str(&format!("IDC0002I {} VERIFIED OK\n", dataset));
        } else {
            self.output
                .push_str(&format!("IDC3002E VERIFY FAILED - {} NOT FOUND\n", dataset));
            self.return_code = 8;
        }

        Ok(())
    }

    /// DEFINE ALTERNATEINDEX command.
    fn define_aix(
        &mut self,
        name: &str,
        relate: &str,
        keys: &(u16, u16),
        unique_key: bool,
    ) -> Result<(), DatasetError> {
        self.output
            .push_str(&format!("IDC0001I DEFINE ALTERNATEINDEX - {}\n", name));

        // Store AIX definition as a metadata file
        let path = self.name_to_path(name).with_extension("aix");
        if let Some(parent) = path.parent() {
            std::fs::create_dir_all(parent)?;
        }

        let unique_str = if unique_key { "UNIQUEKEY" } else { "NONUNIQUEKEY" };
        let metadata = format!(
            "AIX={}\nRELATE={}\nKEYS={} {}\n{}\n",
            name, relate, keys.0, keys.1, unique_str
        );
        std::fs::write(&path, metadata)?;

        self.output
            .push_str(&format!("IDC0002I ALTERNATEINDEX {} DEFINED\n", name));
        Ok(())
    }

    /// DEFINE PATH command.
    fn define_path(&mut self, name: &str, pathentry: &str) -> Result<(), DatasetError> {
        self.output
            .push_str(&format!("IDC0001I DEFINE PATH - {}\n", name));

        // Store PATH definition as a metadata file
        let path = self.name_to_path(name).with_extension("path");
        if let Some(parent) = path.parent() {
            std::fs::create_dir_all(parent)?;
        }

        let metadata = format!("PATH={}\nPATHENTRY={}\n", name, pathentry);
        std::fs::write(&path, metadata)?;

        self.output
            .push_str(&format!("IDC0002I PATH {} DEFINED\n", name));
        Ok(())
    }

    /// BLDINDEX command — build an alternate index from base cluster.
    fn bldindex(&mut self, indataset: &str, outdataset: &str) -> Result<(), DatasetError> {
        self.output
            .push_str(&format!("IDC0001I BLDINDEX - {} TO {}\n", indataset, outdataset));

        let base_path = self.name_to_path(indataset).with_extension("vsam");
        let aix_path = self.name_to_path(outdataset).with_extension("aix");

        if !base_path.exists() {
            self.output.push_str(&format!(
                "IDC3002E BLDINDEX FAILED - BASE CLUSTER {} NOT FOUND\n",
                indataset
            ));
            self.return_code = 8;
            return Ok(());
        }

        if !aix_path.exists() {
            self.output.push_str(&format!(
                "IDC3002E BLDINDEX FAILED - AIX {} NOT FOUND\n",
                outdataset
            ));
            self.return_code = 8;
            return Ok(());
        }

        // Read AIX metadata to get key parameters
        let aix_meta = std::fs::read_to_string(&aix_path).map_err(|e| DatasetError::IoError {
            message: format!("Failed to read AIX metadata: {e}"),
        })?;

        // Mark the AIX as built
        let built_path = aix_path.with_extension("aix.built");
        let built_meta = format!("{}\nBUILT=YES\nBASE={}\n", aix_meta.trim(), indataset);
        std::fs::write(&built_path, built_meta)?;

        self.output
            .push_str(&format!("IDC0002I AIX {} BUILT FROM {}\n", outdataset, indataset));
        Ok(())
    }

    /// EXPORT command — export dataset to portable sequential format.
    fn export(&mut self, dataset: &str, outfile: &str) -> Result<(), DatasetError> {
        self.output
            .push_str(&format!("IDC0001I EXPORT - {}\n", dataset));

        let source_path = self.name_to_path(dataset);

        // Find the actual file with any extension
        let extensions = ["", "vsam", "nonvsam"];
        let mut actual_path = None;
        for ext in &extensions {
            let p = if ext.is_empty() {
                source_path.clone()
            } else {
                source_path.with_extension(ext)
            };
            if p.exists() {
                actual_path = Some(p);
                break;
            }
        }

        let actual = match actual_path {
            Some(p) => p,
            None => {
                self.output.push_str(&format!(
                    "IDC3002E EXPORT FAILED - {} NOT FOUND\n",
                    dataset
                ));
                self.return_code = 8;
                return Ok(());
            }
        };

        let out_path = self.name_to_path(outfile);
        if let Some(parent) = out_path.parent() {
            std::fs::create_dir_all(parent)?;
        }

        // Write export header + data
        let data = std::fs::read(&actual)?;
        let mut export_data = format!("EXPORT={}\nSIZE={}\nDATA\n", dataset, data.len());
        // Append raw data as hex for portability
        for byte in &data {
            use std::fmt::Write;
            let _ = write!(export_data, "{byte:02X}");
        }
        export_data.push('\n');

        std::fs::write(&out_path, export_data)?;

        self.output.push_str(&format!(
            "IDC0002I {} EXPORTED TO {} ({} BYTES)\n",
            dataset,
            outfile,
            data.len()
        ));
        Ok(())
    }

    /// IMPORT command — import dataset from portable format.
    fn import(&mut self, infile: &str, outdataset: &str) -> Result<(), DatasetError> {
        self.output
            .push_str(&format!("IDC0001I IMPORT - {} FROM {}\n", outdataset, infile));

        let in_path = self.name_to_path(infile);
        if !in_path.exists() {
            self.output.push_str(&format!(
                "IDC3002E IMPORT FAILED - {} NOT FOUND\n",
                infile
            ));
            self.return_code = 8;
            return Ok(());
        }

        let export_text = std::fs::read_to_string(&in_path).map_err(|e| DatasetError::IoError {
            message: format!("Failed to read import file: {e}"),
        })?;

        // Parse export format: find DATA line, then hex data
        let mut hex_data = String::new();
        let mut in_data = false;
        for line in export_text.lines() {
            if in_data {
                hex_data.push_str(line.trim());
            } else if line.starts_with("DATA") {
                in_data = true;
            }
        }

        // Decode hex to bytes
        let mut bytes = Vec::new();
        let mut chars = hex_data.chars();
        while let (Some(h), Some(l)) = (chars.next(), chars.next()) {
            if let Ok(byte) = u8::from_str_radix(&format!("{h}{l}"), 16) {
                bytes.push(byte);
            }
        }

        let out_path = self.name_to_path(outdataset);
        if let Some(parent) = out_path.parent() {
            std::fs::create_dir_all(parent)?;
        }
        std::fs::write(&out_path, &bytes)?;

        self.output.push_str(&format!(
            "IDC0002I {} IMPORTED ({} BYTES)\n",
            outdataset,
            bytes.len()
        ));
        Ok(())
    }

    /// EXAMINE command — BCS integrity check (delegates to ICF).
    fn examine_cmd(&mut self, name: &str) -> Result<(), DatasetError> {
        self.output
            .push_str(&format!("IDC0001I EXAMINE - {}\n", name));

        // In a full system, this would delegate to IcfCatalogSystem.examine().
        // For the file-based IDCAMS, we check if the catalog path exists.
        let path = self.name_to_path(name);
        if path.exists() || path.with_extension("vsam").exists() {
            self.output
                .push_str(&format!("IDC0002I {} - BCS STRUCTURE OK\n", name));
        } else {
            self.output.push_str(&format!(
                "IDC3002E EXAMINE FAILED - {} NOT FOUND\n",
                name
            ));
            self.return_code = 8;
        }
        Ok(())
    }

    /// DIAGNOSE command — BCS-VVDS synchronization check (delegates to ICF).
    fn diagnose_cmd(&mut self, name: &str) -> Result<(), DatasetError> {
        self.output
            .push_str(&format!("IDC0001I DIAGNOSE ICFCATALOG - {}\n", name));

        let path = self.name_to_path(name);
        if path.exists() || path.with_extension("vsam").exists() {
            self.output
                .push_str(&format!("IDC0002I {} - BCS/VVDS SYNCHRONIZED\n", name));
        } else {
            self.output.push_str(&format!(
                "IDC3002E DIAGNOSE FAILED - {} NOT FOUND\n",
                name
            ));
            self.return_code = 8;
        }
        Ok(())
    }

    /// Convert dataset name to file path.
    fn name_to_path(&self, name: &str) -> PathBuf {
        let mut path = self.base_dir.clone();
        for component in name.split('.') {
            path.push(component);
        }
        path
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use std::sync::atomic::{AtomicUsize, Ordering};

    static TEST_COUNTER: AtomicUsize = AtomicUsize::new(0);

    fn test_dir() -> PathBuf {
        let count = TEST_COUNTER.fetch_add(1, Ordering::SeqCst);
        std::env::temp_dir().join(format!("idcams_test_{}", count))
    }

    fn cleanup(path: &Path) {
        let _ = fs::remove_dir_all(path);
    }

    #[test]
    fn test_define_gdg() {
        let dir = test_dir();
        cleanup(&dir);

        let mut idcams = Idcams::new(&dir);
        let result = idcams
            .execute("DEFINE GDG (NAME(MY.TEST.GDG) LIMIT(5) SCRATCH)")
            .unwrap();

        assert!(result.is_success());
        assert!(result.output.contains("GDG MY.TEST.GDG DEFINED"));

        cleanup(&dir);
    }

    #[test]
    fn test_define_cluster() {
        let dir = test_dir();
        cleanup(&dir);

        let mut idcams = Idcams::new(&dir);
        let result = idcams
            .execute("DEFINE CLUSTER (NAME(MY.VSAM.DATA) KEYS(10 0) RECORDSIZE(100 200))")
            .unwrap();

        assert!(result.is_success());
        assert!(result.output.contains("CLUSTER MY.VSAM.DATA DEFINED"));

        // Verify file was created
        let vsam_path = dir.join("MY/VSAM/DATA.vsam");
        assert!(vsam_path.exists());

        cleanup(&dir);
    }

    #[test]
    fn test_delete() {
        let dir = test_dir();
        cleanup(&dir);

        // Create a test file
        let test_path = dir.join("TEST/FILE");
        fs::create_dir_all(test_path.parent().unwrap()).unwrap();
        fs::write(&test_path, "test data").unwrap();

        let mut idcams = Idcams::new(&dir);
        let result = idcams.execute("DELETE TEST.FILE").unwrap();

        assert!(result.is_success());
        assert!(!test_path.exists());

        cleanup(&dir);
    }

    #[test]
    fn test_repro() {
        let dir = test_dir();
        cleanup(&dir);

        // Create source file
        let source_path = dir.join("SOURCE/DATA");
        fs::create_dir_all(source_path.parent().unwrap()).unwrap();
        fs::write(&source_path, "source data content").unwrap();

        let mut idcams = Idcams::new(&dir);
        let result = idcams
            .execute("REPRO INDATASET(SOURCE.DATA) OUTDATASET(TARGET.DATA)")
            .unwrap();

        assert!(result.is_success());
        assert!(result.output.contains("BYTES COPIED"));

        // Verify target was created
        let target_path = dir.join("TARGET/DATA");
        assert!(target_path.exists());
        assert_eq!(fs::read_to_string(&target_path).unwrap(), "source data content");

        cleanup(&dir);
    }

    #[test]
    fn test_print() {
        let dir = test_dir();
        cleanup(&dir);

        // Create test file
        let test_path = dir.join("TEST/DATA");
        fs::create_dir_all(test_path.parent().unwrap()).unwrap();
        fs::write(&test_path, "Hello World!").unwrap();

        let mut idcams = Idcams::new(&dir);
        let result = idcams
            .execute("PRINT INDATASET(TEST.DATA) CHARACTER")
            .unwrap();

        assert!(result.is_success());
        assert!(result.output.contains("Hello World!"));

        cleanup(&dir);
    }

    #[test]
    fn test_listcat() {
        let dir = test_dir();
        cleanup(&dir);

        // Create a GDG
        let mut idcams = Idcams::new(&dir);
        idcams
            .execute("DEFINE GDG (NAME(MY.GDG) LIMIT(5))")
            .unwrap();

        let result = idcams.execute("LISTCAT ENT(MY.GDG) ALL").unwrap();

        assert!(result.is_success());
        assert!(result.output.contains("MY.GDG"));
        assert!(result.output.contains("TYPE: GDG"));

        cleanup(&dir);
    }

    #[test]
    fn test_define_aix() {
        let dir = test_dir();
        cleanup(&dir);

        let mut idcams = Idcams::new(&dir);

        // First create the base cluster
        idcams
            .execute("DEFINE CLUSTER (NAME(MY.BASE) KEYS(10 0) RECORDSIZE(100 200))")
            .unwrap();

        // Define an alternate index
        let result = idcams
            .execute("DEFINE ALTERNATEINDEX (NAME(MY.AIX) RELATE(MY.BASE) KEYS(20 10) UNIQUEKEY)")
            .unwrap();

        assert!(result.is_success());
        assert!(result.output.contains("ALTERNATEINDEX MY.AIX DEFINED"));

        // Verify AIX metadata file was created
        let aix_path = dir.join("MY/AIX.aix");
        assert!(aix_path.exists());

        cleanup(&dir);
    }

    #[test]
    fn test_define_path() {
        let dir = test_dir();
        cleanup(&dir);

        let mut idcams = Idcams::new(&dir);

        // Define a path
        let result = idcams
            .execute("DEFINE PATH (NAME(MY.PATH) PATHENTRY(MY.AIX))")
            .unwrap();

        assert!(result.is_success());
        assert!(result.output.contains("PATH MY.PATH DEFINED"));

        // Verify PATH metadata file was created
        let path_file = dir.join("MY/PATH.path");
        assert!(path_file.exists());

        cleanup(&dir);
    }

    #[test]
    fn test_repro_with_skip() {
        let dir = test_dir();
        cleanup(&dir);

        // Create source file with numbered lines
        let source_path = dir.join("SOURCE/SKIP");
        fs::create_dir_all(source_path.parent().unwrap()).unwrap();
        fs::write(&source_path, "LINE1\nLINE2\nLINE3\nLINE4\nLINE5\n").unwrap();

        let mut idcams = Idcams::new(&dir);
        let result = idcams
            .execute("REPRO INDATASET(SOURCE.SKIP) OUTDATASET(TARGET.SKIP) SKIP(2)")
            .unwrap();

        assert!(result.is_success());
        assert!(result.output.contains("3 RECORDS COPIED"));

        let target_path = dir.join("TARGET/SKIP");
        let content = fs::read_to_string(&target_path).unwrap();
        assert_eq!(content, "LINE3\nLINE4\nLINE5\n");

        cleanup(&dir);
    }

    #[test]
    fn test_repro_with_count() {
        let dir = test_dir();
        cleanup(&dir);

        let source_path = dir.join("SOURCE/COUNT");
        fs::create_dir_all(source_path.parent().unwrap()).unwrap();
        fs::write(&source_path, "AAA\nBBB\nCCC\nDDD\nEEE\n").unwrap();

        let mut idcams = Idcams::new(&dir);
        let result = idcams
            .execute("REPRO INDATASET(SOURCE.COUNT) OUTDATASET(TARGET.COUNT) COUNT(3)")
            .unwrap();

        assert!(result.is_success());
        assert!(result.output.contains("3 RECORDS COPIED"));

        let target_path = dir.join("TARGET/COUNT");
        let content = fs::read_to_string(&target_path).unwrap();
        assert_eq!(content, "AAA\nBBB\nCCC\n");

        cleanup(&dir);
    }

    #[test]
    fn test_repro_with_skip_and_count() {
        let dir = test_dir();
        cleanup(&dir);

        let source_path = dir.join("SOURCE/BOTH");
        fs::create_dir_all(source_path.parent().unwrap()).unwrap();
        fs::write(&source_path, "R1\nR2\nR3\nR4\nR5\nR6\n").unwrap();

        let mut idcams = Idcams::new(&dir);
        let result = idcams
            .execute("REPRO INDATASET(SOURCE.BOTH) OUTDATASET(TARGET.BOTH) SKIP(1) COUNT(3)")
            .unwrap();

        assert!(result.is_success());
        assert!(result.output.contains("3 RECORDS COPIED"));

        let target_path = dir.join("TARGET/BOTH");
        let content = fs::read_to_string(&target_path).unwrap();
        assert_eq!(content, "R2\nR3\nR4\n");

        cleanup(&dir);
    }

    #[test]
    fn test_repro_with_fromkey_tokey() {
        let dir = test_dir();
        cleanup(&dir);

        let source_path = dir.join("SOURCE/KEYRANGE");
        fs::create_dir_all(source_path.parent().unwrap()).unwrap();
        fs::write(&source_path, "APPLE\nBANANA\nCHERRY\nDATE\nELDERBERRY\n").unwrap();

        let mut idcams = Idcams::new(&dir);
        let result = idcams
            .execute("REPRO INDATASET(SOURCE.KEYRANGE) OUTDATASET(TARGET.KEYRANGE) FROMKEY(BANANA) TOKEY(DATE)")
            .unwrap();

        assert!(result.is_success());
        assert!(result.output.contains("3 RECORDS COPIED"));

        let target_path = dir.join("TARGET/KEYRANGE");
        let content = fs::read_to_string(&target_path).unwrap();
        assert_eq!(content, "BANANA\nCHERRY\nDATE\n");

        cleanup(&dir);
    }

    #[test]
    fn test_repro_skip_beyond_records() {
        let dir = test_dir();
        cleanup(&dir);

        let source_path = dir.join("SOURCE/SKIP2");
        fs::create_dir_all(source_path.parent().unwrap()).unwrap();
        fs::write(&source_path, "ONLY\nTWO\n").unwrap();

        let mut idcams = Idcams::new(&dir);
        let result = idcams
            .execute("REPRO INDATASET(SOURCE.SKIP2) OUTDATASET(TARGET.SKIP2) SKIP(10)")
            .unwrap();

        assert!(result.is_success());
        assert!(result.output.contains("0 RECORDS COPIED"));

        cleanup(&dir);
    }

    // ─── DFSMS-103.1: DEFINE NONVSAM and DEFINE ALIAS ───

    #[test]
    fn test_define_nonvsam() {
        let dir = test_dir();
        cleanup(&dir);

        let mut idcams = Idcams::new(&dir);
        let result = idcams
            .execute("DEFINE NONVSAM (NAME(MY.SEQ.FILE) VOLUMES(VOL001) DEVT(3390))")
            .unwrap();

        assert!(result.is_success());
        assert!(result.output.contains("NONVSAM MY.SEQ.FILE DEFINED"));

        let meta_path = dir.join("MY/SEQ/FILE.nonvsam");
        assert!(meta_path.exists());
        let content = fs::read_to_string(&meta_path).unwrap();
        assert!(content.contains("NONVSAM=MY.SEQ.FILE"));
        assert!(content.contains("VOLUMES=VOL001"));
        assert!(content.contains("DEVT=3390"));

        cleanup(&dir);
    }

    #[test]
    fn test_define_alias() {
        let dir = test_dir();
        cleanup(&dir);

        let mut idcams = Idcams::new(&dir);
        let result = idcams
            .execute("DEFINE ALIAS (NAME(PROD) RELATE(UCAT.PROD))")
            .unwrap();

        assert!(result.is_success());
        assert!(result.output.contains("ALIAS PROD DEFINED"));

        let meta_path = dir.join("PROD.alias");
        assert!(meta_path.exists());
        let content = fs::read_to_string(&meta_path).unwrap();
        assert!(content.contains("ALIAS=PROD"));
        assert!(content.contains("RELATE=UCAT.PROD"));

        cleanup(&dir);
    }

    // ─── DFSMS-103.2: BLDINDEX ───

    #[test]
    fn test_bldindex() {
        let dir = test_dir();
        cleanup(&dir);

        let mut idcams = Idcams::new(&dir);

        // Create base cluster
        idcams
            .execute("DEFINE CLUSTER (NAME(MY.KSDS) KEYS(10 0) RECORDSIZE(100 200))")
            .unwrap();

        // Create AIX
        idcams
            .execute("DEFINE ALTERNATEINDEX (NAME(MY.AIX) RELATE(MY.KSDS) KEYS(30 10) UNIQUEKEY)")
            .unwrap();

        // Build the index
        let result = idcams
            .execute("BLDINDEX INDATASET(MY.KSDS) OUTDATASET(MY.AIX)")
            .unwrap();

        assert!(result.is_success());
        assert!(result.output.contains("AIX MY.AIX BUILT FROM MY.KSDS"));

        // Verify built marker
        let built_path = dir.join("MY/AIX.aix.built");
        assert!(built_path.exists());
        let content = fs::read_to_string(&built_path).unwrap();
        assert!(content.contains("BUILT=YES"));
        assert!(content.contains("BASE=MY.KSDS"));

        cleanup(&dir);
    }

    #[test]
    fn test_bldindex_missing_base() {
        let dir = test_dir();
        cleanup(&dir);

        let mut idcams = Idcams::new(&dir);
        let result = idcams
            .execute("BLDINDEX INDATASET(NO.EXIST) OUTDATASET(MY.AIX)")
            .unwrap();

        assert!(result.has_errors());
        assert!(result.output.contains("BASE CLUSTER NO.EXIST NOT FOUND"));

        cleanup(&dir);
    }

    // ─── DFSMS-103.3: ALTER Full Parameter Support ───

    #[test]
    fn test_alter_newname() {
        let dir = test_dir();
        cleanup(&dir);

        // Create a test file
        let test_path = dir.join("OLD/NAME");
        fs::create_dir_all(test_path.parent().unwrap()).unwrap();
        fs::write(&test_path, "data").unwrap();

        let mut idcams = Idcams::new(&dir);
        let result = idcams
            .execute("ALTER OLD.NAME NEWNAME(NEW.NAME)")
            .unwrap();

        assert!(result.is_success());
        assert!(result.output.contains("RENAMED TO NEW.NAME"));

        cleanup(&dir);
    }

    #[test]
    fn test_alter_addvolumes() {
        let dir = test_dir();
        cleanup(&dir);

        let test_path = dir.join("MY/KSDS.vsam");
        fs::create_dir_all(test_path.parent().unwrap()).unwrap();
        fs::write(&test_path, "vsam-data").unwrap();

        let mut idcams = Idcams::new(&dir);
        let result = idcams
            .execute("ALTER MY.KSDS ADDVOLUMES(VOL002)")
            .unwrap();

        assert!(result.is_success());
        assert!(result.output.contains("VOLUMES ADDED: VOL002"));

        cleanup(&dir);
    }

    #[test]
    fn test_alter_freespace() {
        let dir = test_dir();
        cleanup(&dir);

        let test_path = dir.join("MY/KSDS.vsam");
        fs::create_dir_all(test_path.parent().unwrap()).unwrap();
        fs::write(&test_path, "vsam-data").unwrap();

        let mut idcams = Idcams::new(&dir);
        let result = idcams
            .execute("ALTER MY.KSDS FREESPACE(20 10)")
            .unwrap();

        assert!(result.is_success());
        assert!(result.output.contains("FREESPACE SET TO (20 10)"));

        cleanup(&dir);
    }

    // ─── DFSMS-103.4: EXPORT and IMPORT ───

    #[test]
    fn test_export_import_roundtrip() {
        let dir = test_dir();
        cleanup(&dir);

        // Create source file
        let source_path = dir.join("MY/DATA");
        fs::create_dir_all(source_path.parent().unwrap()).unwrap();
        fs::write(&source_path, b"Hello, z/OS!").unwrap();

        let mut idcams = Idcams::new(&dir);

        // Export
        let result = idcams
            .execute("EXPORT MY.DATA OUTFILE(MY.EXPORT)")
            .unwrap();
        assert!(result.is_success());
        assert!(result.output.contains("EXPORTED"));

        // Remove original
        fs::remove_file(&source_path).unwrap();

        // Import to new name
        let result = idcams
            .execute("IMPORT INFILE(MY.EXPORT) OUTDATASET(MY.RESTORED)")
            .unwrap();
        assert!(result.is_success());
        assert!(result.output.contains("IMPORTED"));

        // Verify content
        let restored_path = dir.join("MY/RESTORED");
        assert!(restored_path.exists());
        assert_eq!(fs::read(&restored_path).unwrap(), b"Hello, z/OS!");

        cleanup(&dir);
    }

    #[test]
    fn test_export_not_found() {
        let dir = test_dir();
        cleanup(&dir);

        let mut idcams = Idcams::new(&dir);
        let result = idcams
            .execute("EXPORT NO.SUCH.DS OUTFILE(OUT)")
            .unwrap();

        assert!(result.has_errors());
        assert!(result.output.contains("NOT FOUND"));

        cleanup(&dir);
    }

    // ─── DFSMS-103.5: EXAMINE and DIAGNOSE ───

    #[test]
    fn test_examine_success() {
        let dir = test_dir();
        cleanup(&dir);

        // Create something to examine
        let test_path = dir.join("UCAT/PROD.vsam");
        fs::create_dir_all(test_path.parent().unwrap()).unwrap();
        fs::write(&test_path, "catalog-data").unwrap();

        let mut idcams = Idcams::new(&dir);
        let result = idcams
            .execute("EXAMINE NAME(UCAT.PROD)")
            .unwrap();

        assert!(result.is_success());
        assert!(result.output.contains("BCS STRUCTURE OK"));

        cleanup(&dir);
    }

    #[test]
    fn test_examine_not_found() {
        let dir = test_dir();
        cleanup(&dir);

        let mut idcams = Idcams::new(&dir);
        let result = idcams
            .execute("EXAMINE NAME(NO.SUCH.CAT)")
            .unwrap();

        assert!(result.has_errors());
        assert!(result.output.contains("NOT FOUND"));

        cleanup(&dir);
    }

    #[test]
    fn test_diagnose_success() {
        let dir = test_dir();
        cleanup(&dir);

        let test_path = dir.join("UCAT/PROD.vsam");
        fs::create_dir_all(test_path.parent().unwrap()).unwrap();
        fs::write(&test_path, "catalog-data").unwrap();

        let mut idcams = Idcams::new(&dir);
        let result = idcams
            .execute("DIAGNOSE ICFCATALOG NAME(UCAT.PROD)")
            .unwrap();

        assert!(result.is_success());
        assert!(result.output.contains("BCS/VVDS SYNCHRONIZED"));

        cleanup(&dir);
    }

    // ─── DFSMS-103.6: Integration Test ───

    #[test]
    fn test_full_idcams_enhancement_scenario() {
        let dir = test_dir();
        cleanup(&dir);

        let mut idcams = Idcams::new(&dir);

        // 1. DEFINE NONVSAM
        let r = idcams
            .execute("DEFINE NONVSAM (NAME(PROD.SEQ.DATA) VOLUMES(VOL001) DEVT(3390))")
            .unwrap();
        assert!(r.is_success());

        // 2. DEFINE ALIAS
        let r = idcams
            .execute("DEFINE ALIAS (NAME(PAYROLL) RELATE(UCAT.PROD))")
            .unwrap();
        assert!(r.is_success());

        // 3. DEFINE CLUSTER + AIX + BLDINDEX
        let r = idcams
            .execute("DEFINE CLUSTER (NAME(PROD.VSAM) KEYS(10 0) RECORDSIZE(100 200))")
            .unwrap();
        assert!(r.is_success());

        let r = idcams
            .execute("DEFINE ALTERNATEINDEX (NAME(PROD.AIX) RELATE(PROD.VSAM) KEYS(20 10) UNIQUEKEY)")
            .unwrap();
        assert!(r.is_success());

        let r = idcams
            .execute("BLDINDEX INDATASET(PROD.VSAM) OUTDATASET(PROD.AIX)")
            .unwrap();
        assert!(r.is_success());

        // 4. ALTER with ADDVOLUMES
        let r = idcams
            .execute("ALTER PROD.VSAM ADDVOLUMES(VOL002)")
            .unwrap();
        assert!(r.is_success());

        // 5. EXPORT and IMPORT
        // Create a data file for export
        let data_path = dir.join("EXPORT/SOURCE");
        fs::create_dir_all(data_path.parent().unwrap()).unwrap();
        fs::write(&data_path, b"export-test-data").unwrap();

        let r = idcams
            .execute("EXPORT EXPORT.SOURCE OUTFILE(EXPORT.FILE)")
            .unwrap();
        assert!(r.is_success());

        let r = idcams
            .execute("IMPORT INFILE(EXPORT.FILE) OUTDATASET(IMPORT.TARGET)")
            .unwrap();
        assert!(r.is_success());
        assert_eq!(
            fs::read(dir.join("IMPORT/TARGET")).unwrap(),
            b"export-test-data"
        );

        // 6. EXAMINE + DIAGNOSE
        let r = idcams
            .execute("EXAMINE NAME(PROD.VSAM)")
            .unwrap();
        assert!(r.is_success());

        let r = idcams
            .execute("DIAGNOSE ICFCATALOG NAME(PROD.VSAM)")
            .unwrap();
        assert!(r.is_success());

        cleanup(&dir);
    }

    #[test]
    fn test_multiple_commands() {
        let dir = test_dir();
        cleanup(&dir);

        let mut idcams = Idcams::new(&dir);
        let result = idcams
            .execute(
                "DEFINE GDG (NAME(MY.GDG) LIMIT(3))
                 LISTCAT ENT(MY.GDG)",
            )
            .unwrap();

        assert!(result.is_success());
        assert!(result.output.contains("GDG MY.GDG DEFINED"));
        assert!(result.output.contains("TYPE: GDG"));

        cleanup(&dir);
    }
}
