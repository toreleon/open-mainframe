//! # Binder — Symbol Resolution & Relocation Engine
//!
//! Resolves external references across object modules, applies relocations,
//! and produces bound load modules.

use std::collections::HashMap;

// ─────────────────────── Object Module Structures ───────────────────────

/// External Symbol Dictionary entry type.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EsdType {
    /// SD — Section Definition (CSECT).
    SectionDef,
    /// LD — Label Definition (ENTRY).
    LabelDef,
    /// ER — External Reference.
    ExternalRef,
    /// WX — Weak External Reference.
    WeakExternalRef,
}

/// An ESD entry.
#[derive(Debug, Clone)]
pub struct EsdEntry {
    /// Symbol name.
    pub name: String,
    /// Entry type.
    pub esd_type: EsdType,
    /// ESD ID (assigned by assembler).
    pub esdid: u16,
    /// Offset within containing section.
    pub offset: u32,
    /// Length (for SD entries).
    pub length: u32,
}

/// A Text record (TXT) — code/data bytes.
#[derive(Debug, Clone)]
pub struct TextRecord {
    /// ESDID this text belongs to.
    pub esdid: u16,
    /// Offset within section.
    pub offset: u32,
    /// Data bytes.
    pub data: Vec<u8>,
}

/// Address constant type for relocation.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AdconType {
    /// A-type — absolute address.
    AType,
    /// V-type — external address.
    VType,
}

/// A Relocation Dictionary entry (RLD).
#[derive(Debug, Clone)]
pub struct RldEntry {
    /// Relocation target ESDID.
    pub r_esdid: u16,
    /// Position ESDID (where the adcon lives).
    pub p_esdid: u16,
    /// Offset of adcon in position section.
    pub offset: u32,
    /// Address constant type.
    pub adcon_type: AdconType,
    /// Length of address constant in bytes (3 or 4).
    pub adcon_len: u8,
}

/// An object module (one compilation unit).
#[derive(Debug, Clone)]
pub struct ObjectModule {
    /// Module name.
    pub name: String,
    /// ESD entries.
    pub esd_entries: Vec<EsdEntry>,
    /// Text records.
    pub text_records: Vec<TextRecord>,
    /// RLD entries.
    pub rld_entries: Vec<RldEntry>,
}

// ─────────────────────── Binder ───────────────────────

/// Binder error.
#[derive(Debug, Clone, thiserror::Error)]
pub enum BinderError {
    /// Unresolved external reference.
    #[error("IEW2456E SYMBOL {symbol} UNRESOLVED")]
    UnresolvedSymbol { symbol: String },
    /// Duplicate symbol definition.
    #[error("IEW2453E SYMBOL {symbol} DUPLICATE IN MODULE {module}")]
    DuplicateSymbol { symbol: String, module: String },
    /// Invalid relocation.
    #[error("IEW2470E INVALID RELOCATION AT OFFSET {offset:#010X}")]
    InvalidRelocation { offset: u32 },
}

/// A resolved symbol entry.
#[derive(Debug, Clone)]
pub struct ResolvedSymbol {
    /// Final address (offset from load point).
    pub address: u32,
    /// Defining module name.
    pub module: String,
    /// Whether this is a section or label.
    pub is_section: bool,
}

/// Section placement in the bound module.
#[derive(Debug, Clone)]
struct SectionPlacement {
    /// Start offset in output.
    start: u32,
    /// Length.
    _length: u32,
}

/// A bound load module.
#[derive(Debug, Clone)]
pub struct LoadModule {
    /// Module name.
    pub name: String,
    /// Entry point offset.
    pub entry_point: u32,
    /// Bound text (all sections concatenated + relocated).
    pub text: Vec<u8>,
    /// Symbol table.
    pub symbols: HashMap<String, ResolvedSymbol>,
    /// Aliases.
    pub aliases: Vec<String>,
}

/// The Binder.
pub struct Binder {
    /// Input object modules.
    modules: Vec<ObjectModule>,
    /// Entry point symbol name.
    entry_name: Option<String>,
    /// Output module name.
    output_name: String,
    /// Aliases for the output module.
    aliases: Vec<String>,
}

impl Binder {
    /// Create a new binder.
    pub fn new(output_name: &str) -> Self {
        Self {
            modules: Vec::new(),
            entry_name: None,
            output_name: output_name.to_string(),
            aliases: Vec::new(),
        }
    }

    /// Add an object module.
    pub fn add_module(&mut self, module: ObjectModule) {
        self.modules.push(module);
    }

    /// Set the entry point symbol.
    pub fn set_entry(&mut self, symbol: &str) {
        self.entry_name = Some(symbol.to_string());
    }

    /// Add an alias for the output module.
    pub fn add_alias(&mut self, alias: &str) {
        self.aliases.push(alias.to_string());
    }

    /// Bind all modules into a load module.
    pub fn bind(&self) -> Result<LoadModule, Vec<BinderError>> {
        let mut errors = Vec::new();

        // Phase 1: Assign addresses to all sections (SD entries).
        let mut global_symbols: HashMap<String, ResolvedSymbol> = HashMap::new();
        let mut section_placements: HashMap<(usize, u16), SectionPlacement> = HashMap::new();
        let mut current_offset: u32 = 0;

        for (mod_idx, module) in self.modules.iter().enumerate() {
            for esd in &module.esd_entries {
                match esd.esd_type {
                    EsdType::SectionDef => {
                        // Assign section placement.
                        let aligned = (current_offset + 7) & !7; // doubleword align
                        section_placements.insert(
                            (mod_idx, esd.esdid),
                            SectionPlacement {
                                start: aligned,
                                _length: esd.length,
                            },
                        );

                        if global_symbols.contains_key(&esd.name) {
                            errors.push(BinderError::DuplicateSymbol {
                                symbol: esd.name.clone(),
                                module: module.name.clone(),
                            });
                        } else {
                            global_symbols.insert(
                                esd.name.clone(),
                                ResolvedSymbol {
                                    address: aligned,
                                    module: module.name.clone(),
                                    is_section: true,
                                },
                            );
                        }

                        current_offset = aligned + esd.length;
                    }
                    EsdType::LabelDef => {
                        // Label offset is relative to its containing section.
                        // We'll resolve after all sections are placed.
                    }
                    EsdType::ExternalRef | EsdType::WeakExternalRef => {
                        // Will be resolved later.
                    }
                }
            }
        }

        // Phase 1b: Resolve label definitions.
        for (mod_idx, module) in self.modules.iter().enumerate() {
            for esd in &module.esd_entries {
                if esd.esd_type == EsdType::LabelDef {
                    // Find the section this label belongs to (same ESDID).
                    // Labels reference the section via convention: LD shares ESDID with its SD.
                    // In our model, LD offset is relative to the section start.
                    // We need to find the SD for this module with the matching ESDID.
                    if let Some(placement) = section_placements.get(&(mod_idx, esd.esdid)) {
                        let addr = placement.start + esd.offset;
                        global_symbols.insert(
                            esd.name.clone(),
                            ResolvedSymbol {
                                address: addr,
                                module: module.name.clone(),
                                is_section: false,
                            },
                        );
                    }
                }
            }
        }

        // Phase 2: Check all external references are resolved.
        for module in &self.modules {
            for esd in &module.esd_entries {
                if esd.esd_type == EsdType::ExternalRef && !global_symbols.contains_key(&esd.name)
                {
                    errors.push(BinderError::UnresolvedSymbol {
                        symbol: esd.name.clone(),
                    });
                }
                // Weak external refs are OK if unresolved (resolve to 0).
            }
        }

        if !errors.is_empty() {
            return Err(errors);
        }

        // Phase 3: Build output text.
        let total_size = current_offset as usize;
        let mut text = vec![0u8; total_size];

        for (mod_idx, module) in self.modules.iter().enumerate() {
            for txt in &module.text_records {
                if let Some(placement) = section_placements.get(&(mod_idx, txt.esdid)) {
                    let start = (placement.start + txt.offset) as usize;
                    let end = start + txt.data.len();
                    if end <= text.len() {
                        text[start..end].copy_from_slice(&txt.data);
                    }
                }
            }
        }

        // Phase 4: Apply relocations.
        for (mod_idx, module) in self.modules.iter().enumerate() {
            // Build ESDID → name map for this module.
            let esdid_to_name: HashMap<u16, &str> = module
                .esd_entries
                .iter()
                .map(|e| (e.esdid, e.name.as_str()))
                .collect();

            for rld in &module.rld_entries {
                // Position of the adcon in the output.
                let pos_placement = match section_placements.get(&(mod_idx, rld.p_esdid)) {
                    Some(p) => p,
                    None => continue,
                };
                let adcon_pos = (pos_placement.start + rld.offset) as usize;

                // Target address.
                let target_name = match esdid_to_name.get(&rld.r_esdid) {
                    Some(n) => *n,
                    None => continue,
                };
                let target_addr = match global_symbols.get(target_name) {
                    Some(s) => s.address,
                    None => 0, // Weak external
                };

                // Apply relocation.
                match rld.adcon_len {
                    4 => {
                        if adcon_pos + 4 <= text.len() {
                            let existing =
                                u32::from_be_bytes(text[adcon_pos..adcon_pos + 4].try_into().unwrap_or([0; 4]));
                            let relocated = existing.wrapping_add(target_addr);
                            text[adcon_pos..adcon_pos + 4]
                                .copy_from_slice(&relocated.to_be_bytes());
                        }
                    }
                    3 => {
                        if adcon_pos + 3 <= text.len() {
                            let existing = u32::from_be_bytes([
                                0,
                                text[adcon_pos],
                                text[adcon_pos + 1],
                                text[adcon_pos + 2],
                            ]);
                            let relocated = (existing.wrapping_add(target_addr)) & 0x00FF_FFFF;
                            let bytes = relocated.to_be_bytes();
                            text[adcon_pos] = bytes[1];
                            text[adcon_pos + 1] = bytes[2];
                            text[adcon_pos + 2] = bytes[3];
                        }
                    }
                    _ => {}
                }
            }
        }

        // Determine entry point.
        let entry_point = if let Some(ref name) = self.entry_name {
            global_symbols
                .get(name)
                .map(|s| s.address)
                .unwrap_or(0)
        } else {
            // Default: first section.
            0
        };

        Ok(LoadModule {
            name: self.output_name.clone(),
            entry_point,
            text,
            symbols: global_symbols,
            aliases: self.aliases.clone(),
        })
    }
}

// ─────────────────────── Tests ───────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    fn make_simple_module(name: &str, csect: &str, text: &[u8]) -> ObjectModule {
        ObjectModule {
            name: name.into(),
            esd_entries: vec![EsdEntry {
                name: csect.into(),
                esd_type: EsdType::SectionDef,
                esdid: 1,
                offset: 0,
                length: text.len() as u32,
            }],
            text_records: vec![TextRecord {
                esdid: 1,
                offset: 0,
                data: text.to_vec(),
            }],
            rld_entries: Vec::new(),
        }
    }

    #[test]
    fn test_bind_single_module() {
        let mut binder = Binder::new("TESTMOD");
        binder.add_module(make_simple_module("MOD1", "MAIN", &[0x47, 0xF0, 0x00, 0x08]));
        binder.set_entry("MAIN");

        let result = binder.bind().unwrap();
        assert_eq!(result.name, "TESTMOD");
        assert_eq!(result.entry_point, 0);
        assert_eq!(&result.text[..4], &[0x47, 0xF0, 0x00, 0x08]);
        assert!(result.symbols.contains_key("MAIN"));
    }

    #[test]
    fn test_bind_two_modules() {
        let mut binder = Binder::new("LINKED");

        // Module 1: MAIN section (8 bytes)
        let mod1 = ObjectModule {
            name: "MOD1".into(),
            esd_entries: vec![
                EsdEntry {
                    name: "MAIN".into(),
                    esd_type: EsdType::SectionDef,
                    esdid: 1,
                    offset: 0,
                    length: 8,
                },
                EsdEntry {
                    name: "SUB".into(),
                    esd_type: EsdType::ExternalRef,
                    esdid: 2,
                    offset: 0,
                    length: 0,
                },
            ],
            text_records: vec![TextRecord {
                esdid: 1,
                offset: 0,
                data: vec![0x58, 0x10, 0x00, 0x04, 0x00, 0x00, 0x00, 0x00],
            }],
            rld_entries: vec![RldEntry {
                r_esdid: 2,
                p_esdid: 1,
                offset: 4,
                adcon_type: AdconType::VType,
                adcon_len: 4,
            }],
        };

        // Module 2: SUB section (4 bytes)
        let mod2 = ObjectModule {
            name: "MOD2".into(),
            esd_entries: vec![EsdEntry {
                name: "SUB".into(),
                esd_type: EsdType::SectionDef,
                esdid: 1,
                offset: 0,
                length: 4,
            }],
            text_records: vec![TextRecord {
                esdid: 1,
                offset: 0,
                data: vec![0x07, 0xFE, 0x00, 0x00],
            }],
            rld_entries: Vec::new(),
        };

        binder.add_module(mod1);
        binder.add_module(mod2);
        binder.set_entry("MAIN");

        let result = binder.bind().unwrap();

        // SUB should be placed after MAIN (8 bytes, aligned to doubleword = offset 8)
        let sub_addr = result.symbols.get("SUB").unwrap().address;
        assert_eq!(sub_addr, 8);

        // V-type adcon at offset 4 should be relocated to SUB's address
        let adcon = u32::from_be_bytes(result.text[4..8].try_into().unwrap());
        assert_eq!(adcon, 8);
    }

    #[test]
    fn test_unresolved_symbol() {
        let mut binder = Binder::new("BAD");

        let module = ObjectModule {
            name: "MOD1".into(),
            esd_entries: vec![
                EsdEntry {
                    name: "MAIN".into(),
                    esd_type: EsdType::SectionDef,
                    esdid: 1,
                    offset: 0,
                    length: 4,
                },
                EsdEntry {
                    name: "MISSING".into(),
                    esd_type: EsdType::ExternalRef,
                    esdid: 2,
                    offset: 0,
                    length: 0,
                },
            ],
            text_records: Vec::new(),
            rld_entries: Vec::new(),
        };

        binder.add_module(module);
        let err = binder.bind().unwrap_err();
        assert_eq!(err.len(), 1);
        assert!(err[0].to_string().contains("MISSING"));
        assert!(err[0].to_string().contains("IEW2456E"));
    }

    #[test]
    fn test_duplicate_symbol() {
        let mut binder = Binder::new("DUP");
        binder.add_module(make_simple_module("MOD1", "SAME", &[0x00; 4]));
        binder.add_module(make_simple_module("MOD2", "SAME", &[0x00; 4]));

        let err = binder.bind().unwrap_err();
        assert!(err.iter().any(|e| e.to_string().contains("DUPLICATE")));
    }

    #[test]
    fn test_weak_external_resolves_to_zero() {
        let mut binder = Binder::new("WEAK");

        let module = ObjectModule {
            name: "MOD1".into(),
            esd_entries: vec![
                EsdEntry {
                    name: "MAIN".into(),
                    esd_type: EsdType::SectionDef,
                    esdid: 1,
                    offset: 0,
                    length: 8,
                },
                EsdEntry {
                    name: "OPTIONAL".into(),
                    esd_type: EsdType::WeakExternalRef,
                    esdid: 2,
                    offset: 0,
                    length: 0,
                },
            ],
            text_records: vec![TextRecord {
                esdid: 1,
                offset: 0,
                data: vec![0x00; 8],
            }],
            rld_entries: vec![RldEntry {
                r_esdid: 2,
                p_esdid: 1,
                offset: 4,
                adcon_type: AdconType::VType,
                adcon_len: 4,
            }],
        };

        binder.add_module(module);

        // Weak external — should succeed even though OPTIONAL is not defined
        let result = binder.bind().unwrap();
        let adcon = u32::from_be_bytes(result.text[4..8].try_into().unwrap());
        assert_eq!(adcon, 0);
    }

    #[test]
    fn test_label_definition() {
        let mut binder = Binder::new("LABELS");

        let module = ObjectModule {
            name: "MOD1".into(),
            esd_entries: vec![
                EsdEntry {
                    name: "MAIN".into(),
                    esd_type: EsdType::SectionDef,
                    esdid: 1,
                    offset: 0,
                    length: 16,
                },
                EsdEntry {
                    name: "ENTRY2".into(),
                    esd_type: EsdType::LabelDef,
                    esdid: 1,
                    offset: 8,
                    length: 0,
                },
            ],
            text_records: vec![TextRecord {
                esdid: 1,
                offset: 0,
                data: vec![0x00; 16],
            }],
            rld_entries: Vec::new(),
        };

        binder.add_module(module);
        binder.set_entry("ENTRY2");

        let result = binder.bind().unwrap();
        assert_eq!(result.entry_point, 8);
        assert!(result.symbols.contains_key("ENTRY2"));
    }

    #[test]
    fn test_3_byte_adcon_relocation() {
        let mut binder = Binder::new("ADCON3");

        let module = ObjectModule {
            name: "MOD1".into(),
            esd_entries: vec![
                EsdEntry {
                    name: "CODE".into(),
                    esd_type: EsdType::SectionDef,
                    esdid: 1,
                    offset: 0,
                    length: 8,
                },
                EsdEntry {
                    name: "DATA".into(),
                    esd_type: EsdType::SectionDef,
                    esdid: 2,
                    offset: 0,
                    length: 4,
                },
            ],
            text_records: vec![
                TextRecord {
                    esdid: 1,
                    offset: 0,
                    data: vec![0x58, 0x10, 0xF0, 0x04, 0x00, 0x00, 0x00, 0x00],
                },
                TextRecord {
                    esdid: 2,
                    offset: 0,
                    data: vec![0xC1, 0xC2, 0xC3, 0xC4],
                },
            ],
            rld_entries: vec![RldEntry {
                r_esdid: 2,
                p_esdid: 1,
                offset: 5,
                adcon_type: AdconType::AType,
                adcon_len: 3,
            }],
        };

        binder.add_module(module);
        let result = binder.bind().unwrap();

        // DATA section at offset 8 (doubleword aligned after 8-byte CODE)
        let data_addr = result.symbols.get("DATA").unwrap().address;
        assert_eq!(data_addr, 8);

        // 3-byte adcon at offset 5 should contain data_addr (low 3 bytes)
        let adcon_val = u32::from_be_bytes([0, result.text[5], result.text[6], result.text[7]]);
        assert_eq!(adcon_val, 8);
    }

    #[test]
    fn test_aliases() {
        let mut binder = Binder::new("MAIN");
        binder.add_module(make_simple_module("MOD1", "MAIN", &[0x07, 0xFE]));
        binder.add_alias("ALIAS1");
        binder.add_alias("ALIAS2");

        let result = binder.bind().unwrap();
        assert_eq!(result.aliases, vec!["ALIAS1", "ALIAS2"]);
    }
}
