//! MFS-101: MFS Control Block Compiler.
//!
//! Compiles parsed MFS definitions into runtime control blocks:
//! - **MID** -- Message Input Descriptor (from MSG INPUT)
//! - **MOD** -- Message Output Descriptor (from MSG OUTPUT)
//! - **DIF** -- Device Input Format
//! - **DOF** -- Device Output Format
//!
//! A [`FormatLibrary`] stores compiled control blocks by name.

use crate::ImsResult;
use crate::mfs_parser::{MfsMsgDef, MfsFmtDef, MsgType};
use std::collections::HashMap;

// ---------------------------------------------------------------------------
// Compiled control blocks
// ---------------------------------------------------------------------------

/// A compiled field descriptor (used in MID/MOD/DIF/DOF).
#[derive(Debug, Clone)]
pub struct CompiledField {
    /// Field name.
    pub name: String,
    /// Byte offset within the segment/screen.
    pub offset: usize,
    /// Field length.
    pub length: usize,
}

/// A compiled segment descriptor.
#[derive(Debug, Clone)]
pub struct CompiledSegment {
    /// Segment name.
    pub name: String,
    /// Fields in this segment.
    pub fields: Vec<CompiledField>,
    /// Total segment length.
    pub total_length: usize,
}

/// Message Input Descriptor -- compiled from a MSG/INPUT definition.
#[derive(Debug, Clone)]
pub struct Mid {
    /// MID name.
    pub name: String,
    /// Compiled segments.
    pub segments: Vec<CompiledSegment>,
}

/// Message Output Descriptor -- compiled from a MSG/OUTPUT definition.
#[derive(Debug, Clone)]
pub struct Mod {
    /// MOD name.
    pub name: String,
    /// Compiled segments.
    pub segments: Vec<CompiledSegment>,
}

/// Device Input Format -- compiled from the INPUT division of a FMT.
#[derive(Debug, Clone)]
pub struct Dif {
    /// DIF name.
    pub name: String,
    /// Device type.
    pub device_type: String,
    /// Screen field map.
    pub fields: Vec<CompiledScreenField>,
}

/// Device Output Format -- compiled from the OUTPUT division of a FMT.
#[derive(Debug, Clone)]
pub struct Dof {
    /// DOF name.
    pub name: String,
    /// Device type.
    pub device_type: String,
    /// Screen field map.
    pub fields: Vec<CompiledScreenField>,
}

/// A compiled screen field (position-based, for DIF/DOF).
#[derive(Debug, Clone)]
pub struct CompiledScreenField {
    /// Field name.
    pub name: String,
    /// Row on screen.
    pub row: u16,
    /// Column on screen.
    pub col: u16,
    /// Field length.
    pub length: usize,
}

// ---------------------------------------------------------------------------
// Compiler
// ---------------------------------------------------------------------------

/// Compiles parsed MFS definitions into MID/MOD/DIF/DOF control blocks.
#[derive(Debug, Default)]
pub struct MfsCompiler;

impl MfsCompiler {
    /// Create a new compiler.
    pub fn new() -> Self {
        Self
    }

    /// Compile a MSG definition into a MID or MOD.
    pub fn compile_msg(&self, msg: &MfsMsgDef) -> ImsResult<CompiledMsg> {
        let mut segments = Vec::new();

        for lpage in &msg.lpages {
            for seg in &lpage.segs {
                let mut fields = Vec::new();
                let mut running_offset = 0;

                for mfld in &seg.mflds {
                    let offset = if mfld.offset > 0 {
                        mfld.offset
                    } else {
                        running_offset
                    };
                    fields.push(CompiledField {
                        name: mfld.name.clone(),
                        offset,
                        length: mfld.length,
                    });
                    running_offset = offset + mfld.length;
                }

                segments.push(CompiledSegment {
                    name: seg.name.clone(),
                    total_length: running_offset,
                    fields,
                });
            }
        }

        match msg.msg_type {
            MsgType::Input => Ok(CompiledMsg::Mid(Mid {
                name: msg.name.clone(),
                segments,
            })),
            MsgType::Output => Ok(CompiledMsg::Mod(Mod {
                name: msg.name.clone(),
                segments,
            })),
        }
    }

    /// Compile a FMT definition into DIF and DOF control blocks.
    pub fn compile_fmt(&self, fmt: &MfsFmtDef) -> ImsResult<Vec<CompiledFmt>> {
        let mut result = Vec::new();

        for dev in &fmt.devs {
            for div in &dev.divs {
                let mut fields = Vec::new();
                for dpage in &div.dpages {
                    for dfld in &dpage.dflds {
                        fields.push(CompiledScreenField {
                            name: dfld.name.clone(),
                            row: dfld.row,
                            col: dfld.col,
                            length: dfld.length,
                        });
                    }
                }

                let compiled = match div.div_type {
                    MsgType::Input => CompiledFmt::Dif(Dif {
                        name: fmt.name.clone(),
                        device_type: dev.device_type.clone(),
                        fields,
                    }),
                    MsgType::Output => CompiledFmt::Dof(Dof {
                        name: fmt.name.clone(),
                        device_type: dev.device_type.clone(),
                        fields,
                    }),
                };
                result.push(compiled);
            }
        }

        Ok(result)
    }
}

/// Result of compiling a MSG definition.
#[derive(Debug, Clone)]
pub enum CompiledMsg {
    /// Message Input Descriptor.
    Mid(Mid),
    /// Message Output Descriptor.
    Mod(Mod),
}

/// Result of compiling a FMT division.
#[derive(Debug, Clone)]
pub enum CompiledFmt {
    /// Device Input Format.
    Dif(Dif),
    /// Device Output Format.
    Dof(Dof),
}

// ---------------------------------------------------------------------------
// Format Library
// ---------------------------------------------------------------------------

/// Stores compiled MFS control blocks by name.
#[derive(Debug, Default)]
pub struct FormatLibrary {
    /// MID entries.
    mids: HashMap<String, Mid>,
    /// MOD entries.
    mods: HashMap<String, Mod>,
    /// DIF entries.
    difs: HashMap<String, Dif>,
    /// DOF entries.
    dofs: HashMap<String, Dof>,
}

impl FormatLibrary {
    /// Create a new empty library.
    pub fn new() -> Self {
        Self::default()
    }

    /// Add a compiled MSG (MID or MOD) to the library.
    pub fn add_msg(&mut self, compiled: CompiledMsg) {
        match compiled {
            CompiledMsg::Mid(mid) => {
                self.mids.insert(mid.name.clone(), mid);
            }
            CompiledMsg::Mod(m) => {
                self.mods.insert(m.name.clone(), m);
            }
        }
    }

    /// Add a compiled FMT (DIF or DOF) to the library.
    pub fn add_fmt(&mut self, compiled: CompiledFmt) {
        match compiled {
            CompiledFmt::Dif(dif) => {
                self.difs.insert(dif.name.clone(), dif);
            }
            CompiledFmt::Dof(dof) => {
                self.dofs.insert(dof.name.clone(), dof);
            }
        }
    }

    /// Look up a MID by name.
    pub fn get_mid(&self, name: &str) -> Option<&Mid> {
        self.mids.get(name)
    }

    /// Look up a MOD by name.
    pub fn get_mod(&self, name: &str) -> Option<&Mod> {
        self.mods.get(name)
    }

    /// Look up a DIF by name.
    pub fn get_dif(&self, name: &str) -> Option<&Dif> {
        self.difs.get(name)
    }

    /// Look up a DOF by name.
    pub fn get_dof(&self, name: &str) -> Option<&Dof> {
        self.dofs.get(name)
    }

    /// Return total number of entries.
    pub fn total_entries(&self) -> usize {
        self.mids.len() + self.mods.len() + self.difs.len() + self.dofs.len()
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::mfs_parser::{
        MfsLpage, MfsMfld, MfsMsgDef, MfsSeg, MfsFmtDef, MfsDev, MfsDiv,
        MfsDpage, MfsDfld, MsgType, ExtendedAttributes,
    };

    fn sample_output_msg() -> MfsMsgDef {
        MfsMsgDef {
            name: "OUTMSG1".to_string(),
            msg_type: MsgType::Output,
            lpages: vec![MfsLpage {
                name: "LP1".to_string(),
                segs: vec![MfsSeg {
                    name: "SEG1".to_string(),
                    mflds: vec![
                        MfsMfld {
                            name: "FLD1".to_string(),
                            length: 20,
                            offset: 0,
                            literal: None,
                            attributes: ExtendedAttributes::default(),
                        },
                        MfsMfld {
                            name: "FLD2".to_string(),
                            length: 10,
                            offset: 20,
                            literal: None,
                            attributes: ExtendedAttributes::default(),
                        },
                    ],
                }],
            }],
        }
    }

    fn sample_input_msg() -> MfsMsgDef {
        MfsMsgDef {
            name: "INMSG1".to_string(),
            msg_type: MsgType::Input,
            lpages: vec![MfsLpage {
                name: "LP1".to_string(),
                segs: vec![MfsSeg {
                    name: "SEG1".to_string(),
                    mflds: vec![MfsMfld {
                        name: "INFLD".to_string(),
                        length: 8,
                        offset: 0,
                        literal: None,
                        attributes: ExtendedAttributes::default(),
                    }],
                }],
            }],
        }
    }

    fn sample_fmt() -> MfsFmtDef {
        MfsFmtDef {
            name: "FMT01".to_string(),
            devs: vec![MfsDev {
                device_type: "3270-2".to_string(),
                divs: vec![MfsDiv {
                    div_type: MsgType::Output,
                    dpages: vec![MfsDpage {
                        name: "DP1".to_string(),
                        dflds: vec![
                            MfsDfld {
                                name: "DF1".to_string(),
                                row: 1,
                                col: 1,
                                length: 20,
                                attributes: ExtendedAttributes::default(),
                            },
                            MfsDfld {
                                name: "DF2".to_string(),
                                row: 2,
                                col: 1,
                                length: 10,
                                attributes: ExtendedAttributes::default(),
                            },
                        ],
                    }],
                }],
            }],
        }
    }

    #[test]
    fn test_compile_output_msg() {
        let compiler = MfsCompiler::new();
        let result = compiler.compile_msg(&sample_output_msg()).unwrap();
        match result {
            CompiledMsg::Mod(m) => {
                assert_eq!(m.name, "OUTMSG1");
                assert_eq!(m.segments.len(), 1);
                assert_eq!(m.segments[0].fields.len(), 2);
                assert_eq!(m.segments[0].total_length, 30);
            }
            _ => panic!("Expected MOD"),
        }
    }

    #[test]
    fn test_compile_input_msg() {
        let compiler = MfsCompiler::new();
        let result = compiler.compile_msg(&sample_input_msg()).unwrap();
        match result {
            CompiledMsg::Mid(mid) => {
                assert_eq!(mid.name, "INMSG1");
                assert_eq!(mid.segments.len(), 1);
                assert_eq!(mid.segments[0].fields.len(), 1);
                assert_eq!(mid.segments[0].fields[0].length, 8);
            }
            _ => panic!("Expected MID"),
        }
    }

    #[test]
    fn test_compile_fmt() {
        let compiler = MfsCompiler::new();
        let result = compiler.compile_fmt(&sample_fmt()).unwrap();
        assert_eq!(result.len(), 1);
        match &result[0] {
            CompiledFmt::Dof(dof) => {
                assert_eq!(dof.name, "FMT01");
                assert_eq!(dof.device_type, "3270-2");
                assert_eq!(dof.fields.len(), 2);
                assert_eq!(dof.fields[0].row, 1);
                assert_eq!(dof.fields[1].row, 2);
            }
            _ => panic!("Expected DOF"),
        }
    }

    #[test]
    fn test_format_library() {
        let compiler = MfsCompiler::new();
        let mut lib = FormatLibrary::new();

        let msg_out = compiler.compile_msg(&sample_output_msg()).unwrap();
        lib.add_msg(msg_out);

        let msg_in = compiler.compile_msg(&sample_input_msg()).unwrap();
        lib.add_msg(msg_in);

        let fmts = compiler.compile_fmt(&sample_fmt()).unwrap();
        for f in fmts {
            lib.add_fmt(f);
        }

        assert!(lib.get_mod("OUTMSG1").is_some());
        assert!(lib.get_mid("INMSG1").is_some());
        assert!(lib.get_dof("FMT01").is_some());
        assert_eq!(lib.total_entries(), 3);
    }

    #[test]
    fn test_format_library_lookup_missing() {
        let lib = FormatLibrary::new();
        assert!(lib.get_mid("NOEXIST").is_none());
        assert!(lib.get_mod("NOEXIST").is_none());
        assert!(lib.get_dif("NOEXIST").is_none());
        assert!(lib.get_dof("NOEXIST").is_none());
    }

    #[test]
    fn test_compiled_field_offsets() {
        let compiler = MfsCompiler::new();
        let result = compiler.compile_msg(&sample_output_msg()).unwrap();
        if let CompiledMsg::Mod(m) = result {
            assert_eq!(m.segments[0].fields[0].offset, 0);
            assert_eq!(m.segments[0].fields[1].offset, 20);
        }
    }

    #[test]
    fn test_compiled_segment_total_length() {
        let compiler = MfsCompiler::new();
        let result = compiler.compile_msg(&sample_output_msg()).unwrap();
        if let CompiledMsg::Mod(m) = result {
            assert_eq!(m.segments[0].total_length, 30); // 20 + 10
        }
    }

    #[test]
    fn test_auto_offset_calculation() {
        let msg = MfsMsgDef {
            name: "AUTO".to_string(),
            msg_type: MsgType::Output,
            lpages: vec![MfsLpage {
                name: "LP".to_string(),
                segs: vec![MfsSeg {
                    name: "S1".to_string(),
                    mflds: vec![
                        MfsMfld {
                            name: "F1".to_string(),
                            length: 10,
                            offset: 0, // auto
                            literal: None,
                            attributes: ExtendedAttributes::default(),
                        },
                        MfsMfld {
                            name: "F2".to_string(),
                            length: 5,
                            offset: 0, // auto: should be 10
                            literal: None,
                            attributes: ExtendedAttributes::default(),
                        },
                    ],
                }],
            }],
        };
        let compiler = MfsCompiler::new();
        let result = compiler.compile_msg(&msg).unwrap();
        if let CompiledMsg::Mod(m) = result {
            assert_eq!(m.segments[0].fields[0].offset, 0);
            assert_eq!(m.segments[0].fields[1].offset, 10);
            assert_eq!(m.segments[0].total_length, 15);
        }
    }
}
