//! ISPF — Interactive System Productivity Facility.
//!
//! This crate provides:
//!
//! - **Panel** — ISPF panel definition parser and field extraction
//! - Sections: )ATTR, )BODY, )INIT, )REINIT, )PROC, )MODEL, )AREA, )END
//! - Attribute characters with TYPE, INTENS, CAPS, JUST, PAD
//! - Executable statements: assignment, IF/ELSE, VER, VGET, VPUT, TRANS, TRUNC

pub mod panel;

pub use panel::{
    extract_fields, parse_panel, AreaDef, CmpOp, FieldAttr, FieldType, Intensity,
    Justification, Panel, PanelCond, PanelError, PanelExpr, PanelField, PanelStmt,
    VarPool, VerCheck,
};
