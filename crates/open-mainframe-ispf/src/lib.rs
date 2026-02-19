//! ISPF — Interactive System Productivity Facility.
//!
//! This crate provides:
//!
//! - **Panel** — ISPF panel definition parser and field extraction
//! - Sections: )ATTR, )BODY, )INIT, )REINIT, )PROC, )MODEL, )AREA, )END
//! - Attribute characters with TYPE, INTENS, CAPS, JUST, PAD
//! - Executable statements: assignment, IF/ELSE, VER, VGET, VPUT, TRANS, TRUNC
//! - **Dialog** — ISPF dialog manager: DISPLAY, SELECT, SETMSG, CONTROL, variable pools
//! - Four-pool variable model: function, shared, profile, system
//! - VGET/VPUT/VERASE services, pop-up management, message handling

pub mod dialog;
pub mod panel;

pub use dialog::{DialogManager, DisplayEvent, IspfVarPools, MessageDef, VarType};
pub use panel::{
    extract_fields, parse_panel, AreaDef, CmpOp, FieldAttr, FieldType, Intensity,
    Justification, Panel, PanelCond, PanelError, PanelExpr, PanelField, PanelStmt,
    VarPool, VerCheck,
};
