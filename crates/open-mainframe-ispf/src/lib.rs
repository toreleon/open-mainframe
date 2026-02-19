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
//! - **Table** — ISPF table services: in-memory tables with row ops, search, sort, persistence
//! - TBCREATE/TBOPEN/TBCLOSE/TBEND/TBSAVE lifecycle
//! - TBADD/TBPUT/TBMOD/TBDELETE/TBTOP/TBBOT/TBSKIP row operations
//! - TBSCAN/TBSARG/TBSORT search and sort
//! - **Skeleton** — ISPF file tailoring: skeleton processing with &var substitution
//! - FTOPEN/FTINCL/FTCLOSE services
//! - )SEL/)ENDSEL conditional, )DOT/)ENDDOT table iteration, )IM imbed
//! - **Editor** — ISPF line editor with line/primary commands, profiles, undo/redo
//! - Line commands: I, D, C, M, R, CC/DD/MM blocks, >, <, X, S, UC, LC
//! - Primary commands: FIND, CHANGE, SORT, SUBMIT, RESET, PROFILE, BOUNDS

pub mod dialog;
pub mod editor;
pub mod library;
pub mod panel;
pub mod skeleton;
pub mod table;

pub use dialog::{DialogManager, DisplayEvent, IspfVarPools, MessageDef, VarType};
pub use panel::{
    extract_fields, parse_panel, AreaDef, CmpOp, FieldAttr, FieldType, Intensity,
    Justification, Panel, PanelCond, PanelError, PanelExpr, PanelField, PanelStmt,
    VarPool, VerCheck,
};
pub use editor::{EditProfile, Editor, EditorLine, FindResult, VisibleLine};
pub use library::{
    LibdefEntry, LibdefType, LibraryManager, LmRc, MemberStats, OpenMode, PdsMember,
};
pub use skeleton::FileTailor;
pub use table::{IspfTable, SearchCond, TableManager, TableStats};
