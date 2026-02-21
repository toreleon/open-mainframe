---
version: 'v5.0'
planningGroup: 'PG-18'
technology: 'COBOL Precompilers (DB2 + CICS)'
date: '2026-02-21'
status: 'complete'
inputDocuments:
  - 'prd-precompilers-v5.0.md'
---

# Architecture: COBOL Precompilers (DB2 + CICS)

## 1. Crate Strategy

**Extend existing crate:** `open-mainframe-cobol` (new `precompile/` module)

Rationale: Precompilers operate on COBOL source and produce transformed COBOL source. They are tightly coupled to the COBOL parser and AST. Adding a `precompile/` module within the existing COBOL crate avoids creating a thin wrapper crate.

## 2. Module Layout

```
crates/open-mainframe-cobol/src/
├── precompile/
│   ├── mod.rs          # Precompiler orchestrator (DB2, CICS, integrated)
│   ├── db2/
│   │   ├── mod.rs      # DB2 precompiler entry point
│   │   ├── parser.rs   # EXEC SQL parser
│   │   ├── transform.rs # SQL → CALL DSNHLI transformation
│   │   ├── dbrm.rs     # DBRM generation (binary format)
│   │   ├── host_vars.rs # Host variable mapping
│   │   └── sqlca.rs    # SQLCA/SQLCODE generation
│   ├── cics/
│   │   ├── mod.rs      # CICS precompiler entry point
│   │   ├── parser.rs   # EXEC CICS parser
│   │   ├── transform.rs # CICS → CALL DFHEI1 transformation
│   │   └── eib.rs      # DFHEIBLK/DFHCOMMAREA generation
│   └── integrated.rs   # Combined DB2+CICS precompilation
```

## 3. Key Types

```rust
/// Precompiler mode
pub enum PrecompileMode {
    Db2Only,
    CicsOnly,
    Integrated, // Both DB2 and CICS in same source
}

/// Extracted SQL statement (for DBRM)
pub struct SqlStatement {
    pub sequence: u32,
    pub sql_text: String,
    pub host_variables: Vec<HostVariable>,
    pub statement_type: SqlStatementType,
    pub source_line: u32,
}

pub enum SqlStatementType {
    Select, Insert, Update, Delete,
    DeclareCursor, Open, Fetch, Close,
    Prepare, Describe, Execute, ExecuteImmediate,
    Include, Whenever, Commit, Rollback,
}

/// Host variable mapping
pub struct HostVariable {
    pub cobol_name: String,       // COBOL data name (e.g., WS-EMPLOYEE-ID)
    pub sql_type: SqlType,        // Mapped SQL type
    pub offset: u32,              // Offset in host variable area
    pub length: u32,
    pub nullable: bool,           // Has indicator variable
    pub indicator_name: Option<String>,
}

/// DBRM (Database Request Module)
pub struct Dbrm {
    pub program_name: String,
    pub statements: Vec<SqlStatement>,
    pub host_variables: Vec<HostVariable>,
    pub timestamp: [u8; 8],
}

/// CICS command extracted from source
pub struct CicsCommand {
    pub command: String,          // e.g., "SEND MAP"
    pub options: Vec<CicsOption>, // e.g., MAP('MENU01'), MAPSET('MENUSET')
    pub source_line: u32,
}

pub struct CicsOption {
    pub name: String,
    pub value: CicsOptionValue,
}

pub enum CicsOptionValue {
    Literal(String),
    DataName(String),
    Numeric(i64),
}
```

## 4. Design Decisions

### DD-5.0-PRE-01: Source-to-Source Transformation
**Decision:** Precompilers operate as source-to-source transformers. Input is COBOL text with EXEC SQL/EXEC CICS blocks; output is standard COBOL with CALL statements. This matches real z/OS precompiler behavior and keeps the main COBOL compiler unmodified.

### DD-5.0-PRE-02: DBRM as Binary Artifact
**Decision:** The DBRM is generated as a binary structure (not text) compatible with the DB2 BIND utility. SQL statements are stored with parameter markers; host variable metadata enables BIND to create an access plan.

### DD-5.0-PRE-03: Precompiler Pipeline Order
**Decision:** When both EXEC SQL and EXEC CICS exist in the same source, the precompilation order is: CICS first, then DB2 (matching real z/OS convention). In integrated mode, both are processed in a single pass.
