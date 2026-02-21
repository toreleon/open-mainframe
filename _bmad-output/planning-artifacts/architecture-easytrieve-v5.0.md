---
version: 'v5.0'
planningGroup: 'PG-26'
technology: 'Easytrieve'
date: '2026-02-21'
status: 'complete'
inputDocuments:
  - 'prd-easytrieve-v5.0.md'
---

# Architecture: Easytrieve

## 1. Crate Strategy

**New crate:** `open-mainframe-easytrieve`

Rationale: Easytrieve is a distinct 4GL with its own parser, interpreter, file processing model, and report engine. A dedicated crate keeps the language isolated while leveraging existing dataset and DB2 crates for I/O and SQL.

## 2. Module Layout

```
crates/open-mainframe-easytrieve/src/
├── lib.rs              # Crate root, public API
├── parser/
│   ├── mod.rs          # Parser entry point
│   ├── lexer.rs        # 80-column tokenizer
│   ├── ast.rs          # AST nodes (FILE, FIELD, JOB, SORT, REPORT)
│   └── expr.rs         # Expression parser
├── interpreter/
│   ├── mod.rs          # Main execution loop
│   ├── job.rs          # JOB activity (automatic file processing)
│   ├── sort.rs         # SORT activity
│   ├── variables.rs    # Field storage and type system
│   └── control.rs      # IF/DO/PERFORM/GOTO
├── report/
│   ├── mod.rs          # Report engine
│   ├── formatter.rs    # LINE/TITLE/HEADING formatting
│   ├── breaks.rs       # CONTROL breaks and SUM totaling
│   └── output.rs       # Print output generation
├── io/
│   ├── mod.rs          # File I/O dispatch
│   ├── sequential.rs   # Sequential file processing
│   ├── indexed.rs      # VSAM/indexed access
│   └── match_files.rs  # Multi-file synchronization
├── sql_bridge.rs       # DB2 SQL integration
├── dli_bridge.rs       # IMS/DLI integration
├── macros.rs           # %INCLUDE, CALL, LINK
└── error.rs            # Error types
```

## 3. Key Types

```rust
/// Easytrieve data types
pub enum FieldType {
    Alpha(u32),        // A — alphanumeric, length in bytes
    Numeric(u32, u32), // N — numeric display (length, decimals)
    Packed(u32, u32),  // P — packed decimal
    Binary(u32),       // B — binary
    Unsigned(u32, u32),// U — unsigned numeric
    Integer(u32),      // I — integer
}

/// File definition
pub struct FileDefinition {
    pub name: String,
    pub lrecl: u32,
    pub recfm: RecFm,
    pub file_type: FileType,
    pub key_fields: Vec<String>,
    pub fields: Vec<FieldDefinition>,
}

/// Report definition
pub struct ReportDefinition {
    pub name: String,
    pub lines: Vec<ReportLine>,
    pub titles: Vec<TitleLine>,
    pub controls: Vec<ControlBreak>,
    pub sums: Vec<SumField>,
    pub page_size: u32,
    pub line_size: u32,
}

/// JOB activity — automatic file processing loop
pub struct JobActivity {
    pub input_files: Vec<String>,
    pub statements: Vec<Statement>,
    pub reports: Vec<String>,
}
```

## 4. Design Decisions

### DD-5.0-EZ-01: Automatic File Processing Loop
**Decision:** JOB activity is implemented as an automatic loop: open input files, read records, execute user statements for each record, and close files at EOF. This matches Easytrieve's core paradigm where the programmer focuses on per-record logic while the runtime handles I/O loops.

### DD-5.0-EZ-02: Report Engine as Separate Module
**Decision:** The report engine is a standalone module that accumulates data during JOB processing and produces formatted output at control breaks and end-of-file. This separation allows report formatting to be tested independently of file processing.
