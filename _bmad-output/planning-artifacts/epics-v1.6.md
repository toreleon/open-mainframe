---
version: 'v1.6'
baseVersion: 'v1.5'
date: '2026-02-14'
status: 'draft'
---

# Epics - OpenMainframe v1.6: Shared Language Core

## Epic Overview

| Epic | Name | Stories | Complexity | Phase |
|------|------|---------|------------|-------|
| 51 | Create open-mainframe-lang-core Crate | 4 | M | Foundation |
| 52 | COBOL Crate Integration | 5 | M | Integration |
| 53 | JCL Crate Integration | 4 | M | Integration |
| 54 | COBOL Crate Restructuring | 4 | L | Refactor |

**Total: 4 Epics, 17 Stories**

---

## Requirements Coverage Map

| Requirement | Epic(s) |
|-------------|---------|
| FR-v1.6-001 Shared Source Location Types | 51 |
| FR-v1.6-002 Shared Diagnostic Type | 51 |
| FR-v1.6-003 Common AstNode Trait | 51, 52, 53 |
| FR-v1.6-004 JCL Source Location Tracking | 53 |
| FR-v1.6-005 COBOL Backward Compatibility | 52 |
| FR-v1.6-006 COBOL Module Restructuring | 54 |
| NFR-v1.6-001 Zero Additional Dependencies | 51 |
| NFR-v1.6-002 Build Time | 52, 53 |
| NFR-v1.6-003 Test Coverage | 52, 53, 54 |

---

## Epic 51: Create open-mainframe-lang-core Crate

**Goal:** Create the shared foundation crate with types and traits for all language compilers.

**Crate:** `open-mainframe-lang-core` (new)
**FRs:** FR-v1.6-001, FR-v1.6-002, FR-v1.6-003, NFR-v1.6-001

### Story 51.1: Crate Scaffold and Workspace Registration

As a **developer**,
I want **the open-mainframe-lang-core crate created and registered in the workspace**,
So that **other crates can depend on it**.

**Acceptance Criteria:**

**Given** the workspace Cargo.toml
**When** `open-mainframe-lang-core` is added to members and workspace.dependencies
**Then** `cargo check -p open-mainframe-lang-core` succeeds

**Given** the crate's Cargo.toml
**When** `[dependencies]` is inspected
**Then** it is empty (zero external dependencies)

**Complexity:** S
**Supports:** NFR-v1.6-001

---

### Story 51.2: Span, FileId, and Location Types

As a **language crate developer**,
I want **shared source location types**,
So that **all languages track source positions uniformly**.

**Acceptance Criteria:**

**Given** `FileId(u32)` struct
**When** used
**Then** it supports `Debug, Clone, Copy, PartialEq, Eq, Hash, Default` derives and `FileId::MAIN` constant

**Given** `Span { file: FileId, start: u32, end: u32 }` struct
**When** used
**Then** it provides `new()`, `main()`, `point()`, `dummy()`, `len()`, `is_empty()`, `extend()`, `to_range()` methods

**Given** `Location { line: u32, column: u32, file_name: String }` struct
**When** used
**Then** it provides `new()` constructor

**Given** `offset_to_line_col(source: &str, offset: usize)` function
**When** called with a source string and byte offset
**Then** returns correct 1-indexed (line, column) pair

**Given** unit tests from the COBOL crate's span.rs
**When** run against the new implementation
**Then** all pass identically

**Complexity:** M
**Supports:** FR-v1.6-001

---

### Story 51.3: Diagnostic and Severity Types

As a **language crate developer**,
I want **shared diagnostic types**,
So that **all languages report errors/warnings uniformly**.

**Acceptance Criteria:**

**Given** `Severity` enum with `Error`, `Warning`, `Info` variants
**When** used
**Then** it supports `Debug, Clone, Copy, PartialEq, Eq` derives

**Given** `Diagnostic` struct with severity, code, message, span, suggestion fields
**When** created via `Diagnostic::error()` or `Diagnostic::warning()`
**Then** correct severity is set

**Given** `Diagnostic::with_suggestion()`
**When** called
**Then** suggestion field is set

**Complexity:** S
**Supports:** FR-v1.6-002

---

### Story 51.4: AstNode, Lexer, and Parse Traits

As a **language crate developer**,
I want **shared compiler pipeline traits**,
So that **generic tooling can operate across languages**.

**Acceptance Criteria:**

**Given** `AstNode` trait with `fn span(&self) -> Span`
**When** a type implements it
**Then** its source location is accessible

**Given** `Lexer` trait with associated types `Token` and `Error`
**When** `tokenize()` is called
**Then** returns `(Vec<Token>, Vec<Error>)`

**Given** `Parse` trait with associated types `Ast: AstNode` and `Error`
**When** `parse()` is called
**Then** returns `(Option<Ast>, Vec<Error>)`

**Complexity:** S
**Supports:** FR-v1.6-003

---

## Epic 52: COBOL Crate Integration

**Goal:** Wire the COBOL crate to use shared types from lang-core while preserving backward compatibility.

**Crate:** `open-mainframe-cobol`
**FRs:** FR-v1.6-005, NFR-v1.6-003

### Story 52.1: Add lang-core Dependency

As a **developer**,
I want **open-mainframe-cobol to depend on open-mainframe-lang-core**,
So that **it can use the shared types**.

**Acceptance Criteria:**

**Given** `Cargo.toml` updated with `open-mainframe-lang-core.workspace = true`
**When** `cargo check -p open-mainframe-cobol` runs
**Then** it compiles successfully

**Complexity:** S

---

### Story 52.2: Replace Span/FileId/Location with lang-core Types

As a **developer**,
I want **the COBOL crate to use lang-core's Span/FileId/Location**,
So that **these types are shared**.

**Acceptance Criteria:**

**Given** `lexer/span.rs` is updated to re-export from lang-core
**When** code references `crate::lexer::Span`
**Then** it resolves to `open_mainframe_lang_core::Span`

**Given** `lib.rs` re-exports `Span`, `FileId`, `Location`
**When** external code uses `open_mainframe_cobol::Span`
**Then** it compiles unchanged

**Given** all existing tests
**When** run
**Then** all pass without modification

**Complexity:** M
**Supports:** FR-v1.6-005

---

### Story 52.3: Replace Diagnostic/Severity with lang-core Types

As a **developer**,
I want **the COBOL crate to use lang-core's Diagnostic/Severity**,
So that **diagnostics are shared**.

**Acceptance Criteria:**

**Given** `semantic/analyzer.rs` is updated to use lang-core types
**When** `Diagnostic::error()` is called
**Then** it creates a `open_mainframe_lang_core::Diagnostic`

**Given** `lib.rs` re-exports `Diagnostic`, `Severity`
**When** external code uses `open_mainframe_cobol::Diagnostic`
**Then** it compiles unchanged

**Given** all semantic analysis tests
**When** run
**Then** all pass without modification

**Complexity:** M
**Supports:** FR-v1.6-005

---

### Story 52.4: Implement AstNode for Program

As a **developer**,
I want **`Program` to implement the `AstNode` trait**,
So that **generic tooling can access its span**.

**Acceptance Criteria:**

**Given** `ast/mod.rs` with `impl AstNode for Program`
**When** `program.span()` is called
**Then** returns the program's source span

**Complexity:** S
**Supports:** FR-v1.6-003

---

### Story 52.5: Verify Full Backward Compatibility

As a **developer**,
I want **all workspace crates that depend on open-mainframe-cobol to compile unchanged**,
So that **the refactoring has no breaking changes**.

**Acceptance Criteria:**

**Given** `cargo check --workspace`
**When** run
**Then** zero compilation errors

**Given** `cargo test --workspace`
**When** run
**Then** all existing tests pass

**Given** crates open-mainframe-assess, open-mainframe-runtime, open-mainframe-cics, open-mainframe-tui
**When** they reference COBOL types
**Then** imports resolve unchanged

**Complexity:** S
**Supports:** FR-v1.6-005, NFR-v1.6-003

---

## Epic 53: JCL Crate Integration

**Goal:** Wire the JCL crate to use shared types and add source location tracking.

**Crate:** `open-mainframe-jcl`
**FRs:** FR-v1.6-004

### Story 53.1: Add lang-core Dependency

As a **developer**,
I want **open-mainframe-jcl to depend on open-mainframe-lang-core**,
So that **it can use the shared types**.

**Acceptance Criteria:**

**Given** `Cargo.toml` updated
**When** `cargo check -p open-mainframe-jcl` runs
**Then** it compiles successfully

**Complexity:** S

---

### Story 53.2: Add Span to JCL AST Nodes

As a **developer**,
I want **Span fields on Job, Step, DdStatement, and DdDefinition**,
So that **JCL errors can report source locations**.

**Acceptance Criteria:**

**Given** `Job` struct
**When** parsed
**Then** `job.span` contains the byte range of the JOB statement

**Given** `Step` struct
**When** parsed
**Then** `step.span` contains the byte range from EXEC to last DD

**Given** `DdStatement` struct
**When** parsed
**Then** `dd.span` contains the byte range of the DD statement

**Given** existing JCL tests
**When** run
**Then** all pass (new fields don't break existing assertions)

**Complexity:** M
**Supports:** FR-v1.6-004

---

### Story 53.3: Implement AstNode for Job

As a **developer**,
I want **`Job` to implement the `AstNode` trait**,
So that **generic tooling can access its span**.

**Acceptance Criteria:**

**Given** `impl AstNode for Job`
**When** `job.span()` is called
**Then** returns the job's source span

**Complexity:** S
**Supports:** FR-v1.6-003

---

### Story 53.4: Update JCL Parser to Track Positions

As a **developer**,
I want **the JCL parser to compute byte offsets for Span fields**,
So that **AST nodes have accurate source locations**.

**Acceptance Criteria:**

**Given** a JCL source with `//MYJOB JOB` at byte offset 0
**When** parsed
**Then** `job.span.start` is 0 and `job.span.end` covers the full JOB statement

**Given** a multi-line JCL source
**When** step at line 3 is parsed
**Then** `step.span.start` points to the correct byte offset

**Given** inline data (`DD *`)
**When** parsed
**Then** `dd.span` covers from `DD` through the `/*` delimiter

**Complexity:** M
**Supports:** FR-v1.6-004

---

## Epic 54: COBOL Crate Restructuring

**Goal:** Split monolithic parser and AST files into smaller, organized modules.

**Crate:** `open-mainframe-cobol`
**FRs:** FR-v1.6-006

### Story 54.1: Split parser/mod.rs into Sub-modules

As a **developer**,
I want **the parser split into statements.rs, expressions.rs, data.rs, divisions.rs**,
So that **no single file exceeds 2,500 lines**.

**Acceptance Criteria:**

**Given** `parser/mod.rs` (5,019 lines)
**When** split into sub-files
**Then** mod.rs contains Parser struct, helpers, entry point (~300 lines)
**And** statements.rs contains parse_*_statement methods
**And** expressions.rs contains parse_expression, parse_condition
**And** data.rs contains parse_data_division, parse_data_item
**And** divisions.rs contains parse_identification/environment_division

**Given** the split
**When** `cargo test -p open-mainframe-cobol` runs
**Then** all parser tests pass unchanged

**Complexity:** L
**Supports:** FR-v1.6-006

---

### Story 54.2: Split ast/mod.rs into Sub-modules

As a **developer**,
I want **the AST split into statements.rs, expressions.rs, data.rs**,
So that **types are organized by concern**.

**Acceptance Criteria:**

**Given** `ast/mod.rs` (1,619 lines)
**When** split into sub-files
**Then** mod.rs contains Program, top-level types, re-exports
**And** statements.rs contains Statement enum + statement structs
**And** expressions.rs contains Expression, Condition, Literal types
**And** data.rs contains DataDivision, DataItem, PictureClause types

**Given** the split
**When** `cargo test --workspace` runs
**Then** all tests pass unchanged

**Complexity:** M
**Supports:** FR-v1.6-006

---

### Story 54.3: Reorganize JCL Crate into Directories

As a **developer**,
I want **the JCL crate reorganized from flat files to directories**,
So that **it follows the standard language crate layout**.

**Acceptance Criteria:**

**Given** `lexer.rs` (flat file)
**When** moved to `lexer/mod.rs` + `lexer/token.rs` + `lexer/scanner.rs`
**Then** public API unchanged

**Given** `parser.rs` (flat file)
**When** moved to `parser/mod.rs`
**Then** public API unchanged

**Given** `ast.rs` (flat file)
**When** moved to `ast/mod.rs`
**Then** public API unchanged

**Given** `cargo test -p open-mainframe-jcl`
**When** run
**Then** all tests pass

**Complexity:** M

---

### Story 54.4: Verify Standard Layout Compliance

As a **developer**,
I want **both COBOL and JCL crates to follow the standard language crate layout**,
So that **future languages can follow the same pattern**.

**Acceptance Criteria:**

**Given** the standard layout defined in architecture-v1.6.md
**When** COBOL crate structure is examined
**Then** it has `lexer/`, `parser/`, `ast/`, `semantic/`, `error.rs`, `keywords.rs` (or `macros.rs`)

**Given** the standard layout
**When** JCL crate structure is examined
**Then** it has `lexer/`, `parser/`, `ast/`, `error.rs`

**Given** `cargo test --workspace`
**When** run after all restructuring
**Then** all tests pass

**Complexity:** S

---

## Implementation Order

### Phase 1: Foundation (Epic 51)

1. Create `open-mainframe-lang-core` crate skeleton
2. Implement Span, FileId, Location, offset_to_line_col
3. Implement Diagnostic, Severity
4. Implement AstNode, Lexer, Parse traits

### Phase 2: COBOL Integration (Epic 52)

1. Add lang-core dependency to COBOL crate
2. Replace Span/FileId/Location with lang-core re-exports
3. Replace Diagnostic/Severity with lang-core re-exports
4. Implement AstNode for Program
5. Verify backward compatibility

### Phase 3: JCL Integration (Epic 53)

1. Add lang-core dependency to JCL crate
2. Add Span fields to JCL AST nodes
3. Implement AstNode for Job
4. Update JCL parser to track positions

### Phase 4: Restructuring (Epic 54)

1. Split COBOL parser/mod.rs
2. Split COBOL ast/mod.rs
3. Reorganize JCL crate into directories
4. Verify standard layout compliance
