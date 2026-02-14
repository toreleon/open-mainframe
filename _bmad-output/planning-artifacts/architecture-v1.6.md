---
version: 'v1.6'
baseVersion: 'v1.5'
date: '2026-02-14'
status: 'draft'
---

# Architecture Decision Document - v1.6: Shared Language Core

_Addendum to the base architecture document. All patterns, naming conventions, and enforcement guidelines from the base architecture remain in effect._

## Motivation

The workspace currently contains two language crates (`open-mainframe-cobol`, `open-mainframe-jcl`) that independently define foundational compiler types: source location tracking, error diagnostics, token traits, and AST node contracts. This duplication means:

- Shared tooling (`open-mainframe-assess`, future LSP server) must depend on language-specific types
- Adding a new language (PL/I, REXX, Assembler) requires re-inventing Span, Diagnostic, etc.
- The JCL crate lacks source location tracking entirely, blocking proper diagnostics

v1.6 introduces `open-mainframe-lang-core` to unify these foundations.

## New Crate: `open-mainframe-lang-core`

### Responsibility

Provide shared types and traits that all language compiler crates implement. Zero language-specific logic. Pure interfaces and data structures.

### Updated Dependency Graph

```
                    +------------------+
                    |  open-mainframe  |  (binary)
                    |      (CLI)       |
                    +--------+---------+
                             |
      +----------------------+----------------------+
      |                      |                      |
      v                      v                      v
+------------+        +------------+         +------------+
| open-mainframe-tui |        | open-mainframe-jcl |         |open-mainframe-dataset|
|    (TUI)   |        |(interpret) |         |   (files)  |
+------+-----+        +------+----+         +------+-----+
       |                      |                     |
       v                      +----------+----------+
+------------+                           |
|open-mainframe-cics |<------------------+
|   (CICS)   |
+------+-----+
       |
       v
+------------+    +------------+    +------------+
|open-mainframe-cobol|    |open-mainframe-runtime|    | open-mainframe-db2 |
| (compiler) |    |  (library) |    |   (SQL)    |
+------+-----+    +------+----+    +------------+
       |                |
       +--------+-------+
                v
     +-------------------+
     |open-mainframe-encoding|
     |      (codec)      |
     +-------------------+

            NEW: Foundation layer
     +-------------------+
     |open-mainframe-lang-core|  <-- open-mainframe-cobol, open-mainframe-jcl,
     |  (shared traits)  |       open-mainframe-assess, future languages
     +-------------------+
```

### Crate API

```rust
// open-mainframe-lang-core/src/lib.rs

// === Source Location Tracking ===

/// Unique identifier for a source file (main source vs copybooks/includes).
pub struct FileId(pub u32);

/// Contiguous byte range in a source file.
pub struct Span {
    pub file: FileId,
    pub start: u32,
    pub end: u32,
}

/// Resolved line/column for display.
pub struct Location {
    pub line: u32,
    pub column: u32,
    pub file_name: String,
}

pub fn offset_to_line_col(source: &str, offset: usize) -> (u32, u32);

// === Diagnostics ===

pub enum Severity { Error, Warning, Info }

pub struct Diagnostic {
    pub severity: Severity,
    pub code: String,
    pub message: String,
    pub span: Span,
    pub suggestion: Option<String>,
}

// === Traits ===

/// Every AST node provides its source location.
pub trait AstNode {
    fn span(&self) -> Span;
}

/// Common interface for language-specific lexers.
pub trait Lexer {
    type Token;
    type Error;
    fn tokenize(&mut self, source: &str) -> (Vec<Self::Token>, Vec<Self::Error>);
}

/// Common interface for language-specific parsers.
pub trait Parse {
    type Ast: AstNode;
    type Error;
    fn parse(&mut self) -> (Option<Self::Ast>, Vec<Self::Error>);
}
```

### Internal Module Structure

```
crates/open-mainframe-lang-core/
+-- Cargo.toml
+-- src/
    +-- lib.rs          # Public API re-exports
    +-- span.rs         # FileId, Span, Location, offset_to_line_col
    +-- diagnostic.rs   # Severity, Diagnostic
    +-- traits.rs       # AstNode, Lexer, Parse traits
```

### Cargo.toml

```toml
[package]
name = "open-mainframe-lang-core"
version.workspace = true
edition.workspace = true
rust-version.workspace = true
license.workspace = true
repository.workspace = true
description = "Shared types and traits for OpenMainframe language compilers"

[dependencies]
# Intentionally minimal - no heavy dependencies
```

---

## Architectural Decisions

### Decision: Separate Crate vs Module in Existing Crate

**Choice:** New standalone crate `open-mainframe-lang-core`

**Alternatives Considered:**

| Option | Pros | Cons |
|--------|------|------|
| New crate (chosen) | Clean dependency graph, no circular deps, any crate can depend on it | One more crate to maintain |
| Module in open-mainframe-encoding | Already shared | Encoding is about EBCDIC, not compiler infra; wrong abstraction level |
| Module in open-mainframe-runtime | Already shared | Runtime is execution, not compilation; wrong abstraction level |
| Traits duplicated per language | No shared dependency | Defeats the purpose; tooling can't be generic |

**Rationale:**
- `open-mainframe-lang-core` has zero heavy dependencies (no miette, no thiserror) — it's pure data types and traits
- Any crate (COBOL, JCL, assess, LSP, future languages) can depend on it cheaply
- Clean separation: `lang-core` = "what a language compiler IS", `encoding` = "how bytes map", `runtime` = "how programs run"

### Decision: Minimal Traits vs Rich Trait Hierarchy

**Choice:** Minimal traits (`AstNode`, `Lexer`, `Parse`)

**Rationale:**
- COBOL is a compiled language with deep semantic analysis; JCL is an interpreted job control language
- A rich `SemanticAnalyzer` trait would force JCL to implement something it doesn't need
- Better to start minimal and add traits when a second language actually needs them
- The key shared value is in the *types* (Span, Diagnostic), not the traits

### Decision: Migration Strategy

**Choice:** Move types, re-export for backward compatibility

**Implementation:**
1. Move `Span`, `FileId`, `Location`, `offset_to_line_col` from `open-mainframe-cobol::lexer::span` to `open-mainframe-lang-core::span`
2. Move `Severity`, `Diagnostic` from `open-mainframe-cobol::semantic::analyzer` to `open-mainframe-lang-core::diagnostic`
3. COBOL crate re-exports these types via `pub use open_mainframe_lang_core::*` — zero breaking changes for consumers
4. JCL crate adopts the shared types incrementally

**Rationale:**
- Existing code continues to compile unchanged
- `open-mainframe-assess` (which depends on `open-mainframe-cobol`) sees no API change
- JCL can adopt spans at its own pace

### Decision: No Dependency on miette/thiserror

**Choice:** `open-mainframe-lang-core` has zero external dependencies

**Rationale:**
- Span, FileId, Location, Diagnostic are plain data types — no derive macros needed
- Keeping the crate dependency-free makes it trivially fast to compile
- Language crates that need miette/thiserror integration wrap or convert as needed
- Future languages can depend on lang-core without pulling in the error reporting stack

---

## Integration with Existing Crates

### What Changes in Existing Crates

#### `open-mainframe-cobol` Changes

1. **Add dependency** on `open-mainframe-lang-core`
2. **Remove** `Span`, `FileId`, `Location`, `offset_to_line_col` from `lexer/span.rs` — replace with re-exports from `lang-core`
3. **Remove** `Severity`, `Diagnostic` from `semantic/analyzer.rs` — replace with re-exports from `lang-core`
4. **Re-export** these types in `lib.rs` so the public API is unchanged
5. **`CobolError`** remains COBOL-specific (it has COBOL-specific variants like `CopybookNotFound`)

#### `open-mainframe-jcl` Changes

1. **Add dependency** on `open-mainframe-lang-core`
2. **Add `Span` fields** to AST nodes (`Job`, `Step`, `DdStatement`, etc.)
3. **Replace** `JclError` with richer error types using `Diagnostic`
4. These are additive changes — existing public API preserved

#### `open-mainframe-assess` Changes

1. **Add dependency** on `open-mainframe-lang-core` (can now reference `Diagnostic` directly)
2. No required changes in this version — benefits come later when it can assess JCL too

### What Does NOT Change

- `open-mainframe-encoding`: No changes
- `open-mainframe-runtime`: No changes (uses COBOL types via open-mainframe-cobol)
- `open-mainframe-dataset`: No changes
- `open-mainframe-db2`: No changes
- `open-mainframe-cics`: No changes
- `open-mainframe-ims`: No changes
- `open-mainframe-sort`: No changes
- `open-mainframe-tui`: No changes
- `open-mainframe-deploy`: No changes

---

## COBOL Crate Parser/AST Restructuring

As part of v1.6, the large monolithic files in the COBOL crate are split for maintainability:

### parser/mod.rs (5,019 lines) splits into:

```
parser/
+-- mod.rs          # Parser struct, entry point, helpers (~300 lines)
+-- statements.rs   # parse_*_statement methods (~2,000 lines)
+-- expressions.rs  # parse_expression, parse_condition (~800 lines)
+-- data.rs         # parse_data_division, parse_data_item (~1,200 lines)
+-- divisions.rs    # parse_identification/environment_division (~700 lines)
```

### ast/mod.rs (1,619 lines) splits into:

```
ast/
+-- mod.rs          # Program, top-level types, re-exports (~200 lines)
+-- statements.rs   # Statement enum + all statement structs (~600 lines)
+-- expressions.rs  # Expression, Condition, Literal types (~400 lines)
+-- data.rs         # DataDivision, DataItem, PictureClause (~400 lines)
```

**Rationale:** Files >1,000 lines are hard to navigate. The split mirrors the logical grouping already present in the code. No public API changes.

---

## Updated Workspace Cargo.toml

```toml
[workspace]
members = [
    "crates/open-mainframe",
    "crates/open-mainframe-lang-core",  # NEW
    "crates/open-mainframe-cobol",
    "crates/open-mainframe-jcl",
    "crates/open-mainframe-runtime",
    "crates/open-mainframe-dataset",
    "crates/open-mainframe-encoding",
    "crates/open-mainframe-sort",
    "crates/open-mainframe-db2",
    "crates/open-mainframe-cics",
    "crates/open-mainframe-assess",
    "crates/open-mainframe-ims",
    "crates/open-mainframe-deploy",
    "crates/open-mainframe-tui",
]

[workspace.dependencies]
# ... existing dependencies ...
open-mainframe-lang-core = { path = "crates/open-mainframe-lang-core" }  # NEW
```

---

## Standard Language Crate Layout (Convention)

All current and future language crates should follow this directory convention:

```
crates/open-mainframe-<lang>/
+-- Cargo.toml
+-- src/
    +-- lib.rs              # Public API re-exports
    +-- keywords.rs         # Keyword enum + lookup (if applicable)
    +-- error.rs            # Language-specific error types
    +-- lexer/
    |   +-- mod.rs          # Lexer struct
    |   +-- token.rs        # Token, TokenKind
    |   +-- scanner.rs      # Character-level scanning
    |   +-- source.rs       # Source format handling (optional)
    +-- ast/
    |   +-- mod.rs          # Top-level AST
    |   +-- statements.rs   # Statement types
    |   +-- expressions.rs  # Expression types (if applicable)
    |   +-- data.rs         # Data definition types
    +-- parser/
    |   +-- mod.rs          # Parser struct, entry point
    |   +-- statements.rs   # Statement parsing
    |   +-- expressions.rs  # Expression parsing (if applicable)
    |   +-- data.rs         # Data definition parsing
    +-- semantic/           # Optional
    +-- codegen/            # Optional, feature-gated
    +-- intrinsics/         # Optional
```

**All AST nodes must implement `open_mainframe_lang_core::AstNode`** (carry a Span).

---

## Implementation Priority

1. **Create `open-mainframe-lang-core` crate** with shared types
2. **Wire COBOL crate** to use lang-core types (re-export for compat)
3. **Wire JCL crate** to use lang-core types (add Span to AST)
4. **Split COBOL parser/ast** into sub-files
5. **Verify** entire workspace compiles and tests pass
