---
version: 'v2.0'
baseVersion: 'v1.6'
date: '2026-02-16'
status: 'draft'
---

# Architecture Decision Document - v2.0: IBM COBOL v6.4 Conformance

_Addendum to the base architecture document. All patterns, naming conventions, and enforcement guidelines from the base architecture remain in effect._

## Motivation

v2.0 introduces IBM COBOL v6.4 conformance across multiple compiler subsystems. This requires:

- A **new preprocessor phase** for REPLACE, conditional compilation, and CBL/PROCESS directives
- **New AST node types** for 9 additional statements (JSON/XML GENERATE/PARSE, ALLOCATE, FREE, ENTRY, ALTER, INVOKE)
- **New data types** (PIC U/UTF-8, DISPLAY-1, DYNAMIC LENGTH) affecting data layout, MOVE semantics, and intrinsic functions
- **Structural language features** (nested programs, declaratives) that change scope rules and runtime dispatch
- **Special registers** (XML-CODE, XML-EVENT, XML-TEXT, JSON-CODE, JSON-STATUS) that require runtime state management
- **14 new intrinsic functions** spanning ISO 8601 date/time and UTF-8 string operations

These changes touch every layer: preprocessor, lexer, parser, AST, semantic analyzer, and runtime interpreter. Architectural decisions are needed to ensure consistency, avoid regressions, and maintain the macro-driven code generation patterns established in v1.5-v1.6.

---

## Architecture Decisions

### AD-2.0-01: Preprocessor Pipeline for Compiler Directives

**Decision:** Extend the existing preprocessor with a multi-pass pipeline: Pass 1 (CBL/PROCESS and conditional compilation) runs before COPY expansion; Pass 2 (REPLACE) runs after COPY expansion.

**Context:**

The current preprocessing pipeline in `lexer/preprocessor.rs` handles COPY/REPLACING as a single pass. IBM COBOL defines a specific processing order:

1. CBL/PROCESS options parsed from first source lines
2. Conditional compilation directives (`>>DEFINE`, `>>IF`, `>>EVALUATE`, `>>SET`) evaluated to include/exclude source lines
3. COPY statements expanded with REPLACING applied
4. REPLACE statements applied to the post-COPY source text

**Implementation:**

```
Source text
    │
    ▼
┌─────────────────────────┐
│ Pass 0: CBL/PROCESS     │  Parse compiler options from first lines
│ (new: preprocessor.rs)  │  Set CompilerOptions struct
└───────────┬─────────────┘
            │
            ▼
┌─────────────────────────┐
│ Pass 1: Conditional     │  Evaluate >>DEFINE, >>IF, >>EVALUATE, >>SET
│ Compilation             │  Strip excluded lines from source
│ (new: conditional.rs)   │  Track compile-time variables in HashMap
└───────────┬─────────────┘
            │
            ▼
┌─────────────────────────┐
│ Pass 2: COPY Expansion  │  Existing preprocessor.rs logic
│ (existing)              │  COPY ... REPLACING applied per-copybook
└───────────┬─────────────┘
            │
            ▼
┌─────────────────────────┐
│ Pass 3: REPLACE         │  Apply REPLACE directives to post-COPY text
│ (new: replace.rs)       │  Pseudo-text matching with ==...== syntax
└───────────┬─────────────┘
            │
            ▼
┌─────────────────────────┐
│ Lexer (existing)        │  Tokenize preprocessed source
└───────────┬─────────────┘
            │
            ▼
        Parser → AST → Semantic → Runtime
```

**New files:**
- `lexer/conditional.rs` — `>>DEFINE`, `>>IF`, `>>ELSE`, `>>END-IF`, `>>EVALUATE`, `>>WHEN`, `>>END-EVALUATE`, `>>SET`
- `lexer/replace.rs` — REPLACE statement and pseudo-text matching
- `lexer/compiler_options.rs` — CBL/PROCESS parsing and `CompilerOptions` struct

**Rationale:**
- Separate files per concern keep each pass focused and testable
- The pass ordering matches IBM's specified processing sequence
- COPY expansion remains in the existing `preprocessor.rs` — no disruption to current behavior
- `CompilerOptions` struct is shared with the parser and runtime for ARITH/TRUNC semantics

**Alternatives Considered:**

| Option | Pros | Cons |
|--------|------|------|
| Multi-pass pipeline (chosen) | Matches IBM spec ordering, each pass is isolated and testable | Slightly more complex pipeline |
| Single-pass with state machine | Fewer passes | Complex interleaving of COPY + REPLACE + conditionals; error-prone |
| Preprocessor as separate crate | Clean separation | Over-engineering for single-language use; COPY already in lexer module |

---

### AD-2.0-02: Statement Parser Extension Pattern

**Decision:** Follow the established `for_all_statement_variants!` / `for_parse_dispatch!` macro pattern exactly. Add new variants and parse functions using the same pattern.

**Context:**

The current macro system in `macros.rs` generates:
1. The `Statement` enum variants from `for_all_statement_variants!`
2. The `parse_statement()` dispatch and `is_statement_start()` predicate from `for_parse_dispatch!`

Adding new statements requires:
1. New AST struct in `ast/statements.rs`
2. New variant in `for_all_statement_variants!`
3. New dispatch entry in `for_parse_dispatch!`
4. New `parse_*_statement()` function in `parser/statements.rs`

**New Statement Types:**

| Statement | AST Struct | Keyword Dispatch | Notes |
|-----------|-----------|-----------------|-------|
| JSON GENERATE | `JsonGenerateStatement` | `Json => parse_json_statement` | Single dispatch, disambiguate GENERATE vs PARSE inside |
| JSON PARSE | `JsonParseStatement` | (same as above) | |
| XML GENERATE | `XmlGenerateStatement` | `Xml => parse_xml_statement` | Single dispatch, disambiguate inside |
| XML PARSE | `XmlParseStatement` | (same as above) | |
| ALLOCATE | `AllocateStatement` | `Allocate => parse_allocate_statement` | |
| FREE | `FreeStatement` | `Free => parse_free_statement` | |
| ENTRY | `EntryStatement` | `Entry => parse_entry_statement` | |
| ALTER | `AlterStatement` | `Alter => parse_alter_statement` | |
| INVOKE | `InvokeStatement` | `Invoke => parse_invoke_statement` | |

**Runtime wiring:** Each new AST node gets a corresponding `execute_*` function in the runtime interpreter, following the existing match arm pattern.

**Rationale:**
- Zero deviation from established patterns means zero learning curve
- The macro system auto-generates enum variants, Display impls, and dispatch logic
- New statements are added with 3-4 lines of macro entries + the parse function

---

### AD-2.0-03: XML/JSON Special Registers

**Decision:** Implement special registers as named fields in a `SpecialRegisters` struct on the runtime state, accessible by name during statement execution.

**Context:**

IBM COBOL defines special registers that are implicitly available without declaration:
- **XML processing:** XML-CODE (i32), XML-EVENT (string), XML-TEXT (string), XML-NTEXT (national string)
- **JSON processing:** JSON-CODE (i32), JSON-STATUS (i32)

These are not user-declared data items — they exist in the runtime environment and are set/read by XML PARSE, XML GENERATE, JSON PARSE, JSON GENERATE statements.

**Implementation:**

```rust
// runtime/special_registers.rs (new)
pub struct SpecialRegisters {
    // XML registers
    pub xml_code: i32,
    pub xml_event: String,
    pub xml_text: String,
    pub xml_ntext: String,
    // JSON registers
    pub json_code: i32,
    pub json_status: i32,
}

impl SpecialRegisters {
    pub fn reset_xml(&mut self) { ... }
    pub fn reset_json(&mut self) { ... }
}
```

**Semantic resolution:** When the semantic analyzer encounters an identifier `XML-CODE` that is not in the symbol table, it checks if it matches a known special register name and marks it as a special register reference in the AST.

**XML PARSE event-driven model:** The runtime calls the PROCESSING PROCEDURE paragraph repeatedly, once per XML event:
1. Set `xml_event` to event type ("START-OF-ELEMENT", "CONTENT-CHARACTERS", etc.)
2. Set `xml_text` to the associated text
3. Execute the processing procedure paragraph
4. Repeat until document end or error

**Rationale:**
- Centralizing special registers in one struct keeps them organized
- Reset methods prevent stale state between XML/JSON operations
- The event-driven model for XML PARSE maps directly to the existing XML parser in `xml_json/xml.rs`

---

### AD-2.0-04: UTF-8 / PIC U Data Type

**Decision:** Add a new `Utf8` variant to the `Usage` enum and a new `Utf8` category to the picture parser. UTF-8 data items store raw bytes but track character boundaries for operations.

**Implementation:**

1. **Usage enum** (`ast/data.rs`):
   ```rust
   pub enum Usage {
       // ... existing variants ...
       Utf8,       // PIC U — UTF-8 encoded
       Display1,   // PIC G — DBCS
   }
   ```

2. **Picture parsing** (`parser/data.rs`): Accept `U` as a picture character. `PIC U(100)` means 100 UTF-8 characters (variable byte length, max 400 bytes for storage allocation).

3. **Data layout** (`semantic/layout.rs`): UTF-8 items allocate max bytes = 4 * character_count (worst case). Actual length tracked at runtime.

4. **MOVE semantics**: MOVE to/from UTF-8 items performs UTF-8 encoding/decoding. MOVE from PIC X to PIC U treats bytes as ISO-8859-1. MOVE from PIC N (NATIONAL) to PIC U converts UTF-16 to UTF-8.

5. **Intrinsic functions** (`intrinsics/string.rs`):
   - ULENGTH — count Unicode scalar values
   - UPOS — byte position of nth character
   - USUBSTR — substring by character index
   - UVALID — check for valid UTF-8 encoding
   - UWIDTH — display width of character
   - USUPPLEMENTARY — test for supplementary plane characters

**Rationale:**
- UTF-8 is variable-length; allocating max 4 bytes/char matches IBM's approach
- Storing raw bytes avoids unnecessary transcoding during I/O
- The U* intrinsic functions provide character-level operations on the raw bytes

---

### AD-2.0-05: Nested Programs

**Decision:** Extend the `Program` AST node with a `contained_programs: Vec<Program>` field. Each contained program has its own scope in the symbol table, with GLOBAL items visible to all contained programs.

**Implementation:**

1. **AST** (`ast/mod.rs`):
   ```rust
   pub struct Program {
       // ... existing fields ...
       pub contained_programs: Vec<Program>,
       pub is_common: bool,
       pub is_initial: bool,
   }
   ```

2. **Parser** (`parser/divisions.rs`): After parsing `END PROGRAM outer-name`, check if the next token is `IDENTIFICATION DIVISION` or `PROGRAM-ID` — if so, parse another `Program` as a contained program. Collect contained programs until the outer `END PROGRAM` is found.

3. **Symbol table** (`semantic/symbols.rs`): Push a new scope for each contained program. GLOBAL items from the containing program are accessible in the contained scope. COMMON programs are callable from sibling contained programs.

4. **Runtime**: CALL to a contained program name resolves to in-process execution with a separate data state. INITIAL programs reinitialize their WORKING-STORAGE on each invocation.

**Rationale:**
- Recursive `Vec<Program>` is the natural representation for nested structure
- Scope-based symbol resolution already exists for paragraph/section names — extending to program-level scope is incremental
- GLOBAL visibility is a flag on data items, checked during name resolution

---

### AD-2.0-06: Declaratives Section

**Decision:** Add a `declaratives: Vec<DeclarativeSection>` field to the `ProcedureDivision` AST node. The runtime registers file-to-declarative mappings and invokes them on I/O errors.

**Implementation:**

1. **AST** (`ast/statements.rs`):
   ```rust
   pub struct DeclarativeSection {
       pub section_name: String,
       pub use_clause: UseClause,
       pub paragraphs: Vec<Paragraph>,
       pub span: Span,
   }

   pub enum UseClause {
       AfterError { file_names: Vec<String> },   // USE AFTER ERROR ON file-name
       AfterException { file_names: Vec<String> }, // USE AFTER EXCEPTION ON file-name
       AfterErrorInput,   // USE AFTER ERROR ON INPUT
       AfterErrorOutput,  // USE AFTER ERROR ON OUTPUT
       AfterErrorIO,      // USE AFTER ERROR ON I-O
       AfterErrorExtend,  // USE AFTER ERROR ON EXTEND
   }
   ```

2. **Parser**: Parse DECLARATIVES between `PROCEDURE DIVISION` header and the first non-declarative section. Each declarative section starts with `section-name SECTION. USE AFTER ...`.

3. **Runtime**: Maintain a `HashMap<String, String>` mapping file names to their declarative section names. When a file I/O statement encounters an error:
   - Check if a declarative exists for this file
   - If yes, execute the declarative section's paragraphs
   - If no, use default error behavior

**Rationale:**
- Declaratives are syntactically part of the Procedure Division but semantically separate — storing them in a dedicated field keeps the paragraph list clean
- The runtime mapping is simple and efficient for dispatch

---

### AD-2.0-07: Environment Division Extensions

**Decision:** Extend the existing Environment Division AST nodes with new fields for I-O-CONTROL, REPOSITORY, full SPECIAL-NAMES, and file control clauses.

**Implementation:**

1. **I-O-CONTROL** (`ast/mod.rs`):
   ```rust
   pub struct IoControlParagraph {
       pub same_record_areas: Vec<Vec<String>>,  // groups of file names
       pub apply_write_only: Vec<String>,          // file names
       pub rerun: Option<RerunClause>,
       pub span: Span,
   }
   ```

2. **REPOSITORY** (`ast/mod.rs`):
   ```rust
   pub struct RepositoryParagraph {
       pub function_all_intrinsic: bool,
       pub function_names: Vec<String>,      // Named intrinsic functions
       pub class_names: Vec<ClassEntry>,      // For OO COBOL (future)
       pub span: Span,
   }
   ```

3. **SPECIAL-NAMES extensions** (`ast/mod.rs`):
   ```rust
   pub struct AlphabetClause {
       pub name: String,
       pub alphabet_type: AlphabetType,  // Native, Standard1, Standard2, Ebcdic, UserDefined
       pub characters: Vec<AlphabetChar>,
   }

   pub struct ClassClause {
       pub name: String,
       pub characters: Vec<ClassRange>,  // Ranges like 'A' THRU 'Z'
   }
   ```

4. **File control extensions**: Add `lock_mode`, `sharing`, `reserve`, `padding_character`, `record_delimiter` fields to the existing `FileControlEntry` struct.

**Parser integration:** Extend `parse_environment_division()` in `parser/divisions.rs` to handle these new paragraphs. The SPECIAL-NAMES parser grows to handle ALPHABET, CLASS, CURRENCY SIGN with PICTURE SYMBOL, SYMBOLIC CHARACTERS.

**Semantic impact:**
- `FUNCTION ALL INTRINSIC` sets a flag in the semantic analyzer that allows bare function calls without the `FUNCTION` keyword
- ALPHABET clauses are stored and referenced by SORT/MERGE for custom collating sequences
- CLASS clauses define user-defined condition-name tests

**Rationale:**
- All extensions are additive to existing structs — no restructuring needed
- FUNCTION ALL INTRINSIC is the highest-impact change: a single boolean flag that unlocks modern COBOL syntax

---

### AD-2.0-08: Missing Intrinsic Functions

**Decision:** Add 14 new intrinsic functions to the existing `INTRINSIC_FUNCTIONS` array in `intrinsics/mod.rs`, implemented in `intrinsics/datetime.rs` and `intrinsics/string.rs`.

**Implementation:**

**ISO 8601 Date/Time functions** (in `intrinsics/datetime.rs`):

| Function | Arguments | Returns |
|----------|-----------|---------|
| FORMATTED-CURRENT-DATE | format-string | ISO 8601 string |
| FORMATTED-DATE | format-string, date-integer | ISO 8601 date string |
| FORMATTED-DATETIME | format-string, date-integer, time-seconds | ISO 8601 datetime string |
| FORMATTED-TIME | format-string, time-seconds | ISO 8601 time string |
| INTEGER-OF-FORMATTED-DATE | format-string, date-string | integer date |
| SECONDS-FROM-FORMATTED-TIME | format-string, time-string | seconds (numeric) |
| TEST-FORMATTED-DATETIME | format-string, datetime-string | 0 if valid, nonzero if invalid |

Format strings follow ISO 8601 patterns: `"YYYY-MM-DD"`, `"YYYY-MM-DDThh:mm:ss"`, etc.

**UTF-8 functions** (in `intrinsics/string.rs`):

| Function | Arguments | Returns | Depends On |
|----------|-----------|---------|-----------|
| ULENGTH | utf8-item | integer (char count) | PIC U support |
| UPOS | utf8-item, char-position | integer (byte offset) | PIC U support |
| USUBSTR | utf8-item, start, length | utf8 substring | PIC U support |
| UVALID | utf8-item | 0 if valid, byte position of error | PIC U support |
| UWIDTH | utf8-item, char-position | integer (display width) | PIC U support |
| USUPPLEMENTARY | utf8-item, char-position | 1 if supplementary, 0 if not | PIC U support |

**Registration:** Add each function to the `INTRINSIC_FUNCTIONS` lazy static array with name, argument types, return type, and implementation function pointer.

**Rationale:**
- Follows the existing pattern exactly — no architectural changes needed
- ISO 8601 functions have no dependencies and can be implemented immediately
- U* functions depend on PIC U data type support (Phase 3)

---

## Phasing Strategy

Implementation is organized by dependency order. Each phase builds on the previous.

### Phase 1: Compiler Infrastructure (Epics 60-62)

**Goal:** Establish the preprocessor pipeline that all subsequent features depend on.

1. REPLACE statement (preprocessor pass)
2. Conditional compilation (`>>DEFINE`, `>>IF`, `>>EVALUATE`, `>>SET`)
3. CBL/PROCESS option parsing
4. ARITH/TRUNC compiler option semantics

**Dependencies:** None — this is foundational.

### Phase 2: Core Missing Statements (Epics 63-66)

**Goal:** Add the highest-impact missing statements.

1. JSON GENERATE / JSON PARSE statements (wire existing xml_json/ library code)
2. XML GENERATE / XML PARSE statements (wire existing xml_json/ library code)
3. XML/JSON special registers
4. ALLOCATE / FREE statements
5. ENTRY, ALTER statements

**Dependencies:** Phase 1 (compiler options affect statement behavior).

### Phase 3: Data Division Enhancements (Epics 67-69)

**Goal:** Add new data types and clauses.

1. PIC U / UTF-8 data type (Usage::Utf8, picture parsing, layout)
2. DISPLAY-1 / DBCS data type
3. DYNAMIC LENGTH items
4. GROUP-USAGE NATIONAL, CODE-SET, LINAGE

**Dependencies:** Phase 2 (ALLOCATE used for dynamic-length storage).

### Phase 4: Language Features (Epics 70-73)

**Goal:** Add structural language capabilities.

1. Nested programs (contained programs, COMMON, GLOBAL, END PROGRAM)
2. Declaratives section (USE AFTER ERROR, runtime dispatch)
3. COPY REPLACING with LEADING/TRAILING
4. OF/IN qualification and reference modification completeness

**Dependencies:** Phase 1 (REPLACE needed for full COPY semantics), Phase 3 (data types for full qualification support).

### Phase 5: Environment Division & Conformance (Epics 74-76)

**Goal:** Complete coverage and validate.

1. I-O-CONTROL paragraph
2. REPOSITORY paragraph (FUNCTION ALL INTRINSIC)
3. Full SPECIAL-NAMES (ALPHABET, CLASS, CURRENCY, SYMBOLIC CHARACTERS)
4. File control extensions (LOCK MODE, SHARING)
5. ISO 8601 date/time intrinsic functions (no dependencies)
6. UTF-8 intrinsic functions (depends on Phase 3 PIC U)
7. Conformance test suite

**Dependencies:** Phases 1-4 complete.

---

## Updated Module Structure

### New Files in `open-mainframe-cobol`

```
crates/open-mainframe-cobol/src/
├── lexer/
│   ├── preprocessor.rs       (existing — COPY expansion)
│   ├── copybook.rs           (existing)
│   ├── conditional.rs        (NEW — >>DEFINE, >>IF, >>EVALUATE, >>SET)
│   ├── replace.rs            (NEW — REPLACE statement processing)
│   ├── compiler_options.rs   (NEW — CBL/PROCESS parsing, CompilerOptions struct)
│   └── keywords.rs           (existing — add new keywords)
├── parser/
│   ├── statements.rs         (existing — add 9 new parse_*_statement functions)
│   └── divisions.rs          (existing — extend environment division parser)
├── ast/
│   ├── statements.rs         (existing — add 9 new statement structs)
│   ├── data.rs               (existing — add Usage::Utf8, Usage::Display1)
│   └── mod.rs                (existing — add DeclarativeSection, IoControlParagraph, etc.)
├── semantic/
│   └── analyzer.rs           (existing — extend for new types, nested program scopes)
├── intrinsics/
│   ├── mod.rs                (existing — register 14 new functions)
│   ├── datetime.rs           (existing — add FORMATTED-* functions)
│   └── string.rs             (existing — add U* UTF-8 functions)
└── xml_json/
    ├── xml.rs                (existing — used by XML GENERATE/PARSE statements)
    └── json.rs               (existing — used by JSON GENERATE/PARSE statements)
```

### New Files in `open-mainframe-runtime`

```
crates/open-mainframe-runtime/src/
├── special_registers.rs      (NEW — SpecialRegisters struct)
└── interpreter.rs            (existing — add execute_* for new statements)
```

---

## Dependency Graph Update

No new crates are introduced. All changes are within existing crates:

```
open-mainframe-cobol (enhanced)
    ├── New preprocessor passes (conditional.rs, replace.rs, compiler_options.rs)
    ├── New statement AST nodes and parsers
    ├── New data types (Usage::Utf8, Usage::Display1)
    ├── Extended environment division
    └── 14 new intrinsic functions

open-mainframe-runtime (enhanced)
    ├── SpecialRegisters for XML/JSON
    ├── execute_* for new statements
    └── Nested program and declaratives runtime support
```

The inter-crate dependency graph remains unchanged from v1.6.
