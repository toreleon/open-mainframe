# open-mainframe-lang-core

Shared infrastructure for OpenMainframe language compilers and interpreters. This crate provides the foundational types and traits for source location tracking, diagnostics, and common compiler pipeline stages.

## Overview

The `open-mainframe-lang-core` crate is the bedrock for all language-related projects in the OpenMainframe workspace (COBOL, JCL, PL/I, REXX, FOCUS, etc.). By centralizing core types like `Span` and `Diagnostic`, it ensures consistent error reporting and interoperability between different language tools (e.g., source-to-source precompilers).

## Architecture

```
    Compiler / Interpreter                Shared Core Infrastructure
    ┌──────────────────┐                  ┌────────────────────────┐
    │  Specific Lexer  │ ────── Traits ──>│  Lexer & Parse Traits  │
    └──────────────────┘                  └────────────────────────┘
                                                       │
    ┌──────────────────┐                  ┌────────────────────────┐
    │  Specific AST    │ ────── Traits ──>│  AstNode Trait         │
    └──────────────────┘                  └────────────────────────┘
                                                       │
    ┌──────────────────┐                  ┌────────────────────────┐
    │  Error Handling  │ ───── Types ────>│  Span, FileId, Location│
    │  Diagnostic      │                  │  Severity, Diagnostic  │
    └──────────────────┘                  └────────────────────────┘
```

### Module Structure

| Module | Description |
|--------|-------------|
| `span` | Source location tracking: `Span`, `FileId`, `Location`, and offset conversion |
| `diagnostic`| Unified error and warning representation: `Diagnostic`, `Severity` |
| `traits` | Foundational traits for compiler stages: `AstNode`, `Lexer`, `Parse` |
| `preprocess`| Universal source preprocessor: Normalization and line indexing |

## Key Types and Traits

### Source Tracking
- `Span`: Represents a range of bytes within a source file. Supports merging and offsetting.
- `FileId`: Unique identifier for a source file within a diagnostic context.
- `Location`: Human-readable `line:column` representation of a source position.

### Diagnostics
- `Diagnostic`: A rich error message containing a `Severity`, message text, and optional `Span`.
- `Severity`: Level of the diagnostic: `Error`, `Warning`, `Info`, `Hint`.

### Compiler Traits
- `AstNode`: Trait for types representing nodes in an Abstract Syntax Tree.
- `Lexer`: Standard interface for tokenization.
- `Parse`: Standard interface for parsing tokens into an AST.

## Usage Examples

### Using Spans and Locations

```rust
use open_mainframe_lang_core::{Span, offset_to_line_col};

let source = "LINE 1\nLINE 2\nLINE 3";
let span = Span::new(7, 13); // "LINE 2"

let (line, col) = offset_to_line_col(source, span.start);
assert_eq!(line, 1); // 0-indexed
assert_eq!(col, 0);
```

### Implementing a Shared Lexer Trait

```rust
use open_mainframe_lang_core::Lexer;

struct MyLexer<'a> {
    source: &'a str,
}

// Example implementation
impl<'a> MyLexer<'a> {
    fn next_token(&mut self) -> Option<String> {
        None
    }
}
```

## Design Principles

1. **Zero External Dependencies**: This crate is a "pure" Rust crate with no external dependencies (no `miette`, no `thiserror` at this level). This keeps the dependency graph clean for all downstream consumers.
2. **Standardized Interop**: By using the same `Span` and `FileId` types, a COBOL precompiler can report errors that are perfectly integrated with the main COBOL compiler's diagnostic output.
3. **Performance**: All types are designed for low overhead, utilizing small IDs and avoiding heavy allocations in the hot path of lexing and parsing.

## Testing

The core crate is exhaustively tested to ensure mathematical correctness of span operations and coordinate conversions:
- **Span**: Merging, intersection, and containment logic.
- **Coordinates**: Round-trip tests for offset-to-location and location-to-offset.
- **Preprocess**: Validation of line-ending normalization across different platforms (CRLF, LF).

```sh
cargo test -p open-mainframe-lang-core
```
