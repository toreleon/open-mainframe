---
version: 'v1.6'
baseVersion: 'v1.5'
date: '2026-02-14'
status: 'draft'
---

# Product Requirements Document - v1.6: Shared Language Core

_Addendum to the base PRD. All existing requirements remain in effect._

## Overview

v1.6 introduces the `open-mainframe-lang-core` crate as a shared foundation for all language compiler crates. This is an internal refactoring with no user-facing behavior changes in the CLI, but it establishes conventions and shared infrastructure critical for multi-language support.

## Problem Statement

1. The COBOL and JCL crates independently define compiler infrastructure types (source tracking, diagnostics)
2. The JCL crate lacks source location tracking, preventing precise error reporting
3. Adding a new language requires re-implementing foundational types
4. Shared tooling (assessment, future LSP) must couple to COBOL-specific types

## Functional Requirements

### FR-v1.6-001: Shared Source Location Types

The system shall provide a shared `Span`, `FileId`, and `Location` type in `open-mainframe-lang-core` that all language crates use for source position tracking.

**Acceptance:** COBOL and JCL crates both use the same `Span` type from lang-core.

### FR-v1.6-002: Shared Diagnostic Type

The system shall provide a shared `Diagnostic` and `Severity` type in `open-mainframe-lang-core` that all language crates use for error/warning reporting.

**Acceptance:** Semantic analysis diagnostics in the COBOL crate use the shared Diagnostic type.

### FR-v1.6-003: Common AstNode Trait

The system shall provide an `AstNode` trait requiring `fn span(&self) -> Span` that all AST root types implement.

**Acceptance:** Both `open_mainframe_cobol::Program` and `open_mainframe_jcl::Job` implement `AstNode`.

### FR-v1.6-004: JCL Source Location Tracking

The JCL crate's AST nodes (`Job`, `Step`, `DdStatement`) shall carry `Span` fields for source position tracking.

**Acceptance:** JCL parse errors include line/column information.

### FR-v1.6-005: COBOL Backward Compatibility

The COBOL crate's public API shall remain unchanged. Types moved to lang-core shall be re-exported from their original paths.

**Acceptance:** All existing code that uses `open_mainframe_cobol::Span`, `open_mainframe_cobol::Diagnostic`, etc. compiles without changes.

### FR-v1.6-006: COBOL Module Restructuring

The COBOL crate's monolithic `parser/mod.rs` and `ast/mod.rs` files shall be split into smaller files organized by concern (statements, expressions, data definitions).

**Acceptance:** No single file exceeds 2,500 lines. Public API unchanged.

## Non-Functional Requirements

### NFR-v1.6-001: Zero Additional Dependencies

`open-mainframe-lang-core` shall have no external dependencies (no miette, thiserror, serde, etc.). It contains only plain Rust data types and traits.

**Acceptance:** `Cargo.toml` has an empty `[dependencies]` section.

### NFR-v1.6-002: Build Time

Adding `open-mainframe-lang-core` shall not measurably increase workspace build time (< 1 second added to clean build).

**Acceptance:** `cargo build --release` timing before/after within margin of error.

### NFR-v1.6-003: Test Coverage

All existing tests shall continue to pass after the refactoring.

**Acceptance:** `cargo test --workspace` passes with zero failures.

## Out of Scope

- Shared parser/lexer trait _implementations_ (each language implements its own)
- Moving the COBOL interpreter or runtime types
- Adding new language crates (PL/I, REXX, Assembler)
- LSP server implementation (benefits from lang-core but is a separate effort)
- ANTLR4 parser integration (separate v1.7+ effort)
