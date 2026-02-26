# open-mainframe-rexx

A high-performance Rust implementation of the **REXX (Restructured Extended Executor)** language for the OpenMainframe project — providing a complete lexer, parser, and interpreter with full support for TSO/E REXX features, arbitrary-precision decimal arithmetic, and the powerful PARSE template system.

## Overview

REXX is the standard scripting and "glue" language for IBM mainframes, prized for its readability and powerful string manipulation. This crate reimplements the REXX language from the ground up, enabling the execution of REXX scripts in a variety of environments (TSO, USS, Batch). It features a robust interpreter that manages variable pools, supports block scoping, and integrates with host command environments.

The implementation comprises:
1. **Lexical Analysis** — A tokenization engine that handles REXX's unique comment nesting, string literals, and operator rules.
2. **Predictive Parser** — Translates tokens into a structured Abstract Syntax Tree (AST) while maintaining operator precedence and instruction boundaries.
3. **Interpreter Core** — A recursive execution engine that manages the call stack, variable pools, and instruction flow.
4. **Decimal Arithmetic** — A built-in high-precision decimal engine that ensures REXX arithmetic matches mainframe behavior (respecting `NUMERIC DIGITS`).
5. **PARSE Template Engine** — A dedicated module for the sophisticated `PARSE` instruction, supporting positional, pattern, and variable-based splitting.
6. **Built-in Function Library** — A comprehensive implementation of standard REXX functions (SUBSTR, WORD, COPIES, etc.).

## Architecture

```
    REXX Source Text
           │
    ┌──────▼─────────────────────────────────────────────────┐
    │                  Lexer & Tokenizer                     │
    │  - Nested comment support (/* ... /* ... */ ... */)    │
    │  - Symbol and literal identification                   │
    └──────┬─────────────────────────────────────────────────┘
           │
    ┌──────▼─────────────────────────────────────────────────┐
    │                  Parser Engine                         │
    │  - AST construction (Instructions & Expressions)       │
    │  - Template parsing for PARSE/ARG/PULL                 │
    └──────┬─────────────────────────────────────────────────┘
           │
    ┌──────▼─────────────────────────────────────────────────┐
    │                 Interpreter & Runtime                  │
    │  - Variable Pools (Local & Global)                     │
    │  - Scoping (PROCEDURE EXPOSE)                          │
    │  - Call Stack & Subroutine dispatch                    │
    └──────┬─────────────────────────────────────────────────┘
           │
    ┌──────▼─────────────────────────────────────────────────┐
    │                 Support Subsystems                     │
    │  - High-Precision Decimal Arithmetic                   │
    │  - Built-in Function Registry                          │
    │  - Host Command Environment (ADDRESS)                  │
    └────────────────────────────────────────────────────────┘
```

### Module Structure

| Module | Description | Lines |
|--------|-------------|------:|
| `interpreter`| Recursive execution engine and state management | ~1,949 |
| `parser` | Predictive parser building the AST from tokens | ~1,222 |
| `builtins` | Library of 40+ standard REXX functions | ~1,155 |
| `value` | REXX value system and high-precision decimal math | ~922 |
| `lexer` | Robust tokenizer for REXX source code | ~748 |
| `parse_template`| Splitting logic for PARSE, ARG, and PULL | ~447 |
| `ast` | AST node definitions for instructions and expressions | ~215 |
| `token` | Token definitions and source tracking | ~144 |

**Total**: ~6,827 lines of Rust.

## Key Types and Components

### Runtime & Values

| Type | Description |
|------|-------------|
| `RexxValue` | The universal variant type representing REXX strings/numbers. |
| `Interpreter`| Orchestrates the execution of a REXX `Program`. |
| `VariablePool`| Manages variables with support for stemmed arrays (e.g., `VAR.1`). |
| `NumericSettings`| Controls precision (`DIGITS`) and formatting (`FORM`). |

### Parsing

| Type | Description |
|------|-------------|
| `Program` | The root AST node containing a list of `Clause` nodes. |
| `Clause` | The fundamental unit of execution (Instruction or Expression). |
| `Expr` | Recursive enum for expressions with operator precedence. |

### Template Engine

| Type | Description |
|------|-------------|
| `Template` | Parsed representation of a PARSE template. |
| `Pattern` | Individual splitting criteria (Positional, Pattern, Variable). |

## Implementation Details

### Arbitrary-Precision Arithmetic

REXX values are inherently strings, but they behave as numbers when used in arithmetic. This crate implements:
- **Precision Control**: Defaults to 9 digits, but can be set via `NUMERIC DIGITS`.
- **Rounding**: Follows REXX standard rounding rules for multiplication and division.
- **Form**: Supports both `SCIENTIFIC` and `ENGINEERING` exponential notation.

### Variable Scoping and Exposure

The interpreter supports traditional REXX scoping rules:
- **Internal Routines**: Share the same variable pool as the caller by default.
- **PROCEDURE**: Creates a new, isolated pool.
- **EXPOSE**: Allows specific variables from the caller's pool to be "exposed" to the routine.

### The PARSE Instruction

One of REXX's most powerful features is implemented in the `parse_template` module. It supports:
- **Positional Patterns**: `PARSE VAR S 5 VAR1 10 VAR2` (splits at col 5 and 10).
- **String Patterns**: `PARSE VAR S '(' VAR1 ')'` (extracts text between parens).
- **Variable Patterns**: `PARSE VAR S (MARKER) VAR1` (uses value of MARKER as split point).

## Feature Coverage

| Feature | Category | Status |
|---------|----------|--------|
| Decimal Arith   | Types    | Implemented (Arbitrary precision) |
| Stemmed Vars    | Variables| Implemented (e.g., `A.B.C`) |
| Scoping         | Control  | Implemented (PROCEDURE EXPOSE) |
| PARSE Templates | Language | Implemented (All pattern types) |
| DO Loops        | Control  | Implemented (WHILE, UNTIL, FOREVER) |
| SELECT / WHEN   | Control  | Implemented |
| Built-in Funcs  | Lib      | Implemented (40+ functions) |
| Host Commands   | Interface| Implemented (ADDRESS TSO/USS) |
| External Subroutines| Lib   | Implemented |

## Usage Examples

### Executing a REXX Script

```rust
use open_mainframe_rexx::{interpret, parse};

let source = r#"
   PARSE ARG NAME
   SAY 'Hello,' NAME
   RETURN 0
"#;

let program = parse(source).unwrap();
let result = interpret(program, &["WORLD"]).unwrap();
println!("Script RC: {}", result);
```

### High-Precision Arithmetic

```rust
use open_mainframe_rexx::value::RexxValue;

let a = RexxValue::from("1.23456789");
let b = RexxValue::from("2");
let c = a.multiply(&b).unwrap();
assert_eq!(c.to_string(), "2.46913578");
```

## Testing

The REXX crate is tested against the standard REXX test suite:
- **Language Tests**: Verifies complex nesting of IF/DO/SELECT.
- **Arithmetic Tests**: Ensures mathematical parity with IBM REXX across 100+ cases.
- **Parse Tests**: Exhaustive testing of the PARSE engine with multi-level patterns.
- **Built-in Tests**: Unit tests for every standard function with edge-case parameters.

```sh
cargo test -p open-mainframe-rexx
```

## Limitations and Future Work

- **Stream I/O**: `STREAM`, `CHARIN`, and `LINEIN` are currently being integrated with the `open-mainframe-dataset` crate.
- **INTERPRET Instruction**: Support for dynamic code execution (executing a string as code) is in design.
- **SAA Compliance**: Most SAA REXX features are implemented, with few exceptions in less-common built-ins.
