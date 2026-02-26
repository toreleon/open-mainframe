# open-mainframe-pli

Enterprise PL/I — a high-performance Rust compiler front-end and interpreter for the PL/I language, supporting context-sensitive parsing, a robust type system, and complex exception handling for the OpenMainframe project.

## Overview

PL/I (Programming Language One) is a versatile mainframe language that combines features of COBOL and FORTRAN. This crate reimplements a PL/I compiler front-end, featuring a parser capable of handling PL/I's unique "no reserved words" grammar, a comprehensive type system for decimal and character data, and a full-featured exception handling (ON-unit) system.

## Architecture

```
    PL/I Source                           Execution Environment
    ┌──────────────┐                      ┌────────────────────┐
    │ DECLARE ...  │    Parsing           │    Interpreter     │
    │ GET LIST...  │ ──────────────────>  │    (Recursive)     │
    │ ON ERROR...  │    PliParser         │  Context, Scopes   │
    └──────────────┘                      └────────────────────┘
           │                                        │
           ▼                                        ▼
    ┌──────────────┐    Type System       ┌────────────────────┐
    │  Data Types  │ ──────────────────>  │   Exception Mgr    │
    │  Dec, Char   │    PliType           │   ON-units, SIGNAL │
    └────────────────┘                    └────────────────────┘
           │                                        │
           ▼                                        ▼
    ┌──────────────┐    Standard Lib      ┌────────────────────┐
    │  Built-ins   │ <──────────────────  │    I/O Engine      │
    │  SUBSTR, ADDR│    BuiltinRegistry   │  GET / PUT, READ   │
    └──────────────┘                      └────────────────────┘
```

### Module Structure

| Module | Description |
|--------|-------------|
| `parser` | Context-sensitive parser handling the "no reserved words" characteristic |
| `lexer` | Tokenizer for PL/I source, including complex string and comment rules |
| `types` | Implementation of PL/I types: FIXED DECIMAL/BINARY, CHARACTER, PICTURE, etc. |
| `interpreter`| Recursive interpreter for PL/I programs with block-scoped variable pools |
| `exceptions`| ON-unit management: Condition signaling (SIGNAL), REVERT, and ON-units |
| `builtins` | Extensive library of PL/I built-in functions (SUBSTR, INDEX, VERIFY, etc.) |

## Key Types and Components

### Parser
- `Parser`: The main entry point for translating PL/I source into an AST.
- `DataType`: Represents the complex PL/I data type definitions.
- `Statement`: Enum covering all PL/I statement types (ALLOCATE, CALL, DECLARE, etc.).

### Type System
- `PliValue`: Variant type for runtime values supporting complex PL/I conversion rules.
- `StructureMember`: Defines the hierarchical structure levels (1, 2, 3...) in PL/I.

### Exception Handling
- `ConditionManager`: Tracks active ON-units and handles condition propagation.
- `Condition`: Enumeration of standard PL/I conditions (ERROR, FINISH, ZERODIVIDE, etc.).

## Feature Coverage

| Feature | Category | Status |
|---------|----------|--------|
| No Reserved Words| Language | Implemented |
| Block Scoping   | Language | Implemented (PROCEDURE, BEGIN) |
| ON-units (ERROR)| Exception| Implemented |
| Decimal Arith   | Types    | Implemented (FIXED DECIMAL) |
| Built-in Funcs  | Lib      | Implemented (50+ functions) |
| GET / PUT LIST  | I/O      | Implemented |
| Picture Types   | Types    | Implemented |

## Usage Examples

### Executing a PL/I Program

```rust
use open_mainframe_pli::{Parser, Interpreter};

let source = r#"
   SAMPLE: PROCEDURE OPTIONS(MAIN);
      DECLARE NAME CHARACTER(20);
      DECLARE AGE  FIXED DECIMAL(3);
      NAME = 'SMITH';
      AGE = 45;
      PUT LIST('NAME:', NAME, 'AGE:', AGE);
   END SAMPLE;
"#;

let program = Parser::new(source).parse().unwrap();
let mut interpreter = Interpreter::new();
interpreter.execute(program).unwrap();
```

### Signaling an Exception

```rust
use open_mainframe_pli::exceptions::{Condition, ConditionManager};

let mut cm = ConditionManager::new();
cm.on(Condition::Zerodivide, |ctx| {
    println!("Zero divide handled!");
    // RaiseResult::Continue
});

cm.signal(Condition::Zerodivide).unwrap();
```

## Testing

The PL/I crate includes 350+ tests:
- **Parser**: Verification of complex statement combinations where keywords are used as identifiers.
- **Types**: Validation of decimal arithmetic precision and string manipulation rules.
- **Exceptions**: Tests for ON-unit nesting, condition propagation, and REVERT logic.
- **Built-ins**: Exhaustive tests for each implemented built-in function.

```sh
cargo test -p open-mainframe-pli
```
