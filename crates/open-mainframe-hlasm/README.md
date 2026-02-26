# open-mainframe-hlasm

HLASM (High Level Assembler) — a high-performance assembler implementation for the OpenMainframe project, supporting z/Architecture instruction encoding, complex macro processing, and object module generation.

## Overview

HLASM is the standard assembly language for IBM mainframes. This crate provides a complete toolchain for assembling HLASM source code into executable object modules. It features a fixed-format lexer tailored for column-based assembly, a powerful two-pass macro engine, and a comprehensive z/Architecture instruction catalog.

## Architecture

```
    HLASM Source                          Object Generation
    ┌──────────────┐                      ┌────────────────────┐
    │ Label Opcode │    Two-Pass          │   Object Engine    │
    │ Operands...  │ ──────────────────>  │    (ESD/TXT/RLD)   │
    └──────────────┘    MacroEngine       └────────────────────┘
           │                                        │
           ▼                                        ▼
    ┌──────────────┐    Instruction       ┌────────────────────┐
    │  Mnemonic    │ ──────────────────>  │   Binary Encoding  │
    │  Resolution  │    InsnCatalog       │   (2, 4, 6 bytes)  │
    └──────────────┘                      └────────────────────┘
           │                                        │
           ▼                                        ▼
    ┌──────────────┐    Symbol Table      ┌────────────────────┐
    │ Expressions  │ <──────────────────  │   Symbol Table     │
    │ L', T', S'   │    SymbolTable       │   Resolution       │
    └──────────────┘                      └────────────────────┘
```

### Module Structure

| Module | Description |
|--------|-------------|
| `lexer` | Fixed-format source parser (columns 1, 10, 16, 72) and operand tokenizer |
| `macros` | Macro engine supporting `MACRO/MEND`, parameter substitution, and `COPY` |
| `instruction`| z/Architecture instruction catalog and binary encoding for 200+ mnemonics |
| `symbol` | Symbol table management and multi-pass expression evaluation |
| `directives` | Assembler directives (DC, DS, EQU, USING, DROP, CSECT, etc.) |
| `conditional`| Conditional assembly (`AIF`, `AGO`, `SETx`) and variable management |
| `object` | Object module generation in standard (OBJ) and extended (GOFF) formats |

## Key Types and Components

### Lexer
- `SourceLine`: Representation of a physical source line, handling continuation characters in column 72.
- `InstructionLine`: Parsed representation of Label, Opcode, Operands, and Remarks.
- `Token`: Operand tokens including registers (R0-R15), symbols, literals (`=L'8'`), and self-defining terms.

### Instruction Engine
- `InsnCatalog`: Global registry of all supported z/Architecture instructions.
- `InsnFormat`: Enumeration of instruction formats (RR, RX, RXY, RS, RSY, SI, SS, etc.).
- `encode_instruction`: Core function translating mnemonic and operands into binary machine code.

### Macro Engine
- `MacroEngine`: Handles macro expansion, including local and global variable scope.
- `MacroDef`: Internal representation of a macro definition.
- `SystemVars`: Support for `&SYSNDX`, `&SYSDATE`, `&SYSTIME`, etc.

### Object Generation
- `ObjectModule`: Container for ESD (External Symbol Dictionary), TXT (Text), RLD (Relocation Dictionary), and END records.
- `EsdItem`: Defines CSECTs, ENTRYs, and EXTRNs.
- `TxtRecord`: Contains the assembled machine code.

## Instruction Coverage

The crate supports over 200 instructions including:
- **Base**: L, ST, A, S, M, D, LA, BCT, BALR, etc.
- **Extended**: LY, STY, AY, SY, MSY, DSY (Long Displacement).
- **Decimal**: AP, SP, MP, DP, CP, ZAP, PACK, UNPK.
- **Logical**: CL, CLC, CLI, CLM, CLCL, CLCLE.
- **Floating Point**: AE, SE, ME, DE, CE (Hex and Binary).

## Usage Examples

### Assembling a Source Line

```rust
use open_mainframe_hlasm::lexer::parse_source_line;
use open_mainframe_hlasm::instruction::encode_instruction;

// 1. Parse HLASM source line
let line = "STEP1    LA    R1,DATA(R2)      Load Address".to_string();
let insn = parse_source_line(&line).unwrap();

// 2. Encode to machine code
let mut bytes = Vec::new();
encode_instruction(&insn.opcode, &insn.operands, &mut bytes).unwrap();
assert_eq!(bytes, vec![0x41, 0x12, 0x00, 0x00]); // Example RX encoding
```

### Using the Macro Engine

```rust
use open_mainframe_hlasm::macros::MacroEngine;

let mut engine = MacroEngine::new();
engine.define_macro(r#"
    MACRO
    MYMOVE &TO,&FROM
    L     R1,&FROM
    ST    R1,&TO
    MEND
"#).unwrap();

let expanded = engine.expand("    MYMOVE FIELD1,FIELD2").unwrap();
```

## Testing

The test suite ensures high fidelity to IBM HLASM behavior:
- **Lexer**: Validates column-sensitive parsing and complex continuation scenarios.
- **Instruction**: Round-trip tests for encoding all supported formats.
- **Macros**: Nested macro expansion and recursive parameter substitution tests.
- **Expression**: Evaluation of complex arithmetic with symbol attributes (e.g., `L'SYM-1`).

```sh
cargo test -p open-mainframe-hlasm
```
