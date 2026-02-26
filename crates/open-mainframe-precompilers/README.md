# open-mainframe-precompilers

z/OS COBOL Precompilers — a specialized Rust toolset for source-to-source transformation of DB2 and CICS statements within COBOL programs for the OpenMainframe project.

## Overview

Mainframe COBOL programs often contain embedded SQL (`EXEC SQL`) or CICS (`EXEC CICS`) commands that are not valid standard COBOL. This crate provides the precompilers necessary to transform these embedded blocks into standard COBOL `CALL` statements, enabling the resulting code to be compiled by a standard COBOL compiler.

## Architecture

```
    COBOL Source with EXEC                Standard COBOL Source
    ┌────────────────────┐                ┌────────────────────────┐
    │  EXEC SQL ...      │   Precompile   │   CALL 'DSNHLI' ...    │
    │  EXEC CICS ...     │ ─────────────> │   CALL 'DFHEI1' ...    │
    └────────────────────┘    Precomp     └────────────────────────┘
               │                                       │
               ▼                                       ▼
    ┌────────────────────┐                ┌────────────────────────┐
    │   Parser Engine    │ ─────────────> │   Database Request     │
    │  (DB2 / CICS)      │   Extraction   │   Module (DBRM)        │
    └────────────────────┘                └────────────────────────┘
```

### Module Structure

| Module | Description |
|--------|-------------|
| `db2` | DB2 precompiler: Transforms `EXEC SQL` to `CALL 'DSNHLI'` and generates DBRM files |
| `cics` | CICS precompiler: Transforms `EXEC CICS` to `CALL 'DFHEI1'` and processes BMS maps |

## Key Types and Components

### DB2 Precompiler
- `Db2Precompiler`: The main engine for SQL statement transformation.
- `Dbrm`: Database Request Module containing extracted SQL for later bind processing.
- `ExecSqlBlock`: Represents a single `EXEC SQL ... END-EXEC` block.

### CICS Precompiler
- `CicsPrecompiler`: The main engine for CICS command transformation.
- `BmsSymbolicMap`: Handles the generation of COBOL copybooks from BMS map definitions.
- `ExecCicsBlock`: Represents a single `EXEC CICS ... END-EXEC` block.

## Feature Coverage

| Feature | Category | Status |
|---------|----------|--------|
| SQL Detection   | DB2      | Implemented |
| Host Var Mapping| DB2      | Implemented |
| DBRM Generation | DB2      | Implemented |
| CICS Command Map| CICS     | Implemented |
| BMS Map Processing| CICS     | Implemented |
| Integrated Pass | General  | Implemented (DB2 + CICS in one pass) |

## Usage Examples

### Precompiling a COBOL Program with DB2

```rust
use open_mainframe_precompilers::db2::Db2Precompiler;

let source = r#"
       EXEC SQL
           SELECT NAME INTO :HOST-NAME
           FROM CUSTOMERS WHERE ID = :HOST-ID
       END-EXEC.
"#;

let mut precomp = Db2Precompiler::new();
let result = precomp.precompile(source).unwrap();

println!("Transformed source: {}", result.transformed_source);
println!("Generated DBRM has {} statements", result.dbrm.statements.len());
```

## Testing

The Precompilers crate includes 150+ tests:
- **DB2**: Validates host variable detection and SQL-to-CALL mapping logic.
- **CICS**: Tests the transformation of various CICS commands (LINK, READ, SEND MAP).
- **Integrated**: Ensures that programs containing both SQL and CICS statements are handled correctly.
- **BMS**: Verification of symbolic map generation for complex BMS screens.

```sh
cargo test -p open-mainframe-precompilers
```
