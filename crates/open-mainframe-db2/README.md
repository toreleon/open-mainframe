# open-mainframe-db2

DB2 SQL preprocessing, runtime execution, and utility services for the OpenMainframe z/OS clone. This crate provides a complete DB2-for-z/OS compatibility layer including EXEC SQL preprocessing for COBOL programs, DB2-to-PostgreSQL SQL dialect translation, cursor management with scrollable cursors, transaction management with savepoints, SQLCA status tracking with 52 SQLCODE constants, BIND/REBIND/FREE lifecycle, DCLGEN copybook generation, and DSNTEP2 dynamic SQL execution.

## Overview

On z/OS, DB2 is the primary relational database system. COBOL and PL/I programs access DB2 through embedded SQL — `EXEC SQL ... END-EXEC` statements are processed by the DB2 precompiler, which extracts the SQL into a DBRM (Database Request Module), replaces it with COBOL CALL statements, and generates a SQLCA communication area. The DBRM is then bound into a package or plan that the runtime uses to execute the SQL.

This crate emulates the full DB2 development and execution pipeline. The preprocessor scans COBOL fixed-format source for EXEC SQL blocks, classifies 28 SQL statement types, extracts host variables with indicator support, tracks WHENEVER directive state, and generates replacement COBOL CALL code. The runtime translates DB2 SQL dialect to PostgreSQL through 13 translation passes, manages cursors (including scrollable cursors with positioned UPDATE/DELETE), handles transactions with savepoints, and provides complete SQLCA status reporting with bidirectional SQLCODE-to-SQLSTATE mapping. An optional `postgres` feature enables live execution against a PostgreSQL database via connection pooling.

The crate also implements key DB2 utilities: BIND for creating prepared statement packages from DBRMs, DCLGEN for generating COBOL copybooks and PL/I includes from table schemas, DSNTEP2 for interactive SQL execution, and LOAD/UNLOAD for data movement.

## Architecture

```
┌──────────────────────────────────────────────────────────────────┐
│                       Public API (lib.rs)                       │
├────────────────┬──────────────────────┬──────────────────────────┤
│   Preprocess   │      Runtime         │       Utilities          │
├────────────────┼──────────────────────┼──────────────────────────┤
│ scanner.rs     │ connection.rs        │ bind.rs (utilities/)     │
│ mod.rs (pp)    │ cursor.rs            │ dclgen.rs                │
│ sqlca.rs (pp)  │ executor.rs          │                          │
│ dbrm.rs        │ sqlca.rs (runtime/)  │                          │
│                │ transaction.rs       │                          │
│                │ translate.rs         │                          │
│                │ types.rs             │                          │
├────────────────┴──────────────────────┴──────────────────────────┤
│  bind.rs (SYS-114)  │  ops.rs (SYS-115: DSNTEP2, LOAD, UNLOAD) │
└──────────────────────┴───────────────────────────────────────────┘
```

### Module Structure

| Module | Lines | Description |
|--------|------:|-------------|
| `lib.rs` | 84 | Crate root with `Db2Error` and re-exports |
| `preprocess/scanner` | 343 | COBOL fixed-format EXEC SQL block scanner |
| `preprocess/mod` | 1,171 | SQL preprocessor: classification, host vars, WHENEVER, CALL generation |
| `preprocess/sqlca` | 174 | SQLCA COBOL copybook generation and SQLCODE constants |
| `preprocess/dbrm` | 265 | DBRM generation with JSON serialization |
| `runtime/mod` | 123 | `Db2Runtime` top-level context |
| `runtime/connection` | 389 | Connection config, PostgreSQL pooling (cfg-gated) |
| `runtime/cursor` | 1,341 | Cursor lifecycle: DECLARE, OPEN, FETCH, CLOSE, scrollable |
| `runtime/executor` | 1,757 | SQL execution engine with dynamic SQL and COBOL formatting |
| `runtime/sqlca` | 786 | SQLCA with 52 SQLCODE constants, SQLSTATE mapping |
| `runtime/transaction` | 581 | COMMIT, ROLLBACK, savepoints, auto-commit |
| `runtime/translate` | 775 | DB2-to-PostgreSQL SQL dialect translator (13 passes) |
| `runtime/types` | 347 | DB2 type system (18 types), COBOL PIC mapping |
| `bind` | 1,240 | BIND/REBIND/FREE with in-memory DB2 catalog (SYS-114) |
| `ops` | 1,361 | DSNTEP2, DCLGEN, LOAD, UNLOAD utilities (SYS-115) |
| `utilities/mod` | 11 | Utilities re-export hub |
| `utilities/bind` | 323 | DBRM-to-package BIND utility |
| `utilities/dclgen` | 483 | COBOL/PL/I copybook generator from table schemas |
| **Total** | **~11,500** | |

## Key Types and Traits

### Preprocessing

- **`SqlScanner`** — Scans COBOL fixed-format source (columns 1–6 sequence, column 7 indicator, columns 8–72 code) for `EXEC SQL ... END-EXEC` blocks. Handles comment lines (`*`, `/`), continuation lines (`-`), and multi-line SQL.
- **`SqlBlock`** — A located EXEC SQL block with extracted SQL text and source line range.
- **`SqlPreprocessor`** — Orchestrates full preprocessing: scans blocks, classifies statements (28 types via `SqlStatementType`), extracts host variables with `:name:indicator` syntax, tracks WHENEVER directive state, and generates COBOL `CALL "SQLxxxx"` replacements.
- **`SqlStatementType`** — 28-variant enum: `SelectInto`, `Insert`, `Update`, `Delete`, `DeclareCursor`, `Open`, `Fetch`, `Close`, `Commit`, `Rollback`, `Prepare`, `Execute`, `ExecuteImmediate`, `Describe`, `Savepoint`, `ReleaseSavepoint`, `RollbackToSavepoint`, `Include`, `Whenever`, `DeclareTable`, `Merge`, `Call`, `Grant`, `Revoke`, `Label`, `Comment`, `Other`.
- **`HostVariable`** — Extracted host variable with name, optional indicator, statement number, and `HostVariableUsage` (Input/Output/Both).
- **`WheneverState`** — Tracks active WHENEVER directives for SQLERROR, NOT FOUND, and SQLWARNING conditions.
- **`Dbrm`** (preprocess) — Serializable DBRM output with JSON persistence, containing `DbrmStatement` and `DbrmHostVariable` entries.
- **`SqlCode`** — 11-variant enum of common SQL return codes with numeric code, description, and boolean checks.

### Runtime

- **`Db2Runtime`** — Top-level runtime context combining connection config, SQLCA, and SQL translator.
- **`Db2Connection`** / **`Db2ConnectionConfig`** — Connection management with builder pattern. Optional PostgreSQL connection pooling via r2d2 (behind `postgres` feature flag). Supports `from_env()` for environment-based configuration.
- **`Sqlca`** — SQL Communication Area with 52 SQLCODE constants, SQLERRD array, SQLWARN flags, and bidirectional SQLCODE↔SQLSTATE mapping. Includes `pg_state_to_sqlcode()` for mapping ~60 PostgreSQL SQLSTATE codes to DB2 equivalents.
- **`SqlExecutor`** — Central execution engine supporting SELECT INTO, INSERT, UPDATE, DELETE, and dynamic SQL (PREPARE/EXECUTE/EXECUTE IMMEDIATE/DESCRIBE). Operates in Mock or Live mode. Handles parameter binding (`:VAR` → `$1`), indicator variables, and COBOL-specific result formatting.
- **`Cursor`** / **`CursorManager`** — Full cursor lifecycle: DECLARE, OPEN, FETCH (forward-only and scrollable with 6 fetch directions), CLOSE, positioned UPDATE/DELETE, WITH HOLD, FOR UPDATE, COMMIT/ROLLBACK cursor behavior.
- **`FetchDirection`** — 6 variants: `Next`, `Prior`, `First`, `Last`, `Absolute(i64)`, `Relative(i64)`.
- **`TransactionManager`** — COMMIT, ROLLBACK, savepoints (create/release/rollback-to), auto-commit, implicit commit/rollback on program end/error.
- **`SqlTranslator`** — DB2-to-PostgreSQL translator with 13 translation passes covering FETCH FIRST→LIMIT, CONCAT→`||`, SUBSTR→SUBSTRING, special registers, data types, isolation clauses, and MERGE→INSERT...ON CONFLICT.
- **`Db2Type`** — 18-variant DB2 type enum with PostgreSQL conversion, Rust type mapping, and COBOL PIC parsing.
- **`SqlValue`** — Runtime value type: `Null`, `String`, `Integer`, `Float`, `Boolean`, `Binary`.
- **`SqlRow`** — Column-indexed result row with name-based access.
- **`PreparedDynamic`** — Registry entry for prepared dynamic SQL statements.

### BIND System

- **`Db2Catalog`** — In-memory simulated DB2 catalog with SYSPACKAGE, SYSPLAN, and SYSPACKLIST tables (HashMap-based).
- **`BindPackage`** / **`BindPlan`** — BIND PACKAGE and BIND PLAN commands with ACTION(ADD/REPLACE), ISOLATION, VALIDATE, EXPLAIN options.
- **`RebindCommand`** / **`FreeCommand`** — REBIND and FREE for packages and plans.
- **`CatalogPackage`** / **`CatalogPlan`** — Catalog entries for bound packages and plans.
- **`Binder`** (utilities) — Processes DBRM files into prepared statement packages with configurable isolation, action, and release options.

### Operational Utilities

- **`Dsntep2`** — In-memory dynamic SQL processor supporting SELECT, INSERT, UPDATE, DELETE, CREATE TABLE, DROP TABLE with simple WHERE clause filtering.
- **`DclgenUtil`** / **`Dclgen`** — Two DCLGEN implementations: one generates COBOL copybook/SQL DECLARE/host variables from `OpsTableDef`, the other generates from `TableInfo` with COBOL or PL/I output and null indicator support.
- **`LoadUtility`** / **`UnloadUtility`** — Data movement with delimited and fixed-width formats.

## Implementation Details

### SQL Preprocessing Pipeline

The preprocessor operates in a multi-stage pipeline:

1. **Scanning** (`SqlScanner`): Line-by-line state machine extracts `EXEC SQL ... END-EXEC` blocks from COBOL fixed-format source, handling continuation lines and comment filtering.
2. **Classification** (`SqlStatementType::from_sql`): Parses the first keywords to classify into 28 statement types. Multi-keyword disambiguation handles DECLARE CURSOR vs DECLARE TABLE, EXECUTE vs EXECUTE IMMEDIATE, ROLLBACK vs ROLLBACK TO SAVEPOINT.
3. **Host Variable Extraction**: Character-by-character scan for `:name` and `:name:indicator` patterns. Input/Output usage inferred from position relative to INTO/FROM/WHERE clauses.
4. **WHENEVER State Tracking**: Forward pass builds per-block WHENEVER state snapshots. Supports SQLERROR/NOT FOUND/SQLWARNING conditions with GO TO or CONTINUE actions.
5. **Code Generation**: Reverse pass replaces EXEC SQL blocks with COBOL `CALL "SQLxxxx"` statements. Statement type maps to call target (e.g., SelectInto→`SQLSELECT`, Prepare→`SQLPREP`, ExecuteImmediate→`SQLEXECI`). WHENEVER IF checks are appended after each CALL.
6. **DBRM Generation**: Collects all SQL statements and host variables into a serializable DBRM structure with JSON persistence.

### DB2-to-PostgreSQL Translation

The `SqlTranslator` applies 13 sequential passes to convert DB2 SQL syntax:

| Pass | DB2 Syntax | PostgreSQL Equivalent |
|------|-----------|----------------------|
| `translate_fetch_first` | `FETCH FIRST n ROWS ONLY` | `LIMIT n` |
| `translate_optimize_for` | `OPTIMIZE FOR n ROWS` | (removed) |
| `translate_isolation_clause` | `WITH UR/CS/RS/RR` | (removed) |
| `translate_concat` | `CONCAT(a, b)` | `(a \|\| b)` |
| `translate_substr` | `SUBSTR(str, pos, len)` | `SUBSTRING(str FROM pos FOR len)` |
| `translate_current_timestamp` | `CURRENT TIMESTAMP` | `CURRENT_TIMESTAMP` |
| `translate_current_date` | `CURRENT DATE` | `CURRENT_DATE` |
| `translate_functions` | `VALUE`, `LOCATE`, `POSSTR`, etc. | `COALESCE`, `POSITION`, etc. |
| `translate_data_types` | `VARCHAR FOR BIT DATA`, `GRAPHIC` | `BYTEA`, `TEXT` |
| `translate_for_update_of` | `FOR UPDATE OF col1, col2` | `FOR UPDATE` |
| `translate_set_current_schema` | `SET CURRENT SCHEMA = 'X'` | `SET search_path TO 'X'` |
| `translate_special_registers` | `CURRENT SQLID`, `CURRENT SERVER` | `current_schema`, `current_database()` |
| `translate_merge` | `MERGE INTO ... USING ...` | `INSERT ... ON CONFLICT ... DO UPDATE` |

### SQLCA and Error Mapping

The runtime SQLCA defines 52 SQLCODE constants covering all major DB2 error categories. The `code_to_state` method maps ~50 specific SQLCODEs to SQLSTATE values. The `pg_state_to_sqlcode` method maps ~60 PostgreSQL SQLSTATE codes (both specific and class-level) back to DB2 SQLCODEs, enabling seamless error translation when running against PostgreSQL.

### Cursor Management

Cursors support the full DB2 lifecycle:
- **Forward-only**: Standard DECLARE/OPEN/FETCH NEXT/CLOSE flow
- **Scrollable**: FETCH FIRST/LAST/PRIOR/ABSOLUTE(n)/RELATIVE(n) with negative indexing
- **WITH HOLD**: Cursors survive COMMIT (but not ROLLBACK)
- **FOR UPDATE**: Enables positioned UPDATE/DELETE on the current row
- **Multi-row FETCH**: Retrieves up to N rows; sets SQLERRD[2] to actual count
- **Transaction interaction**: COMMIT closes non-WITH-HOLD cursors; ROLLBACK closes all

### BIND Lifecycle

The BIND system models the DB2 catalog with three logical tables:
- **SYSPACKAGE**: Bound packages with collection ID, version, isolation, validation
- **SYSPLAN**: Application plans linking packages
- **SYSPACKLIST**: Package-to-plan associations

BIND PACKAGE validates the DBRM, checks ACTION(ADD/REPLACE) semantics, and inserts catalog entries. BIND PLAN validates package references (unless VALIDATE(RUN)) and creates plan+packlist entries. REBIND updates options and timestamps. FREE removes catalog entries and associated packlist records.

## Feature Coverage

### SQL Preprocessing

| Feature | Status |
|---------|--------|
| EXEC SQL block scanning (fixed-format COBOL) | Implemented |
| Multi-line and continuation line support | Implemented |
| Statement classification (28 types) | Implemented |
| Host variable extraction (`:name:indicator`) | Implemented |
| Input/Output usage inference | Implemented |
| WHENEVER SQLERROR/NOT FOUND/SQLWARNING | Implemented |
| WHENEVER GO TO and CONTINUE actions | Implemented |
| COBOL CALL code generation | Implemented |
| DBRM generation with JSON serialization | Implemented |
| SQLCA COBOL copybook generation | Implemented |
| INCLUDE SQLCA handling | Implemented |
| DECLARE TABLE (comment passthrough) | Implemented |

### SQL Dialect Translation

| Feature | Status |
|---------|--------|
| FETCH FIRST → LIMIT | Implemented |
| CONCAT → \|\| operator | Implemented |
| SUBSTR → SUBSTRING FROM/FOR | Implemented |
| CURRENT TIMESTAMP/DATE registers | Implemented |
| DB2 function mapping (12 functions) | Implemented |
| Data type translation (GRAPHIC, CLOB, etc.) | Implemented |
| FOR UPDATE OF simplification | Implemented |
| SET CURRENT SCHEMA translation | Implemented |
| Special register mapping (SQLID, SERVER, DEGREE) | Implemented |
| MERGE → INSERT ON CONFLICT | Implemented |
| Isolation clause removal | Implemented |
| OPTIMIZE FOR removal | Implemented |

### Runtime Execution

| Feature | Status |
|---------|--------|
| SELECT INTO with mock results | Implemented |
| INSERT/UPDATE/DELETE execution | Implemented |
| PREPARE / EXECUTE (dynamic SQL) | Implemented |
| EXECUTE IMMEDIATE | Implemented |
| DESCRIBE (column metadata) | Implemented |
| Parameter binding (`:VAR` → `$N`) | Implemented |
| Indicator variable handling (input/output) | Implemented |
| COBOL value formatting (pad, decimal) | Implemented |
| Mock mode (no database required) | Implemented |
| Live PostgreSQL execution (feature-gated) | Implemented |
| Connection pooling via r2d2 | Implemented |
| Environment-based configuration | Implemented |

### Cursor Support

| Feature | Status |
|---------|--------|
| DECLARE / OPEN / FETCH / CLOSE | Implemented |
| Forward-only cursors | Implemented |
| Scrollable cursors (6 fetch directions) | Implemented |
| WITH HOLD cursors | Implemented |
| FOR UPDATE / FOR READ ONLY | Implemented |
| Positioned UPDATE / DELETE | Implemented |
| Multi-row FETCH | Implemented |
| COMMIT/ROLLBACK cursor interaction | Implemented |
| Sensitivity (INSENSITIVE/SENSITIVE/ASENSITIVE) | Implemented |

### Transaction Management

| Feature | Status |
|---------|--------|
| COMMIT / ROLLBACK | Implemented |
| Savepoints (create/release/rollback-to) | Implemented |
| Auto-commit mode | Implemented |
| Implicit commit on program end | Implemented |
| Implicit rollback on program error | Implemented |
| Cursor manager integration | Implemented |

### SQLCA

| Feature | Status |
|---------|--------|
| 52 SQLCODE constants | Implemented |
| SQLSTATE mapping (~50 codes) | Implemented |
| PostgreSQL SQLSTATE mapping (~60 codes) | Implemented |
| SQLERRD (rows affected, etc.) | Implemented |
| SQLWARN flags (0–9, A) | Implemented |
| Error message truncation (70 chars) | Implemented |
| SqlcaBuilder pattern | Implemented |

### BIND / Catalog

| Feature | Status |
|---------|--------|
| BIND PACKAGE (ACTION ADD/REPLACE) | Implemented |
| BIND PLAN with PACKLIST | Implemented |
| REBIND PACKAGE / REBIND PLAN | Implemented |
| FREE PACKAGE / FREE PLAN | Implemented |
| Catalog queries (by collection, etc.) | Implemented |
| VALIDATE(BIND/RUN) | Implemented |
| EXPLAIN(YES/NO) | Implemented |
| Isolation levels (CS/RR/UR/RS) | Implemented |

### Operational Utilities

| Feature | Status |
|---------|--------|
| DSNTEP2 SELECT (with WHERE) | Implemented |
| DSNTEP2 INSERT / UPDATE / DELETE | Implemented |
| DSNTEP2 CREATE TABLE / DROP TABLE | Implemented |
| DCLGEN COBOL copybook generation | Implemented |
| DCLGEN PL/I include generation | Implemented |
| DCLGEN null indicator structures | Implemented |
| LOAD (delimited and fixed-width) | Implemented |
| UNLOAD (delimited and fixed-width) | Implemented |
| LOAD/UNLOAD round-trip | Implemented |

### DB2 Type System

| Feature | Status |
|---------|--------|
| 18 DB2 data types (CHAR through VARBINARY) | Implemented |
| DB2 → PostgreSQL type conversion | Implemented |
| DB2 → Rust type mapping | Implemented |
| COBOL PIC → DB2 type mapping | Implemented |
| Type mapping overrides | Implemented |

## Usage Examples

### SQL Preprocessing

```rust
use open_mainframe_db2::preprocess::SqlPreprocessor;

let cobol_source = r#"
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SAMPLE.
       PROCEDURE DIVISION.
           EXEC SQL
               SELECT NAME, SALARY
               INTO :WS-NAME, :WS-SALARY
               FROM EMPLOYEE
               WHERE EMPNO = :WS-EMPNO
           END-EXEC.
"#;

let mut preprocessor = SqlPreprocessor::new();
let result = preprocessor.process(cobol_source)?;
println!("Extracted {} SQL statements", result.sql_statements.len());
println!("Found {} host variables", result.host_variables.len());
println!("Generated COBOL:\n{}", result.cobol_source);
```

### SQL Translation

```rust
use open_mainframe_db2::SqlTranslator;

let translator = SqlTranslator::new();

let db2_sql = "SELECT * FROM EMP FETCH FIRST 10 ROWS ONLY WITH UR";
let pg_sql = translator.translate(db2_sql);
// Result: "SELECT * FROM EMP LIMIT 10"

let db2_merge = "MERGE INTO T USING S ON T.KEY = S.KEY \
    WHEN MATCHED THEN UPDATE SET T.VAL = S.VAL \
    WHEN NOT MATCHED THEN INSERT (KEY, VAL) VALUES (S.KEY, S.VAL)";
let pg_merge = translator.translate(db2_merge);
// Result: INSERT ... ON CONFLICT (KEY) DO UPDATE SET ...
```

### Cursor Management

```rust
use open_mainframe_db2::{Cursor, CursorManager, FetchDirection};

let mut mgr = CursorManager::new();
let cursor = Cursor::new("EMP_CSR", "SELECT * FROM EMP", vec![])
    .with_scroll()
    .with_hold();
mgr.declare(cursor)?;
mgr.open("EMP_CSR", &Default::default())?;

// Scrollable fetch
let first = mgr.fetch_direction("EMP_CSR", FetchDirection::First)?;
let last = mgr.fetch_direction("EMP_CSR", FetchDirection::Last)?;
let row5 = mgr.fetch_direction("EMP_CSR", FetchDirection::Absolute(5))?;

mgr.close("EMP_CSR")?;
```

### BIND Lifecycle

```rust
use open_mainframe_db2::{
    BindPackage, BindPlan, Db2Catalog, BindDbrm, CatalogIsolation,
};

let mut catalog = Db2Catalog::new();
let mut dbrm = BindDbrm::new("MYPROG");
dbrm.add_sql("SELECT * FROM EMP WHERE DEPT = ?");

let bind_pkg = BindPackage::new("PROD", "MYPROG");
let pkg = bind_pkg.bind(&mut catalog, &dbrm)?;

let mut bind_plan = BindPlan::new("MYPLAN");
bind_plan.add_package("PROD", &pkg.name, "");
let plan = bind_plan.bind(&mut catalog)?;
```

### DCLGEN

```rust
use open_mainframe_db2::{Dclgen, DclgenOptions, DclgenLanguage, TableInfo, ColumnInfo};

let table = TableInfo {
    schema: "HR".into(),
    table: "EMPLOYEE".into(),
    columns: vec![
        ColumnInfo { name: "EMPNO".into(), data_type: "INTEGER".into(),
                     max_length: None, precision: None, scale: None, nullable: false },
        ColumnInfo { name: "NAME".into(), data_type: "VARCHAR".into(),
                     max_length: Some(30), precision: None, scale: None, nullable: true },
    ],
};

let dclgen = Dclgen::new();
let copybook = dclgen.generate(&table)?;
println!("{}", copybook);
```

## Dependencies

| Dependency | Purpose |
|------------|---------|
| `thiserror` | Error type derive macros |
| `serde` | DBRM serialization/deserialization |
| `serde_json` | JSON format for DBRM files |
| `postgres` | PostgreSQL client (optional, feature-gated) |
| `r2d2` | Connection pooling (optional, feature-gated) |
| `r2d2_postgres` | PostgreSQL r2d2 adapter (optional, feature-gated) |

### Feature Flags

| Feature | Description |
|---------|-------------|
| `postgres` | Enables live PostgreSQL execution via connection pooling |

## Testing

Run the full test suite:

```bash
cargo test -p open-mainframe-db2
```

The crate includes approximately 200 unit tests organized by module:

- **Scanner** (8 tests): Single/multi-line EXEC SQL, comment handling, continuation lines, unclosed block detection
- **Preprocessor** (30 tests): All 28 statement types, host variable extraction, WHENEVER state management, CALL code generation
- **SQLCA (preprocess)** (4 tests): Copybook generation, SQLCODE conditions, numeric values
- **DBRM** (4 tests): Creation, statement aggregation, listing format, file round-trip
- **Cursor** (28 tests): Lifecycle, scrollable fetch (all 6 directions), WITH HOLD, positioned operations, multi-row fetch
- **Executor** (46 tests): SELECT INTO, DML, dynamic SQL, parameter binding, indicator variables, COBOL formatting, mock/live mode
- **SQLCA (runtime)** (21 tests): 52 constants, SQLSTATE mapping, PostgreSQL error mapping, builder pattern
- **Transaction** (17 tests): COMMIT, ROLLBACK, savepoints, auto-commit, implicit behavior
- **Translate** (19 tests): All 13 translation passes, combined translations, MERGE conversion
- **Types** (8 tests): DB2 type parsing, PostgreSQL conversion, COBOL PIC mapping
- **BIND** (24 tests): BIND/REBIND/FREE lifecycle, ACTION semantics, catalog queries
- **Ops** (22 tests): DSNTEP2 SQL execution, DCLGEN, LOAD/UNLOAD, round-trip tests
- **Utilities** (13 tests): Binder, DCLGEN with COBOL/PL/I output, null indicators

All runtime tests operate in mock mode by default, requiring no live database.

## Limitations and Future Work

- **No stored procedure support**: CALL statements are classified but not executed. Stored procedure creation and invocation are not implemented.
- **Limited SQL parsing in DSNTEP2**: The in-memory SQL engine supports only simple single-table queries with basic WHERE equality filters. Joins, subqueries, aggregations, and GROUP BY are not supported.
- **No EXPLAIN plan generation**: The `EXPLAIN(YES)` bind option is tracked but does not generate access path information.
- **MERGE translation is basic**: Only simple MERGE patterns with a single match column are translated to PostgreSQL ON CONFLICT syntax.
- **No LOB support**: BLOB and CLOB types are mapped but large object streaming I/O is not implemented.
- **No SQLDA**: The SQL Descriptor Area for dynamic SQL is not fully modeled; DESCRIBE returns column metadata but the traditional SQLDA structure is not emulated.
- **No multi-row INSERT**: Batch INSERT operations are not optimized.
- **PostgreSQL-only backend**: The translation layer targets PostgreSQL exclusively. Other SQL backends (SQLite, MySQL) are not supported.
- **No utility execution on live data**: DSNTEP2, LOAD, and UNLOAD operate against in-memory tables only; they do not connect to the PostgreSQL backend.
- **DCLGEN does not query live schemas**: Table metadata must be provided programmatically; automatic schema introspection from a connected database is not implemented.
