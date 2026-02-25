# open-mainframe-easytrieve

Easytrieve Plus language support for the OpenMainframe z/OS clone. This crate provides a complete Easytrieve Plus implementation: a fixed-column lexer and parser (87 keywords, 46+ statement types), an interpreter with arithmetic and control flow, a report generator with headings, page control, control breaks, and summary accumulators, sequential and indexed file processing with edit masks, multi-key SORT and MATCH operations, embedded SQL support with a pluggable bridge trait, and a macro/COPY system.

## Overview

Easytrieve Plus is a report-generation and data-extraction language widely used on z/OS mainframes. Programs follow a declarative structure: FILE definitions describe input/output datasets, DEFINE statements declare fields within those files, JOB sections contain the procedural logic, and REPORT sections control output formatting. Easytrieve excels at reading sequential and indexed files, sorting records, performing control-break processing, and generating formatted reports with automatic subtotals.

This crate emulates the full Easytrieve Plus development experience. The parser handles the fixed-column source format (columns 1–4 line number, 5–72 statement, 73–80 sequence) and produces a structured AST. The interpreter executes the parsed program with variable storage, arithmetic, and nested control flow. The file I/O module supports both sequential (GET/PUT) and indexed (READ/WRITE/POINT) access with edit masks for numeric formatting. The sort module provides multi-key sorting, sorted merge-join matching between files, and BEFORE/AFTER control break processing. The report generator handles page control, headings, detail lines, control breaks with automatic subtotals, and SUM/COUNT/AVG/MIN/MAX summary statistics. Embedded SQL is supported through a `SqlBridge` trait with a mock implementation for testing.

## Architecture

```
┌───────────────────────────────────────────────────────────┐
│                    Public API (lib.rs)                    │
├──────────┬────────────┬────────────┬─────────────────────┤
│  Parser  │ Interpreter│   Report   │   File I/O          │
│          │            │            │                     │
│ parser   │ interpreter│ report     │ fileio              │
├──────────┴────────────┴────────────┼─────────────────────┤
│      Sort / Match / Control Break  │   SQL Bridge        │
│                                    │                     │
│      sort                          │   sql               │
├────────────────────────────────────┴─────────────────────┤
│               Macros / COPY / External Calls             │
│               macros                                     │
├──────────────────────────────────────────────────────────┤
│          miette (diagnostics)  │  thiserror (errors)     │
└────────────────────────────────┴─────────────────────────┘
```

### Module Structure

| Module | Lines | Description |
|--------|------:|-------------|
| `parser` | 1,325 | Fixed-column lexer, 87 keywords, 46+ statement AST types |
| `fileio` | 933 | Sequential/indexed file I/O, records, edit masks, tables |
| `interpreter` | 760 | Variable storage, arithmetic, control flow execution |
| `report` | 693 | Report formatting, page control, control breaks, summaries |
| `sort` | 613 | Multi-key SORT, sorted merge-join MATCH, control breaks |
| `macros` | 520 | MACRO/END-MACRO, COPY library, external CALL |
| `sql` | 388 | Embedded SQL blocks, host variables, SqlBridge trait |
| `lib.rs` | 29 | Crate root and re-exports |
| **Total** | **~5,260** | |

## Key Types and Traits

### Parser

- **`EzParser`** — Stateless parser with `tokenize()` and `parse()` methods. Handles fixed-column Easytrieve format (columns 1–4 line number, 5–72 statement, 73–80 sequence number).
- **`EzToken`** — 10-variant token type: `Keyword`, `Identifier`, `Number`, `StringLiteral`, `Operator`, `LeftParen`, `RightParen`, `Comma`, `Period`, `EndOfLine`.
- **`EzStatement`** — 46+ variant enum covering all Easytrieve statements: `File`, `Define`, `Job`, `Sort`, `Put`, `Get`, `Print`, `Display`, `Heading`, `Line`, `Title`, `If`/`Else`/`ElseIf`/`EndIf`, `Do`/`EndDo`, `GoTo`, `Perform`, `Stop`, `Macro`, `Copy`, `Sql`, `End`, `Assignment`, `Arithmetic`, `Report`, `Sequence`, `Control`, `Sum`, `Call`, `Case`/`When`/`EndCase`, `Read`, `Write`, `Point`, `Close`, `Search`, `Mask`, `Link`, `Transfer`, `Proc`/`EndProc`, `NewPage`, `Skip`, `Release`, `Parm`, `Dli`, `Record`, `Label`, `Comment`.
- **`EzProgram`** — Parsed program with `files`, `defines`, `activities`, and `statements` collections.
- **`SortDirection`** — `Ascending` / `Descending`.

### Interpreter

- **`EzInterpreter`** — Execution engine with variable storage (`HashMap<String, EzVariable>`), output buffer, label index, and runaway protection (default 100,000 statement limit).
- **`EzValue`** — Runtime value: `Numeric(i64)`, `Decimal(f64)`, `Alpha(String)`. Methods: `as_numeric()`, `as_f64()`, `as_string()`, `is_truthy()`.
- **`EzVariable`** — Named variable with value, data type, and length.

### File I/O

- **`EzFile`** — File definition with name, LRECL, RECFM, and ordered field definitions.
- **`EzFieldDef`** — Field definition: name, 1-based position, length, data type, optional heading.
- **`EzRecord`** — Fixed-length byte buffer with field get/set. Numeric fields are right-justified with zero-fill; alpha fields are left-justified with space-fill.
- **`FileProcessor`** — Sequential GET/PUT processor with input/output record buffers and rewind support.
- **`IndexedFileProcessor`** — VSAM KSDS-style keyed access with READ, WRITE (add/update/delete), POINT, and sequential GET_NEXT via sorted key index.
- **`EditMask`** — Numeric formatting with pattern characters: `9` (digit), `Z` (suppress leading zeros), `.` (decimal), `,` (thousands), `$` (floating currency), `CR`/`-` (negative indicators).
- **`EzTable`** — In-memory lookup table with sequential and binary search support.
- **`RecordFormat`** — `Fixed`, `FixedBlocked`, `Variable`, `VariableBlocked`.

### Report Generation

- **`ReportDef`** — Report definition with page dimensions, headings, detail lines, titles, control fields, and sum fields.
- **`ReportFormatter`** — Report output engine with automatic page breaks, header printing, control break detection, and summary accumulation.
- **`PageControl`** — Page number, line position, and page break detection.
- **`ControlBreak`** — Detects value changes in a control field and maintains per-group accumulators.
- **`SummaryLine`** — Running SUM, COUNT, AVG, MIN, MAX aggregation per field.

### Sort and Match

- **`EzSort`** — Multi-key record sorter. Keys are prioritized in order, with per-key ascending/descending direction.
- **`SortKey`** — Sort key definition with field name, position, length, and order.
- **`EzMatch`** — Sorted merge-join between two files producing `MatchedPair` results with `Matched`, `File1Only`, or `File2Only` status.
- **`EzControlBreak`** — BEFORE/AFTER break event processing with accumulation and finalization.

### SQL

- **`SqlBridge`** — Trait for pluggable SQL execution: `execute()`, `query()`, `commit()`, `rollback()`.
- **`EzSqlBlock`** — Parsed SQL block with host variable extraction (`:varname` syntax) and parameterized SQL generation (`?` placeholders).
- **`EzSqlResult`** — Result set with columns, rows (as `HashMap<String, String>`), rows affected, and SQLCODE.
- **`MockSqlBridge`** — Test implementation with pre-configured results and execution logging.

### Macros and COPY

- **`EzMacro`** — Macro definition with positional parameters and body statements. Expansion substitutes `&PARAM` references.
- **`MacroLibrary`** — Named macro registry with depth-limited expansion (default max 10).
- **`EzCopy`** — COPY member reference resolved from a `CopyLibrary`.
- **`CopyLibrary`** — Named copy member registry.
- **`EzExternalCall`** — External program call with parameters and return code (simulated).

## Implementation Details

### Lexer and Parser

The lexer handles Easytrieve's fixed-column format:
- Columns 1–4: line number (ignored)
- Columns 5–72: statement text
- Columns 73–80: sequence number (ignored)
- Comment lines: `*` in column 5 (first column of statement area)

Tokenization supports single-quoted strings with doubled-quote escaping (`''`), decimal numbers, two-character operators (`<=`, `>=`, `<>`, `:=`), and identifiers starting with alphabetic characters or `#`, `@`, `$`, `_`.

The parser recognizes 87 keywords and produces 46+ statement variants. Each keyword has a dedicated parse method. Statements are categorized into three program sections: `files` (FILE/RECORD), `defines` (DEFINE), and `activities` (all procedural statements from JOB onward).

### Interpreter Execution Model

The interpreter uses a program counter (`pc`) iterating over `program.activities`:
- **Initialization**: All DEFINE statements are pre-loaded as variables. Labels are pre-indexed for O(1) lookup.
- **Control flow**: IF/ELSE/END-IF and DO/END-DO use depth-tracking forward scans to find matching end markers. GOTO jumps to pre-indexed labels. PERFORM executes subroutines inline from the label position.
- **Arithmetic**: ADD, SUBTRACT, MULTIPLY, DIVIDE with automatic integer/decimal result selection.
- **Conditions**: Two-operand comparison with 12 operators (=, <>, >, >=, <, <=, EQ, NE, GT, GE, LT, LE). Tries numeric comparison first, falls back to string comparison.
- **Runaway protection**: Execution halts after `max_statements` (default 100,000) iterations.

### Edit Mask Formatting

Edit masks format numeric values using a right-to-left digit fill algorithm:
- `9`: Always displays a digit
- `Z`: Suppresses leading zeros (replaces with space)
- `.`: Decimal point position
- `,`: Thousands separator (suppressed in leading-zero region)
- `$`: Floating currency symbol (placed before first significant digit)
- `CR`: Appended for negative values
- `-`: Minus sign appended for negative values

### Sorted Merge-Join (MATCH)

The MATCH operation implements a classic sorted merge-join (full outer join):
1. Both input record sets must be pre-sorted by key
2. Two pointers advance through the sets, comparing extracted key bytes
3. Equal keys produce `Matched` pairs; unequal keys produce `File1Only` or `File2Only`
4. Remaining records from either file are appended after one set is exhausted

### Control Break Processing

The `EzControlBreak` generates BEFORE/AFTER events:
- First record triggers a BEFORE event
- Value change triggers AFTER (for previous group, with accumulators) then BEFORE (for new group)
- `finalize()` generates the final AFTER event
- Accumulators track running sums per field within each group

## Feature Coverage

### Parser

| Feature | Status |
|---------|--------|
| Fixed-column format (1–4 / 5–72 / 73–80) | Implemented |
| 87 keywords | Implemented |
| 46+ statement types | Implemented |
| String literals with quote escaping | Implemented |
| Comment lines (* in column 5) | Implemented |
| Two-character operators | Implemented |
| FILE with attributes | Implemented |
| DEFINE with position/length/type/value | Implemented |
| JOB with input file | Implemented |
| SORT with direction | Implemented |
| IF/ELSE/ELSEIF/END-IF | Implemented |
| DO/END-DO | Implemented |
| CASE/WHEN/END-CASE | Implemented |
| PROC/END-PROC | Implemented |
| DLI function calls | Implemented |
| PARM options | Implemented |

### Interpreter

| Feature | Status |
|---------|--------|
| Variable initialization from DEFINE | Implemented |
| Assignment statements | Implemented |
| Arithmetic (ADD, SUBTRACT, MULTIPLY, DIVIDE) | Implemented |
| Expression evaluation (infix) | Implemented |
| IF/ELSE/END-IF control flow | Implemented |
| DO/END-DO loops | Implemented |
| GOTO (label jumping) | Implemented |
| PERFORM (subroutine call) | Implemented |
| DISPLAY output | Implemented |
| STOP execution | Implemented |
| Runaway protection | Implemented |
| Nested IF/DO support | Implemented |
| CASE/WHEN execution | Stub / Not yet |

### File I/O

| Feature | Status |
|---------|--------|
| Sequential file processing (GET/PUT) | Implemented |
| Indexed file processing (READ/WRITE/POINT) | Implemented |
| Record format parsing (F/FB/V/VB) | Implemented |
| Field position/length validation | Implemented |
| Numeric field right-justification | Implemented |
| Alpha field space-fill | Implemented |
| Edit masks (9/Z/./,/$/CR/-) | Implemented |
| Table SEARCH (sequential and binary) | Implemented |
| Rewind support | Implemented |
| Write add/update/delete | Implemented |

### Report Generation

| Feature | Status |
|---------|--------|
| Report definition with page dimensions | Implemented |
| Heading lines | Implemented |
| Title lines | Implemented |
| Detail lines | Implemented |
| Automatic page breaks | Implemented |
| Page numbering | Implemented |
| Control break detection | Implemented |
| Control break accumulators | Implemented |
| Summary statistics (SUM/COUNT/AVG/MIN/MAX) | Implemented |
| Summary line output | Implemented |
| Multi-level control breaks | Partial |

### Sort and Match

| Feature | Status |
|---------|--------|
| Multi-key ascending sort | Implemented |
| Multi-key descending sort | Implemented |
| Key priority ordering | Implemented |
| Sorted merge-join MATCH | Implemented |
| File1Only / File2Only / Matched status | Implemented |
| BEFORE/AFTER control break events | Implemented |
| Break accumulation | Implemented |
| Break finalization | Implemented |

### SQL

| Feature | Status |
|---------|--------|
| SQL block parsing | Implemented |
| Host variable extraction (`:varname`) | Implemented |
| Parameterized SQL generation | Implemented |
| SqlBridge trait (pluggable execution) | Implemented |
| MockSqlBridge for testing | Implemented |
| Result set access by column name | Implemented |
| SQLCODE checking | Implemented |
| Commit / Rollback | Implemented |
| Live database bridge | Stub / Not yet |

### Macros and COPY

| Feature | Status |
|---------|--------|
| MACRO/END-MACRO definition | Implemented |
| Positional parameter substitution | Implemented |
| &PARAM reference expansion | Implemented |
| Macro library with registration | Implemented |
| Max depth protection | Implemented |
| COPY member resolution | Implemented |
| COPY library | Implemented |
| External CALL with parameters | Implemented |
| External CALL simulation | Implemented |
| Live external program execution | Stub / Not yet |

## Usage Examples

### Parsing an Easytrieve Program

```rust
use open_mainframe_easytrieve::EzParser;

let source = r#"
     FILE EMPFILE
     DEFINE EMPNO  EMPFILE 1 6 A
     DEFINE NAME   EMPFILE 7 30 A
     JOB INPUT EMPFILE
     DISPLAY EMPNO ' ' NAME
"#;

let program = EzParser::parse(source)?;
println!("Files: {}", program.files.len());
println!("Defines: {}", program.defines.len());
println!("Activities: {}", program.activities.len());
```

### Running a Program

```rust
use open_mainframe_easytrieve::{EzParser, EzInterpreter};

let source = r#"
     DEFINE TOTAL W 8 N VALUE 0
     JOB
     TOTAL = TOTAL + 100
     DISPLAY 'TOTAL IS ' TOTAL
"#;

let program = EzParser::parse(source)?;
let mut interp = EzInterpreter::new();
interp.execute(&program)?;
for line in &interp.output {
    println!("{}", line);
}
```

### File Processing with Edit Masks

```rust
use open_mainframe_easytrieve::{EzFile, EzFieldDef, EzRecord, EditMask, RecordFormat};

let mut file = EzFile::new("PAYROLL", 80, RecordFormat::Fixed);
file.add_field(EzFieldDef::new("SALARY", 1, 10, "N"))?;

let mut record = EzRecord::new(80);
record.set_field(file.get_field("SALARY").unwrap(), "50000")?;

let mask = EditMask::new("SALARY-MASK", "ZZZ,ZZ9.99");
println!("{}", mask.format(50000.0));  // " 50,000.00"
```

### Report Generation

```rust
use open_mainframe_easytrieve::{ReportDef, ReportFormatter};
use std::collections::HashMap;

let mut report = ReportDef::new("EMPRPT");
report.add_heading(1, vec!["EMPLOYEE REPORT".into()]);
report.add_line(1, vec!["EMPNO".into(), "NAME".into(), "SALARY".into()]);
report.set_sum_fields(vec!["SALARY".into()]);

let mut formatter = ReportFormatter::new(report);
let mut values = HashMap::new();
values.insert("EMPNO".into(), "001".into());
values.insert("NAME".into(), "SMITH".into());
values.insert("SALARY".into(), "50000".into());
formatter.print_detail(&values);
formatter.print_summary();

println!("{}", formatter.get_output_text());
```

### Embedded SQL

```rust
use open_mainframe_easytrieve::{EzSqlBlock, MockSqlBridge, SqlBridge, EzSqlResult};

let block = EzSqlBlock::new("SELECT NAME FROM EMP WHERE EMPNO = :WS-EMPNO");
assert_eq!(block.host_variables, vec!["WS-EMPNO"]);
assert_eq!(block.parameterized_sql(), "SELECT NAME FROM EMP WHERE EMPNO = ?");

let mut bridge = MockSqlBridge::new();
let result = bridge.query(&block.parameterized_sql(), &["001".into()])?;
```

## Dependencies

| Dependency | Purpose |
|------------|---------|
| `miette` | Diagnostic error reporting with source spans |
| `thiserror` | Error type derive macros |

## Testing

Run the full test suite:

```bash
cargo test -p open-mainframe-easytrieve
```

The crate includes approximately 87 unit tests:

- **Parser** (12 tests): Tokenization, string/number/operator handling, fixed-column extraction, FILE/DEFINE/JOB/IF/SORT parsing
- **Interpreter** (12 tests): DISPLAY, DEFINE, assignment, arithmetic, IF branches, expressions, STOP, value truthiness
- **File I/O** (18 tests): File/field creation, record get/set, GET/PUT, rewind, indexed CRUD, POINT, edit masks, table search
- **Report** (11 tests): Report creation, page control, control breaks, summaries, formatting with headers/details
- **Sort** (12 tests): Ascending/descending, multi-key, MATCH (matched/unmatched/empty), control break events
- **SQL** (10 tests): Block parsing, host variables, parameterized SQL, result sets, mock bridge
- **Macros** (12 tests): Macro expansion, parameter substitution, library, external calls, COPY resolution

## Limitations and Future Work

- **No CASE/WHEN interpreter execution**: CASE/WHEN statements are parsed but not executed by the interpreter.
- **No live SQL bridge**: The `SqlBridge` trait has only a mock implementation; no live database connector is provided.
- **No live external program calls**: `EzExternalCall::simulate()` always returns 0; actual program invocation is not implemented.
- **Single-level control breaks in reports**: The report formatter detects control breaks but does not support hierarchical multi-level break nesting with subtotals at each level.
- **No DLI interpreter execution**: DLI statements are parsed but not executed by the interpreter.
- **No automatic file I/O integration**: The interpreter does not automatically connect to `FileProcessor` or `IndexedFileProcessor` for GET/PUT/READ/WRITE statements.
- **No PARM processing**: PARM statements are parsed but runtime parameter handling is not implemented.
- **No continuation lines**: The parser does not support multi-line statement continuation.
- **Edit mask CR handling is basic**: The CR suffix is appended but currency symbol placement with CR is not fully compatible with all Easytrieve patterns.
- **Macro expansion does not cover all statement types**: Only Display, Assignment, If, Perform, and GoTo statements support parameter substitution in macro expansion.
