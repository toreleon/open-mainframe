# open-mainframe-focus

Information Builders FOCUS — a comprehensive Rust implementation of the multi-dialect 4GL (Fourth Generation Language) for the OpenMainframe project. This crate provides the full suite of FOCUS engines: TABLE for reporting, GRAPH for visualization, MODIFY/MAINTAIN for data management, Dialogue Manager for control flow, and a robust data adapter layer for cross-platform data access.

## Overview

FOCUS is a powerful 4GL and database management system widely used on IBM mainframes for reporting and application development. This crate reimplements the core FOCUS components, enabling the execution of complex FOCEXECs (FOCUS procedures) against various data sources including native FOCUS files, VSAM, sequential files, and relational databases.

The architecture is centered around a multi-dialect parser (FOC-100) that can switch between different sub-languages depending on the command (e.g., `TABLE FILE`, `GRAPH FILE`, `MODIFY FILE`). The Master File Descriptor (MFD) system (FOC-101) provides the metadata layer that abstracts physical data storage into logical segments and fields.

## Architecture

```
    FOCEXEC Source                        Execution Environment
    ┌──────────────┐                      ┌────────────────────┐
    │ TABLE FILE   │    Parsing           │    Table Engine    │
    │ SUM SALES    │ ──────────────────>  │    (FOC-102)       │
    │ BY REGION    │    FocusParser       │  Aggregators, Joins│
    │ END          │                      └────────────────────┘
    └──────────────┘                                │
           │                                        ▼
           ▼                              ┌────────────────────┐
    ┌──────────────┐    Metadata          │   Data Adapters    │
    │ Master File  │ ──────────────────>  │    (FOC-107)       │
    │ (MFD / ACX)  │    MfdParser         │ Sequential, VSAM,  │
    └──────────────┘                      │ DB2, IMS, Native   │
                                          └────────────────────┘
           │                                        │
           ▼                                        ▼
    ┌──────────────┐    Formatting        ┌────────────────────┐
    │ Report Output│ <──────────────────  │   Output Engine    │
    │ (PDF, HTML,  │    TextFormatter     │    (FOC-108)       │
    │  HOLD, TEXT) │    HtmlFormatter     │ Stylesheets, Fonts │
    └──────────────┘                      └────────────────────┘
```

### Module Structure

| Module | Description |
|--------|-------------|
| `parser` | FOC-100: Multi-dialect parser (TABLE, GRAPH, MODIFY, Dialogue Manager, SQL) |
| `mfd` | FOC-101: Master File Descriptor & Access File parser and metadata model |
| `table_engine` | FOC-102: Reporting engine for data aggregation and cross-tabulation |
| `graph_engine`| FOC-103: Visualization engine producing text and graphical charts |
| `modify_engine`| FOC-104: Data maintenance engine (MODIFY/MAINTAIN) for batch/interactive updates |
| `dialogue` | FOC-105: Dialogue Manager interpreter with amper variables and control flow |
| `functions` | FOC-106: Library of built-in character, date, and numeric functions |
| `adapters` | FOC-107: Data source abstraction layer for MVS, DB2, and sequential data |
| `output` | FOC-108: Report formatting engine (Text, HTML, HOLD files) with styling |
| `joins` | FOC-109: Multi-source processing (JOIN, COMBINE, MATCH FILE) |
| `filedef` | FOC-110: Mainframe environment integration (FILEDEF, DYNAM, TSO/CICS interfaces) |

## Key Types and Components

### Parser (FOC-100)
- `FocusParser`: Orchestrates parsing across different dialects.
- `TableRequest`: AST representation of a `TABLE FILE` request.
- `DialogueCmd`: Control flow commands for Dialogue Manager (`-SET`, `-IF`, `-GOTO`).
- `Expr`: Universal expression evaluator for `DEFINE`, `COMPUTE`, and `WHERE` clauses.

### Metadata (MFD)
- `MasterFileDescriptor`: Logical view of a database including segments, fields, and aliases.
- `Segment`: Defines hierarchical relationships (S0, S1, etc.) and keys.
- `FieldDef`: Field attributes: USAGE (format), ACTUAL (storage), and TITLE.
- `AccessFile`: Physical storage mappings for specific adapters.

### Table Engine (FOC-102)
- `TableEngine`: Executes a `TableRequest` by performing retrieval, sorting, and aggregation.
- `CellValue`: Variant type for numeric, character, and date data during processing.
- `ReportOutput`: Structured result containing headers, data rows, and footers.

### Dialogue Manager (FOC-105)
- `DialogueInterpreter`: Manages the execution of FOCEXECs, including variable substitution.
- `AmperVariable`: Local (`&VAR`) and global (`&&VAR`) variable management.
- `DmValue`: Internal value type for Dialogue Manager operations.

### Data Adapters (FOC-107)
- `AdapterRegistry`: Global registry for data source providers.
- `DataAdapter` trait: Common interface for `Db2Adapter`, `VsamAdapter`, and `SequentialAdapter`.

## Feature Coverage

| Feature | Dialect | Status |
|---------|---------|--------|
| PRINT / LIST | TABLE | Implemented |
| SUM / COUNT | TABLE | Implemented |
| BY / ACROSS | TABLE | Implemented |
| DEFINE / COMPUTE | TABLE | Implemented |
| WHERE / IF | TABLE | Implemented |
| JOIN | JOINS | Implemented |
| MATCH FILE | JOINS | Implemented |
| -SET / -IF / -GOTO | DIALOGUE| Implemented |
| -INCLUDE / -RUN | DIALOGUE| Implemented |
| FIXFORM / CRTFORM | MODIFY  | Implemented |
| MATCH / NOMATCH | MODIFY  | Implemented |

## Usage Examples

### Executing a TABLE Request

```rust
use open_mainframe_focus::{TableEngine, MfdParser, TableRequest};
use open_mainframe_focus::output::TextFormatter;

// 1. Parse metadata
let mfd = MfdParser::parse_file("SALES.MAS").unwrap();

// 2. Execute report
let mut engine = TableEngine::new();
let request = TableRequest::parse("TABLE FILE SALES SUM AMOUNT BY REGION END").unwrap();
let result = engine.execute(request, &mfd).unwrap();

// 3. Format output
let mut formatter = TextFormatter::new();
println!("{}", formatter.format(result));
```

### Using Dialogue Manager

```rust
use open_mainframe_focus::dialogue::{DialogueInterpreter, AmperVariable};

let mut interpreter = DialogueInterpreter::new();
interpreter.set_variable("REGION", "NORTH");

let focexec = r#"
-TYPE STARTING REPORT FOR REGION &REGION
TABLE FILE SALES
SUM AMOUNT
WHERE REGION EQ '&REGION'
END
"#;

interpreter.run(focexec).unwrap();
```

## Testing

The crate includes an extensive test suite covering all FOC-nnn stories:
- **Parser**: Dialect switching, complex expressions, and keyword validation.
- **MFD**: Segment hierarchies, multiple field formats (A, I, F, D, P), and ACX mapping.
- **Engine**: Aggregation logic, cross-tabulation (ACROSS), and multi-level sorting (BY).
- **Adapters**: Simulated VSAM and DB2 retrieval.
- **Output**: Text alignment, HTML table generation, and HOLD file creation.

```sh
cargo test -p open-mainframe-focus
```
