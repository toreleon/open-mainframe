# open-mainframe-sort

DFSORT (Data Facility Sort) — a high-performance Rust implementation of the mainframe's premier sort and data manipulation utility for the OpenMainframe project.

## Overview

DFSORT is the industry standard for high-volume sorting and merging on IBM mainframes. This crate reimplements the core DFSORT engine, providing support for complex multi-key sorts, record filtering (INCLUDE/OMIT), field reformatting (INREC/OUTREC), and the advanced ICETOOL reporting facility.

## Architecture

```
    Input Datasets                        Sort & Merge Engine
    ┌──────────────┐                      ┌────────────────────┐
    │  Unsorted    │    Reading           │    Sort Engine     │
    │  Records     │ ──────────────────>  │    (Multi-Key)     │
    └──────────────┘    SortEngine        │  BI, CH, PD, ZD    │
           │                               └────────────────────┘
           ▼                                        │
    ┌──────────────┐    Processing        ┌────────────────────┐
    │  Filtering   │ ──────────────────>  │   Reformatting     │
    │  INCLUDE/OMIT│    FilterSpec        │   INREC/OUTREC     │
    └──────────────┘                      │   OUTFIL           │
                                          └────────────────────┘
           │                                        │
           ▼                                        ▼
    ┌──────────────┐    Reporting         ┌────────────────────┐
    │  Final Report│ <──────────────────  │    ICETOOL         │
    │  (SYSOUT)    │    IceToolOp         │    DISPLAY, STATS  │
    └──────────────┘                      └────────────────────┘
```

### Module Structure

| Module | Description |
|--------|-------------|
| `engine` | Core sort and merge engine supporting stable and unstable sorts |
| `parser` | Parser for DFSORT control statements (SORT, MERGE, INCLUDE, etc.) |
| `fields` | Data type implementation for CH, ZD, PD, BI, FI, and more |
| `filter` | Record filtering engine: INCLUDE, OMIT, and conditional logic |
| `reformat`| Field reformatting: INREC, OUTREC, and OVERLAY processing |
| `outfil` | Advanced output processing: Multiple output files, headers, and trailers |
| `joinkeys`| Implementation of JOINKEYS for multi-dataset join operations |
| `icetool` | High-level data manipulation facility: DISPLAY, STATS, OCCUR, COUNT |
| `symbols` | Support for DFSORT symbols (SYMNAMES) in control statements |

## Key Types and Components

### Sort Specification
- `SortSpec`: Defines the keys, order (ASC/DESC), and format for a sort operation.
- `DataType`: Enumeration of supported mainframe data formats (e.g., Packed Decimal).

### ICETOOL
- `IceToolOp`: Represents an ICETOOL operator command (e.g., `DISPLAY`).
- `FieldStats`: Captures statistics (MIN, MAX, AVG) for specific fields.

### Processing
- `FilterSpec`: Encapsulates the logic for `INCLUDE` and `OMIT` conditions.
- `OutrecSpec`: Defines the new layout for records in the `OUTREC` phase.

## Feature Coverage

| Feature | Category | Status |
|---------|----------|--------|
| Multi-key Sort  | Engine   | Implemented (CH, ZD, PD, BI, FI) |
| MERGE / COPY    | Engine   | Implemented |
| INCLUDE / OMIT  | Filtering| Implemented (Complex conditions) |
| INREC / OUTREC  | Reformat | Implemented (Overlay, lookup) |
| OUTFIL          | Output   | Implemented (Multiple outputs) |
| JOINKEYS        | Joins    | Implemented (F1/F2, inner/outer) |
| ICETOOL         | Reporting| Implemented (10+ operators) |

## Usage Examples

### Executing a SORT Operation

```rust
use open_mainframe_sort::{SortEngine, SortSpec, SortField, DataType, SortOrder};

let mut spec = SortSpec::new();
spec.add_field(SortField::new(1, 10, DataType::Character, SortOrder::Ascending));

let mut engine = SortEngine::new(spec);
engine.sort_file("INPUT.DAT", "OUTPUT.DAT").unwrap();
```

### Using ICETOOL to Generate a Report

```rust
use open_mainframe_sort::icetool::{IceToolOp, OnField};

let mut op = IceToolOp::new_display("SYSLIST");
op.add_on_field(OnField::new(1, 20, "NAME"));
op.add_on_field(OnField::new(21, 5, "DEPT"));

// Run ICETOOL logic...
```

## Testing

The Sort crate is rigorously tested for data correctness (400+ tests):
- **Engine**: Validates sort stability and multi-key precedence across large datasets.
- **Data Types**: Verification of Packed Decimal (PD) and Zoned Decimal (ZD) comparison logic.
- **Joins**: Tests for inner, left, and full outer joins using JOINKEYS.
- **ICETOOL**: Ensures parity with IBM ICETOOL for statistical and display output.

```sh
cargo test -p open-mainframe-sort
```
