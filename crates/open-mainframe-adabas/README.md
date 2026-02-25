# open-mainframe-adabas

A Rust implementation of the ADABAS (Adaptable Database System) inverted-list database engine, providing the core storage model, field definition system, descriptor indexing, direct-call interface, search and read commands, modification commands, transaction management, nucleus orchestration, and standard utilities for the OpenMainframe z/OS clone.

## Overview

ADABAS is Software AG's high-performance, inverted-list database management system that has been a cornerstone of mainframe data management since the 1970s. Unlike relational databases, ADABAS organizes data around compressed records stored in Data Storage, with all access paths maintained through inverted lists in the Associator.

This crate emulates the key ADABAS subsystems: the inverted-list storage engine, the Field Definition Table (FDT) schema system, the descriptor indexing engine (standard, super, sub, phonetic, hyper, and collation descriptors), the ADABAS Control Block (ACB) direct-call interface, search commands with Boolean logic (AND/OR/NOT), read commands (L1-L9), modification commands (N1/N2, A1, E1), full transaction management with ET/BT semantics, the nucleus command processor, and standard utilities (ADALOD, ADAUNI, ADASAV) plus Natural DDM support.

The crate is self-contained with no workspace dependencies beyond error handling (`miette`, `thiserror`), serialization (`serde`, `serde_json`), and logging (`tracing`).

## Architecture

```
                    +-----------------+
                    | AdabasNucleus   |
                    | (orchestrator)  |
                    +--------+--------+
                             |
       +----------+----------+----------+----------+
       |          |          |          |          |
  +----+----+ +---+---+ +---+---+ +----+----+ +---+---+
  |CommandQ | |ProtLog| |CmdLog | |WorkPool | |TxnMgr |
  +---------+ +-------+ +-------+ +---------+ +---+---+
                                                   |
                                              +----+----+
                                              |HoldQueue|
                                              +---------+
       +----------+----------+----------+
       |          |          |          |
  +----+----+ +---+---+ +---+---+ +----+----+
  |  ACB    | | Read  | | Search| | Modify  |
  |Interface| | Cmds  | | Cmds  | | Cmds    |
  +---------+ +-------+ +-------+ +---------+
       |
  +----+----+
  |AdabasFile|---> DataStorage + AssociatorStorage
  +---------+                    (AddressConverter + InvertedLists)
```

### Module Structure

| Module | Description |
|--------|-------------|
| `storage` | Core storage engine: `DataStorage` (RABN-keyed record blocks), `AssociatorStorage` (inverted lists + address converter), `AdabasFile`, `InvertedList`, `AddressConverter` |
| `fdt` | Field Definition Table: `Fdt`, `FieldDef`, `FieldType` (8 types), `FieldOption` (13 options), `GroupField`, `MultipleValueField` |
| `descriptor` | Descriptor engine: `Descriptor`, `SuperDescriptor`, `SubDescriptor`, `PhoneticDescriptor`, `HyperDescriptor`, `CollationDescriptor`, `DescriptorSet` |
| `acb` | Direct Call Interface: `Acb` (ADABAS Control Block), `AcbCommand` (25 command codes), `AcbResult`, `FormatBuffer`, `FieldRef`, `parse_format_buffer` |
| `search` | Search commands: `SearchCommand` (S1/S2/S4/S8/S9), `SearchCriteria`, `SearchOperator` (6 operators), `SearchBuffer` with Boolean evaluation, `Isnlist` |
| `read` | Read commands: `ReadCommand` (L1-L9/LF), `ReadOptions` (hold, prefetch, multifetch, descriptor), `ReadResult`, `ReadCursor` |
| `modify` | Modification commands: `StoreCommand` (N1/N2), `UpdateCommand` (A1), `DeleteCommand` (E1), `UpdateDescriptors` |
| `transaction` | Transaction management: `TransactionManager`, `TransactionLog`, `HoldQueue`, `ClpNumber`, `TransactionState` (Active/Committed/RolledBack) |
| `nucleus` | Nucleus orchestrator: `AdabasNucleus`, `NucleusParams`, `ProtectionLog`, `CommandLog`, `CommandQueue`, `WorkPool` |
| `utilities` | Utility programs: `AdalodUtility` (load), `AdauniUtility` (unload), `AdasavUtility` (save/restore), `Ddm` (Data Definition Module) |

## Key Types and Traits

### Storage Layer
- `Isn` (`u64`) -- Internal Sequence Number, unique record identifier within a file
- `Rabn` (`u64`) -- Relative ADABAS Block Number, physical block address
- `AdabasFile` -- A database file with data storage, associator, and ISN allocator
- `DataStorage` -- HashMap-backed block store keyed by RABN
- `AssociatorStorage` -- Address converter + inverted lists per descriptor
- `AddressConverter` -- ISN-to-RABN mapping
- `InvertedList` -- BTreeMap-backed index mapping descriptor values to sorted ISN lists

### Field System
- `Fdt` -- Field Definition Table (schema) with HashMap index for O(1) lookup
- `FieldDef` -- Individual field: name, level, type, length, options
- `FieldType` -- 8 variants: `Alpha`, `Numeric`, `Packed`, `Unpacked`, `Binary`, `Wide`, `FixedPoint`, `Float`
- `FieldOption` -- 13 variants including `Descriptor`, `Unique`, `NullSuppression`, `MultipleValue`, `PeriodicGroup`, `LongAlpha`, `LargeObject`
- `GroupField` -- Named group of fields, optionally periodic (PE)
- `MultipleValueField` -- MU/PE occurrence container with max-occurrence enforcement

### Descriptor Engine
- `Descriptor` -- Standard descriptor (single-field index), optionally unique
- `SuperDescriptor` -- Composite index from portions of multiple parent fields
- `SubDescriptor` -- Index on a byte substring of a parent field
- `PhoneticDescriptor` -- Soundex-based phonetic matching index
- `HyperDescriptor` -- User-defined index value generation
- `CollationDescriptor` -- Locale-aware sort key generation
- `DescriptorSet` -- Collection of all descriptor types with inverted-list maintenance methods

### ACB Interface
- `AcbCommand` -- 25 command codes: L1-L6, L9, LF, S1/S2/S4/S8/S9, A1, E1, N1/N2, ET, BT, OP, CL, RC, HI, RI, RE
- `Acb` -- ADABAS Control Block with command code, file number, ISN, format/record/search/value/ISN buffers
- `AcbResult` -- Command result with response code, ISN, record buffer, ISN buffer
- `FormatBuffer` / `FieldRef` -- Parsed format buffer specification

### Transaction Management
- `TransactionManager` -- Begin/commit/rollback with hold queue and CLP tracking
- `TransactionLog` -- Before/after image log with insert/update/delete entries
- `HoldQueue` -- Pessimistic record locking with configurable capacity
- `ClpNumber` -- Command-level protection counter per session
- `TransactionState` -- `Active`, `Committed`, `RolledBack`

### Nucleus
- `AdabasNucleus` -- Main orchestrator: manages files, FDTs, descriptors, executes ACB commands
- `NucleusParams` -- Configuration: max files (5000), buffer size (1 MB), max users (100), max holds (1000), command queue size (256)
- `ProtectionLog` -- Write-ahead log with sequence numbers, before/after images
- `CommandLog` -- Audit trail of all executed commands
- `CommandQueue` -- FIFO queue for incoming ACB commands (bounded capacity)
- `WorkPool` -- Named buffer area allocator with size tracking

## Implementation Details

### Storage Model
The storage engine uses an in-memory model that mirrors ADABAS's three-area architecture:
- **Data Storage**: `HashMap<Rabn, Vec<u8>>` stores compressed records keyed by auto-incrementing RABN
- **Associator**: Contains the `AddressConverter` (ISN-to-RABN map) and per-descriptor `InvertedList` instances
- **Address Converter**: `HashMap<Isn, Rabn>` provides O(1) ISN-to-RABN resolution

### Inverted List Engine
`InvertedList` uses a `BTreeMap<String, Vec<Isn>>` where keys are descriptor values and values are sorted ISN vectors. This enables:
- **Exact match** via direct BTreeMap lookup
- **Range search** via `BTreeMap::range` for GE/LE operators
- **Sorted ISN insertion** via `binary_search` + `insert` to maintain sort order
- **Deduplication** by checking binary search before insert

### Search Evaluation
`SearchBuffer::evaluate` implements compound Boolean search:
1. Each criterion produces an ISN list from the inverted list
2. Criteria are combined left-to-right using the specified logical operator
3. Set operations use merge-join algorithms on sorted ISN vectors:
   - `intersect_sorted` -- two-pointer intersection, O(n+m)
   - `union_sorted` -- two-pointer merge union, O(n+m)
   - `subtract_sorted` -- two-pointer difference, O(n+m)

### Descriptor Maintenance
`DescriptorSet::update_inverted_lists` automatically maintains all descriptor types when records are stored:
- Standard descriptors index the raw field value
- Super-descriptors derive composite values via byte-range extraction from multiple fields
- Sub-descriptors extract byte substrings from parent fields
- Phonetic descriptors generate Soundex codes (first letter + 3 consonant digits)
- The same methods run in reverse for `remove_from_inverted_lists` on delete

### Nucleus Command Dispatch
`AdabasNucleus::execute_acb` dispatches ACB commands to internal handlers that operate directly on `AdabasFile` instances. The nucleus manages the full lifecycle: file definition, FDT/descriptor association, store/read/update/delete execution, transaction commit/rollback via the `TransactionManager`, and hold/release via the `HoldQueue`. Response codes follow ADABAS conventions (0 = success, 17 = file not found, 113 = ISN not found, 145 = hold queue overflow).

### Format Buffer Parser
`parse_format_buffer` parses ADABAS format buffer strings (e.g., `"AA,AB,AC."`) into `FieldRef` structs, supporting optional length and format overrides. The parser handles comma-separated two-character field names with a trailing period terminator.

## Syntax / Feature Coverage

### ACB Command Codes

| Command | Description | Status |
|---------|-------------|--------|
| L1 | Read record by ISN | Implemented |
| L2 | Read physical sequential | Implemented (type defined) |
| L3 | Read logical sequential by descriptor | Implemented (type defined) |
| L4 | Read by ISN from ISN list | Implemented (type defined) |
| L5 | Read physical sequential (alt) | Implemented (type defined) |
| L6 | Read sorted by descriptor | Implemented (type defined) |
| L9 | Read histogram | Implemented (type defined) |
| LF | Read Field Definition Table | Implemented (stub response) |
| S1 | Find first (search) | Implemented (type + evaluation) |
| S2 | Find next (continuation) | Implemented (type defined) |
| S4 | Find and sort | Implemented (type defined) |
| S8 | Multi-criteria search | Implemented (type defined) |
| S9 | Sort ISN list | Implemented (type defined) |
| N1 | Store record (system ISN) | Implemented |
| N2 | Store record (user ISN) | Implemented |
| A1 | Update record | Implemented |
| E1 | Delete record | Implemented |
| ET | End transaction (commit) | Implemented |
| BT | Backout transaction (rollback) | Implemented |
| OP | Open session | Implemented (stub) |
| CL | Close session | Implemented (stub) |
| HI | Hold ISN (explicit lock) | Implemented |
| RI | Release ISN | Implemented |
| RC | Release command ID | Implemented (stub) |
| RE | Read ET data | Implemented (stub) |

### Field Types

| Type | Code | Status |
|------|------|--------|
| Alphanumeric | A | Implemented |
| Numeric (unpacked) | N | Implemented |
| Packed decimal | P | Implemented |
| Unpacked decimal | U | Implemented |
| Binary | B | Implemented |
| Wide (Unicode) | W | Implemented |
| Fixed-point | F | Implemented |
| Floating-point | G | Implemented |

### Descriptor Types

| Type | Status |
|------|--------|
| Standard descriptor | Implemented |
| Super-descriptor | Implemented |
| Sub-descriptor | Implemented |
| Phonetic descriptor | Implemented (Soundex) |
| Hyper-descriptor | Implemented (default: concatenation) |
| Collation descriptor | Implemented (simplified: uppercase) |

### Search Operators

| Operator | Status |
|----------|--------|
| EQ (equal) | Implemented |
| GT (greater than) | Implemented |
| LT (less than) | Implemented |
| GE (greater or equal) | Implemented |
| LE (less or equal) | Implemented |
| NE (not equal) | Implemented |

### Logical Connectors

| Connector | Status |
|-----------|--------|
| AND | Implemented |
| OR | Implemented |
| NOT | Implemented |

### Utilities

| Utility | Description | Status |
|---------|-------------|--------|
| ADALOD | Load data (Initial / Mass Update) | Implemented |
| ADAUNI | Unload data (all / by ISN list) | Implemented |
| ADASAV | Save / Restore backup images | Implemented |
| DDM | Data Definition Module (Natural interface) | Implemented |

## Usage Examples

```rust
use open_mainframe_adabas::*;
use open_mainframe_adabas::fdt::{Fdt, FieldDef, FieldType};

// Create and start the nucleus
let mut nucleus = AdabasNucleus::new(NucleusParams::new());
nucleus.start();

// Define a file with an FDT
let mut fdt = Fdt::new();
fdt.add_field(FieldDef::new("AA", 1, FieldType::Alpha, 20).with_descriptor()).unwrap();
fdt.add_field(FieldDef::new("AB", 1, FieldType::Alpha, 30)).unwrap();

let mut descriptors = DescriptorSet::new();
descriptors.descriptors.push(Descriptor::new("AA"));

nucleus.define_file(1, "EMPLOYEES", fdt, descriptors).unwrap();

// Store a record via ACB
let store_acb = Acb::new(AcbCommand::N1, 1)
    .with_record_buffer(b"SMITH".to_vec());
let result = nucleus.execute_acb(&store_acb);
assert_eq!(result.response_code, 0);
let isn = result.isn;

// Read the record back
let read_acb = Acb::new(AcbCommand::L1, 1).with_isn(isn);
let result = nucleus.execute_acb(&read_acb);
assert_eq!(result.record_buffer, b"SMITH");

// Update the record
let update_acb = Acb::new(AcbCommand::A1, 1)
    .with_isn(isn)
    .with_record_buffer(b"JONES".to_vec());
nucleus.execute_acb(&update_acb);

// Search using inverted lists
let criteria = SearchCriteria::new("AA", SearchOperator::Eq, "JONES");
let search_buf = SearchBuffer::new(criteria);

// Use ADALOD for bulk loading
let mut file = storage::AdabasFile::new(2, "BULK");
let mut loader = AdalodUtility::new(2, LoadMode::Initial);
loader.add_record(b"record1".to_vec());
loader.add_record(b"record2".to_vec());
let isns = loader.execute(&mut file).unwrap();

// Save and restore with ADASAV
let saver = AdasavUtility::new(SaveOperation::Save);
let backup = saver.save_file(&file);
let restorer = AdasavUtility::new(SaveOperation::Restore);
let restored_file = restorer.restore_file(&backup).unwrap();
```

## Dependencies

| Dependency | Purpose |
|------------|---------|
| `miette` | Diagnostic error reporting with `#[derive(Diagnostic)]` |
| `thiserror` | Ergonomic error type derivation for `AdabasError` |
| `serde` | Serialization support for data structures |
| `serde_json` | JSON serialization for backup images and DDM export |
| `tracing` | Structured logging for nucleus operations |

## Testing

Run the full test suite:

```sh
cargo test -p open-mainframe-adabas
```

Every module contains comprehensive inline `#[cfg(test)]` test blocks covering:

- **storage**: File CRUD, data storage round-trips, address converter operations, inverted list insert/remove/range/dedup, associator search
- **fdt**: Field definition creation and builders, FDT add/lookup/duplicate detection, group fields, multiple-value occurrence limits, field type display
- **descriptor**: Standard/unique descriptors, super-descriptor derivation, sub-descriptor extraction, phonetic Soundex codes (Smith/Smythe equivalence), hyper-descriptor generation, descriptor set inverted list maintenance
- **acb**: Command code parsing (case-insensitive), format buffer parsing (simple/empty/single), ACB construction with builder pattern, AcbResult success/error/ISN buffer
- **search**: Operator parsing and display, criteria matching, ISN list creation/dedup/sequential access/contains/intersect/union, search buffer evaluation with AND/OR/NOT
- **read**: Command code parsing, option builders, result field access, EOF detection, cursor sequential iteration and reset
- **modify**: N1/N2 store with ISN allocation, store with descriptor index updates, A1 update with index maintenance, E1 delete, UpdateDescriptors helpers
- **transaction**: Begin/commit/rollback lifecycle, log entry types (insert/update/delete), rollback ordering, hold queue capacity/release, CLP tracking
- **nucleus**: Start/stop, file definition, store-and-read via ACB, update via ACB, delete via ACB, file-not-found response codes, command queue operations, protection log write/inactive/clear, work pool allocate/release, OP/CL commands
- **utilities**: ADALOD initial/mass-update modes, ADAUNI unload all/specific, ADASAV save/restore round-trip, DDM creation/resolution/from_fdt

No external test fixtures are required; all tests use in-memory data.

## Limitations and Future Work

- **Read commands L2-L6, L9**: Type definitions and `ReadCursor` exist, but the nucleus `execute_acb` only dispatches L1 (read by ISN). Sequential and sorted read execution through the nucleus is not yet wired up.
- **Search command execution through nucleus**: S1-S9 search types are defined and `SearchBuffer::evaluate` works against inverted lists directly, but the nucleus does not yet dispatch search commands via `execute_acb`.
- **Format buffer field-level I/O**: The format buffer parser exists but the nucleus does not use it to extract/inject individual field values from/to the record buffer; records are currently treated as opaque byte vectors.
- **Phonetic descriptor**: Uses a simplified Soundex algorithm; a full ADABAS phonetic algorithm may differ in edge cases.
- **Hyper-descriptor**: Default implementation concatenates parent field values; real ADABAS allows user-supplied exit routines.
- **Collation descriptor**: Uses simple uppercase conversion rather than locale-specific collation sequences.
- **Multi-user concurrency**: The nucleus is single-threaded; CLP numbers and hold queues are modeled but no actual concurrent access control exists.
- **Persistent storage**: All data is in-memory; there is no disk-based block I/O or recovery from the protection log.
- **ADABAS response codes**: Only a subset of response codes are emitted (0, 3, 9, 17, 22, 113, 145); a full implementation would cover the complete response code catalog.
- **Compression**: Data Storage does not perform actual ADABAS record compression (null suppression, blank compression).
