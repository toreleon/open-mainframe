# open-mainframe-cics

A comprehensive Rust implementation of IBM CICS (Customer Information Control System), providing the full transaction-processing runtime: EXEC CICS command preprocessing, program control (LINK/XCTL/RETURN), BMS screen mapping with 3270 data stream rendering, file control, temporary storage and transient data queues, channel/container inter-program data passing, interval control, document generation, terminal I/O with page building, web services with REST/JSON support, system programming (INQUIRE/SET), ENQ/DEQ resource locking, storage management, and a command dispatcher bridging preprocessed COBOL to the runtime.

## Overview

CICS is the dominant online transaction processing (OLTP) system on IBM mainframes, managing thousands of concurrent terminal users executing business transactions. This crate reimplements the core CICS services in Rust for the OpenMainframe z/OS clone.

The architecture mirrors real CICS: a COBOL program containing `EXEC CICS ... END-EXEC` blocks is first processed by the `CicsPreprocessor`, which replaces those blocks with `CALL` statements referencing a command parameter block. At runtime, the `CicsDispatcher` routes those calls to the `CicsRuntime`, which provides the actual CICS services — program management, file I/O, queue operations, terminal control, and more. Data passes between programs via the legacy COMMAREA (up to 32KB) or modern Channel/Container mechanism (unlimited size).

The BMS subsystem is fully functional: BMS map source macros (DFHMSD/DFHMDI/DFHMDF) are parsed into structured map definitions, rendered to 3270 data streams with SBA/SF/SFE orders, and can generate COBOL symbolic copybooks. The terminal manager supports multi-terminal sessions, SEND MAP ACCUM page building, and CONVERSE (combined send/receive).

## Architecture

```
    COBOL Source                         Runtime Execution
    ┌─────────────┐                      ┌──────────────────┐
    │ EXEC CICS   │    Preprocessing     │   CicsRuntime    │
    │ LINK ...    │ ──────────────────>  │                  │
    │ END-EXEC    │    CicsPreprocessor  │ ┌──────────────┐ │
    └─────────────┘    CicsScanner       │ │ProgramControl│ │
           │                             │ │LINK/XCTL/RTN │ │
           ▼                             │ └──────────────┘ │
    ┌─────────────┐    Dispatch          │ ┌──────────────┐ │
    │ CALL        │ ──────────────────>  │ │ FileControl   │ │
    │ "CICSLINK"  │    CicsDispatcher    │ │READ/WRITE/DEL│ │
    │ USING ...   │                      │ └──────────────┘ │
    └─────────────┘                      │ ┌──────────────┐ │
                                         │ │ Terminal I/O  │ │
                                         │ │SEND/RECV MAP │ │
                                         │ └──────────────┘ │
    ┌───────────────────────────┐        │ ┌──────────────┐ │
    │        BMS Subsystem      │        │ │ Queue Svcs    │ │
    │ Parser → Map → Renderer   │ <───>  │ │ TS / TD      │ │
    │ SymbolicMapGenerator      │        │ └──────────────┘ │
    │ FROM/INTO buffer decompose│        │ ┌──────────────┐ │
    └───────────────────────────┘        │ │ Channels      │ │
                                         │ │ PUT/GET/MOVE  │ │
    ┌───────────────────────────┐        │ └──────────────┘ │
    │      Support Services     │        │ ┌──────────────┐ │
    │ Document / Interval       │        │ │ Web Services  │ │
    │ SPI / Web / Sysid / ENQ   │        │ │ REST / JSON   │ │
    └───────────────────────────┘        │ └──────────────┘ │
                                         └──────────────────┘
```

### Module Structure

| Module | Description |
|--------|-------------|
| `lib` | Crate root: `CicsError`, `CicsResponse` (50+ CICS response codes), `CicsResult<T>` |
| `bms::parser` | BMS macro parser: `BmsParser`, `BmsMapset`, `BmsMap` from DFHMSD/DFHMDI/DFHMDF source |
| `bms::field` | BMS field definitions: `BmsField`, `FieldAttribute`, `FieldJustify`, `FieldType` |
| `bms::render` | 3270 data stream renderer: `MapRenderer`, `Wcc`, SBA/SF/SFE orders, EBCDIC translation |
| `bms::symbolic` | COBOL copybook generator: `SymbolicMapGenerator`, `decompose_from_buffer`, `compose_to_display_string` |
| `bms` (mod) | BMS core types: `ScreenSize` (Model2-5), `AttributeByte`, `FieldColor` (8), `FieldHighlight` (3), `ExtendedAttribute` |
| `channels` | Channel/Container: `Channel`, `Container`, `ChannelManager` — modern data passing (>32KB) |
| `document` | Document services: `Document`, `DocumentManager`, template registration, symbol substitution, insert/retrieve/delete |
| `interval` | Interval control: `IntervalManager`, `ScheduledTransaction`, START/CANCEL/DELAY/RETRIEVE |
| `preprocess` | EXEC CICS preprocessor: `CicsPreprocessor`, `CicsScanner`, `CicsBlock`, `CicsCommandType` (37 types), `CicsOption` |
| `queues::ts` | Temporary Storage: `TsQueue`, `TsQueueManager`, `TsItem`, `TsType` (Main/Auxiliary), disk persistence |
| `queues::td` | Transient Data: `TdQueue`, `TdQueueManager`, `DctEntry`, trigger mechanism, extrapartition flush |
| `runtime` (mod) | Transaction context: `Commarea`, `ConditionHandler`, `TransactionContext` |
| `runtime::commands` | Command execution engine: `CicsRuntime`, `ProgramRegistry`, `ProgramResult`, all CICS operations |
| `runtime::dispatcher` | CALL-to-runtime bridge: `CicsDispatcher`, `CommandParamBlock`, `DispatchResult`, routes 30+ commands |
| `terminal` | Terminal I/O: `Terminal`, `TerminalManager`, SEND/RECEIVE MAP, SEND TEXT, CONVERSE, ACCUM page building |
| `syspr` | System Programming Interface: `SystemProgrammingInterface`, INQUIRE/SET for programs, transactions, files |
| `web` | Web services: `WebClient`, `WebSession`, `UriRouter`, `UriMap`, `Pipeline`, JSON transform, REST support |

## Key Types and Traits

### Error Handling
- `CicsError` — Top-level error: SyntaxError, UnknownCommand, ProgramNotFound, FileNotFound, NotAuthorized, InvalidRequest, etc.
- `CicsResponse` — 50+ CICS condition codes: Normal, Error, Notfnd, Duprec, Pgmiderr, Invreq, Ioerr, Lengerr, Endfile, Itemerr, Qiderr, etc.
- `CicsResult<T>` — `Result<T, CicsError>` alias

### Program Control
- `ProgramResult` — Return / ReturnTransid / ReturnCommarea / ReturnChannel / Xctl / Abend
- `ProgramEntry` — `Box<dyn Fn(&mut CicsRuntime) -> CicsResult<ProgramResult>>`
- `ProgramRegistry` — Program lookup and registration by name
- `CicsRuntime` — Central execution environment: EIB, context, files, queues, terminal, channels, storage

### BMS
- `BmsParser` — Parses DFHMSD/DFHMDI/DFHMDF macros into `BmsMapset` / `BmsMap` / `BmsField`
- `BmsField` — Field with name, row, column, length, attributes (protection, numeric, bright/dark, color, highlight, initial cursor, PICIN/PICOUT)
- `MapRenderer` — Generates 3270 data streams: SBA address encoding (12-bit/14-bit), SF/SFE orders, WCC, EBCDIC translation via CP037
- `SymbolicMapGenerator` — Generates COBOL copybooks (input/output maps, TIOAPFX, L/F/A/C/data fields, DFHBMSCA constants)
- `ScreenSize` — Model2 (24x80), Model3 (32x80), Model4 (43x80), Model5 (27x132)

### Queues
- `TsQueue` / `TsQueueManager` — WRITEQ TS (append/rewrite), READQ TS, READQ TS NEXT, DELETEQ TS, auxiliary disk persistence
- `TdQueue` / `TdQueueManager` — WRITEQ TD, READQ TD (destructive FIFO), DELETEQ TD, DCT-based triggers, extrapartition file backing

### Terminal
- `Terminal` — Screen buffer, input data, screen size
- `TerminalManager` — Multi-terminal registry, SEND MAP (with FROM buffer decomposition), RECEIVE MAP, SEND MAP ACCUM page building, SEND PAGE, CONVERSE, PURGE MESSAGE
- `SendMapOptions` — Erase, eraseaup, maponly, dataonly, cursor, freekb, alarm, frset, accum

### Web
- `WebClient` — HTTP session management (OPEN/CONVERSE/CLOSE)
- `WebRequest` / `WebResponse` — HTTP request/response with headers, body, query params
- `UriRouter` / `UriMap` — URI pattern matching with wildcards for request routing
- `Pipeline` / `PipelineStep` — SOAP/REST processing pipeline

### System Programming
- `SystemProgrammingInterface` — INQUIRE/SET for programs, transactions, files, system settings
- `ProgramDef`, `TransactionDef`, `FileDef` — Resource definitions with status, language, properties

## Implementation Details

### EXEC CICS Preprocessing
The `CicsScanner` identifies `EXEC CICS ... END-EXEC` blocks in standard COBOL format (columns 8-72), respecting comment lines (column 7 = `*`). Multi-line blocks are joined with whitespace normalization. The `CicsPreprocessor` processes blocks in reverse order (to preserve line numbers), classifies each via `CicsCommandType::from_text()` into 37 command types, parses options with a state machine (tracking parenthesis nesting), and generates `CALL "CICSxxxx" USING CICS-CMD-nnn DFHEIBLK` replacements.

### BMS Map Parsing
`BmsParser` handles the three BMS macro types: DFHMSD (mapset), DFHMDI (map with SIZE), and DFHMDF (field with POS, LENGTH, ATTRB, INITIAL, COLOR, HILIGHT, PICIN, PICOUT, JUSTIFY). The parser supports continuation lines (non-blank column 72 or leading whitespace continuation), multi-value ATTRB lists (PROT, UNPROT, NUM, BRT, DRK, ASKIP, IC, FSET), and all 8 extended colors plus 3 highlight types.

### 3270 Data Stream Rendering
`MapRenderer::render()` generates a byte stream starting with ERASE_WRITE or WRITE command + WCC, then for each field: SBA (Set Buffer Address) with 12-bit or 14-bit encoding, SF (Start Field) for basic attributes or SFE (Start Field Extended) when color/highlight is present with attribute pairs, field data translated to EBCDIC via `open-mainframe-encoding::CP037`, null-padded to field length, and IC (Insert Cursor) if the field has initial cursor. The `render_text()` method produces an ASCII screen representation for debugging.

### Symbolic Map Buffer Layout
The symbolic map layout follows CICS conventions: 12-byte TIOAPFX, then per named field: 2-byte L (length, S9(4) COMP), 1-byte F (flag), 1-byte A (attribute), 1-byte C (color), then data (PIC X(n)). `decompose_from_buffer()` and `decompose_from_display_string()` parse incoming FROM data; `compose_to_display_string()` builds INTO data for RECEIVE MAP.

### Channel/Container Data Passing
Channels replace COMMAREA for new applications, supporting unlimited data size. `ChannelManager` maintains a `HashMap<String, Channel>`, each channel holding a `HashMap<String, Container>`. All names are case-insensitive (uppercased). Channels flow with LINK/XCTL via `link_with_channel()` and `xctl_with_channel()`. The current channel is tracked per transaction.

### Queue Services
- **TS Queues**: In-memory (`Main`) or disk-backed (`Auxiliary`). Items are numbered 1..N. WRITEQ with existing item_number performs rewrite; without one it appends. Auxiliary queues serialize to JSON and persist to a storage directory.
- **TD Queues**: FIFO destructive reads. DCT entries define trigger levels — when record count reaches the threshold, `get_pending_triggers()` returns transaction IDs to start. Extrapartition queues support `flush_to_file()` for file backing.

### Command Dispatcher
`CicsDispatcher::dispatch()` maps CALL names (CICSLINK, CICSREAD, CICSSMAP, etc.) to `CicsRuntime` method calls. It deserializes `CommandParamBlock` fields, invokes the runtime, and returns `DispatchResult` with EIBRESP/EIBRESP2 and any output data. Supports 30+ distinct CICS commands.

## Syntax / Feature Coverage

### EXEC CICS Commands

| Command | Category | Status |
|---------|----------|--------|
| LINK PROGRAM | Program Control | Implemented (with COMMAREA and CHANNEL) |
| XCTL PROGRAM | Program Control | Implemented (with COMMAREA and CHANNEL) |
| RETURN | Program Control | Implemented (with TRANSID, COMMAREA, CHANNEL) |
| READ FILE | File Control | Implemented |
| WRITE FILE | File Control | Implemented |
| REWRITE FILE | File Control | Implemented |
| DELETE FILE | File Control | Implemented |
| SEND MAP | Terminal (BMS) | Implemented (ERASE, MAPONLY, DATAONLY, FROM, ACCUM, ALARM, FREEKB, FRSET, CURSOR) |
| RECEIVE MAP | Terminal (BMS) | Implemented (INTO buffer composition) |
| SEND TEXT | Terminal | Implemented |
| SEND PAGE | Terminal | Implemented (ACCUM page delivery) |
| RECEIVE | Terminal | Implemented |
| CONVERSE | Terminal | Implemented (combined SEND+RECEIVE) |
| PURGE MESSAGE | Terminal | Implemented |
| WRITEQ TS | Temp Storage | Implemented (append, rewrite, auxiliary) |
| READQ TS | Temp Storage | Implemented (by item number) |
| READQ TS NEXT | Temp Storage | Implemented (sequential browse) |
| DELETEQ TS | Temp Storage | Implemented |
| WRITEQ TD | Transient Data | Implemented (with trigger support) |
| READQ TD | Transient Data | Implemented (destructive FIFO) |
| DELETEQ TD | Transient Data | Implemented |
| PUT CONTAINER | Channels | Implemented |
| GET CONTAINER | Channels | Implemented |
| MOVE CONTAINER | Channels | Implemented |
| DELETE CONTAINER | Channels | Implemented |
| GETMAIN | Storage | Implemented |
| FREEMAIN | Storage | Implemented |
| HANDLE CONDITION | Exception | Implemented |
| HANDLE ABEND | Exception | Implemented |
| HANDLE AID | Exception | Implemented (type defined) |
| IGNORE CONDITION | Exception | Implemented |
| START | Interval Control | Implemented (INTERVAL, TIME, DATA) |
| RETRIEVE | Interval Control | Implemented |
| CANCEL | Interval Control | Implemented |
| DELAY | Interval Control | Implemented |
| ASKTIME | Time | Implemented (type defined) |
| FORMATTIME | Time | Implemented (type defined) |
| ASSIGN | System | Implemented (SYSID, APPLID, USERID, OPID, FACILITY, NETNAME, STARTCODE, SCRNHT, SCRNWD, CWALENG, EIBCALEN) |
| ADDRESS | System | Implemented (type defined) |
| ENQ | Synchronization | Implemented |
| DEQ | Synchronization | Implemented |
| SYNCPOINT | Synchronization | Implemented (type defined) |
| ABEND | Exception | Implemented |
| DOCUMENT CREATE | Document | Implemented (template + inline) |
| DOCUMENT INSERT | Document | Implemented (before/after) |
| DOCUMENT SET | Document | Implemented (symbol substitution) |
| DOCUMENT RETRIEVE | Document | Implemented |
| DOCUMENT DELETE | Document | Implemented |
| INQUIRE PROGRAM | SPI | Implemented |
| SET PROGRAM | SPI | Implemented (status, NEWCOPY) |
| INQUIRE TRANSACTION | SPI | Implemented |
| SET TRANSACTION | SPI | Implemented |
| INQUIRE FILE | SPI | Implemented |
| SET FILE | SPI | Implemented (status, open status) |
| INQUIRE SYSTEM | SPI | Implemented |
| WEB OPEN | Web | Implemented |
| WEB CONVERSE | Web | Implemented |
| WEB CLOSE | Web | Implemented |

### BMS Macro Support

| Macro / Keyword | Status |
|----------------|--------|
| DFHMSD TYPE=MAP/FINAL | Implemented |
| DFHMDI SIZE=(r,c) | Implemented |
| DFHMDF POS=(r,c) | Implemented |
| LENGTH | Implemented |
| ATTRB (PROT, UNPROT, NUM, BRT, DRK, ASKIP, IC, FSET) | Implemented |
| INITIAL | Implemented |
| COLOR (BLUE, RED, PINK, GREEN, TURQUOISE, YELLOW, WHITE, NEUTRAL) | Implemented |
| HILIGHT (BLINK, REVERSE, UNDERLINE) | Implemented |
| PICIN / PICOUT | Implemented |
| JUSTIFY (LEFT, RIGHT, ZERO) | Implemented |
| Continuation lines | Implemented |
| SFE extended attribute rendering | Implemented |
| Symbolic map generation (I/O maps, DFHBMSCA) | Implemented |

### 3270 Data Stream Orders

| Order | Status |
|-------|--------|
| SBA (Set Buffer Address) | Implemented (12-bit + 14-bit) |
| SF (Start Field) | Implemented |
| SFE (Start Field Extended) | Implemented (color + highlight) |
| IC (Insert Cursor) | Implemented |
| WRITE / ERASE_WRITE commands | Implemented |
| WCC (Write Control Character) | Implemented (Reset MDT, Alarm, Keyboard Restore) |

## Usage Examples

```rust
use open_mainframe_cics::preprocess::CicsPreprocessor;
use open_mainframe_cics::runtime::commands::{CicsRuntime, ProgramRegistry, ProgramResult};
use open_mainframe_cics::bms::parser::BmsParser;
use open_mainframe_cics::bms::render::MapRenderer;
use open_mainframe_cics::bms::{ScreenSize, SymbolicMapGenerator};
use open_mainframe_cics::channels::ChannelManager;
use open_mainframe_cics::queues::{TsQueueManager, TdQueueManager};

// --- Preprocess COBOL source ---
let mut preprocessor = CicsPreprocessor::new();
let result = preprocessor.process(cobol_source).unwrap();
// result.cobol_source has CALL replacements
// result.commands has extracted CICS commands

// --- Runtime execution ---
let mut runtime = CicsRuntime::new("TXN1");
runtime.register_program("SUBPROG", |rt| {
    // Program logic here
    Ok(ProgramResult::Return)
});
runtime.link("SUBPROG", None)?;  // LINK with no COMMAREA

// --- LINK with Channel ---
runtime.put_container("MY-CH", "DATA1", b"payload")?;
runtime.link_with_channel("SUBPROG", "MY-CH")?;
let data = runtime.get_container("MY-CH", "DATA1")?;

// --- BMS map parsing and rendering ---
let mut parser = BmsParser::new();
let mapset = parser.parse(bms_source).unwrap();
let map = &mapset.maps[0];

let mut renderer = MapRenderer::new(ScreenSize::Model2);
renderer.set_field_string("CUSTNO", "12345");
let stream = renderer.render(map, true);  // 3270 byte stream

// Generate COBOL copybook
let generator = SymbolicMapGenerator::new();
let copybook = generator.generate(&mapset);

// --- TS Queue operations ---
let mut ts = TsQueueManager::new();
ts.writeq("MYQUEUE", b"item data", None, false)?;
let item = ts.readq("MYQUEUE", 1)?;
let next = ts.readq_next("MYQUEUE")?;

// --- TD Queue with trigger ---
let mut td = TdQueueManager::new();
td.define_queue("LOGQ", TdDestType::Intrapartition,
    Some(DctEntry { trigger_level: 100, trigger_transid: "LOGP".into(), .. }));
td.writeq("LOGQ", b"log record")?;
let triggers = td.get_pending_triggers();  // Returns ["LOGP"] when 100 reached
```

## Dependencies

| Dependency | Purpose |
|------------|---------|
| `open-mainframe-encoding` | EBCDIC/ASCII translation (CP037) for 3270 data streams |
| `miette` | Diagnostic error reporting |
| `thiserror` | Ergonomic error type derivation |
| `serde` / `serde_json` | Serialization for TS queue persistence and web JSON transforms |
| `tracing` | Structured logging throughout the runtime |

## Testing

Run the full test suite:

```sh
cargo test -p open-mainframe-cics
```

The crate has extensive inline `#[cfg(test)]` test blocks totaling 200+ tests across all modules:

- **bms::parser**: Mapset/map/field parsing, attributes, COLOR/HILIGHT, continuation lines, POS extraction, PICIN/PICOUT, JUSTIFY
- **bms::field**: Field construction, attribute builders, initial values, buffer position calculation
- **bms::render**: WCC encoding, 14-bit address encoding, EBCDIC translation (full printable ASCII round-trip), SF/SFE rendering, mixed field rendering, attribute encoding
- **bms::symbolic**: Copybook generation, PIC clause generation, DFHBMSCA constants, map size calculation, `decompose_from_buffer`, `decompose_from_display_string`, `compose_to_display_string` round-trips
- **channels**: Container CRUD, case-insensitivity, MOVE CONTAINER, large data (>32KB), ChannelManager operations
- **document**: Template registration, CREATE/INSERT/SET/RETRIEVE/DELETE, symbol substitution, full lifecycle
- **interval**: HHMMSS parsing, START INTERVAL, CANCEL, ready transaction detection, RETRIEVE data, DELAY
- **preprocess**: Command type detection (37 types), option parsing, CALL generation, multi-command source processing, container/TD/CONVERSE commands
- **queues::ts**: Write/read/rewrite, auxiliary persistence, READQ NEXT sequential browse, DELETEQ, corruption handling
- **queues::td**: DCT configuration, WRITEQ/READQ (FIFO), trigger mechanism, extrapartition flush, disabled queue handling
- **runtime**: COMMAREA operations, TransactionContext builders, condition handler registration
- **runtime::commands**: LINK/XCTL/RETURN (60+ tests), container passing, TD queues, SEND/RECEIVE, CONVERSE, ASSIGN fields, GETMAIN/FREEMAIN, HANDLE CONDITION/ABEND, ABEND command
- **runtime::dispatcher**: CommandParamBlock serialization, dispatch routing for all 30+ commands
- **terminal**: SEND MAP/RECEIVE MAP, page building (ACCUM/SEND PAGE), CONVERSE, screen sizes, multi-terminal sessions
- **syspr**: INQUIRE/SET for programs, transactions, files, system settings, NEWCOPY
- **web**: HTTP sessions, CONVERSE, URI routing with wildcards, JSON transforms, pipelines

## Limitations and Future Work

- **File Control**: File I/O (READ/WRITE/REWRITE/DELETE FILE) is dispatched through the runtime but uses in-memory storage rather than the `open-mainframe-dataset` VSAM engine. Integration with actual VSAM datasets would provide realistic file behavior.
- **HANDLE AID**: The preprocessor recognizes HANDLE AID but the runtime does not yet dispatch attention key (PF key) handling to labeled paragraphs.
- **ASKTIME / FORMATTIME**: Command types are defined but runtime execution stubs return default values.
- **SYNCPOINT**: Type defined but not wired to actual transaction commit/rollback logic.
- **Interval Control TIME mode**: `start_at_time()` treats the time value as a relative offset rather than an absolute wall-clock time.
- **BMS SEND MAP DATAONLY**: Recognized in `SendMapOptions` but the renderer does not suppress attribute bytes for data-only updates.
- **Security**: No RACF integration for transaction/resource authorization checks.
- **Multi-threading**: The runtime is single-threaded; real CICS supports concurrent tasks with quasi-reentrant programs.
- **Web Services**: `WebClient` manages sessions but does not perform actual HTTP requests — responses must be simulated. Integration with an HTTP library would enable real outbound calls.
- **URIMAP**: Pattern matching supports wildcards but not full CICS URIMAP template syntax.
- **EIB fields**: EIBRESP and EIBRESP2 are set by the dispatcher; other EIB fields (EIBDATE, EIBTIME, EIBTASKN, EIBTRNID) are populated partially.
