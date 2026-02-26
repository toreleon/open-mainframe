# open-mainframe-smf

SMF (System Management Facilities) — a high-performance Rust implementation of the mainframe's premier auditing and performance monitoring subsystem for the OpenMainframe project.

## Overview

SMF is the central mechanism in z/OS for collecting and recording system and application-level activity. This crate reimplements the core SMF components, including the SMF Writer (SMFWTM), record formatting, exit processing, and an observability bridge for modern monitoring tools like Prometheus and OpenTelemetry.

## Architecture

```
    Application / Subsystem               SMF Recording Environment
    ┌────────────────────┐                ┌────────────────────────┐
    │  Record Event      │    SMFWTM      │    SMF Writer          │
    │  Job, I/O, RACF    │ ─────────────> │    Buffer Mgmt         │
    └────────────────────┘    Writer      │  Dataset / Logstream   │
                                          └────────────────────────┘
                                                       │
    ┌────────────────────┐                ┌────────────────────────┐
    │  Observability     │    Bridge      │    Exit Framework      │
    │  Prometheus / Otel │ <───────────── │    IEFU83, IEFU84      │
    └────────────────────┘    Bridge      │  Filtering, Routing    │
                                          └────────────────────────┘
                                                       │
                                                       ▼
    ┌────────────────────┐                ┌────────────────────────┐
    │  Record Analysis   │    Dumping     │    SMF Record Types    │
    │  IFASMFDP equiv    │ <───────────── │    Types 30, 80, 70    │
    └────────────────────┘    Dump        └────────────────────────┘
```

### Module Structure

| Module | Description |
|--------|-------------|
| `record` | Base record format: Standard headers, self-defining sections, and binary serialization |
| `writer` | SMF Writer engine: Buffer management and persistent recording (SMFWTM) |
| `config` | SMFPRMxx configuration: Filtering rules, recording modes, and global exits |
| `exits` | SMF Exit framework: Dynamic registration and execution of IEFU83, IEFU84, etc. |
| `type30` | Type 30 (Job Accounting): Comprehensive tracking of job and step lifecycle |
| `type80` | Type 80 (Security Audit): RACF command and resource access logging |
| `performance`| Types 70-79 (Performance): CPU, paging, and workload metrics |
| `dataset` | Types 14/15/17/18: Dataset activity and I/O recording |
| `dump` | IFASMFDP-compatible dump utility for record extraction and filtering |
| `bridge` | Modern observability: SMF to Prometheus and OpenTelemetry mapping |

## Key Types and Components

### Record Format
- `SmfHeader`: The standard 24-byte header present in all SMF records.
- `SmfRecord`: Trait for implementing custom SMF record types with binary serde support.
- `SmfTriplet`: Encapsulates a self-defining section (Offset, Length, Number).

### SMF Writer
- `SmfWriter`: The core recording engine with multi-buffer support.
- `SmfWriterConfig`: Defines logstream or dataset-based recording targets.

### Observability
- `SmfToPrometheus`: Bridge for exporting SMF performance data as Prometheus metrics.
- `SmfToOtel`: Bridge for mapping SMF activity to OpenTelemetry spans.

## Feature Coverage

| Feature | Category | Status |
|---------|----------|--------|
| SMFWTM API      | Recording| Implemented |
| Type 30 (Sub 1-5)| Accounting| Implemented |
| Type 80 (RACF)  | Auditing | Implemented |
| Type 70 (CPU)   | Perf     | Implemented |
| IEFU83 Exit     | Filtering| Implemented |
| IFASMFDP Utility| Tools    | Implemented |
| Prometheus Exp  | Observ   | Implemented |

## Usage Examples

### Writing an SMF Record

```rust
use open_mainframe_smf::{SmfWriter, SmfType30};

let mut writer = SmfWriter::new();
let record = SmfType30::new_job_start("MYJOB");

writer.write_record(&record).unwrap();
```

### Configuring SMF via SMFPRMxx

```rust
use open_mainframe_smf::config::SmfPrmConfig;

let config = SmfPrmConfig::parse("TYPE(30,80), NOTYPE(110)").unwrap();
// Apply config to the writer...
```

## Testing

The SMF crate includes 200+ tests:
- **Serde**: Validates binary-perfect serialization of complex record structures.
- **Writer**: Tests buffer overflow scenarios and persistent storage recovery.
- **Exits**: Verifies that filtering exits (IEFU83) correctly suppress records.
- **Bridge**: Ensures accurate mapping of SMF counters to Prometheus gauge types.

```sh
cargo test -p open-mainframe-smf
```
