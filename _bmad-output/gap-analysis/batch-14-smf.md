# Gap Analysis: SMF (System Management Facilities)

## Official Specification Summary

System Management Facilities (SMF) is the z/OS instrumentation and audit subsystem. It provides a standardised method for collecting, recording, and reporting system activity data. Virtually every operational event on the mainframe — job execution, dataset access, security events, performance statistics, subsystem activity — is captured in typed SMF records written to SYS1.MANx datasets or system logger log streams.

SMF is classified as **Core** (required infrastructure) on mainframes:
- Record types 0–127 are reserved for IBM; types 128–255 are available for vendors and user programs
- ~130+ IBM-defined record types covering every z/OS subsystem
- Standard record header (SMFLEN, SMFSEG, SMFFLG, SMFRTY, SMFTME, SMFDTE, SMFSID) provides uniform identification
- Self-defining sections (offset/length/number triplets) allow flexible record layouts
- SMFPRMxx parmlib member controls which record types are collected, exit routines, and recording targets
- IEFU83/IEFU84/IEFU85/IEFU86 exit routines allow installation filtering/modification of records
- SMFWTM and SMFEWTM macros allow user programs to write custom SMF records
- IFASMFDP (dataset dump) and IFASMFDL (log stream dump) utilities extract records for analysis
- Used for: accounting/chargeback, security auditing, performance monitoring, capacity planning, compliance

Key documentation:
- **z/OS MVS System Management Facilities (SMF) (SA38-0667)** — complete reference
- **z/OS MVS Initialization and Tuning Reference (SA23-1380)** — SMFPRMxx parmlib member
- **z/OS MVS Installation Exits (SA23-1381)** — IEFU83/IEFU84/IEFU85/IEFU86
- **z/OS MVS Programming: Authorized Assembler Services Reference (SA23-1372)** — SMFWTM/SMFEWTM macros

## Key Features & Capabilities

### 1. SMF Record Header Structure

#### Standard Header (18 bytes)

| Offset | Field | Size | Description |
|--------|-------|------|-------------|
| 0x00 | SMFLEN | 2 bytes | Record length (total) |
| 0x02 | SMFSEG | 2 bytes | Segment descriptor |
| 0x04 | SMFFLG | 1 byte | System indicator flags |
| 0x05 | SMFRTY | 1 byte | Record type (0–255) |
| 0x06 | SMFTME | 4 bytes | Time since midnight in 1/100ths of a second |
| 0x0A | SMFDTE | 4 bytes | Date in packed format (0cyydddF) |
| 0x0E | SMFSID | 4 bytes | System ID (EBCDIC) |

#### Extended Header (additional fields for subtypes)

| Field | Size | Description |
|-------|------|-------------|
| SMFSSID | 4 bytes | Subsystem identifier |
| SMFSTYP | 2 bytes | Record subtype |

#### Self-Defining Sections (Triplets)

Many SMF records use self-defining sections to locate data within the record:

| Field | Size | Description |
|-------|------|-------------|
| Offset | 4 bytes | Offset from start of record to data section |
| Length | 2 bytes | Length of each data entry |
| Number | 2 bytes | Number of entries |

### 2. Key SMF Record Types

#### System & Job Activity (Types 0–40)

| Type | Subtype | Description |
|------|---------|-------------|
| 0 | — | IPL (Initial Program Load) |
| 2 | — | Dump header |
| 3 | — | Dump trailer |
| 4 | — | Step termination (legacy; replaced by type 30) |
| 5 | — | Job termination (legacy; replaced by type 30) |
| 6 | — | External writer / JES output / PSF |
| 7 | — | SMF data lost notification |
| 8 | — | I/O configuration |
| 14 | — | INPUT or RDBACK dataset activity |
| 15 | — | OUTPUT, UPDAT, INOUT, OUTIN dataset activity |
| 17 | — | Scratch (delete) dataset status |
| 18 | — | Rename non-VSAM dataset status |
| 22 | — | Configuration |
| 23 | — | SMF status |
| 26 | — | JES2 job purge |
| 30 | 1 | Job/task initiation |
| 30 | 2 | Interval record (periodic snapshot) |
| 30 | 3 | Last interval record |
| 30 | 4 | Step end (resource usage: CPU, I/O, paging, storage) |
| 30 | 5 | Job termination (summary: elapsed time, steps, completion code) |
| 32 | — | TSO/E user work accounting |
| 33 | — | APPC/MVS accounting |
| 35 | — | Logoff |
| 40 | — | Dynamic DD |

#### DFSMS & VSAM (Types 41–66)

| Type | Subtype | Description |
|------|---------|-------------|
| 41 | — | DIV objects and VLF statistics |
| 42 | — | DFSMS statistics and configuration |
| 43 | — | JES start |
| 45 | — | JES withdrawal/stop |
| 60 | — | VSAM volume dataset entry |
| 61 | — | ICF define/alter activity |
| 62 | — | VSAM component activity |
| 64 | — | VSAM RLS request data |
| 65 | — | VSAM RLS cache/lock data |
| 66 | — | VSAM RLS storage data |

#### RMF Performance (Types 70–79)

| Type | Subtype | Description |
|------|---------|-------------|
| 70 | 1 | CPU activity |
| 71 | — | Paging activity |
| 72 | 3 | Workload activity (WLM service/report classes) |
| 73 | — | Channel activity |
| 74 | — | Device activity |
| 75 | — | Page/swap dataset activity |
| 77 | — | Enqueue activity |
| 78 | 2,3 | Virtual storage / common storage |
| 79 | — | Monitor III sampling |

#### Security & Audit (Types 80–83)

| Type | Subtype | Description |
|------|---------|-------------|
| 80 | — | RACF event records (access violations, password failures, resource checks) |
| 81 | — | RACF initialization |
| 82 | — | RACF command audit |
| 83 | — | Real storage usage |

#### System Services (Types 88–99)

| Type | Subtype | Description |
|------|---------|-------------|
| 88 | — | System logger |
| 89 | — | Software product usage (sub-capacity pricing / MULC) |
| 92 | — | z/OS UNIX file system activity |
| 99 | — | Loss of data notification |

#### Subsystem Performance (Types 100–120)

| Type | Subtype | Description |
|------|---------|-------------|
| 100 | — | DB2 statistics |
| 101 | — | DB2 accounting |
| 102 | — | DB2 performance |
| 110 | — | CICS performance and accounting |
| 115 | — | IBM MQ accounting |
| 116 | — | TCP/IP statistics |
| 119 | — | IBM MQ channel activity |
| 120 | — | WebSphere / Java batch |
| 125 | — | Generic Tracker (GTZ) data persistence |

#### Vendor/User Records (Types 128–255)

| Range | Description |
|-------|-------------|
| 128–255 | Available for vendor products and user-written SMF records |

### 3. SMF Configuration (SMFPRMxx)

| Parameter | Description |
|-----------|-------------|
| SYS(TYPE(nnn,...)) | Record types to collect system-wide |
| SYS(NOTYPE(nnn,...)) | Record types to suppress |
| SYS(EXITS(exitname,...)) | SMF exits to invoke (IEFU83, IEFU84, etc.) |
| SYS(NOEXITS) | Disable SMF exits |
| SYS(INTERVAL(hhmm)) | Interval recording period |
| SUBSYS(name,TYPE(nnn,...)) | Per-subsystem record type selection |
| RECORDING(LOGSTREAM) | Record to system logger log streams |
| RECORDING(DATASET) | Record to SYS1.MANx datasets |
| DSNAME(dsn1,dsn2,...) | SMF recording dataset names |
| DEFAULTLSNAME(name) | Default log stream name |
| LSNAME(name,TYPE(nnn,...)) | Per-type log stream assignment |
| PROMPT(LIST\|ALL\|IPLR) | Operator prompt control at IPL |
| SUBPARM(name,data) | Pass data to a subsystem |
| INMEM(definition) | In-memory SMF resource definition |
| HFTSINTVL(seconds) | High-frequency throughput statistics interval |

### 4. SMF Exits

| Exit | When Invoked | Description |
|------|-------------|-------------|
| IEFU83 | SMFWTM or SMFEWTM(BRANCH=NO) | Filter/validate records before writing |
| IEFU84 | SMFEWTM(BRANCH=YES, not XMEM) | Branch-entry exit for performance |
| IEFU85 | SMFEWTM(BRANCH=YES, MODE=XMEM) | Cross-memory exit |
| IEFU86 | Extended header records | New unified exit for extended SMF records |
| IFASMFEX | SMF buffer flush | Exit during SMF buffer management |

### 5. SMF Writing APIs

| Macro/API | Description |
|-----------|-------------|
| SMFWTM | Write an SMF record (standard; invokes IEFU83) |
| SMFEWTM | Write extended SMF record (BRANCH=YES/NO, MODE=XMEM options) |
| IFASMFR | SMF record mapping macro (DSECTs for record layouts) |

SMFWTM parameters:
- Register 0: Record type
- Register 1: Address of record
- Record must have standard SMF header filled in

SMFEWTM additional options:
- BRANCH=YES: Use branch entry for performance (avoids SVC overhead)
- BRANCH=NO: Use SVC entry (invokes IEFU83)
- MODE=XMEM: Cross-memory mode (invokes IEFU85)

### 6. SMF Dump/Processing Utilities

| Utility | Description |
|---------|-------------|
| IFASMFDP | Dump SMF records from SYS1.MANx datasets |
| IFASMFDL | Dump SMF records from system logger log streams |
| IFAURP | Usage reporting (product registration from type 89) |

IFASMFDP/IFASMFDL control statements:
- INDD/OUTDD: Input/output DD names
- DATE(start,end): Date range filter
- START/END: Time range filter
- TYPE(nnn,...): Record type filter
- LSNAME(name): Log stream name (IFASMFDL only)

### 7. Common SMF Use Cases

| Use Case | Key Record Types |
|----------|-----------------|
| Accounting / Chargeback | Type 30 (subtypes 4,5), type 89 |
| Security Audit | Type 80 (RACF events), 81, 82 |
| Performance Monitoring | Types 70–79 (RMF), 30 (interval) |
| Dataset Tracking | Types 14, 15, 17, 18, 42 |
| DB2 Performance | Types 100, 101, 102 |
| CICS Performance | Type 110 |
| IBM MQ Activity | Types 115, 119 |
| Capacity Planning | Types 70, 72, 73, 78 |
| TCP/IP Statistics | Type 116 |
| Compliance / STIG | Types 30, 80, 89 (required by DoD STIGs) |

## Current OpenMainframe Status

The `open-mainframe` codebase has **no direct SMF implementation** but contains significant observability infrastructure that could serve as a foundation:

### Partial — Observability Infrastructure (open-mainframe-deploy crate)

1. **Prometheus Metrics** (`crates/open-mainframe-deploy/src/metrics.rs`, 439 lines):
   - `Metrics`: requests_total, request_duration, active_connections, errors_total
   - `CobolMetrics`: programs_executed, execution_duration, compilation_time, active_programs
   - `CicsMetrics`: transactions_total, transaction_duration, active_tasks, queue_depth
   - `ImsMetrics`: dli_calls_total, dli_duration, segments_retrieved, active_psbs
   - `DatabaseMetrics`: query_duration, pool_connections_active/idle, connection_errors

2. **Batch Job Metrics** (`crates/open-mainframe-deploy/src/batch_metrics.rs`, 442 lines):
   - `JobMetrics`: job_name, job_number, steps_total, steps_executed, job_return_code, step_metrics, total_duration
   - `StepMetrics`: step_name, program, return_code, duration, records_read, records_written
   - `BatchMetricsCollector`: jobs_completed/succeeded/failed, total_steps_executed, errors, avg_durations
   - Prometheus text exposition export

3. **Runtime Instrumentation** (`crates/open-mainframe-deploy/src/instrumentation.rs`, 425 lines):
   - RAII guards for COBOL execution, CICS transactions, IMS DL/I calls
   - Automatic duration recording, active program tracking
   - Thread-safe `Arc<MetricsRegistry>`

4. **OpenTelemetry Tracing** (`crates/open-mainframe-deploy/src/tracing_setup.rs`, 244 lines):
   - OTLP exporter, configurable sampling, resource metadata
   - JSON/text/compact log formatting
   - Graceful shutdown

5. **Health Checks** (`crates/open-mainframe-deploy/src/health.rs`, 268 lines):
   - `/health`, `/ready`, `/metrics` HTTP endpoints

### Partial — JCL Accounting Field

- `JobParams.accounting: Option<String>` parsed from JOB statement (`crates/open-mainframe-jcl/src/ast/mod.rs:145`)
- Parser extracts accounting info but does not forward it to any monitoring system

### Partial — SMF Timestamp Format

- `format_dt1()` in `crates/open-mainframe-sort/src/reformat.rs:388-429` decodes SMF DT1 packed decimal date format (0cyydddF)
- Used for DFSORT but applicable to SMF record timestamp parsing

### Missing

- SMF record type definitions (0–255)
- SMF standard/extended header structures
- Self-defining section (triplet) framework
- SMFPRMxx configuration parsing
- SMFWTM/SMFEWTM record writing API
- IEFU83/84/85/86 exit framework
- IFASMFDP/IFASMFDL dump utilities
- SYS1.MANx dataset or log stream recording
- SMF record mapping macros (IFASMFR equivalents)
- Type-specific record layouts (type 14/15, 30, 80, 110, etc.)

## Gap Details

### SMF Record Infrastructure

| Feature | Official z/OS | OpenMainframe | Gap |
|---------|--------------|---------------|-----|
| Standard header (18 bytes) | SMFLEN/SMFSEG/SMFFLG/SMFRTY/SMFTME/SMFDTE/SMFSID | Not implemented | Missing |
| Extended header (with subtype) | SMFSSID + SMFSTYP | Not implemented | Missing |
| Self-defining sections (triplets) | Offset/length/number for flexible layouts | Not implemented | Missing |
| Record type registry (0–255) | Enum of all IBM-defined types | Not implemented | Missing |
| DT1 date format | 0cyydddF packed decimal | `format_dt1()` in sort/reformat.rs | **Present** |
| Time format | 1/100ths since midnight (4-byte binary) | Not implemented | Missing |
| Record serialization | Binary record layout | Not implemented | Missing |
| Record deserialization/parsing | Read binary SMF data | Not implemented | Missing |

### SMF Configuration

| Feature | Official z/OS | OpenMainframe | Gap |
|---------|--------------|---------------|-----|
| SMFPRMxx parmlib member | TYPE/NOTYPE, EXITS, INTERVAL, RECORDING | Not implemented | Missing |
| SYS() parameters | System-wide recording options | Not implemented | Missing |
| SUBSYS() parameters | Per-subsystem recording options | Not implemented | Missing |
| RECORDING(LOGSTREAM\|DATASET) | Recording target selection | Not implemented | Missing |
| DSNAME configuration | SYS1.MANx dataset names | Not implemented | Missing |
| LSNAME/DEFAULTLSNAME | Log stream names | Not implemented | Missing |
| SET SMF=xx command | Dynamic reconfiguration | Not implemented | Missing |
| D SMF,O command | Display SMF configuration | Not implemented | Missing |

### SMF Writing APIs

| Feature | Official z/OS | OpenMainframe | Gap |
|---------|--------------|---------------|-----|
| SMFWTM macro | Write SMF record (SVC path) | Not implemented | Missing |
| SMFEWTM macro | Write extended record (branch/XMEM) | Not implemented | Missing |
| Record validation | Header field validation before write | Not implemented | Missing |
| Buffer management | SMF buffer pool, flush logic | Not implemented | Missing |
| User record types (128–255) | Application-defined records | Not implemented | Missing |

### SMF Exit Framework

| Feature | Official z/OS | OpenMainframe | Gap |
|---------|--------------|---------------|-----|
| IEFU83 exit | SVC-path record filter | Not implemented | Missing |
| IEFU84 exit | Branch-entry record filter | Not implemented | Missing |
| IEFU85 exit | Cross-memory record filter | Not implemented | Missing |
| IEFU86 exit | Extended header record filter | Not implemented | Missing |
| IFASMFEX exit | Buffer management exit | Not implemented | Missing |
| Dynamic exit facility | PROGxx-based exit management | Not implemented | Missing |

### SMF Dump/Processing Utilities

| Feature | Official z/OS | OpenMainframe | Gap |
|---------|--------------|---------------|-----|
| IFASMFDP | Dump from SYS1.MANx datasets | Not implemented | Missing |
| IFASMFDL | Dump from log streams | Not implemented | Missing |
| IFAURP | Usage reporting (type 89) | Not implemented | Missing |
| DATE/TIME/TYPE filters | Record selection on dump | Not implemented | Missing |
| Record splitting | Split by type to separate datasets | Not implemented | Missing |

### Type-Specific Record Implementations

| Record Type | Official z/OS | OpenMainframe | Gap |
|-------------|--------------|---------------|-----|
| Type 0 (IPL) | IPL parameters, system ID | Not implemented | Missing |
| Type 4/5 (Step/Job term) | Legacy job accounting | Not implemented | Missing |
| Type 14/15 (Dataset activity) | Dataset open/close, EXCP count, device | Not implemented | Missing |
| Type 17/18 (Scratch/Rename) | Dataset lifecycle events | Not implemented | Missing |
| Type 26 (JES2 purge) | Job purge events | Not implemented | Missing |
| Type 30 (Address space work) | Job/step accounting (subtypes 1–5) | Batch JobMetrics/StepMetrics tracks similar data | **Partial** |
| Type 32 (TSO accounting) | TSO session accounting | Not implemented | Missing |
| Type 42 (DFSMS statistics) | VSAM/SMS statistics | Not implemented | Missing |
| Type 62 (VSAM activity) | VSAM component activity | Not implemented | Missing |
| Type 70–79 (RMF) | CPU, paging, workload, channel, device | Prometheus metrics (different format) | **Partial** |
| Type 80 (RACF) | Security events, access violations | Not implemented | Missing |
| Type 89 (Usage data) | Product registration, sub-capacity | Not implemented | Missing |
| Type 92 (USS activity) | z/OS UNIX file system | Not implemented | Missing |
| Type 100–102 (DB2) | DB2 statistics/accounting/performance | Not implemented | Missing |
| Type 110 (CICS) | CICS transaction/performance data | CicsMetrics (different format) | **Partial** |
| Type 115/119 (MQ) | MQ accounting/channel activity | Not implemented | Missing |
| Type 116 (TCP/IP) | Network statistics | Not implemented | Missing |

### Observability Bridge (Existing → SMF)

| Feature | Official z/OS | OpenMainframe | Gap |
|---------|--------------|---------------|-----|
| Job/step metrics | SMF type 30 records | BatchMetricsCollector in deploy crate | **Partial** |
| COBOL program metrics | SMF type 30 + step metrics | CobolMetrics in deploy crate | **Partial** |
| CICS transaction metrics | SMF type 110 records | CicsMetrics in deploy crate | **Partial** |
| IMS DL/I metrics | SMF type 30 + IMS-specific | ImsMetrics in deploy crate | **Partial** |
| Database metrics | SMF type 100–102 (DB2) | DatabaseMetrics in deploy crate | **Partial** |
| JCL accounting info | Flows to SMF type 30 | Parsed but not forwarded | **Partial** |
| Prometheus export | Not native z/OS | Full Prometheus metrics | Present (non-SMF) |
| OpenTelemetry tracing | Not native z/OS | OTLP exporter configured | Present (non-SMF) |

## Proposed Epic Structure

### SMF100: SMF Record Infrastructure (M)
- SMF standard header struct (SMFLEN, SMFSEG, SMFFLG, SMFRTY, SMFTME, SMFDTE, SMFSID)
- SMF extended header (SMFSSID, SMFSTYP)
- Self-defining section (triplet) framework
- Record type enum (0–255) with IBM-defined names
- DT1 date encoding/decoding (leverage existing `format_dt1`)
- Time encoding (1/100ths since midnight)
- Binary serialization/deserialization
- **Depends on**: open-mainframe-encoding (EBCDIC)

### SMF101: SMFPRMxx Configuration Parser (M)
- Parse SYS(TYPE/NOTYPE/EXITS/NOEXITS/INTERVAL)
- Parse SUBSYS() per-subsystem parameters
- Parse RECORDING(LOGSTREAM|DATASET)
- Parse DSNAME, LSNAME, DEFAULTLSNAME
- Parse PROMPT, SUBPARM, INMEM, HFTSINTVL
- SET SMF=xx dynamic reconfiguration
- D SMF,O display command
- **Depends on**: SMF100

### SMF102: SMF Record Writing API (M)
- SMFWTM equivalent — write record with header validation
- SMFEWTM equivalent — extended write with branch/XMEM modes
- Buffer management (configurable buffer pool)
- Recording to file (SYS1.MANx equivalent) or log stream
- Record sequencing and numbering
- Support for user record types (128–255)
- **Depends on**: SMF100, SMF101

### SMF103: SMF Exit Framework (S)
- IEFU83 exit point (SVC-path filter)
- IEFU84 exit point (branch-entry filter)
- IEFU85 exit point (cross-memory filter)
- IEFU86 exit point (extended header records)
- Dynamic exit registration (PROGxx equivalent)
- Exit return code handling (accept/reject/modify)
- **Depends on**: SMF102

### SMF104: Job Accounting Records — Type 30 (L)
- Type 30 subtype 1 (job initiation): job name, class, priority, accounting info
- Type 30 subtype 2/3 (interval): CPU time, SRB time, I/O counts, storage usage
- Type 30 subtype 4 (step end): step name, program, return code, EXCP, elapsed time
- Type 30 subtype 5 (job termination): completion code, total steps, summary
- Bridge from existing BatchMetricsCollector/JobMetrics/StepMetrics
- Integration with JCL accounting field (JobParams.accounting)
- **Depends on**: SMF102, open-mainframe-jcl

### SMF105: Dataset Activity Records — Types 14/15/17/18 (M)
- Type 14 (INPUT/RDBACK): dataset name, volume, EXCP count, device type
- Type 15 (OUTPUT/UPDAT/INOUT/OUTIN): same fields for output datasets
- Type 17 (Scratch): dataset deletion events
- Type 18 (Rename): dataset rename events
- Integration with open-mainframe-dataset file operations
- **Depends on**: SMF102, open-mainframe-dataset

### SMF106: Security Audit Records — Type 80 (L)
- Type 80 RACF event records: access violations, password failures, resource checks
- Type 81 RACF initialization events
- Type 82 RACF command audit
- Integration with RACF subsystem (batch-08)
- SAF interface event recording
- Digitally signed SMF record support
- **Depends on**: SMF102, open-mainframe-racf (batch-08)

### SMF107: Performance Records — Types 70–79 (L)
- Type 70 (CPU activity): processor utilization, LPAR data
- Type 71 (Paging): page-in/out rates, steal rates
- Type 72 (Workload activity): WLM service class performance
- Type 73 (Channel activity): channel utilization
- Type 74 (Device activity): device utilization, response times
- Bridge from existing Prometheus metrics to RMF-equivalent records
- **Depends on**: SMF102, WLM (batch-17)

### SMF108: Subsystem Records — Types 100–120 (L)
- Type 100/101/102 (DB2): statistics, accounting, performance trace
- Type 110 (CICS): transaction accounting, performance
- Type 115/119 (MQ): MQ accounting, channel activity
- Type 116 (TCP/IP): network statistics
- Bridge from existing CicsMetrics, ImsMetrics, DatabaseMetrics
- **Depends on**: SMF102, open-mainframe-db2, open-mainframe-cics

### SMF109: SMF Dump Utilities (M)
- IFASMFDP: dump records from dataset files
- IFASMFDL: dump records from log streams
- Control statement parsing (DATE, TIME, TYPE filters)
- Record splitting by type to separate output files
- IFAURP: usage reporting from type 89 records
- JCL procedure for batch execution
- **Depends on**: SMF100, open-mainframe-jcl

### SMF110: Observability Bridge (M)
- Convert existing Prometheus/OpenTelemetry metrics → SMF record format
- Dual-write mode: metrics go to both Prometheus AND SMF
- SMF → Prometheus reverse bridge (read SMF records, expose as metrics)
- Configuration for which SMF types map to which Prometheus metrics
- Grafana dashboard integration for SMF-sourced data
- **Depends on**: SMF102, open-mainframe-deploy (metrics/instrumentation)

## Dependencies

| Epic | Depends On (Internal) | Depends On (External Crate) |
|------|----------------------|----------------------------|
| SMF100 | — | open-mainframe-encoding (EBCDIC) |
| SMF101 | SMF100 | — |
| SMF102 | SMF100, SMF101 | — |
| SMF103 | SMF102 | — |
| SMF104 | SMF102 | open-mainframe-jcl (accounting) |
| SMF105 | SMF102 | open-mainframe-dataset |
| SMF106 | SMF102 | open-mainframe-racf (batch-08) |
| SMF107 | SMF102 | WLM (batch-17) |
| SMF108 | SMF102 | open-mainframe-db2, open-mainframe-cics |
| SMF109 | SMF100 | open-mainframe-jcl |
| SMF110 | SMF102 | open-mainframe-deploy |

## Complexity Estimate

| Epic | Size | Rationale |
|------|------|-----------|
| SMF100 | M | Header structs, triplets, type enum, binary ser/deser; well-defined binary format |
| SMF101 | M | Parmlib parser with ~15 parameters; moderate complexity |
| SMF102 | M | Write API, buffer management, file/logstream recording |
| SMF103 | S | Exit framework — registration, invocation, return code handling |
| SMF104 | L | Type 30 has 5 subtypes with many fields each; bridge from existing batch metrics |
| SMF105 | M | 4 record types (14/15/17/18) with straightforward layouts; dataset integration |
| SMF106 | L | Type 80 has complex event structures; RACF integration; digital signatures |
| SMF107 | L | 8+ record types (70–79); performance data collection; WLM integration |
| SMF108 | L | 6+ record types across DB2/CICS/MQ/TCP; multiple subsystem bridges |
| SMF109 | M | Dump utilities with filtering; JCL integration; straightforward I/O |
| SMF110 | M | Bidirectional bridge between Prometheus/OTEL and SMF; configuration mapping |

**Overall**: L — 11 epics totaling 1S + 6M + 4L. SMF is a pervasive subsystem touching every other component. The existing Prometheus/OpenTelemetry infrastructure provides a strong foundation for metrics collection — the primary work is defining SMF record structures, implementing the recording pipeline, and bridging existing metrics to SMF format. The record-type-specific implementations (types 30, 80, 70–79, 100–120) represent the bulk of the effort.

## Reference Documentation

- IBM z/OS MVS System Management Facilities (SA38-0667) — https://www.ibm.com/docs/en/zos/3.1.0?topic=management-system-facilities-smf
- SMFPRMxx parmlib member (z/OS 3.1) — https://www.ibm.com/docs/en/zos/3.1.0?topic=sys1parmlib-smfprmxx-system-management-facilities-smf-parameters
- SMF record format — https://www.ibm.com/docs/en/zos/2.4.0?topic=sr-smf-record-format
- Standard SMF record header — https://www.ibm.com/docs/SSLTBW_2.2.0/com.ibm.zos.v2r2.ieag200/smfhdr.htm
- SMFWTM macro — https://www.ibm.com/docs/en/zos/2.5.0?topic=wto-smfwtm-write-smf-record
- SMFEWTM macro — https://www.ibm.com/docs/en/zos/2.5.0?topic=macros-smfewtm-writing-smf-records
- IFASMFDP dump program — https://www.ibm.com/docs/en/zos/2.4.0?topic=programs-using-ifasmfdp-smf-data-set-dump-program
- IFASMFDL/IFASMFDP exits — https://www.ibm.com/docs/en/zos/3.1.0?topic=exits-ifasmfdl-ifasmfdp-smf-dump-program
- SMF record type overview (IBM) — https://www.ibm.com/docs/en/zos/2.1.0?topic=smf-records
- IBM SMF Explorer — https://ibm.github.io/IBM-SMF-Explorer/
- IBM SMF Tools (GitHub) — https://github.com/IBM/IBM-Z-zOS/tree/main/SMF-Tools
- Watson Walker SMF Reference — https://watsonwalker.com/wp-content/uploads/2020/11/SMF-Reference-20201107.pdf
- Wikipedia: IBM System Management Facilities — https://en.wikipedia.org/wiki/IBM_System_Management_Facilities
