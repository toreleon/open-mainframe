---
version: 'v5.0'
planningGroup: 'PG-5'
technology: 'Utilities & DFSORT'
date: '2026-02-21'
status: 'complete'
inputDocuments:
  - 'zos-complete-inventory.md (AREA-7)'
  - 'gap-analysis/batch-21-utilities.md'
  - 'gap-analysis/batch-22-priority-matrix.md'
epicIds: ['UTIL-100', 'UTIL-101', 'UTIL-102', 'UTIL-103', 'UTIL-104', 'UTIL-105', 'UTIL-106', 'UTIL-107', 'UTIL-108', 'UTIL-109', 'UTIL-110']
sysEpicIds: []
---

# PRD: Utilities & DFSORT

## 1. Problem Statement

z/OS batch processing relies heavily on utility programs — IEBCOPY, IEBGENER, IEBCOMPR, IEBUPDTE, DFSORT, ICETOOL, and the IEH family. These are invoked in virtually every production JCL job stream. OpenMainframe currently implements IEFBR14, basic IEBGENER, and basic DFSORT, but the majority of utility programs are missing. Without them, real-world JCL job streams fail at the utility steps that copy, compare, sort, and transform data.

Additionally, batch processing bridges (IKJEFT01 for TSO-in-batch, IRXJCL for REXX-in-batch, BPXBATCH for USS-in-batch) are absent, preventing common batch patterns like running TSO commands or REXX scripts within JCL.

## 2. User Personas

| Persona | Description | Key Needs |
|---------|-------------|-----------|
| Batch JCL Developer | Writes JCL for production batch processing | IEBCOPY, IEBGENER, DFSORT, ICETOOL for data manipulation |
| Systems Programmer | Maintains PDS libraries and system datasets | IEBCOPY COMPRESS, IEBUPDTE, IEHPROGM |
| Application Tester | Generates and compares test data | IEBDG, IEBCOMPR, AMASPZAP |
| Automation Engineer | Orchestrates batch workflows | IKJEFT01, IRXJCL, BPXBATCH |

## 3. Scope

### MVP (Phase A)
- UTIL-100: IEBCOPY (PDS copy/compress/merge)
- UTIL-101: IEBGENER enhancements (control statement support)
- UTIL-104: DFSORT enhancements (OUTFIL, IFTHEN, FINDREP, ICETOOL)
- UTIL-110: Utility framework (common DD handling, message formatting)

### Growth (Phase B)
- UTIL-103: IEBUPDTE (PDS update utility)
- UTIL-109: TSO/REXX/USS batch processors (IKJEFT01, IRXJCL, BPXBATCH)
- UTIL-102: IEBCOMPR (dataset comparison)

### Extended (Phase C+)
- UTIL-105: IEBPTPCH (print/punch)
- UTIL-106: IEBDG (test data generator)
- UTIL-107: IEH system utilities (IEHMOVE, IEHPROGM, IEHLIST)
- UTIL-108: AMASPZAP (superzap)

## 4. Functional Requirements

| ID | Requirement | Acceptance Criteria | Source |
|----|-------------|---------------------|--------|
| FR-UTIL-001 | IEBCOPY shall copy members between PDS/PDSE datasets | Given COPY statement with INDD/OUTDD and SELECT/EXCLUDE, when executed, then specified members are copied | Batch 21, AREA-7 |
| FR-UTIL-002 | IEBCOPY shall compress a PDS in place | Given COPY statement with same INDD/OUTDD, when executed, then the PDS is reorganized to reclaim embedded free space | Batch 21 |
| FR-UTIL-003 | IEBCOPY shall merge multiple input PDS into one output | Given multiple INDD with SELECT, when executed, then members from all inputs are merged into the output | Batch 21 |
| FR-UTIL-004 | IEBGENER shall support control statement reformatting (GENERATE/RECORD/MEMBER) | Given RECORD FIELD= specifications, when executed, then fields are rearranged per control statements | Batch 21 |
| FR-UTIL-005 | DFSORT shall support OUTFIL for multiple output files from one sort | Given OUTFIL statements with INCLUDE/OMIT/OUTREC, when executed, then records are split to multiple outputs | Batch 21 |
| FR-UTIL-006 | DFSORT shall support IFTHEN for conditional field modification | Given IFTHEN=(WHEN=condition,OVERLAY=fields), when executed, then matching records are modified | Batch 21 |
| FR-UTIL-007 | ICETOOL shall support multiple operations (SORT, COPY, COUNT, STATS, DISPLAY, OCCUR, SELECT, UNIQUE) | Given ICETOOL TOOLIN/TOOLMSG/CTL control file, when executed, then operations run in sequence | Batch 21 |
| FR-UTIL-008 | IEBUPDTE shall add/replace/delete members using control statements | Given ./ ADD, ./ REPL, ./ CHANGE, ./ ENDUP, when executed, then PDS members are modified | Batch 21 |
| FR-UTIL-009 | IEBCOMPR shall compare sequential or PDS datasets and report differences | Given COMPARE statement, when executed, then mismatched records are reported with record numbers | Batch 21 |
| FR-UTIL-010 | IKJEFT01 shall execute TSO commands in batch mode | Given SYSTSIN DD with TSO commands, when executed, then commands run as if entered at TSO prompt | Batch 21 |
| FR-UTIL-011 | IRXJCL shall execute REXX execs in batch mode | Given SYSTSIN with REXX exec name, when executed, then the REXX exec runs with SYSIN/SYSPRINT | Batch 21 |
| FR-UTIL-012 | BPXBATCH shall execute USS commands/scripts in batch | Given STDPARM DD with Unix command, when executed, then the command runs in USS environment | Batch 21 |
| FR-UTIL-013 | All utilities shall return standard condition codes (0=success, 4=warning, 8=error, 12=severe, 16=fatal) | Given any utility execution, when complete, then the condition code follows z/OS conventions | Convention |
| FR-UTIL-014 | All utilities shall use SYSUT1/SYSUT2 or named DDs per IBM convention | Given standard DD names, when referenced, then utilities read input from SYSUT1/SYSIN and write to SYSUT2/SYSPRINT | Convention |

## 5. Non-Functional Requirements

| ID | Requirement | Threshold |
|----|-------------|-----------|
| NFR-UTIL-001 | IEBCOPY performance for PDS with 1000 members | < 2 seconds per copy operation |
| NFR-UTIL-002 | DFSORT performance for 1M record sort | < 5 seconds for in-memory sort of 1M x 80-byte records |
| NFR-UTIL-003 | Condition code compatibility | Same CC values as IBM utilities for same input |
| NFR-UTIL-004 | Message format compatibility | Message IDs follow IBM conventions (IEB, ICE, IEH prefix) |
| NFR-UTIL-005 | No unsafe code | `#![forbid(unsafe_code)]` |
| NFR-UTIL-006 | Error diagnostics | thiserror + miette per crate convention |

## 6. Dependencies

| Dependency | Direction | Description |
|------------|-----------|-------------|
| open-mainframe-dataset | UTIL → Dataset | All utilities read/write datasets via catalog and PDS/QSAM APIs |
| open-mainframe-sort | UTIL → Sort | DFSORT enhancements extend the existing sort crate |
| open-mainframe-tso | UTIL → TSO | IKJEFT01 invokes TSO command processor |
| open-mainframe-rexx | UTIL → REXX | IRXJCL invokes REXX interpreter |
| open-mainframe-jcl | JCL → UTIL | JCL `EXEC PGM=utility` invokes utility programs |
