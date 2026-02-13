---
version: 'v1.1'
baseVersion: 'v1.0'
date: '2026-02-13'
status: 'draft'
codename: 'Batch Workload Ready'
---

# PRD - zOS-clone v1.1: Batch Workload Ready

**Author:** Tore
**Date:** 2026-02-13
**Base Version:** v1.0 (MVP)

---

## Executive Summary

### Version Overview

zOS-clone v1.1 extends the MVP with capabilities required for production batch workloads. While v1.0 proved the compilation approach works with sequential files, v1.1 adds the file access methods and utilities that real enterprise batch jobs depend on: VSAM indexed files, SORT processing, generation data groups, and proper dataset management through IDCAMS.

### Key Deliverables

1. **VSAM File Support** - Key-sequenced (KSDS), entry-sequenced (ESDS), and relative record (RRDS) datasets
2. **SORT Utility** - DFSORT-compatible sorting with full INCLUDE/OMIT/OUTREC support
3. **GDG Support** - Generation Data Group management for dataset versioning
4. **IDCAMS Utility** - Dataset definition, deletion, and catalog management
5. **Package Distribution** - apt/yum repositories for enterprise Linux deployment

### Target Users

**Primary:** Operations engineers ready to migrate batch workloads beyond sequential file processing.

**Secondary:** Mainframe developers testing programs that use VSAM or SORT.

---

## Version Goals

### Business Goals

| Goal | Target | Measurement |
|------|--------|-------------|
| Batch workload coverage | 60% of typical batch jobs runnable | Survey of pilot users |
| Enterprise deployment | 5+ organizations with apt/yum install | Download/install metrics |
| VSAM adoption | KSDS used in production by 3+ users | Community survey |

### Technical Goals

| Goal | Target | Measurement |
|------|--------|-------------|
| VSAM performance | KSDS read within 2x of z/OS | Benchmark suite |
| SORT throughput | 100MB/min on commodity hardware | Performance tests |
| GDG compatibility | 100% GDG semantics match | Compatibility tests |

### User Goals

| User | Goal | Success Indicator |
|------|------|-------------------|
| Operations Engineer | Run VSAM-dependent batch jobs | Jobs complete successfully |
| Mainframe Developer | Test SORT logic locally | Output matches mainframe |
| System Admin | Install via package manager | Single command deployment |

---

## Dependencies on v1.0

### Required MVP Components

| Component | Dependency Type | Notes |
|-----------|-----------------|-------|
| zos-dataset crate | Extension | VSAM builds on existing file layer |
| zos-encoding | Usage | EBCDIC handling for VSAM records |
| zos-jcl | Extension | DD statements for VSAM/GDG |
| zos-cobol runtime | Usage | COBOL VSAM verbs use runtime |
| CLI infrastructure | Extension | New commands (idcams, sort) |
| CI/CD pipeline | Extension | Package building added |

### MVP Features Required

- Sequential file I/O must work correctly (FR28-FR36)
- JCL DD statement processing (FR23-FR24)
- Dataset catalog/path resolution (FR33-FR34)
- COBOL file status handling (FR36)

---

## Functional Requirements

### VSAM File Support

- **FR-v1.1-001**: Developer can define VSAM KSDS (Key-Sequenced Data Set) clusters with primary and alternate keys
- **FR-v1.1-002**: Developer can perform keyed READ operations on KSDS with exact and generic key matching
- **FR-v1.1-003**: Developer can perform keyed WRITE operations on KSDS with duplicate key detection
- **FR-v1.1-004**: Developer can perform keyed REWRITE and DELETE operations on KSDS
- **FR-v1.1-005**: Developer can perform sequential READ (START/READ NEXT) on KSDS
- **FR-v1.1-006**: Developer can define VSAM ESDS (Entry-Sequenced Data Set) clusters
- **FR-v1.1-007**: Developer can perform sequential READ and WRITE operations on ESDS
- **FR-v1.1-008**: Developer can access ESDS records by RBA (Relative Byte Address)
- **FR-v1.1-009**: Developer can define VSAM RRDS (Relative Record Data Set) clusters
- **FR-v1.1-010**: Developer can perform keyed access to RRDS by relative record number
- **FR-v1.1-011**: Developer can receive proper VSAM file status codes in COBOL programs
- **FR-v1.1-012**: Developer can specify VSAM cluster attributes (RECORDSIZE, KEYS, SHAREOPTIONS)
- **FR-v1.1-013**: Operations engineer can allocate VSAM clusters via JCL DD statements
- **FR-v1.1-014**: Operations engineer can reference existing VSAM clusters in JCL

### SORT Utility

- **FR-v1.1-015**: Operations engineer can invoke SORT utility via JCL EXEC PGM=SORT
- **FR-v1.1-016**: Operations engineer can specify SORT fields with position, length, format, and order
- **FR-v1.1-017**: Operations engineer can use INCLUDE/OMIT statements for record filtering
- **FR-v1.1-018**: Operations engineer can use OUTREC for output record reformatting
- **FR-v1.1-019**: Operations engineer can use INREC for input record reformatting
- **FR-v1.1-020**: Operations engineer can use SUM for numeric field summarization
- **FR-v1.1-021**: Operations engineer can specify multiple input files (SORTIN01-99)
- **FR-v1.1-022**: Operations engineer can use MERGE operation for pre-sorted inputs
- **FR-v1.1-023**: Operations engineer can use COPY operation for reformatting without sort
- **FR-v1.1-024**: Developer can invoke SORT via CLI command for standalone sorting
- **FR-v1.1-025**: Operations engineer can use DFSORT control statement syntax

### Generation Data Groups (GDG)

- **FR-v1.1-026**: Operations engineer can define GDG base entries in catalog
- **FR-v1.1-027**: Operations engineer can reference GDG datasets with relative generation numbers (+1, -1, 0)
- **FR-v1.1-028**: Operations engineer can reference GDG datasets with absolute generation numbers (G0001V00)
- **FR-v1.1-029**: Operations engineer can create new generations with (+1) notation
- **FR-v1.1-030**: Operations engineer can access current generation with (0) notation
- **FR-v1.1-031**: Operations engineer can access previous generations with negative offsets
- **FR-v1.1-032**: System manages GDG limit and rolls off oldest generations per GDG definition
- **FR-v1.1-033**: Operations engineer can list GDG generations via IDCAMS LISTCAT
- **FR-v1.1-034**: Operations engineer can delete specific or all generations

### IDCAMS Utility

- **FR-v1.1-035**: Operations engineer can invoke IDCAMS via JCL EXEC PGM=IDCAMS
- **FR-v1.1-036**: Operations engineer can define VSAM clusters with DEFINE CLUSTER command
- **FR-v1.1-037**: Operations engineer can define GDG base with DEFINE GDG command
- **FR-v1.1-038**: Operations engineer can delete datasets with DELETE command
- **FR-v1.1-039**: Operations engineer can rename datasets with ALTER command
- **FR-v1.1-040**: Operations engineer can list catalog entries with LISTCAT command
- **FR-v1.1-041**: Operations engineer can print dataset contents with PRINT command
- **FR-v1.1-042**: Operations engineer can copy datasets with REPRO command
- **FR-v1.1-043**: Operations engineer can verify VSAM datasets with VERIFY command
- **FR-v1.1-044**: Developer can invoke IDCAMS via CLI command

### Package Distribution

- **FR-v1.1-045**: System administrator can install zOS-clone via apt (Debian/Ubuntu)
- **FR-v1.1-046**: System administrator can install zOS-clone via yum/dnf (RHEL/Fedora)
- **FR-v1.1-047**: System administrator can upgrade zOS-clone via package manager
- **FR-v1.1-048**: System administrator can configure package repository via standard methods
- **FR-v1.1-049**: Package includes man pages for all commands
- **FR-v1.1-050**: Package includes shell completions (bash, zsh, fish)

### CLI Enhancements

- **FR-v1.1-051**: Developer can invoke `zos-clone sort` for standalone SORT operations
- **FR-v1.1-052**: Developer can invoke `zos-clone idcams` for dataset management
- **FR-v1.1-053**: Developer can invoke `zos-clone gdg` for GDG operations

---

## Non-Functional Requirements

### Performance (NFR-v1.1-P)

- **NFR-v1.1-P1**: VSAM KSDS random read: <1ms for datasets <1GB
- **NFR-v1.1-P2**: VSAM KSDS sequential scan: >50MB/s throughput
- **NFR-v1.1-P3**: SORT utility: >100MB/min for in-memory sorts
- **NFR-v1.1-P4**: SORT utility: External merge sort for datasets >available memory
- **NFR-v1.1-P5**: GDG catalog operations: <100ms for generation resolution

### Compatibility (NFR-v1.1-C)

- **NFR-v1.1-C1**: VSAM file status codes match IBM documentation
- **NFR-v1.1-C2**: DFSORT control statement syntax 95%+ compatible
- **NFR-v1.1-C3**: IDCAMS command syntax 90%+ compatible for common commands
- **NFR-v1.1-C4**: GDG relative generation semantics identical to z/OS

### Reliability (NFR-v1.1-R)

- **NFR-v1.1-R1**: VSAM write operations are atomic (no partial records)
- **NFR-v1.1-R2**: VSAM index corruption detectable and recoverable
- **NFR-v1.1-R3**: GDG catalog consistency maintained across failures

### Maintainability (NFR-v1.1-M)

- **NFR-v1.1-M1**: VSAM module test coverage >80%
- **NFR-v1.1-M2**: SORT utility test coverage >85%
- **NFR-v1.1-M3**: Package build automated in CI/CD

---

## Success Criteria

### Exit Criteria

- [ ] All FR-v1.1-* requirements implemented and tested
- [ ] VSAM KSDS, ESDS, RRDS all functional with correct semantics
- [ ] SORT utility passes DFSORT compatibility test suite
- [ ] GDG operations work correctly with relative and absolute generations
- [ ] IDCAMS core commands (DEFINE, DELETE, LISTCAT, REPRO) functional
- [ ] apt and yum packages published and installable
- [ ] Performance benchmarks meet NFR targets
- [ ] Documentation updated for all new features
- [ ] No critical bugs open for 2+ weeks

### User Validation

- [ ] 3+ pilot users run VSAM-dependent batch jobs successfully
- [ ] 2+ organizations install via package manager
- [ ] SORT utility output matches DFSORT for test cases

---

## Risk Assessment

| Risk | Likelihood | Impact | Mitigation |
|------|------------|--------|------------|
| VSAM B+ tree implementation complexity | High | High | Start with KSDS only, defer ESDS/RRDS if needed |
| DFSORT syntax edge cases | Medium | Medium | Focus on common patterns, document unsupported features |
| GDG catalog corruption | Medium | High | Implement write-ahead logging for catalog changes |
| Package repository hosting costs | Low | Low | Use GitHub releases initially, cloud hosting later |
| VSAM performance below targets | Medium | Medium | Profile early, optimize hot paths |

---

## Out of Scope (v1.1)

The following are explicitly **not** in v1.1:

- VSAM alternate indexes (deferred to v1.2)
- VSAM Linear Data Sets (LDS)
- ICETOOL utility (DFSORT companion)
- VSAM spanning records
- SMS (Storage Management Subsystem) integration
- RACF/security integration for datasets
- PDS/PDSE library support

---

## Appendix: VSAM Technical Notes

### KSDS Implementation

KSDS will be implemented using a B+ tree structure:
- Leaf nodes contain data records
- Internal nodes contain key-pointer pairs
- Keys are extracted per DEFINE CLUSTER specification
- Index stored separately from data (CI/CA structure simplified)

### SORT Algorithm

SORT will use:
- In-memory quicksort for datasets fitting in memory
- External merge sort for larger datasets
- Parallel sort threads when multiple CPUs available

### GDG Catalog Structure

GDG information stored in catalog extension:
- Base entry with LIMIT and other attributes
- Generation entries with absolute names and timestamps
- Rolloff handled at job completion
