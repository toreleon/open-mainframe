---
version: 'v1.2'
baseVersion: 'v1.1'
date: '2026-02-13'
status: 'draft'
codename: 'Enterprise Features'
---

# PRD - zOS-clone v1.2: Enterprise Features

**Author:** Tore
**Date:** 2026-02-13
**Base Version:** v1.1 (Batch Workload Ready)

---

## Executive Summary

### Version Overview

zOS-clone v1.2 extends batch processing capabilities with enterprise database and transaction support. While v1.1 handled VSAM files and batch utilities, v1.2 adds DB2 SQL support via PostgreSQL backend and introduces basic CICS command-level programming. This release enables migration of database-driven batch jobs and prepares the foundation for online transaction processing.

### Key Deliverables

1. **DB2 SQL Support** - EXEC SQL preprocessing with PostgreSQL as the execution backend
2. **CICS Foundation** - Basic EXEC CICS commands for BMS screen handling and simple transactions
3. **COBOL Enhancements** - Improved diagnostics, COBOL-2002 compliance gaps
4. **Migration Tooling** - Assessment tools to analyze mainframe codebases

### Target Users

**Primary:** Enterprise teams with DB2-dependent batch jobs ready to migrate after successful v1.1 adoption.

**Secondary:** Development teams evaluating CICS migration feasibility.

---

## Version Goals

### Business Goals

| Goal | Target | Measurement |
|------|--------|-------------|
| DB2 batch coverage | 40% of DB2 batch jobs runnable | Pilot user survey |
| CICS evaluation | 10+ enterprises testing CICS features | Download/usage metrics |
| Enterprise adoption | 15+ organizations with production v1.1/v1.2 | Customer count |

### Technical Goals

| Goal | Target | Measurement |
|------|--------|-------------|
| SQL compatibility | 80% of common DB2 SQL syntax | SQL test suite |
| CICS commands | 20+ EXEC CICS commands | Command coverage tests |
| PostgreSQL performance | Within 3x of DB2 for batch | Benchmark suite |

### User Goals

| User | Goal | Success Indicator |
|------|------|-------------------|
| COBOL/DB2 Developer | Run DB2 batch jobs locally | Jobs produce correct results |
| CICS Developer | Test BMS maps and basic commands | Screen flows work correctly |
| Migration Analyst | Assess codebase compatibility | Clear compatibility report |

---

## Dependencies on v1.1

### Required v1.1 Components

| Component | Dependency Type | Notes |
|-----------|-----------------|-------|
| VSAM file layer | Usage | DB2 tablespaces map to files |
| JCL executor | Extension | DB2 utility steps |
| COBOL runtime | Extension | EXEC SQL integration |
| IDCAMS utility | Usage | Database object management |
| zos-dataset crate | Extension | DB2 data storage |

### v1.1 Features Required

- VSAM KSDS for index simulation (FR-v1.1-001 to FR-v1.1-010)
- SORT utility for ORDER BY operations (FR-v1.1-026 to FR-v1.1-034)
- JCL DD statement processing (FR-v1.1-037)
- Complete file status handling

---

## Functional Requirements

### DB2 SQL Support

#### SQL Preprocessing

- **FR-v1.2-001**: Developer can embed EXEC SQL statements in COBOL programs
- **FR-v1.2-002**: Preprocessor extracts SQL and generates COBOL CALL statements
- **FR-v1.2-003**: SQLCA (SQL Communication Area) provides status after each SQL operation
- **FR-v1.2-004**: Host variables allow data exchange between COBOL and SQL
- **FR-v1.2-005**: DECLARE CURSOR enables multi-row result set processing

#### Data Manipulation

- **FR-v1.2-006**: SELECT INTO retrieves single rows into host variables
- **FR-v1.2-007**: INSERT adds rows using host variable values
- **FR-v1.2-008**: UPDATE modifies rows using host variables and WHERE clause
- **FR-v1.2-009**: DELETE removes rows matching WHERE conditions
- **FR-v1.2-010**: FETCH retrieves rows from opened cursor

#### Cursor Operations

- **FR-v1.2-011**: DECLARE CURSOR defines SQL query for iteration
- **FR-v1.2-012**: OPEN CURSOR executes query and positions before first row
- **FR-v1.2-013**: FETCH CURSOR retrieves next row into host variables
- **FR-v1.2-014**: CLOSE CURSOR releases cursor resources
- **FR-v1.2-015**: WITH HOLD cursors survive COMMIT

#### Transaction Control

- **FR-v1.2-016**: COMMIT makes changes permanent
- **FR-v1.2-017**: ROLLBACK undoes changes since last commit
- **FR-v1.2-018**: Implicit commit at program end (configurable)

#### PostgreSQL Backend

- **FR-v1.2-019**: DB2 SQL syntax translates to PostgreSQL dialect
- **FR-v1.2-020**: Connection pooling manages PostgreSQL connections
- **FR-v1.2-021**: DCLGEN generates COBOL copybooks from PostgreSQL tables
- **FR-v1.2-022**: BIND creates access paths (execution plans)

---

### CICS Foundation

#### Program Control

- **FR-v1.2-023**: EXEC CICS LINK invokes another program
- **FR-v1.2-024**: EXEC CICS XCTL transfers control without return
- **FR-v1.2-025**: EXEC CICS RETURN ends program execution
- **FR-v1.2-026**: COMMAREA passes data between programs

#### BMS Screen Handling

- **FR-v1.2-027**: EXEC CICS SEND MAP displays BMS map to terminal
- **FR-v1.2-028**: EXEC CICS RECEIVE MAP reads input from terminal
- **FR-v1.2-029**: BMS map definitions compile to screen layouts
- **FR-v1.2-030**: MAPSET contains multiple related maps

#### Data Operations

- **FR-v1.2-031**: EXEC CICS READ retrieves VSAM record by key
- **FR-v1.2-032**: EXEC CICS WRITE adds new VSAM record
- **FR-v1.2-033**: EXEC CICS REWRITE updates current record
- **FR-v1.2-034**: EXEC CICS DELETE removes current record

#### Working Storage

- **FR-v1.2-035**: EXEC CICS GETMAIN allocates dynamic storage
- **FR-v1.2-036**: EXEC CICS FREEMAIN releases allocated storage

#### Exception Handling

- **FR-v1.2-037**: HANDLE CONDITION routes to error paragraphs
- **FR-v1.2-038**: RESP/RESP2 return condition codes
- **FR-v1.2-039**: EIBRESP fields provide error information

#### Terminal Emulation

- **FR-v1.2-040**: Console mode displays BMS screens in terminal
- **FR-v1.2-041**: Web mode serves BMS screens as HTML forms
- **FR-v1.2-042**: REST API exposes CICS transactions as endpoints

---

### COBOL Enhancements

#### COBOL-2002 Compliance

- **FR-v1.2-043**: Object-oriented COBOL (CLASS-ID, METHOD-ID)
- **FR-v1.2-044**: Inline PERFORM (PERFORM ... END-PERFORM with inline code)
- **FR-v1.2-045**: Reference modification improvements
- **FR-v1.2-046**: Intrinsic function extensions

#### Diagnostics

- **FR-v1.2-047**: Source-level debugging information in executables
- **FR-v1.2-048**: Stack traces show COBOL paragraph names
- **FR-v1.2-049**: DISPLAY output shows variable contents formatted
- **FR-v1.2-050**: Compile-time warnings for deprecated constructs

---

### Migration Assessment

#### Codebase Analysis

- **FR-v1.2-051**: Scanner identifies all COBOL, JCL, copybook files
- **FR-v1.2-052**: Parser detects unsupported language features
- **FR-v1.2-053**: Dependency graph shows program/copybook relationships
- **FR-v1.2-054**: Summary report estimates compatibility percentage

#### Effort Estimation

- **FR-v1.2-055**: Complexity scoring based on feature usage
- **FR-v1.2-056**: Flagged items list specific incompatibilities
- **FR-v1.2-057**: Remediation suggestions for common issues

---

## Non-Functional Requirements

### Performance

- **NFR-v1.2-001**: DB2 batch queries execute within 3x of native DB2
- **NFR-v1.2-002**: CICS screen response < 500ms for simple transactions
- **NFR-v1.2-003**: PostgreSQL connection pool supports 100+ connections

### Compatibility

- **NFR-v1.2-004**: DB2 SQL Level 1 (SQL-86) fully supported
- **NFR-v1.2-005**: Common DB2 V12 syntax elements supported
- **NFR-v1.2-006**: CICS TS 5.x command-level API subset

### Reliability

- **NFR-v1.2-007**: Transaction atomicity preserved with PostgreSQL
- **NFR-v1.2-008**: Connection failures trigger automatic retry
- **NFR-v1.2-009**: CICS conversation state survives network blips

---

## Architecture Overview

### DB2 to PostgreSQL Architecture

```
┌─────────────────────────────────────────────────┐
│                COBOL Program                     │
│  ┌───────────────────────────────────────────┐  │
│  │  EXEC SQL SELECT ... INTO :HOST-VAR       │  │
│  └───────────────────────────────────────────┘  │
└─────────────────────────────────────────────────┘
                      │
                      ▼
┌─────────────────────────────────────────────────┐
│            SQL Preprocessor (zos-db2)           │
│  - Extract EXEC SQL blocks                      │
│  - Generate COBOL CALLs to runtime              │
│  - Create DBRM (database request module)        │
└─────────────────────────────────────────────────┘
                      │
                      ▼
┌─────────────────────────────────────────────────┐
│            DB2 Runtime Library                   │
│  - SQL syntax translation (DB2 → PostgreSQL)    │
│  - Connection pooling                           │
│  - SQLCA management                             │
│  - Host variable binding                        │
└─────────────────────────────────────────────────┘
                      │
                      ▼
┌─────────────────────────────────────────────────┐
│              PostgreSQL Database                 │
│  - Schema matches DB2 table definitions         │
│  - Indexes for performance                      │
│  - Transactions via standard SQL                │
└─────────────────────────────────────────────────┘
```

### CICS Architecture

```
┌─────────────────────────────────────────────────┐
│              CICS Transaction                    │
│  (COBOL program with EXEC CICS commands)        │
└─────────────────────────────────────────────────┘
                      │
                      ▼
┌─────────────────────────────────────────────────┐
│         CICS Command Processor (zos-cics)       │
│  - Command parsing and dispatch                 │
│  - Resource management                          │
│  - Exception handling                           │
└─────────────────────────────────────────────────┘
            │                   │
            ▼                   ▼
┌───────────────────┐  ┌───────────────────┐
│   BMS Processor   │  │   Data Manager    │
│  - Map compilation │  │  - VSAM access    │
│  - Screen I/O     │  │  - File control   │
│  - Field handling │  │  - Browsing       │
└───────────────────┘  └───────────────────┘
            │
            ▼
┌─────────────────────────────────────────────────┐
│           Terminal Interface                     │
│  ┌─────────┐  ┌─────────┐  ┌─────────┐         │
│  │ Console │  │  HTML   │  │  REST   │         │
│  │  Mode   │  │  Mode   │  │   API   │         │
│  └─────────┘  └─────────┘  └─────────┘         │
└─────────────────────────────────────────────────┘
```

---

## Crate Structure

### New Crates

| Crate | Purpose | Dependencies |
|-------|---------|--------------|
| zos-db2 | DB2 SQL preprocessing and runtime | zos-cobol, tokio-postgres |
| zos-cics | CICS command processor and runtime | zos-cobol, zos-dataset |
| zos-bms | BMS map compiler and screen handler | zos-cics |
| zos-migrate | Migration assessment tooling | zos-cobol, zos-jcl |

### Modified Crates

| Crate | Changes |
|-------|---------|
| zos-cobol | EXEC SQL/CICS preprocessing hooks |
| zos-clone | CLI commands for db2, cics, migrate |
| zos-jcl | DB2 utility recognition |

---

## Implementation Phases

### Phase 1: DB2 Core (Epics 21-22)
- SQL preprocessor
- Basic SQL statements
- PostgreSQL connection
- SQLCA handling

### Phase 2: DB2 Complete (Epics 23-24)
- Cursor operations
- Transaction control
- DCLGEN utility
- JCL integration

### Phase 3: CICS Foundation (Epics 25-26)
- Command processor
- Program control
- BMS basics
- VSAM operations

### Phase 4: CICS Terminal (Epic 27)
- Console mode
- HTML rendering
- REST API

### Phase 5: Assessment Tools (Epic 28)
- Codebase scanner
- Compatibility analyzer
- Report generator

---

## Risk Assessment

| Risk | Impact | Mitigation |
|------|--------|------------|
| DB2 SQL dialect complexity | High | Focus on common subset, document gaps |
| PostgreSQL performance | Medium | Optimize translation, add indexes |
| CICS state management | High | Start with stateless transactions |
| BMS screen complexity | Medium | Support common map features first |

---

## Success Criteria

### v1.2 Exit Criteria

- [ ] 80% of sample DB2/COBOL programs compile and run
- [ ] PostgreSQL batch performance within 3x of DB2
- [ ] 20+ EXEC CICS commands functional
- [ ] BMS screens display correctly in console mode
- [ ] Migration tool produces actionable reports
- [ ] Documentation complete for all features
- [ ] 5+ enterprise pilot users testing DB2/CICS

---

## Timeline Estimate

| Phase | Epics | Stories (est.) |
|-------|-------|----------------|
| DB2 Core | 21-22 | ~15 |
| DB2 Complete | 23-24 | ~12 |
| CICS Foundation | 25-26 | ~18 |
| CICS Terminal | 27 | ~8 |
| Assessment Tools | 28 | ~7 |
| **Total** | **8** | **~60** |
