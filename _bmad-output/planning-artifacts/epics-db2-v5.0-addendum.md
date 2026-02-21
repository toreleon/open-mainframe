---
version: 'v5.0'
planningGroup: 'PG-25'
technology: 'DB2 Utilities & BIND'
date: '2026-02-21'
status: 'complete'
action: 'EXTEND'
parentDocument: 'epics-db2-v3.0.md'
inputDocuments:
  - 'epics-db2-v3.0.md'
totalNewEpics: 2
totalNewStories: 12
---

# DB2 Utilities & BIND Addendum (v5.0)

## Overview

This addendum extends the v3.0 DB2 planning (Epics 300-310, focused on SQL preprocessing and runtime) with two new epics addressing DB2 utilities (BIND, DSNTEP2, DCLGEN) and advanced SQL features not covered in v3.0. The existing v3.0 epics cover the SQL precompiler pipeline; these epics add the operational tools.

---

## New Epics

| Epic | Title | Size | Stories | Phase |
|------|-------|------|---------|-------|
| SYS-114 | DB2 BIND Utility & Package Management | L | 7 | E |
| SYS-115 | DB2 Operational Utilities | M | 5 | E |

---

## SYS-114: DB2 BIND Utility & Package Management

**User Value:** DBRMs produced by the precompiler are bound into packages and plans, creating access plans that DB2 uses for SQL execution.

### SYS-114.1: BIND PACKAGE

**As a** DBA, **I want** BIND PACKAGE to create a DB2 package from a DBRM, **so that** SQL statements have optimized access plans.

**Acceptance Criteria:**
- Given a DBRM produced by the precompiler (SYS-112.6), when BIND PACKAGE is executed, then a package is created containing access plans for each SQL statement
- Given BIND PACKAGE with ISOLATION(CS) ACTION(REPLACE), when executed, then the package uses cursor stability isolation and replaces any existing version

### SYS-114.2: BIND PLAN

**As a** DBA, **I want** BIND PLAN to create an application plan from one or more packages, **so that** programs can execute their SQL.

**Acceptance Criteria:**
- Given BIND PLAN(MYPLAN) PKLIST(COL1.PKG1, COL1.PKG2), when executed, then a plan is created referencing the specified packages
- Given a program that CALLs DSNHLI at runtime, when the plan is active, then SQL statements are dispatched via the bound packages

### SYS-114.3: REBIND

**As a** DBA, **I want** REBIND to regenerate access plans after schema changes or statistics updates.

**Acceptance Criteria:**
- Given an index added to a table, when REBIND PACKAGE is executed, then the access plan is regenerated potentially using the new index
- Given REBIND PLAN, when executed, then all package references are refreshed

### SYS-114.4: FREE PACKAGE/PLAN

**As a** DBA, **I want** FREE to remove packages and plans.

**Acceptance Criteria:**
- Given FREE PACKAGE(COL1.PKG1), when executed, then the package is removed from the DB2 catalog
- Given a plan with no remaining packages, when queried, then it reports empty

### SYS-114.5: BIND Options

**As a** DBA, **I want** comprehensive BIND options for access plan behavior.

**Acceptance Criteria:**
- Given ISOLATION(RR/RS/CS/UR), when specified, then the package uses the specified isolation level
- Given VALIDATE(BIND/RUN), when BIND, then SQL objects are validated at bind time; when RUN, validation is deferred
- Given EXPLAIN(YES), when specified, then the PLAN_TABLE is populated with access path details

### SYS-114.6: DB2 Catalog Tables

**As a** DBA, **I want** DB2 catalog tables (SYSIBM.SYSPACKAGE, SYSIBM.SYSSTMT, SYSIBM.SYSPLAN) populated during BIND.

**Acceptance Criteria:**
- Given BIND PACKAGE, when completed, then SYSIBM.SYSPACKAGE contains the package metadata
- Given SYSIBM.SYSSTMT, when queried after BIND, then each SQL statement in the package is listed with its access path

### SYS-114.7: BIND Tests

**Acceptance Criteria:**
- Given precompile → DBRM → BIND PACKAGE → BIND PLAN → execute pipeline, when tested end-to-end, then SQL statements execute correctly
- Given `cargo test -p open-mainframe-db2` BIND tests, then all pass

---

## SYS-115: DB2 Operational Utilities

**User Value:** DBAs have essential tools for SQL development, schema management, and data operations.

### SYS-115.1: DSNTEP2 — Dynamic SQL Processor

**As a** DBA, **I want** DSNTEP2 to execute dynamic SQL statements from SYSIN input.

**Acceptance Criteria:**
- Given input `SELECT * FROM EMP WHERE DEPTNO = 'D01';`, when DSNTEP2 runs, then the result set is displayed with column headers
- Given multiple SQL statements separated by `;`, when processed, then each is executed sequentially with results

### SYS-115.2: DCLGEN — Declaration Generator

**As a** COBOL developer, **I want** DCLGEN to generate COBOL host variable declarations from a DB2 table definition.

**Acceptance Criteria:**
- Given table EMP with columns EMPNO CHAR(6), ENAME VARCHAR(30), SALARY DECIMAL(9,2), when DCLGEN runs, then a COBOL copybook with `01 DCLEMPLOYEE. 05 EMPNO PIC X(6). 05 ENAME PIC X(30). 05 SALARY PIC S9(7)V9(2) COMP-3.` is generated
- Given an INCLUDE statement in EXEC SQL, when the generated copybook is included, then host variable declarations are available

### SYS-115.3: LOAD Utility

**As a** DBA, **I want** LOAD to bulk-insert data into DB2 tables from sequential datasets.

**Acceptance Criteria:**
- Given a sequential dataset with delimited records and a LOAD control statement, when LOAD runs, then records are inserted into the target table
- Given REPLACE option, when specified, then existing data is deleted before loading

### SYS-115.4: UNLOAD Utility

**As a** DBA, **I want** UNLOAD to extract data from DB2 tables to sequential datasets.

**Acceptance Criteria:**
- Given `UNLOAD TABLESPACE MYDB.MYTS`, when executed, then all rows are written to a sequential dataset in delimited format
- Given WHERE clause filtering, when specified, then only matching rows are unloaded

### SYS-115.5: Utility Tests

**Acceptance Criteria:**
- Given DSNTEP2, DCLGEN, LOAD, and UNLOAD scenarios, when tested, then all utilities produce correct output
- Given `cargo test -p open-mainframe-db2` utility tests, then all pass

---

## Dependency Graph

```
v3.0 Epics (existing):
  300 → 301 → 302 → 303 → 304 → 305 → 306 → 307 → 308 → 309 → 310

v5.0 Addendum (new):
  SYS-114 depends on: 309 (Integration Testing), SYS-112 (DB2 Precompiler/DBRM)
  SYS-115 depends on: 302 (PostgreSQL Runtime)
```
