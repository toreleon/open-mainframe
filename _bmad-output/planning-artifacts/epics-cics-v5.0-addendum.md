---
version: 'v5.0'
planningGroup: 'PG-19'
technology: 'CICS BMS & Extensions'
date: '2026-02-21'
status: 'complete'
action: 'EXTEND'
parentDocument: 'epics-cics-v3.0.md'
inputDocuments:
  - 'epics-cics-v3.0.md'
  - 'architecture-cics-v3.0.md'
  - 'zos-complete-inventory.md (AREA-6)'
totalNewEpics: 3
totalNewStories: 18
---

# CICS Extensions Addendum (v5.0)

## Overview

This addendum extends the v3.0 CICS planning (Epics 200-209) with three new epics addressing critical gaps: web services, DOCUMENT commands, and system programming (INQUIRE/SET).

The existing v3.0 epics cover:
- Epic 200: Channels & Containers
- Epic 201: TD Queue Commands
- Epic 202: Persistent File Storage
- Epic 203: CONVERSE & Terminal
- Epic 204: EBCDIC Translation
- Epic 205: ASSIGN Command
- Epic 206: Preprocessor-to-Runtime Bridge
- Epic 207: Deadlock Detection
- Epic 208: Auxiliary TS Storage
- Epic 209: BMS Enhancements

---

## New Epics

| Epic | Title | Size | Stories | Phase |
|------|-------|------|---------|-------|
| SYS-116 | CICS Web Services & REST/JSON | L | 7 | C |
| CICS-210 | DOCUMENT Commands | M | 5 | C |
| CICS-211 | CICS System Programming (INQUIRE/SET) | L | 6 | C |

---

## SYS-116: CICS Web Services & REST/JSON

**User Value:** CICS programs can expose REST/JSON APIs and invoke external web services, bridging mainframe transactions to modern web architectures.

### SYS-116.1: WEB RECEIVE — Inbound HTTP Request

**As a** CICS developer, **I want** EXEC CICS WEB RECEIVE to parse inbound HTTP requests, **so that** CICS programs can handle web requests.

**Acceptance Criteria:**
- Given an HTTP POST to a CICS URI, when WEB RECEIVE is issued, then the request body, content type, and query string are available
- Given WEB RECEIVE with INTO(WS-BODY) MAXLENGTH(32000), when called, then the request body is placed in WS-BODY

### SYS-116.2: WEB SEND — Outbound HTTP Response

**As a** CICS developer, **I want** EXEC CICS WEB SEND to return HTTP responses, **so that** CICS programs can serve web content.

**Acceptance Criteria:**
- Given WEB SEND FROM(WS-JSON) MEDIATYPE('application/json') STATUSCODE(200), when called, then a 200 OK response with JSON body is returned
- Given WEB SEND STATUSCODE(404), when called, then a 404 Not Found response is returned

### SYS-116.3: WEB OPEN/CONVERSE/CLOSE — Outbound HTTP Client

**As a** CICS developer, **I want** CICS to invoke external REST APIs as an HTTP client.

**Acceptance Criteria:**
- Given WEB OPEN HOST('api.example.com') PORT(443) SCHEME(HTTPS), when called, then an HTTPS session is created
- Given WEB CONVERSE METHOD(POST) PATH('/api/data') BODY(WS-REQUEST), when called, then the response is received into WS-RESPONSE
- Given WEB CLOSE, when called, then the session is terminated

### SYS-116.4: JSON Parsing and Generation (TRANSFORM)

**As a** CICS developer, **I want** EXEC CICS TRANSFORM DATATOXML/XMLTODATA and JSON equivalents, **so that** data conversion between COBOL structures and JSON/XML is automatic.

**Acceptance Criteria:**
- Given a COBOL structure WS-EMPLOYEE and a JSON schema, when TRANSFORM DATATOJSON is called, then JSON output is generated matching the schema
- Given JSON input `{"empId":"12345","name":"Smith"}`, when TRANSFORM JSONTODATA is called, then WS-EMPLOYEE fields are populated

### SYS-116.5: URIMAP Configuration

**As a** CICS administrator, **I want** URIMAP resources to route inbound HTTP requests to CICS programs.

**Acceptance Criteria:**
- Given URIMAP(EMPAPI) with PATH('/api/employees/*') PROGRAM(EMPPROG) OPERATION(POST), when a POST to /api/employees/123 arrives, then EMPPROG is invoked

### SYS-116.6: PIPELINE Configuration

**As a** CICS administrator, **I want** PIPELINE resources for SOAP/XML web services.

**Acceptance Criteria:**
- Given a PIPELINE definition with a WSDL binding, when a SOAP request arrives, then the mapped CICS program is invoked and the response is formatted per WSDL

### SYS-116.7: Web Services Tests

**Acceptance Criteria:**
- Given REST and SOAP web service tests, when `cargo test -p open-mainframe-cics` web tests run, then all pass

---

## CICS-210: DOCUMENT Commands

**User Value:** CICS programs can build dynamic HTML/text documents from templates and data, enabling web page generation without external templating engines.

### CICS-210.1: DOCUMENT CREATE

**As a** CICS developer, **I want** DOCUMENT CREATE to initialize a new document from a template.

**Acceptance Criteria:**
- Given DOCUMENT CREATE DOCTOKEN(WS-TOKEN) TEMPLATE('EMPRPT'), when called, then a document is created from the template with placeholder markers

### CICS-210.2: DOCUMENT INSERT and SET

**As a** CICS developer, **I want** DOCUMENT INSERT and SET to add/modify document content.

**Acceptance Criteria:**
- Given DOCUMENT INSERT DOCTOKEN(WS-TOKEN) TEXT('Employee: ') AFTER, when called, then text is appended to the document
- Given DOCUMENT SET DOCTOKEN(WS-TOKEN) SYMBOL('EMPNAME') VALUE('Smith'), when called, then all occurrences of &EMPNAME; in the template are replaced with 'Smith'

### CICS-210.3: DOCUMENT RETRIEVE

**As a** CICS developer, **I want** DOCUMENT RETRIEVE to get the assembled document content.

**Acceptance Criteria:**
- Given DOCUMENT RETRIEVE DOCTOKEN(WS-TOKEN) INTO(WS-OUTPUT), when called, then the complete assembled document is placed in WS-OUTPUT

### CICS-210.4: DOCUMENT DELETE

**As a** CICS developer, **I want** DOCUMENT DELETE to release document resources.

**Acceptance Criteria:**
- Given DOCUMENT DELETE DOCTOKEN(WS-TOKEN), when called, then the document and all associated memory are freed

### CICS-210.5: Document Command Tests

**Acceptance Criteria:**
- Given document lifecycle tests (CREATE/INSERT/SET/RETRIEVE/DELETE), when run, then all pass

---

## CICS-211: CICS System Programming (INQUIRE/SET)

**User Value:** System programmers can query and modify CICS resource definitions at runtime, enabling dynamic system management.

### CICS-211.1: INQUIRE PROGRAM

**As a** system programmer, **I want** INQUIRE PROGRAM to query program attributes.

**Acceptance Criteria:**
- Given EXEC CICS INQUIRE PROGRAM('EMPPROG') STATUS(WS-STATUS) LANGUAGE(WS-LANG), when called, then the program's status (ENABLED/DISABLED) and language (COBOL/ASSEMBLER) are returned

### CICS-211.2: INQUIRE TRANSACTION

**As a** system programmer, **I want** INQUIRE TRANSACTION to query transaction attributes.

**Acceptance Criteria:**
- Given EXEC CICS INQUIRE TRANSACTION('EMPT') PROGRAM(WS-PROG) STATUS(WS-STATUS), when called, then the associated program and transaction status are returned

### CICS-211.3: INQUIRE FILE

**As a** system programmer, **I want** INQUIRE FILE to query file definition attributes.

**Acceptance Criteria:**
- Given EXEC CICS INQUIRE FILE('CUSTFILE') DSNAME(WS-DSN) OPENSTATUS(WS-OPEN), when called, then the dataset name and open status are returned

### CICS-211.4: SET PROGRAM/TRANSACTION/FILE

**As a** system programmer, **I want** SET commands to modify resource attributes at runtime.

**Acceptance Criteria:**
- Given EXEC CICS SET PROGRAM('EMPPROG') NEWCOPY, when called, then the next LINK/XCTL loads a fresh copy of the program
- Given EXEC CICS SET FILE('CUSTFILE') DISABLED, when called, then the file is disabled and subsequent READ attempts fail with DISABLED condition

### CICS-211.5: INQUIRE/SET SYSTEM

**As a** system programmer, **I want** INQUIRE/SET SYSTEM for global CICS region attributes.

**Acceptance Criteria:**
- Given EXEC CICS INQUIRE SYSTEM MAXTASKS(WS-MAX), when called, then the maximum concurrent task limit is returned
- Given EXEC CICS SET SYSTEM MAXTASKS(200), when called, then the task limit is updated

### CICS-211.6: System Programming Tests

**Acceptance Criteria:**
- Given INQUIRE and SET command tests for PROGRAM, TRANSACTION, FILE, and SYSTEM, when run, then all pass

---

## Dependency Graph

```
v3.0 Epics (existing):
  200 → 201 → 202 → 203 → 204 → 205 → 206 → 207 → 208 → 209

v5.0 Addendum (new):
  SYS-116 depends on: 203 (CONVERSE), 206 (Preprocessor-Runtime Bridge)
  CICS-210 depends on: 206 (Preprocessor-Runtime Bridge)
  CICS-211 depends on: 205 (ASSIGN), 206 (Preprocessor-Runtime Bridge)
```

## New FR Coverage

| FR | Stories |
|----|---------|
| FR-CICS-EXT-001 (WEB RECEIVE) | SYS-116.1 |
| FR-CICS-EXT-002 (WEB SEND) | SYS-116.2 |
| FR-CICS-EXT-003 (HTTP Client) | SYS-116.3 |
| FR-CICS-EXT-004 (JSON/XML Transform) | SYS-116.4 |
| FR-CICS-EXT-005 (URIMAP) | SYS-116.5 |
| FR-CICS-EXT-006 (DOCUMENT commands) | CICS-210.1, CICS-210.2, CICS-210.3, CICS-210.4 |
| FR-CICS-EXT-007 (INQUIRE) | CICS-211.1, CICS-211.2, CICS-211.3, CICS-211.5 |
| FR-CICS-EXT-008 (SET) | CICS-211.4, CICS-211.5 |
