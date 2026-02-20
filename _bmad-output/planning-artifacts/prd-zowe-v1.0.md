---
stepsCompleted: [step-01-init, step-02-discovery, step-03-success, step-04-journeys, step-05-domain, step-06-innovation, step-07-project-type, step-08-scoping, step-09-functional, step-10-nonfunctional, step-11-polish]
inputDocuments: [research-zowe-integration.md]
workflowType: 'prd'
classification:
  projectType: api_service
  domain: enterprise_legacy_systems
  complexity: enterprise
  projectContext: brownfield
date: 2026-02-20
---

# Product Requirements Document — Zowe Integration for OpenMainframe

**Author:** Tore
**Date:** 2026-02-20
**Input:** research-zowe-integration.md (BMAD Technical Research, 1075 lines, 6 steps)

---

## Executive Summary

### Vision

Enable standard Zowe tooling (CLI, Explorer, API ML) to connect to OpenMainframe as if it were a real z/OS instance, by implementing a z/OSMF-compatible REST API layer that bridges Zowe's expected HTTPS/JSON interface to OpenMainframe's existing Rust subsystem crates.

### Product Differentiator

| Aspect | Real z/OSMF on z/OS | OpenMainframe z/OSMF |
|---|---|---|
| Platform | z/OS only | Linux, Docker, Kubernetes |
| Cost | IBM licensing required | Open source (Apache-2.0) |
| Language | Java/z/OS native | Rust (memory-safe, fast) |
| Deployment | LPAR provisioning | `docker run` or `cargo build` |
| Integration | z/OS subsystems | Same API surface, Linux-native backends |

### Target Users

**Primary:** Mainframe developers using Zowe CLI and VS Code with Zowe Explorer for daily z/OS development who want a local or cloud-hosted OpenMainframe target.

**Secondary:** DevOps engineers integrating mainframe workflows into CI/CD pipelines via Zowe CLI. Platform teams deploying OpenMainframe in Kubernetes with Zowe API ML as the service gateway.

### MVP Focus

z/OSMF REST server with authentication, dataset operations, and job management — enough for `zowe zos-files` and `zowe zos-jobs` commands to work end-to-end.

---

## Success Criteria

### User Success

**Mainframe Developer (Zowe CLI user):**

| Metric | Target | Measurement |
|---|---|---|
| `zowe zos-files ls ds` response time | < 200ms for 100-item dataset list | Benchmark via Zowe CLI `--response-format-json` |
| `zowe zos-jobs submit` roundtrip | < 500ms to return job ID | Time from CLI invocation to job ID returned |
| `zowe zos-tso issue cmd "TIME"` | < 300ms for command output | CLI timing |
| Zowe Explorer dataset tree load | < 1s for initial tree population | VS Code extension timing |
| Zero custom plugins required | Standard `zowe zos-*` commands work | No `zowe plugins install` needed |

**DevOps Engineer (CI/CD pipeline user):**

| Metric | Target | Measurement |
|---|---|---|
| Concurrent CLI sessions | 50+ simultaneous connections | Load test with parallel `zowe` invocations |
| API uptime | 99.9% availability | Health check endpoint monitoring |
| Authentication token validity | Configurable TTL (default 8 hours) | JWT `exp` claim verification |

### Business Success

| Metric | Target | Measurement |
|---|---|---|
| Zowe Conformance achievable | Pass self-certification checklist | Conformance test evaluation guide |
| Standard Zowe CLI compatibility | 100% of `zos-files`, `zos-jobs`, `zos-tso` commands | Integration test suite |
| Community adoption | Documented in Zowe ecosystem | Listed in Zowe API Catalog |

### Technical Success

| Metric | Target | Measurement |
|---|---|---|
| z/OSMF endpoint coverage | 40+ endpoints across 6 service categories | Endpoint inventory checklist |
| `cargo test` pass rate | 100% on all z/OSMF handler tests | CI pipeline |
| `cargo clippy` | Zero warnings | CI pipeline |
| JSON response format accuracy | Byte-compatible with z/OSMF for core fields | Diff against real z/OSMF response captures |

---

## Product Scope

### MVP (Phase F.1: ZOW-100 through ZOW-103)

- z/OSMF REST server foundation (Axum, TLS, CORS, CSRF)
- `/zosmf/info` system information endpoint
- Authentication: Basic Auth → JWT token, session management
- Dataset REST API: list, read, write, create, delete datasets and PDS members
- Jobs REST API: list, submit, status, spool, hold/release/cancel/purge

**Validates:** Zowe CLI `zos-files` and `zos-jobs` commands work against OpenMainframe.

### Growth (Phase F.2: ZOW-104 through ZOW-107)

- TSO REST API: stateless command issue, address space lifecycle
- Console REST API: MVS command issue, solicited response matching
- USS file REST API: host filesystem mapping with directory/file operations
- TN3270E server mode: accept terminal connections for ISPF/TSO

**Validates:** Zowe Explorer full feature set works (datasets + USS + jobs + terminal).

### Vision (Phase F.3: ZOW-108 through ZOW-109)

- Zowe API ML integration: Eureka dynamic registration, SSO, API Catalog
- End-to-end conformance testing and certification
- Documentation for connecting Zowe to OpenMainframe

**Validates:** OpenMainframe appears in Zowe API Catalog; Zowe Conformance certification achieved.

---

## User Journeys

### Journey 1: Marcus — Mainframe Developer Using Zowe CLI

Marcus works on COBOL applications and uses Zowe CLI daily. His team has deployed OpenMainframe on a Linux server to reduce mainframe costs for development.

1. Marcus runs `zowe config init` and enters the OpenMainframe host/port
2. He runs `zowe zos-files ls ds "MARCUS.*"` and sees his datasets listed
3. He downloads a COBOL source member: `zowe zos-files download ds "MARCUS.COBOL(PAYROLL)" -f payroll.cbl`
4. He edits the file locally, then uploads: `zowe zos-files upload ftds payroll.cbl "MARCUS.COBOL(PAYROLL)"`
5. He submits JCL: `zowe zos-jobs submit lf ./compile.jcl` and receives job ID JOB00042
6. He checks status: `zowe zos-jobs view jsbj JOB00042` — sees CC 0000
7. He views spool output: `zowe zos-jobs view sfbi JOB00042 2` — sees compiler listing

Marcus never knows (or cares) that the backend is OpenMainframe instead of z/OS.

### Journey 2: Sofia — VS Code Developer Using Zowe Explorer

Sofia uses VS Code with Zowe Explorer for visual dataset and job management.

1. Sofia opens VS Code and creates a Zowe Explorer connection profile pointing at OpenMainframe
2. She expands the "DATA SETS" tree and sees her high-level qualifiers
3. She expands a PDS and sees member list with ISPF stats (modified date, size)
4. She clicks a member to open it in the editor, makes changes, saves — it writes back to OpenMainframe
5. She right-clicks a JCL member and selects "Submit Job"
6. She switches to the "JOBS" tree, sees her job running, then completing with RC=0
7. She expands the job to see spool files, clicks SYSOUT to view output

### Journey 3: Alex — DevOps Engineer Building CI/CD Pipeline

Alex integrates OpenMainframe into a Jenkins pipeline using Zowe CLI.

1. Alex creates a `zowe.config.json` with OpenMainframe connection details and commits it to the repo
2. The Jenkins pipeline runs `zowe zos-files upload ftds` to deploy COBOL source
3. Pipeline submits compile JCL via `zowe zos-jobs submit` and polls for completion
4. On success, pipeline submits test JCL and verifies RC=0000
5. Pipeline collects spool output as build artifacts
6. All of this runs against OpenMainframe in a Docker container in the CI environment

---

## Domain Requirements

### Mainframe Data Conventions

- **Dataset names:** 1-44 characters, segments separated by dots, each segment 1-8 alphanumeric characters, first character alphabetic or national (@, #, $)
- **Member names:** 1-8 characters, same character rules as dataset name segments
- **Job names:** 1-8 characters, first character alphabetic or national
- **Job IDs:** Format `JOBnnnnn` or `TSUnnnnn` or `STCnnnnn` (JES2 convention)
- **RACF user IDs:** 1-8 characters, case-insensitive (stored uppercase)
- **Record formats:** F, FB, V, VB, U — with LRECL and BLKSIZE constraints
- **EBCDIC encoding:** z/OSMF REST API returns text in the system's configured code page; OpenMainframe must handle EBCDIC↔UTF-8 translation via `open-mainframe-encoding`

### z/OSMF API Compliance

- All responses must include `Content-Type: application/json` header
- Error responses must use z/OSMF error JSON format: `{"rc": N, "reason": N, "category": N, "message": "..."}`
- `X-CSRF-ZOSMF-HEADER` must be required on all mutating requests (PUT, POST, DELETE)
- Dataset names in URLs must support special characters via URL encoding
- String numeric fields must be returned as strings, not integers (e.g., `"blksz": "800"` not `"blksz": 800`)

### Security Model

- Authentication via RACF: `AuthService::authenticate(userid, password) → AuthResult`
- Authorization via SAF router: `SafRouter::check_access(userid, resource_class, resource_name, access_level)`
- Dataset access governed by dataset profiles in RACF
- Token management: JWT with configurable expiry, in-memory session store

---

## Innovation Analysis

### Competitive Position

OpenMainframe with Zowe integration is unique in the market:

| Feature | OpenMainframe | Micro Focus | IBM z/OS |
|---|---|---|---|
| z/OSMF REST API | Rust-native implementation | Not supported | Native |
| Zowe CLI compatible | Yes (standard commands) | No | Yes |
| Open source | Apache-2.0 | Proprietary | Proprietary |
| Runs on Linux/Docker/K8s | Yes | Yes (partial) | No (z/OS only) |
| Cost | Free | $100K+ licensing | $1M+ MIPS-based |

No other open-source project implements z/OSMF REST APIs. This makes OpenMainframe the only free, Linux-native target for the entire Zowe ecosystem.

---

## Project Type Requirements (API Service)

### REST API Conventions

- Base path: `/zosmf/` (matching IBM z/OSMF URL structure exactly)
- HTTPS required: TLS 1.2+ via rustls
- JSON request/response bodies with `Content-Type: application/json`
- HTTP methods: GET (read), PUT (create/update/submit), POST (create/allocate), DELETE (remove/purge)
- Authentication: HTTP Basic Auth header or Bearer JWT token or LTPA2 cookie
- Error responses: HTTP status codes (200, 201, 400, 401, 403, 404, 500) with z/OSMF JSON error body

### Configuration

- Port: Configurable (default 10443 for HTTPS)
- TLS certificates: PEM file paths configurable
- USS root directory: Configurable host filesystem path
- RACF database: Path to OpenMainframe RACF database
- Token TTL: Configurable JWT expiration (default 28800 seconds / 8 hours)

### Observability

- Integration with existing `open-mainframe-deploy` metrics (Prometheus)
- Request logging via `tracing` (request method, path, status, duration)
- Health check endpoint: `GET /zosmf/info` (doubles as connectivity test)

---

## Functional Requirements

### FR-ZOW-001 through FR-ZOW-009: Server Foundation

| ID | Requirement | Acceptance Criteria | Source |
|---|---|---|---|
| FR-ZOW-001 | The system serves HTTPS requests on a configurable port | `curl -k https://localhost:10443/zosmf/info` returns 200 | Research §2.6 |
| FR-ZOW-002 | The system returns z/OSMF system info at `GET /zosmf/info` | Response contains `api_version`, `zosmf_version`, `zosmf_hostname`, `plugins` array | Research §3.8 |
| FR-ZOW-003 | The system validates `X-CSRF-ZOSMF-HEADER` on PUT/POST/DELETE requests | Requests without the header return 403 Forbidden | Research §3.2 |
| FR-ZOW-004 | The system supports TLS 1.2+ via configurable PEM certificate/key files | `openssl s_client -connect host:port` succeeds with TLS 1.2+ | Research §2.6 |
| FR-ZOW-005 | The system returns z/OSMF-format error responses for all error conditions | Error JSON contains `rc`, `reason`, `category`, `message` fields | Research §2.3 |
| FR-ZOW-006 | The system supports CORS headers for cross-origin requests | `OPTIONS` preflight returns `Access-Control-Allow-Origin` | Research §2.6 |
| FR-ZOW-007 | The system logs all requests with method, path, status code, and duration | Log entries visible in stdout with tracing | Research §5.1 |
| FR-ZOW-008 | The system provides a configurable server (port, TLS certs, USS root, token TTL) | TOML/YAML config file loaded at startup | Research §5.3 |
| FR-ZOW-009 | The system runs as a workspace member crate `open-mainframe-zosmf` | `cargo build -p open-mainframe-zosmf` succeeds | Research §4.3 |

### FR-ZOW-010 through FR-ZOW-014: Authentication

| ID | Requirement | Acceptance Criteria | Source |
|---|---|---|---|
| FR-ZOW-010 | The system authenticates users via HTTP Basic Auth against RACF | `POST /zosmf/services/authenticate` with valid `Authorization: Basic` header returns 200 + token | Research §3.7 |
| FR-ZOW-011 | The system generates JWT tokens upon successful authentication | Response sets `jwtToken` cookie and returns JSON with token | Research §2.5 |
| FR-ZOW-012 | The system validates JWT tokens on subsequent requests | Requests with valid `Authorization: Bearer` or `jwtToken` cookie are authenticated | Research §2.5 |
| FR-ZOW-013 | The system invalidates tokens on logout | `DELETE /zosmf/services/authenticate` invalidates the session | Research §3.7 |
| FR-ZOW-014 | The system rejects expired JWT tokens with 401 Unauthorized | Requests with expired tokens return 401 | Research §2.5 |

### FR-ZOW-020 through FR-ZOW-030: Dataset Operations

| ID | Requirement | Acceptance Criteria | Source |
|---|---|---|---|
| FR-ZOW-020 | List datasets matching a pattern | `GET /zosmf/restfiles/ds?dslevel=HLQ.*` returns JSON array with `dsname`, `dsorg`, `recfm`, `lrecl`, `blksz` | Research §3.2 |
| FR-ZOW-021 | List PDS/PDSE members | `GET /zosmf/restfiles/ds/HLQ.PDS/member` returns JSON array with member names and ISPF stats | Research §3.2 |
| FR-ZOW-022 | Read sequential dataset content | `GET /zosmf/restfiles/ds/HLQ.SEQ` returns dataset content as text | Research §3.2 |
| FR-ZOW-023 | Read PDS member content | `GET /zosmf/restfiles/ds/HLQ.PDS(MEMBER)` returns member content as text | Research §3.2 |
| FR-ZOW-024 | Write sequential dataset content | `PUT /zosmf/restfiles/ds/HLQ.SEQ` with body text writes to dataset | Research §3.2 |
| FR-ZOW-025 | Write PDS member content | `PUT /zosmf/restfiles/ds/HLQ.PDS(MEMBER)` with body text writes to member | Research §3.2 |
| FR-ZOW-026 | Create a new dataset | `POST /zosmf/restfiles/ds/HLQ.NEW` with JSON allocation params creates the dataset | Research §3.2 |
| FR-ZOW-027 | Delete a dataset | `DELETE /zosmf/restfiles/ds/HLQ.OLD` removes the dataset from catalog | Research §3.2 |
| FR-ZOW-028 | Delete a PDS member | `DELETE /zosmf/restfiles/ds/HLQ.PDS(MEMBER)` removes the member | Research §3.2 |
| FR-ZOW-029 | Support `X-IBM-Max-Items` pagination header | List operations limit results when header is present | Research §3.2 |
| FR-ZOW-030 | Support `X-IBM-Attributes` to control returned fields | `base` returns minimal fields; `vol` adds volume info | Research §3.2 |

### FR-ZOW-040 through FR-ZOW-049: Job Operations

| ID | Requirement | Acceptance Criteria | Source |
|---|---|---|---|
| FR-ZOW-040 | List jobs by owner, prefix, or job ID | `GET /zosmf/restjobs/jobs?owner=USER&prefix=MY*` returns JSON job array | Research §3.3 |
| FR-ZOW-041 | Get job status | `GET /zosmf/restjobs/jobs/MYJOB/JOB00042` returns JSON with `status`, `retcode`, `phase` | Research §3.3 |
| FR-ZOW-042 | Submit JCL from request body | `PUT /zosmf/restjobs/jobs` with JCL body returns 201 + JSON with `jobid` | Research §3.3 |
| FR-ZOW-043 | List spool files for a job | `GET /zosmf/restjobs/jobs/MYJOB/JOB00042/files` returns JSON array of spool DDs | Research §3.3 |
| FR-ZOW-044 | Read spool file content | `GET /zosmf/restjobs/jobs/MYJOB/JOB00042/files/2/records` returns spool text | Research §3.3 |
| FR-ZOW-045 | Hold a job | `PUT .../MYJOB/JOB00042` with `{"request":"hold"}` returns feedback JSON | Research §3.3 |
| FR-ZOW-046 | Release a held job | `PUT .../MYJOB/JOB00042` with `{"request":"release"}` returns feedback JSON | Research §3.3 |
| FR-ZOW-047 | Cancel a running job | `PUT .../MYJOB/JOB00042` with `{"request":"cancel"}` returns feedback JSON | Research §3.3 |
| FR-ZOW-048 | Purge job and output | `DELETE /zosmf/restjobs/jobs/MYJOB/JOB00042` removes job and spool | Research §3.3 |
| FR-ZOW-049 | Job status uses z/OSMF status strings | `status` field returns `"INPUT"`, `"ACTIVE"`, or `"OUTPUT"` | Research §3.3 |

### FR-ZOW-050 through FR-ZOW-055: TSO Operations

| ID | Requirement | Acceptance Criteria | Source |
|---|---|---|---|
| FR-ZOW-050 | Issue TSO command (stateless) | `POST /zosmf/tsoApp/tso` with command body returns output JSON | Research §3.4 |
| FR-ZOW-051 | Start TSO address space | `POST /zosmf/tsoApp/tso/{key}` creates session, returns servlet key | Research §3.4 |
| FR-ZOW-052 | Send command to TSO address space | `PUT /zosmf/tsoApp/tso/{key}` with command returns response | Research §3.4 |
| FR-ZOW-053 | Receive TSO address space response | `GET /zosmf/tsoApp/tso/{key}` returns buffered output | Research §3.4 |
| FR-ZOW-054 | Stop TSO address space | `DELETE /zosmf/tsoApp/tso/{key}` destroys session | Research §3.4 |
| FR-ZOW-055 | Servlet key uniquely identifies session | Each started address space gets a unique key like `USER-123-abc` | Research §3.4 |

### FR-ZOW-060 through FR-ZOW-062: Console Operations

| ID | Requirement | Acceptance Criteria | Source |
|---|---|---|---|
| FR-ZOW-060 | Issue MVS console command | `PUT /zosmf/restconsoles/consoles/defcn` with `{"cmd":"D A,L"}` returns command response | Research §3.5 |
| FR-ZOW-061 | Match solicited response by keyword | `sol-key` parameter filters response to matching message IDs | Research §3.5 |
| FR-ZOW-062 | Support async command execution | `"async":"Y"` returns immediately with polling URL | Research §3.5 |

### FR-ZOW-070 through FR-ZOW-075: USS File Operations

| ID | Requirement | Acceptance Criteria | Source |
|---|---|---|---|
| FR-ZOW-070 | List USS directory contents | `GET /zosmf/restfiles/fs?path=/u/user` returns JSON with file names, sizes, permissions | Research §3.6 |
| FR-ZOW-071 | Read USS file content | `GET /zosmf/restfiles/fs/u/user/file.txt` returns file content | Research §3.6 |
| FR-ZOW-072 | Write USS file content | `PUT /zosmf/restfiles/fs/u/user/file.txt` with body creates/overwrites file | Research §3.6 |
| FR-ZOW-073 | Delete USS file or directory | `DELETE /zosmf/restfiles/fs/u/user/file.txt` removes the file | Research §3.6 |
| FR-ZOW-074 | Create USS directory | `POST /zosmf/restfiles/fs/u/user/newdir` with `{"type":"mkdir"}` creates directory | Research §3.6 |
| FR-ZOW-075 | Map USS paths to host filesystem | Configurable root directory maps `/u/` to host path (e.g., `/opt/openmainframe/uss/`) | Research §3.6 |

### FR-ZOW-080 through FR-ZOW-082: TN3270E Server

| ID | Requirement | Acceptance Criteria | Source |
|---|---|---|---|
| FR-ZOW-080 | Accept TN3270E TCP connections | Server listens on configurable port (default 23) for TN3270E negotiation | Research §3.9 |
| FR-ZOW-081 | Map TN3270E sessions to TSO/ISPF backends | Each connection gets a `TsoSession` with ISPF panel rendering | Research §3.9 |
| FR-ZOW-082 | Support multiple concurrent terminal sessions | At least 10 simultaneous TN3270E connections | Research §3.9 |

### FR-ZOW-090 through FR-ZOW-093: API ML Integration

| ID | Requirement | Acceptance Criteria | Source |
|---|---|---|---|
| FR-ZOW-090 | Generate static API ML definition YAML | CLI command produces valid Zowe API ML static definition file | Research §5.4 |
| FR-ZOW-091 | Register dynamically with Eureka Discovery Service | POST to Eureka `/eureka/apps/{serviceId}` succeeds | Research §2.7 |
| FR-ZOW-092 | Send heartbeats to maintain registration | PUT heartbeat every 30 seconds; registration survives 90s without | Research §2.7 |
| FR-ZOW-093 | Deregister on graceful shutdown | DELETE from Eureka on SIGTERM/SIGINT | Research §2.7 |

---

## Non-Functional Requirements

### Performance

| ID | Requirement | Target | Measurement |
|---|---|---|---|
| NFR-ZOW-001 | Dataset list response time | < 200ms for 100 items | Load test with `zowe zos-files ls ds` |
| NFR-ZOW-002 | Job submit response time | < 500ms to return job ID | Load test with `zowe zos-jobs submit` |
| NFR-ZOW-003 | Concurrent connections | 50+ simultaneous sessions | Stress test with parallel CLI invocations |
| NFR-ZOW-004 | Memory usage per session | < 10MB per authenticated session | Process monitoring under load |
| NFR-ZOW-005 | TLS handshake time | < 50ms | Benchmark with `openssl s_time` |

### Security

| ID | Requirement | Target | Measurement |
|---|---|---|---|
| NFR-ZOW-010 | TLS version | TLS 1.2+ only (no TLS 1.0/1.1) | `nmap --script ssl-enum-ciphers` |
| NFR-ZOW-011 | JWT token expiry | Configurable, default 8 hours | Token decode and verify `exp` claim |
| NFR-ZOW-012 | CSRF protection | All mutating requests require `X-CSRF-ZOSMF-HEADER` | Automated test: POST without header → 403 |
| NFR-ZOW-013 | Password not logged | Credentials never appear in logs or error messages | Log audit |
| NFR-ZOW-014 | RACF authorization on every request | Dataset/job access checked via SAF router | Attempt access to unauthorized resource → 403 |

### Compatibility

| ID | Requirement | Target | Measurement |
|---|---|---|---|
| NFR-ZOW-020 | Zowe CLI V3 LTS compatibility | All `zos-files`, `zos-jobs`, `zos-tso` commands work | Integration test suite |
| NFR-ZOW-021 | Zowe Explorer V3 compatibility | Dataset, USS, Job tree views populate correctly | Manual VS Code testing |
| NFR-ZOW-022 | z/OSMF JSON field naming | Exact field names match IBM spec (hyphenated fields like `files-url`) | JSON diff against spec |
| NFR-ZOW-023 | HTTP status codes match z/OSMF | 200, 201, 400, 401, 403, 404, 500 used correctly | Automated test suite |

### Reliability

| ID | Requirement | Target | Measurement |
|---|---|---|---|
| NFR-ZOW-030 | Graceful error handling | No panics on malformed requests | Fuzz testing with invalid inputs |
| NFR-ZOW-031 | Connection draining on shutdown | In-flight requests complete before process exit | SIGTERM during load test |
| NFR-ZOW-032 | Session cleanup on disconnect | TSO sessions freed when client disconnects | Session count monitoring |

### Code Quality

| ID | Requirement | Target | Measurement |
|---|---|---|---|
| NFR-ZOW-040 | Rust MSRV | 1.82 (matching workspace) | `cargo check` with toolchain 1.82 |
| NFR-ZOW-041 | No unsafe code | `#![forbid(unsafe_code)]` in crate root | Compilation check |
| NFR-ZOW-042 | Clippy clean | Zero warnings with workspace clippy config | `cargo clippy` |
| NFR-ZOW-043 | Test coverage | Unit tests for every REST handler | `cargo test -p open-mainframe-zosmf` |

---

*PRD completed 2026-02-20. All 11 steps executed. Ready for Architecture phase.*
