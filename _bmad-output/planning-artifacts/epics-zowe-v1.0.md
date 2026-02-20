---
stepsCompleted: [1, 2, 3, 4]
inputDocuments: [prd-zowe-v1.0.md, architecture-zowe-v1.0.md]
workflowType: 'epics'
project_name: 'OpenMainframe'
user_name: 'Tore'
date: '2026-02-20'
status: 'complete'
completedAt: '2026-02-20'
totalEpics: 10
totalStories: 62
frCoverage: '47/47'
nfrCoverage: '17/17'
---

# Zowe Integration — Epic Breakdown

## Overview

This document provides the complete epic and story breakdown for the Zowe Integration feature of OpenMainframe, decomposing the requirements from the Zowe PRD (prd-zowe-v1.0.md) and Architecture (architecture-zowe-v1.0.md) into implementable stories.

## Requirements Inventory

### Functional Requirements

**Server Foundation (FR-ZOW-001–009)**
- FR-ZOW-001: HTTPS server on configurable port
- FR-ZOW-002: GET /zosmf/info returns system info
- FR-ZOW-003: X-CSRF-ZOSMF-HEADER validation on PUT/POST/DELETE
- FR-ZOW-004: TLS 1.2+ via PEM certificate/key files
- FR-ZOW-005: z/OSMF-format error responses
- FR-ZOW-006: CORS headers for cross-origin requests
- FR-ZOW-007: Request logging (method, path, status, duration)
- FR-ZOW-008: TOML config file (port, TLS, USS root, token TTL)
- FR-ZOW-009: Workspace member crate `open-mainframe-zosmf`

**Authentication (FR-ZOW-010–014)**
- FR-ZOW-010: HTTP Basic Auth against RACF
- FR-ZOW-011: JWT token generation on login
- FR-ZOW-012: JWT token validation on requests
- FR-ZOW-013: Token invalidation on logout
- FR-ZOW-014: Expired token rejection (401)

**Datasets (FR-ZOW-020–030)**
- FR-ZOW-020: List datasets by pattern
- FR-ZOW-021: List PDS/PDSE members
- FR-ZOW-022: Read sequential dataset content
- FR-ZOW-023: Read PDS member content
- FR-ZOW-024: Write sequential dataset content
- FR-ZOW-025: Write PDS member content
- FR-ZOW-026: Create new dataset
- FR-ZOW-027: Delete dataset
- FR-ZOW-028: Delete PDS member
- FR-ZOW-029: X-IBM-Max-Items pagination
- FR-ZOW-030: X-IBM-Attributes field filtering

**Jobs (FR-ZOW-040–049)**
- FR-ZOW-040: List jobs by owner/prefix/ID
- FR-ZOW-041: Get job status
- FR-ZOW-042: Submit JCL from request body
- FR-ZOW-043: List spool files
- FR-ZOW-044: Read spool file content
- FR-ZOW-045: Hold a job
- FR-ZOW-046: Release a held job
- FR-ZOW-047: Cancel a running job
- FR-ZOW-048: Purge job and output
- FR-ZOW-049: z/OSMF status strings (INPUT/ACTIVE/OUTPUT)

**TSO (FR-ZOW-050–055)**
- FR-ZOW-050: Stateless TSO command issue
- FR-ZOW-051: Start TSO address space
- FR-ZOW-052: Send command to TSO session
- FR-ZOW-053: Receive TSO session response
- FR-ZOW-054: Stop TSO address space
- FR-ZOW-055: Unique servlet key per session

**Console (FR-ZOW-060–062)**
- FR-ZOW-060: Issue MVS console command
- FR-ZOW-061: Match solicited response by keyword
- FR-ZOW-062: Async command execution with polling

**USS Files (FR-ZOW-070–075)**
- FR-ZOW-070: List USS directory contents
- FR-ZOW-071: Read USS file content
- FR-ZOW-072: Write USS file content
- FR-ZOW-073: Delete USS file or directory
- FR-ZOW-074: Create USS directory
- FR-ZOW-075: Map USS paths to host filesystem

**TN3270E (FR-ZOW-080–082)**
- FR-ZOW-080: Accept TN3270E TCP connections
- FR-ZOW-081: Map TN3270E sessions to TSO/ISPF
- FR-ZOW-082: Multiple concurrent terminal sessions

**API ML (FR-ZOW-090–093)**
- FR-ZOW-090: Generate static API ML definition YAML
- FR-ZOW-091: Dynamic Eureka registration
- FR-ZOW-092: Heartbeat to maintain registration
- FR-ZOW-093: Deregister on graceful shutdown

### Non-Functional Requirements

**Performance (NFR-ZOW-001–005)**
- NFR-ZOW-001: Dataset list < 200ms for 100 items
- NFR-ZOW-002: Job submit < 500ms to return job ID
- NFR-ZOW-003: 50+ concurrent connections
- NFR-ZOW-004: < 10MB memory per session
- NFR-ZOW-005: TLS handshake < 50ms

**Security (NFR-ZOW-010–014)**
- NFR-ZOW-010: TLS 1.2+ only
- NFR-ZOW-011: Configurable JWT expiry (default 8h)
- NFR-ZOW-012: CSRF protection on mutating requests
- NFR-ZOW-013: Credentials never in logs
- NFR-ZOW-014: RACF authorization on every request

**Compatibility (NFR-ZOW-020–023)**
- NFR-ZOW-020: Zowe CLI V3 LTS compatibility
- NFR-ZOW-021: Zowe Explorer V3 compatibility
- NFR-ZOW-022: Exact z/OSMF JSON field naming
- NFR-ZOW-023: Correct HTTP status codes

**Reliability (NFR-ZOW-030–032)**
- NFR-ZOW-030: No panics on malformed requests
- NFR-ZOW-031: Connection draining on shutdown
- NFR-ZOW-032: Session cleanup on disconnect

**Code Quality (NFR-ZOW-040–043)**
- NFR-ZOW-040: MSRV 1.82
- NFR-ZOW-041: `#![forbid(unsafe_code)]`
- NFR-ZOW-042: Zero clippy warnings
- NFR-ZOW-043: Unit tests for every handler

### FR Coverage Map

| Category | FRs | Architecture Component | Crate |
|----------|-----|----------------------|-------|
| Server Foundation | FR-ZOW-001–009 | server.rs, config.rs, middleware/* | open-mainframe-zosmf |
| Authentication | FR-ZOW-010–014 | middleware/auth.rs, handlers/authenticate.rs | open-mainframe-zosmf |
| Datasets | FR-ZOW-020–030 | handlers/datasets.rs, types/datasets.rs | open-mainframe-zosmf |
| Jobs | FR-ZOW-040–049 | handlers/jobs.rs, types/jobs.rs | open-mainframe-zosmf |
| TSO | FR-ZOW-050–055 | handlers/tso.rs, types/tso.rs | open-mainframe-zosmf |
| Console | FR-ZOW-060–062 | handlers/console.rs, types/console.rs | open-mainframe-zosmf |
| USS Files | FR-ZOW-070–075 | handlers/files.rs | open-mainframe-zosmf |
| TN3270E | FR-ZOW-080–082 | tn3270_server.rs (or open-mainframe-tui extension) | open-mainframe-zosmf / open-mainframe-tui |
| API ML | FR-ZOW-090–093 | eureka.rs | open-mainframe-zosmf |

## Epic List

| Epic | Title | Stories | Key FRs | Key NFRs | Dependencies |
|------|-------|---------|---------|----------|--------------|
| ZOW-100 | z/OSMF Server Foundation & System Info | 7 | FR-ZOW-001–009 | NFR-ZOW-001,003,005,010,012,013,030,031,040–042 | None |
| ZOW-101 | Authentication & Session Management | 6 | FR-ZOW-010–014 | NFR-ZOW-011,013,014 | ZOW-100 |
| ZOW-102 | Dataset Operations via REST | 8 | FR-ZOW-020–030 | NFR-ZOW-001,014,020,022,023,043 | ZOW-101 |
| ZOW-103 | Job Management via REST | 8 | FR-ZOW-040–049 | NFR-ZOW-002,014,020,022,023,043 | ZOW-101 |
| ZOW-104 | TSO Command Execution via REST | 6 | FR-ZOW-050–055 | NFR-ZOW-003,004,014,032,043 | ZOW-101 |
| ZOW-105 | Console Command Execution via REST | 5 | FR-ZOW-060–062 | NFR-ZOW-014,030,043 | ZOW-101 |
| ZOW-106 | USS File Operations via REST | 7 | FR-ZOW-070–075 | NFR-ZOW-014,020,021,022,023,043 | ZOW-101 |
| ZOW-107 | TN3270E Server for Terminal Access | 5 | FR-ZOW-080–082 | NFR-ZOW-003,004,030,032 | ZOW-100 |
| ZOW-108 | Zowe API Mediation Layer Integration | 5 | FR-ZOW-090–093 | NFR-ZOW-005,020,031 | ZOW-100 |
| ZOW-109 | End-to-End Testing & Conformance | 5 | All FRs | All NFRs | ZOW-100–108 |

**Total: 10 Epics, 62 Stories**

**Phase Mapping:**
- **MVP (Phase F.1):** ZOW-100, ZOW-101, ZOW-102, ZOW-103
- **Growth (Phase F.2):** ZOW-104, ZOW-105, ZOW-106, ZOW-107
- **Vision (Phase F.3):** ZOW-108, ZOW-109

---

## Epic ZOW-100: z/OSMF Server Foundation & System Info

**Goal:** Establish the `open-mainframe-zosmf` crate with Axum HTTP server, TLS, middleware pipeline, configuration, and the `/zosmf/info` endpoint — enough for Zowe CLI to connect and verify the server.

**Architectural Basis:** Architecture §2 (Decisions 1,2,7), §3 (Middleware Chain, Shared State), §4 (Crate Layout).

### Story ZOW-100.1: Create Crate and Axum Scaffold

As a **developer**,
I want **the `open-mainframe-zosmf` crate initialized with Axum and workspace dependencies**,
So that **I have a compilable foundation for all z/OSMF REST handlers**.

**Acceptance Criteria:**

**Given** the workspace Cargo.toml
**When** I add `open-mainframe-zosmf` to the members list
**Then** `cargo build -p open-mainframe-zosmf` compiles with zero errors and zero clippy warnings

**Given** the crate's `lib.rs`
**When** I inspect it
**Then** it contains `#![forbid(unsafe_code)]` and exports `pub fn build_router()` returning an Axum `Router`

**Given** the crate's Cargo.toml
**When** I inspect dependencies
**Then** it includes axum, tokio, tower-http, serde, serde_json, tracing, and workspace crate dependencies (open-mainframe-racf, open-mainframe-dataset, open-mainframe-jes2, open-mainframe-tso, open-mainframe-encoding)

**Covers:** FR-ZOW-009, NFR-ZOW-040, NFR-ZOW-041, NFR-ZOW-042

---

### Story ZOW-100.2: Configuration Loading

As a **system administrator**,
I want **the z/OSMF server to load configuration from a TOML file**,
So that **I can configure port, TLS certificates, USS root, and token TTL without recompilation**.

**Acceptance Criteria:**

**Given** a TOML config file with `[server]`, `[tls]`, `[auth]`, `[uss]`, `[cors]`, and `[zosmf_info]` sections
**When** the server starts with `--config path/to/config.toml`
**Then** all configuration values are loaded into `ZosmfConfig`

**Given** a missing config file
**When** the server starts
**Then** it uses sensible defaults: port 10443, token TTL 28800s, CORS allow-all

**Given** the config struct
**When** I inspect it
**Then** all fields have serde deserialization with default values

**Covers:** FR-ZOW-008

---

### Story ZOW-100.3: TLS Support via rustls

As a **security administrator**,
I want **the server to require HTTPS via TLS 1.2+ using PEM certificate/key files**,
So that **all z/OSMF traffic is encrypted and compatible with Zowe CLI's HTTPS requirement**.

**Acceptance Criteria:**

**Given** a valid PEM certificate and key file configured in `[tls]`
**When** the server starts
**Then** it accepts HTTPS connections on the configured port

**Given** an attempt to connect via TLS 1.0 or 1.1
**When** the handshake is attempted
**Then** the connection is rejected

**Given** a TLS handshake from Zowe CLI
**When** measured under no load
**Then** the handshake completes in < 50ms

**Covers:** FR-ZOW-001, FR-ZOW-004, NFR-ZOW-005, NFR-ZOW-010

---

### Story ZOW-100.4: Middleware Pipeline (CORS, Tracing, CSRF)

As a **developer**,
I want **the middleware chain configured with CORS, request tracing, and CSRF validation**,
So that **cross-origin requests work, all requests are logged, and mutating requests require the CSRF header**.

**Acceptance Criteria:**

**Given** a cross-origin request
**When** the browser sends an OPTIONS preflight
**Then** the response includes `Access-Control-Allow-Origin`, `Access-Control-Allow-Methods`, and `Access-Control-Allow-Headers`

**Given** any HTTP request
**When** it is processed
**Then** the tracing middleware logs: method, path, HTTP status code, and duration in milliseconds

**Given** a PUT, POST, or DELETE request without `X-CSRF-ZOSMF-HEADER`
**When** the request reaches the CSRF middleware
**Then** it is rejected with 403 Forbidden

**Given** a GET request without `X-CSRF-ZOSMF-HEADER`
**When** the request reaches the CSRF middleware
**Then** it passes through (GET is exempt)

**Given** the tracing output
**When** a request includes an `Authorization` header
**Then** the header value is redacted in logs (never contains credentials)

**Covers:** FR-ZOW-003, FR-ZOW-006, FR-ZOW-007, NFR-ZOW-012, NFR-ZOW-013

---

### Story ZOW-100.5: z/OSMF Error Response Format

As a **Zowe CLI user**,
I want **all server errors returned in z/OSMF JSON error format**,
So that **Zowe CLI can parse error responses correctly**.

**Acceptance Criteria:**

**Given** any error condition (400, 401, 403, 404, 500)
**When** the error response is serialized
**Then** the JSON body contains `rc` (int), `reason` (int), `category` (int), and `message` (string) fields

**Given** a malformed JSON request body
**When** deserialization fails
**Then** the response is 400 with z/OSMF error format (no panic, no stack trace)

**Given** an internal server error
**When** the error is caught
**Then** the response is 500 with z/OSMF error format and a generic message (no internal details leaked)

**Covers:** FR-ZOW-005, NFR-ZOW-023, NFR-ZOW-030

---

### Story ZOW-100.6: Shared Application State

As a **developer**,
I want **`AppState` initialized at startup with all crate instances and shared across handlers**,
So that **every handler has access to RACF, Catalog, JES2, TSO, and session stores**.

**Acceptance Criteria:**

**Given** the server starts
**When** `AppState` is constructed
**Then** it contains `Arc<RacfDatabase>`, `Arc<Catalog>`, `Arc<Jes2>`, `Arc<DashMap<String, TsoSessionHandle>>`, `Arc<DashMap<String, AuthenticatedUser>>`, and `Arc<ZosmfConfig>`

**Given** any Axum handler
**When** it receives a request
**Then** it can extract `State<Arc<AppState>>` to access shared crate instances

**Given** 50 concurrent requests
**When** they access shared state
**Then** no deadlocks or data races occur (DashMap provides lock-free concurrent access)

**Covers:** NFR-ZOW-003, NFR-ZOW-004

---

### Story ZOW-100.7: GET /zosmf/info Endpoint and Graceful Shutdown

As a **Zowe CLI user**,
I want **`GET /zosmf/info` to return z/OSMF system information and the server to shutdown gracefully**,
So that **Zowe CLI can verify connectivity and in-flight requests complete before process exit**.

**Acceptance Criteria:**

**Given** an unauthenticated GET request to `/zosmf/info`
**When** the server processes it
**Then** the response is 200 with JSON containing: `api_version` (string), `zosmf_version` (string), `zosmf_hostname` (string from config), `plugins` (array of plugin objects), `zos_version` (string), `zosmf_saf_realm` (string from config)

**Given** the server receives SIGTERM
**When** shutdown is initiated
**Then** in-flight requests complete before the server exits (connection draining)
**And** no new connections are accepted after SIGTERM

**Covers:** FR-ZOW-002, NFR-ZOW-031

---

## Epic ZOW-101: Authentication & Session Management

**Goal:** Implement Basic Auth → JWT authentication flow with RACF integration, token validation, session management, and logout — enabling Zowe CLI to authenticate and maintain sessions.

**Architectural Basis:** Architecture §2 (Decision 3: JWT, Decision 5: DashMap sessions), §3 (Middleware Chain — Auth layer).

### Story ZOW-101.1: Basic Auth Extractor and RACF Verification

As a **Zowe CLI user**,
I want **to authenticate with my userid and password via HTTP Basic Auth**,
So that **the server verifies my credentials against the RACF database**.

**Acceptance Criteria:**

**Given** a request with `Authorization: Basic base64(IBMUSER:password)`
**When** the auth middleware extracts and decodes the header
**Then** it calls `racf::AuthService::authenticate("IBMUSER", "password")`

**Given** valid RACF credentials
**When** `AuthService::authenticate` succeeds
**Then** `AuthContext { userid: "IBMUSER" }` is set in request extensions

**Given** invalid credentials
**When** `AuthService::authenticate` fails
**Then** the response is 401 Unauthorized with z/OSMF error JSON

**Covers:** FR-ZOW-010, NFR-ZOW-014

---

### Story ZOW-101.2: JWT Token Generation

As a **Zowe CLI user**,
I want **`POST /zosmf/services/authenticate` to return a JWT token on successful authentication**,
So that **subsequent requests use the token instead of sending credentials every time**.

**Acceptance Criteria:**

**Given** a POST to `/zosmf/services/authenticate` with valid Basic Auth
**When** RACF authentication succeeds
**Then** the response is 200 with JSON `{"token": "..."}` and a `Set-Cookie: jwtToken=...` header

**Given** the generated JWT token
**When** decoded
**Then** it contains claims: `sub` (userid), `iat` (issued-at timestamp), `exp` (expiry = iat + configured TTL)

**Given** the configured `token_algorithm` is `HS256`
**When** the token is signed
**Then** it uses the configured `token_secret` for HMAC signing

**Covers:** FR-ZOW-011, NFR-ZOW-011

---

### Story ZOW-101.3: JWT Token Validation (Bearer and Cookie)

As a **Zowe CLI user**,
I want **the server to accept JWT tokens via Bearer header or cookie on all authenticated endpoints**,
So that **I don't need to send credentials on every request**.

**Acceptance Criteria:**

**Given** a request with `Authorization: Bearer <valid-jwt>`
**When** the auth middleware validates the token
**Then** it extracts the `sub` claim and sets `AuthContext { userid }` in request extensions

**Given** a request with `Cookie: jwtToken=<valid-jwt>` (no Authorization header)
**When** the auth middleware processes it
**Then** it validates the cookie token and sets `AuthContext` identically

**Given** a request with an expired JWT token
**When** the auth middleware checks the `exp` claim
**Then** the response is 401 Unauthorized with z/OSMF error JSON `{"rc":4, "reason":0, ...}`

**Given** a request with a tampered/invalid JWT signature
**When** the auth middleware validates
**Then** the response is 401 Unauthorized

**Covers:** FR-ZOW-012, FR-ZOW-014, NFR-ZOW-011

---

### Story ZOW-101.4: Session Store with Expiry Sweep

As a **system administrator**,
I want **authenticated sessions stored in memory with automatic expiry cleanup**,
So that **expired sessions don't consume memory indefinitely**.

**Acceptance Criteria:**

**Given** a successful authentication
**When** the token is generated
**Then** the `AuthenticatedUser { userid, token, expires_at }` is stored in `DashMap<String, AuthenticatedUser>`

**Given** sessions in the store
**When** 60 seconds elapse
**Then** a background Tokio task sweeps and removes all sessions where `expires_at < now`

**Given** the token store
**When** checked under concurrent access from 50+ sessions
**Then** no data races or deadlocks occur

**Covers:** NFR-ZOW-003, NFR-ZOW-004, NFR-ZOW-032

---

### Story ZOW-101.5: Logout and Token Invalidation

As a **Zowe CLI user**,
I want **`DELETE /zosmf/services/authenticate` to invalidate my session**,
So that **my token can no longer be used after logout**.

**Acceptance Criteria:**

**Given** an authenticated request to `DELETE /zosmf/services/authenticate`
**When** processed
**Then** the token is removed from the session store and the response is 200 OK

**Given** a subsequent request using the invalidated token
**When** the auth middleware checks
**Then** the response is 401 Unauthorized

**Given** the CSRF header `X-CSRF-ZOSMF-HEADER`
**When** the DELETE request is sent without it
**Then** the response is 403 Forbidden (CSRF applies to DELETE)

**Covers:** FR-ZOW-013

---

### Story ZOW-101.6: RACF Authorization Middleware

As a **security administrator**,
I want **every authenticated request to check RACF access permissions before reaching the handler**,
So that **users can only access resources they are authorized for**.

**Acceptance Criteria:**

**Given** an authenticated request to access dataset `SYS1.PARMLIB`
**When** the authorization middleware runs
**Then** it calls `saf.check_access("IBMUSER", "DATASET", "SYS1.PARMLIB", READ)`

**Given** the SAF check returns `AccessDenied`
**When** the middleware evaluates the result
**Then** the response is 403 Forbidden with z/OSMF error JSON before the handler executes

**Given** the SAF check returns `Permitted`
**When** the middleware evaluates
**Then** the request proceeds to the handler

**Covers:** NFR-ZOW-014

---

## Epic ZOW-102: Dataset Operations via REST

**Goal:** Implement the z/OSMF dataset REST API endpoints so Zowe CLI `zos-files` commands work end-to-end — list, read, write, create, delete datasets and PDS members.

**Architectural Basis:** Architecture §3 (Adapter Pattern), §4 (handlers/datasets.rs, types/datasets.rs). Bridges `open-mainframe-dataset` (Catalog, Pds, QsamReader/Writer) to z/OSMF JSON format.

### Story ZOW-102.1: List Datasets by Pattern

As a **Zowe CLI user**,
I want **`GET /zosmf/restfiles/ds?dslevel=HLQ.*` to return matching datasets**,
So that **`zowe zos-files ls ds "HLQ.*"` works against OpenMainframe**.

**Acceptance Criteria:**

**Given** an authenticated GET to `/zosmf/restfiles/ds?dslevel=IBMUSER.*`
**When** the handler calls `catalog.lookup("IBMUSER.*")`
**Then** the response is 200 with JSON `{"items": [{"dsname":"IBMUSER.COBOL", "dsorg":"PO", "recfm":"FB", "lrecl":"80", "blksz":"800", ...}], "returnedRows": N, "totalRows": N, "JSONversion": 1}`

**Given** the `X-IBM-Max-Items` header is set to `10`
**When** the catalog returns 50 entries
**Then** only the first 10 items are returned

**Given** the `X-IBM-Attributes` header is set to `base`
**When** the response is built
**Then** only `dsname` and `dsorg` fields are included (no volume info)

**Given** numeric dataset attributes (lrecl, blksz)
**When** serialized to JSON
**Then** they are returned as strings, not integers (z/OSMF convention)

**Covers:** FR-ZOW-020, FR-ZOW-029, FR-ZOW-030, NFR-ZOW-001, NFR-ZOW-022

---

### Story ZOW-102.2: List PDS Members

As a **Zowe CLI user**,
I want **`GET /zosmf/restfiles/ds/HLQ.PDS/member` to return member list with ISPF stats**,
So that **`zowe zos-files ls am "HLQ.PDS"` shows members with metadata**.

**Acceptance Criteria:**

**Given** an authenticated GET to `/zosmf/restfiles/ds/IBMUSER.COBOL/member`
**When** the handler reads the PDS directory
**Then** the response is 200 with JSON array of members, each with `member` (name), `vers` (version), `mod` (modification count), `c4date` (created date), `m4date` (modified date), `cnorc` (current record count)

**Given** the PDS does not exist
**When** the handler processes the request
**Then** the response is 404 with z/OSMF error JSON

**Given** the `X-IBM-Max-Items` header is set
**When** the member list is built
**Then** it respects the limit

**Covers:** FR-ZOW-021, FR-ZOW-029, NFR-ZOW-022

---

### Story ZOW-102.3: Read Dataset/Member Content

As a **Zowe CLI user**,
I want **`GET /zosmf/restfiles/ds/HLQ.SEQ` and `GET /zosmf/restfiles/ds/HLQ.PDS(MEMBER)` to return content as text**,
So that **`zowe zos-files download ds` works for both sequential datasets and PDS members**.

**Acceptance Criteria:**

**Given** an authenticated GET to `/zosmf/restfiles/ds/IBMUSER.SEQ`
**When** the handler reads the sequential dataset
**Then** the response is 200 with `Content-Type: text/plain` and the dataset content in UTF-8

**Given** an authenticated GET to `/zosmf/restfiles/ds/IBMUSER.COBOL(PAYROLL)`
**When** the handler reads the PDS member
**Then** the response is 200 with the member content in UTF-8

**Given** the dataset content is in EBCDIC
**When** the handler reads it
**Then** the `open-mainframe-encoding` crate converts EBCDIC → UTF-8 before returning

**Given** the dataset does not exist
**When** requested
**Then** the response is 404 with z/OSMF error JSON

**Covers:** FR-ZOW-022, FR-ZOW-023

---

### Story ZOW-102.4: Write Dataset/Member Content

As a **Zowe CLI user**,
I want **`PUT /zosmf/restfiles/ds/HLQ.SEQ` and `PUT /zosmf/restfiles/ds/HLQ.PDS(MEMBER)` to write content**,
So that **`zowe zos-files upload ftds` works for uploading source code**.

**Acceptance Criteria:**

**Given** an authenticated PUT to `/zosmf/restfiles/ds/IBMUSER.SEQ` with text body
**When** the handler processes it
**Then** the content is written to the sequential dataset (UTF-8 → EBCDIC conversion if needed) and the response is 204 No Content

**Given** an authenticated PUT to `/zosmf/restfiles/ds/IBMUSER.COBOL(PAYROLL)` with text body
**When** the handler processes it
**Then** the member content is written/replaced and the response is 204 No Content

**Given** the request lacks `X-CSRF-ZOSMF-HEADER`
**When** processed
**Then** the response is 403 Forbidden (PUT requires CSRF)

**Given** the user lacks RACF UPDATE access to the dataset
**When** checked
**Then** the response is 403 Forbidden with z/OSMF error JSON

**Covers:** FR-ZOW-024, FR-ZOW-025, NFR-ZOW-014

---

### Story ZOW-102.5: Create Dataset

As a **Zowe CLI user**,
I want **`POST /zosmf/restfiles/ds/HLQ.NEW` with allocation parameters to create a new dataset**,
So that **`zowe zos-files create ds` works**.

**Acceptance Criteria:**

**Given** an authenticated POST to `/zosmf/restfiles/ds/IBMUSER.NEW` with JSON body `{"dsorg":"PS","recfm":"FB","lrecl":80,"blksz":800,"primary":10,"secondary":5,"alcunit":"TRK"}`
**When** the handler processes it
**Then** the dataset is created in the catalog with the specified attributes and the response is 201 Created

**Given** a dataset that already exists
**When** creation is attempted
**Then** the response is 500 with z/OSMF error JSON indicating duplicate

**Given** invalid allocation parameters (e.g., lrecl > blksz for FB)
**When** validation runs
**Then** the response is 400 with descriptive z/OSMF error

**Covers:** FR-ZOW-026, NFR-ZOW-023

---

### Story ZOW-102.6: Delete Dataset and PDS Member

As a **Zowe CLI user**,
I want **`DELETE /zosmf/restfiles/ds/HLQ.OLD` and `DELETE /zosmf/restfiles/ds/HLQ.PDS(MEMBER)` to remove datasets/members**,
So that **`zowe zos-files delete ds` works**.

**Acceptance Criteria:**

**Given** an authenticated DELETE to `/zosmf/restfiles/ds/IBMUSER.OLD`
**When** the handler processes it
**Then** the dataset is removed from the catalog and the response is 204 No Content

**Given** an authenticated DELETE to `/zosmf/restfiles/ds/IBMUSER.COBOL(OBSOLETE)`
**When** the handler processes it
**Then** the member is removed from the PDS and the response is 204 No Content

**Given** a non-existent dataset or member
**When** deletion is attempted
**Then** the response is 404 with z/OSMF error JSON

**Given** the user lacks RACF ALTER access to the dataset
**When** checked
**Then** the response is 403 Forbidden

**Covers:** FR-ZOW-027, FR-ZOW-028, NFR-ZOW-014

---

### Story ZOW-102.7: Dataset Type Mapping (z/OSMF JSON ↔ Catalog)

As a **developer**,
I want **z/OSMF dataset JSON types defined with correct serde attributes**,
So that **all dataset responses match z/OSMF field naming exactly**.

**Acceptance Criteria:**

**Given** the `DatasetListItem` struct
**When** serialized
**Then** fields use exact z/OSMF names: `dsname`, `dsorg`, `recfm`, `lrecl`, `blksz`, `vol`, `cdate`, `rdate`, `sizex`, `extx`, `catnm`

**Given** the `MemberListItem` struct
**When** serialized
**Then** fields include: `member`, `vers`, `mod`, `c4date`, `m4date`, `cnorc`, `inorc`

**Given** numeric fields like `lrecl`, `blksz`
**When** serialized
**Then** they are JSON strings, not numbers (matching z/OSMF convention)

**Covers:** NFR-ZOW-022

---

### Story ZOW-102.8: Dataset Integration Test Suite

As a **developer**,
I want **integration tests that verify all dataset endpoints against expected z/OSMF JSON responses**,
So that **Zowe CLI compatibility is validated by `cargo test`**.

**Acceptance Criteria:**

**Given** the test suite
**When** `cargo test -p open-mainframe-zosmf -- dataset` runs
**Then** all dataset endpoint tests pass (list, read, write, create, delete, member list, pagination, attributes filter)

**Given** a test for listing datasets
**When** executed
**Then** it verifies response JSON matches z/OSMF field names and data types exactly

**Given** a test for error conditions (404, 403, 400)
**When** executed
**Then** it verifies z/OSMF error JSON format with correct `rc`, `reason`, `category` fields

**Covers:** NFR-ZOW-043, NFR-ZOW-020

---

## Epic ZOW-103: Job Management via REST

**Goal:** Implement the z/OSMF jobs REST API endpoints so Zowe CLI `zos-jobs` commands work — list, submit, status, spool, hold/release/cancel/purge.

**Architectural Basis:** Architecture §3 (Adapter Pattern), §4 (handlers/jobs.rs, types/jobs.rs). Bridges `open-mainframe-jes2` (Jes2, Job, SpoolManager, InternalReader) to z/OSMF JSON format.

### Story ZOW-103.1: List Jobs by Owner/Prefix

As a **Zowe CLI user**,
I want **`GET /zosmf/restjobs/jobs?owner=USER&prefix=MY*` to return matching jobs**,
So that **`zowe zos-jobs ls jobs` works against OpenMainframe**.

**Acceptance Criteria:**

**Given** an authenticated GET to `/zosmf/restjobs/jobs?owner=IBMUSER&prefix=MYJOB*`
**When** the handler queries JES2
**Then** the response is 200 with JSON array of jobs, each with `jobid`, `jobname`, `owner`, `status`, `type`, `class`, `retcode`, `url`, `files-url`

**Given** the `status` field
**When** serialized
**Then** it uses z/OSMF strings: `"INPUT"`, `"ACTIVE"`, or `"OUTPUT"` (not internal status codes)

**Given** the `files-url` field
**When** serialized
**Then** it uses hyphenated naming with `#[serde(rename = "files-url")]`

**Covers:** FR-ZOW-040, FR-ZOW-049, NFR-ZOW-022

---

### Story ZOW-103.2: Get Job Status

As a **Zowe CLI user**,
I want **`GET /zosmf/restjobs/jobs/JOBNAME/JOBID` to return detailed job status**,
So that **`zowe zos-jobs view jsbj JOB00042` works**.

**Acceptance Criteria:**

**Given** an authenticated GET to `/zosmf/restjobs/jobs/MYJOB/JOB00042`
**When** the handler queries JES2 for the job
**Then** the response is 200 with JSON containing: `jobid`, `jobname`, `owner`, `status`, `type`, `class`, `retcode`, `phase`, `phase-name`, `url`, `files-url`, `subsystem`, `exec-started`, `exec-ended`

**Given** a non-existent job ID
**When** queried
**Then** the response is 404 with z/OSMF error JSON

**Given** the `phase-name` field
**When** serialized
**Then** it uses the hyphenated name via `#[serde(rename = "phase-name")]`

**Covers:** FR-ZOW-041, FR-ZOW-049, NFR-ZOW-022

---

### Story ZOW-103.3: Submit JCL from Request Body

As a **Zowe CLI user**,
I want **`PUT /zosmf/restjobs/jobs` with JCL body to submit a job and return the job ID**,
So that **`zowe zos-jobs submit` works against OpenMainframe**.

**Acceptance Criteria:**

**Given** an authenticated PUT to `/zosmf/restjobs/jobs` with `Content-Type: text/plain` and JCL body
**When** the handler processes it
**Then** it submits the JCL via `jes2.internal_reader.submit(jcl_text)` and returns 201 Created with JSON `{"jobid":"JOB00042", "jobname":"MYJOB", "owner":"IBMUSER", "status":"INPUT"}`

**Given** invalid JCL (missing JOB statement)
**When** submitted
**Then** the response is 400 with z/OSMF error JSON describing the parse error

**Given** the submit operation
**When** measured
**Then** it completes in < 500ms (from request to job ID returned)

**Covers:** FR-ZOW-042, NFR-ZOW-002

---

### Story ZOW-103.4: List Spool Files and Read Spool Content

As a **Zowe CLI user**,
I want **to list spool files and read their content for a given job**,
So that **`zowe zos-jobs view sfbi` and `zowe zos-jobs view sfbi JOB00042 2` work**.

**Acceptance Criteria:**

**Given** an authenticated GET to `/zosmf/restjobs/jobs/MYJOB/JOB00042/files`
**When** the handler queries the spool manager
**Then** the response is 200 with JSON array of spool file entries, each with `id`, `ddname`, `stepname`, `procstep`, `class`, `byte-count`, `record-count`, `records-url`

**Given** an authenticated GET to `/zosmf/restjobs/jobs/MYJOB/JOB00042/files/2/records`
**When** the handler reads spool file ID 2
**Then** the response is 200 with `Content-Type: text/plain` and the spool content

**Given** a non-existent spool file ID
**When** requested
**Then** the response is 404 with z/OSMF error JSON

**Covers:** FR-ZOW-043, FR-ZOW-044, NFR-ZOW-022

---

### Story ZOW-103.5: Hold, Release, and Cancel Jobs

As a **Zowe CLI user**,
I want **to hold, release, and cancel jobs via PUT requests with action JSON**,
So that **`zowe zos-jobs modify` commands work**.

**Acceptance Criteria:**

**Given** an authenticated PUT to `/zosmf/restjobs/jobs/MYJOB/JOB00042` with body `{"request":"hold","version":"2.0"}`
**When** processed
**Then** the job is held in JES2 and the response is 200 with feedback JSON `{"jobid":"JOB00042", "jobname":"MYJOB", "status":0, "message":"..."}`

**Given** the same endpoint with `{"request":"release","version":"2.0"}`
**When** processed
**Then** the held job is released

**Given** the same endpoint with `{"request":"cancel","version":"2.0"}`
**When** processed
**Then** the running job is cancelled

**Given** an attempt to hold an already-held job
**When** processed
**Then** the response is 200 with appropriate feedback (not an error)

**Covers:** FR-ZOW-045, FR-ZOW-046, FR-ZOW-047

---

### Story ZOW-103.6: Purge Job and Output

As a **Zowe CLI user**,
I want **`DELETE /zosmf/restjobs/jobs/MYJOB/JOB00042` to remove the job and all spool output**,
So that **`zowe zos-jobs delete job` works**.

**Acceptance Criteria:**

**Given** an authenticated DELETE to `/zosmf/restjobs/jobs/MYJOB/JOB00042`
**When** processed
**Then** the job and all spool files are purged from JES2 and the response is 200 with feedback JSON

**Given** a non-existent job
**When** purge is attempted
**Then** the response is 404 with z/OSMF error JSON

**Given** the purge request lacks `X-CSRF-ZOSMF-HEADER`
**When** processed
**Then** the response is 403 Forbidden

**Covers:** FR-ZOW-048

---

### Story ZOW-103.7: Job Type Mapping (z/OSMF JSON ↔ JES2)

As a **developer**,
I want **z/OSMF job JSON types defined with correct serde attributes**,
So that **all job responses match z/OSMF field naming exactly**.

**Acceptance Criteria:**

**Given** the `JobResponse` struct
**When** serialized
**Then** fields use z/OSMF names including hyphenated: `jobid`, `jobname`, `owner`, `status`, `type`, `class`, `retcode`, `phase`, `phase-name`, `url`, `files-url`, `exec-started`, `exec-ended`

**Given** the `SpoolFile` struct
**When** serialized
**Then** fields include hyphenated: `byte-count`, `record-count`, `records-url`

**Given** the `JobFeedback` struct
**When** serialized
**Then** fields include `jobid`, `jobname`, `original-jobid`, `owner`, `member`, `sysname`, `status`, `internal-code`, `message`

**Covers:** NFR-ZOW-022

---

### Story ZOW-103.8: Job Integration Test Suite

As a **developer**,
I want **integration tests that verify all job endpoints against expected z/OSMF JSON responses**,
So that **Zowe CLI compatibility is validated by `cargo test`**.

**Acceptance Criteria:**

**Given** the test suite
**When** `cargo test -p open-mainframe-zosmf -- jobs` runs
**Then** all job endpoint tests pass (list, status, submit, spool list, spool read, hold, release, cancel, purge)

**Given** a submit test
**When** executed
**Then** it verifies the returned job ID format matches `JOBnnnnn` and status is `"INPUT"`

**Given** error condition tests
**When** executed
**Then** they verify 404, 400, 403 responses use z/OSMF error JSON format

**Covers:** NFR-ZOW-043, NFR-ZOW-020

---

## Epic ZOW-104: TSO Command Execution via REST

**Goal:** Implement z/OSMF TSO REST API endpoints so Zowe CLI `zos-tso` commands work — stateless command issue and TSO address space lifecycle.

**Architectural Basis:** Architecture §3 (Adapter Pattern), §4 (handlers/tso.rs, types/tso.rs). Bridges `open-mainframe-tso` (TsoSession, TsoProfile, TsoIo) to z/OSMF JSON format.

### Story ZOW-104.1: Stateless TSO Command Issue

As a **Zowe CLI user**,
I want **`POST /zosmf/tsoApp/tso` with a command to return output immediately**,
So that **`zowe zos-tso issue cmd "TIME"` works without managing sessions**.

**Acceptance Criteria:**

**Given** an authenticated POST to `/zosmf/tsoApp/tso` with JSON `{"TSO COMMAND":"TIME"}`
**When** the handler creates a temporary TsoSession, executes the command, and collects output
**Then** the response is 200 with JSON containing `{"tsoData": [{"TSO MESSAGE": {"DATA": "IKJ56650I TIME-..."}}, ...]}` and the session is destroyed

**Given** a command that produces multi-line output
**When** executed
**Then** each line is a separate entry in the `tsoData` array

**Given** an invalid command
**When** executed
**Then** the error message from TSO is returned in the `tsoData` array (not an HTTP error)

**Covers:** FR-ZOW-050

---

### Story ZOW-104.2: Start TSO Address Space

As a **Zowe CLI user**,
I want **`POST /zosmf/tsoApp/tso` with profile parameters to start a TSO address space and get a servlet key**,
So that **I can maintain a persistent TSO session for interactive use**.

**Acceptance Criteria:**

**Given** an authenticated POST to `/zosmf/tsoApp/tso` with JSON `{"startTso":{"proc":"IKJACCNT","acct":"ACCT","rows":24,"cols":80}}`
**When** the handler creates a new `TsoSession`
**Then** the response is 200 with JSON `{"servletKey":"IBMUSER-123-abc", "queueID":"...", "tsoData":[...]}` and the session is stored in the session DashMap

**Given** the servlet key
**When** stored
**Then** it uniquely identifies this TSO session for subsequent commands

**Covers:** FR-ZOW-051, FR-ZOW-055

---

### Story ZOW-104.3: Send Command to TSO Session

As a **Zowe CLI user**,
I want **`PUT /zosmf/tsoApp/tso/{servletKey}` with a command to send it to my active TSO session**,
So that **I can interact with TSO as a stateful session**.

**Acceptance Criteria:**

**Given** an authenticated PUT to `/zosmf/tsoApp/tso/IBMUSER-123-abc` with body `{"TSO COMMAND":"LISTDS 'SYS1.PARMLIB'"}`
**When** the handler looks up the session and sends the command
**Then** the response is 200 with JSON containing `tsoData` array with output lines

**Given** an invalid servlet key
**When** the handler looks up the session
**Then** the response is 404 with z/OSMF error JSON

**Covers:** FR-ZOW-052

---

### Story ZOW-104.4: Receive TSO Session Response

As a **Zowe CLI user**,
I want **`GET /zosmf/tsoApp/tso/{servletKey}` to retrieve buffered output from my TSO session**,
So that **long-running commands can be polled for output**.

**Acceptance Criteria:**

**Given** an authenticated GET to `/zosmf/tsoApp/tso/IBMUSER-123-abc`
**When** the session has buffered output
**Then** the response is 200 with JSON `tsoData` array of pending messages

**Given** no pending output
**When** polled
**Then** the response is 200 with empty `tsoData` array

**Covers:** FR-ZOW-053

---

### Story ZOW-104.5: Stop TSO Address Space

As a **Zowe CLI user**,
I want **`DELETE /zosmf/tsoApp/tso/{servletKey}` to destroy my TSO session**,
So that **resources are freed when I'm done**.

**Acceptance Criteria:**

**Given** an authenticated DELETE to `/zosmf/tsoApp/tso/IBMUSER-123-abc`
**When** processed
**Then** the `TsoSession` is destroyed, removed from the session map, and the response is 200

**Given** an already-destroyed session
**When** delete is attempted
**Then** the response is 404

**Covers:** FR-ZOW-054, NFR-ZOW-032

---

### Story ZOW-104.6: TSO Integration Test Suite

As a **developer**,
I want **integration tests for all TSO endpoints**,
So that **`cargo test` validates Zowe CLI TSO compatibility**.

**Acceptance Criteria:**

**Given** the test suite
**When** `cargo test -p open-mainframe-zosmf -- tso` runs
**Then** tests pass for: stateless command, start session, send command, receive response, stop session, invalid session key

**Given** the session lifecycle test
**When** executed
**Then** it starts a session, sends a command, retrieves output, and stops the session in sequence

**Covers:** NFR-ZOW-043

---

## Epic ZOW-105: Console Command Execution via REST

**Goal:** Implement z/OSMF console REST API endpoints so Zowe CLI `zos-console` commands work — issue MVS commands and match solicited responses.

**Architectural Basis:** Architecture §4 (handlers/console.rs, types/console.rs). Bridges to EMCS console simulation.

### Story ZOW-105.1: Issue MVS Console Command

As a **Zowe CLI user**,
I want **`PUT /zosmf/restconsoles/consoles/defcn` with a command to issue it to the MVS console**,
So that **`zowe zos-console issue cmd "D A,L"` works**.

**Acceptance Criteria:**

**Given** an authenticated PUT to `/zosmf/restconsoles/consoles/defcn` with JSON `{"cmd":"D A,L"}`
**When** the handler processes the command
**Then** the response is 200 with JSON containing `cmd-response` (command output text) and `sol-key-detected` (response matching key)

**Given** the `cmd-response` field
**When** serialized
**Then** it uses hyphenated naming via serde rename

**Covers:** FR-ZOW-060, NFR-ZOW-022

---

### Story ZOW-105.2: Solicited Response Matching

As a **Zowe CLI user**,
I want **to filter console responses by solicitation key**,
So that **I receive only the response to my specific command**.

**Acceptance Criteria:**

**Given** a console PUT with `"sol-key":"D123456"`
**When** the response includes messages matching the key
**Then** only matching messages appear in `cmd-response`

**Given** a console command that generates no matching response within timeout
**When** polled
**Then** the response indicates no solicited response received

**Covers:** FR-ZOW-061

---

### Story ZOW-105.3: Async Console Command Execution

As a **Zowe CLI user**,
I want **async mode (`"async":"Y"`) to return immediately with a polling URL**,
So that **long-running console commands don't block the client**.

**Acceptance Criteria:**

**Given** a console PUT with `"async":"Y"`
**When** processed
**Then** the response is 200 with JSON containing `sol-key` for polling and no `cmd-response` (response not yet available)

**Given** the `sol-key` from async mode
**When** used in a subsequent GET to poll for response
**Then** the buffered response is returned when available

**Covers:** FR-ZOW-062

---

### Story ZOW-105.4: Console Type Definitions

As a **developer**,
I want **z/OSMF console JSON types with correct serde attributes**,
So that **responses match z/OSMF field naming**.

**Acceptance Criteria:**

**Given** the `ConsoleRequest` struct
**When** deserialized
**Then** it handles fields: `cmd`, `sol-key`, `async`, `system`

**Given** the `ConsoleResponse` struct
**When** serialized
**Then** it uses hyphenated fields: `cmd-response`, `sol-key-detected`

**Covers:** NFR-ZOW-022

---

### Story ZOW-105.5: Console Integration Test Suite

As a **developer**,
I want **integration tests for console endpoints**,
So that **`cargo test` validates console command functionality**.

**Acceptance Criteria:**

**Given** the test suite
**When** `cargo test -p open-mainframe-zosmf -- console` runs
**Then** tests pass for: synchronous command issue, solicited response matching, async mode with polling

**Covers:** NFR-ZOW-043

---

## Epic ZOW-106: USS File Operations via REST

**Goal:** Implement z/OSMF USS file REST API endpoints so Zowe Explorer's USS tree works — list directories, read/write files, create directories, delete.

**Architectural Basis:** Architecture §2 (Decision 6: Host Filesystem Mapping), §4 (handlers/files.rs). Maps USS paths to configurable host directory with RACF permission checks.

### Story ZOW-106.1: USS Path Mapping Configuration

As a **system administrator**,
I want **USS paths mapped to a configurable host directory**,
So that **`/u/ibmuser/` maps to `{uss_root}/ibmuser/` on the host filesystem**.

**Acceptance Criteria:**

**Given** the config `[uss] root_directory = "/opt/openmainframe/uss"`
**When** a request for `/zosmf/restfiles/fs?path=/u/ibmuser`
**Then** the handler resolves to `/opt/openmainframe/uss/ibmuser` on the host

**Given** a path traversal attempt (e.g., `../../../etc/passwd`)
**When** the handler resolves the path
**Then** it rejects the request with 403 Forbidden (path must stay within uss_root)

**Covers:** FR-ZOW-075

---

### Story ZOW-106.2: List USS Directory Contents

As a **Zowe Explorer user**,
I want **`GET /zosmf/restfiles/fs?path=/u/ibmuser` to return directory listing**,
So that **the USS tree in Zowe Explorer populates correctly**.

**Acceptance Criteria:**

**Given** an authenticated GET to `/zosmf/restfiles/fs?path=/u/ibmuser`
**When** the handler reads the mapped host directory
**Then** the response is 200 with JSON `{"items": [{"name":"file.txt", "mode":"-rw-r--r--", "size":1234, "uid":1000, "user":"ibmuser", "gid":100, "group":"users", "mtime":"2026-02-20T10:30:00"}, ...], "returnedRows": N, "totalRows": N}`

**Given** a non-existent directory path
**When** requested
**Then** the response is 404 with z/OSMF error JSON

**Given** the RACF check for the directory
**When** the user lacks READ access
**Then** the response is 403 Forbidden

**Covers:** FR-ZOW-070, NFR-ZOW-014, NFR-ZOW-021

---

### Story ZOW-106.3: Read USS File Content

As a **Zowe Explorer user**,
I want **`GET /zosmf/restfiles/fs/u/ibmuser/file.txt` to return file content**,
So that **I can open USS files in the VS Code editor**.

**Acceptance Criteria:**

**Given** an authenticated GET to `/zosmf/restfiles/fs/u/ibmuser/hello.c`
**When** the handler reads the mapped host file
**Then** the response is 200 with `Content-Type: text/plain` and the file content

**Given** a non-existent file
**When** requested
**Then** the response is 404 with z/OSMF error JSON

**Covers:** FR-ZOW-071

---

### Story ZOW-106.4: Write USS File Content

As a **Zowe Explorer user**,
I want **`PUT /zosmf/restfiles/fs/u/ibmuser/file.txt` to write file content**,
So that **saving files in Zowe Explorer writes back to OpenMainframe**.

**Acceptance Criteria:**

**Given** an authenticated PUT to `/zosmf/restfiles/fs/u/ibmuser/hello.c` with text body
**When** the handler writes to the mapped host file
**Then** the file is created/overwritten and the response is 204 No Content

**Given** the user lacks RACF UPDATE access
**When** checked
**Then** the response is 403 Forbidden

**Covers:** FR-ZOW-072, NFR-ZOW-014

---

### Story ZOW-106.5: Create USS Directory

As a **Zowe Explorer user**,
I want **`POST /zosmf/restfiles/fs/u/ibmuser/newdir` with type mkdir to create a directory**,
So that **I can create new directories via Zowe Explorer**.

**Acceptance Criteria:**

**Given** an authenticated POST to `/zosmf/restfiles/fs/u/ibmuser/newdir` with JSON `{"type":"mkdir","mode":"rwxr-xr-x"}`
**When** the handler processes it
**Then** the directory is created at the mapped path and the response is 201 Created

**Given** the directory already exists
**When** creation is attempted
**Then** the response is 500 with z/OSMF error JSON

**Covers:** FR-ZOW-074

---

### Story ZOW-106.6: Delete USS File or Directory

As a **Zowe Explorer user**,
I want **`DELETE /zosmf/restfiles/fs/u/ibmuser/old.txt` to remove files and directories**,
So that **I can manage USS files via Zowe Explorer**.

**Acceptance Criteria:**

**Given** an authenticated DELETE to `/zosmf/restfiles/fs/u/ibmuser/old.txt`
**When** processed
**Then** the file is removed and the response is 204 No Content

**Given** a DELETE to a directory with `X-IBM-Option: recursive`
**When** processed
**Then** the directory and all contents are removed recursively

**Given** a non-existent path
**When** delete is attempted
**Then** the response is 404

**Covers:** FR-ZOW-073

---

### Story ZOW-106.7: USS Integration Test Suite

As a **developer**,
I want **integration tests for all USS file endpoints**,
So that **`cargo test` validates Zowe Explorer USS compatibility**.

**Acceptance Criteria:**

**Given** the test suite
**When** `cargo test -p open-mainframe-zosmf -- files` runs
**Then** tests pass for: list directory, read file, write file, create directory, delete file, delete directory, path traversal rejection, RACF permission denial

**Covers:** NFR-ZOW-043, NFR-ZOW-021

---

## Epic ZOW-107: TN3270E Server for Terminal Access

**Goal:** Implement a TN3270E TCP server that maps terminal sessions to TSO/ISPF backends, enabling Zowe Desktop's 3270 terminal and third-party TN3270 clients.

**Architectural Basis:** Architecture §4 (tn3270_server.rs or open-mainframe-tui extension). Bridges `open-mainframe-tui` (terminal rendering) with TCP TN3270E protocol.

### Story ZOW-107.1: TN3270E TCP Listener

As a **terminal user**,
I want **the server to accept TN3270E connections on a configurable TCP port**,
So that **I can connect with a 3270 terminal emulator**.

**Acceptance Criteria:**

**Given** the config `[tn3270e] port = 23`
**When** the server starts
**Then** it listens on the configured port for TCP connections

**Given** a TN3270E client connects
**When** the TCP connection is established
**Then** TN3270E negotiation (DO/WILL/WONT/DONT for TN3270E, device type, function) completes successfully

**Covers:** FR-ZOW-080

---

### Story ZOW-107.2: TN3270E Protocol Handling

As a **terminal user**,
I want **the server to handle TN3270E data streams (Write/Read commands, AID keys, structured fields)**,
So that **3270 screen output displays correctly and keyboard input is processed**.

**Acceptance Criteria:**

**Given** a connected TN3270E session
**When** the server sends a Write Structured Field (WSF) command
**Then** the terminal displays the 3270 screen with correct field positions, attributes, and data

**Given** the user presses an AID key (Enter, PF1-PF24)
**When** the client sends the Modified Data Tag (MDT) stream
**Then** the server parses input fields and routes to the TSO/ISPF handler

**Covers:** FR-ZOW-081

---

### Story ZOW-107.3: Map TN3270E Sessions to TSO/ISPF

As a **terminal user**,
I want **each TN3270E connection mapped to a TSO session with ISPF panel rendering**,
So that **I see the ISPF primary option menu after connecting**.

**Acceptance Criteria:**

**Given** a successful TN3270E connection
**When** the user authenticates (RACF login screen)
**Then** a `TsoSession` is created and ISPF is started for the user

**Given** the user navigates ISPF menus (option 2 for Edit, option 3 for Utilities)
**When** panel transitions occur
**Then** the updated 3270 data stream is sent to the terminal

**Covers:** FR-ZOW-081

---

### Story ZOW-107.4: Concurrent Terminal Sessions

As a **system administrator**,
I want **the server to support at least 10 simultaneous TN3270E connections**,
So that **multiple users can access ISPF/TSO concurrently**.

**Acceptance Criteria:**

**Given** 10 TN3270E clients connected simultaneously
**When** they each execute independent TSO commands
**Then** all sessions respond independently without interference

**Given** a client disconnects unexpectedly
**When** the TCP connection drops
**Then** the associated TsoSession is cleaned up and resources freed

**Covers:** FR-ZOW-082, NFR-ZOW-003, NFR-ZOW-032

---

### Story ZOW-107.5: TN3270E Integration Test Suite

As a **developer**,
I want **integration tests for TN3270E connection, negotiation, and basic interaction**,
So that **`cargo test` validates terminal connectivity**.

**Acceptance Criteria:**

**Given** the test suite
**When** `cargo test -p open-mainframe-zosmf -- tn3270` runs
**Then** tests pass for: TCP connection, TN3270E negotiation, data stream send/receive, session cleanup on disconnect

**Covers:** NFR-ZOW-043

---

## Epic ZOW-108: Zowe API Mediation Layer Integration

**Goal:** Enable OpenMainframe to register with Zowe API ML (Eureka Discovery Service) so it appears in the Zowe API Catalog with SSO and dynamic routing.

**Architectural Basis:** Architecture §4 (eureka.rs). Implements Eureka REST client for service registration, heartbeat, and deregistration.

### Story ZOW-108.1: Static API ML Definition Generator

As a **system administrator**,
I want **a CLI command to generate a static Zowe API ML definition YAML file**,
So that **I can register OpenMainframe without dynamic discovery**.

**Acceptance Criteria:**

**Given** the command `open-mainframe-zosmf generate-api-def --host myhost --port 10443 --service-id openmainframe`
**When** executed
**Then** it produces a valid API ML static definition YAML with `serviceId`, `title`, `description`, `basePath`, `instanceBaseUrls`, `homePageRelativeUrl`, `statusPageRelativeUrl`, `healthCheckRelativeUrl`, `routes`, `apiInfo`

**Given** the generated YAML
**When** placed in API ML's static definition directory
**Then** OpenMainframe appears in the API Catalog

**Covers:** FR-ZOW-090

---

### Story ZOW-108.2: Dynamic Eureka Registration

As a **platform engineer**,
I want **the server to dynamically register with Eureka Discovery Service on startup**,
So that **API ML automatically discovers and routes to OpenMainframe**.

**Acceptance Criteria:**

**Given** the config `[eureka] url = "https://apiml:10011/eureka"` and `service_id = "openmainframe"`
**When** the server starts
**Then** it POSTs to `/eureka/apps/openmainframe` with instance info (hostname, port, health URL, status UP)

**Given** a successful registration
**When** the Eureka response is 204
**Then** the server logs "Registered with Eureka Discovery Service"

**Given** Eureka is unreachable
**When** registration fails
**Then** the server logs a warning and retries with exponential backoff (does not fail to start)

**Covers:** FR-ZOW-091

---

### Story ZOW-108.3: Eureka Heartbeat

As a **platform engineer**,
I want **the server to send heartbeats to Eureka every 30 seconds**,
So that **the registration stays active and API ML continues routing to OpenMainframe**.

**Acceptance Criteria:**

**Given** a successful Eureka registration
**When** 30 seconds elapse
**Then** the server sends a PUT heartbeat to `/eureka/apps/openmainframe/{instanceId}`

**Given** the heartbeat returns 200
**When** logged
**Then** the heartbeat is recorded at TRACE level (not flooding logs)

**Given** the heartbeat returns 404 (lease expired)
**When** detected
**Then** the server re-registers with Eureka

**Covers:** FR-ZOW-092

---

### Story ZOW-108.4: Graceful Deregistration

As a **platform engineer**,
I want **the server to deregister from Eureka on graceful shutdown**,
So that **API ML stops routing to a stopped instance immediately**.

**Acceptance Criteria:**

**Given** the server receives SIGTERM or SIGINT
**When** graceful shutdown begins
**Then** it sends DELETE to `/eureka/apps/openmainframe/{instanceId}` before stopping

**Given** the deregistration succeeds
**When** API ML checks its registry
**Then** OpenMainframe is no longer listed

**Covers:** FR-ZOW-093, NFR-ZOW-031

---

### Story ZOW-108.5: API ML Integration Test Suite

As a **developer**,
I want **integration tests for Eureka registration, heartbeat, and deregistration**,
So that **API ML connectivity is validated by `cargo test`**.

**Acceptance Criteria:**

**Given** the test suite
**When** `cargo test -p open-mainframe-zosmf -- eureka` runs
**Then** tests pass for: static definition generation, registration request format, heartbeat timing, deregistration on shutdown

**Given** tests that require an actual Eureka instance
**When** Eureka is not available
**Then** those tests are marked `#[ignore]` for optional integration testing

**Covers:** NFR-ZOW-043

---

## Epic ZOW-109: End-to-End Testing & Conformance

**Goal:** Validate the complete Zowe integration with end-to-end tests using actual Zowe CLI commands, and evaluate conformance against the Zowe Conformance self-certification checklist.

**Architectural Basis:** All prior epics. This epic tests the system as a whole, not individual components.

### Story ZOW-109.1: Zowe CLI Dataset Integration Tests

As a **QA engineer**,
I want **end-to-end tests that run `zowe zos-files` commands against a running OpenMainframe z/OSMF server**,
So that **dataset operations are validated from the Zowe CLI user's perspective**.

**Acceptance Criteria:**

**Given** a running OpenMainframe z/OSMF server with test datasets loaded
**When** the test suite runs `zowe zos-files ls ds "TEST.*"`, `zowe zos-files download ds`, `zowe zos-files upload ftds`, `zowe zos-files create ds`, `zowe zos-files delete ds`
**Then** all commands succeed with correct output

**Given** the test suite
**When** executed
**Then** it verifies JSON output format matches Zowe CLI expectations for all commands

**Covers:** FR-ZOW-020–030, NFR-ZOW-020

---

### Story ZOW-109.2: Zowe CLI Job Integration Tests

As a **QA engineer**,
I want **end-to-end tests that run `zowe zos-jobs` commands against OpenMainframe**,
So that **job operations are validated end-to-end**.

**Acceptance Criteria:**

**Given** a running server
**When** the test suite runs `zowe zos-jobs submit lf test.jcl`, `zowe zos-jobs view jsbj`, `zowe zos-jobs view sfbi`, `zowe zos-jobs delete job`
**Then** all commands succeed with correct output

**Given** a submitted job
**When** it completes
**Then** the status transitions from `INPUT` → `ACTIVE` → `OUTPUT` are observed via polling

**Covers:** FR-ZOW-040–049, NFR-ZOW-020

---

### Story ZOW-109.3: Zowe CLI TSO and Console Integration Tests

As a **QA engineer**,
I want **end-to-end tests for `zowe zos-tso` and `zowe zos-console` commands**,
So that **TSO and console functionality is validated**.

**Acceptance Criteria:**

**Given** a running server
**When** `zowe zos-tso issue cmd "TIME"` is executed
**Then** it returns TSO output containing the current time

**Given** a running server
**When** `zowe zos-console issue cmd "D A,L"` is executed
**Then** it returns console command output

**Covers:** FR-ZOW-050–055, FR-ZOW-060–062, NFR-ZOW-020

---

### Story ZOW-109.4: Performance Benchmark Suite

As a **developer**,
I want **automated benchmarks that measure response times and concurrency**,
So that **NFR performance targets are validated with data**.

**Acceptance Criteria:**

**Given** a benchmark for dataset list
**When** run with 100-item result set
**Then** p95 response time is < 200ms

**Given** a benchmark for job submit
**When** run with a simple JCL
**Then** p95 response time is < 500ms

**Given** a concurrency test
**When** 50 simultaneous Zowe CLI sessions execute commands
**Then** all complete successfully without errors or timeouts

**Covers:** NFR-ZOW-001, NFR-ZOW-002, NFR-ZOW-003

---

### Story ZOW-109.5: Zowe Conformance Self-Certification Evaluation

As a **project maintainer**,
I want **a conformance evaluation document mapping OpenMainframe capabilities to the Zowe Conformance criteria**,
So that **we can identify gaps and achieve conformance certification**.

**Acceptance Criteria:**

**Given** the Zowe Conformance evaluation guide
**When** each criterion is evaluated
**Then** a markdown document is produced listing: criterion, pass/fail status, evidence (test name or explanation), and remediation notes for failures

**Given** all MVP endpoints (datasets, jobs, auth)
**When** evaluated against Zowe V3 conformance criteria
**Then** 100% of applicable criteria pass

**Given** growth features (TSO, USS, console)
**When** evaluated
**Then** pass rate and gap list are documented

**Covers:** All FRs, All NFRs

---

## Validation Summary

### FR Coverage

| FR Group | FRs | Epic(s) | Stories |
|----------|-----|---------|---------|
| Server Foundation (001–009) | 9 | ZOW-100 | 100.1–100.7 |
| Authentication (010–014) | 5 | ZOW-101 | 101.1–101.6 |
| Datasets (020–030) | 11 | ZOW-102 | 102.1–102.8 |
| Jobs (040–049) | 10 | ZOW-103 | 103.1–103.8 |
| TSO (050–055) | 6 | ZOW-104 | 104.1–104.6 |
| Console (060–062) | 3 | ZOW-105 | 105.1–105.5 |
| USS Files (070–075) | 6 | ZOW-106 | 106.1–106.7 |
| TN3270E (080–082) | 3 | ZOW-107 | 107.1–107.5 |
| API ML (090–093) | 4 | ZOW-108 | 108.1–108.5 |
| Cross-cutting | All | ZOW-109 | 109.1–109.5 |

**Total FR Coverage: 47/47 (100%)**

### NFR Coverage

| NFR Group | NFRs | Primary Epic(s) |
|-----------|------|----------------|
| Performance (001–005) | 5 | ZOW-100 (TLS, concurrency), ZOW-102 (dataset latency), ZOW-103 (job latency), ZOW-109 (benchmarks) |
| Security (010–014) | 5 | ZOW-100 (TLS, CSRF, log redaction), ZOW-101 (JWT, RACF) |
| Compatibility (020–023) | 4 | ZOW-102, ZOW-103, ZOW-106 (JSON format), ZOW-109 (CLI integration) |
| Reliability (030–032) | 3 | ZOW-100 (error handling, shutdown), ZOW-104 (session cleanup) |
| Code Quality (040–043) | 4 | ZOW-100 (MSRV, unsafe, clippy), All epics (handler tests) |

**Total NFR Coverage: 17/17 (100%)**

### Summary

```
Total Epics: 10
Total Stories: 62
FR Coverage: 47/47 (100%)
NFR Coverage: 17/17 (100%)
Phase F.1 (MVP): 4 epics, 29 stories
Phase F.2 (Growth): 4 epics, 23 stories
Phase F.3 (Vision): 2 epics, 10 stories
```

### Dependency Graph

```
ZOW-100 (Server Foundation)
  ├── ZOW-101 (Authentication) ← requires server + middleware
  │   ├── ZOW-102 (Datasets) ← requires auth
  │   ├── ZOW-103 (Jobs) ← requires auth
  │   ├── ZOW-104 (TSO) ← requires auth
  │   ├── ZOW-105 (Console) ← requires auth
  │   └── ZOW-106 (USS Files) ← requires auth
  ├── ZOW-107 (TN3270E) ← requires server (independent of REST auth)
  └── ZOW-108 (API ML) ← requires server (independent of REST auth)

ZOW-109 (Conformance) ← requires all above
```

No circular dependencies. Each epic delivers standalone user value. All stories are independently testable with `cargo test`.

---

*Epics completed 2026-02-20. All 4 steps executed. Ready for implementation.*
