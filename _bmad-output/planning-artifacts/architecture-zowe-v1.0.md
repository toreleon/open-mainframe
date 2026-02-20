---
stepsCompleted: [1, 2, 3, 4, 5, 6, 7, 8]
inputDocuments: [prd-zowe-v1.0.md, research-zowe-integration.md]
status: 'complete'
completedAt: '2026-02-20'
---

# Architecture Document — Zowe Integration for OpenMainframe

**Author:** Tore
**Date:** 2026-02-20
**Input:** prd-zowe-v1.0.md, research-zowe-integration.md

---

## 1. Project Context Analysis

### Requirements Overview

The PRD defines **47 functional requirements** (FR-ZOW-001 through FR-ZOW-093) across 8 capability areas and **17 non-functional requirements** (NFR-ZOW-001 through NFR-ZOW-043) covering performance, security, compatibility, reliability, and code quality.

### Architectural Implications by FR Group

| FR Group | Count | Architectural Implication |
|---|---|---|
| Server Foundation (001–009) | 9 | New crate, Axum HTTP server, TLS, middleware pipeline |
| Authentication (010–014) | 5 | JWT library, RACF bridge, session state store |
| Datasets (020–030) | 11 | Adapter layer: z/OSMF JSON ↔ `open-mainframe-dataset` types |
| Jobs (040–049) | 10 | Adapter layer: z/OSMF JSON ↔ `open-mainframe-jes2` types |
| TSO (050–055) | 6 | Session manager with servlet keys, `open-mainframe-tso` bridge |
| Console (060–062) | 3 | EMCS console simulation, response matching |
| USS Files (070–075) | 6 | Host filesystem adapter with RACF permission checks |
| TN3270E (080–082) | 3 | TCP listener, protocol server mode for `open-mainframe-tui` |
| API ML (090–093) | 4 | Eureka HTTP client, heartbeat scheduler |

### NFR-Driven Design Constraints

| NFR | Constraint |
|---|---|
| NFR-ZOW-001/002 (response time) | Async I/O, zero-copy where possible, no blocking in handlers |
| NFR-ZOW-003 (50+ concurrent) | Tokio multi-threaded runtime, `Arc<AppState>` shared state |
| NFR-ZOW-010 (TLS 1.2+) | `rustls` with `axum-server`, no OpenSSL dependency |
| NFR-ZOW-013 (no password logging) | Custom `tracing` filter that redacts `Authorization` headers |
| NFR-ZOW-014 (RACF on every request) | Middleware layer that calls SAF router before handler execution |
| NFR-ZOW-040 (MSRV 1.82) | All dependencies must compile on Rust 1.82 |
| NFR-ZOW-041 (no unsafe) | `#![forbid(unsafe_code)]` in crate root |

---

## 2. Technical Decisions

### Decision 1: HTTP Framework — Axum

**Choice:** Axum 0.8.x
**Rationale:** Tokio-native, Tower middleware ecosystem, macro-free routing, `#![forbid(unsafe_code)]`, performance comparable to raw hyper. OpenMainframe already uses Tokio via `open-mainframe-deploy`.
**Alternatives rejected:** Actix-web (separate actor runtime), Rocket (less flexible middleware), Warp (more complex filter API).

### Decision 2: TLS — rustls via axum-server

**Choice:** `axum-server` crate with `tls-rustls` feature
**Rationale:** Pure Rust TLS (no OpenSSL system dependency), configurable PEM cert/key loading, hot-reload capable.
**Configuration:** `RustlsConfig::from_pem_file(cert_path, key_path)`

### Decision 3: Authentication — JWT via jsonwebtoken

**Choice:** `jsonwebtoken` crate for JWT generation/validation
**Rationale:** Widely used, supports HS256/RS256, configurable claims, no external dependencies.
**Flow:** Basic Auth → `racf::AuthService::authenticate()` → generate JWT with `sub` (userid), `exp` (TTL), `iat` → return in cookie + response body.

### Decision 4: JSON Serialization — serde with z/OSMF field naming

**Choice:** `serde` + `serde_json` with `#[serde(rename)]` for hyphenated fields
**Rationale:** Already a workspace dependency; rename attributes handle z/OSMF's non-standard field names (`files-url`, `phase-name`).
**Key convention:** Numeric values returned as strings to match z/OSMF behavior (e.g., `"blksz": "800"`).

### Decision 5: Session Management — In-memory with DashMap

**Choice:** `DashMap<String, Session>` for concurrent session storage
**Rationale:** Lock-free concurrent hashmap, no external dependencies (Redis/DB), appropriate for single-server deployment.
**Eviction:** Background task sweeps expired sessions every 60 seconds.

### Decision 6: USS File Mapping — Host Filesystem

**Choice:** Map USS paths to configurable host directory
**Rationale:** Simplest approach for MVP; avoids building a full POSIX emulation. `/u/{userid}/` maps to `{uss_root}/{userid}/`.
**Security:** RACF permission checks via `SafRouter::check_access()` before all file operations.

### Decision 7: New Crate Structure

**Choice:** Single new crate `open-mainframe-zosmf` added to workspace
**Rationale:** All z/OSMF REST endpoints are tightly coupled (shared auth, shared state, shared error handling). One crate avoids cross-crate coordination complexity.
**Optional future crate:** `open-mainframe-uss` if USS emulation grows beyond host filesystem mapping.

---

## 3. Design Patterns

### Adapter Pattern (Core Pattern)

Each handler module adapts between z/OSMF REST JSON and existing crate APIs:

```
HTTP Request (JSON) → Deserialize → Validate → Call Crate API → Serialize → HTTP Response (JSON)

Example (list datasets):
  GET /zosmf/restfiles/ds?dslevel=HLQ.*
  → parse query param → Catalog::lookup("HLQ.*")
  → Vec<CatalogEntry> → map to Vec<DatasetListItem>
  → serialize to z/OSMF JSON format → 200 OK
```

### Middleware Chain Pattern

```
Request
  → TLS termination (axum-server)
  → CORS middleware (tower-http)
  → Request logging (tower-http::trace)
  → CSRF validation (custom: check X-CSRF-ZOSMF-HEADER on mutating methods)
  → Authentication (custom: extract Basic/Bearer/Cookie → validate → set AuthContext)
  → Authorization (custom: check RACF access for resource)
  → Route handler
  → Error mapping (custom: convert crate errors to z/OSMF JSON error format)
  → Response
```

### Shared State Pattern

```rust
struct AppState {
    racf: Arc<RacfDatabase>,
    catalog: Arc<Catalog>,
    jes2: Arc<Jes2>,
    sessions: Arc<DashMap<String, TsoSessionHandle>>,
    token_store: Arc<DashMap<String, AuthenticatedUser>>,
    config: Arc<ZosmfConfig>,
}
```

State is created once at server startup and shared across all handlers via `axum::extract::State<Arc<AppState>>`.

### Error Mapping Pattern

All crate errors are mapped to z/OSMF error responses:

```rust
struct ZosmfError {
    rc: i32,        // Return code
    reason: i32,    // Reason code
    category: i32,  // Error category
    message: String, // Human-readable message
}

// Maps to HTTP responses:
// RacfError::AuthFailed → 401 + ZosmfError{rc:4, reason:0, ...}
// DatasetError::NotFound → 404 + ZosmfError{rc:8, reason:0, ...}
// Jes2Error::JobNotFound → 404 + ZosmfError{rc:4, reason:0, ...}
```

---

## 4. System Structure

### Crate Layout

```
crates/open-mainframe-zosmf/
├── Cargo.toml
└── src/
    ├── lib.rs              # Crate root: pub fn build_router(), pub fn start_server()
    ├── config.rs           # ZosmfConfig: port, tls, uss_root, token_ttl
    ├── state.rs            # AppState: shared crate instances
    ├── server.rs           # Server startup: bind, TLS, graceful shutdown
    ├── middleware/
    │   ├── mod.rs
    │   ├── auth.rs         # Authentication extractor (Basic/Bearer/Cookie → AuthContext)
    │   ├── csrf.rs         # X-CSRF-ZOSMF-HEADER validation
    │   └── error.rs        # Error response mapping to z/OSMF JSON format
    ├── handlers/
    │   ├── mod.rs           # Router assembly
    │   ├── info.rs          # GET /zosmf/info
    │   ├── authenticate.rs  # POST/DELETE /zosmf/services/authenticate
    │   ├── datasets.rs      # /zosmf/restfiles/ds/* (9 endpoints)
    │   ├── files.rs         # /zosmf/restfiles/fs/* (5 endpoints, USS)
    │   ├── jobs.rs          # /zosmf/restjobs/jobs/* (9 endpoints)
    │   ├── tso.rs           # /zosmf/tsoApp/* (5 endpoints)
    │   └── console.rs       # /zosmf/restconsoles/* (2 endpoints)
    ├── types/
    │   ├── mod.rs
    │   ├── datasets.rs      # DatasetListItem, DatasetCreateParams, MemberListItem
    │   ├── jobs.rs          # JobResponse, JobSubmitResponse, SpoolFile, JobFeedback
    │   ├── tso.rs           # TsoStartResponse, TsoMessageResponse
    │   ├── console.rs       # ConsoleRequest, ConsoleResponse
    │   ├── auth.rs          # LoginResponse, AuthenticatedUser
    │   ├── info.rs          # ZosmfInfo, ZosmfPlugin
    │   └── error.rs         # ZosmfError
    └── eureka.rs            # Optional: Eureka registration client
```

### Dependency Graph

```
open-mainframe-zosmf
  ├── open-mainframe-dataset   (Catalog, Pds, QsamReader/Writer, DatasetAttributes)
  ├── open-mainframe-jes2      (Jes2, Job, SpoolManager, InternalReader)
  ├── open-mainframe-tso       (TsoSession, TsoProfile, TsoIo)
  ├── open-mainframe-racf      (RacfDatabase, AuthService, SafRouter)
  ├── open-mainframe-encoding  (EBCDIC ↔ UTF-8 conversion)
  ├── open-mainframe-deploy    (metrics integration, tracing setup)
  ├── axum                     (HTTP framework)
  ├── axum-server              (TLS support)
  ├── tower-http               (CORS, tracing, compression)
  ├── serde / serde_json       (JSON serialization)
  ├── jsonwebtoken             (JWT generation/validation)
  ├── dashmap                  (Concurrent session/token storage)
  └── tokio                    (Async runtime)
```

### Request Flow (Example: List Datasets)

```
1. Client: GET https://host:10443/zosmf/restfiles/ds?dslevel=HLQ.*
2. axum-server: TLS termination
3. CORS middleware: Add Access-Control headers
4. Trace middleware: Log "GET /zosmf/restfiles/ds"
5. Auth middleware: Extract Authorization header → validate JWT → set AuthContext{userid: "IBMUSER"}
6. CSRF middleware: Skip (GET request)
7. Router: Match /zosmf/restfiles/ds → handlers::datasets::list_datasets
8. Handler:
   a. Parse query: dslevel="HLQ.*"
   b. Check RACF: saf.check_access("IBMUSER", "DATASET", "HLQ.*", READ) → Ok
   c. Call catalog: catalog.lookup("HLQ.*") → Vec<CatalogEntry>
   d. Map: CatalogEntry → DatasetListItem (z/OSMF JSON format)
   e. Apply X-IBM-Max-Items limit
   f. Apply X-IBM-Attributes field filter
   g. Return Json(DatasetListResponse{items, returnedRows, totalRows, JSONversion: 1})
9. Trace middleware: Log "200 OK 45ms"
10. Client receives JSON response
```

---

## 5. Configuration Model

```toml
# openmainframe-zosmf.toml
[server]
host = "0.0.0.0"
port = 10443

[tls]
cert_file = "/etc/openmainframe/cert.pem"
key_file = "/etc/openmainframe/key.pem"

[auth]
token_ttl_seconds = 28800   # 8 hours
token_algorithm = "HS256"
token_secret = "..."         # Or path to RSA key for RS256

[uss]
root_directory = "/opt/openmainframe/uss"

[cors]
allowed_origins = ["*"]

[zosmf_info]
hostname = "openmainframe-host"
saf_realm = "SAFRealm"
```

---

## 6. Validation Against Requirements

### FR Coverage

| FR Group | FRs | Architecture Component | Covered |
|---|---|---|---|
| Server Foundation | FR-ZOW-001–009 | `server.rs`, `config.rs`, `middleware/*` | Yes |
| Authentication | FR-ZOW-010–014 | `middleware/auth.rs`, `handlers/authenticate.rs` | Yes |
| Datasets | FR-ZOW-020–030 | `handlers/datasets.rs`, `types/datasets.rs` | Yes |
| Jobs | FR-ZOW-040–049 | `handlers/jobs.rs`, `types/jobs.rs` | Yes |
| TSO | FR-ZOW-050–055 | `handlers/tso.rs`, `types/tso.rs` | Yes |
| Console | FR-ZOW-060–062 | `handlers/console.rs`, `types/console.rs` | Yes |
| USS Files | FR-ZOW-070–075 | `handlers/files.rs` | Yes |
| TN3270E | FR-ZOW-080–082 | Separate server mode (future `tn3270_server.rs` or `open-mainframe-tui` extension) | Yes |
| API ML | FR-ZOW-090–093 | `eureka.rs` | Yes |

### NFR Coverage

| NFR Group | Architecture Mechanism |
|---|---|
| Performance (001–005) | Tokio async runtime, zero-copy JSON, connection pooling |
| Security (010–014) | rustls TLS, JWT expiry, CSRF middleware, tracing filter, SAF middleware |
| Compatibility (020–023) | serde rename attributes, z/OSMF error format, integration test suite |
| Reliability (030–032) | Axum graceful shutdown, session eviction task, panic-free handlers |
| Code Quality (040–043) | MSRV 1.82, `#![forbid(unsafe_code)]`, clippy config, test modules |

---

*Architecture completed 2026-02-20. All 8 steps executed. Ready for Epics & Stories phase.*
