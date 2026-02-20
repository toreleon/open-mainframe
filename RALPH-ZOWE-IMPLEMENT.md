# Ralph Loop Prompt: Implement Zowe Integration for OpenMainframe

## Mission

You are implementing the **`open-mainframe-zosmf`** crate — a z/OSMF-compatible REST API server that exposes OpenMainframe subsystems to Zowe CLI, Zowe Explorer, and Zowe API Mediation Layer.

**10 epics, 62 stories, zero left behind.**

All planning artifacts are complete:
- **PRD:** `_bmad-output/planning-artifacts/prd-zowe-v1.0.md` (47 FRs, 17 NFRs)
- **Architecture:** `_bmad-output/planning-artifacts/architecture-zowe-v1.0.md`
- **Epics & Stories:** `_bmad-output/planning-artifacts/epics-zowe-v1.0.md` (10 epics, 62 stories)

---

## Iteration Protocol

**Before doing ANY work, determine where you are:**

1. Check the progress tracker file: `_bmad-output/implementation-progress.md`
   - If it does not exist, create it (see Progress Tracker Format below)
   - If it exists, read it to find the current epic and story

2. Find the **first story with status `pending` or `in_progress`** and work on it

3. After completing a story:
   - Run `cargo check -p open-mainframe-zosmf` — must pass
   - Run `cargo clippy -p open-mainframe-zosmf -- -D warnings` — must pass
   - Run `cargo test -p open-mainframe-zosmf` — must pass
   - Update the progress tracker: mark story `complete`, set next story to `in_progress`
   - Commit with message: `feat(zosmf): ZOW-{epic}.{story} — {title}`

4. If ALL 62 stories are `complete` in the progress tracker, AND all three cargo commands pass:
   - Output the completion promise

5. If you encounter a build error or test failure:
   - Do NOT skip to the next story
   - Fix the error in the current story before proceeding
   - If the error is in a previously completed story, fix it and re-mark it complete

---

## Progress Tracker Format

Create `_bmad-output/implementation-progress.md` with this format:

```markdown
---
currentEpic: ZOW-100
currentStory: ZOW-100.1
storiesComplete: 0
storiesTotal: 62
lastVerified: ""
---

# Zowe Implementation Progress

## ZOW-100: z/OSMF Server Foundation & System Info
- [ ] ZOW-100.1: Create Crate and Axum Scaffold
- [ ] ZOW-100.2: Configuration Loading
- [ ] ZOW-100.3: TLS Support via rustls
- [ ] ZOW-100.4: Middleware Pipeline (CORS, Tracing, CSRF)
- [ ] ZOW-100.5: z/OSMF Error Response Format
- [ ] ZOW-100.6: Shared Application State
- [ ] ZOW-100.7: GET /zosmf/info Endpoint and Graceful Shutdown

## ZOW-101: Authentication & Session Management
- [ ] ZOW-101.1: Basic Auth Extractor and RACF Verification
- [ ] ZOW-101.2: JWT Token Generation
- [ ] ZOW-101.3: JWT Token Validation (Bearer and Cookie)
- [ ] ZOW-101.4: Session Store with Expiry Sweep
- [ ] ZOW-101.5: Logout and Token Invalidation
- [ ] ZOW-101.6: RACF Authorization Middleware

## ZOW-102: Dataset Operations via REST
- [ ] ZOW-102.1: List Datasets by Pattern
- [ ] ZOW-102.2: List PDS Members
- [ ] ZOW-102.3: Read Dataset/Member Content
- [ ] ZOW-102.4: Write Dataset/Member Content
- [ ] ZOW-102.5: Create Dataset
- [ ] ZOW-102.6: Delete Dataset and PDS Member
- [ ] ZOW-102.7: Dataset Type Mapping (z/OSMF JSON <> Catalog)
- [ ] ZOW-102.8: Dataset Integration Test Suite

## ZOW-103: Job Management via REST
- [ ] ZOW-103.1: List Jobs by Owner/Prefix
- [ ] ZOW-103.2: Get Job Status
- [ ] ZOW-103.3: Submit JCL from Request Body
- [ ] ZOW-103.4: List Spool Files and Read Spool Content
- [ ] ZOW-103.5: Hold, Release, and Cancel Jobs
- [ ] ZOW-103.6: Purge Job and Output
- [ ] ZOW-103.7: Job Type Mapping (z/OSMF JSON <> JES2)
- [ ] ZOW-103.8: Job Integration Test Suite

## ZOW-104: TSO Command Execution via REST
- [ ] ZOW-104.1: Stateless TSO Command Issue
- [ ] ZOW-104.2: Start TSO Address Space
- [ ] ZOW-104.3: Send Command to TSO Session
- [ ] ZOW-104.4: Receive TSO Session Response
- [ ] ZOW-104.5: Stop TSO Address Space
- [ ] ZOW-104.6: TSO Integration Test Suite

## ZOW-105: Console Command Execution via REST
- [ ] ZOW-105.1: Issue MVS Console Command
- [ ] ZOW-105.2: Solicited Response Matching
- [ ] ZOW-105.3: Async Console Command Execution
- [ ] ZOW-105.4: Console Type Definitions
- [ ] ZOW-105.5: Console Integration Test Suite

## ZOW-106: USS File Operations via REST
- [ ] ZOW-106.1: USS Path Mapping Configuration
- [ ] ZOW-106.2: List USS Directory Contents
- [ ] ZOW-106.3: Read USS File Content
- [ ] ZOW-106.4: Write USS File Content
- [ ] ZOW-106.5: Create USS Directory
- [ ] ZOW-106.6: Delete USS File or Directory
- [ ] ZOW-106.7: USS Integration Test Suite

## ZOW-107: TN3270E Server for Terminal Access
- [ ] ZOW-107.1: TN3270E TCP Listener
- [ ] ZOW-107.2: TN3270E Protocol Handling
- [ ] ZOW-107.3: Map TN3270E Sessions to TSO/ISPF
- [ ] ZOW-107.4: Concurrent Terminal Sessions
- [ ] ZOW-107.5: TN3270E Integration Test Suite

## ZOW-108: Zowe API Mediation Layer Integration
- [ ] ZOW-108.1: Static API ML Definition Generator
- [ ] ZOW-108.2: Dynamic Eureka Registration
- [ ] ZOW-108.3: Eureka Heartbeat
- [ ] ZOW-108.4: Graceful Deregistration
- [ ] ZOW-108.5: API ML Integration Test Suite

## ZOW-109: End-to-End Testing & Conformance
- [ ] ZOW-109.1: Zowe CLI Dataset Integration Tests
- [ ] ZOW-109.2: Zowe CLI Job Integration Tests
- [ ] ZOW-109.3: Zowe CLI TSO and Console Integration Tests
- [ ] ZOW-109.4: Performance Benchmark Suite
- [ ] ZOW-109.5: Zowe Conformance Self-Certification Evaluation
```

Mark items with `[x]` when complete. Update `storiesComplete` count and `currentStory`/`currentEpic` after each story.

---

## Project Structure

### Workspace Integration

The new crate lives at `crates/open-mainframe-zosmf/` and must be added to the workspace `Cargo.toml` members list.

### Crate File Layout (from Architecture)

```
crates/open-mainframe-zosmf/
├── Cargo.toml
└── src/
    ├── lib.rs              # #![forbid(unsafe_code)], pub fn build_router(), pub fn start_server()
    ├── config.rs           # ZosmfConfig: port, tls, uss_root, token_ttl, cors, zosmf_info
    ├── state.rs            # AppState: Arc<RacfDatabase>, Arc<Catalog>, Arc<Jes2>, etc.
    ├── server.rs           # Server startup: bind, TLS (axum-server + rustls), graceful shutdown
    ├── middleware/
    │   ├── mod.rs
    │   ├── auth.rs         # Authentication extractor (Basic/Bearer/Cookie → AuthContext)
    │   ├── csrf.rs         # X-CSRF-ZOSMF-HEADER validation on PUT/POST/DELETE
    │   └── error.rs        # ZosmfError → z/OSMF JSON error response mapping
    ├── handlers/
    │   ├── mod.rs           # Router assembly: all routes combined
    │   ├── info.rs          # GET /zosmf/info
    │   ├── authenticate.rs  # POST/DELETE /zosmf/services/authenticate
    │   ├── datasets.rs      # /zosmf/restfiles/ds/* (list, read, write, create, delete)
    │   ├── files.rs         # /zosmf/restfiles/fs/* (USS operations)
    │   ├── jobs.rs          # /zosmf/restjobs/jobs/* (list, submit, status, spool, actions)
    │   ├── tso.rs           # /zosmf/tsoApp/* (stateless, session lifecycle)
    │   └── console.rs       # /zosmf/restconsoles/* (issue, solicited, async)
    ├── types/
    │   ├── mod.rs
    │   ├── datasets.rs      # DatasetListItem, DatasetCreateParams, MemberListItem
    │   ├── jobs.rs          # JobResponse, JobSubmitResponse, SpoolFile, JobFeedback
    │   ├── tso.rs           # TsoStartResponse, TsoMessageResponse, TsoData
    │   ├── console.rs       # ConsoleRequest, ConsoleResponse
    │   ├── auth.rs          # LoginResponse, AuthenticatedUser, AuthContext
    │   ├── info.rs          # ZosmfInfo, ZosmfPlugin
    │   └── error.rs         # ZosmfError struct (rc, reason, category, message)
    └── eureka.rs            # Eureka registration client (register, heartbeat, deregister)
```

### Cargo.toml Template

```toml
[package]
name = "open-mainframe-zosmf"
description = "z/OSMF-compatible REST API server for OpenMainframe Zowe integration"
version.workspace = true
edition.workspace = true
rust-version.workspace = true
license.workspace = true

[dependencies]
# Error handling
miette = { workspace = true }
thiserror = { workspace = true }

# Serialization
serde = { workspace = true }
serde_json = { workspace = true }
toml = { workspace = true }

# Logging
tracing = { workspace = true }
tracing-subscriber = { workspace = true }

# Async runtime
tokio = { version = "1", features = ["full"] }

# HTTP framework
axum = "0.8"
axum-server = { version = "0.7", features = ["tls-rustls"] }
tower = "0.5"
tower-http = { version = "0.6", features = ["trace", "cors", "compression-gzip"] }

# Authentication
jsonwebtoken = "9"
base64 = "0.22"

# Concurrent data structures
dashmap = "6"

# Internal crates
open-mainframe-racf = { workspace = true }
open-mainframe-dataset = { workspace = true }
open-mainframe-jes2 = { workspace = true }
open-mainframe-tso = { workspace = true }
open-mainframe-encoding = { workspace = true }
open-mainframe-deploy = { workspace = true }

[dev-dependencies]
tokio-test = "0.4"
reqwest = { version = "0.12", features = ["json", "rustls-tls"] }
tempfile = "3"
```

**IMPORTANT**: After creating this Cargo.toml, run `cargo check -p open-mainframe-zosmf` to verify all dependency versions resolve. If a version doesn't exist on crates.io, adjust to the latest published version. The architecture specifies "Axum 0.8.x" — use the latest 0.8 release. If axum 0.8 is not published, use the latest available (0.7.x) and adjust tower/tower-http versions accordingly.

---

## Coding Standards

### Must Follow (from existing crates)

1. **`#![forbid(unsafe_code)]`** in `lib.rs` crate root
2. **Error pattern:** `thiserror` + `miette` diagnostics for all errors
   ```rust
   #[derive(Debug, thiserror::Error, miette::Diagnostic)]
   pub enum ZosmfError {
       #[error("authentication failed for user '{userid}'")]
       #[diagnostic(code(zosmf::auth_failed))]
       AuthFailed { userid: String },
       // ...
   }
   ```
3. **Public API pattern:** All public types re-exported at crate root in `lib.rs`
4. **Convenience Result type:** `pub type Result<T> = std::result::Result<T, ZosmfError>;`
5. **Module doc comments:** Every module starts with `//!` doc comment
6. **Derive traits:** All public structs derive `Debug, Clone, Serialize, Deserialize` at minimum
7. **Test modules:** Each source file with logic includes `#[cfg(test)] mod tests { ... }`
8. **No unsafe code, no unwrap() on user-controlled paths, no panic in handlers**
9. **clippy clean:** Zero warnings with `-D warnings`
10. **MSRV 1.82:** No features requiring newer Rust

### z/OSMF JSON Conventions

1. **Hyphenated field names** use `#[serde(rename = "field-name")]`:
   ```rust
   #[derive(Serialize)]
   pub struct JobResponse {
       pub jobid: String,
       pub jobname: String,
       #[serde(rename = "files-url")]
       pub files_url: String,
       #[serde(rename = "phase-name")]
       pub phase_name: String,
   }
   ```

2. **Numeric fields as strings** for dataset attributes:
   ```rust
   #[derive(Serialize)]
   pub struct DatasetListItem {
       pub dsname: String,
       pub dsorg: String,
       pub recfm: String,
       pub lrecl: String,  // "80" not 80
       pub blksz: String,  // "800" not 800
   }
   ```

3. **Error response format:**
   ```json
   {"rc": 4, "reason": 0, "category": 1, "message": "Dataset not found"}
   ```

### Axum Handler Pattern

```rust
use axum::{extract::{State, Path, Query}, Json, http::StatusCode};
use std::sync::Arc;

pub async fn list_datasets(
    State(state): State<Arc<AppState>>,
    auth: AuthContext,  // custom extractor
    Query(params): Query<DatasetListQuery>,
) -> Result<Json<DatasetListResponse>, ZosmfApiError> {
    // 1. Check RACF authorization
    state.racf.check_access(&auth.userid, "DATASET", &params.dslevel, "READ")?;

    // 2. Call existing crate API
    let entries = state.catalog.lookup(&params.dslevel)?;

    // 3. Map to z/OSMF types
    let items: Vec<DatasetListItem> = entries.iter().map(|e| e.into()).collect();

    // 4. Return JSON
    Ok(Json(DatasetListResponse {
        items,
        returned_rows: items.len(),
        total_rows: entries.len(),
        json_version: 1,
    }))
}
```

### Integration with Existing Crates

Use these existing APIs (read their `lib.rs` re-exports to discover the full API surface):

| Crate | Key Types | Usage |
|-------|-----------|-------|
| `open-mainframe-racf` | `RacfDatabase`, `AuthService`, `SafRouter`, `AuthResult` | Auth + authorization |
| `open-mainframe-dataset` | `Catalog`, `CatalogEntry`, `Pds`, `PdsMember`, `QsamReader`, `QsamWriter`, `IspfStats` | Dataset operations |
| `open-mainframe-jes2` | `Jes2`, `Job`, `JobId`, `JobState`, `SpoolManager`, `InternalReader` | Job operations |
| `open-mainframe-tso` | `TsoSession`, `TsoProfile`, `TsoIo`, `execute`, `CommandResult` | TSO commands |
| `open-mainframe-encoding` | EBCDIC↔UTF-8 conversion | Text encoding |
| `open-mainframe-deploy` | Metrics, tracing setup | Observability |

**IMPORTANT**: Before implementing any handler, read the relevant crate's `lib.rs` and the specific module source to understand the actual method signatures. Do NOT assume API signatures — verify them by reading the code.

---

## Epic Execution Order

Execute in strict dependency order:

```
Phase F.1 (MVP):
  ZOW-100 → ZOW-101 → ZOW-102 → ZOW-103

Phase F.2 (Growth):
  ZOW-104 → ZOW-105 → ZOW-106 → ZOW-107

Phase F.3 (Vision):
  ZOW-108 → ZOW-109
```

Within each epic, execute stories in order (100.1 → 100.2 → ... → 100.7).

---

## Story Implementation Protocol

For each story:

1. **Read the story** from `_bmad-output/planning-artifacts/epics-zowe-v1.0.md`
2. **Read relevant existing crate code** to understand the API you're bridging to
3. **Create/edit the source files** in `crates/open-mainframe-zosmf/src/`
4. **Write unit tests** in `#[cfg(test)] mod tests` within the same file
5. **Run verification:**
   ```
   cargo check -p open-mainframe-zosmf
   cargo clippy -p open-mainframe-zosmf -- -D warnings
   cargo test -p open-mainframe-zosmf
   ```
6. **Fix any errors** before moving on
7. **Commit:** `git add crates/open-mainframe-zosmf/ Cargo.toml && git commit -m "feat(zosmf): ZOW-{N}.{M} — {title}"`
8. **Update progress tracker** — mark `[x]`, update counters

### Special Notes Per Epic

**ZOW-100 (Foundation):**
- Story 100.1 creates the crate from scratch — add to workspace Cargo.toml members
- Add workspace dependency: `open-mainframe-zosmf = { path = "crates/open-mainframe-zosmf" }` to root Cargo.toml
- TLS (100.3): If `axum-server` with `tls-rustls` causes version issues, implement without TLS first and add a TODO
- Story 100.6 (AppState): Read each dependent crate's `lib.rs` to find the correct constructor types

**ZOW-101 (Auth):**
- Read `crates/open-mainframe-racf/src/auth.rs` for `AuthService` methods
- Read `crates/open-mainframe-racf/src/saf.rs` for `SafRouter` methods
- JWT: Use `jsonwebtoken` crate with HS256 algorithm

**ZOW-102 (Datasets):**
- Read `crates/open-mainframe-dataset/src/catalog.rs` for `Catalog::lookup()`
- Read `crates/open-mainframe-dataset/src/pds.rs` for `Pds` member operations
- Read `crates/open-mainframe-dataset/src/qsam.rs` for `QsamReader`/`QsamWriter`
- Read `crates/open-mainframe-dataset/src/types.rs` for `DatasetAttributes`, `RecordFormat`

**ZOW-103 (Jobs):**
- Read `crates/open-mainframe-jes2/src/queue.rs` for `Jes2` methods
- Read `crates/open-mainframe-jes2/src/job.rs` for `Job`, `JobState`, `JobId`
- Read `crates/open-mainframe-jes2/src/spool.rs` for `SpoolManager`
- Read `crates/open-mainframe-jes2/src/intrdr.rs` for `InternalReader`

**ZOW-104 (TSO):**
- Read `crates/open-mainframe-tso/src/session.rs` for `TsoSession`
- Read `crates/open-mainframe-tso/src/commands.rs` for `execute()`
- Servlet keys use format: `{USERID}-{random_id}`

**ZOW-105 (Console):**
- Console simulation may need a new module if no existing console emulation exists
- Check `crates/open-mainframe-jes2/src/commands.rs` for operator command support

**ZOW-106 (USS):**
- Maps to host filesystem — use `std::fs` operations
- Path security: canonicalize paths and verify they start with `uss_root`
- RACF checks before all file operations

**ZOW-107 (TN3270E):**
- Read `crates/open-mainframe-tui/src/` for terminal rendering
- TN3270E is raw TCP, not HTTP — use `tokio::net::TcpListener`
- This is the most complex epic — take extra care with protocol handling

**ZOW-108 (API ML):**
- Eureka uses simple HTTP REST API — use `reqwest` or raw hyper client
- Heartbeat runs as a Tokio background task

**ZOW-109 (E2E Testing):**
- Integration tests go in `crates/open-mainframe-zosmf/tests/` directory
- Tests that require external services (Zowe CLI, Eureka) should use `#[ignore]`
- Benchmark tests can use `#[bench]` or criterion

---

## Error Recovery

If the build breaks:
1. Read the **exact error message** from cargo
2. Check if it's a **dependency version issue** — adjust Cargo.toml versions
3. Check if it's a **missing import** — read the relevant crate's `lib.rs` for correct paths
4. Check if it's an **API mismatch** — read the actual source of the crate you're calling
5. If a previously-completed story's code needs fixing, fix it in-place (don't create new files)
6. After fixing, re-run all three cargo commands before continuing

If you get stuck on a dependency that doesn't exist at the specified version:
- Search the actual crate's latest version with web search
- Adjust the version in Cargo.toml
- Document the version change in a comment

---

## Completion Criteria

ALL of these must be true:
1. All 62 stories marked `[x]` in `_bmad-output/implementation-progress.md`
2. `cargo check -p open-mainframe-zosmf` passes
3. `cargo clippy -p open-mainframe-zosmf -- -D warnings` passes
4. `cargo test -p open-mainframe-zosmf` passes
5. `storiesComplete: 62` in progress tracker frontmatter

When all criteria are met, output the completion promise.

---

## Completion Promise

When ALL 62 stories are implemented, all tests pass, and all acceptance criteria from the epics document are satisfied:

```
ZOWE IMPLEMENTATION COMPLETE
```

Signal completion with the promise tag containing the text above.
