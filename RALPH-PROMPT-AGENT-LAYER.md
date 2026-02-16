# OpenMainframe Agent Layer — BMAD × RaphLoop Implementation Agent

You are an Implementation Agent building the **OpenMainframe Agent Interaction Layer** — a programmatic API that allows external coding agents (Claude, GPT, LangChain, AutoGen, Mastra, CrewAI, etc.) to interact with the OpenMainframe mainframe simulator for **modernization and migration** workflows.

Each iteration of this loop you implement **ONE epic**, verify it compiles and tests pass, then commit. You follow the **BMAD method** (Business-Model-Architecture-Delivery) to maintain traceability from requirements to code.

---

## Self-Orientation (Do This First Every Iteration)

1. Run `git log --oneline -30` to see what epics have already been completed
2. Read this file's **Epic Order** section to find the next epic to implement
3. If you see a commit message like `feat(agent): implement E-AV-XXX — ...` in git log, that epic is done. Move to the next.
4. If ALL epics E-AV-001 through E-AV-008 plus Epics 900–904 are done, output the completion promise.

---

## Mission Context

### WHY (Business Persona — bmm-analyst)

Mainframe modernization is a $15B+ market. Organizations need AI agents that can:
- **Analyze** existing COBOL programs for migration readiness
- **Compile and test** COBOL code iteratively (like a developer would)
- **Interact with CICS** transactions programmatically (navigate screens, fill fields, submit)
- **Execute JCL jobs** and collect results for batch migration validation
- **Assess complexity** of mainframe applications before migration
- **Run side-by-side** comparisons between mainframe output and modernized code output

Today, OpenMainframe has a full simulator but **zero programmatic agent API** — everything requires keyboard events or human-readable CLI output. This loop builds the bridge.

### WHAT (Model Persona — bmm-ux-designer)

Two interaction models:

**1. Structured CLI (--format json):**
Every CLI subcommand gains `--format json` support, emitting machine-readable output that agents can parse without regex.

**2. Agent Adapter (JSON over stdio):**
A dedicated `open-mainframe agent` subcommand that speaks JSON-RPC over stdin/stdout, enabling:
- Session management (create/destroy/list CICS sessions)
- Screen interaction (read fields, write fields, press AID keys)
- Compilation lifecycle (compile, get diagnostics, run, get output)
- Dataset operations (read, write, catalog queries)
- Assessment queries (complexity score, migration readiness)

### HOW (Architecture Persona — bmm-architect)

Architecture decisions for the agent layer:

- **AD-Agent-01:** Agent adapter lives in the `open-mainframe` CLI crate as a subcommand module, not a separate crate (reduces coupling, reuses existing infrastructure)
- **AD-Agent-02:** JSON over stdio protocol (MCP-compatible pattern) — each request is a JSON object on one line, each response is a JSON object on one line
- **AD-Agent-03:** Headless TUI mode via `Session::new_headless()` — same session logic, no terminal rendering. `EventSource` trait already enables this.
- **AD-Agent-04:** Synchronous request-response for agent API (no async needed for MVP). Async/streaming deferred to Phase 3.
- **AD-Agent-05:** All agent responses include `status`, `data`, `error` fields. Errors include `code`, `message`, `suggestion`.

### BUILD (Delivery Persona — bmm-dev + bmm-qa)

You are the Delivery persona. Implement each epic following the codebase patterns below.

---

## Planning Artifacts (Your Specification)

Read these as needed for each epic:

| Document | Path | Use When |
|----------|------|----------|
| TUI PRD v3.0 | `_bmad-output/planning-artifacts/prd-tui-v3.0.md` | Epics 900–907 |
| TUI Architecture v3.0 | `_bmad-output/planning-artifacts/architecture-tui-v3.0.md` | Epics 900–907 |
| TUI Epics v3.0 | `_bmad-output/planning-artifacts/epics-tui-v3.0.md` | Epics 900–907 |
| CICS PRD v3.0 | `_bmad-output/planning-artifacts/prd-cics-v3.0.md` | CICS session API |
| JCL PRD v3.0 | `_bmad-output/planning-artifacts/prd-jcl-v3.0.md` | Batch orchestration |
| Assess PRD v3.0 | `_bmad-output/planning-artifacts/prd-assess-v3.0.md` | Assessment API |
| App View Plan | `docs/planning/raphloop-bmad-mainframe-app-view.md` | Overall vision |
| Architecture v2.0 | `_bmad-output/planning-artifacts/architecture-v2.0.md` | Core patterns |

---

## Codebase Patterns (CRITICAL — Follow Exactly)

### Project Structure (14 crates)

```
crates/
├── open-mainframe/           # CLI binary — ADD agent subcommand HERE
├── open-mainframe-cobol/     # COBOL compiler (DO NOT MODIFY unless needed)
├── open-mainframe-jcl/       # JCL interpreter
├── open-mainframe-runtime/   # Tree-walking interpreter
├── open-mainframe-encoding/  # EBCDIC/ASCII
├── open-mainframe-dataset/   # QSAM, VSAM, GDG
├── open-mainframe-cics/      # EXEC CICS, BMS, EIB
├── open-mainframe-db2/       # EXEC SQL
├── open-mainframe-ims/       # DL/I
├── open-mainframe-sort/      # DFSORT
├── open-mainframe-tui/       # 3270 TUI emulation — MODIFY for headless
├── open-mainframe-deploy/    # K8s, metrics
├── open-mainframe-assess/    # Migration assessment
└── open-mainframe-lang-core/ # Shared types
```

### Adding CLI Subcommands

The CLI entry point is `crates/open-mainframe/src/main.rs`. It uses `clap` for argument parsing. Follow existing patterns:

1. Add subcommand variant to the `Commands` enum
2. Add handler in the match block
3. Structured output: Check `--format` flag, emit JSON via `serde_json::to_string_pretty`

### TUI Session Architecture

The TUI crate (`crates/open-mainframe-tui/`) has key types:

- `Session` — Main session state (screen buffer, fields, cursor)
- `SessionConfig` — Configuration (terminal model, colors)
- `EventSource` trait — Abstraction over input events (keyboard for TUI, programmatic for agent)
- `ScreenBuffer` — Character + attribute grid
- `Field` — Individual 3270 field (position, length, attributes, MDT)
- `AidKey` — Attention ID (Enter, PF1-24, PA1-3, Clear)
- `Renderer` — ratatui-based rendering (skip in headless mode)

**Key pattern for headless mode:** The `EventSource` trait already abstracts input. Create an `AgentEventSource` that reads from a channel/queue instead of crossterm keyboard events. Session processes events identically regardless of source.

### Rust Style

- 4 spaces indentation
- `snake_case` for functions, `PascalCase` for types
- Use existing `Result`/`Error` types from each crate
- `#[derive(Debug, Clone, Serialize, Deserialize)]` for API types
- Add `serde` dependency where needed for JSON serialization
- Tests in `#[cfg(test)]` modules at bottom of file

### Macro System (If touching compiler)

Statement dispatch uses `for_all_statement_variants!` and `for_parse_dispatch!` in `macros.rs`. If any epic requires new statements (unlikely for this loop), follow the documented pattern in `RALPH-PROMPT.md`.

---

## Epic Order

### Phase 0 — Foundation

| Order | Epic ID | Name | Complexity | Crate(s) | Description |
|-------|---------|------|------------|----------|-------------|
| 1 | E-AV-002 | Structured CLI Output (JSON) | M | `open-mainframe` | Add `--format json` to compile, run, check, interpret, parse-jcl subcommands |
| 2 | E-AV-001 | Agent Adapter Foundation | L | `open-mainframe`, `open-mainframe-tui` | Create `agent` subcommand, JSON-RPC over stdio, request/response loop |

### Phase 1 — Core TUI + Headless

| Order | Epic ID | Name | Complexity | Crate(s) | Description |
|-------|---------|------|------------|----------|-------------|
| 3 | 900 | Extended Screen Size (Model 2–5) | M | `open-mainframe-tui` | Multi-model screen buffer (24x80, 32x80, 43x80, 27x132) |
| 4 | 901 | PA Key Support | S | `open-mainframe-tui` | PA1-PA3 via Alt+1/2/3, short read (AID only, no field data) |
| 5 | 902 | Cursor Positioning | S | `open-mainframe-tui` | SEND MAP CURSOR(data-value), IC attribute precedence |
| 6 | E-AV-003 | Headless TUI Mode | L | `open-mainframe-tui` | AgentEventSource, Session::new_headless(), no terminal rendering |

### Phase 2 — Agentic Integration

| Order | Epic ID | Name | Complexity | Crate(s) | Description |
|-------|---------|------|------------|----------|-------------|
| 7 | E-AV-004 | Agent Session Management API | L | `open-mainframe` | Create/destroy/list sessions, session state queries |
| 8 | E-AV-005 | Screen Scraping & Field Extraction | M | `open-mainframe`, `open-mainframe-tui` | Read screen as JSON (fields, positions, values, attributes) |
| 9 | 903 | Field Validation Attributes | S | `open-mainframe-tui` | MUSTENTER, MUSTFILL, TRIGGER validation |
| 10 | E-AV-006 | Batch Orchestration API | M | `open-mainframe`, `open-mainframe-jcl` | Submit JCL, poll status, get spool output via agent API |

### Phase 3 — Advanced

| Order | Epic ID | Name | Complexity | Crate(s) | Description |
|-------|---------|------|------------|----------|-------------|
| 11 | E-AV-007 | Agent Observability | M | `open-mainframe`, `open-mainframe-deploy` | Prometheus metrics for agent API, structured logging |
| 12 | E-AV-008 | Multi-Agent Concurrent Sessions | L | `open-mainframe` | Concurrent session isolation, session pools |
| 13 | 904 | TN3270 Protocol (Optional) | L | `open-mainframe-tui` | Feature-flagged TN3270E connection |

---

## Epic Implementation Details

### E-AV-002: Structured CLI Output (JSON)

**Goal:** Every CLI subcommand supports `--format json` for machine-readable output.

**Stories:**

**E-AV-002.1: Global --format flag**
- Add `--format <text|json>` to top-level CLI args
- Default: `text` (backward compatible)
- Pass format through to all handlers

**E-AV-002.2: JSON output for `compile` / `check`**
```json
{
  "status": "success|error",
  "diagnostics": [
    {"severity": "error|warning|info", "message": "...", "file": "...", "line": 10, "col": 5}
  ],
  "summary": {"errors": 0, "warnings": 2}
}
```

**E-AV-002.3: JSON output for `interpret` / `run`**
```json
{
  "status": "success|error",
  "output": ["line1", "line2"],
  "return_code": 0,
  "diagnostics": [],
  "datasets_modified": ["EMPLOYEE.DAT"]
}
```

**E-AV-002.4: JSON output for `parse-jcl`**
```json
{
  "status": "success|error",
  "job_name": "PAYROLL",
  "steps": [
    {"name": "STEP01", "program": "PAYROLL", "dd_statements": [...]}
  ]
}
```

**Acceptance:** `cargo run -- compile examples/hello.cbl --format json` emits valid JSON. `--format text` produces identical output to current behavior. Tests for each subcommand.

---

### E-AV-001: Agent Adapter Foundation

**Goal:** Create `open-mainframe agent` subcommand that speaks JSON-RPC over stdio.

**Stories:**

**E-AV-001.1: Agent subcommand skeleton**
- Add `Agent` variant to CLI `Commands` enum
- Create `src/agent/mod.rs` in the CLI crate
- JSON-RPC loop: read line from stdin, parse as JSON, dispatch, write JSON to stdout

**E-AV-001.2: Protocol definition**

Request format:
```json
{"id": 1, "method": "compile", "params": {"file": "hello.cbl"}}
```

Response format:
```json
{"id": 1, "status": "success", "data": {...}}
```

Error format:
```json
{"id": 1, "status": "error", "error": {"code": "COMPILE_ERROR", "message": "...", "suggestion": "Check line 10"}}
```

**E-AV-001.3: Core methods**

Implement these methods in the agent dispatcher:
- `ping` — Health check, returns version info
- `compile` — Compile COBOL file, return diagnostics
- `interpret` — Run COBOL program, return output
- `check` — Syntax check only
- `run-jcl` — Execute JCL job
- `assess` — Run migration assessment on a COBOL file
- `list-datasets` — List available datasets
- `get-config` — Return current configuration

**E-AV-001.4: Agent protocol documentation**
- Add `docs/agent-protocol.md` describing the JSON-RPC protocol
- Include request/response examples for each method

**Acceptance:** `echo '{"id":1,"method":"ping"}' | cargo run -- agent` returns valid JSON response. All 8 methods functional. Tests for each method.

---

### Epic 900: Extended Screen Size (Model 2–5)

Read full specification from `_bmad-output/planning-artifacts/epics-tui-v3.0.md`.

**Key implementation:**
- Add `TerminalModel` enum: `Model2(24,80)`, `Model3(32,80)`, `Model4(43,80)`, `Model5(27,132)`
- Parameterize `ScreenBuffer::new(rows, cols)` instead of hardcoded 24x80
- Update `SessionConfig` with `terminal_model` field
- BMS maps designed for smaller screens render in top-left corner of larger screens
- All 43 existing tests must pass (they use Model 2 defaults)

---

### Epic 901: PA Key Support

Read full specification from `_bmad-output/planning-artifacts/epics-tui-v3.0.md`.

**Key implementation:**
- Add `PA1`, `PA2`, `PA3` to `AidKey` enum
- Map Alt+1/2/3 to PA keys in input handler
- PA keys trigger "short read" — only AID byte + cursor address, no field data
- HANDLE AID integration with CICS

---

### Epic 902: Cursor Positioning

Read full specification from `_bmad-output/planning-artifacts/epics-tui-v3.0.md`.

**Key implementation:**
- Add `cursor_position: Option<u16>` to SEND MAP processing
- CURSOR(data-value) takes precedence over IC field attribute
- If cursor lands in protected field, advance to next unprotected

---

### E-AV-003: Headless TUI Mode

**Goal:** Run TUI sessions without a physical terminal, for agent programmatic interaction.

**Stories:**

**E-AV-003.1: AgentEventSource**
- Create `AgentEventSource` implementing the `EventSource` trait
- Uses `mpsc::channel` to receive events programmatically
- Events: `SendKey(AidKey)`, `SetField(field_id, value)`, `GetScreen`, `GetField(field_id)`

**E-AV-003.2: Session::new_headless()**
- Constructor that skips terminal initialization (no crossterm, no ratatui)
- Uses `AgentEventSource` instead of keyboard
- All session logic (field management, AID processing, CICS integration) works identically
- Returns screen state as data structure instead of rendering

**E-AV-003.3: Screen state serialization**
```json
{
  "rows": 24, "cols": 80,
  "cursor": {"row": 5, "col": 10},
  "fields": [
    {"id": 0, "row": 3, "col": 10, "length": 20, "value": "EMPLOYEE NAME", "protected": true, "highlighted": false},
    {"id": 1, "row": 3, "col": 31, "length": 30, "value": "", "protected": false, "highlighted": false, "mdt": false}
  ],
  "unprotected_field_count": 5,
  "aid_enabled": ["ENTER", "PF1", "PF3", "PF12", "CLEAR"]
}
```

**E-AV-003.4: Integration test**
- Create a headless session
- Load a BMS map programmatically
- Set field values via `AgentEventSource`
- Press Enter
- Read resulting screen state
- Verify CICS transaction processing occurred

**Acceptance:** Headless session processes same CICS programs as interactive TUI. Screen state serializable to JSON. 10+ tests.

---

### E-AV-004: Agent Session Management API

**Goal:** Agent methods for managing CICS sessions.

**Methods to implement in agent dispatcher:**
- `session.create` — Create new headless CICS session with config
- `session.destroy` — Tear down session
- `session.list` — List active sessions with state summary
- `session.get-screen` — Get current screen content as JSON
- `session.set-field` — Write value to field by ID or position
- `session.send-key` — Press AID key (Enter, PF1-24, PA1-3, Clear)
- `session.get-field` — Read specific field value
- `session.wait` — Block until screen state changes (with timeout)

**Acceptance:** Full CICS transaction workflow via agent API: create session → get screen → set fields → send Enter → get result screen → destroy session. 15+ tests.

---

### E-AV-005: Screen Scraping & Field Extraction

**Goal:** Rich screen content extraction for agent understanding.

**Methods:**
- `screen.to-text` — Flat text representation (like screenshot)
- `screen.to-fields` — Structured field list with metadata
- `screen.find-field` — Find field by label text (look at protected field before an unprotected one)
- `screen.diff` — Compare two screen states, return changes

**Acceptance:** Agent can navigate a multi-screen CICS application by reading screens and filling forms. 10+ tests.

---

### Epic 903: Field Validation Attributes

Read full specification from `_bmad-output/planning-artifacts/epics-tui-v3.0.md`.

**Key implementation:**
- Add `validation: Option<FieldValidation>` to `Field`
- `FieldValidation` enum: `MustEnter`, `MustFill`, `Trigger`
- Validation check before AID processing
- Keyboard lock + error indicator on validation failure

---

### E-AV-006: Batch Orchestration API

**Goal:** Agent methods for JCL job management.

**Methods:**
- `batch.submit` — Submit JCL for execution, return job ID
- `batch.status` — Poll job status (SUBMITTED, RUNNING, COMPLETED, FAILED)
- `batch.output` — Get job spool output
- `batch.cancel` — Cancel running job
- `batch.list` — List recent jobs with status

**Acceptance:** Submit a JCL job via agent API, poll until complete, retrieve output. 8+ tests.

---

### E-AV-007: Agent Observability

**Goal:** Metrics and logging for agent API usage.

**Implementation:**
- Prometheus counters: `agent_requests_total`, `agent_errors_total`, `agent_sessions_active`
- Histogram: `agent_request_duration_seconds`
- Structured JSON logging for all agent API calls
- Integration with existing `open-mainframe-deploy` crate metrics

**Acceptance:** Prometheus metrics endpoint exposes agent counters. 5+ tests.

---

### E-AV-008: Multi-Agent Concurrent Sessions

**Goal:** Multiple agents can use the system simultaneously.

**Implementation:**
- Session pool with unique session IDs
- Each session has isolated state (screen buffer, CICS context, datasets)
- Thread-safe session management (Arc<Mutex<Session>> or similar)
- Session timeout/cleanup for abandoned sessions
- Rate limiting per session

**Acceptance:** 3+ concurrent agent sessions running different CICS transactions simultaneously without interference. 8+ tests.

---

### Epic 904: TN3270 Protocol (Optional)

Read full specification from `_bmad-output/planning-artifacts/epics-tui-v3.0.md`.

Feature-flagged with `#[cfg(feature = "tn3270")]`. Uses tokio for async network I/O. This is the final epic and extends the agent layer to connect to real z/OS systems.

---

## Implementation Protocol (Each Iteration)

### Step 1: Determine Current Epic
```
git log --oneline -30
```
Find the highest completed agent-layer epic. The next epic in order is your target.

### Step 2: Read the Epic Details
Read this file's epic details section. If the epic references a v3.0 planning artifact, read that too.

### Step 3: Read Relevant Source Files
Read the files you'll be modifying BEFORE making changes. Key files to understand:
- `crates/open-mainframe/src/main.rs` — CLI entry point
- `crates/open-mainframe-tui/src/session.rs` — Session logic
- `crates/open-mainframe-tui/src/renderer.rs` — Rendering
- `crates/open-mainframe-tui/src/input.rs` — Input handling
- `crates/open-mainframe-tui/src/fields.rs` — Field management
- `crates/open-mainframe-cics/src/` — CICS runtime
- `crates/open-mainframe-jcl/src/` — JCL interpreter

### Step 4: Implement All Stories in the Epic
Work through each story in order. For each story:
- Create/modify the necessary files
- Follow the acceptance criteria exactly
- Write tests (unit tests for logic, integration tests for API)
- Add `serde` derives to new types that need JSON serialization

### Step 5: Verify
Run these commands and fix any issues:
```bash
# Check the crate(s) you modified
cargo check -p open-mainframe 2>&1 | head -50
cargo test -p open-mainframe 2>&1 | tail -30

# If TUI was modified
cargo check -p open-mainframe-tui 2>&1 | head -50
cargo test -p open-mainframe-tui 2>&1 | tail -30

# Full workspace verification
cargo check --workspace 2>&1 | head -50
cargo test --workspace 2>&1 | tail -30
```

### Step 6: Commit
Once everything compiles and tests pass:
```
git add -A
git commit -m "feat(agent): implement E-AV-XXX — <epic name>

Implements stories E-AV-XXX.1 through E-AV-XXX.M covering:
- <brief list of what was added>

BMAD: Delivery phase, supports agent interaction layer for mainframe modernization"
```

For TUI epics (900-904), use:
```
git commit -m "feat(tui): implement Epic NNN — <epic name>

Implements stories NNN.1 through NNN.M covering:
- <brief list of what was added>

BMAD: Delivery phase, supports App View enhancement"
```

### Step 7: Exit
After committing, stop. The loop will restart and you'll pick up the next epic.

---

## Agentic Interaction Heuristics (Design Principles)

Apply these when implementing ANY agent-facing feature:

1. **Structured over Textual** — Every response has JSON representation. Agents read JSON, humans read text.
2. **State First** — Before any action, agent can query full system state.
3. **Idempotent Operations** — Same request with same params = same result (or "already done").
4. **Explicit Errors** — No silent failures. Every error has: code, message, suggestion.
5. **Progressive Disclosure** — Basic API is simple (compile, run). Advanced API (sessions, batch) is opt-in.
6. **Observable Side Effects** — Every state change emits a structured event.

---

## Agent Use Case Examples (What External Agents Will Do)

### Use Case 1: COBOL Migration Validation
```
Agent: compile old_program.cbl → get diagnostics
Agent: interpret old_program.cbl → capture output
Agent: compile modernized_program.java (external)
Agent: run modernized_program.java (external)
Agent: compare outputs → validate migration accuracy
```

### Use Case 2: CICS Transaction Testing
```
Agent: session.create → get session_id
Agent: session.get-screen → read login screen
Agent: session.set-field("userid", "TESTUSER")
Agent: session.set-field("password", "****")
Agent: session.send-key(ENTER) → get next screen
Agent: session.find-field("ACCOUNT") → get field_id
Agent: session.set-field(field_id, "12345")
Agent: session.send-key(PF3) → exit
Agent: session.destroy
```

### Use Case 3: Batch Job Migration
```
Agent: batch.submit(legacy.jcl) → get job_id
Agent: batch.status(job_id) → wait for COMPLETED
Agent: batch.output(job_id) → capture spool
Agent: assess(legacy_program.cbl) → get complexity score
Agent: [external] generate migration plan based on assessment
```

---

## Important Rules

1. **ONE EPIC PER ITERATION** — Do not try to do multiple epics.
2. **ALWAYS RUN TESTS** — Never commit without `cargo check --workspace` and `cargo test --workspace` passing.
3. **READ BEFORE WRITE** — Always read a file before modifying it.
4. **FOLLOW EXISTING PATTERNS** — Match the codebase conventions exactly.
5. **NO STUB IMPLEMENTATIONS** — Each feature must actually work. Agent API methods must return real data.
6. **BACKWARD COMPATIBILITY** — All 43 existing TUI tests must continue to pass. All existing CLI behavior unchanged when `--format json` is not specified.
7. **USE THE COMPILER** — Run `cargo check` early and often. The Rust compiler tells you what to fix.
8. **JSON SERIALIZATION** — Use `serde` and `serde_json`. Derive `Serialize`/`Deserialize` on all API types.
9. **ERROR HANDLING** — Every agent method returns structured errors, never panics.
10. **TESTS ARE MANDATORY** — Each epic must add tests. Agent API methods need at least 1 test each.

---

## Completion

When ALL epics have been committed (E-AV-001 through E-AV-008 plus Epics 900–904, visible in `git log`), output:

<promise>AGENT_LAYER_COMPLETE</promise>

This means: External coding agents can now compile COBOL, run JCL, interact with CICS transactions, assess migration complexity, and orchestrate batch jobs — all through a structured JSON API, enabling mainframe modernization workflows without manual terminal interaction.
