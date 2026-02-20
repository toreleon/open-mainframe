# Ralph Loop Prompt: BMAD Planning — Zowe Integration Implementation Plan

## Mission

You are executing the **BMAD Planning Pipeline** to produce implementation-ready artifacts for the **Zowe Integration** feature of **OpenMainframe**. This takes the completed technical research and transforms it into:

1. **PRD** — Product Requirements Document with functional and non-functional requirements
2. **Architecture** — Technical architecture document with design decisions and system structure
3. **Epics & Stories** — Implementation-ready epics with user stories and acceptance criteria

All artifacts build upon the completed research at:
`_bmad-output/planning-artifacts/research-zowe-integration.md`

---

## Iteration Protocol

**Before doing ANY work, check your progress:**

1. Check which output files exist and their `stepsCompleted` frontmatter:
   - `_bmad-output/planning-artifacts/prd-zowe-v1.0.md`
   - `_bmad-output/planning-artifacts/architecture-zowe-v1.0.md`
   - `_bmad-output/planning-artifacts/epics-zowe-v1.0.md`
2. Determine current phase and resume from next incomplete step
3. If ALL three documents are complete (PRD steps 1-11, Architecture steps 1-8, Epics steps 1-4), output `<promise>ZOWE PLANNING COMPLETE</promise>` and stop

---

## Project Context

### OpenMainframe
- Open-source Rust mainframe emulator (24 crates): COBOL, JCL, REXX, HLASM, PL/I, ISPF, TSO, RACF, JES2, CICS, IMS, DB2, MQ, VSAM, SMF, WLM
- All 60 base epics (Phases A–E) are complete
- Repository: `Cargo.toml` workspace, `crates/` directory, Apache-2.0 license

### Zowe Integration (from research)
- Goal: Expose OpenMainframe subsystems via z/OSMF-compatible REST APIs so Zowe CLI, Zowe Explorer, and API ML can connect
- Recommended architecture: New `open-mainframe-zosmf` crate using Axum framework
- 10 proposed epics (ZOW-100 to ZOW-109) identified in research
- Key subsystems to expose: datasets, jobs, TSO, console, USS files, authentication

### Input Documents
- **Research:** `_bmad-output/planning-artifacts/research-zowe-integration.md` (1075 lines, 6 steps complete)
- **Existing PRD:** `_bmad-output/planning-artifacts/prd.md` (base project — reference for format)
- **Existing Architecture:** `_bmad-output/planning-artifacts/architecture.md` (base project — reference for format)
- **Existing Epics:** `_bmad-output/planning-artifacts/epics.md` (base project — reference for format)

---

## Phase 1: Create PRD (11 Steps)

**Output:** `_bmad-output/planning-artifacts/prd-zowe-v1.0.md`
**Role:** Product Manager — extract requirements from research into a structured PRD

### Autonomous Decision Context
Since this is a Ralph Loop (no interactive user), make these decisions autonomously:
- **Target users:** Mainframe developers, DevOps engineers, system programmers using Zowe tooling
- **Project type:** API service / developer tool
- **Domain:** Enterprise legacy systems / mainframe modernization
- **Complexity:** High / enterprise
- **Context:** Brownfield (extending existing OpenMainframe project)
- **MVP scope:** z/OSMF REST server + datasets + jobs + authentication (ZOW-100 through ZOW-103)
- **Growth scope:** TSO + console + USS + TN3270E (ZOW-104 through ZOW-107)
- **Vision scope:** API ML integration + conformance certification (ZOW-108, ZOW-109)

### Step 1 — Init
Create `prd-zowe-v1.0.md` with YAML frontmatter:
```yaml
---
stepsCompleted: []
inputDocuments: [research-zowe-integration.md]
classification:
  projectType: api_service
  domain: enterprise_legacy_systems
  complexity: enterprise
  projectContext: brownfield
---
```
Read the research document to extract all findings. Mark step 1 complete.

### Step 2 — Discovery & Classification
Document the project classification:
- **Type:** REST API server extending existing Rust workspace
- **Domain:** z/OS mainframe emulation, Zowe ecosystem integration
- **Complexity:** Enterprise — multiple subsystem integrations, security protocols, API compatibility
- **Context:** Brownfield — extends 24 existing crates with new z/OSMF REST layer
Mark step 2 complete.

### Step 3 — Success Criteria
Define measurable success criteria from research findings:
- **User Success:** Zowe CLI `zos-files ls ds` returns dataset list within 200ms; `zos-jobs submit` returns job ID; `zos-tso issue cmd` returns command output
- **Business Success:** Standard Zowe tooling works without custom plugins; Zowe Conformance achievable
- **Technical Success:** All z/OSMF endpoints pass Zowe CLI integration tests; RACF auth integration works with JWT
Mark step 3 complete.

### Step 4 — User Journeys
Write narrative user journeys based on research (Zowe CLI user, VS Code developer, DevOps engineer).
Mark step 4 complete.

### Step 5 — Domain Requirements
Document mainframe-specific domain requirements (EBCDIC handling, dataset naming conventions, JCL format, RACF security model).
Mark step 5 complete.

### Step 6 — Innovation Analysis
Document competitive differentiation (only open-source z/OSMF-compatible implementation; runs on Linux/Docker/K8s; Rust performance).
Mark step 6 complete.

### Step 7 — Project Type Requirements
Document API-specific requirements (REST conventions, JSON schemas, HTTPS/TLS, CORS, CSRF headers, authentication flows).
Mark step 7 complete.

### Step 8 — Scoping
Refine MVP/Growth/Vision scope boundaries using the research's proposed epic structure.
Mark step 8 complete.

### Step 9 — Functional Requirements
Extract ALL functional requirements from research. Each FR must be:
- Uniquely numbered (FR-ZOW-001 through FR-ZOW-NNN)
- Testable with specific acceptance criteria
- Mapped to a research section
- Covering: z/OSMF info, auth, datasets, jobs, TSO, console, USS, TN3270E, API ML

**Sources for FRs:**
- Step 2 of research: z/OSMF REST API endpoint specification
- Step 3 of research: Integration patterns and gap analysis tables
- Step 4 of research: Architectural requirements
- Step 5 of research: Implementation requirements

Mark step 9 complete.

### Step 10 — Non-Functional Requirements
Extract NFRs from research:
- **Performance:** Response times, throughput, concurrent connections
- **Security:** TLS 1.2+, JWT expiry, RACF integration, CSRF protection
- **Compatibility:** Zowe V3 LTS, z/OSMF API version compatibility
- **Reliability:** Graceful error handling matching z/OSMF error codes
- **Scalability:** Concurrent session limits, async I/O
- **Observability:** Integration with existing `open-mainframe-deploy` metrics

Mark step 10 complete.

### Step 11 — Polish
Review complete PRD for:
- No subjective adjectives (replace "fast" with "< 200ms")
- All FRs have testable criteria
- All NFRs have measurable thresholds
- Complete traceability to research document
- High information density (no filler)

Mark step 11 complete.

---

## Phase 2: Create Architecture (8 Steps)

**Output:** `_bmad-output/planning-artifacts/architecture-zowe-v1.0.md`
**Role:** Architect — design the technical solution
**Input Required:** `prd-zowe-v1.0.md` (must be complete before starting)

### Step 1 — Init
Create `architecture-zowe-v1.0.md` with frontmatter. Read PRD and research documents.
Mark step 1 complete.

### Step 2 — Project Context Analysis
Analyze all FRs and NFRs for architectural implications:
- Which crates need new public APIs?
- What new crates are needed?
- What are the integration boundaries?
- What are the performance constraints?
Mark step 2 complete.

### Step 3 — Starter Template Evaluation
Evaluate existing project structure:
- Brownfield: 24 existing crates in Rust workspace
- New crate `open-mainframe-zosmf` fits workspace pattern
- Axum + Tokio aligns with existing async patterns in `open-mainframe-deploy`
- Optional `open-mainframe-uss` crate for USS emulation
Mark step 3 complete.

### Step 4 — Core Architectural Decisions
Document decisions with rationale:
- **HTTP Framework:** Axum (reasons from research Step 5)
- **TLS:** rustls via axum-server (reasons from research)
- **Auth:** JWT via `jsonwebtoken` crate + RACF integration
- **JSON:** serde/serde_json with z/OSMF field naming conventions
- **Session Management:** In-memory token store with configurable TTL
- **USS Strategy:** Host filesystem mapping (Phase 1) vs full emulation (Phase 2)
Mark step 4 complete.

### Step 5 — Design Patterns
Document patterns:
- **Adapter Pattern:** Each REST handler module adapts z/OSMF JSON ↔ existing crate API types
- **Middleware Chain:** Auth → CSRF → Logging → Handler → JSON Response
- **State Management:** `Arc<AppState>` shared across Axum handlers with crate instances
- **Error Mapping:** z/OSMF error response format (HTTP status + JSON error body)
Mark step 5 complete.

### Step 6 — System Structure
Define component structure:
- Crate layout (`open-mainframe-zosmf/src/` modules)
- Dependency graph (which existing crates each module uses)
- Request flow diagrams (HTTP request → handler → crate API → response)
- Configuration model (ports, TLS certs, auth settings, USS root path)
Mark step 6 complete.

### Step 7 — Validation
Validate architecture against PRD:
- Every FR is addressable by the architecture
- Every NFR has an architectural mechanism
- No orphan requirements
Mark step 7 complete.

### Step 8 — Completion
Final review and architecture document polish.
Mark step 8 complete.

---

## Phase 3: Create Epics & Stories (4 Steps)

**Output:** `_bmad-output/planning-artifacts/epics-zowe-v1.0.md`
**Role:** Product Manager — break architecture into implementable units
**Input Required:** `prd-zowe-v1.0.md` AND `architecture-zowe-v1.0.md` (both must be complete)

### Step 1 — Validate Prerequisites
- Confirm PRD and Architecture are complete
- Extract ALL FRs and NFRs into a requirements inventory
- List all architectural components that need implementation
Mark step 1 complete.

### Step 2 — Design Epics
Design epics around **user value** (not technical layers):
- Each epic delivers complete, testable functionality
- Epics follow dependency order (foundation → features → integration)
- Epic naming follows project convention: ZOW-100, ZOW-101, etc.
- Map every FR to at least one epic
- Map every NFR to at least one epic

**Expected epic structure** (from research, refined by PRD):
| Epic | Title | User Value |
|---|---|---|
| ZOW-100 | z/OSMF Server Foundation & System Info | Users can connect Zowe CLI to OpenMainframe and verify connectivity |
| ZOW-101 | Authentication & Session Management | Users can authenticate with username/password and receive tokens |
| ZOW-102 | Dataset Operations via REST | Users can browse, read, write, create, delete datasets via Zowe |
| ZOW-103 | Job Management via REST | Users can submit JCL, monitor jobs, view spool output via Zowe |
| ZOW-104 | TSO Command Execution via REST | Users can issue TSO commands and receive output via Zowe |
| ZOW-105 | Console Command Execution via REST | Users can issue MVS console commands via Zowe |
| ZOW-106 | USS File Operations via REST | Users can browse and manage USS files via Zowe Explorer |
| ZOW-107 | TN3270E Server for Terminal Access | Users can access ISPF/TSO via Zowe Desktop's 3270 terminal |
| ZOW-108 | Zowe API Mediation Layer Integration | OpenMainframe appears in Zowe API Catalog with SSO |
| ZOW-109 | End-to-End Testing & Conformance | Zowe Conformance self-certification passes |

Present epic list. Mark step 2 complete.

### Step 3 — Create Stories
For EACH epic, generate user stories with:
- **Story format:** "As a [user], I want [capability], so that [value]"
- **Acceptance criteria:** Given/When/Then format
- **Size:** Each story completable by a single dev agent in one session
- **No future dependencies** within same epic
- **Database/infra created inline** — no separate "setup" stories

**Story count target:** 4–8 stories per epic (40–80 total)

**Critical rules:**
- Stories must create ONLY the infrastructure they need (no "setup all tables" stories)
- Each story must be independently testable with `cargo test`
- Stories must follow existing project quality standards (clippy-clean, no unsafe)

Mark step 3 complete.

### Step 4 — Final Validation
Validate:
- 100% FR coverage (every FR mapped to at least one story)
- 100% NFR coverage (every NFR addressed by at least one story)
- No circular dependencies between epics
- Each epic delivers standalone user value
- Story acceptance criteria are specific and testable
- Total story count is reasonable (40–80 range)

Generate coverage summary:
```
Total Epics: N
Total Stories: N
FR Coverage: N/N (100%)
NFR Coverage: N/N (100%)
```

Mark step 4 complete.

---

## Output File Locations

```
_bmad-output/planning-artifacts/prd-zowe-v1.0.md          # Phase 1 output
_bmad-output/planning-artifacts/architecture-zowe-v1.0.md  # Phase 2 output
_bmad-output/planning-artifacts/epics-zowe-v1.0.md         # Phase 3 output
```

## Quality Standards

- Follow the format of existing documents in `_bmad-output/planning-artifacts/`
- Use YAML frontmatter with `stepsCompleted` tracking in every output file
- Every requirement must be testable and measurable (no "fast", "easy", "intuitive")
- High information density — no filler text
- Traceability: Research → PRD → Architecture → Epics → Stories
- Epic/story naming follows project convention (ZOW-NNN)
- Reference the research document with specific section citations

## Phase Dependencies

```
Phase 1 (PRD)          → No dependencies, start immediately
Phase 2 (Architecture) → Requires Phase 1 complete
Phase 3 (Epics)        → Requires Phase 1 AND Phase 2 complete
```

## Completion Promise

When ALL three documents are complete with all steps marked done:
- `prd-zowe-v1.0.md` — steps 1–11 complete
- `architecture-zowe-v1.0.md` — steps 1–8 complete
- `epics-zowe-v1.0.md` — steps 1–4 complete

Output:
```
<promise>ZOWE PLANNING COMPLETE</promise>
```
