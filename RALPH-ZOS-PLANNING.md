# Ralph Loop Prompt: BMAD Planning — Fill All z/OS Gaps

## Mission

You are executing the **BMAD Planning Pipeline** to produce implementation-ready artifacts for **every gap** between OpenMainframe and real z/OS. This covers **234 epics** (214 existing from gap batches 1-21 + 20 new SYS-100 through SYS-119 from the utilities research).

Each planning group (PG) gets the full BMAD treatment:
1. **PRD** — Product Requirements Document with functional and non-functional requirements
2. **Architecture** — Technical architecture with design decisions and crate structure
3. **Epics & Stories** — Implementation-ready epics with user stories, acceptance criteria, and cargo test expectations

---

## Input Documents

### Research & Gap Analysis
- **Complete Inventory:** `_bmad-output/zos-complete-inventory.md` (387 components, 50 new gaps, cross-references)
- **Gap Batches 1-21:** `_bmad-output/gap-analysis/batch-{01..21}-*.md` (214 epics, 1,733+ features)
- **Priority Matrix:** `_bmad-output/gap-analysis/batch-22-priority-matrix.md` (Phase A-F ordering, scoring)

### Existing Planning Artifacts (DO NOT RECREATE — extend or reference)
**v3.0 series (PRD + Architecture + Epics):**
- JCL, CICS, DB2, IMS, Runtime/LE, Dataset, Encoding, Sort, TUI, Assess, Deploy

**v4.0 series (Epic refinements only):**
- REXX, HLASM, PL/I, RACF, JES2, TSO/ISPF, MQ, LE

**Zowe v1.0 (separate track — complete):**
- PRD, Architecture, Epics (10 epics, 62 stories — DONE)

### Codebase
- 24-crate Rust workspace at `crates/`
- Workspace root: `Cargo.toml`
- All 60 base epics (Phases A-E) implemented + Zowe (62 stories) implemented

---

## Iteration Protocol

**Before doing ANY work, check your progress:**

1. Read the progress tracker: `_bmad-output/zos-gap-planning-progress.md`
   - If it doesn't exist, create it (see Progress Tracker Format below)
   - If it exists, find the first planning group with status `pending` or `in_progress`

2. For the current planning group:
   a. Read ALL relevant input documents (gap batch, inventory section, existing v3/v4 artifacts)
   b. Determine what already exists vs what needs creation
   c. Execute the BMAD pipeline (PRD → Architecture → Epics)
   d. Run quality checks
   e. Mark the group complete, advance to next

3. If ALL planning groups are complete, output the completion promise

4. **One planning group per iteration** — complete all 3 artifacts before moving to the next group

---

## Planning Groups (Ordered by Phase Priority)

### Phase A — Foundation Layer (PG-1 through PG-7)

| PG | Technology | Gap Batch | Existing Plan | Epic IDs | New SYS Epics | Action |
|----|-----------|-----------|---------------|----------|---------------|--------|
| PG-1 | MVS System Services & SVCs | NONE | NONE | — | SYS-100, SYS-101 | **CREATE NEW** (PRD + Arch + Epics) |
| PG-2 | RACF Extensions | Batch 08 | v4.0 epics | RACF100-109 | SYS-109, SYS-110 | **EXTEND** v4.0 with SYS epics |
| PG-3 | Language Environment Extensions | Batch 12 | v4.0 epics, runtime-v3.0 | LE100-108 | — | **VALIDATE** v4.0 against inventory |
| PG-4 | JES2 Extensions | Batch 11 | v4.0 epics | J100-J112 | SYS-108 | **EXTEND** v4.0 with SYS-108 |
| PG-5 | Utilities & DFSORT | Batch 21 | sort-v3.0 (partial) | UTIL100-110 | — | **CREATE NEW** for full utilities scope |
| PG-6 | DFSMS, HSM & Catalog Extensions | Batch 19 | dataset-v3.0 | DFSMS100-110 | SYS-106, SYS-107 | **EXTEND** v3.0 with SYS epics |
| PG-7 | ABEND Framework & Debugging | NONE | NONE | — | SYS-111 | **CREATE NEW** (PRD + Arch + Epics) |

### Phase B — Interactive & Scripting (PG-8 through PG-12)

| PG | Technology | Gap Batch | Existing Plan | Epic IDs | New SYS Epics | Action |
|----|-----------|-----------|---------------|----------|---------------|--------|
| PG-8 | TSO/ISPF Extensions | Batch 09 | v4.0 epics | T100-T112 | — | **VALIDATE** v4.0 against inventory |
| PG-9 | REXX Extensions | Batch 01 | v4.0 epics | R100-R111 | — | **VALIDATE** v4.0 against inventory |
| PG-10 | System Commands & Console (SDSF) | NONE | NONE | — | SYS-102, SYS-103 | **CREATE NEW** (PRD + Arch + Epics) |
| PG-11 | Program Management & Binder | NONE | NONE | — | SYS-104, SYS-105 | **CREATE NEW** (PRD + Arch + Epics) |
| PG-12 | WLM (Workload Manager) | Batch 17 | NONE | WLM100-111 | — | **CREATE NEW** (PRD + Arch + Epics) |

### Phase C — Middleware & Communications (PG-13 through PG-19)

| PG | Technology | Gap Batch | Existing Plan | Epic IDs | New SYS Epics | Action |
|----|-----------|-----------|---------------|----------|---------------|--------|
| PG-13 | HLASM Extensions | Batch 02 | v4.0 epics | H100-H110 | — | **VALIDATE** v4.0 against inventory |
| PG-14 | IBM MQ Extensions | Batch 10 | v4.0 epics | MQ100-MQ112 | — | **VALIDATE** v4.0 against inventory |
| PG-15 | USS & POSIX Extensions | Batch 18 | NONE | USS100-111 | SYS-118, SYS-119 | **CREATE NEW** (PRD + Arch + Epics) |
| PG-16 | Networking & TCP/IP | Batch 20 | tui-v3.0 (partial) | NET100-109 | — | **CREATE NEW** for full networking scope |
| PG-17 | SMF Extensions | Batch 14 | NONE | SMF100-110 | — | **CREATE NEW** (PRD + Arch + Epics) |
| PG-18 | COBOL Precompilers (DB2 + CICS) | NONE | NONE | — | SYS-112, SYS-113 | **CREATE NEW** (PRD + Arch + Epics) |
| PG-19 | CICS BMS & Extensions | Batch N/A | cics-v3.0 | — | SYS-116 | **EXTEND** v3.0 with SYS-116 |

### Phase D — Extended Languages (PG-20 through PG-23)

| PG | Technology | Gap Batch | Existing Plan | Epic IDs | New SYS Epics | Action |
|----|-----------|-----------|---------------|----------|---------------|--------|
| PG-20 | PL/I Extensions | Batch 03 | v4.0 epics | PL100-PL112 | — | **VALIDATE** v4.0 against inventory |
| PG-21 | CLIST | Batch 05 | NONE | CL100-CL106 | — | **CREATE NEW** (PRD + Arch + Epics) |
| PG-22 | System Init & PARMLIB | NONE | NONE | — | SYS-117 | **CREATE NEW** (PRD + Arch + Epics) |
| PG-23 | Crypto, PKI & Security Infra | NONE | NONE | — | — | **CREATE NEW** (PRD + Arch + Epics) |

### Phase E — Transaction Monitors & Databases (PG-24 through PG-28)

| PG | Technology | Gap Batch | Existing Plan | Epic IDs | New SYS Epics | Action |
|----|-----------|-----------|---------------|----------|---------------|--------|
| PG-24 | IMS Extensions | Batch 13 | ims-v3.0 | IMS100-109 | — | **VALIDATE** v3.0 against inventory |
| PG-25 | DB2 Utilities & BIND | Batch N/A | db2-v3.0 | — | SYS-114, SYS-115 | **EXTEND** v3.0 with SYS epics |
| PG-26 | Easytrieve | Batch 04 | NONE | ET100-ET107 | — | **CREATE NEW** (PRD + Arch + Epics) |
| PG-27 | IDMS | Batch 15 | NONE | IDMS100-108 | — | **CREATE NEW** (PRD + Arch + Epics) |
| PG-28 | ADABAS | Batch 16 | NONE | ADA100-ADA108 | — | **CREATE NEW** (PRD + Arch + Epics) |

### Phase F — Niche & Legacy (PG-29 through PG-30)

| PG | Technology | Gap Batch | Existing Plan | Epic IDs | New SYS Epics | Action |
|----|-----------|-----------|---------------|----------|---------------|--------|
| PG-29 | Natural | Batch 06 | NONE | NAT100-NAT109 | — | **CREATE NEW** (PRD + Arch + Epics) |
| PG-30 | FOCUS | Batch 07 | NONE | FOC100-FOC107 | — | **CREATE NEW** (PRD + Arch + Epics) |

---

## Action Type Definitions

### CREATE NEW — Full BMAD Pipeline

For planning groups with **no existing planning artifacts**, produce all 3 documents.

#### Step 1: Create PRD
**Output:** `_bmad-output/planning-artifacts/prd-{tech}-v5.0.md`

```yaml
---
version: 'v5.0'
planningGroup: 'PG-N'
technology: '{technology name}'
date: '{today}'
status: 'complete'
inputDocuments:
  - 'gap-analysis/batch-{NN}-{tech}.md'
  - 'zos-complete-inventory.md (AREA-N)'
  - 'gap-analysis/batch-22-priority-matrix.md'
epicIds: ['XXX-100', 'XXX-101', ...]
sysEpicIds: ['SYS-NNN', ...]
---
```

PRD must include:
1. **Problem Statement** — What's missing in OpenMainframe for this technology area
2. **User Personas** — Who benefits (mainframe developer, system programmer, operator, etc.)
3. **Functional Requirements** (FR-{TECH}-001 through FR-{TECH}-NNN):
   - Extract from gap batch epic descriptions and story lists
   - Extract from zos-complete-inventory.md component tables
   - Each FR must be testable with specific acceptance criteria
4. **Non-Functional Requirements** (NFR-{TECH}-001 through NFR-{TECH}-NNN):
   - Performance: processing speed, memory limits
   - Compatibility: z/OS behavior fidelity, EBCDIC handling
   - Integration: how it connects to existing crates
5. **Scope** — MVP vs Full vs Deferred, referencing Phase placement from batch-22
6. **Dependencies** — Which other PGs or existing crates are prerequisites

#### Step 2: Create Architecture
**Output:** `_bmad-output/planning-artifacts/architecture-{tech}-v5.0.md`

```yaml
---
version: 'v5.0'
planningGroup: 'PG-N'
technology: '{technology name}'
date: '{today}'
status: 'complete'
inputDocuments:
  - 'prd-{tech}-v5.0.md'
---
```

Architecture must include:
1. **Crate Strategy** — New crate, extend existing crate, or module within zos-core
2. **Module Layout** — File/directory structure within the crate
3. **Key Types** — Core structs, enums, traits with Rust signatures
4. **Integration Points** — How this crate connects to existing 24 crates
5. **Data Flow** — How data moves through the system for key operations
6. **Design Decisions** — Choices made and rationale (e.g., trait-based vs enum dispatch)
7. **Error Handling** — Error types using thiserror + miette pattern
8. **Testing Strategy** — Unit test approach, integration test approach, test data

#### Step 3: Create Epics & Stories
**Output:** `_bmad-output/planning-artifacts/epics-{tech}-v5.0.md`

```yaml
---
version: 'v5.0'
planningGroup: 'PG-N'
technology: '{technology name}'
date: '{today}'
status: 'complete'
inputDocuments:
  - 'prd-{tech}-v5.0.md'
  - 'architecture-{tech}-v5.0.md'
totalEpics: N
totalStories: N
frCoverage: 'N/N (100%)'
nfrCoverage: 'N/N (100%)'
---
```

Epics & Stories must include:
1. **Epic list** with dependency order, size (S/M/L/XL), and user value summary
2. **For each epic:**
   - Title and description
   - User stories in "As a [user], I want [capability], so that [value]" format
   - Acceptance criteria in Given/When/Then format
   - Size estimate per story (each story = 1 dev agent session)
   - Story count: 4-8 per epic
3. **FR/NFR Coverage Matrix** — Every FR and NFR mapped to at least one story
4. **Dependency Graph** — Epic execution order within this PG

### EXTEND — Add New Epics to Existing Plans

For planning groups where v3.0/v4.0 artifacts exist but new SYS epics need adding:

1. Read existing PRD/Architecture/Epics documents
2. Read the relevant AREA section from `zos-complete-inventory.md`
3. Create an **addendum** document:
   **Output:** `_bmad-output/planning-artifacts/epics-{tech}-v5.0-addendum.md`
4. The addendum contains:
   - New SYS epic(s) with full stories and acceptance criteria
   - Updated dependency graph showing how SYS epics relate to existing epics
   - Any new FRs discovered from the inventory research
   - Reference to parent document (v3.0 or v4.0)

### VALIDATE — Confirm Existing Plans Cover Inventory

For planning groups where v4.0 epics exist and no new SYS epics are needed:

1. Read existing v4.0 epic document
2. Read the relevant AREA section from `zos-complete-inventory.md`
3. Cross-check: Does every component in the inventory appear in at least one story?
4. Create a **validation report:**
   **Output:** `_bmad-output/planning-artifacts/validation-{tech}-v5.0.md`
5. The report contains:
   - Coverage percentage (components in inventory covered by existing stories)
   - Any gaps found (components in inventory NOT covered by any story)
   - If gaps > 0: add new stories to an addendum file
   - If gaps = 0: mark as "Validated — no changes needed"

---

## Autonomous Decision Context

Since this is a Ralph Loop (no interactive user), make these decisions autonomously:

- **Crate naming:** Follow existing pattern `open-mainframe-{tech}` (e.g., `open-mainframe-wlm`, `open-mainframe-sdsf`)
- **New crate vs module:** If the technology has 5+ epics, it gets its own crate. Under 5 epics, it's a module in the most relevant existing crate or in a new `open-mainframe-mvs` core crate.
- **Story sizing:** Each story must be completable by a single dev agent in one session (~200-400 lines of Rust)
- **Test approach:** Unit tests with `#[cfg(test)] mod tests` in each file; integration tests in `tests/` directory
- **Error types:** `thiserror::Error` + `miette::Diagnostic` per crate
- **MSRV:** Rust 1.82 (match existing workspace)
- **No unsafe code:** `#![forbid(unsafe_code)]` in all new crates

---

## Progress Tracker Format

Create `_bmad-output/zos-gap-planning-progress.md`:

```markdown
---
currentGroup: PG-1
groupsComplete: 0
groupsTotal: 30
lastUpdated: ""
---

# z/OS Gap Planning Progress

## Phase A — Foundation Layer
- [ ] PG-1: MVS System Services & SVCs (CREATE NEW)
- [ ] PG-2: RACF Extensions (EXTEND)
- [ ] PG-3: Language Environment (VALIDATE)
- [ ] PG-4: JES2 Extensions (EXTEND)
- [ ] PG-5: Utilities & DFSORT (CREATE NEW)
- [ ] PG-6: DFSMS, HSM & Catalog (EXTEND)
- [ ] PG-7: ABEND Framework & Debugging (CREATE NEW)

## Phase B — Interactive & Scripting
- [ ] PG-8: TSO/ISPF (VALIDATE)
- [ ] PG-9: REXX (VALIDATE)
- [ ] PG-10: System Commands & Console (CREATE NEW)
- [ ] PG-11: Program Management & Binder (CREATE NEW)
- [ ] PG-12: WLM (CREATE NEW)

## Phase C — Middleware & Communications
- [ ] PG-13: HLASM (VALIDATE)
- [ ] PG-14: IBM MQ (VALIDATE)
- [ ] PG-15: USS & POSIX (CREATE NEW)
- [ ] PG-16: Networking & TCP/IP (CREATE NEW)
- [ ] PG-17: SMF (CREATE NEW)
- [ ] PG-18: COBOL Precompilers (CREATE NEW)
- [ ] PG-19: CICS BMS & Extensions (EXTEND)

## Phase D — Extended Languages
- [ ] PG-20: PL/I (VALIDATE)
- [ ] PG-21: CLIST (CREATE NEW)
- [ ] PG-22: System Init & PARMLIB (CREATE NEW)
- [ ] PG-23: Crypto, PKI & Security (CREATE NEW)

## Phase E — Transaction Monitors & Databases
- [ ] PG-24: IMS (VALIDATE)
- [ ] PG-25: DB2 Utilities & BIND (EXTEND)
- [ ] PG-26: Easytrieve (CREATE NEW)
- [ ] PG-27: IDMS (CREATE NEW)
- [ ] PG-28: ADABAS (CREATE NEW)

## Phase F — Niche & Legacy
- [ ] PG-29: Natural (CREATE NEW)
- [ ] PG-30: FOCUS (CREATE NEW)

## Summary
- Total planning groups: 30
- CREATE NEW: 18
- EXTEND: 5
- VALIDATE: 7
- Artifacts produced: 0
- Total epics planned: 0/234
- Total stories planned: 0/~1400
```

Mark items with `[x]` when complete. Update counters after each group.

---

## Web Research Strategy

For each CREATE NEW planning group, research:
1. IBM documentation for the specific z/OS component
2. Component behavior, interfaces, and configuration options
3. Common real-world usage patterns
4. Error codes and edge cases specific to this component

Use web search for:
- `site:ibm.com z/OS {component} programming guide`
- `site:ibm.com z/OS {component} reference`
- `z/OS {component} examples tutorial`
- `{component} ABEND codes error handling`

---

## Output File Locations

```
# CREATE NEW artifacts (v5.0 series)
_bmad-output/planning-artifacts/prd-{tech}-v5.0.md
_bmad-output/planning-artifacts/architecture-{tech}-v5.0.md
_bmad-output/planning-artifacts/epics-{tech}-v5.0.md

# EXTEND artifacts (v5.0 addenda)
_bmad-output/planning-artifacts/epics-{tech}-v5.0-addendum.md

# VALIDATE artifacts (v5.0 validation reports)
_bmad-output/planning-artifacts/validation-{tech}-v5.0.md

# Progress tracker
_bmad-output/zos-gap-planning-progress.md
```

---

## Quality Standards

1. **Testable requirements** — No "fast", "easy", "intuitive". Use measurable thresholds.
2. **Traceability** — Every FR traces to inventory component, every story traces to FR
3. **Coverage** — 100% FR coverage, 100% NFR coverage in every epic document
4. **Rust conventions** — Architecture documents specify Rust types, traits, modules
5. **Existing pattern compliance** — Match the patterns in v3.0/v4.0 artifacts
6. **Story sizing** — 4-8 stories per epic, each story = 1 dev agent session
7. **No orphan requirements** — Every requirement must be addressed by architecture and stories
8. **Dependency awareness** — Note cross-PG dependencies (e.g., PG-1 MVS services needed by PG-4 JES2)
9. **High information density** — No filler text, no redundant descriptions
10. **YAML frontmatter** — Every output file has structured frontmatter for machine parsing

---

## Rules

1. **One planning group per iteration** — Complete all artifacts for PG-N before starting PG-N+1
2. **Read before writing** — Always read existing artifacts before creating extensions or validations
3. **Don't recreate** — If v3.0 or v4.0 artifacts already cover the scope, VALIDATE rather than recreate
4. **Inventory is truth** — The `zos-complete-inventory.md` is the authoritative component list
5. **Batch-22 is priority** — Follow the Phase A-F ordering from the priority matrix
6. **Cross-reference** — Use the cross-reference section in the inventory to link areas to batches
7. **Be specific** — Architecture docs must include actual Rust type signatures, not pseudocode
8. **Test every FR** — Every functional requirement must have at least one story with Given/When/Then
9. **No speculative features** — Only plan what the gap analysis and inventory explicitly identify
10. **Commit nothing** — This is planning only. No code changes, no Cargo.toml edits.

---

## Completion Promise

When ALL 30 planning groups are complete (all marked `[x]` in progress tracker):

```
ZOS GAP PLANNING COMPLETE — 30/30 groups, ~234 epics, ~1400 stories planned
```

Signal completion with the promise tag containing the text above.
