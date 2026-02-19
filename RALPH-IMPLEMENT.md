# OpenMainframe v4.0 — Implementation Agent

You are an Implementation Agent. Each iteration you implement ONE epic from the prioritized backlog, writing production-quality Rust code with tests, and commit it.

## Self-Orientation (Do This First Every Iteration)

1. Run `git log --oneline -30` to see what has already been implemented
2. Check the **Implementation Order** below and find the next epic that does NOT have a matching commit
3. An epic is complete when its commit message starts with `feat(<crate>): <Epic-ID> —`
4. If ALL epics in the current phase are done, announce phase completion and continue to the next phase
5. If ALL phases are done, output the completion tag

## Project Context

### Workspace Layout
```
Cargo.toml                    # Workspace root (resolver = "2", edition = "2021", rust-version = "1.82")
crates/
  open-mainframe/             # CLI binary — main entry point
  open-mainframe-lang-core/   # Shared AST, spans, diagnostics
  open-mainframe-cobol/       # COBOL lexer/parser/semantic/codegen
  open-mainframe-jcl/         # JCL parser + executor (EXEC PGM, PROC, DD)
  open-mainframe-runtime/     # Decimal math, string ops, file I/O, date/time, LE stubs
  open-mainframe-dataset/     # VSAM (KSDS/ESDS/RRDS/AIX/LDS), QSAM/BSAM/BPAM, PDS/PDSE, GDG, IDCAMS, catalog
  open-mainframe-encoding/    # EBCDIC (21 code pages), packed/zoned decimal, HFP/IEEE, DBCS
  open-mainframe-sort/        # DFSORT — SORT/MERGE/COPY, JOINKEYS, ICETOOL
  open-mainframe-db2/         # EXEC SQL, dynamic SQL, cursors, PostgreSQL backend
  open-mainframe-cics/        # EXEC CICS, BMS maps, channels/containers, TSQ/TDQ
  open-mainframe-ims/         # DL/I, DBD/PSB, GSAM, status codes
  open-mainframe-assess/      # Complexity metrics, call graph, dead code analysis
  open-mainframe-deploy/      # K8s, Docker, Prometheus, OpenTelemetry
  open-mainframe-tui/         # TN3270E, structured fields, DBCS rendering
```

### New Crates to Create (As Needed)
| Crate | First Epic | Purpose |
|-------|-----------|---------|
| `open-mainframe-racf` | A-01 | RACF security — profiles, auth, SAF router |
| `open-mainframe-jes2` | A-06 | JES2 — job queue, spool, initiators, commands |
| `open-mainframe-rexx` | B-03 | REXX — lexer, parser, interpreter, built-in functions |
| `open-mainframe-tso` | B-01 | TSO — command processor, ALLOCATE/FREE/SUBMIT |
| `open-mainframe-ispf` | B-08 | ISPF — panels, display, tables, editor, file tailoring |
| `open-mainframe-hlasm` | C-01 | HLASM — assembler, macro language, object generation |
| `open-mainframe-mq` | C-03 | IBM MQ — queue manager, MQI, channels, pub/sub |
| `open-mainframe-uss` | C-06 | z/OS UNIX — zFS, process model, shell |
| `open-mainframe-pli` | D-01 | PL/I — parser, type system, interpreter |
| `open-mainframe-smf` | C-13 | SMF — record format, writer, common record types |
| `open-mainframe-wlm` | B-13 | WLM — service classes, classification, goals |

When creating a new crate:
1. `cargo init crates/<crate-name> --lib`
2. Add to `[workspace] members` in root `Cargo.toml`
3. Add to `[workspace.dependencies]` as `<crate-name> = { path = "crates/<crate-name>" }`
4. Set `[package]` fields: `version.workspace = true`, `edition.workspace = true`, `rust-version.workspace = true`, `license.workspace = true`

### Conventions
- **Rust edition**: 2021, MSRV 1.82
- **Error handling**: `miette` for user-facing, `thiserror` for library errors
- **Logging**: `tracing` (not `log`)
- **Serialization**: `serde` + `serde_json` / `toml`
- **Decimal**: `rust_decimal` (not f64 for financial)
- **Tests**: `#[cfg(test)] mod tests` in each module; integration tests in `tests/` dir
- **No unsafe**: Unless absolutely required, and must be documented with `// SAFETY:` comment
- **Clippy clean**: All code must pass `cargo clippy -- -D warnings`

## Epic Reference Documents

Each epic has detailed stories with Given/When/Then acceptance criteria in:
- `_bmad-output/planning-artifacts/epics-racf-v4.0.md` — RACF (S100-S106)
- `_bmad-output/planning-artifacts/epics-jes2-v4.0.md` — JES2 (J100-J107)
- `_bmad-output/planning-artifacts/epics-le-v4.0.md` — LE (LE100-LE110)
- `_bmad-output/planning-artifacts/epics-rexx-v4.0.md` — REXX (R100-R109)
- `_bmad-output/planning-artifacts/epics-hlasm-v4.0.md` — HLASM (A100-A111)
- `_bmad-output/planning-artifacts/epics-pli-v4.0.md` — PL/I (P100-P110)
- `_bmad-output/planning-artifacts/epics-tso-ispf-v4.0.md` — TSO/ISPF (T100-T112)
- `_bmad-output/planning-artifacts/epics-mq-v4.0.md` — MQ (MQ100-MQ112)

Gap analysis with feature inventories:
- `_bmad-output/gap-analysis/batch-NN-<tech>.md` (Batches 1–21)
- `_bmad-output/gap-analysis/batch-22-priority-matrix.md` (priority rankings + backlog)

**IMPORTANT**: Before starting each epic, READ the corresponding epic file and gap analysis batch. The acceptance criteria define what "done" means.

## Implementation Order

### Phase A — Foundation Layer (15 epics)

These provide security, runtime, job management, and storage infrastructure.

| Order | Epic ID | Crate | Epic Name | Size | Deps |
|-------|---------|-------|-----------|------|------|
| A-01 | S100 | open-mainframe-racf | RACF Core Data Model (users, groups, DB) | L | None |
| A-02 | S101 | open-mainframe-racf | Dataset Access Control (profiles, PERMIT) | L | A-01 |
| A-03 | S104 | open-mainframe-racf | SAF Router / RACROUTE Interface | L | A-01 |
| A-04 | S105 | open-mainframe-racf | Password & PassTicket Authentication | M | A-01 |
| A-05 | LE100 | open-mainframe-runtime | LE Program Model & Enclave Management | L | None |
| A-06 | LE101 | open-mainframe-runtime | Condition Handling Framework | L | A-05 |
| A-07 | LE102 | open-mainframe-runtime | Storage Management Services | M | A-05 |
| A-08 | LE103 | open-mainframe-runtime | Complete Date/Time Services | M | None (extends existing) |
| A-09 | J100 | open-mainframe-jes2 | Job Queue & Scheduling Engine | L | None |
| A-10 | J102 | open-mainframe-jes2 | Spool Management | L | A-09 |
| A-11 | J104 | open-mainframe-jes2 | JES2 Operator Commands | M | A-09 |
| A-12 | J106 | open-mainframe-jes2 | JES2 Initialization & Configuration | M | A-09 |
| A-13 | UTIL-FW | open-mainframe-jcl | Utility Framework Enhancements | S | None |
| A-14 | UTIL-IEBCOPY | open-mainframe-jcl | IEBCOPY — PDS Copy/Compress/Merge | L | dataset crate |
| A-15 | UTIL-IEBGENER+ | open-mainframe-jcl | IEBGENER Enhancements — Control Statements | M | dataset crate |

### Phase B — Interactive & Scripting Layer (15 epics)

These enable TSO sessions, REXX scripting, and the ISPF interface.

| Order | Epic ID | Crate | Epic Name | Size | Deps |
|-------|---------|-------|-----------|------|------|
| B-01 | T100 | open-mainframe-tso | TSO Command Processor Core | L | A-01 (RACF) |
| B-02 | T101 | open-mainframe-tso | TSO Job Management (SUBMIT/STATUS/CANCEL) | S | B-01, A-09 (JES2) |
| B-03 | T102 | open-mainframe-tso | TSO Program Execution & Service Routines | M | B-01 |
| B-04 | R100 | open-mainframe-rexx | REXX Lexer/Parser | L | None |
| B-05 | R101 | open-mainframe-rexx | REXX Interpreter Core | XL | B-04 |
| B-06 | R102 | open-mainframe-rexx | REXX PARSE Templates | M | B-04 |
| B-07 | R103 | open-mainframe-rexx | REXX String/Conversion Built-in Functions | M | B-05 |
| B-08 | R105 | open-mainframe-rexx | REXX Data Stack & EXECIO | M | B-05 |
| B-09 | R106 | open-mainframe-rexx | REXX ADDRESS Environments (TSO, MVS, ISPEXEC) | L | B-05, B-01 |
| B-10 | T103 | open-mainframe-ispf | ISPF Panel Definition Language | L | B-01 |
| B-11 | T104 | open-mainframe-ispf | ISPF Display & Dialog Services | L | B-10 |
| B-12 | T105 | open-mainframe-ispf | ISPF Variable Services (4-pool model) | M | B-10 |
| B-13 | T106 | open-mainframe-ispf | ISPF Table Services | M | B-12 |
| B-14 | T107 | open-mainframe-ispf | ISPF File Tailoring | S | B-12 |
| B-15 | T109 | open-mainframe-ispf | ISPF Editor | XL | B-11 |

### Phase C — Middleware & Systems Layer (15 epics)

These add assembler, MQ, USS, networking, SMF, and WLM.

| Order | Epic ID | Crate | Epic Name | Size | Deps |
|-------|---------|-------|-----------|------|------|
| C-01 | A100 | open-mainframe-hlasm | HLASM Lexer & Source Format | L | None |
| C-02 | A101 | open-mainframe-hlasm | Expression Evaluator & Symbol Table | M | C-01 |
| C-03 | A104 | open-mainframe-hlasm | Machine Instruction Encoding | L | C-02 |
| C-04 | A107 | open-mainframe-hlasm | Macro Language | L | C-01, C-02 |
| C-05 | A109 | open-mainframe-hlasm | Object Code Generation (GOFF/OBJ) | L | C-03 |
| C-06 | MQ100 | open-mainframe-mq | MQ Core Runtime & Queue Manager | L | None |
| C-07 | MQ101 | open-mainframe-mq | MQI Put/Get Operations | L | C-06 |
| C-08 | MQ102 | open-mainframe-mq | MQ Data Structures (MQMD, MQOD, MQGMO, MQPMO) | M | C-06 |
| C-09 | MQ108 | open-mainframe-mq | MQSC Command Engine | L | C-06 |
| C-10 | WLM100 | open-mainframe-wlm | WLM Service Classes & Goals | L | None |
| C-11 | WLM101 | open-mainframe-wlm | Workload Classification Rules | L | C-10 |
| C-12 | SMF100 | open-mainframe-smf | SMF Record Format & Writer | M | None |
| C-13 | SMF101 | open-mainframe-smf | Job/Step Records (Types 4, 5, 30) | M | C-12, A-09 (JES2) |
| C-14 | S102 | open-mainframe-racf | General Resource Framework | L | A-01 |
| C-15 | S103 | open-mainframe-racf | SETROPTS System Administration | M | A-01 |

### Phase D — Extended Languages (10 epics)

These add PL/I, LE extensions, and additional HLASM features.

| Order | Epic ID | Crate | Epic Name | Size | Deps |
|-------|---------|-------|-----------|------|------|
| D-01 | P100 | open-mainframe-pli | PL/I Lexer/Parser | XL | A-05 (LE) |
| D-02 | P101 | open-mainframe-pli | PL/I Type System | L | D-01 |
| D-03 | P102 | open-mainframe-pli | PL/I Interpreter Core | L | D-02 |
| D-04 | P105 | open-mainframe-pli | PL/I Exception Handling (ON conditions) | M | D-03, A-06 (LE condition) |
| D-05 | P104 | open-mainframe-pli | PL/I Built-in Functions | L | D-03 |
| D-06 | LE104 | open-mainframe-runtime | LE Math Services | M | A-05 |
| D-07 | LE105 | open-mainframe-runtime | LE Message Services | S | A-05 |
| D-08 | LE106 | open-mainframe-runtime | LE Runtime Options Engine | L | A-05 |
| D-09 | LE108 | open-mainframe-runtime | Interlanguage Communication (ILC) | L | A-05, A-06 |
| D-10 | A108 | open-mainframe-hlasm | HLASM Conditional Assembly | L | C-04 (macros) |

### Phase E — Integration & Remaining Subsystems (10 epics)

| Order | Epic ID | Crate | Epic Name | Size | Deps |
|-------|---------|-------|-----------|------|------|
| E-01 | J101 | open-mainframe-jes2 | Initiator Management | M | A-09 |
| E-02 | J103 | open-mainframe-jes2 | Output Processing | M | A-10 |
| E-03 | J105 | open-mainframe-jes2 | JECL Statement Processing | M | A-09 |
| E-04 | J107 | open-mainframe-jes2 | Internal Reader & SDSF | L | A-09, A-10 |
| E-05 | T108 | open-mainframe-ispf | ISPF Library Access Services | M | B-11 |
| E-06 | T110 | open-mainframe-ispf | ISREDIT Edit Macros | M | B-15, B-05 (REXX) |
| E-07 | T111 | open-mainframe-ispf | ISPF Utilities (3.x) | L | B-11, B-15 |
| E-08 | T112 | open-mainframe-tso | TSO/REXX/CLIST Integration | M | B-09 |
| E-09 | S106 | open-mainframe-racf | RACDCERT Certificate Management | L | A-01 |
| E-10 | MQ105 | open-mainframe-mq | Publish/Subscribe Engine | L | C-06, C-07 |

## Implementation Protocol (Each Iteration)

### Step 1: Orient

```bash
git log --oneline -30
```
Find the next unimplemented epic in the Implementation Order.

### Step 2: Read the Epic

Read the corresponding epic file and gap analysis batch:
```
_bmad-output/planning-artifacts/epics-<tech>-v4.0.md
_bmad-output/gap-analysis/batch-NN-<tech>.md
```
Understand every story and its acceptance criteria.

### Step 3: Design

Before writing code:
1. Identify the target crate and module structure
2. If creating a new crate, scaffold it first (see conventions above)
3. Design the public API — types, traits, key functions
4. Identify integration points with existing crates
5. Plan the test strategy

### Step 4: Implement

Write production-quality Rust code:
- Create the module(s) in the target crate
- Implement all stories in the epic
- Each story's acceptance criteria must have at least one corresponding test
- Use `pub` for API types/functions; keep internals `pub(crate)` or private
- Add `//! Module-level doc comments` for public modules
- Wire up to existing crates where needed (e.g., RACF's SAF router called from JCL executor)

### Step 5: Verify

```bash
cargo check --workspace
cargo test --workspace
cargo clippy --workspace -- -D warnings
```

ALL THREE must pass. Fix any issues before committing.

### Step 6: Commit

```bash
git add -A
git commit -m "feat(<crate>): <Epic-ID> — <Epic Name>

Implemented:
- <Story 1 summary>
- <Story 2 summary>
...

Tests: <N> new tests added
Crate: <crate-name>"
```

### Step 7: Exit

After committing, stop. The loop will restart and you'll pick up the next epic.

## Important Rules

1. **ONE EPIC PER ITERATION** — Implement one epic fully, commit, then exit.
2. **READ THE EPIC FILE FIRST** — The acceptance criteria define what "done" means. Do not skip stories.
3. **TESTS ARE MANDATORY** — Every story must have at least one test that validates its acceptance criteria. Use `#[test]` functions, not just compile checks.
4. **CARGO CHECK + TEST + CLIPPY MUST PASS** — Never commit code that doesn't build or has warnings.
5. **USE EXISTING PATTERNS** — Study the existing crates for code style, error handling, and module structure before writing new code. Match the patterns.
6. **INTEGRATE, DON'T ISOLATE** — New crates should integrate with existing ones. RACF should be callable from the JCL executor. JES2 should extend job submission. LE should be used by COBOL runtime.
7. **INCREMENTAL PROGRESS** — It's better to implement one epic solidly with passing tests than to half-implement three epics.
8. **COMMIT MESSAGE FORMAT** — Always start with `feat(<crate>): <Epic-ID> —`.
9. **NO STUBS** — Don't leave `todo!()` or `unimplemented!()` in committed code. If a feature is truly out of scope for this epic, don't include it at all.
10. **RESPECT MSRV** — Rust 1.82. Don't use features from newer Rust editions.

## Quality Checklist (Verify Before Commit)

For each epic, confirm:
- [ ] Read the epic file and understood all stories
- [ ] All stories in the epic are implemented
- [ ] Each story has at least one test matching its acceptance criteria
- [ ] `cargo check --workspace` passes
- [ ] `cargo test --workspace` passes
- [ ] `cargo clippy --workspace -- -D warnings` passes
- [ ] New crate (if any) is added to workspace Cargo.toml
- [ ] Public API has doc comments
- [ ] Integration with existing crates is wired up where applicable
- [ ] Commit message follows format

## Completion

When ALL epics in ALL phases have been implemented and committed (visible in `git log`), output the completion tag: **V4 IMPLEMENTATION COMPLETE**.
