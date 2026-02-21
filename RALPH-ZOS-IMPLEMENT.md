# Ralph Loop Prompt: Implement All v5.0 Epics

## Mission

You are implementing **162 epics / 949 stories** from the BMAD v5.0 planning pipeline for the OpenMainframe z/OS clone. The planning is complete — PRDs, architectures, and epics with acceptance criteria exist for every piece of work. Your job is to write the Rust code, tests, and wire everything into the workspace.

---

## Workspace Context

**Existing 24-crate Rust workspace** at `crates/`:

| Crate | Technology | Base Epics (implemented) |
|-------|-----------|--------------------------|
| open-mainframe-assess | Migration assessment | v3.0 |
| open-mainframe-cics | CICS TP monitor | v3.0 |
| open-mainframe-cobol | COBOL compiler | v3.0 |
| open-mainframe-dataset | Dataset/VSAM/catalog | v3.0 |
| open-mainframe-db2 | DB2 SQL engine | v3.0 |
| open-mainframe-deploy | Deployment tools | v3.0 |
| open-mainframe-encoding | EBCDIC/codepages | v3.0 |
| open-mainframe-hlasm | HLASM assembler | v4.0 |
| open-mainframe-ims | IMS database | v3.0 |
| open-mainframe-ispf | ISPF panel engine | v4.0 |
| open-mainframe-jcl | JCL parser/engine | v3.0 |
| open-mainframe-jes2 | JES2 spool/job mgmt | v4.0 |
| open-mainframe-lang-core | Shared language core | v3.0 |
| open-mainframe-mq | IBM MQ messaging | v4.0 |
| open-mainframe-pli | PL/I compiler | v4.0 |
| open-mainframe-racf | RACF security | v4.0 |
| open-mainframe-rexx | REXX interpreter | v4.0 |
| open-mainframe-runtime | Language Environment | v4.0 |
| open-mainframe-smf | SMF records (basic) | v3.0 |
| open-mainframe-sort | DFSORT/ICETOOL | v3.0 |
| open-mainframe-tso | TSO command processor | v4.0 |
| open-mainframe-tui | Terminal UI (3270) | v3.0 |
| open-mainframe-wlm | WLM (basic) | v3.0 |
| open-mainframe-zosmf | z/OSMF REST API | v4.0 + conformance |

**Workspace settings:** MSRV 1.82, `#![forbid(unsafe_code)]`, `thiserror` + `miette` errors.

---

## Input Documents

All planning artifacts are in `_bmad-output/planning-artifacts/`. For each implementation group, read:

1. **Architecture doc** — Module layout, key types, traits, design decisions
2. **Epics doc** — Stories with acceptance criteria (Given/When/Then)
3. **PRD** — Requirements for context (only if architecture is unclear)

---

## Implementation Groups (24 groups, 162 epics, 949 stories)

### Phase A — Foundation Layer

#### IG-1: MVS System Services (PG-1) — NEW CRATE `open-mainframe-mvs`
- **Read:** `architecture-mvs-services-v5.0.md`, `epics-mvs-services-v5.0.md`
- **Epics:** SYS-100 (8 stories), SYS-101 (8 stories) = **16 stories**
- **Dependencies:** open-mainframe-dataset, open-mainframe-racf

#### IG-2: RACF Extensions (PG-2) — EXTEND `open-mainframe-racf`
- **Read:** `epics-racf-v5.0-addendum.md`
- **Epics:** SYS-109 (7 stories), SYS-110 (7 stories) = **14 stories**
- **Dependencies:** Existing RACF v4.0 code in crate

#### IG-3: JES2 Extensions (PG-4) — EXTEND `open-mainframe-jes2`
- **Read:** `epics-jes2-v5.0-addendum.md`
- **Epics:** SYS-108 (6 stories) = **6 stories**
- **Dependencies:** Existing JES2 v4.0 code in crate

#### IG-4: Utilities & DFSORT (PG-5) — NEW CRATE `open-mainframe-utilities`
- **Read:** `architecture-utilities-v5.0.md`, `epics-utilities-v5.0.md`
- **Epics:** UTIL-110 (4), UTIL-100 (6), UTIL-101 (4), UTIL-102 (4), UTIL-103 (4), UTIL-104 (7), UTIL-105 (4), UTIL-106 (4), UTIL-107 (6), UTIL-108 (6), UTIL-109 (6) = **55 stories**
- **Dependencies:** open-mainframe-dataset, open-mainframe-sort

#### IG-5: DFSMS, HSM & Catalog (PG-6) — EXTEND `open-mainframe-dataset`
- **Read:** `epics-dfsms-v5.0-addendum.md`
- **Epics:** DFSMS-100 (7), DFSMS-101 (7), DFSMS-102 (7), DFSMS-103 (7), DFSMS-104 (5), DFSMS-105 (5), DFSMS-106 (6), DFSMS-107 (5), DFSMS-108 (5), DFSMS-109 (5), DFSMS-110 (4) = **68 stories** (note: 5 extra over plan — verify actual)
- **Dependencies:** Existing dataset/VSAM code in crate

#### IG-6: ABEND Framework (PG-7) — EXTEND `open-mainframe-runtime`
- **Read:** `architecture-abend-v5.0.md`, `epics-abend-v5.0.md`
- **Epics:** SYS-111 (8 stories) = **8 stories**
- **Dependencies:** SYS-100 (MVS Services)

---

### Phase B — Interactive & Scripting

#### IG-7: System Commands & Console (PG-10) — NEW CRATE `open-mainframe-syscmd`
- **Read:** `architecture-syscmd-v5.0.md`, `epics-syscmd-v5.0.md`
- **Epics:** SYS-102 (9 stories), SYS-103 (9 stories) = **18 stories**
- **Dependencies:** open-mainframe-mvs, open-mainframe-dataset

#### IG-8: Program Management & Binder (PG-11) — NEW CRATE `open-mainframe-pgmmgmt`
- **Read:** `architecture-pgmmgmt-v5.0.md`, `epics-pgmmgmt-v5.0.md`
- **Epics:** SYS-104 (10 stories), SYS-105 (6 stories) = **16 stories**
- **Dependencies:** open-mainframe-mvs, open-mainframe-runtime

#### IG-9: WLM Extensions (PG-12) — EXTEND `open-mainframe-wlm`
- **Read:** `architecture-wlm-v5.0.md`, `epics-wlm-v5.0.md`
- **Epics:** WLM-100 (7), WLM-101 (6), WLM-102 (6), WLM-103 (5), WLM-104 (6), WLM-105 (5), WLM-106 (7), WLM-107 (5), WLM-108 (4), WLM-109 (5), WLM-110 (6) = **62 stories**
- **Dependencies:** open-mainframe-jes2, open-mainframe-mvs

---

### Phase C — Middleware & Communications

#### IG-10: USS & POSIX (PG-15) — NEW CRATE `open-mainframe-uss`
- **Read:** `architecture-uss-v5.0.md`, `epics-uss-v5.0.md`
- **Epics:** USS-100 (7), USS-101 (5), USS-102 (8), USS-103 (5), USS-104 (5), USS-105 (6), USS-106 (6), USS-107 (6), USS-108 (5), USS-109 (4), USS-110 (3) = **60 stories**
- **Dependencies:** open-mainframe-mvs

#### IG-11: Networking & TCP/IP (PG-16) — NEW CRATE `open-mainframe-networking`
- **Read:** `architecture-networking-v5.0.md`, `epics-networking-v5.0.md`
- **Epics:** NET-100 (5), NET-101 (4), NET-102 (6), NET-103 (5), NET-104 (6), NET-105 (5), NET-106 (6), NET-107 (5), NET-108 (5), NET-109 (5) = **52 stories**
- **Dependencies:** open-mainframe-mvs, open-mainframe-uss

#### IG-12: SMF Extensions (PG-17) — EXTEND `open-mainframe-smf`
- **Read:** `architecture-smf-v5.0.md`, `epics-smf-v5.0.md`
- **Epics:** SMF-100 (5), SMF-101 (4), SMF-102 (5), SMF-103 (5), SMF-104 (6), SMF-105 (5), SMF-106 (5), SMF-107 (5), SMF-108 (5), SMF-109 (5), SMF-110 (4) = **54 stories**
- **Dependencies:** open-mainframe-mvs, open-mainframe-wlm

#### IG-13: COBOL Precompilers (PG-18) — NEW CRATE `open-mainframe-precompilers`
- **Read:** `architecture-precompilers-v5.0.md`, `epics-precompilers-v5.0.md`
- **Epics:** SYS-112 (8 stories), SYS-113 (6 stories) = **14 stories**
- **Dependencies:** open-mainframe-cobol, open-mainframe-db2, open-mainframe-cics

#### IG-14: CICS BMS & Extensions (PG-19) — EXTEND `open-mainframe-cics`
- **Read:** `epics-cics-v5.0-addendum.md`
- **Epics:** SYS-116 (7), CICS-210 (5), CICS-211 (6) = **18 stories**
- **Dependencies:** Existing CICS v3.0 code in crate

---

### Phase D — Extended Languages

#### IG-15: CLIST (PG-21) — NEW CRATE `open-mainframe-clist`
- **Read:** `architecture-clist-v5.0.md`, `epics-clist-v5.0.md`
- **Epics:** CL-100 (5), CL-101 (6), CL-102 (4), CL-103 (6), CL-104 (6) = **27 stories**
- **Dependencies:** open-mainframe-tso, open-mainframe-ispf

#### IG-16: System Init & PARMLIB (PG-22) — NEW CRATE `open-mainframe-parmlib`
- **Read:** `architecture-syscfg-v5.0.md`, `epics-syscfg-v5.0.md`
- **Epics:** SYS-117 (8), SYS-118 (4), SYS-119 (7), SYS-120 (5) = **24 stories**
- **Dependencies:** open-mainframe-mvs, open-mainframe-racf, open-mainframe-jes2

#### IG-17: Crypto, PKI & Security (PG-23) — NEW CRATE `open-mainframe-crypto` + EXTEND `open-mainframe-racf`
- **Read:** `architecture-crypto-v5.0.md`, `epics-crypto-v5.0.md`
- **Epics:** CRYPTO-100 (6), CRYPTO-101 (5), CRYPTO-102 (6), CRYPTO-103 (4), SEC-107 (5), SEC-108 (6), SEC-109 (6) = **38 stories**
- **Dependencies:** open-mainframe-racf, open-mainframe-smf

---

### Phase E — Transaction Monitors & Databases

#### IG-18: IMS TM & MFS (PG-24) — EXTEND `open-mainframe-ims`
- **Read:** `epics-ims-v5.0-addendum.md`
- **Epics:** IMS-TM100 (5), IMS-TM101 (4), IMS-TM102 (5), IMS-TM103 (4), IMS-TM104 (6), IMS-TM105 (6), IMS-TM106 (5), IMS-TM107 (6), IMS-TM108 (4), IMS-TM109 (4), MFS-100 (5), MFS-101 (5), MFS-102 (5), IMS-TM110 (4) = **72 stories**
- **Dependencies:** Existing IMS v3.0 DB code in crate

#### IG-19: DB2 Utilities & BIND (PG-25) — EXTEND `open-mainframe-db2`
- **Read:** `epics-db2-v5.0-addendum.md`
- **Epics:** SYS-114 (7), SYS-115 (5) = **12 stories**
- **Dependencies:** Existing DB2 v3.0 code in crate

#### IG-20: Easytrieve (PG-26) — NEW CRATE `open-mainframe-easytrieve`
- **Read:** `architecture-easytrieve-v5.0.md`, `epics-easytrieve-v5.0.md`
- **Epics:** EZ-100 (5), EZ-101 (5), EZ-102 (5), EZ-103 (6), EZ-104 (4), EZ-105 (4), EZ-106 (4) = **33 stories**
- **Dependencies:** open-mainframe-sort, open-mainframe-dataset

#### IG-21: IDMS (PG-27) — NEW CRATE `open-mainframe-idms`
- **Read:** `architecture-idms-v5.0.md`, `epics-idms-v5.0.md`
- **Epics:** IDMS-100 (5), IDMS-101 (5), IDMS-102 (7), IDMS-103 (4), IDMS-104 (4), IDMS-105 (6), IDMS-106 (6), IDMS-107 (4), IDMS-108 (5), IDMS-109 (5), IDMS-110 (5) = **56 stories**
- **Dependencies:** open-mainframe-dataset, open-mainframe-cobol

#### IG-22: ADABAS (PG-28) — NEW CRATE `open-mainframe-adabas`
- **Read:** `architecture-adabas-v5.0.md`, `epics-adabas-v5.0.md`
- **Epics:** ADA-100 (6), ADA-101 (5), ADA-102 (5), ADA-103 (5), ADA-104 (5), ADA-105 (6), ADA-106 (4), ADA-107 (5), ADA-108 (6), ADA-109 (5) = **52 stories**
- **Dependencies:** open-mainframe-dataset

---

### Phase F — Niche & Legacy

#### IG-23: Natural (PG-29) — NEW CRATE `open-mainframe-natural`
- **Read:** `architecture-natural-v5.0.md`, `epics-natural-v5.0.md`
- **Epics:** NAT-100 (6), NAT-101 (5), NAT-102 (5), NAT-103 (5), NAT-104 (6), NAT-105 (4), NAT-106 (5), NAT-107 (5), NAT-108 (5), NAT-109 (4), NAT-110 (5) = **55 stories**
- **Dependencies:** open-mainframe-adabas, open-mainframe-db2

#### IG-24: FOCUS (PG-30) — NEW CRATE `open-mainframe-focus`
- **Read:** `architecture-focus-v5.0.md`, `epics-focus-v5.0.md`
- **Epics:** FOC-100 (6), FOC-101 (5), FOC-102 (6), FOC-103 (4), FOC-104 (5), FOC-105 (5), FOC-106 (5), FOC-107 (6), FOC-108 (5), FOC-109 (4), FOC-110 (4) = **55 stories**
- **Dependencies:** open-mainframe-adabas, open-mainframe-db2

---

## Iteration Protocol

**Before doing ANY work, check your progress:**

1. Read the progress tracker: `_bmad-output/zos-v5-implementation-progress.md`
   - If it doesn't exist, create it (see Progress Tracker Format below)
   - Find the first epic with status `[ ]` (pending)

2. For the current epic:
   a. Read the relevant architecture and epics documents (see Implementation Groups above)
   b. Read existing code in the target crate to understand current patterns
   c. If this is the FIRST epic in a NEW CRATE: scaffold the crate first (see New Crate Protocol)
   d. Implement ALL stories in the epic — write modules, types, traits, logic, tests
   e. Run `cargo check -p {crate-name}` — fix all errors
   f. Run `cargo test -p {crate-name}` — fix all failures
   g. Run `cargo clippy -p {crate-name}` — fix all warnings
   h. Mark the epic `[x]` in the progress tracker
   i. **Commit** with message: `feat({crate}): {EPIC-ID} — {epic title}`

3. **One epic per iteration.** Complete all stories in the epic before moving to the next.

4. If ALL epics are complete, output the completion promise.

---

## New Crate Protocol

When creating a brand new crate (14 new crates total):

### Step 1: Create directory structure
```bash
mkdir -p crates/open-mainframe-{name}/src
```

### Step 2: Create `Cargo.toml`
```toml
[package]
name = "open-mainframe-{name}"
version = "0.1.0"
edition = "2021"
rust-version = "1.82"
description = "OpenMainframe {Technology Name}"
license = "MIT OR Apache-2.0"

[dependencies]
thiserror = "2"
miette = { version = "7", features = ["fancy"] }
# Add crate deps as needed from architecture doc

[dev-dependencies]
# test deps as needed
```

### Step 3: Create `src/lib.rs`
```rust
#![forbid(unsafe_code)]
#![doc = "OpenMainframe {Technology Name}"]

// Module declarations per architecture doc
```

### Step 4: Create `src/error.rs`
```rust
use miette::Diagnostic;
use thiserror::Error;

#[derive(Debug, Error, Diagnostic)]
pub enum Error {
    // Variants per architecture doc
}

pub type Result<T> = std::result::Result<T, Error>;
```

### Step 5: Add to workspace
Edit root `Cargo.toml` to add the new crate to `[workspace] members`.

### Step 6: Verify
```bash
cargo check -p open-mainframe-{name}
```

---

## Coding Standards

### Patterns to Follow

1. **Read existing crate code first.** Before adding to any crate, read its `lib.rs` and key modules to match existing patterns (naming, error handling, trait design, test style).

2. **Module organization:** Follow the module layout in the architecture doc. Each module gets its own file. Use `mod.rs` only for directories with multiple sub-modules.

3. **Error types:** One `Error` enum per crate in `error.rs`. Use `thiserror::Error` + `miette::Diagnostic`. Propagate with `?`.

4. **Types:** Use the exact Rust type signatures from the architecture doc. If the architecture specifies `pub struct Foo { ... }`, implement it as specified.

5. **Traits:** Implement traits specified in the architecture doc. Use trait objects (`Box<dyn Trait>`) when the architecture calls for it. Prefer static dispatch (`impl Trait` or generics) otherwise.

6. **Tests:** Every module gets `#[cfg(test)] mod tests { ... }` with tests covering the acceptance criteria from the epics doc. Each story's Given/When/Then maps to at least one `#[test]` function.

7. **No panics in library code.** Return `Result` types. `unwrap()` only in tests.

8. **Documentation:** Add `///` doc comments to all public items. Keep them concise — one line for simple items, a brief paragraph for complex ones.

9. **Imports:** Use `use crate::` for intra-crate imports. Use the crate name for cross-crate imports.

10. **Feature flags:** Don't use them unless the architecture doc specifies them.

### What NOT to Do

- Don't add dependencies not mentioned in the architecture doc without justification
- Don't refactor existing code unless the story specifically requires it
- Don't add CLI interfaces, HTTP endpoints, or UI — this is library code
- Don't use `unsafe` — `#![forbid(unsafe_code)]` is enforced
- Don't stub out implementations with `todo!()` — implement them fully
- Don't create integration test files unless the epic specifically calls for integration tests

---

## Commit Protocol

After completing each epic:

```bash
git add crates/open-mainframe-{name}/
git commit -m "feat({name}): {EPIC-ID} — {epic title short description}"
```

For new crate scaffolding (before first epic implementation):
```bash
git add crates/open-mainframe-{name}/ Cargo.toml
git commit -m "feat({name}): scaffold crate for {Technology}"
```

Do NOT push. The user will push when ready.

---

## Progress Tracker Format

Create `_bmad-output/zos-v5-implementation-progress.md`:

```markdown
---
currentIG: IG-1
currentEpic: SYS-100
epicsComplete: 0
epicsTotal: 162
storiesComplete: 0
storiesTotal: 949
lastUpdated: ""
---

# z/OS v5.0 Implementation Progress

## Phase A — Foundation Layer

### IG-1: MVS System Services (NEW: open-mainframe-mvs)
- [ ] Scaffold crate
- [ ] SYS-100: MVS System Services Core (8 stories)
- [ ] SYS-101: ESTAE/ESPIE Recovery (8 stories)

### IG-2: RACF Extensions (EXTEND: open-mainframe-racf)
- [ ] SYS-109: General Resource Class Framework (7 stories)
- [ ] SYS-110: PassTicket & Digital Certificate Mgmt (7 stories)

### IG-3: JES2 Extensions (EXTEND: open-mainframe-jes2)
- [ ] SYS-108: JES2 Installation Exits Framework (6 stories)

### IG-4: Utilities & DFSORT (NEW: open-mainframe-utilities)
- [ ] Scaffold crate
- [ ] UTIL-110: Utility Framework (4 stories)
- [ ] UTIL-100: IEBCOPY (6 stories)
- [ ] UTIL-101: IEBGENER Enhancements (4 stories)
- [ ] UTIL-102: IEBCOMPR (4 stories)
- [ ] UTIL-103: IEBUPDTE (4 stories)
- [ ] UTIL-104: DFSORT Enhancements (7 stories)
- [ ] UTIL-105: IEBPTPCH (4 stories)
- [ ] UTIL-106: IEBDG (4 stories)
- [ ] UTIL-107: IEH System Utilities (6 stories)
- [ ] UTIL-108: AMASPZAP (6 stories)
- [ ] UTIL-109: TSO/REXX/USS Batch (6 stories)

### IG-5: DFSMS, HSM & Catalog (EXTEND: open-mainframe-dataset)
- [ ] DFSMS-100: SMS Construct Data Model (7 stories)
- [ ] DFSMS-101: ACS Routine Interpreter (7 stories)
- [ ] DFSMS-102: ICF Catalogs (7 stories)
- [ ] DFSMS-103: IDCAMS Extensions (7 stories)
- [ ] DFSMS-104: DFSMShsm Migration (5 stories)
- [ ] DFSMS-105: DFSMShsm Backup (5 stories)
- [ ] DFSMS-106: DFSMSdss DUMP/RESTORE (6 stories)
- [ ] DFSMS-107: DFSMSrmm Tape Mgmt (5 stories)
- [ ] DFSMS-108: PDSE & Member Generations (5 stories)
- [ ] DFSMS-109: Space & Volume Mgmt (5 stories)
- [ ] DFSMS-110: GDG-to-ICF Integration (4 stories)

### IG-6: ABEND Framework (EXTEND: open-mainframe-runtime)
- [ ] SYS-111: ABEND Code Framework & Dump Support (8 stories)

## Phase B — Interactive & Scripting

### IG-7: System Commands & Console (NEW: open-mainframe-syscmd)
- [ ] Scaffold crate
- [ ] SYS-102: System Command Dispatcher & DISPLAY (9 stories)
- [ ] SYS-103: SDSF (9 stories)

### IG-8: Program Management & Binder (NEW: open-mainframe-pgmmgmt)
- [ ] Scaffold crate
- [ ] SYS-104: Binder & Program Execution (10 stories)
- [ ] SYS-105: Object & Load Module Format (6 stories)

### IG-9: WLM Extensions (EXTEND: open-mainframe-wlm)
- [ ] WLM-100: Service Definition Data Model (7 stories)
- [ ] WLM-101: Classification Rule Engine (6 stories)
- [ ] WLM-102: Goal Evaluation & PI (6 stories)
- [ ] WLM-103: Resource Groups & Capping (5 stories)
- [ ] WLM-104: WLM-Managed Initiator Scheduling (6 stories)
- [ ] WLM-105: Enclave Framework (5 stories)
- [ ] WLM-106: IWM Services API (7 stories)
- [ ] WLM-107: Scheduling & App Environments (5 stories)
- [ ] WLM-108: Service Definition Persistence (4 stories)
- [ ] WLM-109: Operator Commands & Monitoring (5 stories)
- [ ] WLM-110: WLM Health API (6 stories)

## Phase C — Middleware & Communications

### IG-10: USS & POSIX (NEW: open-mainframe-uss)
- [ ] Scaffold crate
- [ ] USS-100: POSIX Process Model (7 stories)
- [ ] USS-101: Signal Handling (5 stories)
- [ ] USS-102: zFS Hierarchical File System (8 stories)
- [ ] USS-103: Directory & File Metadata (5 stories)
- [ ] USS-104: Pthreads (5 stories)
- [ ] USS-105: IPC Mechanisms (6 stories)
- [ ] USS-106: POSIX Sockets (6 stories)
- [ ] USS-107: UNIX Shell (/bin/sh) (6 stories)
- [ ] USS-108: Core UNIX Utilities (5 stories)
- [ ] USS-109: BPXPRMxx Config & Security (4 stories)
- [ ] USS-110: Daemon Infrastructure (3 stories)

### IG-11: Networking & TCP/IP (NEW: open-mainframe-networking)
- [ ] Scaffold crate
- [ ] NET-100: VTAM Application Interface (5 stories)
- [ ] NET-101: SNA LU Type Support (4 stories)
- [ ] NET-102: APPC / LU 6.2 / CPI-C (6 stories)
- [ ] NET-103: TCP/IP Stack Configuration (5 stories)
- [ ] NET-104: Sockets API Compatibility (6 stories)
- [ ] NET-105: AT-TLS & Security (5 stories)
- [ ] NET-106: FTP Client/Server (6 stories)
- [ ] NET-107: SSH Server & TN3270E (5 stories)
- [ ] NET-108: Sysplex Networking (5 stories)
- [ ] NET-109: IP Filtering & IPSec (5 stories)

### IG-12: SMF Extensions (EXTEND: open-mainframe-smf)
- [ ] SMF-100: SMF Record Infrastructure (5 stories)
- [ ] SMF-101: SMFPRMxx Configuration (4 stories)
- [ ] SMF-102: SMF Record Writing API (5 stories)
- [ ] SMF-103: SMF Exit Framework (5 stories)
- [ ] SMF-104: Type 30 — Job Accounting (6 stories)
- [ ] SMF-105: Types 14/15/17/18 — Dataset Activity (5 stories)
- [ ] SMF-106: Type 80 — Security Audit (5 stories)
- [ ] SMF-107: Types 70-79 — Performance (5 stories)
- [ ] SMF-108: Types 100-120 — Subsystem (5 stories)
- [ ] SMF-109: SMF Dump Utilities (5 stories)
- [ ] SMF-110: Observability Bridge (4 stories)

### IG-13: COBOL Precompilers (NEW: open-mainframe-precompilers)
- [ ] Scaffold crate
- [ ] SYS-112: DB2 COBOL Precompiler (8 stories)
- [ ] SYS-113: CICS COBOL Precompiler (6 stories)

### IG-14: CICS BMS & Extensions (EXTEND: open-mainframe-cics)
- [ ] SYS-116: CICS Web Services & REST/JSON (7 stories)
- [ ] CICS-210: DOCUMENT Commands (5 stories)
- [ ] CICS-211: CICS System Programming (6 stories)

## Phase D — Extended Languages

### IG-15: CLIST (NEW: open-mainframe-clist)
- [ ] Scaffold crate
- [ ] CL-100: Lexer & Parser (5 stories)
- [ ] CL-101: Interpreter Core (6 stories)
- [ ] CL-102: Built-in Functions (4 stories)
- [ ] CL-103: I/O and Error Handling (6 stories)
- [ ] CL-104: TSO/ISPF Integration (6 stories)

### IG-16: System Init & PARMLIB (NEW: open-mainframe-parmlib)
- [ ] Scaffold crate
- [ ] SYS-117: PARMLIB Framework & Core Members (8 stories)
- [ ] SYS-118: System Symbol Substitution (4 stories)
- [ ] SYS-119: Subsystem Configuration Members (7 stories)
- [ ] SYS-120: Initialization Sequence & Operator Commands (5 stories)

### IG-17: Crypto, PKI & Security (NEW: open-mainframe-crypto + EXTEND: open-mainframe-racf)
- [ ] Scaffold crate (open-mainframe-crypto)
- [ ] CRYPTO-100: ICSF Symmetric & Hashing (6 stories)
- [ ] CRYPTO-101: ICSF Asymmetric Keys (5 stories)
- [ ] CRYPTO-102: Key Management & Stores (6 stories)
- [ ] CRYPTO-103: CSFKEYS/CSFSERV Integration (4 stories)
- [ ] SEC-107: Security Labels (MAC) (5 stories)
- [ ] SEC-108: RACF Audit & SMF Integration (6 stories)
- [ ] SEC-109: RACF Exits, Utilities & Config (6 stories)

## Phase E — Transaction Monitors & Databases

### IG-18: IMS TM & MFS (EXTEND: open-mainframe-ims)
- [ ] IMS-TM100: Alt PCB & Extended Messages (5 stories)
- [ ] IMS-TM101: Conversational Transactions (4 stories)
- [ ] IMS-TM102: Advanced System Service Calls (5 stories)
- [ ] IMS-TM103: Environment Query Calls (4 stories)
- [ ] IMS-TM104: Region Model & Scheduling (6 stories)
- [ ] IMS-TM105: Operator Command Framework (6 stories)
- [ ] IMS-TM106: OTMA Protocol (5 stories)
- [ ] IMS-TM107: IMS Connect Gateway (6 stories)
- [ ] IMS-TM108: Fast Path (EMH + IFP) (4 stories)
- [ ] IMS-TM109: MSC & Shared Queues (4 stories)
- [ ] MFS-100: MFS Source Language Parser (5 stories)
- [ ] MFS-101: MFS Control Block Compiler (5 stories)
- [ ] MFS-102: MFS Runtime Integration (5 stories)
- [ ] IMS-TM110: EXEC DLI Code Generation (4 stories)

### IG-19: DB2 Utilities & BIND (EXTEND: open-mainframe-db2)
- [ ] SYS-114: DB2 BIND & Package Mgmt (7 stories)
- [ ] SYS-115: DB2 Operational Utilities (5 stories)

### IG-20: Easytrieve (NEW: open-mainframe-easytrieve)
- [ ] Scaffold crate
- [ ] EZ-100: Lexer & Parser (5 stories)
- [ ] EZ-101: Interpreter Core (5 stories)
- [ ] EZ-102: File Processing (5 stories)
- [ ] EZ-103: Report Generator (6 stories)
- [ ] EZ-104: SQL/Database Integration (4 stories)
- [ ] EZ-105: SORT & Utilities (4 stories)
- [ ] EZ-106: Macros, External Calls & Tests (4 stories)

### IG-21: IDMS (NEW: open-mainframe-idms)
- [ ] Scaffold crate
- [ ] IDMS-100: CODASYL Data Model Core (5 stories)
- [ ] IDMS-101: Schema & Subschema DDL Parser (5 stories)
- [ ] IDMS-102: Navigational DML Engine (7 stories)
- [ ] IDMS-103: Currency Indicator System (4 stories)
- [ ] IDMS-104: COBOL DML Precompiler (4 stories)
- [ ] IDMS-105: DMCL & Physical Storage (6 stories)
- [ ] IDMS-106: IDMS-DC Transaction Processing (6 stories)
- [ ] IDMS-107: ADS/Online 4GL (4 stories)
- [ ] IDMS-108: SQL Option (5 stories)
- [ ] IDMS-109: Recovery & Operations (5 stories)
- [ ] IDMS-110: Lock Management (5 stories)

### IG-22: ADABAS (NEW: open-mainframe-adabas)
- [ ] Scaffold crate
- [ ] ADA-100: Inverted-List Storage Engine (6 stories)
- [ ] ADA-101: FDT & Field System (5 stories)
- [ ] ADA-102: Descriptor Engine (5 stories)
- [ ] ADA-103: Direct Call Interface (ACB) (5 stories)
- [ ] ADA-104: Search Commands (5 stories)
- [ ] ADA-105: Read Commands (6 stories)
- [ ] ADA-106: Modification Commands (4 stories)
- [ ] ADA-107: Transaction Management (5 stories)
- [ ] ADA-108: Nucleus & Logging (6 stories)
- [ ] ADA-109: Utilities & DDM (5 stories)

## Phase F — Niche & Legacy

### IG-23: Natural (NEW: open-mainframe-natural)
- [ ] Scaffold crate
- [ ] NAT-100: Lexer & Parser (6 stories)
- [ ] NAT-101: Data Model & DEFINE DATA (5 stories)
- [ ] NAT-102: Interpreter Core (5 stories)
- [ ] NAT-103: Data Manipulation (5 stories)
- [ ] NAT-104: ADABAS Database Access (6 stories)
- [ ] NAT-105: SQL Database Access (4 stories)
- [ ] NAT-106: Output & Reporting (5 stories)
- [ ] NAT-107: Interactive I/O & Maps (5 stories)
- [ ] NAT-108: System Variables & Functions (5 stories)
- [ ] NAT-109: Error Handling & Work Files (4 stories)
- [ ] NAT-110: Environment & Security (5 stories)

### IG-24: FOCUS (NEW: open-mainframe-focus)
- [ ] Scaffold crate
- [ ] FOC-100: Multi-Dialect Parser (6 stories)
- [ ] FOC-101: Master File Descriptor (5 stories)
- [ ] FOC-102: TABLE Request Engine (6 stories)
- [ ] FOC-103: GRAPH Engine (4 stories)
- [ ] FOC-104: MODIFY/MAINTAIN Engine (5 stories)
- [ ] FOC-105: Dialogue Manager (5 stories)
- [ ] FOC-106: Built-in Functions (5 stories)
- [ ] FOC-107: Data Adapters (6 stories)
- [ ] FOC-108: Output Formatting (5 stories)
- [ ] FOC-109: Joins & Multi-Source (4 stories)
- [ ] FOC-110: Mainframe Integration (4 stories)

## Summary
- Implementation groups: 24
- New crates to scaffold: 14
- Existing crates to extend: 10
- Total epics: 162
- Total stories: 949
- Epics complete: 0
- Stories complete: 0
```

Update `currentIG`, `currentEpic`, counters, and check off items as you go.

---

## Autonomous Decision Context

Since this is a Ralph Loop (no interactive user), make these decisions autonomously:

- **Dependency crates:** If a crate depends on another that isn't implemented yet, implement the dependency types as minimal stubs or re-export from the architecture doc's key types. Don't block on full dependency implementation.
- **Cross-crate imports:** Add `path = "../open-mainframe-{dep}"` to `Cargo.toml` for workspace dependencies.
- **Test data:** Create test data inline in test functions. Don't create separate test fixture files unless the test data is large (>50 lines).
- **External crate choices:** Use `ring` for crypto, `rustls` for TLS, `tokio` for async (only if architecture specifies async), `serde` for serialization. Prefer standard library when possible.
- **When stories conflict with existing code:** The story wins. Refactor existing code to accommodate, but minimize blast radius.
- **When acceptance criteria are ambiguous:** Implement the most straightforward interpretation. Add a `// NOTE: interpreted AC as ...` comment.

---

## Quality Gates (per epic)

Before marking an epic complete:

1. `cargo check -p {crate}` — zero errors
2. `cargo test -p {crate}` — all tests pass
3. `cargo clippy -p {crate} -- -D warnings` — zero warnings
4. Every story's acceptance criteria has at least one test
5. All public items have doc comments
6. No `todo!()`, `unimplemented!()`, or `panic!()` in library code

---

## Rules

1. **One epic per iteration.** Implement all stories, run quality gates, commit, update tracker.
2. **Read before writing.** Always read existing crate code + architecture doc + epics doc before implementing.
3. **Follow the architecture.** Use the module layout, type names, and trait definitions from the architecture doc.
4. **Tests are mandatory.** No epic is complete without tests covering every story's acceptance criteria.
5. **Commit after every epic.** Don't batch commits across epics.
6. **Phase order matters.** Complete Phase A before B, B before C, etc. Within a phase, follow IG order.
7. **Don't break existing tests.** Run `cargo test` for any crate you modify to ensure nothing regresses.
8. **Keep stories focused.** Each story = one logical unit of work (~200-400 lines). Don't over-engineer.
9. **Real implementations.** No mocks, no stubs, no placeholder code. Every function does what its doc comment says.
10. **Workspace builds must pass.** Periodically run `cargo check --workspace` to catch cross-crate issues.

---

## Completion Promise

When ALL 162 epics are marked `[x]` in the progress tracker and all quality gates pass:

```
ZOS V5.0 IMPLEMENTATION COMPLETE — 162 epics, 949 stories, all tests passing
```

Signal completion with the promise tag containing the text above.
