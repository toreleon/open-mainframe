# open-mainframe-racf

A high-performance Rust implementation of the **IBM RACF (Resource Access Control Facility)** security subsystem for the OpenMainframe project. This crate provides comprehensive user/group management, resource authorization, and digital certificate handling, following the z/OS SAF (System Authorization Facility) model.

## Overview

RACF is the "gold standard" for mainframe security. `open-mainframe-racf` reimplements the core security logic required to protect system resources. It handles everything from user authentication (passwords, PassTickets) to complex access checks against thousands of discrete and generic profiles.

## Architecture

```
    Application / Subsystem               RACF Security Subsystem
    ┌────────────────────┐                ┌────────────────────────┐
    │  Security Request  │    SAF Route   │    SAF Router          │
    │  (RACROUTE AUTH)   │ ─────────────> │    (Dispatcher)        │
    └────────────────────┘    Interface   └────────────────────────┘
                                                       │
    ┌────────────────────┐                ┌────────────────────────┐
    │  Administrative    │    Management  │    Profile Manager     │
    │  Commands          │ ─────────────> │    Discrete/Generic    │
    └────────────────────┘    Commands    │  Search & Match        │
                                          └────────────────────────┘
                                                       │
                                                       ▼
    ┌────────────────────┐                ┌────────────────────────┐
    │  Credential Store  │ <── Auth ─────     Authentication Mgr   │
    │  Passwords, Keys   │                │    PassTickets, JWT    │
    └────────────────────────┘            └────────────────────────┘
                                                       │
                                                       ▼
    ┌────────────────────┐                ┌────────────────────────┐
    │  Persistent DB     │ <── Storage ──     Security Database    │
    │  JSON / Backup     │                │    Profiles, ACLs      │
    └────────────────────┘                └────────────────────────┘
```

### Module Structure & Lines of Code

| Module | Lines | Description |
|--------|------:|-------------|
| `resource` | 2,364 | Authorization: Generic pattern matching and ACL check logic |
| `certificate`| 1,478 | Digital certificates: Keyrings, certificate management, and PKI |
| `database` | 1,408 | Persistence: Profile lifecycle, search, and JSON/Binary storage |
| `auth` | 1,015 | Authentication: Passwords, passphrases, and PassTicket algorithms |
| `setropts` | 896 | Global policy: System-wide security options and class activation |
| `saf` | 878 | SAF Dispatcher: Implementation of the `RACROUTE` interface |
| `seclabel` | 545 | Multi-level security: Bell-LaPadula dominance checks and MLS modes |
| `exits` | 504 | Hooks: Support for custom logic at pre/post authorization points |
| `utilities`| 460 | Tools: Implementation of IRRDBU00 (database unload) and IRREVX01 |
| `dataset` | 264 | Dataset specific: HLQ and generic protection logic |
| `profile` | 162 | Model: Data structures for User, Group, and Resource profiles |
| **Total** | **~10,318** | |

## Implementation Details

### Generic Profile Matching Algorithm
RACF uses a proprietary matching algorithm for generic profiles (e.g., `SYS1.*.DATA**`).
- **Specificity Scoring**: The engine calculates a specificity score for every matching profile. `*` (single qualifier) matches differently than `**` (multiple qualifiers).
- **Match Order**: The most specific profile always wins over a less specific one, regardless of creation order.
- **Backtracking**: The implementation uses a non-recursive backtracking matcher to handle deep `**` patterns without stack overflow.

### PassTicket Algorithm (Secured Sign-On)
Implements the IBM DES-based PassTicket algorithm.
- **Time-based**: PassTickets are valid only for a short window (typically 10 minutes).
- **Service-specific**: A PassTicket generated for `CICS` cannot be used for `TSO`.
- **Secret Key**: Uses a shared secret stored in the `PTKTDATA` class.

### Access Control Lists (ACLs)
Every profile contains an ACL mapping UserIDs/Groups to access levels.
- **Universal Access (UACC)**: Provides a default access level for users not on the ACL.
- **Group Connection**: Users gain access if any group they are connected to is on the ACL.

## Implementation vs Mainframe Gaps

| Feature | Real z/OS Mainframe | OpenMainframe implementation |
|---------|---------------------|------------------------------|
| **Storage Model**| VSAM B-tree (ICB/BAM blocks). | In-memory `HashMap` with JSON/Binary persistence. |
| **Encryption** | ICSF Hardware Security Module (HSM). | Rust-native crypto libraries (ring/aes). |
| **SAF Interface** | `RACROUTE` macro generates SVC 131. | Rust trait calls within the `SafRouter`. |
| **Exits** | Assembler modules in LPA/LNKLST. | Rust closures/traits registered at runtime. |
| **Auditing** | Type 80 SMF records. | Structured logging and optional `open-mainframe-smf` output. |
| **Large DBs** | 10M+ profiles via database splitting. | Optimized for 100k profiles; design for 1M+ underway. |

## Feature Coverage

### RACF Resource Classes

| Class | Status | Description |
|-------|--------|-------------|
| `USER` | Full | Authentication, attributes (SPECIAL), connections. |
| `GROUP` | Full | Hierarchical structure, ownership. |
| `DATASET` | Full | Multi-level generic protection. |
| `FACILITY` | Full | General system protection. |
| `SURROGAT` | Full | Job submission on behalf of other users. |
| `PTKTDATA` | Full | PassTicket keys and profiles. |
| `DIGTCERT` | Full | Digital certificates and trust chains. |
| `SECLABEL` | Full | Mandatory Access Control labels. |

## Usage Examples

### Performing an Authorization Check

```rust
use open_mainframe_racf::{RacfDatabase, AccessLevel};
use open_mainframe_racf::saf::SafRouter;

let db = RacfDatabase::load("security.db")?;
let router = SafRouter::new(db);

// Check if user 'JSMITH' can UPDATE 'SYS1.PARMLIB'
let result = router.check_auth("JSMITH", "DATASET", "SYS1.PARMLIB", AccessLevel::Update)?;

if result.is_allowed() {
    println!("Access granted");
}
```

### Generating a PassTicket

```rust
use open_mainframe_racf::auth::PassTicketGenerator;

let generator = PassTicketGenerator::new("CICSAPPL", "SECRET_KEY");
let ptkt = generator.generate("IBMUSER")?;
println!("Generated PassTicket: {}", ptkt);
```

## Testing

The RACF crate is verified for security correctness via 400+ tests:
- **Matrix Tests**: Validates generic matching against 500+ complex patterns.
- **ACL Tests**: Ensures group connection inheritance and UACC logic.
- **Crypto Tests**: Verified against IBM reference implementations for PassTicket results.
- **Persistence**: Tests data integrity during multi-threaded save/load cycles.

```sh
cargo test -p open-mainframe-racf
```
