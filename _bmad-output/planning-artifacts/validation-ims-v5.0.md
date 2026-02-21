---
version: 'v5.0'
planningGroup: 'PG-24'
technology: 'IMS'
date: '2026-02-21'
status: 'complete'
action: 'VALIDATE'
existingArtifact: 'epics-ims-v3.0.md'
gapAnalysis: 'batch-13-ims-tm-mfs.md'
result: 'Major gaps — addendum required for IMS TM and MFS'
---

# Validation Report: IMS (PG-24)

## Summary

**Result: v3.0 covers IMS DB comprehensively but has major IMS TM/MFS gaps — addendum required.**

The existing v3.0 epics (400-410, 11 epics, ~15 stories) cover:
- Epic 400: EXEC DLI Preprocessor (3 stories)
- Epic 401: System Service Calls (3 stories)
- Epic 402: I/O PCB and Message Handling Foundation (2 stories)
- Epic 403: Database Persistence (2 stories)
- Epic 404: GSAM Database Support (1 story)
- Epic 405: Comprehensive Status Codes (1 story)
- Epic 406: Secondary Index Support (1 story)
- Epic 407: Logical Relationships (1 story)
- Epic 408: Boolean SSA Qualifications (1 story)
- Epic 409: Segment Data Extraction (1 story)
- Epic 410: DBD Parser Enhancements (2 stories)

The gap analysis (batch-13) proposes 14 additional epics for IMS TM and MFS that are NOT covered in v3.0:
- Region types (MPP/BMP/IFP) and transaction scheduling
- Alt PCB operations (ISRT, CHNG, PURG)
- Conversational transactions (SPA)
- Advanced system services (XRST, SETS/SETU, ROLS, ROLL)
- Environment queries (INIT, INQY, AUTH)
- Operator command framework (100+ commands)
- OTMA protocol (XCF, TPIPE)
- IMS Connect (TCP/IP gateway)
- Fast Path (EMH + IFP)
- MSC & Shared Queues
- MFS source language parser
- MFS control block compiler
- MFS runtime integration
- EXEC DLI code generation (scanner exists but no CALL generation)

## Cross-Check Matrix

| Gap Epic | Gap Title | v3.0 Coverage | Status |
|----------|-----------|---------------|--------|
| IMS-TM100 | Alt PCB & Extended Message Ops | Epic 402 (partial) | **Gap — needs addendum** |
| IMS-TM101 | Conversational Transactions (SPA) | None | **Gap — needs addendum** |
| IMS-TM102 | Advanced System Services | Epic 401 (partial) | **Gap — needs addendum** |
| IMS-TM103 | Environment Query Calls | None | **Gap — needs addendum** |
| IMS-TM104 | Region Model & Scheduling | None | **Gap — needs addendum** |
| IMS-TM105 | Operator Command Framework | None | **Gap — needs addendum** |
| IMS-TM106 | OTMA Protocol | None | **Gap — needs addendum** |
| IMS-TM107 | IMS Connect Gateway | None | **Gap — needs addendum** |
| IMS-TM108 | Fast Path (EMH + IFP) | None | **Gap — needs addendum** |
| IMS-TM109 | MSC & Shared Queues | None | **Gap — needs addendum** |
| MFS100 | MFS Source Language Parser | None | **Gap — needs addendum** |
| MFS101 | MFS Control Block Compiler | None | **Gap — needs addendum** |
| MFS102 | MFS Runtime Integration | None | **Gap — needs addendum** |
| IMS-TM110 | EXEC DLI Code Generation | Epic 400 (partial) | **Gap — needs addendum** |

## Recommendation

Create `epics-ims-v5.0-addendum.md` with 14 new epics covering IMS TM and MFS. The v3.0 IMS DB planning remains valid and does not need changes.
