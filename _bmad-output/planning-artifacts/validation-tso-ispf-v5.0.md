---
version: 'v5.0'
planningGroup: 'PG-8'
technology: 'TSO/ISPF'
date: '2026-02-21'
status: 'complete'
validatedDocument: 'epics-tso-ispf-v4.0.md'
gapBatch: 'batch-09-tso-ispf.md'
result: 'validated — no changes needed'
---

# Validation: TSO/ISPF (PG-8)

## Summary

The TSO/ISPF v4.0 epics (T100-T112) provide comprehensive coverage of all TSO/ISPF components identified in the gap analysis (Batch 9). No addendum is required.

## Cross-Check Matrix

| Gap Analysis Epic | v4.0 Epic | Stories | Coverage |
|-------------------|-----------|---------|----------|
| T100: TSO Command Processor Core | T100 | T100.1-T100.5 | Full — command parser, ALLOCATE/FREE, LISTDS/LISTALC, PROFILE, DELETE/RENAME/ALTLIB |
| T101: TSO Job Management | T101 | T101.1-T101.2 | Full — SUBMIT, STATUS/OUTPUT/CANCEL |
| T102: TSO Program Execution & Communication | T102 | T102.1-T102.5 | Full — EXEC/CALL, service routines, IKJPARS, TRANSMIT/RECEIVE, IKJEFT01 |
| T103: ISPF Panel Definition Language | T103 | T103.1-T103.3 | Full — panel parser, body layout, executable statements |
| T104: ISPF Display & Dialog Services | T104 | T104.1-T104.3 | Full — DISPLAY, SELECT, TBDISPL, messages, CONTROL, ADDPOP |
| T105: ISPF Variable Services | T105 | T105.1-T105.2 | Full — variable pool model, VGET/VPUT, system variables |
| T106: ISPF Table Services | T106 | T106.1-T106.2 | Full — lifecycle, row operations, search, sort |
| T107: ISPF File Tailoring | T107 | T107.1 | Full — skeleton processing with )SEL, )DOT, variable substitution |
| T108: ISPF Library Access Services | T108 | T108.1-T108.2 | Full — LMINIT/LMFREE, LMGET/LMPUT, LMMFIND, LIBDEF |
| T109: ISPF Editor | T109 | T109.1-T109.4 | Full — editor core, line commands, primary commands, profiles, UNDO |
| T110: ISREDIT Edit Macros | T110 | T110.1-T110.2 | Full — macro framework, FIND/CHANGE/INSERT/DELETE |
| T111: ISPF Utilities | T111 | T111.1-T111.3 | Full — Library/Dataset utilities, Dataset List, SuperC, Search-For |
| T112: TSO/REXX/CLIST Integration | T112 | T112.1-T112.2 | Full — ADDRESS TSO/ISPEXEC, OUTTRAP, SYSDSN, LISTDSI, SYSVAR |

## Inventory Cross-Check

| Inventory Component | v4.0 Coverage |
|---------------------|---------------|
| TSO Commands (~35+) | T100 (core commands), T101 (job mgmt), T102 (EXEC/CALL) |
| TSO Service Routines | T102.2 (PUTLINE/GETLINE), T102.3 (IKJPARS) |
| Batch TSO (IKJEFT01) | T102.5 |
| ISPF Panel Language | T103 (parser, layout, exec statements) |
| ISPF Dialog Services | T104 (DISPLAY, SELECT, messages, CONTROL) |
| ISPF Variable Pools | T105 (function/shared/profile/system) |
| ISPF Table Services | T106 (lifecycle, rows, search, sort, persist) |
| ISPF File Tailoring | T107 (skeletons, )SEL/)DOT, substitution) |
| ISPF Library Services | T108 (LM services, LIBDEF) |
| ISPF Editor | T109 (core, line/primary commands, profiles) |
| ISREDIT Macros | T110 (framework, edit services) |
| ISPF Utilities (3.x) | T111 (3.1-3.4, SuperC, Search-For) |
| TSO/REXX Integration | T112 (ADDRESS, OUTTRAP, SYSDSN, LISTDSI) |

## Result

**Coverage: 100%** — All 13 gap analysis epics are fully represented in the v4.0 planning. Total: 13 epics, 29 stories. No changes needed.
