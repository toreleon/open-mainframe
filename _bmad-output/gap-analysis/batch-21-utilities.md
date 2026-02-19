# Gap Analysis: z/OS Utilities

## Official Specification Summary

z/OS ships a suite of system utilities — programs invoked via `EXEC PGM=` in JCL — that perform common data management tasks: copying, comparing, sorting, printing, cataloging, moving, and patching datasets. These utilities have been part of the operating system since OS/360 and remain in daily production use on every z/OS system. Additionally, TSO/REXX batch processors (IKJEFT01, IRXJCL) allow command and script execution outside interactive sessions.

Utilities are classified as **Core** — every z/OS installation uses them extensively in production JCL:
- **IEB-prefix utilities** (data set utilities): IEBGENER (copy), IEBCOPY (PDS copy/compress), IEBCOMPR (compare), IEBUPDTE (PDS update), IEBPTPCH (print/punch), IEBDG (test data generator)
- **IEH-prefix utilities** (system utilities): IEHMOVE (move/copy volumes), IEHPROGM (scratch/rename/catalog), IEHLIST (list VTOC/PDS directory)
- **Sort utilities**: DFSORT (sort/merge/copy), ICEGENER (fast copy), ICETOOL (multi-function batch), ICEMAN (alias)
- **IDCAMS**: VSAM/dataset catalog management (covered in Batch 19, included here for utility execution context)
- **ADRDSSU/DFDSS**: DFSMSdss dump/restore (covered in Batch 19, included here for utility execution context)
- **Control utilities**: IEFBR14 (null program), AMASPZAP/SPZAP (superzap patch)
- **TSO/REXX batch**: IKJEFT01/IKJEFT1A/IKJEFT1B (TSO batch TMP), IRXJCL (REXX batch), BPXBATCH (USS batch)

Key documentation:
- **z/OS DFSMSdfp Utilities** (SC23-6864) — IEBGENER, IEBCOPY, IEBCOMPR, IEBUPDTE, IEBPTPCH, IEBDG, IEHMOVE, IEHPROGM, IEHLIST
- **z/OS DFSORT Application Programming Guide** (SC23-6878) — SORT FIELDS, INCLUDE/OMIT, INREC/OUTREC, OUTFIL, ICETOOL, ICEGENER
- **z/OS MVS Program Management: User's Guide** (SA23-1393) — AMASPZAP/SPZAP
- **z/OS TSO/E User's Guide** (SA32-0971) — IKJEFT01/1A/1B batch TMP
- **z/OS TSO/E REXX User's Guide** (SA32-0982) — IRXJCL

## Key Features & Capabilities

### 1. IEBGENER — Sequential Copy/Generate

| Feature | Description |
|---------|-------------|
| Simple copy | Copy SYSUT1 → SYSUT2 with SYSIN DD DUMMY (no control statements) |
| GENERATE statement | Declare max resources: MAXFLDS, MAXLITS, MAXNAME, MAXGPS |
| RECORD statement | Define record groups with IDENT (last record identifier) and FIELD (length, input-pos/literal, conversion, output-pos) |
| FIELD conversions | PZ (packed→zoned), ZP (zoned→packed), HE (hex→EBCDIC), EH (EBCDIC→hex) |
| EXITS statement | Specify user exit routines for I/O, header/trailer labels, record processing |
| LABELS statement | Control label handling (DATA=, TOTALING=, OUTPUT=) |
| MEMBER statement | Split sequential file into PDS members using IDENT boundaries |
| SYSUT1/SYSUT2 | Input/output DD names; SYSUT2 can be PDS member |
| SYSPRINT | Message/diagnostic output |
| Return codes | 0 (success), 4 (write to SYSPRINT), 8 (cannot complete — missing DD), 12 (cannot open), 16 (control statement error) |

### 2. IEBCOPY — PDS/PDSE Copy, Compress, Merge

| Feature | Description |
|---------|-------------|
| COPY statement | Copy members from INDD to OUTDD PDS/PDSE |
| SELECT statement | Select specific members: `SELECT MEMBER=((name1,,R),(name2,newname))` — rename and replace |
| EXCLUDE statement | Exclude specific members: `EXCLUDE MEMBER=(name1,name2)` |
| COPY vs SELECT/EXCLUDE | SELECT and EXCLUDE cannot appear in the same COPY group |
| UNLOAD | Create sequential backup (PDSU) of PDS/PDSE — portable via FTP/tape |
| LOAD | Restore PDS/PDSE from sequential PDSU backup |
| Compress in place | INDD and OUTDD reference same DD — recovers embedded free space |
| PDS ↔ PDSE conversion | Convert PDS load modules to PDSE program objects and vice versa |
| Replace option | `R` on SELECT replaces existing members in output; without `R`, skip if exists |
| Rename on copy | `SELECT MEMBER=((oldname,newname,R))` renames during copy |
| Multiple input DDNAMEs | Merge members from multiple input PDS into one output |
| COPYMOD statement | Copy load modules with overlay/downward-compatible modifications |
| ALTERMOD statement | Alter load module scatter-load status |
| Return codes | 0 (success), 4 (member not found/already exists), 8 (error), 12 (unrecoverable), 16 (error in control statement) |

### 3. IEBCOMPR — Dataset Compare

| Feature | Description |
|---------|-------------|
| Sequential compare | Compare SYSUT1 vs SYSUT2 record-by-record |
| PDS compare | Compare all members of two partitioned datasets |
| COMPARE statement | `COMPARE TYPORG=PS` (sequential) or `TYPORG=PO` (partitioned) |
| Equality criteria | Same number of records, corresponding records and keys identical |
| Error limit | Processing halts after 10 unequal comparisons |
| Output | Prints record/block numbers, DD names, and unequal records on SYSOUT |
| EXITS statement | Specify exit routines for error handling and label processing |
| Limitations | No column-range filtering, no whitespace tolerance, no byte-level diff location |
| Alternative | ISPF SuperC (ISRSUPC) preferred for source/report comparison |
| Return codes | 0 (equal), 8 (unequal), 12 (unrecoverable error), 16 (control statement error) |

### 4. IEBUPDTE — PDS/Sequential Update

| Feature | Description |
|---------|-------------|
| PARM=(NEW\|MOD) | NEW: input data in SYSIN; MOD: control statements in SYSIN, data in SYSUT1 |
| `./ ADD` | Add a new member to PDS (NAME=membername) |
| `./ CHANGE` | Modify existing member (NAME=membername, LIST=ALL, UPDATE=INPLACE) |
| `./ REPL` | Replace entire member with new data |
| `./ REPRO` | Copy member from SYSUT1 to SYSUT2 without modification |
| `./ NUMBER` | Renumber sequence columns: SEQ1=ALL, NEW1=nnn, INCR=n |
| `./ DELETE` | Delete records by sequence number range (SEQ1=nnn, SEQ2=mmm) |
| `./ ENDUP` | End-of-update marker for SYSIN data |
| Sequence columns | Columns 73-80 hold sequence numbers; data in columns 1-72 |
| UPDATE=INPLACE | Modify records in place (cannot add/delete records) |
| Return codes | 0 (success), 4 (member not found in input), 8 (error), 12 (unrecoverable), 16 (control statement error) |

### 5. IEFBR14 — Null Utility

| Feature | Description |
|---------|-------------|
| No-op execution | Returns RC=0 immediately; no SYSIN, no processing |
| Allocation pattern | `DISP=(NEW,CATLG)` — create dataset via DD allocation only |
| Deletion pattern | `DISP=(OLD,DELETE)` — delete dataset via DD disposition only |
| Common use | Create/delete/catalog datasets without running any program logic |
| Step-level processing | JCL step allocation/deallocation handled by Job Entry Subsystem, not by IEFBR14 |

### 6. DFSORT / ICEGENER / ICETOOL

| Feature | Description |
|---------|-------------|
| SORT FIELDS | `SORT FIELDS=(pos,len,fmt,order,...)` — sort by one or more keys; CH/ZD/PD/BI/FI/FL formats |
| MERGE FIELDS | Merge pre-sorted input files |
| INCLUDE condition | `INCLUDE COND=(pos,len,fmt,op,value)` — filter records to include; ops: EQ/NE/GT/GE/LT/LE/SS/NONE |
| OMIT condition | `OMIT COND=(...)` — inverse of INCLUDE |
| INREC | `INREC FIELDS=(pos,len,...)` or `INREC BUILD=(...)` — reformat records before sort |
| OUTREC | `OUTREC BUILD=(...)` — reformat records after sort |
| OUTFIL | Multiple output files with independent BUILD/INCLUDE/OMIT/HEADER/TRAILER; VTOF/FTOV format conversion |
| SUM FIELDS | `SUM FIELDS=(pos,len,fmt,...)` — accumulate numeric fields; `SUM FIELDS=NONE` removes duplicates |
| ICEGENER | Fast sequential copy — invoked via EXEC PGM=ICEGENER or as IEBGENER replacement |
| ICETOOL | Multi-function utility: COPY, COUNT, DISPLAY, MODE, OCCUR, RANGE, SELECT, SORT, STATS, UNIQUE, VERIFY operators |
| SYMNAMES | Symbolic field name definitions — map names to pos/len/fmt for readable control statements |
| IFTHEN | Conditional reformatting: `IFTHEN=(WHEN=(cond),BUILD=(...))` with WHEN=INIT/NONE/GROUP |
| FINDREP | Find and replace within records: `FINDREP=(IN=x'...',OUT=x'...')` |
| OVERLAY | Overlay fields: `OUTREC OVERLAY=(pos:value)` — modify specific positions without rebuilding |
| E15/E35 exits | User exit points: E15 (input processing), E35 (output processing), E18 (DFSORT termination) |
| SORTIN/SORTOUT | Input/output DD names; SORTWK01-nn for sort work datasets |
| DFSPARM | DD name for control statements when invoked from COBOL or other HLL |
| PARM | `PARM='SORT FIELDS=(...)` — control statements via EXEC PARM |
| Return codes | 0 (success), 4 (success with warnings), 16 (error), 20 (severe error) |

### 7. IEBPTPCH — Print/Punch

| Feature | Description |
|---------|-------------|
| PRINT statement | Print records to SYSUT2; must be first control statement |
| PUNCH statement | Punch records to SYSUT2 in card image format |
| RECORD statement | Define field mapping: `RECORD FIELD=(len,input-pos,conv,output-pos)` |
| TITLE statement | Define page title: `TITLE ITEM=('title text',column)` |
| MEMBER statement | Select specific PDS member for printing |
| MAXFLDS/MAXNAME | Resource limits in PRINT/PUNCH statement |
| TYPORG | `TYPORG=PS` (sequential) or `TYPORG=PO` (partitioned) |
| STRTAFT/STOPAFT | Start after N records / stop after N records |
| TOTCONV/PRECONV | Convert to hex before/after printing |
| Return codes | 0 (success), 4 (abend on SYSUT2), 8 (error), 12 (unrecoverable), 16 (control statement error) |

### 8. IEBDG — Test Data Generator

| Feature | Description |
|---------|-------------|
| DSD statement | Define output dataset and optional input: `DSD OUTPUT=(ddname),INPUT=(ddname)` |
| FD statement | Field definition: `FD NAME=fldname,LENGTH=n,FORMAT=AN/ZD/PD/BI,ACTION=action` |
| CREATE statement | Generate records: `CREATE QUANTITY=n,NAME=(fld1,fld2,...)` |
| REPEAT statement | Repeat previous CREATE with variations: `REPEAT QUANTITY=n,CREATE=n` |
| END statement | End of control statements |
| Actions | FX (fixed), RL (roll left), RO (roll right), SL (shift left), SR (shift right), TL (truncate left), TR (truncate right), WV (wave) |
| Pattern fill | STARTLOC, FILL, SIGN, INDEX for numeric patterns |
| Copy/modify | Read existing records via INPUT DD and modify specific fields |
| Return codes | 0 (success), 4 (abend on output), 8 (error), 12 (unrecoverable), 16 (control statement error) |

### 9. IEHMOVE — Dataset/Volume Move

| Feature | Description |
|---------|-------------|
| MOVE DSNAME | Move single dataset between volumes (source scratched after copy) |
| COPY DSNAME | Copy single dataset between volumes (source retained) |
| MOVE DSGROUP | Move all datasets matching a generic filter |
| COPY DSGROUP | Copy all datasets matching a generic filter |
| MOVE PDS | Move PDS with SELECT member list |
| MOVE VOLUME | Move all datasets on a volume |
| UNLOAD/LOAD | Unload to tape / load from tape |
| CATLG/UNCATLG | Catalog or uncatalog moved/copied datasets |
| RENAME | Rename during move/copy: `RENAME=newdsn` |
| Limitations | Cannot use with PDSEs, VSAM, or ISAM; IBM discourages use in SMS-managed environments |
| DD requirements | SYSUT1 (work), SYSPRINT (messages), DD for each volume referenced |
| Return codes | 0 (success), 8 (error on one operation), 12 (unrecoverable), 16 (control statement error) |

### 10. IEHPROGM — System Maintenance Utility

| Feature | Description |
|---------|-------------|
| SCRATCH | Delete (scratch) a dataset: `SCRATCH DSNAME=dsn,VOL=volser` |
| SCRATCH MEMBER | Delete PDS member: `SCRATCH DSNAME=dsn,VOL=volser,MEMBER=memname` |
| RENAME | Rename dataset: `RENAME DSNAME=dsn,VOL=volser,NEWNAME=newdsn` |
| RENAME MEMBER | Rename PDS member: `RENAME DSNAME=dsn,VOL=volser,MEMBER=old,NEWNAME=new` |
| CATLG | Catalog a dataset: `CATLG DSNAME=dsn,VOL=volser` |
| UNCATLG | Uncatalog a dataset: `UNCATLG DSNAME=dsn` |
| PURGE | Override expiration date on SCRATCH: `PURGE` operand |
| DD requirements | SYSPRINT, DD for each volume |
| SMS limitations | Cannot SCRATCH/RENAME SMS-managed datasets (use IDCAMS DELETE/ALTER instead) |
| Return codes | 0 (success), 8 (error), 12 (unrecoverable), 16 (control statement error) |

### 11. IEHLIST — Catalog/VTOC/PDS Directory Listing

| Feature | Description |
|---------|-------------|
| LISTCTLG | List catalog entries (OS CVOLs only, not ICF catalogs) |
| LISTVTOC | List VTOC (Volume Table of Contents): `LISTVTOC VOL=volser,FORMAT` |
| LISTVTOC DUMP | Dump raw VTOC DSCB contents in hex |
| LISTPDS | List PDS directory entries: `LISTPDS DSNAME=dsn,FORMAT` |
| FORMAT | Formatted output (without FORMAT, raw hex dump) |
| DSNAME filter | LISTVTOC with DSNAME= limits to specific datasets |
| DD requirements | SYSPRINT, DD for each volume |
| ICF note | LISTCTLG only works for OS CVOLs; use IDCAMS LISTCAT for ICF catalogs |
| Return codes | 0 (success), 4 (partial), 8 (error), 12 (unrecoverable), 16 (control statement error) |

### 12. AMASPZAP (SPZAP/IMASPZAP) — Superzap Patch Utility

| Feature | Description |
|---------|-------------|
| NAME statement | `NAME membername csectname` — identify load module and CSECT to patch |
| VER (verify) | `VER offset hexdata` — verify contents at CSECT offset match expected data |
| REP (replace) | `REP offset hexdata` — replace contents at CSECT offset with new data |
| BASE statement | `BASE offset` — adjust base offset for subsequent VER/REP |
| SETSSI | `SETSSI hexvalue` — set system status indicator in load module |
| DUMP | `DUMP csectname` — dump CSECT contents in hex |
| DUMPT | `DUMPT csectname` — dump with EBCDIC translation and instruction mnemonics |
| IDRDATA | `IDRDATA userdata` — insert up to 8 bytes of user ID data in CSECT ID record |
| CCHHR mode | Patch by absolute disk address (rarely used — CSECT mode preferred) |
| VER before REP | Best practice: all VER statements before REP statements in a group; failed VER skips REP until next NAME |
| SYSLIB | DD for load module library containing the target member |
| Security concern | No inherent audit trail — auditors typically restrict AMASPZAP access |
| Return codes | 0 (all VER/REP successful), 4 (VER failed — REP skipped), 8 (error), 16 (severe error) |

### 13. IKJEFT01/IKJEFT1A/IKJEFT1B — TSO Batch TMP

| Feature | Description |
|---------|-------------|
| IKJEFT01 | Terminal Monitor Program — executes TSO commands in batch; ignores non-zero return codes (always processes next command) |
| IKJEFT1A | Terminates on first non-zero return code; returns RC in register 15; S04C on system abend |
| IKJEFT1B | Like 1A but also terminates on user abend with S04C; returns RC in register 15 |
| SYSTSIN | Input DD — contains TSO commands or REXX exec names to execute |
| SYSTSPRT | Output DD — receives SAY output, command responses, trace info |
| SYSEXEC | DD for REXX exec libraries (PDS containing REXX programs) |
| SYSPROC | Alternative DD for REXX/CLIST libraries |
| Limitations | Only TSO commands in SYSTSIN — no REXX keywords (IF/DO/SELECT); must invoke exec by name for REXX logic |
| ALLOCATE | TSO ALLOCATE command available for dynamic file allocation within batch |
| Common use | Run DB2 commands (DSN/RUN), RACF commands, LISTDS, LISTCAT in batch |

### 14. IRXJCL — REXX Batch Processor

| Feature | Description |
|---------|-------------|
| Invocation | `EXEC PGM=IRXJCL,PARM='execname'` — execute REXX exec directly |
| SYSEXEC | DD for REXX exec library (PDS or z/OS 2.5+ sequential) |
| SYSTSIN | Standard input for PULL/PARSE PULL |
| SYSTSPRT | Standard output for SAY |
| Efficiency | More efficient than IKJEFT01 for pure REXX execution (no TSO TMP overhead) |
| Return code | REXX EXIT value passed as step return code |
| Search order | SYSEXEC → SYSPROC for exec lookup |

### 15. BPXBATCH — z/OS UNIX Batch Processor

| Feature | Description |
|---------|-------------|
| Invocation | `EXEC PGM=BPXBATCH,PARM='SH command'` or `PARM='PGM /path/program'` |
| SH mode | Execute shell command pipeline |
| PGM mode | Execute z/OS UNIX program directly |
| STDIN/STDOUT/STDERR | DD names for USS I/O redirection |
| PATH DD | Specify HFS/zFS path for program |
| Return code | Exit status from shell/program passed as step RC |

## Codebase Assessment

### Current Implementation Status

The codebase has a **utility execution framework** in the JCL executor with a registry pattern:

**Utility Registry** (`crates/open-mainframe-jcl/src/executor/utility.rs:26-69`):
- `UtilityProgram` trait (lines 13-24) — extensible interface for built-in utilities
- `UtilityRegistry` HashMap<String, Box<dyn UtilityProgram>> — lookup by program name
- Execution flow: `execute_program()` → check registry → fallback to external binary

**Implemented utilities:**

| Utility | Location | Lines | Status |
|---------|----------|-------|--------|
| IEFBR14 | `executor/utility.rs:75-97` | 23 | **Complete** — returns RC=0, tested |
| IEBGENER | `executor/utility.rs:99-148` | 50 | **Functional** — copies SYSUT1→SYSUT2 via fs::copy; no control statement parsing (GENERATE/RECORD/FIELD) |
| IDCAMS (JCL) | `executor/utility.rs:162-253` | 92 | **Stub** — DEFINE CLUSTER creates marker files, DELETE removes files, REPRO copies files |
| IDCAMS (full) | `dataset/src/idcams/mod.rs` | 912 | **Substantial** — DEFINE CLUSTER/GDG, DELETE, ALTER, LISTCAT, PRINT, REPRO, VERIFY (see Batch 19) |
| SORT/DFSORT/ICEMAN | `executor/mod.rs:519-651` + `open-mainframe-sort` crate | 73 + 13K+ | **Substantial** — SORT FIELDS, INCLUDE/OMIT, INREC/OUTREC, SUM, MERGE, COPY mode |

**Execution flow** (`crates/open-mainframe-jcl/src/executor/mod.rs:517-534`):
```
JCL Parser → JobExecutor::execute_step()
  → SORT/DFSORT/ICEMAN? → execute_sort() [specialized handler]
  → In utility registry? → lookup(pgm_name).execute()
  → Otherwise → find_program() + Command::new() [external binary]
```

**Not implemented (no code):**
IEBCOPY, IEBCOMPR, IEBUPDTE, ICEGENER, IEBPTPCH, IEBDG, IEHMOVE, IEHPROGM, IEHLIST, AMASPZAP, IKJEFT01/1A/1B, IRXJCL, BPXBATCH (parsed as PGM= but falls through to external binary lookup)

**Adjacent capabilities:**
- PDS member CRUD in `dataset/src/pds.rs` (500+ lines) — could back IEBCOPY
- Catalog operations in `dataset/src/catalog.rs` (435 lines) — could back IEHPROGM CATLG/UNCATLG
- VSAM operations in `dataset/src/vsam/*.rs` (10 files) — back IDCAMS
- GDG operations in `dataset/src/gdg/` (400+ lines) — back IDCAMS
- QSAM/BSAM I/O in `dataset/src/qsam.rs`, `bsam.rs` — could back IEBGENER enhancements

## Gap Matrix

| # | Feature Area | Present | Partial | Missing | Notes |
|---|-------------|---------|---------|---------|-------|
| 1 | IEBGENER — simple copy (SYSIN DUMMY) | **YES** | | | executor/utility.rs:99-148 — fs::copy |
| 2 | IEBGENER — GENERATE/RECORD/FIELD reformatting | | | **GAP** | No control statement parser |
| 3 | IEBGENER — EXITS/LABELS/MEMBER | | | **GAP** | No exit support, no PDS member split |
| 4 | IEBGENER — FIELD conversions (PZ/ZP/HE/EH) | | | **GAP** | No data conversion |
| 5 | IEBCOPY — COPY PDS members | | | **GAP** | PDS member CRUD exists in dataset crate |
| 6 | IEBCOPY — SELECT/EXCLUDE member lists | | | **GAP** | |
| 7 | IEBCOPY — UNLOAD/LOAD (PDSU sequential format) | | | **GAP** | |
| 8 | IEBCOPY — Compress in place | | | **GAP** | |
| 9 | IEBCOPY — PDS ↔ PDSE conversion | | | **GAP** | |
| 10 | IEBCOPY — Rename on copy | | | **GAP** | |
| 11 | IEBCOPY — COPYMOD/ALTERMOD (load modules) | | | **GAP** | |
| 12 | IEBCOMPR — Sequential compare | | | **GAP** | |
| 13 | IEBCOMPR — PDS compare | | | **GAP** | |
| 14 | IEBUPDTE — ./ ADD / ./ REPL members | | | **GAP** | |
| 15 | IEBUPDTE — ./ CHANGE with UPDATE=INPLACE | | | **GAP** | |
| 16 | IEBUPDTE — ./ NUMBER / ./ DELETE sequence ops | | | **GAP** | |
| 17 | IEBUPDTE — ./ REPRO / ./ ENDUP | | | **GAP** | |
| 18 | IEFBR14 — Null utility | **YES** | | | executor/utility.rs:75-97 — RC=0 |
| 19 | DFSORT — SORT FIELDS | **YES** | | | open-mainframe-sort crate (13K+) |
| 20 | DFSORT — MERGE FIELDS | **YES** | | | Sort crate |
| 21 | DFSORT — INCLUDE/OMIT filtering | **YES** | | | Sort crate |
| 22 | DFSORT — INREC/OUTREC BUILD/OVERLAY | | **PARTIAL** | | Basic reformatting; no IFTHEN/FINDREP |
| 23 | DFSORT — OUTFIL multiple outputs | | | **GAP** | |
| 24 | DFSORT — SUM FIELDS aggregation | **YES** | | | Sort crate |
| 25 | DFSORT — SYMNAMES | | | **GAP** | |
| 26 | DFSORT — IFTHEN conditional reformatting | | | **GAP** | |
| 27 | DFSORT — FINDREP find/replace | | | **GAP** | |
| 28 | DFSORT — E15/E35/E18 user exits | | | **GAP** | |
| 29 | ICEGENER — Fast sequential copy | | | **GAP** | Not registered as utility; IEBGENER could alias |
| 30 | ICETOOL — COPY/COUNT/DISPLAY operators | | | **GAP** | |
| 31 | ICETOOL — MODE/OCCUR/RANGE/SELECT | | | **GAP** | |
| 32 | ICETOOL — SORT/STATS/UNIQUE/VERIFY | | | **GAP** | |
| 33 | IEBPTPCH — PRINT statement | | | **GAP** | |
| 34 | IEBPTPCH — PUNCH statement | | | **GAP** | |
| 35 | IEBPTPCH — RECORD FIELD formatting | | | **GAP** | |
| 36 | IEBPTPCH — TITLE/MEMBER selection | | | **GAP** | |
| 37 | IEBDG — DSD/FD/CREATE/REPEAT/END | | | **GAP** | |
| 38 | IEBDG — Actions (FX/RL/RO/SL/SR/TL/TR/WV) | | | **GAP** | |
| 39 | IEBDG — Pattern fill and copy/modify | | | **GAP** | |
| 40 | IEHMOVE — MOVE/COPY DSNAME | | | **GAP** | |
| 41 | IEHMOVE — MOVE/COPY DSGROUP | | | **GAP** | |
| 42 | IEHMOVE — MOVE VOLUME | | | **GAP** | |
| 43 | IEHMOVE — UNLOAD/LOAD tape | | | **GAP** | |
| 44 | IEHPROGM — SCRATCH dataset/member | | | **GAP** | Adjacent: catalog.rs delete |
| 45 | IEHPROGM — RENAME dataset/member | | | **GAP** | |
| 46 | IEHPROGM — CATLG/UNCATLG | | | **GAP** | Adjacent: catalog.rs add/remove |
| 47 | IEHLIST — LISTVTOC | | | **GAP** | |
| 48 | IEHLIST — LISTPDS | | | **GAP** | Adjacent: pds.rs directory listing |
| 49 | IEHLIST — LISTCTLG | | | **GAP** | Adjacent: IDCAMS LISTCAT |
| 50 | AMASPZAP — NAME/VER/REP patching | | | **GAP** | |
| 51 | AMASPZAP — BASE/SETSSI/DUMP/DUMPT | | | **GAP** | |
| 52 | AMASPZAP — IDRDATA | | | **GAP** | |
| 53 | IDCAMS (JCL level) — DEFINE CLUSTER | | **PARTIAL** | | Marker files only (utility.rs:191-210) |
| 54 | IDCAMS (JCL level) — DELETE | | **PARTIAL** | | Simple file removal (utility.rs:211-217) |
| 55 | IDCAMS (JCL level) — REPRO | | **PARTIAL** | | File copy (utility.rs:218-233) |
| 56 | IDCAMS (full) — see Batch 19 | **YES** | | | 912-line implementation in dataset crate |
| 57 | IKJEFT01 — TSO batch command execution | | | **GAP** | |
| 58 | IKJEFT1A — TSO batch with RC propagation | | | **GAP** | |
| 59 | IKJEFT1B — TSO batch with abend handling | | | **GAP** | |
| 60 | IKJEFT01/1A/1B — SYSTSIN/SYSTSPRT DD names | | | **GAP** | |
| 61 | IKJEFT01/1A/1B — SYSEXEC/SYSPROC exec search | | | **GAP** | |
| 62 | IRXJCL — REXX batch execution | | | **GAP** | Documented in Batch 1 gap |
| 63 | IRXJCL — SYSEXEC/SYSPROC search order | | | **GAP** | |
| 64 | BPXBATCH — SH mode (shell command) | | | **GAP** | Parsed as PGM= but no built-in handler |
| 65 | BPXBATCH — PGM mode (USS program) | | | **GAP** | |
| 66 | BPXBATCH — STDIN/STDOUT/STDERR DD mapping | | | **GAP** | |
| 67 | Utility registry — extensible framework | **YES** | | | executor/utility.rs:26-69 — UtilityProgram trait |
| 68 | SORT JCL integration — SORTIN/SORTOUT/SYSIN | **YES** | | | executor/mod.rs:519-651 |

**Summary: 10 present, 5 partial, 53 missing out of 68 features.**

## Proposed Epics

### UTIL-100: IEBCOPY — PDS/PDSE Copy, Compress, and Merge
**Complexity:** L
**Priority:** High — IEBCOPY is one of the most frequently used utilities in production JCL
**Stories:**
- UTIL-100.1: COPY statement — copy all members from INDD PDS to OUTDD PDS using existing pds.rs member CRUD
- UTIL-100.2: SELECT statement — select specific members by name list, with rename and replace options
- UTIL-100.3: EXCLUDE statement — exclude specific members by name list
- UTIL-100.4: Compress in place — INDD=OUTDD recovers embedded free space
- UTIL-100.5: UNLOAD to sequential PDSU — serialize PDS/PDSE to portable sequential format
- UTIL-100.6: LOAD from sequential PDSU — restore PDS/PDSE from unloaded format
- UTIL-100.7: PDS ↔ PDSE conversion — convert between PDS load modules and PDSE program objects
- UTIL-100.8: Register IEBCOPY in UtilityRegistry
**Dependencies:** open-mainframe-dataset (pds.rs)
**Reference:** https://www.ibm.com/docs/en/zos/2.4.0?topic=utilities-iebcopy-library-copymergecreate-program

### UTIL-101: IEBGENER Enhancements — Control Statement Support
**Complexity:** M
**Priority:** Medium — simple copy works; GENERATE/RECORD needed for real-world JCL
**Stories:**
- UTIL-101.1: Parse GENERATE statement (MAXFLDS, MAXLITS, MAXNAME, MAXGPS)
- UTIL-101.2: Parse RECORD statement with IDENT and FIELD operands
- UTIL-101.3: Implement FIELD data conversion (PZ, ZP, HE, EH)
- UTIL-101.4: MEMBER statement — split sequential file into PDS members by IDENT boundaries
- UTIL-101.5: EXITS statement — exit routine invocation points
- UTIL-101.6: LABELS statement — label copy control
**Dependencies:** open-mainframe-dataset (pds.rs, qsam.rs)
**Reference:** https://www.ibm.com/docs/en/zos/2.4.0?topic=utilities-iebgener-sequential-copygenerate-data-set-program

### UTIL-102: IEBCOMPR — Dataset Comparison
**Complexity:** S
**Priority:** Low — ISPF SuperC (ISRSUPC) is the preferred comparison tool in practice
**Stories:**
- UTIL-102.1: Sequential compare (TYPORG=PS) — record-by-record SYSUT1 vs SYSUT2
- UTIL-102.2: PDS compare (TYPORG=PO) — compare all members
- UTIL-102.3: Error reporting — unequal record/block numbers, 10-mismatch limit
- UTIL-102.4: Register IEBCOMPR in UtilityRegistry
**Dependencies:** open-mainframe-dataset (qsam.rs, pds.rs)
**Reference:** https://www.ibm.com/docs/en/zos/2.4.0?topic=utilities-iebcompr-compare-data-sets-program

### UTIL-103: IEBUPDTE — PDS Update Utility
**Complexity:** M
**Priority:** Medium — used for distributing source libraries and maintaining macro/proc libraries
**Stories:**
- UTIL-103.1: Parse ./ control cards (ADD, CHANGE, REPL, REPRO, NUMBER, DELETE, ENDUP)
- UTIL-103.2: ADD — create new PDS member from inline SYSIN data
- UTIL-103.3: CHANGE with UPDATE=INPLACE — modify records by sequence number
- UTIL-103.4: REPL — replace entire member
- UTIL-103.5: NUMBER — renumber sequence columns (73-80)
- UTIL-103.6: DELETE — remove records by sequence number range
- UTIL-103.7: PARM=(NEW|MOD) — SYSIN-only vs SYSUT1+SYSIN modes
- UTIL-103.8: Register IEBUPDTE in UtilityRegistry
**Dependencies:** open-mainframe-dataset (pds.rs)
**Reference:** https://www.ibm.com/docs/en/zos/2.4.0?topic=utilities-iebupdte-create-modify-data-set-program

### UTIL-104: DFSORT Enhancements — OUTFIL, IFTHEN, FINDREP, ICETOOL
**Complexity:** L
**Priority:** High — DFSORT advanced features are widely used in production batch
**Stories:**
- UTIL-104.1: OUTFIL statement — multiple output files with independent BUILD/INCLUDE/OMIT
- UTIL-104.2: OUTFIL HEADER/TRAILER — report headers and trailers
- UTIL-104.3: OUTFIL VTOF/FTOV — variable↔fixed format conversion
- UTIL-104.4: IFTHEN conditional reformatting — WHEN=(cond)/WHEN=INIT/WHEN=NONE/WHEN=GROUP
- UTIL-104.5: FINDREP — find and replace within records
- UTIL-104.6: OVERLAY — position-specific field overlay
- UTIL-104.7: SYMNAMES — symbolic field name definitions
- UTIL-104.8: ICEGENER — register as utility alias; fast copy mode
- UTIL-104.9: ICETOOL — multi-operator batch front-end (COPY, COUNT, DISPLAY, MODE, OCCUR, RANGE, SELECT, SORT, STATS, UNIQUE, VERIFY)
- UTIL-104.10: E15/E35/E18 user exit points
**Dependencies:** open-mainframe-sort crate
**Reference:** https://www.ibm.com/docs/en/SSLTBW_3.1.0/pdf/icea100_v3r1.pdf

### UTIL-105: IEBPTPCH — Print/Punch Utility
**Complexity:** S
**Priority:** Low — primarily used for dataset inspection; modern alternatives exist
**Stories:**
- UTIL-105.1: PRINT statement — formatted record output to SYSUT2
- UTIL-105.2: PUNCH statement — card image output
- UTIL-105.3: RECORD FIELD formatting and conversions
- UTIL-105.4: TITLE statement — page headers
- UTIL-105.5: MEMBER selection for PDS
- UTIL-105.6: STRTAFT/STOPAFT record range
- UTIL-105.7: Register IEBPTPCH in UtilityRegistry
**Dependencies:** open-mainframe-dataset (qsam.rs, pds.rs)
**Reference:** https://www.ibm.com/docs/en/zos/2.4.0?topic=utilities-iebptpch-print-punch-program

### UTIL-106: IEBDG — Test Data Generator
**Complexity:** S
**Priority:** Low — useful for testing but rarely in production workflows
**Stories:**
- UTIL-106.1: Parse DSD/FD/CREATE/REPEAT/END control statements
- UTIL-106.2: Field definitions — FORMAT (AN/ZD/PD/BI), LENGTH, STARTLOC, FILL
- UTIL-106.3: Actions — FX (fixed), RL/RO (roll), SL/SR (shift), TL/TR (truncate), WV (wave)
- UTIL-106.4: Pattern generation and record output
- UTIL-106.5: Copy/modify mode — read existing records and modify fields
- UTIL-106.6: Register IEBDG in UtilityRegistry
**Dependencies:** open-mainframe-dataset (qsam.rs)
**Reference:** https://www.ibm.com/docs/en/zos/2.1.0?topic=utilities-iebdg-test-data-generator-program

### UTIL-107: IEH-Prefix System Utilities
**Complexity:** M
**Priority:** Low — IEHMOVE deprecated in SMS environments; IEHPROGM superseded by IDCAMS
**Stories:**
- UTIL-107.1: IEHPROGM — SCRATCH dataset/member (delete via VTOC, not catalog)
- UTIL-107.2: IEHPROGM — RENAME dataset/member
- UTIL-107.3: IEHPROGM — CATLG/UNCATLG (delegate to catalog.rs)
- UTIL-107.4: IEHLIST — LISTPDS (delegate to pds.rs directory listing)
- UTIL-107.5: IEHLIST — LISTVTOC (simulated volume listing)
- UTIL-107.6: IEHMOVE — MOVE/COPY DSNAME (basic dataset move/copy)
- UTIL-107.7: Register all IEH utilities in UtilityRegistry
**Dependencies:** open-mainframe-dataset (catalog.rs, pds.rs)
**Reference:** https://www.ibm.com/docs/en/zos/2.4.0?topic=utilities-iehprogm-program-maintenance-program

### UTIL-108: AMASPZAP — Superzap Patch Utility
**Complexity:** M
**Priority:** Low — specialized systems programming tool; high security sensitivity
**Stories:**
- UTIL-108.1: Parse NAME/VER/REP/BASE/SETSSI/DUMP/DUMPT/IDRDATA control statements
- UTIL-108.2: VER — verify bytes at CSECT offset
- UTIL-108.3: REP — replace bytes at CSECT offset (only if preceding VER passed)
- UTIL-108.4: DUMP/DUMPT — hex dump of CSECT contents
- UTIL-108.5: SETSSI — set system status indicator
- UTIL-108.6: IDRDATA — insert user identification data
- UTIL-108.7: Audit logging for all modifications
- UTIL-108.8: Register AMASPZAP/SPZAP/IMASPZAP in UtilityRegistry
**Dependencies:** Load module format support
**Reference:** https://www.ibm.com/docs/en/zos/2.4.0?topic=aids-amaspzap

### UTIL-109: TSO/REXX Batch Processors
**Complexity:** L
**Priority:** High — IKJEFT01 is the standard way to run TSO commands and REXX in batch JCL
**Stories:**
- UTIL-109.1: IKJEFT01 — TSO TMP command loop: read SYSTSIN, execute commands sequentially, ignore non-zero RC
- UTIL-109.2: IKJEFT1A — terminate on first non-zero RC, propagate RC in register 15
- UTIL-109.3: IKJEFT1B — like 1A with user abend → S04C handling
- UTIL-109.4: SYSTSIN/SYSTSPRT DD mapping — input commands, output messages
- UTIL-109.5: SYSEXEC/SYSPROC DD — REXX exec library search path
- UTIL-109.6: TSO command routing — ALLOCATE, FREE, LISTDS, LISTCAT, SUBMIT, EXEC, CALL
- UTIL-109.7: IRXJCL — direct REXX exec invocation (no TMP overhead)
- UTIL-109.8: IRXJCL — PARM='execname' and SYSEXEC/SYSPROC search order
- UTIL-109.9: BPXBATCH — SH mode (shell command execution via USS)
- UTIL-109.10: BPXBATCH — PGM mode (direct USS program invocation)
- UTIL-109.11: BPXBATCH — STDIN/STDOUT/STDERR DD mapping
- UTIL-109.12: Register all batch processors in UtilityRegistry
**Dependencies:** open-mainframe-tso (Batch 9), open-mainframe-rexx (Batch 1), open-mainframe-uss (Batch 18)
**Reference:** https://www.ibm.com/docs/en/zos/2.4.0?topic=commands-running-in-batch

### UTIL-110: Utility Framework Enhancements
**Complexity:** S
**Priority:** Medium — improves extensibility for all utility implementations
**Stories:**
- UTIL-110.1: SYSIN control statement parser framework — generic `./ card` and keyword=value parsing
- UTIL-110.2: Standardized DD name resolution — SYSUT1/SYSUT2/SYSIN/SYSPRINT/SORTIN/SORTOUT mapping
- UTIL-110.3: Return code standardization — 0/4/8/12/16 conventions per utility
- UTIL-110.4: SYSPRINT message formatting — utility-standard message IDs and formatting
- UTIL-110.5: IDCAMS JCL-level → dataset-level bridge — route utility.rs IDCAMS stubs to full dataset crate implementation
**Dependencies:** open-mainframe-jcl (executor)
**Reference:** Internal architecture

## Complexity Assessment

| Epic | Size | Justification |
|------|------|---------------|
| UTIL-100 (IEBCOPY) | L | 8 stories; UNLOAD/LOAD format serialization; PDS/PDSE conversion; existing pds.rs helps |
| UTIL-101 (IEBGENER+) | M | 6 stories; control statement parser; data conversion; basic copy already works |
| UTIL-102 (IEBCOMPR) | S | 4 stories; straightforward record comparison |
| UTIL-103 (IEBUPDTE) | M | 8 stories; sequence number management; UPDATE=INPLACE logic |
| UTIL-104 (DFSORT+) | L | 10 stories; ICETOOL is a substantial utility; IFTHEN/FINDREP complex; sort crate exists |
| UTIL-105 (IEBPTPCH) | S | 7 stories; simple print formatting |
| UTIL-106 (IEBDG) | S | 6 stories; pattern generation logic |
| UTIL-107 (IEH utils) | M | 7 stories; mostly delegation to existing catalog/pds code; IEHMOVE has volume scope |
| UTIL-108 (AMASPZAP) | M | 8 stories; binary patching requires load module format knowledge |
| UTIL-109 (TSO/REXX batch) | L | 12 stories; requires TSO command processor, REXX runtime, USS shell — cross-cutting |
| UTIL-110 (Framework) | S | 5 stories; infrastructure improvements for all utilities |

**Overall complexity: L (Large)** — 11 epics, 81 stories covering 15+ distinct utility programs. The existing utility framework (UtilityRegistry, IEFBR14, IEBGENER, SORT) provides a strong foundation. IEBCOPY, DFSORT enhancements, and TSO/REXX batch processors are the highest-priority gaps.
