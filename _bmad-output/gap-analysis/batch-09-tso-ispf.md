# Gap Analysis: TSO/ISPF (Time Sharing Option / Interactive System Productivity Facility)

## Official Specification Summary

**TSO/E (Time Sharing Option/Extended)** is the interactive command processor for z/OS, providing users with a command-line interface for dataset management, job submission, program execution, and system administration. TSO/E has been part of z/OS since the earliest MVS releases.

**ISPF (Interactive System Productivity Facility)** is a full-screen menu-driven interface built on top of TSO, providing panel-based dialogs, a programmable editor, table services, file tailoring, and library management. ISPF is the primary interactive development environment on z/OS.

Together, TSO/ISPF is classified as **Core** (required infrastructure):
- TSO is the primary interactive environment — all z/OS interactive users log on to TSO
- ISPF provides the full-screen 3270-based development environment
- REXX and CLIST scripts run within TSO address spaces
- ISPF services are invoked by programs in COBOL, PL/I, REXX, CLIST, and assembler
- TSO/E provides ~35+ commands for dataset management, job control, and session management
- ISPF provides ~60+ dialog services for display, tables, variables, file tailoring, library access
- ISPF Panel Definition Language defines 3270 screen layouts with )ATTR, )BODY, )INIT, )PROC sections
- ISPF Editor is the primary source code editor with line commands, macros (ISREDIT), and profiles

Key documentation:
- **z/OS TSO/E Command Reference (SA32-0975)** — TSO command syntax
- **z/OS TSO/E User's Guide (SA32-0971)** — TSO usage
- **z/OS ISPF Dialog Developer's Guide and Reference (SC19-3619)** — ISPF programming
- **z/OS ISPF Edit and Edit Macros (SC19-3621)** — ISPF editor reference
- **z/OS ISPF Reference Summary (SC19-3624)** — quick reference for all ISPF services

## Key Features & Capabilities

### 1. TSO/E Commands (~35+ commands)

#### Dataset Management
| Command | Abbreviation | Purpose |
|---------|-------------|---------|
| ALLOCATE | ALLOC | Dynamically allocate datasets (create or connect) |
| FREE | FREE | Deallocate datasets |
| ATTRIB | ATTR | Build attribute list for dataset creation |
| DELETE | DEL | Delete datasets or PDS members |
| RENAME | REN | Rename datasets |
| LISTDS | LISTD | List dataset attributes (LRECL, BLKSIZE, DSORG, etc.) |
| LISTCAT | LISTC | List catalog entries (VSAM and non-VSAM) |
| LISTALC | LISTA | List currently allocated datasets |
| PRINTDS | PR | Print dataset contents |
| COPY | COPY | Copy datasets |
| SMCOPY | | Session Manager copy |

#### Job Management
| Command | Purpose |
|---------|---------|
| SUBMIT | Submit JCL for batch execution |
| STATUS | Display job status (waiting, executing, on output queue) |
| CANCEL | Cancel a batch job |
| OUTPUT | Process job output (SYSOUT) |

#### Program Execution
| Command | Purpose |
|---------|---------|
| EXEC | Execute a CLIST or REXX exec |
| CALL | Load and execute a program (load module) |
| RUN | Compile and execute a program |
| LOADGO | Load and execute an object module |
| TEST | Invoke the TSO test facility (debugger) |
| LINK | Invoke the linkage editor |
| ALTLIB | Define alternative libraries for CLISTs/REXX execs |

#### Session Management
| Command | Purpose |
|---------|---------|
| LOGON | Start TSO session |
| LOGOFF | End TSO session |
| PROFILE | Set session profile options (PREFIX, MSGID, etc.) |
| TERMINAL | Set terminal characteristics |
| SEND | Send message to another user or operator |
| RECEIVE | Receive datasets transmitted by another user |
| TRANSMIT (XMIT) | Transmit datasets to another user or node |
| TIME | Display current time and session duration |
| HELP | Display command help |
| WHEN | Schedule action based on job completion |
| OUTDES | Define output descriptor for SYSOUT |
| LISTBC | List broadcast messages |

#### Dynamic Allocation
| Command | Purpose |
|---------|---------|
| ALLOCATE | Equivalent to JCL DD statement — allocates datasets dynamically |
| FREE | Frees (deallocates) datasets |
| PARMLIB | Access SYS1.PARMLIB members |

### 2. TSO Service Routines (Programmatic Interface)

| Service | Purpose |
|---------|---------|
| IKJPARS | Parse command operands (TSO command syntax parsing) |
| PUTLINE | Write a line of output to terminal |
| GETLINE | Read a line of input from terminal |
| PUTGET | Combined write/read (prompt and response) |
| STACK | Manage input stack (stacked commands) |
| DAIR | Dynamic Allocation Interface Routines |
| IKJEHCIR | Catalog information routine |
| STAX | Set attention exit routine |
| IKJEFT01 | Batch TSO address space (run TSO commands in batch) |
| IKJEFT1A/1B | Alternative batch TSO entry points |

### 3. ISPF Panel Definition Language

#### Panel Sections
| Section | Purpose |
|---------|---------|
| )PANEL | Panel documentation and version info |
| )ATTR | Attribute character definitions (field types, colors, highlighting) |
| )BODY | Physical screen layout (text, input fields, output fields) |
| )INIT | Initialization processing (before display) |
| )REINIT | Reinitialization processing (before redisplay) |
| )PROC | Processing section (after user input — validation, variable processing) |
| )MODEL | Model lines for table display (TBDISPL) |
| )AREA | Scrollable area definition |
| )PNTS | Point-and-shoot field definitions |
| )FIELD | Scrollable field definitions |
| )END | End of panel definition |

#### Attribute Types
| Type | Keyword | Purpose |
|------|---------|---------|
| TEXT | TYPE(TEXT) | Protected display text |
| INPUT | TYPE(INPUT) | Unprotected input field |
| OUTPUT | TYPE(OUTPUT) | Protected output field |
| NEF | TYPE(NEF) | Normal Entry Field (CUA) |
| CEF | TYPE(CEF) | Choice Entry Field (CUA) |
| PS | TYPE(PS) | Point-and-Shoot field |
| PT | TYPE(PT) | Panel Title (CUA) |
| PIN | TYPE(PIN) | Panel Instruction (CUA) |
| SAC | TYPE(SAC) | Scrollable Area Choice |

#### Attribute Keywords
| Keyword | Purpose |
|---------|---------|
| INTENS(HIGH/LOW/NON) | Display intensity |
| COLOR(RED/GREEN/BLUE/WHITE/YELLOW/TURQ/PINK) | Field color |
| HILITE(USCORE/REVERSE/BLINK) | Highlighting |
| CAPS(ON/OFF) | Uppercase translation |
| JUST(LEFT/RIGHT/ASIS) | Justification |
| PAD(char/NULLS/USER) | Pad character for input fields |
| SKIP(YES/NO) | Auto-skip after field |
| SCROLL(ON/OFF) | Scrollable field |
| PADC(char) | Pad character |

#### Panel Executable Statements (in )INIT, )REINIT, )PROC)
| Statement | Purpose |
|-----------|---------|
| &var = value | Variable assignment |
| IF / ELSE / END | Conditional logic |
| VER(field,type,LIST/RANGE/PICT/MSG) | Field verification |
| VGET var (SHARED/PROFILE/ASIS) | Retrieve dialog variable |
| VPUT var (SHARED/PROFILE/ASIS) | Store dialog variable |
| REFRESH(fields) | Refresh field display |
| .ZVARS = 'varnames' | Map Z placeholders to variable names |
| .MSG = msgid | Set message to display |
| .CURSOR = fieldname | Set cursor position |
| .CSRROW = n | Set cursor row for table display |
| .ALARM = YES/NO | Sound terminal alarm |
| .HELP = panelname | Set help panel |
| .TRAIL = text | Set trailing text |
| TRANS(var, val1,res1, ...) | Translate values |
| TRUNC(var, char) | Truncate at character |
| *REXX(...) | Invoke panel REXX |

### 4. ISPF Services (~60+ services)

#### Display Services
| Service | Purpose |
|---------|---------|
| DISPLAY | Display a panel and process input |
| TBDISPL | Display a table in scrollable panel |
| SETMSG | Set a message for next display |
| PQUERY | Query panel area information |
| ADDPOP | Add pop-up window |
| REMPOP | Remove pop-up window |
| CONTROL | Set processing modes (ERRORS, DISPLAY, SPLIT, etc.) |
| SELECT | Start a dialog function (PGM, CMD, PANEL) |

#### Variable Services
| Service | Purpose |
|---------|---------|
| VGET | Retrieve variables from shared/profile/system pool |
| VPUT | Update variables in shared/profile pool |
| VCOPY | Copy variable value to program variable |
| VDEFINE | Define program variable to ISPF |
| VDELETE | Delete variable definition |
| VREPLACE | Replace variable value |
| VRESET | Reset all function pool variables |
| VERASE | Erase variable from pool |
| VMASK | Associate edit mask with variable |

#### Table Services
| Service | Purpose |
|---------|---------|
| TBCREATE | Create a new table |
| TBOPEN | Open an existing table |
| TBCLOSE | Close and save table |
| TBEND | Close table without saving |
| TBADD | Add row to table |
| TBPUT | Update current row |
| TBMOD | Update or add row (by key) |
| TBDELETE | Delete current row |
| TBGET | Retrieve row by key or position |
| TBSCAN | Search table for matching row |
| TBSARG | Define search argument for TBSCAN |
| TBSKIP | Move row pointer forward/backward |
| TBTOP | Set row pointer to top |
| TBBOTTOM | Set row pointer to bottom |
| TBSORT | Sort table by specified columns |
| TBQUERY | Query table information (rows, keys, names) |
| TBSTATS | Retrieve table statistics |
| TBSAVE | Save table to library |
| TBDISPL | Display table in scrollable panel |
| TBVCLEAR | Clear table variables |
| TBEXIST | Check if row exists (by key) |

#### File Tailoring Services
| Service | Purpose |
|---------|---------|
| FTOPEN | Begin file tailoring |
| FTINCL | Include and process a skeleton |
| FTCLOSE | End file tailoring (write output) |
| FTERASE | Erase file tailoring output |

Skeleton control statements:
| Statement | Purpose |
|-----------|---------|
| )SEL / )ENDSEL | Conditional inclusion |
| )DOT / )ENDDOT | Loop through table rows |
| )SET | Set variable value |
| )BLANK | Insert blank line |
| )CM | Comment |
| )DEFAULT | Set default delimiters |
| )IM | Imbed another skeleton |
| )TB | Set tab positions |

#### Library Access Services (LM services)
| Service | Purpose |
|---------|---------|
| LMINIT | Initialize a data-id for a dataset |
| LMFREE | Free a data-id |
| LMOPEN | Open dataset for processing |
| LMCLOSE | Close dataset |
| LMGET | Read a record |
| LMPUT | Write a record |
| LMCOPY | Copy a member |
| LMMOVE | Move a member |
| LMRENAME | Rename a member |
| LMMFIND | Find a member |
| LMMADD | Add a member |
| LMMDEL | Delete a member |
| LMMREP | Replace a member |
| LMMLIST | List members |
| LMMSTATS | Set/get member statistics |
| LMQUERY | Query dataset attributes |
| LMCOMP | Compress PDS |
| LMPROM | Promote member |
| LIBDEF | Define application libraries (ISPPLIB, ISPMLIB, ISPSLIB, ISPLLIB) |

#### Other Services
| Service | Purpose |
|---------|---------|
| BROWSE | Browse a dataset (read-only) |
| EDIT | Edit a dataset |
| VIEW | View a dataset (like edit, but changes not saved) |
| BRIF | Browse interface (with user exit) |
| EDIF | Edit interface (with user exit) |
| VIIF | View interface (with user exit) |
| EDREC | Edit recovery |
| LOG | Write message to log dataset |
| LIST | Write to list dataset |
| GETMSG | Get message text by message ID |
| GRINIT/GRTERM/GRERROR | Graphics services |
| TRANS | Translation service |

### 5. ISPF Editor

#### Line Commands
| Command | Purpose |
|---------|---------|
| I / In | Insert n lines |
| D / Dn / DD | Delete line(s) or block |
| C / CC | Copy line or block |
| M / MM | Move line or block |
| R / Rn / RR | Repeat line(s) or block |
| A | After (destination for copy/move) |
| B | Before (destination for copy/move) |
| O / OO | Overlay block |
| > / < | Shift right/left |
| ) / ( | Shift data right/left |
| X / XX | Exclude line(s) or block |
| S / Sn | Show excluded lines |
| F / Fn | First n excluded lines |
| L / Ln | Last n excluded lines |
| TE / TEn | Text entry mode |
| TF / TFn | Text flow mode |
| TS | Text split |
| LC / UC | Lowercase / uppercase line |
| LCC / UCC | Lowercase / uppercase block |
| .label | Set line label (.A through .Z) |
| COLS | Display column ruler |
| TABS | Display tab line |
| MASK | Display data mask |
| BNDS | Display bounds |
| MD | Display model line |
| NOTE | Display note line |

#### Primary Commands
| Command | Purpose |
|---------|---------|
| FIND string | Find text in data |
| CHANGE string1 string2 | Find and replace |
| SORT | Sort lines |
| COPY | Copy from another dataset/member |
| MOVE | Move from another dataset/member |
| CREATE | Create a new dataset/member from current data |
| REPLACE | Replace a dataset/member |
| SUBMIT | Submit current data as JCL |
| SAVE | Save current data |
| END | Save and exit |
| CANCEL | Exit without saving |
| RESET | Reset pending line commands and excluded lines |
| PROFILE | Display/set edit profile |
| BOUNDS | Set column boundaries |
| COLS | Toggle column ruler |
| HEX | Toggle hexadecimal display |
| COMPARE | Compare with another dataset/member |
| CUT / PASTE | Clipboard operations |
| UNDO / REDO | Undo/redo changes |
| HILITE | Syntax highlighting (COBOL, JCL, REXX, etc.) |
| LOCATE | Position to a line or label |
| EXCLUDE | Exclude matching lines |
| NUMBER | Toggle line numbering |
| UNNUMBER | Remove line numbers |
| RECOVERY | Toggle recovery mode |
| TABS | Set tab definitions |
| DEFINE | Define PFKEY or command alias |
| BUILTIN | Execute built-in (vs. macro) command |
| MACRO | Execute edit macro |

#### Edit Macros (ISREDIT)
Edit macros are REXX or CLIST programs that invoke ISPF Edit services:
```rexx
/* REXX edit macro */
ADDRESS ISREDIT
"MACRO (ARGS)"
"FIND 'TODO' FIRST"
DO WHILE RC = 0
  "(LINE) = LINE .ZCSR"
  "(DATA) = LINE" LINE
  SAY "Line" LINE":" DATA
  "FIND 'TODO' NEXT"
END
```

Key ISREDIT services: MACRO, FIND, CHANGE, LINE (get/set), LINENUM, CURSOR, LABEL, INSERT, DELETE, COPY, MOVE, SHIFT, BOUNDS, EXCLUDE, RESET, PROCESS, SAVE, END, CANCEL, DEFINE, BUILTIN, USER_STATE, DATA_CHANGED, DISPLAY_COLS, DISPLAY_LINES, PROFILE, RECOVERY, DATASET, MEMBER, etc.

### 6. ISPF Dialog Variable Pools

| Pool | Scope | Persistence |
|------|-------|-------------|
| Function Pool | Current function only | Session only |
| Shared Pool | Across functions in split screen | Session only |
| Profile Pool | Per ISPF application | Persistent (saved to ISPPROF) |
| System Pool | System-wide | Read-only (system variables like ZDATE, ZTIME, ZUSER) |

Key system variables: ZDATE, ZTIME, ZDAY, ZJDATE, ZUSER, ZPREFIX, ZSCREEN, ZSPLIT, ZTEMPF, ZTEMPN, ZENVIR, ZAPPLID, ZPANELID, ZLANG, ZACCTNUM, etc.

### 7. ISPF Utilities (Option 3.x)

| Option | Utility | Purpose |
|--------|---------|---------|
| 3.1 | Library Utility | View/manage PDS/PDSE members (browse, edit, delete, rename, copy, print) |
| 3.2 | Dataset Utility | Allocate, rename, delete, catalog, uncatalog datasets |
| 3.3 | Move/Copy Utility | Copy or move datasets and members |
| 3.4 | Dataset List (DSLIST) | List datasets matching a pattern, with actions |
| 3.5 | Reset Statistics | Reset ISPF statistics on PDS members |
| 3.6 | Hardcopy Utility | Print datasets |
| 3.8 | Outlist Utility | View job output |
| 3.9 | Command Table Utility | Modify command tables |
| 3.12 | SuperC (Compare) | Compare datasets |
| 3.13 | SuperCE (Extended Compare) | Extended compare with merge |
| 3.14 | Search-For (SRCHFOR) | Search for strings across datasets |
| 3.15 | Search-ForE (Extended Search) | Extended string search |
| 3.17 | Toolkit Utility | General utilities toolkit |

### 8. TSO/REXX and TSO/CLIST Integration

#### ADDRESS Environments
| Environment | Purpose |
|-------------|---------|
| ADDRESS TSO | Execute TSO commands from REXX |
| ADDRESS ISPEXEC | Invoke ISPF services from REXX |
| ADDRESS ISREDIT | Invoke ISPF Edit services from REXX (edit macros) |
| ADDRESS CONSOLE | Issue MVS operator commands |
| ADDRESS MVS | Execute MVS programs via ATTACH |
| ADDRESS LINK/LINKMVS | Call programs via LINK SVC |
| ADDRESS ATTACH/ATTCHMVS | Call programs via ATTACH SVC |

#### Key REXX/TSO Functions
| Function | Purpose |
|----------|---------|
| OUTTRAP | Trap TSO command output into stem variable |
| SYSDSN | Check if dataset exists |
| SYSVAR | Get TSO session variable (SYSUID, SYSPROC, etc.) |
| LISTDSI | Get dataset information into variables |
| MSG | Control message display |
| PROMPT | Control prompting |
| SYSCALLS | Control UNIX syscall environment |

## Current OpenMainframe Status

**Minimal TSO/ISPF implementation exists.** A search across the codebase found:

- **`crates/open-mainframe-dataset/src/pds.rs`**: Contains `IspfStats` struct for PDS member statistics — tracks version, modification date/time, modification count, user ID, current/initial line counts. This is ISPF-compatible metadata for PDS members.
- No TSO command processor
- No ISPF panel definition parser
- No ISPF dialog services
- No ISPF editor
- No ISPF table services
- No file tailoring
- No ISREDIT macro support
- No TSO session management (LOGON/LOGOFF, PROFILE)
- No ISPF variable pools

The existing `IspfStats` in the PDS crate represents a small but useful partial implementation — ISPF statistics are critical metadata for PDS member management.

## Gap Details

| Feature | Official z/OS | OpenMainframe | Gap |
|---------|--------------|---------------|-----|
| TSO command processor (~35+ commands) | Full | None | **Missing** |
| ALLOCATE/FREE — dynamic dataset allocation | Full | None (dataset crate exists, no TSO command layer) | **Missing** |
| SUBMIT/STATUS/CANCEL — job management | Full | None | **Missing** |
| EXEC — CLIST/REXX execution environment | Full | None (no REXX/CLIST engines yet) | **Missing** |
| LISTDS/LISTCAT — dataset information | Full | None (datasets accessible, no TSO commands) | **Missing** |
| TRANSMIT/RECEIVE — dataset transfer | Full | None | **Missing** |
| TSO session management (LOGON/LOGOFF/PROFILE) | Full | None | **Missing** |
| TSO service routines (PUTLINE/GETLINE/IKJPARS) | Full | None | **Missing** |
| IKJEFT01 — batch TSO | Full | None | **Missing** |
| ISPF Panel Definition Language ()ATTR/)BODY/)INIT/)PROC) | Full | None | **Missing** |
| Panel attribute types (TEXT/INPUT/OUTPUT/NEF/PS) | Full | None | **Missing** |
| Panel executable statements (IF/VER/VGET/VPUT/TRANS) | Full | None | **Missing** |
| ISPF DISPLAY service | Full | None | **Missing** |
| ISPF SELECT service | Full | None | **Missing** |
| ISPF variable services (VGET/VPUT/VCOPY/VDEFINE) | Full | None | **Missing** |
| ISPF variable pools (function/shared/profile/system) | Full | None | **Missing** |
| ISPF table services (20+ services: TBCREATE/TBOPEN/TBPUT/etc.) | Full | None | **Missing** |
| ISPF file tailoring (FTOPEN/FTINCL/FTCLOSE, skeletons) | Full | None | **Missing** |
| ISPF library services (15+ LM services) | Full | None | **Missing** |
| ISPF BROWSE/EDIT/VIEW services | Full | None | **Missing** |
| ISPF Editor line commands (~30+) | Full | None | **Missing** |
| ISPF Editor primary commands (~30+) | Full | None | **Missing** |
| ISREDIT edit macros | Full | None (no REXX yet) | **Missing** |
| ISPF Edit profiles | Full | None | **Missing** |
| ISPF Utilities (3.1–3.17) | Full | None | **Missing** |
| ISPF system variables (ZDATE, ZTIME, ZUSER, etc.) | Full | None | **Missing** |
| PDS member ISPF statistics | Full | Partial (IspfStats struct exists) | **Partial** |
| ISPF ADDPOP/REMPOP pop-up windows | Full | None | **Missing** |
| ISPF CONTROL service | Full | None | **Missing** |
| ISPF LIBDEF — application library management | Full | None | **Missing** |
| ISPF message system (SETMSG/GETMSG) | Full | None | **Missing** |
| ADDRESS TSO/ISPEXEC/ISREDIT environments | Full | None | **Missing** |
| OUTTRAP/SYSDSN/LISTDSI REXX functions | Full | None | **Missing** |

## Proposed Epic Structure

### Epic T100: TSO Command Processor Core
- **T100.1**: Command parser — TSO command syntax, operands, keyword/positional params
- **T100.2**: ALLOCATE/FREE — dynamic dataset allocation (map to dataset crate)
- **T100.3**: DELETE/RENAME — dataset and member management
- **T100.4**: LISTDS/LISTCAT/LISTALC — dataset information display
- **T100.5**: PROFILE — session profile management (PREFIX, MSGID, etc.)
- **T100.6**: HELP — online help system
- **T100.7**: ALTLIB — alternative library management
- **Complexity**: M (Medium)

### Epic T101: TSO Job Management
- **T101.1**: SUBMIT — submit JCL from dataset or terminal
- **T101.2**: STATUS — display job status
- **T101.3**: CANCEL — cancel running/queued jobs
- **T101.4**: OUTPUT — retrieve and display job output
- **T101.5**: WHEN — conditional action on job completion
- **Complexity**: S (Small — depends on JES2, Batch 11)

### Epic T102: TSO Program Execution & Communication
- **T102.1**: EXEC — execute CLIST/REXX exec (dispatch to engines)
- **T102.2**: CALL — load and execute a program
- **T102.3**: SEND/RECEIVE/TRANSMIT — inter-user communication
- **T102.4**: TERMINAL — terminal characteristics management
- **T102.5**: TSO service routines — PUTLINE/GETLINE/PUTGET/STACK
- **T102.6**: IKJPARS — command operand parsing
- **T102.7**: IKJEFT01 — batch TSO execution
- **Complexity**: M (Medium)

### Epic T103: ISPF Panel Definition Language
- **T103.1**: Panel parser — )ATTR, )BODY, )INIT, )REINIT, )PROC, )MODEL, )AREA, )PNTS, )END
- **T103.2**: Attribute definitions — TYPE, INTENS, COLOR, HILITE, CAPS, JUST, PAD, SKIP
- **T103.3**: Body layout — field placement, Z variables, dynamic areas
- **T103.4**: Executable statements — assignment, IF/ELSE/END, VER, VGET, VPUT, REFRESH
- **T103.5**: Built-in functions — TRANS, TRUNC
- **T103.6**: Shadow variables — character-level attributes
- **T103.7**: Point-and-shoot ()PNTS) and scrollable fields ()FIELD)
- **T103.8**: Panel REXX integration (*REXX)
- **Complexity**: L (Large — complex panel language)

### Epic T104: ISPF Display & Dialog Services
- **T104.1**: DISPLAY — panel display and input processing
- **T104.2**: TBDISPL — table display with scrolling
- **T104.3**: SELECT — start dialog function (PGM, CMD, PANEL)
- **T104.4**: SETMSG/GETMSG — message display and retrieval
- **T104.5**: ADDPOP/REMPOP — pop-up window management
- **T104.6**: CONTROL — processing mode control (ERRORS, DISPLAY, SPLIT)
- **T104.7**: LOG/LIST — logging services
- **Complexity**: L (Large — core ISPF runtime)

### Epic T105: ISPF Variable Services
- **T105.1**: Variable pool model — function, shared, profile, system pools
- **T105.2**: VGET/VPUT — variable retrieval and storage
- **T105.3**: VCOPY/VDEFINE/VREPLACE/VDELETE/VRESET — variable management
- **T105.4**: VERASE — variable cleanup
- **T105.5**: VMASK — edit masks
- **T105.6**: System variables — ZDATE, ZTIME, ZUSER, ZSCREEN, ZAPPLID, etc.
- **T105.7**: Profile pool persistence (ISPPROF dataset)
- **Complexity**: M (Medium)

### Epic T106: ISPF Table Services
- **T106.1**: TBCREATE/TBOPEN/TBCLOSE/TBEND — table lifecycle
- **T106.2**: TBADD/TBPUT/TBMOD/TBDELETE — row operations
- **T106.3**: TBGET/TBSCAN/TBSARG — row retrieval and search
- **T106.4**: TBSKIP/TBTOP/TBBOTTOM — row pointer navigation
- **T106.5**: TBSORT — table sorting
- **T106.6**: TBQUERY/TBSTATS — table information
- **T106.7**: TBSAVE — persist table to library
- **T106.8**: TBDISPL — table display integration
- **T106.9**: TBVCLEAR/TBEXIST — utility operations
- **Complexity**: M (Medium)

### Epic T107: ISPF File Tailoring
- **T107.1**: FTOPEN/FTCLOSE — begin/end file tailoring session
- **T107.2**: FTINCL — include and process skeleton
- **T107.3**: FTERASE — erase output
- **T107.4**: Skeleton control statements — )SEL/)ENDSEL, )DOT/)ENDDOT, )SET, )BLANK, )CM, )DEFAULT, )IM, )TB
- **T107.5**: Variable substitution in skeletons
- **Complexity**: S (Small)

### Epic T108: ISPF Library Access Services
- **T108.1**: LMINIT/LMFREE — dataset initialization and cleanup
- **T108.2**: LMOPEN/LMCLOSE — dataset open/close
- **T108.3**: LMGET/LMPUT — record read/write
- **T108.4**: LMCOPY/LMMOVE/LMRENAME — member operations
- **T108.5**: LMMFIND/LMMADD/LMMDEL/LMMREP/LMMLIST — member management
- **T108.6**: LMMSTATS — member statistics (integrates with existing IspfStats)
- **T108.7**: LMQUERY — dataset attribute query
- **T108.8**: LIBDEF — application library allocation (ISPPLIB, ISPMLIB, ISPSLIB, ISPLLIB)
- **Complexity**: M (Medium — leverages existing dataset crate)

### Epic T109: ISPF Editor
- **T109.1**: Editor core — line-based editor with numbered/unnumbered mode
- **T109.2**: Line commands — I, D, C, M, R, CC/MM/RR blocks, >/< shift, X/S exclude/show
- **T109.3**: Primary commands — FIND, CHANGE, SORT, RESET, COPY, MOVE, CREATE, REPLACE
- **T109.4**: SUBMIT/SAVE/END/CANCEL — file operations
- **T109.5**: Edit profiles — per-type settings (COBOL, JCL, REXX, etc.)
- **T109.6**: UNDO/REDO — change tracking
- **T109.7**: HEX display — hexadecimal view mode
- **T109.8**: COMPARE — dataset comparison
- **T109.9**: HILITE — syntax highlighting
- **T109.10**: CUT/PASTE — clipboard operations
- **T109.11**: Line labels (.A through .Z) and LOCATE
- **Complexity**: XL (Extra Large — full-featured editor)

### Epic T110: ISREDIT Edit Macros
- **T110.1**: ISREDIT service interface — ADDRESS ISREDIT from REXX/CLIST
- **T110.2**: MACRO command — declare edit macro with parameters
- **T110.3**: Data access — LINE (get/set), LINENUM, CURSOR, LABEL, DATASET, MEMBER
- **T110.4**: Edit operations — FIND, CHANGE, INSERT, DELETE, COPY, MOVE, SHIFT
- **T110.5**: State management — USER_STATE, DATA_CHANGED, SAVE, END, CANCEL
- **T110.6**: Display control — DISPLAY_COLS, DISPLAY_LINES, EXCLUDE, RESET
- **Complexity**: M (Medium — depends on REXX engine, Batch 1)

### Epic T111: ISPF Utilities
- **T111.1**: Library Utility (3.1) — PDS member management
- **T111.2**: Dataset Utility (3.2) — dataset allocation, rename, delete
- **T111.3**: Move/Copy Utility (3.3) — dataset/member copy and move
- **T111.4**: Dataset List (3.4) — pattern-based dataset listing with actions
- **T111.5**: SuperC/SuperCE (3.12/3.13) — dataset comparison
- **T111.6**: Search-For (3.14/3.15) — string search across datasets
- **Complexity**: L (Large — many utilities, each with own panels)

### Epic T112: TSO/REXX/CLIST Integration
- **T112.1**: ADDRESS TSO — execute TSO commands from REXX
- **T112.2**: ADDRESS ISPEXEC — invoke ISPF services from REXX
- **T112.3**: ADDRESS ISREDIT — invoke ISREDIT from REXX (edit macros)
- **T112.4**: OUTTRAP — trap command output into stem variable
- **T112.5**: SYSDSN — check dataset existence
- **T112.6**: LISTDSI — get dataset attributes into variables
- **T112.7**: SYSVAR — get TSO session variables
- **T112.8**: MSG/PROMPT — message and prompt control
- **Complexity**: M (Medium — depends on REXX/CLIST engines)

## Dependencies

| Dependency | Crate / Component | Reason |
|------------|-------------------|--------|
| open-mainframe-dataset | Dataset access | ALLOCATE/FREE, LM services, PDS member ops |
| open-mainframe-tui | 3270 terminal | ISPF panels render on 3270 |
| open-mainframe-encoding | EBCDIC support | TSO/ISPF operates in EBCDIC |
| open-mainframe-jcl | JCL processing | SUBMIT command, file tailoring generates JCL |
| RACF (Batch 8) | Security | TSO LOGON authentication, ISPF library access |
| REXX (Batch 1) | Scripting | Edit macros, ADDRESS ISPEXEC, EXEC command |
| CLIST (Batch 5) | Scripting | EXEC command, TSO CLIST execution |
| JES2 (Batch 11) | Job management | SUBMIT/STATUS/CANCEL/OUTPUT commands |
| open-mainframe-sort | Sorting | SORT operations in editor and tables |

**Critical dependency**: TSO/ISPF is the interactive environment for z/OS. REXX scripts, CLIST scripts, COBOL programs, and virtually all interactive applications depend on TSO/ISPF services. The ISPF panel language and display services (T103-T104) are the most critical — they enable all interactive application development.

## Complexity Estimate

| Epic | Complexity | Rationale |
|------|-----------|-----------|
| T100 (TSO Commands) | M | Command parser + dataset management |
| T101 (Job Management) | S | Thin layer over JES2 |
| T102 (Program Execution) | M | EXEC, CALL, service routines |
| T103 (Panel Language) | L | Complex panel definition parser |
| T104 (Display/Dialog) | L | Core ISPF runtime engine |
| T105 (Variables) | M | Four-pool variable model |
| T106 (Table Services) | M | 20+ table operations |
| T107 (File Tailoring) | S | Skeleton processing with substitution |
| T108 (Library Services) | M | LM services atop dataset crate |
| T109 (Editor) | XL | Full-featured source code editor |
| T110 (Edit Macros) | M | ISREDIT service interface |
| T111 (Utilities) | L | Multiple utility dialogs |
| T112 (REXX/CLIST Integration) | M | ADDRESS environments |

**Total estimated effort**: 13 epics, overall XXL (Double Extra Large) complexity. TSO/ISPF is arguably the largest single subsystem in z/OS from an implementation perspective. The ISPF editor alone (T109) is an XL epic, and the panel language (T103) combined with the dialog runtime (T104) form the core of all interactive mainframe applications.

**Recommendation**: Implement in stages:
1. **Phase A (Foundation)**: T100 (TSO commands) + T105 (variables) + T103 (panel parser) — enables basic interactive sessions
2. **Phase B (Display)**: T104 (display services) + T106 (tables) — enables panel-based dialogs
3. **Phase C (Editor)**: T109 (editor) + T110 (edit macros) — enables source code editing
4. **Phase D (Services)**: T107 (file tailoring) + T108 (library) + T111 (utilities) — full ISPF environment
5. **Phase E (Integration)**: T101 (jobs) + T102 (execution) + T112 (REXX/CLIST) — complete TSO/ISPF

## Reference Documentation

- [z/OS TSO/E Command Reference — List of Commands (z/OS 3.1)](https://www.ibm.com/docs/en/zos/3.1.0?topic=commands-list-tsoe)
- [z/OS TSO/E Command Reference (SA32-0975, PDF, z/OS 2.4)](https://www.ibm.com/docs/en/SSLTBW_2.4.0/pdf/ikjc500_v2r4.pdf)
- [z/OS TSO/E User's Guide (SA32-0971, PDF, z/OS 3.1)](https://www.ibm.com/docs/en/SSLTBW_3.1.0/pdf/ikjc200_v3r1.pdf)
- [z/OS ISPF Dialog Developer's Guide and Reference (SC19-3619, PDF, z/OS 3.1)](https://www.ibm.com/docs/en/SSLTBW_3.1.0/pdf/f54dg00_v3r1.pdf)
- [z/OS ISPF Edit and Edit Macros (SC19-3621, PDF, z/OS 3.1)](https://www.ibm.com/docs/en/SSLTBW_3.1.0/pdf/f54em00_v3r1.pdf)
- [z/OS ISPF Reference Summary (SC19-3624, PDF, z/OS 3.1)](https://www.ibm.com/docs/en/SSLTBW_3.1.0/pdf/f54rs00_v3r1.pdf)
- [z/OS ISPF User's Guide Volume I (SC19-3627, PDF, z/OS 2.4)](https://www.ibm.com/docs/en/SSLTBW_2.4.0/pdf/f54ug00_v2r4.pdf)
- [IBM ISPF Product Page](https://www.ibm.com/products/interactive-system-productivity-facility)
- [ISPF — Wikipedia](https://en.wikipedia.org/wiki/ISPF)
- [ISPF Panels Advanced — SHARE Presentation (PDF)](https://share.confex.com/share/117/webprogram/Handout/Session9764/S9764%20-%20ISPF%20Panels%20Advanced.pdf)
- [Write Your First Mainframe App Part 3 — Create an ISPF Panel](https://www.ibm.com/support/pages/node/6614813)
- [TSO Tutorial (Jay Moseley)](https://www.jaymoseley.com/hercules/tso_tutor/tsotutor.htm)
- [REXX ISPF Panels Tutorial](https://www.ibmmainframer.com/rexx-tutorial/rexx-ispf-panels-with-examples/)
- [Developing Dialog Manager Applications in z/OS (PDF)](https://www.trainersfriend.com/SpecialSale/SampleLectureFile.pdf)
