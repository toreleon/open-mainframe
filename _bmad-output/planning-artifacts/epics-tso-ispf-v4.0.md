# TSO/ISPF Crate — Epics & Stories

## Epic T100: TSO Command Processor Core

**Goal:** Implement the TSO command processor with dataset allocation, profile management, and core commands.

**Crate:** `open-mainframe-tso`
**FRs:** FR-v4.0-T100

### Story T100.1: TSO Command Parser

As a **TSO user**,
I want **TSO commands to be parsed with keyword and positional parameters**,
So that **I can interact with the system via the TSO command language**.

**Acceptance Criteria:**

**Given** `ALLOC DA('PROD.DATA') FILE(INFILE) SHR REUSE`
**When** parsed
**Then** the command name is ALLOC, keyword parameters DA, FILE, and boolean parameters SHR, REUSE are extracted

**Given** `LISTDS 'SYS1.PARMLIB' MEMBERS STATUS`
**When** parsed
**Then** the positional parameter (dataset name) and keyword flags MEMBERS and STATUS are identified

**Complexity:** M

### Story T100.2: ALLOCATE/FREE Commands

As a **TSO user**,
I want **ALLOCATE and FREE to dynamically allocate and deallocate datasets**,
So that **I can assign datasets to DD names at the terminal**.

**Acceptance Criteria:**

**Given** `ALLOC DA('PROD.DATA') FILE(INFILE) SHR`
**When** executed
**Then** dataset PROD.DATA is allocated to DD name INFILE with shared disposition

**Given** `ALLOC DA('USER01.NEW.DATA') FILE(OUTFILE) NEW SPACE(5,5) TRACKS LRECL(80) RECFM(F,B) BLKSIZE(3120)`
**When** executed
**Then** a new dataset is created with the specified DCB attributes and allocated to OUTFILE

**Given** `FREE FILE(INFILE)`
**When** executed
**Then** DD name INFILE is deallocated; the dataset is released

**Complexity:** M

### Story T100.3: Dataset Information Commands

As a **TSO user**,
I want **LISTDS, LISTCAT, and LISTALC to display dataset information**,
So that **I can query dataset attributes and current allocations**.

**Acceptance Criteria:**

**Given** `LISTDS 'SYS1.PARMLIB' MEMBERS STATUS`
**When** executed
**Then** the dataset attributes (DSORG, RECFM, LRECL, BLKSIZE) and member list are displayed

**Given** `LISTALC STATUS`
**When** executed
**Then** all currently allocated datasets are listed with DD names and dispositions

**Complexity:** S

### Story T100.4: PROFILE and Session Management

As a **TSO user**,
I want **the PROFILE command to manage session settings**,
So that **I can configure PREFIX, message display, and terminal behavior**.

**Acceptance Criteria:**

**Given** `PROFILE PREFIX(USER01) MSGID WTPMSG`
**When** executed
**Then** dataset prefix is set to USER01, message IDs are shown, and WTP messages are displayed

**Given** `PROFILE NOPREFIX NOMSGID`
**When** executed
**Then** dataset prefix is cleared and message IDs are suppressed

**Given** `HELP ALLOCATE`
**When** executed
**Then** the help text for the ALLOCATE command is displayed with syntax and operands

**Complexity:** S

### Story T100.5: DELETE, RENAME, and ALTLIB Commands

As a **TSO user**,
I want **DELETE, RENAME, and ALTLIB to manage datasets and libraries**,
So that **I can perform basic dataset operations from the TSO command line**.

**Acceptance Criteria:**

**Given** `DELETE 'USER01.TEMP.DATA'`
**When** executed
**Then** the dataset is deleted (scratched and uncataloged)

**Given** `RENAME 'USER01.OLD.DATA' 'USER01.NEW.DATA'`
**When** executed
**Then** the dataset is renamed in the catalog

**Given** `ALTLIB ACTIVATE APPLICATION(EXEC) DA('USER01.MY.EXEC')`
**When** executed
**Then** USER01.MY.EXEC is added to the application-level exec search path

**Complexity:** S

---

## Epic T101: TSO Job Management

**Goal:** Implement TSO commands for submitting, monitoring, and managing batch jobs via JES2.

**Crate:** `open-mainframe-tso`
**FRs:** FR-v4.0-T101

### Story T101.1: SUBMIT Command

As a **TSO user**,
I want **the SUBMIT command to submit JCL from a dataset**,
So that **I can run batch jobs from my TSO session**.

**Acceptance Criteria:**

**Given** `SUBMIT 'USER01.JCL(MYJOB)'`
**When** executed
**Then** the JCL is read from the PDS member and submitted to JES2; the job number is displayed

**Given** `SUBMIT *` followed by JCL entered at the terminal
**When** END is entered
**Then** the entered JCL is submitted to JES2

**Complexity:** S

### Story T101.2: STATUS and OUTPUT Commands

As a **TSO user**,
I want **STATUS and OUTPUT to monitor and retrieve job results**,
So that **I can track job progress and view output**.

**Acceptance Criteria:**

**Given** `STATUS MYJOB`
**When** executed
**Then** all jobs named MYJOB owned by the current user are displayed with status (INPUT, ACTIVE, OUTPUT)

**Given** `OUTPUT MYJOB(JOB12345) PRINT(*)`
**When** executed
**Then** the job output is displayed at the terminal

**Given** `CANCEL MYJOB(JOB12345)`
**When** executed
**Then** the running or queued job is cancelled

**Complexity:** S

---

## Epic T102: TSO Program Execution & Service Routines

**Goal:** Implement TSO program execution (EXEC, CALL) and service routines (PUTLINE, GETLINE, IKJPARS).

**Crate:** `open-mainframe-tso`
**FRs:** FR-v4.0-T102

### Story T102.1: EXEC and CALL Commands

As a **TSO user**,
I want **EXEC to run REXX/CLIST execs and CALL to invoke programs**,
So that **I can execute scripts and programs from TSO**.

**Acceptance Criteria:**

**Given** `EXEC 'USER01.EXEC(MYSCRIPT)' 'param1 param2'`
**When** executed
**Then** the REXX or CLIST exec is loaded and executed with the parameters passed as the argument string

**Given** `CALL 'SYS1.LINKLIB(IKJEFT1B)'`
**When** executed
**Then** the named program is loaded and given control

**Complexity:** M

### Story T102.2: TSO Service Routines

As a **subsystem developer**,
I want **PUTLINE/GETLINE/PUTGET/STACK service routines**,
So that **TSO commands and programs can perform terminal I/O**.

**Acceptance Criteria:**

**Given** a TSO command calls PUTLINE with a message
**When** processed
**Then** the message is written to the terminal output stream

**Given** a TSO command calls GETLINE
**When** the user enters text
**Then** the text is returned to the calling program

**Given** a TSO command calls STACK with input lines
**When** processed
**Then** the lines are placed on the input stack for subsequent GETLINE calls

**Complexity:** M

### Story T102.3: IKJPARS Command Parsing Service

As a **TSO command developer**,
I want **IKJPARS to parse command operands according to a PCE (parse control entry)**,
So that **TSO commands have standardized parameter parsing**.

**Acceptance Criteria:**

**Given** a command PCE defining keyword DATASET(dsname) and boolean SHR
**When** `MYCOMMAND 'SYS1.DATA' SHR` is parsed via IKJPARS
**Then** the dsname and SHR values are placed in the PDL (parse descriptor list)

**Complexity:** M

### Story T102.4: TRANSMIT/RECEIVE and SEND

As a **TSO user**,
I want **TRANSMIT and RECEIVE to transfer datasets between users**,
So that **I can share datasets with other users or remote systems**.

**Acceptance Criteria:**

**Given** `TRANSMIT USER02 DA('USER01.DATA') SYSOUT`
**When** executed
**Then** the dataset is packaged in XMIT format and sent to USER02's receive queue

**Given** `RECEIVE`
**When** executed and a transmitted dataset is pending
**Then** the user is prompted for the target dataset name and the data is restored

**Complexity:** M

### Story T102.5: Batch TSO Execution (IKJEFT01/1A/1B)

As a **batch programmer**,
I want **IKJEFT01 to execute TSO commands and REXX execs in batch**,
So that **I can script TSO operations in JCL**.

**Acceptance Criteria:**

**Given** `//TSO EXEC PGM=IKJEFT01` with SYSTSIN containing `LISTDS 'SYS1.PARMLIB'`
**When** the step executes
**Then** the TSO command runs in batch and output goes to SYSTSPRT

**Given** `//TSO EXEC PGM=IKJEFT1B` with SYSTSIN containing `%MYSCRIPT`
**When** the step executes
**Then** IKJEFT1B executes the REXX exec; non-zero REXX return code becomes the step return code

**Complexity:** M

---

## Epic T103: ISPF Panel Definition Language

**Goal:** Implement the ISPF panel definition language parser and runtime.

**Crate:** `open-mainframe-ispf`
**FRs:** FR-v4.0-T103

### Story T103.1: Panel Section Parser

As a **ISPF dialog developer**,
I want **ISPF panel sections ()ATTR, )BODY, )INIT, )REINIT, )PROC, )END) parsed**,
So that **panels can be loaded and displayed**.

**Acceptance Criteria:**

**Given** a panel with ``)ATTR`, `)BODY`, `)INIT`, `)PROC`, `)END` sections
**When** the panel is loaded
**Then** each section is parsed and its contents are stored for processing

**Given** `)ATTR DEFAULT(%+_)` with `% TYPE(TEXT) INTENS(HIGH)` and `_ TYPE(INPUT) INTENS(LOW) CAPS(ON)`
**When** parsed
**Then** the attribute characters %, +, _ are mapped to their display characteristics

**Complexity:** L

### Story T103.2: Panel Body Layout and Field Placement

As a **ISPF dialog developer**,
I want **panel body with text, input fields, and dynamic areas**,
So that **panels display data and accept user input**.

**Acceptance Criteria:**

**Given** `)BODY` containing `%COMMAND ===>_ZCMD` and `+DATASET NAME%===>_DSN`
**When** rendered
**Then** `COMMAND ===>` displays as high-intensity text, ZCMD as a low-intensity input field

**Given** a ``)AREA` section defining a scrollable dynamic area
**When** the panel is displayed
**Then** the area supports scrolling through content larger than the visible region

**Complexity:** L

### Story T103.3: Panel Executable Statements

As a **ISPF dialog developer**,
I want **)INIT and )PROC sections to execute assignment, IF/ELSE, VER, VGET, and VPUT**,
So that **panels can validate input and interact with variable pools**.

**Acceptance Criteria:**

**Given** `)PROC` containing `VER (&DSN,NB,DSNAME,MSG=ISRZ002)`
**When** the user presses Enter with an empty DSN field
**Then** error message ISRZ002 is displayed and the cursor positions on DSN

**Given** `)INIT` containing `VGET (ZPREFIX) PROFILE` and `&DSN = &ZPREFIX..DATA`
**When** the panel initializes
**Then** ZPREFIX is retrieved from the profile pool and DSN is assigned

**Given** `IF (&OPT = 1)` / `&SEL = 'PANEL(MAIN1)'` / `ELSE` / `&SEL = 'PANEL(MAIN2)'`
**When** the user enters OPT=1
**Then** SEL is set to `PANEL(MAIN1)`

**Complexity:** M

---

## Epic T104: ISPF Display & Dialog Services

**Goal:** Implement ISPF display, select, message, and control services.

**Crate:** `open-mainframe-ispf`
**FRs:** FR-v4.0-T104

### Story T104.1: DISPLAY and SELECT Services

As a **ISPF dialog developer**,
I want **DISPLAY to show panels and SELECT to invoke dialog functions**,
So that **I can build multi-panel dialog applications**.

**Acceptance Criteria:**

**Given** `ISPEXEC DISPLAY PANEL(MYPANEL)`
**When** invoked
**Then** the panel is displayed, user input is collected, and )PROC is executed

**Given** `ISPEXEC SELECT PGM(MYPROG) PARM(data)`
**When** invoked
**Then** program MYPROG is called with a new function variable pool

**Given** `ISPEXEC SELECT PANEL(MENU1)`
**When** invoked
**Then** panel MENU1 is displayed as a selection menu

**Complexity:** L

### Story T104.2: TBDISPL Table Display

As a **ISPF dialog developer**,
I want **TBDISPL to display table data in scrollable format**,
So that **users can browse and select from tabular data**.

**Acceptance Criteria:**

**Given** `ISPEXEC TBDISPL MYTABLE PANEL(TBLPANEL)`
**When** invoked
**Then** table rows are displayed using the )MODEL section of the panel; scrolling is supported

**Given** the user enters `S` next to a row in the table display
**When** the panel processes
**Then** the row's variables are loaded and the selection action is taken

**Complexity:** L

### Story T104.3: Message and Control Services

As a **ISPF dialog developer**,
I want **SETMSG, GETMSG, CONTROL, and LOG services**,
So that **dialogs can display messages and control processing modes**.

**Acceptance Criteria:**

**Given** `ISPEXEC SETMSG MSG(ISRZ001)`
**When** the next panel is displayed
**Then** message ISRZ001 appears in the message area (short message or long message on Help)

**Given** `ISPEXEC CONTROL ERRORS RETURN`
**When** a subsequent ISPF service returns RC=12
**Then** control returns to the dialog instead of terminating

**Given** `ISPEXEC ADDPOP ROW(5) COLUMN(10)` followed by `ISPEXEC DISPLAY PANEL(POPUP1)`
**When** invoked
**Then** a pop-up window is displayed at the specified position

**Complexity:** M

---

## Epic T105: ISPF Variable Services

**Goal:** Implement the four-pool ISPF variable model with VGET/VPUT and system variables.

**Crate:** `open-mainframe-ispf`
**FRs:** FR-v4.0-T105

### Story T105.1: Variable Pool Model

As a **ISPF dialog developer**,
I want **function, shared, profile, and system variable pools**,
So that **dialogs can store and share data across functions and sessions**.

**Acceptance Criteria:**

**Given** a dialog sets variable `&MYVAR` in its function pool
**When** the dialog's function returns
**Then** `&MYVAR` is no longer accessible (function pool destroyed)

**Given** `ISPEXEC VPUT (MYVAR) SHARED`
**When** another function in the same split screen reads MYVAR
**Then** the value is accessible from the shared pool

**Given** `ISPEXEC VPUT (MYVAR) PROFILE`
**When** the ISPF session ends and a new session starts
**Then** the value is restored from the persistent profile pool (ISPPROF dataset)

**Complexity:** M

### Story T105.2: VGET/VPUT and System Variables

As a **ISPF dialog developer**,
I want **VGET/VPUT to move variables between pools, and system variables to provide runtime info**,
So that **dialogs can access dates, user IDs, and screen dimensions**.

**Acceptance Criteria:**

**Given** `ISPEXEC VGET (ZUSER ZDATE ZTIME ZSCREEN) SHARED`
**When** invoked
**Then** ZUSER contains the TSO user ID, ZDATE the current date, ZTIME the current time, ZSCREEN the screen dimensions

**Given** `ISPEXEC VCOPY MYVAR MYLENGTH MYVALUE MOVE`
**When** invoked
**Then** the variable value is copied to a program buffer with its length

**Given** `ISPEXEC VERASE (TEMPVAR) SHARED`
**When** invoked
**Then** TEMPVAR is removed from the shared pool

**Complexity:** M

---

## Epic T106: ISPF Table Services

**Goal:** Implement ISPF in-memory tables with row operations, search, sort, and persistence.

**Crate:** `open-mainframe-ispf`
**FRs:** FR-v4.0-T106

### Story T106.1: Table Lifecycle

As a **ISPF dialog developer**,
I want **TBCREATE/TBOPEN/TBCLOSE/TBEND/TBSAVE to manage table lifecycle**,
So that **dialogs can work with structured in-memory data**.

**Acceptance Criteria:**

**Given** `ISPEXEC TBCREATE MYTABLE KEYS(KEY1) NAMES(COL1 COL2 COL3) NOWRITE`
**When** invoked
**Then** an in-memory table is created with KEY1 as the key column and three data columns

**Given** `ISPEXEC TBOPEN PERSTTBL WRITE`
**When** invoked
**Then** the table is loaded from the ISPTLIB library into memory

**Given** `ISPEXEC TBSAVE PERSTTBL`
**When** invoked
**Then** the table is written to the ISPTABL library

**Complexity:** M

### Story T106.2: Row Operations and Search

As a **ISPF dialog developer**,
I want **TBADD/TBPUT/TBMOD/TBDELETE for row operations and TBSCAN/TBSARG for searching**,
So that **I can manipulate table data programmatically**.

**Acceptance Criteria:**

**Given** `ISPEXEC TBADD MYTABLE` with KEY1='A001', COL1='Smith'
**When** invoked
**Then** a new row is inserted in key order

**Given** `ISPEXEC TBSCAN MYTABLE ARGLIST(COL1) CONDLIST(EQ)` with COL1='Smith'
**When** invoked
**Then** the CRP is positioned to the first row where COL1 equals 'Smith'

**Given** `ISPEXEC TBSORT MYTABLE FIELDS(COL1,C,A,COL2,C,D)`
**When** invoked
**Then** the table is sorted by COL1 ascending, then COL2 descending (character comparisons)

**Complexity:** M

---

## Epic T107: ISPF File Tailoring

**Goal:** Implement ISPF skeleton processing with variable substitution and control statements.

**Crate:** `open-mainframe-ispf`
**FRs:** FR-v4.0-T107

### Story T107.1: Skeleton Processing

As a **ISPF dialog developer**,
I want **FTOPEN/FTINCL/FTCLOSE to process skeletons with variable substitution**,
So that **I can generate JCL and text files from templates**.

**Acceptance Criteria:**

**Given** `ISPEXEC FTOPEN` then `ISPEXEC FTINCL JCLSKEL` then `ISPEXEC FTCLOSE NAME(OUTDD)`
**When** the skeleton JCLSKEL contains `//&JOBNAME JOB &ACCT,'&PGMR'`
**Then** variables are substituted from the current variable pools and output is written to OUTDD

**Given** a skeleton with `)SEL &OPT = 1` / lines / `)ENDSEL`
**When** &OPT is 1
**Then** the enclosed lines are included; when &OPT is not 1, they are omitted

**Given** a skeleton with `)DOT MYTABLE` / `//DD&ZCNT DD DSN=&DSN,DISP=SHR` / `)ENDDOT`
**When** MYTABLE has 3 rows
**Then** the enclosed lines are repeated once per table row with variables from each row

**Complexity:** S

---

## Epic T108: ISPF Library Access Services

**Goal:** Implement ISPF LM (Library Management) services for programmatic dataset and member access.

**Crate:** `open-mainframe-ispf`
**FRs:** FR-v4.0-T108

### Story T108.1: Dataset Open/Close and Record I/O

As a **ISPF dialog developer**,
I want **LMINIT/LMFREE/LMOPEN/LMCLOSE/LMGET/LMPUT for dataset I/O**,
So that **dialogs can read and write dataset records programmatically**.

**Acceptance Criteria:**

**Given** `ISPEXEC LMINIT DATAID(ID1) DATASET('SYS1.PARMLIB')` then `ISPEXEC LMOPEN DATAID(&ID1) OPTION(INPUT)`
**When** `ISPEXEC LMGET DATAID(&ID1) MODE(INVAR) DATALOC(RECORD) DATALEN(LEN) MAXLEN(80)`
**Then** the next record is returned in variable RECORD

**Given** `ISPEXEC LMCLOSE DATAID(&ID1)` then `ISPEXEC LMFREE DATAID(&ID1)`
**When** invoked
**Then** the dataset is closed and the data ID is released

**Complexity:** M

### Story T108.2: Member Management and LIBDEF

As a **ISPF dialog developer**,
I want **LMMFIND/LMMADD/LMMDEL/LMMREP/LMMLIST/LMMSTATS for member operations and LIBDEF for library allocation**,
So that **dialogs can manage PDS members and override ISPF library concatenations**.

**Acceptance Criteria:**

**Given** `ISPEXEC LMMFIND DATAID(&ID1) MEMBER(MYMBR) STATS(YES)`
**When** invoked
**Then** the member is located and ISPF statistics (ZLVERS, ZLMOD, ZLMDATE, ZLMTIME, ZLUSER) are set

**Given** `ISPEXEC LMMLIST DATAID(&ID1) MEMBER(MBR) OPTION(LIST)`
**When** invoked repeatedly
**Then** each call returns the next member name in alphabetical order

**Given** `ISPEXEC LIBDEF ISPPLIB DATASET ID('USER01.PANELS') STACK`
**When** a subsequent DISPLAY references a panel
**Then** USER01.PANELS is searched first (stacked ahead of the default ISPPLIB concatenation)

**Complexity:** M

---

## Epic T109: ISPF Editor

**Goal:** Implement the ISPF line-based editor with line commands, primary commands, and edit profiles.

**Crate:** `open-mainframe-ispf`
**FRs:** FR-v4.0-T109

### Story T109.1: Editor Core

As a **TSO user**,
I want **a line-based editor for sequential and PDS member datasets**,
So that **I can create and modify source code and data files**.

**Acceptance Criteria:**

**Given** `ISPEXEC EDIT DATASET('USER01.COBOL(MYPROG)')`
**When** invoked
**Then** the member is opened in the editor with line numbers, profile applied, and data displayed

**Given** the user modifies lines and presses PF3 (END)
**When** processed
**Then** the modified data is saved back to the member and the editor exits

**Given** the user presses PF12 (CANCEL)
**When** processed
**Then** all changes are discarded and the original data is preserved

**Complexity:** L

### Story T109.2: Line Commands

As a **TSO user**,
I want **editor line commands (I, D, C, M, R, CC/MM/DD, >, <, X/S, LC/UC)**,
So that **I can insert, delete, copy, move, repeat, shift, and transform lines**.

**Acceptance Criteria:**

**Given** the user types `I5` in the line command area of line 10
**When** processed
**Then** 5 blank lines are inserted after line 10

**Given** `CC` on line 5 and `CC` on line 10, then `A` on line 20
**When** processed
**Then** lines 5-10 are copied after line 20

**Given** `X99999` on line 1 (exclude all) then `F 'ERROR'` with FIND
**When** FIND locates matches
**Then** only lines containing 'ERROR' are shown; excluded lines show `- - - n line(s) excluded`

**Complexity:** L

### Story T109.3: Primary Commands

As a **TSO user**,
I want **FIND, CHANGE, SORT, SUBMIT, COPY, SAVE, and other primary commands**,
So that **I can search, replace, and manage data within the editor**.

**Acceptance Criteria:**

**Given** `FIND 'PERFORM' ALL`
**When** executed
**Then** all occurrences of PERFORM are highlighted and the count is displayed

**Given** `CHANGE 'OLD-VAR' 'NEW-VAR' ALL`
**When** executed
**Then** all occurrences are replaced and the change count is displayed

**Given** `SUBMIT`
**When** executed
**Then** the current editor data is submitted to JES2 as a batch job

**Given** `COPY 'SYS1.MACLIB(DCBD)' AFTER .A`
**When** executed
**Then** the contents of member DCBD are inserted after the line labeled .A

**Complexity:** L

### Story T109.4: Edit Profiles and UNDO/REDO

As a **TSO user**,
I want **edit profiles per dataset type and UNDO/REDO for change tracking**,
So that **the editor remembers my settings and I can reverse mistakes**.

**Acceptance Criteria:**

**Given** `PROFILE COBOL` is active
**When** a COBOL member is edited
**Then** CAPS ON, NULLS ON, TABS ON, columns 7-72 are set as the edit bounds

**Given** the user issues `UNDO`
**When** processed
**Then** the last edit operation is reversed and the data is restored to the previous state

**Given** `HEX ON`
**When** processed
**Then** each line is displayed with two additional lines showing the hexadecimal representation

**Complexity:** M

---

## Epic T110: ISREDIT Edit Macros

**Goal:** Implement the ISREDIT macro programming interface for REXX/CLIST edit macros.

**Crate:** `open-mainframe-ispf`
**FRs:** FR-v4.0-T110

### Story T110.1: Edit Macro Framework

As a **ISPF macro developer**,
I want **ADDRESS ISREDIT to invoke edit services from REXX macros**,
So that **I can automate repetitive editing tasks**.

**Acceptance Criteria:**

**Given** a REXX exec beginning with `ADDRESS ISREDIT; "MACRO (ARGS)"`
**When** invoked as an initial or line macro
**Then** the macro has access to ISREDIT services and receives its parameters in ARGS

**Given** `"(DATA) = LINE" linenum` via ISREDIT
**When** invoked
**Then** the content of the specified line is placed in variable DATA

**Given** `"LINE" linenum "= (NEWDATA)"` via ISREDIT
**When** invoked
**Then** the specified line is replaced with the value of NEWDATA

**Complexity:** M

### Story T110.2: Macro Edit Services

As a **ISPF macro developer**,
I want **FIND, CHANGE, INSERT, DELETE, EXCLUDE, and cursor control in macros**,
So that **macros can perform complex editing operations programmatically**.

**Acceptance Criteria:**

**Given** `"FIND 'TODO' FIRST"` followed by `"(LINE) = LINE .ZCSR"`
**When** processed in a loop
**Then** the macro can iterate over all lines containing 'TODO' and read their content

**Given** `"INSERT" linenum "DATALINE (NEWLINE)"`
**When** invoked
**Then** a new line is inserted after the specified line number

**Given** `"USER_STATE = (USTATE)"` / edit operations / `"USER_STATE = (USTATE)"`
**When** invoked
**Then** the user state is saved and can be restored to undo macro changes if needed

**Complexity:** M

---

## Epic T111: ISPF Utilities

**Goal:** Implement key ISPF utilities (3.x options) including dataset list, SuperC compare, and search.

**Crate:** `open-mainframe-ispf`
**FRs:** FR-v4.0-T111

### Story T111.1: Library and Dataset Utilities

As a **TSO user**,
I want **Library Utility (3.1) and Dataset Utility (3.2) for member and dataset management**,
So that **I can browse, edit, delete, rename, and copy PDS members and datasets**.

**Acceptance Criteria:**

**Given** the Library Utility (3.1) is invoked with a PDS name
**When** the member list is displayed
**Then** members are listed with ISPF statistics (size, date, user); line commands (E/V/B/D/R/S) are accepted

**Given** the Dataset Utility (3.2) is invoked
**When** the user enters a dataset name and action (A=allocate, D=delete, R=rename)
**Then** the selected action is performed and results are displayed

**Complexity:** M

### Story T111.2: Dataset List and Move/Copy

As a **TSO user**,
I want **Dataset List (3.4) and Move/Copy Utility (3.3) for dataset navigation and transfer**,
So that **I can find and operate on datasets matching a pattern**.

**Acceptance Criteria:**

**Given** Dataset List (3.4) is invoked with pattern `USER01.**`
**When** processed
**Then** all datasets matching the pattern are listed with volume, device, and attributes; line commands are accepted

**Given** the Move/Copy utility (3.3) is invoked
**When** the user specifies source and target datasets with member selections
**Then** the selected members are copied (or moved) to the target PDS

**Complexity:** M

### Story T111.3: SuperC Compare and Search-For

As a **TSO user**,
I want **SuperC (3.12) for dataset comparison and Search-For (3.14) for string searching**,
So that **I can compare versions and search across multiple datasets**.

**Acceptance Criteria:**

**Given** SuperC (3.12) with old and new dataset names
**When** executed
**Then** a listing is produced showing inserted, deleted, and changed lines with context

**Given** Search-For (3.14) with a search string and dataset list
**When** executed
**Then** all occurrences of the string are listed with dataset name, member name, and line

**Complexity:** L

---

## Epic T112: TSO/REXX/CLIST Integration

**Goal:** Implement ADDRESS environments and TSO-specific REXX functions.

**Crate:** `open-mainframe-tso`
**FRs:** FR-v4.0-T112

### Story T112.1: ADDRESS TSO and ADDRESS ISPEXEC

As a **REXX programmer**,
I want **ADDRESS TSO and ADDRESS ISPEXEC to invoke TSO commands and ISPF services from REXX**,
So that **my REXX execs can interact with TSO and ISPF**.

**Acceptance Criteria:**

**Given** `ADDRESS TSO "ALLOC DA('USER01.DATA') FILE(INFILE) SHR"`
**When** executed from REXX
**Then** the TSO ALLOCATE command is executed and RC is set to the command return code

**Given** `ADDRESS ISPEXEC "DISPLAY PANEL(MYPANEL)"`
**When** executed from REXX
**Then** the ISPF panel is displayed and RC reflects the ISPF return code

**Complexity:** M

### Story T112.2: TSO-Specific REXX Functions

As a **REXX programmer**,
I want **OUTTRAP, SYSDSN, LISTDSI, SYSVAR, and MSG functions**,
So that **I can capture command output, check datasets, and query session info from REXX**.

**Acceptance Criteria:**

**Given** `CALL OUTTRAP 'OUT.'` then `ADDRESS TSO "LISTDS 'SYS1.PARMLIB'"` then `CALL OUTTRAP 'OFF'`
**When** executed
**Then** the LISTDS output is captured into stem variable OUT. (OUT.0 = line count, OUT.1..n = lines)

**Given** `X = SYSDSN("'SYS1.PARMLIB'")`
**When** executed
**Then** X is set to 'OK' if the dataset exists, or a diagnostic string like 'DATASET NOT FOUND'

**Given** `X = LISTDSI("'SYS1.PARMLIB'")`
**When** executed
**Then** TSO variables SYSDSORG, SYSRECFM, SYSLRECL, SYSBLKSIZE, SYSALLOC, SYSUSED, etc. are populated

**Given** `X = SYSVAR('SYSUID')`
**When** executed
**Then** X contains the current TSO user ID

**Complexity:** M

---
