# Runtime Crate — Epics & Stories

## Epic 500: External CALL Resolution

**Goal:** Implement external program CALL with parameter passing and program lifecycle management.

**Crate:** `open-mainframe-runtime`
**FRs:** FR-v3.0-500, FR-v3.0-510

### Story 500.1: Program Registry and CALL Dispatch

As a **COBOL developer**,
I want **CALL to resolve and invoke external programs**,
So that **modular COBOL applications execute correctly**.

**Acceptance Criteria:**

**Given** a CALL 'SUBPROG' USING BY REFERENCE WS-DATA
**When** SUBPROG is registered in the program registry
**Then** SUBPROG executes with access to WS-DATA (shared reference)

**Given** CALL 'UNKNOWN'
**When** the program is not in the registry
**Then** a runtime error is raised (or ON EXCEPTION handler invoked)

**Complexity:** L

### Story 500.2: BY REFERENCE, BY CONTENT, BY VALUE

As a **COBOL developer**,
I want **proper parameter passing modes on CALL**,
So that **called programs receive data with correct sharing semantics**.

**Acceptance Criteria:**

**Given** CALL 'SUB' USING BY REFERENCE WS-A, BY CONTENT WS-B
**When** SUB modifies its first parameter
**Then** WS-A in the caller is modified (BY REFERENCE)

**Given** SUB modifies its second parameter
**When** control returns to the caller
**Then** WS-B in the caller is unchanged (BY CONTENT = copy)

**Complexity:** M

### Story 500.3: RETURN-CODE and CANCEL

As a **COBOL developer**,
I want **RETURN-CODE set by called programs and CANCEL to unload programs**,
So that **program status communication and resource management work correctly**.

**Acceptance Criteria:**

**Given** a called program executes MOVE 8 TO RETURN-CODE then GOBACK
**When** control returns to the caller
**Then** RETURN-CODE in the caller contains 8

**Given** CANCEL 'SUBPROG'
**When** executed
**Then** SUBPROG is removed from the program registry and its resources freed

**Complexity:** S

---

## Epic 501: True GO TO and Paragraph Flow Control

**Goal:** Implement unstructured GO TO, PERFORM THRU, and paragraph-level flow control.

**Crate:** `open-mainframe-runtime`
**FRs:** FR-v3.0-501

### Story 501.1: GO TO as Unstructured Jump

As a **COBOL developer**,
I want **GO TO to transfer control without returning**,
So that **legacy COBOL programs with unstructured flow execute correctly**.

**Acceptance Criteria:**

**Given** paragraph A contains GO TO C
**When** executed during PERFORM A THRU D
**Then** control jumps to paragraph C (skipping B), and execution continues through D

**Given** GO TO C outside of PERFORM
**When** executed
**Then** control transfers to paragraph C and does not return to the GO TO location

**Complexity:** L

### Story 501.2: PERFORM THRU and EXIT PARAGRAPH/SECTION

As a **COBOL developer**,
I want **PERFORM para-1 THRU para-n and EXIT PARAGRAPH/SECTION**,
So that **standard COBOL paragraph ranges execute correctly**.

**Acceptance Criteria:**

**Given** PERFORM INIT-PARA THRU INIT-EXIT
**When** executed
**Then** all paragraphs from INIT-PARA to INIT-EXIT are executed sequentially

**Given** EXIT PARAGRAPH within a PERFORM range
**When** executed
**Then** control exits the current paragraph and continues with the next paragraph (or ends PERFORM if it's the THRU target)

**Complexity:** M

### Story 501.3: ALTER Statement

As a **COBOL developer**,
I want **ALTER to change GO TO targets dynamically**,
So that **legacy programs using ALTER execute correctly**.

**Acceptance Criteria:**

**Given** ALTER DISPATCH-PARA TO PROCEED TO NEW-TARGET
**When** DISPATCH-PARA (which contains GO TO) executes
**Then** control transfers to NEW-TARGET instead of the original target

**Complexity:** S

---

## Epic 502: Complete Intrinsic Functions

**Goal:** Expand from 4 to 40+ standard IBM COBOL intrinsic functions.

**Crate:** `open-mainframe-runtime`
**FRs:** FR-v3.0-502

### Story 502.1: String Functions

As a **COBOL developer**,
I want **TRIM, REVERSE, ORD, CHAR, CONCATENATE intrinsic functions**,
So that **string manipulation uses standard COBOL syntax**.

**Acceptance Criteria:**

**Given** FUNCTION TRIM(WS-TEXT)
**When** WS-TEXT = "  HELLO  "
**Then** result is "HELLO"

**Given** FUNCTION REVERSE(WS-TEXT)
**When** WS-TEXT = "ABCDE"
**Then** result is "EDCBA"

**Complexity:** M

### Story 502.2: Numeric Functions

As a **COBOL developer**,
I want **NUMVAL, NUMVAL-C, MOD, REM, INTEGER, MAX, MIN, RANDOM, ABS**,
So that **numeric conversion and math use standard functions**.

**Acceptance Criteria:**

**Given** FUNCTION NUMVAL("  -123.45  ")
**When** evaluated
**Then** result is -123.45 as NumericValue

**Given** FUNCTION MOD(17, 5)
**When** evaluated
**Then** result is 2

**Complexity:** M

### Story 502.3: Date Functions

As a **COBOL developer**,
I want **INTEGER-OF-DATE, DATE-OF-INTEGER, DATE-TO-YYYYMMDD, DAY-OF-INTEGER, WHEN-COMPILED**,
So that **date arithmetic uses standard intrinsic functions**.

**Acceptance Criteria:**

**Given** FUNCTION INTEGER-OF-DATE(20240115)
**When** evaluated
**Then** result is the Lilian day number for January 15, 2024

**Given** FUNCTION DATE-OF-INTEGER(lilian-day)
**When** evaluated
**Then** result is the YYYYMMDD date corresponding to that Lilian day

**Complexity:** M

---

## Epic 503: Numeric Editing and Display Formats

**Goal:** Implement PIC clause numeric editing for display and report formatting.

**Crate:** `open-mainframe-runtime`
**FRs:** FR-v3.0-503

### Story 503.1: Numeric Edit PIC Patterns

As a **COBOL developer**,
I want **numeric editing for Z, *, $, +, -, CR, DB, B, 0 patterns**,
So that **formatted numeric output displays correctly**.

**Acceptance Criteria:**

**Given** PIC ZZ,ZZ9.99 and value 1234.56
**When** displayed
**Then** output is " 1,234.56"

**Given** PIC $$,$$$,$$9.99CR and value -1234.56
**When** displayed
**Then** output is "    $1,234.56CR"

**Given** PIC ***,**9.99 and value 42.00
**When** displayed
**Then** output is "*****42.00"

**Complexity:** L

---

## Epic 504: Binary Storage Formats

**Goal:** Implement COMP, COMP-3, and COMP-5 storage representations.

**Crate:** `open-mainframe-runtime`
**FRs:** FR-v3.0-504

### Story 504.1: Packed Decimal (COMP-3) Support

As a **COBOL developer**,
I want **COMP-3 packed decimal storage and conversion**,
So that **data read from z/OS files is interpreted correctly**.

**Acceptance Criteria:**

**Given** a 4-byte packed decimal field containing 0x01234C
**When** read into a PIC S9(5) COMP-3 variable
**Then** the numeric value is +1234

**Given** a numeric value of -5678
**When** stored as PIC S9(5) COMP-3
**Then** the byte representation is 0x05678D

**Complexity:** M

### Story 504.2: Binary (COMP/COMP-5) Support

As a **COBOL developer**,
I want **COMP and COMP-5 binary storage formats**,
So that **binary integer data is handled correctly**.

**Acceptance Criteria:**

**Given** PIC S9(4) COMP with value 1234
**When** stored
**Then** the representation is a 2-byte big-endian signed integer (0x04D2)

**Given** PIC S9(9) COMP with value -100000
**When** stored
**Then** the representation is a 4-byte big-endian signed integer

**Complexity:** M

---

## Epic 505: File I/O with Status Codes

**Goal:** Implement proper file handling with organizations, status codes, and dataset integration.

**Crate:** `open-mainframe-runtime`
**FRs:** FR-v3.0-505

### Story 505.1: File Status Codes and Sequential File I/O

As a **COBOL developer**,
I want **file operations to set standard 2-byte file status codes**,
So that **programs can handle I/O errors using standard patterns**.

**Acceptance Criteria:**

**Given** READ FILE-1 AT END MOVE 'Y' TO EOF-FLAG
**When** the end of file is reached
**Then** file status = "10" and AT END handler executes

**Given** OPEN INPUT FILE-1
**When** the file does not exist
**Then** file status = "35" (file not found)

**Complexity:** L

### Story 505.2: Indexed File Operations

As a **COBOL developer**,
I want **indexed file operations (READ by key, START, READ NEXT, WRITE, REWRITE, DELETE)**,
So that **VSAM KSDS files are accessible from COBOL programs**.

**Acceptance Criteria:**

**Given** READ FILE-1 KEY IS WS-KEY
**When** the key exists
**Then** the record is returned and file status = "00"

**Given** READ FILE-1 KEY IS WS-KEY
**When** the key does not exist
**Then** file status = "23" (record not found)

**Complexity:** L

---

## Epic 506: SORT Verb Runtime

**Goal:** Implement the SORT verb with integration to the sort crate.

**Crate:** `open-mainframe-runtime`
**FRs:** FR-v3.0-506

### Story 506.1: SORT USING/GIVING

As a **COBOL developer**,
I want **SORT to sort records from input files to output files**,
So that **batch sorting operations work correctly**.

**Acceptance Criteria:**

**Given** SORT SORT-FILE ON ASCENDING KEY SORT-NAME USING INPUT-FILE GIVING OUTPUT-FILE
**When** executed
**Then** records from INPUT-FILE are sorted by SORT-NAME and written to OUTPUT-FILE

**Complexity:** L

### Story 506.2: SORT with INPUT/OUTPUT PROCEDURE

As a **COBOL developer**,
I want **SORT with INPUT PROCEDURE and OUTPUT PROCEDURE for custom processing**,
So that **records can be filtered/transformed during sorting**.

**Acceptance Criteria:**

**Given** SORT SORT-FILE INPUT PROCEDURE IS FILTER-RECS OUTPUT PROCEDURE IS FORMAT-RECS
**When** executed
**Then** FILTER-RECS uses RELEASE to provide records; FORMAT-RECS uses RETURN to retrieve sorted records

**Complexity:** L

---

## Epic 507: PERFORM VARYING and 88-Level Conditions

**Goal:** Implement PERFORM VARYING with AFTER and 88-level condition name evaluation.

**Crate:** `open-mainframe-runtime`
**FRs:** FR-v3.0-507, FR-v3.0-508

### Story 507.1: PERFORM VARYING with AFTER

As a **COBOL developer**,
I want **PERFORM VARYING and nested AFTER for multi-dimensional iteration**,
So that **table processing loops work correctly**.

**Acceptance Criteria:**

**Given** PERFORM PROCESS-CELL VARYING WS-ROW FROM 1 BY 1 UNTIL WS-ROW > 10 AFTER WS-COL FROM 1 BY 1 UNTIL WS-COL > 5
**When** executed
**Then** PROCESS-CELL is called 50 times (10 rows x 5 columns)

**Complexity:** M

### Story 507.2: 88-Level Condition Names

As a **COBOL developer**,
I want **88-level condition names evaluated as boolean conditions and SET to TRUE**,
So that **standard COBOL state management works correctly**.

**Acceptance Criteria:**

**Given** 05 WS-STATUS PIC X. 88 VALID VALUE 'Y'. 88 INVALID VALUE 'N'.
**When** IF VALID is evaluated and WS-STATUS = 'Y'
**Then** the condition is TRUE

**Given** SET INVALID TO TRUE
**When** executed
**Then** WS-STATUS = 'N'

**Complexity:** M

---

## Epic 508: Accurate Date/Time and LE Callable Services

**Goal:** Implement accurate calendar arithmetic and a subset of LE callable services.

**Crate:** `open-mainframe-runtime`
**FRs:** FR-v3.0-509, FR-v3.0-511

### Story 508.1: Calendar-Accurate Date Library

As a **developer**,
I want **accurate Lilian day calculations and date arithmetic**,
So that **date intrinsic functions and LE services produce correct results**.

**Acceptance Criteria:**

**Given** INTEGER-OF-DATE(15821015) — the Lilian epoch (October 15, 1582)
**When** evaluated
**Then** result is 1

**Given** INTEGER-OF-DATE(20240229) — leap year date
**When** evaluated
**Then** result is correct (February 29, 2024 is valid)

**Complexity:** M

### Story 508.2: LE Date/Time Callable Services

As a **COBOL developer**,
I want **CALL 'CEEDAYS', 'CEEDATE', 'CEESECS' for date manipulation**,
So that **programs using LE date services execute correctly**.

**Acceptance Criteria:**

**Given** CALL 'CEEDAYS' USING WS-DATE, 'YYYYMMDD', WS-LILIAN, FC
**When** WS-DATE = '20240115'
**Then** WS-LILIAN contains the correct Lilian day number and FC indicates success

**Complexity:** M
