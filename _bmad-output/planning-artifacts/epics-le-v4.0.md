# Language Environment Crate — Epics & Stories

## Epic LE100: LE Program Model & Enclave Management

**Goal:** Implement the Language Environment process/enclave/thread hierarchy and program lifecycle services.

**Crate:** `open-mainframe-runtime` (extends existing)
**FRs:** FR-v4.0-LE100

### Story LE100.1: Enclave Lifecycle

As a **LE application developer**,
I want **enclave creation, initialization, and termination**,
So that **HLL programs run within a proper LE execution environment**.

**Acceptance Criteria:**

**Given** a COBOL program is invoked via JCL `EXEC PGM=MYPROG`
**When** LE initializes
**Then** a process is created with one enclave; the enclave contains the main routine and its call chain

**Given** `CEE3ABD` is called with abend code U4000 and CLEANUP reason
**When** invoked
**Then** the enclave terminates with an abend; all registered exit procedures (CEERTX) are driven

**Given** `CEE3GRC(&return_code, &fc)` is called
**When** invoked
**Then** the current enclave return code is returned in return_code

**Complexity:** L

### Story LE100.2: Thread Management and Program Information

As a **LE application developer**,
I want **thread creation within enclaves and system information queries**,
So that **multi-threaded LE applications work correctly and can query their environment**.

**Acceptance Criteria:**

**Given** a C program calls `pthread_create()` within an LE enclave
**When** the thread is created
**Then** a new LE thread is established with its own stack and condition handler chain

**Given** `CEE3INF(&sys_type, &env_type, &run_opt, &fc)` is called
**When** invoked
**Then** the system type (z/OS), environment (batch/CICS/TSO), and runtime option settings are returned

**Given** `CEEGPID(&version, &platform, &fc)` is called
**When** invoked
**Then** the LE product version and platform identifier are returned

**Complexity:** L

---

## Epic LE101: Condition Handling Framework

**Goal:** Implement the stack-frame-based condition handler chain with signal, resume, and propagation semantics.

**Crate:** `open-mainframe-runtime`
**FRs:** FR-v4.0-LE101

### Story LE101.1: Condition Handler Registration and Signaling

As a **LE application developer**,
I want **CEEHDLR/CEEHDLU to register/unregister condition handlers and CEESGL to signal conditions**,
So that **programs can intercept and handle runtime errors**.

**Acceptance Criteria:**

**Given** `CEEHDLR(&handler_routine, &token, &fc)` is called in routine A
**When** a condition occurs in routine A or a routine called by A
**Then** the registered handler_routine is invoked with the condition token and can RESUME, PERCOLATE, or PROMOTE

**Given** `CEESGL(&condition_token, &result_code, &fc)` is called with a severity-3 condition
**When** signaled
**Then** the condition handler chain is walked; if no handler resumes, the enclave terminates

**Given** `CEEHDLU(&handler_routine, &fc)` is called
**When** invoked
**Then** the handler is removed from the condition handler chain for the current stack frame

**Complexity:** L

### Story LE101.2: Resume Cursor and Condition Information

As a **LE application developer**,
I want **CEEMRCR to move the resume cursor and CEE3CIB to access condition information**,
So that **handlers can control where execution resumes after a condition**.

**Acceptance Criteria:**

**Given** a condition handler calls `CEEMRCR(1, &fc)` (move one stack frame toward the caller)
**When** the handler resumes
**Then** execution resumes in the caller of the routine where the condition occurred

**Given** `CEE3CIB(&cib_address, &fc)` is called within a condition handler
**When** invoked
**Then** a pointer to the Condition Information Block is returned containing the condition token, machine state, and original instruction address

**Given** `CEE3GRN(&routine_name, &fc)` is called
**When** invoked
**Then** the name of the currently executing routine is returned

**Complexity:** L

### Story LE101.3: Exit Procedures

As a **LE application developer**,
I want **CEERTX/CEEUTX to register and unregister enclave termination exit procedures**,
So that **cleanup code runs when the enclave terminates normally or abnormally**.

**Acceptance Criteria:**

**Given** `CEERTX(&exit_routine, &token, &fc)` registers an exit procedure
**When** the enclave terminates (normal or abend)
**Then** the exit_routine is called with the termination reason before final cleanup

**Given** `CEEUTX(&exit_routine, &fc)` is called
**When** invoked
**Then** the previously registered exit routine is unregistered

**Complexity:** S

---

## Epic LE102: Storage Management Services

**Goal:** Implement LE heap allocation, deallocation, reallocation, and user-created heaps.

**Crate:** `open-mainframe-runtime`
**FRs:** FR-v4.0-LE102

### Story LE102.1: Heap Allocation Services

As a **LE application developer**,
I want **CEEGTST/CEEFRES/CEEFRST/CEECZST for heap memory management**,
So that **programs can dynamically allocate, free, and resize storage**.

**Acceptance Criteria:**

**Given** `CEEGTST(&heap_id, &size, &address, &fc)` with heap_id=0 (default heap)
**When** invoked
**Then** a block of the requested size is allocated from the default heap and the address is returned

**Given** `CEEFRST(&address, &fc)` with an address from CEEGTST
**When** invoked
**Then** the storage is freed back to the heap

**Given** `CEECZST(&address, &new_size, &fc)` with a previously allocated address
**When** invoked
**Then** the storage is reallocated to the new size; data is preserved up to the minimum of old and new sizes

**Complexity:** M

### Story LE102.2: User Heaps

As a **LE application developer**,
I want **CEECRHP/CEEDSHP to create and destroy user-defined heaps**,
So that **I can partition storage for different purposes within an enclave**.

**Acceptance Criteria:**

**Given** `CEECRHP(&heap_id, &initial_size, &increment, &options, &fc)`
**When** invoked
**Then** a new user heap is created and a unique heap_id is returned

**Given** `CEEGTST(&heap_id, &size, &address, &fc)` with the user heap_id
**When** invoked
**Then** storage is allocated from the user heap, not the default heap

**Given** `CEEDSHP(&heap_id, &fc)`
**When** invoked
**Then** the entire user heap is destroyed and all its storage is released at once

**Complexity:** M

---

## Epic LE103: Complete Date/Time Services

**Goal:** Extend existing CEEDAYS/CEEDATE/CEESECS with the full set of LE date/time callable services.

**Crate:** `open-mainframe-runtime`
**FRs:** FR-v4.0-LE103

### Story LE103.1: Date/Time Conversion Services

As a **LE application developer**,
I want **CEEDATM, CEESECI, CEEISEC, CEEDYWK, and CEECBLDY**,
So that **I can convert between Lilian dates, timestamps, integer components, and formatted strings**.

**Acceptance Criteria:**

**Given** `CEEDATM(&lilian_seconds, &picture, &output, &fc)` with picture 'YYYY-MM-DD HH:MI:SS'
**When** invoked
**Then** the Lilian seconds are converted to the formatted string (e.g., '2024-03-15 14:30:00')

**Given** `CEESECI(&lilian_seconds, &year, &month, &day, &hour, &min, &sec, &ms, &fc)`
**When** invoked
**Then** the Lilian seconds are decomposed into 7 integer components

**Given** `CEEISEC(&year, &month, &day, &hour, &min, &sec, &ms, &lilian_seconds, &fc)`
**When** invoked with year=2024, month=3, day=15, hour=14, min=30, sec=0, ms=0
**Then** the corresponding Lilian seconds value is computed

**Given** `CEEDYWK(&lilian_day, &day_of_week, &fc)`
**When** invoked
**Then** the day of week (1=Sunday through 7=Saturday) is returned

**Complexity:** M

### Story LE103.2: Current Time and GMT Services

As a **LE application developer**,
I want **CEEGMT, CEELOCT, CEEUTC, and CEEGMTO for current time queries**,
So that **programs can get the current time in various formats and zones**.

**Acceptance Criteria:**

**Given** `CEEGMT(&lilian_seconds, &gregorian, &fc)`
**When** invoked
**Then** the current GMT is returned as Lilian seconds and as a Gregorian string

**Given** `CEELOCT(&lilian_day, &lilian_seconds, &gregorian, &fc)`
**When** invoked
**Then** the current local date/time is returned in three formats

**Given** `CEEGMTO(&offset_hours, &offset_minutes, &offset_seconds, &fc)`
**When** invoked
**Then** the offset from GMT to local time is returned

**Complexity:** S

### Story LE103.3: Century Window and Delay

As a **LE application developer**,
I want **CEESCEN/CEEQCEN for century window control and CEE3DLY for time delays**,
So that **2-digit year interpretation is configurable and programs can suspend execution**.

**Acceptance Criteria:**

**Given** `CEESCEN(&century_window, &fc)` with century_window=80
**When** a 2-digit year 25 is subsequently used
**Then** it is interpreted as 2025 (within the 100-year window starting at 1980)

**Given** `CEEQCEN(&century_window, &fc)`
**When** invoked
**Then** the current century window value is returned

**Given** `CEE3DLY(&seconds, &fc)` with seconds=5
**When** invoked
**Then** the calling thread is suspended for approximately 5 seconds

**Complexity:** S

---

## Epic LE104: Math Services

**Goal:** Implement the full set of LE callable math services across three floating-point precisions.

**Crate:** `open-mainframe-runtime`
**FRs:** FR-v4.0-LE104

### Story LE104.1: Trigonometric and Logarithmic Functions

As a **LE application developer**,
I want **CEESxSIN/COS/TAN/ASIN/ACOS/ATAN/LOG/LOG10/EXP/SQRT in FLOAT4, FLOAT8, and FLOAT16**,
So that **COBOL and PL/I programs can call LE math services**.

**Acceptance Criteria:**

**Given** `CEESDSIN(&angle_rad, &result, &fc)` (double-precision sine)
**When** invoked with angle_rad = 1.5707963 (pi/2)
**Then** result is 1.0 (within floating-point precision)

**Given** `CEESSLOG(&value, &result, &fc)` (single-precision natural log)
**When** invoked with value = 2.718282
**Then** result is approximately 1.0

**Given** `CEESDEXP(&value, &result, &fc)` (double-precision exponential)
**When** invoked with value = 0.0
**Then** result is 1.0

**Complexity:** M

### Story LE104.2: Additional Math Functions and Random Number

As a **LE application developer**,
I want **power, modular, and random number functions**,
So that **programs have access to the complete LE math library**.

**Acceptance Criteria:**

**Given** `CEESDPOW(&base, &exponent, &result, &fc)` (double-precision power)
**When** invoked with base=2.0, exponent=10.0
**Then** result is 1024.0

**Given** `CEESDMOD(&dividend, &divisor, &result, &fc)` (double-precision modular)
**When** invoked with dividend=10.0, divisor=3.0
**Then** result is 1.0

**Given** `CEERAN0(&seed, &result, &fc)` (uniform random number)
**When** invoked
**Then** result is a pseudo-random number in [0.0, 1.0) and seed is updated

**Complexity:** M

---

## Epic LE105: Message Services

**Goal:** Implement LE message catalog access, condition token encoding/decoding, and message formatting.

**Crate:** `open-mainframe-runtime`
**FRs:** FR-v4.0-LE105

### Story LE105.1: Message Retrieval and Condition Tokens

As a **LE application developer**,
I want **CEEMSG/CEEMOUT/CEEMGET for messages and CEENCOD/CEEDCOD for condition tokens**,
So that **programs can retrieve LE error messages and encode/decode condition information**.

**Acceptance Criteria:**

**Given** `CEEMSG(&condition_token, &destination, &fc)` with a severity-2 condition token
**When** invoked
**Then** the message text is retrieved from the LE message catalog and written to the destination (MSGFILE)

**Given** `CEEMGET(&condition_token, &buffer, &msg_length, &fc)`
**When** invoked
**Then** the message text is placed in the caller's buffer with the actual length

**Given** `CEENCOD(&severity, &msg_number, &case_severity, &facility_id, &isi, &condition_token, &fc)`
**When** invoked
**Then** a 12-byte condition token is constructed from the components

**Given** `CEEDCOD(&condition_token, &severity, &msg_number, &case_severity, &facility_id, &isi, &fc)`
**When** invoked
**Then** the condition token is decomposed into its individual components

**Complexity:** S

---

## Epic LE106: Runtime Options Engine

**Goal:** Implement the LE runtime options parser with the merge/precedence chain and ~50+ options.

**Crate:** `open-mainframe-runtime`
**FRs:** FR-v4.0-LE106

### Story LE106.1: Options Parsing and Merge Chain

As a **system programmer**,
I want **LE runtime options parsed from IBM defaults, CEEPRMxx, region, CEEUOPT, and JCL PARM**,
So that **the option precedence chain correctly determines effective runtime behavior**.

**Acceptance Criteria:**

**Given** CEEPRMxx member specifies `HEAP(4M,1M,ANYWHERE,FREE)` and JCL PARM specifies `HEAP(8M)`
**When** the program is loaded
**Then** the effective HEAP initial size is 8M (JCL overrides parmlib) but increment remains 1M (from CEEPRMxx)

**Given** CEEPRMxx marks TRAP as NONOVR (non-overridable)
**When** JCL PARM attempts to change TRAP
**Then** the JCL override is ignored and the CEEPRMxx value is used

**Given** `CEE3PRM(&options_string, &fc)` is called
**When** invoked
**Then** the effective runtime options string is returned

**Complexity:** L

### Story LE106.2: Key Runtime Options Implementation

As a **system programmer**,
I want **HEAP, STACK, TRAP, ALL31, STORAGE, ERRCOUNT, ABEND, and POSIX options**,
So that **LE programs execute with proper memory, error handling, and execution mode settings**.

**Acceptance Criteria:**

**Given** `HEAP(4M,1M,ANYWHERE,FREE)` is the effective option
**When** a program allocates heap storage
**Then** the initial heap is 4MB, increments are 1MB, allocated anywhere (above/below the line), and free storage is returned to the OS

**Given** `TRAP(ON,SPIE)` is the effective option
**When** a program exception occurs (e.g., S0C7 data exception)
**Then** LE intercepts the exception and drives the condition handler chain

**Given** `ERRCOUNT(20)` is active and 20 severity-2+ conditions have occurred
**When** the 21st severity-2 condition is signaled
**Then** the enclave terminates with U4094 abend

**Given** `RPTOPTS(ON)` is active
**When** the program terminates
**Then** a report of all effective runtime options and their sources is written to MSGFILE

**Complexity:** L

---

## Epic LE107: Locale Services

**Goal:** Implement LE locale-sensitive services for date/time formatting, monetary values, and string comparison.

**Crate:** `open-mainframe-runtime`
**FRs:** FR-v4.0-LE107

### Story LE107.1: Locale Management and Formatting

As a **LE application developer**,
I want **CEESETL/CEEQRYL for locale control and CEEFMON/CEEFTDS for locale-aware formatting**,
So that **programs can format dates, times, and monetary values according to locale conventions**.

**Acceptance Criteria:**

**Given** `CEESETL('Fr_FR', &fc)` (set French locale)
**When** `CEEFTDS(&lilian_seconds, &picture, &output, &fc)` is called with a date
**Then** the date is formatted using French conventions (e.g., day names in French)

**Given** `CEEQRYL(&locale_name, &fc)`
**When** invoked
**Then** the currently active locale name is returned (e.g., 'Fr_FR')

**Given** `CEEFMON(&amount, &currency, &output, &fc)` with amount=1234.56
**When** the locale is Fr_FR
**Then** the output is formatted as '1 234,56 EUR' (French monetary conventions)

**Given** `CEESCOL(&string1, &string2, &result, &fc)`
**When** invoked under the current locale
**Then** the comparison result reflects locale-specific collation rules

**Complexity:** S

---

## Epic LE108: Interlanguage Communication (ILC)

**Goal:** Implement cross-language calling conventions between COBOL, PL/I, C, and Assembler under LE.

**Crate:** `open-mainframe-runtime`
**FRs:** FR-v4.0-LE108

### Story LE108.1: Cross-Language Call Conventions

As a **LE application developer**,
I want **COBOL-to-PL/I, COBOL-to-C, and PL/I-to-C calling with proper parameter passing**,
So that **mixed-language applications work correctly under LE**.

**Acceptance Criteria:**

**Given** a COBOL program calls a PL/I subroutine with `CALL 'PLISUB' USING BY REFERENCE WS-FIELD`
**When** the call is made
**Then** the PL/I routine receives the parameter by reference with correct descriptor mapping

**Given** a C function calls a COBOL program with parameters
**When** the call is made
**Then** the COBOL program receives the parameters in its LINKAGE SECTION with proper BY REFERENCE semantics

**Given** a COBOL program passes a `PIC X(20)` field to a C function expecting `char*`
**When** the call is made
**Then** the C function receives a pointer to the 20-byte field (not null-terminated unless explicitly done)

**Complexity:** L

### Story LE108.2: Condition Propagation Across Languages

As a **LE application developer**,
I want **conditions to propagate correctly across language boundaries**,
So that **a PL/I ON condition raised in a COBOL-called routine is handled properly**.

**Acceptance Criteria:**

**Given** COBOL calls PL/I which signals a ZERODIVIDE condition
**When** the PL/I ON ZERODIVIDE handler is not established
**Then** the condition percolates to COBOL as a SIZE ERROR condition

**Given** a C program raises SIGFPE while called from a COBOL program
**When** LE intercepts the signal
**Then** it is mapped to an LE condition and the COBOL program's condition handler chain is walked

**Given** heap storage allocated in a PL/I routine within a shared enclave
**When** a COBOL routine in the same enclave accesses the heap
**Then** the storage is accessible (shared within the enclave)

**Complexity:** L

---

## Epic LE109: Diagnostics & Debugging

**Goal:** Implement CEEDUMP, traceback formatting, and LE abend code handling.

**Crate:** `open-mainframe-runtime`
**FRs:** FR-v4.0-LE109

### Story LE109.1: CEEDUMP and Traceback

As a **LE application developer**,
I want **CEE3DMP to generate formatted LE dumps with traceback and storage**,
So that **I can diagnose runtime failures**.

**Acceptance Criteria:**

**Given** `CEE3DMP(&title, &options, &fc)` with options=TRACEBACK|BLOCKS|STORAGE
**When** invoked
**Then** a formatted dump is written to CEEDUMP DD containing: traceback (call chain with routine names, entry addresses, offsets), dynamic storage areas (stack frames), and heap storage

**Given** an unhandled severity-3 condition
**When** the enclave terminates
**Then** a CEEDUMP is automatically generated (if TRAP(ON) and CEEDUMP DD is allocated)

**Complexity:** M

### Story LE109.2: LE Abend Codes

As a **LE application developer**,
I want **LE abend codes (U4036, U4038, U4039, U4093, U4094) to have structured diagnostics**,
So that **I can quickly identify the cause of LE-related abends**.

**Acceptance Criteria:**

**Given** an unhandled condition reaches severity 3
**When** the enclave terminates
**Then** abend U4038 (unhandled condition of severity 2-4) is issued with the condition token in the dump

**Given** ERRCOUNT(20) is exceeded
**When** the limit is reached
**Then** abend U4094 is issued with a message indicating the error count was exceeded

**Given** CEETEST is invoked (or ABEND runtime option triggers)
**When** processed
**Then** the appropriate abend code is issued and the CEEDUMP includes the LE diagnostic information

**Complexity:** M

---

## Epic LE110: Bit Manipulation & Utility Services

**Goal:** Implement LE bit manipulation and miscellaneous utility callable services.

**Crate:** `open-mainframe-runtime`
**FRs:** FR-v4.0-LE110

### Story LE110.1: Bit Manipulation Services

As a **LE application developer**,
I want **CEESICLR/CEESISET/CEESISHF/CEESITST for bit-level operations**,
So that **COBOL programs can perform bit manipulation via LE callable services**.

**Acceptance Criteria:**

**Given** `CEESISET(&target, &bit_offset, &count, &fc)` with target=X'00', bit_offset=0, count=4
**When** invoked
**Then** target becomes X'F0' (first 4 bits set to 1)

**Given** `CEESITST(&target, &bit_offset, &result, &fc)` with target=X'F0', bit_offset=0
**When** invoked
**Then** result=1 (bit is set)

**Given** `CEESISHF(&target, &shift_count, &result, &fc)` with shift_count=4 (left shift)
**When** invoked
**Then** the target value is shifted left by 4 bit positions

**Given** `CEESICLR(&target, &bit_offset, &count, &fc)` with target=X'FF', bit_offset=0, count=4
**When** invoked
**Then** target becomes X'0F' (first 4 bits cleared)

**Complexity:** S

### Story LE110.2: Utility Services

As a **LE application developer**,
I want **CEEGTJS for JES job information and CEE3USR for user service interface**,
So that **programs can query batch job context and register user services**.

**Acceptance Criteria:**

**Given** `CEEGTJS(&job_name, &job_id, &step_name, &proc_step, &fc)` called in a batch job
**When** invoked
**Then** the JES job name, job number, step name, and procedure step name are returned

**Given** `CEE3USR(&user_word, &fc)` to set the user word
**When** invoked
**Then** a 4-byte user word is stored in the enclave's LE control block for application use

**Complexity:** S

---
