# Gap Analysis: Language Environment (LE)

## Official Specification Summary

Language Environment (LE) is IBM's common runtime infrastructure for z/OS high-level languages (HLLs), providing a unified set of services shared by COBOL, PL/I, C/C++, FORTRAN, and assembler programs. Introduced in the early 1990s (replacing separate language-specific runtimes), LE standardizes condition handling, storage management, date/time services, math services, and interlanguage communication across all z/OS compilers.

LE is classified as **Core** (required infrastructure) on mainframes:
- Every COBOL, PL/I, C/C++, and FORTRAN program on z/OS runs within LE
- Provides a three-level execution model: Process → Enclave → Thread
- ~70+ callable services (CEExxxx prefix) for condition handling, storage, math, date/time, messages, locale
- Condition handling model based on stack frames with user-registered handlers
- Storage management with user-defined heaps and stack allocation
- ~50+ runtime options (HEAP, STACK, TRAP, ABTERMENC, ALL31, XPLINK, etc.) control behavior
- Interlanguage Communication (ILC) enables mixed-language applications (COBOL↔PL/I↔C↔assembler)
- XPLINK (Extra Performance Linkage) for high-performance C/C++ calling conventions
- CEEPRMxx parmlib member for system-wide defaults
- LE-conforming programs share a common runtime; LE-nonconforming programs use compatibility interfaces

Key documentation:
- **z/OS Language Environment Concepts Guide (SA38-0687)** — architecture and models
- **z/OS Language Environment Programming Reference (SA38-0683)** — callable services and runtime options
- **z/OS Language Environment Programming Guide (SA38-0682)** — application development
- **z/OS Language Environment Writing ILC Applications (SA22-7563)** — mixed-language programming
- **z/OS Language Environment Debugging Guide (GA32-0908)** — diagnostics and debugging
- **z/OS Language Environment Runtime Messages (SA38-0686)** — CEE message catalog

## Key Features & Capabilities

### 1. Program Management Model

| Concept | Description |
|---------|-------------|
| Process | Highest level — collection of enclaves; independent address space |
| Enclave | Unit of work — main routine + subroutines; defines scope of HLL semantics |
| Thread | Execution path within enclave; has own stack and instruction counter |
| Routine | Individual program/function/procedure within a thread |

Language equivalents:
| LE Term | COBOL | PL/I | C/C++ | FORTRAN |
|---------|-------|------|-------|---------|
| Enclave | Run unit | Main procedure + subs | main() + functions | PROGRAM + subs |
| Routine | Program | Procedure/BEGIN block | Function | Subroutine |

### 2. Condition Handling Services

| Service | Purpose |
|---------|---------|
| CEEHDLR | Register a user-written condition handler |
| CEEHDLU | Unregister a user-written condition handler |
| CEESGL | Signal a condition (raise) |
| CEEMRCR | Move resume cursor (control where execution resumes) |
| CEE3CIB | Return pointer to Condition Information Block |
| CEE3GRN | Get name of routine that incurred the condition |
| CEERTX | Register user exit procedure (termination cleanup) |
| CEEUTX | Unregister user exit procedure |

Condition token structure:
| Field | Description |
|-------|-------------|
| Condition_ID | Unique identifier |
| Severity | 0=informational, 1=warning, 2=error, 3=severe, 4=critical |
| Msg_No | Message number (e.g., 2501, 2502) |
| Facility_ID | Originating facility (CEE, IGZ, IBM, etc.) |
| I_S_Info | Instance-specific information |

Condition handling flow:
1. Condition occurs (hardware interrupt, CEESGL, language-specific)
2. LE walks stack frames from most recent to oldest
3. At each frame: user condition handlers → HLL-specific handlers
4. If unhandled after full stack walk: LE default action or language default

### 3. Storage Management Services

| Service | Purpose |
|---------|---------|
| CEEGTST | Get heap storage (allocate from heap) |
| CEEFRES / CEEFRST | Free heap storage (deallocate) |
| CEECZST | Reallocate heap storage (resize) |
| CEECRHP | Create a new additional heap (user heap) |
| CEEDSHP | Destroy a user heap |

Storage types:
| Type | Description |
|------|-------------|
| Initial heap | Default heap for COBOL WORKING-STORAGE, PL/I CONTROLLED/BASED |
| Additional heaps | User-created heaps via CEECRHP for isolation |
| Stack | Automatic variables, linkage; grows upward (standard) or downward (XPLINK) |

### 4. Date/Time Services

| Service | Purpose |
|---------|---------|
| CEEDAYS | Convert date string to Lilian day number |
| CEEDATE | Convert Lilian day number to formatted date string |
| CEESECS | Convert date/time string to Lilian seconds (float) |
| CEEDATM | Convert Lilian seconds to formatted date/time string |
| CEESECI | Convert Lilian seconds to 7 integer components (Y/M/D/H/M/S/ms) |
| CEEISEC | Convert 7 integer components to Lilian seconds |
| CEEDYWK | Get day of week from Lilian day (1=Sunday..7=Saturday) |
| CEEGMT | Get current Greenwich Mean Time as Lilian seconds |
| CEELOCT | Get current local date/time as Lilian seconds |
| CEEUTC | Get Coordinated Universal Time as Lilian seconds |
| CEEGMTO | Get offset from GMT to local time |
| CEESCEN | Set century window for 2-digit year handling |
| CEEQCEN | Query current century window setting |
| CEECBLDY | Convert date for COBOL compatibility |
| CEE3DLY | Delay (suspend) processing for specified seconds |
| CEEDLYM | Delay (suspend) processing for specified milliseconds |

Lilian calendar: Day 1 = October 15, 1582 (Gregorian calendar start). Range: 1582–9999.

### 5. Math Services (~30+ functions)

| Service Pattern | Purpose |
|-----------------|---------|
| CEESxABS | Absolute value |
| CEESxACS | Arccosine |
| CEESxASN | Arcsine |
| CEESxATN | Arctangent |
| CEESxAT2 | Arctangent of y/x (atan2) |
| CEESxATH | Hyperbolic arctangent |
| CEESxCOS | Cosine |
| CEESxCSH | Hyperbolic cosine |
| CEESxCTN | Cotangent |
| CEESxEXP | Exponential (e^x) |
| CEESxLG1 | Log base 10 |
| CEESxLOG | Natural logarithm (ln) |
| CEESxMOD | Modular arithmetic |
| CEESxNIN | Nearest integer |
| CEESxNWN | Nearest whole number |
| CEESxSGN | Signum function |
| CEESxSIN | Sine |
| CEESxSNH | Hyperbolic sine |
| CEESxSQT | Square root |
| CEESxTAN | Tangent |
| CEESxTNH | Hyperbolic tangent |
| CEESxXPx | Exponentiation (x^y) |
| CEERAN0 | Uniform random number generator |

The `x` placeholder represents data type: `D`=FLOAT(16), `S`=FLOAT(8), `Q`=FLOAT(4).

### 6. Message Services

| Service | Purpose |
|---------|---------|
| CEEMSG | Get and dispatch a runtime message |
| CEEMOUT | Format and dispatch a message to output |
| CEEMGET | Get a message text into a buffer |
| CEENCOD | Encode a condition token from components |
| CEEDCOD | Decode a condition token into components |
| CEECMI | Create a message insert for parameterized messages |

### 7. General/Utility Services

| Service | Purpose |
|---------|---------|
| CEE3DMP | Generate a formatted LE dump (CEEDUMP) |
| CEE3ABD | Terminate enclave with an ABEND |
| CEE3AB2 | Terminate enclave with ABEND and reason code |
| CEE3INF | Query system information (LE level, CICS, etc.) |
| CEE3GRC | Get enclave return code |
| CEE3SRC | Set enclave return code |
| CEE3PRM | Query runtime parameters |
| CEE3PR2 | Query runtime parameters (extended) |
| CEE3USR | User-defined service |
| CEEGPID | Get LE product ID and version |
| CEEGTJS | Get JES job step information |
| CEETEST | Invoke debug tool (e.g., Debug Tool for z/OS) |

### 8. Locale Services

| Service | Purpose |
|---------|---------|
| CEESETL | Set locale |
| CEEQRYL | Query current locale |
| CEEFMON | Format monetary value per locale |
| CEEFTDS | Format date/time per locale conventions |
| CEELCNV | Get locale conventions structure |
| CEESCOL | Compare strings per locale collation |
| CEEQDTC | Query date/time conventions for locale |

### 9. Bit Manipulation Services

| Service | Purpose |
|---------|---------|
| CEESICLR | Clear bit(s) |
| CEESISET | Set bit(s) |
| CEESISHF | Shift bits left/right |
| CEESITST | Test bit |

### 10. Runtime Options (~50+)

#### Storage Options
| Option | Purpose |
|--------|---------|
| HEAP(init,incr,loc,free,initc,incrc) | Heap allocation parameters |
| STACK(init,incr,loc,free) | Stack allocation parameters |
| LIBSTACK(init,incr) | Library stack allocation |
| STORAGE(init,free,dsa,heap) | Storage initialization/free values |
| ANYHEAP(init,incr,loc) | Anywhere-in-storage heap |
| BELOWHEAP(init,incr) | Below-the-line heap |

#### Condition/Error Handling
| Option | Purpose |
|--------|---------|
| TRAP(ON/OFF) | Whether LE intercepts program interrupts |
| ABTERMENC(ABEND/RETCODE) | Enclave termination behavior for unhandled conditions |
| ABPERC(code) | Percolate specific ABEND codes |
| ERRCOUNT(n) | Maximum errors before termination |
| TERMTHDACT(action) | Action on thread termination |

#### Execution Control
| Option | Purpose |
|--------|---------|
| ALL31(ON/OFF) | All programs AMODE 31 |
| XPLINK(ON/OFF) | Enable Extra Performance Linkage |
| POSIX(ON/OFF) | Enable POSIX semantics (threads, signals) |
| CEEDUMP(option) | Control dump generation |
| TEST(ON/OFF,params) | Debug tool activation |

#### Reporting
| Option | Purpose |
|--------|---------|
| RPTOPTS(ON/OFF) | Report runtime options in effect |
| RPTSTG(ON/OFF) | Report storage usage at termination |
| MSGFILE(ddname) | DD name for LE messages |

#### Configuration
| Option | Purpose |
|--------|---------|
| NONOVR | Mark option as non-overridable |
| USRHDLR(addr) | Default user condition handler |
| ENVAR("name=value") | Set environment variables |

Options merge precedence: IBM defaults → CEEPRMxx parmlib → region defaults → program link-edit (CEEUOPT) → JCL PARM= or EXEC PARM= → application CEEOPTS

### 11. Interlanguage Communication (ILC)

| Feature | Description |
|---------|-------------|
| COBOL ↔ C/C++ | COBOL CALL to C functions; C calling COBOL via function pointers |
| COBOL ↔ PL/I | Mixed run units with shared LE enclave |
| COBOL ↔ Assembler | CALL to assembler subroutines; assembler calling COBOL |
| PL/I ↔ C/C++ | Shared enclave, condition handling interop |
| PL/I ↔ Assembler | PL/I CALL to assembler; assembler FETCH of PL/I |
| Parameter passing | BY REFERENCE, BY CONTENT, BY VALUE conventions |
| String descriptors | COBOL descriptors, PL/I varying strings, C null-terminated |
| Heap sharing | Heap storage shared across languages within enclave |
| Condition propagation | Conditions flow across language boundaries via LE framework |

### 12. LE Abend Codes

| Code Range | Description |
|------------|-------------|
| U4038-U4039 | Heap storage exhaustion |
| U4093 | Unhandled condition of severity 3+ |
| U4094 | LE initialization failure |
| U4036 | ABEND issued by CEE3ABD |
| CEE0nnn | LE system messages |
| IGZ0nnn | COBOL runtime messages |
| IBM0nnn | PL/I runtime messages |
| EDC0nnn | C/C++ runtime messages |

## Current OpenMainframe Status

### Partial Implementations Found

1. **CEEDAYS — Convert date to Lilian** — `crates/open-mainframe-runtime/src/date_time.rs`
   - Fully implemented: `pub fn ceedays(date_str, picture) -> (i64, FeedbackCode)`
   - Supports YYYYMMDD, YYYYDDD, MMDDYYYY, DDMMYYYY formats
   - Returns Lilian day number with feedback code

2. **CEEDATE — Convert Lilian to date** — `crates/open-mainframe-runtime/src/date_time.rs`
   - Fully implemented: `pub fn ceedate(lilian, picture) -> (String, FeedbackCode)`
   - Reverse of CEEDAYS with same format support

3. **CEESECS — Convert date/time to seconds** — `crates/open-mainframe-runtime/src/date_time.rs`
   - Fully implemented: `pub fn ceesecs(date_time_str, picture) -> (f64, FeedbackCode)`
   - YYYYMMDDHHMMSS format support

4. **FeedbackCode structure** — `crates/open-mainframe-runtime/src/date_time.rs`
   - Implemented: `pub struct FeedbackCode { condition_id, severity, msg_no }`
   - Used by all three callable services above

5. **Lilian calendar infrastructure** — `crates/open-mainframe-runtime/src/date_time.rs`
   - `is_leap_year()`, `date_to_lilian()`, `lilian_to_date()`
   - Full Gregorian accuracy from 1582–9999
   - COBOL intrinsic functions: `integer_of_date()`, `date_of_integer()`, `integer_of_day()`, `day_of_integer()`

6. **CICS condition handling** — `crates/open-mainframe-cics/src/runtime/mod.rs`
   - `ConditionHandler` struct with `condition` and `label` fields
   - `TransactionContext.handle_condition()` and `handle_abend()`
   - EXEC CICS HANDLE ABEND / HANDLE CONDITION support
   - Not LE CEEHDLR — CICS-specific condition handling

7. **ABEND error type** — `crates/open-mainframe-runtime/src/error.rs`
   - `RuntimeError::Abend { code, message }` variant
   - `RuntimeError::DataException` (S0C7 equivalent)

8. **External CALL infrastructure** — `crates/open-mainframe-runtime/src/interpreter.rs`
   - `ProgramRegistry` trait for dynamic program lookup
   - `CallParam` enum (ByReference/ByContent/ByValue)
   - Foundation for calling LE services and ILC

### Existing Planning
- **Epic 508** in `epics-runtime-v3.0.md`: Date/Time and LE Callable Services (Stories 508.1-508.2 — IMPLEMENTED)
- **FR-v3.0-509** in `prd-runtime-v3.0.md`: Language Environment Callable Services (MINOR priority — PLANNED)
- **AD-3.0-06** in `architecture-runtime-v3.0.md`: Calendar-Accurate Date Library (IMPLEMENTED)

## Gap Details

| # | Feature | Official z/OS | OpenMainframe | Gap |
|---|---------|--------------|---------------|-----|
| 1 | Program model (Process/Enclave/Thread) | Full 3-level hierarchy | None — single-threaded execution | **Missing** |
| 2 | CEEDAYS (date to Lilian) | Full implementation | Implemented in date_time.rs | **Present** |
| 3 | CEEDATE (Lilian to date) | Full implementation | Implemented in date_time.rs | **Present** |
| 4 | CEESECS (date/time to seconds) | Full implementation | Implemented in date_time.rs | **Present** |
| 5 | FeedbackCode / condition token | Full 12-byte token | Simplified struct (3 fields) | **Partial** |
| 6 | CEEDATM (seconds to date/time string) | Full implementation | Not implemented | **Missing** |
| 7 | CEESECI/CEEISEC (seconds ↔ integers) | Full implementation | Not implemented | **Missing** |
| 8 | CEEDYWK (day of week) | Full implementation | Not implemented | **Missing** |
| 9 | CEEGMT/CEEUTC/CEELOCT (current time) | System clock access | Not implemented | **Missing** |
| 10 | CEEGMTO (GMT offset) | Time zone offset | Not implemented | **Missing** |
| 11 | CEESCEN/CEEQCEN (century window) | 2-digit year handling | Not implemented | **Missing** |
| 12 | CEEHDLR/CEEHDLU (condition handlers) | Stack-frame-based handler chain | CICS-only (not LE general) | **Partial** |
| 13 | CEESGL (signal condition) | Raise condition from application | Not implemented | **Missing** |
| 14 | CEEMRCR (move resume cursor) | Control post-condition resume | Not implemented | **Missing** |
| 15 | CEE3CIB (condition info block) | Access condition details | Not implemented | **Missing** |
| 16 | CEEGTST (get heap storage) | Heap allocation | Not implemented | **Missing** |
| 17 | CEEFRES/CEEFRST (free storage) | Heap deallocation | Not implemented | **Missing** |
| 18 | CEECZST (reallocate storage) | Heap resize | Not implemented | **Missing** |
| 19 | CEECRHP/CEEDSHP (create/destroy heap) | User heap management | Not implemented | **Missing** |
| 20 | Math services (~30+ CEESx functions) | Trig, log, exp, sqrt, etc. | Not implemented | **Missing** |
| 21 | CEERAN0 (random numbers) | Uniform random generator | Not implemented | **Missing** |
| 22 | Message services (CEEMSG/CEEMOUT) | Message dispatch/formatting | Not implemented | **Missing** |
| 23 | CEENCOD/CEEDCOD (encode/decode token) | Condition token manipulation | Not implemented | **Missing** |
| 24 | CEE3DMP (LE dump) | Formatted diagnostic dump | Not implemented | **Missing** |
| 25 | CEE3ABD/CEE3AB2 (terminate enclave) | Controlled ABEND | RuntimeError::Abend exists (partial) | **Partial** |
| 26 | CEE3INF (query system info) | LE level, environment queries | Not implemented | **Missing** |
| 27 | CEE3GRC/CEE3SRC (get/set return code) | Enclave return code | Not implemented | **Missing** |
| 28 | Locale services (CEESETL, etc.) | 6+ locale services | Not implemented | **Missing** |
| 29 | Bit manipulation (CEESI*) | 4 bit services | Not implemented | **Missing** |
| 30 | Runtime options (~50+) | HEAP/STACK/TRAP/ALL31/XPLINK | None | **Missing** |
| 31 | CEEPRMxx parmlib processing | System-wide LE defaults | None | **Missing** |
| 32 | Options merge/precedence | IBM→parmlib→region→pgm→JCL | None | **Missing** |
| 33 | RPTOPTS/RPTSTG reporting | Options and storage reports | None | **Missing** |
| 34 | ILC (interlanguage communication) | COBOL↔PL/I↔C↔Assembler | External CALL exists (partial) | **Partial** |
| 35 | XPLINK (Extra Performance Linkage) | Downward-growing stack, registers | None | **Missing** |
| 36 | POSIX thread support | pthreads within LE enclave | None | **Missing** |
| 37 | Preinitialization interface | Persistent LE environment | None | **Missing** |
| 38 | LE abend codes (U40xx, CEE0nnn) | Structured error messages | RuntimeError::Abend (basic) | **Partial** |
| 39 | CEEDUMP trace and diagnostics | Traceback, storage dump | None | **Missing** |
| 40 | LE-conforming/nonconforming model | Compatibility interfaces | None | **Missing** |
| 41 | CEERTX/CEEUTX (exit procedures) | Termination cleanup hooks | None | **Missing** |
| 42 | CEETEST (debug tool invocation) | Interactive debugging | None | **Missing** |

**Total: 42 features analyzed — 3 present, 5 partial, 34 missing**

## Proposed Epic Structure

### LE100 — LE Program Model & Enclave Management (L)
- Process/Enclave/Thread hierarchy
- Enclave creation, initialization, termination
- Thread creation within enclaves
- Enclave return code management (CEE3GRC/CEE3SRC)
- CEE3ABD/CEE3AB2 (terminate with ABEND)
- CEE3INF (query system information)
- CEE3PRM/CEE3PR2 (query runtime parameters)
- CEEGPID (get product ID)
- LE-conforming vs LE-nonconforming program distinction
- **Depends on**: open-mainframe-runtime

### LE101 — Condition Handling Framework (L)
- Stack-frame-based condition handler chain
- CEEHDLR (register handler) / CEEHDLU (unregister)
- CEESGL (signal condition)
- CEEMRCR (move resume cursor)
- CEE3CIB (condition information block access)
- CEE3GRN (get routine name)
- CEERTX/CEEUTX (exit procedure registration)
- Full condition token structure (12-byte ISI)
- Condition severity levels (0-4) and propagation
- Integration with existing CICS condition handler
- HLL-specific condition handling (COBOL ON SIZE ERROR, PL/I ON conditions)
- **Depends on**: LE100

### LE102 — Storage Management Services (M)
- CEEGTST (get heap storage)
- CEEFRES/CEEFRST (free heap storage)
- CEECZST (reallocate heap storage)
- CEECRHP (create additional heap)
- CEEDSHP (destroy user heap)
- Heap tracking and statistics
- Integration with COBOL WORKING-STORAGE and PL/I CONTROLLED/BASED
- **Depends on**: LE100

### LE103 — Complete Date/Time Services (M)
- CEEDATM (seconds to formatted date/time) — extends existing CEESECS
- CEESECI (seconds to 7 integer components)
- CEEISEC (7 integer components to seconds)
- CEEDYWK (day of week from Lilian)
- CEEGMT (current GMT as Lilian seconds)
- CEELOCT (current local time)
- CEEUTC (current UTC)
- CEEGMTO (GMT to local offset)
- CEESCEN/CEEQCEN (century window for 2-digit years)
- CEECBLDY (COBOL date compatibility)
- CEE3DLY/CEEDLYM (delay/suspend processing)
- Enhance FeedbackCode to full 12-byte condition token
- **Depends on**: Existing date_time.rs (CEEDAYS, CEEDATE, CEESECS)

### LE104 — Math Services (M)
- All ~30+ CEESx math functions (trig, log, exp, sqrt, etc.)
- Three precision variants: FLOAT(4), FLOAT(8), FLOAT(16)
- CEERAN0 (uniform random number generator)
- Integration with COBOL FUNCTION intrinsics
- **Depends on**: LE100, open-mainframe-runtime

### LE105 — Message Services (S)
- CEEMSG (get and dispatch message)
- CEEMOUT (format and dispatch message)
- CEEMGET (get message text into buffer)
- CEENCOD (encode condition token)
- CEEDCOD (decode condition token)
- CEECMI (create message insert)
- LE message catalog (CEE0nnn, IGZ0nnn, IBM0nnn, EDC0nnn)
- **Depends on**: LE100, LE101

### LE106 — Runtime Options Engine (L)
- Parse and merge runtime options (~50+ options)
- Option precedence chain: IBM defaults → CEEPRMxx → region → CEEUOPT → JCL PARM
- HEAP/STACK/STORAGE allocation options
- TRAP/ABTERMENC/ERRCOUNT condition handling options
- ALL31/XPLINK execution control
- RPTOPTS/RPTSTG reporting
- MSGFILE/CEEDUMP output control
- TEST/DEBUG tool options
- NONOVR (non-overridable marking)
- Dynamic option modification during execution
- **Depends on**: LE100

### LE107 — Locale Services (S)
- CEESETL (set locale)
- CEEQRYL (query locale)
- CEEFMON (format monetary value)
- CEEFTDS (format date/time per locale)
- CEELCNV (get locale conventions)
- CEESCOL (locale-aware string comparison)
- CEEQDTC (query date/time conventions)
- **Depends on**: LE100, open-mainframe-encoding

### LE108 — Interlanguage Communication (L)
- COBOL ↔ C/C++ calling conventions
- COBOL ↔ PL/I calling conventions
- COBOL ↔ Assembler calling conventions
- PL/I ↔ C and PL/I ↔ Assembler
- Parameter passing: BY REFERENCE, BY CONTENT, BY VALUE
- String descriptor interoperability
- Heap sharing across languages within enclave
- Condition propagation across language boundaries
- Extend existing ProgramRegistry for multi-language dispatch
- **Depends on**: LE100, LE101, open-mainframe-cobol

### LE109 — Diagnostics & Debugging (M)
- CEE3DMP (generate formatted LE dump — CEEDUMP)
- Traceback formatting (stack frame walk with routine names)
- Storage dump (heap and stack contents)
- LE abend codes (U4036, U4038, U4039, U4093, U4094)
- CEETEST (invoke debug tool)
- RPTOPTS report generation
- RPTSTG storage usage report
- Integration with existing RuntimeError types
- **Depends on**: LE100, LE101

### LE110 — Bit Manipulation & Utilities (S)
- CEESICLR (clear bits)
- CEESISET (set bits)
- CEESISHF (shift bits)
- CEESITST (test bits)
- CEEGTJS (get JES job step information)
- CEE3USR (user-defined service interface)
- CEEUSGD (usage data collection)
- **Depends on**: LE100

## Dependencies

### Existing Crate Dependencies
| Crate | Relationship |
|-------|-------------|
| open-mainframe-runtime | Contains existing LE date/time services, error types, interpreter |
| open-mainframe-cobol | COBOL programs use LE services (CALL 'CEExxxx'); condition handling for ON SIZE ERROR etc. |
| open-mainframe-cics | CICS condition handling extends to LE condition model |
| open-mainframe-encoding | EBCDIC/CCSID for locale services, string conversion |

### Cross-Batch Dependencies
| Batch | Dependency |
|-------|------------|
| Batch 1 (REXX) | REXX can call LE services via ADDRESS LINKPGM |
| Batch 3 (PL/I) | PL/I programs are primary LE consumers; ILC with COBOL/C |
| Batch 8 (RACF) | PROGRAM class for authorized LE modules |
| Batch 9 (TSO/ISPF) | TSO CALL command invokes LE programs |
| Batch 11 (JES2) | Batch jobs run LE programs; CEEDUMP to SYSOUT |
| Batch 14 (SMF) | SMF Type 89 for LE usage data (CEEUSGD) |

### Crate Recommendation
- LE services should be added to **open-mainframe-runtime** (extends existing date_time.rs)
- Consider new sub-modules: `le/condition`, `le/storage`, `le/math`, `le/message`, `le/locale`, `le/options`

## Complexity Estimate

| Epic | Size | Rationale |
|------|------|-----------|
| LE100 — Program Model & Enclave | L | Foundational execution model; process/enclave/thread hierarchy |
| LE101 — Condition Handling | L | Stack-frame handler chain, resume cursor, condition propagation |
| LE102 — Storage Management | M | Heap allocation/free/resize with user heaps |
| LE103 — Complete Date/Time | M | Extends existing 3 services to full ~16 services |
| LE104 — Math Services | M | ~30 functions × 3 precision variants; mostly wrapper functions |
| LE105 — Message Services | S | 6 services for message catalog access and formatting |
| LE106 — Runtime Options | L | ~50+ options with merge precedence chain and CEEPRMxx parsing |
| LE107 — Locale Services | S | 7 locale services; depends on encoding crate |
| LE108 — ILC | L | Multi-language calling conventions and condition propagation |
| LE109 — Diagnostics & Debugging | M | CEEDUMP, traceback, storage dump, abend codes |
| LE110 — Bit Manipulation & Utilities | S | Simple utility services |

**Overall complexity: L** — Language Environment is foundational infrastructure but many services are relatively straightforward (math wrappers, date/time extensions). The complex parts are condition handling (LE101), runtime options merging (LE106), and ILC (LE108). Existing date/time services provide a strong starting point.

## Reference Documentation

- [z/OS Language Environment Concepts Guide (SA38-0687) — z/OS 3.1](https://www.ibm.com/docs/en/SSLTBW_3.1.0/pdf/ceea800_v3r1.pdf)
- [z/OS Language Environment Programming Reference (SA38-0683) — z/OS 3.1](https://www.ibm.com/docs/en/SSLTBW_3.1.0/pdf/ceea300_v3r1.pdf)
- [z/OS Language Environment Programming Guide (SA38-0682) — z/OS 2.5](https://www.ibm.com/docs/en/SSLTBW_2.5.0/pdf/ceea200_v2r5.pdf)
- [z/OS Language Environment Debugging Guide (GA32-0908) — z/OS 3.1](https://www.ibm.com/docs/en/SSLTBW_3.1.0/pdf/ceea100_v3r1.pdf)
- [z/OS Language Environment Runtime Messages (SA38-0686) — z/OS 2.5](https://www.ibm.com/docs/en/SSLTBW_2.5.0/pdf/ceea900_v2r5.pdf)
- [z/OS Language Environment Callable Services (IBM Docs z/OS 2.1)](https://www.ibm.com/docs/en/zos/2.1.0?topic=reference-language-environment-callable-services)
- [z/OS Language Environment Concepts Guide (SA38-0687) — z/OS 2.5](https://www.ibm.com/docs/en/SSLTBW_2.5.0/pdf/ceea800_v2r5.pdf)
- [z/OS Language Environment Writing ILC Applications (SA22-7563)](https://www-01.ibm.com/support/docview.wss?uid=pub1sa22756308)
- [z/OS Language Environment Vendor Interfaces (SA38-0688)](https://www.ibm.com/docs/en/SSLTBW_2.4.0/pdf/ceev100_v2r4.pdf)
- [Language Environment for Dummies — SHARE Conference](https://share.confex.com/share/115/webprogram/Handout/Session7481/LEDUMtpa.pdf)
- [Enterprise COBOL: Using LE Callable Services](https://www.ibm.com/docs/en/cobol-zos/6.1.0?topic=coding-using-language-environment-callable-services)
- [C/C++ Examples Using CEEHDLR, CEEGTST, CEECZST (z/OS 3.1)](https://www.ibm.com/docs/en/zos/3.1.0?topic=pli-cc-examples-using-ceehdlr-ceegtst-ceeczst-ceemrcr)
