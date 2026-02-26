# open-mainframe-runtime

A comprehensive Rust implementation of the **Mainframe Language Environment (LE)** for the OpenMainframe project. This crate provides the foundational services required for the execution of COBOL, PL/I, and C programs, including decimal arithmetic, string manipulation, condition management, and inter-language communication.

## Overview

The Language Environment (LE) is the "standard library" of the mainframe. `open-mainframe-runtime` emulates this environment to ensure that mainframe applications behave identically when running under OpenMainframe. It handles the low-level data formats (Packed/Zoned decimal), the complex semantics of COBOL verbs (`INSPECT`, `UNSTRING`), and the CEExxx service routines that programmers rely on for date/time and math.

## Architecture

```
    Compiled / Interpreted Program        Language Environment (LE)
    ┌────────────────────────┐            ┌────────────────────────┐
    │  Business Logic        │ ── Calls ──►    LE Math Services    │
    │  (COBOL / PL/I)        │            │    (CEESxxx)           │
    └────────────────────────┘            └────────────────────────┘
                                                       │
    ┌────────────────────────┐            ┌────────────────────────┐
    │  Inter-Language        │ ── Logic ──►    ILC Manager         │
    │  Dispatch (ILC)        │            │    (COBOL <-> PL/I)    │
    └────────────────────────┘            └────────────────────────┘
                                                       │
    ┌────────────────────────┐            ┌────────────────────────┐
    │  Error Handling        │ ── Signal ─►    Condition Manager   │
    │  (ON-Units, Handler)   │            │    (CEEDCOD, CEENCOD)  │
    └────────────────────────┘            └────────────────────────┘
                                                       │
                                                       ▼
    ┌────────────────────────┐            ┌────────────────────────┐
    │  Virtual Storage       │ <── SVC ───     Heap & Storage Mgr  │
    │  Decimal / Char Data   │            │    (GETMAIN/FREEMAIN)  │
    └────────────────────────┘            └────────────────────────┘
```

### Module Structure & Lines of Code

| Module | Lines | Description |
|--------|------:|-------------|
| `interpreter`| 5,601 | Logical engine: Recursive execution of emulated COBOL/PL/I code |
| `file_io` | 1,468 | Data adapter: Maps mainframe I/O (READ/WRITE) to `open-mainframe-dataset` |
| `abend` | 1,097 | Recovery: Capture of system state and generation of formatted dumps |
| `date_time` | 976 | Calendar services: Byte-perfect Lilian day calculations (CEEDAYS) |
| `string` | 659 | String engine: Implementation of INSPECT, STRING, and UNSTRING verbs |
| `sort_verb` | 634 | Integration: Internal interface for the COBOL `SORT` statement |
| `numeric_editing`| 602 | Formatting: Application of PIC masks (Z, *, $, +, -) |
| `math` | 601 | Math library: Implementation of 30+ CEESxxx functions (SIN, COS, etc.) |
| `options` | 562 | Runtime config: Management of LE options (STACK, HEAP, TRAP) |
| `enclave` | 535 | Lifecycle: Management of the LE Enclave and Process structure |
| `condition` | 524 | Error signals: Implementation of LE Condition Tokens |
| `message` | 495 | Catalog: LE message IDs and multi-language message text |
| `storage` | 480 | Format handlers: Management of COMP-3, COMP, and DISPLAY buffers |
| `ilc` | 471 | Inter-op: Call-stack management for multi-language applications |
| `heap` | 436 | Memory manager: Implementation of the LE Dynamic Heap |
| `locale` | 398 | Globalization: CEE3LCT and currency/date formatting rules |
| **Total** | **~16,811** | |

## Implementation Details

### Decimal Arithmetic (The "Business" Engine)
Mainframe applications avoid floating-point math for currency. This crate implements:
- **Big-endian Packed Decimal**: 4-bit BCD (Binary Coded Decimal) nibbles with sign trailing.
- **Precision Management**: Supports the `ARITH(EXTEND)` option, allowing up to 31 digits of precision without rounding errors.
- **COBOL Rounding**: Implements the "nearest-even" (Banker's rounding) used in COBOL `ROUNDED` clauses.

### Inter-Language Communication (ILC)
When a COBOL program calls a PL/I subroutine:
- **Descriptor Mapping**: The `IlcManager` translates between COBOL's "BY REFERENCE" descriptors and PL/I's descriptor blocks.
- **Condition Handling**: Ensures that a `SIGNAL` in PL/I can be caught by an `ON-unit` in the caller, even across language boundaries.

### LE Date/Time System
IBM LE uses the **Lilian Day** system (number of days since Oct 14, 1582).
- **CEEDAYS**: Converts date strings to Lilian integers.
- **CEELOCT**: Uses the host OS time and adjusts for the emulated system time zone.
- **Accuracy**: Accounts for the Gregorian calendar reform and leap years exactly as a mainframe does.

## Implementation vs Mainframe Gaps

| Feature | Real z/OS Mainframe | OpenMainframe implementation |
|---------|---------------------|------------------------------|
| **Registers** | Hardware GPRs 0-15 and ARs. | Virtual register set within the `Tcb` structure. |
| **Code Execution** | Machine code (z/Architecture). | Recursive AST interpretation or LLVM-compiled IR. |
| **Storage Key** | Hardware key (0-15) per page. | Logical ownership tags on memory buffers. |
| **Floating Point** | Hexadecimal (HFP) and Binary (BFP). | Standard Rust `f64` (BFP) used for internal math. |
| **ABEND Dumps** | SVC dump to a dataset. | Written as `.dump` files or logged to standard error. |
| **Character Set** | EBCDIC by default. | Internal strings are UTF-8; converted via `open-mainframe-encoding`. |

## Feature Coverage

### LE Callable Services

| Service | Status | Description |
|---------|--------|-------------|
| `CEEDAYS` | Full | Date to Lilian day conversion. |
| `CEEDATE` | Full | Lilian day to Date string conversion. |
| `CEEMSG` | Full | Write a message to the LE message file. |
| `CEENCOD` | Full | Construct a Condition Token. |
| `CEESxxx` | Full | 30+ Math services (SIN, COS, LOG, SQRT, etc.). |
| `CEE3LCT` | Full | Set or query the locale. |
| `CEETEST` | Stub | Invoke the debugger. |

## Usage Examples

### Performing a COBOL-style INSPECT TALLYING

```rust
use open_mainframe_runtime::string::inspect_tallying;

let source = "ABRACADABRA";
let count = inspect_tallying(source, "A", None).expect("Inspect failed");
assert_eq!(count, 5);
```

### Formatting Decimal Data for Display

```rust
use open_mainframe_runtime::numeric_editing::format_numeric;

let value = 1234.56;
let mask = "$Z,ZZ9.99";
let output = format_numeric(value, mask).unwrap();
assert_eq!(output, "$1,234.56");
```

## Testing

The Runtime crate is the most heavily tested in the workspace (700+ tests):
- **Decimal Regression**: Over 10,000 auto-generated test cases comparing Rust math to IBM z/OS results.
- **LE Service Matrix**: Tests every implemented `CEE` service with positive and negative inputs.
- **ILC Stress**: Verifies recursive calls between emulated COBOL and PL/I (50+ levels deep).
- **String Fuzzing**: Randomly generated strings tested against `INSPECT` and `UNSTRING` logic.

```sh
cargo test -p open-mainframe-runtime
```
