# HLASM Crate — Epics & Stories

## Epic A100: HLASM Lexer and Source Format

**Goal:** Parse HLASM fixed-format source (columns 1-71, continuation column 72, sequence 73-80) into structured tokens.

**Crate:** `open-mainframe-hlasm`
**FRs:** FR-v4.0-A100

### Story A100.1: Source Format Parser

As a **assembler programmer**,
I want **fixed-format source to be correctly parsed**,
So that **labels, opcodes, operands, and remarks are extracted from their column positions**.

**Acceptance Criteria:**

**Given** `MYLAB    L     R5,MYDATA          LOAD VALUE`
**When** parsed (label col 1-8, opcode 10-14, operands 16-71, remarks after operands)
**Then** label='MYLAB', opcode='L', operands='R5,MYDATA', remarks='LOAD VALUE'

**Given** a line with `*` in column 1
**When** parsed
**Then** it is recognized as a full-line comment

**Given** a non-blank character in column 72
**When** the next line is read
**Then** the next line's content starting at column 16 is concatenated as continuation

**Complexity:** M

### Story A100.2: Tokenizer — Opcodes, Registers, Expressions, Literals

As a **assembler programmer**,
I want **operand fields tokenized into registers, expressions, literals, and addresses**,
So that **the parser can process any valid operand combination**.

**Acceptance Criteria:**

**Given** operand `R5,=F'100'(R12)`
**When** tokenized
**Then** tokens: register(R5), literal(=F'100'), index-register(R12)

**Given** operand `MYLAB+4(R3)`
**When** tokenized
**Then** tokens: symbol(MYLAB), operator(+), number(4), index(R3)

**Complexity:** M

---

## Epic A101: Expression Evaluator and Symbol Table

**Goal:** Implement the assembler symbol table and expression evaluation with attribute references.

**Crate:** `open-mainframe-hlasm`
**FRs:** FR-v4.0-A101

### Story A101.1: Symbol Table

As a **assembler programmer**,
I want **labels and EQU values to be recorded in a symbol table**,
So that **forward and backward references are resolved correctly**.

**Acceptance Criteria:**

**Given** `MYDATA   DC    F'100'` defined after a reference
**When** pass 2 resolves the reference
**Then** MYDATA's address, length (4), and type (F) are available

**Given** `TEN      EQU   10`
**When** referenced in `LA R1,TEN`
**Then** TEN resolves to the value 10

**Complexity:** L

### Story A101.2: Expression Evaluation and Attribute References

As a **assembler programmer**,
I want **arithmetic expressions and attribute references in operands**,
So that **I can compute addresses and offsets at assembly time**.

**Acceptance Criteria:**

**Given** `L  R5,MYTABLE+(4*ENTLEN)`
**When** assembled
**Then** the expression is evaluated using ENTLEN's EQU value

**Given** `DC  AL(L'MYFIELD) MYFIELD` where MYFIELD is DC CL20
**When** assembled
**Then** L'MYFIELD resolves to 20

Attribute references: L' (length), T' (type), S' (scale), I' (integer), O' (opcode), D' (defined)

**Complexity:** L

---

## Epic A102: Section Control and Data Definition

**Goal:** Implement CSECT/DSECT section control and the full DC/DS data definition type system.

**Crate:** `open-mainframe-hlasm`
**FRs:** FR-v4.0-A102

### Story A102.1: Section Control — CSECT, DSECT, START, END

As a **assembler programmer**,
I want **section control directives to organize code and data**,
So that **I can define executable sections and data mappings**.

**Acceptance Criteria:**

**Given** `MYPROG   CSECT`
**When** assembled
**Then** a new control section is started with location counter at 0

**Given** `MYDSECT  DSECT`
**When** assembled
**Then** a dummy section is created (no object code generated, used for field offsets)

**Complexity:** M

### Story A102.2: DC — Define Constant (All Type Codes)

As a **assembler programmer**,
I want **DC directives to generate all z/Architecture constant types**,
So that **I can define data in any format required by z/OS programs**.

**Acceptance Criteria:**

**Given** `DC  F'100'`
**When** assembled
**Then** 4 bytes of fullword integer value 100 are generated

**Given** `DC  CL20'Hello'`
**When** assembled
**Then** 20 bytes are generated: 'Hello' in EBCDIC followed by 15 spaces

**Given** `DC  PL4'12345'`
**When** assembled
**Then** 4 bytes of packed decimal 12345 (X'0012345C') are generated

Type codes: A, B, C, D, E, F, H, P, S, V, X, Y, Z (and extended: AD, FD, etc.)

**Complexity:** L

### Story A102.3: DS — Define Storage and Literal Pool (LTORG)

As a **assembler programmer**,
I want **DS for storage reservation and LTORG for literal pool management**,
So that **I can reserve uninitialized storage and collect literal constants**.

**Acceptance Criteria:**

**Given** `BUFFER   DS    CL256`
**When** assembled
**Then** 256 bytes are reserved at BUFFER's address with no initialization

**Given** references to `=F'1'` and `=C'ABC'` followed by `LTORG`
**When** assembled
**Then** the literal pool contains both constants at the LTORG location

**Complexity:** M

---

## Epic A103: Base Register Management (USING/DROP)

**Goal:** Implement the USING/DROP directives and automatic base-displacement resolution — the most complex part of HLASM.

**Crate:** `open-mainframe-hlasm`
**FRs:** FR-v4.0-A103

### Story A103.1: Ordinary USING and DROP

As a **assembler programmer**,
I want **USING to establish base register addressability**,
So that **symbolic addresses are resolved to base-displacement form**.

**Acceptance Criteria:**

**Given** `USING MYPROG,R12` and `L R5,MYDATA` where MYDATA is at offset 100 from MYPROG
**When** assembled
**Then** the instruction encodes as L R5,100(0,R12) — base=R12, displacement=100

**Given** `DROP R12`
**When** a subsequent instruction references a symbol
**Then** assembly fails with "no addressability" error (unless another USING is active)

**Complexity:** L

### Story A103.2: Labeled USING and Dependent USING

As a **assembler programmer**,
I want **labeled USING for explicit qualification and dependent USING for DSECT bases**,
So that **I can address multiple DSECT instances simultaneously**.

**Acceptance Criteria:**

**Given** `FIRST USING MYDSECT,R9` and `SECOND USING MYDSECT,R10`
**When** `L R5,FIRST.FIELD1` is assembled
**Then** the instruction uses R9 as base register

**Given** `USING MYDSECT,R9` (ordinary) followed by a dependent USING of an inner DSECT
**When** assembled
**Then** the inner fields resolve through the outer base register plus the inner offset

**Complexity:** XL

### Story A103.3: Base-Displacement Resolution Algorithm

As a **assembler programmer**,
I want **the assembler to automatically select the best base register**,
So that **I don't need to manually calculate displacements**.

**Acceptance Criteria:**

**Given** multiple active USINGs (R12 for 0-4095, R11 for 4096-8191)
**When** a symbol at offset 5000 is referenced
**Then** R11 is selected with displacement 904

**Given** a symbol beyond 4095 bytes from any active USING base
**When** referenced without extended base (20-bit displacement instructions)
**Then** assembly error is reported

**Complexity:** L

---

## Epic A104: Machine Instruction Encoding (General)

**Goal:** Encode all general-purpose z/Architecture instructions across 30+ instruction formats.

**Crate:** `open-mainframe-hlasm`
**FRs:** FR-v4.0-A104

### Story A104.1: Instruction Format Table

As a **assembler developer**,
I want **a comprehensive instruction format table mapping opcodes to formats**,
So that **any z/Architecture instruction can be encoded**.

**Acceptance Criteria:**

**Given** the opcode table is loaded
**When** mnemonic 'L' is looked up
**Then** format=RX-a, opcode=X'58', operands=R1,D2(X2,B2)

**Given** mnemonic 'LG' is looked up
**Then** format=RXY-a, opcode=X'E304', operands=R1,D2(X2,B2) with 20-bit displacement

**Complexity:** L

### Story A104.2: RR, RX, RS, SI, SS Format Encoding

As a **assembler programmer**,
I want **all classic instruction formats encoded correctly**,
So that **I can write programs using any S/370-S/390 instruction**.

**Acceptance Criteria:**

**Given** `LR    R5,R3` (RR format)
**When** assembled
**Then** 2 bytes: X'1853'

**Given** `MVC   TARGET(20),SOURCE` (SS-a format)
**When** assembled
**Then** 6 bytes with length-1=19, source and target base-displacement pairs

**Complexity:** XL

### Story A104.3: Extended Branch Mnemonics

As a **assembler programmer**,
I want **extended branch mnemonics (BE, BNE, BH, BL, etc.)**,
So that **I can write readable conditional branches**.

**Acceptance Criteria:**

**Given** `BE    EQUAL` (branch if equal)
**When** assembled
**Then** encodes as `BC 8,EQUAL` (mask=8 for equal condition)

**Given** `JNE   NOTEQUAL` (relative jump if not equal)
**When** assembled
**Then** encodes as `BRC 7,NOTEQUAL` (mask=7 for not equal)

**Complexity:** M

---

## Epic A105: Machine Instruction Encoding (Specialized)

**Goal:** Encode specialized instruction sets: decimal arithmetic, floating-point, and control instructions.

**Crate:** `open-mainframe-hlasm`
**FRs:** FR-v4.0-A105

### Story A105.1: Decimal Arithmetic Instructions

As a **assembler programmer**,
I want **decimal instructions (AP, SP, MP, DP, ZAP, PACK, UNPK, ED, EDMK)**,
So that **I can write programs that process packed decimal data**.

**Acceptance Criteria:**

**Given** `AP    TOTAL,AMOUNT` (SS-b format)
**When** assembled
**Then** 6 bytes with separate lengths for both operands

**Given** `PACK  PACKED(8),ZONED` (SS-b format)
**When** assembled
**Then** correct encoding with destination and source lengths

**Complexity:** M

### Story A105.2: Floating-Point Instructions (BFP/HFP/DFP)

As a **assembler programmer**,
I want **floating-point instructions for all three formats**,
So that **I can write scientific and financial floating-point code**.

**Acceptance Criteria:**

**Given** `AEBR  F4,F6` (BFP add, RRE format)
**When** assembled
**Then** 4 bytes: X'B30A0046'

**Complexity:** L

---

## Epic A106: Vector Facility Instructions

**Goal:** Encode 150+ vector instructions (z13+) across VRI/VRR/VRS/VRV/VRX/VSI formats.

**Crate:** `open-mainframe-hlasm`
**FRs:** FR-v4.0-A106

### Story A106.1: Vector Register-Register and Register-Storage Instructions

As a **assembler programmer**,
I want **vector instructions for SIMD operations**,
So that **I can write high-performance data processing code**.

**Acceptance Criteria:**

**Given** `VAF   V1,V2,V3` (vector add fullword)
**When** assembled
**Then** correct VRR-c format encoding with element size indicator

**Given** `VL    V1,0(R5)` (vector load)
**When** assembled
**Then** correct VRX format encoding

**Complexity:** XL

---

## Epic A107: Macro Language

**Goal:** Implement the HLASM macro language — MACRO/MEND definitions, system variable symbols, and macro expansion.

**Crate:** `open-mainframe-hlasm`
**FRs:** FR-v4.0-A107

### Story A107.1: MACRO/MEND Definition and Expansion

As a **assembler programmer**,
I want **macros to be defined with positional/keyword parameters and expanded inline**,
So that **I can write reusable code templates**.

**Acceptance Criteria:**

**Given** a macro definition:
```
         MACRO
&LABEL   SAVE  &REGS
&LABEL   STM   &REGS,14,12(13)
         MEND
```
**When** `MYSAVE SAVE (14,12)` is encountered
**Then** expands to `MYSAVE STM 14,12,12(13)` with unique &SYSNDX

**Complexity:** L

### Story A107.2: System Variable Symbols and COPY

As a **assembler programmer**,
I want **system variable symbols (&SYSNDX, &SYSECT, &SYSDATE, etc.) and COPY**,
So that **macros can generate unique labels and include external source**.

**Acceptance Criteria:**

**Given** `&SYSNDX` in a macro body
**When** the macro is expanded for the 5th time
**Then** &SYSNDX = '0005'

**Given** `COPY MYCOPYBK`
**When** processed
**Then** the contents of member MYCOPYBK from the macro library are inserted

~50 system variable symbols

**Complexity:** M

---

## Epic A108: Conditional Assembly

**Goal:** Implement the conditional assembly language — SET symbols, AIF/AGO control flow, and ~50 built-in functions.

**Crate:** `open-mainframe-hlasm`
**FRs:** FR-v4.0-A108

### Story A108.1: SET Symbols and AIF/AGO Control Flow

As a **assembler programmer**,
I want **conditional assembly for compile-time programming**,
So that **I can generate different code based on parameters and conditions**.

**Acceptance Criteria:**

**Given**:
```
         LCLC  &TYPE
&TYPE    SETC  'FULL'
         AIF   ('&TYPE' EQ 'FULL').GENFULL
         AGO   .GENHALF
.GENFULL ANOP
         DC    F'0'
         AGO   .DONE
.GENHALF ANOP
         DC    H'0'
.DONE    ANOP
```
**When** assembled
**Then** `DC F'0'` is generated (FULL path taken)

**Complexity:** L

### Story A108.2: Conditional Assembly Built-in Functions (~50)

As a **assembler programmer**,
I want **built-in functions for string, arithmetic, and type operations at assembly time**,
So that **I can write powerful macro logic**.

**Acceptance Criteria:**

**Given** `&LEN SETA K'&PARM` (K' = character count)
**When** &PARM is 'HELLO'
**Then** &LEN = 5

Functions include: A2B, A2C, A2D, A2X, B2A, B2C, B2D, B2X, BYTE, C2A, C2B, C2D, C2X, D2A, D2B, D2C, D2X, DCLEN, DCVAL, DEQUOTE, DOUBLE, FIND, INDEX, ISBIN, ISDEC, ISHEX, ISSYM, LOWER, SIGNED, SYSATTRA, SYSATTRP, UPPER, X2A, X2B, X2C, X2D, and more

**Complexity:** L

---

## Epic A109: Object Code Generation and Linkage

**Goal:** Generate OBJ and GOFF format object decks with relocation information.

**Crate:** `open-mainframe-hlasm`
**FRs:** FR-v4.0-A109

### Story A109.1: OBJ Format Generation

As a **assembler programmer**,
I want **standard 80-byte object deck generation**,
So that **assembled programs can be link-edited into load modules**.

**Acceptance Criteria:**

**Given** an assembled program with one CSECT and external references
**When** object code is emitted
**Then** ESD (External Symbol Dictionary), TXT (text), RLD (Relocation Dictionary), and END records are generated

**Complexity:** L

### Story A109.2: GOFF Format and Cross-Reference Listing

As a **assembler programmer**,
I want **GOFF format output and cross-reference listings**,
So that **I can use modern linkage features and debug assembly listings**.

**Acceptance Criteria:**

**Given** `GOFF` option specified
**When** assembled
**Then** Generalized Object File Format records are generated with long names and class support

**Given** assembly completes
**When** listing is generated
**Then** cross-reference of all symbols and USING map are included

**Complexity:** L

---

## Epic A110: Structured Programming and LE Integration

**Goal:** Implement structured programming macros (IF/DO/SELECT) and LE-compatible linkage conventions.

**Crate:** `open-mainframe-hlasm`
**FRs:** FR-v4.0-A110

### Story A110.1: Structured Programming Macros

As a **assembler programmer**,
I want **IF/ELSE/ENDIF, DO/ENDDO, SELECT/WHEN/ENDSEL structured macros**,
So that **I can write readable assembler code without explicit branch labels**.

**Acceptance Criteria:**

**Given** `IF (CR,R5,EQ,R6); MVC OUT,IN1; ELSE; MVC OUT,IN2; ENDIF`
**When** assembled
**Then** correct conditional branch instructions are generated

**Complexity:** M

### Story A110.2: LE-Compatible Linkage (CEEENTRY/CEETERM)

As a **assembler programmer**,
I want **LE-compatible function prolog/epilog**,
So that **assembler programs can interoperate with COBOL and PL/I via Language Environment**.

**Acceptance Criteria:**

**Given** `CEEENTRY PPA=YES,MAIN=YES`
**When** assembled
**Then** LE-standard save area (F4SA/F7SA), entry point descriptor, and PPA are generated

**Complexity:** M

---

## Epic A111: Two-Pass Assembly Engine

**Goal:** Implement the complete two-pass assembly engine tying together all components.

**Crate:** `open-mainframe-hlasm`
**FRs:** FR-v4.0-A111

### Story A111.1: Pass 1 — Symbol Definition and Macro Expansion

As a **assembler developer**,
I want **Pass 1 to process all symbol definitions, macros, and conditional assembly**,
So that **all symbols are known before instruction encoding in Pass 2**.

**Acceptance Criteria:**

**Given** a source file with forward references, macros, and conditional assembly
**When** Pass 1 completes
**Then** the symbol table is fully populated with addresses, lengths, and types

**Complexity:** XL

### Story A111.2: Pass 2 — Instruction Encoding and Object Emission

As a **assembler developer**,
I want **Pass 2 to encode all instructions and emit object code**,
So that **the final object deck is produced with all references resolved**.

**Acceptance Criteria:**

**Given** a completed symbol table from Pass 1
**When** Pass 2 processes each statement
**Then** machine instructions are encoded with correct base-displacement, immediate, and relative values

**Given** an address constant `DC A(EXTRN)` referencing an external symbol
**When** Pass 2 processes it
**Then** an RLD entry is generated for the linker to resolve

**Complexity:** XL

---
