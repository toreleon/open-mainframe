# Gap Analysis: HLASM (High Level Assembler)

## Official Specification Summary

HLASM (High Level Assembler) is IBM's assembler for the z/Architecture instruction set, used on z/OS, z/VM, and z/VSE. It translates assembly language source code into machine code (object decks) for execution on IBM Z processors. HLASM is classified as a **core** mainframe technology because:

- It is used for system-level programming (SVC routines, authorized programs, LE exits)
- Performance-critical code paths in CICS, IMS, DB2, and the z/OS kernel itself are written in assembler
- Many customer shops maintain significant assembler code bases (particularly in banking, government)
- System exits, user modifications (USERMODS), and I/O appendages require assembler
- HLASM macros define system control blocks (CVT, TCB, ASCB, etc.)

The language is documented in:
- **HLASM Language Reference** (SC26-4940) — instruction syntax, macro language, conditional assembly
- **HLASM Programmer's Guide** (SC26-4941) — assembly, link-edit, execution procedures
- **z/Architecture Principles of Operation** (SA22-7832) — definitive machine instruction reference

HLASM is currently at Version 1 Release 6 and supports instruction sets through IBM z17.

## Key Features & Capabilities

### 1. Machine Instruction Formats

z/Architecture defines multiple instruction formats based on operand types:

| Format | Length | Description |
|--------|--------|-------------|
| E | 2 bytes | No operands |
| I | 2 bytes | Immediate operand |
| RR | 2 bytes | Register-Register |
| RRE | 4 bytes | Register-Register (extended opcode) |
| RRF | 4 bytes | Register-Register with additional fields |
| RRD | 4 bytes | Register-Register for FP |
| RX | 4 bytes | Register-Indexed storage |
| RXE | 6 bytes | Register-Indexed (extended) |
| RXF | 6 bytes | Register-Indexed for FP |
| RXY | 6 bytes | Register-Indexed with long displacement |
| RS | 4 bytes | Register-Storage |
| RSE | 6 bytes | Register-Storage (extended) |
| RSY | 6 bytes | Register-Storage with long displacement |
| RSL | 6 bytes | Register-Storage for decimal |
| RI | 4 bytes | Register-Immediate |
| RIE | 6 bytes | Register-Immediate (extended) |
| RIL | 6 bytes | Register-Immediate (long) |
| SI | 4 bytes | Storage-Immediate |
| SIL | 6 bytes | Storage-Immediate (long) |
| SIY | 6 bytes | Storage-Immediate with long displacement |
| S | 4 bytes | Storage operand only |
| SS | 6 bytes | Storage-Storage (1 or 2 length fields) |
| SSE | 6 bytes | Storage-Storage (extended) |
| SSF | 6 bytes | Storage-Storage with register |
| VRR | 6 bytes | Vector Register-Register |
| VRI | 6 bytes | Vector Register-Immediate |
| VRS | 6 bytes | Vector Register-Storage |
| VRV | 6 bytes | Vector Register-Vector index |
| VRX | 6 bytes | Vector Register-Indexed storage |
| VSI | 6 bytes | Vector Storage-Immediate |

### 2. Machine Instruction Categories (~1000+ instructions)

#### General Instructions
- **Load/Store**: L, LR, LH, LG, LGR, LGF, LGFR, LA, LAY, LM, LMG, ST, STH, STG, STM, STMG, IC, ICY, ICM, ICMH, STC, STCY, STCM, STCMH
- **Arithmetic**: A, AR, AH, AHI, AG, AGR, AGHI, AFI, S, SR, SH, SG, SGR, M, MR, MH, MHI, MSG, MSGR, D, DR, DL, DLG, DSG, DSGR
- **Compare**: C, CR, CH, CHI, CG, CGR, CGHI, CL, CLR, CLG, CLGR, CLI, CLIY
- **Logical**: N, NR, NG, NGR, NI, NIY, O, OR, OG, OGR, OI, OIY, X, XR, XG, XGR, XI, XIY
- **Shift**: SLA, SLAK, SRA, SRAK, SLL, SLLK, SRL, SRLK, SLAG, SRAG, SLLG, SRLG
- **Branch**: B (BC 15), BR, BAS, BASR, BAL, BALR, BCT, BCTR, BXH, BXHG, BXLE, BXLEG, BRC, BRCL, BRASL
- **Extended branch mnemonics**: BE, BNE, BH, BL, BNH, BNL, BZ, BNZ, BO, BNO, BP, BM, BC (all condition code variants)
- **Move**: MVC, MVCL, MVCLE, MVI, MVIY, MVZ, MVN, MVO
- **Test**: TM, TMY, TMH, TMHH, TMHL, TMLH, TMLL
- **Conversion**: CVB, CVD, CVBG, CVDG
- **Other**: EX, EXECUTE RELATIVE LONG (EXRL), CLM, STCM, CLST, MVST, SRST

#### Decimal Instructions
- **Arithmetic**: AP (Add Packed), SP (Subtract Packed), MP (Multiply Packed), DP (Divide Packed), ZAP (Zero and Add Packed), SRP (Shift and Round Packed)
- **Convert**: PACK, UNPK, PKA, PKU, UNPKA, UNPKU
- **Compare**: CP (Compare Packed)
- **Edit**: ED (Edit), EDMK (Edit and Mark)
- **Move**: MVN (Move Numeric), MVZ (Move Zone), MVO (Move with Offset)

#### Floating-Point Instructions (BFP/HFP/DFP)
- **Load**: LE, LER, LD, LDR, LX, LXR, LZER, LZDR, LZXR
- **Store**: STE, STD
- **Add**: AEB, AEBR, ADB, ADBR, AXR, AXBR
- **Subtract**: SEB, SEBR, SDB, SDBR, SXR, SXBR
- **Multiply**: MEEB, MEEBR, MDB, MDBR, MXBR
- **Divide**: DEB, DEBR, DDB, DDBR, DXBR
- **Compare**: CEB, CEBR, CDB, CDBR, CXBR
- **Convert**: CEFBR, CDFBR, CXFBR, CFEBR, CFDBR, CFXBR, CEGBR, CDGBR, CXGBR, CGEBR, CGDBR, CGXBR
- **DFP**: ADTR, SDTR, MDTR, DDTR, CDTR, AXTR, SXTR, MXTR, DXTR, CXTR, TDCET, TDCDT, TDCXT, LEDTR, LDXTR, FIDTR, FIXTR

#### Control Instructions (Privileged)
- LPSW, LPSWE, IPK, ISK, SPM, SSK, SSM, SVC, DIAGNOSE, SIGP, STCTL, LCTL, STAP, STIDP, STPT, SPT, STCK, SCKC, PTLB, IPTE, ISKE, SSKE, RRBE, IDTE, CRDTE, TPROT, LURA, STURG, LASP, PALB, PC, PT, SAC, SACF, BSG, TAR, ESAR, EPAR, EREG, ESTA, MVCP, MVCS, MVCDK, MVSK

#### I/O Instructions
- SIO, SIOF, TIO, TSCH, HIO, SSCH, STSCH, MSCH, CSCH, HSCH, RSCH, RCHP, SCHM, STCRW, SAL, XSCH

#### Vector Instructions (~150+ instructions, z13+)
- **Load/Store**: VL, VLR, VLREP, VLEB, VLEH, VLEF, VLEG, VLM, VST, VSTM, VLBB, VLVG
- **Arithmetic**: VA, VS, VML, VMLH, VMAE, VMALE, VMAH, VMALH, VMO, VMLO, VME, VMLE, VLC, VLP, VAVG, VAVGL, VACC, VSCBI, VMRH, VMRL
- **Compare**: VEC, VECL, VCEQ, VCH, VCHL, VFCE, VFCH, VFCHE
- **Logical**: VN, VO, VX, VNN, VNO, VNX, VOC, VNOT, VSEL
- **Shift**: VERLL, VERLLV, VESL, VESLV, VESRA, VESRAV, VESRL, VESRLV
- **String**: VFAE, VFEE, VFENE, VISTR, VSTRC, VSTRS
- **FP (vector)**: VFA, VFS, VFM, VFD, VFSQ, VFMA, VFMS, VFLR, WFC, WFK
- **Packed-decimal (z15+)**: VLIP, VSRP, VMSP, VDP, VSDP, VRP, VCLZDP, VCSPH, VPKZR, VUPKZH, VUPKZL

### 3. Assembler Directives (~60+ directives)

#### Section Control
| Directive | Purpose |
|-----------|---------|
| CSECT | Start/continue a control section |
| DSECT | Start/continue a dummy section (data layout without storage) |
| RSECT | Read-only control section (reenterability checking) |
| COM | Start/continue a common section |
| DXD | Define external dummy section |
| START | Define first control section with optional origin |
| END | End of assembly module |
| LOCTR | Define alternate location counter within a section |

#### Data Definition
| Directive | Purpose |
|-----------|---------|
| DC | Define Constant — 20+ type codes: A, AD, B, C, CA, CE, D, DH, DB, DD, E, ED, EH, F, FD, G, H, P, Q, R, S, V, X, Y, Z |
| DS | Define Storage — same types as DC but no initialization |
| CCW, CCW0, CCW1 | Define channel command words |

#### Symbol and Addressing
| Directive | Purpose |
|-----------|---------|
| EQU | Equate symbol to value/expression |
| USING | Establish base register for addressing |
| DROP | Remove base register assignment |
| ORG | Set location counter |
| PUSH/POP | Save/restore USING, PRINT, ACONTROL state |
| OPSYN | Define/redefine operation code synonym |

#### External Linkage
| Directive | Purpose |
|-----------|---------|
| ENTRY | Declare entry point visible to linker |
| EXTRN | Declare external symbol reference |
| WXTRN | Weak external reference |

#### Source Management
| Directive | Purpose |
|-----------|---------|
| COPY | Include source from library member |
| LTORG | Generate literal pool |
| CNOP | Conditional no-op for alignment |
| AMODE | Set addressing mode (24, 31, 64, ANY) |
| RMODE | Set residency mode (24, 31, ANY, 64) |

#### Listing Control
| Directive | Purpose |
|-----------|---------|
| PRINT | Control listing options (ON, OFF, GEN, NOGEN, DATA, NODATA) |
| TITLE | Set listing title |
| EJECT | Force new page |
| SPACE | Insert blank lines |
| PUNCH/REPRO | Generate object deck records |

#### Conditional Assembly & Control
| Directive | Purpose |
|-----------|---------|
| ACONTROL | Change assembly options mid-stream |
| ADATA | Generate user-defined ADATA records |
| AINSERT | Insert records into input stream |
| EXITCTL | Control assembler exit |
| XATTR | Extended attributes (GOFF) |
| CATTR | Class attributes (GOFF) |
| ALIAS | Define external alias name |

### 4. Macro Language

HLASM's macro facility is a full programming language within the assembler:

#### Macro Definition
```
         MACRO
&LABEL   MACNAME &PARM1,&PARM2,&KEY=DEFAULT
.*       Comment
         ... macro body ...
         MEND
```

#### Macro Features
- **Positional parameters**: `&SYSLIST(n)`, variable number via `&SYSLIST`
- **Keyword parameters**: `&KEY=value` with defaults
- **System variable symbols**: `&SYSNDX` (unique counter), `&SYSECT` (current section), `&SYSLOC` (current LOCTR), `&SYSDATE`, `&SYSTIME`, `&SYSPARM`, `&SYSASM`, `&SYSMAC`, `&SYSLIB`, `&SYSSTMT`, `&SYSOPT_*`, `&SYSTEM_ID`
- **Inner macro calls**: Macros calling macros
- **MNOTE**: Generate assembly-time messages with severity codes
- **MEXIT**: Early exit from macro expansion

### 5. Conditional Assembly Language

A full programming language executed at assembly time:

#### SET Symbols (Assembly-time Variables)
| Type | Declaration | Purpose |
|------|------------|---------|
| SETA | LCLA/GBLA | Arithmetic (integer) |
| SETB | LCLB/GBLB | Boolean (0 or 1) |
| SETC | LCLC/GBLC | Character string |

#### Control Flow
| Directive | Purpose |
|-----------|---------|
| AIF | Conditional branch (assembly-time IF) |
| AGO | Unconditional branch (assembly-time GO TO) |
| ANOP | Assembly-time NOP (target for branches) |
| ACTR | Set conditional assembly loop counter (prevent infinite loops) |

#### Conditional Assembly Built-in Functions
- **Arithmetic**: A2B, A2C, A2D, A2X, B2A, C2A, D2A, X2A, SIGNED, SLLA, SRLA, SLA, SRA, AND, OR, NOT, XOR, FIND, INDEX
- **Character**: BYTE, DCLEN, DCVAL, DEQUOTE, DOUBLE, ISBIN, ISDEC, ISHEX, ISSYM, LOWER, SIGNED, UPPER, A2C, A2D, A2X, B2C, B2D, B2X, C2B, C2D, C2X, D2B, D2C, D2X, X2B, X2C, X2D
- **String ops**: via `SETC` with substring notation `'string'(start,length)`
- **Type attribute**: T' (type), L' (length), S' (scale), I' (integer), K' (count), N' (number), O' (opcode), D' (defined)

### 6. Addressing Modes

| Mode | Description |
|------|-------------|
| AMODE 24 | 24-bit addressing (below the line, 16MB) |
| AMODE 31 | 31-bit addressing (below the bar, 2GB) |
| AMODE 64 | 64-bit addressing (above the bar) |
| AMODE ANY | Runs in any addressing mode |
| RMODE 24 | Must reside below 16MB |
| RMODE 31 | May reside below 2GB |
| RMODE ANY | May reside anywhere below 2GB |
| RMODE 64 | May reside above 2GB (with AMODE 64) |

### 7. Linkage Conventions

- **Standard linkage**: R1=parameter list, R13=save area, R14=return address, R15=entry point
- **64-bit linkage**: Extended save areas (F4SA, F7SA formats)
- **LE-compatible**: CEEENTRY/CEETERM macros for Language Environment
- **XPLINK**: Extra Performance Linkage (optimized calling convention)

### 8. Object Formats

| Format | Description |
|--------|-------------|
| OBJ | Traditional 80-byte object deck (PUNCH format) |
| GOFF | Generalized Object File Format (supports long names, classes) |
| XOBJ | Extended Object Format |

## Current OpenMainframe Status

**No HLASM implementation exists.** The codebase has only incidental references:

- `crates/open-mainframe/src/main.rs` — `emit_asm` flag for LLVM backend (emits native assembly, not HLASM)
- `crates/open-mainframe-cics/src/bms/parser.rs` — `MapLanguage::Assembler` enum variant for BMS map language detection
- `crates/open-mainframe-ims/src/psb/mod.rs` — `PsbLanguage::Asm` for PSB language attribute

There is no HLASM lexer, parser, assembler, macro processor, or z/Architecture instruction encoder.

## Gap Details

| Feature | Official z/OS | OpenMainframe | Gap |
|---------|--------------|---------------|-----|
| Source format (fixed cols 1-71, continuation col 72) | Full | None | **Missing** |
| Lexer/Tokenizer | Full | None | **Missing** |
| Parser (instruction operands, expressions) | Full | None | **Missing** |
| Machine instruction encoding (~1000+ mnemonics) | Full (through z17) | None | **Missing** |
| Instruction formats (RR, RX, RS, SI, SS, etc. — 30+ formats) | Full | None | **Missing** |
| Assembler directives (~60+ directives) | Full | None | **Missing** |
| DC/DS data definition (20+ type codes) | Full | None | **Missing** |
| USING/DROP base register management | Full | None | **Missing** |
| CSECT/DSECT/RSECT section control | Full | None | **Missing** |
| EQU, ORG, LTORG symbol management | Full | None | **Missing** |
| Macro language (MACRO/MEND, parameters, MNOTE) | Full | None | **Missing** |
| System variable symbols (&SYSNDX, &SYSECT, etc.) | Full (~50 symbols) | None | **Missing** |
| Conditional assembly (AIF/AGO/ANOP, SET symbols) | Full | None | **Missing** |
| Conditional assembly built-in functions | Full (~50 functions) | None | **Missing** |
| COPY member inclusion | Full | None | **Missing** |
| Literal pool management (=, LTORG) | Full | None | **Missing** |
| Expression evaluation (arithmetic, logical, attribute refs) | Full | None | **Missing** |
| AMODE/RMODE addressing mode control | Full | None | **Missing** |
| Object code generation (OBJ/GOFF) | Full | None | **Missing** |
| Listing generation (cross-reference, USING map) | Full | None | **Missing** |
| ENTRY/EXTRN/WXTRN external linkage | Full | None | **Missing** |
| Vector instruction support (z13+) | Full (~150 instructions) | None | **Missing** |
| Decimal instruction support | Full (AP, SP, MP, etc.) | None | **Missing** |
| Floating-point instructions (BFP/HFP/DFP) | Full | None | **Missing** |
| Structured programming macros (IF/ELSE/ENDIF, DO/ENDDO) | Full | None | **Missing** |
| LE-compatible entry/exit (CEEENTRY/CEETERM) | Full | None | **Missing** |
| ADATA (Associated Data) generation | Full | None | **Missing** |

## Proposed Epic Structure

### Epic A100: HLASM Lexer and Source Format
- **A100.1**: Source format parser — fixed format (columns 1-71), continuation column 72, sequence field (73-80), comment lines
- **A100.2**: Tokenizer — labels, operation codes, operands, remarks, literals (=F'1', =C'text', etc.)
- **A100.3**: Continuation line handling — non-blank in column 72, next line starts at column 16
- **A100.4**: TITLE, EJECT, SPACE, PRINT listing directives
- **Complexity**: M (Medium)

### Epic A101: Expression Evaluator and Symbol Table
- **A101.1**: Symbol table — labels, EQU values, lengths, types, section association
- **A101.2**: Expression evaluation — arithmetic (+, -, *, /), logical (AND, OR, NOT), parentheses
- **A101.3**: Attribute references — L' (length), T' (type), S' (scale), I' (integer), O' (opcode), D' (defined)
- **A101.4**: Location counter management — ORG, CSECT, DSECT, LOCTR
- **Complexity**: L (Large)

### Epic A102: Section Control and Data Definition
- **A102.1**: CSECT, DSECT, RSECT, COM, DXD, START, END
- **A102.2**: DC — all type codes (A, B, C, D, E, F, H, P, S, V, X, Y, Z and extended)
- **A102.3**: DS — storage reservation with alignment
- **A102.4**: CCW, CCW0, CCW1 channel command words
- **A102.5**: Literal pool management — literal syntax (=type'value'), LTORG
- **Complexity**: L (Large)

### Epic A103: Base Register Management
- **A103.1**: USING — ordinary USING (register, address)
- **A103.2**: Labeled USING — named qualification for explicit base references
- **A103.3**: Dependent USING — DSECTs based off other USINGs
- **A103.4**: DROP — remove USING associations
- **A103.5**: PUSH/POP USING — save/restore USING state
- **A103.6**: Base-displacement resolution — automatic base register selection for symbolic addresses
- **Complexity**: XL (Extra Large — this is one of the most complex parts of HLASM)

### Epic A104: Machine Instruction Encoding (General)
- **A104.1**: Instruction format table — all 30+ formats with field positions and sizes
- **A104.2**: RR format instructions (register-register)
- **A104.3**: RX/RXY format instructions (register-indexed storage)
- **A104.4**: RS/RSY format instructions (register-storage)
- **A104.5**: SI/SIY/SIL format instructions (storage-immediate)
- **A104.6**: SS format instructions (storage-storage)
- **A104.7**: RI/RIE/RIL format instructions (register-immediate)
- **A104.8**: Extended branch mnemonics (BE, BNE, BH, BL, etc.)
- **Complexity**: XL (Extra Large — hundreds of instructions)

### Epic A105: Machine Instruction Encoding (Specialized)
- **A105.1**: Decimal instructions (AP, SP, MP, DP, ZAP, SRP, PACK, UNPK, ED, EDMK)
- **A105.2**: Floating-point instructions (HFP, BFP, DFP)
- **A105.3**: Control instructions (privileged — for completeness/testing)
- **A105.4**: I/O instructions (channel subsystem)
- **Complexity**: L (Large)

### Epic A106: Vector Facility Instructions
- **A106.1**: VRI format instructions (vector register-immediate)
- **A106.2**: VRR format instructions (vector register-register)
- **A106.3**: VRS format instructions (vector register-storage)
- **A106.4**: VRV/VRX/VSI format instructions
- **A106.5**: Vector string instructions (VFAE, VFEE, VFENE, VISTR, VSTRC)
- **A106.6**: Vector FP instructions
- **A106.7**: Vector packed-decimal instructions (z15+)
- **Complexity**: XL (Extra Large — 150+ instructions)

### Epic A107: Macro Language
- **A107.1**: MACRO/MEND definition, positional and keyword parameters
- **A107.2**: System variable symbols (&SYSNDX, &SYSECT, &SYSDATE, etc.)
- **A107.3**: Macro expansion with &SYSLIST, inner macro calls
- **A107.4**: MNOTE (assembly-time messages), MEXIT (early exit)
- **A107.5**: COPY member inclusion from macro libraries
- **Complexity**: L (Large)

### Epic A108: Conditional Assembly
- **A108.1**: SET symbols — LCLA/GBLB/LCLC declarations, SETA/SETB/SETC assignment
- **A108.2**: Control flow — AIF (conditional branch), AGO (unconditional), ANOP (target), ACTR (loop limit)
- **A108.3**: Conditional assembly built-in functions (~50 functions)
- **A108.4**: Substring notation, concatenation, type/attribute functions
- **Complexity**: L (Large)

### Epic A109: Object Code Generation and Linkage
- **A109.1**: OBJ format — 80-byte ESD, TXT, RLD, END records
- **A109.2**: GOFF format — generalized object with classes and long names
- **A109.3**: ENTRY/EXTRN/WXTRN external symbol resolution
- **A109.4**: AMODE/RMODE setting
- **A109.5**: RLD (Relocation Dictionary) generation for address constants
- **A109.6**: Cross-reference listing and USING map
- **Complexity**: L (Large)

### Epic A110: Structured Programming and LE Integration
- **A110.1**: Structured programming macros — IF/ELSE/ENDIF, DO/ENDDO, SELECT/WHEN/ENDSEL, CASENTRY/CASE/ENDCASE
- **A110.2**: LE-compatible linkage — CEEENTRY/CEETERM, F4SA/F7SA save areas
- **A110.3**: XPLINK calling convention
- **A110.4**: Standard linkage convention (R1/R13/R14/R15)
- **Complexity**: M (Medium)

### Epic A111: Two-Pass Assembly Engine
- **A111.1**: Pass 1 — symbol definition, macro expansion, conditional assembly processing
- **A111.2**: Pass 2 — instruction encoding, address resolution, object code emission
- **A111.3**: Forward reference resolution
- **A111.4**: Error detection and diagnostic messages
- **A111.5**: Assembly statistics and listing generation
- **Complexity**: XL (Extra Large — the core assembly engine)

## Dependencies

| Dependency | Crate | Reason |
|------------|-------|--------|
| open-mainframe-encoding | EBCDIC support | Assembler source may be EBCDIC-encoded; DC C'...' uses EBCDIC |
| open-mainframe-lang-core | Shared traits | Span, Diagnostic, AstNode traits |
| open-mainframe-dataset | COPY/MACRO libraries | Library members for COPY and macro definitions |
| open-mainframe-jcl | Batch assembly | EXEC PGM=ASMA90 for assembler invocation |
| open-mainframe-runtime (new) | Execution | Running assembled programs would need a z/Architecture emulator |

**Critical note**: Unlike COBOL (which can be interpreted), HLASM produces machine code for z/Architecture. Full execution would require a z/Architecture CPU emulator or translation layer, which is a massive separate effort. The assembler itself (source-to-object-code translation) is the primary deliverable; execution is secondary.

## Complexity Estimate

| Epic | Complexity | Rationale |
|------|-----------|-----------|
| A100 (Lexer/Source) | M | Fixed-format, well-defined columns, straightforward tokenization |
| A101 (Expressions/Symbols) | L | Complex expression syntax with attribute references and forward references |
| A102 (Sections/Data) | L | 20+ DC type codes with sub-options (length, duplication, alignment) |
| A103 (USING/DROP) | XL | Base-displacement resolution is the hardest part of any S/390 assembler |
| A104 (General Instructions) | XL | Hundreds of instructions across 30+ formats |
| A105 (Specialized Instructions) | L | Decimal, FP, control, I/O — fewer but complex encoding |
| A106 (Vector Instructions) | XL | 150+ vector instructions with element-size variants |
| A107 (Macro Language) | L | Full programming language with nested expansion |
| A108 (Conditional Assembly) | L | Another programming language layer with ~50 built-in functions |
| A109 (Object Code/Linkage) | L | Two object formats (OBJ, GOFF) with complex relocation |
| A110 (SPM/LE) | M | Structured macros are sugar; LE linkage is well-defined |
| A111 (Assembly Engine) | XL | Two-pass engine tying everything together |

**Total estimated effort**: 12 epics, overall XXL complexity (the largest possible addition — HLASM is essentially writing a full compiler for a CISC architecture with ~1000 instructions)

## Reference Documentation

- [HLASM V1R6 Language Reference (SC26-4940)](https://www.ibm.com/docs/en/SSENW6_1.6.0/pdf/asmr1024_pdf.pdf)
- [HLASM V1R6 Programmer's Guide (SC26-4941)](https://www.ibm.com/docs/en/SSENW6_1.6.0/pdf/asmp1024_pdf.pdf)
- [z/Architecture Principles of Operation (SA22-7832)](https://www.ibm.com/docs/en/SSQ2R2_15.0.0/com.ibm.tpf.toolkit.hlasm.doc/dz9zr006.pdf)
- [z/Architecture Reference Summary (SA22-7871)](https://www.ibm.com/support/pages/sites/default/files/2021-05/SA22-7871-10.pdf)
- [IBM High Level Assembler Product Page](https://www.ibm.com/products/high-level-assembler-and-toolkit-feature)
- [HLASM z16 Instruction Support (APAR PH39324)](https://www.ibm.com/support/pages/high-level-assembler-support-ibm-z16-instructions-apar-ph39324)
- [HLASM z15 Instruction Support (APAR PH00902)](https://www.ibm.com/support/pages/high-level-assembler-support-ibm-z15-instructions-apar-ph00902)
- [Structured Programming in Assembler — IBM SHARE](https://share.confex.com/share/125/webprogram/Handout/Session17871/structured_programming_white.pdf)
- [Macros and Conditional Assembly Techniques — IBM SHARE](https://public.dhe.ibm.com/software/websphere/awdtools/hlasm/s8167h.pdf)
- [Bixoft: Complete HLASM Instruction List](https://bixoft.nl/english/opl_bbbo.htm)
- [z/Architecture — Wikipedia](https://en.wikipedia.org/wiki/Z/Architecture)
