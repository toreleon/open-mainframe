# open-mainframe-cobol

A **COBOL compiler** for the OpenMainframe z/OS clone — providing a full
compilation pipeline from source text through lexical analysis, parsing,
semantic analysis, and LLVM code generation. Supports COBOL-85, COBOL-2002,
and COBOL-2014 features along with IBM Enterprise COBOL extensions.

## Overview

This crate implements a traditional multi-pass compiler architecture for
COBOL programs. The pipeline comprises:

1. **Pass 0** — `CBL`/`PROCESS` compiler option extraction
2. **Pass 1** — Conditional compilation directives (`>>IF`, `>>EVALUATE`, `>>DEFINE`, `>>SET`)
3. **Pass 2** — Copybook expansion (`COPY ... REPLACING`) and DFHRESP pseudo-function expansion
4. **Pass 3** — `REPLACE` pseudo-text substitution
5. **Lexical analysis** — Tokenization of processed source
6. **Parsing** — Recursive descent parser producing a complete AST
7. **Semantic analysis** — Three-phase type checking, symbol resolution, and diagnostics
8. **Code generation** — LLVM IR emission (optional, behind `llvm` feature flag)

The crate also includes a library of 77+ intrinsic functions (numeric,
string, date/time) and JSON/XML generation and parsing facilities matching
the COBOL-2014 standard.

## Architecture

```
  COBOL Source
       │
  ┌────▼─────────────────────────────────────────────────┐
  │                  Preprocessor Pipeline                │
  │  Pass 0: CompilerOptions  (CBL/PROCESS)              │
  │  Pass 1: ConditionalProcessor (>>IF/>>EVALUATE)      │
  │  Pass 2: Preprocessor  (COPY/DFHRESP)                │
  │  Pass 3: ReplaceProcessor  (REPLACE ==...==)         │
  └────┬─────────────────────────────────────────────────┘
       │
  ┌────▼────────┐     ┌──────────────┐     ┌────────────────┐
  │   Scanner   │────▶│    Parser    │────▶│   Semantic     │
  │  (Lexer)    │     │  (AST gen)   │     │   Analyzer     │
  └─────────────┘     └──────────────┘     └───────┬────────┘
                                                   │
                                          ┌────────▼────────┐
                                          │  Code Generator  │
                                          │  (LLVM IR)       │
                                          └─────────────────┘
```

### Module Structure

| Module | Lines | Description |
|---|---|---|
| `lexer/` | ~4 460 | 4-pass preprocessor, tokenizer, source management, compiler options |
| `ast/` | ~2 000 | Complete AST: divisions, statements, data items, expressions |
| `parser/` | ~5 600 | Recursive descent parser for all COBOL divisions |
| `semantic/` | ~2 350 | Symbol table, type system, three-phase analyzer |
| `codegen/` | ~1 115 | LLVM IR generation (behind `llvm` feature flag) |
| `intrinsics/` | ~2 870 | 77+ COBOL-2014 intrinsic functions |
| `xml_json/` | ~1 440 | JSON GENERATE/PARSE and XML GENERATE/PARSE |
| `macros.rs` | 531 | Macro-driven keyword and statement definition tables |
| `error.rs` | 109 | Top-level `CobolError` enum with miette diagnostics |

**Total**: ~20 500 lines of Rust.

## Key Types and Traits

### Lexer

| Type | Description |
|---|---|
| `Scanner` | Tokenizes a `SourceFile` into `Vec<Token>` |
| `Token` / `TokenKind` | Token with span; ~30 kinds (literals, keywords, operators, punctuation) |
| `Keyword` | Enum with ~200 COBOL keywords (macro-generated from `for_all_keywords!`) |
| `SourceFile` / `SourceLine` | Source text with fixed/free format line parsing |
| `SourceManager` | Multi-file registry with `FileId`-based lookup |
| `SourceFormat` / `Indicator` | Fixed (cols 1-6/7/8-72) vs free format; column 7 indicators |
| `Preprocessor` | Pass 2: COPY expansion with REPLACING, DFHRESP expansion |
| `CopybookResolver` / `CopybookConfig` | Copybook search path management and circular detection |
| `ReplaceProcessor` / `PseudoTextReplacement` | Pass 3: REPLACE pseudo-text substitution |
| `ConditionalProcessor` | Pass 1: `>>IF`/`>>EVALUATE`/`>>DEFINE`/`>>SET` directive processing |
| `CompilerOptions` | Pass 0: CBL/PROCESS option parsing (ARITH, TRUNC, NUMPROC, etc.) |
| `Replacement` / `ReplacementMode` | REPLACING clause: Full, Leading, or Trailing |

### AST

| Type | Description |
|---|---|
| `Program` | Top-level AST: all four divisions + contained programs |
| `IdentificationDivision` | PROGRAM-ID, AUTHOR, etc. |
| `EnvironmentDivision` | CONFIGURATION, INPUT-OUTPUT, FILE-CONTROL |
| `DataDivision` | FILE, WORKING-STORAGE, LOCAL-STORAGE, LINKAGE sections |
| `DataItem` | Variable with level number, PICTURE, USAGE, OCCURS, children |
| `PictureClause` / `PictureCategory` | Analyzed PICTURE string with category and size |
| `ProcedureDivision` | USING/RETURNING, DECLARATIVES, sections/paragraphs/statements |
| `Statement` | Enum with 39 variants (macro-generated from `for_all_statement_variants!`) |
| `Expression` | Recursive enum: literals, variables, binary/unary ops, functions, LENGTH OF, ADDRESS OF |
| `Condition` | Comparison, Class, Sign, ConditionName, Not, And, Or |
| `FileControlEntry` | SELECT ... ASSIGN with organization, access mode, keys, status |

### Semantic

| Type | Description |
|---|---|
| `SemanticAnalyzer` | Three-phase analysis: symbol construction, validation, nested programs |
| `SemanticResult` | Symbol table + diagnostics + error flag |
| `SymbolTable` | HashMap-based registry with qualified name resolution |
| `Symbol` / `SymbolKind` | DataItem, Paragraph, Section, File, Index, ConditionName |
| `CobolType` / `TypeCategory` | Type system: category, size, decimal positions, usage, sign |
| `ExtendedType` | COBOL-2014: Boolean, FloatShort/Long/Extended, ObjectReference, National |
| `TypeDef` / `TypeDefRegistry` | User-defined TYPEDEF support with STRONG typing |
| `ExceptionCondition` | 40+ EC-* exception conditions (EC-SIZE, EC-BOUND, etc.) |
| `MemoryAllocator` | Heap management for ALLOCATE/FREE statements |

### Code Generation (feature `llvm`)

| Type | Description |
|---|---|
| `CodeGenerator<'ctx>` | LLVM IR generator: compile, write object/assembly |
| `DataLayout<'ctx>` | COBOL data item ↔ LLVM global mapping |
| `LlvmType<'ctx>` | COBOL USAGE to LLVM type mapping (ByteArray, Integer, Struct, Array) |
| `CodegenOptions` | Optimization level, target triple, debug info |

### Intrinsics

| Type | Description |
|---|---|
| `IntrinsicFunction` | Function definition: name, category, result type, arg bounds |
| `FunctionCategory` | String, Numeric, DateTime, General |
| 77+ public functions | Numeric (34), String (34+), DateTime (24+), General (2) |

### XML/JSON

| Type | Description |
|---|---|
| `JsonGenerateOptions` | pretty_print, suppress_nulls, camelCase, name mappings |
| `JsonValue` / `JsonParser` | JSON value tree and recursive descent parser |
| `XmlGenerateOptions` | with_declaration, namespace, encoding, pretty_print |
| `XmlEvent` / `XmlParser` | Event-driven XML parser (StartElement, EndElement, Content, etc.) |
| `CobolField` / `FieldType` | Tree structure for COBOL data → JSON/XML generation |

## Implementation Details

### Preprocessor Pipeline

The four-pass preprocessor runs before tokenization:

**Pass 0 — Compiler Options** (`compiler_options.rs`): Scans for `CBL`/`PROCESS`
statements on the first non-blank lines. Parses options like `ARITH(EXTEND)`,
`TRUNC(BIN)`, `NUMPROC(PFD)`, `NSYMBOL(NATIONAL)`, `INTDATE(LILIAN)`,
`CODEPAGE(1140)`. Abbreviated forms accepted (e.g., `AR` for `ARITH`).

**Pass 1 — Conditional Compilation** (`conditional.rs`): Stack-based state
machine that evaluates `>>DEFINE`, `>>IF`/`>>ELSE`/`>>END-IF`, and
`>>EVALUATE`/`>>WHEN`/`>>END-EVALUATE` directives. Supports conditions:
`DEFINED var`, `var = 'value'`, `NOT DEFINED var`. Also processes
`>>SET SOURCEFORMAT` directives for fixed/free format switching.

**Pass 2 — Copybook Expansion** (`preprocessor.rs` + `copybook.rs`): Expands
`COPY name [IN library] [REPLACING ...]` statements. Searches configured paths
with extensions (.cpy, .cbl, .cob). Detects circular inclusions via an
include stack. Supports `REPLACING` with Full, Leading, and Trailing modes
using `==pseudo-text==` delimiters. Also expands `DFHRESP(condition)` CICS
pseudo-functions to numeric codes.

**Pass 3 — REPLACE** (`replace.rs`): Processes `REPLACE ==from== BY ==to==`
and `REPLACE OFF` statements independently of COPY. Maintains stateful
replacement rules that persist across lines. Each new `REPLACE` statement
overwrites previous rules.

### Scanner / Tokenizer

The `Scanner` processes source line-by-line respecting fixed-format column
boundaries (sequence area cols 1-6, indicator col 7, code cols 8-72) or
free-format conventions. Character dispatch:

- Single-quoted strings with doubled-quote escaping
- `X"..."` hex literals, `N"..."` national literals
- Numeric literals distinguishing integers, decimals, and paragraph names (e.g., `2000-OUTFILE-OPEN`)
- PICTURE string recognition (sets `in_picture` flag after PIC/PICTURE keyword)
- `/* ... */` block comment stripping
- All COBOL operators and punctuation

### Parser

Recursive descent parser driven by macro-generated dispatch tables. The
`for_parse_dispatch!` macro maps keywords to parse functions, and
`for_all_statement_variants!` generates the `Statement` enum with 39 variants.

Parser supports all four COBOL divisions with full clause recognition:

- **IDENTIFICATION DIVISION**: PROGRAM-ID with COMMON/INITIAL, informational paragraphs
- **ENVIRONMENT DIVISION**: CONFIGURATION (SOURCE-COMPUTER, OBJECT-COMPUTER, SPECIAL-NAMES, REPOSITORY), INPUT-OUTPUT (FILE-CONTROL, I-O-CONTROL)
- **DATA DIVISION**: FILE SECTION (FD/SD), WORKING-STORAGE, LOCAL-STORAGE, LINKAGE; level numbers 01-49, 66 (RENAMES), 77, 88 (condition names)
- **PROCEDURE DIVISION**: USING/RETURNING, DECLARATIVES, sections, paragraphs

Expression parsing uses operator precedence: power > unary > multiplicative > additive.
Condition parsing handles AND/OR/NOT with comparison, class, and sign conditions.

Error recovery: consumes tokens until next period on parse errors, accumulating
diagnostics without aborting.

### Semantic Analysis

Three-phase architecture:

1. **Symbol table construction** — Walks DATA DIVISION to register all data
   items with types derived from PICTURE/USAGE. Calculates storage offsets.
   Registers paragraphs and sections from PROCEDURE DIVISION.

2. **Validation** — Walks all statements checking:
   - MOVE compatibility (alphanumeric receives anything; numeric requires numeric source)
   - COMPUTE targets must be numeric
   - PERFORM/GO TO targets must exist as paragraphs or sections
   - File references must match FILE SECTION entries
   - Expression type inference and validation
   - Qualified name resolution through parent chain

3. **Nested program analysis** — Recursively analyzes contained programs,
   propagating GLOBAL data items and registering COMMON siblings as callable
   targets.

### COBOL Type System

`CobolType` tracks category, byte size, decimal positions, usage, sign, and
occurs count. Storage size calculation follows IBM COBOL rules:

| Usage | Size Rule |
|---|---|
| DISPLAY | 1 byte per character |
| BINARY / COMP / COMP-4 | 2 bytes (1-4 digits), 4 bytes (5-9), 8 bytes (10-18) |
| PACKED-DECIMAL / COMP-3 | (digits + 2) / 2 bytes |
| COMP-1 | 4 bytes (single-precision float) |
| COMP-2 | 8 bytes (double-precision float) |
| COMP-5 | Native binary (same sizing as BINARY) |
| POINTER / FUNCTION-POINTER | 8 bytes (64-bit) |
| INDEX | 4 bytes |
| NATIONAL | 2 bytes per character (UTF-16) |
| DISPLAY-1 | 2 bytes per character (DBCS) |
| UTF-8 | 4 bytes per character (worst-case) |

Extended types (COBOL-2014): Boolean (1 byte), FloatShort (4),
FloatLong (8), FloatExtended (16), ObjectReference (8).

### Code Generation (LLVM)

Behind the `llvm` feature flag, `CodeGenerator` uses Inkwell bindings to:

1. Initialize LLVM targets and create a target machine
2. Generate LLVM globals for WORKING-STORAGE and LINKAGE data items
3. Generate a `main()` entry point
4. (TODO) Generate procedure code for statements
5. Verify the module and emit object files or assembly

Supports optimization levels (None/Less/Default/Aggressive) and multiple
target triples (host, x86_64-linux, aarch64-linux).

### Intrinsic Functions

77+ functions organized into four categories:

| Category | Count | Examples |
|---|---|---|
| Numeric | 34 | ABS, SIN, COS, SQRT, MOD, FACTORIAL, ANNUITY, PRESENT-VALUE |
| String | 34+ | TRIM, REVERSE, UPPER-CASE, NUMVAL, HEX-OF, UUID4, ULENGTH |
| DateTime | 24+ | CURRENT-DATE, INTEGER-OF-DATE, DATE-TO-YYYYMMDD, formatted ISO 8601 |
| General | 2 | UUID4, CONTENT-OF |

Notable semantics: COBOL `MOD` returns result with sign of divisor (differs
from Rust `%`). `RANDOM` uses a Linear Congruential Generator for
reproducibility. Ordinal functions return 1-based indices.

### XML/JSON Support

Implements the COBOL-2014 JSON GENERATE/PARSE and XML GENERATE/PARSE
statements:

- **JSON GENERATE**: Converts COBOL data tree to JSON with optional pretty
  printing, null/zero suppression, name mappings, and camelCase conversion
- **JSON PARSE**: Recursive descent JSON parser producing `JsonValue` tree
- **XML GENERATE**: Converts COBOL data tree to XML with optional declaration,
  namespace, encoding, and pretty printing
- **XML PARSE**: Event-driven parser producing `XmlEvent` stream (StartElement,
  EndElement, Content, Comment, ProcessingInstruction, CDATA)

## Syntax / Feature Coverage

### Procedure Division Statements (39 implemented)

| Statement | Status | Notes |
|---|---|---|
| MOVE | Implemented | MOVE ... TO, MOVE CORRESPONDING |
| COMPUTE | Implemented | Arithmetic expressions with ROUNDED, ON SIZE ERROR |
| ADD | Implemented | ADD ... TO / GIVING, ON SIZE ERROR |
| SUBTRACT | Implemented | SUBTRACT ... FROM / GIVING, ON SIZE ERROR |
| MULTIPLY | Implemented | MULTIPLY ... BY / GIVING, ON SIZE ERROR |
| DIVIDE | Implemented | DIVIDE ... INTO / BY / GIVING / REMAINDER |
| IF / ELSE / END-IF | Implemented | Nested, with scope terminators |
| EVALUATE / WHEN / END-EVALUATE | Implemented | ALSO, ANY, TRUE/FALSE, OTHER |
| PERFORM | Implemented | Named, inline, VARYING, UNTIL, AFTER |
| CALL / END-CALL | Implemented | BY REFERENCE/CONTENT/VALUE, RETURNING, ON EXCEPTION |
| DISPLAY | Implemented | UPON, WITH NO ADVANCING |
| ACCEPT | Implemented | FROM DATE/DAY/TIME/DAY-OF-WEEK |
| OPEN | Implemented | INPUT/OUTPUT/I-O/EXTEND |
| CLOSE | Implemented | Standard close |
| READ / END-READ | Implemented | NEXT, INTO, AT END, INVALID KEY |
| WRITE / END-WRITE | Implemented | ADVANCING (BEFORE/AFTER, lines/page) |
| REWRITE / END-REWRITE | Implemented | INVALID KEY handling |
| DELETE / END-DELETE | Implemented | INVALID KEY handling |
| START / END-START | Implemented | Key comparisons |
| STOP RUN | Implemented | With optional literal |
| GOBACK | Implemented | Return to caller |
| EXIT | Implemented | EXIT PROGRAM / PARAGRAPH |
| GO TO | Implemented | Simple and DEPENDING ON |
| INITIALIZE | Implemented | REPLACING with category |
| INSPECT | Implemented | TALLYING, REPLACING, CONVERTING, BEFORE/AFTER |
| STRING / END-STRING | Implemented | DELIMITED BY, INTO, POINTER, ON OVERFLOW |
| UNSTRING / END-UNSTRING | Implemented | DELIMITED BY, INTO, DELIMITER, COUNT, TALLYING |
| SET | Implemented | TO TRUE, UP/DOWN BY, ADDRESS |
| SEARCH / END-SEARCH | Implemented | Sequential and ALL (binary) |
| CANCEL | Implemented | Cancel loaded program |
| SORT | Implemented | ASCENDING/DESCENDING KEY, INPUT/OUTPUT PROCEDURE, USING/GIVING |
| MERGE | Implemented | Multi-file merge |
| RELEASE | Implemented | Release record to sort |
| RETURN / END-RETURN | Implemented | Return record from sort/merge |
| CONTINUE | Implemented | No-op statement |
| EXEC CICS / END-EXEC | Implemented | Embedded CICS commands |
| EXEC SQL / END-EXEC | Implemented | Embedded SQL statements |
| JSON GENERATE/PARSE | Implemented | Full JSON support |
| XML GENERATE/PARSE | Implemented | Full XML support |
| ALLOCATE / FREE | Implemented | COBOL-2014 dynamic memory |
| ENTRY | Implemented | Alternate entry point |
| ALTER | Implemented | Legacy GO TO modification |
| INVOKE | Implemented | Object-oriented method call |

### Data Division Features

| Feature | Status | Notes |
|---|---|---|
| Level numbers 01-49 | Implemented | Group and elementary items |
| Level 66 RENAMES | Implemented | RENAMES ... THRU |
| Level 77 | Implemented | Independent items |
| Level 88 condition names | Implemented | VALUE/VALUES, THRU ranges |
| PICTURE / PIC | Implemented | Full analysis: category, size, decimal positions |
| USAGE (all variants) | Implemented | DISPLAY, BINARY, COMP1-5, PACKED-DECIMAL, POINTER, INDEX, NATIONAL, UTF-8, DISPLAY-1 |
| OCCURS | Implemented | Fixed and variable (DEPENDING ON), ASCENDING/DESCENDING KEY, INDEXED BY |
| REDEFINES | Implemented | Overlay storage |
| VALUE | Implemented | Initial value assignment |
| SIGN | Implemented | LEADING/TRAILING, SEPARATE CHARACTER |
| JUSTIFIED RIGHT | Implemented | Right justification |
| BLANK WHEN ZERO | Implemented | Display formatting |
| SYNCHRONIZED | Implemented | Alignment (LEFT/RIGHT) |
| EXTERNAL / GLOBAL | Implemented | Sharing scope |
| FD / SD | Implemented | File and sort descriptions with LINAGE, BLOCK CONTAINS, RECORD CONTAINS |
| GROUP-USAGE NATIONAL | Implemented | National group items |

### Preprocessor Directives

| Directive | Status |
|---|---|
| `COPY ... [IN ...] [REPLACING ...]` | Implemented |
| `REPLACE ==...== BY ==...==` | Implemented |
| `REPLACE OFF` | Implemented |
| `>>DEFINE` | Implemented |
| `>>IF` / `>>ELSE` / `>>END-IF` | Implemented |
| `>>EVALUATE` / `>>WHEN` / `>>END-EVALUATE` | Implemented |
| `>>SET SOURCEFORMAT` | Implemented |
| `CBL` / `PROCESS` options | Implemented |

## Usage Examples

### Parse and Analyze a COBOL Program

```rust
use open_mainframe_cobol::{
    SourceManager, SourceFormat, scan, Parser, analyze,
};

// Load source
let mut sm = SourceManager::new();
let id = sm.add_text(source_text.to_string(), SourceFormat::Fixed);
let source = sm.get(id).unwrap();

// Tokenize
let (tokens, lex_errors) = scan(source);

// Parse
let (program, parse_errors) = Parser::parse(&tokens);

// Semantic analysis
if let Some(prog) = &program {
    let result = analyze(prog);
    for diag in &result.diagnostics {
        eprintln!("{}", diag.message);
    }
}
```

### Preprocess with Copybooks

```rust
use open_mainframe_cobol::lexer::{
    CopybookConfig, Preprocessor, SourceFormat,
};

let mut config = CopybookConfig::new();
config.add_path("/path/to/copybooks");

let mut pp = Preprocessor::new(config, SourceFormat::Fixed);
let expanded = pp.preprocess(source_text)?;
```

### JSON Generation from COBOL Data

```rust
use open_mainframe_cobol::xml_json::{
    CobolField, JsonGenerateOptions, json_generate,
};

let mut root = CobolField::group("CUSTOMER-RECORD", 1);
root.add_child(CobolField::alphanumeric("CUST-NAME", 5, "SMITH"));
root.add_child(CobolField::numeric("CUST-BALANCE", 5, "1500.00"));

let options = JsonGenerateOptions { pretty_print: true, ..Default::default() };
let json = json_generate(&root, &options)?;
```

### Evaluate Intrinsic Functions

```rust
use open_mainframe_cobol::intrinsics::{numeric, datetime, string};

let result = numeric::factorial(10);       // 3628800.0
let today = datetime::current_date();      // "20260225120000000+0000"
let trimmed = string::trim("  HELLO  ", string::TrimDirection::Both); // "HELLO"
```

## Dependencies

| Crate | Purpose |
|---|---|
| `open-mainframe-lang-core` | Shared `Span`, `FileId`, `Location`, `Diagnostic`, `AstNode` |
| `open-mainframe-encoding` | EBCDIC ↔ ASCII codepage conversion |
| `open-mainframe-runtime` | Runtime support library |
| `thiserror` | Derive `Error` for all error types |
| `miette` | Rich diagnostic error reporting with source annotations |
| `bumpalo` | Arena allocator for AST nodes |
| `inkwell` | LLVM bindings (optional, behind `llvm` feature flag) |

## Testing

Each module contains its own `#[cfg(test)]` section. Approximate test counts:

| Module | Tests | Covers |
|---|---|---|
| `lexer/scanner.rs` | 12 | Keywords, literals, operators, PICTURE, hex |
| `lexer/keywords.rs` | 3 | Keyword lookup, non-keywords |
| `lexer/source.rs` | 7 | Fixed/free format, continuation, source manager |
| `lexer/preprocessor.rs` | 8 | COPY expansion, REPLACING, comments |
| `lexer/copybook.rs` | 12 | Path resolution, circular detection, replacement modes |
| `lexer/replace.rs` | 19 | Pseudo-text matching, REPLACE OFF, multi-line |
| `lexer/conditional.rs` | 35 | >>IF/>>EVALUATE nesting, >>SET, error cases |
| `lexer/compiler_options.rs` | 19 | CBL/PROCESS parsing, option values, abbreviations |
| `parser/mod.rs` | 12 | Minimal programs, nested programs, repository, entry/alter/invoke |
| `semantic/analyzer.rs` | 5 | Symbol table build, undefined references, qualified names |
| `semantic/symbol_table.rs` | 4 | Data items, qualified lookup, paragraphs |
| `semantic/types.rs` | 3 | Type categories, MOVE compatibility, storage sizes |
| `semantic/types_2014.rs` | 7 | Extended types, booleans, TYPEDEF, exceptions, allocator |
| `codegen/*.rs` | 5 | CodeGenerator creation, options, target triples, data layout |
| `intrinsics/mod.rs` | 14 | Function registry, categories, argument validation |
| `intrinsics/numeric.rs` | 18 | Math functions, MOD/REM, financial functions |
| `intrinsics/datetime.rs` | 24 | Date conversion, leap years, ISO 8601 |
| `intrinsics/string.rs` | 28 | String operations, NUMVAL, UTF-8, encoding |
| `xml_json/mod.rs` | 2 | Field creation, error display |
| `xml_json/json.rs` | 24 | JSON generation/parsing, escaping, camelCase |
| `xml_json/xml.rs` | 8 | XML generation/parsing, attributes, CDATA |

```sh
cargo test -p open-mainframe-cobol
# With LLVM code generation:
cargo test -p open-mainframe-cobol --features llvm
```

## Limitations and Future Work

- **Code generation** is partially implemented — generates LLVM module structure
  and data layout but procedure code generation (statement → IR) is still TODO
- **PERFORM VARYING** inline body execution is parsed but runtime semantics
  rely on an interpreter (not yet in this crate)
- **Nested COPY** expansion depth is capped (configurable) but default is modest
- **88-level condition evaluation** is parsed and validated but runtime evaluation
  requires an interpreter
- **EXEC CICS / EXEC SQL** contents are captured as raw strings; actual
  preprocessing is deferred to `open-mainframe-precompilers`
- **Report Writer** (GENERATE, INITIATE, TERMINATE) is not yet implemented
- **Communication Section** is not implemented
- **Object-oriented COBOL** (CLASS-ID, FACTORY, OBJECT) is not yet supported
  beyond the INVOKE statement
- **Debugging mode** (WITH DEBUGGING MODE) is recognized but debug lines are
  not conditionally included at runtime
- **ARITH(EXTEND)** 31-digit precision is declared but not enforced in the
  intrinsic function implementations (they use f64)
- **National / UTF-16** data items are typed but runtime encoding conversion
  is delegated to `open-mainframe-encoding`
