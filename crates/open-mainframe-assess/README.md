# open-mainframe-assess

A Rust toolkit for assessing mainframe COBOL codebases prior to migration. Provides static analysis (text-based and AST-based), code metrics, compatibility checking, CICS command inventory, DB2 SQL complexity analysis, dead code detection, call graph construction, JCL dependency mapping, batch directory scanning with copybook resolution, multi-format report generation, and assessment snapshot comparison for tracking migration progress.

## Overview

Migrating legacy mainframe COBOL applications to the OpenMainframe runtime requires a thorough understanding of what each program uses: which language features, which middleware (CICS, DB2, IMS), which datasets, and how complex the code actually is. This crate automates that assessment.

The crate offers two analysis paths. A text-based `Analyzer` scans source lines for keyword patterns — fast and tolerant of any input. An AST-based `AstAnalyzer` leverages the `open-mainframe-cobol` parser to walk the actual syntax tree for precise feature detection, cyclomatic complexity, and data-item counting, with automatic fallback to text-based analysis when parsing fails.

Beyond per-program analysis, the crate provides batch scanning (`Scanner`) that discovers COBOL files in a directory tree, resolves `COPY` copybook includes, runs analysis on each file, and aggregates results into a single report. Reports can be generated in Text, Markdown, JSON, or HTML format. Assessment snapshots can be serialized to JSON and compared across runs to track migration progress over time.

## Architecture

```
                       +----------------+
                       |    Scanner     |
                       | (batch scan)   |
                       +-------+--------+
                               |
               +---------------+---------------+
               |                               |
       +-------+-------+              +--------+-------+
       |  AstAnalyzer  |              | CopybookResolver|
       | (AST-first)   |              |  (COPY inline)  |
       +-------+-------+              +----------------+
               |
     +---------+---------+
     |                   |
+----+----+       +------+------+
| Analyzer|       | open-mainframe-cobol |
|(text)   |       |   (parser)  |
+---------+       +-------------+
     |
     +-------+--------+--------+---------+
     |       |        |        |         |
+----+-+ +---+---+ +--+---+ +-+------+ +-+--------+
|Compat| |Metrics| |Report| |Features| |Complexity|
|Checker| |       | |      | |detect  | |scoring   |
+------+ +-------+ +------+ +--------+ +----------+

Additional stand-alone analyzers:
+-------------+ +-----------+ +---------+ +----------+ +---------+
|CicsInventory| |SqlAnalysis| |DeadCode | |CallGraph | |JclDeps  |
+-------------+ +-----------+ +---------+ +----------+ +---------+
```

### Module Structure

| Module | Description |
|--------|-------------|
| `analyzer` | Text-based COBOL source analyzer: `Analyzer`, `AnalysisResult`, `Feature`, feature pattern matching, metrics calculation, complexity scoring, recommendation generation |
| `ast_analyzer` | AST-based analyzer: `AstAnalyzer` parses via `open-mainframe-cobol`, walks the AST for accurate feature/metric extraction, falls back to text-based analysis on parse failure |
| `callgraph` | Call graph builder: `CallGraph`, `CallEdge`, `CallType` (static/dynamic/CICS LINK/XCTL), cycle detection via DFS, topological sort via Kahn's algorithm, source text extraction |
| `cics_inventory` | CICS command inventory: `CicsInventory`, `CicsCommand`, `CicsCategory` (12 categories), `SupportStatus` (Supported/Partial/Unsupported), source scanning and classification |
| `compatibility` | Compatibility checker: `CompatibilityChecker` with 10 default rules, `CompatibilityRule`, `CompatibilityIssue`, `Severity` (Info/Warning/High/Critical), `FeatureSupport` catalog |
| `dead_code` | Dead code detection: `detect_dead_code`, `DeadCodeReport`, `DeadCodeItem`, paragraph/section identification, PERFORM/GO TO/THRU reference tracking |
| `jcl_deps` | JCL dependency analysis: `analyze_jcl`, `JclDependencyMap`, `JclJob`, `JclStep`, `DdStatement`, program-to-dataset and dataset-to-program cross-reference maps |
| `metrics` | Code metrics: `CodeMetrics`, `ComplexityLevel`, `calculate_maintainability_index`, `estimate_technical_debt`, line counts, cyclomatic complexity, comment ratio, statements per paragraph |
| `report` | Report generation: `Report`, `ReportFormat` (Text/Markdown/JSON/HTML), executive summary, per-file details, issue listing, recommendations, feature support table |
| `scanner` | Batch scanner: `Scanner`, `ScanConfig`, `ScanEntry`, `ScanResult`, recursive directory walking, glob pattern filtering, copybook resolution from configurable include paths |
| `snapshot` | Snapshot comparison: `AssessmentSnapshot`, `ProgramSnapshot`, `SnapshotDiff`, `ProgressSummary`, `ComplexityChange`, JSON serialization round-trip, migration progress tracking |
| `sql_analysis` | DB2 SQL analysis: `SqlAnalysis`, `SqlStatement`, `SqlComplexity` (Simple/Join/Subquery/Cursor/Dynamic/Utility), PostgreSQL compatibility notes, effort scoring |

## Key Types and Traits

### Core Analysis
- `Analyzer` — Text-based COBOL source analyzer with feature pattern matching and compatibility checking
- `AstAnalyzer` — AST-based analyzer using the `open-mainframe-cobol` parser, with automatic fallback
- `AnalysisResult` — Per-file result: program ID, metrics, features, issues, complexity rating, recommendations
- `Feature` — Detected feature with name, category, occurrence count, and line numbers
- `FeatureCategory` — `CoreLanguage`, `FileHandling`, `Database`, `Transaction`, `PlatformSpecific`, `Interoperability`, `Batch`
- `MigrationComplexity` — `Low`, `Medium`, `High`, `VeryHigh` with effort multipliers (1.0x / 2.0x / 3.5x / 5.0x)

### Metrics
- `CodeMetrics` — Total/code/blank/comment lines, executable statements, cyclomatic complexity, paragraph count, data items, division presence flags
- `ComplexityLevel` — `Low` (<= 10), `Moderate` (11-20), `High` (21-50), `VeryHigh` (50+)
- `calculate_maintainability_index` — Simplified MI formula: 171 - 5.2*ln(HV) - 0.23*CC - 16.2*ln(LOC), clamped to 0-100
- `estimate_technical_debt` — Base hours (1hr per 100 LOC) + complexity penalty + documentation penalty

### Compatibility
- `CompatibilityChecker` — Rule-based checker with 10 default rules (IMS/DL1, console display, date/time, relative files, external assignments, ALTER, GO TO DEPENDING, ENTRY, BY CONTENT LENGTH, SEARCH ALL)
- `CompatibilityIssue` — Issue with code, description, severity, category, optional line number, recommendation
- `Severity` — Ordered: `Info` < `Warning` < `High` < `Critical`
- `FeatureSupport` — Catalog of 10 feature areas with support boolean, level (0-100), and notes

### CICS Inventory
- `CicsInventory` — Scans COBOL source for EXEC CICS commands, classifies by category and support status
- `CicsCommand` — Verb, category, count, line numbers, support status
- `CicsCategory` — 12 categories: FileControl, TerminalControl, ProgramControl, QueueControl, IntervalControl, TaskControl, StorageControl, BmsControl, JournalControl, SyncpointControl, ExceptionHandling, Other
- `SupportStatus` — `Supported`, `Partial`, `Unsupported` — maps each CICS verb to its support level in `open-mainframe-cics`

### SQL Analysis
- `SqlAnalysis` — Extracts EXEC SQL blocks, classifies complexity, computes effort scores
- `SqlStatement` — Verb, complexity level, line number, PostgreSQL compatibility notes
- `SqlComplexity` — 6 levels with effort weights: Simple (1.0), Join (1.5), Subquery (2.0), Cursor (2.5), Dynamic (3.0), Utility (1.0)

### Call Graph
- `CallGraph` — Directed graph with adjacency list, supports adding programs/edges, cycle detection (DFS), topological sort (Kahn's)
- `CallEdge` — Caller, callee, call type, uncertain flag
- `CallType` — `StaticCall`, `DynamicCall`, `CicsLink`, `CicsXctl`

### Dead Code
- `detect_dead_code` — Scans procedure division for paragraph/section definitions and references (PERFORM, GO TO, THRU/THROUGH, INPUT/OUTPUT PROCEDURE), flags unreachable code
- `DeadCodeReport` — Total paragraphs, dead items, dead lines, dead percentage, adjusted line count
- `DeadCodeItem` — Name, line, line count, kind (Paragraph/Section)

### JCL Dependencies
- `analyze_jcl` — Parses JCL job streams into `JclDependencyMap`
- `JclDependencyMap` — Jobs, program-to-job mapping, program-to-dataset mapping, dataset-to-program mapping, shared dataset detection
- `JclJob` / `JclStep` / `DdStatement` — Job/step/DD hierarchy with PGM, PROC, DSN, DISP, I/O type classification

### Scanning & Reporting
- `Scanner` — Batch scanner: discovers COBOL files, resolves copybooks, analyzes each, produces a `Report`
- `ScanConfig` — Root directory, include patterns (glob), source extensions, copybook paths, recursive flag
- `Report` — Aggregated results with Text/Markdown/JSON/HTML generation
- `AssessmentSnapshot` — Serializable assessment run for progress tracking
- `SnapshotDiff` / `ProgressSummary` — Comparison between snapshots: resolved programs, new programs, complexity changes, migration progress percentage, remaining effort

## Implementation Details

### Text-Based vs AST-Based Analysis
The `Analyzer` (text-based) performs line-by-line keyword matching against configurable `FeaturePattern` lists. It's fast and never fails, but can produce false positives (e.g., EXEC SQL in a comment). The `AstAnalyzer` parses source via `open-mainframe-cobol` (`scan` + `Parser::parse_program`), then walks the AST using `collect_all_statements` which recursively descends through `ProcedureBody::Sections/Paragraphs/Statements` and nested `If`/`Evaluate`/`Perform`/`Compute`/`Read` blocks. When parsing fails, `AstAnalyzer` falls back to `Analyzer` with a warning recommendation.

### Complexity Scoring
Both analyzers use a point-based scoring system:
- Code size: >500 lines (+1), >2000 (+2), >5000 (+3)
- Cyclomatic complexity: >10 (+1), >20 (+2), >50 (+3)
- Feature categories: Database (+2), Transaction (+2), PlatformSpecific (+3), FileHandling (+1)
- Issue severity: Critical (+3 each), High (+2 each)
- Total maps to: 0-3 = Low, 4-7 = Medium, 8-12 = High, 13+ = VeryHigh

### Search Buffer / CICS Classification
`CicsInventory::from_source` does a two-pass approach: first extracts the verb (handling two-word verbs like "SEND MAP", "WRITEQ TS", etc.), then classifies via `classify_verb` into 12 categories and `support_status` into 3 support levels, all based on match tables.

### SQL Complexity Detection
`SqlAnalysis::from_source` collects multi-line EXEC SQL...END-EXEC blocks, then `classify_sql` determines the verb and base complexity, then upgrades Simple to Join (if contains " JOIN ") or Subquery (if contains "(SELECT" or "EXISTS("). DB2-specific constructs (WITH UR/CS/RS/RR, OPTIMIZE FOR, FOR UPDATE OF) generate PostgreSQL compatibility notes.

### Call Graph Algorithms
- **Cycle detection**: DFS with on-stack tracking; back edges indicate cycles, which are extracted from the stack
- **Topological sort**: Kahn's algorithm (BFS from zero-in-degree nodes), then reversed to produce leaf-first order for migration planning
- **Source extraction**: Text-based pattern matching for `CALL 'literal'`, `CALL variable`, `EXEC CICS LINK PROGRAM('name')`, `EXEC CICS XCTL PROGRAM('name')`

### Copybook Resolution
`Scanner::resolve_copybooks` scans for `COPY <name>.` lines, searches configured include paths with copybook extensions (.cpy, .CPY, .copy, .COPY), source extensions (.cbl, .cob), and bare names, then inlines the first match. Already-resolved names are tracked via `HashSet` to prevent infinite recursion.

## Syntax / Feature Coverage

### Detected COBOL Features

| Feature | Category | Detection |
|---------|----------|-----------|
| VSAM (ORGANIZATION IS INDEXED) | FileHandling | Implemented (text + AST) |
| Sequential Files | FileHandling | Implemented (text + AST) |
| Relative Files | FileHandling | Implemented (AST only) |
| DB2 (EXEC SQL) | Database | Implemented (text + AST) |
| IMS (EXEC DLI) | Database | Implemented (text only) |
| CICS (EXEC CICS) | Transaction | Implemented (text + AST) |
| Subprogram Calls (CALL) | Interoperability | Implemented (text + AST) |
| COPY Statements | Interoperability | Implemented (text only) |
| DISPLAY | CoreLanguage | Implemented (text + AST) |
| ACCEPT | CoreLanguage | Implemented (text + AST) |
| STRING/UNSTRING | CoreLanguage | Implemented (text + AST) |
| INSPECT | CoreLanguage | Implemented (text + AST) |
| COMPUTE | CoreLanguage | Implemented (text + AST) |
| SORT/MERGE | Batch | Implemented (AST only) |

### Compatibility Rules (Default)

| Code | Pattern | Severity | Category |
|------|---------|----------|----------|
| PLAT001 | EXEC DLI | Critical | Database |
| PLAT002 | UPON CONSOLE | Warning | PlatformSpecific |
| PLAT003 | ACCEPT FROM DATE | Info | PlatformSpecific |
| PLAT004 | ACCEPT FROM TIME | Info | PlatformSpecific |
| FILE001 | ORGANIZATION IS RELATIVE | High | FileHandling |
| FILE002 | ASSIGN TO EXTERNAL | Warning | FileHandling |
| DEPR001 | ALTER | High | CoreLanguage |
| DEPR002 | GO TO DEPENDING | Warning | CoreLanguage |
| DEPR003 | ENTRY | Warning | Interoperability |
| CALL001 | CALL USING BY CONTENT LENGTH | Warning | Interoperability |
| PERF001 | SEARCH ALL | Info | CoreLanguage |

### Report Formats

| Format | Status |
|--------|--------|
| Plain Text | Implemented |
| Markdown | Implemented |
| JSON | Implemented |
| HTML | Implemented |

## Usage Examples

```rust
use open_mainframe_assess::analyzer::Analyzer;
use open_mainframe_assess::ast_analyzer::AstAnalyzer;
use open_mainframe_assess::report::{Report, ReportFormat};
use open_mainframe_assess::scanner::{ScanConfig, Scanner};
use open_mainframe_assess::cics_inventory::CicsInventory;
use open_mainframe_assess::sql_analysis::SqlAnalysis;
use open_mainframe_assess::dead_code::detect_dead_code;
use open_mainframe_assess::callgraph::{CallGraph, CallType};
use open_mainframe_assess::jcl_deps::analyze_jcl;
use open_mainframe_assess::snapshot::{AssessmentSnapshot, compare_snapshots};

// --- Text-based analysis ---
let analyzer = Analyzer::new();
let result = analyzer.analyze(cobol_source, "CUSTINQ.cbl").unwrap();
println!("{}", result.summary());

// --- AST-based analysis (preferred) ---
let ast_analyzer = AstAnalyzer::new();
let result = ast_analyzer.analyze(cobol_source, "CUSTINQ.cbl").unwrap();

// --- Batch directory scan ---
let config = ScanConfig::new("/path/to/cobol/src")
    .with_copybook_path("/path/to/copybooks")
    .with_pattern("src/**/*.cbl");
let scanner = Scanner::new(config);
let scan_result = scanner.scan().unwrap();
let report = scan_result.report;
println!("{}", report.generate(ReportFormat::Markdown));

// --- CICS command inventory ---
let inventory = CicsInventory::from_source(cobol_source);
println!("Total CICS commands: {}", inventory.total_count);
println!("Supported: {}", inventory.supported_count());

// --- DB2 SQL analysis ---
let sql = SqlAnalysis::from_source(cobol_source);
println!("SQL statements: {}, effort score: {:.1}", sql.total_count, sql.effort_score);

// --- Dead code detection ---
let dead = detect_dead_code("CUSTINQ", cobol_source);
println!("Dead paragraphs: {} ({:.1}%)", dead.dead_items.len(), dead.dead_percentage());

// --- Call graph ---
let mut graph = CallGraph::new();
let edges = CallGraph::extract_from_source("MAINPROG", cobol_source);
for edge in edges {
    graph.add_edge(&edge.caller, &edge.callee, edge.call_type);
}
if let Some(order) = graph.topological_sort() {
    println!("Migration order: {:?}", order);
}

// --- JCL dependency analysis ---
let dep_map = analyze_jcl(jcl_source);
println!("Programs: {:?}", dep_map.all_programs());
println!("Shared datasets: {:?}", dep_map.shared_dataset_programs());

// --- Snapshot comparison ---
let baseline = AssessmentSnapshot::from_json(&baseline_json).unwrap();
let current = AssessmentSnapshot::from_json(&current_json).unwrap();
let diff = compare_snapshots(&baseline, &current);
println!("Progress: {:.1}%", diff.progress.progress_percent);
```

## Dependencies

| Dependency | Purpose |
|------------|---------|
| `open-mainframe-cobol` | COBOL parser for AST-based analysis (lexer, parser, AST types) |
| `miette` | Diagnostic error reporting |
| `thiserror` | Ergonomic error type derivation for `AssessError` |
| `serde` / `serde_json` | Serialization for reports, snapshots, and analysis results |

## Testing

Run the full test suite:

```sh
cargo test -p open-mainframe-assess
```

Each module contains comprehensive inline `#[cfg(test)]` test blocks:

- **analyzer**: Basic analysis, metrics calculation, DB2/CICS feature detection, complexity rating, recommendation generation
- **ast_analyzer**: Basic AST parsing, comment false-positive avoidance, DISPLAY detection, cyclomatic complexity with IF statements, paragraph counting, graceful fallback on parse failure
- **callgraph**: Graph edges, topological sort, cycle detection, static/dynamic call extraction, CICS LINK/XCTL extraction, single-node graph
- **cics_inventory**: Command classification, support status tracking, category grouping, comment skipping, multiple occurrences, category/status display names
- **compatibility**: Rule matching, severity ordering, IMS detection, custom rule addition, feature support catalog, issue builder pattern
- **dead_code**: Dead paragraph detection, no false positives on all-live code, PERFORM THRU range marking, GO TO references, section dead code, percentage calculation, entry point exemption
- **jcl_deps**: Simple job parsing, step-to-program mapping, dataset dependencies, multi-step jobs, DD I/O classification, comment skipping, shared datasets, multiple jobs, job class extraction
- **metrics**: Complexity level thresholds, code metrics calculation, maintainability index range, technical debt comparison, summary generation
- **report**: Report creation, Text/Markdown/JSON/HTML generation, empty report handling
- **scanner**: File discovery, recursive/non-recursive scanning, glob filtering, full scan end-to-end, non-existent directory error, copybook resolution, missing copybook tolerance, analysis result verification (uses temp directories)
- **snapshot**: JSON round-trip serialization, migrated count, resolved programs, complexity changes, newly migrated detection, progress summary, new program detection, remaining effort calculation, empty snapshots
- **sql_analysis**: Simple queries, cursor operations, JOIN detection, subquery detection, dynamic SQL, PostgreSQL compatibility notes (WITH HOLD, WHENEVER, isolation clauses), effort scoring

Scanner tests create and clean up temporary directories; all other tests use inline source strings.

## Limitations and Future Work

- **IMS/DL1 detection**: Only text-based (EXEC DLI pattern match); no AST-level analysis since IMS precompiler syntax is not parsed by `open-mainframe-cobol`
- **Dead code analysis**: Only tracks explicit references (PERFORM, GO TO, THRU/THROUGH, INPUT/OUTPUT PROCEDURE). Fall-through execution between paragraphs is not modeled, so a paragraph reachable only by fall-through from the preceding paragraph will be flagged as dead.
- **CICS support status mapping**: The `support_status` function is a static snapshot of `open-mainframe-cics` capabilities and may drift as that crate evolves.
- **SQL analysis**: Only detects EXEC SQL blocks embedded in COBOL. Standalone SQL scripts or stored procedures are not analyzed. The PostgreSQL compatibility notes cover common DB2 idioms but not exhaustive DB2/zOS SQL dialect differences.
- **Call graph**: Dynamic calls (CALL variable) are marked as uncertain; the actual target cannot be resolved statically.
- **Copybook resolution**: Single-level only; nested COPY statements within copybooks are not recursively resolved.
- **Cyclomatic complexity (text mode)**: Approximated via keyword counting; may overcount due to patterns appearing in data names or string literals. AST mode is more accurate.
- **Report formats**: HTML report includes basic styling but no interactive features (charts, filtering).
- **Snapshot comparison**: Progress tracking is program-level; no intra-program change tracking (e.g., which specific issues were resolved).
