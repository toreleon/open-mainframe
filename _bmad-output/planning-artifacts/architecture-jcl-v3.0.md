# JCL Crate — Architecture Decisions

## AD-3.0-01: Procedure Expansion as a Preprocessing Pass

**Context:** Cataloged and in-stream procedures must be expanded before job execution. Procedure expansion involves resolving EXEC PROC=name, substituting symbolic parameters, applying DD overrides, and handling nested procedures up to 15 levels deep.

**Decision:** Implement procedure expansion as a separate preprocessing pass that runs between parsing and execution. The `Parser` produces a raw AST with `ExecType::Procedure(name)` nodes. A new `ProcedureExpander` resolves these against a `ProcedureLibrary` trait (abstracting PDS access), substitutes symbolics, and produces a fully-expanded `Job` with only `ExecType::Program` nodes.

**Consequences:**
- Clean separation of concerns — parser stays simple, expander handles complexity
- Enables testing expansion independently of parsing and execution
- Requires a `ProcedureLibrary` trait that can be backed by filesystem (PDS directories) or in-memory (for tests)
- Nested procedure handling requires recursion with a depth counter (max 15)

## AD-3.0-02: Symbolic Parameter Resolution Strategy

**Context:** JCL symbolic parameters (&NAME) must be resolved throughout the JCL text. SET statements define values, PROC statements define defaults, and EXEC statements override values. Double ampersand (&&) creates temporary dataset names. Period following a symbolic (&HLQ..DATA) must be handled specially.

**Decision:** Implement a `SymbolTable` (HashMap<String, String>) that is populated in order: (1) PROC defaults, (2) SET statements, (3) EXEC overrides. Resolution runs as a text-level pass on operand strings before token parsing, similar to how COBOL's COPY REPLACING works at the text level.

**Consequences:**
- Text-level substitution is simpler and matches IBM behavior (substitution happens before parsing)
- Double ampersand must be resolved to single ampersand after first pass (for temporary datasets)
- Must handle the double-period case: &HLQ..DATA where HLQ=MY resolves to MY.DATA (period is a separator, not part of the symbolic)

## AD-3.0-03: IF/THEN/ELSE/ENDIF as Control Flow AST Nodes

**Context:** IF/THEN/ELSE/ENDIF constructs control which steps execute based on return codes. They can be nested up to 15 levels. The current parser skips these statements entirely.

**Decision:** Add `IfConstruct` to the AST representing the full IF/THEN/ELSE/ENDIF block. The executor evaluates conditions at runtime against step results. Parse conditions into an expression tree supporting RC, ABENDCC, ABEND, RUN keywords with relational and logical operators.

**Consequences:**
- Job AST changes from a flat list of steps to a tree structure with conditional branches
- Executor needs a condition evaluator that accesses the step result history
- Nesting support requires recursive parsing with depth tracking

## AD-3.0-04: Executor Architecture — Utility Program Registry

**Context:** IBM z/OS includes many system utility programs (IEFBR14, IEBGENER, IEBCOPY, IDCAMS, etc.) that are referenced in JCL EXEC PGM= statements. The current executor hard-codes IEFBR14 and SORT handling.

**Decision:** Create a `UtilityRegistry` trait with a `HashMap<String, Box<dyn UtilityProgram>>` mapping program names to handlers. Each utility implements a `UtilityProgram` trait with `fn execute(&self, step: &Step, dd_files: &DdFiles) -> Result<StepResult>`. Built-in utilities are registered by default; users can register custom ones.

**Consequences:**
- Extensible — new utilities can be added without modifying executor core
- Each utility is independently testable
- SORT integration moves from inline code to a registered utility
- External programs (not in registry) still execute via Command::new as today

## AD-3.0-05: Error Reporting with Span Information

**Context:** Current errors are plain strings without source location. Users need line numbers and column information to debug JCL syntax errors.

**Decision:** Enhance `JclError` to carry `Option<Span>` for source location. The `JclStatement` already tracks `byte_offset` and `byte_end` — propagate these through parse errors. Add a `display_with_source` method that shows the relevant source line with a caret pointing to the error.

**Consequences:**
- All parser methods must propagate span information through error paths
- Error display becomes more complex but much more useful
- Existing tests may need minor updates if error message format changes

## AD-3.0-06: GDG Resolution via Dataset Catalog

**Context:** GDG (Generation Data Group) references like MY.GDG(+1) and MY.GDG(0) are relative references that must be resolved against a catalog of existing generations.

**Decision:** Add a `DatasetCatalog` trait that provides `resolve_gdg(base: &str, generation: i32) -> Result<String>`. The default implementation uses the filesystem (scanning directory entries to find the latest generation). The catalog is consulted during DD resolution in the executor.

**Consequences:**
- GDG resolution is deferred to execution time (not parse time)
- Requires a naming convention for generation datasets on the filesystem (e.g., MY.GDG.G0001V00)
- Enables mock catalogs for testing
