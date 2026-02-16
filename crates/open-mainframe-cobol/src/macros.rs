//! Macro definitions for generating COBOL keyword and statement boilerplate.
//!
//! These callback-style macros define the master lists of keywords and statements.
//! Each consumer module provides its own processing macro to generate the
//! appropriate code (enum definitions, dispatch tables, HashMaps, etc.).
//!
//! # Adding a new keyword
//!
//! Add one line to [`for_all_keywords!`] in the `@primary` section (or `@alias`
//! for contextual keywords not in the lexer lookup map).
//!
//! # Adding a new statement
//!
//! 1. Add one line to [`for_all_statement_variants!`] with the variant name and
//!    struct type.
//! 2. Add one line to [`for_parse_dispatch!`] with the keyword(s) and parse
//!    function name.
//! 3. Define the struct in `ast/mod.rs` (must have a `span: Span` field).
//! 4. Write the `parse_xxx_statement()` method in `parser/mod.rs`.
//! 5. Add a match arm in `semantic/analyzer.rs` (the compiler will remind you).
//! 6. Add a match arm in the interpreter (the compiler will remind you).
//!
//! That's it — the enum variant, `span()` arm, parse dispatch, and
//! `is_statement_start()` are all generated automatically.

// ═══════════════════════════════════════════════════════════════════════════
// KEYWORD DEFINITIONS
// ═══════════════════════════════════════════════════════════════════════════

/// Master keyword definition table.
///
/// Invokes `$mac` with all COBOL keyword definitions in two groups:
///
/// - `@primary`: Keywords that appear in the lexer lookup HashMap, the `Keyword`
///   enum, AND the `as_str()` method. The string literal is used as both the
///   HashMap key and the `as_str()` return value.
///
/// - `@alias`: Contextual keyword variants that appear in the `Keyword` enum
///   and `as_str()` only (NOT in the HashMap). These are constructed by the
///   parser from multi-token phrases (e.g., `NotAtEnd` from "NOT AT END").
macro_rules! for_all_keywords {
    ($mac:ident) => {
        $mac! {
            @primary {
                // ── Division / Section ──────────────────────────────────
                Identification  => "IDENTIFICATION",
                Environment     => "ENVIRONMENT",
                Data            => "DATA",
                Procedure       => "PROCEDURE",
                Division        => "DIVISION",
                Section         => "SECTION",

                // ── Identification Division ─────────────────────────────
                ProgramId       => "PROGRAM-ID",
                Author          => "AUTHOR",
                Installation    => "INSTALLATION",
                DateWritten     => "DATE-WRITTEN",
                DateCompiled    => "DATE-COMPILED",
                Security        => "SECURITY",

                // ── Environment Division ────────────────────────────────
                Configuration       => "CONFIGURATION",
                SourceComputer      => "SOURCE-COMPUTER",
                ObjectComputer      => "OBJECT-COMPUTER",
                SpecialNames        => "SPECIAL-NAMES",
                InputOutput         => "INPUT-OUTPUT",
                FileControl         => "FILE-CONTROL",
                Select              => "SELECT",
                Assign              => "ASSIGN",
                Organization        => "ORGANIZATION",
                Sequential          => "SEQUENTIAL",
                AccessMode          => "ACCESS",
                Random              => "RANDOM",
                Dynamic             => "DYNAMIC",
                AlternateRecordKey  => "ALTERNATE",
                Relative            => "RELATIVE",

                // ── Data Division ───────────────────────────────────────
                File            => "FILE",
                Working         => "WORKING",
                Storage         => "STORAGE",
                Status          => "STATUS",
                WorkingStorage  => "WORKING-STORAGE",
                LocalStorage    => "LOCAL-STORAGE",
                Linkage         => "LINKAGE",
                Fd              => "FD",
                Sd              => "SD",
                Pic             => "PIC",
                Picture         => "PICTURE",
                Value           => "VALUE",
                Values          => "VALUES",
                Redefines       => "REDEFINES",
                Occurs          => "OCCURS",
                Times           => "TIMES",
                Indexed         => "INDEXED",
                By              => "BY",
                Ascending       => "ASCENDING",
                Descending      => "DESCENDING",
                Key             => "KEY",
                Depending       => "DEPENDING",
                On              => "ON",

                // ── Usage clauses ───────────────────────────────────────
                Comp            => "COMP",
                Comp1           => "COMP-1",
                Comp2           => "COMP-2",
                Comp3           => "COMP-3",
                Comp4           => "COMP-4",
                Comp5           => "COMP-5",
                Computational   => "COMPUTATIONAL",
                Computational1  => "COMPUTATIONAL-1",
                Computational2  => "COMPUTATIONAL-2",
                Computational3  => "COMPUTATIONAL-3",
                Computational4  => "COMPUTATIONAL-4",
                Computational5  => "COMPUTATIONAL-5",
                Binary          => "BINARY",
                PackedDecimal   => "PACKED-DECIMAL",
                Display         => "DISPLAY",
                Usage           => "USAGE",

                // ── Sign / Justify ──────────────────────────────────────
                Sign            => "SIGN",
                Leading         => "LEADING",
                Trailing        => "TRAILING",
                Separate        => "SEPARATE",
                Character       => "CHARACTER",
                Justified       => "JUSTIFIED",
                Just            => "JUST",
                Right           => "RIGHT",
                Left            => "LEFT",
                Blank           => "BLANK",
                When            => "WHEN",

                // ── Figurative constants ────────────────────────────────
                Zero            => "ZERO",
                Zeros           => "ZEROS",
                Zeroes          => "ZEROES",
                Space           => "SPACE",
                Spaces          => "SPACES",
                HighValue       => "HIGH-VALUE",
                HighValues      => "HIGH-VALUES",
                LowValue        => "LOW-VALUE",
                LowValues       => "LOW-VALUES",
                Quote           => "QUOTE",
                Quotes          => "QUOTES",
                All             => "ALL",
                Filler          => "FILLER",

                // ── Data item clauses ───────────────────────────────────
                Renames         => "RENAMES",
                Through         => "THROUGH",
                Thru            => "THRU",

                // ── Copybook ────────────────────────────────────────────
                Copy            => "COPY",
                Replacing       => "REPLACING",

                // ── Procedure Division verbs ────────────────────────────
                Perform         => "PERFORM",
                Until           => "UNTIL",
                Varying         => "VARYING",
                From            => "FROM",
                After           => "AFTER",
                Move            => "MOVE",
                To              => "TO",
                Corresponding   => "CORRESPONDING",
                Corr            => "CORR",
                Add             => "ADD",
                Subtract        => "SUBTRACT",
                Multiply        => "MULTIPLY",
                Divide          => "DIVIDE",
                Compute         => "COMPUTE",
                Giving          => "GIVING",
                Rounded         => "ROUNDED",
                Size            => "SIZE",
                Remainder       => "REMAINDER",

                // ── Control flow ────────────────────────────────────────
                If              => "IF",
                Then            => "THEN",
                Else            => "ELSE",
                EndIf           => "END-IF",
                Evaluate        => "EVALUATE",
                Also            => "ALSO",
                Any             => "ANY",
                True            => "TRUE",
                False           => "FALSE",
                Other           => "OTHER",
                EndEvaluate     => "END-EVALUATE",
                Go              => "GO",
                Stop            => "STOP",
                Run             => "RUN",
                Exit            => "EXIT",
                Program         => "PROGRAM",
                Paragraph       => "PARAGRAPH",
                Continue        => "CONTINUE",

                // ── CALL ────────────────────────────────────────────────
                Call            => "CALL",
                Using           => "USING",
                Returning       => "RETURNING",
                Reference       => "REFERENCE",
                Content         => "CONTENT",
                Length          => "LENGTH",
                OnException     => "EXCEPTION",
                Cancel          => "CANCEL",
                GoBack          => "GOBACK",

                // ── String handling ─────────────────────────────────────
                Initialize      => "INITIALIZE",
                Inspect         => "INSPECT",
                Tallying        => "TALLYING",
                For             => "FOR",
                Characters      => "CHARACTERS",
                Initial         => "INITIAL",
                Converting      => "CONVERTING",
                Before          => "BEFORE",
                String          => "STRING",
                Delimited       => "DELIMITED",
                Into            => "INTO",
                Pointer         => "POINTER",
                Overflow        => "OVERFLOW",
                EndString       => "END-STRING",
                Unstring        => "UNSTRING",
                Count           => "COUNT",
                Delimiter       => "DELIMITER",
                EndUnstring     => "END-UNSTRING",

                // ── I/O ─────────────────────────────────────────────────
                Accept          => "ACCEPT",
                Date            => "DATE",
                Day             => "DAY",
                DayOfWeek       => "DAY-OF-WEEK",
                Time            => "TIME",
                Open            => "OPEN",
                Input           => "INPUT",
                Output          => "OUTPUT",
                Io              => "I-O",
                Extend          => "EXTEND",
                Close           => "CLOSE",
                Read            => "READ",
                Next            => "NEXT",
                Record          => "RECORD",
                End             => "END",
                AtEnd           => "AT",
                InvalidKey      => "INVALID",
                EndRead         => "END-READ",
                Write           => "WRITE",
                Advancing       => "ADVANCING",
                Page            => "PAGE",
                Line            => "LINE",
                Lines           => "LINES",
                EndWrite        => "END-WRITE",
                Rewrite         => "REWRITE",
                EndRewrite      => "END-REWRITE",
                Delete          => "DELETE",
                EndDelete       => "END-DELETE",
                Start           => "START",
                EndStart        => "END-START",
                Return          => "RETURN",
                EndReturn       => "END-RETURN",

                // ── Sort / Merge ────────────────────────────────────────
                Sort            => "SORT",
                Merge           => "MERGE",
                Release         => "RELEASE",
                Search          => "SEARCH",
                EndSearch       => "END-SEARCH",
                Set             => "SET",
                UpBy            => "UP",
                DownBy          => "DOWN",
                Address         => "ADDRESS",
                Of              => "OF",
                Upon            => "UPON",
                NoAdvancing     => "NO",

                // ── Conditions ──────────────────────────────────────────
                And             => "AND",
                Or              => "OR",
                Not             => "NOT",
                Is              => "IS",
                Are             => "ARE",
                Equal           => "EQUAL",
                Greater         => "GREATER",
                Less            => "LESS",
                Than            => "THAN",
                Numeric         => "NUMERIC",
                Alphabetic      => "ALPHABETIC",
                AlphabeticLower => "ALPHABETIC-LOWER",
                AlphabeticUpper => "ALPHABETIC-UPPER",
                Positive        => "POSITIVE",
                Negative        => "NEGATIVE",
                Class           => "CLASS",
                Omitted         => "OMITTED",

                // ── EXEC CICS/SQL ───────────────────────────────────────
                Exec            => "EXEC",
                Cics            => "CICS",
                Sql             => "SQL",
                EndExec         => "END-EXEC",

                // ── COBOL-2002 / IBM Extensions ─────────────────────────
                Function        => "FUNCTION",
                EndProgram      => "END-PROGRAM",
                EndPerform      => "END-PERFORM",
                EndCompute      => "END-COMPUTE",
                EndAdd          => "END-ADD",
                EndSubtract     => "END-SUBTRACT",
                EndMultiply     => "END-MULTIPLY",
                EndDivide       => "END-DIVIDE",
                EndCall         => "END-CALL",
                EndAccept       => "END-ACCEPT",
                Native          => "NATIVE",
                Standard1       => "STANDARD-1",
                Standard2       => "STANDARD-2",
                With            => "WITH",
                Debugging       => "DEBUGGING",
                Mode            => "MODE",
                Common          => "COMMON",
                External        => "EXTERNAL",
                Global          => "GLOBAL",
                CurrencySign    => "CURRENCY",
                DecimalPoint    => "DECIMAL-POINT",
                Comma           => "COMMA",
                AlphabetName    => "ALPHABET",
                SymbolicCharacters => "SYMBOLIC",
                ConditionName   => "CONDITION",

                // ── IBM COBOL extensions ────────────────────────────────
                Alter           => "ALTER",
                Entry           => "ENTRY",
                Invoke          => "INVOKE",
                Allocate        => "ALLOCATE",
                Free            => "FREE",
                Json            => "JSON",
                Xml             => "XML",
                Generate        => "GENERATE",
                Parse           => "PARSE",
                Validate        => "VALIDATE",
                Raise           => "RAISE",
                Resume          => "RESUME",
                Typedef         => "TYPEDEF",
                Strong          => "STRONG",
                EndJson         => "END-JSON",
                EndXml          => "END-XML",
                Name            => "NAME",
                Suppress        => "SUPPRESS",
                Detail          => "DETAIL",
                Encoding        => "ENCODING",
                Initialized     => "INITIALIZED",
                Proceed         => "PROCEED",
                Utf8            => "UTF-8",
                Display1        => "DISPLAY-1",
                GroupUsage      => "GROUP-USAGE",
                Linage          => "LINAGE",
                Footing         => "FOOTING",
                Top             => "TOP",
                Bottom          => "BOTTOM",
                CodeSet         => "CODE-SET",
                Namespace       => "NAMESPACE",
                Processing      => "PROCESSING",
                Validating      => "VALIDATING",
                Attribute       => "ATTRIBUTE",
                Type            => "TYPE",
                In              => "IN",
                Declaratives    => "DECLARATIVES",
                Repository      => "REPOSITORY",
                Intrinsic       => "INTRINSIC",
                IoControl       => "I-O-CONTROL",
                Same            => "SAME",
                Area            => "AREA",
                Apply           => "APPLY",
                WriteOnly       => "WRITE-ONLY",
                Lock            => "LOCK",
                Manual          => "MANUAL",
                Automatic       => "AUTOMATIC",
                Exclusive       => "EXCLUSIVE",
                Sharing         => "SHARING",
                Reserve         => "RESERVE",
                Padding         => "PADDING",
                EndDeclaratives => "END-DECLARATIVES",
                Use             => "USE",
                Error           => "ERROR",
                Standard        => "STANDARD",
            }
            @alias {
                // Contextual variants constructed by the parser, NOT in the
                // lexer lookup map (they share a string with a @primary entry).
                DynamicLength   => "DYNAMIC",
                RecordKey       => "RECORD",
                FileStatus      => "FILE",
                OnSizeError     => "SIZE",
                NotOnSizeError  => "SIZE",
                NotAtEnd        => "END",
                NotInvalidKey   => "INVALID",
                OnOverflow      => "OVERFLOW",
                NotOnOverflow   => "OVERFLOW",
                NotOnException  => "EXCEPTION",
                GoTo            => "GO",
                Chain           => "CHAIN",
                WithPointer     => "POINTER",
                ClassCondition  => "CLASS",
            }
        }
    };
}

// ═══════════════════════════════════════════════════════════════════════════
// STATEMENT DEFINITIONS
// ═══════════════════════════════════════════════════════════════════════════

/// Master statement variant table — defines the `Statement` enum.
///
/// Each entry is `Variant(StructType)`.  The struct must have `pub span: Span`.
/// This macro generates:
/// - The `Statement` enum with all variants.
/// - The `Statement::span()` method.
macro_rules! for_all_statement_variants {
    ($mac:ident) => {
        $mac! {
            Move(MoveStatement),
            Compute(ComputeStatement),
            Add(AddStatement),
            Subtract(SubtractStatement),
            Multiply(MultiplyStatement),
            Divide(DivideStatement),
            If(IfStatement),
            Evaluate(EvaluateStatement),
            Perform(PerformStatement),
            Call(CallStatement),
            Display(DisplayStatement),
            Accept(AcceptStatement),
            Open(OpenStatement),
            Close(CloseStatement),
            Read(ReadStatement),
            Write(WriteStatement),
            StopRun(StopRunStatement),
            GoBack(GoBackStatement),
            Exit(ExitStatement),
            GoTo(GoToStatement),
            Initialize(InitializeStatement),
            Inspect(InspectStatement),
            String(StringStatement),
            Unstring(UnstringStatement),
            Set(SetStatement),
            Search(SearchStatement),
            Cancel(CancelStatement),
            Sort(SortStatement),
            Merge(MergeStatement),
            Release(ReleaseStatement),
            ReturnStmt(ReturnStatement),
            Continue(ContinueStatement),
            ExecCics(ExecCicsStatement),
            ExecSql(ExecSqlStatement),
            JsonGenerate(JsonGenerateStatement),
            JsonParse(JsonParseStatement),
            XmlGenerate(XmlGenerateStatement),
            XmlParse(XmlParseStatement),
            Allocate(AllocateStatement),
            Free(FreeStatement),
            Entry(EntryStatement),
            Alter(AlterStatement),
            Invoke(InvokeStatement),
        }
    };
}

/// Statement parse dispatch table — maps keywords to parse functions.
///
/// Each entry is `Keyword => parse_function`.  Multiple keywords can map to the
/// same parse function (e.g., `Go` and `GoTo` both go to `parse_goto_statement`).
///
/// Some parse functions return a *different* Statement variant than you'd expect:
/// - `parse_rewrite_statement` → `Statement::Write`
/// - `parse_delete_statement` → `Statement::Read`
/// - `parse_start_statement`  → `Statement::Read`
///
/// This macro generates:
/// - The `parse_statement()` dispatch (if-else chain).
/// - The `is_statement_start()` predicate.
macro_rules! for_parse_dispatch {
    ($mac:ident) => {
        $mac! {
            Move        => parse_move_statement,
            Display     => parse_display_statement,
            Stop        => parse_stop_statement,
            Perform     => parse_perform_statement,
            If          => parse_if_statement,
            Continue    => parse_continue_statement,
            Add         => parse_add_statement,
            Subtract    => parse_subtract_statement,
            Multiply    => parse_multiply_statement,
            Divide      => parse_divide_statement,
            Compute     => parse_compute_statement,
            String      => parse_string_statement,
            Unstring    => parse_unstring_statement,
            Call        => parse_call_statement,
            GoBack      => parse_goback_statement,
            GoTo        => parse_goto_statement,
            Go          => parse_goto_statement,
            Exit        => parse_exit_statement,
            Exec        => parse_exec_statement,
            Set         => parse_set_statement,
            Initialize  => parse_initialize_statement,
            Accept      => parse_accept_statement,
            Open        => parse_open_statement,
            Close       => parse_close_statement,
            Read        => parse_read_statement,
            Write       => parse_write_statement,
            Rewrite     => parse_rewrite_statement,
            Delete      => parse_delete_statement,
            Start       => parse_start_statement,
            Search      => parse_search_statement,
            Inspect     => parse_inspect_statement,
            Evaluate    => parse_evaluate_statement,
            Cancel      => parse_cancel_statement,
            Sort        => parse_sort_statement,
            Merge       => parse_merge_statement,
            Release     => parse_release_statement,
            Return      => parse_return_statement,
            Json        => parse_json_statement,
            Xml         => parse_xml_statement,
            Allocate    => parse_allocate_statement,
            Free        => parse_free_statement,
            Entry       => parse_entry_statement,
            Alter       => parse_alter_statement,
            Invoke      => parse_invoke_statement,
        }
    };
}
