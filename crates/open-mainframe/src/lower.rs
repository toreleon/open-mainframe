//! AST lowering: converts COBOL AST (`Program`) to `SimpleProgram`.
//!
//! This module walks the parsed COBOL AST produced by `open_mainframe_cobol::ast`
//! and produces the simplified `SimpleProgram` representation consumed by the
//! tree-walking interpreter in `open_mainframe_runtime::interpreter`.
//!
//! The lowering is intentionally lossy — information that the interpreter does not
//! need (spans, semantic annotations, ON SIZE ERROR handlers, etc.) is discarded.

use std::collections::HashMap;

use open_mainframe_cobol::ast::*;
// Re-import the interpreter types, renaming the conflicting `ConditionValue` enum
// to avoid clashing with `open_mainframe_cobol::ast::ConditionValue` (struct).
use open_mainframe_runtime::interpreter::{
    CallParam, ConditionDef, ConditionValue as RtConditionValue, DataItemMeta,
    GroupField, SimpleAdvancing, SimpleBinaryOp, SimpleClassType, SimpleCompareOp,
    SimpleCondition, SimpleExpr, SimpleInspectFor, SimpleInspectMode, SimpleInspectRule,
    SimpleOpenMode, SimpleProgram, SimpleSearchWhen, SimpleStatement, SimpleWhenClause,
    VaryingAfter,
};

// ---------------------------------------------------------------------------
// Public entry point
// ---------------------------------------------------------------------------

/// Lower a parsed COBOL `Program` into a `SimpleProgram` suitable for
/// interpretation.
pub fn lower_program(program: &Program) -> SimpleProgram {
    let mut ctx = LowerCtx::new();

    // 1. Identification division — extract program metadata.
    let name = program.identification.program_id.name.clone();
    let is_initial = program.identification.program_id.is_initial;
    let is_common = program.identification.program_id.is_common;

    // 2. Data division — build data_items, group_layouts, condition_names,
    //    redefines_aliases.
    if let Some(ref data) = program.data {
        lower_data_division(data, &mut ctx);
    }

    // 3. Declaratives — build declarative_handlers.
    let mut declarative_handlers: HashMap<String, String> = HashMap::new();
    if let Some(ref proc) = program.procedure {
        for decl in &proc.declaratives {
            let handler_name = if !decl.paragraphs.is_empty() {
                decl.paragraphs[0].name.clone()
            } else {
                decl.name.clone()
            };
            match &decl.use_clause.target {
                UseTarget::File(f) => {
                    declarative_handlers.insert(f.to_uppercase(), handler_name.to_uppercase());
                }
                UseTarget::Input => {
                    declarative_handlers
                        .insert("INPUT".to_string(), handler_name.to_uppercase());
                }
                UseTarget::Output => {
                    declarative_handlers
                        .insert("OUTPUT".to_string(), handler_name.to_uppercase());
                }
                UseTarget::InputOutput => {
                    declarative_handlers
                        .insert("I-O".to_string(), handler_name.to_uppercase());
                }
                UseTarget::Extend => {
                    declarative_handlers
                        .insert("EXTEND".to_string(), handler_name.to_uppercase());
                }
            }

            // Also lower paragraphs inside declarative sections so they are
            // callable via PERFORM.
            for para in &decl.paragraphs {
                let stmts = lower_statements(&para.statements);
                let upper = para.name.to_uppercase();
                ctx.paragraph_order.push(upper.clone());
                ctx.paragraphs.insert(upper, stmts);
            }
        }
    }

    // 4. Procedure division body — lower paragraphs and statements.
    let mut statements: Vec<SimpleStatement> = Vec::new();
    if let Some(ref proc) = program.procedure {
        match &proc.body {
            ProcedureBody::Sections(sections) => {
                for section in sections {
                    // Record the section name as a paragraph (so PERFORM
                    // SECTION-NAME works).  Its body is the concatenation of
                    // all statements of all paragraphs in the section.
                    let mut section_stmts: Vec<SimpleStatement> = Vec::new();
                    for para in &section.paragraphs {
                        let stmts = lower_statements(&para.statements);
                        let upper = para.name.to_uppercase();
                        ctx.paragraph_order.push(upper.clone());
                        ctx.paragraphs.insert(upper, stmts.clone());
                        section_stmts.extend(stmts);
                    }
                    // Also register section name pointing to all its statements.
                    let section_upper = section.name.to_uppercase();
                    if !ctx.paragraphs.contains_key(&section_upper) {
                        ctx.paragraph_order.push(section_upper.clone());
                        ctx.paragraphs.insert(section_upper, section_stmts);
                    }
                }
            }
            ProcedureBody::Paragraphs(paragraphs) => {
                for para in paragraphs {
                    let stmts = lower_statements(&para.statements);
                    let upper = para.name.to_uppercase();
                    ctx.paragraph_order.push(upper.clone());
                    ctx.paragraphs.insert(upper, stmts);
                }
            }
            ProcedureBody::Statements(stmts) => {
                statements = lower_statements(stmts);
            }
        }

        // If the body was sections or paragraphs, the "main" statements are
        // the concatenation of all paragraphs in order.
        if statements.is_empty() && !ctx.paragraph_order.is_empty() {
            for name in &ctx.paragraph_order {
                if let Some(stmts) = ctx.paragraphs.get(name) {
                    statements.extend(stmts.clone());
                }
            }
        }
    }

    // 5. Contained programs.
    let contained_programs: Vec<SimpleProgram> = program
        .contained_programs
        .iter()
        .map(lower_program)
        .collect();

    SimpleProgram {
        name,
        data_items: ctx.data_items,
        statements,
        paragraphs: ctx.paragraphs,
        paragraph_order: ctx.paragraph_order,
        condition_names: ctx.condition_names,
        group_layouts: ctx.group_layouts,
        contained_programs,
        is_initial,
        is_common,
        declarative_handlers,
        redefines_aliases: ctx.redefines_aliases,
    }
}

// ---------------------------------------------------------------------------
// Lowering context — accumulates data during traversal
// ---------------------------------------------------------------------------

struct LowerCtx {
    data_items: Vec<(String, DataItemMeta)>,
    paragraphs: HashMap<String, Vec<SimpleStatement>>,
    paragraph_order: Vec<String>,
    condition_names: HashMap<String, ConditionDef>,
    group_layouts: HashMap<String, Vec<GroupField>>,
    redefines_aliases: Vec<(String, String)>,
    /// Counter for generating unique FILLER names.
    filler_counter: usize,
}

impl LowerCtx {
    fn new() -> Self {
        Self {
            data_items: Vec::new(),
            paragraphs: HashMap::new(),
            paragraph_order: Vec::new(),
            condition_names: HashMap::new(),
            group_layouts: HashMap::new(),
            redefines_aliases: Vec::new(),
            filler_counter: 0,
        }
    }

    fn next_filler_name(&mut self) -> String {
        self.filler_counter += 1;
        format!("FILLER-{}", self.filler_counter)
    }
}

// ---------------------------------------------------------------------------
// Data division lowering
// ---------------------------------------------------------------------------

fn lower_data_division(data: &DataDivision, ctx: &mut LowerCtx) {
    // File section: lower each FD/SD record description.
    for fd in &data.file_section {
        for record in &fd.records {
            lower_data_item(record, None, ctx);
        }
    }

    // Working-storage section.
    for item in &data.working_storage {
        lower_data_item(item, None, ctx);
    }

    // Local-storage section.
    for item in &data.local_storage {
        lower_data_item(item, None, ctx);
    }

    // Linkage section.
    for item in &data.linkage {
        lower_data_item(item, None, ctx);
    }
}

/// Recursively lower a single data item and its children.
///
/// `parent_name` is `Some(name)` when this item is a child of a group item,
/// used for building condition name parent references.
fn lower_data_item(item: &DataItem, parent_name: Option<&str>, ctx: &mut LowerCtx) {
    let item_name = match &item.name {
        DataItemName::Named(s) => s.to_uppercase(),
        DataItemName::Filler => ctx.next_filler_name(),
    };

    // Handle level 88 condition values — these define condition names, not
    // real data items.
    if item.level == 88 {
        // Level 88 items should have been attached as condition_values on
        // the parent DataItem, but handle them here defensively.
        if let Some(parent) = parent_name {
            if let DataItemName::Named(ref cond_name) = item.name {
                let values = lower_condition_value_entries(item);
                ctx.condition_names.insert(
                    cond_name.to_uppercase(),
                    ConditionDef {
                        parent: parent.to_uppercase(),
                        values,
                    },
                );
            }
        }
        return;
    }

    // Handle level 66 RENAMES — skip for now (no SimpleProgram equivalent).
    if item.level == 66 {
        return;
    }

    // Handle REDEFINES.
    if let Some(ref redef) = item.redefines {
        ctx.redefines_aliases
            .push((item_name.clone(), redef.name.to_uppercase()));
    }

    // Process condition_values attached to this item (level-88 children).
    for cv in &item.condition_values {
        let values = lower_condition_values(&cv.values);
        ctx.condition_names.insert(
            cv.name.to_uppercase(),
            ConditionDef {
                parent: item_name.clone(),
                values,
            },
        );
    }

    if !item.children.is_empty() {
        // Group item — build group layout from children.
        let mut group_fields: Vec<GroupField> = Vec::new();
        let mut offset: usize = 0;
        build_group_fields(&item.children, &mut group_fields, &mut offset, ctx);

        let total_size = offset;
        ctx.group_layouts
            .insert(item_name.clone(), group_fields);

        // Register the group item itself as a data item with the total size.
        let meta = DataItemMeta {
            size: total_size,
            decimals: 0,
            is_numeric: false,
            picture: None,
            is_justified: item.justified,
            initial_value: item.value.as_ref().map(|lit| literal_to_string(lit)),
        };
        ctx.data_items.push((item_name.clone(), meta));

        // Recursively lower children so they are also registered individually.
        for child in &item.children {
            lower_data_item(child, Some(&item_name), ctx);
        }

        // Register subscripted data items for children of groups with OCCURS > 1.
        if let Some(ref occurs) = item.occurs {
            let times = occurs.times as usize;
            if times > 1 {
                // Register subscripted variants for each elementary child
                fn register_subscripted_children(
                    children: &[DataItem],
                    times: usize,
                    ctx: &mut LowerCtx,
                ) {
                    for child in children {
                        if child.level == 88 || child.level == 66 {
                            continue;
                        }
                        if let DataItemName::Named(ref child_name) = child.name {
                            let (size, decimals, is_numeric, picture) = compute_elementary_meta(child);
                            for occ in 1..=times {
                                let name = format!("{}({})", child_name.to_uppercase(), occ);
                                ctx.data_items.push((
                                    name,
                                    DataItemMeta {
                                        size,
                                        decimals,
                                        is_numeric,
                                        picture: picture.clone(),
                                        is_justified: child.justified,
                                        initial_value: None,
                                    },
                                ));
                            }
                        }
                        // Recurse into nested groups
                        if !child.children.is_empty() {
                            register_subscripted_children(&child.children, times, ctx);
                        }
                    }
                }
                register_subscripted_children(&item.children, times, ctx);
            }
        }
    } else {
        // Elementary item — compute metadata from PIC / USAGE.
        let (size, decimals, is_numeric, picture) = compute_elementary_meta(item);

        let meta = DataItemMeta {
            size,
            decimals,
            is_numeric,
            picture,
            is_justified: item.justified,
            initial_value: item.value.as_ref().map(|lit| literal_to_string(lit)),
        };
        ctx.data_items.push((item_name.clone(), meta));
    }
}

/// Build the flat list of `GroupField` entries for a group item's children,
/// tracking cumulative byte offset.
fn build_group_fields(
    children: &[DataItem],
    fields: &mut Vec<GroupField>,
    offset: &mut usize,
    ctx: &mut LowerCtx,
) {
    for child in children {
        if child.level == 88 {
            // Condition values are not physical fields.
            continue;
        }
        if child.level == 66 {
            continue;
        }

        let field_name = match &child.name {
            DataItemName::Named(s) => s.to_uppercase(),
            DataItemName::Filler => String::new(),
        };

        let default_value = child.value.as_ref().map(|lit| literal_to_string(lit));

        // Handle REDEFINES: reset offset to the redefined item's offset
        // instead of continuing from the current position.
        if let Some(ref redef) = child.redefines {
            let redef_name = redef.name.to_uppercase();
            if let Some(redef_field) = fields.iter().rev().find(|f| f.name.eq_ignore_ascii_case(&redef_name)) {
                *offset = redef_field.offset;
            }
        }

        if !child.children.is_empty() {
            // Nested group: recurse to get the sub-fields' total size.
            let occurs_times = child.occurs.as_ref().map(|o| o.times as usize).unwrap_or(1);
            let start = *offset;
            let fields_before = fields.len();
            build_group_fields(&child.children, fields, offset, ctx);
            let element_size = *offset - start;

            if occurs_times > 1 {
                // Snapshot the base fields (occurrence 1)
                let base_fields: Vec<GroupField> = fields[fields_before..].to_vec();

                // Rename occurrence 1 fields with (1) suffix
                for f in &mut fields[fields_before..] {
                    if !f.name.is_empty() {
                        f.name = format!("{}(1)", f.name);
                    }
                }

                // Replicate for occurrences 2..N
                for occ in 2..=occurs_times {
                    let occ_offset = start + (occ - 1) * element_size;
                    for bf in &base_fields {
                        fields.push(GroupField {
                            name: if !bf.name.is_empty() {
                                format!("{}({})", bf.name, occ)
                            } else {
                                String::new()
                            },
                            offset: occ_offset + (bf.offset - start),
                            size: bf.size,
                            is_numeric: bf.is_numeric,
                            default_value: bf.default_value.clone(),
                        });
                    }
                }
                *offset = start + occurs_times * element_size;
            }

            // Also register this nested group as a field in the parent.
            let total_child_size = *offset - start;
            fields.push(GroupField {
                name: field_name,
                offset: start,
                size: total_child_size,
                is_numeric: false,
                default_value,
            });
        } else {
            let (size, _decimals, is_numeric, _pic) = compute_elementary_meta(child);

            // Apply OCCURS multiplier.
            let occurs_times = child
                .occurs
                .as_ref()
                .map(|o| o.times as usize)
                .unwrap_or(1);
            let total_size = size * occurs_times;

            fields.push(GroupField {
                name: field_name,
                offset: *offset,
                size: total_size,
                is_numeric,
                default_value,
            });

            *offset += total_size;
        }
    }
}

/// Compute elementary data item metadata from PIC / USAGE.
fn compute_elementary_meta(item: &DataItem) -> (usize, u32, bool, Option<String>) {
    if let Some(ref pic) = item.picture {
        let is_numeric = matches!(
            pic.category,
            PictureCategory::Numeric | PictureCategory::NumericEdited
        );
        let size = compute_storage_size(pic, item.usage.as_ref());
        (size, pic.decimal_positions, is_numeric, Some(pic.picture.clone()))
    } else {
        // No PIC — derive size from USAGE if possible.
        match item.usage {
            Some(Usage::Comp1) => (4, 0, true, None),
            Some(Usage::Comp2) => (8, 0, true, None),
            Some(Usage::Pointer) | Some(Usage::FunctionPointer) | Some(Usage::ProcedurePointer) => {
                (4, 0, false, None)
            }
            Some(Usage::Index) => (4, 0, true, None),
            _ => {
                // Group item with no PIC or USAGE; handled by caller for
                // group items.  For elementary items with no PIC, default to 1.
                (1, 0, false, None)
            }
        }
    }
}

/// Compute the actual storage size in bytes, taking USAGE into account.
fn compute_storage_size(pic: &PictureClause, usage: Option<&Usage>) -> usize {
    let display_size = pic.size as usize;
    match usage {
        Some(Usage::Binary) | Some(Usage::Comp5) => {
            // IBM binary: 1-4 digits → 2 bytes, 5-9 → 4 bytes, 10-18 → 8 bytes.
            match display_size {
                0..=4 => 2,
                5..=9 => 4,
                _ => 8,
            }
        }
        Some(Usage::PackedDecimal) => {
            // Packed decimal: (digits + 1) / 2 rounded up, plus sign nibble.
            (display_size + 2) / 2
        }
        Some(Usage::Comp1) => 4,
        Some(Usage::Comp2) => 8,
        Some(Usage::Index) => 4,
        Some(Usage::Pointer) | Some(Usage::FunctionPointer) | Some(Usage::ProcedurePointer) => 4,
        Some(Usage::National) => display_size * 2,
        Some(Usage::Display1) => display_size * 2,
        _ => display_size, // DISPLAY (default)
    }
}

/// Convert a `Literal` to its string representation for default values.
fn literal_to_string(lit: &Literal) -> String {
    match &lit.kind {
        LiteralKind::Integer(n) => n.to_string(),
        LiteralKind::Decimal(s) => s.clone(),
        LiteralKind::String(s) => s.clone(),
        LiteralKind::Hex(s) => {
            // Decode hex pairs to characters: X'7D' → byte 0x7D → char '}'
            // Uses char::from(u8) so each COBOL byte maps to exactly one char.
            let mut result = String::new();
            let hex_chars: Vec<char> = s.chars().collect();
            for chunk in hex_chars.chunks(2) {
                if chunk.len() == 2 {
                    let hex_str: String = chunk.iter().collect();
                    if let Ok(byte) = u8::from_str_radix(&hex_str, 16) {
                        result.push(char::from(byte));
                    }
                }
            }
            result
        }
        LiteralKind::Figurative(fig) => match fig {
            FigurativeConstant::Zero => "0".to_string(),
            FigurativeConstant::Space => " ".to_string(),
            FigurativeConstant::HighValue => "\u{FF}".to_string(),
            FigurativeConstant::LowValue => "\0".to_string(),
            FigurativeConstant::Quote => "\"".to_string(),
            FigurativeConstant::All => String::new(),
        },
        LiteralKind::AllOf(inner) => literal_to_string(inner),
    }
}

/// Lower condition value entries from an item's `value` field (level-88
/// items that somehow ended up as standalone DataItem nodes).
fn lower_condition_value_entries(item: &DataItem) -> Vec<RtConditionValue> {
    let mut result = Vec::new();
    if let Some(ref lit) = item.value {
        result.push(RtConditionValue::Single(literal_to_string(lit)));
    }
    // Also check condition_values on the item itself.
    for cv in &item.condition_values {
        result.extend(lower_condition_values(&cv.values));
    }
    result
}

/// Convert AST `ConditionValueEntry` list to interpreter `RtConditionValue` list.
fn lower_condition_values(entries: &[ConditionValueEntry]) -> Vec<RtConditionValue> {
    entries
        .iter()
        .map(|e| match e {
            ConditionValueEntry::Single(lit) => {
                RtConditionValue::Single(literal_to_string(lit))
            }
            ConditionValueEntry::Range { from, to } => {
                RtConditionValue::Range(literal_to_string(from), literal_to_string(to))
            }
        })
        .collect()
}

// ---------------------------------------------------------------------------
// Statement lowering
// ---------------------------------------------------------------------------

fn lower_statements(stmts: &[Statement]) -> Vec<SimpleStatement> {
    stmts.iter().filter_map(lower_statement).collect()
}

/// Lower a single AST statement to a `SimpleStatement`. Returns `None` for
/// statement types that have no interpreter equivalent (they are silently
/// skipped).
fn lower_statement(stmt: &Statement) -> Option<SimpleStatement> {
    match stmt {
        // -- DISPLAY --
        Statement::Display(s) => Some(SimpleStatement::Display {
            items: s.items.iter().map(lower_expr).collect(),
            no_advancing: s.no_advancing,
        }),

        // -- ACCEPT --
        Statement::Accept(s) => Some(SimpleStatement::Accept {
            target: s.target.name.to_uppercase(),
        }),

        // -- MOVE --
        Statement::Move(s) => {
            let from = lower_expr(&s.from);
            let to: Vec<SimpleExpr> = s
                .to
                .iter()
                .map(lower_qualified_name_to_expr)
                .collect();
            Some(SimpleStatement::Move { from, to })
        }

        // -- COMPUTE --
        Statement::Compute(s) => {
            // COMPUTE can target multiple variables; we emit one Compute
            // per target.  If there are multiple targets, we chain them.
            // For simplicity, take the first target only — the interpreter
            // handles a single target.
            let target = s
                .targets
                .first()
                .map(|t| t.name.name.to_uppercase())
                .unwrap_or_default();
            let expr = lower_expr(&s.expression);
            Some(SimpleStatement::Compute { target, expr })
        }

        // -- ADD --
        Statement::Add(s) => {
            let values: Vec<SimpleExpr> = s.operands.iter().map(lower_expr).collect();
            let to: Vec<String> = if !s.giving.is_empty() {
                s.giving.iter().map(|g| g.name.name.to_uppercase()).collect()
            } else {
                s.to.iter().map(|t| t.name.name.to_uppercase()).collect()
            };
            Some(SimpleStatement::Add { values, to })
        }

        // -- SUBTRACT --
        Statement::Subtract(s) => {
            let values: Vec<SimpleExpr> = s.operands.iter().map(lower_expr).collect();
            let from: Vec<String> = if !s.giving.is_empty() {
                s.giving.iter().map(|g| g.name.name.to_uppercase()).collect()
            } else {
                s.from.iter().map(|f| f.name.name.to_uppercase()).collect()
            };
            Some(SimpleStatement::Subtract { values, from })
        }

        // -- MULTIPLY --
        Statement::Multiply(s) => {
            let value = lower_expr(&s.operand);
            let by = lower_expr(&s.by);
            let giving = s.giving.first().map(|g| g.name.name.to_uppercase());
            Some(SimpleStatement::Multiply { value, by, giving })
        }

        // -- DIVIDE --
        Statement::Divide(s) => {
            let value = lower_expr(&s.operand);
            let into = lower_expr(&s.into_or_by);
            let giving = s.giving.first().map(|g| g.name.name.to_uppercase());
            Some(SimpleStatement::Divide {
                value,
                into,
                giving,
            })
        }

        // -- IF --
        Statement::If(s) => {
            let condition = lower_condition(&s.condition);
            let then_branch = lower_statements(&s.then_branch);
            let else_branch = s.else_branch.as_ref().map(|b| lower_statements(b));
            Some(SimpleStatement::If {
                condition,
                then_branch,
                else_branch,
            })
        }

        // -- EVALUATE --
        Statement::Evaluate(s) => {
            let subjects: Vec<SimpleExpr> = s.subjects.iter().map(lower_expr).collect();
            let mut when_clauses: Vec<SimpleWhenClause> = Vec::new();

            for wc in &s.when_clauses {
                let condition = lower_when_clause_to_condition(&subjects, &wc.conditions);
                let statements = lower_statements(&wc.statements);
                when_clauses.push(SimpleWhenClause {
                    condition,
                    statements,
                });
            }

            // WHEN OTHER → catch-all (always true).
            if let Some(ref other_stmts) = s.when_other {
                when_clauses.push(SimpleWhenClause {
                    condition: SimpleCondition::Compare {
                        left: SimpleExpr::Integer(1),
                        op: SimpleCompareOp::Equal,
                        right: SimpleExpr::Integer(1),
                    },
                    statements: lower_statements(other_stmts),
                });
            }

            Some(SimpleStatement::Evaluate {
                subjects,
                when_clauses,
            })
        }

        // -- PERFORM --
        Statement::Perform(s) => lower_perform(s),

        // -- STOP RUN --
        Statement::StopRun(s) => {
            let return_code = s.return_code.as_ref().and_then(|e| {
                if let Expression::Literal(lit) = e {
                    if let LiteralKind::Integer(n) = &lit.kind {
                        return Some(*n as i32);
                    }
                }
                None
            });
            Some(SimpleStatement::StopRun { return_code })
        }

        // -- GOBACK --
        Statement::GoBack(s) => {
            let return_code = s.returning.as_ref().and_then(|e| {
                if let Expression::Literal(lit) = e {
                    if let LiteralKind::Integer(n) = &lit.kind {
                        return Some(*n as i32);
                    }
                }
                None
            });
            Some(SimpleStatement::GoBack { return_code })
        }

        // -- EXIT --
        Statement::Exit(s) => {
            if s.program {
                Some(SimpleStatement::StopRun { return_code: None })
            } else if s.perform_cycle {
                Some(SimpleStatement::ExitParagraph)
            } else {
                Some(SimpleStatement::ExitParagraph)
            }
        }

        // -- GO TO --
        Statement::GoTo(s) => {
            // Simple GO TO: take the first target.
            let target = s
                .targets
                .first()
                .map(|t| t.to_uppercase())
                .unwrap_or_default();
            Some(SimpleStatement::GoTo { target })
        }

        // -- INITIALIZE --
        Statement::Initialize(s) => {
            let targets: Vec<String> = s
                .variables
                .iter()
                .map(|q| q.name.to_uppercase())
                .collect();
            Some(SimpleStatement::Initialize { targets })
        }

        // -- STRING --
        Statement::String(s) => {
            let sources: Vec<SimpleExpr> = s
                .sources
                .iter()
                .map(|src| lower_expr(&src.value))
                .collect();
            let into = s.into.name.to_uppercase();
            Some(SimpleStatement::StringConcat { sources, into })
        }

        // -- UNSTRING --
        Statement::Unstring(s) => {
            let source = s.source.name.to_uppercase();
            let delimiters: Vec<String> = s
                .delimiters
                .iter()
                .map(|d| expr_to_string_value(&d.value))
                .collect();
            let into: Vec<String> = s
                .into
                .iter()
                .map(|t| t.name.name.to_uppercase())
                .collect();
            let tallying = s.tallying.as_ref().map(|q| q.name.to_uppercase());
            Some(SimpleStatement::Unstring {
                source,
                delimiters,
                into,
                tallying,
            })
        }

        // -- SET --
        Statement::Set(s) => lower_set_statement(s),

        // -- CALL --
        Statement::Call(s) => {
            let program = lower_expr(&s.program);
            let using: Vec<CallParam> = s
                .using
                .iter()
                .map(|p| {
                    let name = expr_to_variable_name(&p.value);
                    match p.mode {
                        ParameterMode::Reference => CallParam::ByReference(name),
                        ParameterMode::Content => CallParam::ByContent(name),
                        ParameterMode::Value => CallParam::ByValue(name),
                    }
                })
                .collect();
            Some(SimpleStatement::Call { program, using })
        }

        // -- CANCEL --
        Statement::Cancel(s) => {
            // CANCEL can have multiple programs; take the first.
            let program = s
                .programs
                .first()
                .map(lower_expr)
                .unwrap_or(SimpleExpr::String(String::new()));
            Some(SimpleStatement::Cancel { program })
        }

        // -- INSPECT --
        Statement::Inspect(s) => lower_inspect(s),

        // -- SEARCH --
        Statement::Search(s) => {
            let table = s.table.name.to_uppercase();
            let all = s.all;
            let at_end = s.at_end.as_ref().map(|stmts| lower_statements(stmts));
            let when_clauses: Vec<SimpleSearchWhen> = s
                .when_clauses
                .iter()
                .map(|w| SimpleSearchWhen {
                    condition: lower_condition(&w.condition),
                    statements: lower_statements(&w.statements),
                })
                .collect();
            Some(SimpleStatement::Search {
                table,
                all,
                at_end,
                when_clauses,
            })
        }

        // -- EXEC CICS --
        Statement::ExecCics(s) => {
            let command = s.command.clone();
            let options: Vec<(String, Option<SimpleExpr>)> = s
                .options
                .iter()
                .map(|opt| {
                    (
                        opt.name.clone(),
                        opt.value.as_ref().map(lower_expr),
                    )
                })
                .collect();
            Some(SimpleStatement::ExecCics { command, options })
        }

        // -- EXEC SQL -- (no interpreter equivalent; skip)
        Statement::ExecSql(_) => None,

        // -- OPEN --
        Statement::Open(s) => {
            let files: Vec<(String, SimpleOpenMode)> = s
                .files
                .iter()
                .map(|f| {
                    let mode = match f.mode {
                        OpenMode::Input => SimpleOpenMode::Input,
                        OpenMode::Output => SimpleOpenMode::Output,
                        OpenMode::InputOutput => SimpleOpenMode::InputOutput,
                        OpenMode::Extend => SimpleOpenMode::Extend,
                    };
                    (f.name.to_uppercase(), mode)
                })
                .collect();
            Some(SimpleStatement::FileOpen { files })
        }

        // -- CLOSE --
        Statement::Close(s) => {
            let files: Vec<String> = s.files.iter().map(|f| f.to_uppercase()).collect();
            Some(SimpleStatement::FileClose { files })
        }

        // -- READ --
        Statement::Read(s) => {
            let file = s.file.to_uppercase();
            let into = s.into.as_ref().map(|q| q.name.to_uppercase());
            let at_end = s.at_end.as_ref().map(|stmts| lower_statements(stmts));
            let not_at_end = s.not_at_end.as_ref().map(|stmts| lower_statements(stmts));
            Some(SimpleStatement::FileRead {
                file,
                into,
                at_end,
                not_at_end,
            })
        }

        // -- WRITE --
        Statement::Write(s) => {
            let record = s.record.name.to_uppercase();
            let from = s.from.as_ref().map(|q| q.name.to_uppercase());
            let advancing = s.advancing.as_ref().map(|adv| match adv {
                WriteAdvancing::Lines { count, before } => {
                    let n = expr_to_integer(count).unwrap_or(1);
                    SimpleAdvancing::Lines {
                        count: n,
                        before: *before,
                    }
                }
                WriteAdvancing::Page { before } => SimpleAdvancing::Page { before: *before },
            });
            Some(SimpleStatement::FileWrite {
                record,
                from,
                advancing,
            })
        }

        // -- CONTINUE -- (no-op)
        Statement::Continue(_) => None,

        // -- SORT -- (no direct interpreter equivalent; skip)
        Statement::Sort(_) => None,

        // -- MERGE -- (no direct interpreter equivalent; skip)
        Statement::Merge(_) => None,

        // -- RELEASE -- (no direct interpreter equivalent; skip)
        Statement::Release(_) => None,

        // -- RETURN (file I/O) -- (no direct interpreter equivalent; skip)
        Statement::ReturnStmt(_) => None,

        // -- JSON GENERATE --
        Statement::JsonGenerate(s) => Some(SimpleStatement::JsonGenerate {
            receiver: s.receiver.name.to_uppercase(),
            source: s.source.name.to_uppercase(),
            count_in: s.count_in.as_ref().map(|q| q.name.to_uppercase()),
        }),

        // -- JSON PARSE --
        Statement::JsonParse(s) => Some(SimpleStatement::JsonParse {
            source: s.source.name.to_uppercase(),
            target: s.target.name.to_uppercase(),
        }),

        // -- XML GENERATE --
        Statement::XmlGenerate(s) => Some(SimpleStatement::XmlGenerate {
            receiver: s.receiver.name.to_uppercase(),
            source: s.source.name.to_uppercase(),
            count_in: s.count_in.as_ref().map(|q| q.name.to_uppercase()),
        }),

        // -- XML PARSE --
        Statement::XmlParse(s) => Some(SimpleStatement::XmlParse {
            source: s.source.name.to_uppercase(),
            processing_procedure: s.processing_procedure.to_uppercase(),
        }),

        // -- ALLOCATE --
        Statement::Allocate(s) => {
            let data_name = s.data_name.name.to_uppercase();
            let characters = s.characters.as_ref().and_then(|e| {
                if let Expression::Literal(lit) = e {
                    if let LiteralKind::Integer(n) = &lit.kind {
                        return Some(*n);
                    }
                }
                None
            });
            let returning = s.returning.as_ref().map(|q| q.name.to_uppercase());
            Some(SimpleStatement::Allocate {
                data_name,
                characters,
                returning,
            })
        }

        // -- FREE --
        Statement::Free(s) => {
            let pointers: Vec<String> = s
                .pointers
                .iter()
                .map(|q| q.name.to_uppercase())
                .collect();
            Some(SimpleStatement::Free { pointers })
        }

        // -- ENTRY --
        Statement::Entry(s) => Some(SimpleStatement::Entry {
            literal: s.literal.clone(),
        }),

        // -- ALTER --
        Statement::Alter(s) => Some(SimpleStatement::Alter {
            source: s.source.to_uppercase(),
            target: s.target.to_uppercase(),
        }),

        // -- INVOKE -- (no direct interpreter equivalent; skip)
        Statement::Invoke(_) => None,

        // Catch-all for any future statement variants — silently skip.
        #[allow(unreachable_patterns)]
        _ => None,
    }
}

// ---------------------------------------------------------------------------
// PERFORM lowering
// ---------------------------------------------------------------------------

fn lower_perform(s: &PerformStatement) -> Option<SimpleStatement> {
    // Inline PERFORM (with embedded statements).
    if let Some(ref inline_stmts) = s.inline {
        let statements = lower_statements(inline_stmts);
        let until = s.until.as_ref().map(lower_condition);
        let varying = s.varying.as_ref().map(|v| {
            (
                v.variable.name.to_uppercase(),
                lower_expr(&v.from),
                lower_expr(&v.by),
            )
        });
        let after: Vec<VaryingAfter> = s
            .varying
            .as_ref()
            .map(|v| {
                v.after
                    .iter()
                    .map(|a| VaryingAfter {
                        variable: a.variable.name.to_uppercase(),
                        from: lower_expr(&a.from),
                        by: lower_expr(&a.by),
                        until: lower_condition(&a.until),
                    })
                    .collect()
            })
            .unwrap_or_default();

        // If we have a VARYING clause, use the varying's UNTIL as the loop
        // condition (overrides the top-level `until`).
        let effective_until = if s.varying.is_some() {
            s.varying.as_ref().map(|v| lower_condition(&v.until))
        } else {
            until
        };

        return Some(SimpleStatement::PerformInline {
            until: effective_until,
            statements,
            varying,
            after,
        });
    }

    // Named PERFORM (calls a paragraph/section).
    if let Some(ref target) = s.target {
        let target_name = target.name.to_uppercase();

        // Determine static TIMES value if possible.
        let times = s.times.as_ref().and_then(|e| {
            if let Expression::Literal(lit) = e {
                if let LiteralKind::Integer(n) = &lit.kind {
                    return Some(*n as u32);
                }
            }
            None
        });

        // PERFORM ... VARYING with a named target.
        if let Some(ref varying) = s.varying {
            let stmts = vec![SimpleStatement::Perform {
                target: target_name.clone(),
                times: None,
            }];
            let varying_tuple = (
                varying.variable.name.to_uppercase(),
                lower_expr(&varying.from),
                lower_expr(&varying.by),
            );
            let until = lower_condition(&varying.until);
            let after: Vec<VaryingAfter> = varying
                .after
                .iter()
                .map(|a| VaryingAfter {
                    variable: a.variable.name.to_uppercase(),
                    from: lower_expr(&a.from),
                    by: lower_expr(&a.by),
                    until: lower_condition(&a.until),
                })
                .collect();

            return Some(SimpleStatement::PerformInline {
                until: Some(until),
                statements: stmts,
                varying: Some(varying_tuple),
                after,
            });
        }

        // PERFORM ... UNTIL with a named target.
        if let Some(ref until_cond) = s.until {
            let stmts = vec![SimpleStatement::Perform {
                target: target_name.clone(),
                times: None,
            }];
            return Some(SimpleStatement::PerformInline {
                until: Some(lower_condition(until_cond)),
                statements: stmts,
                varying: None,
                after: Vec::new(),
            });
        }

        // PERFORM ... THRU ...
        if let Some(ref thru) = s.thru {
            return Some(SimpleStatement::PerformThru {
                from: target_name,
                thru: thru.to_uppercase(),
                times,
            });
        }

        // Simple PERFORM target [n TIMES].
        return Some(SimpleStatement::Perform {
            target: target_name,
            times,
        });
    }

    // Bare PERFORM with no target and no inline — treated as no-op.
    None
}

// ---------------------------------------------------------------------------
// SET statement lowering
// ---------------------------------------------------------------------------

fn lower_set_statement(s: &SetStatement) -> Option<SimpleStatement> {
    match &s.mode {
        SetMode::IndexTo { targets, value } => {
            // SET target(s) TO value — emit one Set per target.
            // For simplicity, take the first target.
            let target = targets
                .first()
                .map(|q| q.name.to_uppercase())
                .unwrap_or_default();
            let val = lower_expr(value);
            Some(SimpleStatement::Set {
                target,
                value: val,
            })
        }
        SetMode::IndexUpDown {
            targets,
            up,
            value,
        } => {
            // SET target UP/DOWN BY value — translate to Set with Add/Subtract.
            let target = targets
                .first()
                .map(|q| q.name.to_uppercase())
                .unwrap_or_default();
            let val = lower_expr(value);
            if *up {
                Some(SimpleStatement::Set {
                    target: target.clone(),
                    value: SimpleExpr::Binary {
                        left: Box::new(SimpleExpr::Variable(target)),
                        op: SimpleBinaryOp::Add,
                        right: Box::new(val),
                    },
                })
            } else {
                Some(SimpleStatement::Set {
                    target: target.clone(),
                    value: SimpleExpr::Binary {
                        left: Box::new(SimpleExpr::Variable(target)),
                        op: SimpleBinaryOp::Subtract,
                        right: Box::new(val),
                    },
                })
            }
        }
        SetMode::ConditionTo { target, value } => Some(SimpleStatement::SetCondition {
            condition_name: target.name.to_uppercase(),
            value: *value,
        }),
        SetMode::AddressOf { target, source } => {
            // SET ADDRESS OF target TO source — approximate as a Move.
            Some(SimpleStatement::Set {
                target: target.name.to_uppercase(),
                value: SimpleExpr::Variable(source.name.to_uppercase()),
            })
        }
    }
}

// ---------------------------------------------------------------------------
// INSPECT lowering
// ---------------------------------------------------------------------------

fn lower_inspect(s: &InspectStatement) -> Option<SimpleStatement> {
    let target = s.target.name.to_uppercase();

    if let Some(ref tallying) = s.tallying {
        let counter = tallying.counter.name.to_uppercase();
        let for_clauses: Vec<SimpleInspectFor> = tallying
            .for_clauses
            .iter()
            .map(|fc| {
                let mode = lower_inspect_mode(fc.mode);
                let pattern = fc.pattern.as_ref().map(|e| expr_to_string_value(e));
                let (before, after) = extract_inspect_delimiters(&fc.delimiters);
                SimpleInspectFor {
                    mode,
                    pattern,
                    before,
                    after,
                }
            })
            .collect();
        return Some(SimpleStatement::InspectTallying {
            target,
            counter,
            for_clauses,
        });
    }

    if let Some(ref replacing) = s.replacing {
        let rules: Vec<SimpleInspectRule> = replacing
            .rules
            .iter()
            .map(|rule| {
                let mode = lower_inspect_mode(rule.mode);
                let pattern = rule.pattern.as_ref().map(|e| expr_to_string_value(e));
                let by = expr_to_string_value(&rule.by);
                let (before, after) = extract_inspect_delimiters(&rule.delimiters);
                SimpleInspectRule {
                    mode,
                    pattern,
                    by,
                    before,
                    after,
                }
            })
            .collect();
        return Some(SimpleStatement::InspectReplacing { target, rules });
    }

    if let Some(ref converting) = s.converting {
        let from = expr_to_string_value(&converting.from);
        let to = expr_to_string_value(&converting.to);
        let (before, after) = extract_inspect_delimiters(&converting.delimiters);
        return Some(SimpleStatement::InspectConverting {
            target,
            from,
            to,
            before,
            after,
        });
    }

    None
}

fn lower_inspect_mode(mode: InspectMode) -> SimpleInspectMode {
    match mode {
        InspectMode::Characters => SimpleInspectMode::Characters,
        InspectMode::All => SimpleInspectMode::All,
        InspectMode::Leading => SimpleInspectMode::Leading,
        InspectMode::First => SimpleInspectMode::First,
    }
}

/// Extract BEFORE/AFTER INITIAL delimiter strings from INSPECT delimiter
/// clauses.
fn extract_inspect_delimiters(
    delimiters: &[InspectDelimiter],
) -> (Option<String>, Option<String>) {
    let mut before = None;
    let mut after = None;
    for d in delimiters {
        let val = expr_to_string_value(&d.value);
        if d.before {
            before = Some(val);
        } else {
            after = Some(val);
        }
    }
    (before, after)
}

// ---------------------------------------------------------------------------
// Expression lowering
// ---------------------------------------------------------------------------

fn lower_expr(expr: &Expression) -> SimpleExpr {
    match expr {
        Expression::Literal(lit) => lower_literal(lit),
        Expression::Variable(qn) => lower_qualified_name_to_expr(qn),
        Expression::RefMod(rm) => {
            let variable = Box::new(lower_qualified_name_to_expr(&rm.variable));
            let start = Box::new(lower_expr(&rm.start));
            let length = rm.length.as_ref().map(|l| Box::new(lower_expr(l)));
            SimpleExpr::RefMod {
                variable,
                start,
                length,
            }
        }
        Expression::Function(fc) => {
            let name = fc.name.to_uppercase();
            let args: Vec<SimpleExpr> = fc.arguments.iter().map(lower_expr).collect();
            SimpleExpr::FunctionCall { name, args }
        }
        Expression::Binary(bin) => {
            let left = Box::new(lower_expr(&bin.left));
            let right = Box::new(lower_expr(&bin.right));
            let op = match bin.op {
                BinaryOp::Add => SimpleBinaryOp::Add,
                BinaryOp::Subtract => SimpleBinaryOp::Subtract,
                BinaryOp::Multiply => SimpleBinaryOp::Multiply,
                BinaryOp::Divide => SimpleBinaryOp::Divide,
                // Power has no SimpleBinaryOp equivalent; approximate as multiply.
                BinaryOp::Power => SimpleBinaryOp::Multiply,
            };
            SimpleExpr::Binary { left, op, right }
        }
        Expression::Unary(un) => {
            let operand = lower_expr(&un.operand);
            match un.op {
                UnaryOp::Minus => SimpleExpr::Binary {
                    left: Box::new(SimpleExpr::Integer(0)),
                    op: SimpleBinaryOp::Subtract,
                    right: Box::new(operand),
                },
                UnaryOp::Plus => operand,
            }
        }
        Expression::Paren(inner) => lower_expr(inner),
        Expression::LengthOf(lo) => {
            // LENGTH OF x → FUNCTION LENGTH(x)
            SimpleExpr::FunctionCall {
                name: "LENGTH".to_string(),
                args: vec![SimpleExpr::Variable(lo.item.name.to_uppercase())],
            }
        }
        Expression::AddressOf(ao) => {
            // ADDRESS OF x — no direct equivalent; use variable reference.
            SimpleExpr::Variable(ao.item.name.to_uppercase())
        }
    }
}

/// Convert a `QualifiedName` to a `SimpleExpr`, handling subscripts and
/// reference modification.
fn lower_qualified_name_to_expr(qn: &QualifiedName) -> SimpleExpr {
    let base_name = qn.name.to_uppercase();

    // COBOL keywords TRUE/FALSE used as expressions (e.g. EVALUATE TRUE).
    if qn.subscripts.is_empty() && qn.refmod.is_none() {
        match base_name.as_str() {
            "TRUE" => return SimpleExpr::Integer(1),
            "FALSE" => return SimpleExpr::Integer(0),
            _ => {}
        }
    }

    // Handle subscripts: ARRAY(idx) → Subscript.
    let base = if !qn.subscripts.is_empty() {
        // Use the first subscript for simple single-dimensional arrays.
        let index = Box::new(lower_expr(&qn.subscripts[0]));
        SimpleExpr::Subscript {
            variable: base_name,
            index,
        }
    } else {
        SimpleExpr::Variable(base_name)
    };

    // Handle reference modification: VAR(start:length).
    if let Some((ref start, ref length)) = qn.refmod {
        let start_expr = Box::new(lower_expr(start));
        let length_expr = length.as_ref().map(|l| Box::new(lower_expr(l)));
        SimpleExpr::RefMod {
            variable: Box::new(base),
            start: start_expr,
            length: length_expr,
        }
    } else {
        base
    }
}

fn lower_literal(lit: &Literal) -> SimpleExpr {
    match &lit.kind {
        LiteralKind::Integer(n) => SimpleExpr::Integer(*n),
        LiteralKind::Decimal(s) => {
            // Try to parse as integer if there are no fractional digits.
            if let Ok(n) = s.parse::<i64>() {
                SimpleExpr::Integer(n)
            } else {
                SimpleExpr::String(s.clone())
            }
        }
        LiteralKind::String(s) => SimpleExpr::String(s.clone()),
        LiteralKind::Hex(s) => {
            let mut result = String::new();
            let hex_chars: Vec<char> = s.chars().collect();
            for chunk in hex_chars.chunks(2) {
                if chunk.len() == 2 {
                    let hex_str: String = chunk.iter().collect();
                    if let Ok(byte) = u8::from_str_radix(&hex_str, 16) {
                        result.push(char::from(byte));
                    }
                }
            }
            SimpleExpr::String(result)
        }
        LiteralKind::Figurative(fig) => match fig {
            FigurativeConstant::Zero => SimpleExpr::Integer(0),
            FigurativeConstant::Space => SimpleExpr::String(" ".to_string()),
            FigurativeConstant::HighValue => SimpleExpr::String("\u{FF}".to_string()),
            FigurativeConstant::LowValue => SimpleExpr::String("\0".to_string()),
            FigurativeConstant::Quote => SimpleExpr::String("\"".to_string()),
            FigurativeConstant::All => SimpleExpr::String(String::new()),
        },
        LiteralKind::AllOf(inner) => lower_literal(inner),
    }
}

// ---------------------------------------------------------------------------
// Condition lowering
// ---------------------------------------------------------------------------

fn lower_condition(cond: &Condition) -> SimpleCondition {
    match cond {
        Condition::Comparison(cmp) => {
            let left = lower_expr(&cmp.left);
            let right = lower_expr(&cmp.right);
            let op = match cmp.op {
                ComparisonOp::Equal => SimpleCompareOp::Equal,
                ComparisonOp::NotEqual => SimpleCompareOp::NotEqual,
                ComparisonOp::GreaterThan => SimpleCompareOp::GreaterThan,
                ComparisonOp::GreaterOrEqual => SimpleCompareOp::GreaterOrEqual,
                ComparisonOp::LessThan => SimpleCompareOp::LessThan,
                ComparisonOp::LessOrEqual => SimpleCompareOp::LessOrEqual,
            };
            SimpleCondition::Compare { left, op, right }
        }
        Condition::Class(cc) => {
            let operand = lower_expr(&cc.operand);
            let class = match cc.class {
                ClassType::Numeric => SimpleClassType::Numeric,
                ClassType::Alphabetic => SimpleClassType::Alphabetic,
                ClassType::AlphabeticLower => SimpleClassType::AlphabeticLower,
                ClassType::AlphabeticUpper => SimpleClassType::AlphabeticUpper,
            };
            SimpleCondition::ClassCondition {
                operand,
                class,
                negated: cc.negated,
            }
        }
        Condition::Sign(sc) => {
            // Sign conditions: map to comparison with zero.
            let operand = lower_expr(&sc.operand);
            let (op, val) = match sc.sign {
                SignType::Positive => (SimpleCompareOp::GreaterThan, SimpleExpr::Integer(0)),
                SignType::Negative => (SimpleCompareOp::LessThan, SimpleExpr::Integer(0)),
                SignType::Zero => (SimpleCompareOp::Equal, SimpleExpr::Integer(0)),
            };
            let condition = SimpleCondition::Compare {
                left: operand,
                op,
                right: val,
            };
            if sc.negated {
                SimpleCondition::Not(Box::new(condition))
            } else {
                condition
            }
        }
        Condition::ConditionName(qn) => {
            SimpleCondition::ConditionName(qn.name.to_uppercase())
        }
        Condition::Not(inner) => {
            SimpleCondition::Not(Box::new(lower_condition(inner)))
        }
        Condition::And(left, right) => SimpleCondition::And(
            Box::new(lower_condition(left)),
            Box::new(lower_condition(right)),
        ),
        Condition::Or(left, right) => SimpleCondition::Or(
            Box::new(lower_condition(left)),
            Box::new(lower_condition(right)),
        ),
        Condition::Paren(inner) => lower_condition(inner),
    }
}

// ---------------------------------------------------------------------------
// EVALUATE WHEN condition lowering
// ---------------------------------------------------------------------------

/// Convert a list of `WhenCondition`s (one per subject) into a single
/// `SimpleCondition` for the interpreter.
fn lower_when_clause_to_condition(
    subjects: &[SimpleExpr],
    conditions: &[WhenCondition],
) -> SimpleCondition {
    let mut parts: Vec<SimpleCondition> = Vec::new();

    for (i, wc) in conditions.iter().enumerate() {
        let subject = subjects.get(i).cloned().unwrap_or(SimpleExpr::Integer(0));

        match wc {
            WhenCondition::Any => {
                // ANY matches anything — always true.
                parts.push(SimpleCondition::Compare {
                    left: SimpleExpr::Integer(1),
                    op: SimpleCompareOp::Equal,
                    right: SimpleExpr::Integer(1),
                });
            }
            WhenCondition::True => {
                // EVALUATE TRUE → the condition was already the subject;
                // TRUE matches when the subject is truthy. Since subjects
                // for EVALUATE TRUE are typically conditions themselves, we
                // treat this as "always true" for the when-clause matching.
                parts.push(SimpleCondition::Compare {
                    left: subject,
                    op: SimpleCompareOp::Equal,
                    right: SimpleExpr::Integer(1),
                });
            }
            WhenCondition::False => {
                parts.push(SimpleCondition::Compare {
                    left: subject,
                    op: SimpleCompareOp::Equal,
                    right: SimpleExpr::Integer(0),
                });
            }
            WhenCondition::Value(expr) => {
                let val = lower_expr(expr);
                parts.push(SimpleCondition::Compare {
                    left: subject,
                    op: SimpleCompareOp::Equal,
                    right: val,
                });
            }
            WhenCondition::Range { from, to } => {
                let from_val = lower_expr(from);
                let to_val = lower_expr(to);
                // subject >= from AND subject <= to
                let ge = SimpleCondition::Compare {
                    left: subject.clone(),
                    op: SimpleCompareOp::GreaterOrEqual,
                    right: from_val,
                };
                let le = SimpleCondition::Compare {
                    left: subject,
                    op: SimpleCompareOp::LessOrEqual,
                    right: to_val,
                };
                parts.push(SimpleCondition::And(Box::new(ge), Box::new(le)));
            }
            WhenCondition::Condition(cond) => {
                parts.push(lower_condition(cond));
            }
        }
    }

    // Combine all parts with AND.
    if parts.is_empty() {
        SimpleCondition::Compare {
            left: SimpleExpr::Integer(1),
            op: SimpleCompareOp::Equal,
            right: SimpleExpr::Integer(1),
        }
    } else {
        let mut combined = parts.remove(0);
        for part in parts {
            combined = SimpleCondition::And(Box::new(combined), Box::new(part));
        }
        combined
    }
}

// ---------------------------------------------------------------------------
// Utility helpers
// ---------------------------------------------------------------------------

/// Best-effort extraction of a string value from an expression, for use in
/// contexts that require a plain `String` (INSPECT delimiters, UNSTRING
/// delimiters, etc.).
fn expr_to_string_value(expr: &Expression) -> String {
    match expr {
        Expression::Literal(lit) => literal_to_string(lit),
        Expression::Variable(qn) => qn.name.to_uppercase(),
        _ => format!("{:?}", expr),
    }
}

/// Best-effort extraction of a variable name from an expression.
fn expr_to_variable_name(expr: &Expression) -> String {
    match expr {
        Expression::Variable(qn) => qn.name.to_uppercase(),
        Expression::Literal(lit) => literal_to_string(lit),
        _ => String::new(),
    }
}

/// Try to extract a static integer from an expression.
fn expr_to_integer(expr: &Expression) -> Option<i64> {
    if let Expression::Literal(lit) = expr {
        if let LiteralKind::Integer(n) = &lit.kind {
            return Some(*n);
        }
    }
    None
}
