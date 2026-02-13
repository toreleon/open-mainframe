//! Semantic analyzer for COBOL programs.
//!
//! This module walks the AST and:
//! - Builds the symbol table
//! - Performs type checking
//! - Validates references
//! - Generates diagnostics

use super::symbol_table::{Scope, SymbolId, SymbolTable};
use super::types::{CobolType, TypeCategory};
use crate::ast::*;
use crate::lexer::Span;

/// Diagnostic severity.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Severity {
    /// Error - prevents compilation.
    Error,
    /// Warning - compilation continues.
    Warning,
    /// Info - informational message.
    Info,
}

/// A semantic diagnostic message.
#[derive(Debug, Clone)]
pub struct Diagnostic {
    /// Severity of the diagnostic.
    pub severity: Severity,
    /// Error code (e.g., "E001").
    pub code: String,
    /// Human-readable message.
    pub message: String,
    /// Source location.
    pub span: Span,
    /// Optional suggestion for fixing.
    pub suggestion: Option<String>,
}

impl Diagnostic {
    /// Create a new error diagnostic.
    pub fn error(code: impl Into<String>, message: impl Into<String>, span: Span) -> Self {
        Self {
            severity: Severity::Error,
            code: code.into(),
            message: message.into(),
            span,
            suggestion: None,
        }
    }

    /// Create a new warning diagnostic.
    pub fn warning(code: impl Into<String>, message: impl Into<String>, span: Span) -> Self {
        Self {
            severity: Severity::Warning,
            code: code.into(),
            message: message.into(),
            span,
            suggestion: None,
        }
    }

    /// Add a suggestion to this diagnostic.
    pub fn with_suggestion(mut self, suggestion: impl Into<String>) -> Self {
        self.suggestion = Some(suggestion.into());
        self
    }
}

/// Result of semantic analysis.
pub struct SemanticResult {
    /// The symbol table.
    pub symbol_table: SymbolTable,
    /// Diagnostics generated during analysis.
    pub diagnostics: Vec<Diagnostic>,
    /// Whether any errors were found.
    pub has_errors: bool,
}

/// Semantic analyzer for COBOL programs.
pub struct SemanticAnalyzer {
    /// Symbol table being built.
    symbol_table: SymbolTable,
    /// Accumulated diagnostics.
    diagnostics: Vec<Diagnostic>,
    /// Current scope.
    current_scope: Scope,
    /// Current offset for data items.
    current_offset: u32,
    /// Whether to treat warnings as errors.
    strict_mode: bool,
}

impl SemanticAnalyzer {
    /// Create a new semantic analyzer.
    pub fn new() -> Self {
        Self {
            symbol_table: SymbolTable::new(),
            diagnostics: Vec::new(),
            current_scope: Scope::Global,
            current_offset: 0,
            strict_mode: false,
        }
    }

    /// Enable strict mode (warnings become errors).
    pub fn strict(mut self) -> Self {
        self.strict_mode = true;
        self
    }

    /// Analyze a COBOL program.
    pub fn analyze(mut self, program: &Program) -> SemanticResult {
        // Phase 1: Build symbol table from declarations
        self.build_symbols(program);

        // Phase 2: Validate references and perform type checking
        if program.procedure.is_some() {
            self.validate_procedure(program.procedure.as_ref().unwrap());
        }

        let has_errors = self.diagnostics.iter().any(|d| {
            d.severity == Severity::Error || (self.strict_mode && d.severity == Severity::Warning)
        });

        SemanticResult {
            symbol_table: self.symbol_table,
            diagnostics: self.diagnostics,
            has_errors,
        }
    }

    /// Add an error diagnostic.
    fn error(&mut self, code: &str, message: impl Into<String>, span: Span) {
        self.diagnostics
            .push(Diagnostic::error(code, message, span));
    }

    /// Add a warning diagnostic.
    fn warning(&mut self, code: &str, message: impl Into<String>, span: Span) {
        self.diagnostics
            .push(Diagnostic::warning(code, message, span));
    }

    // ========================================================================
    // Phase 1: Symbol Table Construction
    // ========================================================================

    fn build_symbols(&mut self, program: &Program) {
        // Process DATA DIVISION
        if let Some(ref data) = program.data {
            self.process_data_division(data);
        }

        // Process PROCEDURE DIVISION
        if let Some(ref procedure) = program.procedure {
            self.process_procedure_symbols(procedure);
        }
    }

    fn process_data_division(&mut self, data: &DataDivision) {
        // File section
        self.current_scope = Scope::FileSection;
        for fd in &data.file_section {
            self.process_file_description(fd);
        }

        // Working storage
        self.current_scope = Scope::WorkingStorage;
        self.current_offset = 0;
        for item in &data.working_storage {
            self.process_data_item(item, None);
        }

        // Local storage
        self.current_scope = Scope::LocalStorage;
        self.current_offset = 0;
        for item in &data.local_storage {
            self.process_data_item(item, None);
        }

        // Linkage section
        self.current_scope = Scope::Linkage;
        self.current_offset = 0;
        for item in &data.linkage {
            self.process_data_item(item, None);
        }
    }

    fn process_file_description(&mut self, fd: &FileDescription) {
        let file_id = self.symbol_table.add_file(fd.name.clone(), fd.span);

        self.current_offset = 0;
        for record in &fd.records {
            let record_id = self.process_data_item(record, None);
            if let Some(id) = record_id {
                self.symbol_table.add_record_to_file(file_id, id);
            }
        }
    }

    fn process_data_item(&mut self, item: &DataItem, parent: Option<SymbolId>) -> Option<SymbolId> {
        let name = match &item.name {
            DataItemName::Named(n) => n.clone(),
            DataItemName::Filler => format!("FILLER-{}", self.symbol_table.len()),
        };

        // Determine type from PICTURE and USAGE
        let cobol_type = Self::derive_type(item);
        let size = cobol_type.total_size();

        // Check for duplicate at same level
        let mut is_duplicate = false;
        if let DataItemName::Named(ref n) = item.name {
            let existing = self.symbol_table.lookup_data_item(n);
            for sym in existing {
                if let super::symbol_table::SymbolKind::DataItem {
                    level,
                    parent: sym_parent,
                    ..
                } = &sym.kind
                {
                    if *level == item.level && *sym_parent == parent {
                        is_duplicate = true;
                        break;
                    }
                }
            }
        }
        if is_duplicate {
            if let DataItemName::Named(ref n) = item.name {
                self.error(
                    "E101",
                    format!("Duplicate data item '{}' at level {}", n, item.level),
                    item.span,
                );
            }
        }

        let item_id = self.symbol_table.add_data_item(
            name,
            item.level,
            parent,
            cobol_type,
            self.current_offset,
            item.span,
        );

        // Update offset
        self.current_offset += size;

        // Add to parent's children
        if let Some(parent_id) = parent {
            self.symbol_table.add_child(parent_id, item_id);
        }

        // Process children (for group items)
        for child in &item.children {
            self.process_data_item(child, Some(item_id));
        }

        // Process condition values (level 88)
        for cond in &item.condition_values {
            self.symbol_table
                .add_condition_name(cond.name.clone(), item_id, cond.span);
        }

        // Process indexes from OCCURS clause
        if let Some(ref occurs) = item.occurs {
            for index_name in &occurs.indexed_by {
                self.symbol_table
                    .add_index(index_name.clone(), item_id, occurs.span);
            }
        }

        Some(item_id)
    }

    fn derive_type(item: &DataItem) -> CobolType {
        // Group item (no PICTURE)
        if item.picture.is_none() && !item.children.is_empty() {
            let child_size: u32 = item
                .children
                .iter()
                .map(|c| Self::derive_type(c).total_size())
                .sum();
            return CobolType::group(child_size);
        }

        // Elementary item
        let (category, size, decimal_positions) = if let Some(ref pic) = item.picture {
            (
                TypeCategory::from(pic.category),
                pic.size,
                pic.decimal_positions,
            )
        } else {
            // No picture and no children - assume alphanumeric
            (TypeCategory::Alphanumeric, 1, 0)
        };

        let usage = item.usage.unwrap_or(Usage::Display);
        let is_signed = item
            .picture
            .as_ref()
            .map(|p| p.picture.to_uppercase().contains('S'))
            .unwrap_or(false);

        let storage_size =
            CobolType::calculate_storage_size(size, decimal_positions, usage, is_signed);

        let occurs = item.occurs.as_ref().map(|o| o.times).unwrap_or(1);

        CobolType {
            category,
            size: storage_size,
            decimal_positions,
            usage,
            is_signed,
            is_group: false,
            occurs,
        }
    }

    fn process_procedure_symbols(&mut self, procedure: &ProcedureDivision) {
        self.current_scope = Scope::Procedure;

        match &procedure.body {
            ProcedureBody::Sections(sections) => {
                for section in sections {
                    let section_id = self
                        .symbol_table
                        .add_section(section.name.clone(), section.span);
                    for para in &section.paragraphs {
                        let para_id = self.symbol_table.add_paragraph(
                            para.name.clone(),
                            Some(section_id),
                            para.span,
                        );
                        self.symbol_table
                            .add_paragraph_to_section(section_id, para_id);
                    }
                }
            }
            ProcedureBody::Paragraphs(paragraphs) => {
                for para in paragraphs {
                    self.symbol_table
                        .add_paragraph(para.name.clone(), None, para.span);
                }
            }
            ProcedureBody::Statements(_) => {
                // No named paragraphs/sections
            }
        }
    }

    // ========================================================================
    // Phase 2: Validation and Type Checking
    // ========================================================================

    fn validate_procedure(&mut self, procedure: &ProcedureDivision) {
        match &procedure.body {
            ProcedureBody::Sections(sections) => {
                for section in sections {
                    for para in &section.paragraphs {
                        self.validate_statements(&para.statements);
                    }
                }
            }
            ProcedureBody::Paragraphs(paragraphs) => {
                for para in paragraphs {
                    self.validate_statements(&para.statements);
                }
            }
            ProcedureBody::Statements(statements) => {
                self.validate_statements(statements);
            }
        }
    }

    fn validate_statements(&mut self, statements: &[Statement]) {
        for stmt in statements {
            self.validate_statement(stmt);
        }
    }

    fn validate_statement(&mut self, stmt: &Statement) {
        match stmt {
            Statement::Move(s) => self.validate_move(s),
            Statement::Compute(s) => self.validate_compute(s),
            Statement::Add(s) => self.validate_add(s),
            Statement::Subtract(s) => self.validate_subtract(s),
            Statement::Multiply(s) => self.validate_multiply(s),
            Statement::Divide(s) => self.validate_divide(s),
            Statement::If(s) => self.validate_if(s),
            Statement::Evaluate(s) => self.validate_evaluate(s),
            Statement::Perform(s) => self.validate_perform(s),
            Statement::Call(s) => self.validate_call(s),
            Statement::Display(s) => self.validate_display(s),
            Statement::Read(s) => self.validate_read(s),
            Statement::Write(s) => self.validate_write(s),
            Statement::GoTo(s) => self.validate_goto(s),
            Statement::Initialize(s) => self.validate_initialize(s),
            Statement::Search(s) => self.validate_search(s),
            Statement::String(s) => self.validate_string(s),
            Statement::Unstring(s) => self.validate_unstring(s),
            Statement::Set(s) => self.validate_set(s),
            // Simple statements that don't need validation
            Statement::Accept(_)
            | Statement::Open(_)
            | Statement::Close(_)
            | Statement::StopRun(_)
            | Statement::Exit(_)
            | Statement::Continue(_)
            | Statement::Inspect(_)
            | Statement::ExecCics(_)
            | Statement::ExecSql(_) => {}
        }
    }

    fn validate_move(&mut self, stmt: &MoveStatement) {
        // Validate source
        self.validate_expression(&stmt.from);

        // Validate targets
        for target in &stmt.to {
            if let Some(sym) = self.resolve_qualified_name(target) {
                // Type checking: warn if incompatible move
                let target_type = self.get_symbol_type(sym);
                let source_type = self.infer_expression_type(&stmt.from);

                if let (Some(target_type), Some(source_type)) = (target_type, source_type) {
                    if !target_type.can_receive_move_from(source_type) {
                        self.warning(
                            "W201",
                            format!(
                                "Potentially incompatible MOVE from {:?} to {:?}",
                                source_type, target_type
                            ),
                            target.span,
                        );
                    }
                }
            }
        }
    }

    fn validate_compute(&mut self, stmt: &ComputeStatement) {
        // Validate expression
        self.validate_expression(&stmt.expression);

        // Validate targets - must be numeric
        for target in &stmt.targets {
            if let Some(sym) = self.resolve_qualified_name(&target.name) {
                let target_type = self.get_symbol_type(sym);
                if let Some(cat) = target_type {
                    if !cat.is_numeric() {
                        self.error(
                            "E201",
                            format!("COMPUTE target '{}' must be numeric", target.name.name),
                            target.name.span,
                        );
                    }
                }
            }
        }

        // Validate handlers
        if let Some(ref stmts) = stmt.on_size_error {
            self.validate_statements(stmts);
        }
        if let Some(ref stmts) = stmt.not_on_size_error {
            self.validate_statements(stmts);
        }
    }

    fn validate_add(&mut self, stmt: &AddStatement) {
        for op in &stmt.operands {
            self.validate_expression(op);
        }
        for target in &stmt.to {
            self.resolve_qualified_name(&target.name);
        }
        for target in &stmt.giving {
            self.resolve_qualified_name(&target.name);
        }
    }

    fn validate_subtract(&mut self, stmt: &SubtractStatement) {
        for op in &stmt.operands {
            self.validate_expression(op);
        }
        for target in &stmt.from {
            self.resolve_qualified_name(&target.name);
        }
        for target in &stmt.giving {
            self.resolve_qualified_name(&target.name);
        }
    }

    fn validate_multiply(&mut self, stmt: &MultiplyStatement) {
        self.validate_expression(&stmt.operand);
        self.validate_expression(&stmt.by);
        for target in &stmt.giving {
            self.resolve_qualified_name(&target.name);
        }
    }

    fn validate_divide(&mut self, stmt: &DivideStatement) {
        self.validate_expression(&stmt.operand);
        self.validate_expression(&stmt.into_or_by);
        for target in &stmt.giving {
            self.resolve_qualified_name(&target.name);
        }
        if let Some(ref rem) = stmt.remainder {
            self.resolve_qualified_name(rem);
        }
    }

    fn validate_if(&mut self, stmt: &IfStatement) {
        self.validate_condition(&stmt.condition);
        self.validate_statements(&stmt.then_branch);
        if let Some(ref else_stmts) = stmt.else_branch {
            self.validate_statements(else_stmts);
        }
    }

    fn validate_evaluate(&mut self, stmt: &EvaluateStatement) {
        for subject in &stmt.subjects {
            self.validate_expression(subject);
        }
        for when in &stmt.when_clauses {
            for cond in &when.conditions {
                if let WhenCondition::Value(ref expr) = cond {
                    self.validate_expression(expr);
                }
                if let WhenCondition::Condition(ref cond) = cond {
                    self.validate_condition(cond);
                }
            }
            self.validate_statements(&when.statements);
        }
        if let Some(ref other) = stmt.when_other {
            self.validate_statements(other);
        }
    }

    fn validate_perform(&mut self, stmt: &PerformStatement) {
        // Validate target paragraph/section
        if let Some(ref target) = stmt.target {
            if !self.symbol_table.procedure_target_exists(&target.name) {
                self.error(
                    "E301",
                    format!("Undefined paragraph or section: '{}'", target.name),
                    target.span,
                );
            }
        }

        // Validate THRU target
        if let Some(ref thru_name) = stmt.thru {
            if !self.symbol_table.procedure_target_exists(thru_name) {
                self.error(
                    "E301",
                    format!("Undefined paragraph or section in THRU: '{}'", thru_name),
                    stmt.span,
                );
            }
        }

        // Validate inline statements
        if let Some(ref inline) = stmt.inline {
            self.validate_statements(inline);
        }

        // Validate UNTIL condition
        if let Some(ref until) = stmt.until {
            self.validate_condition(until);
        }

        // Validate VARYING clause
        if let Some(ref varying) = stmt.varying {
            self.resolve_qualified_name(&varying.variable);
            self.validate_expression(&varying.from);
            self.validate_expression(&varying.by);
            self.validate_condition(&varying.until);
        }
    }

    fn validate_call(&mut self, stmt: &CallStatement) {
        self.validate_expression(&stmt.program);

        for param in &stmt.using {
            self.validate_expression(&param.value);
        }

        if let Some(ref stmts) = stmt.on_exception {
            self.validate_statements(stmts);
        }
        if let Some(ref stmts) = stmt.not_on_exception {
            self.validate_statements(stmts);
        }
    }

    fn validate_display(&mut self, stmt: &DisplayStatement) {
        for item in &stmt.items {
            self.validate_expression(item);
        }
    }

    fn validate_read(&mut self, stmt: &ReadStatement) {
        // Validate file exists
        if self.symbol_table.lookup_file(&stmt.file).is_none() {
            self.error(
                "E401",
                format!("Undefined file: '{}'", stmt.file),
                stmt.span,
            );
        }

        if let Some(ref into) = stmt.into {
            self.resolve_qualified_name(into);
        }

        if let Some(ref stmts) = stmt.at_end {
            self.validate_statements(stmts);
        }
        if let Some(ref stmts) = stmt.not_at_end {
            self.validate_statements(stmts);
        }
    }

    fn validate_write(&mut self, stmt: &WriteStatement) {
        self.resolve_qualified_name(&stmt.record);

        if let Some(ref from) = stmt.from {
            self.resolve_qualified_name(from);
        }

        if let Some(ref stmts) = stmt.invalid_key {
            self.validate_statements(stmts);
        }
        if let Some(ref stmts) = stmt.not_invalid_key {
            self.validate_statements(stmts);
        }
    }

    fn validate_goto(&mut self, stmt: &GoToStatement) {
        for target in &stmt.targets {
            if !self.symbol_table.procedure_target_exists(target) {
                self.error(
                    "E301",
                    format!("Undefined GO TO target: '{}'", target),
                    stmt.span,
                );
            }
        }

        if let Some(ref dep) = stmt.depending {
            self.resolve_qualified_name(dep);
        }
    }

    fn validate_initialize(&mut self, stmt: &InitializeStatement) {
        for var in &stmt.variables {
            self.resolve_qualified_name(var);
        }
    }

    fn validate_search(&mut self, stmt: &SearchStatement) {
        self.resolve_qualified_name(&stmt.table);

        if let Some(ref varying) = stmt.varying {
            self.resolve_qualified_name(varying);
        }

        if let Some(ref stmts) = stmt.at_end {
            self.validate_statements(stmts);
        }

        for when in &stmt.when_clauses {
            self.validate_condition(&when.condition);
            self.validate_statements(&when.statements);
        }
    }

    fn validate_string(&mut self, stmt: &StringStatement) {
        for source in &stmt.sources {
            self.validate_expression(&source.value);
        }
        self.resolve_qualified_name(&stmt.into);
        if let Some(ref ptr) = stmt.pointer {
            self.resolve_qualified_name(ptr);
        }

        if let Some(ref stmts) = stmt.on_overflow {
            self.validate_statements(stmts);
        }
        if let Some(ref stmts) = stmt.not_on_overflow {
            self.validate_statements(stmts);
        }
    }

    fn validate_unstring(&mut self, stmt: &UnstringStatement) {
        self.resolve_qualified_name(&stmt.source);

        for target in &stmt.into {
            self.resolve_qualified_name(&target.name);
        }

        if let Some(ref ptr) = stmt.pointer {
            self.resolve_qualified_name(ptr);
        }
        if let Some(ref tally) = stmt.tallying {
            self.resolve_qualified_name(tally);
        }

        if let Some(ref stmts) = stmt.on_overflow {
            self.validate_statements(stmts);
        }
        if let Some(ref stmts) = stmt.not_on_overflow {
            self.validate_statements(stmts);
        }
    }

    fn validate_set(&mut self, stmt: &SetStatement) {
        match &stmt.mode {
            SetMode::IndexTo { targets, value } => {
                for target in targets {
                    self.resolve_qualified_name(target);
                }
                self.validate_expression(value);
            }
            SetMode::IndexUpDown { targets, value, .. } => {
                for target in targets {
                    self.resolve_qualified_name(target);
                }
                self.validate_expression(value);
            }
            SetMode::ConditionTo { target, .. } => {
                self.resolve_qualified_name(target);
            }
            SetMode::AddressOf { target, source } => {
                self.resolve_qualified_name(target);
                self.resolve_qualified_name(source);
            }
        }
    }

    fn validate_expression(&mut self, expr: &Expression) {
        match expr {
            Expression::Variable(name) => {
                self.resolve_qualified_name(name);
            }
            Expression::RefMod(refmod) => {
                self.resolve_qualified_name(&refmod.variable);
                self.validate_expression(&refmod.start);
                if let Some(ref len) = refmod.length {
                    self.validate_expression(len);
                }
            }
            Expression::Function(func) => {
                for arg in &func.arguments {
                    self.validate_expression(arg);
                }
            }
            Expression::Binary(bin) => {
                self.validate_expression(&bin.left);
                self.validate_expression(&bin.right);
            }
            Expression::Unary(un) => {
                self.validate_expression(&un.operand);
            }
            Expression::Paren(inner) => {
                self.validate_expression(inner);
            }
            Expression::Literal(_) => {}
            Expression::LengthOf(len_of) => {
                self.resolve_qualified_name(&len_of.item);
            }
            Expression::AddressOf(addr_of) => {
                self.resolve_qualified_name(&addr_of.item);
            }
        }
    }

    fn validate_condition(&mut self, cond: &Condition) {
        match cond {
            Condition::Comparison(cmp) => {
                self.validate_expression(&cmp.left);
                self.validate_expression(&cmp.right);
            }
            Condition::Class(cls) => {
                self.validate_expression(&cls.operand);
            }
            Condition::Sign(sign) => {
                self.validate_expression(&sign.operand);
            }
            Condition::ConditionName(name) => {
                self.resolve_qualified_name(name);
            }
            Condition::Not(inner) => {
                self.validate_condition(inner);
            }
            Condition::And(left, right) | Condition::Or(left, right) => {
                self.validate_condition(left);
                self.validate_condition(right);
            }
            Condition::Paren(inner) => {
                self.validate_condition(inner);
            }
        }
    }

    fn resolve_qualified_name(&mut self, name: &QualifiedName) -> Option<SymbolId> {
        // Try qualified lookup
        let symbol_id = self
            .symbol_table
            .lookup_qualified(&name.name, &name.qualifiers)
            .map(|s| s.id);

        if symbol_id.is_none() {
            // Also check if it's an index
            let is_index = self.symbol_table.lookup_index(&name.name).is_some();
            if !is_index {
                self.error(
                    "E102",
                    format!("Undefined identifier: '{}'", name.name),
                    name.span,
                );
            }
        }

        // Validate subscripts
        for subscript in &name.subscripts {
            self.validate_expression(subscript);
        }

        symbol_id
    }

    fn get_symbol_type(&self, id: SymbolId) -> Option<TypeCategory> {
        self.symbol_table.get(id).and_then(|sym| match &sym.kind {
            super::symbol_table::SymbolKind::DataItem { cobol_type, .. } => {
                Some(cobol_type.category)
            }
            _ => None,
        })
    }

    fn infer_expression_type(&self, expr: &Expression) -> Option<TypeCategory> {
        match expr {
            Expression::Literal(lit) => match &lit.kind {
                LiteralKind::Integer(_) | LiteralKind::Decimal(_) => Some(TypeCategory::Numeric),
                LiteralKind::String(_) | LiteralKind::Hex(_) => Some(TypeCategory::Alphanumeric),
                LiteralKind::Figurative(fig) => match fig {
                    FigurativeConstant::Zero => Some(TypeCategory::Numeric),
                    FigurativeConstant::Space
                    | FigurativeConstant::HighValue
                    | FigurativeConstant::LowValue
                    | FigurativeConstant::Quote
                    | FigurativeConstant::All => Some(TypeCategory::Alphanumeric),
                },
                LiteralKind::AllOf(inner) => self.infer_expression_type(&Expression::Literal((**inner).clone())),
            },
            Expression::Variable(name) => self
                .symbol_table
                .lookup_qualified(&name.name, &name.qualifiers)
                .and_then(|s| self.get_symbol_type(s.id)),
            Expression::Binary(_) => Some(TypeCategory::Numeric),
            Expression::Unary(_) => Some(TypeCategory::Numeric),
            Expression::Function(_) => None, // Would need function return type
            Expression::RefMod(_) => Some(TypeCategory::Alphanumeric),
            Expression::Paren(inner) => self.infer_expression_type(inner),
            Expression::LengthOf(_) => Some(TypeCategory::Numeric), // LENGTH OF returns integer
            Expression::AddressOf(_) => Some(TypeCategory::Pointer), // ADDRESS OF returns pointer
        }
    }
}

impl Default for SemanticAnalyzer {
    fn default() -> Self {
        Self::new()
    }
}

/// Analyze a COBOL program and return semantic results.
pub fn analyze(program: &Program) -> SemanticResult {
    SemanticAnalyzer::new().analyze(program)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::{scan, FileId, SourceFile, SourceFormat};
    use crate::parser::parse;

    fn analyze_text(text: &str) -> SemanticResult {
        let source = SourceFile::from_text(FileId::MAIN, text.to_string(), SourceFormat::Free);
        let (tokens, _) = scan(&source);
        let (program, _) = parse(tokens);
        let program = program.expect("Program should parse");
        analyze(&program)
    }

    #[test]
    fn test_build_symbol_table() {
        let result = analyze_text(
            r#"
            IDENTIFICATION DIVISION.
            PROGRAM-ID. TEST.
            DATA DIVISION.
            WORKING-STORAGE SECTION.
            01 WS-NAME PIC X(20).
            01 WS-COUNT PIC 9(5).
            PROCEDURE DIVISION.
                STOP RUN.
        "#,
        );

        assert!(!result.has_errors);
        assert_eq!(result.symbol_table.lookup_data_item("WS-NAME").len(), 1);
        assert_eq!(result.symbol_table.lookup_data_item("WS-COUNT").len(), 1);
    }

    #[test]
    fn test_undefined_variable_error() {
        let result = analyze_text(
            r#"
            IDENTIFICATION DIVISION.
            PROGRAM-ID. TEST.
            DATA DIVISION.
            WORKING-STORAGE SECTION.
            01 WS-NAME PIC X(20).
            PROCEDURE DIVISION.
                MOVE WS-UNDEFINED TO WS-NAME.
                STOP RUN.
        "#,
        );

        assert!(result.has_errors);
        let errors: Vec<_> = result
            .diagnostics
            .iter()
            .filter(|d| d.severity == Severity::Error)
            .collect();
        assert!(!errors.is_empty());
        assert!(errors.iter().any(|e| e.message.contains("WS-UNDEFINED")));
    }

    #[test]
    fn test_undefined_paragraph_error() {
        let result = analyze_text(
            r#"
            IDENTIFICATION DIVISION.
            PROGRAM-ID. TEST.
            PROCEDURE DIVISION.
                PERFORM NONEXISTENT-PARA.
                STOP RUN.
        "#,
        );

        assert!(result.has_errors);
        let errors: Vec<_> = result
            .diagnostics
            .iter()
            .filter(|d| d.severity == Severity::Error)
            .collect();
        assert!(!errors.is_empty());
        assert!(errors
            .iter()
            .any(|e| e.message.contains("NONEXISTENT-PARA")));
    }

    #[test]
    fn test_valid_perform_target() {
        let result = analyze_text(
            r#"
            IDENTIFICATION DIVISION.
            PROGRAM-ID. TEST.
            PROCEDURE DIVISION.
            MAIN-PARA.
                PERFORM HELPER-PARA.
                STOP RUN.
            HELPER-PARA.
                DISPLAY "HELPER".
        "#,
        );

        // Should have no errors about undefined paragraph
        let errors: Vec<_> = result
            .diagnostics
            .iter()
            .filter(|d| d.severity == Severity::Error && d.message.contains("HELPER-PARA"))
            .collect();
        assert!(errors.is_empty());
    }

    #[test]
    fn test_qualified_name_resolution() {
        let result = analyze_text(
            r#"
            IDENTIFICATION DIVISION.
            PROGRAM-ID. TEST.
            DATA DIVISION.
            WORKING-STORAGE SECTION.
            01 CUSTOMER-RECORD.
               05 CUSTOMER-NAME PIC X(30).
               05 CUSTOMER-ID PIC 9(5).
            01 VENDOR-RECORD.
               05 CUSTOMER-NAME PIC X(30).
            PROCEDURE DIVISION.
                MOVE "JOHN" TO CUSTOMER-NAME OF CUSTOMER-RECORD.
                STOP RUN.
        "#,
        );

        // Both CUSTOMER-NAME items should be in the table
        let names = result.symbol_table.lookup_data_item("CUSTOMER-NAME");
        assert_eq!(names.len(), 2);
    }
}
