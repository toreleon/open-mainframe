//! Interpret command implementation.
//!
//! Runs COBOL programs directly using the tree-walking interpreter.

use std::collections::HashMap;
use std::path::PathBuf;

use miette::{IntoDiagnostic, Result, WrapErr};

use zos_cobol::ast::{
    DataItem, DataItemName, Expression, LiteralKind, ProcedureBody, Program, Statement,
};
use zos_cobol::{scan, FileId, SourceFile, SourceFormat, CopybookConfig, Preprocessor};
use zos_runtime::interpreter::{
    DataItemMeta, Environment, SimpleBinaryOp, SimpleCompareOp, SimpleCondition, SimpleExpr,
    SimpleProgram, SimpleStatement,
};

use super::cics_bridge::CicsBridge;

/// Load and parse a single COBOL source file into a SimpleProgram.
/// Preprocesses COPY statements using the given include paths.
pub fn load_program(path: &std::path::Path, include_paths: &[PathBuf]) -> Result<SimpleProgram> {
    let source_text = std::fs::read_to_string(path)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to read: {}", path.display()))?;

    // Preprocess - expand COPY statements
    let mut copybook_config = CopybookConfig::new();

    // Add the directory containing the source file
    if let Some(parent) = path.parent() {
        copybook_config.add_path(parent);
    }

    // Add user-specified include paths
    for inc in include_paths {
        copybook_config.add_path(inc);
        tracing::debug!("Added include path: {}", inc.display());
    }

    let mut preprocessor = Preprocessor::new(copybook_config, SourceFormat::Fixed);
    let preprocessed = preprocessor.preprocess(&source_text).map_err(|e| {
        miette::miette!("Preprocessing failed for {}: {}", path.display(), e)
    })?;

    let source_file = SourceFile::from_text(FileId(0), preprocessed, SourceFormat::Fixed);
    let (tokens, lex_errors) = scan(&source_file);

    if !lex_errors.is_empty() {
        for err in &lex_errors {
            tracing::error!("Lex error: {:?}", err);
        }
        return Err(miette::miette!(
            "Lexing failed with {} errors in {}",
            lex_errors.len(),
            path.display()
        ));
    }

    let (program, parse_errors) = zos_cobol::parser::parse(tokens);

    if !parse_errors.is_empty() {
        for err in &parse_errors {
            tracing::error!("Parse error: {:?}", err);
        }
        return Err(miette::miette!(
            "Parse failed with {} errors in {}",
            parse_errors.len(),
            path.display()
        ));
    }

    let program = program.ok_or_else(|| miette::miette!("Failed to parse program"))?;
    convert_program(&program)
}

/// Try to find a COBOL source file for a program name in a directory.
pub fn find_program_source(program_name: &str, search_dir: &std::path::Path) -> Option<PathBuf> {
    let name_upper = program_name.to_uppercase();
    // Try common naming patterns: NAME.cbl, NAME.CBL, NAME.cob
    for ext in &["cbl", "CBL", "cob", "COB"] {
        let candidate = search_dir.join(format!("{}.{}", name_upper, ext));
        if candidate.exists() {
            return Some(candidate);
        }
    }
    None
}

/// Load fixed-length VSAM records from a data file.
/// Each record is `rec_len` bytes, key is the first `key_len` bytes.
pub fn load_vsam_data(
    path: &str,
    key_len: usize,
    rec_len: usize,
) -> std::result::Result<Vec<zos_cics::runtime::FileRecord>, String> {
    let data =
        std::fs::read(path).map_err(|e| format!("Failed to read {}: {}", path, e))?;

    let mut records = Vec::new();
    let mut offset = 0;
    while offset + rec_len <= data.len() {
        let record_bytes = &data[offset..offset + rec_len];
        let key = record_bytes[..key_len.min(rec_len)].to_vec();
        records.push(zos_cics::runtime::FileRecord {
            key,
            data: record_bytes.to_vec(),
        });
        offset += rec_len;
    }
    Ok(records)
}

/// Map an AID key name to its EBCDIC hex value.
fn aid_key_to_byte(name: &str) -> u8 {
    match name.to_uppercase().as_str() {
        "ENTER" => 0x7D,
        "CLEAR" => 0x6D,
        "PA1" => 0x6C,
        "PA2" => 0x6E,
        "PA3" => 0x6B,
        "PF1" => 0xF1,
        "PF2" => 0xF2,
        "PF3" => 0xF3,
        "PF4" => 0xF4,
        "PF5" => 0xF5,
        "PF6" => 0xF6,
        "PF7" => 0xF7,
        "PF8" => 0xF8,
        "PF9" => 0xF9,
        "PF10" => 0x7A,
        "PF11" => 0x7B,
        "PF12" => 0x7C,
        _ => 0x7D, // Default to ENTER
    }
}

/// Interpret a COBOL program directly.
pub fn interpret(
    input: PathBuf,
    include_paths: Vec<PathBuf>,
    data_files: Vec<String>,
    eibcalen: Option<i64>,
    eibaid: Option<String>,
    set_vars: Vec<String>,
) -> Result<()> {
    tracing::info!("Interpreting: {}", input.display());

    let simple = load_program(&input, &include_paths)?;
    let program_name = simple.name.clone();
    tracing::info!("Program: {}", program_name);

    // Check if program uses CICS (has any ExecCics statements)
    let uses_cics = has_cics_statements(&simple);

    if !uses_cics {
        // Simple non-CICS program - execute directly
        let mut env = Environment::new();
        let rc = zos_runtime::interpreter::execute(&simple, &mut env)
            .map_err(|e| miette::miette!("Runtime error: {}", e))?;
        tracing::info!("Program completed with RC={}", rc);
        return if rc == 0 {
            Ok(())
        } else {
            Err(miette::miette!("Program {} exited with RC={}", program_name, rc))
        };
    }

    // CICS program - set up bridge with XCTL dispatch support
    tracing::info!("CICS program detected, installing CICS bridge");
    let search_dir = input.parent().unwrap_or(std::path::Path::new(".")).to_path_buf();

    // Program registry: cache loaded programs by name
    let mut program_cache: HashMap<String, SimpleProgram> = HashMap::new();
    program_cache.insert(program_name.to_uppercase(), simple);

    let mut bridge = CicsBridge::new("CARD", "T001");

    // Load VSAM data files (format: DDNAME=path or DDNAME=path:key_len:rec_len)
    for spec in &data_files {
        if let Some((ddname, rest)) = spec.split_once('=') {
            let parts: Vec<&str> = rest.split(':').collect();
            let file_path = parts[0];
            let key_len: usize = parts.get(1).and_then(|s| s.parse().ok()).unwrap_or(8);
            let rec_len: usize = parts.get(2).and_then(|s| s.parse().ok()).unwrap_or(80);

            match load_vsam_data(file_path, key_len, rec_len) {
                Ok(records) => {
                    tracing::info!(
                        "Loaded {} records for VSAM file {} from {}",
                        records.len(),
                        ddname,
                        file_path
                    );
                    bridge.register_file(ddname, rec_len, key_len, records);
                }
                Err(e) => {
                    tracing::warn!("Failed to load VSAM data for {}: {}", ddname, e);
                }
            }
        }
    }

    let mut env = Environment::new().with_cics_handler(Box::new(bridge));

    // Inject CICS EIB (Execute Interface Block) fields
    use zos_runtime::CobolValue;
    let calen = eibcalen.unwrap_or(0);
    let aid_byte = eibaid
        .as_ref()
        .map(|s| aid_key_to_byte(s))
        .unwrap_or(0x7D); // Default: ENTER key
    env.set("EIBCALEN", CobolValue::from_i64(calen)).ok();
    env.set("EIBAID", CobolValue::alphanumeric(String::from(aid_byte as char))).ok();
    env.set("EIBRESP", CobolValue::from_i64(0)).ok();
    env.set("EIBRESP2", CobolValue::from_i64(0)).ok();
    env.set("EIBTRMID", CobolValue::alphanumeric("T001")).ok();
    env.set("EIBTRNID", CobolValue::alphanumeric("CARD")).ok();

    // Set user-specified variables (for simulating terminal input, COMMAREA, etc.)
    for spec in &set_vars {
        if let Some((name, value)) = spec.split_once('=') {
            env.set(name, CobolValue::alphanumeric(value)).ok();
        }
    }

    let mut current_program = program_name.to_uppercase();
    let max_xctl_chain = 50; // Safety limit to prevent infinite XCTL loops

    for xctl_depth in 0..max_xctl_chain {
        // Ensure program is in cache
        if !program_cache.contains_key(&current_program) {
            match find_program_source(&current_program, &search_dir) {
                Some(path) => {
                    tracing::info!("Loading program {} from {}", current_program, path.display());
                    let p = load_program(&path, &include_paths)?;
                    program_cache.insert(current_program.clone(), p);
                }
                None => {
                    return Err(miette::miette!(
                        "XCTL target program not found: {} (searched in {})",
                        current_program,
                        search_dir.display()
                    ));
                }
            }
        }
        let program = program_cache.get(&current_program).unwrap();

        tracing::info!(
            "Executing program {} (XCTL depth {})",
            current_program,
            xctl_depth
        );

        // Resume environment for new program (clear stopped state)
        env.resume();

        // Execute the program
        let exec_result = zos_runtime::interpreter::execute(&program, &mut env);

        // If execution failed, check for a HANDLE ABEND handler before propagating.
        let rc = match exec_result {
            Ok(rc) => rc,
            Err(e) => {
                let abend_label = env.cics_handler.as_mut().and_then(|h| {
                    h.as_any_mut()
                        .and_then(|a| a.downcast_mut::<CicsBridge>())
                        .and_then(|b| b.runtime.context.abend_handler.clone())
                });
                if let Some(label) = abend_label {
                    tracing::info!(
                        "Error in {}: {} — invoking HANDLE ABEND label '{}'",
                        current_program, e, label
                    );
                    if let Some(para) = program.paragraphs.get(&label.to_uppercase()) {
                        env.resume();
                        let _ = zos_runtime::interpreter::execute_statements(para, program, &mut env);
                    }
                    0 // Handler ran — continue as normal
                } else {
                    return Err(miette::miette!("Runtime error in {}: {}", current_program, e));
                }
            }
        };

        // Check if bridge has a pending abend label (EXEC CICS ABEND with handler)
        if let Some(mut handler) = env.cics_handler.take() {
            let bridge = handler.as_any_mut()
                .and_then(|a| a.downcast_mut::<CicsBridge>());
            if let Some(bridge) = bridge {
                if let Some(label) = bridge.abend_label.take() {
                    tracing::info!("EXEC CICS ABEND — invoking handler label '{}'", label);
                    if let Some(para) = program.paragraphs.get(&label.to_uppercase()) {
                        env.cics_handler = Some(handler);
                        env.resume();
                        let _ = zos_runtime::interpreter::execute_statements(para, program, &mut env);
                    } else {
                        env.cics_handler = Some(handler);
                    }
                } else {
                    env.cics_handler = Some(handler);
                }
            } else {
                env.cics_handler = Some(handler);
            }
        }

        // Check if XCTL was issued - extract handler to inspect state
        if let Some(mut handler) = env.cics_handler.take() {
            let bridge = handler
                .as_any_mut()
                .and_then(|a| a.downcast_mut::<CicsBridge>());

            if let Some(bridge) = bridge {
                // Handle LINK: execute linked program then return to caller
                if let Some(ref link_target) = bridge.link_program.clone() {
                    let commarea_data = bridge.commarea_var.as_ref().and_then(|ca| {
                        env.get(ca).map(|v| v.to_display_string())
                    });

                    let saved_program = current_program.clone();
                    tracing::info!("LINK from {} to {}", current_program, link_target);
                    current_program = link_target.to_uppercase();
                    bridge.link_program = None;
                    bridge.commarea_var = None;
                    bridge.runtime.eib.reset_for_command();
                    env.cics_handler = Some(handler);

                    if let Some(data) = commarea_data {
                        let len = data.len();
                        env.set("DFHCOMMAREA", CobolValue::Alphanumeric(data)).ok();
                        env.set("EIBCALEN", CobolValue::from_i64(len as i64)).ok();
                    }

                    // Load + execute the linked program
                    if !program_cache.contains_key(&current_program) {
                        if let Some(path) = find_program_source(&current_program, &search_dir) {
                            if let Ok(p) = load_program(&path, &include_paths) {
                                program_cache.insert(current_program.clone(), p);
                            }
                        }
                    }
                    if program_cache.contains_key(&current_program) {
                        let linked_prog = program_cache.get(&current_program).unwrap();
                        env.resume();
                        let _ = zos_runtime::interpreter::execute(linked_prog, &mut env);
                    }

                    // Return to the calling program
                    current_program = saved_program;
                    continue;
                }

                if let Some(ref xctl_target) = bridge.xctl_program.clone() {
                    // Extract COMMAREA data before resetting
                    let commarea_data = if let Some(ref ca_var) = bridge.commarea_var {
                        env.get(ca_var).map(|v| v.to_display_string())
                    } else {
                        None
                    };

                    tracing::info!(
                        "XCTL from {} to {} (RC={})",
                        current_program,
                        xctl_target,
                        rc
                    );
                    current_program = xctl_target.to_uppercase();
                    bridge.reset_for_xctl();
                    env.cics_handler = Some(handler);

                    // Pass COMMAREA data as DFHCOMMAREA in the target program
                    if let Some(data) = commarea_data {
                        let data_len = data.len();
                        tracing::debug!(
                            "Passing COMMAREA ({} bytes) to {} as DFHCOMMAREA",
                            data_len,
                            current_program
                        );
                        env.set("DFHCOMMAREA", CobolValue::Alphanumeric(data))
                            .ok();
                        // Also set EIBCALEN to the COMMAREA length
                        env.set("EIBCALEN", CobolValue::from_i64(data_len as i64))
                            .ok();
                    }
                    continue;
                }

                if bridge.returned {
                    if let Some(ref transid) = bridge.return_transid.clone() {
                        tracing::info!(
                            "RETURN TRANSID({}) from {} (RC={})",
                            transid,
                            current_program,
                            rc
                        );
                    } else {
                        tracing::info!("RETURN from {} (RC={})", current_program, rc);
                    }
                    env.cics_handler = Some(handler);
                    break;
                }
            }

            env.cics_handler = Some(handler);
        }

        // Normal completion (no XCTL, no RETURN) - done
        tracing::info!("Program {} completed with RC={}", current_program, rc);
        break;
    }

    Ok(())
}

/// Convert a COBOL AST Program to SimpleProgram.
fn convert_program(program: &Program) -> Result<SimpleProgram> {
    let name = program.identification.program_id.name.clone();

    // Collect data items, initial values, level-88 condition names, and group layouts
    let mut data_items = Vec::new();
    let mut initial_values = Vec::new();
    let mut condition_names: HashMap<String, (String, String)> = HashMap::new();
    let mut group_layouts: HashMap<String, Vec<zos_runtime::interpreter::GroupField>> = HashMap::new();
    if let Some(ref data) = program.data {
        collect_data_items(
            &data.working_storage,
            &mut data_items,
            &mut initial_values,
            &mut condition_names,
        );
        collect_group_layouts(&data.working_storage, &mut group_layouts);
    }

    // Convert statements
    let mut statements = Vec::new();
    let mut paragraphs = HashMap::new();

    // Add initial value assignments at the start
    for (name, value) in initial_values {
        statements.push(SimpleStatement::Move {
            from: value,
            to: vec![name],
        });
    }

    if let Some(ref procedure) = program.procedure {
        match &procedure.body {
            ProcedureBody::Statements(stmts) => {
                // Append statements after initial values
                for stmt in stmts {
                    if let Some(s) = convert_statement(stmt)? {
                        statements.push(s);
                    }
                }
            }
            ProcedureBody::Paragraphs(paras) => {
                let mut is_first = true;
                for para in paras {
                    let mut para_stmts = Vec::new();
                    for stmt in &para.statements {
                        if let Some(s) = convert_statement(stmt)? {
                            para_stmts.push(s);
                        }
                    }
                    // First paragraph becomes main (append to initial value statements)
                    if is_first {
                        statements.extend(para_stmts);
                        is_first = false;
                    } else {
                        paragraphs.insert(para.name.to_uppercase(), para_stmts);
                    }
                }
            }
            ProcedureBody::Sections(sections) => {
                let mut is_first = true;
                for section in sections {
                    for para in &section.paragraphs {
                        let mut para_stmts = Vec::new();
                        for stmt in &para.statements {
                            if let Some(s) = convert_statement(stmt)? {
                                para_stmts.push(s);
                            }
                        }
                        // First section's first paragraph becomes main (append to initial values)
                        if is_first {
                            statements.extend(para_stmts);
                            is_first = false;
                        } else {
                            paragraphs.insert(para.name.to_uppercase(), para_stmts);
                        }
                    }
                }
            }
        }
    }

    if std::env::var("ZOS_DEBUG_GROUPS").is_ok() {
        eprintln!("[GROUP_LAYOUTS] {:?}", group_layouts.keys().collect::<Vec<_>>());
    }

    Ok(SimpleProgram {
        name,
        data_items,
        statements,
        paragraphs,
        condition_names,
        group_layouts,
    })
}

/// Collect data items from DATA DIVISION.
fn collect_data_items(
    items: &[DataItem],
    out: &mut Vec<(String, DataItemMeta)>,
    inits: &mut Vec<(String, SimpleExpr)>,
    cond_names: &mut HashMap<String, (String, String)>,
) {
    for item in items {
        if let DataItemName::Named(ref name) = item.name {
            let meta = DataItemMeta {
                size: item.picture.as_ref().map(|p| p.size as usize).unwrap_or(80),
                decimals: item
                    .picture
                    .as_ref()
                    .map(|p| p.decimal_positions)
                    .unwrap_or(0),
                is_numeric: item
                    .picture
                    .as_ref()
                    .map(|p| {
                        matches!(
                            p.category,
                            zos_cobol::ast::PictureCategory::Numeric
                                | zos_cobol::ast::PictureCategory::NumericEdited
                        )
                    })
                    .unwrap_or(false),
                picture: item.picture.as_ref().map(|p| p.picture.clone()),
            };
            out.push((name.clone(), meta));

            // Handle VALUE clause
            if let Some(ref value) = item.value {
                if let Ok(expr) = convert_literal(value) {
                    inits.push((name.clone(), expr));
                }
            }

            // Collect level-88 condition names
            for cv in &item.condition_values {
                if let Some(first_val) = cv.values.first() {
                    let val_str = match first_val {
                        zos_cobol::ast::ConditionValueEntry::Single(lit) => {
                            match &lit.kind {
                                LiteralKind::String(s) => s.clone(),
                                LiteralKind::Integer(n) => n.to_string(),
                                LiteralKind::Hex(s) => decode_hex_string(s),
                                _ => String::new(),
                            }
                        }
                        _ => String::new(),
                    };
                    cond_names.insert(
                        cv.name.to_uppercase(),
                        (name.to_uppercase(), val_str),
                    );
                }
            }
        }
        // Recurse into children
        collect_data_items(&item.children, out, inits, cond_names);
    }
}

/// Compute the byte size of a data item (recursive for group items).
fn compute_item_size(item: &DataItem) -> usize {
    if let Some(ref pic) = item.picture {
        pic.size as usize
    } else if !item.children.is_empty() {
        // Group item: sum of children sizes (excluding level-88)
        item.children
            .iter()
            .filter(|c| c.level != 88)
            .map(compute_item_size)
            .sum()
    } else {
        0
    }
}

/// Collect group layouts: for each group item, record sub-field offsets.
fn collect_group_layouts(
    items: &[DataItem],
    layouts: &mut HashMap<String, Vec<zos_runtime::interpreter::GroupField>>,
) {
    for item in items {
        if !item.children.is_empty() && item.level != 88 {
            // This is a group item - collect its sub-fields with offsets
            let mut fields = Vec::new();
            collect_sub_fields(&item.children, 0, &mut fields);

            if let DataItemName::Named(ref name) = item.name {
                if !fields.is_empty() {
                    layouts.insert(name.to_uppercase(), fields);
                }
            }

            // Recurse for nested groups
            collect_group_layouts(&item.children, layouts);
        }
    }
}

/// Recursively collect sub-fields of a group item with their byte offsets.
fn collect_sub_fields(
    items: &[DataItem],
    base_offset: usize,
    fields: &mut Vec<zos_runtime::interpreter::GroupField>,
) {
    let mut offset = base_offset;
    for item in items {
        if item.level == 88 {
            continue; // Skip condition names
        }
        // REDEFINES items share the same offset as the item they redefine;
        // they must NOT advance the offset counter.
        if item.redefines.is_some() {
            continue;
        }
        let size = compute_item_size(item);
        match &item.name {
            DataItemName::Named(name) => {
                let is_numeric = item
                    .picture
                    .as_ref()
                    .map(|p| {
                        matches!(
                            p.category,
                            zos_cobol::ast::PictureCategory::Numeric
                                | zos_cobol::ast::PictureCategory::NumericEdited
                        )
                    })
                    .unwrap_or(false);
                fields.push(zos_runtime::interpreter::GroupField {
                    name: name.to_uppercase(),
                    offset,
                    size,
                    is_numeric,
                    default_value: None,
                });
            }
            DataItemName::Filler => {
                // Include FILLER fields with VALUE clauses (e.g., "/", ":") so
                // compose_group can reproduce the separator characters.
                if let Some(ref val_lit) = item.value {
                    let val_str = match &val_lit.kind {
                        zos_cobol::ast::LiteralKind::String(s) => s.clone(),
                        zos_cobol::ast::LiteralKind::Integer(n) => n.to_string(),
                        _ => String::new(),
                    };
                    if !val_str.is_empty() {
                        fields.push(zos_runtime::interpreter::GroupField {
                            name: String::new(), // Unnamed / FILLER
                            offset,
                            size,
                            is_numeric: false,
                            default_value: Some(val_str),
                        });
                    }
                }
            }
        }
        // If this is a sub-group, also collect its children at the same offset
        if !item.children.is_empty() {
            collect_sub_fields(&item.children, offset, fields);
        }
        offset += size;
    }
}

/// Convert a Literal to SimpleExpr.
fn convert_literal(lit: &zos_cobol::ast::Literal) -> Result<SimpleExpr> {
    match &lit.kind {
        LiteralKind::Integer(n) => Ok(SimpleExpr::Integer(*n)),
        LiteralKind::Decimal(s) => {
            let n: i64 = s.parse().unwrap_or(0);
            Ok(SimpleExpr::Integer(n))
        }
        LiteralKind::String(s) => Ok(SimpleExpr::String(s.clone())),
        LiteralKind::Hex(s) => Ok(SimpleExpr::String(decode_hex_string(s))),
        LiteralKind::Figurative(f) => {
            use zos_cobol::ast::FigurativeConstant;
            match f {
                FigurativeConstant::Zero => Ok(SimpleExpr::Integer(0)),
                FigurativeConstant::Space => Ok(SimpleExpr::String(" ".to_string())),
                FigurativeConstant::HighValue => Ok(SimpleExpr::String(
                    String::from_utf8_lossy(&[0xFF]).to_string(),
                )),
                FigurativeConstant::LowValue => Ok(SimpleExpr::String(
                    String::from_utf8_lossy(&[0x00]).to_string(),
                )),
                FigurativeConstant::Quote => Ok(SimpleExpr::String("\"".to_string())),
                FigurativeConstant::All => Ok(SimpleExpr::String("".to_string())),
            }
        }
        LiteralKind::AllOf(inner) => convert_literal(inner),
    }
}

/// Decode a hex literal string (e.g., "7D" -> "\x7D", "F1F2" -> "\xF1\xF2").
fn decode_hex_string(hex: &str) -> String {
    let bytes: Vec<u8> = hex
        .as_bytes()
        .chunks(2)
        .filter_map(|chunk| {
            let s = std::str::from_utf8(chunk).ok()?;
            u8::from_str_radix(s, 16).ok()
        })
        .collect();
    String::from_utf8_lossy(&bytes).to_string()
}

/// Convert a COBOL Statement to SimpleStatement.
fn convert_statement(stmt: &Statement) -> Result<Option<SimpleStatement>> {
    match stmt {
        Statement::Display(d) => {
            let items = d
                .items
                .iter()
                .filter_map(|e| convert_expr(e).ok())
                .collect();
            Ok(Some(SimpleStatement::Display {
                items,
                no_advancing: d.no_advancing,
            }))
        }

        Statement::Accept(a) => Ok(Some(SimpleStatement::Accept {
            target: a.target.name.clone(),
        })),

        Statement::Move(m) => {
            let from = convert_expr(&m.from)?;
            let to = m.to.iter().map(|q| q.name.clone()).collect();
            Ok(Some(SimpleStatement::Move { from, to }))
        }

        Statement::Compute(c) => {
            if let Some(first) = c.targets.first() {
                let expr = convert_expr(&c.expression)?;
                Ok(Some(SimpleStatement::Compute {
                    target: first.name.name.clone(),
                    expr,
                }))
            } else {
                Ok(None)
            }
        }

        Statement::Add(a) => {
            let values = a
                .operands
                .iter()
                .filter_map(|e| convert_expr(e).ok())
                .collect();
            let to = a.to.iter().map(|t| t.name.name.clone()).collect();
            Ok(Some(SimpleStatement::Add { values, to }))
        }

        Statement::Subtract(s) => {
            let values = s
                .operands
                .iter()
                .filter_map(|e| convert_expr(e).ok())
                .collect();
            let from = s.from.iter().map(|t| t.name.name.clone()).collect();
            Ok(Some(SimpleStatement::Subtract { values, from }))
        }

        Statement::Multiply(m) => {
            let value = convert_expr(&m.operand)?;
            let by = convert_expr(&m.by)?;
            let giving = m.giving.first().map(|t| t.name.name.clone());
            Ok(Some(SimpleStatement::Multiply { value, by, giving }))
        }

        Statement::Divide(d) => {
            let value = convert_expr(&d.operand)?;
            let into = convert_expr(&d.into_or_by)?;
            let giving = d.giving.first().map(|t| t.name.name.clone());
            Ok(Some(SimpleStatement::Divide {
                value,
                into,
                giving,
            }))
        }

        Statement::If(i) => {
            let condition = convert_condition(&i.condition)?;
            let then_branch = i
                .then_branch
                .iter()
                .filter_map(|s| convert_statement(s).ok().flatten())
                .collect();
            let else_branch = i.else_branch.as_ref().map(|stmts| {
                stmts
                    .iter()
                    .filter_map(|s| convert_statement(s).ok().flatten())
                    .collect()
            });
            Ok(Some(SimpleStatement::If {
                condition,
                then_branch,
                else_branch,
            }))
        }

        Statement::Perform(p) => {
            // Inline PERFORM with statements
            if let Some(ref inline_stmts) = p.inline {
                let stmts: Vec<SimpleStatement> = inline_stmts
                    .iter()
                    .filter_map(|s| convert_statement(s).ok().flatten())
                    .collect();
                // For PERFORM VARYING, use varying's UNTIL; for plain PERFORM, use p.until
                let until = if let Some(ref v) = p.varying {
                    Some(convert_condition(&v.until)?)
                } else {
                    p.until.as_ref().and_then(|c| convert_condition(c).ok())
                };

                // Convert VARYING clause
                let varying = if let Some(ref v) = p.varying {
                    let var_name = v.variable.name.clone();
                    let from_expr = convert_expr(&v.from)?;
                    let by_expr = convert_expr(&v.by)?;
                    Some((var_name, from_expr, by_expr))
                } else {
                    None
                };

                return Ok(Some(SimpleStatement::PerformInline {
                    until,
                    statements: stmts,
                    varying,
                }));
            }

            let target = p
                .target
                .as_ref()
                .map(|t| t.name.clone())
                .unwrap_or_default();
            let times = p.times.as_ref().and_then(|e| {
                if let Expression::Literal(l) = e {
                    if let LiteralKind::Integer(n) = l.kind {
                        return Some(n as u32);
                    }
                }
                None
            });
            Ok(Some(SimpleStatement::Perform { target, times }))
        }

        Statement::StopRun(s) => {
            let return_code = s.return_code.as_ref().and_then(|e| {
                if let Expression::Literal(l) = e {
                    if let LiteralKind::Integer(n) = l.kind {
                        return Some(n as i32);
                    }
                }
                None
            });
            Ok(Some(SimpleStatement::StopRun { return_code }))
        }

        Statement::Continue(_) => Ok(None),
        Statement::Exit(_) => Ok(None),

        Statement::ExecCics(e) => {
            let options = e
                .options
                .iter()
                .map(|opt| {
                    let value = opt.value.as_ref().map(|v| convert_expr(v)).transpose()?;
                    Ok((opt.name.clone(), value))
                })
                .collect::<Result<Vec<_>>>()?;
            Ok(Some(SimpleStatement::ExecCics {
                command: e.command.clone(),
                options,
            }))
        }

        Statement::Evaluate(e) => {
            let subjects = e
                .subjects
                .iter()
                .filter_map(|s| convert_expr(s).ok())
                .collect();

            let mut when_clauses: Vec<zos_runtime::interpreter::SimpleWhenClause> = e
                .when_clauses
                .iter()
                .filter_map(|w| {
                    // Use first condition (simplified - ignore ALSO for now)
                    let condition = if let Some(cond) = w.conditions.first() {
                        convert_when_condition(cond).ok()?
                    } else {
                        return None;
                    };

                    let statements: Vec<SimpleStatement> = w
                        .statements
                        .iter()
                        .filter_map(|s| convert_statement(s).ok().flatten())
                        .collect();

                    Some(zos_runtime::interpreter::SimpleWhenClause {
                        condition,
                        statements,
                    })
                })
                .collect();

            // Add WHEN OTHER as a catch-all clause
            if let Some(other_stmts) = &e.when_other {
                let statements: Vec<SimpleStatement> = other_stmts
                    .iter()
                    .filter_map(|s| convert_statement(s).ok().flatten())
                    .collect();
                when_clauses.push(zos_runtime::interpreter::SimpleWhenClause {
                    condition: SimpleCondition::Compare {
                        left: SimpleExpr::Integer(1),
                        op: SimpleCompareOp::Equal,
                        right: SimpleExpr::Integer(1),
                    },
                    statements,
                });
            }

            Ok(Some(SimpleStatement::Evaluate {
                subjects,
                when_clauses,
            }))
        }

        Statement::GoTo(g) => {
            let target = g.targets.first().cloned().unwrap_or_default();
            Ok(Some(SimpleStatement::GoTo { target }))
        }

        Statement::Initialize(i) => {
            let targets = i.variables.iter().map(|v| v.name.clone()).collect();
            Ok(Some(SimpleStatement::Initialize { targets }))
        }

        Statement::Set(s) => {
            use zos_cobol::ast::SetMode;
            match &s.mode {
                SetMode::ConditionTo { target, value } => {
                    // SET condition-name TO TRUE/FALSE
                    // At runtime, resolves parent field from condition_names map
                    Ok(Some(SimpleStatement::SetCondition {
                        condition_name: target.name.clone(),
                        value: *value,
                    }))
                }
                SetMode::IndexTo { targets, value } => {
                    let val = convert_expr(value)?;
                    if let Some(first) = targets.first() {
                        Ok(Some(SimpleStatement::Set {
                            target: first.name.clone(),
                            value: val,
                        }))
                    } else {
                        Ok(None)
                    }
                }
                SetMode::IndexUpDown { targets, up, value } => {
                    let val = convert_expr(value)?;
                    if let Some(first) = targets.first() {
                        let op = if *up { SimpleBinaryOp::Add } else { SimpleBinaryOp::Subtract };
                        Ok(Some(SimpleStatement::Compute {
                            target: first.name.clone(),
                            expr: SimpleExpr::Binary {
                                left: Box::new(SimpleExpr::Variable(first.name.clone())),
                                op,
                                right: Box::new(val),
                            },
                        }))
                    } else {
                        Ok(None)
                    }
                }
                SetMode::AddressOf { .. } => {
                    // Pointer operations not meaningful in interpreter
                    Ok(None)
                }
            }
        }

        Statement::Call(c) => {
            let program = convert_expr(&c.program)?;
            let using = c.using.iter().filter_map(|a| {
                if let Expression::Variable(q) = &a.value {
                    Some(q.name.clone())
                } else {
                    None
                }
            }).collect();
            Ok(Some(SimpleStatement::Call { program, using }))
        }

        Statement::String(s) => {
            let sources = s.sources.iter()
                .filter_map(|src| convert_expr(&src.value).ok())
                .collect();
            Ok(Some(SimpleStatement::StringConcat {
                sources,
                into: s.into.name.clone(),
            }))
        }

        // Other statements not yet implemented (Open, Close, Read, Write, etc.)
        _ => Ok(None),
    }
}

/// Convert an Expression to SimpleExpr.
fn convert_expr(expr: &Expression) -> Result<SimpleExpr> {
    match expr {
        Expression::Literal(l) => match &l.kind {
            LiteralKind::Integer(n) => Ok(SimpleExpr::Integer(*n)),
            LiteralKind::Decimal(s) => {
                // Parse as integer for simplicity
                let n: i64 = s.parse().unwrap_or(0);
                Ok(SimpleExpr::Integer(n))
            }
            LiteralKind::String(s) => Ok(SimpleExpr::String(s.clone())),
            LiteralKind::Hex(s) => Ok(SimpleExpr::String(decode_hex_string(s))),
            LiteralKind::Figurative(f) => {
                use zos_cobol::ast::FigurativeConstant;
                match f {
                    FigurativeConstant::Zero => Ok(SimpleExpr::Integer(0)),
                    FigurativeConstant::Space => Ok(SimpleExpr::String(" ".to_string())),
                    FigurativeConstant::HighValue => Ok(SimpleExpr::String(
                        String::from_utf8_lossy(&[0xFF]).to_string(),
                    )),
                    FigurativeConstant::LowValue => Ok(SimpleExpr::String(
                        String::from_utf8_lossy(&[0x00]).to_string(),
                    )),
                    FigurativeConstant::Quote => Ok(SimpleExpr::String("\"".to_string())),
                    FigurativeConstant::All => Ok(SimpleExpr::String("".to_string())),
                }
            }
            LiteralKind::AllOf(inner) => convert_literal(inner),
        },

        Expression::Variable(q) => {
            // Handle COBOL figurative constants TRUE/FALSE
            match q.name.to_uppercase().as_str() {
                "TRUE" => Ok(SimpleExpr::Integer(1)),
                "FALSE" => Ok(SimpleExpr::Integer(0)),
                _ => Ok(SimpleExpr::Variable(q.name.clone())),
            }
        }

        Expression::Binary(b) => {
            let left = Box::new(convert_expr(&b.left)?);
            let right = Box::new(convert_expr(&b.right)?);
            let op = match b.op {
                zos_cobol::ast::BinaryOp::Add => SimpleBinaryOp::Add,
                zos_cobol::ast::BinaryOp::Subtract => SimpleBinaryOp::Subtract,
                zos_cobol::ast::BinaryOp::Multiply => SimpleBinaryOp::Multiply,
                zos_cobol::ast::BinaryOp::Divide => SimpleBinaryOp::Divide,
                zos_cobol::ast::BinaryOp::Power => SimpleBinaryOp::Multiply, // Approximate
            };
            Ok(SimpleExpr::Binary { left, op, right })
        }

        Expression::Unary(u) => {
            let inner = convert_expr(&u.operand)?;
            match u.op {
                zos_cobol::ast::UnaryOp::Minus => {
                    // Negate by multiplying by -1
                    Ok(SimpleExpr::Binary {
                        left: Box::new(SimpleExpr::Integer(-1)),
                        op: SimpleBinaryOp::Multiply,
                        right: Box::new(inner),
                    })
                }
                zos_cobol::ast::UnaryOp::Plus => Ok(inner),
            }
        }

        Expression::Paren(inner) => convert_expr(inner),

        Expression::RefMod(r) => {
            // Reference modification: variable(start:length)
            let variable = Box::new(SimpleExpr::Variable(r.variable.name.clone()));
            let start = Box::new(convert_expr(&r.start)?);
            let length = if let Some(ref len) = r.length {
                Some(Box::new(convert_expr(len)?))
            } else {
                None
            };
            Ok(SimpleExpr::RefMod { variable, start, length })
        }

        Expression::Function(f) => {
            match f.name.to_uppercase().as_str() {
                "UPPER-CASE" => {
                    if let Some(arg) = f.arguments.first() {
                        let inner = convert_expr(arg)?;
                        // Wrap the argument in a special function expression
                        Ok(SimpleExpr::FunctionCall {
                            name: "UPPER-CASE".to_string(),
                            args: vec![inner],
                        })
                    } else {
                        Ok(SimpleExpr::String(String::new()))
                    }
                }
                "LOWER-CASE" => {
                    if let Some(arg) = f.arguments.first() {
                        let inner = convert_expr(arg)?;
                        Ok(SimpleExpr::FunctionCall {
                            name: "LOWER-CASE".to_string(),
                            args: vec![inner],
                        })
                    } else {
                        Ok(SimpleExpr::String(String::new()))
                    }
                }
                "CURRENT-DATE" => Ok(SimpleExpr::FunctionCall {
                    name: "CURRENT-DATE".to_string(),
                    args: vec![],
                }),
                "LENGTH" => {
                    if let Some(arg) = f.arguments.first() {
                        let inner = convert_expr(arg)?;
                        Ok(SimpleExpr::FunctionCall {
                            name: "LENGTH".to_string(),
                            args: vec![inner],
                        })
                    } else {
                        Ok(SimpleExpr::Integer(0))
                    }
                }
                _ => {
                    tracing::warn!("Function {} not implemented, returning 0", f.name);
                    Ok(SimpleExpr::Integer(0))
                }
            }
        }

        Expression::LengthOf(l) => {
            // LENGTH OF returns the size of a data item - resolved at runtime
            Ok(SimpleExpr::FunctionCall {
                name: "LENGTH".to_string(),
                args: vec![SimpleExpr::Variable(l.item.name.clone())],
            })
        }

        Expression::AddressOf(a) => {
            // ADDRESS OF returns a pointer
            // Not meaningful in interpreter mode
            tracing::debug!("ADDRESS OF {} evaluated as placeholder", a.item.name);
            Ok(SimpleExpr::Integer(0))
        }
    }
}

/// Convert a WhenCondition to SimpleCondition.
fn convert_when_condition(cond: &zos_cobol::ast::WhenCondition) -> Result<SimpleCondition> {
    use zos_cobol::ast::WhenCondition;
    match cond {
        WhenCondition::Any => Ok(SimpleCondition::Compare {
            left: SimpleExpr::Integer(1),
            op: SimpleCompareOp::Equal,
            right: SimpleExpr::Integer(1),
        }),
        WhenCondition::True => Ok(SimpleCondition::Compare {
            left: SimpleExpr::Integer(1),
            op: SimpleCompareOp::Equal,
            right: SimpleExpr::Integer(1),
        }),
        WhenCondition::False => Ok(SimpleCondition::Compare {
            left: SimpleExpr::Integer(0),
            op: SimpleCompareOp::Equal,
            right: SimpleExpr::Integer(1),
        }),
        WhenCondition::Value(expr) => {
            // For EVALUATE variable: the runtime compares subject == value
            let val = convert_expr(expr)?;
            Ok(SimpleCondition::Compare {
                left: SimpleExpr::Variable("__EVAL_SUBJECT__".to_string()),
                op: SimpleCompareOp::Equal,
                right: val,
            })
        }
        WhenCondition::Condition(c) => convert_condition(c),
        WhenCondition::Range { .. } => {
            // Approximate range as always-true
            Ok(SimpleCondition::Compare {
                left: SimpleExpr::Integer(1),
                op: SimpleCompareOp::Equal,
                right: SimpleExpr::Integer(1),
            })
        }
    }
}

/// Check if a SimpleProgram contains any EXEC CICS statements.
fn has_cics_statements(program: &SimpleProgram) -> bool {
    fn stmts_have_cics(stmts: &[SimpleStatement]) -> bool {
        stmts.iter().any(|s| match s {
            SimpleStatement::ExecCics { .. } => true,
            SimpleStatement::If {
                then_branch,
                else_branch,
                ..
            } => {
                stmts_have_cics(then_branch)
                    || else_branch.as_ref().is_some_and(|eb| stmts_have_cics(eb))
            }
            SimpleStatement::Evaluate { when_clauses, .. } => {
                when_clauses.iter().any(|w| stmts_have_cics(&w.statements))
            }
            _ => false,
        })
    }

    stmts_have_cics(&program.statements)
        || program.paragraphs.values().any(|stmts| stmts_have_cics(stmts))
}

/// Convert a Condition to SimpleCondition.
fn convert_condition(cond: &zos_cobol::ast::Condition) -> Result<SimpleCondition> {
    match cond {
        zos_cobol::ast::Condition::Comparison(c) => {
            let left = convert_expr(&c.left)?;
            let right = convert_expr(&c.right)?;
            let op = match c.op {
                zos_cobol::ast::ComparisonOp::Equal => SimpleCompareOp::Equal,
                zos_cobol::ast::ComparisonOp::NotEqual => SimpleCompareOp::NotEqual,
                zos_cobol::ast::ComparisonOp::LessThan => SimpleCompareOp::LessThan,
                zos_cobol::ast::ComparisonOp::LessOrEqual => SimpleCompareOp::LessOrEqual,
                zos_cobol::ast::ComparisonOp::GreaterThan => SimpleCompareOp::GreaterThan,
                zos_cobol::ast::ComparisonOp::GreaterOrEqual => SimpleCompareOp::GreaterOrEqual,
            };
            Ok(SimpleCondition::Compare { left, op, right })
        }

        zos_cobol::ast::Condition::Not(inner) => {
            Ok(SimpleCondition::Not(Box::new(convert_condition(inner)?)))
        }

        zos_cobol::ast::Condition::And(left, right) => Ok(SimpleCondition::And(
            Box::new(convert_condition(left)?),
            Box::new(convert_condition(right)?),
        )),

        zos_cobol::ast::Condition::Or(left, right) => Ok(SimpleCondition::Or(
            Box::new(convert_condition(left)?),
            Box::new(convert_condition(right)?),
        )),

        zos_cobol::ast::Condition::Paren(inner) => convert_condition(inner),

        // Class and sign conditions - evaluate to true for now
        zos_cobol::ast::Condition::Class(_) => Ok(SimpleCondition::Compare {
            left: SimpleExpr::Integer(1),
            op: SimpleCompareOp::Equal,
            right: SimpleExpr::Integer(1),
        }),

        zos_cobol::ast::Condition::Sign(_) => Ok(SimpleCondition::Compare {
            left: SimpleExpr::Integer(1),
            op: SimpleCompareOp::Equal,
            right: SimpleExpr::Integer(1),
        }),

        // Level-88 condition name - resolved at runtime
        zos_cobol::ast::Condition::ConditionName(q) => {
            Ok(SimpleCondition::ConditionName(q.name.clone()))
        }
    }
}
