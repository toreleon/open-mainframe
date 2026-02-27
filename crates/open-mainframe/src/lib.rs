//! OpenMainframe library — COBOL compilation, lowering, CICS bridge.
//!
//! Exports the components needed by both the CLI binary and the z/OSMF REST
//! server to compile COBOL programs and run full CICS sessions.

pub mod bridge;
pub mod headless;
pub mod lower;

use std::cell::RefCell;
use std::path::{Path, PathBuf};
use std::rc::Rc;

use open_mainframe_cobol::lexer::{
    scan, CopybookConfig, FileId, Preprocessor, SourceFile, SourceFormat,
};
use open_mainframe_cobol::parser;
use open_mainframe_runtime::interpreter::{
    CicsCommandHandler, Environment, InterpreterError, SimpleProgram,
};
use open_mainframe_runtime::value::CobolValue;

use bridge::CicsBridge;

// ---------------------------------------------------------------------------
// BridgeHandler — thin wrapper around Rc<RefCell<CicsBridge>> that implements
// CicsCommandHandler.  This allows the main loop to retain a reference to the
// bridge while the interpreter holds a Box<dyn CicsCommandHandler>.
// ---------------------------------------------------------------------------

/// Wrapper that implements [`CicsCommandHandler`] by delegating to a shared
/// [`CicsBridge`].
pub struct BridgeHandler {
    pub inner: Rc<RefCell<CicsBridge>>,
}

impl CicsCommandHandler for BridgeHandler {
    fn execute(
        &mut self,
        command: &str,
        options: &[(String, Option<CobolValue>)],
        env: &mut Environment,
    ) -> Result<(), InterpreterError> {
        self.inner.borrow_mut().execute(command, options, env)
    }

    fn as_any_mut(&mut self) -> Option<&mut dyn std::any::Any> {
        Some(self)
    }
}

// ---------------------------------------------------------------------------
// Compilation helpers
// ---------------------------------------------------------------------------

/// Compile a COBOL source file to a [`SimpleProgram`].
pub fn compile_program(
    path: &Path,
    include_paths: &[PathBuf],
) -> Result<SimpleProgram, String> {
    let source = std::fs::read_to_string(path)
        .map_err(|e| format!("Failed to read {}: {e}", path.display()))?;

    // Set up copybook include paths
    let mut all_includes = include_paths.to_vec();
    if let Some(parent) = path.parent() {
        all_includes.push(parent.to_path_buf());
    }

    // Preprocess: expand COPY statements
    let mut cb_config = CopybookConfig::new();
    cb_config.search_paths = all_includes;
    let mut preprocessor = Preprocessor::new(cb_config, SourceFormat::Fixed);
    let expanded = preprocessor
        .preprocess(&source)
        .map_err(|e| format!("Preprocess error in {}: {e}", path.display()))?;

    // Tokenize the expanded source
    let src_file = SourceFile::from_text(FileId(0), expanded, SourceFormat::Fixed);
    let (tokens, _scan_errors) = scan(&src_file);

    // Parse
    let (program_opt, errors) = parser::parse(tokens);
    if !errors.is_empty() && program_opt.is_none() {
        return Err(format!(
            "Parse errors in {}:\n{}",
            path.display(),
            errors
                .iter()
                .map(|e| format!("  {e}"))
                .collect::<Vec<_>>()
                .join("\n")
        ));
    }

    let program = program_opt.ok_or_else(|| {
        format!("Failed to parse {}", path.display())
    })?;

    // Lower AST to SimpleProgram
    Ok(lower::lower_program(&program))
}

/// Find a COBOL source file for a program name in a set of search directories.
pub fn find_program_source(program_name: &str, search_dirs: &[PathBuf]) -> Option<PathBuf> {
    let name_upper = program_name.to_uppercase();
    for dir in search_dirs {
        if !dir.is_dir() {
            continue;
        }
        // Try exact name with various extensions
        for ext in &["cbl", "CBL", "cob", "COB"] {
            let candidate = dir.join(format!("{}.{}", name_upper, ext));
            if candidate.exists() {
                return Some(candidate);
            }
            // Also try lowercase
            let candidate = dir.join(format!("{}.{}", program_name.to_lowercase(), ext));
            if candidate.exists() {
                return Some(candidate);
            }
        }
        // Try case-insensitive search in directory
        if let Ok(entries) = std::fs::read_dir(dir) {
            for entry in entries.flatten() {
                let file_name = entry.file_name();
                let name_str = file_name.to_string_lossy();
                if let Some(stem) = name_str
                    .strip_suffix(".cbl")
                    .or_else(|| name_str.strip_suffix(".CBL"))
                    .or_else(|| name_str.strip_suffix(".cob"))
                    .or_else(|| name_str.strip_suffix(".COB"))
                {
                    if stem.eq_ignore_ascii_case(&name_upper) {
                        return Some(entry.path());
                    }
                }
            }
        }
    }
    None
}

/// Set up EIB fields in the interpreter environment for a CICS execution cycle.
pub fn setup_env(
    bridge: &Rc<RefCell<CicsBridge>>,
    commarea: &Option<Vec<u8>>,
    pending_aid: &mut Option<u8>,
) -> Environment {
    let mut env = Environment::new();

    {
        let br = bridge.borrow();
        let eib = &br.runtime().eib;
        env.set("EIBCALEN", CobolValue::from_i64(0)).ok();
        let aid_char = pending_aid
            .take()
            .map(|b| String::from(char::from(b)))
            .unwrap_or_else(|| " ".to_string());
        env.set("EIBAID", CobolValue::Alphanumeric(aid_char)).ok();
        env.set("EIBRESP", CobolValue::from_i64(0)).ok();
        env.set("EIBRESP2", CobolValue::from_i64(0)).ok();
        env.set(
            "EIBTRNID",
            CobolValue::alphanumeric(eib.transaction_id()),
        )
        .ok();
        env.set("EIBDATE", CobolValue::from_i64(0)).ok();
        env.set("EIBTIME", CobolValue::from_i64(0)).ok();
        env.set("EIBTRMID", CobolValue::alphanumeric("T001")).ok();
        env.set("DFHCOMMAREA", CobolValue::alphanumeric("")).ok();
    }

    if let Some(ref ca) = commarea {
        env.set("EIBCALEN", CobolValue::from_i64(ca.len() as i64))
            .ok();
        let ca_str = String::from_utf8_lossy(ca).to_string();
        env.set("DFHCOMMAREA", CobolValue::alphanumeric(ca_str))
            .ok();
    }

    env
}
