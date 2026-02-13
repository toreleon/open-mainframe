//! LLVM context and code generator infrastructure.
//!
//! This module provides the main code generation context and options.

use std::path::Path;

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::targets::{
    CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine,
    TargetTriple as LlvmTargetTriple,
};
use inkwell::values::{FunctionValue, GlobalValue, PointerValue};
use inkwell::OptimizationLevel as LlvmOptLevel;

use crate::ast::Program;
use crate::error::CobolError;

/// Optimization level for code generation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum OptimizationLevel {
    /// No optimization (fast compile).
    #[default]
    None,
    /// Basic optimizations.
    Less,
    /// Standard optimizations.
    Default,
    /// Aggressive optimizations.
    Aggressive,
}

impl From<OptimizationLevel> for LlvmOptLevel {
    fn from(level: OptimizationLevel) -> Self {
        match level {
            OptimizationLevel::None => LlvmOptLevel::None,
            OptimizationLevel::Less => LlvmOptLevel::Less,
            OptimizationLevel::Default => LlvmOptLevel::Default,
            OptimizationLevel::Aggressive => LlvmOptLevel::Aggressive,
        }
    }
}

/// Target triple for code generation.
#[derive(Debug, Clone)]
pub struct TargetTriple {
    inner: String,
}

impl TargetTriple {
    /// Create a target triple for the host machine.
    pub fn host() -> Self {
        Self {
            inner: TargetMachine::get_default_triple().to_string(),
        }
    }

    /// Create a target triple for x86_64 Linux.
    pub fn x86_64_linux() -> Self {
        Self {
            inner: "x86_64-unknown-linux-gnu".to_string(),
        }
    }

    /// Create a target triple for ARM64 Linux.
    pub fn aarch64_linux() -> Self {
        Self {
            inner: "aarch64-unknown-linux-gnu".to_string(),
        }
    }

    /// Get the triple string.
    pub fn as_str(&self) -> &str {
        &self.inner
    }
}

impl Default for TargetTriple {
    fn default() -> Self {
        Self::host()
    }
}

/// Options for code generation.
#[derive(Debug, Clone, Default)]
pub struct CodegenOptions {
    /// Optimization level.
    pub optimization: OptimizationLevel,
    /// Target triple.
    pub target: TargetTriple,
    /// Whether to emit debug information.
    pub debug_info: bool,
    /// Module name.
    pub module_name: String,
}

impl CodegenOptions {
    /// Create default options with a module name.
    pub fn new(module_name: impl Into<String>) -> Self {
        Self {
            module_name: module_name.into(),
            ..Default::default()
        }
    }

    /// Set optimization level.
    pub fn with_optimization(mut self, level: OptimizationLevel) -> Self {
        self.optimization = level;
        self
    }

    /// Set target triple.
    pub fn with_target(mut self, target: TargetTriple) -> Self {
        self.target = target;
        self
    }

    /// Enable debug info.
    pub fn with_debug_info(mut self) -> Self {
        self.debug_info = true;
        self
    }
}

/// Code generator for COBOL programs.
pub struct CodeGenerator<'ctx> {
    /// LLVM context.
    context: &'ctx Context,
    /// LLVM module being built.
    module: Module<'ctx>,
    /// IR builder.
    builder: Builder<'ctx>,
    /// Target machine for code emission.
    target_machine: TargetMachine,
    /// Code generation options.
    options: CodegenOptions,
}

impl<'ctx> CodeGenerator<'ctx> {
    /// Create a new code generator.
    pub fn new(context: &'ctx Context, options: CodegenOptions) -> Result<Self, CobolError> {
        // Initialize LLVM targets
        Target::initialize_all(&InitializationConfig::default());

        // Create module
        let module = context.create_module(&options.module_name);

        // Set target triple
        let target_triple = LlvmTargetTriple::create(&options.target.inner);
        module.set_triple(&target_triple);

        // Get target
        let target = Target::from_triple(&target_triple).map_err(|e| CobolError::CodegenError {
            message: format!("Failed to get target: {}", e),
        })?;

        // Create target machine
        let target_machine = target
            .create_target_machine(
                &target_triple,
                "generic",
                "",
                options.optimization.into(),
                RelocMode::Default,
                CodeModel::Default,
            )
            .ok_or_else(|| CobolError::CodegenError {
                message: "Failed to create target machine".to_string(),
            })?;

        // Set data layout
        module.set_data_layout(&target_machine.get_target_data().get_data_layout());

        // Create builder
        let builder = context.create_builder();

        Ok(Self {
            context,
            module,
            builder,
            target_machine,
            options,
        })
    }

    /// Get the LLVM context.
    pub fn context(&self) -> &'ctx Context {
        self.context
    }

    /// Get the LLVM module.
    pub fn module(&self) -> &Module<'ctx> {
        &self.module
    }

    /// Get the IR builder.
    pub fn builder(&self) -> &Builder<'ctx> {
        &self.builder
    }

    /// Compile a COBOL program to LLVM IR.
    pub fn compile(&mut self, program: &Program) -> Result<(), CobolError> {
        // Generate data layout from DATA DIVISION
        if let Some(ref data) = program.data {
            self.generate_data_layout(data)?;
        }

        // Generate main function
        self.generate_main_function(program)?;

        // Generate procedure code
        if let Some(ref procedure) = program.procedure {
            self.generate_procedure(procedure)?;
        }

        // Verify module
        self.module.verify().map_err(|e| CobolError::CodegenError {
            message: format!("Module verification failed: {}", e),
        })?;

        Ok(())
    }

    /// Generate data layout for DATA DIVISION.
    fn generate_data_layout(&mut self, data: &crate::ast::DataDivision) -> Result<(), CobolError> {
        // Generate globals for WORKING-STORAGE
        for item in &data.working_storage {
            self.generate_data_item_global(item, "WS")?;
        }

        // Generate globals for LINKAGE section (external references)
        for item in &data.linkage {
            self.generate_data_item_global(item, "LS")?;
        }

        Ok(())
    }

    /// Generate a global variable for a data item.
    fn generate_data_item_global(
        &mut self,
        item: &crate::ast::DataItem,
        prefix: &str,
    ) -> Result<GlobalValue<'ctx>, CobolError> {
        use super::types::LlvmType;

        let name = match &item.name {
            crate::ast::DataItemName::Named(n) => format!("{}_{}", prefix, n),
            crate::ast::DataItemName::Filler => {
                format!("{}_FILLER_{}", prefix, self.module.get_globals().count())
            }
        };

        let llvm_type = LlvmType::from_data_item(self.context, item);
        let basic_type = llvm_type.to_basic_type(self.context);

        let global = self.module.add_global(basic_type, None, &name);

        // Initialize to zero
        global.set_initializer(&basic_type.const_zero());

        Ok(global)
    }

    /// Generate the main entry function.
    fn generate_main_function(
        &mut self,
        program: &Program,
    ) -> Result<FunctionValue<'ctx>, CobolError> {
        // Create main function: i32 main()
        let i32_type = self.context.i32_type();
        let main_type = i32_type.fn_type(&[], false);
        let main_fn = self.module.add_function("main", main_type, None);

        // Create entry block
        let entry_block = self.context.append_basic_block(main_fn, "entry");
        self.builder.position_at_end(entry_block);

        // For now, just return 0
        let zero = i32_type.const_int(0, false);
        self.builder
            .build_return(Some(&zero))
            .map_err(|e| CobolError::CodegenError {
                message: format!("Failed to build return: {:?}", e),
            })?;

        Ok(main_fn)
    }

    /// Generate code for PROCEDURE DIVISION.
    fn generate_procedure(
        &mut self,
        procedure: &crate::ast::ProcedureDivision,
    ) -> Result<(), CobolError> {
        // TODO: Implement procedure code generation
        // This will generate functions for paragraphs/sections
        // and translate statements to LLVM IR
        Ok(())
    }

    /// Write object file to path.
    pub fn write_object_file(&self, path: &Path) -> Result<(), CobolError> {
        self.target_machine
            .write_to_file(&self.module, FileType::Object, path)
            .map_err(|e| CobolError::CodegenError {
                message: format!("Failed to write object file: {}", e),
            })
    }

    /// Write assembly to path.
    pub fn write_assembly(&self, path: &Path) -> Result<(), CobolError> {
        self.target_machine
            .write_to_file(&self.module, FileType::Assembly, path)
            .map_err(|e| CobolError::CodegenError {
                message: format!("Failed to write assembly: {}", e),
            })
    }

    /// Get LLVM IR as a string.
    pub fn to_ir_string(&self) -> String {
        self.module.print_to_string().to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_create_codegen() {
        let context = Context::create();
        let options = CodegenOptions::new("test_module");
        let codegen = CodeGenerator::new(&context, options);
        assert!(codegen.is_ok());
    }

    #[test]
    fn test_codegen_options() {
        let options = CodegenOptions::new("test")
            .with_optimization(OptimizationLevel::Aggressive)
            .with_debug_info();

        assert_eq!(options.optimization, OptimizationLevel::Aggressive);
        assert!(options.debug_info);
    }

    #[test]
    fn test_target_triple() {
        let host = TargetTriple::host();
        assert!(!host.as_str().is_empty());

        let x86 = TargetTriple::x86_64_linux();
        assert!(x86.as_str().contains("x86_64"));
    }
}
