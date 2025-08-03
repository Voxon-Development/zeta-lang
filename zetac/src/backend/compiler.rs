use crate::midend::ir::hir::HirModule;
use crate::midend::ir::lowerer::{lower_module, LowerCtx};
use crate::midend::ir::ssa_ir::{Function, Module};

/// Trait defining code generation interface
pub trait Backend {
    /// Emit an entire SSA module
    fn emit_module(&mut self, module: &Module);

    /// Emit a single function
    fn emit_function(&mut self, function: &Function);

    /// Emit an extern function
    fn emit_extern(&mut self, function: &Function);

    /// Finish the module
    fn finish(self);
}

/// The compiler driver: parses AST → HIR → SSA → Backend
pub struct Compiler<B: Backend> {
    backend: B,
    lower_ctx: LowerCtx
}

impl<B: Backend> Compiler<B> {
    pub fn new(backend: B) -> Self {
        Compiler { backend, lower_ctx: LowerCtx::new() }
    }

    pub fn compile(mut self, hir: &HirModule) {
        // Lower HIR → SSA
        let ssa_module = lower_module(hir);
        // Emit through selected backend
        self.backend.emit_module(&ssa_module);
        self.backend.finish();
    }
}