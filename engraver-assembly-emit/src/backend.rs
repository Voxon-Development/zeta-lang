use ir::ssa_ir::{Function, Module};

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