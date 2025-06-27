use crate::codegen::cranelift::bytecode_translator;
use cranelift::codegen::ir::{Function, UserExternalName, UserFuncName};
use cranelift::codegen::isa::CallConv;
use cranelift::prelude::*;
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{DataId, Linkage, Module};
use ir::{Bytecode, VMValue};
use std::collections::HashMap;
use crate::codegen::ir::module::NativeFnPtr;

/// Compiler that translates Zeta VM bytecode to Cranelift JIT code
pub struct IRToCraneliftCompiler {
    /// The JIT module containing compiled functions
    module: JITModule,
    /// Map from function names to their compiled functions
    compiled_functions: HashMap<String, NativeFnPtr>,
    /// Data section for string literals
    string_data: HashMap<String, (DataId, *const u8)>,
    /// Function builder context for Cranelift
    builder_ctx: FunctionBuilderContext,
    /// Cranelift context
    ctx: codegen::Context,
}

impl IRToCraneliftCompiler {
    /// Create a new IR to Cranelift compiler
    pub fn new() -> Self {
        let isa = cranelift_native::builder()
            .expect("Failed to get native ISA")
            .finish(settings::Flags::new(settings::builder()));

        let builder = JITBuilder::with_isa(isa.unwrap(), cranelift_module::default_libcall_names());
        let module = JITModule::new(builder);

        Self {
            module,
            compiled_functions: HashMap::new(),
            string_data: HashMap::new(),
            builder_ctx: FunctionBuilderContext::new(),
            ctx: codegen::Context::new(),
        }
    }

    /// Compile a function from VM bytecode to native code
    /// # Safety
    /// This function is unsafe because it performs raw pointer casts.
    /// The caller must ensure that the function pointer is valid and follows the NativeFnPtr ABI.
    pub fn compile_function(&mut self, name: &str, bytecode: &[u8]) -> anyhow::Result<()> {
        self.ctx.clear();

        // Define the function signature: fn(*mut c_void, *const VMValue, usize) -> VMValue
        let mut sig = Signature::new(CallConv::SystemV);
        sig.params.push(AbiParam::new(types::I64)); // vm_ptr
        sig.params.push(AbiParam::new(types::I64)); // args_ptr
        sig.params.push(AbiParam::new(types::I32)); // args_len
        sig.returns.push(AbiParam::new(types::I128)); // VMValue is 128-bit

        // Create the function in the module
        let id = self.module.declare_function(name, Linkage::Export, &sig)?;

        // Set up the function builder
        self.ctx.func = Function::with_name_signature(
            UserFuncName::User(UserExternalName::new(0, 0)),
            sig,
        );


        // Step 1: Scoped builder block to end the borrow on self.ctx early
        let entry_block;
        let vm_ptr;
        let args_ptr;
        let _args_len;

        {
            let mut builder = FunctionBuilder::new(&mut self.ctx.func, &mut self.builder_ctx);

            entry_block = builder.create_block();
            builder.switch_to_block(entry_block);
            builder.seal_block(entry_block);

            vm_ptr = builder.func.dfg.block_params(entry_block)[0];
            args_ptr = builder.func.dfg.block_params(entry_block)[1];
            _args_len = builder.func.dfg.block_params(entry_block)[2];

            bytecode_translator::translate_bytecode(&mut builder, bytecode, vm_ptr, args_ptr)?;

            builder.finalize(); // Done with builder => ctx borrow ends here
        }

        // Step 2: Now it's safe to call module.define_function
        self.module.define_function(id, &mut self.ctx)?;

        // Get the function pointer and store it
        let func_ptr = self.module.get_finalized_function(id);
        self.compiled_functions.insert(name.to_string(), unsafe { std::mem::transmute(func_ptr) });

        Ok(())
    }

    /// Finalize the module and make all functions available for execution
    pub fn finalize(&mut self) -> anyhow::Result<()> {
        self.module.finalize_definitions()?;
        Ok(())
    }

    /// Get a compiled function by name
    pub fn get_compiled_function(&self, name: &str) -> Option<NativeFnPtr> {
        self.compiled_functions.get(name).copied()
    }

    /// Create a safe Rust function wrapper for a compiled function
    pub fn create_vm_function_wrapper(&self, name: &str) -> Option<NativeFnPtr> {
        self.get_compiled_function(name)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ir::VMValue;

    #[test]
    fn test_compiled_function() -> anyhow::Result<()> {
        let mut compiler = IRToCraneliftCompiler::new();

        // Simple function that adds two i32s and returns the result
        let bytecode = vec![
            // Push first argument (index 0)
            Bytecode::LoadLocal as u8, 0,
            // Push second argument (index 1)
            Bytecode::LoadLocal as u8, 1,
            // Add them
            Bytecode::Add as u8,
            // Return the result
            Bytecode::Return as u8,
        ];

        // Compile the function
        compiler.compile_function("test_add", &bytecode)?;
        compiler.finalize()?;

        // Test getting a function wrapper
        if let Some(add_func) = compiler.create_vm_function_wrapper("test_add") {
            let result = unsafe { add_func([VMValue::I32(5), VMValue::I32(3)].as_ptr(), 2) };
            assert_eq!(result, VMValue::I32(8));
        } else {
            panic!("Failed to get compiled function");
        }

        Ok(())
    }
}