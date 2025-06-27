use crate::vm::memory::frames::OptimizationFrame;
use ir::VMValue;
use parking_lot::Mutex;
use trc::SharedTrc;
use zetac::codegen::cranelift::compiler_from_ir::IRToCraneliftCompiler;
pub(crate) use zetac::codegen::ir::module::Function;
use zetac::codegen::ir::module::ZetaModule;
use zetac::codegen::ir::optimization::pass::OptimizationPassPriority;
use zetac::codegen::ir::optimization::pass_manager::PassManager;

pub(crate) unsafe fn run_native_function(function: &Function, args: *const VMValue, len: usize) -> VMValue {
    if let Some(func) = function.native_pointer {
        unsafe { func(args, len) }
    } else {
        panic!("Tried to call native function, but no native pointer was set");
    }
}


pub(crate) fn compile_function(function: &mut Function) {
    let mut compiler_from_ir = IRToCraneliftCompiler::new();
    
    let name = function.name.as_str();
    compiler_from_ir.compile_function(name, function.code.as_slice())
        .expect("Failed to compile the function.");
    
    function.native_pointer = compiler_from_ir.create_vm_function_wrapper(name);
    function.is_native = true;
}

pub(crate) fn optimize_function_with_priority(priority: OptimizationPassPriority, function: &mut Function, pass_manager: &mut PassManager, module: &ZetaModule) {
    pass_manager.run_passes_with_priority(priority, &mut function.code, module).unwrap()
}

#[inline]
pub(crate) async fn optimize_function_async(
    function_module: SharedTrc<Mutex<ZetaModule>>,
    pass_manager: SharedTrc<Mutex<PassManager>>,
    task: OptimizationFrame,
) {
    let mut function: Function = {
        function_module.lock()
            .get_function(task.function_id)
            .cloned()
            .unwrap()
    };

    optimize_function(
        &mut function,
        task,
        &mut pass_manager.lock(),
        &function_module.lock(),
    );

    function_module.lock().add_function(function);
}

#[inline]
pub(crate) fn optimize_function(function: &mut Function, function_call: OptimizationFrame, pass_manager: &mut PassManager, module: &ZetaModule) {
    if function.optimization_level == OptimizationPassPriority::Max {
        return;
    }

    let function_call_count = function_call.call_count;

    if function_call_count == 200 {
        optimize_function_with_priority(OptimizationPassPriority::Max, function, pass_manager, module);
        compile_function(function);

        function.optimization_level = OptimizationPassPriority::Max;
    }
    
    if function_call_count == 150 {
        pass_manager.run_passes_with_priority(
            OptimizationPassPriority::High,
            &mut function.code,
            module
        ).unwrap();

        compile_function(function);
    }

    if function_call_count == 100 {
        pass_manager.run_passes_with_priority(
            OptimizationPassPriority::Medium,
            &mut function.code,
            module
        ).unwrap();

        compile_function(function);
    }

    if function_call_count == 50 {
        pass_manager.run_passes_with_priority(
            OptimizationPassPriority::Low,
            &mut function.code,
            module
        ).unwrap();

        compile_function(function);
    }

    if function_call_count == 20 {
        compile_function(function);
    }

    if function_call_count == 5 {
        pass_manager.run_passes_with_priority(
            OptimizationPassPriority::Min,
            &mut function.code,
            module
        ).unwrap();
    }
}