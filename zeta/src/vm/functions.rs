use crate::vm::profiler::FunctionCall;
pub(crate) use zetac::codegen::ir::module::Function;
use zetac::codegen::ir::module::ZetaModule;
use zetac::codegen::ir::optimization::pass::OptimizationPassPriority;
use zetac::codegen::ir::optimization::pass_manager;
use crate::vm::frames::OptimizationFrame;

type NativeFn = fn();

#[allow(dead_code)]

pub(crate) fn run_native_function(function: &Function) {
    let pointer = function.native_pointer.clone().unwrap();

    unsafe {
        let pointer: NativeFn = std::mem::transmute(pointer);
        pointer();
    }
}

pub(crate) fn compile_function(function: &mut Function) {
    function.is_native = true;
}

pub(crate) fn optimize_function_with_priority(priority: OptimizationPassPriority, function: &mut Function, pass_manager: &mut pass_manager::PassManager, module: &ZetaModule) {
    pass_manager.run_passes_with_priority(priority, &mut function.code, module).unwrap()
}

#[inline]
pub(crate) fn optimize_function(function: &mut Function, function_call: OptimizationFrame, pass_manager: &mut pass_manager::PassManager, module: &ZetaModule) {
    if function.optimization_level == OptimizationPassPriority::Max {
        return;
    }

    let function_call_count = function_call.call_count;

    if function_call_count > 200 {
        optimize_function_with_priority(OptimizationPassPriority::Max, function, pass_manager, module);
        compile_function(function);

        function.optimization_level = OptimizationPassPriority::Max;
    }
    if function_call_count > 150 {
        pass_manager.run_passes_with_priority(
            OptimizationPassPriority::High,
            &mut function.code,
            module
        ).unwrap();

        compile_function(function);
    }

    if function_call_count > 100 {
        pass_manager.run_passes_with_priority(
            OptimizationPassPriority::Medium,
            &mut function.code,
            module
        ).unwrap();

        compile_function(function);
    }

    if function_call_count > 50 {
        pass_manager.run_passes_with_priority(
            OptimizationPassPriority::Low,
            &mut function.code,
            module
        ).unwrap();

        compile_function(function);
    }

    if function_call_count > 20 {
        compile_function(function);
    }

    if function_call_count > 10 {
        pass_manager.run_passes_with_priority(
            OptimizationPassPriority::Min,
            &mut function.code,
            module
        ).unwrap();
    }
}