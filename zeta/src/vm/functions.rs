use ir::Bytecode;
pub(crate) use zetac::codegen::ir::module::Function;
use zetac::codegen::ir::optimization::pass::OptimizationPassPriority;
use zetac::codegen::ir::optimization::pass_manager;
use crate::vm::frames;
use crate::vm::frames::StackFrame;
use crate::vm::profiler::FunctionCall;

pub(crate) fn run_native_function(function: &Function) {
    let pointer = function.native_pointer.clone().unwrap();

    unsafe {
        let pointer: fn() = std::mem::transmute(pointer);
        pointer();
    }
}

pub(crate) fn compile_function(function: &mut Function) {
    function.is_native = true;
}

pub(crate) fn optimize_function_with_priority(priority: OptimizationPassPriority, function: &mut Function, pass_manager: &mut pass_manager::PassManager) {
    pass_manager.run_passes_with_priority(priority, &mut function.code).unwrap()
}

#[inline]
pub(crate) fn optimize_function(function: &mut Function, function_call: &FunctionCall, pass_manager: &mut pass_manager::PassManager) {
    if function.optimization_level == OptimizationPassPriority::Max {
        return;
    }

    let function_call_count = function_call.count;

    if function_call_count > 200 {
        optimize_function_with_priority(OptimizationPassPriority::Max, function, pass_manager);
        compile_function(function);

        function.optimization_level = OptimizationPassPriority::Max;
    }
    if function_call_count > 150 {
        pass_manager.run_passes_with_priority(
            OptimizationPassPriority::High,
            &mut function.code
        ).unwrap();

        compile_function(function);
    }

    if function_call_count > 100 {
        pass_manager.run_passes_with_priority(
            OptimizationPassPriority::Medium,
            &mut function.code
        ).unwrap();

        compile_function(function);
    }

    if function_call_count > 50 {
        pass_manager.run_passes_with_priority(
            OptimizationPassPriority::Low,
            &mut function.code
        ).unwrap();

        compile_function(function);
    }

    if function_call_count > 20 {
        compile_function(function);
    }

    if function_call_count > 10 {
        pass_manager.run_passes_with_priority(
            OptimizationPassPriority::Min,
            &mut function.code
        ).unwrap();
    }
}

pub(crate) fn step_function(
    stack_frame: &frames::StackFrame, 
    program_counter: usize, 
    instruction_counter: usize, 
    function: &mut Function
) -> bool {
    let instruction = &function.code[program_counter];
    true
}