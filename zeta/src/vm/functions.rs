use zetac::codegen::ir::module::Function;
use zetac::codegen::ir::optimization::pass::OptimizationPassPriority;
use zetac::codegen::ir::optimization::pass_manager;

use crate::vm::frames::StackFrame;
use crate::vm::profiler::FunctionCall;

pub(crate) fn interpret_function(call_frame: &StackFrame, program_counter: usize, instruction_counter: usize, function: &Function) {
    for instruction in function.code.iter() {
        match instruction {
            _ => todo!()
        }
    }
}

pub(crate) fn run_native_function(function: &Function) {
    let pointer = function.native_pointer.unwrap();
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

pub(crate) fn optimize_function(function: &mut Function, function_call: &FunctionCall, pass_manager: &mut pass_manager::PassManager) {
    let function_call_count = function_call.time_taken;
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