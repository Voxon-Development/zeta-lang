use crate::vm::functions;
use crate::vm::interpreter::{CALL, CALL_IN_FIBER, RETURN};
use crate::vm::memory::frames;
use crate::vm::memory::stack::BumpStack;
use crate::vm::virtualmachine::VirtualMachine;

#[derive(Debug, Clone, Default)]
pub struct Fiber {
    pub function_id: u64,
    pub frame: frames::StackFrame,
    pub done: bool,
    pub yielded: bool,
    pub yield_value: Option<u64>
}

unsafe impl Send for Fiber {}
unsafe impl Sync for Fiber {}


impl Fiber {
    pub(crate) fn new(function_id: u64) -> Fiber {
        Fiber {
            function_id,
            frame: frames::StackFrame::new(0, 0, function_id),
            ..Default::default()
        }
    }
    
    pub(crate) fn reset(&mut self) {
        self.done = false;
        self.frame.reset();
        self.yielded = false;
        self.yield_value = None;
    }
    
    pub(crate) fn step(&mut self, vm: &mut VirtualMachine) {
        if self.done {
            self.reset();
            // Decrement stack depth when fiber completes
            if vm.current_stack_depth > 0 {
                vm.current_stack_depth -= 1;
            }
            return;
        }
        
        // Check stack depth before executing instruction
        if vm.current_stack_depth >= vm.max_stack_depth {
            panic!(
                "Stack overflow in fiber: Maximum call stack depth of {} exceeded",
                vm.max_stack_depth
            );
        }
        
        let func = {
            let module = vm.function_module.lock();
            module.get_function(self.frame.function_id).cloned().unwrap_or_else(|| {
                panic!("Function with ID {} not found", self.frame.function_id);
            })
        };
        
        let function = &func.code;
        
        if let Some(&instr) = function.get(self.frame.program_counter) {
            self.frame.program_counter = self.frame.program_counter.wrapping_add(1);
            let frame = &mut self.frame;
            
            // Handle function calls specially to track stack depth
            if instr == CALL || instr == CALL_IN_FIBER {
                vm.current_stack_depth += 1;
            }
            
            vm.interpret_instruction(instr, frame, function);
            
            // Handle returns to properly track stack depth
            if instr == RETURN && vm.current_stack_depth > 0 {
                vm.current_stack_depth -= 1;
            }
        } else {
            // End of function, decrement stack depth
            if vm.current_stack_depth > 0 {
                vm.current_stack_depth -= 1;
            }
            self.done = true;
        }
    }
}