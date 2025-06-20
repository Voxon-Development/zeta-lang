use crate::vm::functions;
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
            return;
        }

        let func = {
            let module = vm.function_module.lock();
            module.get_function(self.frame.function_id).cloned().unwrap()
        };
        let function = &func.code;
        
        // 2. Do the rest (no double borrow now)
        if let Some(instr) = function.get(self.frame.program_counter) {
            self.frame.program_counter += 1;
            let frame = &mut self.frame;
            vm.interpret_instruction(*instr, frame, function);
        } else {
            self.done = true;
        }

    }
}