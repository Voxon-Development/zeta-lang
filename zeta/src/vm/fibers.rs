use std::ops::DerefMut;
use std::sync::{Arc, Mutex};
use crossbeam::deque::Injector;
use ir::Bytecode;
use zetac::codegen::ir::module;
use crate::vm::{frames, functions};
use crate::vm::virtualmachine::VirtualMachine;

#[derive(Debug, Clone, Default)]
pub struct Fiber {
    pub(crate) function_id: u64,
    pub(crate) function: functions::Function,
    pub(crate) instruction_counter: usize,
    pub(crate) program_counter: usize,
    pub(crate) frame: Option<frames::StackFrame>,
    pub done: bool,
    pub(crate) stack: crate::vm::stack::BumpStack,
    pub(crate) yielded: bool,
    pub(crate) yield_value: Option<u64>
}

unsafe impl Send for Fiber {}
unsafe impl Sync for Fiber {}


impl Fiber {
    pub(crate) fn new(function_id: u64, function: functions::Function) -> Fiber {
        Fiber {
            function_id,
            function,
            ..Default::default()
        }
    }
    
    pub(crate) fn reset(&mut self) {
        self.done = false;
        self.instruction_counter = 0;
        self.program_counter = 0;
        self.frame = Some(frames::StackFrame::new(0, 0, self.function_id));
        self.yielded = false;
        self.yield_value = None;
        self.stack.reset();
    }
    
    pub(crate) fn step(&mut self, vm: &mut VirtualMachine) {
        if self.done {
            return;
        }
        
        let mut cloned_frame = self.frame.clone();
        let frame = cloned_frame.as_mut().unwrap();
        

        if let Some(instruction) = self.function.code.get(self.program_counter) {
            self.instruction_counter += 1;
            vm.interpret_instruction(*instruction, frame, &mut self.function.code);
            self.program_counter += 1;
        }
    }
}

pub struct FiberPool {
    pub(crate) injector: Injector<Fiber>,
    pub(crate) injector_index: usize
}

impl FiberPool {
    pub fn new() -> Self {
        Self {
            injector: Injector::new(),
            injector_index: 0
        }
    }
    
    pub fn spawn(&mut self, fiber: Fiber) {
        self.injector.push(fiber);
    }
    
    pub fn spawn_and_run(&mut self, fiber: Fiber, vm: Arc<Mutex<VirtualMachine>>) {
        self.spawn(fiber);
    }
    
    pub fn free_all(&mut self) {
        self.injector = Injector::new();
    }
}
