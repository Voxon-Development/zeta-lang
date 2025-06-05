use crate::{context, vm};

struct EventLoop {
    function_queue: Vec<Task>,
    virtual_machine: vm::VirtualMachine
}

impl EventLoop {
    pub fn new(virtual_machine: vm::VirtualMachine) -> Self {
        EventLoop {
           function_queue: Vec::new(),
            virtual_machine
        }
    }

    pub fn schedule(&mut self, task: Task) {
        self.function_queue.push(task);
    }

    pub fn run(&mut self) {
        while let Some(function) = self.function_queue.pop() {
            self.virtual_machine.run_function(function);
        }
    }
}

pub struct Task {
    pub execution_context: context::ExecutionContext,
    pub function_id: usize,
}