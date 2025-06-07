use std::collections::VecDeque;
use crate::vm::virtualmachine::VirtualMachine;

pub(crate) struct Event {
    pub function_id: u64,
    pub execution_context: ExecutionContext
}

#[derive(Default)]
pub(crate) struct EventLoop {
    event_queue: VecDeque<Event>
}

impl EventLoop {
    pub(crate) fn new() -> EventLoop {
        EventLoop {
            event_queue: VecDeque::new(),
        }
    }

    pub(crate) fn add_event(&mut self, event: Event) {
        self.event_queue.push_back(event);
    }

    fn process_events(&mut self, virtual_machine: &mut VirtualMachine) {
        for event in self.event_queue.iter() {
            match event.execution_context {
                ExecutionContext::Sync => virtual_machine.run_function(event.function_id),
                ExecutionContext::Fiber => todo!(),
                ExecutionContext::Thread => todo!(),
                ExecutionContext::Parallelized => todo!()
            }
        }
        
        self.event_queue.clear();
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Default)]
pub(crate) enum ExecutionContext {
    #[default]
    Sync,
    
    Fiber,
    Thread,
    Parallelized
}