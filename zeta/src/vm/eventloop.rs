use std::io::Read;
use crate::vm::{virtualmachine::VirtualMachine, fibers::Fiber};
use crossbeam::deque::{Injector, Stealer, Worker};
use rayon::ThreadPoolBuilder;
use std::sync::{Arc, Mutex};
use std::thread;
use std::time::Duration;

#[derive(Debug)]
pub struct FiberScheduler {
    injector: Arc<Injector<Fiber>>,
    local: Worker<Fiber>, // local worker queue for this thread
}

impl FiberScheduler {
    pub fn new() -> Self {
        Self {
            injector: Arc::new(Injector::new()),
            local: Worker::new_fifo(),
        }
    }

    pub fn spawn(&self, fiber: Fiber) {
        self.injector.push(fiber);
    }

    /// Tick the scheduler once: process 1 fiber (or batch-steal from global queue).
    /// Returns `true` if there are still fibers to run.
    pub fn tick(&self, vm: &mut VirtualMachine) -> bool {
        let fiber = self
            .local
            .pop()
            .or_else(|| self.injector.steal_batch_and_pop(&self.local).success());

        match fiber {
            Some(mut fiber) => {
                fiber.step(vm);

                if !fiber.done && !fiber.yielded {
                    self.local.push(fiber);
                } else if fiber.yielded {
                    self.injector.push(fiber);
                } else {
                    fiber.reset(); // optional: clean-up/reset
                }

                true
            }
            None => {
                // No work found
                !self.injector.is_empty() || !self.local.is_empty()
            }
        }
    }
    
    pub fn shutdown(&self) {
        
    }
}
