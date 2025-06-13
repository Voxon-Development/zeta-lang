use crate::vm::{virtualmachine::VirtualMachine, fibers::Fiber};
use crossbeam::deque::{Injector, Stealer, Worker};
use rayon::ThreadPoolBuilder;
use std::sync::{Arc, Mutex};
use std::thread;
use std::time::Duration;

pub struct FiberScheduler {
    injector: Arc<Injector<Fiber>>,
}

impl FiberScheduler {
    pub fn new() -> Self {
        Self {
            injector: Arc::new(Injector::new()),
        }
    }

    pub fn spawn(&self, fiber: Fiber) {
        self.injector.push(fiber);
    }

    pub fn run(self, vm: Arc<Mutex<VirtualMachine>>) {
        let injector = self.injector.clone();

        ThreadPoolBuilder::new().build_global().unwrap();

        rayon::scope(|s| {
            for _ in 0..rayon::current_num_threads() {
                let injector = injector.clone();
                let vm = vm.clone();

                s.spawn(move |_| {
                    let local = Worker::new_fifo();
                    Self::loop_through_fibers(injector, &vm, &local);
                });
            }
        });
    }

    fn loop_through_fibers(injector: Arc<Injector<Fiber>>, vm: &Arc<Mutex<VirtualMachine>>, local: &Worker<Fiber>) {
        loop {
            let fiber = local.pop()
                .or_else(|| injector.steal_batch_and_pop(&local).success());

            match fiber {
                Some(mut fiber) => {
                    fiber.step(&mut vm.lock().unwrap());

                    if !fiber.done && !fiber.yielded {
                        local.push(fiber);
                    } else if fiber.yielded {
                        injector.push(fiber);
                    } else {
                        fiber.reset();
                    }
                }
                None => {
                    thread::sleep(Duration::from_millis(1));
                }
            }
        }
    }
}
