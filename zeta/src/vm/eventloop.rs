use crate::vm::{virtualmachine::VirtualMachine, fibers::Fiber};
use crossbeam::deque::{Injector, Stealer, Worker};
use std::sync::atomic::{AtomicBool, Ordering};
use std::thread;
use trc::SharedTrc;
use crate::vm::fibers::FiberState;
use ir::atomic_utils::UnsafeArc;

#[derive(Default)]
pub struct FiberScheduler {
    injector: SharedTrc<Injector<Fiber>>,
    stealers: Vec<Stealer<Fiber>>,
    workers: Vec<Worker<Fiber>>,
    shutdown_flag: SharedTrc<AtomicBool>,
}

impl FiberScheduler {
    pub fn new(num_threads: usize) -> Self {
        let injector = SharedTrc::new(Injector::new());
        let mut workers = Vec::new();
        let mut stealers = Vec::new();

        for _ in 0..num_threads {
            let worker = Worker::new_fifo();
            stealers.push(worker.stealer());
            workers.push(worker);
        }

        Self {
            injector,
            stealers,
            workers,
            shutdown_flag: SharedTrc::new(AtomicBool::new(false))
        }
    }

    pub fn spawn(&self, fiber: Fiber) {
        self.injector.push(fiber);
    }

    pub fn run(&mut self, vm: UnsafeArc<VirtualMachine>) {
        let injector = self.injector.clone();
        let stealers = self.stealers.clone();
        let shutdown_flag = self.shutdown_flag.clone();

        for (_, local) in self.workers.drain(..).enumerate() {
            let stealers = stealers.clone();
            let injector = injector.clone();
            let shutdown_flag = shutdown_flag.clone();
            let mut vm = vm.clone();

            thread::spawn(move || {
                loop {
                    if shutdown_flag.load(Ordering::Relaxed) {
                        return;
                    }
                    let fiber = local.pop()
                        .or_else(|| injector.steal_batch_and_pop(&local).success())
                        .or_else(|| {
                            for stealer in &stealers {
                                if let Some(fiber) = stealer.steal().success() {
                                    return Some(fiber);
                                }
                            }
                            None
                        });

                    match fiber {
                        Some(mut fiber) => {
                            fiber.step(unsafe { vm.get_mut_unchecked() });

                            if fiber.state != FiberState::Done && !fiber.yielded {
                                local.push(fiber);
                            } else if fiber.yielded {
                                injector.push(fiber);
                            }
                        }
                        None => {
                            thread::yield_now();
                        }
                    }
                }
            });
        }
    }

    pub fn shutdown(&mut self) {
        self.shutdown_flag.store(true, Ordering::Relaxed);
    }

    pub fn thread_count(&self) -> usize {
        self.workers.len()
    }

    pub fn get_worker(&self, thread_id: usize) -> &Worker<Fiber> {
        &self.workers[thread_id]
    }
}