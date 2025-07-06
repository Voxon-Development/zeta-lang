use crate::vm::functions;
use crate::vm::memory::frames;
use crate::vm::memory::stack::BumpStack;
use crate::vm::virtualmachine::VirtualMachine;
use ir::VMValue;

#[derive(Debug, Clone, Default)]
pub struct Fiber {
    pub function_id: u64,
    pub state: FiberState,
    pub yielded: bool,
    pub yield_value: Option<VMValue>
}

unsafe impl Send for Fiber {}
unsafe impl Sync for Fiber {}

impl Fiber {
    pub(crate) fn new(function_id: u64) -> Fiber {
        Fiber {
            function_id,
            ..Default::default()
        }
    }

    pub(crate) fn reset(&mut self) {
        self.state = FiberState::Running;
        self.yielded = false;
        self.yield_value = None;
    }

    pub(crate) fn step(&mut self, vm: &mut VirtualMachine) {
        if self.state == FiberState::Done {
            self.reset();
            return;
        }

        if vm.current_stack_depth >= vm.max_stack_depth {
            panic!(
                "Stack overflow in fiber: Maximum call stack depth of {} exceeded",
                vm.max_stack_depth
            );
        }

        let result = functions::run_native_fiber(vm, self.function_id, std::ptr::null(), 0);

        match result {
            Some(yield_val) => {
                self.yielded = true;
                self.yield_value = Some(yield_val);
            }
            None => {
                self.state = FiberState::Done;
            }
        }
    }
}

#[repr(u8)]
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub enum FiberState {
    #[default]
    Running,
    
    Yielded,
    Done,
}