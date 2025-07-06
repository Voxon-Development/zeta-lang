use crate::vm::eventloop::FiberScheduler;
use crate::vm::memory::frames::{OptimizationFrame, StackFrame};
use crate::vm::memory::heap;
use crate::vm::memory::stack::BumpStack;
use crate::vm::memory::string_pool::StringPool;
use crate::vm::{functions, println, profiler};
use crossbeam::channel::{unbounded, Receiver, Sender};
use ir::{VMValue};
use parking_lot::Mutex;
use rayon::max_num_threads;
use trc::SharedTrc;
use zetac::codegen::ir::ir_compiler::CompressedClassTable;
use zetac::codegen::ir::module::Function;
use zetac::codegen::ir::module::ZetaModule;
use zetac::codegen::ir::optimization::pass_manager::PassManager;

#[global_allocator]
static GLOBAL: snmalloc_rs::SnMalloc = snmalloc_rs::SnMalloc;

unsafe impl Send for VirtualMachine {}
unsafe impl Sync for VirtualMachine {}

pub struct VirtualMachine {
    pub(crate) stack: BumpStack,
    pub(crate) heap: heap::HeapMemory,
    pub(crate) profiler: profiler::Profiler,
    pub(crate) function_module: SharedTrc<Mutex<ZetaModule>>,
    pub(crate) pass_manager: SharedTrc<Mutex<PassManager>>,
    pub(crate) event_loop: FiberScheduler,
    pub(crate) string_pool: SharedTrc<StringPool>,
    pub(crate) optimization_receiver: Receiver<OptimizationFrame>,
    pub(crate) optimization_sender: Sender<OptimizationFrame>,
    pub call_stack: Vec<StackFrame>,
    pub(crate) max_stack_depth: usize,
    pub(crate) current_stack_depth: usize,
    pub(crate) class_table: SharedTrc<CompressedClassTable>
}

impl VirtualMachine {
    pub fn new(mut function_module: ZetaModule, class_table: CompressedClassTable) -> VirtualMachine {
        use zetac::codegen::ir::module::NativeFnPtr;

        let string_pool = SharedTrc::new(StringPool::new());
        let println_fn = Function::new_native("println".to_string(), println::println_smt as NativeFnPtr);
        function_module.add_function(println_fn);

        let (optimization_sender, optimization_receiver) = unbounded();

        VirtualMachine {
            stack: BumpStack::new(1024),
            heap: heap::HeapMemory::new(),
            profiler: profiler::Profiler::new(),
            function_module: SharedTrc::new(Mutex::new(function_module)),
            pass_manager: SharedTrc::new(Mutex::new(PassManager::new())),
            event_loop: FiberScheduler::new(max_num_threads()),
            string_pool,
            optimization_receiver,
            optimization_sender,
            call_stack: Vec::new(),
            max_stack_depth: 1024 * 1024,
            current_stack_depth: 0,
            class_table: SharedTrc::new(class_table)
        }
    }

    pub fn run_entry(&mut self) {
        let module = self.function_module.lock();
        let entry_ptr = module.entry.expect("No entry point defined");

        let ptr = std::ptr::null::<VMValue>();

        unsafe { functions::run_native_ptr(entry_ptr, ptr, 0) };
    }

    pub fn run_function(&mut self, function_id: u64, vm_args: Vec<VMValue>) {
        if self.current_stack_depth >= self.max_stack_depth {
            panic!("Stack overflow: Maximum call stack depth exceeded");
        }

        let function = {
            let module = self.function_module.lock();
            module.get_function(function_id).cloned().unwrap()
        };

        let result = unsafe {
            functions::run_native_function(&function, vm_args.as_ptr(), vm_args.len())
        };

        if let VMValue::Void = result {
        } else {
            self.stack.push_vm(result);
        }

        self.current_stack_depth += 1;

        let call_count = self.profiler.get_func_call_count(function_id);
        let optimization_frame = OptimizationFrame {
            function_id,
            call_count: call_count.count,
        };
        tokio::spawn(functions::optimize_function_async(
            self.function_module.clone(),
            self.pass_manager.clone(),
            optimization_frame,
        ));
    }

    pub fn tick(&mut self) -> bool {
        if let Ok(task) = self.optimization_receiver.try_recv() {
            tokio::spawn(functions::optimize_function_async(
                self.function_module.clone(),
                self.pass_manager.clone(),
                task,
            ));
        }
        true
    }

    pub fn halt(&mut self) {
        self.shutdown();
        std::process::exit(1);
    }

    pub fn shutdown(&mut self) {
        self.event_loop.shutdown();
        self.heap.free_all();
        self.current_stack_depth = 0;
        self.call_stack.clear();
        self.function_module.lock().clear();
    }
}