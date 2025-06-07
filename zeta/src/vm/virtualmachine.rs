use std::cmp::PartialEq;
use crate::vm::frames::StackFrame;
use crate::vm::functions;
use crate::vm::{profiler, stack};
use ir::Bytecode;
use std::collections::HashMap;
use zetac::codegen::ir::module;
use zetac::codegen::ir::module::Function;
use zetac::codegen::ir::optimization::pass_manager;
use crate::vm::eventloop::{Event, EventLoop, ExecutionContext};
use crate::vm::heap;

pub struct VirtualMachine {
    stack: stack::BumpStack,
    heap: heap::HeapMemory,
    program_counter: usize,
    bytecode: Vec<Bytecode>,
    variables: HashMap<String, i64>,
    profiler: profiler::Profiler,
    pass_manager: pass_manager::PassManager,
    function_module: module::ZetaModule,
    event_loop: EventLoop,
    instruction_counter: usize,
}

impl VirtualMachine {
    pub fn new(bytecode: Vec<Bytecode>) -> VirtualMachine {
        VirtualMachine {
            stack: stack::BumpStack::new(1024),
            heap: heap::HeapMemory::new(),
            program_counter: 1024,
            bytecode,
            variables: HashMap::new(),
            profiler: profiler::Profiler::new(),
            pass_manager: pass_manager::PassManager::new(),
            function_module: module::ZetaModule::new(),
            event_loop: EventLoop::new(),
            instruction_counter: 0
        }
    }
    
    pub fn schedule_function(&mut self, function_id: u64, execution_context: ExecutionContext) {
        let function: &mut Function = self.function_module
            .get_function_mut(function_id)
            .unwrap();
        
        if execution_context == ExecutionContext::Sync {
            // We don't need an event loop for sync functions
            self.run_function(function_id);
            return;
        }
        
        self.event_loop.add_event(Event {
            function_id,
            execution_context
        });
    }

    pub fn run_function(&mut self,
                        function_id: u64) {
        let function: &mut Function = self.function_module
            .get_function_mut(function_id)
            .unwrap();
        if function.is_native {
            let now = std::time::Instant::now();
            functions::run_native_function(function);
            self.profiler.record_call(function_id, now.elapsed().as_nanos() as u64);
            return;
        }

        let call_frame: &StackFrame = self.stack.push_frame();
        let program_counter: usize = 0;
        let instruction_counter: usize = 0;

        let now = std::time::Instant::now();
        functions::interpret_function(
            call_frame,
            program_counter,
            instruction_counter,
            function
        );
        self.profiler.record_call(function_id, now.elapsed().as_nanos() as u64);
        self.stack.pop_frame();

        // check profiler results
        let function_call_count = self.profiler.get_func_call_count(function_id);
        functions::optimize_function(function, function_call_count, &mut self.pass_manager);
    }

    pub fn run(&mut self, program: &[Bytecode]) {
        // Pre program
        while self.program_counter < program.len() {
            // ...
        }

        // Post program
        self.stack.reset();
        self.heap.free_all();
    }
}
