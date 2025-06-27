use std::cell::LazyCell;
// Core VM components
use crate::vm::eventloop::FiberScheduler;
use crate::vm::fibers::Fiber;
use crate::vm::instruction_handlers::{
    arithmetic::ArithmeticHandler,
    array_ops::ArrayOpsHandler,
    class_ops::ClassInitHandler,
    control_flow::ControlFlowHandler,
    memory::MemoryHandler,
    pointer_ops::PointerOpsHandler,
    stack_ops::StackOpsHandler,
    type_ops::TypeOpsHandler,
    InstructionDispatcher
};
use crate::vm::interpreter::*;
use crate::vm::memory::frames::{OptimizationFrame, StackFrame};
use crate::vm::memory::heap;
use crate::vm::memory::stack::BumpStack;
use crate::vm::memory::string_pool::StringPool;
use crate::vm::{functions, println, profiler};

// External dependencies
use crossbeam::channel::{unbounded, Receiver, Sender};
use ir::VMValue;
use mimalloc;
use parking_lot::Mutex;
use trc::SharedTrc;
use zetac::codegen::ir::ir_compiler::CompressedClassTable;
use zetac::codegen::ir::module::Function;
use zetac::codegen::ir::module::ZetaModule;
use zetac::codegen::ir::optimization::pass_manager::PassManager;

#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

unsafe impl Send for VirtualMachine {}
unsafe impl Sync for VirtualMachine {}

pub struct VirtualMachine {
    pub(crate) stack: BumpStack,
    pub(crate) heap: heap::HeapMemory,
    pub(crate) profiler: profiler::Profiler,
    pub(crate) function_module: SharedTrc<Mutex<ZetaModule>>,
    pub(crate) pass_manager: SharedTrc<Mutex<PassManager>>,
    pub(crate) event_loop: LazyCell<FiberScheduler>,
    pub(crate) string_pool: SharedTrc<Mutex<StringPool>>,
    pub(crate) optimization_receiver: Receiver<OptimizationFrame>,
    pub(crate) optimization_sender: Sender<OptimizationFrame>,
    pub(crate) instruction_dispatcher: InstructionDispatcher,
    pub call_stack: Vec<StackFrame>,
    pub(crate) max_stack_depth: usize,
    pub(crate) current_stack_depth: usize
}

impl VirtualMachine {
    pub fn new(mut function_module: ZetaModule, class_table: CompressedClassTable) -> VirtualMachine {
        // Register native methods
        use zetac::codegen::ir::module::NativeFnPtr;
        
        let string_pool = Mutex::new(StringPool::new());
        let string_pool = SharedTrc::new(string_pool);
        
        let println_fn = Function::new_native(
            "println".to_string(),
            println::println as NativeFnPtr
        );
        function_module.add_function(println_fn);

        let (optimization_sender, optimization_receiver) = unbounded();
        
        let mut instruction_dispatcher = InstructionDispatcher::new();
        instruction_dispatcher.register_many_opcodes(
            &[
                ADD, SUB, MUL, DIV, MOD,
                AND, OR, XOR, NOT, SHL, SHR,
                EQ, NEQ, LT, LTE, GT, GTE,
                ASSIGN, ADD_ASSIGN, SUB_ASSIGN, MUL_ASSIGN, DIV_ASSIGN, MOD_ASSIGN,
                BIT_AND_ASSIGN, BIT_OR_ASSIGN, BIT_XOR_ASSIGN, SHL_ASSIGN, SHR_ASSIGN, POW_ASSIGN
            ],
            ArithmeticHandler,
        );
        
        instruction_dispatcher.register(CAST, TypeOpsHandler);
        
        // Control flow
        instruction_dispatcher.register_many_opcodes(
            &[ 
                BRANCH, RETURN, HALT
            ],
            ControlFlowHandler,
        );
        
        instruction_dispatcher.register_many_opcodes(
            &[ 
                GET_FIELD, GET_ARRAY_MUT, SET_ARRAY_MUT, ARRAY_ALLOC, ARRAY_LEN, ARRAY_GET, ARRAY_SET
            ],
            PointerOpsHandler,
        );
        
        instruction_dispatcher.register_many_opcodes(
            &[ 
                LOAD, STORE, LOAD_VAR, STORE_VAR, LOAD_GLOBAL, STORE_GLOBAL, LOAD_LOCAL, STORE_LOCAL
            ],
            MemoryHandler::new(),
        );
        
        // Register class operations
        instruction_dispatcher.register(
            CLASS_INIT,
            ClassInitHandler { class_table },
        );

        instruction_dispatcher.register_many_opcodes(
            &[ 
                GET_PTR, SET_PTR, GET_PTR_MUT, SET_PTR_MUT, ADDRESS_OF, DEREF, DEREF_MUT, CAST
            ],
            PointerOpsHandler,
        );

        instruction_dispatcher.register_many_opcodes(
            &[ 
                DUP, SWAP, POP, PUSH_BOOL, PUSH_STR, PUSH_INT,
                PUSH_I8, PUSH_I16, PUSH_I32, PUSH_I64, PUSH_U8, PUSH_U16, PUSH_U32, PUSH_U64, PUSH_F32, PUSH_F64
            ],
            StackOpsHandler::new(string_pool.clone()),
        );

        instruction_dispatcher.register_many_opcodes(
            &[
                ARRAY_ALLOC, ARRAY_LEN, ARRAY_GET, ARRAY_SET, GET_ARRAY_MUT, SET_ARRAY_MUT
            ],
            ArrayOpsHandler,
        );
        
        VirtualMachine {
            stack: BumpStack::new(1024),
            heap: heap::HeapMemory::new(),
            profiler: profiler::Profiler::new(),
            function_module: SharedTrc::new(Mutex::new(function_module)),
            pass_manager: SharedTrc::new(Mutex::new(PassManager::new())),
            event_loop: LazyCell::new(|| FiberScheduler::new()),
            string_pool,
            optimization_receiver,
            optimization_sender,
            instruction_dispatcher,
            call_stack: Vec::new(),
            max_stack_depth: 1024 * 1024, // 1MB stack
            current_stack_depth: 0
        }
    }

    /// Run a function but in a fiber
    pub fn run_function_in_fiber(&mut self, function_id: u64) {
        // Check stack depth before spawning new fiber
        if self.current_stack_depth >= self.max_stack_depth {
            panic!("Stack overflow: Maximum call stack depth of {} exceeded", self.max_stack_depth);
        }
        self.current_stack_depth += 1;
        self.event_loop.spawn(Fiber::new(function_id));
    }

    pub fn run_function(&mut self, function_id: u64, vm_args: Vec<VMValue>) {
        // Check stack depth before calling function
        if self.current_stack_depth >= self.max_stack_depth {
            panic!("Stack overflow: Maximum call stack depth of {} exceeded", self.max_stack_depth);
        }
        
        let mut function = {
            let mut module = self.function_module.lock();
            module.get_function_mut(function_id).cloned().unwrap() 
        };

        if function.is_native {
            // For native functions, we need to pass the arguments as a raw pointer and length
            let result = unsafe {
                let resolved_args: Vec<VMValue> = vm_args.into_iter()
                    .map(|arg| match arg {
                        VMValue::Str(vm_str) => {
                            println!("Resolving string: {}", vm_str);
                            println!("String pool: {:#?}", self.string_pool);
                            let lock = self.string_pool.lock();
                            let resolved = lock.resolve_string(&vm_str);
                            VMValue::ResolvedStr(resolved.to_owned())
                        }
                        other => other,
                    })
                    .collect();
                if !resolved_args.is_empty() {
                    functions::run_native_function(&function, resolved_args.as_ptr(), resolved_args.len())
                } else {
                    functions::run_native_function(&function, std::ptr::null(), 0)
                }
            };
            
            // If the function returns a value, push it to the stack
            if let VMValue::Void = result {
                // No return value
            } else {
                self.stack.push_vm(result);
            }
            return;
        }

        let mut frame = StackFrame::new(0, 0, function_id);
        for arg in vm_args.into_iter().rev() {
            frame.stack.push_vm(arg);
        }
        
        let mut frame = StackFrame::new(0, 0, function_id);
        self.interpret_function(&mut frame, &mut function.code);
        self.current_stack_depth += 1;

        let call_count = self.profiler.get_func_call_count(function_id);
        let optimization_frame = OptimizationFrame {
            function_id,
            call_count: call_count.count,
        };
        self.optimization_sender.send(optimization_frame).unwrap();
    }

    /*pub fn execute(&mut self) {
        while let Some(frame) = self.call_stack.last_mut() {
            let function_code = {
                let module = self.function_module.lock();
                module.get_function(frame.function_id).cloned().unwrap()
            };
            
            let code = &function_code.code;

            while frame.program_counter < code.len() {
                let instr = code[frame.program_counter];
                frame.program_counter += 1;

                if instr == RETURN {
                    self.call_stack.pop();
                    break;
                }

                self.interpret_instruction(instr, frame, code);
            }
        }
    }*/


    pub fn run(&mut self) {
        // Pre program
        let optional_main_function_id = self.function_module.lock().entry;
        if let Some(main_function_id) = optional_main_function_id {
            self.run_function_in_fiber(main_function_id);

            while self.tick() {}

            // Post program
            self.halt()
        } else {
            eprintln!("No entry point found, please add a main function like the following:");
            eprintln!("fun main() {{\n    println_str(\"Hello World!\");\n}}");
            std::process::exit(1);
        }
    }

    pub fn tick(&mut self) -> bool {
        
        let event_loop = std::mem::replace(&mut self.event_loop, LazyCell::new(|| FiberScheduler::new()));
        let work_left = event_loop.tick(self);
        self.event_loop = event_loop;

        if !work_left {
            return false;
        }

        // Throttle optimization (e.g., 1 per tick)
        if let Ok(task) = self.optimization_receiver.try_recv() {
            tokio::spawn(functions::optimize_function_async(
                self.function_module.clone(),
                self.pass_manager.clone(),
                task,
            ));
        }
        true
    }

    pub fn interpret_function(&mut self, call_frame: &mut StackFrame, function: &mut Vec<u8>) {
        call_frame.program_counter = 0;
        while call_frame.program_counter < function.len() {
            let opcode = function[call_frame.program_counter];
            call_frame.program_counter += 1;
            self.interpret_instruction(opcode, call_frame, function);
        }
    }

    /// Interpret a single instruction.
    ///
    /// Returns `true` if execution should continue, or `false` if the current function should return.
    pub fn interpret_instruction(&mut self, instr: u8, frame: &mut StackFrame, function: &[u8]) {
        // Set the current opcode in the frame for handlers to access
        frame.current_opcode = instr;

        if instr == HALT {
            self.halt();
            return;
        }
        
        // Handle RETURN instruction to properly decrement stack depth
        if instr == RETURN {
            if self.current_stack_depth > 0 {
                self.current_stack_depth -= 1;
            }
            return;
        }

        if instr == CALL {
            if self.current_stack_depth >= self.max_stack_depth {
                panic!("Stack overflow: Maximum call stack depth of {} exceeded", self.max_stack_depth);
            }
            self.current_stack_depth += 1;

            let (name, arguments, func) = self.get_function_metadata(frame, &function.to_vec());
            if let Some(func) = func {
                self.run_function(func.id, arguments);
            } else {
                eprintln!("Function {} not found", name);
                self.halt();
            }
            return;
        }

        if instr == CALL_IN_FIBER {
            if self.current_stack_depth >= self.max_stack_depth {
                panic!("Stack overflow: Maximum call stack depth of {} exceeded", self.max_stack_depth);
            }
            self.current_stack_depth += 1;

            let (name, _, func) = self.get_function_metadata(frame, &function.to_vec());
            if let Some(func) = func {
                self.run_function_in_fiber(func.id);
            } else {
                eprintln!("Function {} not found", name);
                self.halt();
            }
            return;
        }

        let result = self.instruction_dispatcher.execute(instr, frame, function);
        match result {
            Ok(_) => {}
            Err(e) => {
                eprintln!("Error at instruction {} (0x{:x}): {}", instr, instr, e);
                if let Some(func) = self.function_module.lock().get_function(frame.function_id) {
                    eprintln!("In function: {} (ID: {})", func.name, frame.function_id);
                }
                eprintln!("Stack trace (depth: {}):", self.current_stack_depth);
                for (i, frame) in self.call_stack.iter().enumerate() {
                    if let Some(func) = self.function_module.lock().get_function(frame.function_id) {
                        eprintln!("  #{}: {} (PC: {})", i, func.name, frame.program_counter);
                    } else {
                        eprintln!("  #{}: <unknown> (ID: {}, PC: {})", i, frame.function_id, frame.program_counter);
                    }
                }
                self.halt();
            }
        }
    }

    pub(crate) fn halt(&mut self) {
        self.event_loop.shutdown();
        self.heap.free_all();
        std::process::exit(1);
    }

    fn get_function_metadata(&mut self, frame: &mut StackFrame, function: &Vec<u8>) -> (String, Vec<VMValue>, Option<Function>) {
        let name = fetch_string_u16_len(function, &mut frame.program_counter);

        let arity = fetch_u8(function, &mut frame.program_counter);

        // Pop arguments in reverse order (or leave on stack if callee expects to read)
        let mut arguments = Vec::with_capacity(arity as usize);
        for _ in 0..arity {
            arguments.push(frame.stack.pop_vm().unwrap());
        }
        arguments.reverse(); // preserve original order

        let func = {
            self.function_module.lock().get_function_by_name(&name).cloned()
        };
        (name, arguments, func)
    }
}
