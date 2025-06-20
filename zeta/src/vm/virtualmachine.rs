use crate::vm::memory::frames::{OptimizationFrame, StackFrame};
use crate::vm::{functions, println, profiler};
use ir::{Bytecode, VMValue};
use std::sync::Arc;
use parking_lot::Mutex;
use crossbeam::channel::{unbounded, Receiver, Sender};
use crate::vm::eventloop::FiberScheduler;
use crate::vm::fibers::Fiber;
use crate::vm::memory::heap;
use crate::vm::memory::string_pool::StringPool;
use zetac::codegen::ir::module::ZetaModule;

use mimalloc;
use zetac::ast::Visibility;
use zetac::codegen::ir::module::Function;
use zetac::codegen::ir::optimization::pass::OptimizationPassPriority;
use zetac::codegen::ir::optimization::pass_manager::PassManager;
use crate::{binary_op_cmp, binary_op_int};
use crate::vm::interpreter::*;
use crate::vm::memory::regions::RegionAllocator;
use crate::vm::memory::stack;

#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

unsafe impl Send for VirtualMachine {}
unsafe impl Sync for VirtualMachine {}

pub struct VirtualMachine {
    pub(crate) stack: stack::BumpStack,
    pub(crate) heap: heap::HeapMemory,
    pub(crate) profiler: profiler::Profiler,
    pub(crate) function_module: Arc<Mutex<ZetaModule>>,
    pub(crate) pass_manager: Arc<Mutex<PassManager>>,
    pub(crate) event_loop: FiberScheduler,
    pub(crate) string_pool: StringPool,
    pub(crate) optimization_receiver: Receiver<OptimizationFrame>,
    pub(crate) optimization_sender: Sender<OptimizationFrame>,
    pub(crate) region_allocator: RegionAllocator
}

impl VirtualMachine {
    #[inline]
    pub fn new(mut function_module: ZetaModule) -> VirtualMachine {
        // Let's add some native methods, like println_str
        let println_str_function = Function {
            name: "println_str".to_string(),
            id: 2,
            params: vec![],
            return_type: None,
            visibility: Some(Visibility::Public),
            is_method: false,
            is_main: false,
            is_native: true,
            native_pointer: Some(Box::new(println::println_str as *const u8)),
            locals: vec![],
            code: vec![],
            optimization_level: OptimizationPassPriority::Max // To prevent the virtual machine from optimizing this function
        };

        function_module.add_function(println_str_function);

        let (optimization_sender, optimization_receiver) = unbounded();
        VirtualMachine {
            stack: stack::BumpStack::new(1024),
            heap: heap::HeapMemory::new(),
            profiler: profiler::Profiler::new(),
            function_module: Arc::new(Mutex::new(function_module)),
            pass_manager: Arc::new(Mutex::new(PassManager::new())),
            event_loop: FiberScheduler::new(),
            string_pool: StringPool::new(),
            optimization_receiver,
            optimization_sender,
            region_allocator: RegionAllocator::new()
        }
    }

    /// Run a function but in a fiber
    pub fn run_function_in_fiber(&mut self, function_id: u64) {
        self.event_loop.spawn(Fiber::new(function_id));
    }

    pub fn run_function(&mut self, function_id: u64, vm_args: Vec<VMValue>) {
        let function_module = self.function_module.clone();

        let function = {
            function_module.lock().get_function(function_id).unwrap().clone()
        };

        if function.is_native && function.optimization_level == OptimizationPassPriority::Max {
            functions::run_native_function(&function, &vm_args, &self.string_pool);
            return;
        }

        let now = std::time::Instant::now();
        if function.is_native {
            functions::run_native_function(&function, &vm_args, &self.string_pool);
        } else {
            let mut frame = StackFrame::new(0, 0, function_id);
            // For non-native functions, push arguments to the stack
            for arg in vm_args.into_iter().rev() {
                frame.stack.push_vm(arg);
            }
            self.interpret_function(&mut frame, &mut function.code.clone());
        }

        self.profiler.record_call(function_id, now.elapsed().as_nanos());

        // Offload optimization to a queue
        let call_count = self.profiler.get_func_call_count(function_id);
        let optimization_frame = OptimizationFrame {
            function_id,
            call_count: call_count.count,
        };
        self.optimization_sender.send(optimization_frame).unwrap();
    }

    pub fn run(&mut self) {
        // Pre program
        let optional_main_function_id = self.function_module.lock().entry;
        if let Some(main_function_id) = optional_main_function_id {
            self.run_function_in_fiber(main_function_id);

            while self.tick() {}

            // Post program
            self.heap.free_all();
        } else {
            eprintln!("No entry point found, please add a main function like the following:");
            eprintln!("fun main() {{\n    println_str(\"Hello World!\");\n}}");
            std::process::exit(1);
        }
    }

    pub fn tick(&mut self) -> bool {
        let event_loop = std::mem::replace(&mut self.event_loop, FiberScheduler::new());
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

    pub fn interpret_instruction(&mut self, instr: u8, frame: &mut StackFrame, function: &Vec<u8>) -> bool {
        match instr {
            ADD => binary_op_int!(frame.stack, +),
            SUB => binary_op_int!(frame.stack, -),
            MUL => binary_op_int!(frame.stack, *),
            DIV => binary_op_int!(frame.stack, /),
            MOD => binary_op_int!(frame.stack, %),
            AND => binary_op_int!(frame.stack, &),
            OR => binary_op_int!(frame.stack, |),
            XOR => binary_op_int!(frame.stack, ^),
            NOT => {
                let val = frame.stack.pop_vm();
                match val {
                    Some(VMValue::Bool(val)) => {
                        frame.stack.push_vm(VMValue::Bool(!val));
                    }
                    _ => panic!("Invalid type for NOT")
                }
            }
            SHL => binary_op_int!(frame.stack, <<),
            SHR => binary_op_int!(frame.stack, >>),

            EQ => binary_op_cmp!(frame.stack, ==),
            NEQ => binary_op_cmp!(frame.stack, !=),
            LT => binary_op_cmp!(frame.stack, <),
            LTE => binary_op_cmp!(frame.stack, <=),
            GT => binary_op_cmp!(frame.stack, >),
            GTE => binary_op_cmp!(frame.stack, >=),

            PUSH_U8 => {
                let v = fetch_u8(function, &mut frame.program_counter);
                frame.stack.push_vm(VMValue::U8(v));
            }

            PUSH_I8 => {
                let v = fetch_i8(function, &mut frame.program_counter);
                frame.stack.push_vm(VMValue::I8(v));
            }

            PUSH_U16 => {
                let v = fetch_u16(function, &mut frame.program_counter);
                frame.stack.push_vm(VMValue::U16(v))
            }

            PUSH_I16 => {
                let v = fetch_i16(function, &mut frame.program_counter);
                frame.stack.push_vm(VMValue::I16(v))
            }

            PUSH_U32 => {
                let v = fetch_u32(function, &mut frame.program_counter);
                frame.stack.push_vm(VMValue::U32(v))
            }

            PUSH_I32 => {
                let v = fetch_i32(function, &mut frame.program_counter);
                frame.stack.push_vm(VMValue::I32(v))
            }

            PUSH_F32 => {
                let v = fetch_f32(function, &mut frame.program_counter);
                frame.stack.push_vm(VMValue::F32(v))
            }

            PUSH_F64 => {
                let v = fetch_f64(function, &mut frame.program_counter);
                frame.stack.push_vm(VMValue::F64(v))
            }

            PUSH_U64 => {
                let v = fetch_u64(function, &mut frame.program_counter);
                frame.stack.push_vm(VMValue::U64(v))
            }

            PUSH_I64 => {
                let v = fetch_i64(function, &mut frame.program_counter);
                frame.stack.push_vm(VMValue::I64(v))
            }


            PUSH_BOOL => {
                let b = fetch_u8(function, &mut frame.program_counter) != 0;
                frame.stack.push_vm(VMValue::Bool(b))
            }

            PUSH_STR => {
                let offset = fetch_u16(function, &mut frame.program_counter);
                let s = fetch_string(offset, function, &mut frame.program_counter);

                let id = self.string_pool.intern(&s);
                frame.stack.push_vm(VMValue::Str(id))
            }

            POP => {
                frame.stack.pop_vm();
            }

            DUP => {
                let v = frame.stack.peek_vm().cloned();
                frame.stack.push_vm(v.unwrap());
            }

            SWAP => {
                let v1 = frame.stack.pop_vm().unwrap();
                let v2 = frame.stack.pop_vm().unwrap();
                frame.stack.push_vm(v1);
                frame.stack.push_vm(v2);
            }

            RETURN => {
                return false;
            }

            HALT => {
                std::process::exit(0);
            }

            // Jumps
            JUMP => {
                frame.program_counter += 1;
                let offset = fetch_i16(function, &mut frame.program_counter) as usize;
                frame.program_counter += offset;
            }

            JUMP_IF_TRUE => {
                frame.program_counter += 1;
                let offset = fetch_i16(function, &mut frame.program_counter) as usize;
                let condition = frame.stack.pop_vm();
                match condition {
                    Some(VMValue::Bool(condition)) => {
                        if condition {
                            frame.program_counter += offset;
                        }
                    }
                    _ => panic!("Invalid type for JUMP_IF_TRUE")
                }
            }

            JUMP_IF_FALSE => {
                frame.program_counter += 1;
                let offset = fetch_i16(function, &mut frame.program_counter) as usize;
                let condition = frame.stack.pop_vm();
                match condition {
                    Some(VMValue::Bool(condition)) => {
                        if condition {
                            frame.program_counter += offset;
                        }
                    }
                    _ => panic!("Invalid type for JUMP_IF_TRUE")
                }
            }

            BRANCH => {
                todo!("BRANCH");
            }

            // Regions
            NEW_REGION => {
                frame.program_counter += 1;
                let offset = fetch_u16(function, &mut frame.program_counter);
                let name_as_string = fetch_string(offset, function, &mut frame.program_counter);
                let name = name_as_string.trim();
                frame.push_region(
                    self.string_pool.intern(name),
                    self.region_allocator.new_region()
                );
            }

            CALL => {
                let name = fetch_string_u16_len(function, &mut frame.program_counter);

                let arity = fetch_u8(function, &mut frame.program_counter);

                // Pop arguments in reverse order (or leave on stack if callee expects to read)
                let mut arguments = Vec::with_capacity(arity as usize);
                for _ in 0..arity {
                    arguments.push(frame.stack.pop_vm().unwrap());
                }
                arguments.reverse(); // preserve original order

                let func = {
                    self.function_module.lock().get_function_by_name(&name)
                };
                if let Some(func) = func {
                    self.run_function(func.id, arguments);
                } else {
                    eprintln!("Function {} not found", name);
                    std::process::exit(1);
                }
            }

            ASSIGN => {
                let value = frame.stack.pop_vm().expect("Stack underflow on Assign");
                frame.stack.push_vm(value);
            }

            ADD_ASSIGN => binary_op_int!(frame.stack, +),

            SUB_ASSIGN => binary_op_int!(frame.stack, -),

            MUL_ASSIGN => binary_op_int!(frame.stack, *),

            DIV_ASSIGN => binary_op_int!(frame.stack, /),

            MOD_ASSIGN => binary_op_int!(frame.stack, %),

            SHL_ASSIGN => binary_op_int!(frame.stack, <<),
            SHR_ASSIGN => binary_op_int!(frame.stack, >>),

            BIT_AND_ASSIGN => binary_op_int!(frame.stack, &),

            BIT_OR_ASSIGN => binary_op_int!(frame.stack, |),

            BIT_XOR_ASSIGN => binary_op_int!(frame.stack, ^),

            POW_ASSIGN => {
                let rhs_option = frame.stack.pop_vm();
                let lhs_option = frame.stack.pop_vm();

                let rhs = match rhs_option {
                    Some(vm_value) => vm_value,
                    None => panic!("RHS is None")
                };

                let lhs = match lhs_option {
                    Some(vm_value) => vm_value,
                    None => panic!("RHS is None")
                };

                match (lhs.clone(), rhs.clone()) {
                    (VMValue::I32(lhs), VMValue::I32(rhs)) => {
                        frame.stack.push_vm(VMValue::I32(lhs.pow(rhs as u32)));
                    }
                    (VMValue::I64(lhs), VMValue::I64(rhs)) => {
                        frame.stack.push_vm(VMValue::I64(lhs.pow(rhs as u32)));
                    }
                    (VMValue::U32(lhs), VMValue::U32(rhs)) => {
                        frame.stack.push_vm(VMValue::U32(lhs.pow(rhs)));
                    }
                    (VMValue::U64(lhs), VMValue::U64(rhs)) => {
                        frame.stack.push_vm(VMValue::U64(lhs.pow(rhs as u32)));
                    }
                    (VMValue::F32(lhs), VMValue::F32(rhs)) => {
                        frame.stack.push_vm(VMValue::F32(lhs.powf(rhs)));
                    }
                    (VMValue::F64(lhs), VMValue::F64(rhs)) => {
                        frame.stack.push_vm(VMValue::F64(lhs.powf(rhs)));
                    }
                    (VMValue::I8(lhs), VMValue::I8(rhs)) => {
                        frame.stack.push_vm(VMValue::I8(lhs.pow(rhs as u32)));
                    }
                    (VMValue::I16(lhs), VMValue::I16(rhs)) => {
                        frame.stack.push_vm(VMValue::I16(lhs.pow(rhs as u32)));
                    }
                    (VMValue::U8(lhs), VMValue::U8(rhs)) => {
                        frame.stack.push_vm(VMValue::U8(lhs.pow(rhs as u32)));
                    }
                    (VMValue::U16(lhs), VMValue::U16(rhs)) => {
                        frame.stack.push_vm(VMValue::U16(lhs.pow(rhs as u32)));
                    }
                    (VMValue::Ptr(lhs), VMValue::Ptr(rhs)) => {
                        frame.stack.push_vm(VMValue::Ptr(lhs.pow(rhs as u32)));
                    }

                    _ => panic!("Invalid types for binary operation: {:?} {:?}", lhs, rhs)
                }
            }

            TAIL_CALL => {
                todo!("TAIL_CALL");
            }

            LOAD => {
                todo!("LOAD");
            }

            STORE => {
                todo!("STORE");
            }

            LOAD_VAR => {
                let offset = fetch_u16(function, &mut frame.program_counter);
                let name = fetch_string(offset, function, &mut frame.program_counter);

                match frame.locals.get(&name) {
                    Some(value) => frame.stack.push_vm(value.clone()),
                    None => panic!("Undefined local variable '{}'", name),
                }
            }

            STORE_VAR => {
                let offset = fetch_u16(function, &mut frame.program_counter);
                let name = fetch_string(offset, function, &mut frame.program_counter);

                let value = frame.stack.pop_vm().expect("Stack underflow in STORE_VAR");
                frame.locals.insert(name.to_string(), value);
            }

            LOAD_GLOBAL => {
                todo!("LOAD_GLOBAL");
            }

            STORE_GLOBAL => {
                todo!("STORE_GLOBAL");
            }

            LOAD_LOCAL => {
                todo!("LOAD_LOCAL");
            }

            STORE_LOCAL => {
                todo!("STORE_LOCAL");
            }

            GET_PTR => {
                todo!("GET_PTR");
            }

            SET_PTR => {
                todo!("SET_PTR");
            }

            GET_PTR_MUT => {
                todo!("GET_PTR_MUT");
            }

            SET_PTR_MUT => {
                todo!("SET_PTR_MUT");
            }

            ADDRESS_OF => {
                todo!("ADDRESS_OF");
            }

            DEREF => {
                todo!("DEREF");
            }

            DEREF_MUT => {
                todo!("DEREF_MUT");
            }

            CAST => {
                todo!("CAST");
            }

            ARRAY_GET => {
                todo!("ARRAY_GET");
            }

            ARRAY_SET => {
                todo!("ARRAY_SET");
            }

            ARRAY_LEN => {
                todo!("ARRAY_LEN");
            }

            ARRAY_ALLOC => {
                todo!("ARRAY_ALLOC");
            }

            GET_ARRAY_MUT => {
                todo!("GET_ARRAY_MUT");
            }

            SET_ARRAY_MUT => {
                todo!("SET_ARRAY_MUT");
            }

            GET_FIELD => {
                todo!("GET_FIELD");
            }

            _ => {
                panic!("Unimplemented opcode: {instr}");
            }
        }

        true
    }
}
