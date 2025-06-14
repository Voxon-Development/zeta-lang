use crate::vm::frames::{OptimizationFrame, StackFrame};
use crate::vm::{functions, profiler, stack};
use ir::{Bytecode, VMValue};
use std::sync::{Arc, Mutex};
use crossbeam::channel::{unbounded, Receiver, Sender};
use crate::vm::eventloop::FiberScheduler;
use crate::vm::fibers::Fiber;
use crate::vm::heap;
use crate::vm::string_pool::StringPool;
use radix_trie::Trie;
use zetac::codegen::ir::module;

use mimalloc;
use zetac::ast::{Param, Type, Visibility};
use zetac::codegen::ir::module::Function;
use zetac::codegen::ir::optimization::pass::OptimizationPassPriority;
use zetac::codegen::ir::optimization::pass_manager::PassManager;

#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

const ADD: u8 = Bytecode::Add as u8;
const SUB: u8 = Bytecode::Sub as u8;
const MUL: u8 = Bytecode::Mul as u8;
const DIV: u8 = Bytecode::Div as u8;
const MOD: u8 = Bytecode::Mod as u8;
const AND: u8 = Bytecode::And as u8;
const OR: u8 = Bytecode::Or as u8;
const XOR: u8 = Bytecode::Xor as u8;
const NOT: u8 = Bytecode::Not as u8;
const SHL: u8 = Bytecode::Shl as u8;
const SHR: u8 = Bytecode::Shr as u8;
const EQ: u8 = Bytecode::Eq as u8;
const NEQ: u8 = Bytecode::Ne as u8;
const LT: u8 = Bytecode::Lt as u8;
const LTE: u8 = Bytecode::Le as u8;
const GT: u8 = Bytecode::Gt as u8;
const GTE: u8 = Bytecode::Ge as u8;
const JUMP: u8 = Bytecode::Jump as u8;
const JUMP_IF_TRUE: u8 = Bytecode::JumpIfTrue as u8;
const JUMP_IF_FALSE: u8 = Bytecode::JumpIfFalse as u8;

// Assignments

const ASSIGN: u8 = Bytecode::Assign as u8;
const ADD_ASSIGN: u8 = Bytecode::AddAssign as u8;
const SUB_ASSIGN: u8 = Bytecode::SubAssign as u8;
const MUL_ASSIGN: u8 = Bytecode::MulAssign as u8;
const DIV_ASSIGN: u8 = Bytecode::DivAssign as u8;
const MOD_ASSIGN: u8 = Bytecode::ModAssign as u8;
const BIT_AND_ASSIGN: u8 = Bytecode::BitAndAssign as u8;
const BIT_OR_ASSIGN: u8 = Bytecode::BitOrAssign as u8;
const BIT_XOR_ASSIGN: u8 = Bytecode::BitXorAssign as u8;
const SHL_ASSIGN: u8 = Bytecode::ShlAssign as u8;
const SHR_ASSIGN: u8 = Bytecode::ShrAssign as u8;

const BRANCH: u8 = Bytecode::Branch as u8;
const RETURN: u8 = Bytecode::Return as u8;
const HALT: u8 = Bytecode::Halt as u8;
const CALL: u8 = Bytecode::Call as u8;
const TAIL_CALL: u8 = Bytecode::TailCall as u8;
const ARRAY_GET: u8 = Bytecode::ArrayGet as u8;
const ARRAY_SET: u8 = Bytecode::ArraySet as u8;
const GET_ARRAY_MUT: u8 = Bytecode::GetArrayMut as u8;
const SET_ARRAY_MUT: u8 = Bytecode::SetArrayMut as u8;
const GET_FIELD: u8 = Bytecode::GetField as u8;
const ARRAY_ALLOC: u8 = Bytecode::ArrayAlloc as u8;
const ARRAY_LEN: u8 = Bytecode::ArrayLen as u8;

const PUSH_I64: u8 = Bytecode::PushI64 as u8;
const PUSH_F64: u8 = Bytecode::PushF64 as u8;
const PUSH_U8: u8 = Bytecode::PushU8 as u8;
const PUSH_U16: u8 = Bytecode::PushU16 as u8;
const PUSH_U32: u8 = Bytecode::PushU32 as u8;
const PUSH_U64: u8 = Bytecode::PushU64 as u8;
const PUSH_I8: u8 = Bytecode::PushI8 as u8;
const PUSH_I16: u8 = Bytecode::PushI16 as u8;
const PUSH_I32: u8 = Bytecode::PushI32 as u8;

const PUSH_F32: u8 = Bytecode::PushF32 as u8;
const PUSH_STR: u8 = Bytecode::PushStr as u8;
const PUSH_BOOL: u8 = Bytecode::PushBool as u8;

const POP: u8 = Bytecode::Pop as u8;
const DUP: u8 = Bytecode::Dup as u8;
const SWAP: u8 = Bytecode::Swap as u8;

const LOAD: u8 = Bytecode::Load as u8;
const STORE: u8 = Bytecode::Store as u8;
const LOAD_VAR: u8 = Bytecode::LoadVar as u8;
const STORE_VAR: u8 = Bytecode::StoreVar as u8;
const LOAD_GLOBAL: u8 = Bytecode::LoadGlobal as u8;
const STORE_GLOBAL: u8 = Bytecode::StoreGlobal as u8;
const LOAD_LOCAL: u8 = Bytecode::LoadLocal as u8;
const STORE_LOCAL: u8 = Bytecode::StoreLocal as u8;

const GET_PTR: u8 = Bytecode::GetPtr as u8;
const SET_PTR: u8 = Bytecode::SetPtr as u8;
const GET_PTR_MUT: u8 = Bytecode::GetPtrMut as u8;
const SET_PTR_MUT: u8 = Bytecode::SetPtrMut as u8;
const ADDRESS_OF: u8 = Bytecode::AddressOf as u8;
const DEREF: u8 = Bytecode::Deref as u8;
const DEREF_MUT: u8 = Bytecode::DerefMut as u8;
const CAST: u8 = Bytecode::Cast as u8;

macro_rules! binary_op_int {
    ($self:ident, $op:tt) => {{
        let rhs = $self.stack.pop_int();
        let lhs = $self.stack.pop_int();
        $self.stack.push_int(lhs $op rhs);
    }};
}

macro_rules! binary_op_cmp {
    ($self:ident, $op:tt) => {{
        let rhs = $self.stack.pop_int();
        let lhs = $self.stack.pop_int();
        $self.stack.push_bool(lhs $op rhs);
    }};
}


pub struct VirtualMachine {
    stack: stack::BumpStack,
    heap: heap::HeapMemory,
    program_counter: usize,
    variables: Trie<String, i64>,
    profiler: profiler::Profiler,
    function_module: Arc<Mutex<module::ZetaModule>>,
    pass_manager: Arc<Mutex<PassManager>>,
    event_loop: FiberScheduler,
    instruction_counter: usize,
    string_pool: StringPool,
    optimization_receiver: Receiver<OptimizationFrame>,
    optimization_sender: Sender<OptimizationFrame>,
}

unsafe impl Send for VirtualMachine {}
unsafe impl Sync for VirtualMachine {}

pub extern "C" fn println_str(s: *const u8) {
    unsafe {
        let cstr = std::ffi::CStr::from_ptr(s as *const i8);
        println!("{}", cstr.to_str().unwrap());
    }
}

impl VirtualMachine {
    #[inline]
    pub fn new(mut function_module: module::ZetaModule) -> VirtualMachine {
        // Let's add some native methods, like println_str
        let println_str_function = Function {
            name: "println_s".to_string(),
            id: 2,
            params: vec![Param {
                name: "thing_to_print".to_string(),
                type_annotation: Some(Type::String)
            }],
            return_type: None,
            visibility: Some(Visibility::Public),
            is_method: false,
            is_main: false,
            is_native: true,
            native_pointer: Some(Box::new(println_str as *const u8)),
            locals: vec![],
            code: vec![],
            optimization_level: OptimizationPassPriority::Max // To prevent the virtual machine from optimizing this function
        };

        function_module.add_function(println_str_function);

        let (optimization_sender, optimization_receiver) = unbounded();
        VirtualMachine {
            stack: stack::BumpStack::new(1024),
            heap: heap::HeapMemory::new(),
            program_counter: 1024,
            variables: Trie::new(),
            profiler: profiler::Profiler::new(),
            function_module: Arc::new(Mutex::new(function_module)),
            pass_manager: Arc::new(Mutex::new(PassManager::new())),
            event_loop: FiberScheduler::new(),
            instruction_counter: 0,
            string_pool: StringPool::new(),
            optimization_receiver,
            optimization_sender,
        }
    }

    /// Run a function but in a fiber
    pub fn run_function_in_fiber(&mut self, function_id: u64) {
        self.event_loop.spawn(Fiber::new(function_id, self.function_module.lock().unwrap().get_function(function_id).unwrap().clone()));
    }

    pub fn run_function(&mut self, function_id: u64, vm_args: Vec<VMValue>) {
        let function_module = self.function_module.clone();
        let pass_manager = self.pass_manager.clone();

        let function = {
            function_module.lock().unwrap().get_function(function_id).unwrap().clone()
        };

        if function.is_native && function.optimization_level == OptimizationPassPriority::Max {
            println!("Running native function optimized to the max");
            functions::run_native_function(&function);
            return;
        }

        let now = std::time::Instant::now();
        if function.is_native {
            functions::run_native_function(&function);
        } else {
            let stack_frame = StackFrame::new(0, 0, function_id);
            self.interpret_function(&stack_frame, &mut function.code.clone());
        }

        self.profiler.record_call(function_id, now.elapsed().as_nanos() as u64);

        // Offload optimization to a background thread
        let call_count = self.profiler.get_func_call_count(function_id);
        let optimization_frame = OptimizationFrame {
            function_id,
            call_count: call_count.count,
        };
        self.optimization_sender.send(optimization_frame).unwrap();
    }

    pub fn run(&mut self) {
        // Pre program
        let optional_main_function_id = self.function_module.lock().unwrap().entry;
        if let Some(main_function_id) = optional_main_function_id {
            println!("Running main function");
            self.run_function_in_fiber(main_function_id);

            while self.tick() {
                // Each tick executes one fiber and one optimization (at most)
            }

            // Post program
            self.stack.reset();
            self.heap.free_all();
        } else {
            eprintln!("No entry point found, please add a main function like the following:");
            eprintln!("fun main() {{\
                println_str(\"Hello World!\");
            }}");
            std::process::exit(1);
        }
    }
    
    async fn optimize_function_async(
        function_module: Arc<Mutex<module::ZetaModule>>,
        pass_manager: Arc<Mutex<PassManager>>,
        task: OptimizationFrame,
    ) {
        let mut function = {
            function_module.lock().unwrap()
                .get_function(task.function_id)
                .cloned()
                .unwrap()
        };

        functions::optimize_function(
            &mut function,
            task,
            &mut pass_manager.lock().unwrap(),
            &function_module.lock().unwrap(),
        );

        function_module.lock().unwrap().add_function(function);
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
            let mut function = {
                self.function_module.lock().unwrap()
                    .get_function(task.function_id)
                    .cloned()
                    .unwrap()
            };

            tokio::spawn(Self::optimize_function_async(
                self.function_module.clone(),
                self.pass_manager.clone(),
                task,
            ));
        }
        true
    }

    pub fn interpret_function(&mut self, call_frame: &StackFrame, function: &mut Vec<u8>) {
        for instruction in function.clone() {
            self.interpret_instruction(instruction, call_frame, function);
        }
    }

    pub fn interpret_instruction(&mut self, instr: u8, _frame: &StackFrame, function: &mut Vec<u8>) -> bool {
        println!("{:?}", Bytecode::from(instr));
        println!("Bytecode: {:?}", function);
        println!("IP: {}", self.instruction_counter);
        println!("Next opcode: {:?}", function[self.instruction_counter]);

        match instr {
            ADD => binary_op_int!(self, +),
            SUB => binary_op_int!(self, -),
            MUL => binary_op_int!(self, *),
            DIV => binary_op_int!(self, /),
            MOD => binary_op_int!(self, %),
            AND => binary_op_int!(self, &),
            OR => binary_op_int!(self, |),
            XOR => binary_op_int!(self, ^),
            NOT => {
                let val = self.stack.pop_int();
                self.stack.push_int(!val);
            }
            SHL => binary_op_int!(self, <<),
            SHR => binary_op_int!(self, >>),

            EQ => binary_op_cmp!(self, ==),
            NEQ => binary_op_cmp!(self, !=),
            LT => binary_op_cmp!(self, <),
            LTE => binary_op_cmp!(self, <=),
            GT => binary_op_cmp!(self, >),
            GTE => binary_op_cmp!(self, >=),

            PUSH_U8 => {
                let v = self.fetch_u8(function);
                self.stack.push_u8(v);
            }

            PUSH_I8 => {
                let v = self.fetch_i8(function);
                self.stack.push_i8(v);
            }

            PUSH_U16 => {
                let v = self.fetch_u16(function);
                self.stack.push_u16(v);
            }

            PUSH_I16 => {
                let v = self.fetch_i16(function);
                self.stack.push_i16(v);
            }

            PUSH_U32 => {
                let v = self.fetch_u32(function);
                self.stack.push_u32(v);
            }

            PUSH_I32 => {
                let v = self.fetch_i32(function);
                self.stack.push_i32(v);
            }

            PUSH_F32 => {
                let v = self.fetch_f32(function);
                self.stack.push_f32(v);
            }

            PUSH_F64 => {
                let v = self.fetch_f64(function);
                self.stack.push_f64(v);
            }

            PUSH_U64 => {
                let v = self.fetch_u64(function);
                self.stack.push_u64(v);
            }

            PUSH_I64 => {
                let v = self.fetch_i64(function);
                self.stack.push_i64(v);
            }


            PUSH_BOOL => {
                let b = self.fetch_u8(function) != 0;
                self.stack.push_bool(b);
            }

            PUSH_STR => {

                let s = self.fetch_string(13, function);
                let id = self.string_pool.intern(&s);
                self.stack.push_string(id);
            }

            POP => {
                self.stack.pop();
            }

            DUP => {
                let v = self.stack.peek().clone();
                self.stack.push(v.unwrap());
            }

            SWAP => {
                let v1 = self.stack.pop().unwrap();
                let v2 = self.stack.pop().unwrap();
                self.stack.push(v1);
                self.stack.push(v2);
            }

            RETURN => {
                return false;
            }

            HALT => {
                std::process::exit(0);
            }

            // Jumps
            JUMP => {
                let offset = self.fetch_i16(function) as usize;
                self.program_counter += offset;
            }

            JUMP_IF_TRUE => {
                let offset = self.fetch_i16(function) as usize;
                let condition = self.stack.pop_bool();
                if condition {
                    self.program_counter += offset;
                }
            }

            JUMP_IF_FALSE => {
                let offset = self.fetch_i16(function) as usize;
                let condition = self.stack.pop_bool();
                if !condition {
                    self.program_counter += offset;
                }
            }

            BRANCH => {
                todo!("BRANCH");
            }

            CALL => {
                self.instruction_counter += 1;
                let offset = self.fetch_u16(function);
                println!("Getting function name at offset {}", offset);

                let name = self.fetch_string(offset, function).trim().to_string();
                println!("Function name: {}", name);

                let arity = self.fetch_u8(function);            // Read arity
                println!("Function arity: {}", arity);

                // Pop arguments in reverse order (or leave on stack if callee expects to read)
                let mut args = Vec::with_capacity(arity as usize);
                for _ in 0..arity {
                    args.push(self.stack.pop());
                }
                args.reverse(); // preserve original order

                let func = {
                    self.function_module.lock().unwrap().get_function(2).cloned()
                };
                if let Some(func) = func {
                    self.run_function(func.id, Vec::new());
                } else {
                    eprintln!("Function {} not found", name);
                    std::process::exit(1);
                }
            }

            TAIL_CALL => {
                todo!("TAIL_CALL");
            }

            ASSIGN => {
                todo!("ASSIGN");
            }

            ADD_ASSIGN => {
                let rhs = self.stack.pop_int();
                let lhs = self.stack.pop_int();
                self.stack.push_int(lhs + rhs);
            }

            SUB_ASSIGN => {
                let rhs = self.stack.pop_int();
                let lhs = self.stack.pop_int();
                self.stack.push_int(lhs - rhs);
            }

            MUL_ASSIGN => {
                let rhs = self.stack.pop_int();
                let lhs = self.stack.pop_int();
                self.stack.push_int(lhs * rhs);
            }

            DIV_ASSIGN => {
                let rhs = self.stack.pop_int();
                let lhs = self.stack.pop_int();
                self.stack.push_int(lhs / rhs);
            }

            MOD_ASSIGN => {
                let rhs = self.stack.pop_int();
                let lhs = self.stack.pop_int();
                self.stack.push_int(lhs % rhs);
            }

            SHL_ASSIGN => {
                let rhs = self.stack.pop_int();
                let lhs = self.stack.pop_int();
                self.stack.push_int(lhs << rhs);
            }

            SHR_ASSIGN => {
                let rhs = self.stack.pop_int();
                let lhs = self.stack.pop_int();
                self.stack.push_int(lhs >> rhs);
            }

            BIT_AND_ASSIGN => {
                let rhs = self.stack.pop_int();
                let lhs = self.stack.pop_int();
                self.stack.push_int(lhs & rhs);
            }

            BIT_OR_ASSIGN => {
                let rhs = self.stack.pop_int();
                let lhs = self.stack.pop_int();
                self.stack.push_int(lhs | rhs);
            }

            BIT_XOR_ASSIGN => {
                let rhs = self.stack.pop_int();
                let lhs = self.stack.pop_int();
                self.stack.push_int(lhs ^ rhs);
            }

            LOAD => {
                todo!("LOAD");
            }

            STORE => {
                todo!("STORE");
            }

            LOAD_VAR => {
                todo!("LOAD_VAR");
            }

            STORE_VAR => {
                todo!("STORE_VAR");
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

    pub fn fetch_u8(&mut self, code: &mut Vec<u8>) -> u8 {
        let byte = code[self.instruction_counter];
        println!("Fetched u8: {}", byte);
        self.instruction_counter += 1;
        byte
    }

    pub fn fetch_u16(&mut self, code: &mut Vec<u8>) -> u16 {
        let bytes = &code[self.instruction_counter..self.instruction_counter + 2];
        self.instruction_counter += 2;
        u16::from_ne_bytes(bytes.try_into().unwrap())
    }

    pub fn fetch_u32(&mut self, code: &mut Vec<u8>) -> u32 {
        let bytes = &code[self.instruction_counter..self.instruction_counter + 4];
        self.instruction_counter += 4;
        u32::from_ne_bytes(bytes.try_into().unwrap())
    }

    pub fn fetch_u64(&mut self, code: &mut Vec<u8>) -> u64 {
        let bytes = &code[self.instruction_counter..self.instruction_counter + 8];
        self.instruction_counter += 8;
        u64::from_ne_bytes(bytes.try_into().unwrap())
    }

    pub fn fetch_i8(&mut self, code: &mut Vec<u8>) -> i8 {
        self.fetch_u8(code) as i8
    }

    pub fn fetch_i16(&mut self, code: &mut Vec<u8>) -> i16 {
        self.fetch_u16(code) as i16
    }

    pub fn fetch_i32(&mut self, code: &mut Vec<u8>) -> i32 {
        self.fetch_u32(code) as i32
    }

    pub fn fetch_i64(&mut self, code: &mut Vec<u8>) -> i64 {
        self.fetch_u64(code) as i64
    }

    pub fn fetch_f32(&mut self, code: &mut Vec<u8>) -> f32 {
        f32::from_bits(self.fetch_u32(code))
    }

    pub fn fetch_f64(&mut self, code: &mut Vec<u8>) -> f64 {
        f64::from_bits(self.fetch_u64(code))
    }

    /// Assumes string is encoded as a u16 length followed by that many bytes
    pub fn fetch_string(&mut self, len: u16, function: &mut Vec<u8>) -> String {
        println!("Fetching string of length {}", len);
        let mut result = Vec::with_capacity(len as usize);
        for _ in 0..len {
            let byte = self.fetch_u8(function);
            result.push(byte);
        }
        let s = String::from_utf8(result).expect("Invalid UTF-8 in string");
        println!("Fetched string: {}", s);
        s
    }
}
