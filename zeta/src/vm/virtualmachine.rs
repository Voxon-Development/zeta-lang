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
use crate::vm::string_pool::StringPool;

use mimalloc;

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
const BRANCH: u8 = Bytecode::Branch as u8;
const RETURN: u8 = Bytecode::Return as u8;
const HALT: u8 = Bytecode::Halt as u8;
const CALL: u8 = Bytecode::Call as u8;
const TAIL_CALL: u8 = Bytecode::TailCall as u8;
const CALL_NATIVE: u8 = Bytecode::CallNative as u8;
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
    pass_manager: pass_manager::PassManager,
    function_module: module::ZetaModule,
    event_loop: FiberScheduler,
    instruction_counter: usize,
    string_pool: StringPool
}

unsafe impl Send for VirtualMachine {}
unsafe impl Sync for VirtualMachine {}

impl VirtualMachine {
    #[inline]
    pub fn new(function_module: module::ZetaModule) -> VirtualMachine {
        VirtualMachine {
            stack: stack::BumpStack::new(1024),
            heap: heap::HeapMemory::new(),
            program_counter: 1024,
            variables: Trie::new(),
            profiler: profiler::Profiler::new(),
            pass_manager: pass_manager::PassManager::new(),
            function_module,
            event_loop: FiberScheduler::new(),
            instruction_counter: 0,
            string_pool: StringPool::new()
        }
    }

    /// Run a function but in a fiber
    pub fn run_function_in_fiber(&mut self, function_id: u64) {
        self.event_loop.spawn(Fiber::new(function_id, self.function_module.get_function(function_id).unwrap().clone()));
    }
    
    pub fn run_function(&mut self, function_id: u64) {
        /*println!("Running function {}", function_id);
        let (is_native, code, optimization_level) = {
            let function = match self.function_module.get_function_mut(function_id) {
                None => {
                    panic!("Tried to run a function that doesn't exist");
                }
                Some(function) => function,
            };
            let is_native = function.is_native;
            let code = &mut function.code;
            let optimization_level = function.optimization_level;
            (is_native, code, optimization_level)
        };

        if is_native {
            println!("Running native function");
            if optimization_level == OptimizationPassPriority::Max {
                println!("Running native function optimized to the max");
                functions::run_native_function(function); // optimized to the max
                return;
            }
            let now = std::time::Instant::now();
            functions::run_native_function(function);
            self.profiler.record_call(function_id, now.elapsed().as_nanos() as u64);
            return;
        }

        let stack_frame = StackFrame::new(0, 0, function_id);
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
