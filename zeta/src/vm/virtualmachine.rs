use std::collections::HashMap;

use ir::Bytecode;
use crate::vm::profiler;

pub struct VirtualMachine {
    program_counter: usize,
    bytecode: Vec<Bytecode>,
    variables: HashMap<String, i64>,
    profiler: profiler::Profiler
}

impl VirtualMachine {
    pub fn new(bytecode: Vec<Bytecode>) -> VirtualMachine {
        VirtualMachine {
            program_counter: 1024,
            bytecode,
            variables: HashMap::new(),
            profiler: profiler::Profiler::new()
        }
    }

    pub fn run_function(&self,
                        function_id: u32,
                        execution_context: u32) {
        //let function_id = task.function_id;
        //let execution_context = task.execution_context;


        todo!()
    }

    pub fn interpret_function(&self,
                              function_id: u32,
                              execution_context: u32) {
        todo!()
    }

    pub fn compile_function(&self,
                            function_id: u32,
                            execution_context: u32) {
        todo!()
    }

    pub fn optimize_function(&self,
                             function_id: u32,
                             execution_context: u32) {
        todo!()
    }

    pub fn run_native_function(&self,
                               function_id: u32,
                               execution_context: u32) {
        todo!()
    }

    pub fn run(&self) {
        todo!()
    }
}