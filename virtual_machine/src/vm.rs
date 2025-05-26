use std::collections::HashMap;
use crate::{bump, event_loop};

pub enum OpCode {
    
}

pub struct VirtualMachine {
    stack: bump::BumpStack,
    program_counter: usize,
    bytecode: Vec<OpCode>,
    variables: HashMap<String, i64>,
    profiler: Profiler
}

impl VirtualMachine {
    pub fn new(bytecode: Vec<OpCode>) -> VirtualMachine {
        VirtualMachine {
            stack: bump::BumpStack::new(1024),
            program_counter: 1024,
            bytecode,
            variables: HashMap::new(),
            profiler: Profiler { call_counts: Vec::new() }
        }
    }
    
    pub fn run_function(&self, task: event_loop::Task) {
        //let function_id = task.function_id;
        //let execution_context = task.execution_context;
        
        
        todo!()
    }
}

struct Profiler {
    call_counts: Vec<u64>
}