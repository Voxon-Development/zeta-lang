use radix_trie::Trie;
use ir::{VMValue, VmString};

#[derive(Debug, Clone)]
pub struct StackFrame {
    pub pc: usize,               // program counter for this frame
    pub return_address: usize,   // where to resume after return
    pub locals: Trie<String, VMValue>,
    pub local_count: usize,
    
    // The owning function of this stack frame
    pub function_id: u64
}

impl StackFrame {
    #[inline]
    pub fn new(pc: usize, return_address: usize, function_id: u64) -> StackFrame {
        StackFrame {
            pc,
            return_address,
            locals: Trie::new(),
            local_count: 0,
            function_id
        }
    }

    pub fn push(&mut self, name: String, value: VMValue) {
        self.locals.insert(name, value);
        self.local_count += 1;
    }
    
    pub fn get(&self, name: &str) -> Option<&VMValue> {
        self.locals.get(name)
    }
}

#[derive(Debug, Clone, Copy, Default)]
pub struct OptimizationFrame {
    pub function_id: u64,
    pub call_count: u64,
}
