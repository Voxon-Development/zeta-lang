use std::collections::HashMap;
use ahash::RandomState;
use radix_trie::Trie;
use ir::{VMValue, VmString};
use crate::vm::memory::regions::Region;
use crate::vm::memory::stack::BumpStack;

#[derive(Debug, Clone, Default)]
pub struct StackFrame {
    pub program_counter: usize,  // program counter for this frame
    pub return_address: usize,   // where to resume after return
    pub locals: Trie<String, VMValue>,
    pub regions: HashMap<VmString, Region, RandomState>,
    pub local_count: usize,
    pub stack: BumpStack,
    
    // The owning function of this stack frame
    pub function_id: u64,
    
    // Current opcode being executed (used by instruction handlers)
    pub current_opcode: u8,
}

unsafe impl Send for StackFrame {}
unsafe impl Sync for StackFrame {}

impl StackFrame {
    #[inline]
    pub fn new(pc: usize, return_address: usize, function_id: u64) -> StackFrame {
        StackFrame {
            program_counter: pc,
            return_address,
            locals: Trie::new(),
            regions: HashMap::default(),
            local_count: 0,
            stack: BumpStack::new(2048),
            function_id,
            current_opcode: 0,
        }
    }
    
    /// Create a new stack frame with default values for testing
    #[cfg(test)]
    pub fn new_test() -> Self {
        Self::new(0, 0, 0)
    }

    pub fn push(&mut self, name: String, value: VMValue) {
        self.locals.insert(name, value);
        self.local_count += 1;
    }
    
    pub fn push_region(&mut self, vm_string: VmString, region: Region) {
        self.regions.insert(vm_string, region);
    }
    
    pub fn get(&self, name: &str) -> Option<&VMValue> {
        self.locals.get(name)
    }
    
    #[inline(always)]
    pub fn get_class(&self, id: usize) -> Option<&ir::Class> {
        self.stack.get_class(id)
    }
    
    pub fn reset(&mut self) {
        self.locals = Trie::new();
        self.regions.clear();
        self.program_counter = 0;
        self.return_address = 0;
        self.local_count = 0;
        self.stack.reset();
    }
}

#[derive(Debug, Clone, Copy, Default)]
pub struct OptimizationFrame {
    pub function_id: u64,
    pub call_count: u64,
}
