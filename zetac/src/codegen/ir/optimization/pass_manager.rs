use std::collections::HashMap;
use crate::ast;
use crate::codegen::ir::optimization::pass::{Pass, PassPriority};

pub struct PassManager {
    passes: HashMap<PassPriority, Vec<Box<dyn Pass>>>
}

impl PassManager {
    pub fn new() -> PassManager {
        PassManager {
            passes: HashMap::new()
        }
    }
    
    pub fn add_pass(&mut self, priority: PassPriority, pass: Box<dyn Pass>) {
        self.passes.entry(priority).or_insert(Vec::new()).push(pass);
    }
    
    pub fn run_passes(&mut self, ir: &mut Vec<ast::Stmt>) -> anyhow::Result<()> {
        // run based on priority
        for passes in self.passes.get_mut(&PassPriority::Highest).unwrap() {
            passes.optimize(ir)?;
        }
        
        for passes in self.passes.get_mut(&PassPriority::High).unwrap() {
            passes.optimize(ir)?;
        }
        
        for passes in self.passes.get_mut(&PassPriority::Normal).unwrap() {
            passes.optimize(ir)?;
        }
        
        for passes in self.passes.get_mut(&PassPriority::Low).unwrap() {
            passes.optimize(ir)?;
        }
        
        for passes in self.passes.get_mut(&PassPriority::Lowest).unwrap() {
            passes.optimize(ir)?;
        }
        
        Ok(())
    }
}