use std::collections::HashMap;
use crate::ast;
use crate::codegen::ir::optimization::pass::{Pass, OptimizationPassPriority};

pub struct PassManager {
    passes: HashMap<OptimizationPassPriority, Vec<Box<dyn Pass>>>
}

impl PassManager {
    pub fn new() -> PassManager {
        PassManager {
            passes: HashMap::new()
        }
    }
    
    pub fn add_pass(&mut self, priority: OptimizationPassPriority, pass: Box<dyn Pass>) {
        self.passes.entry(priority).or_insert(Vec::new()).push(pass);
    }
    
    pub fn run_passes(&mut self, ir: &mut Vec<ir::Bytecode>) -> anyhow::Result<()> {
        self.run_passes_with_priority(OptimizationPassPriority::Max, ir)?;
        self.run_passes_with_priority(OptimizationPassPriority::High, ir)?;
        self.run_passes_with_priority(OptimizationPassPriority::Medium, ir)?;
        self.run_passes_with_priority(OptimizationPassPriority::Low, ir)?;
        self.run_passes_with_priority(OptimizationPassPriority::Min, ir)?;
        
        Ok(())
    }
    
    pub fn run_passes_with_priority(&mut self, priority: OptimizationPassPriority, ir: &mut Vec<ir::Bytecode>) -> anyhow::Result<()> {
        // run based on priority
        if !self.passes.contains_key(&priority) {
            return Ok(());
        }
        
        for optimization_pass in self.passes.get_mut(&priority).unwrap() {
            optimization_pass.optimize(ir)?;
        }
        
        Ok(())
    }
}