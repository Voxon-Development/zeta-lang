use enum_map::{enum_map, EnumMap};
use crate::codegen::ir::optimization::pass::{Pass, OptimizationPassPriority};

pub struct PassManager {
    passes: EnumMap<OptimizationPassPriority, Vec<Box<dyn Pass>>>
}

impl PassManager {
    pub fn new() -> PassManager {
        PassManager {
            passes: 
                enum_map! {
                    OptimizationPassPriority::Max => Vec::new(),
                    OptimizationPassPriority::High => Vec::new(),
                    OptimizationPassPriority::Medium => Vec::new(),
                    OptimizationPassPriority::Low => Vec::new(),
                    OptimizationPassPriority::Min => Vec::new(),
                }
        }
    }
    
    pub fn add_pass(&mut self, priority: OptimizationPassPriority, pass: Box<dyn Pass>) {
        self.passes[priority].push(pass);
    }
    
    pub fn run_passes(&mut self, ir: &mut Vec<u8>) -> anyhow::Result<()> {
        self.run_passes_with_priority(OptimizationPassPriority::Max, ir)?;
        self.run_passes_with_priority(OptimizationPassPriority::High, ir)?;
        self.run_passes_with_priority(OptimizationPassPriority::Medium, ir)?;
        self.run_passes_with_priority(OptimizationPassPriority::Low, ir)?;
        self.run_passes_with_priority(OptimizationPassPriority::Min, ir)?;
        
        Ok(())
    }
    
    pub fn run_passes_with_priority(&mut self, priority: OptimizationPassPriority, ir: &mut Vec<u8>) -> anyhow::Result<()> {
        // run based on priority
        
        if self.passes[priority].is_empty() {
            return Ok(());
        }
        
        for optimization_pass in self.passes[priority].iter_mut() {
            optimization_pass.optimize(ir)?;
        }
        
        Ok(())
    }
}