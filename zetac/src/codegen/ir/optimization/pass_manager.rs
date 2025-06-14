use std::sync::{Arc, Mutex, RwLock};
use std::thread;
use enum_map::{enum_map, EnumMap};
use crate::codegen::ir::module::ZetaModule;
use crate::codegen::ir::optimization::pass::{Pass, OptimizationPassPriority};
use anyhow::{Result, Context};
use rayon::prelude::*;

pub struct PassManager {
    passes: EnumMap<OptimizationPassPriority, Vec<Arc<Box<dyn Pass>>>>
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
    
    pub fn add_pass<P: Pass + 'static>(&mut self, priority: OptimizationPassPriority, pass: P) {
        self.passes[priority].push(Arc::from(pass.boxed()));
    }
    
    pub fn add_arc_pass(&mut self, priority: OptimizationPassPriority, pass: Arc<Box<dyn Pass>>) {
        self.passes[priority].push(pass);
    }
    
    /// Run all passes in parallel where possible
    pub fn run_passes_parallel(&self, ir: &mut Vec<u8>, module: &ZetaModule) -> Result<()> {
        // Create thread-safe references
        let ir_arc = Arc::new(RwLock::new(std::mem::take(ir)));
        let module_arc = Arc::new(module);
        
        // Execute each priority level in order, but run passes within each level in parallel
        for &priority in &[
            OptimizationPassPriority::Max,
            OptimizationPassPriority::High,
            OptimizationPassPriority::Medium,
            OptimizationPassPriority::Low,
            OptimizationPassPriority::Min,
        ] {
            self.run_parallel_passes(priority, &ir_arc, &module_arc)?;
        }
        
        // Move the IR back
        *ir = Arc::try_unwrap(ir_arc)
            .map_err(|_| anyhow::anyhow!("Failed to unwrap IR after optimization"))?
            .into_inner()
            .map_err(|_| anyhow::anyhow!("Failed to acquire lock on IR"))?;
            
        Ok(())
    }
    
    /// Run passes of a specific priority level in parallel
    fn run_parallel_passes(
        &self,
        priority: OptimizationPassPriority,
        ir_arc: &Arc<RwLock<Vec<u8>>>,
        module_arc: &Arc<&ZetaModule>,
    ) -> Result<()> {
        if self.passes[priority].is_empty() {
            return Ok(());
        }
        
        // Collect results to ensure we check all passes for errors
        let results: Vec<Result<()>> = self.passes[priority]
            .par_iter()
            .map(|pass| {
                let mut ir = ir_arc.write()
                    .map_err(|e| anyhow::anyhow!("Failed to acquire write lock on IR: {}", e))?;
                
                pass.optimize(&mut ir, module_arc.as_ref())
                    .with_context(|| format!("Error in pass with priority {:?}", priority))
            })
            .collect();
            
        // Check for any errors
        for result in results {
            result?;
        }
        
        Ok(())
    }
    
    /// Run all passes sequentially
    pub fn run_passes(&mut self, ir: &mut Vec<u8>, module: &ZetaModule) -> Result<()> {
        self.run_passes_with_priority(OptimizationPassPriority::Max, ir, module)?;
        self.run_passes_with_priority(OptimizationPassPriority::High, ir, module)?;
        self.run_passes_with_priority(OptimizationPassPriority::Medium, ir, module)?;
        self.run_passes_with_priority(OptimizationPassPriority::Low, ir, module)?;
        self.run_passes_with_priority(OptimizationPassPriority::Min, ir, module)?;
        
        Ok(())
    }
    
    pub fn run_passes_with_priority(
        &mut self, 
        priority: OptimizationPassPriority, 
        ir: &mut Vec<u8>,
        module: &ZetaModule
    ) -> Result<()> {
        // run based on priority
        
        if self.passes[priority].is_empty() {
            return Ok(());
        }
        
        for optimization_pass in &self.passes[priority] {
            Arc::clone(optimization_pass).optimize(ir, module)?;
        }
        
        Ok(())
    }
}