use std::ops::{Deref, DerefMut};
use std::sync::Arc;
use enum_map::{enum_map, EnumMap};
use crate::codegen::ir::module::ZetaModule;
use crate::codegen::ir::optimization::pass::{Pass, OptimizationPassPriority};
use anyhow::{Result, Context};
use parking_lot::RwLock;
use rayon::prelude::*;
use trc::SharedTrc;
use ir::bump::{AtomicBump};

pub struct PassManager {
    passes: EnumMap<OptimizationPassPriority, Vec<SharedTrc<Box<dyn Pass>>, AtomicBump>>
}

impl PassManager {
    pub fn new() -> PassManager {
        PassManager {
            passes: 
                enum_map! {
                    OptimizationPassPriority::Max => Vec::new_in(AtomicBump::new()),
                    OptimizationPassPriority::High => Vec::new_in(AtomicBump::new()),
                    OptimizationPassPriority::Medium => Vec::new_in(AtomicBump::new()),
                    OptimizationPassPriority::Low => Vec::new_in(AtomicBump::new()),
                    OptimizationPassPriority::Min => Vec::new_in(AtomicBump::new()),
                    OptimizationPassPriority::None => Vec::new_in(AtomicBump::new()),
                    OptimizationPassPriority::Invisible => Vec::new_in(AtomicBump::new()),
                }
        }
    }
    
    pub fn add_pass<P: Pass + 'static>(&mut self, priority: OptimizationPassPriority, pass: P) {
        self.passes[priority].push(SharedTrc::new(pass.boxed()));
    }
    
    /// Run all passes in parallel where possible
    pub fn run_passes_parallel(&self, ir: &mut Vec<u8, AtomicBump>, module: &ZetaModule) -> Result<()> {
        // Create thread-safe references
        let ir_arc = Arc::new(RwLock::new(std::mem::replace(ir, Vec::new_in(AtomicBump::new()))));
        let module_arc = SharedTrc::new(module);
        
        // Execute each priority level in order, but run passes within each level in parallel
        for &priority in &[
            OptimizationPassPriority::Max,
            OptimizationPassPriority::High,
            OptimizationPassPriority::Medium,
            OptimizationPassPriority::Low,
            OptimizationPassPriority::Min,
        ] {
            self.run_parallel_passes(priority, ir_arc.clone(), module_arc.clone())?;
        }
        
        // Move the IR back
        *ir = Arc::try_unwrap(ir_arc)
            .map_err(|_| anyhow::anyhow!("Failed to unwrap IR after optimization"))?
            .into_inner();
            
        Ok(())
    }
    
    pub fn run_passes_parallel_with_priority(
        &self, 
        priority: OptimizationPassPriority, 
        ir: &mut Vec<u8, AtomicBump>, 
        module: &ZetaModule
    ) -> Result<()> {
        // Create thread-safe references
        let ir_arc = Arc::new(RwLock::new(std::mem::replace(ir, Vec::new_in(AtomicBump::new()))));
        let module_arc = SharedTrc::new(module);
        
        self.run_parallel_passes(priority, ir_arc, module_arc)
    }
    
    /// Run passes of a specific priority level in parallel
    fn run_parallel_passes(
        &self,
        priority: OptimizationPassPriority,
        ir_arc: Arc<RwLock<Vec<u8, AtomicBump>>>,
        module_arc: SharedTrc<&ZetaModule>,
    ) -> Result<()> {
        if self.passes[priority].is_empty() {
            return Ok(());
        }
        
        // Collect results to ensure we check all passes for errors
        let results: Vec<Result<()>> = self.passes[priority]
            .par_iter()
            .map(|pass| pass.clone())
            .map(|pass| {
                let mut ir = ir_arc.write();
                
                pass.optimize(ir.deref_mut(), module_arc.deref())
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
    pub fn run_passes(&mut self, ir: &mut Vec<u8, AtomicBump>, module: &ZetaModule) -> Result<()> {
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
        ir: &mut Vec<u8, AtomicBump>,
        module: &ZetaModule
    ) -> Result<()> {
        // run based on priority
        
        if self.passes[priority].is_empty() {
            return Ok(());
        }
        
        for optimization_pass in &self.passes[priority] {
            SharedTrc::clone(optimization_pass).optimize(ir, module)?;
        }
        
        Ok(())
    }
}