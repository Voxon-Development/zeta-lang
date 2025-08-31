use crate::pass::{OptimizationPass, PassManager as Pipeline};
use ir::ssa_ir::Module;

pub struct PassManager {
    pipeline: Pipeline,
}

impl PassManager {
    pub fn new_default() -> Self { Self { pipeline: Pipeline::with_default_pipeline() } }
    pub fn with(passes: Vec<OptimizationPass>) -> Self {
        let mut p = Pipeline::new();
        for pass in passes { p.add_pass(pass); }
        Self { pipeline: p }
    }

    pub fn run(&self, module: &mut Module) -> usize { self.pipeline.run(module) }
}