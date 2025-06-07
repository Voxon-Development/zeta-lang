pub trait Pass {
    fn optimize(&mut self, stmts: &mut Vec<ir::Bytecode>) -> anyhow::Result<()>;

    fn priority(&self) -> OptimizationPassPriority;
}

impl From<u16> for OptimizationPassPriority {
    fn from(value: u16) -> Self {
        match value {
            5 => OptimizationPassPriority::Max,
            4 => OptimizationPassPriority::High,
            3 => OptimizationPassPriority::Medium,
            2 => OptimizationPassPriority::Low,
            1 => OptimizationPassPriority::Min,
            _ => OptimizationPassPriority::Medium
        }
    }
}

impl From<OptimizationPassPriority> for u16 {
    fn from(value: OptimizationPassPriority) -> Self {
        value as u16
    }
}

#[derive(Copy, Clone, Hash, Eq, PartialEq)]
pub enum OptimizationPassPriority {
    Max = 5,
    High = 4,
    Medium = 3,
    Low = 2,
    Min = 1
}

impl OptimizationPassPriority {
    pub fn to_string(&self) -> String {
        match self {
            OptimizationPassPriority::Max => "Max".to_string(),
            OptimizationPassPriority::High => "High".to_string(),
            OptimizationPassPriority::Medium => "Normal".to_string(),
            OptimizationPassPriority::Low => "Low".to_string(),
            OptimizationPassPriority::Min => "Min".to_string(),
        }
    }
}

impl Default for OptimizationPassPriority {
    fn default() -> Self {
        OptimizationPassPriority::Medium
    }
}

pub struct ConstantFoldingPass;

impl Pass for ConstantFoldingPass {
    fn optimize(&mut self, bytecode: &mut Vec<ir::Bytecode>) -> anyhow::Result<()> {
         for stmt in bytecode.iter_mut() {
             // TODO: check the bytecode, and if it's a math operation with two constants then let's replace it with a constant
             
         }
        todo!()
    }

    fn priority(&self) -> OptimizationPassPriority {
        OptimizationPassPriority::Min
    }
}

pub struct DeadCodeEliminationPass;

impl Pass for DeadCodeEliminationPass {
    fn optimize(&mut self, bytecode: &mut Vec<ir::Bytecode>) -> anyhow::Result<()> {
        todo!()
    }

    fn priority(&self) -> OptimizationPassPriority {
        OptimizationPassPriority::Min
    }
}

pub struct InliningPass;

impl Pass for InliningPass {
    fn optimize(&mut self, bytecode: &mut Vec<ir::Bytecode>) -> anyhow::Result<()> {
        todo!()
    }

    fn priority(&self) -> OptimizationPassPriority {
        OptimizationPassPriority::Max
    }
}