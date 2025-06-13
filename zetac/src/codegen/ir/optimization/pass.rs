use enum_map::Enum;

pub trait Pass: Send + Sync {
    fn optimize(&mut self, stmts: &mut Vec<u8>) -> anyhow::Result<()>;

    fn priority(&self) -> OptimizationPassPriority;
}

#[derive(Copy, Clone, Hash, Eq, PartialEq, Debug, Enum)]
pub enum OptimizationPassPriority {
    Max,
    High,
    Medium,
    Low,
    Min,
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

impl From<u8> for OptimizationPassPriority {
    fn from(value: u8) -> Self {
        match value {
            0 => OptimizationPassPriority::Max,
            1 => OptimizationPassPriority::High,
            2 => OptimizationPassPriority::Medium,
            3 => OptimizationPassPriority::Low,
            _ => OptimizationPassPriority::Min,
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
    fn optimize(&mut self, bytecode: &mut Vec<u8>) -> anyhow::Result<()> {
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
    fn optimize(&mut self, bytecode: &mut Vec<u8>) -> anyhow::Result<()> {
        todo!()
    }

    fn priority(&self) -> OptimizationPassPriority {
        OptimizationPassPriority::Min
    }
}

pub struct InliningPass;

impl Pass for InliningPass {
    fn optimize(&mut self, bytecode: &mut Vec<u8>) -> anyhow::Result<()> {
        todo!()
    }

    fn priority(&self) -> OptimizationPassPriority {
        OptimizationPassPriority::Max
    }
}