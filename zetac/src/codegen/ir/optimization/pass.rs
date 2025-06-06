use crate::ast;

pub trait Pass {
    fn optimize(&mut self, stmts: &mut Vec<ast::Stmt>) -> anyhow::Result<()>;

    fn priority(&self) -> u16;
}

impl From<u16> for PassPriority {
    fn from(value: u16) -> Self {
        match value {
            5 => PassPriority::Highest,
            4 => PassPriority::High,
            3 => PassPriority::Normal,
            2 => PassPriority::Low,
            1 => PassPriority::Lowest,
            _ => PassPriority::Normal
        }
    }
}

impl From<PassPriority> for u16 {
    fn from(value: PassPriority) -> Self {
        value as u16
    }
}

#[derive(Copy, Clone, Hash, Eq, PartialEq)]
pub enum PassPriority {
    Highest = 5,
    High = 4,
    Normal = 3,
    Low = 2,
    Lowest = 1
}