use crate::event_loop;

pub struct StackFrame {
    pub offset: usize   
}

pub struct CallFrame {
    pub return_address: usize,
    pub locals: Vec<Value>,
    pub task: event_loop::Task
}

#[derive(Clone, Debug)]
pub enum Value {
    Int(i32),
    UInt(u32),
    Long(i64),
    ULong(u64),
    Double(f64),
    Float(f32),
    Function(u32)
}