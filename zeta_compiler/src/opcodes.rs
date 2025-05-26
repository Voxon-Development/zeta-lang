enum OpCode {
    Const(i64),
    Add,
    Sub,
    Mul,
    Div,
    Store(String),
    Load(String),
    Call(String),
    Jump(usize),
    JumpIfFalse(usize),
}