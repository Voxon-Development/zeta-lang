use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum TypeError {
    UndefinedVariable(String),
    UndefinedType(String),
    UndefinedFunction(String),
    TypeMismatch { expected: String, found: String },
    InvalidBinaryOp { op: String, left: String, right: String },
    InvalidUnaryOp { op: String, operand: String },
    FieldNotFound { struct_name: String, field: String },
    MethodNotFound { struct_name: String, method: String },
    InvalidFunctionCall { expected_args: usize, found_args: usize },
    InvalidReturnType { expected: String, found: String },
    BreakOutsideLoop,
    ContinueOutsideLoop,
    Generic(String),
}

impl fmt::Display for TypeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TypeError::UndefinedVariable(name) => write!(f, "Undefined variable: {}", name),
            TypeError::UndefinedType(name) => write!(f, "Undefined type: {}", name),
            TypeError::UndefinedFunction(name) => write!(f, "Undefined function: {}", name),
            TypeError::TypeMismatch { expected, found } => {
                write!(f, "Type mismatch: expected {}, found {}", expected, found)
            }
            TypeError::InvalidBinaryOp { op, left, right } => {
                write!(f, "Invalid binary operation: {} {} {}", left, op, right)
            }
            TypeError::InvalidUnaryOp { op, operand } => {
                write!(f, "Invalid unary operation: {}{}", op, operand)
            }
            TypeError::FieldNotFound { struct_name, field } => {
                write!(f, "Field '{}' not found in struct '{}'", field, struct_name)
            }
            TypeError::MethodNotFound { struct_name, method } => {
                write!(f, "Method '{}' not found in struct '{}'", method, struct_name)
            }
            TypeError::InvalidFunctionCall { expected_args, found_args } => {
                write!(f, "Invalid function call: expected {} arguments, found {}", expected_args, found_args)
            }
            TypeError::InvalidReturnType { expected, found } => {
                write!(f, "Invalid return type: expected {}, found {}", expected, found)
            }
            TypeError::BreakOutsideLoop => write!(f, "Break statement outside loop"),
            TypeError::ContinueOutsideLoop => write!(f, "Continue statement outside loop"),
            TypeError::Generic(msg) => write!(f, "{}", msg),
        }
    }
}

impl std::error::Error for TypeError {}

pub type TypeCheckResult<T> = Result<T, TypeError>;
