use crate::span::SourceSpan;
use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum TypeErrorKind {
    UndefinedVariable(String),
    UndefinedType(String),
    UndefinedFunction(String),
    TypeMismatch {
        expected: String,
        found: String,
    },
    InvalidBinaryOp {
        op: String,
        left: String,
        right: String,
    },
    InvalidUnaryOp {
        op: String,
        operand: String,
    },
    FieldNotFound {
        struct_name: String,
        field: String,
    },
    MethodNotFound {
        struct_name: String,
        method: String,
    },
    InvalidFunctionCall {
        expected_args: usize,
        found_args: usize,
    },
    InvalidReturnType {
        expected: String,
        found: String,
    },
    BreakOutsideLoop,
    ContinueOutsideLoop,
    Generic(String),
    UndefinedFunctionWithSuggestion {
        name: String,
        suggested_modules: Vec<String>,
    },
    VariableAlreadyExists {
        var_name: String,
    },
    IllegalThisParam {
        func_name: String,
    },
}

impl TypeErrorKind {
    pub fn at(self, span: SourceSpan<'_>) -> TypeError<'_> {
        TypeError { kind: self, span }
    }
}

impl fmt::Display for TypeErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TypeErrorKind::UndefinedVariable(name) => write!(f, "Undefined variable: {}", name),
            TypeErrorKind::UndefinedType(name) => write!(f, "Undefined type: {}", name),
            TypeErrorKind::UndefinedFunction(name) => write!(f, "Undefined function: {}", name),
            TypeErrorKind::TypeMismatch { expected, found } => {
                write!(f, "Type mismatch: expected {}, found {}", expected, found)
            }
            TypeErrorKind::InvalidBinaryOp { op, left, right } => {
                write!(f, "Invalid binary operation: {} {} {}", left, op, right)
            }
            TypeErrorKind::InvalidUnaryOp { op, operand } => {
                write!(f, "Invalid unary operation: {}{}", op, operand)
            }
            TypeErrorKind::FieldNotFound { struct_name, field } => {
                write!(f, "Field '{}' not found in struct '{}'", field, struct_name)
            }
            TypeErrorKind::MethodNotFound {
                struct_name,
                method,
            } => {
                write!(
                    f,
                    "Method '{}' not found in struct '{}'",
                    method, struct_name
                )
            }
            TypeErrorKind::InvalidFunctionCall {
                expected_args,
                found_args,
            } => {
                write!(
                    f,
                    "Invalid function call: expected {} arguments, found {}",
                    expected_args, found_args
                )
            }
            TypeErrorKind::InvalidReturnType { expected, found } => {
                write!(
                    f,
                    "Invalid return type: expected {}, found {}",
                    expected, found
                )
            }
            TypeErrorKind::BreakOutsideLoop => write!(f, "Break statement outside loop"),
            TypeErrorKind::ContinueOutsideLoop => write!(f, "Continue statement outside loop"),
            TypeErrorKind::Generic(msg) => write!(f, "{}", msg),
            TypeErrorKind::UndefinedFunctionWithSuggestion {
                name,
                suggested_modules,
            } => {
                if suggested_modules.len() == 1 {
                    write!(
                        f,
                        "Undefined function: {} (a function with this name exists in module `{}`, did you mean to import it, or use `{}.{}`?)",
                        name,
                        suggested_modules[0],
                        suggested_modules[0],
                        name.rsplit("::").next().unwrap_or(name)
                    )
                } else {
                    write!(
                        f,
                        "Undefined function: {} (a function with this name exists in modules: {})",
                        name,
                        suggested_modules.join(", ")
                    )
                }
            }
            TypeErrorKind::VariableAlreadyExists { var_name } => {
                write!(f, "Variable already exists: {}", var_name)
            }
            TypeErrorKind::IllegalThisParam { func_name } => write!(
                f,
                "Illegal this parameter: free function {} contains `this`",
                func_name
            ),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeError<'a> {
    pub kind: TypeErrorKind,
    pub span: SourceSpan<'a>,
}

impl<'a> TypeError<'a> {
    pub fn new(kind: TypeErrorKind, span: SourceSpan<'a>) -> Self {
        Self { kind, span }
    }

    pub fn has_known_span(&self) -> bool {
        self.span != SourceSpan::default()
    }
}

impl<'a> fmt::Display for TypeError<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.has_known_span() {
            write!(f, "{} (at {})", self.kind, self.span)
        } else {
            write!(f, "{}", self.kind)
        }
    }
}

impl<'a> std::error::Error for TypeError<'a> {}

pub type TypeCheckResult<'a, T> = Result<T, TypeError<'a>>;
