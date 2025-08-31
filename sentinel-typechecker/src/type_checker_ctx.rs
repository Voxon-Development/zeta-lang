use std::collections::HashMap;

use ir::errors::reporter::ErrorReporter;
use ir::{ast::{ClassDecl, InterfaceDecl}, ssa_ir::Value};

#[derive(Default)]
pub struct TypeCheckerCtx {
    var_map: HashMap<String, Value>,
    classes: HashMap<String, ClassDecl>,
    interfaces: HashMap<String, InterfaceDecl>,
    pub error_reporter: ErrorReporter
}

impl TypeCheckerCtx {
    pub fn new() -> Self {
        TypeCheckerCtx { var_map: HashMap::new(), classes: HashMap::new(), interfaces: HashMap::new(), error_reporter: ErrorReporter::new() }
    }
}