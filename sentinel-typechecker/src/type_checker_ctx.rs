use std::collections::HashMap;

use ir::errors::reporter::ErrorReporter;
use ir::{ast::{ClassDecl, InterfaceDecl}, ssa_ir::Value};
use ir::hir::StrId;
use ir::ir_hasher::FxHashBuilder;

pub struct TypeCheckerCtx {
    var_map: HashMap<StrId, Value, FxHashBuilder>,
    classes: HashMap<StrId, ClassDecl, FxHashBuilder>,
    interfaces: HashMap<StrId, InterfaceDecl, FxHashBuilder>,
    pub error_reporter: ErrorReporter
}

impl TypeCheckerCtx {
    pub fn new() -> Self {
        TypeCheckerCtx {
            var_map: HashMap::with_hasher(FxHashBuilder),
            classes: HashMap::with_hasher(FxHashBuilder),
            interfaces: HashMap::with_hasher(FxHashBuilder),
            error_reporter: ErrorReporter::new()
        }
    }
}