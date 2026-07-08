use crate::dep_graph::AstModule;
use crate::symbol_table::{ModulesSoA, SymbolsSoA};
use ir::ast::Stmt;
use ir::hir::StrId;
use ir::ir_hasher::FxHashBuilder;
use std::collections::HashSet;
use std::path::PathBuf;
use std::sync::Arc;
use zetaruntime::string_pool::StringPool;

pub struct ModuleBuilder<'bump> {
    pub modules: ModulesSoA<'bump>,
}

impl<'bump> ModuleBuilder<'bump> {
    pub fn run<'a>(ast_modules: &[AstModule<'a, '_>], string_pool: Arc<StringPool>) -> Self {
        let mut modules = ModulesSoA::new();

        for module in ast_modules {
            let mut symbols = SymbolsSoA::new();

            for stmt in module.stmts {
                let (name, kind) = extract_symbol_info(stmt, &string_pool);
                symbols.names.push(name);
                symbols.kinds.push(kind);
            }

            let path = PathBuf::from(format!("{}.zeta", string_pool.resolve_string(&module.name)));
            let deps: HashSet<usize, FxHashBuilder> =
                HashSet::with_hasher(FxHashBuilder::default());

            modules.push_module(module.name, path, deps, symbols);
        }

        Self { modules }
    }
}

fn extract_symbol_info(stmt: &Stmt<'_, '_>, pool: &StringPool) -> (StrId, StrId) {
    let kind = |s: &str| StrId(pool.intern(s));

    match stmt {
        Stmt::FuncDecl(f) => (f.name, kind("function")),
        Stmt::StructDecl(s) => (s.name, kind("struct")),
        Stmt::Const(c) => (c.ident, kind("const")),
        Stmt::InterfaceDecl(i) => (i.name, kind("interface")),
        Stmt::EnumDecl(e) => (e.name, kind("enum")),
        Stmt::ImplDecl(i) => (i.target, kind("impl")),
        _ => (StrId(pool.intern("<anon>")), kind("unknown")),
    }
}
