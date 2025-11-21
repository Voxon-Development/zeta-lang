
use std::collections::HashSet;
use std::path::PathBuf;
use std::sync::Arc;
use ir::hir::{Hir, HirModule, StrId};
use ir::ir_hasher::FxHashBuilder;
use zetaruntime::string_pool::StringPool;
use crate::symbol_table::{ModulesSoA, SymbolsSoA};

pub struct ModuleBuilder<'bump> {
    pub modules: ModulesSoA<'bump>,
}

impl<'bump> ModuleBuilder<'bump> {
    pub fn run(hir_modules: &Vec<HirModule>, string_pool: Arc<StringPool>) -> Self {
        let mut modules = ModulesSoA::new();

        for module in hir_modules {
            let mut symbols = SymbolsSoA::new();

            for item in module.items {
                let (name, kind) = extract_symbol_info(item, string_pool.clone());
                symbols.names.push(name);
                symbols.kinds.push(kind);
            }

            let path = PathBuf::from(format!("{}.zeta", module.name));
            let deps: HashSet<usize, FxHashBuilder> = HashSet::with_hasher(FxHashBuilder::default());
            modules.push_module(module.name, path, deps, symbols);
        }

        Self { modules }
    }
}


fn extract_symbol_info(hir: &Hir, string_pool: Arc<StringPool>) -> (StrId, StrId) {
    match hir {
        Hir::Func(f) => (f.name, StrId(string_pool.intern("function"))),
        Hir::Struct(s) => (s.name, StrId(string_pool.intern("struct"))),
        Hir::Const(c) => (c.name, StrId(string_pool.intern("const"))),
        _ => (StrId(string_pool.intern("<anon>")), StrId(string_pool.intern("unknown"))),
    }
}
