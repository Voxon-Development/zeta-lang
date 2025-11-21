use std::collections::{HashSet};
use std::path::PathBuf;
use ir::hir::StrId;
use ir::ir_hasher::FxHashBuilder;
use zetaruntime::bump::GrowableBump;

#[derive(Debug)]
pub struct SymbolsSoA<'bump> {
    pub names: Vec<StrId, GrowableBump<'bump>>,
    pub kinds: Vec<StrId, GrowableBump<'bump>>
}

impl<'bump> SymbolsSoA<'bump> {
    pub fn new() -> Self {
        let bump = GrowableBump::new(8 * 1024, 8);
        Self {
            names: Vec::new_in(bump.clone()),
            kinds: Vec::new_in(bump),
        }
    }
}

pub struct ModulesSoA<'bump> {
    pub names: Vec<StrId, GrowableBump<'bump>>,
    pub paths: Vec<PathBuf, GrowableBump<'bump>>,
    pub symbols: Vec<SymbolsSoA<'bump>, GrowableBump<'bump>>,
    pub deps: Vec<HashSet<usize, FxHashBuilder>, GrowableBump<'bump>>,
}

impl<'bump> ModulesSoA<'bump> {
    pub fn new() -> Self {
        let bump = GrowableBump::new(8 * 1024, 8);
        Self {
            names: Vec::new_in(bump.clone()),
            paths: Vec::new_in(bump.clone()),
            deps: Vec::new_in(bump.clone()),
            symbols: Vec::new_in(bump),
        }
    }


    pub fn push_module(
        &mut self,
        name: StrId,
        path: PathBuf,
        deps: HashSet<usize, FxHashBuilder>,
        symbols: SymbolsSoA<'bump>,
    ) -> usize {
        let idx = self.names.len();
        self.names.push(name);
        self.paths.push(path);
        self.deps.push(deps);
        self.symbols.push(symbols);
        idx
    }
}
