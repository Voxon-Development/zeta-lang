use std::collections::HashSet;
use ir::hir::StrId;
use ir::ir_hasher::{FxHashBuilder, HashMap};
use crate::CTRCAnalysisResult;

impl CTRCAnalysisResult {
    pub fn new() -> Self {
        Self {
            structs_with_destructors: HashSet::with_hasher(FxHashBuilder),
            droppable_fields: HashSet::with_hasher(FxHashBuilder),
            variable_aliases: HashMap::default(),
            allocation_sites: HashMap::default(),
            drop_insertions: Vec::new(),
            destructor_calls: Vec::new(),
            potential_leaks: Vec::new(),
        }
    }

    pub fn has_memory_safety_issues(&self) -> bool {
        !self.potential_leaks.is_empty()
    }

    pub fn get_drop_points_for_variable(&self, var_name: StrId) -> Vec<crate::ctrc_pvg_graph::ProgramPoint> {
        self.drop_insertions
            .iter()
            .filter(|drop| drop.variable_name == var_name)
            .map(|drop| drop.program_point)
            .collect()
    }
}