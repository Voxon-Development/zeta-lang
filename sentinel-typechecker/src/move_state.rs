use ir::{hir::StrId, ir_hasher::FxHashBuilder};
use std::collections::{HashMap, HashSet};

#[derive(Debug, Clone, Default)]
pub struct LocalMoveState {
    pub whole_moved: bool,
    pub moved_fields: HashSet<StrId>,
}

#[derive(Debug, Clone, Default)]
pub struct MoveState {
    locals: HashMap<StrId, LocalMoveState, FxHashBuilder>,
}

impl MoveState {
    pub fn new() -> Self {
        Self {
            locals: HashMap::default(),
        }
    }

    pub fn is_whole_moved(&self, name: StrId) -> bool {
        self.locals.get(&name).map_or(false, |l| l.whole_moved)
    }

    pub fn blocks_whole_use(&self, name: StrId) -> bool {
        self.locals
            .get(&name)
            .map_or(false, |l| l.whole_moved || !l.moved_fields.is_empty())
    }

    pub fn is_field_moved(&self, name: StrId, field: StrId) -> bool {
        self.locals
            .get(&name)
            .map_or(false, |l| l.whole_moved || l.moved_fields.contains(&field))
    }

    pub fn mark_whole_moved(&mut self, name: StrId) {
        self.locals.entry(name).or_default().whole_moved = true;
    }

    pub fn mark_field_moved(&mut self, name: StrId, field: StrId) {
        self.locals
            .entry(name)
            .or_default()
            .moved_fields
            .insert(field);
    }

    pub fn clear(&mut self, name: StrId) {
        self.locals.remove(&name);
    }

    /// Join two states from divergent control-flow paths.
    /// Union of everything moved on *either* path: a place moved on one
    /// branch but not the other becomes "moved" in the merged state
    pub fn join(a: &MoveState, b: &MoveState) -> MoveState {
        let mut result = a.clone();
        for (name, b_local) in &b.locals {
            let entry = result.locals.entry(*name).or_default();
            entry.whole_moved |= b_local.whole_moved;
            entry
                .moved_fields
                .extend(b_local.moved_fields.iter().copied());
        }
        result
    }

    /// True if `join(self, other)` would change nothing and is used to detect
    /// fixpoint convergence around loop back-edges
    pub fn is_superset_of(&self, other: &MoveState) -> bool {
        other.locals.iter().all(|(name, o)| {
            self.locals
                .get(name)
                .map_or(o.whole_moved == false && o.moved_fields.is_empty(), |s| {
                    (s.whole_moved || !o.whole_moved) && o.moved_fields.is_subset(&s.moved_fields)
                })
        })
    }
}
