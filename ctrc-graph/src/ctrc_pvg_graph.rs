use dashmap::DashMap;
use std::cmp::max;
use std::cmp::min;
use ir::ir_hasher::{FxHashBuilder, HashMap};
use zetaruntime::bump::GrowableBump;
use ir::hir::*;
use std::collections::HashSet;

pub type AliasID = usize;
pub type ProgramPoint = usize;
pub type NodeID = usize;
pub type EdgeID = usize;

#[derive(Debug, Clone, Copy)]
pub enum ValueKind {
    Alloc,
    Var,
    Temp,
    Param,
    Return,
}

#[derive(Debug, Clone, Copy)]
pub enum EdgeKind {
    Copy,
    Move,
    Borrow,
    Return,
    ParamPass,
}

#[derive(Debug, Clone, Copy)]
pub enum ConstraintReason { Copy, Drop, Move, Borrow, }

#[derive(Debug, Clone, Copy)]
#[derive(PartialEq)]
pub struct CountRange {
    pub(crate) lower: usize,
    pub(crate) upper: Option<usize>, // None represents infiinite
}

/// Pointer Value flow Graph Node
#[derive(Debug, Clone, Copy)]
pub struct PVGNode {
    pub(crate) id: usize,
    pub(crate) kind: ValueKind,
    pub(crate) alias: Option<AliasID>,
    pub(crate) name: Option<StrId>, // variable name if applicable
    pub(crate) program_point: ProgramPoint,
}

/// Pointer Value flow Graph Edge
#[derive(Debug, Clone, Copy)]
pub struct PVGEdge {
    pub(crate) src: NodeID,  // node id
    pub(crate) dst: NodeID,  // node id
    pub(crate) kind: EdgeKind,
}

#[derive(Debug, Clone, Copy)]
pub struct Constraint {
    pub(crate) alias: AliasID,
    pub(crate) point: ProgramPoint,
    pub(crate) delta: isize,  // +n/-n
    pub(crate) reason: ConstraintReason,
}

#[derive(Debug, Clone)]
pub struct AliasSet {
    pub(crate) id: AliasID,
    pub(crate) type_info: Option<HirType<'static, 'static>>,
    pub(crate) escape: bool,
    pub(crate) concurrent: bool,
    pub(crate) origin: Option<ProgramPoint>, // allocation site
}

#[derive(Debug, Clone)]
pub struct CTRCGraph {
    pub(crate) aliases: HashMap<AliasID, AliasSet>,
    pub(crate) pvg_nodes: Vec<PVGNode>,
    pub(crate) pvg_edges: Vec<PVGEdge>,
    pub(crate) constraints: Vec<Constraint>,
    pub(crate) solutions: HashMap<AliasID, CountRange>,
    pub(crate) next_alias_id: AliasID,
    pub(crate) next_node_id: NodeID,
    pub(crate) next_program_point: ProgramPoint,
    pub(crate) drop_points: HashMap<AliasID, Vec<ProgramPoint>>, // where to insert drops
}

pub struct SolverState {
    pub(crate) counts: HashMap<AliasID, CountRange>,
    pub(crate) predecessors: HashMap<ProgramPoint, Vec<ProgramPoint>>,
}

fn init_state(_graph: &CTRCGraph, _constraints: &[Constraint]) -> DashMap<ProgramPoint, SolverState> {
    DashMap::default()
}

pub(crate) fn topo_sort(_points: &[ProgramPoint]) -> Vec<ProgramPoint> { vec![] }

pub(crate) fn apply_widening(_state: &mut DashMap<ProgramPoint, SolverState>) {}

pub(crate) fn classify_aliases(_state: &DashMap<ProgramPoint, SolverState>) {}

pub(crate) fn final_counts(state: &DashMap<ProgramPoint, SolverState>) -> HashMap<AliasID, CountRange> {
    let mut result: HashMap<AliasID, CountRange> = HashMap::default();
    for s in state.iter() {
        for (alias, cr) in &s.counts {
            result.insert(*alias, *cr);
        }
    }
    result
}

// Main solver
pub(crate)  fn solve_ctrc(graph: &CTRCGraph, constraints: &[Constraint], program_points: &[ProgramPoint]) -> HashMap<AliasID, CountRange> {
    let mut state: DashMap<ProgramPoint, SolverState> = init_state(graph, constraints);

    loop {
        let mut changed = false;

        for &pp in &topo_sort(program_points) {
            if let Some(mut solver_state) = state.get_mut(&pp) {
                let old_counts: HashMap<AliasID, CountRange> = solver_state.counts.clone();
                solver_state.counts = transfer(&state, pp, constraints);

                if old_counts != solver_state.counts {
                    changed = true;
                }
            }
        }

        if !changed {
            break;
        }

        apply_widening(&mut state);
    }

    classify_aliases(&state);
    final_counts(&state)
}

// Transfer counts for a program point
pub(crate) fn transfer(state: &DashMap<ProgramPoint, SolverState>, pp: ProgramPoint, constraints: &[Constraint]) -> HashMap<AliasID, CountRange> {
    let mut new_counts: HashMap<AliasID, CountRange> = HashMap::default();

    for c in constraints.iter().filter(|c| c.point == pp) {
        let entry = new_counts.entry(c.alias).or_insert(CountRange { lower: 0, upper: Some(0) });
        adjust_upper_count(c, entry);
    }

    let Some(solver_state) = state.get(&pp) else { return new_counts; };
    let Some(preds) = solver_state.predecessors.get(&pp) else { return new_counts; };

    for &pred in preds {
        let Some(pred_state) = state.get(&pred) else { continue; };

        for (alias, cr) in &pred_state.counts {
            let entry = new_counts.entry(*alias).or_insert(*cr);
            entry.lower = min(entry.lower, cr.lower);
            entry.upper = match (entry.upper, cr.upper) {
                (Some(u1), Some(u2)) => Some(max(u1, u2)),
                _ => None,
            };
        }

    }

    new_counts
}

pub(crate) fn adjust_upper_count(c: &Constraint, entry: &mut CountRange) {
    match c.reason {
        ConstraintReason::Copy => {
            if let Some(u) = entry.upper { entry.upper = Some(u + 1); }
        }
        ConstraintReason::Drop => {
            entry.lower = entry.lower.saturating_sub(1);
            if let Some(u) = entry.upper { entry.upper = Some(u.saturating_sub(1)); }
        }
        ConstraintReason::Move => { /* ownership transferred, no count change */ }
        ConstraintReason::Borrow => { /* no count change, exclusivity checked elsewhere */ }
    }
}

#[derive(Debug, Clone)]
pub struct CTRCAnalysisResult {
    pub structs_with_destructors: HashSet<StrId, FxHashBuilder>,
    pub droppable_fields: HashSet<(StrId, StrId), FxHashBuilder>, // (struct_name, field_name)
    pub variable_aliases: HashMap<StrId, AliasID>,
    pub allocation_sites: HashMap<ProgramPoint, AliasID>,
    pub drop_insertions: Vec<DropInsertion>,
    pub destructor_calls: Vec<DestructorCall>,
    pub potential_leaks: Vec<AliasID>,
}

#[derive(Debug, Clone)]
pub struct DropInsertion {
    pub program_point: ProgramPoint,
    pub variable_name: StrId,
    pub alias_id: AliasID,
}

#[derive(Debug, Clone)]
pub struct DestructorCall {
    pub program_point: ProgramPoint,
    pub alias_id: AliasID,
    pub call_type: DestructorCallType,
}

#[derive(Debug, Clone)]
pub enum DestructorCallType {
    AutoDrop,
    ExplicitDrop,
    ScopeDrop,
}

// Public API
pub fn analyze_hir_for_ctrc<'bump>(
    module: &HirModule<'_, 'bump>,
    _bump: &'bump GrowableBump<'bump>
) -> CTRCAnalysisResult {
    let mut graph = CTRCGraph::new();
    graph.analyze_hir_module(module)
}

pub fn verify_hir_values_droppable<'bump>(
    module: &HirModule<'_, 'bump>,
    bump: &'bump GrowableBump<'bump>
) -> bool {
    let result = analyze_hir_for_ctrc(module, bump);
    !result.has_memory_safety_issues()
}