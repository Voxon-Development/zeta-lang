use std::cell::{Ref, RefCell, RefMut};
use std::rc::Rc;
use std::cmp::max;
use std::cmp::min;
use ir::ir_hasher::{FxHashBuilder, HashMap, HashSet};
use zetaruntime::bump::GrowableBump;
use ir::hir::*;

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

fn init_state(_graph: &CTRCGraph, _constraints: &[Constraint]) -> HashMap<ProgramPoint, SolverState> {
    HashMap::default()
}

/// Returns a deterministic, sorted list of program points.
///
/// This does **not** perform a graph-based topological sort.
/// It simply:
/// 1. Copies the input slice,
/// 2. Sorts the points in ascending order,
/// 3. Removes duplicates.
///
/// The solver relies on this to ensure stable iteration order.
pub(crate) fn dedup_and_sort_points(points: &[ProgramPoint]) -> Vec<ProgramPoint> {
    let mut v: Vec<ProgramPoint> = points.iter().copied().collect();
    v.sort_unstable();
    v.dedup();
    v
}

/// Applies a widening step to all solver states.
///
/// Widening prevents the analysis from oscillating or growing without bound:
/// - If an alias's upper bound exceeds `WIDEN_THRESHOLD`, it becomes `None`
///   (representing an unbounded range).
/// - Lower bounds above the threshold are clamped back down to the threshold.
/// - If `lower > upper` where `upper` is `Some(...)`, the lower bound is reduced
///   to maintain invariants.
///
/// This function guarantees monotonicity and enforces convergence in the
/// iterative dataflow solver.
pub(crate) fn widen_count_ranges(state: Rc<RefCell<HashMap<ProgramPoint, SolverState>>>) {
    const WIDEN_THRESHOLD: usize = 64;

    let mut binding: RefMut<HashMap<ProgramPoint, SolverState>> = state.borrow_mut();
    for solver_state in binding.values_mut() {
        for cr in solver_state.counts.values_mut() {
            // widen upper if above threshold
            if let Some(u) = cr.upper {
                if u > WIDEN_THRESHOLD {
                    cr.upper = None;
                }
            }

            // clamp lower
            if cr.lower > WIDEN_THRESHOLD {
                cr.lower = WIDEN_THRESHOLD;
            }

            // maintain invariant lower <= upper
            if let Some(u) = cr.upper {
                if cr.lower > u {
                    cr.lower = u;
                }
            }
        }
    }
}

/// Scans all solver states and computes three classification sets:
///
/// - `unbounded_aliases`: aliases with `upper == None` at *any* program point
/// - `zero_upper_aliases`: aliases with `upper == Some(0)` somewhere
/// - `maybe_leak_aliases`: aliases that appear both unbounded *and* have
///   a positive lower bound at some point
///
/// The classification is conservative: any occurrence at any program point
/// contributes to the classification.
///
/// This function currently only logs a summary. It does not mutate external
/// structures or return the computed sets. Extend or modify based on needs.
pub(crate) fn summarize_alias_classifications(
    state: Rc<RefCell<HashMap<ProgramPoint, SolverState>>>
) {
    let binding: Ref<HashMap<ProgramPoint, SolverState>> = state.borrow();

    let mut unbounded_aliases: std::collections::HashSet<AliasID, FxHashBuilder> = HashSet::default();
    let mut zero_upper_aliases: std::collections::HashSet<AliasID, FxHashBuilder> = HashSet::default();
    let mut positive_lower_aliases: std::collections::HashSet<AliasID, FxHashBuilder> = HashSet::default();

    for solver_state in binding.values() {
        for (alias, cr) in solver_state.counts.iter() {
            if cr.upper.is_none() { unbounded_aliases.insert(*alias); }

            if cr.upper == Some(0) { zero_upper_aliases.insert(*alias); }

            if cr.lower > 0 { positive_lower_aliases.insert(*alias); }
        }
    }

    let maybe_leak_aliases: Vec<AliasID> = positive_lower_aliases
        .intersection(&unbounded_aliases)
        .copied()
        .collect();

    /*println!(
        "alias classification: unbounded={}, zero_upper={}, maybe_leak={}",
        unbounded_aliases.len(),
        zero_upper_aliases.len(),
        maybe_leak_aliases.len()
    );*/
}

pub(crate) fn final_counts(state: Rc<RefCell<HashMap<ProgramPoint, SolverState>>>) -> HashMap<AliasID, CountRange> {
    let mut result: HashMap<AliasID, CountRange> = HashMap::default();
    for s in state.borrow().iter() {
        for (alias, cr) in &s.1.counts {
            result.insert(*alias, *cr);
        }
    }
    result
}

// Main solver
pub(crate)  fn solve_ctrc(graph: &CTRCGraph, constraints: &[Constraint], program_points: &[ProgramPoint]) -> HashMap<AliasID, CountRange> {
    let state: Rc<RefCell<HashMap<ProgramPoint, SolverState>>> = Rc::new(RefCell::new(init_state(graph, constraints)));

    loop {
        let mut changed = false;

        for pp in dedup_and_sort_points(program_points) {
            let new_counts = transfer(state.clone(), pp, constraints);

            {
                let mut binding = state.borrow_mut();
                if let Some(solver_state) = binding.get_mut(&pp) {
                    if solver_state.counts != new_counts {
                        solver_state.counts = new_counts;
                        changed = true;
                    }
                }
            } // and done
        }

        if !changed {
            break;
        }

        widen_count_ranges(state.clone());
    }

    summarize_alias_classifications(state.clone());
    final_counts(state)
}

// Transfer counts for a program point
pub(crate) fn transfer(state: Rc<RefCell<HashMap<ProgramPoint, SolverState>>>, pp: ProgramPoint, constraints: &[Constraint]) -> HashMap<AliasID, CountRange> {
    let mut new_counts: HashMap<AliasID, CountRange> = HashMap::default();

    for c in constraints.iter().filter(|c| c.point == pp) {
        let entry = new_counts.entry(c.alias).or_insert(CountRange { lower: 0, upper: Some(0) });
        adjust_upper_count(c, entry);
    }

    let immutable_binding = state.borrow();
    let Some(solver_state) = immutable_binding.get(&pp) else { return new_counts; };
    let Some(preds) = solver_state.predecessors.get(&pp) else { return new_counts; };

    for &pred in preds {
        let Some(pred_state) = immutable_binding.get(&pred) else { continue; };

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
    pub structs_with_destructors: HashSet<StrId>,
    pub droppable_fields: HashSet<(StrId, StrId)>, // (struct_name, field_name)
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