use crate::hir::{HirErrorHandlerPattern, HirExpr, HirStmt};
use crate::ir_hasher::FxHashMap;

#[derive(Copy, Clone, Debug, Default, Eq, PartialEq, Hash, PartialOrd, Ord)]
pub struct PointId(pub u32);

#[derive(Default)]
pub struct Cfg {
    pub entry: Option<PointId>,
    /// Points at which control leaves the function normally (an explicit
    /// `return`, or falling off the end of the body). Unreachable tails
    /// (dead code after a diverging statement) are neither exits nor wired
    /// into the reachable graph.
    pub exits: Vec<PointId>,
    pub successors: FxHashMap<PointId, Vec<PointId>>,
    pub predecessors: FxHashMap<PointId, Vec<PointId>>,
    next: u32,
}

impl Cfg {
    fn fresh(&mut self) -> PointId {
        let id = PointId(self.next);
        self.next += 1;
        id
    }

    fn edge(&mut self, from: PointId, to: PointId) {
        self.successors.entry(from).or_default().push(to);
        self.predecessors.entry(to).or_default().push(from);
    }
}

#[derive(Default)]
pub struct CfgPoints {
    /// Point representing "control sits just before this statement executes."
    pub stmt_points: FxHashMap<usize, PointId>,
    /// Point representing "control just after this statement completes
    /// normally." Absent for statements that never fall through (`return`,
    /// an unconditional `break`/`continue`, or one whose last reachable
    /// statement diverges).
    pub stmt_after_points: FxHashMap<usize, PointId>,
    /// Point representing "control sits just before this expression executes."
    pub expr_points: FxHashMap<usize, PointId>,
}

struct LoopTargets {
    /// Where `continue` jumps: the condition re-check point for `while`,
    /// the header (pre-increment-and-recheck) point for `for`.
    continue_target: PointId,
    /// Where `break` jumps: the first point after the loop.
    break_target: PointId,
}

pub struct CfgBuilder {
    cfg: Cfg,
    points: CfgPoints,
    loop_stack: Vec<LoopTargets>,
}

impl CfgBuilder {
    pub fn new() -> Self {
        Self {
            cfg: Cfg::default(),
            points: CfgPoints::default(),
            loop_stack: Vec::new(),
        }
    }

    pub fn build<'a, 'bump>(mut self, body: &HirStmt<'a, 'bump>) -> (Cfg, CfgPoints) {
        let entry = self.cfg.fresh();
        self.cfg.entry = Some(entry);
        if let Some(tail) = self.visit_stmt(entry, body) {
            self.cfg.exits.push(tail);
        }
        (self.cfg, self.points)
    }

    fn stmt_key<'a, 'bump>(stmt: &HirStmt<'a, 'bump>) -> usize {
        stmt as *const HirStmt<'a, 'bump> as usize
    }

    fn expr_key<'a, 'bump>(expr: &HirExpr<'a, 'bump>) -> usize {
        expr as *const HirExpr<'a, 'bump> as usize
    }

    fn visit_stmt<'a, 'bump>(
        &mut self,
        entry: PointId,
        stmt: &HirStmt<'a, 'bump>,
    ) -> Option<PointId> {
        self.points.stmt_points.insert(Self::stmt_key(stmt), entry);

        let after = self.visit_stmt_inner(entry, stmt);

        if let Some(after) = after {
            self.points
                .stmt_after_points
                .insert(Self::stmt_key(stmt), after);
        }

        after
    }

    fn visit_stmt_inner<'a, 'bump>(
        &mut self,
        entry: PointId,
        stmt: &HirStmt<'a, 'bump>,
    ) -> Option<PointId> {
        match stmt {
            HirStmt::Let {
                value,
                else_block,
                catch_pattern,
                ..
            } => {
                self.visit_expr_control_flow(entry, value);

                let after = self.cfg.fresh();
                self.cfg.edge(entry, after);

                // `? else { .. }`: taken on the null path, converges with
                // the normal path at `after` unless it itself diverges.
                if let Some(else_stmt) = *else_block {
                    let else_entry = self.cfg.fresh();
                    self.cfg.edge(entry, else_entry);
                    if let Some(tail) = self.visit_stmt(else_entry, else_stmt) {
                        self.cfg.edge(tail, after);
                    }
                }

                // `catch { .. }`: taken on the error path, one branch per
                // handler arm
                if let Some(pattern) = *catch_pattern {
                    match pattern {
                        HirErrorHandlerPattern::Single { body, .. } => {
                            let catch_entry = self.cfg.fresh();
                            self.cfg.edge(entry, catch_entry);
                            if let Some(tail) = self.visit_block(catch_entry, body) {
                                self.cfg.edge(tail, after);
                            }
                        }
                        HirErrorHandlerPattern::Multiple { branches } => {
                            for branch in branches.iter() {
                                let branch_entry = self.cfg.fresh();
                                self.cfg.edge(entry, branch_entry);
                                if let Some(tail) = self.visit_block(branch_entry, branch.body) {
                                    self.cfg.edge(tail, after);
                                }
                            }
                        }
                    }
                }

                Some(after)
            }

            HirStmt::Expr(e) => {
                self.visit_expr_control_flow(entry, e);
                let after = self.cfg.fresh();
                self.cfg.edge(entry, after);
                Some(after)
            }

            HirStmt::Const(c) => {
                self.visit_expr_control_flow(entry, &c.value);
                let after = self.cfg.fresh();
                self.cfg.edge(entry, after);
                Some(after)
            }

            HirStmt::Import(..) | HirStmt::Package(..) => {
                let after = self.cfg.fresh();
                self.cfg.edge(entry, after);
                Some(after)
            }

            HirStmt::If {
                cond,
                then_block,
                else_block,
            } => {
                self.visit_expr_control_flow(entry, cond);

                let then_entry = self.cfg.fresh();
                self.cfg.edge(entry, then_entry);
                let then_tail = self.visit_block(then_entry, then_block);

                let else_tail = match *else_block {
                    Some(else_stmt) => {
                        let else_entry = self.cfg.fresh();
                        self.cfg.edge(entry, else_entry);
                        self.visit_stmt(else_entry, else_stmt)
                    }
                    None => {
                        let skip = self.cfg.fresh();
                        self.cfg.edge(entry, skip);
                        Some(skip)
                    }
                };

                match (then_tail, else_tail) {
                    (None, None) => None,
                    (Some(a), None) => Some(a),
                    (None, Some(b)) => Some(b),
                    (Some(a), Some(b)) => {
                        let join = self.cfg.fresh();
                        self.cfg.edge(a, join);
                        self.cfg.edge(b, join);
                        Some(join)
                    }
                }
            }

            HirStmt::While { cond, body } => {
                self.visit_expr_control_flow(entry, cond);

                let body_entry = self.cfg.fresh();
                self.cfg.edge(entry, body_entry);
                let after = self.cfg.fresh();
                self.cfg.edge(entry, after);

                self.loop_stack.push(LoopTargets {
                    continue_target: entry,
                    break_target: after,
                });
                if let Some(body_tail) = self.visit_stmt(body_entry, body) {
                    self.cfg.edge(body_tail, entry);
                }
                self.loop_stack.pop();

                Some(after)
            }

            HirStmt::For {
                init,
                condition,
                increment,
                body,
            } => {
                let header = match *init {
                    Some(init_stmt) => self.visit_stmt(entry, init_stmt).unwrap_or(entry),
                    None => entry,
                };
                if let Some(cond) = *condition {
                    self.visit_expr_control_flow(header, cond);
                }
                if let Some(inc) = *increment {
                    self.visit_expr_control_flow(header, inc);
                }

                let body_entry = self.cfg.fresh();
                self.cfg.edge(header, body_entry);
                let after = self.cfg.fresh();
                self.cfg.edge(header, after);

                self.loop_stack.push(LoopTargets {
                    continue_target: header,
                    break_target: after,
                });
                if let Some(body_tail) = self.visit_stmt(body_entry, body) {
                    self.cfg.edge(body_tail, header);
                }
                self.loop_stack.pop();

                Some(after)
            }

            HirStmt::Block { body } => self.visit_block(entry, body),

            HirStmt::Match { expr, arms } => {
                self.visit_expr_control_flow(entry, expr);

                let mut tails = Vec::new();
                for arm in arms.iter() {
                    let arm_entry = self.cfg.fresh();
                    self.cfg.edge(entry, arm_entry);
                    if let Some(guard) = arm.guard {
                        self.visit_expr_control_flow(arm_entry, guard);
                    }
                    if let Some(t) = self.visit_stmt(arm_entry, arm.body) {
                        tails.push(t);
                    }
                }
                match tails.len() {
                    0 => None,
                    1 => Some(tails[0]),
                    _ => {
                        let join = self.cfg.fresh();
                        for t in tails {
                            self.cfg.edge(t, join);
                        }
                        Some(join)
                    }
                }
            }

            HirStmt::Break(value, _) => {
                if let Some(v) = *value {
                    self.visit_expr_control_flow(entry, v);
                }
                if let Some(target) = self.loop_stack.last() {
                    self.cfg.edge(entry, target.break_target);
                }
                None
            }

            HirStmt::Continue(_) => {
                if let Some(target) = self.loop_stack.last() {
                    self.cfg.edge(entry, target.continue_target);
                }
                None
            }

            HirStmt::Return(value) => {
                if let Some(v) = *value {
                    self.visit_expr_control_flow(entry, v);
                }
                self.cfg.exits.push(entry);
                None
            }

            HirStmt::UnsafeBlock { body } | HirStmt::Defer(body) => self.visit_stmt(entry, body),
        }
    }

    fn visit_block<'a, 'bump>(
        &mut self,
        entry: PointId,
        body: &[HirStmt<'a, 'bump>],
    ) -> Option<PointId> {
        let mut current = Some(entry);
        for stmt in body {
            current = match current {
                Some(p) => self.visit_stmt(p, stmt),
                None => {
                    let dead = self.cfg.fresh();
                    self.visit_stmt(dead, stmt);
                    None
                }
            };
        }
        current
    }

    fn visit_expr_control_flow<'a, 'bump>(&mut self, entry: PointId, expr: &HirExpr<'a, 'bump>) {
        self.points.expr_points.insert(Self::expr_key(expr), entry);
        match expr {
            HirExpr::If { if_stmt, .. } => {
                self.visit_stmt(entry, if_stmt);
            }
            HirExpr::Match {
                expr: scrutinee,
                arms,
                ..
            } => {
                self.visit_expr_control_flow(entry, scrutinee);
                for arm in arms.iter() {
                    let arm_entry = self.cfg.fresh();
                    self.cfg.edge(entry, arm_entry);
                    if let Some(guard) = arm.guard {
                        self.visit_expr_control_flow(arm_entry, guard);
                    }
                    self.visit_stmt(arm_entry, arm.body);
                }
            }
            HirExpr::Block { body, .. } => {
                self.visit_block(entry, body);
            }
            HirExpr::Lambda { .. } => {
                // Own Cfg, built and analyzed separately
            }
            _ => {}
        }
    }
}

impl Default for CfgBuilder {
    fn default() -> Self {
        Self::new()
    }
}
