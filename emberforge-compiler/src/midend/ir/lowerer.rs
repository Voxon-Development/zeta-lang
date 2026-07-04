use crate::midend::ir::block_data::CurrentBlockData;
use crate::midend::ir::expr_lowerer::MirExprLowerer;
use crate::midend::ir::ir_conversion::lower_type_hir;
use codex_dependency_graph::DepGraph;
use ir::hir::{
    HirErrorHandlerPattern, HirExpr, HirFunc, HirParam, HirStmt, HirStruct, HirType, StrId,
    ThisPassingKind,
};
use ir::ir_hasher::{FxHashBuilder, HashSet};
use ir::layout::{TargetInfo, alignof_ssa, layout_of_ssa, round_up_to_align};
use ir::ssa_ir::{BasicBlock, BinOp, BlockId, Function, Instruction, Operand, SsaType, Value};
use smallvec::SmallVec;
use std::collections::HashMap;
use std::marker::PhantomData;
use std::sync::Arc;
use zetaruntime::string_pool::StringPool;

/// Bookkeeping for one enclosing loop, needed so `break`/`continue` (which
/// can appear arbitrarily deep inside nested control flow) can contribute a
/// phi edge to the right join point. `phis` are `(name, instruction-index,
/// phi-dest-value)` triples identifying placeholder `Instruction::Phi`s
/// created by `open_join`, still waiting for more incoming edges.
struct LoopCtx {
    /// Where `continue` jumps to (the loop header for `while`; the
    /// increment block for `for`).
    continue_target: BlockId,
    /// The block whose phis a `continue` must contribute an edge to (same
    /// as `continue_target`).
    continue_join_bb: BlockId,
    continue_join_phis: Vec<(StrId, usize, Value)>,
    /// Where `break` jumps to (the block right after the loop).
    break_target: BlockId,
    /// The block (== `break_target`) whose phis a `break` must contribute
    /// an edge to.
    break_join_phis: Vec<(StrId, usize, Value)>,
}

pub struct FunctionLowerer<'a, 'bump> {
    current_block_data: CurrentBlockData,
    var_map: HashMap<StrId, Value, FxHashBuilder>,
    phantom_data: PhantomData<&'bump ()>,

    /// Stack of enclosing loops, innermost last. `break`/`continue` jump to
    /// the top entry's targets, and must also contribute a phi edge to the
    /// corresponding join point (see `LoopCtx`) since they're extra
    /// predecessors the loop-header/exit blocks don't otherwise know about.
    loop_stack: Vec<LoopCtx>,

    // immutable metadata snapshots
    funcs: &'a HashMap<StrId, Function, FxHashBuilder>,
    class_field_offsets: &'a HashMap<StrId, HashMap<StrId, usize, FxHashBuilder>, FxHashBuilder>,
    class_method_slots: &'a HashMap<StrId, HashMap<StrId, usize, FxHashBuilder>, FxHashBuilder>,
    class_mangled_map: &'a HashMap<StrId, HashMap<StrId, StrId, FxHashBuilder>, FxHashBuilder>,
    class_vtable_slots: &'a HashMap<StrId, Vec<StrId>, FxHashBuilder>,
    interface_id_map: &'a HashMap<StrId, usize, FxHashBuilder>,
    interface_method_slots: &'a HashMap<StrId, HashMap<StrId, usize, FxHashBuilder>, FxHashBuilder>,
    classes: &'a HashMap<StrId, HirStruct<'a, 'a>, FxHashBuilder>,

    enum_variant_tags: &'a HashMap<StrId, HashMap<StrId, usize, FxHashBuilder>, FxHashBuilder>,
    context: Arc<StringPool>,

    extern_c_names: &'a HashSet<StrId>,
    pub dep_graph: &'a DepGraph,
    pub module_idx: usize,
    return_type: Option<HirType<'a, 'bump>>,
    global_funcs: &'a HashMap<StrId, Function, FxHashBuilder>,
}

impl<'a, 'bump> FunctionLowerer<'a, 'bump>
where
    'bump: 'a,
{
    pub fn new(
        hir_fn: &HirFunc<'a, 'bump>,
        funcs: &'a HashMap<StrId, Function, FxHashBuilder>,
        global_funcs: &'a HashMap<StrId, Function, FxHashBuilder>,
        class_field_offsets: &'a HashMap<
            StrId,
            HashMap<StrId, usize, FxHashBuilder>,
            FxHashBuilder,
        >,
        class_method_slots: &'a HashMap<StrId, HashMap<StrId, usize, FxHashBuilder>, FxHashBuilder>,
        class_mangled_map: &'a HashMap<StrId, HashMap<StrId, StrId, FxHashBuilder>, FxHashBuilder>,
        class_vtable_slots: &'a HashMap<StrId, Vec<StrId>, FxHashBuilder>,
        interface_id_map: &'a HashMap<StrId, usize, FxHashBuilder>,
        interface_method_slots: &'a HashMap<
            StrId,
            HashMap<StrId, usize, FxHashBuilder>,
            FxHashBuilder,
        >,
        classes: &'a HashMap<StrId, HirStruct<'a, 'a>, FxHashBuilder>,
        enum_variant_tags: &'a HashMap<StrId, HashMap<StrId, usize, FxHashBuilder>, FxHashBuilder>,
        context: Arc<StringPool>,
        extern_c_names: &'a HashSet<StrId>,
        dep_graph: &'a DepGraph,
        module_idx: usize,
    ) -> Result<Self, std::alloc::AllocError> {
        Self::new_internal(
            hir_fn,
            None,
            funcs,
            global_funcs,
            class_field_offsets,
            class_method_slots,
            class_mangled_map,
            class_vtable_slots,
            interface_id_map,
            interface_method_slots,
            classes,
            enum_variant_tags,
            context,
            extern_c_names,
            dep_graph,
            module_idx,
        )
    }

    pub fn new_with_class(
        hir_fn: &HirFunc<'a, 'bump>,
        class_name: StrId,
        funcs: &'a HashMap<StrId, Function, FxHashBuilder>,
        global_funcs: &'a HashMap<StrId, Function, FxHashBuilder>,
        class_field_offsets: &'a HashMap<
            StrId,
            HashMap<StrId, usize, FxHashBuilder>,
            FxHashBuilder,
        >,
        class_method_slots: &'a HashMap<StrId, HashMap<StrId, usize, FxHashBuilder>, FxHashBuilder>,
        class_mangled_map: &'a HashMap<StrId, HashMap<StrId, StrId, FxHashBuilder>, FxHashBuilder>,
        class_vtable_slots: &'a HashMap<StrId, Vec<StrId>, FxHashBuilder>,
        interface_id_map: &'a HashMap<StrId, usize, FxHashBuilder>,
        interface_method_slots: &'a HashMap<
            StrId,
            HashMap<StrId, usize, FxHashBuilder>,
            FxHashBuilder,
        >,
        classes: &'a HashMap<StrId, HirStruct<'a, 'a>, FxHashBuilder>,
        enum_variant_tags: &'a HashMap<StrId, HashMap<StrId, usize, FxHashBuilder>, FxHashBuilder>,
        context: Arc<StringPool>,
        extern_c_names: &'a HashSet<StrId>,
        dep_graph: &'a DepGraph,
        module_idx: usize,
    ) -> Result<Self, std::alloc::AllocError> {
        Self::new_internal(
            hir_fn,
            Some(class_name),
            funcs,
            global_funcs,
            class_field_offsets,
            class_method_slots,
            class_mangled_map,
            class_vtable_slots,
            interface_id_map,
            interface_method_slots,
            classes,
            enum_variant_tags,
            context,
            extern_c_names,
            dep_graph,
            module_idx,
        )
    }

    fn new_internal(
        hir_fn: &HirFunc<'a, 'bump>,
        class_context: Option<StrId>,
        funcs: &'a HashMap<StrId, Function, FxHashBuilder>,
        global_funcs: &'a HashMap<StrId, Function, FxHashBuilder>,
        class_field_offsets: &'a HashMap<
            StrId,
            HashMap<StrId, usize, FxHashBuilder>,
            FxHashBuilder,
        >,
        class_method_slots: &'a HashMap<StrId, HashMap<StrId, usize, FxHashBuilder>, FxHashBuilder>,
        class_mangled_map: &'a HashMap<StrId, HashMap<StrId, StrId, FxHashBuilder>, FxHashBuilder>,
        class_vtable_slots: &'a HashMap<StrId, Vec<StrId>, FxHashBuilder>,
        interface_id_map: &'a HashMap<StrId, usize, FxHashBuilder>,
        interface_method_slots: &'a HashMap<
            StrId,
            HashMap<StrId, usize, FxHashBuilder>,
            FxHashBuilder,
        >,
        classes: &'a HashMap<StrId, HirStruct<'a, 'a>, FxHashBuilder>,
        enum_variant_tags: &'a HashMap<StrId, HashMap<StrId, usize, FxHashBuilder>, FxHashBuilder>,
        context: Arc<StringPool>,
        extern_c_names: &'a HashSet<StrId>,
        dep_graph: &'a DepGraph,
        module_idx: usize,
    ) -> Result<Self, std::alloc::AllocError> {
        let mut func: Function = Function {
            name: hir_fn.name,
            params: SmallVec::new(),
            ret_type: lower_type_hir(hir_fn.return_type.as_ref().unwrap_or(&HirType::Void)),
            blocks: SmallVec::new(),
            value_types: HashMap::with_hasher(FxHashBuilder),
            entry: BlockId(0),
            function_metadata: hir_fn.function_metadata,
        };

        let mut next_value = 0usize;
        let mut next_block = 0usize;
        let mut var_map = HashMap::with_hasher(FxHashBuilder);
        let mut value_types = HashMap::with_hasher(FxHashBuilder);

        if let Some(params) = hir_fn.params {
            // allocate params
            Self::insert_existing_params_with_class(
                context.clone(),
                &mut func,
                &mut next_value,
                &mut var_map,
                &mut value_types,
                params,
                class_context,
            );
        }

        // create entry block
        let entry_bb = BlockId(next_block);
        next_block += 1;
        func.entry = entry_bb;
        func.blocks.push(BasicBlock {
            id: entry_bb,
            instructions: Vec::new(),
        });

        let current_block_data: CurrentBlockData =
            CurrentBlockData::new(func, entry_bb, next_value, next_block, value_types);

        Ok(Self {
            current_block_data,
            funcs,
            var_map,
            loop_stack: Vec::new(),
            class_field_offsets,
            class_method_slots,
            class_mangled_map,
            class_vtable_slots,
            interface_id_map,
            interface_method_slots,
            classes,
            enum_variant_tags,
            context,
            phantom_data: Default::default(),
            extern_c_names,
            dep_graph,
            module_idx,
            return_type: hir_fn.return_type,
            global_funcs,
        })
    }

    fn insert_existing_params_with_class(
        context: Arc<StringPool>,
        func: &mut Function,
        next_value: &mut usize,
        var_map: &mut HashMap<StrId, Value, FxHashBuilder>,
        value_types: &mut HashMap<Value, SsaType, FxHashBuilder>,
        params: &[HirParam],
        _class_context: Option<StrId>,
    ) {
        for p in params {
            let v = Value(*next_value);
            *next_value += 1;

            match *p {
                HirParam::This { kind } => {
                    let name = StrId(context.intern("this"));
                    // For ref/ptr passing kinds, `this` arrives as a pointer (i64 address).
                    // For move/move-mut, it arrives as an inline struct value, but since our
                    // ABI already passes structs as stack-allocated pointers everywhere, we
                    // always use Pointer here. The difference is only semantic (mutability).
                    let inner_ty = if let Some(cn) = _class_context {
                        SsaType::User(cn, vec![])
                    } else {
                        SsaType::Dyn
                    };
                    let ty = match kind {
                        ThisPassingKind::Move | ThisPassingKind::MoveMut => inner_ty,
                        _ => SsaType::Pointer(Box::new(inner_ty)),
                    };
                    func.params.push((v, ty.clone()));
                    value_types.insert(v, ty);
                    var_map.insert(name, v);
                }
                HirParam::Normal { name, param_type } => {
                    let ty = lower_type_hir(&param_type);
                    func.params.push((v, ty.clone()));
                    value_types.insert(v, ty);
                    var_map.insert(name, v);
                }
            }
        }
    }

    pub(super) fn lower_body(&mut self, body: Option<HirStmt<'a, 'bump>>) {
        if let Some(b) = body {
            match b {
                HirStmt::Block { body } => {
                    self.lower_stmt_seq(body);
                }
                _ => panic!(),
            }
        }
    }

    /// Lower a sequence of statements in order, stopping as soon as the
    /// current block has been terminated (e.g. by a `return`/`break`/
    /// `continue`) since anything after that point is unreachable and must
    /// not be appended to an already-terminated basic block.
    fn lower_stmt_seq(&mut self, stmts: &[HirStmt<'a, 'bump>]) {
        for stmt in stmts {
            if self.block_terminated() {
                break;
            }
            self.lower_stmt(stmt);
        }
    }

    /// Whether the current basic block already ends in a terminator
    /// instruction (`Ret`, `Jump`, or `Branch`). Once a block is terminated,
    /// no further instructions may be appended to it.
    fn block_terminated(&mut self) -> bool {
        matches!(
            self.current_block_data.bb().instructions.last(),
            Some(Instruction::Ret { .. } | Instruction::Jump { .. } | Instruction::Branch { .. })
        )
    }

    pub(super) fn lower_stmt(&mut self, stmt: &HirStmt<'a, 'bump>) {
        match stmt {
            HirStmt::Let {
                name,
                value,
                catch_pattern,
                else_block,
                ..
            } => {
                let mut val = self.allow_lowering_expr(value);

                if let Some(pat) = catch_pattern {
                    self.lower_catch(val, pat);
                }

                if let Some(else_stmts) = else_block {
                    val = self.lower_nullable_unwrap(val, else_stmts);
                }

                self.var_map.insert(name.clone(), val);
            }
            HirStmt::Return(expr) => {
                let value = expr.as_ref().map(|e| {
                    let val = match e {
                        HirExpr::Null(_) => self.lower_null_as(self.return_type),
                        _ => self.allow_lowering_expr(e),
                    };
                    Operand::Value(val)
                });
                self.emit(Instruction::Ret { value });
            }
            HirStmt::Expr(expr) => {
                let _ = self.allow_lowering_expr(expr);
            }
            HirStmt::Block { body } => {
                self.lower_stmt_seq(body);
            }
            HirStmt::UnsafeBlock { body } => {
                self.lower_stmt(body);
            }
            HirStmt::If {
                cond,
                then_block,
                else_block,
            } => {
                self.lower_if(cond, then_block, else_block);
            }
            HirStmt::While { cond, body } => {
                self.lower_while(cond, body);
            }
            HirStmt::For {
                init,
                condition,
                increment,
                body,
            } => {
                self.lower_for(*init, *condition, *increment, body);
            }
            HirStmt::Break(_expr, _span) => {
                let ctx = self.loop_stack.last().expect(
                    "`break` outside of a loop (should have been caught by the typechecker)",
                );
                let break_target = ctx.break_target;
                let phis = ctx.break_join_phis.clone();
                let from_bb = self.current_block_data.current_block;
                let vars = self.var_map.clone();
                self.contribute_join_edge(break_target, from_bb, &vars, &phis);
                self.emit(Instruction::Jump {
                    target: break_target,
                });
            }
            HirStmt::Continue(_span) => {
                let ctx = self.loop_stack.last().expect(
                    "`continue` outside of a loop (should have been caught by the typechecker)",
                );
                let continue_target = ctx.continue_target;
                let join_bb = ctx.continue_join_bb;
                let phis = ctx.continue_join_phis.clone();
                let from_bb = self.current_block_data.current_block;
                let vars = self.var_map.clone();
                self.contribute_join_edge(join_bb, from_bb, &vars, &phis);
                self.emit(Instruction::Jump {
                    target: continue_target,
                });
            }
            _ => unimplemented!("Statement {:?} not yet lowered", stmt),
        }
    }

    /// Creates a placeholder phi (with no incoming edges yet) in the
    /// *current* block for every currently-live local, and rewrites
    /// `var_map` to route future reads through those phis. Must be called
    /// immediately after switching into an otherwise-empty join block
    /// (phis must be the leading instructions of a block, see the
    /// cranelift backend's predecessor-scanning convention). Each block that
    /// can jump into this join point must later call `contribute_join_edge`.
    fn open_join(&mut self) -> Vec<(StrId, usize, Value)> {
        let names: Vec<StrId> = self.var_map.keys().copied().collect();
        let mut phis = Vec::with_capacity(names.len());
        for name in names {
            let dest = self.current_block_data.fresh_value();
            let idx = self.current_block_data.bb().instructions.len();
            self.emit(Instruction::Phi {
                dest,
                incoming: SmallVec::new(),
            });
            self.var_map.insert(name, dest);
            phis.push((name, idx, dest));
        }
        phis
    }

    /// Records that control can reach the join block owning `phis` from
    /// `from_bb`, carrying the values captured in `vars` (a snapshot of
    /// `var_map` taken in `from_bb` right before the jump).
    fn contribute_join_edge(
        &mut self,
        join_bb: BlockId,
        from_bb: BlockId,
        vars: &HashMap<StrId, Value, FxHashBuilder>,
        phis: &[(StrId, usize, Value)],
    ) {
        if phis.is_empty() {
            return;
        }
        let block = self
            .current_block_data
            .func
            .blocks
            .iter_mut()
            .find(|b| b.id == join_bb)
            .expect("join block missing");
        for (name, idx, _) in phis {
            if let Some(val) = vars.get(name).copied() {
                if let Instruction::Phi { incoming, .. } = &mut block.instructions[*idx] {
                    incoming.push((from_bb, val));
                }
            }
        }
    }

    /// Merges the local-variable state of every *live* (non-terminated)
    /// incoming branch into the current block (a merge point that's already
    /// active, e.g. an if/else join). Unlike `open_join`, this decides
    /// per-variable whether a phi is actually needed (only when branches
    /// disagree), since for a simple two-way merge we know every
    /// contributing branch's final value up front and don't have the
    /// loop-style chicken-and-egg problem that forces placeholders.
    fn merge_var_maps(&mut self, branches: Vec<(BlockId, HashMap<StrId, Value, FxHashBuilder>)>) {
        match branches.len() {
            0 => {
                // Join point is unreachable (every incoming branch diverged);
                // leave `var_map` as-is, nothing will ever read it here.
            }
            1 => {
                self.var_map = branches.into_iter().next().unwrap().1;
            }
            _ => {
                let mut all_names: std::collections::HashSet<StrId> =
                    std::collections::HashSet::new();
                for (_, vars) in &branches {
                    all_names.extend(vars.keys().copied());
                }

                let mut merged = HashMap::with_hasher(FxHashBuilder);
                for name in all_names {
                    let mut entries: Vec<(BlockId, Value)> = Vec::new();
                    for (bb, vars) in &branches {
                        if let Some(v) = vars.get(&name) {
                            entries.push((*bb, *v));
                        }
                    }
                    let Some((_, first_val)) = entries.first().copied() else {
                        continue;
                    };
                    if entries.iter().all(|(_, v)| *v == first_val) {
                        merged.insert(name, first_val);
                    } else {
                        let dest = self.current_block_data.fresh_value();
                        self.emit(Instruction::Phi {
                            dest,
                            incoming: entries.into_iter().collect(),
                        });
                        merged.insert(name, dest);
                    }
                }
                self.var_map = merged;
            }
        }
    }

    fn lower_if(
        &mut self,
        cond: &HirExpr<'a, 'bump>,
        then_block: &'bump [HirStmt<'a, 'bump>],
        else_block: &Option<&'bump HirStmt<'a, 'bump>>,
    ) {
        let pre_if_bb = self.current_block_data.current_block;
        let vars_before = self.var_map.clone();
        let cond_val = self.allow_lowering_expr(cond);

        let then_bb = self.current_block_data.new_block();
        let merge_bb = self.current_block_data.new_block();
        let else_bb = if else_block.is_some() {
            self.current_block_data.new_block()
        } else {
            merge_bb
        };

        self.emit(Instruction::Branch {
            cond: Operand::Value(cond_val),
            then_bb,
            else_bb,
        });

        self.var_map = vars_before.clone();
        self.current_block_data.switch_to(then_bb);
        self.lower_stmt_seq(then_block);
        let then_live = if self.block_terminated() {
            None
        } else {
            let tail = self.current_block_data.current_block;
            let vars = self.var_map.clone();
            self.emit(Instruction::Jump { target: merge_bb });
            Some((tail, vars))
        };

        let else_live = if let Some(else_stmt) = else_block {
            self.var_map = vars_before.clone();
            self.current_block_data.switch_to(else_bb);
            self.lower_stmt(else_stmt);
            if self.block_terminated() {
                None
            } else {
                let tail = self.current_block_data.current_block;
                let vars = self.var_map.clone();
                self.emit(Instruction::Jump { target: merge_bb });
                Some((tail, vars))
            }
        } else {
            // Implicit empty else: falls straight from `pre_if_bb` into
            // `merge_bb` (the `Branch` above already wired `else_bb ==
            // merge_bb` directly, there's no separate block/jump for it).
            Some((pre_if_bb, vars_before))
        };

        self.current_block_data.switch_to(merge_bb);
        let live_branches: Vec<_> = [then_live, else_live].into_iter().flatten().collect();
        self.merge_var_maps(live_branches);
    }

    fn lower_while(&mut self, cond: &HirExpr<'a, 'bump>, body: &HirStmt<'a, 'bump>) {
        let pre_loop_bb = self.current_block_data.current_block;
        let vars_before = self.var_map.clone();

        let cond_bb = self.current_block_data.new_block();
        let body_bb = self.current_block_data.new_block();
        let after_bb = self.current_block_data.new_block();

        self.emit(Instruction::Jump { target: cond_bb });

        // Loop header: a join of the pre-loop state and the back-edge(s)
        // from the body (natural fallthrough and/or `continue`).
        self.current_block_data.switch_to(cond_bb);
        let header_phis = self.open_join();
        self.contribute_join_edge(cond_bb, pre_loop_bb, &vars_before, &header_phis);

        let cond_val = self.allow_lowering_expr(cond);
        self.emit(Instruction::Branch {
            cond: Operand::Value(cond_val),
            then_bb: body_bb,
            else_bb: after_bb,
        });
        // Snapshot before `open_join` (below) rewrites `var_map` to point at
        // `after_bb`'s own placeholder phis.
        let header_vars = self.var_map.clone();

        // Loop exit join: the natural false-condition edge plus any `break`s.
        self.current_block_data.switch_to(after_bb);
        let exit_phis = self.open_join();
        self.contribute_join_edge(after_bb, cond_bb, &header_vars, &exit_phis);

        self.loop_stack.push(LoopCtx {
            continue_target: cond_bb,
            continue_join_bb: cond_bb,
            continue_join_phis: header_phis.clone(),
            break_target: after_bb,
            break_join_phis: exit_phis.clone(),
        });

        // `open_join` above (for `after_bb`) clobbered `var_map` to point at
        // its own placeholders; restore the header's values before lowering
        // the body, which runs right after the condition is checked true.
        self.var_map = header_vars;
        self.current_block_data.switch_to(body_bb);
        self.lower_stmt(body);
        if !self.block_terminated() {
            let tail_bb = self.current_block_data.current_block;
            let vars = self.var_map.clone();
            self.contribute_join_edge(cond_bb, tail_bb, &vars, &header_phis);
            self.emit(Instruction::Jump { target: cond_bb });
        }
        self.loop_stack.pop();

        self.current_block_data.switch_to(after_bb);
        self.var_map = exit_phis
            .into_iter()
            .map(|(name, _, dest)| (name, dest))
            .collect();
    }

    fn lower_for(
        &mut self,
        init: Option<&'bump HirStmt<'a, 'bump>>,
        condition: Option<&'bump HirExpr<'a, 'bump>>,
        increment: Option<&'bump HirExpr<'a, 'bump>>,
        body: &HirStmt<'a, 'bump>,
    ) {
        if let Some(init_stmt) = init {
            self.lower_stmt(init_stmt);
        }

        let pre_loop_bb = self.current_block_data.current_block;
        let vars_before = self.var_map.clone();

        let cond_bb = self.current_block_data.new_block();
        let body_bb = self.current_block_data.new_block();
        let incr_bb = self.current_block_data.new_block();
        let after_bb = self.current_block_data.new_block();

        self.emit(Instruction::Jump { target: cond_bb });

        // Loop header: joins the pre-loop state with the single edge coming
        // from `incr_bb` (which itself merges the body's fallthrough and any
        // `continue`s, see below).
        self.current_block_data.switch_to(cond_bb);
        let header_phis = self.open_join();
        self.contribute_join_edge(cond_bb, pre_loop_bb, &vars_before, &header_phis);

        match condition {
            Some(cond_expr) => {
                let cond_val = self.allow_lowering_expr(cond_expr);
                self.emit(Instruction::Branch {
                    cond: Operand::Value(cond_val),
                    then_bb: body_bb,
                    else_bb: after_bb,
                });
            }
            None => {
                self.emit(Instruction::Jump { target: body_bb });
            }
        }
        // Snapshot before the `open_join` calls below rewrite `var_map` to
        // point at `after_bb`/`incr_bb`'s own placeholder phis.
        let header_vars = self.var_map.clone();

        // Loop exit join: the natural false-condition edge plus any `break`s.
        self.current_block_data.switch_to(after_bb);
        let exit_phis = self.open_join();
        self.contribute_join_edge(after_bb, cond_bb, &header_vars, &exit_phis);

        // Increment join: the body's natural fallthrough plus any
        // `continue`s (which must still run the increment before)
        self.current_block_data.switch_to(incr_bb);
        let incr_phis = self.open_join();

        self.loop_stack.push(LoopCtx {
            continue_target: incr_bb,
            continue_join_bb: incr_bb,
            continue_join_phis: incr_phis.clone(),
            break_target: after_bb,
            break_join_phis: exit_phis.clone(),
        });

        // Restore the header's values before lowering the body, which runs
        // right after the condition is checked true.
        self.var_map = header_vars;
        self.current_block_data.switch_to(body_bb);
        self.lower_stmt(body);
        if !self.block_terminated() {
            let tail_bb = self.current_block_data.current_block;
            let vars = self.var_map.clone();
            self.contribute_join_edge(incr_bb, tail_bb, &vars, &incr_phis);
            self.emit(Instruction::Jump { target: incr_bb });
        }
        self.loop_stack.pop();

        // Resume `incr_bb` with its own merged (phi) values, not whatever
        // was left over from lowering the body.
        self.var_map = incr_phis
            .iter()
            .map(|(name, _, dest)| (*name, *dest))
            .collect();
        self.current_block_data.switch_to(incr_bb);
        if let Some(inc_expr) = increment {
            let _ = self.allow_lowering_expr(inc_expr);
        }
        if !self.block_terminated() {
            let tail_bb = self.current_block_data.current_block;
            let vars = self.var_map.clone();
            self.contribute_join_edge(cond_bb, tail_bb, &vars, &header_phis);
            self.emit(Instruction::Jump { target: cond_bb });
        }

        self.current_block_data.switch_to(after_bb);
        self.var_map = exit_phis
            .into_iter()
            .map(|(name, _, dest)| (name, dest))
            .collect();
    }

    fn stmt_diverges(stmt: &HirStmt) -> bool {
        matches!(
            stmt,
            HirStmt::Return(_) | HirStmt::Break(..) | HirStmt::Continue(_)
        )
    }

    fn lower_catch(&mut self, raw_val: Value, pattern: &HirErrorHandlerPattern<'a, 'bump>) {
        let branches: Vec<(HirType, Option<StrId>, &[HirStmt])> = match pattern {
            HirErrorHandlerPattern::Single {
                error_type,
                binding,
                body,
            } => vec![(*error_type, *binding, *body)],
            HirErrorHandlerPattern::Multiple { branches } => branches
                .iter()
                .map(|b| (b.error_type, b.binding, b.body))
                .collect(),
        };

        let throws_enum = StrId(self.context.intern("__throws"));
        let tags = self
            .enum_variant_tags
            .get(&throws_enum)
            .expect("__throws table missing, was MirModuleLowerer::register_throws run?");

        // raw_val's real SsaType::Enum(variants) drives the layout
        let enum_ty = self
            .current_block_data
            .value_type(raw_val)
            .expect("catch target must have a known SsaType")
            .clone();
        let SsaType::Enum(variant_types) = &enum_ty else {
            panic!(
                "`catch` used on non-enum SsaType {:?}, thrown values must be SsaType::Enum",
                enum_ty
            );
        };

        let mut max_variant = ir::layout::Layout { size: 0, align: 1 };
        for vty in variant_types {
            let l = layout_of_ssa(&vty, TargetInfo { ptr_bytes: 8 })
                .unwrap_or_else(|e| panic!("failed to compute layout for error variant: {:?}", e));
            max_variant.size = max_variant.size.max(l.size);
            max_variant.align = max_variant.align.max(l.align);
        }
        let union_size = round_up_to_align(max_variant.size, max_variant.align);
        let tag_offset = round_up_to_align(union_size, 1); // tag align is 1, per layout_of_ssa
        let payload_offset = 0usize; // union fields all start at 0

        let tag_val = self.current_block_data.fresh_value();
        self.emit(Instruction::LoadField {
            dest: tag_val,
            base: Operand::Value(raw_val),
            offset: tag_offset,
        });

        let mut next_check_bb = self.current_block_data.current_block;

        for (i, (error_type, binding, body)) in branches.iter().enumerate() {
            let error_name = match error_type {
                HirType::Struct(name, _) | HirType::Enum(name, _) => *name,
                other => panic!("catch branch type {:?} is not a nominal error type", other),
            };
            let arm_tag = *tags.get(&error_name).unwrap_or_else(|| {
                panic!(
                    "catch branch handles `{:?}`, which is not in the __throws table",
                    error_name
                )
            });

            let arm_body_bb = self.current_block_data.new_block();
            let is_last = i == branches.len() - 1;
            let fallthrough_bb = if is_last {
                None
            } else {
                Some(self.current_block_data.new_block())
            };

            self.current_block_data.switch_to(next_check_bb);
            let cond = self.current_block_data.fresh_value();
            self.emit(Instruction::Binary {
                dest: cond,
                op: BinOp::Eq,
                left: Operand::Value(tag_val),
                right: Operand::ConstInt(arm_tag as i64),
            });
            match fallthrough_bb {
                Some(next) => {
                    self.emit(Instruction::Branch {
                        cond: Operand::Value(cond),
                        then_bb: arm_body_bb,
                        else_bb: next,
                    });
                    next_check_bb = next;
                }
                None => {
                    self.emit(Instruction::Branch {
                        cond: Operand::Value(cond),
                        then_bb: arm_body_bb,
                        else_bb: arm_body_bb, // last arm: assumes exhaustiveness was enforced earlier
                    });
                }
            }

            self.current_block_data.switch_to(arm_body_bb);
            if let Some(b) = binding {
                let payload = self.current_block_data.fresh_value();
                self.emit(Instruction::LoadField {
                    dest: payload,
                    base: Operand::Value(raw_val),
                    offset: payload_offset,
                });
                self.var_map.insert(*b, payload);
            }
            for s in *body {
                self.lower_stmt(s);
            }
            if !body.last().map_or(false, Self::stmt_diverges) {
                panic!(
                    "`catch` arm for `{:?}` must end in return, throw, break, or continue",
                    error_name
                );
            }
        }
    }

    fn lower_null_as(&mut self, expected: Option<HirType<'a, 'bump>>) -> Value {
        let v = self.current_block_data.fresh_value();

        match expected {
            Some(HirType::Nullable(inner)) => {
                let inner_ssa = lower_type_hir(inner);

                if inner_ssa.is_pointer() {
                    // Pointer-optimized nullable: null is just 0.
                    self.emit(Instruction::Const {
                        dest: v,
                        ty: SsaType::I64,
                        value: Operand::ConstInt(0),
                    });
                    self.current_block_data
                        .value_types
                        .insert(v, SsaType::Nullable(Box::new(inner_ssa)));
                } else {
                    // Tagged-union nullable: StackAlloc + Store(tag=0) at offset 0.
                    let _payload_align =
                        alignof_ssa(&inner_ssa, TargetInfo { ptr_bytes: 8 }).unwrap_or(1);
                    let tag_offset = 0usize; // tag at offset 0 per layout_of_ssa for Nullable
                    let nullable_ty = SsaType::Nullable(Box::new(inner_ssa.clone()));
                    let size = ir::layout::sizeof_ssa(&nullable_ty, TargetInfo { ptr_bytes: 8 })
                        .unwrap_or(16);

                    self.emit(Instruction::StackAlloc {
                        dest: v,
                        ty: nullable_ty.clone(),
                        count: size,
                    });

                    // Write null tag (0) at offset 0.
                    let null_tag_val = self.current_block_data.fresh_value();
                    self.emit(Instruction::Const {
                        dest: null_tag_val,
                        ty: SsaType::U8,
                        value: Operand::ConstInt(0),
                    });
                    self.emit(Instruction::StoreField {
                        base: Operand::Value(v),
                        offset: tag_offset,
                        value: Operand::Value(null_tag_val),
                    });

                    self.current_block_data.value_types.insert(v, nullable_ty);
                }
            }
            _ => {
                self.emit(Instruction::Const {
                    dest: v,
                    ty: SsaType::I64,
                    value: Operand::ConstInt(0),
                });
                self.current_block_data.value_types.insert(v, SsaType::Null);
            }
        }

        v
    }

    fn lower_nullable_unwrap(&mut self, val: Value, else_stmts: &HirStmt<'a, 'bump>) -> Value {
        let then_bb = self.current_block_data.new_block();
        let else_bb = self.current_block_data.new_block();

        let ty = self
            .current_block_data
            .value_type(val)
            .expect("nullable-unwrapped value must have a known SsaType")
            .clone();

        if ty.nullable_pointer_repr().is_some() {
            let cond = self.current_block_data.fresh_value();
            self.emit(Instruction::Binary {
                dest: cond,
                op: BinOp::Eq,
                left: Operand::Value(val),
                right: Operand::ConstInt(0),
            });
            self.emit(Instruction::Branch {
                cond: Operand::Value(cond),
                then_bb: else_bb,
                else_bb: then_bb,
            });
        } else if let SsaType::Nullable(_) = &ty {
            let nullable_enum = StrId(self.context.intern("__nullable"));
            let null_tag = *self
                .enum_variant_tags
                .get(&nullable_enum)
                .and_then(|m| m.get(&StrId(self.context.intern("null"))))
                .expect("__nullable enum's `null` tag missing from enum_variant_tags");

            let tag = self.current_block_data.fresh_value();
            self.emit(Instruction::LoadField {
                dest: tag,
                base: Operand::Value(val),
                offset: 0, // Nullable's tag is always at offset 0, per layout_of_ssa
            });
            self.current_block_data.value_types.insert(tag, SsaType::U8);

            let cond = self.current_block_data.fresh_value();
            self.emit(Instruction::Binary {
                dest: cond,
                op: BinOp::Eq,
                left: Operand::Value(tag),
                right: Operand::ConstInt(null_tag as i64),
            });
            self.emit(Instruction::Branch {
                cond: Operand::Value(cond),
                then_bb: else_bb,
                else_bb: then_bb,
            });
        } else {
            panic!("`? else` used on non-nullable SsaType {:?}", ty);
        }

        self.current_block_data.switch_to(else_bb);
        let HirStmt::Block { body } = else_stmts else {
            unreachable!()
        };
        for s in *body {
            self.lower_stmt(s);
        }
        if !body.last().map_or(false, Self::stmt_diverges) {
            panic!("`? else` block must end in return, throw, break, or continue");
        }

        self.current_block_data.switch_to(then_bb);
        let unwrapped = if ty.nullable_pointer_repr().is_some() {
            val
        } else if let SsaType::Nullable(inner) = &ty {
            // Real layout now, not a placeholder: payload sits right after the
            // 1-byte tag, padded up to the payload's own alignment.
            let payload_align =
                alignof_ssa(inner, TargetInfo { ptr_bytes: 8 }).unwrap_or_else(|e| {
                    panic!("failed to compute alignment for nullable payload: {:?}", e)
                });
            let payload_offset = round_up_to_align(1, payload_align);

            let payload = self.current_block_data.fresh_value();
            self.emit(Instruction::LoadField {
                dest: payload,
                base: Operand::Value(val),
                offset: payload_offset,
            });
            self.current_block_data
                .value_types
                .insert(payload, *inner.clone());
            payload
        } else {
            unreachable!()
        };

        unwrapped
    }

    fn allow_lowering_expr(&mut self, value: &HirExpr<'a, 'bump>) -> Value {
        let mut el = MirExprLowerer::new(
            &mut self.current_block_data,
            &self.funcs,
            self.global_funcs,
            &mut self.var_map,
            self.context.clone(),
            &self.class_field_offsets,
            &self.class_method_slots,
            &self.class_mangled_map,
            &self.class_vtable_slots,
            &self.interface_id_map,
            &self.interface_method_slots,
            &self.classes,
            self.extern_c_names,
            self.dep_graph,
            self.module_idx,
        );
        el.lower_expr(value)
    }

    pub(super) fn finish(self) -> Function {
        self.current_block_data.finish()
    }

    fn emit(&mut self, instruction: Instruction) {
        self.current_block_data.bb().instructions.push(instruction);
    }
}
