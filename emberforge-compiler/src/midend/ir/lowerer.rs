use crate::midend::ir::block_data::CurrentBlockData;
use crate::midend::ir::expr_lowerer::MirExprLowerer;
use crate::midend::ir::ir_conversion::lower_type_hir;
use codex_dependency_graph::DepGraph;
use ir::hir::{
    HirErrorHandlerPattern, HirExpr, HirFunc, HirParam, HirStmt, HirStruct, HirType, StrId,
    ThisPassingKind,
};
use ir::ir_hasher::{FxHashBuilder, HashSet};
use ir::ssa_ir::{BasicBlock, BinOp, BlockId, Function, Instruction, Operand, SsaType, Value};
use smallvec::SmallVec;
use std::collections::HashMap;
use std::marker::PhantomData;
use std::sync::Arc;
use zetaruntime::string_pool::StringPool;

pub struct FunctionLowerer<'a, 'bump> {
    current_block_data: CurrentBlockData,
    var_map: HashMap<StrId, Value, FxHashBuilder>,
    phantom_data: PhantomData<&'bump ()>,

    // immutable metadata snapshots
    funcs: &'a HashMap<StrId, Function, FxHashBuilder>,
    class_field_offsets: &'a HashMap<StrId, HashMap<StrId, usize, FxHashBuilder>, FxHashBuilder>,
    class_method_slots: &'a HashMap<StrId, HashMap<StrId, usize, FxHashBuilder>, FxHashBuilder>,
    class_mangled_map: &'a HashMap<StrId, HashMap<StrId, StrId, FxHashBuilder>, FxHashBuilder>,
    class_vtable_slots: &'a HashMap<StrId, Vec<StrId>, FxHashBuilder>,
    interface_id_map: &'a HashMap<StrId, usize, FxHashBuilder>,
    interface_method_slots: &'a HashMap<StrId, HashMap<StrId, usize, FxHashBuilder>, FxHashBuilder>,
    classes: &'a HashMap<StrId, HirStruct<'a, 'a>, FxHashBuilder>,

    context: Arc<StringPool>,
    extern_c_names: &'a HashSet<StrId>,
    pub dep_graph: &'a DepGraph,
    pub module_idx: usize,
}

impl<'a, 'bump> FunctionLowerer<'a, 'bump>
where
    'bump: 'a,
{
    pub fn new(
        hir_fn: &HirFunc<'a, 'bump>,
        funcs: &'a HashMap<StrId, Function, FxHashBuilder>,
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
        context: Arc<StringPool>,
        extern_c_names: &'a HashSet<StrId>,
        dep_graph: &'a DepGraph,
        module_idx: usize,
    ) -> Result<Self, std::alloc::AllocError> {
        Self::new_internal(
            hir_fn,
            None,
            funcs,
            class_field_offsets,
            class_method_slots,
            class_mangled_map,
            class_vtable_slots,
            interface_id_map,
            interface_method_slots,
            classes,
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
        context: Arc<StringPool>,
        extern_c_names: &'a HashSet<StrId>,
        dep_graph: &'a DepGraph,
        module_idx: usize,
    ) -> Result<Self, std::alloc::AllocError> {
        Self::new_internal(
            hir_fn,
            Some(class_name),
            funcs,
            class_field_offsets,
            class_method_slots,
            class_mangled_map,
            class_vtable_slots,
            interface_id_map,
            interface_method_slots,
            classes,
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
            class_field_offsets,
            class_method_slots,
            class_mangled_map,
            class_vtable_slots,
            interface_id_map,
            interface_method_slots,
            classes,
            context,
            phantom_data: Default::default(),
            extern_c_names,
            dep_graph,
            module_idx,
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
                    for stmt in body {
                        self.lower_stmt(stmt);
                    }
                }
                _ => panic!(),
            }
        }
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
                    val = self.lower_catch(val, pat);
                }

                if let Some(else_stmts) = else_block {
                    val = self.lower_nullable_unwrap(val, else_stmts);
                }

                self.var_map.insert(name.clone(), val);
            }
            HirStmt::Return(expr) => {
                let value = expr.as_ref().map(|e| {
                    let val = self.allow_lowering_expr(e);
                    Operand::Value(val)
                });
                self.emit(Instruction::Ret { value });
            }
            HirStmt::Expr(expr) => {
                let _ = self.allow_lowering_expr(expr);
            }
            _ => unimplemented!("Statement {:?} not yet lowered", stmt),
        }
    }

    /// Lowers `expr catch { Type a => {}, ... }` (or single-branch form) into a
    /// MatchEnum over the thrown-error tagged union, with one block per branch
    /// and a Phi joining the results.
    fn lower_catch(
        &mut self,
        raw_val: Value,
        pattern: &HirErrorHandlerPattern<'a, 'bump>,
    ) -> Value {
        let branches: Vec<(HirType, Option<StrId>, &[HirStmt])> = match pattern {
            HirErrorHandlerPattern::Single {
                error_type,
                binding,
                body,
            } => {
                vec![(*error_type, *binding, *body)]
            }
            HirErrorHandlerPattern::Multiple { branches } => branches
                .iter()
                .map(|b| (b.error_type, b.binding, b.body))
                .collect(),
        };

        let join_bb = self.current_block_data.fresh_block();
        let mut arms = SmallVec::new();
        let mut incoming = SmallVec::new();

        for (error_type, binding, body) in branches {
            let arm_bb = self.current_block_data.fresh_block();
            // Discriminant tag = the error type's name, per EnumConstruct's
            // (enum_name, variant) convention — see note above on this assumption.
            let tag = match error_type {
                HirType::Struct(name, _) | HirType::Enum(name, _) => name,
                other => panic!("catch branch type {:?} is not a nominal error type", other),
            };
            arms.push((tag, arm_bb));

            self.current_block_data.switch_to(arm_bb);
            if let Some(b) = binding {
                // Bound error payload: loaded out of the tagged union's data slot.
                let payload = self.current_block_data.fresh_value();
                self.emit(Instruction::LoadField {
                    dest: payload,
                    base: Operand::Value(raw_val),
                    offset: 0, // payload offset within the Enum repr — TODO confirm against your layout
                });
                self.var_map.insert(b, payload);
            }
            for s in body {
                self.lower_stmt(s);
            }
            let arm_result = self.current_block_data.fresh_value(); // placeholder result of the arm's tail expr, if catch-as-expr is needed
            self.emit(Instruction::Jump { target: join_bb });
            incoming.push((arm_bb, arm_result));
        }

        self.emit(Instruction::MatchEnum {
            value: raw_val,
            arms,
        });

        self.current_block_data.switch_to(join_bb);
        let dest = self.current_block_data.fresh_value();
        self.emit(Instruction::Phi { dest, incoming });
        dest
    }

    fn lower_nullable_unwrap(&mut self, val: Value, else_stmts: &HirStmt<'a, 'bump>) -> Value {
        let then_bb = self.current_block_data.fresh_block(); // not-null path
        let else_bb = self.current_block_data.fresh_block();
        let join_bb = self.current_block_data.fresh_block();

        if self
            .current_block_data
            .value_type(val)
            .expect("nullable-unwrapped value must have a known SsaType")
            .nullable_pointer_repr()
            .is_some()
        {
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
        } else if self
            .current_block_data
            .value_type(val)
            .expect("nullable-unwrapped value must have a known SsaType")
            .is_tagged_nullable()
        {
            // MatchEnum is itself the terminator here — dispatches on the
            // variant tag directly, no LoadField/Binary/Branch needed.
            self.emit(Instruction::MatchEnum {
                value: val,
                arms: SmallVec::from_buf([
                    (StrId(self.context.intern("null")), else_bb),
                    (StrId(self.context.intern("some")), then_bb),
                ]),
            });
        } else {
            panic!(
                "`? else` used on non-nullable SsaType {:?}",
                self.current_block_data
                    .value_type(val)
                    .expect("nullable-unwrapped value must have a known SsaType")
            );
        }

        self.current_block_data.switch_to(else_bb);
        let HirStmt::Block { body } = else_stmts else {
            unreachable!()
        };
        for s in *body {
            self.lower_stmt(s);
        }
        self.emit(Instruction::Jump { target: join_bb });

        self.current_block_data.switch_to(then_bb);
        let unwrapped = if self
            .current_block_data
            .value_type(val)
            .expect("nullable-unwrapped value must have a known SsaType")
            .nullable_pointer_repr()
            .is_some()
        {
            val
        } else {
            let payload = self.current_block_data.fresh_value();
            self.emit(Instruction::LoadField {
                dest: payload,
                base: Operand::Value(val),
                offset: 0, // still wrong per layout_of_ssa — payload offset, not 0; unresolved
            });
            payload
        };
        self.emit(Instruction::Jump { target: join_bb });

        self.current_block_data.switch_to(join_bb);
        let dest = self.current_block_data.fresh_value();
        self.emit(Instruction::Phi {
            dest,
            incoming: SmallVec::from_buf([(else_bb, unwrapped), (then_bb, unwrapped)]),
        });
        dest
    }

    fn allow_lowering_expr(&mut self, value: &HirExpr<'a, 'bump>) -> Value {
        let mut el = MirExprLowerer::new(
            &mut self.current_block_data,
            &self.funcs,
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
