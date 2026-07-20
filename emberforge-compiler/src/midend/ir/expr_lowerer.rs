use crate::midend::copy_analysis::drop_tracking::{DropMoveState, DropScope, record_move_if_any};
use crate::midend::ir::block_data::CurrentBlockData;
use crate::optimized_string_buffering;
use codex_dependency_graph::DepGraph;
use core::panic;
use ir::hir::{
    AssignmentOperator, HirExpr, HirFieldInit, HirStmt, HirStruct, HirType, Operator, StrId,
};
use ir::ir_conversion::{assign_op_to_bin_op, lower_operator_bin, lower_type_hir};
use ir::ir_hasher::{FxHashBuilder, HashSet};
use ir::layout::TargetInfo;
use ir::ssa_ir::{BinOp, Function, Instruction, Operand, SsaType, Value, cast_kind};
use smallvec::SmallVec;
use std::cell::RefCell;
use std::collections::HashMap;
use std::marker::PhantomData;
use std::sync::Arc;
use zetaruntime::string_pool::StringPool;

/// MIR Expr Lowerer, which converts HIR expressions to MIR expressions.
/// MIR is similar to a higher level representation of assembly which can be optimized in Zeta specific ways.
/// If we are here this means we passed all type safety, memory safety and semantic checks, and we can safely discard of stuff and all debug like span's
pub struct MirExprLowerer<'el, 'f, 'a, 'cx, 'bump> {
    pub current_block_data: &'el mut CurrentBlockData<'f>,
    pub var_map: &'el mut HashMap<StrId, Value, FxHashBuilder>,
    pub context: Arc<StringPool>,
    phantom_data: PhantomData<&'bump ()>,

    pub funcs: &'a HashMap<StrId, Function, FxHashBuilder>,
    pub class_field_offsets:
        &'a HashMap<StrId, HashMap<StrId, usize, FxHashBuilder>, FxHashBuilder>,
    pub class_method_slots: &'a HashMap<StrId, HashMap<StrId, usize, FxHashBuilder>, FxHashBuilder>,
    pub class_mangled_map: &'a HashMap<StrId, HashMap<StrId, StrId, FxHashBuilder>, FxHashBuilder>,
    pub class_vtable_slots: &'a HashMap<StrId, Vec<StrId>, FxHashBuilder>,
    pub interface_id_map: &'a HashMap<StrId, usize, FxHashBuilder>,
    pub interface_method_slots:
        &'a HashMap<StrId, HashMap<StrId, usize, FxHashBuilder>, FxHashBuilder>,
    pub classes: &'a HashMap<StrId, HirStruct<'a, 'a>, FxHashBuilder>,

    pub cx_phantom: PhantomData<&'cx ()>,
    pub extern_c_names: &'a HashSet<StrId>,
    pub dep_graph: &'a RefCell<DepGraph>,
    pub module_idx: usize,
    global_funcs: &'a HashMap<StrId, Function, FxHashBuilder>,
    pub scope_stack: &'el [DropScope],
    pub drop_state: &'el mut DropMoveState,
    pub interface_methods: &'a HashMap<StrId, Vec<(StrId, Vec<SsaType>, SsaType)>, FxHashBuilder>,
}

impl<'el, 'f, 'a, 'cx, 'bump> MirExprLowerer<'el, 'f, 'a, 'cx, 'bump>
where
    'bump: 'a,
{
    #[inline(always)]
    pub fn new(
        current_block_data: &'el mut CurrentBlockData<'f>,
        funcs: &'a HashMap<StrId, Function, FxHashBuilder>,
        global_funcs: &'a HashMap<StrId, Function, FxHashBuilder>,
        var_map: &'el mut HashMap<StrId, Value, FxHashBuilder>,
        context: Arc<StringPool>,
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
        classes: &'a HashMap<StrId, HirStruct<'a, 'bump>, FxHashBuilder>,
        extern_c_names: &'a HashSet<StrId>,
        dep_graph: &'a RefCell<DepGraph>,
        module_idx: usize,
        scope_stack: &'el [DropScope],
        drop_state: &'el mut DropMoveState,
        interface_methods: &'a HashMap<StrId, Vec<(StrId, Vec<SsaType>, SsaType)>, FxHashBuilder>,
    ) -> Self {
        Self {
            current_block_data,
            funcs,
            global_funcs,
            var_map,
            context,
            class_field_offsets,
            class_method_slots,
            class_mangled_map,
            class_vtable_slots,
            interface_id_map,
            interface_method_slots,
            classes,
            phantom_data: PhantomData,
            cx_phantom: PhantomData,
            extern_c_names,
            dep_graph,
            module_idx,
            scope_stack,
            drop_state,
            interface_methods,
        }
    }

    /// Seed `var_map` and `current_block_data.value_types` with known parameters and locals.
    ///
    /// This should be called before lowering a function body. It associates each parameter
    /// and local with a fresh `Value` and registers its SSA type so later lowering can
    /// rely on lookups without panicking.
    pub fn seed_locals_and_params_from_hir(
        &mut self,
        params: &[(StrId, HirType<'a, 'bump>)],
        locals: &[(StrId, HirType<'a, 'bump>)],
    ) {
        for (name, ty) in params.iter().copied() {
            let v = self.current_block_data.fresh_value();
            self.var_map.insert(name.clone(), v);
            self.current_block_data
                .value_types
                .insert(v, lower_type_hir(&ty));
        }

        for (name, ty) in locals.iter().copied() {
            let v = self.current_block_data.fresh_value();
            self.var_map.insert(name.clone(), v);
            self.current_block_data
                .value_types
                .insert(v, lower_type_hir(&ty));
        }
    }

    pub fn lower_expr(&mut self, expr: &HirExpr<'a, 'bump>) -> Value {
        match expr {
            HirExpr::Null(_) => self.lower_expr_null(),
            HirExpr::Number(n, _) => self.lower_expr_number(*n),

            HirExpr::Binary {
                left,
                op,
                right,
                span: _,
            } => self.lower_expr_binary(left, op, right),

            HirExpr::Ident(name, _) => *self.var_map.get(name).unwrap_or_else(|| {
                panic!(
                    "lower_expr: variable {:?} referenced before definition",
                    name
                )
            }),

            HirExpr::StructInit {
                name,
                args,
                span: _,
                type_args: _,
            } => self.lower_class_init(name, args),

            HirExpr::Undefined { span: _, ty } => {
                let ssa_ty = lower_type_hir(ty);
                self.lower_zeroed_value(&ssa_ty)
            }

            HirExpr::FieldAccess {
                object,
                field,
                span: _,
            }
            | HirExpr::Get {
                object,
                field,
                span: _,
            } => self.lower_field_access(object, *field),

            HirExpr::Call {
                callee,
                args,
                span: _,
                type_args: _, // Turns into None after monomorphization
            } => self.lower_call(callee, args),

            HirExpr::InterfaceCall {
                callee,
                args,
                interface,
                span: _,
            } => self.lower_interface_call(callee, args, *interface),

            HirExpr::Assignment {
                target,
                op,
                value,
                span: _,
            } => self.lower_expr_assignment(target, *op, value),

            HirExpr::String(s, _) => {
                let v = self.current_block_data.fresh_value();
                self.emit(Instruction::Const {
                    dest: v,
                    ty: SsaType::String,
                    value: Operand::ConstString(*s),
                });
                self.current_block_data
                    .value_types
                    .insert(v, SsaType::String);
                v
            }

            HirExpr::Boolean(b, _) => {
                let v = self.current_block_data.fresh_value();
                self.emit(Instruction::Const {
                    dest: v,
                    ty: SsaType::I8,
                    value: Operand::ConstInt(if *b { 1 } else { 0 }),
                });
                self.current_block_data.value_types.insert(v, SsaType::I8);
                v
            }

            HirExpr::Decimal(d, _) => {
                let v = self.current_block_data.fresh_value();
                self.emit(Instruction::Const {
                    dest: v,
                    ty: SsaType::F64,
                    value: Operand::ConstFloat(*d),
                });
                self.current_block_data.value_types.insert(v, SsaType::F64);
                v
            }

            HirExpr::Tuple(elements, _) => {
                if elements.is_empty() {
                    let v = self.current_block_data.fresh_value();
                    self.current_block_data.value_types.insert(v, SsaType::I64);
                    v
                } else {
                    self.lower_expr(&elements[0])
                }
            }

            HirExpr::InterpolatedString(_parts) => {
                let v = self.current_block_data.fresh_value();
                let empty_str = self.context.intern("");
                self.emit(Instruction::Const {
                    dest: v,
                    ty: SsaType::String,
                    value: Operand::ConstString(StrId(empty_str)),
                });
                self.current_block_data
                    .value_types
                    .insert(v, SsaType::String);
                v
            }

            HirExpr::EnumInit {
                enum_name,
                variant: _,
                args,
                span: _,
                type_args: _,
            } => {
                let v = self.current_block_data.fresh_value();

                let mut arg_values: Vec<Value> = Vec::with_capacity(args.len());
                for arg in *args {
                    arg_values.push(self.lower_expr(arg));
                }

                self.emit(Instruction::StackAlloc {
                    dest: v,
                    ty: SsaType::User(*enum_name, vec![]),
                    count: 1,
                });

                self.current_block_data
                    .value_types
                    .insert(v, SsaType::User(*enum_name, vec![]));
                v
            }

            HirExpr::ExprList { list, span: _ } => {
                if list.is_empty() {
                    let v = self.current_block_data.fresh_value();
                    self.current_block_data.value_types.insert(v, SsaType::I64);
                    v
                } else {
                    let mut result = self.lower_expr(&list[0]);
                    for expr in &list[1..] {
                        result = self.lower_expr(expr);
                    }
                    result
                }
            }

            HirExpr::Comparison {
                left,
                op,
                right,
                span: _,
            } => {
                let l = self.lower_expr(left);
                let r = self.lower_expr(right);
                let v = self.current_block_data.fresh_value();
                self.emit(Instruction::Binary {
                    dest: v,
                    op: lower_operator_bin(op),
                    left: Operand::Value(l),
                    right: Operand::Value(r),
                });
                self.current_block_data.value_types.insert(v, SsaType::I64);
                v
            }

            HirExpr::This { .. } => {
                let this_name = StrId(self.context.intern("this"));
                let v = *self.var_map.get(&this_name).unwrap();

                v
            }
            HirExpr::Ref { expr, .. } => {
                let (addr, _pointee_ty) = self.lower_place_addr(expr);
                addr
            }

            HirExpr::Deref { expr, .. } => {
                let ptr = self.lower_expr(expr);

                let dest = self.current_block_data.fresh_value();

                self.emit(Instruction::Load {
                    dest,
                    ptr: Operand::Value(ptr),
                });

                let pointee_ty = match self.current_block_data.value_types[&ptr].clone() {
                    SsaType::Pointer(inner) => *inner,
                    other => panic!("cannot dereference {:?}", other),
                };

                self.current_block_data.value_types.insert(dest, pointee_ty);

                dest
            }
            HirExpr::ModuleAccess(hir_module_access) => {
                let mangled = optimized_string_buffering::build_module_scoped_name(
                    hir_module_access.path,
                    hir_module_access.member,
                    None,
                    self.context.clone(),
                );

                let dest = self.current_block_data.fresh_value();
                self.emit(Instruction::Const {
                    dest,
                    ty: SsaType::I64, // TODO: this is a placeholder; refined once type info flows through
                    value: Operand::GlobalRef(mangled),
                });
                self.current_block_data
                    .value_types
                    .insert(dest, SsaType::I64);
                dest
            }
            HirExpr::Lambda { .. } => {
                todo!("There should be no lambdas here so we should handle that error")
            }
            HirExpr::Index {
                object,
                index,
                span: _,
            } => self.lower_index(object, index),
            HirExpr::ArrayLiteral { elements, span: _ } => self.lower_array_literal(elements),
            HirExpr::GenericIdent(..) => unreachable!(),
            HirExpr::Cast {
                expr, target_type, ..
            } => {
                let src = self.lower_expr(expr);

                let src_ty = self.current_block_data.value_types[&src].clone();
                let dst_ty = lower_type_hir(target_type);

                let kind = cast_kind(&src_ty, &dst_ty);

                let dest = self.current_block_data.fresh_value();

                self.emit(Instruction::Cast {
                    dest,
                    value: Operand::Value(src),
                    kind,
                });

                self.current_block_data.value_types.insert(dest, dst_ty);

                dest
            }
            HirExpr::Char(c, _) => {
                let v = self.current_block_data.fresh_value();

                self.emit(Instruction::Const {
                    dest: v,
                    ty: SsaType::Char,
                    value: Operand::ConstInt(*c as u32 as i64),
                });

                self.current_block_data.value_types.insert(v, SsaType::Char);

                v
            }
            HirExpr::Intrinsic {
                kind,
                type_args,
                args,
                span: _,
            } => {
                use ir::hir::IntrinsicKind;
                use ir::ssa_ir::IntrinsicOp;

                match kind {
                    IntrinsicKind::SizeOf | IntrinsicKind::AlignOf | IntrinsicKind::TypeName => {
                        let query_ty = lower_type_hir(&type_args[0]);
                        let op = match kind {
                            IntrinsicKind::SizeOf => IntrinsicOp::SizeOf,
                            IntrinsicKind::AlignOf => IntrinsicOp::AlignOf,
                            IntrinsicKind::TypeName => IntrinsicOp::TypeName,
                            _ => unreachable!(),
                        };

                        let dest = self.current_block_data.fresh_value();
                        self.emit(Instruction::Intrinsic {
                            dest: Some(dest),
                            op,
                            query_ty: Some(query_ty),
                            args: SmallVec::new(),
                        });

                        let result_ty = match kind {
                            IntrinsicKind::SizeOf | IntrinsicKind::AlignOf => SsaType::Usize,
                            IntrinsicKind::TypeName => SsaType::String,
                            _ => unreachable!(),
                        };
                        self.current_block_data.value_types.insert(dest, result_ty);
                        dest
                    }

                    IntrinsicKind::AssertAlign => {
                        let ptr_val = self.lower_expr(&args[0]);
                        let align_val = self.lower_expr(&args[1]);

                        // mask = align - 1; misaligned if (ptr & mask) != 0
                        let one = self.current_block_data.fresh_value();
                        self.emit(Instruction::Const {
                            dest: one,
                            ty: SsaType::Usize,
                            value: Operand::ConstInt(1),
                        });
                        self.current_block_data
                            .value_types
                            .insert(one, SsaType::Usize);

                        let mask = self.current_block_data.fresh_value();
                        self.emit(Instruction::Binary {
                            dest: mask,
                            op: BinOp::Sub,
                            left: Operand::Value(align_val),
                            right: Operand::Value(one),
                        });
                        self.current_block_data
                            .value_types
                            .insert(mask, SsaType::Usize);

                        let masked = self.current_block_data.fresh_value();
                        self.emit(Instruction::Binary {
                            dest: masked,
                            op: BinOp::BitAnd,
                            left: Operand::Value(ptr_val),
                            right: Operand::Value(mask),
                        });
                        self.current_block_data
                            .value_types
                            .insert(masked, SsaType::Usize);

                        let zero = self.current_block_data.fresh_value();
                        self.emit(Instruction::Const {
                            dest: zero,
                            ty: SsaType::Usize,
                            value: Operand::ConstInt(0),
                        });
                        self.current_block_data
                            .value_types
                            .insert(zero, SsaType::Usize);

                        let is_misaligned = self.current_block_data.fresh_value();
                        self.emit(Instruction::Binary {
                            dest: is_misaligned,
                            op: BinOp::Ne,
                            left: Operand::Value(masked),
                            right: Operand::Value(zero),
                        });
                        self.current_block_data
                            .value_types
                            .insert(is_misaligned, SsaType::Bool);

                        let panic_bb = self.current_block_data.new_block();
                        let cont_bb = self.current_block_data.new_block();

                        self.emit(Instruction::Branch {
                            cond: Operand::Value(is_misaligned),
                            then_bb: panic_bb,
                            else_bb: cont_bb,
                        });

                        self.current_block_data.switch_to(panic_bb);
                        let msg = self.current_block_data.fresh_value();
                        let msg_str = self.context.intern("alignment assertion failed");
                        self.emit(Instruction::Const {
                            dest: msg,
                            ty: SsaType::String,
                            value: Operand::ConstString(StrId(msg_str)),
                        });
                        self.current_block_data
                            .value_types
                            .insert(msg, SsaType::String);
                        self.emit(Instruction::Panic {
                            message: Operand::Value(msg),
                        });

                        self.current_block_data.switch_to(cont_bb);
                        let dest = self.current_block_data.fresh_value();
                        self.current_block_data
                            .value_types
                            .insert(dest, SsaType::Void);
                        dest
                    }
                }
            }
            HirExpr::UnknownIntrinsic { .. } => unreachable!(),
            HirExpr::If { if_stmt, span: _ } => {
                let HirStmt::If {
                    cond,
                    then_block,
                    else_block,
                } = *if_stmt
                else {
                    unreachable!("HirExpr::If must always wrap HirStmt::If")
                };
                self.lower_if_expr(&cond, then_block, *else_block)
            }
        }
    }

    fn lower_if_expr(
        &mut self,
        condition: &HirExpr<'a, 'bump>,
        then_block: &[HirStmt<'a, 'bump>],
        else_block: Option<&'bump HirStmt<'a, 'bump>>,
    ) -> Value {
        let cond = self.lower_expr(condition);

        let then_bb = self.current_block_data.new_block();
        let else_bb = self.current_block_data.new_block();
        let merge_bb = self.current_block_data.new_block();

        self.emit(Instruction::Branch {
            cond: Operand::Value(cond),
            then_bb,
            else_bb,
        });

        // then
        self.current_block_data.switch_to(then_bb);
        let then_val = self.lower_block_value(then_block);
        let then_end = self.current_block_data.current_block;
        let then_terminated = self.block_terminated();
        if !then_terminated {
            self.emit(Instruction::Jump { target: merge_bb });
        }

        // else
        self.current_block_data.switch_to(else_bb);
        let else_val = match else_block {
            Some(HirStmt::Block { body }) => self.lower_block_value(body),
            Some(HirStmt::If {
                cond: ec,
                then_block: etb,
                else_block: eeb,
            }) => self.lower_if_expr(ec, etb, *eeb),
            Some(other) => panic!(
                "if-expression else-arm must be a block or else-if, found {:?}: \
                 every path through an if used as an expression needs a value",
                other
            ),
            None => panic!(
                "if-expression used without an else arm; the type checker should \
                 have caught this before MIR lowering"
            ),
        };
        let else_end = self.current_block_data.current_block;
        let else_terminated = self.block_terminated();
        if !else_terminated {
            self.emit(Instruction::Jump { target: merge_bb });
        }

        // both arms diverged, nothing reaches merge_bb, so there's no real value
        if then_terminated && else_terminated {
            return self.unreachable_value();
        }

        self.current_block_data.switch_to(merge_bb);

        let result = self.current_block_data.fresh_value();
        let mut incoming = SmallVec::new();
        if !then_terminated {
            incoming.push((then_end, then_val));
        }
        if !else_terminated {
            incoming.push((else_end, else_val));
        }
        self.emit(Instruction::Phi {
            dest: result,
            incoming,
        });

        let ty = self
            .value_type(if !then_terminated { then_val } else { else_val })
            .cloned()
            .unwrap_or(SsaType::Void);
        self.current_block_data.value_types.insert(result, ty);

        result
    }

    fn lower_block_value(&mut self, stmts: &[HirStmt<'a, 'bump>]) -> Value {
        if stmts.is_empty() {
            return self.unit_value();
        }
        let (last, rest) = stmts.split_last().unwrap();
        for stmt in rest {
            self.lower_block_stmt_for_effect(stmt);
        }
        match last {
            HirStmt::Expr(e) => self.lower_expr(e),
            other => {
                self.lower_block_stmt_for_effect(other);
                self.unit_value()
            }
        }
    }

    // TODO replace with real `lower_stmt` for highest reusability and flexibility
    fn lower_block_stmt_for_effect(&mut self, stmt: &HirStmt<'a, 'bump>) {
        match stmt {
            HirStmt::Expr(e) => {
                self.lower_expr(e);
            }
            HirStmt::Let { name, value, .. } => {
                let v = self.lower_expr(value);
                self.var_map.insert(*name, v);
            }
            other => panic!(
                "lower_block_stmt_for_effect: {:?} inside an if-expression arm isn't \
                 wired up yet, needs the real statement lowerer",
                other
            ),
        }
    }

    fn block_terminated(&mut self) -> bool {
        self.current_block_data
            .bb()
            .instructions
            .last()
            .map_or(false, |i| ir::ssa_ir::inst_is_terminator(i))
    }

    fn unit_value(&mut self) -> Value {
        let v = self.current_block_data.fresh_value();
        self.current_block_data.value_types.insert(v, SsaType::Void);
        v
    }

    fn unreachable_value(&mut self) -> Value {
        let v = self.current_block_data.fresh_value();
        self.current_block_data.value_types.insert(v, SsaType::Void);
        v
    }

    fn value_type(&self, v: Value) -> Option<&SsaType> {
        self.current_block_data.value_types.get(&v)
    }

    fn lower_index_addr(
        &mut self,
        object: &HirExpr<'a, 'bump>,
        index: &HirExpr<'a, 'bump>,
    ) -> (Value, SsaType) {
        let base = self.lower_expr(object);
        let idx = self.lower_expr(index);

        let base_ty = self
            .current_block_data
            .value_types
            .get(&base)
            .cloned()
            .expect("lower_index_addr: base value has no known type");

        let elem_ty = match &base_ty {
            SsaType::Pointer(inner) => (**inner).clone(),
            SsaType::Array(inner, _) => (**inner).clone(),
            SsaType::Slice(inner) => (**inner).clone(),
            other => panic!("lower_index_addr: cannot index into {:?}", other),
        };

        let elem_size = ir::layout::sizeof_ssa(&elem_ty, TargetInfo { ptr_bytes: 8 })
            .expect("lower_index_addr: element type has no known size")
            as i64;

        let size_v = self.current_block_data.fresh_value();
        self.emit(Instruction::Const {
            dest: size_v,
            ty: SsaType::I64,
            value: Operand::ConstInt(elem_size),
        });
        self.current_block_data
            .value_types
            .insert(size_v, SsaType::I64);

        let offset_v = self.current_block_data.fresh_value();
        self.emit(Instruction::Binary {
            dest: offset_v,
            op: BinOp::Mul,
            left: Operand::Value(idx),
            right: Operand::Value(size_v),
        });
        self.current_block_data
            .value_types
            .insert(offset_v, SsaType::I64);

        let addr_v = self.current_block_data.fresh_value();
        self.emit(Instruction::Binary {
            dest: addr_v,
            op: BinOp::Add,
            left: Operand::Value(base),
            right: Operand::Value(offset_v),
        });
        self.current_block_data
            .value_types
            .insert(addr_v, SsaType::Pointer(Box::new(elem_ty.clone())));

        (addr_v, elem_ty)
    }

    fn lower_index(&mut self, object: &HirExpr<'a, 'bump>, index: &HirExpr<'a, 'bump>) -> Value {
        let (addr_v, elem_ty) = self.lower_index_addr(object, index);

        let dest = self.current_block_data.fresh_value();
        self.emit(Instruction::Load {
            dest,
            ptr: Operand::Value(addr_v),
        });
        self.current_block_data.value_types.insert(dest, elem_ty);

        dest
    }

    fn try_flatten_module_path(&self, expr: &HirExpr<'a, 'bump>) -> Option<StrId> {
        match expr {
            HirExpr::ModuleAccess(acc) => {
                Some(self.resolve_module_qualified_name(acc.path, acc.member, None))
            }
            HirExpr::FieldAccess { object, field, .. } | HirExpr::Get { object, field, .. } => {
                if let HirExpr::ModuleAccess(acc) = object {
                    Some(self.resolve_module_qualified_name(acc.path, acc.member, Some(*field)))
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    /// Resolves a module-qualified reference to its correct call-site symbol.
    fn resolve_module_qualified_name(
        &self,
        path: &[StrId],
        member: StrId,
        extra: Option<StrId>,
    ) -> StrId {
        let bare_name = extra.unwrap_or(member);

        if self.extern_c_names.contains(&bare_name) {
            return bare_name;
        }

        match extra {
            // Method call through a module-qualified class: `path::ClassName.method()`.
            // Registration mangles as [ClassName, ...path] + method, so mirror that
            // order here rather than passing class_name/method through as
            // build_module_scoped_name's member/extra slots.
            Some(method_name) => {
                let mut segments: Vec<StrId> = Vec::with_capacity(path.len() + 1);
                segments.push(member); // class name
                segments.extend_from_slice(path);
                optimized_string_buffering::build_module_scoped_name(
                    &segments,
                    method_name,
                    None,
                    self.context.clone(),
                )
            }
            // Plain module-qualified free function/global: `path::name`.
            None => optimized_string_buffering::build_module_scoped_name(
                path,
                member,
                None,
                self.context.clone(),
            ),
        }
    }

    fn lower_array_literal(&mut self, elements: &[HirExpr<'a, 'bump>]) -> Value {
        let elem_values: Vec<Value> = elements.iter().map(|e| self.lower_expr(e)).collect();

        let elem_ty = self
            .current_block_data
            .value_types
            .get(&elem_values[0])
            .cloned()
            .expect("lower_array_literal: element value has no known type");

        let elem_size = ir::layout::sizeof_ssa(&elem_ty, TargetInfo { ptr_bytes: 8 })
            .expect("lower_array_literal: element type has no known size")
            as i64;

        let arr_v = self.current_block_data.fresh_value();
        self.emit(Instruction::StackAlloc {
            dest: arr_v,
            ty: elem_ty.clone(),
            count: elem_values.len(),
        });
        self.current_block_data
            .value_types
            .insert(arr_v, SsaType::Pointer(Box::new(elem_ty.clone())));

        for (i, val) in elem_values.into_iter().enumerate() {
            let addr_v = self.current_block_data.fresh_value();
            self.emit(Instruction::FieldAddr {
                dest: addr_v,
                base: Operand::Value(arr_v),
                offset: (i as i64 * elem_size) as usize,
            });
            self.current_block_data
                .value_types
                .insert(addr_v, SsaType::Pointer(Box::new(elem_ty.clone())));

            self.emit(Instruction::Store {
                ptr: Operand::Value(addr_v),
                value: Operand::Value(val),
            });
        }

        arr_v
    }

    /// Recursively materializes a zero value of `ssa_ty`. For scalars this is
    /// a plain `Const 0`. For arrays/structs/tuples, it stack-allocates and
    /// stores a recursively-zeroed value into every field/element. Typechecker
    /// guarantees (`is_zeroable`) that this is only ever called on types where
    /// this is semantically valid, bool/char/enum/interface/pointer types
    /// never reach here.
    fn lower_zeroed_value(&mut self, ssa_ty: &SsaType) -> Value {
        match ssa_ty {
            SsaType::Array(inner, len) => {
                let dest = self.current_block_data.fresh_value();
                self.emit(Instruction::StackAlloc {
                    dest,
                    ty: (**inner).clone(),
                    count: *len,
                });
                self.current_block_data
                    .value_types
                    .insert(dest, SsaType::Pointer(inner.clone()));

                let elem_size = ir::layout::sizeof_ssa(inner, TargetInfo { ptr_bytes: 8 })
                    .expect("lower_zeroed_value: element type has no known size")
                    as i64;

                for i in 0..*len {
                    let zero_v = self.lower_zeroed_value(inner);
                    let addr_v = self.current_block_data.fresh_value();
                    self.emit(Instruction::FieldAddr {
                        dest: addr_v,
                        base: Operand::Value(dest),
                        offset: (i as i64 * elem_size) as usize,
                    });
                    self.current_block_data
                        .value_types
                        .insert(addr_v, SsaType::Pointer(inner.clone()));
                    self.emit(Instruction::Store {
                        ptr: Operand::Value(addr_v),
                        value: Operand::Value(zero_v),
                    });
                }

                dest
            }

            SsaType::User(_, field_types) => {
                let dest = self.current_block_data.fresh_value();
                self.emit(Instruction::StackAlloc {
                    dest,
                    ty: ssa_ty.clone(),
                    count: 1,
                });
                self.current_block_data
                    .value_types
                    .insert(dest, SsaType::Pointer(Box::new(ssa_ty.clone())));

                let mut offset = 0usize;
                for field_ty in field_types {
                    let zero_v = self.lower_zeroed_value(field_ty);
                    let addr_v = self.current_block_data.fresh_value();
                    self.emit(Instruction::FieldAddr {
                        dest: addr_v,
                        base: Operand::Value(dest),
                        offset,
                    });
                    self.current_block_data
                        .value_types
                        .insert(addr_v, SsaType::Pointer(Box::new(field_ty.clone())));
                    self.emit(Instruction::Store {
                        ptr: Operand::Value(addr_v),
                        value: Operand::Value(zero_v),
                    });
                    offset += ir::layout::sizeof_ssa(field_ty, TargetInfo { ptr_bytes: 8 })
                        .expect("lower_zeroed_value: field type has no known size");
                }

                dest
            }

            SsaType::Tuple(elem_types) => {
                let dest = self.current_block_data.fresh_value();
                self.emit(Instruction::StackAlloc {
                    dest,
                    ty: ssa_ty.clone(),
                    count: 1,
                });
                self.current_block_data
                    .value_types
                    .insert(dest, SsaType::Pointer(Box::new(ssa_ty.clone())));

                let mut offset = 0usize;
                for elem_ty in elem_types {
                    let zero_v = self.lower_zeroed_value(elem_ty);
                    let addr_v = self.current_block_data.fresh_value();
                    self.emit(Instruction::FieldAddr {
                        dest: addr_v,
                        base: Operand::Value(dest),
                        offset,
                    });
                    self.current_block_data
                        .value_types
                        .insert(addr_v, SsaType::Pointer(Box::new(elem_ty.clone())));
                    self.emit(Instruction::Store {
                        ptr: Operand::Value(addr_v),
                        value: Operand::Value(zero_v),
                    });
                    offset += ir::layout::sizeof_ssa(elem_ty, TargetInfo { ptr_bytes: 8 })
                        .expect("lower_zeroed_value: tuple element type has no known size");
                }

                dest
            }

            // Scalars: I8..F64 etc.
            _ => {
                let dest = self.current_block_data.fresh_value();
                self.emit(Instruction::Const {
                    dest,
                    ty: ssa_ty.clone(),
                    value: Operand::ConstInt(0),
                });
                self.current_block_data
                    .value_types
                    .insert(dest, ssa_ty.clone());
                dest
            }
        }
    }

    fn lower_expr_assignment(
        &mut self,
        target: &HirExpr<'a, 'bump>,
        op: AssignmentOperator,
        value: &HirExpr<'a, 'bump>,
    ) -> Value {
        let rhs = self.lower_expr(value);

        match target {
            HirExpr::Ident(name, _) => self.handle_ident(op, rhs, *name),

            HirExpr::FieldAccess {
                object,
                field,
                span: _,
            }
            | HirExpr::Get {
                object,
                field,
                span: _,
            } => self.handle_field_access(op, rhs, object, *field),

            HirExpr::Deref { expr, span: _ } => {
                let ptr = self.lower_expr(expr);
                self.handle_deref_assign(ptr, rhs, op)
            }

            HirExpr::Index { object, index, .. } => {
                let (addr_v, elem_ty) = self.lower_index_addr(object, index);

                let rhs_v = match op {
                    AssignmentOperator::Assign => self.lower_expr(value),
                    _ => {
                        // compound assignment: arr[i] += x  =>  load, binop, store
                        let cur = self.current_block_data.fresh_value();
                        self.emit(Instruction::Load {
                            dest: cur,
                            ptr: Operand::Value(addr_v),
                        });
                        self.current_block_data
                            .value_types
                            .insert(cur, elem_ty.clone());

                        let rhs = self.lower_expr(value);
                        let result = self.current_block_data.fresh_value();
                        self.emit(Instruction::Binary {
                            dest: result,
                            op: assign_op_to_bin_op(op), // strips the "Assign" suffix to base op
                            left: Operand::Value(cur),
                            right: Operand::Value(rhs),
                        });
                        self.current_block_data
                            .value_types
                            .insert(result, elem_ty.clone());
                        result
                    }
                };

                self.emit(Instruction::Store {
                    ptr: Operand::Value(addr_v),
                    value: Operand::Value(rhs_v),
                });

                rhs_v
            }

            _ => unimplemented!("Assignment target {:?} not yet supported", target),
        }
    }

    fn lower_expr_null(&mut self) -> Value {
        let v = self.current_block_data.fresh_value();
        self.emit(Instruction::Const {
            dest: v,
            ty: SsaType::I64,
            value: Operand::ConstInt(0),
        });
        self.current_block_data.value_types.insert(v, SsaType::Null);
        v
    }

    fn handle_ident(&mut self, op: AssignmentOperator, rhs: Value, name: StrId) -> Value {
        let var_val = *self.var_map.get(&name).unwrap_or_else(|| {
            panic!(
                "handle_ident: variable {:?} referenced before definition",
                name
            )
        });

        let result = match op {
            AssignmentOperator::Assign => rhs,
            AssignmentOperator::AddAssign
            | AssignmentOperator::SubtractAssign
            | AssignmentOperator::MultiplyAssign
            | AssignmentOperator::DivideAssign
            | AssignmentOperator::ModuloAssign
            | AssignmentOperator::BitAndAssign
            | AssignmentOperator::BitOrAssign
            | AssignmentOperator::BitXorAssign
            | AssignmentOperator::ShiftLeftAssign
            | AssignmentOperator::ShiftRightAssign => {
                let dest = self.new_value();
                let bin_op = assign_op_to_bin_op(op);

                self.emit(Instruction::Binary {
                    dest,
                    op: bin_op,
                    left: Operand::Value(var_val),
                    right: Operand::Value(rhs),
                });

                let result_ty = self
                    .current_block_data
                    .value_types
                    .get(&var_val)
                    .cloned()
                    .or_else(|| self.current_block_data.value_types.get(&rhs).cloned())
                    .unwrap_or(SsaType::I64);

                self.current_block_data.value_types.insert(dest, result_ty);

                dest
            }
        };

        self.var_map.insert(name.clone(), result);
        result
    }

    fn handle_field_access(
        &mut self,
        op: AssignmentOperator,
        rhs: Value,
        object: &HirExpr<'a, 'bump>,
        field: StrId,
    ) -> Value {
        // Module-qualified static field: `zeta::io::files.File.DEFAULT`
        if let Some(mangled) = self.try_flatten_module_path(&HirExpr::FieldAccess {
            object,
            field,
            span: Default::default(),
        }) {
            let dest = self.current_block_data.fresh_value();
            self.emit(Instruction::Const {
                dest,
                ty: SsaType::I64,
                value: Operand::GlobalRef(mangled),
            });
            self.current_block_data
                .value_types
                .insert(dest, SsaType::I64);
            return dest;
        }

        let obj_val = self.lower_expr(object);
        let field_offset = self.get_field_offset(&obj_val, field);

        let new_value = match op {
            AssignmentOperator::Assign => rhs,
            _ => {
                let current = self.new_value();
                self.emit(Instruction::LoadField {
                    dest: current,
                    base: Operand::Value(obj_val),
                    offset: field_offset,
                });

                let _ = self
                    .current_block_data
                    .value_types
                    .entry(current)
                    .or_insert(SsaType::I64);

                let bin_op = assign_op_to_bin_op(op);

                let dest = self.new_value();
                self.emit(Instruction::Binary {
                    dest,
                    op: bin_op,
                    left: Operand::Value(current),
                    right: Operand::Value(rhs),
                });

                let res_ty = self
                    .current_block_data
                    .value_types
                    .get(&current)
                    .cloned()
                    .unwrap_or(SsaType::I64);

                self.current_block_data.value_types.insert(dest, res_ty);

                dest
            }
        };

        self.emit(Instruction::StoreField {
            base: Operand::Value(obj_val),
            offset: field_offset,
            value: Operand::Value(new_value),
        });

        new_value
    }

    fn lower_expr_number(&mut self, n: i64) -> Value {
        let v = self.current_block_data.fresh_value();
        self.emit(Instruction::Const {
            dest: v,
            ty: SsaType::I64,
            value: Operand::ConstInt(n),
        });

        self.current_block_data.value_types.insert(v, SsaType::I64);
        v
    }

    fn lower_expr_binary(
        &mut self,
        left: &HirExpr<'a, 'bump>,
        op: &Operator,
        right: &HirExpr<'a, 'bump>,
    ) -> Value {
        let l = self.lower_expr(left);
        let r = self.lower_expr(right);
        let v = self.current_block_data.fresh_value();
        self.emit(Instruction::Binary {
            dest: v,
            op: lower_operator_bin(op),
            left: Operand::Value(l),
            right: Operand::Value(r),
        });
        self.current_block_data.value_types.insert(v, SsaType::I64);
        v
    }

    fn lower_class_init(&mut self, name: &HirExpr, args: &[HirFieldInit<'a, 'bump>]) -> Value {
        let class_name = match name {
            HirExpr::Ident(n, _) => *n,
            other => panic!("ClassInit name must be identifier; got {:?}", other),
        };

        let obj = self.new_value();

        // Moved up from after the arg-lowering loop so field types are known
        // before checking whether each positional arg moves into its field.
        let field_types: Vec<SsaType> = if let Some(hir_class) = self.classes.get(&class_name) {
            hir_class
                .fields
                .iter()
                .map(|f| lower_type_hir(&f.field_type))
                .collect()
        } else {
            panic!("Class {} not found", class_name)
        };

        let mut arg_values: Vec<Value> = Vec::with_capacity(args.len());
        for (i, arg) in args.iter().enumerate() {
            // Assigning a droppable struct value into a field consumes the
            // source place, same as passing it to a by-value param.
            if matches!(field_types.get(i), Some(SsaType::User(_, _))) {
                // TODO: make arg.value in a way where it ignores position
                record_move_if_any(self.scope_stack, self.drop_state, &arg.value);
            }
            arg_values.push(self.lower_expr(&arg.value));
        }

        let alloc_ty = SsaType::User(class_name, field_types.clone());

        self.emit(Instruction::StackAlloc {
            dest: obj,
            ty: alloc_ty.clone(),
            count: 0,
        });

        self.current_block_data.value_types.insert(obj, alloc_ty);

        if !arg_values.is_empty() {
            self.init_class_fields_from_args(obj, class_name, &arg_values);
        }

        self.store_vtable_if_any(obj, class_name);
        obj
    }

    fn init_class_fields_from_args(&mut self, obj: Value, class_name: StrId, args: &Vec<Value>) {
        let offsets = self
            .class_field_offsets
            .get(&class_name)
            .unwrap_or_else(|| {
                panic!(
                    "Unknown class {} when initializing",
                    self.context.resolve_string(&*class_name)
                )
            });

        let mut fields_by_offset: Vec<(&StrId, &usize)> = offsets.iter().collect();
        fields_by_offset.sort_by_key(|(_, off)| *off);

        for (i, (field_name, _)) in fields_by_offset.iter().enumerate() {
            if i >= args.len() {
                break;
            }

            self.emit(Instruction::StoreField {
                base: Operand::Value(obj),
                offset: offsets.get(*field_name).copied().unwrap(),
                value: Operand::Value(args[i]),
            });
        }
    }

    fn store_vtable_if_any(&mut self, obj: Value, class_name: StrId) {
        let Some(vslots) = self.class_vtable_slots.get(&class_name) else {
            return;
        };
        if vslots.is_empty() {
            return;
        }

        let vtable_name =
            optimized_string_buffering::make_vtable_name(class_name, self.context.clone());
        self.emit(Instruction::StoreField {
            base: Operand::Value(obj),
            offset: 0usize,
            value: Operand::GlobalRef(vtable_name),
        });
    }

    fn lower_field_access(&mut self, object: &HirExpr<'a, 'bump>, field: StrId) -> Value {
        let obj_val = self.lower_expr_as_receiver(object);

        let cls_name = match self.current_block_data.value_types.get(&obj_val) {
            Some(SsaType::User(name, _)) => {
                let resolved = self.context.resolve_string(name);
                StrId(self.context.intern(resolved))
            }
            Some(SsaType::Pointer(inner)) => {
                if let SsaType::User(name, _) = inner.as_ref() {
                    let resolved = self.context.resolve_string(name);
                    StrId(self.context.intern(resolved))
                } else {
                    panic!("FieldAccess through pointer to non-User type: {:?}", inner)
                }
            }
            other => panic!(
                "Could not determine object's class for FieldAccess: {:?}",
                other
            ),
        };

        let offsets = self
            .class_field_offsets
            .get(&cls_name)
            .unwrap_or_else(|| panic!("Unknown class {} in FieldAccess", cls_name));

        let offset = *offsets
            .get(&field)
            .unwrap_or_else(|| panic!("Unknown field {} on class {}", field, cls_name));

        let dest = self.new_value();
        self.emit(Instruction::LoadField {
            dest,
            base: Operand::Value(obj_val),
            offset,
        });

        let field_type = self
            .classes
            .get(&cls_name)
            .and_then(|hir_class| hir_class.fields.iter().find(|f| f.name == field))
            .map(|hir_field| lower_type_hir(&hir_field.field_type))
            .unwrap_or_else(|| {
                eprintln!(
                    "WARNING: lower_field_access could not find field {:?} on class {:?}, defaulting to I64",
                    field, cls_name
                );
                SsaType::I64
            });
        self.current_block_data.value_types.insert(dest, field_type);

        dest
    }

    fn lower_call(&mut self, callee: &HirExpr<'a, 'bump>, args: &[HirExpr<'a, 'bump>]) -> Value {
        if let Some(mangled) = self.try_flatten_module_path(callee) {
            let param_types: Vec<SsaType> = self
                .funcs
                .get(&mangled)
                .or_else(|| self.global_funcs.get(&mangled))
                .map(|f| f.params.iter().map(|(_, ty)| ty.clone()).collect())
                .unwrap_or_default();

            let arg_ops: SmallVec<Operand, 8> = args
                .iter()
                .enumerate()
                .map(|(i, a)| {
                    if matches!(param_types.get(i), Some(SsaType::User(_, _))) {
                        record_move_if_any(self.scope_stack, self.drop_state, a);
                    }
                    Operand::Value(self.lower_expr(a))
                })
                .collect();

            let dest = self.current_block_data.fresh_value();
            self.emit(Instruction::Call {
                dest: Some(dest),
                func: Operand::FunctionRef(mangled),
                args: arg_ops,
            });

            let ret_ty = self.funcs
                .get(&mangled)
                .or_else(|| self.global_funcs.get(&mangled))
                .map(|f| f.ret_type.clone())
                .unwrap_or_else(|| {
                    panic!(
                        "lower_call: unknown non-extern function `{:?}`, not in funcs table or global_funcs",
                        mangled
                    )
                });
            self.current_block_data.value_types.insert(dest, ret_ty);
            return dest;
        }

        match callee {
            HirExpr::Ident(fname, _) => {
                let param_types: Vec<SsaType> = self
                    .funcs
                    .get(fname)
                    .or_else(|| self.global_funcs.get(fname))
                    .map(|f| f.params.iter().map(|(_, ty)| ty.clone()).collect())
                    .unwrap_or_default();

                let arg_ops: SmallVec<Operand, 8> = args
                    .iter()
                    .enumerate()
                    .map(|(i, a)| {
                        if matches!(param_types.get(i), Some(SsaType::User(_, _))) {
                            record_move_if_any(self.scope_stack, self.drop_state, a);
                        }
                        Operand::Value(self.lower_expr(a))
                    })
                    .collect();

                let dest = self.current_block_data.fresh_value();
                self.emit(Instruction::Call {
                    dest: Some(dest),
                    func: Operand::FunctionRef(fname.clone()),
                    args: arg_ops,
                });

                let ret_ty = self
                    .funcs
                    .get(fname)
                    .or_else(|| self.global_funcs.get(fname))
                    .map(|f| f.ret_type.clone())
                    .unwrap_or_else(|| {
                        panic!(
                            "lower_call: unknown function `{:?}`, not in funcs table",
                            fname
                        )
                    });
                self.current_block_data.value_types.insert(dest, ret_ty);

                dest
            }

            HirExpr::FieldAccess {
                object,
                field,
                span: _,
            }
            | HirExpr::Get {
                object,
                field,
                span: _,
            } => self.lower_method_call(object, *field, args),

            other => unimplemented!(
                "Non-identifier callee not yet supported in Call: {:?}",
                other
            ),
        }
    }

    fn lower_place_addr(&mut self, expr: &HirExpr<'a, 'bump>) -> (Value, SsaType) {
        match expr {
            HirExpr::Index {
                object,
                index,
                span: _,
            } => self.lower_index_addr(object, index),

            HirExpr::FieldAccess {
                object,
                field,
                span: _,
            }
            | HirExpr::Get {
                object,
                field,
                span: _,
            } => self.lower_field_addr(object, *field),

            HirExpr::Deref {
                expr: inner,
                span: _,
            } => {
                // &*ptr == ptr; no load needed, the pointer value already is
                // the address.
                let ptr = self.lower_expr(inner);
                let pointee_ty = match self.current_block_data.value_types.get(&ptr).cloned() {
                    Some(SsaType::Pointer(inner_ty)) => *inner_ty,
                    other => panic!("lower_place_addr: Deref of non-pointer {:?}", other),
                };
                (ptr, pointee_ty)
            }

            // Whole-variable refs: if the value is already a pointer (arrays,
            // structs, anything StackAlloc'd), that pointer IS the address.
            // Scalar locals that were never stack-allocated have no address
            // to take.
            other => {
                let val = self.lower_expr(other);
                match self.current_block_data.value_types.get(&val).cloned() {
                    Some(SsaType::Pointer(inner_ty)) => (val, *inner_ty),
                    Some(ty) => panic!(
                        "lower_place_addr: cannot take address of a non-pointer-backed \
                         value of type {:?}, scalar locals must be stack-allocated to \
                         be referenced, not yet implemented",
                        ty
                    ),
                    None => panic!("lower_place_addr: value has no known type"),
                }
            }
        }
    }

    fn lower_field_addr(&mut self, object: &HirExpr<'a, 'bump>, field: StrId) -> (Value, SsaType) {
        let obj_val = self.lower_expr_as_receiver(object);

        let cls_name = match self.current_block_data.value_types.get(&obj_val) {
            Some(SsaType::User(name, _)) => *name,
            Some(SsaType::Pointer(inner)) => match inner.as_ref() {
                SsaType::User(name, _) => *name,
                _ => panic!("lower_field_addr: pointer to non-User type: {:?}", inner),
            },
            other => panic!(
                "lower_field_addr: could not determine object's class: {:?}",
                other
            ),
        };

        let offsets = self
            .class_field_offsets
            .get(&cls_name)
            .unwrap_or_else(|| panic!("Unknown class {} in FieldAccess", cls_name));
        let offset = *offsets
            .get(&field)
            .unwrap_or_else(|| panic!("Unknown field {} on class {}", field, cls_name));

        let field_ty = self
            .classes
            .get(&cls_name)
            .and_then(|hc| hc.fields.iter().find(|f| f.name == field))
            .map(|f| lower_type_hir(&f.field_type))
            .unwrap_or(SsaType::I64);

        let addr = self.current_block_data.fresh_value();
        self.emit(Instruction::FieldAddr {
            dest: addr,
            base: Operand::Value(obj_val),
            offset,
        });
        self.current_block_data
            .value_types
            .insert(addr, SsaType::Pointer(Box::new(field_ty.clone())));

        (addr, field_ty)
    }

    fn lower_method_call(
        &mut self,
        object: &HirExpr<'a, 'bump>,
        field: StrId,
        args: &[HirExpr<'a, 'bump>],
    ) -> Value {
        if let HirExpr::Ident(scope_name, _) = object {
            if !self.var_map.contains_key(scope_name) {
                let direct_name: StrId = optimized_string_buffering::mangle_method_name(
                    self.dep_graph,
                    self.module_idx,
                    *scope_name,
                    field,
                    self.context.clone(),
                );

                let param_types: Vec<SsaType> = self
                    .funcs
                    .get(&direct_name)
                    .map(|f| f.params.iter().map(|(_, ty)| ty.clone()).collect())
                    .unwrap_or_default();

                let mut operands: SmallVec<Operand, 8> = SmallVec::new();
                for (i, a) in args.iter().enumerate() {
                    if matches!(param_types.get(i), Some(SsaType::User(_, _))) {
                        record_move_if_any(self.scope_stack, self.drop_state, a);
                    }
                    operands.push(Operand::Value(self.lower_expr(a)));
                }

                let dest: Value = self.current_block_data.fresh_value();
                self.emit(Instruction::Call {
                    dest: Some(dest),
                    func: Operand::FunctionRef(direct_name),
                    args: operands,
                });

                self.current_block_data
                    .value_types
                    .insert(dest, SsaType::I64);
                return dest;
            }
        }

        let obj_val: Value = self.lower_expr_as_receiver(object);
        let mut operands: SmallVec<Operand, 8> = SmallVec::new();

        let maybe_cls_name_ssa: Option<SsaType> =
            self.current_block_data.value_types.get(&obj_val).cloned();

        let cls_name_id: Option<StrId> = match maybe_cls_name_ssa {
            Some(SsaType::User(name, _)) => Some(name),
            Some(SsaType::Pointer(ref inner)) => match unwrap_pointers(inner) {
                Some(SsaType::User(name, _)) => Some(*name),
                _ => None,
            },
            _ => None,
        };

        if let Some(cls_name) = cls_name_id {
            let receiver_is_moved = self
                .class_mangled_map
                .get(&cls_name)
                .and_then(|mmap| mmap.get(&field))
                .and_then(|mangled| self.funcs.get(mangled))
                .and_then(|f| f.params.first())
                .map(|(_, ty)| matches!(ty, SsaType::User(_, _)))
                .unwrap_or(false);

            if receiver_is_moved {
                record_move_if_any(self.scope_stack, self.drop_state, object);
            }
        }

        if cls_name_id.is_some() {
            self.current_block_data
                .value_types
                .insert(obj_val, SsaType::I64);
        }

        operands.push(Operand::Value(obj_val));
        for a in args {
            let av = self.lower_expr(a);
            operands.push(Operand::Value(av));
        }

        if let Some(value) = self.emit_call(field, obj_val, &mut operands, cls_name_id) {
            return value;
        }

        panic!(
            "lower_method_call: no mangled mapping or vtable slot found for method `{}` on class `{:?}`, \
             this means class_mangled_map/class_method_slots is missing an entry, not that the method doesn't exist. \
             Check MirModuleLowerer::build_mangled_map / build_class_vtable for this class.",
            self.context.resolve_string(&field),
            cls_name_id.map(|id| self.context.resolve_string(&id).to_string())
        );
    }

    fn lower_expr_as_receiver(&mut self, object: &HirExpr<'a, 'bump>) -> Value {
        if let HirExpr::Ident(name, _) = object {
            if let Some(&v) = self.var_map.get(name) {
                if matches!(
                    self.current_block_data.value_types.get(&v),
                    Some(SsaType::Pointer(_))
                ) {
                    return v;
                }
            }
        }
        if let HirExpr::This { .. } = object {
            let this_name = StrId(self.context.intern("this"));
            if let Some(&v) = self.var_map.get(&this_name) {
                if matches!(
                    self.current_block_data.value_types.get(&v),
                    Some(SsaType::Pointer(_))
                ) {
                    return v;
                }
            }
        }
        self.lower_expr(object)
    }

    fn emit_call(
        &mut self,
        field: StrId,
        obj_val: Value,
        operands: &mut SmallVec<Operand, 8>,
        maybe_cls_name: Option<StrId>,
    ) -> Option<Value> {
        let Some(cls_name) = maybe_cls_name else {
            return None;
        };

        if let Some(mmap) = self.class_mangled_map.get(&cls_name) {
            if let Some(mangled_name) = mmap.get(&field) {
                let dest = self.current_block_data.fresh_value();
                self.emit(Instruction::Call {
                    dest: Some(dest),
                    func: Operand::FunctionRef(mangled_name.clone()),
                    args: operands.clone(),
                });

                let ret_ty = self
                    .funcs
                    .get(mangled_name)
                    .map(|f| f.ret_type.clone())
                    .unwrap_or(SsaType::I64);
                self.current_block_data.value_types.insert(dest, ret_ty);
                return Some(dest);
            }
        }

        if let Some(class_slots) = self.class_method_slots.get(&cls_name) {
            if let Some(slot_idx) = class_slots.get(&field) {
                let dest = self.current_block_data.fresh_value();
                self.emit(Instruction::InterfaceDispatch {
                    dest: Some(dest),
                    object: obj_val,
                    method_slot: *slot_idx,
                    args: operands.clone(),
                });
                self.current_block_data
                    .value_types
                    .insert(dest, SsaType::I64);
                return Some(dest);
            }
        }

        None
    }

    fn lower_interface_call(
        &mut self,
        callee: &HirExpr<'a, 'bump>,
        args: &[HirExpr<'a, 'bump>],
        interface: StrId,
    ) -> Value {
        let HirExpr::FieldAccess {
            object,
            field,
            span: _,
        } = callee
        else {
            panic!("InterfaceCall callee not FieldAccess; unsupported shape")
        };

        let obj_val = self.lower_expr(object);

        let param_types: Vec<SsaType> = self
            .interface_methods
            .get(&interface)
            .and_then(|methods| methods.iter().find(|(name, _, _)| name == field))
            .map(|(_, params, _)| params.clone())
            .unwrap_or_default();

        if matches!(param_types.first(), Some(SsaType::Dyn)) {
            record_move_if_any(self.scope_stack, self.drop_state, object);
        }

        let mut operands: SmallVec<Operand, 8> = SmallVec::new();
        for (i, a) in args.iter().enumerate() {
            if matches!(param_types.get(i + 1), Some(SsaType::User(_, _))) {
                record_move_if_any(self.scope_stack, self.drop_state, a);
            }
            operands.push(Operand::Value(self.lower_expr(a)));
        }

        let iface_id = *self.interface_id_map.get(&interface).unwrap_or_else(|| {
            panic!(
                "Unknown interface {} in InterfaceCall",
                self.context.resolve_string(&*interface)
            )
        });

        let iface_slot_map = self
            .interface_method_slots
            .get(&interface)
            .unwrap_or_else(|| {
                panic!(
                    "Interface {} has no method slots",
                    self.context.resolve_string(&*interface)
                )
            });

        let method_slot_in_iface = iface_slot_map.get(field).unwrap_or_else(|| {
            panic!(
                "Interface {} has no method {}",
                self.context.resolve_string(&*interface),
                self.context.resolve_string(&*field)
            )
        });

        let interface_val = match self.current_block_data.value_types.get(&obj_val).cloned() {
            Some(SsaType::User(ref _name, _args)) => {
                let upcast_dest = self.current_block_data.fresh_value();
                self.emit(Instruction::UpcastToInterface {
                    dest: upcast_dest,
                    object: obj_val,
                    interface_id: iface_id,
                });

                self.current_block_data
                    .value_types
                    .insert(upcast_dest, SsaType::Interface(interface));
                upcast_dest
            }
            _ => obj_val,
        };

        let dest = self.current_block_data.fresh_value();
        self.emit(Instruction::InterfaceDispatch {
            dest: Some(dest),
            object: interface_val,
            method_slot: *method_slot_in_iface,
            args: operands,
        });

        self.current_block_data
            .value_types
            .insert(dest, SsaType::I64);
        dest
    }

    fn get_field_offset(&mut self, obj: &Value, field: StrId) -> usize {
        let cls_name = match self.current_block_data.value_types.get(obj) {
            Some(SsaType::User(name, _)) => *name,
            Some(SsaType::Pointer(inner)) => {
                if let SsaType::User(name, _) = inner.as_ref() {
                    *name
                } else {
                    panic!("get_field_offset: pointer to non-User type: {:?}", inner)
                }
            }
            other => panic!(
                "Could not determine object's class for FieldAccess: {:?}",
                other
            ),
        };

        self.class_field_offsets
            .get(&cls_name)
            .unwrap_or_else(|| panic!("Unknown class {} in FieldAccess", cls_name))
            .get(&field)
            .copied()
            .unwrap_or_else(|| {
                panic!(
                    "Unknown field {} on class {}",
                    self.context.resolve_string(&*field),
                    self.context.resolve_string(&*cls_name)
                )
            })
    }

    #[inline(always)]
    fn emit(&mut self, instr: Instruction) {
        self.current_block_data.bb().instructions.push(instr);
    }

    #[inline(always)]
    fn new_value(&mut self) -> Value {
        self.current_block_data.fresh_value()
    }

    fn handle_deref_assign(&mut self, ptr: Value, rhs: Value, op: AssignmentOperator) -> Value {
        // Load the current value if this is a compound assignment.
        let value_to_store = match op {
            AssignmentOperator::Assign => rhs,

            _ => {
                let current = self.new_value();

                self.emit(Instruction::Load {
                    dest: current,
                    ptr: Operand::Value(ptr),
                });

                // Record the loaded type.
                if let Some(SsaType::Pointer(inner)) =
                    self.current_block_data.value_types.get(&ptr).cloned()
                {
                    self.current_block_data.value_types.insert(current, *inner);
                }

                let dest = self.new_value();
                let bin_op = assign_op_to_bin_op(op);

                self.emit(Instruction::Binary {
                    dest,
                    op: bin_op,
                    left: Operand::Value(current),
                    right: Operand::Value(rhs),
                });

                let result_ty = self
                    .current_block_data
                    .value_types
                    .get(&current)
                    .cloned()
                    .or_else(|| self.current_block_data.value_types.get(&rhs).cloned())
                    .unwrap_or(SsaType::I64);

                self.current_block_data.value_types.insert(dest, result_ty);

                dest
            }
        };

        self.emit(Instruction::Store {
            ptr: Operand::Value(ptr),
            value: Operand::Value(value_to_store),
        });

        value_to_store
    }
}

fn unwrap_pointers(ty: &SsaType) -> Option<&SsaType> {
    match ty {
        SsaType::Pointer(inner) => unwrap_pointers(inner),
        other => Some(other),
    }
}
