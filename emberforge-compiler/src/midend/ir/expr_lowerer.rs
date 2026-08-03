use crate::midend::copy_analysis::drop_tracking::{DropMoveState, DropScope, record_move_if_any};
use crate::midend::ir::block_data::CurrentBlockData;
use crate::optimized_string_buffering;
use codex_dependency_graph::DepGraph;
use core::panic;
use ir::hir::{
    AssignmentOperator, HirEnum, HirExpr, HirFieldInit, HirMatchArm, HirPattern, HirStmt,
    HirStruct, HirType, Operator, StrId,
};
use ir::ir_conversion::{assign_op_to_bin_op, lower_operator_bin, lower_type_hir};
use ir::ir_hasher::{FxHashBuilder, HashSet};
use ir::layout::TargetInfo;
use ir::ssa_ir::{BinOp, BlockId, Function, Instruction, Operand, SsaType, Value, cast_kind};
use smallvec::SmallVec;
use smallvec::smallvec;
use std::cell::RefCell;
use std::collections::HashMap;
use std::marker::PhantomData;
use std::sync::Arc;
use zetaruntime::intern_fmt;
use zetaruntime::string_pool::StringPool;

/// MIR Expr Lowerer, which converts HIR expressions to MIR expressions.
/// MIR is similar to a higher level representation of assembly which can be optimized in Zeta specific ways or lowered to backends and this is the most portable way
/// If we are here this means we passed all type safety, memory safety and semantic checks, and we can safely discard of stuff and all debug like span's
pub struct MirExprLowerer<'el, 'f, 'a, 'cx, 'bump> {
    pub current_block_data: &'el mut CurrentBlockData<'f>,
    pub var_map: &'el mut HashMap<StrId, Value, FxHashBuilder>,
    pub context: Arc<StringPool>,
    phantom_data: PhantomData<&'bump ()>,

    pub funcs: &'a HashMap<StrId, Function, FxHashBuilder>,
    enums: &'a HashMap<StrId, HirEnum<'a, 'bump>, FxHashBuilder>,
    pub struct_field_offsets:
        &'a HashMap<StrId, HashMap<StrId, usize, FxHashBuilder>, FxHashBuilder>,
    pub struct_method_slots:
        &'a HashMap<StrId, HashMap<StrId, usize, FxHashBuilder>, FxHashBuilder>,
    pub struct_mangled_map: &'a HashMap<StrId, HashMap<StrId, StrId, FxHashBuilder>, FxHashBuilder>,
    pub struct_vtable_slots: &'a HashMap<StrId, Vec<StrId>, FxHashBuilder>,
    pub interface_id_map: &'a HashMap<StrId, usize, FxHashBuilder>,
    pub interface_method_slots:
        &'a HashMap<StrId, HashMap<StrId, usize, FxHashBuilder>, FxHashBuilder>,
    pub structs: &'a HashMap<StrId, HirStruct<'a, 'a>, FxHashBuilder>,

    pub cx_phantom: PhantomData<&'cx ()>,
    pub extern_c_names: &'a HashSet<StrId>,
    pub dep_graph: &'a RefCell<DepGraph>,
    pub module_idx: usize,
    global_funcs: &'a HashMap<StrId, Function, FxHashBuilder>,
    pub scope_stack: &'el [DropScope<'a, 'bump>],
    pub drop_state: &'el mut DropMoveState<'a, 'bump>,
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
        struct_field_offsets: &'a HashMap<
            StrId,
            HashMap<StrId, usize, FxHashBuilder>,
            FxHashBuilder,
        >,
        struct_method_slots: &'a HashMap<
            StrId,
            HashMap<StrId, usize, FxHashBuilder>,
            FxHashBuilder,
        >,
        struct_mangled_map: &'a HashMap<StrId, HashMap<StrId, StrId, FxHashBuilder>, FxHashBuilder>,
        struct_vtable_slots: &'a HashMap<StrId, Vec<StrId>, FxHashBuilder>,
        interface_id_map: &'a HashMap<StrId, usize, FxHashBuilder>,
        interface_method_slots: &'a HashMap<
            StrId,
            HashMap<StrId, usize, FxHashBuilder>,
            FxHashBuilder,
        >,
        structs: &'a HashMap<StrId, HirStruct<'a, 'bump>, FxHashBuilder>,
        extern_c_names: &'a HashSet<StrId>,
        dep_graph: &'a RefCell<DepGraph>,
        module_idx: usize,
        scope_stack: &'el [DropScope<'a, 'bump>],
        drop_state: &'el mut DropMoveState<'a, 'bump>,
        interface_methods: &'a HashMap<StrId, Vec<(StrId, Vec<SsaType>, SsaType)>, FxHashBuilder>,
        enums: &'a HashMap<StrId, HirEnum<'a, 'bump>, FxHashBuilder>,
    ) -> Self {
        Self {
            current_block_data,
            funcs,
            global_funcs,
            var_map,
            context,
            struct_field_offsets,
            struct_method_slots,
            struct_mangled_map,
            struct_vtable_slots,
            interface_id_map,
            interface_method_slots,
            structs,
            phantom_data: PhantomData,
            cx_phantom: PhantomData,
            extern_c_names,
            dep_graph,
            module_idx,
            scope_stack,
            drop_state,
            interface_methods,
            enums,
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
            } => self.lower_struct_init(name, args),

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
                variant,
                args,
                span: _,
                type_args: _,
            } => self.lower_enum_init(enum_name, variant, args),

            HirExpr::ExprList { list, span: _ } => {
                if list.is_empty() {
                    let v = self.current_block_data.fresh_value();
                    self.current_block_data.value_types.insert(v, SsaType::Void);
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
                self.current_block_data.value_types.insert(v, SsaType::Bool);
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
                unreachable!("There should be no lambdas here")
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
                let mut src = self.lower_expr(expr);

                let mut src_ty = self.current_block_data.value_types[&src].clone();
                let dst_ty = lower_type_hir(target_type);

                if dst_ty.is_pointer() {
                    match &src_ty {
                        SsaType::Slice(inner) => {
                            let ptr = self.current_block_data.fresh_value();

                            self.emit(Instruction::LoadField {
                                dest: ptr,
                                base: Operand::Value(src),
                                offset: 0,
                            });

                            let ptr_ty = SsaType::Pointer(Box::new((**inner).clone()));
                            self.current_block_data
                                .value_types
                                .insert(ptr, ptr_ty.clone());

                            src = ptr;
                            src_ty = ptr_ty;
                        }

                        SsaType::Owned(inner) => {
                            if let SsaType::Slice(elem) = inner.as_ref() {
                                let ptr = self.current_block_data.fresh_value();

                                self.emit(Instruction::LoadField {
                                    dest: ptr,
                                    base: Operand::Value(src),
                                    offset: 0,
                                });

                                let ptr_ty = SsaType::Pointer(Box::new((**elem).clone()));
                                self.current_block_data
                                    .value_types
                                    .insert(ptr, ptr_ty.clone());

                                src = ptr;
                                src_ty = ptr_ty;
                            }
                        }

                        _ => {}
                    }
                }

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
            HirExpr::Intrinsic {
                kind,
                type_args,
                args,
                span: _,
            } => {
                use ir::hir::IntrinsicKind;
                use ir::ssa_ir::IntrinsicOp;

                match kind {
                    IntrinsicKind::Reinterpret => {
                        let src = self.lower_expr(&args[0]);
                        let target_ty = lower_type_hir(&type_args[0]);
                        self.current_block_data.value_types.insert(src, target_ty);
                        src
                    }
                    IntrinsicKind::Unreachable => {
                        let dest = self.current_block_data.fresh_value();
                        let msg = self.current_block_data.fresh_value();
                        let msg_str = self.context.intern("entered unreachable code");
                        self.emit(Instruction::Const {
                            dest: msg,
                            ty: SsaType::String,
                            value: Operand::ConstString(StrId(msg_str)),
                        });
                        self.current_block_data
                            .value_types
                            .insert(msg, SsaType::String);
                        // TODO: lower to a real trap/panic instruction once one exists
                        // Or preferrably $unreachable() should instead be an unsafe alternative where LLVM or Cranelift assume this path never exists and simply UBs instead.
                        self.current_block_data
                            .value_types
                            .insert(dest, SsaType::Void);
                        dest
                    }
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
                        // todo: panic here

                        self.current_block_data.switch_to(cont_bb);
                        let dest = self.current_block_data.fresh_value();
                        self.current_block_data
                            .value_types
                            .insert(dest, SsaType::Void);
                        dest
                    }

                    IntrinsicKind::Own => {
                        let ptr_val = self.lower_expr(&args[0]);
                        let ptr_ty = self
                            .current_block_data
                            .value_types
                            .get(&ptr_val)
                            .cloned()
                            .expect("$own: pointer arg has no known type");
                        let pointee_ty = match &ptr_ty {
                            SsaType::Pointer(inner) => (**inner).clone(),
                            other => {
                                panic!("$own: expected pointer-typed first arg, got {:?}", other)
                            }
                        };

                        let len_cap_exprs = if args.len() == 4 {
                            Some((&args[2], &args[3]))
                        } else {
                            None
                        };

                        match len_cap_exprs {
                            // Owned slice: {ptr, len, cap} fat pointer, 24 bytes.
                            Some((len_expr, cap_expr)) => {
                                let len_val = self.lower_expr(len_expr);
                                let cap_val = self.lower_expr(cap_expr);

                                let fat_ptr = self.current_block_data.fresh_value();
                                let fat_ptr_layout_ty = SsaType::Tuple(vec![
                                    SsaType::Pointer(Box::new(pointee_ty.clone())),
                                    SsaType::Usize, // len
                                    SsaType::Usize, // cap
                                ]);
                                self.emit(Instruction::StackAlloc {
                                    dest: fat_ptr,
                                    ty: fat_ptr_layout_ty,
                                    count: 1,
                                });

                                let slice_ty =
                                    SsaType::Owned(Box::new(SsaType::Slice(Box::new(pointee_ty))));
                                self.current_block_data
                                    .value_types
                                    .insert(fat_ptr, slice_ty);

                                self.emit(Instruction::StoreField {
                                    base: Operand::Value(fat_ptr),
                                    offset: 0,
                                    value: Operand::Value(ptr_val),
                                });
                                self.emit(Instruction::StoreField {
                                    base: Operand::Value(fat_ptr),
                                    offset: 8,
                                    value: Operand::Value(len_val),
                                });
                                self.emit(Instruction::StoreField {
                                    base: Operand::Value(fat_ptr),
                                    offset: 16,
                                    value: Operand::Value(cap_val),
                                });

                                fat_ptr
                            }

                            None => {
                                let owned_ty = SsaType::Owned(Box::new(pointee_ty));
                                self.current_block_data
                                    .value_types
                                    .insert(ptr_val, owned_ty);
                                ptr_val
                            }
                        }
                    }
                    IntrinsicKind::AtomicCasU32 => {
                        let ptr_val = self.lower_expr(&args[0]);
                        let expected_val = self.lower_expr(&args[1]);
                        let new_val = self.lower_expr(&args[2]);

                        let dest = self.current_block_data.fresh_value();
                        self.emit(Instruction::Intrinsic {
                            dest: Some(dest),
                            op: IntrinsicOp::AtomicCasU32,
                            query_ty: None,
                            args: smallvec![
                                Operand::Value(ptr_val),
                                Operand::Value(expected_val),
                                Operand::Value(new_val)
                            ],
                        });
                        self.current_block_data
                            .value_types
                            .insert(dest, SsaType::U32);
                        dest
                    }

                    IntrinsicKind::AtomicLoadU32 => {
                        let ptr_val = self.lower_expr(&args[0]);

                        let dest = self.current_block_data.fresh_value();
                        self.emit(Instruction::Intrinsic {
                            dest: Some(dest),
                            op: IntrinsicOp::AtomicLoadU32,
                            query_ty: None,
                            args: smallvec![Operand::Value(ptr_val)],
                        });
                        self.current_block_data
                            .value_types
                            .insert(dest, SsaType::U32);
                        dest
                    }

                    IntrinsicKind::AtomicStoreU32 => {
                        let ptr_val = self.lower_expr(&args[0]);
                        let val_val = self.lower_expr(&args[1]);

                        self.emit(Instruction::Intrinsic {
                            dest: None,
                            op: IntrinsicOp::AtomicStoreU32,
                            query_ty: None,
                            args: smallvec![Operand::Value(ptr_val), Operand::Value(val_val)],
                        });

                        let dest = self.current_block_data.fresh_value();
                        self.current_block_data
                            .value_types
                            .insert(dest, SsaType::Void);
                        dest
                    }

                    IntrinsicKind::CpuRelax => {
                        self.emit(Instruction::Intrinsic {
                            dest: None,
                            op: IntrinsicOp::CpuRelax,
                            query_ty: None,
                            args: SmallVec::new(),
                        });

                        let dest = self.current_block_data.fresh_value();
                        self.current_block_data
                            .value_types
                            .insert(dest, SsaType::Void);
                        dest
                    }
                }
            }
            HirExpr::Block { body, .. } => self.lower_block_value(body),
            HirExpr::Match { expr, arms, .. } => self.lower_match_expr(expr, arms),
            HirExpr::Range { start, end, .. } => self.lower_range_expr(start, end),
            HirExpr::Slice {
                object,
                start,
                end,
                inclusive,
                ..
            } => self.lower_slice_expr(object, start, end, *inclusive),
            HirExpr::UnknownIntrinsic { span, name } => unreachable!("span {span} name {name}"),
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
            HirExpr::Char(c, _) => {
                let v = self.current_block_data.fresh_value();
                self.emit(Instruction::Const {
                    dest: v,
                    ty: SsaType::Char,
                    value: Operand::ConstInt(*c as i64),
                });
                self.current_block_data.value_types.insert(v, SsaType::Char);
                v
            }
        }
    }

    fn lower_index_base(&mut self, object: &HirExpr<'a, 'bump>) -> (Value, SsaType) {
        let base = self.lower_expr(object);
        let base_ty = self
            .current_block_data
            .value_types
            .get(&base)
            .cloned()
            .expect("lower_index_base: base value has no known type");

        match &base_ty {
            SsaType::Pointer(inner) if matches!(inner.as_ref(), SsaType::Slice(_)) => {
                let elem_inner = match inner.as_ref() {
                    SsaType::Slice(e) => (**e).clone(),
                    _ => unreachable!(),
                };
                let data_ptr = self.current_block_data.fresh_value();
                self.emit(Instruction::LoadField {
                    dest: data_ptr,
                    base: Operand::Value(base),
                    offset: 0,
                });
                self.current_block_data
                    .value_types
                    .insert(data_ptr, SsaType::Pointer(Box::new(elem_inner.clone())));
                (data_ptr, elem_inner)
            }
            SsaType::Pointer(inner) => (base, (**inner).clone()),
            SsaType::Array(inner, _) => (base, (**inner).clone()),
            SsaType::Slice(inner) => {
                let data_ptr = self.current_block_data.fresh_value();
                self.emit(Instruction::LoadField {
                    dest: data_ptr,
                    base: Operand::Value(base),
                    offset: 0,
                });
                self.current_block_data
                    .value_types
                    .insert(data_ptr, SsaType::Pointer(Box::new((**inner).clone())));
                (data_ptr, (**inner).clone())
            }
            SsaType::Owned(inner) => match inner.as_ref() {
                SsaType::Slice(inner) => {
                    let data_ptr = self.current_block_data.fresh_value();
                    self.emit(Instruction::LoadField {
                        dest: data_ptr,
                        base: Operand::Value(base),
                        offset: 0,
                    });
                    self.current_block_data
                        .value_types
                        .insert(data_ptr, SsaType::Pointer(Box::new((**inner).clone())));
                    (data_ptr, (**inner).clone())
                }
                _ => panic!("[lower_index_base] cannot index into {:?}", inner),
            },
            other => panic!("[lower_index_base] cannot index into {:?}", other),
        }
    }

    fn align_up(value: usize, align: usize) -> usize {
        if align == 0 {
            return value;
        }
        (value + align - 1) & !(align - 1)
    }

    fn lower_enum_init(
        &mut self,
        enum_name: &StrId,
        variant: &StrId,
        args: &[HirExpr<'a, 'bump>],
    ) -> Value {
        let arg_values: Vec<Value> = args.iter().map(|a| self.lower_expr(a)).collect();
        let arg_types: Vec<SsaType> = arg_values
            .iter()
            .map(|v| {
                self.current_block_data
                    .value_types
                    .get(v)
                    .cloned()
                    .unwrap_or(SsaType::I64)
            })
            .collect();

        let target = TargetInfo { ptr_bytes: 8 };
        let mut offsets = Vec::with_capacity(arg_types.len());
        let mut cursor = 0usize;
        for ty in &arg_types {
            let (size, align) = ir::layout::layout_of_ssa(ty, target)
                .map(|l| (l.size, l.align))
                .unwrap_or((8, 8)); // conservative fallback
            cursor = Self::align_up(cursor, align);
            offsets.push(cursor);
            cursor += size;
        }
        let payload_size = cursor;

        let hir_enum = self.enums.get(enum_name).unwrap_or_else(|| {
            panic!(
                "lower_enum_init: unknown enum `{}`; the type checker should have caught this",
                enum_name
            )
        });
        let tag = hir_enum.variants.iter().position(|v| v.name == *variant).unwrap_or_else(|| {
            panic!(
                "lower_enum_init: enum `{}` has no variant `{}`; the type checker should have caught this",
                enum_name, variant
            )
        }) as i64;

        let tag_v = self.current_block_data.fresh_value();
        self.emit(Instruction::Const {
            dest: tag_v,
            ty: SsaType::I64,
            value: Operand::ConstInt(tag),
        });
        self.current_block_data
            .value_types
            .insert(tag_v, SsaType::I64);

        let size_v = self.current_block_data.fresh_value();
        self.emit(Instruction::Const {
            dest: size_v,
            ty: SsaType::I64,
            value: Operand::ConstInt(payload_size as i64),
        });
        self.current_block_data
            .value_types
            .insert(size_v, SsaType::I64);

        let enum_new_fn = StrId(self.context.intern("__enum_new"));
        let obj = self.current_block_data.fresh_value();
        self.emit(Instruction::Call {
            dest: Some(obj),
            func: Operand::FunctionRef(enum_new_fn),
            args: smallvec![Operand::Value(tag_v), Operand::Value(size_v)],
        });
        self.current_block_data
            .value_types
            .insert(obj, SsaType::Enum(*enum_name, vec![]));

        for ((val, _ty), field_offset) in
            arg_values.iter().zip(arg_types.iter()).zip(offsets.iter())
        {
            self.emit(Instruction::StoreField {
                base: Operand::Value(obj),
                offset: 8 + field_offset, // tag occupies bytes [0, 8)
                value: Operand::Value(*val),
            });
        }

        obj
    }

    fn lower_range_expr(&mut self, start: &HirExpr<'a, 'bump>, end: &HirExpr<'a, 'bump>) -> Value {
        let start_v = self.lower_expr(start);
        let end_v = self.lower_expr(end);

        let dest = self.current_block_data.fresh_value();
        let ty = SsaType::Tuple(vec![SsaType::Usize, SsaType::Usize]);
        self.emit(Instruction::StackAlloc {
            dest,
            ty: ty.clone(),
            count: 1,
        });
        self.current_block_data.value_types.insert(dest, ty);

        self.emit(Instruction::StoreField {
            base: Operand::Value(dest),
            offset: 0,
            value: Operand::Value(start_v),
        });
        self.emit(Instruction::StoreField {
            base: Operand::Value(dest),
            offset: 8,
            value: Operand::Value(end_v),
        });
        dest
    }

    fn slice_pseudo_field(&self, ty: &SsaType, field: StrId) -> Option<(usize, SsaType)> {
        let actual_ty = match ty {
            SsaType::Pointer(inner) => inner.as_ref(),
            other => other,
        };
        match (self.context.resolve_string(&field), actual_ty) {
            ("len", SsaType::Owned(inner)) if matches!(inner.as_ref(), SsaType::Slice(_)) => {
                Some((8, SsaType::Usize))
            }
            ("cap" | "capacity", SsaType::Owned(inner))
                if matches!(inner.as_ref(), SsaType::Slice(_)) =>
            {
                Some((16, SsaType::Usize))
            }
            ("len", SsaType::Slice(_)) => Some((8, SsaType::Usize)),
            ("cap" | "capacity", SsaType::Slice(_)) => Some((16, SsaType::Usize)),
            _ => None,
        }
    }

    fn resolve_slice_pseudo_field(&self, ty: &SsaType, field: StrId) -> Option<(usize, SsaType)> {
        match ty {
            SsaType::Slice(_) | SsaType::Owned(_) => self.slice_pseudo_field(ty, field),
            SsaType::Pointer(inner) => self.resolve_slice_pseudo_field(inner, field),
            _ => None,
        }
    }

    fn lower_short_circuit_and(
        &mut self,
        left: &HirExpr<'a, 'bump>,
        right: &HirExpr<'a, 'bump>,
    ) -> Value {
        let lhs = self.lower_expr(left);

        let rhs_bb = self.current_block_data.new_block();
        let false_bb = self.current_block_data.new_block();
        let merge_bb = self.current_block_data.new_block();

        self.emit(Instruction::Branch {
            cond: Operand::Value(lhs),
            then_bb: rhs_bb,
            else_bb: false_bb,
        });

        // RHS
        self.current_block_data.switch_to(rhs_bb);
        let rhs = self.lower_expr(right);
        self.emit(Instruction::Jump { target: merge_bb });

        // FALSE
        self.current_block_data.switch_to(false_bb);
        let false_val = self.current_block_data.fresh_value();

        self.emit(Instruction::Const {
            dest: false_val,
            ty: SsaType::Bool,
            value: Operand::ConstBool(false),
        });

        self.emit(Instruction::Jump { target: merge_bb });

        // MERGE
        self.current_block_data.switch_to(merge_bb);

        let result = self.current_block_data.fresh_value();

        self.emit(Instruction::Phi {
            dest: result,
            incoming: smallvec![(rhs_bb, rhs), (false_bb, false_val),],
        });

        self.current_block_data
            .value_types
            .insert(result, SsaType::Bool);

        result
    }

    fn lower_short_circuit_or(
        &mut self,
        left: &HirExpr<'a, 'bump>,
        right: &HirExpr<'a, 'bump>,
    ) -> Value {
        let lhs = self.lower_expr(left);

        let true_bb = self.current_block_data.new_block();
        let rhs_bb = self.current_block_data.new_block();
        let merge_bb = self.current_block_data.new_block();

        self.emit(Instruction::Branch {
            cond: Operand::Value(lhs),
            then_bb: true_bb,
            else_bb: rhs_bb,
        });

        // TRUE
        self.current_block_data.switch_to(true_bb);

        let true_val = self.current_block_data.fresh_value();

        self.emit(Instruction::Const {
            dest: true_val,
            ty: SsaType::Bool,
            value: Operand::ConstBool(true),
        });

        self.emit(Instruction::Jump { target: merge_bb });

        // RHS
        self.current_block_data.switch_to(rhs_bb);

        let rhs = self.lower_expr(right);

        self.emit(Instruction::Jump { target: merge_bb });

        // MERGE
        self.current_block_data.switch_to(merge_bb);

        let result = self.current_block_data.fresh_value();

        self.emit(Instruction::Phi {
            dest: result,
            incoming: smallvec![(true_bb, true_val), (rhs_bb, rhs),],
        });

        self.current_block_data
            .value_types
            .insert(result, SsaType::Bool);

        result
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
        let idx = self.lower_expr(index);
        let (base_ptr, elem_ty) = self.lower_index_base(object);

        let elem_size = ir::layout::sizeof_ssa(&elem_ty, TargetInfo { ptr_bytes: 8 })
            .expect("[lower_index_addr] element type has no known size")
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
            left: Operand::Value(base_ptr),
            right: Operand::Value(offset_v),
        });
        self.current_block_data
            .value_types
            .insert(addr_v, SsaType::Pointer(Box::new(elem_ty.clone())));

        (addr_v, elem_ty)
    }

    fn lower_slice_expr(
        &mut self,
        object: &HirExpr<'a, 'bump>,
        start: &HirExpr<'a, 'bump>,
        end: &HirExpr<'a, 'bump>,
        inclusive: bool,
    ) -> Value {
        let (base_addr, elem_ty) = self.lower_index_base(object);
        let start_v = self.lower_expr(start);
        let end_v = self.lower_expr(end);

        let elem_size = ir::layout::sizeof_ssa(&elem_ty, TargetInfo { ptr_bytes: 8 })
            .expect("slice element has no known size") as i64;

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
            left: Operand::Value(start_v),
            right: Operand::Value(size_v),
        });
        self.current_block_data
            .value_types
            .insert(offset_v, SsaType::I64);

        let ptr_v = self.current_block_data.fresh_value();
        self.emit(Instruction::Binary {
            dest: ptr_v,
            op: BinOp::Add,
            left: Operand::Value(base_addr),
            right: Operand::Value(offset_v),
        });
        self.current_block_data
            .value_types
            .insert(ptr_v, SsaType::Pointer(Box::new(elem_ty.clone())));

        let mut len_v = self.current_block_data.fresh_value();
        self.emit(Instruction::Binary {
            dest: len_v,
            op: BinOp::Sub,
            left: Operand::Value(end_v),
            right: Operand::Value(start_v),
        });
        self.current_block_data
            .value_types
            .insert(len_v, SsaType::Usize);

        if inclusive {
            let one = self.current_block_data.fresh_value();
            self.emit(Instruction::Const {
                dest: one,
                ty: SsaType::Usize,
                value: Operand::ConstInt(1),
            });
            self.current_block_data
                .value_types
                .insert(one, SsaType::Usize);
            let adjusted = self.current_block_data.fresh_value();
            self.emit(Instruction::Binary {
                dest: adjusted,
                op: BinOp::Add,
                left: Operand::Value(len_v),
                right: Operand::Value(one),
            });
            self.current_block_data
                .value_types
                .insert(adjusted, SsaType::Usize);
            len_v = adjusted;
        }

        let fat_ptr = self.current_block_data.fresh_value();
        let fat_ty = SsaType::Tuple(vec![
            SsaType::Pointer(Box::new(elem_ty.clone())),
            SsaType::Usize,
            SsaType::Usize,
        ]);
        self.emit(Instruction::StackAlloc {
            dest: fat_ptr,
            ty: fat_ty,
            count: 1,
        });
        self.current_block_data
            .value_types
            .insert(fat_ptr, SsaType::Slice(Box::new(elem_ty)));

        self.emit(Instruction::StoreField {
            base: Operand::Value(fat_ptr),
            offset: 0,
            value: Operand::Value(ptr_v),
        });
        self.emit(Instruction::StoreField {
            base: Operand::Value(fat_ptr),
            offset: 8,
            value: Operand::Value(len_v),
        });
        self.emit(Instruction::StoreField {
            base: Operand::Value(fat_ptr),
            offset: 16,
            value: Operand::Value(len_v),
        });

        fat_ptr
    }

    fn lower_match_expr(
        &mut self,
        scrutinee: &HirExpr<'a, 'bump>,
        arms: &[HirMatchArm<'a, 'bump>],
    ) -> Value {
        let scrutinee_val = self.lower_expr(scrutinee);
        let scrutinee_ty = self
            .current_block_data
            .value_types
            .get(&scrutinee_val)
            .cloned();

        let merge_bb = self.current_block_data.new_block();
        let mut incoming: SmallVec<(BlockId, Value), 4> = SmallVec::new();
        let mut any_reachable = false;

        for (arm_idx, arm) in arms.iter().enumerate() {
            let is_last = arm_idx + 1 == arms.len();
            let body_bb = self.current_block_data.new_block();
            let fail_bb = if is_last {
                None
            } else {
                Some(self.current_block_data.new_block())
            };

            let pattern_cond =
                self.lower_pattern_test(&arm.pattern, scrutinee_val, scrutinee_ty.as_ref());

            let cond = match (pattern_cond, arm.guard) {
                (Some(pc), Some(guard_expr)) => {
                    let guard_bb = self.current_block_data.new_block();
                    self.emit(Instruction::Branch {
                        cond: Operand::Value(pc),
                        then_bb: guard_bb,
                        else_bb: fail_bb.unwrap_or(body_bb),
                    });
                    self.current_block_data.switch_to(guard_bb);
                    Some(self.lower_expr(guard_expr))
                }
                (Some(pc), None) => Some(pc),
                (None, Some(guard_expr)) => Some(self.lower_expr(guard_expr)),
                (None, None) => None,
            };

            match (cond, fail_bb) {
                (Some(c), Some(fb)) => {
                    self.emit(Instruction::Branch {
                        cond: Operand::Value(c),
                        then_bb: body_bb,
                        else_bb: fb,
                    });
                }
                (Some(c), None) => {
                    let trap_bb = self.current_block_data.new_block();
                    self.emit(Instruction::Branch {
                        cond: Operand::Value(c),
                        then_bb: body_bb,
                        else_bb: trap_bb,
                    });
                    self.current_block_data.switch_to(trap_bb);
                    let msg = self.current_block_data.fresh_value();
                    let msg_str = self.context.intern("non-exhaustive match: no arm matched");
                    self.emit(Instruction::Const {
                        dest: msg,
                        ty: SsaType::String,
                        value: Operand::ConstString(StrId(msg_str)),
                    });
                    self.current_block_data
                        .value_types
                        .insert(msg, SsaType::String);
                    // TODO: lower panic
                }
                (None, _) => {
                    self.emit(Instruction::Jump { target: body_bb });
                }
            }

            self.current_block_data.switch_to(body_bb);
            self.bind_pattern(&arm.pattern, scrutinee_val, scrutinee_ty.as_ref());
            let HirStmt::Block { body } = arm.body else {
                panic!("match arm body must be a block")
            };
            let arm_val = self.lower_block_value(body);
            let arm_end_bb = self.current_block_data.current_block;
            if !self.block_terminated() {
                self.emit(Instruction::Jump { target: merge_bb });
                incoming.push((arm_end_bb, arm_val));
                any_reachable = true;
            }

            if let Some(fb) = fail_bb {
                self.current_block_data.switch_to(fb);
            }
        }

        self.current_block_data.switch_to(merge_bb);
        if !any_reachable {
            return self.unreachable_value();
        }

        let result = self.current_block_data.fresh_value();
        let ty = incoming
            .first()
            .and_then(|(_, v)| self.current_block_data.value_types.get(v).cloned())
            .unwrap_or(SsaType::Void);
        self.emit(Instruction::Phi {
            dest: result,
            incoming,
        });
        self.current_block_data.value_types.insert(result, ty);
        result
    }

    fn lower_pattern_test(
        &mut self,
        pattern: &HirPattern<'bump>,
        scrutinee: Value,
        scrutinee_ty: Option<&SsaType>,
    ) -> Option<Value> {
        match pattern {
            HirPattern::Wildcard | HirPattern::Ident(_) => None,

            HirPattern::Array(elems) => {
                let SsaType::Array(elem_ty, _) = scrutinee_ty.expect(
                    "lower_pattern_test: array pattern has no scrutinee type; the type checker should have caught this"
                ) else {
                    panic!("lower_pattern_test: array pattern used on a non-array scrutinee");
                };
                let elem_ty = (**elem_ty).clone();
                let elem_size = ir::layout::sizeof_ssa(&elem_ty, TargetInfo { ptr_bytes: 8 })
                    .expect("lower_pattern_test: array element type has no known size")
                    as i64;

                let mut combined: Option<Value> = None;
                for (i, elem_pat) in elems.iter().enumerate() {
                    let addr = self.current_block_data.fresh_value();
                    self.emit(Instruction::FieldAddr {
                        dest: addr,
                        base: Operand::Value(scrutinee),
                        offset: (i as i64 * elem_size) as usize,
                    });
                    self.current_block_data
                        .value_types
                        .insert(addr, SsaType::Pointer(Box::new(elem_ty.clone())));

                    let elem_val = self.current_block_data.fresh_value();
                    self.emit(Instruction::Load {
                        dest: elem_val,
                        ptr: Operand::Value(addr),
                    });
                    self.current_block_data
                        .value_types
                        .insert(elem_val, elem_ty.clone());

                    if let Some(cond) = self.lower_pattern_test(elem_pat, elem_val, Some(&elem_ty))
                    {
                        combined = Some(self.and_conds(combined, cond));
                    }
                }
                combined
            }

            HirPattern::Struct { name, fields } => {
                let offsets = self
                    .struct_field_offsets
                    .get(name)
                    .unwrap_or_else(|| panic!("lower_pattern_test: unknown struct `{}`", name));
                let hir_struct = self.structs.get(name);

                let mut combined: Option<Value> = None;
                for (field_name, field_pat) in fields.iter() {
                    let offset = *offsets.get(field_name).unwrap_or_else(|| {
                        panic!(
                            "lower_pattern_test: unknown field `{}` on struct `{}`",
                            field_name, name
                        )
                    });
                    let field_ty = hir_struct
                        .and_then(|s| s.fields.iter().find(|f| f.name == *field_name))
                        .map(|f| lower_type_hir(&f.field_type))
                        .unwrap_or(SsaType::I64);

                    let field_val = self.current_block_data.fresh_value();
                    self.emit(Instruction::LoadField {
                        dest: field_val,
                        base: Operand::Value(scrutinee),
                        offset,
                    });
                    self.current_block_data
                        .value_types
                        .insert(field_val, field_ty.clone());

                    if let Some(cond) =
                        self.lower_pattern_test(field_pat, field_val, Some(&field_ty))
                    {
                        combined = Some(self.and_conds(combined, cond));
                    }
                }
                combined
            }

            HirPattern::Or(alts) => {
                let mut combined: Option<Value> = None;
                let mut always_matches = false;
                for alt in alts.iter() {
                    match self.lower_pattern_test(alt, scrutinee, scrutinee_ty) {
                        None => always_matches = true,
                        Some(cond) => combined = Some(self.or_conds(combined, cond)),
                    }
                }
                if always_matches { None } else { combined }
            }

            HirPattern::Boolean(b) => {
                let lit = self.current_block_data.fresh_value();
                self.emit(Instruction::Const {
                    dest: lit,
                    ty: SsaType::Bool,
                    value: Operand::ConstBool(*b),
                });
                self.current_block_data
                    .value_types
                    .insert(lit, SsaType::Bool);
                let cmp = self.current_block_data.fresh_value();
                self.emit(Instruction::Binary {
                    dest: cmp,
                    op: BinOp::Eq,
                    left: Operand::Value(scrutinee),
                    right: Operand::Value(lit),
                });
                self.current_block_data
                    .value_types
                    .insert(cmp, SsaType::Bool);
                Some(cmp)
            }

            HirPattern::Number(n) => {
                let lit = self.current_block_data.fresh_value();
                self.emit(Instruction::Const {
                    dest: lit,
                    ty: SsaType::I64,
                    value: Operand::ConstInt(*n),
                });
                self.current_block_data
                    .value_types
                    .insert(lit, SsaType::I64);
                let cmp = self.current_block_data.fresh_value();
                self.emit(Instruction::Binary {
                    dest: cmp,
                    op: BinOp::Eq,
                    left: Operand::Value(scrutinee),
                    right: Operand::Value(lit),
                });
                self.current_block_data
                    .value_types
                    .insert(cmp, SsaType::Bool);
                Some(cmp)
            }

            HirPattern::String(s) => {
                let lit = self.current_block_data.fresh_value();
                self.emit(Instruction::Const {
                    dest: lit,
                    ty: SsaType::String,
                    value: Operand::ConstString(*s),
                });
                self.current_block_data
                    .value_types
                    .insert(lit, SsaType::String);

                let streq_fn = StrId(self.context.intern("__zeta_streq"));
                let cmp = self.current_block_data.fresh_value();
                self.emit(Instruction::Call {
                    dest: Some(cmp),
                    func: Operand::FunctionRef(streq_fn),
                    args: smallvec![Operand::Value(scrutinee), Operand::Value(lit)],
                });
                self.current_block_data
                    .value_types
                    .insert(cmp, SsaType::Bool);
                Some(cmp)
            }

            HirPattern::EnumVariant { variant, .. } => {
                let SsaType::User(enum_name, _) = scrutinee_ty.expect(
                    "lower_pattern_test: enum pattern has no scrutinee type; the type checker should have caught this"
                ) else {
                    panic!(
                        "lower_pattern_test: enum pattern `{}(..)` used on a non-enum scrutinee; \
                         the type checker should have caught this",
                        variant
                    );
                };
                let hir_enum = self
                    .enums
                    .get(&enum_name)
                    .unwrap_or_else(|| panic!("lower_pattern_test: unknown enum `{}`", enum_name));
                let expected_tag = hir_enum.variants.iter().position(|v| v.name == *variant)
                    .unwrap_or_else(|| panic!(
                        "lower_pattern_test: enum `{}` has no variant `{}`; the type checker should have caught this",
                        enum_name, variant
                    )) as i64;

                let tag_fn = StrId(self.context.intern("__enum_tag"));
                let tag_val = self.current_block_data.fresh_value();
                self.emit(Instruction::Call {
                    dest: Some(tag_val),
                    func: Operand::FunctionRef(tag_fn),
                    args: smallvec![Operand::Value(scrutinee)],
                });
                self.current_block_data
                    .value_types
                    .insert(tag_val, SsaType::I64);

                let cmp = self.current_block_data.fresh_value();
                self.emit(Instruction::Binary {
                    dest: cmp,
                    op: BinOp::Eq,
                    left: Operand::Value(tag_val),
                    right: Operand::ConstInt(expected_tag),
                });
                self.current_block_data
                    .value_types
                    .insert(cmp, SsaType::Bool);
                Some(cmp)
            }

            HirPattern::Tuple(_) => {
                todo!("tuple patterns aren't implemented upstream in lower_pattern either")
            }
        }
    }

    fn bind_pattern(
        &mut self,
        pattern: &HirPattern<'bump>,
        scrutinee: Value,
        scrutinee_ty: Option<&SsaType>,
    ) {
        match pattern {
            HirPattern::Ident(name) => {
                self.var_map.insert(*name, scrutinee);
            }
            HirPattern::Array(elems) => {
                let SsaType::Array(elem_ty, _) = scrutinee_ty.expect(
                    "bind_pattern: array pattern has no scrutinee type; the type checker should have caught this"
                ) else {
                    panic!("bind_pattern: array pattern used on a non-array scrutinee");
                };
                let elem_ty = (**elem_ty).clone();
                let elem_size = ir::layout::sizeof_ssa(&elem_ty, TargetInfo { ptr_bytes: 8 })
                    .expect("bind_pattern: array element type has no known size")
                    as i64;

                for (i, elem_pat) in elems.iter().enumerate() {
                    let addr = self.current_block_data.fresh_value();
                    self.emit(Instruction::FieldAddr {
                        dest: addr,
                        base: Operand::Value(scrutinee),
                        offset: (i as i64 * elem_size) as usize,
                    });
                    self.current_block_data
                        .value_types
                        .insert(addr, SsaType::Pointer(Box::new(elem_ty.clone())));

                    let elem_val = self.current_block_data.fresh_value();
                    self.emit(Instruction::Load {
                        dest: elem_val,
                        ptr: Operand::Value(addr),
                    });
                    self.current_block_data
                        .value_types
                        .insert(elem_val, elem_ty.clone());

                    self.bind_pattern(elem_pat, elem_val, Some(&elem_ty));
                }
            }

            HirPattern::Struct { name, fields } => {
                let offsets = self
                    .struct_field_offsets
                    .get(name)
                    .unwrap_or_else(|| panic!("bind_pattern: unknown struct `{}`", name));
                let hir_struct = self.structs.get(name);

                for (field_name, field_pat) in fields.iter() {
                    let offset = *offsets.get(field_name).unwrap_or_else(|| {
                        panic!(
                            "bind_pattern: unknown field `{}` on struct `{}`",
                            field_name, name
                        )
                    });
                    let field_ty = hir_struct
                        .and_then(|s| s.fields.iter().find(|f| f.name == *field_name))
                        .map(|f| lower_type_hir(&f.field_type))
                        .unwrap_or(SsaType::I64);

                    let field_val = self.current_block_data.fresh_value();
                    self.emit(Instruction::LoadField {
                        dest: field_val,
                        base: Operand::Value(scrutinee),
                        offset,
                    });
                    self.current_block_data
                        .value_types
                        .insert(field_val, field_ty.clone());

                    self.bind_pattern(field_pat, field_val, Some(&field_ty));
                }
            }

            HirPattern::Or(alts) => {
                for alt in alts.iter() {
                    self.bind_pattern(alt, scrutinee, scrutinee_ty);
                }
            }
            HirPattern::EnumVariant {
                variant, bindings, ..
            } if !bindings.is_empty() => {
                let SsaType::User(enum_name, _) = scrutinee_ty.expect(
                    "bind_pattern: enum pattern has no scrutinee type; the type checker should have caught this"
                ) else {
                    panic!(
                        "bind_pattern: enum pattern `{}(..)` used on a non-enum scrutinee; \
                         the type checker should have caught this",
                        variant
                    );
                };
                let hir_enum = self
                    .enums
                    .get(enum_name)
                    .unwrap_or_else(|| panic!("bind_pattern: unknown enum `{}`", enum_name));
                let variant_def = hir_enum.variants.iter().find(|v| v.name == *variant)
                    .unwrap_or_else(|| panic!(
                        "bind_pattern: enum `{}` has no variant `{}`; the type checker should have caught this",
                        enum_name, variant
                    ));
                debug_assert_eq!(
                    bindings.len(),
                    variant_def.fields.len(),
                    "bind_pattern: binding count for variant `{}` doesn't match its field count; \
                     the type checker should have caught this",
                    variant
                );

                let target = TargetInfo { ptr_bytes: 8 };
                let mut cursor = 0usize;
                for (&binding_name, field) in bindings.iter().zip(variant_def.fields.iter()) {
                    let field_ssa_ty = lower_type_hir(&field.field_type);
                    let align = ir::layout::alignof_ssa(&field_ssa_ty, target).unwrap_or(8);
                    cursor = Self::align_up(cursor, align);

                    let dest = self.current_block_data.fresh_value();
                    self.emit(Instruction::LoadField {
                        dest,
                        base: Operand::Value(scrutinee),
                        offset: 8 + cursor, // tag occupies bytes [0, 8), matches lower_enum_init
                    });
                    self.current_block_data
                        .value_types
                        .insert(dest, field_ssa_ty.clone());
                    self.var_map.insert(binding_name, dest);

                    let size = ir::layout::sizeof_ssa(&field_ssa_ty, target).unwrap_or(8);
                    cursor += size;
                }
            }
            _ => {}
        }
    }

    fn and_conds(&mut self, acc: Option<Value>, cond: Value) -> Value {
        match acc {
            None => cond,
            Some(prev) => {
                let v = self.current_block_data.fresh_value();
                self.emit(Instruction::Binary {
                    dest: v,
                    op: BinOp::BitAnd,
                    left: Operand::Value(prev),
                    right: Operand::Value(cond),
                });
                self.current_block_data.value_types.insert(v, SsaType::Bool);
                v
            }
        }
    }

    fn or_conds(&mut self, acc: Option<Value>, cond: Value) -> Value {
        match acc {
            None => cond,
            Some(prev) => {
                let v = self.current_block_data.fresh_value();
                self.emit(Instruction::Binary {
                    dest: v,
                    op: BinOp::BitOr,
                    left: Operand::Value(prev),
                    right: Operand::Value(cond),
                });
                self.current_block_data.value_types.insert(v, SsaType::Bool);
                v
            }
        }
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
            Some(method_name) => {
                let mut segments: Vec<StrId> = Vec::with_capacity(path.len() + 1);
                segments.push(member); // struct name
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
    /// stores a recursively-zeroed value into every field/element.
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
        object: &'a HirExpr<'a, 'bump>,
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

        let obj_val = self.lower_expr_as_receiver(object);

        if let Some(obj_ty) = self.current_block_data.value_types.get(&obj_val).cloned() {
            if let Some((offset, field_ty)) = self.resolve_slice_pseudo_field(&obj_ty, field) {
                let new_value = match op {
                    AssignmentOperator::Assign => rhs,
                    _ => {
                        let current = self.new_value();
                        self.emit(Instruction::LoadField {
                            dest: current,
                            base: Operand::Value(obj_val),
                            offset,
                        });
                        self.current_block_data
                            .value_types
                            .insert(current, field_ty.clone());

                        let bin_op = assign_op_to_bin_op(op);
                        let dest = self.new_value();
                        self.emit(Instruction::Binary {
                            dest,
                            op: bin_op,
                            left: Operand::Value(current),
                            right: Operand::Value(rhs),
                        });
                        self.current_block_data.value_types.insert(dest, field_ty);
                        dest
                    }
                };

                self.emit(Instruction::StoreField {
                    base: Operand::Value(obj_val),
                    offset,
                    value: Operand::Value(new_value),
                });

                return new_value;
            }
        }

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

        let is_slice = matches!(self.current_block_data.value_types.get(&new_value), Some(SsaType::Owned(inner)) if matches!(inner.as_ref(), SsaType::Slice(_)))
            || matches!(
                self.current_block_data.value_types.get(&new_value),
                Some(SsaType::Slice(_))
            );

        if is_slice {
            let ptr_val = self.new_value();
            let len_val = self.new_value();
            let cap_val = self.new_value();

            self.current_block_data
                .value_types
                .insert(ptr_val, SsaType::Pointer(Box::new(SsaType::I8)));
            self.current_block_data
                .value_types
                .insert(len_val, SsaType::Usize);
            self.current_block_data
                .value_types
                .insert(cap_val, SsaType::Usize);

            self.emit(Instruction::LoadField {
                dest: ptr_val,
                base: Operand::Value(new_value),
                offset: 0,
            });
            self.emit(Instruction::LoadField {
                dest: len_val,
                base: Operand::Value(new_value),
                offset: 8,
            });
            self.emit(Instruction::LoadField {
                dest: cap_val,
                base: Operand::Value(new_value),
                offset: 16,
            });

            self.emit(Instruction::StoreField {
                base: Operand::Value(obj_val),
                offset: field_offset + 0,
                value: Operand::Value(ptr_val),
            });
            self.emit(Instruction::StoreField {
                base: Operand::Value(obj_val),
                offset: field_offset + 8,
                value: Operand::Value(len_val),
            });
            self.emit(Instruction::StoreField {
                base: Operand::Value(obj_val),
                offset: field_offset + 16,
                value: Operand::Value(cap_val),
            });
        } else {
            self.emit(Instruction::StoreField {
                base: Operand::Value(obj_val),
                offset: field_offset,
                value: Operand::Value(new_value),
            });
        }

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
        match op {
            Operator::LogicalAnd => self.lower_short_circuit_and(left, right),
            Operator::LogicalOr => self.lower_short_circuit_or(left, right),
            _ => {
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
        }
    }

    fn lower_struct_init(&mut self, name: &HirExpr, args: &[HirFieldInit<'a, 'bump>]) -> Value {
        let struct_name = match name {
            HirExpr::Ident(n, _) => *n,
            other => panic!("StructInit name must be identifier; got {:?}", other),
        };

        let obj = self.new_value();

        let field_types: Vec<SsaType> = if let Some(hir_struct) = self.structs.get(&struct_name) {
            hir_struct
                .fields
                .iter()
                .map(|f| lower_type_hir(&f.field_type))
                .collect()
        } else {
            panic!("Struct {} not found", struct_name)
        };

        let mut arg_values: Vec<Value> = Vec::with_capacity(args.len());
        for (i, arg) in args.iter().enumerate() {
            if matches!(field_types.get(i), Some(SsaType::User(_, _))) {
                // TODO: make arg.value in a way where it ignores position
                record_move_if_any(self.scope_stack, self.drop_state, &arg.value);
            }
            arg_values.push(self.lower_expr(&arg.value));
        }

        let alloc_ty = SsaType::User(struct_name, field_types.clone());

        self.emit(Instruction::StackAlloc {
            dest: obj,
            ty: alloc_ty.clone(),
            count: 0,
        });

        self.current_block_data.value_types.insert(obj, alloc_ty);

        if !arg_values.is_empty() {
            self.init_struct_fields_from_args(obj, struct_name, &arg_values);
        }

        self.store_vtable_if_any(obj, struct_name);
        obj
    }

    fn init_struct_fields_from_args(&mut self, obj: Value, struct_name: StrId, args: &Vec<Value>) {
        let offsets = self
            .struct_field_offsets
            .get(&struct_name)
            .unwrap_or_else(|| {
                panic!(
                    "Unknown struct {} when initializing",
                    self.context.resolve_string(&*struct_name)
                )
            });

        let mut fields_by_offset: Vec<(&StrId, &usize)> = offsets.iter().collect();
        fields_by_offset.sort_by_key(|(_, off)| *off);

        for (i, (field_name, _)) in fields_by_offset.iter().enumerate() {
            if i >= args.len() {
                break;
            }

            let field_offset = offsets.get(*field_name).copied().unwrap();
            let val = args[i];
            let val_ty = self.current_block_data.value_types.get(&val).cloned();

            let is_slice = matches!(val_ty, Some(SsaType::Owned(ref inner)) if matches!(inner.as_ref(), SsaType::Slice(_)))
                || matches!(val_ty, Some(SsaType::Slice(_)));

            if is_slice {
                let ptr_val = self.new_value();
                let len_val = self.new_value();
                let cap_val = self.new_value();

                self.current_block_data
                    .value_types
                    .insert(ptr_val, SsaType::Pointer(Box::new(SsaType::I8)));
                self.current_block_data
                    .value_types
                    .insert(len_val, SsaType::Usize);
                self.current_block_data
                    .value_types
                    .insert(cap_val, SsaType::Usize);

                self.emit(Instruction::LoadField {
                    dest: ptr_val,
                    base: Operand::Value(val),
                    offset: 0,
                });
                self.emit(Instruction::LoadField {
                    dest: len_val,
                    base: Operand::Value(val),
                    offset: 8,
                });
                self.emit(Instruction::LoadField {
                    dest: cap_val,
                    base: Operand::Value(val),
                    offset: 16,
                });

                self.emit(Instruction::StoreField {
                    base: Operand::Value(obj),
                    offset: field_offset + 0,
                    value: Operand::Value(ptr_val),
                });
                self.emit(Instruction::StoreField {
                    base: Operand::Value(obj),
                    offset: field_offset + 8,
                    value: Operand::Value(len_val),
                });
                self.emit(Instruction::StoreField {
                    base: Operand::Value(obj),
                    offset: field_offset + 16,
                    value: Operand::Value(cap_val),
                });
            } else {
                self.emit(Instruction::StoreField {
                    base: Operand::Value(obj),
                    offset: field_offset,
                    value: Operand::Value(val),
                });
            }
        }
    }

    fn store_vtable_if_any(&mut self, obj: Value, struct_name: StrId) {
        let Some(vslots) = self.struct_vtable_slots.get(&struct_name) else {
            return;
        };
        if vslots.is_empty() {
            return;
        }

        let vtable_name =
            optimized_string_buffering::make_vtable_name(struct_name, self.context.clone());
        self.emit(Instruction::StoreField {
            base: Operand::Value(obj),
            offset: 0usize,
            value: Operand::GlobalRef(vtable_name),
        });
    }

    fn lower_field_access(&mut self, object: &HirExpr<'a, 'bump>, field: StrId) -> Value {
        let obj_val = self.lower_expr_as_receiver(object);

        if let Some(obj_ty) = self.current_block_data.value_types.get(&obj_val).cloned() {
            if let Some((offset, field_ty)) = self.resolve_slice_pseudo_field(&obj_ty, field) {
                let dest = self.new_value();
                self.emit(Instruction::LoadField {
                    dest,
                    base: Operand::Value(obj_val),
                    offset,
                });
                self.current_block_data.value_types.insert(dest, field_ty);
                return dest;
            }
        }

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
                "Could not determine object's struct for FieldAccess: {:?}",
                other
            ),
        };

        let offsets = self
            .struct_field_offsets
            .get(&cls_name)
            .unwrap_or_else(|| panic!("Unknown struct {} in FieldAccess", cls_name));

        let offset = *offsets
            .get(&field)
            .unwrap_or_else(|| panic!("Unknown field {} on struct {}", field, cls_name));

        let field_type = self
            .structs
            .get(&cls_name)
            .and_then(|hir_struct| hir_struct.fields.iter().find(|f| f.name == field))
            .map(|hir_field| lower_type_hir(&hir_field.field_type))
            .unwrap_or_else(|| {
                eprintln!(
                    "WARNING: lower_field_access could not find field {:?} on struct {:?}, defaulting to I64",
                    field, cls_name
                );
                SsaType::I64
            });

        let is_slice_field = matches!(field_type, SsaType::Slice(_))
            || matches!(field_type, SsaType::Owned(ref inner) if matches!(inner.as_ref(), SsaType::Slice(_)));

        if is_slice_field {
            let addr = self.new_value();
            self.emit(Instruction::FieldAddr {
                dest: addr,
                base: Operand::Value(obj_val),
                offset,
            });
            self.current_block_data
                .value_types
                .insert(addr, SsaType::Pointer(Box::new(field_type)));
            return addr;
        }

        let dest = self.new_value();
        self.emit(Instruction::LoadField {
            dest,
            base: Operand::Value(obj_val),
            offset,
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
                        "[lower_place_addr] cannot take address of a non-pointer-backed value of type {:?}, \
                         scalar locals must be stack-allocated to be referenced, not yet implemented",
                        ty
                    ),
                    None => panic!("[lower_place_addr] value has no known type"),
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
                "lower_field_addr: could not determine object's struct: {:?}",
                other
            ),
        };

        let offsets = self
            .struct_field_offsets
            .get(&cls_name)
            .unwrap_or_else(|| panic!("Unknown struct {} in FieldAccess", cls_name));
        let offset = *offsets
            .get(&field)
            .unwrap_or_else(|| panic!("Unknown field {} on struct {}", field, cls_name));

        let field_ty = self
            .structs
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

        let cls_name_id: Option<StrId> = maybe_cls_name_ssa
            .as_ref()
            .and_then(|ty| self.resolve_receiver_target_key(ty));

        if let Some(cls_name) = cls_name_id {
            let receiver_is_moved = self
                .struct_mangled_map
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
            "[lower_method_call] no mangled mapping or vtable slot found for method `{}` on struct `{:?}`.",
            self.context.resolve_string(&field),
            cls_name_id.map(|id| self.context.resolve_string(&id).to_string())
        );
    }

    fn resolve_receiver_target_key(&self, ty: &SsaType) -> Option<StrId> {
        match ty {
            SsaType::User(name, _) => Some(*name),
            SsaType::Pointer(inner) | SsaType::Owned(inner) => {
                self.resolve_receiver_target_key(inner)
            }
            other => self.builtin_target_key(other),
        }
    }

    fn builtin_target_key(&self, ty: &SsaType) -> Option<StrId> {
        let prim = |s: &str| Some(StrId(self.context.intern(s)));

        match ty {
            SsaType::I8 => prim("i8"),
            SsaType::I16 => prim("i16"),
            SsaType::I32 => prim("i32"),
            SsaType::I64 => prim("i64"),
            SsaType::I128 => prim("i128"),
            SsaType::U8 => prim("u8"),
            SsaType::U16 => prim("u16"),
            SsaType::U32 => prim("u32"),
            SsaType::U64 => prim("u64"),
            SsaType::U128 => prim("u128"),
            SsaType::Isize => prim("isize"),
            SsaType::Usize => prim("usize"),
            SsaType::F32 => prim("f32"),
            SsaType::F64 => prim("f64"),
            SsaType::Bool => prim("bool"),
            SsaType::String => prim("str"),
            SsaType::Char => prim("char"),
            SsaType::Slice(elem) | SsaType::Array(elem, _) => {
                let elem_key = match elem.as_ref() {
                    SsaType::User(n, _) => Some(*n),
                    other => self.builtin_target_key(other),
                };
                if let Some(ek) = elem_key {
                    let specialized = StrId(intern_fmt!(self.context, "slice_{}", ek));
                    if self.struct_mangled_map.contains_key(&specialized) {
                        return Some(specialized);
                    }
                }
                prim("slice")
            }
            SsaType::Owned(inner) => self.resolve_receiver_target_key(inner),
            _ => None,
        }
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
        if let HirExpr::FieldAccess {
            object: base_obj,
            field,
            ..
        }
        | HirExpr::Get {
            object: base_obj,
            field,
            ..
        } = object
        {
            let (addr, _) = self.lower_field_addr(base_obj, *field);
            return addr;
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

        if let Some(mmap) = self.struct_mangled_map.get(&cls_name) {
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

        if let Some(struct_slots) = self.struct_method_slots.get(&cls_name) {
            if let Some(slot_idx) = struct_slots.get(&field) {
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
                    panic!("[get_field_offset] pointer to non-User type: {:?}", inner)
                }
            }
            other => panic!(
                "Could not determine object's struct for FieldAccess: {:?}",
                other
            ),
        };

        self.struct_field_offsets
            .get(&cls_name)
            .unwrap_or_else(|| panic!("Unknown struct {} in FieldAccess", cls_name))
            .get(&field)
            .copied()
            .unwrap_or_else(|| {
                panic!(
                    "Unknown field {} on struct {}",
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
        let value_to_store = match op {
            AssignmentOperator::Assign => rhs,

            _ => {
                let current = self.new_value();

                self.emit(Instruction::Load {
                    dest: current,
                    ptr: Operand::Value(ptr),
                });

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
