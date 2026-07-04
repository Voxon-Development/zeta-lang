use crate::midend::ir::block_data::CurrentBlockData;
use crate::midend::ir::ir_conversion::{assign_op_to_bin_op, lower_operator_bin, lower_type_hir};
use crate::optimized_string_buffering;
use codex_dependency_graph::DepGraph;
use core::panic;
use ir::hir::{AssignmentOperator, HirExpr, HirStruct, HirType, Operator, StrId};
use ir::ir_hasher::{FxHashBuilder, HashSet};
use ir::ssa_ir::{Function, Instruction, Operand, SsaType, Value};
use smallvec::SmallVec;
use std::collections::HashMap;
use std::marker::PhantomData;
use std::sync::Arc;
use zetaruntime::string_pool::StringPool;

/// MIR Expr Lowerer, which converts HIR expressions to MIR expressions.
/// MIR is similar to a higher level representation of assembly which can be optimized in Zeta specific ways.
/// If we are here this means we passed all type safety, memory safety and semantic checks, and we can safely discard of stuff and all debug like span's
pub struct MirExprLowerer<'el, 'a, 'cx, 'bump> {
    pub current_block_data: &'el mut CurrentBlockData,
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
    pub dep_graph: &'a DepGraph,
    pub module_idx: usize,
    global_funcs: &'a HashMap<StrId, Function, FxHashBuilder>,
}

impl<'el, 'a, 'cx, 'bump> MirExprLowerer<'el, 'a, 'cx, 'bump>
where
    'bump: 'a,
{
    #[inline(always)]
    pub fn new(
        current_block_data: &'el mut CurrentBlockData,
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
        dep_graph: &'a DepGraph,
        module_idx: usize,
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
            } => self.lower_class_init(name, args),

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
                *self.var_map.get(&this_name).unwrap_or_else(|| {
                    panic!("`this` referenced but not found in var_map, are we inside a method?")
                })
            }
            HirExpr::Ref { expr, .. } => {
                let src = self.lower_expr(expr);

                let dest = self.current_block_data.fresh_value();

                self.emit(Instruction::AddressOf { dest, source: src });

                let inner_ty = self
                    .current_block_data
                    .value_types
                    .get(&src)
                    .unwrap()
                    .clone();

                self.current_block_data
                    .value_types
                    .insert(dest, SsaType::Pointer(Box::new(inner_ty)));

                dest
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
                    ty: SsaType::I64, // TODO this is a placeholder; refined once type info flows through
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
        }
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

        optimized_string_buffering::build_module_scoped_name(
            path,
            member,
            extra,
            self.context.clone(),
        )
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

    fn lower_class_init(&mut self, name: &HirExpr, args: &[HirExpr<'a, 'bump>]) -> Value {
        let class_name = match name {
            HirExpr::Ident(n, _) => *n,
            other => panic!("ClassInit name must be identifier; got {:?}", other),
        };

        let obj = self.new_value();

        let mut arg_values: Vec<Value> = Vec::with_capacity(args.len());
        for arg in args {
            arg_values.push(self.lower_expr(arg));
        }

        // Use the declared field types from the HIR class definition, not the
        // runtime value types of the args, those may be stripped (e.g. User(Vec3f, [])
        // instead of User(Vec3f, [I64, I64, I64])) when they came from a call result.
        let field_types: Vec<SsaType> = if let Some(hir_class) = self.classes.get(&class_name) {
            hir_class
                .fields
                .iter()
                .map(|f| lower_type_hir(&f.field_type))
                .collect()
        } else {
            panic!("Class {} not found", class_name)
        };

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
                StrId(
                    self.context
                        .intern(resolved.split('_').next().unwrap_or(resolved)),
                )
            }
            Some(SsaType::Pointer(inner)) => {
                if let SsaType::User(name, _) = inner.as_ref() {
                    let resolved = self.context.resolve_string(name);
                    StrId(
                        self.context
                            .intern(resolved.split('_').next().unwrap_or(resolved)),
                    )
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
            let arg_ops: SmallVec<Operand, 8> = args
                .iter()
                .map(|a| Operand::Value(self.lower_expr(a)))
                .collect();

            let dest = self.current_block_data.fresh_value();
            self.emit(Instruction::Call {
                dest: Some(dest),
                func: Operand::FunctionRef(mangled),
                args: arg_ops,
            });

            let ret_ty = if self.extern_c_names.contains(&mangled) {
                SsaType::I64
            } else {
                self.funcs
                    .get(&mangled)
                    .or_else(|| self.global_funcs.get(&mangled))
                    .map(|f| f.ret_type.clone())
                    .unwrap_or_else(|| {
                        panic!(
                            "lower_call: unknown non-extern function `{:?}`, not in funcs table or global_funcs",
                            mangled
                        )
                    })
            };
            self.current_block_data.value_types.insert(dest, ret_ty);
            return dest;
        }

        match callee {
            HirExpr::Ident(fname, _) => {
                let arg_ops: SmallVec<Operand, 8> = args
                    .iter()
                    .map(|a| Operand::Value(self.lower_expr(a)))
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

    fn lower_method_call(
        &mut self,
        object: &HirExpr<'a, 'bump>,
        field: StrId,
        args: &[HirExpr<'a, 'bump>],
    ) -> Value {
        if let HirExpr::Ident(scope_name, _) = object {
            if !self.var_map.contains_key(scope_name) {
                let mut operands: SmallVec<Operand, 8> = SmallVec::new();
                for a in args {
                    operands.push(Operand::Value(self.lower_expr(a)));
                }

                let direct_name: StrId = optimized_string_buffering::mangle_method_name(
                    self.dep_graph,
                    self.module_idx,
                    *scope_name,
                    field,
                    self.context.clone(),
                );

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

        // Prefer direct mangled call over vtable dispatch for non-interface methods
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

        // Only use InterfaceDispatch (vtable dispatch) for interface methods
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
        let mut operands: SmallVec<Operand, 8> = SmallVec::new();

        for a in args {
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
            Some(SsaType::User(name, _)) => {
                optimized_string_buffering::get_type(*name, self.context.clone())
            }
            Some(SsaType::Pointer(inner)) => {
                if let SsaType::User(name, _) = inner.as_ref() {
                    optimized_string_buffering::get_type(*name, self.context.clone())
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
}

fn unwrap_pointers(ty: &SsaType) -> Option<&SsaType> {
    match ty {
        SsaType::Pointer(inner) => unwrap_pointers(inner),
        other => Some(other),
    }
}
