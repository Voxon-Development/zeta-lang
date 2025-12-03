use crate::midend::ir::block_data::CurrentBlockData;
use crate::midend::ir::ir_conversion::{assign_op_to_bin_op, lower_operator_bin, lower_type_hir};
use crate::midend::ir::optimized_string_buffering;
use ir::hir::{AssignmentOperator, HirStruct, HirExpr, HirType, Operator, StrId};
use ir::ir_hasher::FxHashBuilder;
use ir::ssa_ir::{Function, Instruction, Operand, SsaType, Value};
use smallvec::SmallVec;
use std::collections::HashMap;
use std::sync::Arc;
use zetaruntime::string_pool::StringPool;

/// MIR Expr Lowerer, which converts HIR expressions to MIR expressions.
/// MIR is similar to a higher level representation of assembly which can be optimized in Zeta specific ways.
/// If we are here this means we passed all type safety, memory safety and semantic checks, and we can safely discard of stuff and all debug like span's
pub struct MirExprLowerer<'a> {
    pub current_block_data: &'a mut CurrentBlockData,
    pub var_map: &'a mut HashMap<StrId, Value, FxHashBuilder>,
    pub context: Arc<StringPool>,

    pub funcs: &'a HashMap<StrId, Function, FxHashBuilder>,
    pub class_field_offsets:
        &'a HashMap<StrId, HashMap<StrId, usize, FxHashBuilder>, FxHashBuilder>,
    pub class_method_slots:
        &'a HashMap<StrId, HashMap<StrId, usize, FxHashBuilder>, FxHashBuilder>,
    pub class_mangled_map:
        &'a HashMap<StrId, HashMap<StrId, StrId, FxHashBuilder>, FxHashBuilder>,
    pub class_vtable_slots: &'a HashMap<StrId, Vec<StrId>, FxHashBuilder>,
    pub interface_id_map: &'a HashMap<StrId, usize, FxHashBuilder>,
    pub interface_method_slots:
        &'a HashMap<StrId, HashMap<StrId, usize, FxHashBuilder>, FxHashBuilder>,
    pub classes: &'a HashMap<StrId, HirStruct<'a, 'a>, FxHashBuilder>,
}

impl<'a> MirExprLowerer<'a> {
    #[inline(always)]
    pub fn new(
        current_block_data: &'a mut CurrentBlockData,
        funcs: &'a HashMap<StrId, Function, FxHashBuilder>,
        var_map: &'a mut HashMap<StrId, Value, FxHashBuilder>,
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
        classes: &'a HashMap<StrId, HirStruct<'a, 'a>, FxHashBuilder>,
    ) -> Self {
        Self {
            current_block_data,
            funcs,
            var_map,
            context,
            class_field_offsets,
            class_method_slots,
            class_mangled_map,
            class_vtable_slots,
            interface_id_map,
            interface_method_slots,
            classes,
        }
    }

    /// Seed `var_map` and `current_block_data.value_types` with known parameters and locals.
    ///
    /// This should be called before lowering a function body. It associates each parameter
    /// and local with a fresh `Value` and registers its SSA type so later lowering can
    /// rely on lookups without panicking.
    pub fn seed_locals_and_params_from_hir(
        &mut self,
        params: &[(StrId, HirType<'a, 'a>)],
        locals: &[(StrId, HirType<'a, 'a>)],
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

    pub fn lower_expr(&mut self, expr: &HirExpr) -> Value {
        match expr {
            HirExpr::Number(n) => self.lower_expr_number(*n),

            HirExpr::Binary { left, op, right, span: _ } => self.lower_expr_binary(left, op, right),

            HirExpr::Ident(name) => *self.var_map.get(name).unwrap_or_else(|| {
                panic!("lower_expr: variable {:?} referenced before definition", name)
            }),

            HirExpr::StructInit { name, args, span: _ } => self.lower_class_init(name, args),

            HirExpr::FieldAccess { object, field, span: _ } | HirExpr::Get { object, field, span: _ } => {
                self.lower_field_access(object, *field)
            }

            HirExpr::Call { callee, args } => self.lower_call(callee, args),

            HirExpr::InterfaceCall {
                callee,
                args,
                interface,
            } => self.lower_interface_call(callee, args, *interface),

            HirExpr::Assignment { target, op, value, span: _ } => {
                self.lower_expr_assignment(target, *op, value)
            }

            HirExpr::String(s) => {
                // Strings are represented as constants
                let v = self.current_block_data.fresh_value();
                self.emit(Instruction::Const {
                    dest: v,
                    ty: SsaType::String,
                    value: Operand::ConstString(*s),
                });
                self.current_block_data.value_types.insert(v, SsaType::String);
                v
            }

            HirExpr::Boolean(b) => {
                // Booleans are represented as i64 (0 or 1)
                let v = self.current_block_data.fresh_value();
                self.emit(Instruction::Const {
                    dest: v,
                    ty: SsaType::I8,
                    value: Operand::ConstInt(if *b { 1 } else { 0 }),
                });
                self.current_block_data.value_types.insert(v, SsaType::I8);
                v
            }

            HirExpr::Decimal(d) => {
                // Decimals are represented as f64 constants
                let v = self.current_block_data.fresh_value();
                self.emit(Instruction::Const {
                    dest: v,
                    ty: SsaType::F64,
                    value: Operand::ConstFloat(*d),
                });
                self.current_block_data.value_types.insert(v, SsaType::F64);
                v
            }

            HirExpr::Tuple(elements) => {
                // Tuples are represented as a sequence of values
                // For now, we'll just lower the first element as a placeholder
                // Proper tuple support would need HIR/SSA changes
                if elements.is_empty() {
                    let v = self.current_block_data.fresh_value();
                    self.current_block_data.value_types.insert(v, SsaType::I64);
                    v
                } else {
                    self.lower_expr(&elements[0])
                }
            }

            HirExpr::InterpolatedString(_parts) => {
                // Interpolated strings need special handling
                // For now, return a placeholder string value
                let v = self.current_block_data.fresh_value();
                let empty_str = self.context.intern("");
                self.emit(Instruction::Const {
                    dest: v,
                    ty: SsaType::String,
                    value: Operand::ConstString(StrId(empty_str)),
                });
                self.current_block_data.value_types.insert(v, SsaType::String);
                v
            }

            HirExpr::EnumInit { enum_name, variant, args } => {
                // Enum initialization - create a tagged value
                let v = self.current_block_data.fresh_value();

                // Lower all arguments
                let mut arg_values: Vec<Value> = Vec::with_capacity(args.len());
                for arg in *args {
                    arg_values.push(self.lower_expr(arg));
                }

                // Allocate enum variant as a user type
                self.emit(Instruction::Alloc {
                    dest: v,
                    ty: SsaType::User(*enum_name, vec![]),
                    count: 0,
                });

                self.current_block_data.value_types.insert(v, SsaType::User(*enum_name, vec![]));
                v
            }

            HirExpr::ExprList { list, span: _ } => {
                // Expression lists evaluate all expressions and return the last one
                if list.is_empty() {
                    let v = self.current_block_data.fresh_value();
                    self.current_block_data.value_types.insert(v, SsaType::I64);
                    v
                } else {
                    // Evaluate all expressions, return the last one
                    let mut result = self.lower_expr(&list[0]);
                    for expr in &list[1..] {
                        result = self.lower_expr(expr);
                    }
                    result
                }
            }

            HirExpr::Comparison { left, op, right, span: _ } => {
                // Comparisons are similar to binary operations
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

    fn lower_expr_assignment(
        &mut self,
        target: &HirExpr,
        op: AssignmentOperator,
        value: &HirExpr,
    ) -> Value {
        let rhs = self.lower_expr(value);

        match target {
            HirExpr::Ident(name) => self.handle_ident(op, rhs, *name),

            HirExpr::FieldAccess { object, field, span: _ } | HirExpr::Get { object, field, span: _ } => {
                self.handle_field_access(op, rhs, object, *field)
            }

            _ => unimplemented!("Assignment target {:?} not yet supported", target),
        }
    }

    fn handle_ident(&mut self, op: AssignmentOperator, rhs: Value, name: StrId) -> Value {
        let var_val = *self.var_map.get(&name).unwrap_or_else(|| {
            panic!("handle_ident: variable {:?} referenced before definition", name)
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

                // Try to infer result type from lhs or rhs, fall back to I64
                let result_ty = self.current_block_data.value_types.get(&var_val)
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
        object: &HirExpr,
        field: StrId,
    ) -> Value {
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

                // ensure current has a type recorded
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

                // infer dest type conservatively
                let res_ty = self.current_block_data.value_types.get(&current)
                    .cloned()
                    .unwrap_or(SsaType::I64); // left side

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

    fn lower_expr_binary(&mut self, left: &HirExpr, op: &Operator, right: &HirExpr) -> Value {
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

    fn lower_class_init(&mut self, name: &HirExpr, args: &[HirExpr]) -> Value {
        let class_name = match name {
            HirExpr::Ident(n) => n.clone(),
            other => panic!("ClassInit name must be identifier; got {:?}", other),
        };

        let obj = self.new_value();

        // Lower args and record their values and types safely
        let mut arg_values: Vec<Value> = Vec::with_capacity(args.len());
        let mut types: Vec<SsaType> = Vec::with_capacity(args.len());
        for arg in args {
            let v = self.lower_expr(arg);
            let ty = self.current_block_data.value_types.get(&v)
                .cloned()
                .unwrap_or(SsaType::I32);

            arg_values.push(v);
            types.push(ty);
        }

        self.emit(Instruction::Alloc {
            dest: obj,
            ty: SsaType::User(class_name, types.clone()),
            count: 0,
        });

        self.current_block_data
            .value_types
            .insert(obj, SsaType::User(class_name, types.clone()));

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
                    self.context
                        .resolve_string(&*class_name)
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

        let vtable_name = optimized_string_buffering::make_vtable_name(
            class_name,
            self.context.clone(),
        );
        self.emit(Instruction::StoreField {
            base: Operand::Value(obj),
            offset: 0usize,
            value: Operand::GlobalRef(vtable_name),
        });
    }

    fn lower_field_access(&mut self, object: &HirExpr, field: StrId) -> Value {
        let obj_val = self.lower_expr(object);

        let cls_name = match self.current_block_data.value_types.get(&obj_val) {
            Some(SsaType::User(name, _args)) => {
                let resolved = self.context.resolve_string(&name);
                if let Some(inner_value_name) = resolved.split("_").last() {
                    StrId(
                        self.context
                            .intern(inner_value_name),
                    )
                } else {
                    panic!("Could not determine object's class for FieldAccess.")
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

        if let Some(hir_class) = self.classes.get(&cls_name) {
            if let Some(hir_field) = hir_class.fields.iter().find(|f| f.name == field) {
                let field_type = lower_type_hir(&hir_field.field_type);
                self.current_block_data
                    .value_types
                    .insert(dest, field_type);
            }
        }
        dest
    }

    fn lower_call(&mut self, callee: &HirExpr, args: &[HirExpr]) -> Value {
        match callee {
            HirExpr::Ident(fname) => {
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

                // Try to get function return type from `funcs` map, otherwise default
                if let Some(f) = self.funcs.get(fname) {
                    // adapt this according to your Function struct
                    // Placeholder: assume `f.return_type: Option<SsaType>` exists
                    #[allow(unused_variables)]
                    let ret_ty = None::<SsaType>;
                    if let Some(rt) = ret_ty {
                        self.current_block_data.value_types.insert(dest, rt);
                    } else {
                        self.current_block_data.value_types.insert(dest, SsaType::I64);
                    }
                } else {
                    self.current_block_data.value_types.insert(dest, SsaType::I64);
                }

                dest
            }

            HirExpr::FieldAccess { object, field, span: _ } | HirExpr::Get { object, field, span: _ } => {
                self.lower_method_call(object, *field, args)
            }

            other => unimplemented!(
                "Non-identifier callee not yet supported in Call: {:?}",
                other
            ),
        }
    }

    fn lower_method_call(&mut self, object: &HirExpr, field: StrId, args: &[HirExpr]) -> Value {
        // If the object is an identifier that is NOT a local variable, treat this as a
        // namespaced/static function call like Namespace.func(...), not a method call.
        if let HirExpr::Ident(scope_name) = object {
            if !self.var_map.contains_key(scope_name) {
                let mut operands: SmallVec<Operand, 8> = SmallVec::new();
                for a in args {
                    operands.push(Operand::Value(self.lower_expr(a)));
                }

                let scope_str = self
                    .context
                    .resolve_string(scope_name);

                let direct_name: StrId = optimized_string_buffering::build_scoped_name(
                    Some(scope_str),
                    field,
                    self.context.clone(),
                );

                let dest: Value = self.current_block_data.fresh_value();
                self.emit(Instruction::Call {
                    dest: Some(dest),
                    func: Operand::FunctionRef(direct_name),
                    args: operands,
                });

                // conservative default return type
                self.current_block_data.value_types.insert(dest, SsaType::I64);
                return dest;
            }
        }

        // Regular instance method call path
        let obj_val: Value = self.lower_expr(object);
        let mut operands: SmallVec<Operand, 8> = SmallVec::new();

        // Get the class name BEFORE we change the type
        let maybe_cls_name: Option<SsaType> = self.current_block_data.value_types.get(&obj_val).cloned();
        
        let maybe_cls_name: Option<&str> = match maybe_cls_name {
            Some(SsaType::User(ref name, _args)) => {
                let resolved = self.context.resolve_string(name);
                // The name might be in format "ClassName" or "module_ClassName"
                // Try to extract the class name by taking the last part after underscore
                if let Some(last) = resolved.split("_").last() {
                    if !last.is_empty() {
                        Some(last)
                    } else {
                        Some(resolved)
                    }
                } else {
                    Some(resolved)
                }
            }
            _ => None,
        };

        // Ensure the object (this) is treated as i64 (pointer type) for the call
        // But only if it's actually a User type (not already i64 or other type)
        if maybe_cls_name.is_some() {
            self.current_block_data.value_types.insert(obj_val, SsaType::I64);
        }
        
        operands.push(Operand::Value(obj_val));
        for a in args {
            let av = self.lower_expr(a);
            operands.push(Operand::Value(av));
        }

        let cls_name_id: Option<StrId> = if let Some(cls_name) = maybe_cls_name {
            Some(StrId(
                self.context.intern(cls_name),
            ))
        } else {
            None
        };

        if let Some(value) = self.emit_call(field, obj_val, &mut operands, cls_name_id) {
            return value;
        }

        let direct_name: StrId = optimized_string_buffering::build_scoped_name(
            maybe_cls_name,
            field,
            self.context.clone(),
        );

        let dest: Value = self.current_block_data.fresh_value();
        self.emit(Instruction::Call {
            dest: Some(dest),
            func: Operand::FunctionRef(direct_name),
            args: operands,
        });

        self.current_block_data.value_types.insert(dest, SsaType::I64);
        dest
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

        if let Some(class_slots) = self.class_method_slots.get(&cls_name) {
            let Some(slot_idx) = class_slots.get(&field) else {
                return None;
            };
            let dest = self.current_block_data.fresh_value();
            self.emit(Instruction::ClassCall {
                dest: Some(dest),
                object: obj_val,
                method_id: *slot_idx,
                args: operands.clone(),
            });

            // conservative default return type
            self.current_block_data.value_types.insert(dest, SsaType::I64);
            return Some(dest);
        }

        if let Some(mmap) = self.class_mangled_map.get(&cls_name) {
            let Some(mangled_name) = mmap.get(&field) else {
                return None;
            };

            let dest = self.current_block_data.fresh_value();
            self.emit(Instruction::Call {
                dest: Some(dest),
                func: Operand::FunctionRef(mangled_name.clone()),
                args: operands.clone(),
            });

            self.current_block_data.value_types.insert(dest, SsaType::I64);
            return Some(dest);
        }

        None
    }

    fn lower_interface_call(
        &mut self,
        callee: &HirExpr,
        args: &[HirExpr],
        interface: StrId,
    ) -> Value {
        let HirExpr::FieldAccess { object, field, span: _ } = callee else {
            panic!("InterfaceCall callee not FieldAccess; unsupported shape")
        };

        let obj_val = self.lower_expr(object);
        let mut operands: SmallVec<Operand, 8> = SmallVec::new();
        operands.push(Operand::Value(obj_val));

        for a in args {
            operands.push(Operand::Value(self.lower_expr(a)));
        }

        let iface_id = *self.interface_id_map.get(&interface).unwrap_or_else(|| {
            panic!(
                "Unknown interface {} in InterfaceCall",
                self.context
                    .resolve_string(&*interface)
            )
        });

        let iface_slot_map = self
            .interface_method_slots
            .get(&interface)
            .unwrap_or_else(|| {
                panic!(
                    "Interface {} has no method slots",
                    self.context
                        .resolve_string(&*interface)
                )
            });

        let method_slot_in_iface = iface_slot_map.get(field).unwrap_or_else(|| {
            panic!(
                "Interface {} has no method {}",
                self.context
                    .resolve_string(&*interface),
                self.context
                    .resolve_string(&*field)
            )
        });

        let interface_val = match self.current_block_data.value_types.get(&obj_val).cloned() {
            Some(SsaType::User(ref _name, args)) => {
                /*let Some(inner_value_name) = self
                    .context
                    .resolve_string(&*name)
                    .split("_")
                    .last()
                else {
                    panic!()
                };*/

                let upcast_dest = self.current_block_data.fresh_value();
                self.emit(Instruction::UpcastToInterface {
                    dest: upcast_dest,
                    object: obj_val,
                    interface_id: iface_id,
                });

                self.current_block_data
                    .value_types
                    .insert(upcast_dest, SsaType::User(interface, args));
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

        self.current_block_data.value_types.insert(dest, SsaType::I64);
        dest
    }

    fn get_field_offset(&mut self, obj: &Value, field: StrId) -> usize {
        let cls_name = match self.current_block_data.value_types.get(obj) {
            Some(SsaType::User(name, _args)) => {
                optimized_string_buffering::get_type(*name, self.context.clone())
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
                    self.context
                        .resolve_string(&*field),
                    self.context
                        .resolve_string(&*cls_name)
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