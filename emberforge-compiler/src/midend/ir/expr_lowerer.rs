use crate::midend::ir::block_data::CurrentBlockData;
use crate::midend::ir::{ir_conversion, optimized_string_buffering};
use ir::hir::{AssignmentOperator, HirClass, HirExpr, Operator, StrId};
use ir::ir_hasher::FxHashBuilder;
use ir::ssa_ir::{BinOp, Instruction, Operand, SsaType, Value};
use smallvec::SmallVec;
use std::collections::HashMap;
use zetaruntime::string_pool::StringPool;
use crate::midend::ir::ir_conversion::{assign_op_to_bin_op, lower_operator_bin, lower_type_hir};

pub(super) struct MirExprLowerer<'a> {
    pub(super) current_block_data: &'a mut CurrentBlockData,
    pub(super) var_map: &'a mut HashMap<StrId, Value, FxHashBuilder>,

    pub string_pool: &'a mut StringPool,

    // read-only metadata
    pub(super) class_field_offsets: &'a HashMap<StrId, HashMap<StrId, usize, FxHashBuilder>, FxHashBuilder>,
    pub(super) class_method_slots: &'a HashMap<StrId, HashMap<StrId, usize, FxHashBuilder>, FxHashBuilder>,
    pub(super) class_mangled_map: &'a HashMap<StrId, HashMap<StrId, StrId, FxHashBuilder>, FxHashBuilder>,
    pub(super) class_vtable_slots: &'a HashMap<StrId, Vec<StrId>, FxHashBuilder>,
    pub(super) interface_id_map: &'a HashMap<StrId, usize, FxHashBuilder>,
    pub(super) interface_method_slots: &'a HashMap<StrId, HashMap<StrId, usize, FxHashBuilder>, FxHashBuilder>,
    pub(super) classes: &'a HashMap<StrId, HirClass, FxHashBuilder>,
}

impl<'a> MirExprLowerer<'a> {
    pub(super) fn new(
        current_block_data: &'a mut CurrentBlockData,
        var_map: &'a mut HashMap<StrId, Value, FxHashBuilder>,
        string_pool: &'a mut StringPool,
        class_field_offsets: &'a HashMap<StrId, HashMap<StrId, usize, FxHashBuilder>, FxHashBuilder>,
        class_method_slots: &'a HashMap<StrId, HashMap<StrId, usize, FxHashBuilder>, FxHashBuilder>,
        class_mangled_map: &'a HashMap<StrId, HashMap<StrId, StrId, FxHashBuilder>, FxHashBuilder>,
        class_vtable_slots: &'a HashMap<StrId, Vec<StrId>, FxHashBuilder>,
        interface_id_map: &'a HashMap<StrId, usize, FxHashBuilder>,
        interface_method_slots: &'a HashMap<StrId, HashMap<StrId, usize, FxHashBuilder>, FxHashBuilder>,
        classes: &'a HashMap<StrId, HirClass, FxHashBuilder>,
    ) -> Self {
        Self {
            current_block_data,
            var_map,
            string_pool,
            class_field_offsets,
            class_method_slots,
            class_mangled_map,
            class_vtable_slots,
            interface_id_map,
            interface_method_slots,
            classes,
        }
    }

    pub(super) fn lower_expr(&mut self, expr: &HirExpr) -> Value {
        match expr {
            HirExpr::Number(n) =>
                self.lower_expr_number(*n),

            HirExpr::Binary { left, op, right } =>
                self.lower_expr_binary(left, op, right),

            HirExpr::Ident(name) => self.var_map[name],

            HirExpr::ClassInit { name, args } =>
                self.lower_class_init(name, args),

            HirExpr::FieldAccess { object, field }
                    | HirExpr::Get { object, field } =>
                self.lower_field_access(object, *field),

            HirExpr::Call { callee, args } =>
                self.lower_call(callee, args),

            HirExpr::InterfaceCall { callee, args, interface } =>
                self.lower_interface_call(callee, args, *interface),

            HirExpr::Assignment { target, op, value } =>
                self.lower_expr_assignment(target, *op, value),

            other => unimplemented!("Expr {:?} not yet lowered", other),
        }
    }

    fn lower_expr_assignment(&mut self, target: &HirExpr, op: AssignmentOperator, value: &HirExpr) -> Value {
        // Lower RHS first
        let rhs = self.lower_expr(value);

        match target {
            HirExpr::Ident(name) => {
                self.handle_ident(op, rhs, *name)
            }

            HirExpr::FieldAccess { object, field }
                    | HirExpr::Get { object, field } => {
                self.handle_field_access(op, rhs, object, *field)
            }

            _ => unimplemented!("Assignment target {:?} not yet supported", target),
        }
    }

    fn handle_ident(&mut self, op: AssignmentOperator, rhs: Value, name: StrId) -> Value {
        let var_val = self.var_map[&name];

        // Determine if this is a simple assign or a compound assign
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
        object: &Box<HirExpr>,
        field: StrId
    ) -> Value {
        let obj_val = self.lower_expr(object);
        let field_offset = self.get_field_offset(&obj_val, field);

        // Load current field value if compound assign
        let new_value = match op {
            AssignmentOperator::Assign => rhs,
            _ => {
                let current = self.new_value();
                self.emit(Instruction::LoadField {
                    dest: current,
                    base: Operand::Value(obj_val),
                    offset: field_offset,
                });

                let bin_op = assign_op_to_bin_op(op);

                let dest = self.new_value();
                self.emit(Instruction::Binary {
                    dest,
                    op: bin_op,
                    left: Operand::Value(current),
                    right: Operand::Value(rhs),
                });
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
            value: Operand::ConstInt(n)
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
            right: Operand::Value(r)
        });
        self.current_block_data.value_types.insert(v, SsaType::I64);
        v
    }

    fn lower_class_init(&mut self, name: &Box<HirExpr>, args: &Vec<HirExpr>) -> Value {
        let class_name = match &**name {
            HirExpr::Ident(n) => n.clone(),
            other => panic!("ClassInit name must be identifier; got {:?}", other)
        };

        let obj = self.new_value();

        let args: Vec<Value> = args.iter().map(|arg| self.lower_expr(arg)).collect();

        self.emit(Instruction::Alloc {
            dest: obj,
            ty: SsaType::User(class_name, Vec::new()),
            count: 0,
        });

        self.current_block_data.value_types.insert(obj, SsaType::User(class_name, Vec::new()));
        if !args.is_empty() {
            self.init_class_fields_from_args(obj, class_name, &args);
        }
        self.store_vtable_if_any(obj, class_name);
        obj
    }

    fn init_class_fields_from_args(&mut self, obj: Value, class_name: StrId, args: &Vec<Value>) {
        let offsets = self.class_field_offsets.get(&class_name)
            .expect(&format!("Unknown class {} when initializing", self.string_pool.resolve_string(&*class_name)));

        let mut fields_by_offset: Vec<(&StrId, &usize)> = offsets.iter().collect();
        fields_by_offset.sort_by_key(|(_, off)| *off);

        for (i, (field_name, _)) in fields_by_offset.iter().enumerate() {
            if i >= args.len() { break; }

            self.emit(Instruction::StoreField {
                base: Operand::Value(obj),
                offset: offsets.get(*field_name).copied().unwrap(),
                value: Operand::Value(args[i])
            });
        }
    }

    fn store_vtable_if_any(&mut self, obj: Value, class_name: StrId) {
        let Some(vslots) = self.class_vtable_slots.get(&class_name) else { return; };
        if vslots.is_empty() { return; }

        let vtable_name = optimized_string_buffering::make_vtable_name(class_name, self.string_pool);
        self.emit(Instruction::StoreField {
            base: Operand::Value(obj),
            offset: 0usize,
            value: Operand::GlobalRef(vtable_name)
        });
    }

    fn lower_field_access(&mut self, object: &HirExpr, field: StrId) -> Value {
        let obj_val = self.lower_expr(object);

        let cls_name = match self.current_block_data.value_types.get(&obj_val) {
            Some(SsaType::User(name, args) ) => {
                if let Some(inner_value_name) = self.string_pool.resolve_string(name).split("_").last() {
                   StrId(self.string_pool.intern(inner_value_name))
                } else {
                    panic!("Could not determine object's class for FieldAccess.")
                }
            }
            other => panic!("Could not determine object's class for FieldAccess: {:?}", other),
        };

        let offsets = self.class_field_offsets.get(&cls_name)
            .expect(&format!("Unknown class {} in FieldAccess", cls_name));

        let offset = *offsets.get(&field)
            .expect(&format!("Unknown field {} on class {}", field, cls_name));

        let dest = self.new_value();
        self.emit(Instruction::LoadField {
            dest,
            base: Operand::Value(obj_val),
            offset
        });

        if let Some(hir_class) = self.classes.get(&cls_name) {
            if let Some(hir_field) = hir_class.fields.iter().find(|f| f.name == field) {
                self.current_block_data.value_types.insert(dest, lower_type_hir(&hir_field.field_type));
            }
        }
        dest
    }

    fn lower_call(&mut self, callee: &Box<HirExpr>, args: &Vec<HirExpr>) -> Value {
        match &**callee {
            HirExpr::Ident(fname) => {
                // global function call
                let arg_ops: SmallVec<[Operand; 8]> = args.iter()
                    .map(|a| Operand::Value(self.lower_expr(a)))
                    .collect();

                let dest = self.current_block_data.fresh_value();
                self.emit(Instruction::Call {
                    dest: Some(dest),
                    func: Operand::FunctionRef(fname.clone()),
                    args: arg_ops,
                });
                dest
            }

            HirExpr::FieldAccess { object, field }
                    | HirExpr::Get { object, field } => {
                // method call via object
                self.lower_method_call(object, *field, args)
            }

            other => unimplemented!("Non-identifier callee not yet supported in Call: {:?}", other),
        }
    }

    fn lower_method_call(&mut self, object: &HirExpr, field: StrId, args: &Vec<HirExpr>) -> Value {
        let obj_val: Value = self.lower_expr(object);
        let mut operands: SmallVec<[Operand; 8]> = SmallVec::new();

        operands.push(Operand::Value(obj_val));
        for a in args {
            let av = self.lower_expr(a);
            operands.push(Operand::Value(av));
        }

        let maybe_cls_name: Option<SsaType> = self.current_block_data.value_types.get(&obj_val).cloned();
        let maybe_cls_name: Option<&str> = match maybe_cls_name {
            Some(SsaType::User(ref name, _args)) => self.string_pool.resolve_string(name).split("_").last(),
            _ => None
        };

        let cls_name_id: Option<StrId> = if let Some(cls_name) = maybe_cls_name {
            Some(StrId(self.string_pool.intern(cls_name)))
        } else {
            None
        };

        if let Some(value) = self.emit_call(field, obj_val, &mut operands, cls_name_id) {
            return value;
        }

        let direct_name: StrId = optimized_string_buffering::build_scoped_name(maybe_cls_name, field, self.string_pool);

        let dest: Value = self.current_block_data.fresh_value();
        self.emit(Instruction::Call {
            dest: Some(dest),
            func: Operand::FunctionRef(direct_name),
            args: operands
        });

        dest
    }

    fn emit_call(
        &mut self,
        field: StrId,
        obj_val: Value,
        operands: &mut SmallVec<[Operand; 8]>,
        maybe_cls_name: Option<StrId>
    ) -> Option<Value> {
        let Some(cls_name) = maybe_cls_name else { return None };

        if let Some(class_slots) = self.class_method_slots.get(&cls_name) {
            let Some(slot_idx) = class_slots.get(&field) else { return None };
            let dest = self.current_block_data.fresh_value();
            self.emit(Instruction::ClassCall {
                dest: Some(dest),
                object: obj_val,
                method_id: *slot_idx,
                args: operands.clone()
            });

            return Some(dest);
        }

        if let Some(mmap) = self.class_mangled_map.get(&cls_name) {
            let Some(mangled_name) = mmap.get(&field) else { return None };

            let dest = self.current_block_data.fresh_value();
            self.emit(Instruction::Call {
                dest: Some(dest),
                func: Operand::FunctionRef(mangled_name.clone()),
                args: operands.clone()
            });

            return Some(dest);
        }

        None
    }

    fn lower_interface_call(&mut self, callee: &Box<HirExpr>, args: &Vec<HirExpr>, interface: StrId) -> Value {
        let HirExpr::FieldAccess { object, field } = &**callee else {
            panic!("InterfaceCall callee not FieldAccess; unsupported shape")
        };

        let obj_val = self.lower_expr(object);
        let mut operands: SmallVec<[Operand; 8]> = SmallVec::new();
        operands.push(Operand::Value(obj_val));

        for a in args {
            operands.push(Operand::Value(self.lower_expr(a)));
        }

        let iface_id = *self.interface_id_map.get(&interface)
            .unwrap_or_else(|| panic!("Unknown interface {} in InterfaceCall", self.string_pool.resolve_string(&*interface)));

        let iface_slot_map = self.interface_method_slots.get(&interface)
            .unwrap_or_else(|| panic!("Interface {} has no method slots", self.string_pool.resolve_string(&*interface)));

        let method_slot_in_iface = iface_slot_map.get(field)
            .unwrap_or_else(|| panic!("Interface {} has no method {}",
                                      self.string_pool.resolve_string(&*interface),
                                      self.string_pool.resolve_string(&*field)));

        let interface_val = match self.current_block_data.value_types.get(&obj_val).cloned() {
            Some(SsaType::User(ref name, args)) => {
                let Some(inner_value_name) = self.string_pool.resolve_string(&*name).split("_").last() else { panic!() };
                let upcast_dest = self.current_block_data.fresh_value();
                self.emit(Instruction::UpcastToInterface {
                    dest: upcast_dest,
                    object: obj_val,
                    interface_id: iface_id
                });

                self.current_block_data.value_types.insert(upcast_dest, SsaType::User(interface, args));
                upcast_dest
            },
            _ => obj_val,
        };

        let dest = self.current_block_data.fresh_value();
        self.emit(Instruction::InterfaceDispatch {
            dest: Some(dest),
            object: interface_val,
            method_slot: *method_slot_in_iface,
            args: operands
        });

        dest
    }

    fn get_field_offset(&mut self, obj: &Value, field: StrId) -> usize {
        let cls_name = match self.current_block_data.value_types.get(obj) {
            Some(SsaType::User(name, _args)) => optimized_string_buffering::get_type(*name, self.string_pool),
            other => panic!("Could not determine object's class for FieldAccess: {:?}", other),
        };

        self.class_field_offsets
            .get(&cls_name)
            .expect(&format!("Unknown class {} in FieldAccess", cls_name))
            .get(&field)
            .copied()
            .unwrap_or_else(|| panic!("Unknown field {} on class {}",
                                      self.string_pool.resolve_string(&*field),
                                      self.string_pool.resolve_string(&*cls_name)))
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