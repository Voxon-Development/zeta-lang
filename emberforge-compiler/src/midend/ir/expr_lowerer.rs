use std::collections::HashMap;
use crate::midend::ir::block_data::{CurrentBlockData};
use ir::hir::{AssignmentOperator, HirClass, HirExpr, Operator};
use ir::sea_hasher::SeaHashBuilder;
use ir::ssa_ir::{BinOp, Instruction, Operand, SsaType, Value};

pub(super) struct MirExprLowerer<'a> {
    pub(super) current_block_data: &'a mut CurrentBlockData,
    pub(super) var_map: &'a mut HashMap<String, Value, SeaHashBuilder>,

    // read-only metadata
    pub(super) class_field_offsets: &'a HashMap<String, HashMap<String, usize, SeaHashBuilder>, SeaHashBuilder>,
    pub(super) class_method_slots: &'a HashMap<String, HashMap<String, usize, SeaHashBuilder>, SeaHashBuilder>,
    pub(super) class_mangled_map: &'a HashMap<String, HashMap<String, String, SeaHashBuilder>, SeaHashBuilder>,
    pub(super) class_vtable_slots: &'a HashMap<String, Vec<String>, SeaHashBuilder>,
    pub(super) interface_id_map: &'a HashMap<String, usize, SeaHashBuilder>,
    pub(super) interface_method_slots: &'a HashMap<String, HashMap<String, usize, SeaHashBuilder>, SeaHashBuilder>,
    pub(super) classes: &'a HashMap<String, HirClass, SeaHashBuilder>,
}

impl<'a> MirExprLowerer<'a> {
    pub(super) fn new(
        current_block_data: &'a mut CurrentBlockData,
        var_map: &'a mut HashMap<String, Value, SeaHashBuilder>,
        class_field_offsets: &'a HashMap<String, HashMap<String, usize, SeaHashBuilder>, SeaHashBuilder>,
        class_method_slots: &'a HashMap<String, HashMap<String, usize, SeaHashBuilder>, SeaHashBuilder>,
        class_mangled_map: &'a HashMap<String, HashMap<String, String, SeaHashBuilder>, SeaHashBuilder>,
        class_vtable_slots: &'a HashMap<String, Vec<String>, SeaHashBuilder>,
        interface_id_map: &'a HashMap<String, usize, SeaHashBuilder>,
        interface_method_slots: &'a HashMap<String, HashMap<String, usize, SeaHashBuilder>, SeaHashBuilder>,
        classes: &'a HashMap<String, HirClass, SeaHashBuilder>,
    ) -> Self {
        Self {
            current_block_data,
            var_map,
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
            HirExpr::Number(n) => self.lower_expr_number(*n),
            HirExpr::Binary { left, op, right } => self.lower_expr_binary(left, op, right),
            HirExpr::Ident(name) => self.var_map[name],
            HirExpr::ClassInit { name, args } => self.lower_class_init(name, args),
            HirExpr::FieldAccess { object, field } | HirExpr::Get { object, field } => self.lower_field_access(object, field),
            HirExpr::Call { callee, args } => self.lower_call(callee, args),
            HirExpr::InterfaceCall { callee, args, interface } => self.lower_interface_call(callee, args, interface),
            HirExpr::Assignment { target, op, value } => self.lower_expr_assignment(target, *op, value),
            other => unimplemented!("Expr {:?} not yet lowered", other),
        }
    }

    fn lower_expr_assignment(&mut self, target: &HirExpr, op: AssignmentOperator, value: &HirExpr) -> Value {
        // Lower RHS first
        let rhs = self.lower_expr(value);

        match target {
            HirExpr::Ident(name) => {
                let var_val = self.var_map[name];

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
                        let bin_op = match op {
                            AssignmentOperator::AddAssign => BinOp::Add,
                            AssignmentOperator::SubtractAssign => BinOp::Sub,
                            AssignmentOperator::MultiplyAssign => BinOp::Mul,
                            AssignmentOperator::DivideAssign => BinOp::Div,
                            AssignmentOperator::ModuloAssign => BinOp::Mod,
                            AssignmentOperator::BitAndAssign => BinOp::BitAnd,
                            AssignmentOperator::BitOrAssign => BinOp::BitOr,
                            AssignmentOperator::BitXorAssign => BinOp::BitXor,
                            AssignmentOperator::ShiftLeftAssign => BinOp::ShiftLeft,
                            AssignmentOperator::ShiftRightAssign => BinOp::ShiftRight,
                            _ => unreachable!(),
                        };
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

            HirExpr::FieldAccess { object, field } | HirExpr::Get { object, field } => {
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

                        let bin_op = match op {
                            AssignmentOperator::AddAssign => BinOp::Add,
                            AssignmentOperator::SubtractAssign => BinOp::Sub,
                            AssignmentOperator::MultiplyAssign => BinOp::Mul,
                            AssignmentOperator::DivideAssign => BinOp::Div,
                            AssignmentOperator::ModuloAssign => BinOp::Mod,
                            AssignmentOperator::BitAndAssign => BinOp::BitAnd,
                            AssignmentOperator::BitOrAssign => BinOp::BitOr,
                            AssignmentOperator::BitXorAssign => BinOp::BitXor,
                            AssignmentOperator::ShiftLeftAssign => BinOp::ShiftLeft,
                            AssignmentOperator::ShiftRightAssign => BinOp::ShiftRight,
                            _ => unreachable!(),
                        };

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

            _ => unimplemented!("Assignment target {:?} not yet supported", target),
        }
    }


    fn lower_expr_number(&mut self, n: i64) -> Value {
        let v = self.current_block_data.fresh_value();
        self.current_block_data.bb().instructions.push(Instruction::Const { dest: v, ty: SsaType::I64, value: Operand::ConstInt(n) });
        self.current_block_data.value_types.insert(v, SsaType::I64);
        v
    }

    fn lower_expr_binary(&mut self, left: &HirExpr, op: &Operator, right: &HirExpr) -> Value {
        let l = self.lower_expr(left);
        let r = self.lower_expr(right);
        let v = self.current_block_data.fresh_value();
        self.current_block_data.bb().instructions.push(Instruction::Binary { dest: v, op: crate::midend::ir::lowerer::lower_operator_bin(op), left: Operand::Value(l), right: Operand::Value(r) });
        self.current_block_data.value_types.insert(v, SsaType::I64);
        v
    }

    fn lower_class_init(&mut self, name: &Box<HirExpr>, args: &Vec<HirExpr>) -> Value {
        let class_name = match &**name { HirExpr::Ident(n) => n.clone(), other => panic!("ClassInit name must be identifier; got {:?}", other) };
        let obj = self.current_block_data.fresh_value();
        self.current_block_data.bb().instructions.push(Instruction::Alloc { dest: obj, ty: SsaType::Ptr(Box::new(SsaType::User(class_name.clone()))) });
        self.current_block_data.value_types.insert(obj, SsaType::Ptr(Box::new(SsaType::User(class_name.clone()))));
        if !args.is_empty() { self.init_class_fields_from_args(obj, &class_name, args); }
        self.store_vtable_if_any(obj, &class_name);
        obj
    }

    fn init_class_fields_from_args(&mut self, obj: Value, class_name: &str, args: &Vec<HirExpr>) {
        let offsets = self.class_field_offsets.get(class_name).expect(&format!("Unknown class {} when initializing", class_name));
        let mut fields_by_offset: Vec<(&String, &usize)> = offsets.iter().collect();
        fields_by_offset.sort_by_key(|(_, off)| *off);
        for (i, (field_name, _)) in fields_by_offset.iter().enumerate() {
            if i >= args.len() { break; }
            let arg_val = self.lower_expr(&args[i]);
            self.current_block_data.bb().instructions.push(Instruction::StoreField { base: Operand::Value(obj), offset: *offsets.get(*field_name).unwrap(), value: Operand::Value(arg_val) });
        }
    }

    fn store_vtable_if_any(&mut self, obj: Value, class_name: &str) {
        if let Some(vslots) = self.class_vtable_slots.get(class_name) {
            if !vslots.is_empty() {
                let vtable_name = format!("vtable::{}", class_name);
                self.current_block_data.bb().instructions.push(Instruction::StoreField { base: Operand::Value(obj), offset: 0usize, value: Operand::GlobalRef(vtable_name) });
            }
        }
    }

    fn lower_field_access(&mut self, object: &HirExpr, field: &str) -> Value {
        let obj_val = self.lower_expr(object);
        let cls_name = match self.current_block_data.value_types.get(&obj_val) {
            Some(SsaType::Ptr(inner)) => match &**inner { SsaType::User(name) => name.clone(), other => panic!("FieldAccess on non-user pointer type {:?}", other) },
            other => panic!("Could not determine object's class for FieldAccess: {:?}", other),
        };
        let offsets = self.class_field_offsets.get(&cls_name).expect(&format!("Unknown class {} in FieldAccess", cls_name));
        let offset = *offsets.get(field).expect(&format!("Unknown field {} on class {}", field, cls_name));
        let dest = self.current_block_data.fresh_value();
        self.current_block_data.bb().instructions.push(Instruction::LoadField { dest, base: Operand::Value(obj_val), offset });
        if let Some(hir_class) = self.classes.get(&cls_name) {
            if let Some(hir_field) = hir_class.fields.iter().find(|f| f.name == field) {
                self.current_block_data.value_types.insert(dest, crate::midend::ir::lowerer::lower_type_hir(&hir_field.field_type));
            }
        }
        dest
    }

    fn lower_call(&mut self, callee: &Box<HirExpr>, args: &Vec<HirExpr>) -> Value {
        match &**callee {
            HirExpr::Ident(fname) => {
                // global function call
                let arg_ops: Vec<_> = args.iter().map(|a| Operand::Value(self.lower_expr(a))).collect();
                let dest = self.current_block_data.fresh_value();
                self.current_block_data.bb().instructions.push(Instruction::Call {
                    dest: Some(dest),
                    func: Operand::FunctionRef(fname.clone()),
                    args: arg_ops,
                });
                dest
            }

            HirExpr::FieldAccess { object, field } | HirExpr::Get { object, field } => {
                // method call via object
                self.lower_method_call(object, field, args)
            }

            other => unimplemented!("Non-identifier callee not yet supported in Call: {:?}", other),
        }
    }

    fn lower_method_call(&mut self, object: &HirExpr, field: &str, args: &Vec<HirExpr>) -> Value {
        let obj_val = self.lower_expr(object);
        let mut operands = Vec::new();
        operands.push(Operand::Value(obj_val));
        for a in args { let av = self.lower_expr(a); operands.push(Operand::Value(av)); }
        let maybe_cls_name = match self.current_block_data.value_types.get(&obj_val) { Some(SsaType::Ptr(inner)) => match &**inner { SsaType::User(name) => Some(name.clone()), _ => None }, _ => None };
        if let Some(ref cls_name) = maybe_cls_name {
            if let Some(class_slots) = self.class_method_slots.get(cls_name) {
                if let Some(slot_idx) = class_slots.get(field) {
                    let dest = self.current_block_data.fresh_value();
                    self.current_block_data.bb().instructions.push(Instruction::ClassCall { dest: Some(dest), object: obj_val, method_id: *slot_idx, args: operands });
                    return dest;
                }
            }
            if let Some(mmap) = self.class_mangled_map.get(cls_name) {
                if let Some(mangled_name) = mmap.get(field) {
                    let dest = self.current_block_data.fresh_value();
                    self.current_block_data.bb().instructions.push(Instruction::Call { dest: Some(dest), func: Operand::FunctionRef(mangled_name.clone()), args: operands });
                    return dest;
                }
            }
        }
        let direct_name = format!("{}::{}", maybe_cls_name.unwrap_or_else(|| "Unknown".into()), field);
        let dest = self.current_block_data.fresh_value();
        self.current_block_data.bb().instructions.push(Instruction::Call { dest: Some(dest), func: Operand::FunctionRef(direct_name), args: operands });
        dest
    }

    fn lower_interface_call(&mut self, callee: &Box<HirExpr>, args: &Vec<HirExpr>, interface: &String) -> Value {
        let HirExpr::FieldAccess { object, field } = &**callee else { panic!("InterfaceCall callee not FieldAccess; unsupported shape") };
        let obj_val = self.lower_expr(object);
        let mut operands = Vec::new();
        operands.push(Operand::Value(obj_val));
        for a in args { operands.push(Operand::Value(self.lower_expr(a))); }
        let iface_id = self.interface_id_map.get(interface).expect(&format!("Unknown interface {} in InterfaceCall", interface)).clone();
        let iface_slot_map = self.interface_method_slots.get(interface).expect(&format!("Interface {} has no method slots", interface));
        let method_slot_in_iface = iface_slot_map.get(field).expect(&format!("Interface {} has no method {}", interface, field)).clone();
        let interface_val = match self.current_block_data.value_types.get(&obj_val).cloned() {
            Some(SsaType::Ptr(inner)) => match *inner {
                SsaType::User(ref name) if name != interface => {
                    let upcast_dest = self.current_block_data.fresh_value();
                    self.current_block_data.bb().instructions.push(Instruction::UpcastToInterface { dest: upcast_dest, object: obj_val, interface_id: iface_id });
                    self.current_block_data.value_types.insert(upcast_dest, SsaType::Ptr(Box::new(SsaType::User(interface.clone()))));
                    upcast_dest
                }
                _ => obj_val,
            },
            _ => obj_val,
        };
        let dest = self.current_block_data.fresh_value();
        self.current_block_data.bb().instructions.push(Instruction::InterfaceDispatch { dest: Some(dest), object: interface_val, method_slot: method_slot_in_iface, args: operands });
        dest
    }

    fn get_field_offset(&self, obj: &Value, field: &str) -> usize {
        let cls_name = match self.current_block_data.value_types.get(obj) {
            Some(SsaType::Ptr(inner)) => match &**inner {
                SsaType::User(name) => name,
                other => panic!("FieldAccess on non-user pointer type {:?}", other),
            },
            other => panic!("Could not determine object's class for FieldAccess: {:?}", other),
        };
        self.class_field_offsets
            .get(cls_name)
            .expect(&format!("Unknown class {} in FieldAccess", cls_name))
            .get(field)
            .copied()
            .expect(&format!("Unknown field {} on class {}", field, cls_name))
    }

    fn emit(&mut self, instr: Instruction) {
        self.current_block_data.bb().instructions.push(instr);
    }

    fn new_value(&mut self) -> Value {
        self.current_block_data.fresh_value()
    }
}
