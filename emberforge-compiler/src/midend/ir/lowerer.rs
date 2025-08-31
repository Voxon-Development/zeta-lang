use std::collections::HashMap;
use crate::midend::ir::block_data::{CurrentBlockData};
use crate::midend::ir::expr_lowerer::MirExprLowerer;
use ir::hir::{
    Hir, HirClass, HirExpr, HirFunc, HirInterface, HirModule, HirParam, HirStmt, HirType, Operator,
};
use ir::sea_hasher::SeaHashBuilder;
use ir::ssa_ir::{
    BasicBlock, BinOp, BlockId, Function, Instruction, Module, Operand, SsaType, Value,
};

// Helpers shared across lowerers
pub(super) fn lower_type_hir(ty: &HirType) -> SsaType {
    match ty {
        HirType::I8 => SsaType::I8,
        HirType::I16 => SsaType::I16,
        HirType::I32 => SsaType::I32,
        HirType::I64 => SsaType::I64,
        HirType::I128 => SsaType::I128,
        HirType::U8 => SsaType::U8,
        HirType::U16 => SsaType::U16,
        HirType::U32 => SsaType::U32,
        HirType::U64 => SsaType::U64,
        HirType::U128 => SsaType::U64,
        HirType::F32 => SsaType::F32,
        HirType::F64 => SsaType::F64,
        HirType::Boolean => SsaType::Bool,
        HirType::String => SsaType::String,
        HirType::Class(name, _) | HirType::Interface(name, _) | HirType::Enum(name, _) => SsaType::User(name.clone()),
        HirType::Void => SsaType::Void,
        _ => unimplemented!("Unsupported type {:?}", ty),
    }
}

pub(super) fn lower_operator_bin(operator: &Operator) -> BinOp {
    match operator {
        Operator::Add => BinOp::Add,
        Operator::Subtract => BinOp::Sub,
        Operator::Multiply => BinOp::Mul,
        Operator::Divide => BinOp::Div,
        Operator::Modulo => BinOp::Mod,
        Operator::Equals => BinOp::Eq,
        Operator::NotEquals => BinOp::Ne,
        Operator::LessThan => BinOp::Lt,
        Operator::LessThanOrEqual => BinOp::Le,
        Operator::GreaterThan => BinOp::Gt,
        Operator::GreaterThanOrEqual => BinOp::Ge,
        _ => unimplemented!("Operator {:?} not yet lowered", operator),
    }
}

pub struct FunctionLowerer {
    current_block_data: CurrentBlockData,
    var_map: HashMap<String, Value, SeaHashBuilder>,

    // immutable metadata snapshots
    class_field_offsets: HashMap<String, HashMap<String, usize, SeaHashBuilder>, SeaHashBuilder>,
    class_method_slots: HashMap<String, HashMap<String, usize, SeaHashBuilder>, SeaHashBuilder>,
    class_mangled_map: HashMap<String, HashMap<String, String, SeaHashBuilder>, SeaHashBuilder>,
    class_vtable_slots: HashMap<String, Vec<String>, SeaHashBuilder>,
    interface_id_map: HashMap<String, usize, SeaHashBuilder>,
    interface_method_slots: HashMap<String, HashMap<String, usize, SeaHashBuilder>, SeaHashBuilder>,
    classes: HashMap<String, HirClass,SeaHashBuilder>,
}

impl FunctionLowerer {
    pub fn new(
        hir_fn: &HirFunc,
        class_field_offsets: &HashMap<String, HashMap<String, usize, SeaHashBuilder>, SeaHashBuilder>,
        class_method_slots: &HashMap<String, HashMap<String, usize, SeaHashBuilder>, SeaHashBuilder>,
        class_mangled_map: &HashMap<String, HashMap<String, String, SeaHashBuilder>, SeaHashBuilder>,
        class_vtable_slots: &HashMap<String, Vec<String>, SeaHashBuilder>,
        interface_id_map: &HashMap<String, usize, SeaHashBuilder>,
        interface_method_slots: &HashMap<String, HashMap<String, usize, SeaHashBuilder>, SeaHashBuilder>,
        classes: &HashMap<String, HirClass, SeaHashBuilder>,
    ) -> Self {
        let mut func = Function { name: hir_fn.name.clone(), params: Vec::new(), ret_type: lower_type_hir(hir_fn.return_type.as_ref().unwrap_or(&HirType::Void)), blocks: Vec::new(), value_types: HashMap::with_hasher(SeaHashBuilder), entry: BlockId(0) };
        let mut next_value = 0usize;
        let mut next_block = 0usize;
        let mut var_map = HashMap::with_hasher(SeaHashBuilder);
        let mut value_types = HashMap::with_hasher(SeaHashBuilder);
        // allocate params
        for p in &hir_fn.params {
            let v = Value(next_value); next_value += 1;
            let ty = lower_type_hir(&p.ty);
            func.params.push((v, ty.clone()));
            value_types.insert(v, ty);
            var_map.insert(p.name.clone(), v);
        }
        // create entry block
        let entry_bb = BlockId(next_block); 
        next_block += 1;
        func.entry = entry_bb;
        func.blocks.push(BasicBlock { id: entry_bb, instructions: Vec::new() });
        Self {
            current_block_data: CurrentBlockData::new(func, entry_bb, next_value, next_block, value_types),
            var_map,
            class_field_offsets: class_field_offsets.clone(),
            class_method_slots: class_method_slots.clone(),
            class_mangled_map: class_mangled_map.clone(),
            class_vtable_slots: class_vtable_slots.clone(),
            interface_id_map: interface_id_map.clone(),
            interface_method_slots: interface_method_slots.clone(),
            classes: classes.clone(),
        }
    }

    pub(super) fn lower_body(&mut self, body: Option<&HirStmt>) {
        if let Some(b) = body { match b { HirStmt::Block { body } => { for stmt in body { self.lower_stmt(stmt); } }, _ => unimplemented!("Function body must be a block"), } }
    }

    pub(super) fn lower_stmt(&mut self, stmt: &HirStmt) {
        match stmt {
            HirStmt::Let { name, value, .. } => {
                let val = {
                    let mut el = MirExprLowerer::new(
                        &mut self.current_block_data,
                        &mut self.var_map,
                        &self.class_field_offsets,
                        &self.class_method_slots,
                        &self.class_mangled_map,
                        &self.class_vtable_slots,
                        &self.interface_id_map,
                        &self.interface_method_slots,
                        &self.classes,
                    );
                    el.lower_expr(value)
                };
                self.var_map.insert(name.clone(), val);
            }
            HirStmt::Return(expr) => {
                let value = expr.as_ref().map(|e| {
                    let v = {
                        let mut el = MirExprLowerer::new(
                            &mut self.current_block_data,
                            &mut self.var_map,
                            &self.class_field_offsets,
                            &self.class_method_slots,
                            &self.class_mangled_map,
                            &self.class_vtable_slots,
                            &self.interface_id_map,
                            &self.interface_method_slots,
                            &self.classes,
                        );
                        el.lower_expr(e)
                    };
                    Operand::Value(v)
                });
                self.current_block_data.bb().instructions.push(Instruction::Ret { value });
            }
            HirStmt::Expr(expr) => {
                let mut el = MirExprLowerer::new(
                    &mut self.current_block_data,
                    &mut self.var_map,
                    &self.class_field_offsets,
                    &self.class_method_slots,
                    &self.class_mangled_map,
                    &self.class_vtable_slots,
                    &self.interface_id_map,
                    &self.interface_method_slots,
                    &self.classes,
                );
                let _ = el.lower_expr(expr);
            }
            _ => unimplemented!("Statement {:?} not yet lowered", stmt),
        }
    }





















    pub(super) fn finish(self) -> Function {
        self.current_block_data.finish()
    }
}

#[cfg(test)]
mod tests {
    use crate::midend::ir::module_lowerer::MirModuleLowerer;
    use super::*;

    fn iface_printable() -> HirInterface {
        HirInterface { name: "Printable".into(), visibility: ir::hir::Visibility::Public, methods: vec![
            HirFunc { name: "print".into(), visibility: ir::hir::Visibility::Public, is_static: false, is_unsafe: false, generics: vec![], params: vec![], return_type: None, body: None }
        ], generics: vec![] }
    }

    fn class_foo_with_print() -> HirClass {
        HirClass { name: "Foo".into(), visibility: ir::hir::Visibility::Public, generics: vec![], fields: vec![], methods: vec![
            HirFunc { name: "print".into(), visibility: ir::hir::Visibility::Public, is_static: false, is_unsafe: false, generics: vec![], params: vec![], return_type: None, body: Some(HirStmt::Block { body: vec![] }) }
        ], interfaces: vec!["Printable".into()] }
    }

    #[test]
    fn lowers_class_method_call_to_classcall() {
        let module_hir = HirModule { name: "root".into(), imports: vec![], items: vec![
            Hir::Interface(iface_printable()),
            Hir::Class(class_foo_with_print()),
        ] };
        let module = MirModuleLowerer::new().lower_module(&module_hir);
        let main_fn = module.funcs.get("main").expect("main lowered");
        let entry = main_fn.blocks.iter().find(|b| b.id == main_fn.entry).unwrap();
        let has_classcall = entry.instructions.iter().any(|insn| matches!(insn, Instruction::ClassCall { .. }));
        assert!(has_classcall, "expected ClassCall in lowered instructions: {:?}", entry.instructions);
        assert!(module.funcs.contains_key("Foo::print"), "mangled method lowered as function");
    }

    #[test]
    fn lowers_interface_call_to_dispatch() {
        let iface = iface_printable();
        let class = class_foo_with_print();
        let hir_func = {
            let let_stmt = HirStmt::Let { name: "foo".into(), ty: HirType::Class("Foo".into(), vec![]), value: HirExpr::ClassInit { name: Box::new(HirExpr::Ident("Foo".into())), args: vec![] }, mutable: false };
            let callee = HirExpr::FieldAccess { object: Box::new(HirExpr::Ident("foo".into())), field: "print".into() };
            let call = HirStmt::Expr(HirExpr::InterfaceCall { callee: Box::new(callee), args: vec![], interface: "Printable".into() });
            HirFunc { name: "main".into(), visibility: ir::hir::Visibility::Public, is_static: true, is_unsafe: false, generics: vec![], params: vec![], return_type: None, body: Some(HirStmt::Block { body: vec![let_stmt, call] }) }
        };
        let module_hir = HirModule { name: "root".into(), imports: vec![], items: vec![Hir::Interface(iface), Hir::Class(class), Hir::Func(hir_func)] };
        let module = MirModuleLowerer::new().lower_module(&module_hir);
        let main_fn = module.funcs.get("main").expect("main lowered");
        let entry = main_fn.blocks.iter().find(|b| b.id == main_fn.entry).unwrap();
        let has_upcast = entry.instructions.iter().any(|insn| matches!(insn, Instruction::UpcastToInterface { .. }));
        let has_dispatch = entry.instructions.iter().any(|insn| matches!(insn, Instruction::InterfaceDispatch { .. }));
        assert!(has_dispatch, "expected InterfaceDispatch in lowered instructions: {:?}", entry.instructions);
        assert!(has_upcast, "expected UpcastToInterface before dispatch when calling via interface");
    }
}
