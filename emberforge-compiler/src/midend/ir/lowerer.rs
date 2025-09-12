use crate::midend::ir::block_data::CurrentBlockData;
use crate::midend::ir::expr_lowerer::MirExprLowerer;
use ir::hir::{Hir, HirClass, HirExpr, HirFunc, HirInterface, HirModule, HirStmt, HirType, Operator, StrId};
use ir::ir_hasher::FxHashBuilder;
use ir::ssa_ir::{
    BasicBlock, BinOp, BlockId, Function, Instruction, Operand, SsaType, Value,
};
use smallvec::SmallVec;
use std::collections::HashMap;
use zetaruntime::string_pool::StringPool;

pub struct FunctionLowerer {
    current_block_data: CurrentBlockData,
    var_map: HashMap<StrId, Value, FxHashBuilder>,

    // immutable metadata snapshots
    class_field_offsets: HashMap<StrId, HashMap<StrId, usize, FxHashBuilder>, FxHashBuilder>,
    class_method_slots: HashMap<StrId, HashMap<StrId, usize, FxHashBuilder>, FxHashBuilder>,
    class_mangled_map: HashMap<StrId, HashMap<StrId, StrId, FxHashBuilder>, FxHashBuilder>,
    class_vtable_slots: HashMap<StrId, Vec<StrId>, FxHashBuilder>,
    interface_id_map: HashMap<StrId, usize, FxHashBuilder>,
    interface_method_slots: HashMap<StrId, HashMap<StrId, usize, FxHashBuilder>, FxHashBuilder>,
    classes: HashMap<StrId, HirClass, FxHashBuilder>,

    string_pool: StringPool
}

impl FunctionLowerer {
    pub fn new(
        hir_fn: &HirFunc,
        class_field_offsets: &HashMap<StrId, HashMap<StrId, usize, FxHashBuilder>, FxHashBuilder>,
        class_method_slots: &HashMap<StrId, HashMap<StrId, usize, FxHashBuilder>, FxHashBuilder>,
        class_mangled_map: &HashMap<StrId, HashMap<StrId, StrId, FxHashBuilder>, FxHashBuilder>,
        class_vtable_slots: &HashMap<StrId, Vec<StrId>, FxHashBuilder>,
        interface_id_map: &HashMap<StrId, usize, FxHashBuilder>,
        interface_method_slots: &HashMap<StrId, HashMap<StrId, usize, FxHashBuilder>, FxHashBuilder>,
        classes: &HashMap<StrId, HirClass, FxHashBuilder>,
    ) -> Result<Self, std::alloc::AllocError> {
        let mut func = Function { 
            name: hir_fn.name.clone(), 
            params: SmallVec::new(),
            ret_type: lower_type_hir(hir_fn.return_type.as_ref().unwrap_or(&HirType::Void)), 
            blocks: SmallVec::new(),
            value_types: HashMap::with_hasher(FxHashBuilder), 
            entry: BlockId(0) 
        };

        let mut next_value = 0usize;
        let mut next_block = 0usize;
        let mut var_map = HashMap::with_hasher(FxHashBuilder);
        let mut value_types = HashMap::with_hasher(FxHashBuilder);
        // allocate params
        for p in &hir_fn.params {
            let v = Value(next_value); next_value += 1;
            let ty = lower_type_hir(&p.param_type);
            func.params.push((v, ty.clone()));
            value_types.insert(v, ty);
            var_map.insert(p.name.clone(), v);
        }
        // create entry block
        let entry_bb = BlockId(next_block); 
        next_block += 1;
        func.entry = entry_bb;
        func.blocks.push(BasicBlock { id: entry_bb, instructions: Vec::new() });
        Ok(Self {
            current_block_data: CurrentBlockData::new(func, entry_bb, next_value, next_block, value_types),
            var_map,
            class_field_offsets: class_field_offsets.clone(),
            class_method_slots: class_method_slots.clone(),
            class_mangled_map: class_mangled_map.clone(),
            class_vtable_slots: class_vtable_slots.clone(),
            interface_id_map: interface_id_map.clone(),
            interface_method_slots: interface_method_slots.clone(),
            classes: classes.clone(),
            string_pool: StringPool::new()?
        })
    }

    pub(super) fn lower_body(&mut self, body: Option<&HirStmt>) {
        if let Some(b) = body { match b { HirStmt::Block { body } => { for stmt in body { self.lower_stmt(stmt); } }, _ => unimplemented!("Function body must be a block"), } }
    }

    pub(super) fn lower_stmt(&mut self, stmt: &HirStmt) {
        match stmt {
            HirStmt::Let { name, value, .. } => {
                let val = self.allow_lowering_expr(value);
                self.var_map.insert(name.clone(), val);
            }
            HirStmt::Return(expr) => {
                let value = expr.as_ref().map(|e| {
                    let val = self.allow_lowering_expr(e);
                    Operand::Value(val)
                });
                self.current_block_data.bb().instructions.push(Instruction::Ret { value });
            }
            HirStmt::Expr(expr) => {
                let _ = self.allow_lowering_expr(expr);
            }
            _ => unimplemented!("Statement {:?} not yet lowered", stmt),
        }
    }

    fn allow_lowering_expr(&mut self, value: &HirExpr) -> Value {
        let val = {
            let mut el = MirExprLowerer::new(
                &mut self.current_block_data,
                &mut self.var_map,
                &mut self.string_pool,
                &self.class_field_offsets,
                &self.class_method_slots,
                &self.class_mangled_map,
                &self.class_vtable_slots,
                &self.interface_id_map,
                &self.interface_method_slots,
                &self.classes
            );
            el.lower_expr(value)
        };
        val
    }

    pub(super) fn finish(self) -> Function {
        self.current_block_data.finish()
    }
}