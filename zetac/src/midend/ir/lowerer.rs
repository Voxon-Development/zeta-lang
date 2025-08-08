use std::collections::HashMap;
use crate::midend::ir::hir::{Hir, HirExpr, HirFunc, HirModule, HirStmt, HirType, Operator};
use crate::midend::ir::ssa_ir::{BasicBlock, BinOp, BlockId, Function, Instruction, Module, Operand, SsaType, UnOp, Value};

pub struct MirLowerer {
    pub module: Module,
    current_fn: Option<Function>,
    current_block: Option<BlockId>,
    next_value: usize,
    next_block: usize,
    var_map: HashMap<String, Value>,
    value_types: HashMap<Value, SsaType>,
}

impl MirLowerer {
    pub fn new() -> Self {
        Self {
            module: Module::new(),
            current_fn: None,
            current_block: None,
            next_value: 0,
            next_block: 0,
            var_map: HashMap::new(),
            value_types: HashMap::new(),
        }
    }

    fn fresh_value(&mut self) -> Value {
        let v = Value(self.next_value);
        self.next_value += 1;
        v
    }

    fn fresh_block(&mut self) -> BlockId {
        let id = BlockId(self.next_block);
        self.next_block += 1;
        id
    }

    /// Top-level entry: lower an entire HIR module
    pub fn lower_module(mut self, hir_mod: &HirModule) -> Module {
        for item in &hir_mod.items {
            match item {
                Hir::Func(func) => self.lower_function(func),
                _ => unimplemented!("Class lowering not yet supported"),
            }
        }
        self.module
    }

    fn lower_function(&mut self, hir_fn: &HirFunc) {
        let mut func = Function {
            name: hir_fn.name.clone(),
            params: hir_fn.params.iter().map(|p| (self.fresh_value(), Self::lower_type(&p.ty))).collect(),
            ret_type: Self::lower_type(hir_fn.return_type.as_ref().unwrap_or(&HirType::Void)),
            blocks: Vec::new(),
            value_types: HashMap::new(),
            entry: BlockId(0),
        };

        let entry_bb = self.fresh_block();
        func.entry = entry_bb;
        func.blocks.push(BasicBlock { id: entry_bb, instructions: Vec::new() });

        self.current_block = Some(entry_bb);
        self.current_fn = Some(func);

        // bind params
        for ((val, ty), p) in self.current_fn.as_mut().unwrap().params.iter().zip(&hir_fn.params) {
            self.value_types.insert(*val, ty.clone());
            self.var_map.insert(p.name.clone(), *val);
        }

        // lower function body
        if let Some(body) = &hir_fn.body {
            match body {
                HirStmt::Block { body } => {
                    for stmt in body {
                        self.lower_stmt(stmt);
                    }
                }
                _ => unimplemented!("Function body must be a block"),
            }
        }

        let func = self.current_fn.take().unwrap();
        self.module.funcs.push(func);
    }

    fn lower_stmt(&mut self, stmt: &HirStmt) {
        match stmt {
            HirStmt::Let { name, value, .. } => {
                let val = self.lower_expr(value);
                self.var_map.insert(name.clone(), val);
            }
            HirStmt::Return(expr) => {
                let value = expr.as_ref().map(|e| Operand::Value(self.lower_expr(e)));
                self.current_block_mut().instructions.push(Instruction::Ret { value });
            }
            HirStmt::Expr(expr) => {
                self.lower_expr(expr);
            }
            _ => unimplemented!("Statement {:?} not yet lowered", stmt),
        }
    }

    fn lower_expr(&mut self, expr: &HirExpr) -> Value {
        match expr {
            HirExpr::Number(n) => {
                let v = self.fresh_value();
                self.current_block_mut().instructions.push(Instruction::Unary {
                    dest: v,
                    op: UnOp::Not,
                    operand: Operand::ConstInt(*n),
                });
                self.value_types.insert(v, SsaType::I64);
                v
            }
            HirExpr::Binary { left, op, right } => {
                let l = self.lower_expr(left);
                let r = self.lower_expr(right);
                let v = self.fresh_value();
                self.current_block_mut().instructions.push(Instruction::Binary {
                    dest: v,
                    op: Self::lower_operator(op),
                    left: Operand::Value(l),
                    right: Operand::Value(r),
                });
                self.value_types.insert(v, SsaType::I64);
                v
            }
            HirExpr::Ident(name) => self.var_map[name],
            _ => unimplemented!("Expr {:?} not yet lowered", expr),
        }
    }

    fn current_block_mut(&mut self) -> &mut BasicBlock {
        let bb_id = self.current_block.unwrap();
        self.current_fn.as_mut().unwrap().blocks.iter_mut().find(|b| b.id == bb_id).unwrap()
    }

    fn lower_type(ty: &HirType) -> SsaType {
        match ty {
            HirType::Primitive(p) if p == "i64" => SsaType::I64,
            HirType::Primitive(p) if p == "i32" => SsaType::I32,
            HirType::Primitive(p) if p == "bool" => SsaType::Bool,
            HirType::Primitive(p) if p == "f32" => SsaType::F32,
            HirType::Primitive(p) if p == "f64" => SsaType::F64,
            HirType::Primitive(p) if p == "str" => SsaType::String,
            HirType::Pointer(inner) => SsaType::Ptr(Box::new(Self::lower_type(inner))),
            HirType::Class(name, _) | HirType::Interface(name, _) | HirType::Enum(name, _) => SsaType::User(name.clone()),
            HirType::Void => SsaType::Void,
            _ => unimplemented!("Unsupported type {:?}", ty),
        }
    }

    fn lower_operator(operator: &Operator) -> BinOp {
        match operator {
            Operator::Equals => BinOp::Eq,
            Operator::NotEquals => BinOp::Ne,
        }
    }
}