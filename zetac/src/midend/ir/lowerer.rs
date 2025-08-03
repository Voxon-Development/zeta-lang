//! HIR → SSA lowering framework
//!
//! This module provides functions to convert your HIR (High-Level IR) into the SSA IR

use std::collections::HashMap;
use crate::midend::ir::hir::{Hir, HirExpr, HirFunc, HirModule, HirStmt, HirType, Operator};
use crate::midend::ir::ssa_ir::{BasicBlock, BinOp, BlockId, Function, Instruction, Module, Operand, SsaType, Type, UnOp, Value};

/// Context for SSA lowering: tracks next IDs and current block
pub struct LowerCtx {
    pub module: Module,
    pub current_fn: Option<Function>,
    pub current_block: Option<BlockId>,
    pub next_value: usize,
    pub next_block: usize,
    /// mapping HIR-local variables to SSA Values
    pub var_map: HashMap<String, Value>,
    pub value_types: HashMap<Value, SsaType>
}

impl LowerCtx {
    pub fn new() -> Self {
        LowerCtx {
            module: Module::new(),
            current_fn: None,
            current_block: None,
            next_value: 0,
            next_block: 0,
            var_map: HashMap::new(),
            value_types: HashMap::new()
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
}

/// Top-level entry: lower an entire HIR module
pub fn lower_module(hir_mod: &HirModule) -> Module {
    let mut ctx = LowerCtx::new();
    for item in &hir_mod.items {
        match item {
            Hir::Func(func) => lower_function(&mut ctx, func),
            _ => unimplemented!("Class lowering not yet supported"),
        }
    }
    ctx.module
}

fn lower_function(ctx: &mut LowerCtx, hir_fn: &HirFunc) {
    // create empty SSA Function
    let mut func = Function {
        name: hir_fn.name.clone(),
        params: hir_fn.params.iter().map(|p| (ctx.fresh_value(), lower_type(&p.ty))).collect(),
        ret_type: lower_type(hir_fn.return_type.as_ref().unwrap_or(&HirType::Void)),
        blocks: Vec::new(),
        value_types: HashMap::new(),
        entry: BlockId(0),
    };

    // entry block
    let entry_bb = ctx.fresh_block();
    func.entry = entry_bb;
    func.blocks.push(BasicBlock { id: entry_bb, instructions: Vec::new() });
    ctx.current_block = Some(entry_bb);
    ctx.current_fn = Some(func);

    // bind parameters
    for ((val, ty), p) in ctx.current_fn.as_mut().unwrap().params.iter().zip(&hir_fn.params) {
        ctx.value_types.insert(*val, ty.clone());
        ctx.var_map.insert(p.name.clone(), *val);
    }

    // lower body statements
    if let Some(body) = &hir_fn.body {
        for stmt in body {
            lower_stmt(ctx, stmt);
        }
    }

    // finalize function
    let func = ctx.current_fn.take().unwrap();
    ctx.module.funcs.push(func);
}

fn lower_stmt(ctx: &mut LowerCtx, stmt: &HirStmt) {
    let bb_id = ctx.current_block.unwrap();

    match stmt {
        HirStmt::Let { mutable, name, ty: _, value } => {
            let val = lower_expr(ctx, value);
            ctx.var_map.insert(name.clone(), val);
        }
        HirStmt::Return(expr) => {
            let value = match expr {
                Some(e) => {
                    let val = lower_expr(ctx, e);
                    Some(Operand::Value(val))
                }
                None => None,
            };

            ctx.current_fn.as_mut()
                .unwrap()
                .blocks
                .iter_mut()
                .find(|b| b.id == bb_id)
                .unwrap()
                .instructions
                .push(Instruction::Ret { value });
        }
        HirStmt::Expr(expr) => {
            lower_expr(ctx, expr);
        }
        _ => unimplemented!("Statement {:?} not yet lowered", stmt),
    }
}

fn lower_expr(ctx: &mut LowerCtx, expr: &HirExpr) -> Value {
    match expr {
        HirExpr::Number(n) => {
            let v = ctx.fresh_value();
            let bb = ctx.current_fn.as_mut().unwrap().blocks.iter_mut()
                .find(|b| b.id == ctx.current_block.unwrap()).unwrap();
            bb.instructions.push(Instruction::Unary { dest: v, op: UnOp::Not, operand: Operand::ConstInt(*n) });
            ctx.value_types.insert(v, SsaType::I64);
            v
        }
        HirExpr::Binary { left, op, right } => {
            let l = lower_expr(ctx, left);
            let r = lower_expr(ctx, right);
            let v = ctx.fresh_value();
            let bb = ctx.current_fn.as_mut().unwrap().blocks.iter_mut()
                .find(|b| b.id == ctx.current_block.unwrap()).unwrap();
            bb.instructions.push(Instruction::Binary {
                dest: v,
                op: lower_operator(op),
                left: Operand::Value(l),
                right: Operand::Value(r),
            });
            // assume i64 for now
            ctx.value_types.insert(v, SsaType::I64);
            v
        }
        HirExpr::Ident(name) => ctx.var_map[name],
        _ => unimplemented!("Expr {:?} not yet lowered", expr),
    }
}

fn lower_type(ty: &HirType) -> SsaType {
    match ty {
        HirType::Primitive(p) if p == "i64" => SsaType::I64,
        HirType::Primitive(p) if p == "i32" => SsaType::I32,
        HirType::Primitive(p) if p == "bool" => SsaType::Bool,
        HirType::Primitive(p) if p == "f32" => SsaType::F32,
        HirType::Primitive(p) if p == "f64" => SsaType::F64,
        HirType::Primitive(p) if p == "str" => SsaType::String,
        HirType::Pointer(inner) => SsaType::Ptr(Box::new(lower_type(inner))),
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