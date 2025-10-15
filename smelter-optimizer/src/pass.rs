use ir::ssa_ir::{BinOp, Function, Instruction, Module, Operand, SsaType, Value};
use std::collections::{HashMap, HashSet};
use ir::ir_hasher::FxHashBuilder;

// A trait for optimization passes over SSA IR
pub trait SsaPass {
    fn name(&self) -> &'static str;
    // Returns true if the pass modified the module
    fn run(&self, module: &mut Module) -> bool;
}

// An ADT enum to choose optimization passes
#[derive(Debug, Clone)]
pub enum OptimizationPass {
    ConstFolding,
    DeadCodeElimination,
    CopyPropagation,
    SimplifyCfg,
    InlineSmallFuncs { max_instructions: usize },
    CommonSubexpressionElimination,
}

impl SsaPass for OptimizationPass {
    fn name(&self) -> &'static str {
        match self {
            OptimizationPass::ConstFolding => "const-folding",
            OptimizationPass::DeadCodeElimination => "dead-code-elim",
            OptimizationPass::CopyPropagation => "copy-prop",
            OptimizationPass::SimplifyCfg => "simplify-cfg",
            OptimizationPass::InlineSmallFuncs { .. } => "inline-small",
            OptimizationPass::CommonSubexpressionElimination => "cse",
        }
    }

    fn run(&self, module: &mut Module) -> bool {
        match self {
            OptimizationPass::ConstFolding => const_folding(module),
            OptimizationPass::DeadCodeElimination => dead_code_elimination(module),
            OptimizationPass::CopyPropagation => copy_propagation(module),
            OptimizationPass::SimplifyCfg => simplify_cfg(module),
            OptimizationPass::InlineSmallFuncs { .. } => inline_small_funcs(module),
            OptimizationPass::CommonSubexpressionElimination => cse(module),
        }
    }
}

// =========================
// Pass manager
// =========================
#[derive(Default)]
pub struct PassManager {
    passes: Vec<OptimizationPass>,
    pub max_iterations: usize,
}

impl PassManager {
    pub fn new() -> Self { Self { passes: Vec::new(), max_iterations: 6 } }

    pub fn with_default_pipeline() -> Self {
        // A reasonable default order
        let mut pm = Self::new();
        pm.passes.push(OptimizationPass::CopyPropagation);
        pm.passes.push(OptimizationPass::ConstFolding);
        pm.passes.push(OptimizationPass::DeadCodeElimination);
        pm.passes.push(OptimizationPass::SimplifyCfg);
        pm
    }

    pub fn add_pass(&mut self, pass: OptimizationPass) { self.passes.push(pass); }

    // Run passes to a fixpoint (or until max_iterations)
    pub fn run(&self, module: &mut Module) -> usize {
        let mut total_changes = 0usize;
        for _ in 0..self.max_iterations {
            let mut any_change = false;
            for pass in &self.passes {
                let changed = pass.run(module);
                any_change |= changed;
                if changed { total_changes += 1; }
            }
            if !any_change { break; }
        }
        total_changes
    }
}

// =========================
// Pass implementations
// =========================

fn const_folding(module: &mut Module) -> bool {
    let mut changed = false;

    for func in module.funcs.values_mut() {
        for block in func.blocks.iter_mut() {
            for inst in block.instructions.iter_mut() {
                match inst {
                    Instruction::Binary { dest, op, left, right } => {
                        if let (Some(lc), Some(rc)) = (as_const(left), as_const(right)) {
                            if let Some(folded) = fold_binop(*op, lc, rc) {
                                let ty = func.value_types.get(dest).cloned().unwrap_or(SsaType::I64);
                                *inst = Instruction::Const { dest: *dest, ty, value: folded };
                                changed = true;
                            }
                        }
                    }
                    _ => {}
                }
            }
        }
    }

    changed
}

fn as_const(op: &Operand) -> Option<Operand> {
    match op {
        Operand::ConstInt(_) | Operand::ConstBool(_) | Operand::ConstString(_) => Some(op.clone()),
        _ => None,
    }
}

fn fold_binop(op: BinOp, left: Operand, right: Operand) -> Option<Operand> {
    use BinOp::*;
    match (left, right) {
        (Operand::ConstInt(a), Operand::ConstInt(b)) => {
            let v = match op {
                Add => a.wrapping_add(b),
                Sub => a.wrapping_sub(b),
                Mul => a.wrapping_mul(b),
                Div => if b == 0 { return None } else { a.wrapping_div(b) },
                Mod => if b == 0 { return None } else { a.wrapping_rem(b) },
                Eq => return Some(Operand::ConstBool(a == b)),
                Ne => return Some(Operand::ConstBool(a != b)),
                Lt => return Some(Operand::ConstBool(a < b)),
                Le => return Some(Operand::ConstBool(a <= b)),
                Gt => return Some(Operand::ConstBool(a > b)),
                Ge => return Some(Operand::ConstBool(a >= b)),
                _ => return None,
            };
            Some(Operand::ConstInt(v))
        }
        (Operand::ConstBool(a), Operand::ConstBool(b)) => {
            // Only comparisons make sense; arithmetic no-op
            match op {
                Eq => Some(Operand::ConstBool(a == b)),
                Ne => Some(Operand::ConstBool(a != b)),
                _ => None,
            }
        }
        _ => None,
    }
}

fn dead_code_elimination(module: &mut Module) -> bool {
    let mut changed = false;

    for func in module.funcs.values_mut() {
        // Compute use-set of SSA values across all instructions
        let used = compute_used_values(func);

        for block in func.blocks.iter_mut() {
            let mut new_insts = Vec::with_capacity(block.instructions.len());
            for inst in block.instructions.drain(..) {
                let keep = match &inst {
                    Instruction::Binary { dest, .. } => used.contains(dest),
                    Instruction::Unary { dest, .. } => used.contains(dest),
                    Instruction::Phi { dest, .. } => used.contains(dest),
                    Instruction::Call { dest, .. } => dest.is_some() && used.contains(&dest.unwrap()) || has_side_effects(&inst),
                    Instruction::Interpolate { dest, .. } => used.contains(dest),
                    Instruction::EnumConstruct { dest, .. } => used.contains(dest),
                    Instruction::MatchEnum { .. } => true, // control flow
                    Instruction::Jump { .. } | Instruction::Branch { .. } | Instruction::Ret { .. } => true,
                    Instruction::Const { dest, .. } => used.contains(dest),
                    Instruction::ClassCall { dest, .. } | Instruction::InterfaceDispatch { dest, .. } => dest.map(|d| used.contains(&d)).unwrap_or(true) || has_side_effects(&inst),
                    Instruction::UpcastToInterface { dest, .. } => used.contains(dest),
                    Instruction::Alloc { dest, .. } => used.contains(dest),
                    Instruction::StoreField { .. } => true, // side effect
                    Instruction::LoadField { dest, .. } => used.contains(dest),
                };
                if keep { new_insts.push(inst); } else { changed = true; }
            }
            block.instructions = new_insts;
        }
    }

    changed
}

fn has_side_effects(inst: &Instruction) -> bool {
    match inst {
        Instruction::StoreField { .. } => true,
        Instruction::Call { .. } | Instruction::ClassCall { .. } | Instruction::InterfaceDispatch { .. } => {
            // Be conservative: assume calls have side effects
            true
        }
        _ => false,
    }
}

fn compute_used_values(func: &Function) -> HashSet<Value> {
    let mut used: HashSet<Value> = HashSet::new();

    func.blocks.iter().flat_map(|block| block.instructions.iter())
        .for_each(|inst| compute_used_value(&mut used, inst));

    used
}

fn compute_used_value(mut used: &mut HashSet<Value>, inst: &Instruction) {
    match inst {
        Instruction::Binary { left, right, .. } => {
            collect_val(left, &mut used);
            collect_val(right, &mut used);
        }
        Instruction::Unary { operand, .. } => {
            collect_val(operand, &mut used);
        }
        Instruction::Phi { incomings, .. } => {
            for (_, v) in incomings {
                used.insert(*v);
            }
        }
        Instruction::Call { dest: _, func, args } => {
            collect_val(func, &mut used);
            for a in args { collect_val(a, &mut used); }
        }
        Instruction::Interpolate { parts, .. } => {
            for p in parts {
                if let ir::ssa_ir::InterpolationOperand::Value(v) = p { used.insert(*v); }
            }
        }
        Instruction::EnumConstruct { args, .. } => {
            for a in args { collect_val(a, &mut used); }
        }
        Instruction::MatchEnum { value, .. } => {
            used.insert(*value);
        }
        Instruction::Branch { cond, .. } => {
            collect_val(cond, &mut used);
        }
        Instruction::Ret { value } => {
            if let Some(v) = value { collect_val(v, &mut used); }
        }
        Instruction::ClassCall { object, args, .. } => {
            used.insert(*object);
            for a in args { collect_val(a, &mut used); }
        }
        Instruction::InterfaceDispatch { object, args, .. } => {
            used.insert(*object);
            for a in args { collect_val(a, &mut used); }
        }
        Instruction::UpcastToInterface { object, .. } => {
            used.insert(*object);
        }
        Instruction::StoreField { base, value, .. } => {
            collect_val(base, &mut used);
            collect_val(value, &mut used);
        }
        Instruction::LoadField { base, .. } => {
            collect_val(base, &mut used);
        }
        Instruction::Jump { .. } => {}
        Instruction::Const { .. } => {}
        Instruction::Alloc { .. } => {}
    }
}

fn collect_val(op: &Operand, used: &mut HashSet<Value>) {
    if let Operand::Value(v) = op { used.insert(*v); }
}

fn copy_propagation(module: &mut Module) -> bool {
    let mut changed = false;

    for func in module.funcs.values_mut() {
        // Map SSA value -> constant operand (if known)
        let const_map = build_const_map(func);

        for block in func.blocks.iter_mut() {
            for inst in block.instructions.iter_mut() {
                changed |= rewrite_operands_with_consts(inst, &const_map);
            }
        }
    }

    changed
}

fn build_const_map(func: &Function) -> HashMap<Value, Operand, FxHashBuilder> {
    let mut map = HashMap::with_hasher(FxHashBuilder);
    for block in &func.blocks {
        for inst in &block.instructions {
            if let Instruction::Const { dest, value, .. } = inst { map.insert(*dest, value.clone()); }
        }
    }
    map
}

fn rewrite_operands_with_consts(inst: &mut Instruction, const_map: &HashMap<Value, Operand, FxHashBuilder>) -> bool {
    let mut changed = false;
    let mut subst = |op: &mut Operand| {
        if let Operand::Value(v) = op {
            if let Some(k) = const_map.get(v) { *op = k.clone(); changed = true; }
        }
    };

    match inst {
        Instruction::Binary { left, right, .. } => { subst(left); subst(right); }
        Instruction::Unary { operand, .. } => { subst(operand); }
        Instruction::Phi { .. } => {}
        Instruction::Call { func, args, .. } => { subst(func); for a in args { subst(a); } }
        Instruction::Interpolate { .. } => {}
        Instruction::EnumConstruct { args, .. } => { for a in args { subst(a); } }
        Instruction::MatchEnum { .. } => {}
        Instruction::Jump { .. } => {}
        Instruction::Branch { cond, .. } => { subst(cond); }
        Instruction::Ret { value } => { if let Some(v) = value { subst(v); } }
        Instruction::Const { .. } => {}
        Instruction::ClassCall { object: _, args, .. } => { /* object is a Value, not Operand; can't rewrite */ for a in args { subst(a); } }
        Instruction::InterfaceDispatch { object: _, args, .. } => { for a in args { subst(a); } }
        Instruction::UpcastToInterface { .. } => {}
        Instruction::Alloc { .. } => {}
        Instruction::StoreField { base, value, .. } => { subst(base); subst(value); }
        Instruction::LoadField { base, .. } => { subst(base); }
    }

    changed
}

fn simplify_cfg(_module: &mut Module) -> bool {
    // Placeholder: CFG simplifications (e.g., remove empty blocks, branch threading)
    false
}

fn inline_small_funcs(_module: &mut Module) -> bool {
    // Placeholder: inlining would require SSA splice and value remapping
    false
}

fn cse(_module: &mut Module) -> bool {
    // Placeholder: local CSE could be added after const-fold and copy-prop
    false
}
