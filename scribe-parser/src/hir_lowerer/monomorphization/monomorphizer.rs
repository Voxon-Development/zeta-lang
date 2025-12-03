use std::collections::hash_map::Entry;
use std::cell::RefCell;
use std::sync::Arc;
use std::marker::PhantomData;
use std::rc::Rc;
use smallvec::SmallVec;

use super::naming::suffix_for_subs;
use super::type_substitution::substitute_type;
use crate::hir_lowerer::context::{LoweringCtx};
use ir::ir_hasher::{FxHashMap, HashMap};
use ir::hir::{HirExpr, HirFunc, HirParam, HirStmt, HirType, StrId};
use zetaruntime::arena::GrowableAtomicBump;
use zetaruntime::string_pool::StringPool;
/// Handles monomorphization of generic functions and expressions
pub struct Monomorphizer<'a, 'bump> {
    /// Maps (original_name, type_suffix) to instantiated function name
    pub instantiated_functions: RefCell<FxHashMap<(StrId, StrId), StrId>>,
    /// Maps (original_name, type_suffix) to instantiated class name
    pub instantiated_classes: RefCell<FxHashMap<(StrId, StrId), StrId>>,
    /// Reference to the lowering context
    functions: Rc<RefCell<FxHashMap<StrId, HirFunc<'a, 'bump>>>>,
    /// Bump allocator for allocations
    bump: Arc<GrowableAtomicBump<'bump>>,
    /// String interner for string operations
    context: Arc<StringPool>,
    _phantom: PhantomData<&'bump ()>,
}

impl<'a, 'bump> Monomorphizer<'a, 'bump> {
    pub fn new(
        context: Arc<StringPool>,
        bump: Arc<GrowableAtomicBump<'bump>>,
        functions: Rc<RefCell<FxHashMap<StrId, HirFunc<'a, 'bump>>>>,
    ) -> Self {
        Self {
            instantiated_functions: RefCell::new(FxHashMap::default()),
            instantiated_classes: RefCell::new(FxHashMap::default()),
            functions,
            bump,
            context,
            _phantom: PhantomData,
        }
    }

    /// Monomorphize a function with substitutions mapping generic-name -> concrete HirType
    pub fn monomorphize_function(
        &self,
        func: &HirFunc<'a, 'bump>,
        substitutions: &FxHashMap<StrId, HirType<'a, 'bump>>,
    ) -> Option<StrId> {
        // Create a new function with substitutions applied
        let mut new_func = func.clone();
        self.apply_substitutions_to_func(&mut new_func, substitutions);

        // Generate suffix for the specialized function name
        let suffix = suffix_for_subs(self.context.clone(), substitutions);
        let orig_name = new_func.name.clone();

        // Check if we've already monomorphized this function with these types
        let key = (orig_name.clone(), suffix.clone());
        let mut instantiated_functions = self.instantiated_functions.borrow_mut();
        
        if let Some(existing) = instantiated_functions.get(&key) {
            // Return existing monomorphized function name
            return Some(*existing);
        }

        // Generate new monomorphized function name
        const UNDERSCORE_LEN: usize = 1;
        let mut small_vec: SmallVec<u8, 32> =
            SmallVec::with_capacity(orig_name.len() + UNDERSCORE_LEN + suffix.len());
        small_vec.extend_from_slice(self.context.resolve_bytes(&*orig_name));
        small_vec.push(b'_');
        small_vec.extend_from_slice(self.context.resolve_bytes(&*suffix));

        let new_name = StrId(self.context.intern_bytes(small_vec.as_slice()));
        
        // Update function name and body with monomorphized types
        new_func.name = new_name;
        
        if let Some(body) = new_func.body {
            let new_body = self.monomorphize_stmt(&body, substitutions);
            new_func.body = Some(*self.bump.alloc_value_immutable(new_body));
        }

        // Insert the new function into the functions map
        self.functions.borrow_mut().insert(new_func.name.clone(), new_func);
        
        // Cache the monomorphized function name
        instantiated_functions.insert(key, new_name);
        
        Some(new_name)
    }

    fn apply_substitutions_to_func(
        &self,
        func: &mut HirFunc<'a, 'bump>,
        substitutions: &FxHashMap<StrId, HirType<'a, 'bump>>,
    ) {
        self.apply_substitutions_to_params(func, substitutions);

        if let Some(ret) = &mut func.return_type {
            *ret = substitute_type(ret, substitutions, self.bump.clone());
        }
    }

    fn apply_substitutions_to_params(&self, func: &mut HirFunc<'a, '_>, substitutions: &FxHashMap<StrId, HirType<'a, '_>>) {
        if let Some(params) = func.params {
            if params.is_empty() {
                func.params = None;
                return;
            }

            let mut new_params: Vec<HirParam> = Vec::new();

            for param in params.iter() {
                let new_param = match param {
                    HirParam::Normal { name, param_type } => {
                        HirParam::Normal {
                            name: *name,
                            param_type: substitute_type(param_type, substitutions, self.bump.clone()),
                        }
                    }
                    HirParam::This { param_type } => {
                        let new_type = param_type.as_ref().map(|ty| {
                            substitute_type(ty, substitutions, self.bump.clone())
                        });
                        HirParam::This { param_type: new_type }
                    }
                };
                new_params.push(new_param);
            }

            func.params = Some(self.bump.alloc_slice_immutable(&new_params));
        }
    }

    pub fn monomorphize_stmt(
        &self,
        stmt: &HirStmt<'a, 'bump>,
        substitutions: &HashMap<StrId, HirType<'_, 'a>>,
    ) -> HirStmt<'a, 'bump> {
        match stmt {
            HirStmt::Block { body } => {
                let new_body: Vec<HirStmt> = body.iter()
                    .map(|s| self.monomorphize_stmt(s, substitutions))
                    .collect();
                let body_slice = self.bump.alloc_slice(&new_body);
                HirStmt::Block { body: body_slice }
            }
            HirStmt::If { cond, then_block, else_block } => {
                let new_cond = self.monomorphize_expr(cond, substitutions);
                let new_then: Vec<HirStmt> = then_block.iter()
                    .map(|s| self.monomorphize_stmt(s, substitutions))
                    .collect();
                let then_slice = self.bump.alloc_slice(&new_then);
                let new_else = else_block.map(|e| {
                    let new_stmt = self.monomorphize_stmt(e, substitutions);
                    self.bump.alloc_value_immutable(new_stmt)
                });
                HirStmt::If {
                    cond: *self.bump.alloc_value_immutable(new_cond),
                    then_block: then_slice,
                    else_block: new_else,
                }
            }
            HirStmt::While { cond, body } => {
                let new_cond = self.monomorphize_expr(cond, substitutions);
                let new_body = self.monomorphize_stmt(body, substitutions);
                HirStmt::While {
                    cond: self.bump.alloc_value_immutable(new_cond),
                    body: self.bump.alloc_value_immutable(new_body),
                }
            }
            HirStmt::Let { name, ty, value } => {
                let new_ty = substitute_type(ty, substitutions, self.bump.clone());
                let new_value = self.monomorphize_expr(value, substitutions);
                HirStmt::Let {
                    name: *name,
                    ty: new_ty,
                    value: *self.bump.alloc_value_immutable(new_value),
                }
            }
            HirStmt::Return(opt) => {
                let new_opt = opt.map(|e| {
                    let new_expr = self.monomorphize_expr(e, substitutions);
                    self.bump.alloc_value_immutable(new_expr)
                });
                HirStmt::Return(new_opt)
            }
            HirStmt::Expr(e) => {
                let new_expr = self.monomorphize_expr(e, substitutions);
                HirStmt::Expr(self.bump.alloc_value_immutable(new_expr))
            }
            HirStmt::UnsafeBlock { body } => {
                let new_body = self.monomorphize_stmt(body, substitutions);
                HirStmt::UnsafeBlock {
                    body: self.bump.alloc_value_immutable(new_body),
                }
            }

            _ => stmt.clone(),
        }
    }

    pub fn monomorphize_expr(
        &self,
        expr: &HirExpr<'a, 'bump>,
        subs: &HashMap<StrId, HirType>,
    ) -> HirExpr<'a, 'bump> {
        match expr {
            HirExpr::Call { callee, args } => {
                let new_callee = self.monomorphize_expr(callee, subs);
                let new_args: Vec<HirExpr> = args.iter()
                    .map(|a| self.monomorphize_expr(a, subs))
                    .collect();
                let args_slice = self.bump.alloc_slice(&new_args);
                HirExpr::Call {
                    callee: self.bump.alloc_value_immutable(new_callee),
                    args: args_slice,
                }
            }
            HirExpr::InterfaceCall { callee, interface, args } => {
                let new_callee = self.monomorphize_expr(callee, subs);
                let new_args: Vec<HirExpr> = args.iter()
                    .map(|a| self.monomorphize_expr(a, subs))
                    .collect();
                let args_slice = self.bump.alloc_slice(&new_args);
                HirExpr::InterfaceCall {
                    callee: self.bump.alloc_value_immutable(new_callee),
                    interface: *interface,
                    args: args_slice,
                }
            }
            HirExpr::StructInit { name, args, span } => {
                let new_name = self.monomorphize_expr(name, subs);
                let new_args: Vec<HirExpr> = args.iter()
                    .map(|a| self.monomorphize_expr(a, subs))
                    .collect();
                let args_slice = self.bump.alloc_slice(&new_args);
                HirExpr::StructInit {
                    name: self.bump.alloc_value_immutable(new_name),
                    args: args_slice,
                    span: *span,
                }
            }
            HirExpr::FieldAccess { object, field, span } => {
                let new_object = self.monomorphize_expr(object, subs);
                HirExpr::FieldAccess {
                    object: self.bump.alloc_value_immutable(new_object),
                    field: *field,
                    span: *span,
                }
            }
            HirExpr::Get { object, field, span } => {
                let new_object = self.monomorphize_expr(object, subs);
                HirExpr::Get {
                    object: self.bump.alloc_value_immutable(new_object),
                    field: *field,
                    span: *span,
                }
            }
            HirExpr::Binary { left, op, right, span } => {
                let new_left = self.monomorphize_expr(left, subs);
                let new_right = self.monomorphize_expr(right, subs);
                HirExpr::Binary {
                    left: self.bump.alloc_value_immutable(new_left),
                    op: *op,
                    right: self.bump.alloc_value_immutable(new_right),
                    span: *span,
                }
            }
            HirExpr::Assignment { target, op, value, span } => {
                let new_target = self.monomorphize_expr(target, subs);
                let new_value = self.monomorphize_expr(value, subs);
                HirExpr::Assignment {
                    target: self.bump.alloc_value_immutable(new_target),
                    op: *op,
                    value: self.bump.alloc_value_immutable(new_value),
                    span: *span,
                }
            }
            HirExpr::ExprList { list, span } => {
                let new_list: Vec<HirExpr> = list.iter()
                    .map(|e| self.monomorphize_expr(e, subs))
                    .collect();
                let list_slice = self.bump.alloc_slice(&new_list);
                HirExpr::ExprList {
                    list: list_slice,
                    span: *span,
                }
            }
            HirExpr::Comparison { left, op, right, span } => {
                let new_left = self.monomorphize_expr(left, subs);
                let new_right = self.monomorphize_expr(right, subs);
                HirExpr::Comparison {
                    left: self.bump.alloc_value_immutable(new_left),
                    op: *op,
                    right: self.bump.alloc_value_immutable(new_right),
                    span: *span,
                }
            }
            _ => expr.clone(),
        }
    }
}
