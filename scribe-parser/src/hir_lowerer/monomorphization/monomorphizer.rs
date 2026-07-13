use smallvec::SmallVec;
use std::cell::RefCell;
use std::marker::PhantomData;
use std::rc::Rc;
use std::sync::Arc;

use crate::hir_lowerer::LoweringCtx;
use crate::hir_lowerer::monomorphization::instantiate_class_for_types;

use super::naming::suffix_for_subs;
use super::type_substitution::substitute_type;
use ir::hir::{Hir, HirExpr, HirFieldInit, HirFunc, HirModule, HirParam, HirStmt, HirType, StrId};
use ir::ir_hasher::{FxHashMap, HashMap};
use zetaruntime::arena::GrowableAtomicBump;
use zetaruntime::string_pool::StringPool;

pub struct Monomorphizer<'a, 'bump, 'ctx> {
    /// Maps (original_name, type_suffix) to instantiated function name
    pub instantiated_functions: RefCell<FxHashMap<(StrId, StrId), StrId>>,
    /// Maps (original_name, type_suffix) to instantiated class name
    pub instantiated_classes: RefCell<FxHashMap<(StrId, StrId), StrId>>,
    functions: Rc<RefCell<FxHashMap<StrId, HirFunc<'a, 'bump>>>>,
    bump: Arc<GrowableAtomicBump<'bump>>,
    context: Arc<StringPool>,
    ctx: &'ctx LoweringCtx<'a, 'bump>,
    _phantom: PhantomData<&'bump ()>,
}

impl<'a, 'bump, 'ctx> Monomorphizer<'a, 'bump, 'ctx> {
    pub fn new(
        context: Arc<StringPool>,
        bump: Arc<GrowableAtomicBump<'bump>>,
        functions: Rc<RefCell<FxHashMap<StrId, HirFunc<'a, 'bump>>>>,
        ctx: &'ctx LoweringCtx<'a, 'bump>,
    ) -> Self {
        Self {
            instantiated_functions: RefCell::new(FxHashMap::default()),
            instantiated_classes: RefCell::new(FxHashMap::default()),
            functions,
            bump,
            context,
            ctx,
            _phantom: PhantomData,
        }
    }

    /// Walks the whole module looking for generic call sites, monomorphizes
    /// each one it hits, rewrites the call to point at the mangled name, and
    /// appends the newly-instantiated functions as module items. Must run
    /// after type checking (which validates arity/type-arg counts) and after
    /// closure hoisting.
    pub fn run(&self, module: HirModule<'a, 'bump>) -> HirModule<'a, 'bump> {
        let empty_subs: FxHashMap<StrId, HirType<'a, 'bump>> = FxHashMap::default();
        let mut new_items: Vec<Hir<'a, 'bump>> = Vec::with_capacity(module.items.len());

        for item in module.items {
            match item {
                Hir::Func(f) => {
                    let mut new_func = (*f).clone();
                    if let Some(body) = new_func.body {
                        let new_body = self.monomorphize_stmt(&body, &empty_subs);
                        new_func.body = Some(*self.bump.alloc_value_immutable(new_body));
                    }
                    new_items.push(Hir::Func(self.bump.alloc_value_immutable(new_func)));
                }
                other => new_items.push(*other),
            }
        }

        for new_name in self.instantiated_functions.borrow().values() {
            if let Some(func) = self.functions.borrow().get(new_name) {
                new_items.push(Hir::Func(self.bump.alloc_value_immutable(func.clone())));
            }
        }

        for new_class_name in self.instantiated_classes.borrow().values() {
            if let Some(new_struct) = self.ctx.classes.borrow().get(new_class_name) {
                new_items.push(Hir::Struct(
                    self.bump.alloc_value_immutable(new_struct.clone()),
                ));
            }
        }

        new_items.retain(|hir| match hir {
            Hir::Func(hir_func) => hir_func.generics.is_none(),
            Hir::Struct(hir_struct) => hir_struct.generics.is_none(),
            Hir::Interface(hir_interface) => hir_interface.generics.is_none(),
            Hir::Impl(hir_impl) => hir_impl.generics.is_none(),
            Hir::Enum(hir_enum) => hir_enum.generics.is_none(),
            _ => true,
        });

        HirModule {
            name: module.name,
            imports: module.imports,
            items: self.bump.alloc_slice(&new_items),
        }
    }

    pub fn monomorphize_function<'subs>(
        &self,
        func: &HirFunc<'a, 'bump>,
        substitutions: &'subs FxHashMap<StrId, HirType<'a, 'bump>>,
    ) -> Option<StrId> {
        let mut new_func = func.clone();
        self.apply_substitutions_to_func(&mut new_func, substitutions);

        let suffix = suffix_for_subs(self.context.clone(), substitutions);
        let orig_name = new_func.name.clone();

        // Check if we've already monomorphized this function with these types
        let key = (orig_name.clone(), suffix.clone());
        {
            let instantiated_functions = self.instantiated_functions.borrow();
            if let Some(existing) = instantiated_functions.get(&key) {
                return Some(*existing);
            }
        }

        const UNDERSCORE_LEN: usize = 1;
        let mut small_vec: SmallVec<u8, 32> =
            SmallVec::with_capacity(orig_name.len() + UNDERSCORE_LEN + suffix.len());

        small_vec.extend_from_slice(self.context.resolve_bytes(&*orig_name));
        small_vec.push(b'_');
        small_vec.extend_from_slice(self.context.resolve_bytes(&*suffix));

        let new_name = StrId(self.context.intern_bytes(small_vec.as_slice()));

        new_func.name = new_name;

        if let Some(body) = new_func.body {
            let new_body = self.monomorphize_stmt(&body, substitutions);
            new_func.body = Some(*self.bump.alloc_value_immutable(new_body));
        }

        self.functions
            .borrow_mut()
            .insert(new_func.name.clone(), new_func);

        self.instantiated_functions
            .borrow_mut()
            .insert(key, new_name);

        Some(new_name)
    }

    fn apply_substitutions_to_func<'subs>(
        &self,
        func: &mut HirFunc<'a, 'bump>,
        substitutions: &'subs FxHashMap<StrId, HirType<'a, 'bump>>,
    ) {
        self.apply_substitutions_to_params(func, substitutions);

        if let Some(ret) = &mut func.return_type {
            *ret = substitute_type(ret, substitutions, self.bump.clone());
        }
    }

    fn apply_substitutions_to_params<'subs>(
        &self,
        func: &mut HirFunc<'a, 'bump>,
        substitutions: &'subs FxHashMap<StrId, HirType<'a, 'bump>>,
    ) {
        if let Some(params) = func.params {
            if params.is_empty() {
                func.params = None;
                return;
            }

            let mut new_params: Vec<HirParam> = Vec::new();

            for param in params.iter() {
                let new_param = match param {
                    HirParam::Normal {
                        name,
                        param_type,
                        span,
                    } => HirParam::Normal {
                        name: *name,
                        param_type: substitute_type(param_type, substitutions, self.bump.clone()),
                        span: *span,
                    },
                    HirParam::This { kind, span } => HirParam::This {
                        kind: *kind,
                        span: *span,
                    },
                };
                new_params.push(new_param);
            }

            func.params = Some(self.bump.alloc_slice_immutable(&new_params));
        }
    }

    pub fn monomorphize_stmt<'subs>(
        &self,
        stmt: &HirStmt<'a, 'bump>,
        substitutions: &'subs HashMap<StrId, HirType<'a, 'bump>>,
    ) -> HirStmt<'a, 'bump> {
        match stmt {
            HirStmt::Block { body } => {
                let new_body: Vec<HirStmt> = body
                    .iter()
                    .map(|s| self.monomorphize_stmt(s, substitutions))
                    .collect();
                let body_slice = self.bump.alloc_slice(&new_body);
                HirStmt::Block { body: body_slice }
            }
            HirStmt::If {
                cond,
                then_block,
                else_block,
            } => {
                let new_cond = self.monomorphize_expr(cond, substitutions);
                let new_then: Vec<HirStmt> = then_block
                    .iter()
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
            HirStmt::Let {
                name,
                ty,
                value,
                mutable,
                is_static,
                catch_pattern,
                else_block,
                span,
            } => {
                let new_ty = substitute_type(ty, substitutions, self.bump.clone());
                let new_value = self.monomorphize_expr(value, substitutions);
                HirStmt::Let {
                    name: *name,
                    ty: new_ty,
                    value: *self.bump.alloc_value_immutable(new_value),
                    mutable: *mutable,
                    is_static: *is_static,
                    catch_pattern: *catch_pattern,
                    else_block: *else_block,
                    span: *span,
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

    pub fn monomorphize_expr<'subs>(
        &self,
        expr: &HirExpr<'a, 'bump>,
        subs: &'subs HashMap<StrId, HirType<'a, 'bump>>,
    ) -> HirExpr<'a, 'bump> {
        match expr {
            HirExpr::Call {
                callee,
                args,
                type_args,
                span,
            } => {
                if let (HirExpr::Ident(func_name, ident_span), Some(targs)) = (&**callee, type_args)
                {
                    let maybe_func = self.functions.borrow().get(func_name).cloned();
                    if let Some(func) = maybe_func {
                        if let Some(type_params) = func.generics {
                            if !type_params.is_empty() {
                                // Resolve type args against the substitutions already in
                                // scope, in case this call sits inside another generic
                                // function's body (nested generics).
                                let resolved_targs: Vec<HirType> = targs
                                    .iter()
                                    .map(|t| substitute_type(t, subs, self.bump.clone()))
                                    .collect();
                                let mut inner_subs: FxHashMap<StrId, HirType> =
                                    FxHashMap::default();
                                type_params
                                    .iter()
                                    .zip(resolved_targs.iter())
                                    .for_each(|(p, a)| {
                                        inner_subs.insert(p.name, a.clone());
                                    });

                                let new_args: Vec<HirExpr> = args
                                    .iter()
                                    .map(|a| self.monomorphize_expr(a, subs))
                                    .collect();
                                let args_slice = self.bump.alloc_slice(&new_args);

                                if let Some(new_name) =
                                    self.monomorphize_function(&func, &inner_subs)
                                {
                                    let new_callee = HirExpr::Ident(new_name, *ident_span);
                                    return HirExpr::Call {
                                        callee: self.bump.alloc_value_immutable(new_callee),
                                        args: args_slice,
                                        type_args: None,
                                        span: *span,
                                    };
                                }
                            }
                        }
                    }
                }

                let new_callee = self.monomorphize_expr(callee, subs);
                let new_args: Vec<HirExpr> = args
                    .iter()
                    .map(|a| self.monomorphize_expr(a, subs))
                    .collect();
                let args_slice = self.bump.alloc_slice(&new_args);
                HirExpr::Call {
                    callee: self.bump.alloc_value_immutable(new_callee),
                    args: args_slice,
                    type_args: *type_args,
                    span: *span,
                }
            }
            HirExpr::InterfaceCall {
                callee,
                interface,
                args,
                span,
            } => {
                let new_callee = self.monomorphize_expr(callee, subs);
                let new_args: Vec<HirExpr> = args
                    .iter()
                    .map(|a| self.monomorphize_expr(a, subs))
                    .collect();
                let args_slice = self.bump.alloc_slice(&new_args);
                HirExpr::InterfaceCall {
                    callee: self.bump.alloc_value_immutable(new_callee),
                    interface: *interface,
                    args: args_slice,
                    span: *span,
                }
            }
            HirExpr::StructInit {
                name,
                args,
                type_args,
                span,
            } => {
                let new_args: Vec<HirFieldInit<'a, 'bump>> = args
                    .iter()
                    .map(|a| HirFieldInit {
                        name: a.name,
                        name_span: a.name_span,
                        value: self.monomorphize_expr(&a.value, subs),
                    })
                    .collect();
                let args_slice = self.bump.alloc_slice(&new_args);

                if let (HirExpr::Ident(struct_name, ident_span), Some(targs)) = (&**name, type_args)
                {
                    // resolve against outer subs first, same nested-generics reasoning
                    // as the Call arm (this StructInit might sit inside another generic
                    // function's body, e.g. `Container<T>{...}` inside `fn identity<T>`)
                    let resolved_targs: Vec<HirType> = targs
                        .iter()
                        .map(|t| substitute_type(t, subs, self.bump.clone()))
                        .collect();

                    if let Some(new_struct) = instantiate_class_for_types(
                        self.ctx,
                        self,
                        *struct_name,
                        &resolved_targs,
                        self.bump.clone(),
                    ) {
                        let new_name_expr = HirExpr::Ident(new_struct.name, *ident_span);
                        return HirExpr::StructInit {
                            name: self.bump.alloc_value_immutable(new_name_expr),
                            args: args_slice,
                            type_args: None,
                            span: *span,
                        };
                    }
                }

                let new_name = self.monomorphize_expr(name, subs);
                HirExpr::StructInit {
                    name: self.bump.alloc_value_immutable(new_name),
                    args: args_slice,
                    type_args: *type_args,
                    span: *span,
                }
            }
            HirExpr::FieldAccess {
                object,
                field,
                span,
            } => {
                let new_object = self.monomorphize_expr(object, subs);
                HirExpr::FieldAccess {
                    object: self.bump.alloc_value_immutable(new_object),
                    field: *field,
                    span: *span,
                }
            }
            HirExpr::Get {
                object,
                field,
                span,
            } => {
                let new_object = self.monomorphize_expr(object, subs);
                HirExpr::Get {
                    object: self.bump.alloc_value_immutable(new_object),
                    field: *field,
                    span: *span,
                }
            }
            HirExpr::Binary {
                left,
                op,
                right,
                span,
            } => {
                let new_left = self.monomorphize_expr(left, subs);
                let new_right = self.monomorphize_expr(right, subs);
                HirExpr::Binary {
                    left: self.bump.alloc_value_immutable(new_left),
                    op: *op,
                    right: self.bump.alloc_value_immutable(new_right),
                    span: *span,
                }
            }
            HirExpr::Assignment {
                target,
                op,
                value,
                span,
            } => {
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
                let new_list: Vec<HirExpr> = list
                    .iter()
                    .map(|e| self.monomorphize_expr(e, subs))
                    .collect();
                let list_slice = self.bump.alloc_slice(&new_list);
                HirExpr::ExprList {
                    list: list_slice,
                    span: *span,
                }
            }
            HirExpr::Comparison {
                left,
                op,
                right,
                span,
            } => {
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
