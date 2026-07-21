use smallvec::SmallVec;
use std::cell::RefCell;
use std::marker::PhantomData;
use std::rc::Rc;
use std::sync::Arc;

use crate::hir_lowerer::LoweringCtx;
use crate::hir_lowerer::monomorphization::instantiate_struct_for_types;

use super::naming::suffix_for_subs;
use super::type_substitution::substitute_type;
use ir::hir::{Hir, HirExpr, HirFieldInit, HirFunc, HirModule, HirParam, HirStmt, HirType, StrId};
use ir::ir_hasher::{FxHashMap, HashMap};
use zetaruntime::arena::GrowableAtomicBump;
use zetaruntime::string_pool::StringPool;

pub struct Monomorphizer<'a, 'bump, 'ctx> {
    pub instantiated_functions: Rc<RefCell<FxHashMap<(StrId, StrId), StrId>>>,
    pub instantiated_structs: Rc<RefCell<FxHashMap<(StrId, StrId), StrId>>>,
    pub instantiated_struct_origins:
        Rc<RefCell<FxHashMap<StrId, (StrId, Vec<HirType<'a, 'bump>>)>>>,
    functions: Rc<RefCell<FxHashMap<StrId, HirFunc<'a, 'bump>>>>,
    bump: Arc<GrowableAtomicBump<'bump>>,
    context: Arc<StringPool>,
    ctx: &'ctx LoweringCtx<'a, 'bump>,
    /// Concrete type of `this` for whatever method body is currently being
    /// walked. Set/restored around each specialization in
    /// `resolve_method_for_type`. None outside a method body (free
    /// functions, or a method that hasn't been receiver-resolved yet).
    current_this: RefCell<Option<HirType<'a, 'bump>>>,
    _phantom: PhantomData<&'bump ()>,
}

impl<'a, 'bump, 'ctx> Monomorphizer<'a, 'bump, 'ctx> {
    pub fn new(
        context: Arc<StringPool>,
        bump: Arc<GrowableAtomicBump<'bump>>,
        functions: Rc<RefCell<FxHashMap<StrId, HirFunc<'a, 'bump>>>>,
        ctx: &'ctx LoweringCtx<'a, 'bump>,
        instantiated_functions: Rc<RefCell<FxHashMap<(StrId, StrId), StrId>>>,
        instantiated_structs: Rc<RefCell<FxHashMap<(StrId, StrId), StrId>>>,
        instantiated_struct_origins: Rc<
            RefCell<FxHashMap<StrId, (StrId, Vec<HirType<'a, 'bump>>)>>,
        >,
    ) -> Self {
        Self {
            instantiated_functions,
            instantiated_structs,
            instantiated_struct_origins,
            functions,
            bump,
            context,
            ctx,
            current_this: RefCell::new(None),
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
                Hir::Impl(i) => {
                    // Always walk method bodies with empty_subs at this stage, this
                    // catches non-generic impls outright (ArrayList itself has no
                    // concrete counterpart yet, but a `impl Point { .. }` block's
                    // methods need this pass regardless), and for generic impls it
                    // rewrites any nested generic calls inside the still-templated
                    // body.
                    // The template itself gets dropped by the retain filter
                    // below, only requested instantiations (harvested next) survive.
                    let mut new_impl = (*i).clone();
                    if let Some(methods) = new_impl.methods {
                        let new_methods: Vec<HirFunc> = methods
                            .iter()
                            .map(|m| {
                                let mut nm = m.clone();
                                if let Some(body) = nm.body {
                                    let new_body = self.monomorphize_stmt(&body, &empty_subs);
                                    nm.body = Some(*self.bump.alloc_value_immutable(new_body));
                                }
                                nm
                            })
                            .collect();
                        new_impl.methods = Some(self.bump.alloc_slice(&new_methods));
                    }
                    new_items.push(Hir::Impl(self.bump.alloc_value_immutable(new_impl)));
                }
                other => new_items.push(*other),
            }
        }

        // Force-instantiate `drop` for every generic struct instantiation that
        // implements Drop. `drop` is normally only monomorphized when there's an
        // explicit call site, but it's also invoked implicitly by generated drop
        // glue in MIR, without forcing it here, the glue
        // call site has nothing to point at and drop bodies silently vanish.
        {
            let drop_iface = StrId(self.context.intern("Drop"));
            let drop_method_name = StrId(self.context.intern("drop"));

            // Snapshot first: resolve_method_for_type -> instantiate_struct_for_types
            // borrows instantiated_struct_origins internally, so we can't hold our
            // own borrow across the call.
            let origins: Vec<(StrId, Vec<HirType<'a, 'bump>>)> = self
                .instantiated_struct_origins
                .borrow()
                .values()
                .cloned()
                .collect();

            for (origin_name, origin_targs) in origins {
                let implements_drop = self
                    .ctx
                    .struct_interfaces
                    .borrow()
                    .get(&origin_name)
                    .map(|ifaces| ifaces.contains(&drop_iface))
                    .unwrap_or(false);

                if !implements_drop {
                    continue;
                }

                let targs_slice = self.bump.alloc_slice_immutable(&origin_targs);
                let generic_ty = HirType::Struct {
                    name: origin_name,
                    field_types: &[],
                    type_args: targs_slice,
                };

                self.resolve_method_for_type(&generic_ty, drop_method_name);
            }
        }

        for new_name in self.instantiated_functions.borrow().values() {
            match self.functions.borrow().get(new_name) {
                Some(func) => {
                    new_items.push(Hir::Func(self.bump.alloc_value_immutable(func.clone())));
                }
                None => {
                    panic!("missing instantiated function {}", new_name);
                }
            }
        }

        for new_struct_name in self.instantiated_structs.borrow().values() {
            if let Some(new_struct) = self.ctx.structs.borrow().get(new_struct_name) {
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

        let new_module = HirModule {
            name: module.name,
            imports: module.imports,
            items: self.bump.alloc_slice(&new_items),
        };
        new_module
    }

    fn concrete_type_of(&self, expr: &HirExpr<'a, 'bump>) -> Option<HirType<'a, 'bump>> {
        match expr {
            HirExpr::This { .. } => *self.current_this.borrow(),
            HirExpr::Ident(name, _) => self.ctx.variable_types.borrow().get(name).copied(),
            _ => None, // anything else falls through unspecialized rather than risk misresolving. We should extend this as real cases surface.
        }
    }

    fn resolve_method_for_type(
        &self,
        struct_ty: &HirType<'a, 'bump>,
        method_name: StrId,
    ) -> Option<StrId> {
        let HirType::Struct {
            name, type_args, ..
        } = struct_ty
        else {
            return None;
        };

        if type_args.is_empty() {
            if let Some((origin_name, origin_targs)) =
                self.instantiated_struct_origins.borrow().get(name).cloned()
            {
                let targs_slice = self.bump.alloc_slice_immutable(&origin_targs);
                let generic_ty = HirType::Struct {
                    name: origin_name,
                    field_types: &[],
                    type_args: targs_slice,
                };
                return self.resolve_method_for_type(&generic_ty, method_name);
            }

            return self
                .ctx
                .struct_methods
                .borrow()
                .get(name)
                .and_then(|m| m.get(&method_name))
                .copied();
        }

        let instantiated = instantiate_struct_for_types(
            self.ctx,
            &self.instantiated_structs,
            &self.instantiated_struct_origins,
            *name,
            type_args,
            self.bump.clone(),
        )?;
        let concrete_name = instantiated.name;

        let base_method_name = *self
            .ctx
            .struct_methods
            .borrow()
            .get(name)?
            .get(&method_name)?;
        let base_func = self.functions.borrow().get(&base_method_name)?.clone();
        let type_params = base_func.generics?;
        if type_params.len() != type_args.len() {
            return None;
        }

        let mut inner_subs: FxHashMap<StrId, HirType> = FxHashMap::default();
        for (p, a) in type_params.iter().zip(type_args.iter()) {
            inner_subs.insert(p.name, a.clone());
        }

        let field_types: Vec<HirType> = instantiated.fields.iter().map(|f| f.field_type).collect();
        let concrete_recv_ty = HirType::Struct {
            name: concrete_name,
            field_types: self.bump.alloc_slice_immutable(&field_types),
            type_args: &[],
        };

        let prev_self = self.current_this.replace(Some(concrete_recv_ty));
        let result = self.monomorphize_function(&base_func, &inner_subs);
        self.current_this.replace(prev_self);
        result
    }

    pub fn monomorphize_function<'subs>(
        &self,
        func: &HirFunc<'a, 'bump>,
        substitutions: &'subs FxHashMap<StrId, HirType<'a, 'bump>>,
    ) -> Option<StrId> {
        let mut new_func = func.clone();
        self.apply_substitutions_to_func(&mut new_func, substitutions);

        if new_func.impl_target.is_some() {
            let cur_self = *self.current_this.borrow();
            if let Some(HirType::Struct { name, .. }) = cur_self {
                new_func.impl_target = Some(name);
            }
        }

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

    fn substitute_type_with_self(
        &self,
        ty: &HirType<'a, 'bump>,
        subs: &FxHashMap<StrId, HirType<'a, 'bump>>,
    ) -> HirType<'a, 'bump> {
        if let HirType::This = ty {
            if let Some(self_ty) = *self.current_this.borrow() {
                return self_ty;
            }
        }
        substitute_type(ty, subs, self.bump.clone())
    }

    fn apply_substitutions_to_func<'subs>(
        &self,
        func: &mut HirFunc<'a, 'bump>,
        substitutions: &'subs FxHashMap<StrId, HirType<'a, 'bump>>,
    ) {
        self.apply_substitutions_to_params(func, substitutions);

        if let Some(ret) = &mut func.return_type {
            *ret = self.substitute_type_with_self(ret, substitutions);
        }

        func.generics = None;
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
                        param_type: self.substitute_type_with_self(param_type, substitutions),
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
            HirStmt::Panic { message, span } => {
                let new_msg = self.monomorphize_expr(message, substitutions);
                HirStmt::Panic {
                    message: self.bump.alloc_value_immutable(new_msg),
                    span: *span,
                }
            }
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

                let new_value = self
                    .try_monomorphize_assoc_call(value, &new_ty, substitutions)
                    .unwrap_or_else(|| self.monomorphize_expr(value, substitutions));

                let new_ty = self.instantiate_struct_ty_if_needed(new_ty);

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

    fn try_monomorphize_assoc_call<'subs>(
        &self,
        value: &HirExpr<'a, 'bump>,
        expected_ty: &HirType<'a, 'bump>,
        outer_subs: &'subs HashMap<StrId, HirType<'a, 'bump>>,
    ) -> Option<HirExpr<'a, 'bump>> {
        let HirExpr::Call {
            callee,
            args,
            type_args: None,
            span,
        } = value
        else {
            return None;
        };
        let HirExpr::ModuleAccess(acc) = &**callee else {
            return None;
        };
        let HirType::Struct {
            type_args: expected_targs,
            ..
        } = expected_ty
        else {
            return None;
        };
        if expected_targs.is_empty() {
            return None;
        }

        let (&struct_name, module_path) = acc.path.split_last()?;
        let target_key = self.ctx.resolve_type_path_name(module_path, struct_name);

        let base_method_name = *self
            .ctx
            .struct_methods
            .borrow()
            .get(&target_key)?
            .get(&acc.member)?;
        let base_func = self.functions.borrow().get(&base_method_name)?.clone();
        let type_params = base_func.generics?;
        if type_params.len() != expected_targs.len() {
            return None;
        }

        let mut inner_subs: FxHashMap<StrId, HirType> = FxHashMap::default();
        for (p, a) in type_params.iter().zip(expected_targs.iter()) {
            inner_subs.insert(p.name, substitute_type(a, outer_subs, self.bump.clone()));
        }

        let new_args: Vec<HirExpr> = args
            .iter()
            .map(|a| self.monomorphize_expr(a, outer_subs))
            .collect();
        let args_slice = self.bump.alloc_slice(&new_args);

        // Resolve the concrete receiver struct so monomorphize_function can rewrite
        // impl_target (mirrors resolve_method_for_type's current_this handling).
        let concrete_recv_ty = instantiate_struct_for_types(
            self.ctx,
            &self.instantiated_structs,
            &self.instantiated_struct_origins,
            target_key,
            expected_targs,
            self.bump.clone(),
        )
        .map(|instantiated| {
            let field_types: Vec<HirType> =
                instantiated.fields.iter().map(|f| f.field_type).collect();
            HirType::Struct {
                name: instantiated.name,
                field_types: self.bump.alloc_slice_immutable(&field_types),
                type_args: &[],
            }
        });

        let prev_self = self.current_this.replace(concrete_recv_ty);
        let new_name = self.monomorphize_function(&base_func, &inner_subs);
        self.current_this.replace(prev_self);
        let new_name = new_name?;

        Some(HirExpr::Call {
            callee: self
                .bump
                .alloc_value_immutable(HirExpr::Ident(new_name, acc.span)),
            args: args_slice,
            type_args: None,
            span: *span,
        })
    }

    fn instantiate_struct_ty_if_needed(&self, ty: HirType<'a, 'bump>) -> HirType<'a, 'bump> {
        let HirType::Struct {
            name, type_args, ..
        } = &ty
        else {
            return ty;
        };
        if type_args.is_empty() {
            return ty;
        }
        let Some(new_struct) = instantiate_struct_for_types(
            self.ctx,
            &self.instantiated_structs,
            &self.instantiated_struct_origins,
            *name,
            type_args,
            self.bump.clone(),
        ) else {
            return ty;
        };

        let field_types: Vec<HirType> = new_struct.fields.iter().map(|f| f.field_type).collect();
        HirType::Struct {
            name: new_struct.name,
            field_types: self.bump.alloc_slice_immutable(&field_types),
            type_args: &[],
        }
    }

    pub fn monomorphize_expr<'subs>(
        &self,
        expr: &HirExpr<'a, 'bump>,
        subs: &'subs HashMap<StrId, HirType<'a, 'bump>>,
    ) -> HirExpr<'a, 'bump> {
        match expr {
            HirExpr::Cast {
                expr,
                target_type,
                span,
            } => HirExpr::Cast {
                expr: self
                    .bump
                    .alloc_value_immutable(self.monomorphize_expr(expr, subs)),
                target_type: self.substitute_type_with_self(target_type, subs),
                span: *span,
            },
            HirExpr::Intrinsic {
                kind,
                type_args,
                args,
                span,
            } => {
                let new_type_args: Vec<HirType> = type_args
                    .iter()
                    .map(|t| self.substitute_type_with_self(t, subs))
                    .collect();

                let new_args: Vec<HirExpr> = args
                    .iter()
                    .map(|a| self.monomorphize_expr(a, subs))
                    .collect();
                HirExpr::Intrinsic {
                    kind: *kind,
                    type_args: self.bump.alloc_slice_immutable(&new_type_args),
                    args: self.bump.alloc_slice(&new_args),
                    span: *span,
                }
            }
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

                if let HirExpr::FieldAccess {
                    object,
                    field,
                    span: fa_span,
                } = &**callee
                {
                    let new_object = self.monomorphize_expr(object, subs);

                    if let Some(concrete_recv_ty) = self.concrete_type_of(&new_object) {
                        if matches!(concrete_recv_ty, HirType::Struct { .. }) {
                            if let Some(concrete_method_name) =
                                self.resolve_method_for_type(&concrete_recv_ty, *field)
                            {
                                let mut new_args: Vec<HirExpr> = Vec::with_capacity(args.len() + 1);
                                new_args.push(new_object.clone());
                                new_args
                                    .extend(args.iter().map(|a| self.monomorphize_expr(a, subs)));
                                let args_slice = self.bump.alloc_slice(&new_args);
                                let new_callee = HirExpr::Ident(concrete_method_name, *fa_span);
                                return HirExpr::Call {
                                    callee: self.bump.alloc_value_immutable(new_callee),
                                    args: args_slice,
                                    type_args: None,
                                    span: *span,
                                };
                            }
                        }
                    }

                    let new_args: Vec<HirExpr> = args
                        .iter()
                        .map(|a| self.monomorphize_expr(a, subs))
                        .collect();
                    let args_slice = self.bump.alloc_slice(&new_args);
                    let new_callee = HirExpr::FieldAccess {
                        object: self.bump.alloc_value_immutable(new_object),
                        field: *field,
                        span: *fa_span,
                    };
                    return HirExpr::Call {
                        callee: self.bump.alloc_value_immutable(new_callee),
                        args: args_slice,
                        type_args: *type_args,
                        span: *span,
                    };
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

                    if let Some(new_struct) = instantiate_struct_for_types(
                        self.ctx,
                        &self.instantiated_structs.clone(),
                        &&self.instantiated_struct_origins.clone(),
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
