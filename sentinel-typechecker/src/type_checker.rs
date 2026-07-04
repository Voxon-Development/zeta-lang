use crate::type_context::TypeContext;
use codex_dependency_graph::DepGraph;
use ir::ast::MutabilityState;
use ir::errors::type_error::{TypeCheckResult, TypeError, TypeErrorKind};
use ir::hir::{
    Hir, HirExpr, HirFunc, HirModule, HirParam, HirStmt, HirType, Operator, StrId, ThisPassingKind,
};
use ir::span::SourceSpan;
use zetaruntime::bump::GrowableBump;

pub struct TypeChecker<'a, 'bump> {
    context: TypeContext<'a, 'bump>,
    /// All type errors found so far. Checking never aborts on the first
    /// error: `check_expr`/`check_stmt` record errors here and keep going
    /// with a best-effort placeholder type, so a single pass can surface as
    /// many real problems as possible instead of just the first one.
    errors: Vec<TypeError<'a>>,
    /// The most specific span seen so far while walking down into the
    /// current statement/expression. Not every HIR node carries a span yet
    /// (see `ir::hir`), so when we need to report an error at a spanless
    /// node we fall back to whatever span was last set by an ancestor or
    /// sibling, better an approximate location than none at all.
    current_span: SourceSpan<'a>,
}

impl<'a, 'bump> TypeChecker<'a, 'bump> {
    pub fn new(dep_graph: &'a DepGraph, bump: &'bump GrowableBump<'bump>) -> Self {
        Self {
            context: TypeContext::new(dep_graph, bump),
            errors: Vec::new(),
            current_span: SourceSpan::default(),
        }
    }

    /// All type errors accumulated across every `check_module_body` call so
    /// far.
    pub fn errors(&self) -> &[TypeError<'a>] {
        &self.errors
    }

    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }

    /// Drains and returns all accumulated errors, resetting the internal
    /// list to empty.
    pub fn take_errors(&mut self) -> Vec<TypeError<'a>> {
        std::mem::take(&mut self.errors)
    }

    /// Records a type error at the current best-known location.
    fn record(&mut self, kind: TypeErrorKind) {
        self.errors.push(kind.at(self.current_span));
    }

    /// Updates the "current best-known span" used for errors detected at
    /// nodes that don't carry their own span.
    fn set_span(&mut self, span: SourceSpan<'a>) {
        self.current_span = span;
    }

    /// Runs a fallible sub-check; on failure, records the error at the
    /// current span and substitutes `fallback` so the caller can keep
    /// checking instead of aborting.
    fn recover<T>(&mut self, result: TypeCheckResult<'a, T>, fallback: T) -> T {
        match result {
            Ok(v) => v,
            Err(e) => {
                self.errors.push(e);
                fallback
            }
        }
    }

    pub fn register_module(&mut self, module: &HirModule<'a, 'bump>, module_idx: usize) {
        let prev_module_idx = self.context.current_module_idx;
        self.context.current_module_idx = module_idx;

        for item in module.items {
            match item {
                Hir::Struct(s) => {
                    let name = s.name.to_string();
                    self.context.add_struct(name.clone(), **s);
                    if let Some(interfaces) = s.interfaces {
                        for iface in interfaces {
                            self.context.add_struct_interface(&name, iface.to_string());
                        }
                    }
                }
                Hir::Impl(i) => {
                    let target = i.target.to_string();
                    if let Some(methods) = i.methods {
                        self.context.add_impl_methods(&target, methods);
                    }
                    if let Some(interface) = i.interface {
                        self.context
                            .add_struct_interface(&target, interface.to_string());
                    }
                }
                Hir::Interface(i) => {
                    let name = i.name.to_string();
                    self.context.add_interface(name, **i);
                }
                Hir::Enum(e) => {
                    let name = e.name.to_string();
                    self.context.add_enum(name, **e);
                }
                Hir::Func(f) => {
                    let name = f.unmangled_name.to_string();
                    self.context.add_function(module_idx, name, **f);
                }
                _ => {}
            }
        }

        self.context.current_module_idx = prev_module_idx;
    }

    /// Type-checks every function body in `module`. Never stops early: all
    /// functions are checked even if earlier ones had errors. Check
    /// `self.errors()` afterwards (or after checking every module) to see
    /// everything that was found.
    pub fn check_module_body(&mut self, module: &HirModule<'a, 'bump>, module_idx: usize) {
        self.context.current_module_idx = module_idx;
        for item in module.items {
            if let Hir::Func(func) = item {
                self.check_function(func);
            }
        }
    }

    fn check_function(&mut self, func: &HirFunc<'a, 'bump>) {
        let mut func_context = self.context.create_child_scope();

        if let Some(params) = func.params {
            for param in params {
                if let HirParam::Normal { name, param_type } = param {
                    let param_name = self.str_id_to_string(*name);
                    func_context.add_variable(param_name, *param_type);
                }
            }
        }

        func_context.current_return_type = func.return_type;

        if let Some(body) = func.body {
            let old_context = std::mem::replace(&mut self.context, func_context);
            let HirStmt::Block { body } = body else {
                unreachable!()
            };
            for stmt in body {
                self.check_stmt(&stmt);
            }
            self.context = old_context;
        }
    }

    /// Type-checks a statement. Always continues (never aborts a whole
    /// function on one bad statement); on error, records it and does its
    /// best to keep the surrounding checks meaningful.
    fn check_stmt(&mut self, stmt: &HirStmt<'a, 'bump>) -> Option<HirType<'a, 'bump>> {
        match stmt {
            HirStmt::Let {
                name,
                ty,
                value,
                mutable,
                else_block,
                ..
            } => {
                let value_type = self.check_expr(value);

                if let Some(else_block) = else_block {
                    match &value_type {
                        HirType::Nullable(inner) => {
                            let inner = **inner;
                            let result = self.types_compatible(ty, &inner);
                            self.recover(result, ());

                            let else_context = self.context.create_child_scope();
                            let old_context = std::mem::replace(&mut self.context, else_context);
                            self.check_stmt(else_block);
                            self.context = old_context;
                        }
                        _ => {
                            self.record(TypeErrorKind::Generic(format!(
                                "`? else` used on non-nullable type `{}`",
                                self.type_to_string(&value_type)
                            )));
                        }
                    }
                } else {
                    let result = self.types_compatible(ty, &value_type);
                    self.recover(result, ());
                }
                let var_name = self.str_id_to_string(*name);

                if self.expr_is_dangling(value) {
                    self.context.mark_dangling(var_name.clone());
                }

                self.context
                    .add_variable_with_mutability(var_name, *ty, *mutable);
                None
            }
            HirStmt::Return(expr) => {
                if let Some(e) = expr {
                    let expr_type = self.check_expr(e);
                    let dangling = self.check_no_dangling_pointer(e);
                    self.recover(dangling, ());
                    if let Some(expected_return) = self.context.current_return_type {
                        let result = self.types_compatible(&expected_return, &expr_type);
                        self.recover(result, ());
                    }
                } else if let Some(expected_return) = self.context.current_return_type {
                    if expected_return != HirType::Void {
                        self.record(TypeErrorKind::InvalidReturnType {
                            expected: self.type_to_string(&expected_return),
                            found: "void".to_string(),
                        });
                    }
                }
                None
            }
            HirStmt::Expr(e) => {
                self.check_expr(e);
                None
            }
            HirStmt::If {
                cond,
                then_block,
                else_block,
            } => {
                let cond_type = self.check_expr(cond);
                if cond_type != HirType::Boolean {
                    self.record(TypeErrorKind::TypeMismatch {
                        expected: "bool".to_string(),
                        found: self.type_to_string(&cond_type),
                    });
                }

                let mut then_context = self.context.create_child_scope();
                for stmt in *then_block {
                    let old_context = std::mem::replace(&mut self.context, then_context);
                    self.check_stmt(stmt);
                    then_context = self.context.clone();
                    self.context = old_context;
                }

                if let Some(else_stmt) = else_block {
                    let else_context = self.context.create_child_scope();
                    let old_context = std::mem::replace(&mut self.context, else_context);
                    self.check_stmt(else_stmt);
                    self.context = old_context;
                }

                None
            }
            HirStmt::While { cond, body } => {
                let cond_type = self.check_expr(cond);
                if cond_type != HirType::Boolean {
                    self.record(TypeErrorKind::TypeMismatch {
                        expected: "bool".to_string(),
                        found: self.type_to_string(&cond_type),
                    });
                }

                self.context.enter_loop();
                self.check_stmt(body);
                self.context.exit_loop();

                None
            }
            HirStmt::For {
                init,
                condition,
                increment,
                body,
            } => {
                if let Some(init_stmt) = init {
                    self.check_stmt(init_stmt);
                }

                if let Some(cond) = condition {
                    let cond_type = self.check_expr(cond);
                    if cond_type != HirType::Boolean {
                        self.record(TypeErrorKind::TypeMismatch {
                            expected: "bool".to_string(),
                            found: self.type_to_string(&cond_type),
                        });
                    }
                }

                self.context.enter_loop();
                self.check_stmt(body);
                self.context.exit_loop();

                if let Some(inc) = increment {
                    self.check_expr(inc);
                }

                None
            }
            HirStmt::Block { body } => {
                let mut block_context = self.context.create_child_scope();
                for stmt in *body {
                    let old_context = std::mem::replace(&mut self.context, block_context);
                    self.check_stmt(stmt);
                    block_context = self.context.clone();
                    self.context = old_context;
                }
                None
            }
            HirStmt::Break(expr, span) => {
                self.set_span(*span);
                if !self.context.in_loop {
                    self.record(TypeErrorKind::BreakOutsideLoop);
                }
                if let Some(e) = expr {
                    let expr_type = self.check_expr(e);
                    if let Some(expected_return) = self.context.current_return_type {
                        let result = self.types_compatible(&expected_return, &expr_type);
                        self.recover(result, ());
                    }
                }
                None
            }
            HirStmt::Continue(span) => {
                self.set_span(*span);
                if !self.context.in_loop {
                    self.record(TypeErrorKind::ContinueOutsideLoop);
                }
                None
            }
            HirStmt::Const(const_stmt) => {
                let value_type = self.check_expr(&const_stmt.value);
                let result = self.types_compatible(&const_stmt.ty, &value_type);
                self.recover(result, ());
                let var_name = self.str_id_to_string(const_stmt.name);
                self.context.add_variable(var_name, const_stmt.ty);
                None
            }
            HirStmt::Match { expr, arms } => {
                self.check_expr(expr);
                for arm in *arms {
                    let arm_context = self.context.create_child_scope();
                    let old_context = std::mem::replace(&mut self.context, arm_context);

                    if let Some(guard) = arm.guard {
                        let guard_type = self.check_expr(guard);
                        if guard_type != HirType::Boolean {
                            self.record(TypeErrorKind::TypeMismatch {
                                expected: "bool".to_string(),
                                found: self.type_to_string(&guard_type),
                            });
                        }
                    }
                    self.check_stmt(arm.body);

                    self.context = old_context;
                }
                None
            }
            HirStmt::UnsafeBlock { body } => {
                self.check_stmt(body);
                None
            }
            HirStmt::Defer(hir_stmt) => {
                self.check_stmt(hir_stmt);
                None
            }
            HirStmt::Import(_path, span) => {
                self.set_span(*span);
                None
            }
            HirStmt::Package(_path, span) => {
                self.set_span(*span);
                None
            }
        }
    }

    fn check_expr(&mut self, expr: &HirExpr<'a, 'bump>) -> HirType<'a, 'bump> {
        match expr {
            HirExpr::Number(_, _) => HirType::I64,
            HirExpr::Null(_) => HirType::Null,
            HirExpr::Decimal(_, _) => HirType::F64,
            HirExpr::Boolean(_, _) => HirType::Boolean,
            HirExpr::String(_, _) => HirType::String,
            HirExpr::Ident(name, _) => {
                let var_name = self.str_id_to_string(*name);
                match self.context.get_variable(&var_name) {
                    Some(ty) => ty,
                    None => {
                        self.record(TypeErrorKind::UndefinedVariable(var_name));
                        HirType::Infer
                    }
                }
            }
            HirExpr::Tuple(exprs, _) => {
                let mut types = Vec::new();
                for e in *exprs {
                    types.push(self.check_expr(e));
                }
                types.first().copied().unwrap_or(HirType::Void)
            }
            HirExpr::Binary {
                left,
                op,
                right,
                span,
            } => {
                self.set_span(*span);
                let left_type = self.check_expr(left);
                let right_type = self.check_expr(right);
                let result = self.check_binary_op(&left_type, op, &right_type);
                self.recover(result, HirType::Infer)
            }
            HirExpr::Call { callee, args, span } => match &callee {
                HirExpr::Ident(func_name, _) => {
                    self.set_span(*span);
                    let name = self.str_id_to_string(*func_name);
                    let func = match self.context.get_function(&name) {
                        Some(f) => f,
                        None => {
                            self.record(TypeErrorKind::UndefinedFunction(name));
                            return HirType::Infer;
                        }
                    };

                    let expected_args = func.params.map(|p| p.len()).unwrap_or(0);
                    if args.len() != expected_args {
                        self.record(TypeErrorKind::InvalidFunctionCall {
                            expected_args,
                            found_args: args.len(),
                        });
                    }

                    if let Some(params) = func.params {
                        for (arg, param) in args.iter().zip(params.iter()) {
                            let arg_type = self.check_expr(arg);
                            if let Some(param_type) = param.get_type() {
                                let result = self.types_compatible(param_type, &arg_type);
                                self.recover(result, ());
                            }
                        }
                    }

                    func.return_type.unwrap_or(HirType::Void)
                }

                HirExpr::FieldAccess { object, field, .. } => {
                    let obj_type = self.check_expr(object);
                    let stripped = Self::strip_ref(&obj_type);

                    let interface_name = match stripped {
                        HirType::DynInterface(name, _) => Some(name.to_string()),
                        HirType::Dyn { bounds } => bounds.iter().find_map(|b| match b {
                            HirType::DynInterface(name, _) => Some(name.to_string()),
                            HirType::Struct(name, _) => {
                                let name_str = name.to_string();
                                self.context.get_interface(&name_str).map(|_| name_str)
                            }
                            _ => None,
                        }),
                        _ => None,
                    };

                    if let Some(iface_name) = interface_name {
                        let method_name = field.to_string();
                        let iface = match self.context.get_interface(&iface_name) {
                            Some(i) => i,
                            None => {
                                self.record(TypeErrorKind::UndefinedType(iface_name));
                                return HirType::Infer;
                            }
                        };

                        let method = iface.methods.and_then(|methods| {
                            methods
                                .iter()
                                .find(|m| m.unmangled_name.to_string() == method_name)
                        });
                        let Some(method) = method else {
                            self.record(TypeErrorKind::Generic(format!(
                                "no method `{}` on interface `{}`",
                                method_name, iface_name
                            )));
                            return HirType::Infer;
                        };

                        let total_params = method.params.map(|p| p.len()).unwrap_or(0);
                        let expected_args = total_params.saturating_sub(1); // skip `this`
                        if args.len() != expected_args {
                            self.record(TypeErrorKind::InvalidFunctionCall {
                                expected_args,
                                found_args: args.len(),
                            });
                        }

                        if let Some(params) = method.params {
                            for (arg, param) in args.iter().zip(params.iter().skip(1)) {
                                let arg_type = self.check_expr(arg);
                                if let Some(param_type) = param.get_type() {
                                    let result = self.types_compatible(param_type, &arg_type);
                                    self.recover(result, ());
                                }
                            }
                        }

                        return method.return_type.unwrap_or(HirType::Void);
                    }

                    let type_name = match stripped {
                        HirType::Struct(name, _) => name.to_string(),
                        _ => {
                            self.record(TypeErrorKind::Generic(format!(
                                "cannot call method on non-struct type: {}",
                                self.type_to_string(&obj_type)
                            )));
                            return HirType::Infer;
                        }
                    };

                    let method_name = field.to_string();
                    let func = match self.context.get_method(&type_name, &method_name).copied() {
                        Some(f) => f,
                        None => {
                            self.record(TypeErrorKind::Generic(format!(
                                "no method `{}` on `{}`",
                                method_name, type_name
                            )));
                            return HirType::Infer;
                        }
                    };

                    let total_params = func.params.map(|p| p.len()).unwrap_or(0);
                    let expected_args = total_params.saturating_sub(1);
                    if args.len() != expected_args {
                        self.record(TypeErrorKind::InvalidFunctionCall {
                            expected_args,
                            found_args: args.len(),
                        });
                    }

                    if let Some(params) = func.params {
                        if let Some(HirParam::This { kind }) = params.first() {
                            let requires_mut = matches!(
                                kind,
                                ThisPassingKind::RefMut
                                    | ThisPassingKind::MutSafePtr
                                    | ThisPassingKind::MoveMut
                            );
                            if requires_mut {
                                let result = self.check_receiver_is_mutable(object, &method_name);
                                self.recover(result, ());
                            }
                        }

                        for (arg, param) in args.iter().zip(params.iter().skip(1)) {
                            let arg_type = self.check_expr(arg);
                            if let Some(param_type) = param.get_type() {
                                let result = self.types_compatible(param_type, &arg_type);
                                self.recover(result, ());
                            }
                        }
                    }

                    func.return_type.unwrap_or(HirType::Void)
                }
                HirExpr::ModuleAccess(access) => {
                    let member_name = access.member.to_string();
                    let resolved_module_idx =
                        self.context.dep_graph.resolve_module_path(access.path);

                    let free_func = resolved_module_idx
                        .and_then(|midx| self.context.get_module_function(midx, &member_name));

                    let method_func = if free_func.is_none() {
                        access.path.last().and_then(|last_segment| {
                            let type_name = last_segment.to_string();
                            self.context.get_method(&type_name, &member_name).copied()
                        })
                    } else {
                        None
                    };

                    let func = match free_func.or(method_func) {
                        Some(f) => f,
                        None => {
                            let path_str = access
                                .path
                                .iter()
                                .map(|s| s.to_string())
                                .collect::<Vec<_>>()
                                .join("::");
                            let qualified_name = format!("{}::{}", path_str, member_name);
                            let candidate_modules = self
                                .context
                                .dep_graph
                                .find_function_by_name_anywhere(access.member);
                            if candidate_modules.is_empty() {
                                self.record(TypeErrorKind::UndefinedFunction(qualified_name));
                            } else {
                                let suggestion_paths: Vec<String> = candidate_modules
                                    .iter()
                                    .filter_map(|&midx| {
                                        self.context.dep_graph.get_module_package(midx)
                                    })
                                    .map(|pkg| pkg.to_string())
                                    .collect();
                                self.record(TypeErrorKind::UndefinedFunctionWithSuggestion {
                                    name: qualified_name,
                                    suggested_modules: suggestion_paths,
                                });
                            }
                            return HirType::Infer;
                        }
                    };

                    let expected_args = func.params.map(|p| p.len()).unwrap_or(0);
                    if args.len() != expected_args {
                        self.record(TypeErrorKind::InvalidFunctionCall {
                            expected_args,
                            found_args: args.len(),
                        });
                    }
                    if let Some(params) = func.params {
                        for (arg, param) in args.iter().zip(params.iter()) {
                            let arg_type = self.check_expr(arg);
                            if let Some(param_type) = param.get_type() {
                                let result = self.types_compatible(param_type, &arg_type);
                                self.recover(result, ());
                            }
                        }
                    }
                    func.return_type.unwrap_or(HirType::Void)
                }

                other => {
                    let callee_type = self.check_expr(other);
                    match callee_type {
                        HirType::Lambda { return_type, .. } => *return_type,
                        _ => {
                            self.record(TypeErrorKind::Generic(format!(
                                "Expression of type `{}` is not callable",
                                self.type_to_string(&callee_type)
                            )));
                            HirType::Infer
                        }
                    }
                }
            },
            HirExpr::FieldAccess {
                object,
                field,
                span,
            } => {
                self.set_span(*span);
                self.check_field_access(object, *field)
            }
            HirExpr::StructInit {
                name,
                args: _,
                span,
            } => {
                self.set_span(*span);
                let HirExpr::Ident(name, _) = name else {
                    return HirType::Void;
                };
                let struct_name_str = self.str_id_to_string(*name);
                let field_slice = if let Some(class) = self.context.get_struct(&struct_name_str) {
                    let field_types: Vec<HirType<'a, 'bump>> =
                        class.fields.iter().map(|f| f.field_type).collect();
                    self.context.bump.alloc_slice(&field_types)
                } else {
                    self.record(TypeErrorKind::UndefinedType(struct_name_str));
                    &[]
                };
                HirType::Struct(*name, field_slice)
            }
            HirExpr::InterfaceCall {
                callee, interface, ..
            } => {
                // Not yet emitted by the lowerer (dynamic dispatch currently
                // goes through `Call` on a `Dyn`-typed field access instead),
                // but handle it defensively: the receiver must implement the
                // named interface, and the call's type is that interface's
                // sole/declared method return type if one can be found.
                let _ = self.check_expr(callee);
                let iface_name = interface.to_string();
                match self.context.get_interface(&iface_name) {
                    Some(iface) => iface
                        .methods
                        .and_then(|methods| methods.first())
                        .and_then(|m| m.return_type)
                        .unwrap_or(HirType::Void),
                    None => {
                        self.record(TypeErrorKind::UndefinedType(iface_name));
                        HirType::Infer
                    }
                }
            }
            HirExpr::Assignment {
                target,
                op,
                value,
                span,
            } => {
                self.set_span(*span);
                let value_type = self.check_expr(value);
                let target_type = self.check_expr(target);

                // Mutability is only tracked for plain local bindings today;
                // field/deref targets aren't tracked per-field yet.
                if let HirExpr::Ident(name, _) = target {
                    let var_name = self.str_id_to_string(*name);
                    if self.context.is_local_binding(&var_name)
                        && !self.context.is_mutable(&var_name)
                    {
                        self.record(TypeErrorKind::Generic(format!(
                            "cannot assign to `{}`: it is not declared `mut`",
                            var_name
                        )));
                    }
                }

                use ir::hir::AssignmentOperator::*;
                let bin_result = match op {
                    Assign => self.types_compatible(&target_type, &value_type),
                    AddAssign => self
                        .check_binary_op(&target_type, &Operator::Add, &value_type)
                        .map(|_| ()),
                    SubtractAssign => self
                        .check_binary_op(&target_type, &Operator::Subtract, &value_type)
                        .map(|_| ()),
                    MultiplyAssign => self
                        .check_binary_op(&target_type, &Operator::Multiply, &value_type)
                        .map(|_| ()),
                    DivideAssign => self
                        .check_binary_op(&target_type, &Operator::Divide, &value_type)
                        .map(|_| ()),
                    ModuloAssign => self
                        .check_binary_op(&target_type, &Operator::Modulo, &value_type)
                        .map(|_| ()),
                    BitAndAssign => self
                        .check_binary_op(&target_type, &Operator::BitAnd, &value_type)
                        .map(|_| ()),
                    BitOrAssign => self
                        .check_binary_op(&target_type, &Operator::BitOr, &value_type)
                        .map(|_| ()),
                    BitXorAssign => self
                        .check_binary_op(&target_type, &Operator::BitXor, &value_type)
                        .map(|_| ()),
                    ShiftLeftAssign => self
                        .check_binary_op(&target_type, &Operator::ShiftLeft, &value_type)
                        .map(|_| ()),
                    ShiftRightAssign => self
                        .check_binary_op(&target_type, &Operator::ShiftRight, &value_type)
                        .map(|_| ()),
                };
                self.recover(bin_result, ());

                target_type
            }
            HirExpr::InterpolatedString(parts) => {
                // Not yet emitted by the lowerer, but check any embedded
                // expressions defensively so this doesn't panic once it is.
                for part in *parts {
                    if let ir::hir::InterpolationPart::Expr(e) = part {
                        self.check_expr(e);
                    }
                }
                HirType::String
            }
            HirExpr::EnumInit {
                enum_name,
                variant,
                args,
                span: _,
            } => {
                let enum_name_str = self.str_id_to_string(*enum_name);
                let Some(enum_def) = self.context.get_enum(&enum_name_str) else {
                    self.record(TypeErrorKind::UndefinedType(enum_name_str));
                    return HirType::Infer;
                };

                let variant_name = self.str_id_to_string(*variant);
                let variant_def = enum_def
                    .variants
                    .iter()
                    .find(|v| self.str_id_to_string(v.name) == variant_name);
                let Some(variant_def) = variant_def else {
                    self.record(TypeErrorKind::Generic(format!(
                        "enum `{}` has no variant `{}`",
                        enum_name_str, variant_name
                    )));
                    return HirType::Infer;
                };

                if args.len() != variant_def.fields.len() {
                    self.record(TypeErrorKind::InvalidFunctionCall {
                        expected_args: variant_def.fields.len(),
                        found_args: args.len(),
                    });
                }
                for (arg, field) in args.iter().zip(variant_def.fields.iter()) {
                    let arg_type = self.check_expr(arg);
                    let result = self.types_compatible(&field.field_type, &arg_type);
                    self.recover(result, ());
                }

                HirType::Enum(*enum_name, &[])
            }
            HirExpr::ExprList { list, span } => {
                self.set_span(*span);
                let mut last = HirType::Void;
                for e in *list {
                    last = self.check_expr(e);
                }
                last
            }
            HirExpr::Get {
                object,
                field,
                span,
            } => {
                self.set_span(*span);
                self.check_field_access(object, *field)
            }
            HirExpr::Comparison {
                left,
                op,
                right,
                span,
            } => {
                self.set_span(*span);
                let left_type = self.check_expr(left);
                let right_type = self.check_expr(right);
                let result = self.check_binary_op(&left_type, op, &right_type);
                self.recover(result, HirType::Infer)
            }
            HirExpr::Deref { expr, span } => {
                self.set_span(*span);
                let inner_ty = self.check_expr(expr);
                match inner_ty {
                    HirType::Ref { inner, .. } => *inner,
                    HirType::SafePointer(inner) => *inner,
                    HirType::UnsafePointer(inner) => *inner,
                    _ => {
                        self.record(TypeErrorKind::Generic(format!(
                            "cannot dereference non-pointer type `{}`",
                            self.type_to_string(&inner_ty)
                        )));
                        HirType::Infer
                    }
                }
            }
            HirExpr::Ref {
                expr,
                mutable,
                span,
            } => {
                self.set_span(*span);
                let expr = self.check_expr(expr);
                HirType::Ref {
                    inner: self.context.bump.alloc_value(expr),
                    mutability_state: if *mutable {
                        MutabilityState::Mut
                    } else {
                        MutabilityState::Const
                    },
                }
            }
            HirExpr::This { span } => {
                self.set_span(*span);
                // The concrete receiver type isn't bound into scope yet (see
                // `check_function`, which only registers `HirParam::Normal`
                // params); fall back to the generic `This` marker so method
                // bodies referencing `this` don't panic the checker.
                self.context.get_variable("this").unwrap_or(HirType::This)
            }
            HirExpr::ModuleAccess(access) => {
                self.set_span(access.span);
                let member_name = access.member.to_string();
                let resolved_module_idx = self.context.dep_graph.resolve_module_path(access.path);

                let func = resolved_module_idx
                    .and_then(|midx| self.context.get_module_function(midx, &member_name));

                match func {
                    Some(f) => {
                        let param_types: Vec<HirType<'a, 'bump>> = f
                            .params
                            .unwrap_or(&[])
                            .iter()
                            .filter_map(|p| p.get_type().copied())
                            .collect();
                        HirType::Lambda {
                            params: self.context.bump.alloc_slice(&param_types),
                            return_type: self
                                .context
                                .bump
                                .alloc_value(f.return_type.unwrap_or(HirType::Void)),
                        }
                    }
                    None => {
                        self.record(TypeErrorKind::Generic(format!(
                            "cannot resolve module member `{}`",
                            member_name
                        )));
                        HirType::Infer
                    }
                }
            }
            HirExpr::Lambda {
                params,
                return_type,
                body,
                span,
                ..
            } => {
                self.set_span(*span);
                let mut lambda_context = self.context.create_child_scope();
                for p in *params {
                    let param_name = self.str_id_to_string(p.name);
                    let param_ty = p.param_type.unwrap_or(HirType::Infer);
                    lambda_context.add_variable(param_name, param_ty);
                }

                let old_context = std::mem::replace(&mut self.context, lambda_context);
                self.check_stmt(body);
                self.context = old_context;

                let param_types: Vec<HirType<'a, 'bump>> = params
                    .iter()
                    .map(|p| p.param_type.unwrap_or(HirType::Infer))
                    .collect();

                HirType::Lambda {
                    params: self.context.bump.alloc_slice(&param_types),
                    return_type: self.context.bump.alloc_value(*return_type),
                }
            }
        }
    }

    fn check_receiver_is_mutable(
        &self,
        receiver: &HirExpr<'a, 'bump>,
        method_name: &str,
    ) -> TypeCheckResult<'a, ()> {
        let Some(root_name) = self.find_root_local_ident(receiver) else {
            return Ok(());
        };

        if !self.context.is_mutable(&root_name) {
            return Err(TypeErrorKind::Generic(format!(
                "cannot call `{}` on `{}`: `{}` is not declared `mut`",
                method_name, root_name, root_name
            ))
            .at(self.current_span));
        }

        Ok(())
    }

    fn expr_is_dangling(&self, expr: &HirExpr<'a, 'bump>) -> bool {
        match expr {
            HirExpr::Ref { expr: inner, .. } => match self.find_root_local_ident(inner) {
                Some(root_name) => match self.context.get_variable(&root_name) {
                    Some(root_type) => !root_type.is_pointer_semantics(),
                    None => false,
                },
                None => false,
            },
            HirExpr::Ident(name, _) => {
                let var_name = self.str_id_to_string(*name);
                self.context.is_dangling(&var_name)
            }
            _ => false,
        }
    }

    fn check_no_dangling_pointer(&self, expr: &HirExpr<'a, 'bump>) -> TypeCheckResult<'a, ()> {
        // Direct case: `return &local;`
        if let HirExpr::Ref { expr: inner, .. } = expr {
            if let Some(root_name) = self.find_root_local_ident(inner) {
                if let Some(root_type) = self.context.get_variable(&root_name) {
                    if !root_type.is_pointer_semantics() {
                        return Err(TypeErrorKind::Generic(format!(
                            "cannot return a pointer to local variable `{}`: its storage does not outlive this function",
                            root_name
                        )).at(self.current_span));
                    }
                }
            }
            return Ok(());
        }

        // Indirect case: `return i;` where `i` was assigned `&local` earlier.
        if let HirExpr::Ident(name, _) = expr {
            let var_name = self.str_id_to_string(*name);
            if self.context.is_dangling(&var_name) {
                return Err(TypeErrorKind::Generic(format!(
                    "cannot return `{}`: it holds a pointer to local stack memory that does not outlive this function",
                    var_name
                )).at(self.current_span));
            }
        }

        Ok(())
    }

    fn find_root_local_ident(&self, expr: &HirExpr<'a, 'bump>) -> Option<String> {
        match expr {
            HirExpr::Ident(name, _) => Some(self.str_id_to_string(*name)),
            HirExpr::FieldAccess { object, .. } | HirExpr::Get { object, .. } => {
                self.find_root_local_ident(object)
            }
            HirExpr::Deref { expr: inner, .. } => self.find_root_local_ident(inner),
            // Call results, literals, etc: not a named local's address.
            _ => None,
        }
    }

    fn check_binary_op(
        &self,
        left: &HirType<'a, 'bump>,
        op: &Operator,
        right: &HirType<'a, 'bump>,
    ) -> TypeCheckResult<'a, HirType<'a, 'bump>> {
        use Operator::*;

        match op {
            Add | Subtract | Multiply | Divide | Modulo => {
                if self.is_numeric(left) && self.is_numeric(right) {
                    Ok(*left)
                } else {
                    Err(TypeErrorKind::InvalidBinaryOp {
                        op: format!("{:?}", op),
                        left: self.type_to_string(left),
                        right: self.type_to_string(right),
                    }
                    .at(self.current_span))
                }
            }
            Equals | NotEquals | LessThan | LessThanOrEqual | GreaterThan | GreaterThanOrEqual => {
                if self.is_comparable(left) && self.is_comparable(right) {
                    Ok(HirType::Boolean)
                } else {
                    Err(TypeErrorKind::InvalidBinaryOp {
                        op: format!("{:?}", op),
                        left: self.type_to_string(left),
                        right: self.type_to_string(right),
                    }
                    .at(self.current_span))
                }
            }

            LogicalAnd | LogicalOr => {
                if *left == HirType::Boolean && *right == HirType::Boolean {
                    Ok(HirType::Boolean)
                } else {
                    Err(TypeErrorKind::InvalidBinaryOp {
                        op: format!("{:?}", op),
                        left: self.type_to_string(left),
                        right: self.type_to_string(right),
                    }
                    .at(self.current_span))
                }
            }

            BitAnd | BitOr | BitXor | ShiftLeft | ShiftRight => {
                if self.is_integer(left) && self.is_integer(right) {
                    Ok(*left)
                } else {
                    Err(TypeErrorKind::InvalidBinaryOp {
                        op: format!("{:?}", op),
                        left: self.type_to_string(left),
                        right: self.type_to_string(right),
                    }
                    .at(self.current_span))
                }
            }

            _ => Err(TypeErrorKind::Generic(format!(
                "operator `{:?}` cannot appear in this position",
                op
            ))
            .at(self.current_span)),
        }
    }

    /// Shared by `FieldAccess` and `Get`, which have identical semantics
    /// today (`Get` is currently also used for array indexing, see
    /// `expr_lowering.rs`).
    fn check_field_access(
        &mut self,
        object: &HirExpr<'a, 'bump>,
        field: StrId,
    ) -> HirType<'a, 'bump> {
        let obj_type = self.check_expr(object);
        match obj_type {
            HirType::Struct(struct_name, _) => {
                let struct_name_str = self.str_id_to_string(struct_name);
                let Some(struct_def) = self.context.get_struct(&struct_name_str) else {
                    self.record(TypeErrorKind::UndefinedType(struct_name_str));
                    return HirType::Infer;
                };

                let field_name = self.str_id_to_string(field);
                for f in struct_def.fields {
                    if self.str_id_to_string(f.name) == field_name {
                        return f.field_type;
                    }
                }

                self.record(TypeErrorKind::FieldNotFound {
                    struct_name: struct_name_str,
                    field: field_name,
                });
                HirType::Infer
            }
            _ => {
                self.record(TypeErrorKind::Generic(format!(
                    "Cannot access field on non-struct type: {}",
                    self.type_to_string(&obj_type)
                )));
                HirType::Infer
            }
        }
    }

    fn types_compatible(
        &self,
        expected: &HirType<'a, 'bump>,
        found: &HirType<'a, 'bump>,
    ) -> TypeCheckResult<'a, ()> {
        if expected == found {
            return Ok(());
        }

        // `HirType::Infer` stands in for "already reported" (a placeholder
        // returned after some other error), so don't cascade a second
        // complaint about it.
        if *expected == HirType::Infer || *found == HirType::Infer {
            return Ok(());
        }

        if self.struct_satisfies_interface_type(expected, found)
            || self.struct_satisfies_interface_type(found, expected)
        {
            return Ok(());
        }

        if let HirType::Nullable(_) = expected {
            if found == &HirType::Null {
                return Ok(());
            }
        }

        Err(TypeErrorKind::TypeMismatch {
            expected: self.type_to_string(expected),
            found: self.type_to_string(found),
        }
        .at(self.current_span))
    }

    /// Returns true if `found` is a struct (possibly behind a Ref/pointer) that
    /// implements the interface named by `expected` (possibly behind a Ref/
    /// pointer, possibly wrapped in `Dyn { bounds }`). Checks one direction;
    /// callers should check both directions since either operand position can
    /// be the interface or the concrete type depending on call context.
    fn struct_satisfies_interface_type(
        &self,
        expected: &HirType<'a, 'bump>,
        found: &HirType<'a, 'bump>,
    ) -> bool {
        let expected_inner = Self::strip_ref(expected);
        let found_inner = Self::strip_ref(found);

        let interface_name = match expected_inner {
            // Lowerer represents interface bounds as Struct(name, []) inside Dyn,
            // not as a separate DynInterface variant — confirm by name against
            // the interfaces table rather than trusting the HirType shape alone.
            HirType::DynInterface(name, _) => Some(name.to_string()),
            HirType::Dyn { bounds } => bounds.iter().find_map(|b| match b {
                HirType::DynInterface(name, _) => Some(name.to_string()),
                HirType::Struct(name, _) => {
                    let name_str = name.to_string();
                    if self.context.get_interface(&name_str).is_some() {
                        Some(name_str)
                    } else {
                        None
                    }
                }
                _ => None,
            }),
            _ => None,
        };

        let Some(interface_name) = interface_name else {
            return false;
        };

        let struct_name = match found_inner {
            HirType::Struct(name, _) => name.to_string(),
            _ => return false,
        };

        self.context
            .struct_implements(&struct_name, &interface_name)
    }

    /// Unwraps a single layer of `Ref` to get at the underlying type, since
    /// `&Vec3f` vs `&dyn Printable` need to be compared on their pointee types.
    fn strip_ref<'x>(ty: &'x HirType<'a, 'bump>) -> &'x HirType<'a, 'bump> {
        match ty {
            HirType::Ref { inner, .. } => inner,
            HirType::SafePointer(inner) => inner,
            _ => ty,
        }
    }

    fn is_numeric(&self, ty: &HirType<'a, 'bump>) -> bool {
        matches!(
            ty,
            HirType::I8
                | HirType::I16
                | HirType::I32
                | HirType::I64
                | HirType::U8
                | HirType::U16
                | HirType::U32
                | HirType::U64
                | HirType::F32
                | HirType::F64
                | HirType::I128
                | HirType::U128
        )
    }

    fn is_integer(&self, ty: &HirType<'a, 'bump>) -> bool {
        matches!(
            ty,
            HirType::I8
                | HirType::I16
                | HirType::I32
                | HirType::I64
                | HirType::U8
                | HirType::U16
                | HirType::U32
                | HirType::U64
                | HirType::I128
                | HirType::U128
        )
    }

    fn is_comparable(&self, ty: &HirType<'a, 'bump>) -> bool {
        self.is_numeric(ty) || matches!(ty, HirType::Boolean | HirType::String)
    }

    fn str_id_to_string(&self, id: StrId) -> String {
        format!("{}", id)
    }

    fn type_to_string(&self, ty: &HirType<'a, 'bump>) -> String {
        match ty {
            HirType::I8 => "i8".to_string(),
            HirType::I16 => "i16".to_string(),
            HirType::I32 => "i32".to_string(),
            HirType::I64 => "i64".to_string(),
            HirType::U8 => "u8".to_string(),
            HirType::U16 => "u16".to_string(),
            HirType::U32 => "u32".to_string(),
            HirType::U64 => "u64".to_string(),
            HirType::F32 => "f32".to_string(),
            HirType::F64 => "f64".to_string(),
            HirType::I128 => "i128".to_string(),
            HirType::U128 => "u128".to_string(),
            HirType::Boolean => "bool".to_string(),
            HirType::String => "string".to_string(),
            HirType::Void => "void".to_string(),
            HirType::Infer => "<inferred>".to_string(),
            HirType::Struct(name, _) => format!("struct {}", self.str_id_to_string(*name)),
            HirType::DynInterface(name, _) => format!("interface {}", self.str_id_to_string(*name)),
            HirType::Enum(name, _) => format!("enum {}", self.str_id_to_string(*name)),
            HirType::Generic(name) => format!("generic {}", self.str_id_to_string(*name)),
            HirType::SafePointer(_) => "*Ptr".to_string(),
            HirType::UnsafePointer(_) => "[*]Ptr".to_string(),
            HirType::Lambda { .. } => "lambda".to_string(),
            HirType::This => "this".to_string(),
            HirType::Null => "null".to_string(),
            HirType::Char => "char".to_string(),
            HirType::Ref {
                inner,
                mutability_state,
            } => {
                if let MutabilityState::Mut = mutability_state {
                    format!("&mut {}", inner)
                } else {
                    format!("&{}", inner)
                }
            }
            HirType::Nullable(hir_type) => format!("{}?", hir_type),
            HirType::Dyn { bounds } => {
                let mut bounds_str = String::new();
                let mut start = true;
                for bound in *bounds {
                    bounds_str.push_str(&bound.to_string());
                    if start {
                        start = false;
                    } else {
                        bounds_str.push_str(" + ");
                    }
                }
                format!("dyn {}", bounds_str)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_type_checker_creation() {
        let dep_graph = DepGraph::new();

        let bump = GrowableBump::new(4096, 8);
        let checker = TypeChecker::new(&dep_graph, &bump);
        assert_eq!(checker.context.variables.len(), 0);
        assert!(!checker.has_errors());
    }

    #[test]
    fn test_is_numeric() {
        let dep_graph = DepGraph::new();
        let bump = GrowableBump::new(4096, 8);
        let checker = TypeChecker::new(&dep_graph, &bump);
        assert!(checker.is_numeric(&HirType::I32));
        assert!(checker.is_numeric(&HirType::F64));
        assert!(!checker.is_numeric(&HirType::Boolean));
        assert!(!checker.is_numeric(&HirType::String));
    }

    #[test]
    fn test_is_comparable() {
        let dep_graph = DepGraph::new();
        let bump = GrowableBump::new(4096, 8);
        let checker = TypeChecker::new(&dep_graph, &bump);
        assert!(checker.is_comparable(&HirType::I32));
        assert!(checker.is_comparable(&HirType::Boolean));
        assert!(checker.is_comparable(&HirType::String));
    }

    #[test]
    fn test_type_to_string() {
        let dep_graph = DepGraph::new();
        let bump = GrowableBump::new(4096, 8);
        let checker = TypeChecker::new(&dep_graph, &bump);
        assert_eq!(checker.type_to_string(&HirType::I32), "i32");
        assert_eq!(checker.type_to_string(&HirType::Boolean), "bool");
        assert_eq!(checker.type_to_string(&HirType::String), "string");
        assert_eq!(checker.type_to_string(&HirType::Void), "void");
    }

    #[test]
    fn test_types_compatible_same_type() {
        let dep_graph = DepGraph::new();
        let bump = GrowableBump::new(4096, 8);
        let checker = TypeChecker::new(&dep_graph, &bump);
        let result = checker.types_compatible(&HirType::I32, &HirType::I32);
        assert!(result.is_ok());
    }

    #[test]
    fn test_types_compatible_different_type() {
        let dep_graph = DepGraph::new();
        let bump = GrowableBump::new(4096, 8);
        let checker = TypeChecker::new(&dep_graph, &bump);
        let result = checker.types_compatible(&HirType::I32, &HirType::I64);
        assert!(result.is_err());
    }

    #[test]
    fn test_binary_op_addition() {
        let dep_graph = DepGraph::new();
        let bump = GrowableBump::new(4096, 8);
        let checker = TypeChecker::new(&dep_graph, &bump);
        let result = checker.check_binary_op(&HirType::I32, &Operator::Add, &HirType::I32);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), HirType::I32);
    }

    #[test]
    fn test_binary_op_comparison() {
        let dep_graph = DepGraph::new();
        let bump = GrowableBump::new(4096, 8);
        let checker = TypeChecker::new(&dep_graph, &bump);
        let result = checker.check_binary_op(&HirType::I32, &Operator::LessThan, &HirType::I32);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), HirType::Boolean);
    }

    #[test]
    fn test_binary_op_logical() {
        let dep_graph = DepGraph::new();
        let bump = GrowableBump::new(4096, 8);
        let checker = TypeChecker::new(&dep_graph, &bump);
        let result =
            checker.check_binary_op(&HirType::Boolean, &Operator::LogicalAnd, &HirType::Boolean);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), HirType::Boolean);
    }

    #[test]
    fn test_binary_op_type_mismatch() {
        let dep_graph = DepGraph::new();
        let bump = GrowableBump::new(4096, 8);
        let checker = TypeChecker::new(&dep_graph, &bump);
        let result = checker.check_binary_op(&HirType::I32, &Operator::Add, &HirType::String);
        assert!(result.is_err());
    }
}
