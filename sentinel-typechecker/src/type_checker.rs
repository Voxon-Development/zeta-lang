use crate::type_context::TypeContext;
use crate::type_error::{TypeCheckResult, TypeError};
use codex_dependency_graph::DepGraph;
use ir::ast::MutabilityState;
use ir::hir::{
    Hir, HirExpr, HirFunc, HirModule, HirParam, HirStmt, HirType, Operator, StrId, ThisPassingKind,
};
use zetaruntime::bump::GrowableBump;

pub struct TypeChecker<'a, 'bump> {
    context: TypeContext<'a, 'bump>,
}

impl<'a, 'bump> TypeChecker<'a, 'bump> {
    pub fn new(dep_graph: &'a DepGraph, bump: &'bump GrowableBump<'bump>) -> Self {
        Self {
            context: TypeContext::new(dep_graph, bump),
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
                Hir::Func(f) => {
                    let name = f.unmangled_name.to_string();
                    self.context.add_function(module_idx, name, **f);
                }
                _ => {}
            }
        }

        self.context.current_module_idx = prev_module_idx;
    }

    pub fn check_module_body(
        &mut self,
        module: &HirModule<'a, 'bump>,
        module_idx: usize,
    ) -> TypeCheckResult<()> {
        self.context.current_module_idx = module_idx;
        for item in module.items {
            if let Hir::Func(func) = item {
                self.check_function(func)?;
            }
        }
        Ok(())
    }

    fn check_function(&mut self, func: &HirFunc<'a, 'bump>) -> TypeCheckResult<()> {
        let mut func_context = self.context.create_child_scope();

        if let Some(params) = func.params {
            for param in params {
                match param {
                    HirParam::Normal { name, param_type } => {
                        let param_name = self.str_id_to_string(*name);
                        func_context.add_variable(param_name, *param_type);
                    }
                    _ => {}
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
                self.check_stmt(&stmt)?;
            }
            self.context = old_context;
        }

        Ok(())
    }

    fn check_stmt(
        &mut self,
        stmt: &HirStmt<'a, 'bump>,
    ) -> TypeCheckResult<Option<HirType<'a, 'bump>>> {
        match stmt {
            HirStmt::Let {
                name,
                ty,
                value,
                mutable,
                else_block,
                ..
            } => {
                let value_type = self.check_expr(value)?;

                if let Some(else_block) = else_block {
                    self.check_stmt(else_block)?;
                    // TODO: Check if the type of the variable is the non-null version of this
                } else {
                    self.types_compatible(ty, &value_type)?;
                }
                let var_name = self.str_id_to_string(*name);

                if self.expr_is_dangling(value) {
                    self.context.mark_dangling(var_name.clone());
                }

                self.context
                    .add_variable_with_mutability(var_name, *ty, *mutable);
                Ok(None)
            }
            HirStmt::Return(expr) => {
                if let Some(e) = expr {
                    let expr_type = self.check_expr(e)?;
                    self.check_no_dangling_pointer(e)?;
                    if let Some(expected_return) = self.context.current_return_type {
                        self.types_compatible(&expected_return, &expr_type)?;
                    }
                } else if let Some(expected_return) = self.context.current_return_type {
                    if expected_return != HirType::Void {
                        return Err(TypeError::InvalidReturnType {
                            expected: self.type_to_string(&expected_return),
                            found: "void".to_string(),
                        });
                    }
                }
                Ok(None)
            }
            HirStmt::Expr(e) => {
                self.check_expr(e)?;
                Ok(None)
            }
            HirStmt::If {
                cond,
                then_block,
                else_block,
            } => {
                let cond_type = self.check_expr(cond)?;
                if cond_type != HirType::Boolean {
                    return Err(TypeError::TypeMismatch {
                        expected: "bool".to_string(),
                        found: self.type_to_string(&cond_type),
                    });
                }

                let mut then_context = self.context.create_child_scope();
                for stmt in *then_block {
                    let old_context = std::mem::replace(&mut self.context, then_context);
                    self.check_stmt(stmt)?;
                    then_context = self.context.clone();
                    self.context = old_context;
                }

                if let Some(else_stmt) = else_block {
                    let else_context = self.context.create_child_scope();
                    let old_context = std::mem::replace(&mut self.context, else_context);
                    self.check_stmt(else_stmt)?;
                    self.context = old_context;
                }

                Ok(None)
            }
            HirStmt::While { cond, body } => {
                let cond_type = self.check_expr(cond)?;
                if cond_type != HirType::Boolean {
                    return Err(TypeError::TypeMismatch {
                        expected: "bool".to_string(),
                        found: self.type_to_string(&cond_type),
                    });
                }

                self.context.enter_loop();
                self.check_stmt(body)?;
                self.context.exit_loop();

                Ok(None)
            }
            HirStmt::For {
                init,
                condition,
                increment,
                body,
            } => {
                if let Some(init_stmt) = init {
                    self.check_stmt(init_stmt)?;
                }

                if let Some(cond) = condition {
                    let cond_type = self.check_expr(cond)?;
                    if cond_type != HirType::Boolean {
                        return Err(TypeError::TypeMismatch {
                            expected: "bool".to_string(),
                            found: self.type_to_string(&cond_type),
                        });
                    }
                }

                self.context.enter_loop();
                self.check_stmt(body)?;
                self.context.exit_loop();

                if let Some(inc) = increment {
                    self.check_expr(inc)?;
                }

                Ok(None)
            }
            HirStmt::Block { body } => {
                let mut block_context = self.context.create_child_scope();
                for stmt in *body {
                    let old_context = std::mem::replace(&mut self.context, block_context);
                    self.check_stmt(stmt)?;
                    block_context = self.context.clone();
                    self.context = old_context;
                }
                Ok(None)
            }
            HirStmt::Break(expr, _span) => {
                if !self.context.in_loop {
                    return Err(TypeError::BreakOutsideLoop);
                }
                if let Some(e) = expr {
                    let expr_type = self.check_expr(e)?;
                    if let Some(expected_return) = self.context.current_return_type {
                        self.types_compatible(&expected_return, &expr_type)?;
                    }
                }
                Ok(None)
            }
            HirStmt::Continue(_span) => {
                if !self.context.in_loop {
                    return Err(TypeError::ContinueOutsideLoop);
                }
                Ok(None)
            }
            HirStmt::Const(const_stmt) => todo!(),
            HirStmt::Match { expr, arms } => todo!(),
            HirStmt::UnsafeBlock { body } => todo!(),
            HirStmt::Defer(hir_stmt) => todo!(),
            HirStmt::Import(path, source_span) => Ok(None),
            HirStmt::Package(path, source_span) => Ok(None),
            HirStmt::Throw { inner, span } => todo!(),
        }
    }

    fn check_expr(&mut self, expr: &HirExpr<'a, 'bump>) -> TypeCheckResult<HirType<'a, 'bump>> {
        match expr {
            HirExpr::Number(_) => Ok(HirType::I64),
            HirExpr::Null => Ok(HirType::Null),
            HirExpr::Decimal(_) => Ok(HirType::F64),
            HirExpr::Boolean(_) => Ok(HirType::Boolean),
            HirExpr::String(_) => Ok(HirType::String),
            HirExpr::Ident(name) => {
                let var_name = self.str_id_to_string(*name);
                self.context
                    .get_variable(&var_name)
                    .ok_or_else(|| TypeError::UndefinedVariable(var_name))
            }
            HirExpr::Tuple(exprs) => {
                let mut types = Vec::new();
                for e in *exprs {
                    types.push(self.check_expr(e)?);
                }
                Ok(types.first().copied().unwrap_or(HirType::Void))
            }
            HirExpr::Binary {
                left, op, right, ..
            } => {
                let left_type = self.check_expr(left)?;
                let right_type = self.check_expr(right)?;
                self.check_binary_op(&left_type, op, &right_type)
            }
            HirExpr::Call { callee, args } => match &callee {
                HirExpr::Ident(func_name) => {
                    let name = self.str_id_to_string(*func_name);
                    let func = self
                        .context
                        .get_function(&name)
                        .ok_or_else(|| TypeError::UndefinedFunction(name))?;

                    let expected_args = func.params.map(|p| p.len()).unwrap_or(0);
                    if args.len() != expected_args {
                        return Err(TypeError::InvalidFunctionCall {
                            expected_args,
                            found_args: args.len(),
                        });
                    }

                    if let Some(params) = func.params {
                        for (arg, param) in args.iter().zip(params.iter()) {
                            let arg_type = self.check_expr(arg)?;
                            if let Some(param_type) = param.get_type() {
                                self.types_compatible(param_type, &arg_type)?;
                            }
                        }
                    }

                    Ok(func.return_type.unwrap_or(HirType::Void))
                }

                HirExpr::FieldAccess { object, field, .. } => {
                    let obj_type = self.check_expr(object)?;
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
                        let iface = self
                            .context
                            .get_interface(&iface_name)
                            .ok_or_else(|| TypeError::UndefinedType(iface_name.clone()))?;

                        let method = iface
                            .methods
                            .and_then(|methods| {
                                methods
                                    .iter()
                                    .find(|m| m.unmangled_name.to_string() == method_name)
                            })
                            .ok_or_else(|| {
                                TypeError::Generic(format!(
                                    "no method `{}` on interface `{}`",
                                    method_name, iface_name
                                ))
                            })?;

                        let total_params = method.params.map(|p| p.len()).unwrap_or(0);
                        let expected_args = total_params.saturating_sub(1); // skip `this`
                        if args.len() != expected_args {
                            return Err(TypeError::InvalidFunctionCall {
                                expected_args,
                                found_args: args.len(),
                            });
                        }

                        if let Some(params) = method.params {
                            for (arg, param) in args.iter().zip(params.iter().skip(1)) {
                                let arg_type = self.check_expr(arg)?;
                                if let Some(param_type) = param.get_type() {
                                    self.types_compatible(param_type, &arg_type)?;
                                }
                            }
                        }

                        return Ok(method.return_type.unwrap_or(HirType::Void));
                    }

                    let type_name = match stripped {
                        HirType::Struct(name, _) => name.to_string(),
                        _ => {
                            return Err(TypeError::Generic(format!(
                                "cannot call method on non-struct type: {}",
                                self.type_to_string(&obj_type)
                            )))
                        }
                    };

                    let method_name = field.to_string();
                    let func = self
                        .context
                        .get_method(&type_name, &method_name)
                        .copied()
                        .ok_or_else(|| {
                            TypeError::Generic(format!(
                                "no method `{}` on `{}`",
                                method_name, type_name
                            ))
                        })?;

                    let total_params = func.params.map(|p| p.len()).unwrap_or(0);
                    let expected_args = total_params.saturating_sub(1);
                    if args.len() != expected_args {
                        return Err(TypeError::InvalidFunctionCall {
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
                                self.check_receiver_is_mutable(object, &method_name)?;
                            }
                        }

                        for (arg, param) in args.iter().zip(params.iter().skip(1)) {
                            let arg_type = self.check_expr(arg)?;
                            if let Some(param_type) = param.get_type() {
                                self.types_compatible(param_type, &arg_type)?;
                            }
                        }
                    }

                    Ok(func.return_type.unwrap_or(HirType::Void))
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
                            return if candidate_modules.is_empty() {
                                Err(TypeError::UndefinedFunction(qualified_name))
                            } else {
                                let suggestion_paths: Vec<String> = candidate_modules
                                    .iter()
                                    .filter_map(|&midx| {
                                        self.context.dep_graph.get_module_package(midx)
                                    })
                                    .map(|pkg| pkg.to_string())
                                    .collect();
                                Err(TypeError::UndefinedFunctionWithSuggestion {
                                    name: qualified_name,
                                    suggested_modules: suggestion_paths,
                                })
                            };
                        }
                    };

                    let expected_args = func.params.map(|p| p.len()).unwrap_or(0);
                    if args.len() != expected_args {
                        return Err(TypeError::InvalidFunctionCall {
                            expected_args,
                            found_args: args.len(),
                        });
                    }
                    if let Some(params) = func.params {
                        for (arg, param) in args.iter().zip(params.iter()) {
                            let arg_type = self.check_expr(arg)?;
                            if let Some(param_type) = param.get_type() {
                                self.types_compatible(param_type, &arg_type)?;
                            }
                        }
                    }
                    Ok(func.return_type.unwrap_or(HirType::Void))
                }

                other => {
                    let callee_type = self.check_expr(other)?;
                    match callee_type {
                        HirType::Lambda { return_type, .. } => Ok(*return_type),
                        _ => Err(TypeError::Generic(format!(
                            "Expression of type `{}` is not callable",
                            self.type_to_string(&callee_type)
                        ))),
                    }
                }
            },
            HirExpr::FieldAccess {
                object,
                field,
                span: _,
            } => {
                let obj_type = self.check_expr(object)?;
                match obj_type {
                    HirType::Struct(struct_name, _) => {
                        let struct_name_str = self.str_id_to_string(struct_name);
                        let struct_def = self
                            .context
                            .get_struct(&struct_name_str)
                            .ok_or_else(|| TypeError::UndefinedType(struct_name_str.clone()))?;

                        let field_name = self.str_id_to_string(*field);
                        for f in struct_def.fields {
                            if self.str_id_to_string(f.name) == field_name {
                                return Ok(f.field_type);
                            }
                        }

                        Err(TypeError::FieldNotFound {
                            struct_name: struct_name_str,
                            field: field_name,
                        })
                    }
                    _ => Err(TypeError::Generic(format!(
                        "Cannot access field on non-struct type: {}",
                        self.type_to_string(&obj_type)
                    ))),
                }
            }
            HirExpr::StructInit {
                name,
                args: _,
                span: _,
            } => {
                let HirExpr::Ident(name) = name else {
                    println!("I tried, but the real expr was {name:#?}");
                    return Ok(HirType::Void);
                };
                Ok(HirType::Struct(*name, &[]))
            }
            HirExpr::InterfaceCall {
                callee,
                args,
                interface,
            } => todo!(),
            HirExpr::Assignment {
                target,
                op,
                value,
                span,
            } => todo!(),
            HirExpr::InterpolatedString(interpolation_parts) => todo!(),
            HirExpr::EnumInit {
                enum_name,
                variant,
                args,
            } => todo!(),
            HirExpr::ExprList { list, span } => todo!(),
            HirExpr::Get {
                object,
                field,
                span,
            } => todo!(),
            HirExpr::Comparison {
                left,
                op,
                right,
                span,
            } => todo!(),
            HirExpr::Deref { expr, span } => todo!(),
            HirExpr::Ref {
                expr,
                mutable,
                span,
            } => {
                let expr = self.check_expr(expr)?;
                Ok(HirType::Ref {
                    inner: self.context.bump.alloc_value(expr),
                    mutability_state: if *mutable {
                        MutabilityState::Mut
                    } else {
                        MutabilityState::Const
                    },
                })
            }
            HirExpr::This { span } => todo!(),
            HirExpr::ModuleAccess(hir_module_access) => todo!(),
            HirExpr::Lambda {
                modifier,
                params,
                return_type,
                throws,
                body,
                span,
            } => todo!(),
        }
    }

    fn check_receiver_is_mutable(
        &self,
        receiver: &HirExpr<'a, 'bump>,
        method_name: &str,
    ) -> TypeCheckResult<()> {
        let Some(root_name) = self.find_root_local_ident(receiver) else {
            return Ok(());
        };

        if !self.context.is_mutable(&root_name) {
            return Err(TypeError::Generic(format!(
                "cannot call `{}` on `{}`: `{}` is not declared `mut`",
                method_name, root_name, root_name
            )));
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
            HirExpr::Ident(name) => {
                let var_name = self.str_id_to_string(*name);
                self.context.is_dangling(&var_name)
            }
            _ => false,
        }
    }

    fn check_no_dangling_pointer(&self, expr: &HirExpr<'a, 'bump>) -> TypeCheckResult<()> {
        // Direct case: `return &local;`
        if let HirExpr::Ref { expr: inner, .. } = expr {
            if let Some(root_name) = self.find_root_local_ident(inner) {
                if let Some(root_type) = self.context.get_variable(&root_name) {
                    if !root_type.is_pointer_semantics() {
                        return Err(TypeError::Generic(format!(
                            "cannot return a pointer to local variable `{}`: its storage does not outlive this function",
                            root_name
                        )));
                    }
                }
            }
            return Ok(());
        }

        // Indirect case: `return i;` where `i` was assigned `&local` earlier.
        if let HirExpr::Ident(name) = expr {
            let var_name = self.str_id_to_string(*name);
            if self.context.is_dangling(&var_name) {
                return Err(TypeError::Generic(format!(
                    "cannot return `{}`: it holds a pointer to local stack memory that does not outlive this function",
                    var_name
                )));
            }
        }

        Ok(())
    }

    fn find_root_local_ident(&self, expr: &HirExpr<'a, 'bump>) -> Option<String> {
        match expr {
            HirExpr::Ident(name) => Some(self.str_id_to_string(*name)),
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
    ) -> TypeCheckResult<HirType<'a, 'bump>> {
        use Operator::*;

        match op {
            Add | Subtract | Multiply | Divide | Modulo => {
                if self.is_numeric(left) && self.is_numeric(right) {
                    Ok(*left)
                } else {
                    Err(TypeError::InvalidBinaryOp {
                        op: format!("{:?}", op),
                        left: self.type_to_string(left),
                        right: self.type_to_string(right),
                    })
                }
            }
            Equals | NotEquals | LessThan | LessThanOrEqual | GreaterThan | GreaterThanOrEqual => {
                if self.is_comparable(left) && self.is_comparable(right) {
                    Ok(HirType::Boolean)
                } else {
                    Err(TypeError::InvalidBinaryOp {
                        op: format!("{:?}", op),
                        left: self.type_to_string(left),
                        right: self.type_to_string(right),
                    })
                }
            }

            LogicalAnd | LogicalOr => {
                if *left == HirType::Boolean && *right == HirType::Boolean {
                    Ok(HirType::Boolean)
                } else {
                    Err(TypeError::InvalidBinaryOp {
                        op: format!("{:?}", op),
                        left: self.type_to_string(left),
                        right: self.type_to_string(right),
                    })
                }
            }
            _ => todo!(),
        }
    }

    fn types_compatible(
        &self,
        expected: &HirType<'a, 'bump>,
        found: &HirType<'a, 'bump>,
    ) -> TypeCheckResult<()> {
        if expected == found {
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

        Err(TypeError::TypeMismatch {
            expected: self.type_to_string(expected),
            found: self.type_to_string(found),
        })
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
            } /*
              HirType::Dyn { bounds } => {
                  write!(f, "dyn ")?;
                  let mut is_start = true;
                  let mut iter = bounds.into_iter();
                  while let Some(bound) = iter.next() {
                      write!(f, "{}", bound)?;
                      if !is_start {
                          write!(f, " + ")?;
                      }
                      is_start = false;
                  }
                  Ok(())
              }
               */
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
