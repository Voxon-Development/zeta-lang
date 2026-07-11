use std::cell::RefCell;
use std::rc::Rc;
use std::sync::Arc;

use crate::move_state::MoveState;
use crate::type_context::TypeContext;
use codex_dependency_graph::DepGraph;
use ir::analysis_context::CopyAnalysisCtx;
use ir::ast::MutabilityState;
use ir::borrow_checker::{
    BorrowChecker, BorrowError, BorrowKind, Bound, IndexContainer, IndexTemplate, Interval, LoanId,
    PlaceId, RefTemplate, TemplateProjection,
};
use ir::errors::type_error::{TypeCheckResult, TypeError, TypeErrorKind};
use ir::hir::{
    Hir, HirExpr, HirFunc, HirModule, HirParam, HirStmt, HirType, Operator, ProvenanceAnnotation,
    ProvenancePathSegment, ProvenanceRoot, StrId, ThisPassingKind,
};
use ir::ir_hasher::FxHashMap;
use ir::span::SourceSpan;
use zetaruntime::bump::GrowableBump;
use zetaruntime::string_pool::StringPool;

pub struct TypeChecker<'a, 'bump> {
    context: TypeContext<'a, 'bump>,
    errors: Vec<TypeError<'a>>,
    current_span: SourceSpan<'a>,
    copy_analysis: Rc<RefCell<CopyAnalysisCtx<'a, 'bump>>>,
    move_state: MoveState,
    borrow_checker: BorrowChecker,
    this_id: StrId,
    suppress_errors: bool,
    ref_templates: FxHashMap<StrId, RefTemplate>,
}

impl<'a, 'bump> TypeChecker<'a, 'bump> {
    pub fn new(
        dep_graph: &'a RefCell<DepGraph>,
        bump: &'bump GrowableBump<'bump>,
        copy_analysis: Rc<RefCell<CopyAnalysisCtx<'a, 'bump>>>,
        string_pool: Arc<StringPool>,
    ) -> Self {
        Self {
            this_id: StrId(string_pool.intern("this")),
            context: TypeContext::new(dep_graph, bump, string_pool),
            errors: Vec::new(),
            current_span: SourceSpan::default(),
            copy_analysis,
            move_state: MoveState::new(),
            borrow_checker: BorrowChecker::new(),
            suppress_errors: false,
            ref_templates: FxHashMap::default(),
        }
    }

    pub fn errors(&self) -> &[TypeError<'a>] {
        &self.errors
    }

    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }

    pub fn take_errors(&mut self) -> Vec<TypeError<'a>> {
        std::mem::take(&mut self.errors)
    }

    fn set_span(&mut self, span: SourceSpan<'a>) {
        self.current_span = span;
    }

    fn record(&mut self, kind: TypeErrorKind) {
        if self.suppress_errors {
            return;
        }
        self.errors.push(kind.at(self.current_span));
    }

    fn recover<T>(&mut self, result: TypeCheckResult<'a, T>, fallback: T) -> T {
        match result {
            Ok(v) => v,
            Err(e) => {
                if !self.suppress_errors {
                    self.errors.push(e);
                }
                fallback
            }
        }
    }

    fn with_suppressed_errors<F: FnOnce(&mut Self)>(&mut self, f: F) {
        let prev = self.suppress_errors;
        self.suppress_errors = true;
        f(self);
        self.suppress_errors = prev;
    }

    fn converge_loop_move_state(
        &mut self,
        body: &HirStmt<'a, 'bump>,
        entry_state: MoveState,
    ) -> MoveState {
        let saved_move_state = self.move_state.clone();
        let saved_context = self.context.clone();

        let mut converged = entry_state;

        self.with_suppressed_errors(|this| loop {
            this.move_state = converged.clone();
            this.check_stmt(body);
            let next = MoveState::join(&converged, &this.move_state);

            let stable = converged.is_superset_of(&next);
            converged = next;
            if stable {
                break;
            }
        });

        self.move_state = saved_move_state;
        self.context = saved_context;
        converged
    }

    fn is_zeroable(&self, ty: &HirType<'a, 'bump>) -> bool {
        match ty {
            HirType::I8
            | HirType::I16
            | HirType::I32
            | HirType::I64
            | HirType::I128
            | HirType::U8
            | HirType::U16
            | HirType::U32
            | HirType::U64
            | HirType::U128
            | HirType::F32
            | HirType::F64 => true,

            HirType::Array(inner, _) => self.is_zeroable(inner),

            HirType::Tuple(elems) => elems.iter().all(|e| self.is_zeroable(e)),

            HirType::Struct(name, _) => {
                let name_str = self.str_id_to_string(*name);
                match self.context.get_struct(&name_str) {
                    Some(def) => def.fields.iter().all(|f| self.is_zeroable(&f.field_type)),
                    None => false, // unresolved struct
                }
            }

            HirType::Nullable(inner) => match **inner {
                // Pointer-shaped: all-zero bits legitimately means "null". Safe to zero-init.
                HirType::SafePointer(_)
                | HirType::UnsafePointer(_)
                | HirType::OwnedPointer(_)
                | HirType::Ref { .. } => true,
                // Non-pointer nullable (e.g. i32?) needs a discriminant/tag, not just zero
                // bits
                // We could probably add some optimizations like `NonZero<u32>` like in Rust, but for now, this is good enough.
                _ => false,
            },

            // Impermissible: bool, char, string, enums, interfaces/dyn, lambdas,
            // pointers/refs, nullable, slices, generics, void/null/this/unknown.
            // Zeroing these either produces an invalid bit pattern (bool/char/enum
            // discriminants), a dangling/null reference where one shouldn't
            // silently appear (pointers), or is simply meaningless (lambda, dyn).
            _ => false,
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
                    let name = f.name.to_string();
                    self.context.add_function(module_idx, name, **f);
                }
                _ => {}
            }
        }

        self.context.current_module_idx = prev_module_idx;
    }

    pub fn check_module_body(&mut self, module: &HirModule<'a, 'bump>, module_idx: usize) {
        self.context.current_module_idx = module_idx;
        for item in module.items {
            if let Hir::Func(func) = item {
                self.check_function(func);
            }
            if let Hir::Struct(ty_struct) = item {
                let Some(struct_interfaces) = self
                    .context
                    .struct_interfaces
                    .get(&ty_struct.name.to_string())
                else {
                    continue;
                };

                if struct_interfaces.contains("Copy") && struct_interfaces.contains("Drop") {
                    self.record(TypeErrorKind::Generic(format!(
                        "{} should not implement Copy and Drop at the same time",
                        ty_struct.name
                    )));
                }
            }
        }
    }

    fn check_function(&mut self, func: &HirFunc<'a, 'bump>) {
        let mut func_context = self.context.create_child_scope();

        self.borrow_checker = BorrowChecker::new();
        self.borrow_checker.begin_scope();

        if let Some(params) = func.params {
            for param in params {
                match param {
                    HirParam::Normal { name, param_type } => {
                        let param_name = self.str_id_to_string(*name);
                        func_context.add_variable(param_name, *param_type);
                        self.borrow_checker.declare_local(*name);
                    }
                    HirParam::This { .. } => {
                        self.borrow_checker.declare_local(self.this_id);
                    }
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

        self.check_return_provenance(func);

        self.borrow_checker.end_scope();
    }

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

                self.check_and_record_value_use(value, &value_type);

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

                if self.context.variables.contains_key(&var_name) {
                    self.record(TypeErrorKind::VariableAlreadyExists {
                        var_name: var_name.clone(),
                    });
                }

                if self.expr_is_dangling(value) {
                    self.context.mark_dangling(var_name.clone());
                }

                self.context
                    .add_variable_with_mutability(var_name, *ty, *mutable);
                self.borrow_checker.declare_local(*name);

                if matches!(ty, HirType::SafePointer(_) | HirType::UnsafePointer(_)) {
                    if let Some(place) = self.resolve_place(value) {
                        if let Some(&(base, ref offset)) = self.borrow_checker.pointee_of(place) {
                            let declared = *self.borrow_checker.local_place(*name).unwrap();
                            self.borrow_checker
                                .record_pointee(declared, base, offset.clone());
                        }
                    }
                }

                None
            }
            HirStmt::Return(expr) => {
                if let Some(e) = expr {
                    let expr_type = self.check_expr(e);
                    self.check_and_record_value_use(e, &expr_type);
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

                let move_state_before = self.move_state.clone();

                let fact = self.condition_to_fact(cond);

                self.borrow_checker.begin_scope();
                if let Some((lhs, rhs, is_equal)) = &fact {
                    if *is_equal {
                        self.borrow_checker
                            .assume_equal_scoped(lhs.clone(), rhs.clone());
                    } else {
                        self.borrow_checker
                            .assume_not_equal_scoped(lhs.clone(), rhs.clone());
                    }
                }

                let mut then_context = self.context.create_child_scope();
                for stmt in *then_block {
                    let old_context = std::mem::replace(&mut self.context, then_context);
                    self.check_stmt(stmt);
                    then_context = self.context.clone();
                    self.context = old_context;
                }
                self.borrow_checker.end_scope();
                let then_move_state = self.move_state.clone();

                self.move_state = move_state_before.clone();
                if let Some(else_stmt) = else_block {
                    self.borrow_checker.begin_scope();
                    if let Some((lhs, rhs, is_equal)) = &fact {
                        if *is_equal {
                            self.borrow_checker
                                .assume_not_equal_scoped(lhs.clone(), rhs.clone());
                        } else {
                            self.borrow_checker
                                .assume_equal_scoped(lhs.clone(), rhs.clone());
                        }
                    }
                    let else_context = self.context.create_child_scope();
                    let old_context = std::mem::replace(&mut self.context, else_context);
                    self.check_stmt(else_stmt);
                    self.context = old_context;
                    self.borrow_checker.end_scope();
                }
                let else_move_state = self.move_state.clone();

                self.move_state = MoveState::join(&then_move_state, &else_move_state);

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

                let entry_state = self.move_state.clone();
                let converged_entry = self.converge_loop_move_state(body, entry_state);

                self.move_state = converged_entry;
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

                let entry_state = self.move_state.clone();
                let converged_entry = self.converge_loop_move_state(body, entry_state);

                self.move_state = converged_entry;
                self.check_stmt(body);

                self.context.exit_loop();

                if let Some(inc) = increment {
                    self.check_expr(inc);
                }

                None
            }
            HirStmt::Block { body } => {
                self.borrow_checker.begin_scope();
                let mut block_context = self.context.create_child_scope();
                for stmt in *body {
                    let old_context = std::mem::replace(&mut self.context, block_context);
                    self.check_stmt(stmt);
                    block_context = self.context.clone();
                    self.context = old_context;
                }
                self.borrow_checker.end_scope();
                None
            }
            HirStmt::Break(expr, span) => {
                self.set_span(*span);
                if !self.context.in_loop {
                    self.record(TypeErrorKind::BreakOutsideLoop);
                }
                if let Some(e) = expr {
                    let expr_type = self.check_expr(e);
                    self.check_and_record_value_use(e, &expr_type);
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

                let move_state_before = self.move_state.clone();
                let mut arm_move_states: Vec<MoveState> = Vec::with_capacity(arms.len());

                for arm in *arms {
                    self.move_state = move_state_before.clone();
                    self.borrow_checker.begin_scope();

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
                    self.borrow_checker.end_scope();
                    arm_move_states.push(self.move_state.clone());
                }

                self.move_state = arm_move_states
                    .into_iter()
                    .fold(move_state_before, |acc, arm_state| {
                        MoveState::join(&acc, &arm_state)
                    });

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
            // TODO: Check if they reference valid paths
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

    fn condition_to_fact(&self, cond: &HirExpr<'a, 'bump>) -> Option<(Bound, Bound, bool)> {
        if let HirExpr::Comparison {
            left, op, right, ..
        } = cond
        {
            let is_equal = match op {
                Operator::Equals => true,
                Operator::NotEquals => false,
                _ => return None,
            };
            Some((
                self.expr_to_bound(left),
                self.expr_to_bound(right),
                is_equal,
            ))
        } else {
            None
        }
    }

    fn check_expr(&mut self, expr: &HirExpr<'a, 'bump>) -> HirType<'a, 'bump> {
        match expr {
            HirExpr::Number(_, _) => HirType::I64,
            HirExpr::Null(_) => HirType::Null,
            HirExpr::Decimal(_, _) => HirType::F64,
            HirExpr::Boolean(_, _) => HirType::Boolean,
            HirExpr::String(_, _) => HirType::String,
            HirExpr::Undefined { span, ty } => {
                self.set_span(*span);
                match ty {
                    HirType::Unknown => {
                        self.record(TypeErrorKind::TypeCannotBeInferred);
                        HirType::Unknown
                    }
                    other_type => {
                        if !self.is_zeroable(other_type) {
                            self.record(TypeErrorKind::Generic(format!(
                                "`undefined` cannot be used for type `{}`: it cannot be safely zero-initialized",
                                self.type_to_string(other_type)
                            )));
                            return HirType::Unknown;
                        }
                        *other_type
                    }
                }
            }
            HirExpr::Ident(name, _) => {
                let var_name = self.str_id_to_string(*name);
                match self.context.get_variable(&var_name) {
                    Some(ty) => ty,
                    None => {
                        self.record(TypeErrorKind::UndefinedVariable(var_name));
                        HirType::Unknown
                    }
                }
            }
            HirExpr::Tuple(exprs, _span) => {
                let mut types = Vec::new();
                for e in *exprs {
                    types.push(self.check_expr(e));
                }
                HirType::Tuple(self.context.bump.alloc_slice_copy(types.as_slice()))
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
                self.recover(result, HirType::Unknown)
            }
            HirExpr::Call { callee, args, span } => match &callee {
                HirExpr::Ident(func_name, _) => {
                    self.set_span(*span);
                    let lookup_name = self.str_id_to_string(*func_name);
                    let func = match self.context.get_function(&lookup_name) {
                        Some(f) => f,
                        None => {
                            self.record(TypeErrorKind::UndefinedFunction(
                                self.str_id_to_string(*func_name),
                            ));
                            return HirType::Unknown;
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
                        if let Some(value) =
                            self.check_potential_this_param_for_move(args, func, params)
                        {
                            return value;
                        }
                        let arg_loans = self.check_all_func_args(args, params, None);

                        let ret_ty = func.return_type.unwrap_or(HirType::Void);
                        if !self.return_type_may_alias(&ret_ty) {
                            for loan in arg_loans {
                                self.borrow_checker.end_loan_now(loan);
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
                                return HirType::Unknown;
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
                            return HirType::Unknown;
                        };

                        let total_params = method.params.map(|p| p.len()).unwrap_or(0);
                        let expected_args = total_params.saturating_sub(1);
                        if args.len() != expected_args {
                            self.record(TypeErrorKind::InvalidFunctionCall {
                                expected_args,
                                found_args: args.len(),
                            });
                        }

                        if let Some(params) = method.params {
                            if let Some(HirParam::This { kind }) = params.first() {
                                if matches!(kind, ThisPassingKind::Move | ThisPassingKind::MoveMut)
                                {
                                    self.check_and_record_value_use(object, &obj_type);
                                }
                            }
                            for (arg, param) in args.iter().zip(params.iter().skip(1)) {
                                let arg_type = self.check_expr(arg);
                                self.check_and_record_value_use(arg, &arg_type);
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
                            return HirType::Unknown;
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
                            return HirType::Unknown;
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
                            if matches!(kind, ThisPassingKind::Move | ThisPassingKind::MoveMut) {
                                self.check_and_record_value_use(object, &obj_type);
                            } else if let Some(place) = self.resolve_place(object) {
                                let borrow_kind = if requires_mut {
                                    BorrowKind::Mutable
                                } else {
                                    BorrowKind::Shared
                                };
                                self.check_borrow_use(expr, place, borrow_kind);
                            }
                        }

                        for (arg, param) in args.iter().zip(params.iter().skip(1)) {
                            let arg_type = self.check_expr(arg);
                            self.check_and_record_value_use(arg, &arg_type);
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
                    let resolved_module_idx = self
                        .context
                        .dep_graph
                        .borrow()
                        .resolve_module_path(access.path);

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
                                .borrow()
                                .find_function_by_name_anywhere(access.member);
                            if candidate_modules.is_empty() {
                                self.record(TypeErrorKind::UndefinedFunction(qualified_name));
                            } else {
                                let suggestion_paths: Vec<String> = candidate_modules
                                    .iter()
                                    .filter_map(|&midx| {
                                        self.context.dep_graph.borrow().get_module_package(midx)
                                    })
                                    .map(|pkg| pkg.to_string())
                                    .collect();
                                self.record(TypeErrorKind::UndefinedFunctionWithSuggestion {
                                    name: qualified_name,
                                    suggested_modules: suggestion_paths,
                                });
                            }

                            return HirType::Unknown;
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
                        let arg_loans = self.check_all_func_args(args, params, None);

                        let ret_ty = func.return_type.unwrap_or(HirType::Void);
                        if !self.return_type_may_alias(&ret_ty) {
                            for loan in arg_loans {
                                self.borrow_checker.end_loan_now(loan);
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
                            HirType::Unknown
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
            HirExpr::StructInit { name, args, span } => {
                self.set_span(*span);
                let HirExpr::Ident(name, _) = name else {
                    return HirType::Void;
                };
                let struct_name_str = self.str_id_to_string(*name);
                let field_slice = if let Some(class) = self.context.get_struct(&struct_name_str) {
                    let field_types: Vec<HirType<'a, 'bump>> =
                        class.fields.iter().map(|f| f.field_type).collect();

                    if args.len() != field_types.len() {
                        self.record(TypeErrorKind::InvalidFunctionCall {
                            expected_args: field_types.len(),
                            found_args: args.len(),
                        });
                    }

                    for (arg, field_type) in args.iter().zip(field_types.iter()) {
                        let arg_type = self.check_expr(arg);
                        self.check_and_record_value_use(arg, &arg_type);
                        let result = self.types_compatible(field_type, &arg_type);
                        self.recover(result, ());
                    }

                    self.context.bump.alloc_slice(&field_types)
                } else {
                    self.record(TypeErrorKind::UndefinedType(struct_name_str));
                    &[]
                };
                HirType::Struct(*name, field_slice)
            }
            HirExpr::InterfaceCall {
                callee,
                interface,
                args,
                ..
            } => {
                let _ = self.check_expr(callee);
                let iface_name = interface.to_string();
                for arg in *args {
                    let arg_type = self.check_expr(arg);
                    self.check_and_record_value_use(arg, &arg_type);
                }

                match self.context.get_interface(&iface_name) {
                    Some(iface) => iface
                        .methods
                        .and_then(|methods| methods.first())
                        .and_then(|m| m.return_type)
                        .unwrap_or(HirType::Void),
                    None => {
                        self.record(TypeErrorKind::UndefinedType(iface_name));
                        HirType::Unknown
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

                if let HirExpr::Deref { expr, .. } = target {
                    if let Some(base) = self.resolve_place(expr) {
                        let place = self.borrow_checker.project_deref(base);
                        self.check_borrow_use(expr, place, BorrowKind::Mutable);
                    }
                }

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
                    return HirType::Unknown;
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
                    return HirType::Unknown;
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
                self.recover(result, HirType::Unknown)
            }
            HirExpr::Deref { expr, span } => {
                self.set_span(*span);
                let inner_ty = self.check_expr(expr);
                if let Some(base) = self.resolve_place(expr) {
                    let place = self.borrow_checker.project_deref(base);
                    self.check_borrow_use(expr, place, BorrowKind::Shared);
                }
                match inner_ty {
                    HirType::Ref { inner, .. } => *inner,
                    HirType::SafePointer(inner) => *inner,
                    HirType::UnsafePointer(inner) => *inner,
                    HirType::OwnedPointer(inner) => *inner,
                    _ => {
                        self.record(TypeErrorKind::Generic(format!(
                            "cannot dereference non-pointer type `{}`",
                            self.type_to_string(&inner_ty)
                        )));
                        HirType::Unknown
                    }
                }
            }
            HirExpr::Ref {
                expr,
                mutable,
                span,
            } => self.check_ref_expr(expr, *mutable, *span, true),
            HirExpr::This { span } => {
                self.set_span(*span);
                self.context.get_variable("this").unwrap_or(HirType::This)
            }
            HirExpr::ModuleAccess(access) => {
                self.set_span(access.span);
                let member_name = access.member.to_string();
                let resolved_module_idx = self
                    .context
                    .dep_graph
                    .borrow()
                    .resolve_module_path(access.path);

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
                        HirType::Unknown
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
                    let param_ty = p.param_type.unwrap_or(HirType::Unknown);
                    lambda_context.add_variable(param_name, param_ty);
                }

                let old_context = std::mem::replace(&mut self.context, lambda_context);
                self.check_stmt(body);
                self.context = old_context;

                let param_types: Vec<HirType<'a, 'bump>> = params
                    .iter()
                    .map(|p| p.param_type.unwrap_or(HirType::Unknown))
                    .collect();

                HirType::Lambda {
                    params: self.context.bump.alloc_slice(&param_types),
                    return_type: self.context.bump.alloc_value(*return_type),
                }
            }
            HirExpr::Index {
                object,
                index,
                span,
            } => {
                self.set_span(*span);

                let object_ty = self.check_expr(object);
                let index_ty = self.check_expr(index);

                self.recover(self.types_compatible(&HirType::I64, &index_ty), ());

                match *Self::strip_ref(&object_ty) {
                    HirType::Array(inner, _) => *inner,
                    HirType::Slice(inner) => *inner,

                    _ => {
                        self.record(TypeErrorKind::Generic(format!(
                            "cannot index type `{}`",
                            self.type_to_string(&object_ty)
                        )));
                        HirType::Unknown
                    }
                }
            }
            HirExpr::ArrayLiteral { elements, span } => {
                self.set_span(*span);

                if elements.is_empty() {
                    self.record(TypeErrorKind::TypeCannotBeInferred);
                    return HirType::Unknown;
                }

                let first_ty = self.check_expr(&elements[0]);
                self.check_and_record_value_use(&elements[0], &first_ty);

                for elem in &elements[1..] {
                    let elem_ty = self.check_expr(elem);
                    self.check_and_record_value_use(elem, &elem_ty);
                    let result = self.types_compatible(&first_ty, &elem_ty);
                    self.recover(result, ());
                }

                HirType::Array(self.context.bump.alloc_value(first_ty), elements.len())
            }
        }
    }

    fn infer_provenance(&self, expr: &HirExpr<'a, 'bump>) -> Option<ProvenanceAnnotation<'bump>> {
        let mut segments = Vec::new();
        let root = self.infer_provenance_root(expr, &mut segments)?;
        segments.reverse();
        Some(ProvenanceAnnotation {
            root,
            path: self.context.bump.alloc_slice(&segments),
        })
    }

    fn infer_provenance_root(
        &self,
        expr: &HirExpr<'a, 'bump>,
        segments: &mut Vec<ProvenancePathSegment>,
    ) -> Option<ProvenanceRoot> {
        match expr {
            HirExpr::Ident(name, _) => Some(ProvenanceRoot::Var(*name)),
            HirExpr::This { .. } => Some(ProvenanceRoot::ThisRoot),

            HirExpr::FieldAccess { object, field, .. } | HirExpr::Get { object, field, .. } => {
                segments.push(ProvenancePathSegment::Field(*field));
                self.infer_provenance_root(object, segments)
            }

            HirExpr::Deref { expr: inner, .. } => {
                segments.push(ProvenancePathSegment::Deref);
                self.infer_provenance_root(inner, segments)
            }

            // Indexing loses static field-path precision (the index is runtime
            // data), but the borrowed region is still rooted in `object`, keep
            // the root so diagnostics can still say "derived from `x`" instead
            // of dropping to nothing. This intentionally does NOT push a path
            // segment, since there's no StrId/Deref to represent "at index i".
            HirExpr::Index { object, .. } => self.infer_provenance_root(object, segments),

            _ => None,
        }
    }

    fn check_ref_expr(
        &mut self,
        expr: &HirExpr<'a, 'bump>,
        mutable: bool,
        span: SourceSpan<'a>,
        register_loan: bool,
    ) -> HirType<'a, 'bump> {
        self.set_span(span);
        let inner_ty = self.check_expr(expr);
        let provenance = self.infer_provenance(expr);

        if register_loan {
            if let Some(place) = self.resolve_place(expr) {
                let result = if mutable {
                    self.borrow_checker.borrow_mut(place)
                } else {
                    self.borrow_checker.borrow_shared(place)
                };
                if let Err(e) = result {
                    let msg = self.describe_borrow_error(&e, provenance.as_ref());
                    self.record(TypeErrorKind::Generic(msg));
                }
            }
        }

        HirType::Ref {
            inner: self.context.bump.alloc_value(inner_ty),
            mutability_state: if mutable {
                MutabilityState::Mut
            } else {
                MutabilityState::Const
            },
            provenance,
        }
    }

    fn check_potential_this_param_for_move(
        &mut self,
        args: &[HirExpr<'a, 'bump>],
        func: HirFunc<'a, 'bump>,
        params: &[HirParam<'a, 'bump>],
    ) -> Option<HirType<'a, 'bump>> {
        let ret_ty = func.return_type.unwrap_or(HirType::Void);

        if let Some(this_param) = params.first() {
            if matches!(this_param, HirParam::This { .. }) {
                self.record(TypeErrorKind::IllegalThisParam {
                    func_name: func.unmangled_name.to_string(),
                });
                return Some(ret_ty);
            }

            let template = if self.return_type_may_alias(&ret_ty) {
                Some(self.analyze_ref_template(&func))
            } else {
                None
            };

            let templated_base_param = match &template {
                Some(RefTemplate::Path { base_param, .. }) => Some(*base_param),
                _ => None,
            };

            let arg_loans = self.check_all_func_args(args, params, templated_base_param);
            self.finalize_call_loans(args, arg_loans, &ret_ty, template);

            return Some(ret_ty);
        }
        Some(ret_ty) // Zero parameters
    }

    fn check_all_func_args(
        &mut self,
        args: &[HirExpr<'a, 'bump>],
        params: &[HirParam<'a, 'bump>],
        templated_base_param: Option<usize>,
    ) -> Vec<LoanId> {
        let mut arg_loans: Vec<LoanId> = Vec::new();

        for (i, (arg, param)) in args.iter().zip(params.iter()).enumerate() {
            if Some(i) == templated_base_param {
                if let HirExpr::Ref {
                    expr,
                    mutable,
                    span,
                } = arg
                {
                    let arg_type = self.check_ref_expr(expr, *mutable, *span, false);
                    if let Some(param_type) = param.get_type() {
                        let result = self.types_compatible(param_type, &arg_type);
                        self.recover(result, ());
                    }
                } else {
                    let arg_type = self.check_expr(arg);
                    self.check_and_record_value_use(arg, &arg_type);
                    if let Some(param_type) = param.get_type() {
                        let result = self.types_compatible(param_type, &arg_type);
                        self.recover(result, ());
                    }
                }
                continue;
            }

            let arg_type = self.check_expr(arg);
            self.check_and_record_value_use(arg, &arg_type);
            if let Some(param_type) = param.get_type() {
                let result = self.types_compatible(param_type, &arg_type);
                self.recover(result, ());
            }

            if let HirExpr::Ref { expr, .. } = arg {
                if let Some(place) = self.resolve_place(expr) {
                    if let Some(&loan_id) = self.borrow_checker.loan_for_place(place) {
                        arg_loans.push(loan_id);
                    }
                }
            }
        }

        arg_loans
    }

    /// True if a value of this type could itself hold or be a borrowed
    /// reference, i.e. calling a function returning this type might hand
    /// back something that aliases one of its ref-typed arguments.
    /// Conservative: struct/enum/tuple types that might *contain* a
    /// reference field also count, since e.g. `struct Pair { r: &mut i64 }`
    /// returned by value still carries the alias forward.
    fn return_type_may_alias(&self, ty: &HirType<'a, 'bump>) -> bool {
        match ty {
            // Direct reference-like types.
            HirType::Ref { .. }
            | HirType::SafePointer(_)
            | HirType::UnsafePointer(_)
            | HirType::OwnedPointer(_) => true,

            HirType::Nullable(inner) => self.return_type_may_alias(inner),

            HirType::Array(inner, _) => self.return_type_may_alias(inner),

            HirType::Tuple(elems) => elems.iter().any(|e| self.return_type_may_alias(e)),

            HirType::Struct(name, _) => {
                let name_str = self.str_id_to_string(*name);
                match self.context.get_struct(&name_str) {
                    Some(def) => def
                        .fields
                        .iter()
                        .any(|f| self.return_type_may_alias(&f.field_type)),
                    None => true, // unresolved
                }
            }

            HirType::Enum(name, _) => {
                let name_str = self.str_id_to_string(*name);
                match self.context.get_enum(&name_str) {
                    Some(def) => def
                        .variants
                        .iter()
                        .flat_map(|v| v.fields.iter())
                        .any(|f| self.return_type_may_alias(&f.field_type)),
                    None => true, // unresolved
                }
            }

            HirType::Dyn { .. } | HirType::DynInterface(..) => true,

            // Primitive/value-only types.
            _ => false,
        }
    }

    fn provenance_from_template(
        &self,
        template: &RefTemplate,
        args: &[HirExpr<'a, 'bump>],
    ) -> Option<ProvenanceAnnotation<'bump>> {
        let RefTemplate::Path {
            base_param,
            projections,
            ..
        } = template
        else {
            return None;
        };

        let base_arg = args.get(*base_param)?;
        let base_expr = match base_arg {
            HirExpr::Ref { expr, .. } => expr,
            other => other,
        };

        // Root/path of the argument expression itself, e.g. `list` in `&list.head`.
        let mut base_provenance = self.infer_provenance(base_expr)?;

        // Then extend with the template's own projections (skipping Index, same
        // rationale as infer_provenance_root: no static field/deref to name).
        let mut path: Vec<ProvenancePathSegment> = base_provenance.path.to_vec();
        for proj in projections {
            match proj {
                TemplateProjection::Field(f) => path.push(ProvenancePathSegment::Field(*f)),
                TemplateProjection::Deref => path.push(ProvenancePathSegment::Deref),
                TemplateProjection::Index(_) => {}
            }
        }
        base_provenance.path = self.context.bump.alloc_slice(&path);
        Some(base_provenance)
    }

    fn finalize_call_loans(
        &mut self,
        args: &[HirExpr<'a, 'bump>],
        arg_loans: Vec<LoanId>,
        ret_ty: &HirType<'a, 'bump>,
        template: Option<RefTemplate>,
    ) {
        let Some(template) = template else {
            for loan in arg_loans {
                self.borrow_checker.end_loan_now(loan);
            }
            return;
        };

        if matches!(template, RefTemplate::Path { .. }) {
            for &loan in &arg_loans {
                self.borrow_checker.end_loan_now(loan);
            }
        }

        if let Some(place) = self.resolve_template_place(&template, args) {
            let mutable = matches!(
                ret_ty,
                HirType::Ref {
                    mutability_state: MutabilityState::Mut,
                    ..
                }
            );
            let result = if mutable {
                self.borrow_checker.borrow_mut(place)
            } else {
                self.borrow_checker.borrow_shared(place)
            };

            if let Err(e) = result {
                let provenance = self.provenance_from_template(&template, args);
                let msg = self.describe_borrow_error(&e, provenance.as_ref());
                self.record(TypeErrorKind::Generic(msg));
            }
        }
    }

    fn analyze_ref_template(&mut self, func: &HirFunc<'a, 'bump>) -> RefTemplate {
        if let Some(t) = self.ref_templates.get(&func.name) {
            return t.clone();
        }

        // Cycle guard, mirrors IndexDisjointCtx::MethodAnalysisState::InProgress.
        self.ref_templates.insert(func.name, RefTemplate::Opaque);

        let template = Self::build_ref_template(func);
        self.ref_templates.insert(func.name, template.clone());
        template
    }

    fn check_return_provenance(&mut self, func: &HirFunc<'a, 'bump>) {
        let Some(HirType::Ref {
            provenance: Some(ann),
            ..
        }) = func.return_type
        else {
            return;
        };

        let template = Self::build_ref_template(func);
        let RefTemplate::Path { base_param, .. } = template else {
            self.record(TypeErrorKind::Generic(format!(
                "return type declares provenance `{}` but the body's returned reference isn't a simple projection",
                self.provenance_to_string(&ann)
            )));
            return;
        };

        let param = func.params.and_then(|p| p.get(base_param));
        let root_matches = match (ann.root, param) {
            (ProvenanceRoot::Var(name), Some(HirParam::Normal { name: pname, .. })) => {
                name == *pname
            }
            (ProvenanceRoot::ThisRoot, Some(HirParam::This { .. })) => true,
            _ => false,
        };

        if !root_matches {
            self.record(TypeErrorKind::Generic(format!(
                "declared provenance `{}` doesn't match the parameter the returned reference is actually rooted in",
                self.provenance_to_string(&ann)
            )));
        }
    }

    fn build_ref_template(func: &HirFunc<'a, 'bump>) -> RefTemplate {
        let Some(params) = func.params else {
            return RefTemplate::Opaque;
        };

        let mut param_index: FxHashMap<StrId, usize> = FxHashMap::default();
        for (i, p) in params.iter().enumerate() {
            if let HirParam::Normal { name, .. } = p {
                param_index.insert(*name, i);
            }
        }

        let Some(HirStmt::Block { body }) = func.body else {
            return RefTemplate::Opaque;
        };

        let [HirStmt::Return(Some(HirExpr::Ref { expr, mutable, .. }))] = body else {
            return RefTemplate::Opaque;
        };

        let Some((base_param, projections)) = Self::expr_to_template(expr, &param_index) else {
            return RefTemplate::Opaque;
        };

        RefTemplate::Path {
            base_param,
            mutable: *mutable,
            projections,
        }
    }

    fn expr_to_template(
        expr: &HirExpr<'a, 'bump>,
        param_index: &FxHashMap<StrId, usize>,
    ) -> Option<(usize, Vec<TemplateProjection>)> {
        match expr {
            HirExpr::Ident(name, _) => Some((*param_index.get(name)?, Vec::new())),

            HirExpr::FieldAccess { object, field, .. } | HirExpr::Get { object, field, .. } => {
                let (base, mut proj) = Self::expr_to_template(object, param_index)?;
                proj.push(TemplateProjection::Field(*field));
                Some((base, proj))
            }

            HirExpr::Deref { expr, .. } => {
                let (base, mut proj) = Self::expr_to_template(expr, param_index)?;
                proj.push(TemplateProjection::Deref);
                Some((base, proj))
            }

            HirExpr::Index { object, index, .. } => {
                let (base, mut proj) = Self::expr_to_template(object, param_index)?;
                let idx = match &**index {
                    HirExpr::Number(n, _) => IndexTemplate::Const(*n),
                    HirExpr::Ident(name, _) => param_index
                        .get(name)
                        .map(|&i| IndexTemplate::Param(i))
                        .unwrap_or(IndexTemplate::Opaque),
                    _ => IndexTemplate::Opaque,
                };
                proj.push(TemplateProjection::Index(idx));
                Some((base, proj))
            }

            _ => None,
        }
    }

    fn resolve_template_place(
        &mut self,
        template: &RefTemplate,
        args: &[HirExpr<'a, 'bump>],
    ) -> Option<PlaceId> {
        let RefTemplate::Path {
            base_param,
            projections,
            ..
        } = template
        else {
            return None;
        };

        let base_arg = args.get(*base_param)?;
        let base_expr = match base_arg {
            HirExpr::Ref { expr, .. } => expr,
            other => other,
        };

        let mut place = self.resolve_place(base_expr)?;

        for proj in projections {
            place = match proj {
                TemplateProjection::Field(f) => self.borrow_checker.project_field(place, *f),
                TemplateProjection::Deref => self.borrow_checker.project_deref(place),
                TemplateProjection::Index(idx_template) => {
                    let bound = match idx_template {
                        IndexTemplate::Const(c) => Bound::Const(*c),
                        IndexTemplate::Param(i) => self.expr_to_bound(args.get(*i)?),
                        IndexTemplate::Opaque => Bound::Opaque,
                    };
                    let interval = Interval {
                        lower: bound.clone(),
                        upper: bound,
                    };
                    self.borrow_checker
                        .project_index(place, interval, IndexContainer::Primitive)
                }
            };
        }

        Some(place)
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
                    return HirType::Unknown;
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
                HirType::Unknown
            }
            _ => {
                self.record(TypeErrorKind::Generic(format!(
                    "Cannot access field on non-struct type: {}",
                    self.type_to_string(&obj_type)
                )));
                HirType::Unknown
            }
        }
    }

    fn record_move(
        &mut self,
        root: StrId,
        field: Option<StrId>,
        field_ty: &HirType<'a, 'bump>,
        container_ty: Option<&HirType<'a, 'bump>>,
    ) {
        if self.copy_analysis.borrow().type_is_copy(field_ty) {
            return;
        }

        if let Some(&base_place) = self.borrow_checker.local_place(root) {
            let moved_place = match field {
                None => base_place,
                Some(f) => self.borrow_checker.project_field(base_place, f),
            };
            if let Err(e) = self.borrow_checker.check_move(moved_place) {
                let path = match field {
                    None => &[][..],
                    Some(f) => &[ProvenancePathSegment::Field(f)][..],
                };
                let provenance = ProvenanceAnnotation {
                    root: ProvenanceRoot::Var(root),
                    path: self.context.bump.alloc_slice(path),
                };
                let msg = self.describe_borrow_error(&e, Some(&provenance));
                self.record(TypeErrorKind::Generic(msg));
            }
        }

        match field {
            None => self.move_state.mark_whole_moved(root),
            Some(f) => {
                let blocks_partial_move = container_ty.is_some_and(|cty| match cty {
                    HirType::Struct(name, _) | HirType::Enum(name, _) => {
                        self.copy_analysis.borrow().implements_drop(*name)
                    }
                    _ => false,
                });

                if blocks_partial_move {
                    self.record(TypeErrorKind::Generic(format!(
                        "cannot partially move out of `{}`, which implements `Drop`",
                        container_ty
                            .map(|t| self.type_to_string(t))
                            .unwrap_or_default()
                    )));
                    return;
                }

                self.move_state.mark_field_moved(root, f);
            }
        }
    }

    fn check_use(&mut self, root: StrId, field: Option<StrId>, root_ty: &HirType<'a, 'bump>) {
        if self.copy_analysis.borrow().type_is_copy(root_ty) {
            return;
        }
        let name = self.str_id_to_string(root);
        match field {
            None => {
                if self.move_state.blocks_whole_use(root) {
                    self.record(TypeErrorKind::Generic(format!(
                        "use of moved value: `{}`",
                        name
                    )));
                }
            }
            Some(f) => {
                if self.move_state.is_field_moved(root, f) {
                    self.record(TypeErrorKind::Generic(format!(
                        "use of moved value: `{}.{}`",
                        name,
                        self.str_id_to_string(f)
                    )));
                }
            }
        }
    }

    fn check_and_record_value_use(&mut self, expr: &HirExpr<'a, 'bump>, ty: &HirType<'a, 'bump>) {
        if let Some(place) = self.resolve_place(expr) {
            self.check_borrow_use(expr, place, BorrowKind::Shared);
        }
        match expr {
            HirExpr::Ident(name, _) => {
                self.check_use(*name, None, ty);
                self.record_move(*name, None, ty, None);
            }
            HirExpr::FieldAccess { object, field, .. } | HirExpr::Get { object, field, .. } => {
                if let HirExpr::Ident(root, _) = &**object {
                    let root_name = self.str_id_to_string(*root);
                    let container_ty = self.context.get_variable(&root_name);
                    self.check_use(*root, Some(*field), ty);
                    self.record_move(*root, Some(*field), ty, container_ty.as_ref());
                }
            }
            _ => {}
        }
    }

    fn types_structurally_equal(&self, a: &HirType<'a, 'bump>, b: &HirType<'a, 'bump>) -> bool {
        use HirType::*;
        match (a, b) {
            (
                Ref {
                    inner: ia,
                    mutability_state: ma,
                    ..
                },
                Ref {
                    inner: ib,
                    mutability_state: mb,
                    ..
                },
            ) => {
                // Deliberately ignore provenance here, it's borrow-checker
                // bookkeeping about where a reference came from, not part of
                // the reference's type identity. Two `&mut i64` are the same
                // type regardless of which place each one happens to alias.
                ma == mb && self.types_structurally_equal(ia, ib)
            }
            (Nullable(ia), Nullable(ib)) => self.types_structurally_equal(ia, ib),
            (SafePointer(ia), SafePointer(ib)) => self.types_structurally_equal(ia, ib),
            (UnsafePointer(ia), UnsafePointer(ib)) => self.types_structurally_equal(ia, ib),
            (OwnedPointer(ia), OwnedPointer(ib)) => self.types_structurally_equal(ia, ib),
            (Array(ia, la), Array(ib, lb)) => la == lb && self.types_structurally_equal(ia, ib),
            (Slice(ia), Slice(ib)) => self.types_structurally_equal(ia, ib),
            (Tuple(ta), Tuple(tb)) => {
                ta.len() == tb.len()
                    && ta
                        .iter()
                        .zip(tb.iter())
                        .all(|(x, y)| self.types_structurally_equal(x, y))
            }
            (Dyn { bounds: ba }, Dyn { bounds: bb }) => {
                ba.len() == bb.len()
                    && ba
                        .iter()
                        .zip(bb.iter())
                        .all(|(x, y)| self.types_structurally_equal(x, y))
            }
            (
                Lambda {
                    params: pa,
                    return_type: ra,
                },
                Lambda {
                    params: pb,
                    return_type: rb,
                },
            ) => {
                pa.len() == pb.len()
                    && pa
                        .iter()
                        .zip(pb.iter())
                        .all(|(x, y)| self.types_structurally_equal(x, y))
                    && self.types_structurally_equal(ra, rb)
            }
            _ => a == b,
        }
    }

    fn types_compatible(
        &self,
        expected: &HirType<'a, 'bump>,
        found: &HirType<'a, 'bump>,
    ) -> TypeCheckResult<'a, ()> {
        if self.types_structurally_equal(expected, found) {
            return Ok(());
        }

        if *expected == HirType::Unknown || *found == HirType::Unknown {
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

    fn struct_satisfies_interface_type(
        &self,
        expected: &HirType<'a, 'bump>,
        found: &HirType<'a, 'bump>,
    ) -> bool {
        let expected_inner = Self::strip_ref(expected);
        let found_inner = Self::strip_ref(found);

        let interface_name = match expected_inner {
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
            HirType::OwnedPointer(inner) => inner,
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
            HirType::Unknown => "<unknown>".to_string(),
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
                provenance,
            } => {
                let displayed_provenance = if let Some(provenance) = provenance {
                    format!("{}", provenance)
                } else {
                    String::new()
                };
                if let MutabilityState::Mut = mutability_state {
                    format!("&{}mut {}", displayed_provenance, inner)
                } else {
                    format!("&{}{}", displayed_provenance, inner)
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
            HirType::Tuple(hir_types) => format!(
                "({})",
                hir_types
                    .iter()
                    .map(|t| self.type_to_string(t))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            HirType::Array(inner, len) => format!("[{}]{}", len, self.type_to_string(inner)),
            HirType::Slice(inner) => format!("[]{}", self.type_to_string(inner)),
            HirType::OwnedPointer(inner) => format!("^{}", self.type_to_string(inner)),
        }
    }

    fn describe_borrow_error(
        &self,
        err: &BorrowError,
        provenance: Option<&ProvenanceAnnotation>,
    ) -> String {
        let base = match err {
            BorrowError::UseAfterMove { .. } => "use of a value after it was moved".to_string(),
            BorrowError::MutablyBorrowed { .. } => {
                "cannot borrow: value is already mutably borrowed".to_string()
            }
            BorrowError::AlreadyMutablyBorrowed { .. } => {
                "cannot borrow as mutable: already mutably borrowed elsewhere".to_string()
            }
            BorrowError::Borrowed { .. } => {
                "cannot borrow as mutable: value is already borrowed".to_string()
            }
            BorrowError::InvalidMove { .. } => "invalid move".to_string(),
            BorrowError::InvalidWrite { .. } => "invalid write".to_string(),
            BorrowError::InvalidRead { .. } => "invalid read".to_string(),
            BorrowError::CannotMoveBorrowed { .. } => {
                "cannot move out of a value while it is borrowed".to_string()
            }
            BorrowError::UnknownAlias { .. } => {
                "cannot prove these two accesses don't overlap".to_string()
            }
            BorrowError::LoanNotFound(_)
            | BorrowError::PlaceNotFound(_)
            | BorrowError::ProvenanceNotFound(_) => "internal borrow-checker error".to_string(),
        };

        match provenance {
            Some(p) => format!("{} (via {})", base, self.provenance_to_string(p)),
            None => base.to_string(),
        }
    }

    fn provenance_to_string(&self, p: &ProvenanceAnnotation) -> String {
        let root = match p.root {
            ProvenanceRoot::Var(name) => self.str_id_to_string(name),
            ProvenanceRoot::ThisRoot => "this".to_string(),
        };
        p.path.iter().fold(root, |acc, seg| match seg {
            ProvenancePathSegment::Field(f) => format!("{}.{}", acc, self.str_id_to_string(*f)),
            ProvenancePathSegment::Deref => format!("*{}", acc),
        })
    }

    fn peek_type(&self, expr: &HirExpr<'a, 'bump>) -> HirType<'a, 'bump> {
        match expr {
            HirExpr::Ident(name, _) => {
                let var_name = self.str_id_to_string(*name);
                self.context
                    .get_variable(&var_name)
                    .unwrap_or(HirType::Unknown)
            }
            HirExpr::This { .. } => self.context.get_variable("this").unwrap_or(HirType::This),
            HirExpr::FieldAccess { object, field, .. } | HirExpr::Get { object, field, .. } => {
                match self.peek_type(object) {
                    HirType::Struct(struct_name, _) => {
                        let struct_name_str = self.str_id_to_string(struct_name);
                        let field_name = self.str_id_to_string(*field);
                        self.context
                            .get_struct(&struct_name_str)
                            .and_then(|s| {
                                s.fields
                                    .iter()
                                    .find(|f| self.str_id_to_string(f.name) == field_name)
                            })
                            .map(|f| f.field_type)
                            .unwrap_or(HirType::Unknown)
                    }
                    _ => HirType::Unknown,
                }
            }
            HirExpr::Deref { expr, .. } => match self.peek_type(expr) {
                HirType::Ref { inner, .. } => *inner,
                HirType::SafePointer(inner) => *inner,
                HirType::UnsafePointer(inner) => *inner,
                HirType::OwnedPointer(inner) => *inner,
                _ => HirType::Unknown,
            },
            HirExpr::Index { object, .. } => match self.peek_type(object) {
                HirType::Array(inner, _) => *inner,
                HirType::Slice(inner) => *inner,
                _ => HirType::Unknown,
            },
            _ => HirType::Unknown,
        }
    }

    fn resolve_place(&mut self, expr: &HirExpr<'a, 'bump>) -> Option<PlaceId> {
        match expr {
            HirExpr::Ident(name, _) => self.borrow_checker.local_place(*name).copied(),

            HirExpr::This { .. } => self.borrow_checker.local_place(self.this_id).copied(),

            HirExpr::FieldAccess { object, field, .. } | HirExpr::Get { object, field, .. } => {
                let base = self.resolve_place(object)?;
                let base = match self.peek_type(object) {
                    HirType::Ref { .. }
                    | HirType::SafePointer(_)
                    | HirType::UnsafePointer(_)
                    | HirType::OwnedPointer(_) => self.borrow_checker.project_deref(base),
                    _ => base,
                };
                Some(self.borrow_checker.project_field(base, *field))
            }

            HirExpr::Deref { expr, .. } => {
                let base = self.resolve_place(expr)?;
                Some(self.borrow_checker.project_deref(base))
            }

            HirExpr::Index { object, index, .. } => match self.peek_type(object) {
                HirType::Array(_, _) | HirType::Slice(_) => {
                    let base = self.resolve_place(object)?;
                    let bound = self.expr_to_bound(index);
                    let interval = Interval {
                        lower: bound.clone(),
                        upper: bound,
                    };
                    Some(self.borrow_checker.project_index(
                        base,
                        interval,
                        IndexContainer::Primitive,
                    ))
                }
                _ => None,
            },

            HirExpr::Binary {
                left,
                op: op @ (Operator::Add | Operator::Subtract),
                right,
                ..
            } => {
                if !matches!(
                    self.peek_type(left),
                    HirType::SafePointer(_) | HirType::UnsafePointer(_)
                ) {
                    return None;
                }
                let ptr_place = self.resolve_place(left)?;
                let (base, cur) = self.borrow_checker.pointee_of(ptr_place)?.clone();
                let delta = self.expr_to_bound(right);
                let signed = match op {
                    Operator::Subtract => Bound::Scale {
                        base: Box::new(delta),
                        factor: -1,
                    },
                    _ => delta,
                };
                let combined = Bound::Sum(Box::new(cur.lower.clone()), Box::new(signed));
                let interval = Interval {
                    lower: combined.clone(),
                    upper: combined,
                };
                Some(
                    self.borrow_checker
                        .project_index(base, interval, IndexContainer::Primitive),
                )
            }

            _ => None,
        }
    }

    fn expr_to_bound(&self, expr: &HirExpr<'a, 'bump>) -> Bound {
        match expr {
            HirExpr::Number(value, _) => Bound::Const(*value),
            HirExpr::Ident(name, _) => Bound::Symbol(*name),
            _ => Bound::Opaque,
        }
    }

    fn check_borrow_use(&mut self, expr: &HirExpr<'a, 'bump>, place: PlaceId, kind: BorrowKind) {
        if let Err(e) = self.borrow_checker.check_use(place, kind) {
            let provenance = self.infer_provenance(expr);
            let msg = self.describe_borrow_error(&e, provenance.as_ref());
            self.record(TypeErrorKind::Generic(msg));
        }
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use ir::registry::global_registry::GlobalRegistry;
    use zetaruntime::string_pool::StringPool;

    use super::*;

    #[test]
    fn test_type_checker_creation() {
        let dep_graph = RefCell::new(DepGraph::new());

        let bump = GrowableBump::new(4096, 8);
        let context = Arc::new(StringPool::new().unwrap());
        let registry = GlobalRegistry::new();
        let cpy_ctx = Rc::new(RefCell::new(CopyAnalysisCtx::new(
            &[],
            registry.clone(),
            context.clone(),
        )));
        let checker = TypeChecker::new(&dep_graph, &bump, cpy_ctx, context.clone());
        assert_eq!(checker.context.variables.len(), 0);
        assert!(!checker.has_errors());
    }

    #[test]
    fn test_is_numeric() {
        let dep_graph = RefCell::new(DepGraph::new());
        let bump = GrowableBump::new(4096, 8);
        let context = Arc::new(StringPool::new().unwrap());
        let registry = GlobalRegistry::new();
        let cpy_ctx = Rc::new(RefCell::new(CopyAnalysisCtx::new(
            &[],
            registry.clone(),
            context.clone(),
        )));
        let checker = TypeChecker::new(&dep_graph, &bump, cpy_ctx, context.clone());
        assert!(checker.is_numeric(&HirType::I32));
        assert!(checker.is_numeric(&HirType::F64));
        assert!(!checker.is_numeric(&HirType::Boolean));
        assert!(!checker.is_numeric(&HirType::String));
    }

    #[test]
    fn test_is_comparable() {
        let dep_graph = RefCell::new(DepGraph::new());
        let bump = GrowableBump::new(4096, 8);
        let context = Arc::new(StringPool::new().unwrap());
        let registry = GlobalRegistry::new();
        let cpy_ctx = Rc::new(RefCell::new(CopyAnalysisCtx::new(
            &[],
            registry.clone(),
            context.clone(),
        )));
        let checker = TypeChecker::new(&dep_graph, &bump, cpy_ctx, context.clone());
        assert!(checker.is_comparable(&HirType::I32));
        assert!(checker.is_comparable(&HirType::Boolean));
        assert!(checker.is_comparable(&HirType::String));
    }

    #[test]
    fn test_type_to_string() {
        let dep_graph = RefCell::new(DepGraph::new());
        let bump = GrowableBump::new(4096, 8);
        let context = Arc::new(StringPool::new().unwrap());
        let registry = GlobalRegistry::new();
        let cpy_ctx = Rc::new(RefCell::new(CopyAnalysisCtx::new(
            &[],
            registry.clone(),
            context.clone(),
        )));
        let checker = TypeChecker::new(&dep_graph, &bump, cpy_ctx, context.clone());
        assert_eq!(checker.type_to_string(&HirType::I32), "i32");
        assert_eq!(checker.type_to_string(&HirType::Boolean), "bool");
        assert_eq!(checker.type_to_string(&HirType::String), "string");
        assert_eq!(checker.type_to_string(&HirType::Void), "void");
    }

    #[test]
    fn test_types_compatible_same_type() {
        let dep_graph = RefCell::new(DepGraph::new());
        let bump = GrowableBump::new(4096, 8);
        let context = Arc::new(StringPool::new().unwrap());
        let registry = GlobalRegistry::new();
        let cpy_ctx = Rc::new(RefCell::new(CopyAnalysisCtx::new(
            &[],
            registry.clone(),
            context.clone(),
        )));
        let checker = TypeChecker::new(&dep_graph, &bump, cpy_ctx, context.clone());
        let result = checker.types_compatible(&HirType::I32, &HirType::I32);
        assert!(result.is_ok());
    }

    #[test]
    fn test_types_compatible_different_type() {
        let dep_graph = RefCell::new(DepGraph::new());
        let bump = GrowableBump::new(4096, 8);
        let context = Arc::new(StringPool::new().unwrap());
        let registry = GlobalRegistry::new();
        let cpy_ctx = Rc::new(RefCell::new(CopyAnalysisCtx::new(
            &[],
            registry.clone(),
            context.clone(),
        )));
        let checker = TypeChecker::new(&dep_graph, &bump, cpy_ctx, context.clone());
        let result = checker.types_compatible(&HirType::I32, &HirType::I64);
        assert!(result.is_err());
    }

    #[test]
    fn test_binary_op_addition() {
        let dep_graph = RefCell::new(DepGraph::new());
        let bump = GrowableBump::new(4096, 8);
        let context = Arc::new(StringPool::new().unwrap());
        let registry = GlobalRegistry::new();
        let cpy_ctx = Rc::new(RefCell::new(CopyAnalysisCtx::new(
            &[],
            registry.clone(),
            context.clone(),
        )));
        let checker = TypeChecker::new(&dep_graph, &bump, cpy_ctx, context.clone());
        let result = checker.check_binary_op(&HirType::I32, &Operator::Add, &HirType::I32);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), HirType::I32);
    }

    #[test]
    fn test_binary_op_comparison() {
        let dep_graph = RefCell::new(DepGraph::new());
        let bump = GrowableBump::new(4096, 8);
        let context = Arc::new(StringPool::new().unwrap());
        let registry = GlobalRegistry::new();
        let cpy_ctx = Rc::new(RefCell::new(CopyAnalysisCtx::new(
            &[],
            registry.clone(),
            context.clone(),
        )));
        let checker = TypeChecker::new(&dep_graph, &bump, cpy_ctx, context.clone());
        let result = checker.check_binary_op(&HirType::I32, &Operator::LessThan, &HirType::I32);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), HirType::Boolean);
    }

    #[test]
    fn test_binary_op_logical() {
        let dep_graph = RefCell::new(DepGraph::new());
        let bump = GrowableBump::new(4096, 8);
        let context = Arc::new(StringPool::new().unwrap());
        let registry = GlobalRegistry::new();
        let cpy_ctx = Rc::new(RefCell::new(CopyAnalysisCtx::new(
            &[],
            registry.clone(),
            context.clone(),
        )));
        let checker = TypeChecker::new(&dep_graph, &bump, cpy_ctx, context.clone());
        let result =
            checker.check_binary_op(&HirType::Boolean, &Operator::LogicalAnd, &HirType::Boolean);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), HirType::Boolean);
    }

    #[test]
    fn test_binary_op_type_mismatch() {
        let dep_graph = RefCell::new(DepGraph::new());
        let bump = GrowableBump::new(4096, 8);
        let context = Arc::new(StringPool::new().unwrap());
        let registry = GlobalRegistry::new();
        let cpy_ctx = Rc::new(RefCell::new(CopyAnalysisCtx::new(
            &[],
            registry.clone(),
            context.clone(),
        )));
        let checker = TypeChecker::new(&dep_graph, &bump, cpy_ctx, context.clone());
        let result = checker.check_binary_op(&HirType::I32, &Operator::Add, &HirType::String);
        assert!(result.is_err());
    }
}
