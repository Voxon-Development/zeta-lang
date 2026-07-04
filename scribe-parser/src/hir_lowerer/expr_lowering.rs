use super::context::HirLowerer;
use super::utils::lower_cmp_operator;
use ir::ast::{Expr, InlineModifier, Op, Pattern, Type, TypeKind};
use ir::hir::{
    AssignmentOperator, HirExpr, HirFunc, HirLambdaParam, HirModuleAccess, HirPattern, HirStmt,
    HirType, Operator, StrId,
};
use ir::ir_hasher::{FxHashBuilder, FxHashMap};
use ir::span::SourceSpan;
use std::collections::HashMap;

impl<'a, 'bump> HirLowerer<'a, 'bump> {
    pub(super) fn lower_expr(&self, expr: &Expr<'a, 'bump>) -> HirExpr<'a, 'bump> {
        match expr {
            Expr::Null { span } => HirExpr::Null(*span),
            Expr::Ref {
                expr,
                span,
                mutable,
            } => HirExpr::Ref {
                expr: self.ctx.bump.alloc_value(self.lower_expr(expr)),
                mutable: *mutable,
                span: *span,
            },
            Expr::Call {
                callee,
                generic_args,
                arguments,
                span,
            } => {
                if !generic_args.is_empty() {
                    if let Expr::Ident { name, .. } = callee {
                        let hir_types: Vec<HirType> =
                            generic_args.iter().map(|t| self.lower_type(t)).collect();

                        // Try to find and monomorphize the function
                        let func_opt = self.ctx.functions.borrow().get(&name).cloned();
                        if let Some(func) = func_opt {
                            let mut substitutions = FxHashMap::default();
                            if let Some(params) = func.generics {
                                for (i, param) in params.iter().enumerate() {
                                    if let Some(hir_ty) = hir_types.get(i) {
                                        substitutions.insert(param.name, *hir_ty);
                                    }
                                }
                            }

                            if let Some(mono_name) =
                                self.mono.monomorphize_function(&func, &substitutions)
                            {
                                // Call the monomorphized version
                                let args_vec: Vec<HirExpr> =
                                    arguments.iter().map(|a| self.lower_expr(a)).collect();
                                let args = self.ctx.bump.alloc_slice(&args_vec);

                                return HirExpr::Call {
                                    callee: self
                                        .ctx
                                        .bump
                                        .alloc_value(HirExpr::Ident(mono_name, *span)),
                                    args,
                                    span: *span,
                                };
                            }
                        }
                    }
                }

                self.lower_expr_call(callee, arguments, *span)
            }

            Expr::Number { value, span } => HirExpr::Number(*value, *span),
            Expr::String { value, span } => HirExpr::String(*value, *span),
            Expr::Boolean { value, span } => HirExpr::Boolean(*value, *span),

            Expr::Ident { name, span } => {
                if self.ctx.imported_modules.borrow().contains_key(&name) {
                    let access = self.ctx.bump.alloc_value_immutable(HirModuleAccess {
                        path: self.ctx.bump.alloc_slice_immutable(&[*name]),
                        member: StrId(self.ctx.context.intern("")),
                        span: *span,
                    });
                    HirExpr::ModuleAccess(access)
                } else {
                    HirExpr::Ident(*name, *span)
                }
            }

            Expr::FieldAccess {
                object,
                field,
                span,
            } => {
                let lowered_object = self.lower_expr(object);
                match lowered_object {
                    HirExpr::ModuleAccess(acc) => {
                        // `foo::bar.baz`, the field is the member on the module.
                        // If acc.member is empty this is the first `.` after the path.
                        // If acc.member is non-empty, this is a chained `.` meaning
                        // we are accessing a member of a type inside the module:
                        // `std::io.File.new`, keep extending as a field access on top.
                        if acc.member.is_empty() {
                            let new_acc = self.ctx.bump.alloc_value_immutable(HirModuleAccess {
                                path: acc.path,
                                member: *field,
                                span: *span,
                            });
                            HirExpr::ModuleAccess(new_acc)
                        } else {
                            // Further chained: `mod.Type.method`, lower as a normal
                            // FieldAccess on top of the resolved ModuleAccess.
                            HirExpr::FieldAccess {
                                object: self.ctx.bump.alloc_value_immutable(lowered_object),
                                field: *field,
                                span: *span,
                            }
                        }
                    }
                    other => HirExpr::FieldAccess {
                        object: self.ctx.bump.alloc_value_immutable(other),
                        field: *field,
                        span: *span,
                    },
                }
            }

            Expr::GenericIdent {
                name,
                generic_args,
                span,
            } => {
                let hir_types: Vec<HirType> =
                    generic_args.iter().map(|t| self.lower_type(t)).collect();

                let func_opt = self.ctx.functions.borrow().get(name).cloned();
                if let Some(func) = func_opt {
                    // Build substitutions from generic params to concrete types
                    let mut substitutions = FxHashMap::default();
                    if let Some(params) = func.generics {
                        for (i, param) in params.iter().enumerate() {
                            if let Some(hir_ty) = hir_types.get(i) {
                                substitutions.insert(param.name, *hir_ty);
                            }
                        }
                    }

                    // Monomorphize and return the new function name
                    if let Some(mono_name) = self.mono.monomorphize_function(&func, &substitutions)
                    {
                        return HirExpr::Ident(mono_name, *span);
                    }
                }

                // Fallback: return original name (will fail later if not found)
                HirExpr::Ident(*name, *span)
            }

            Expr::Decimal { value, span } => HirExpr::Decimal(*value, *span),

            Expr::Comparison { lhs, op, rhs, span } => {
                let left = self.lower_expr(lhs);
                let right = self.lower_expr(rhs);
                HirExpr::Comparison {
                    left: self.ctx.bump.alloc_value(left),
                    op: lower_cmp_operator(*op),
                    right: self.ctx.bump.alloc_value(right),
                    span: *span,
                }
            }

            Expr::StructInit {
                callee,
                arguments,
                span,
            } => {
                let name = self.lower_expr(callee);
                let args_vec: Vec<HirExpr<'a, 'bump>> =
                    arguments.iter().map(|a| self.lower_expr(a)).collect();
                let args = self.ctx.bump.alloc_slice(&args_vec);
                HirExpr::StructInit {
                    name: self.ctx.bump.alloc_value(name),
                    args,
                    span: *span,
                }
            }

            Expr::Deref { expr, span } => {
                let inner = self.lower_expr(expr);
                HirExpr::Deref {
                    expr: self.ctx.bump.alloc_value(inner),
                    span: *span,
                }
            }

            Expr::Binary {
                left,
                op,
                right,
                span,
            } => {
                let left_expr = self.lower_expr(left);
                let right_expr = self.lower_expr(right);

                // `=`/`+=`/etc. are parsed as plain `Expr::Binary` (the parser
                // has no separate assignment-expression production), but they
                // need to become `HirExpr::Assignment` so mutability checks
                // and (later) move-tracking actually see them.
                if Self::is_assignment_op(*op) {
                    HirExpr::Assignment {
                        target: self.ctx.bump.alloc_value(left_expr),
                        op: Self::lower_assignment_operator(*op),
                        value: self.ctx.bump.alloc_value(right_expr),
                        span: *span,
                    }
                } else {
                    HirExpr::Binary {
                        left: self.ctx.bump.alloc_value(left_expr),
                        op: Self::lower_op(*op),
                        right: self.ctx.bump.alloc_value(right_expr),
                        span: *span,
                    }
                }
            }

            Expr::Get {
                object,
                field,
                span,
            } => {
                let obj = self.lower_expr(object);
                HirExpr::Get {
                    object: self.ctx.bump.alloc_value(obj),
                    field: *field,
                    span: *span,
                }
            }

            Expr::Assignment { lhs, op, rhs, span } => {
                let target = self.lower_expr(lhs);
                let value = self.lower_expr(rhs);
                HirExpr::Assignment {
                    target: self.ctx.bump.alloc_value(target),
                    op: Self::lower_assignment_operator(*op),
                    value: self.ctx.bump.alloc_value(value),
                    span: *span,
                }
            }

            Expr::ExprList {
                expressions: exprs,
                span,
            } => {
                let list_vec: Vec<HirExpr<'a, 'bump>> =
                    exprs.iter().map(|e| self.lower_expr(e)).collect();
                let list = self.ctx.bump.alloc_slice(&list_vec);
                HirExpr::ExprList { list, span: *span }
            }

            Expr::Char { value, span } => {
                // TODO
                // Convert char to its numeric value
                HirExpr::Number(*value as i64, *span)
            }

            Expr::FieldInit {
                ident: _,
                expr,
                span,
            } => {
                // TODO
                // FieldInit is used in struct initialization and should be lowered to a simple expression
                // The field name is tracked separately in StructInit, so we just lower the expression
                let lowered_expr = self.lower_expr(expr);
                HirExpr::ExprList {
                    list: self.ctx.bump.alloc_slice(&[lowered_expr]),
                    span: *span,
                }
            }

            Expr::If { if_stmt: _, span } => {
                // TODO
                // Lower if statement as an expression
                // For now, convert if expression to an empty expression list
                HirExpr::ExprList {
                    list: self.ctx.bump.alloc_slice(&[]),
                    span: *span,
                }
            }

            Expr::Match {
                match_stmt: _match_stmt,
                span,
            } => {
                // TODO
                // Lower match statement as an expression
                HirExpr::ExprList {
                    list: self.ctx.bump.alloc_slice(&[]),
                    span: *span,
                }
            }

            Expr::Unary { op, operand, span } => {
                let operand_expr = self.lower_expr(operand);
                let hir_op = Self::lower_op(*op);

                // Unary operations are represented as binary operations with a placeholder left operand
                HirExpr::Binary {
                    left: self.ctx.bump.alloc_value(HirExpr::Number(0, *span)),
                    op: hir_op,
                    right: self.ctx.bump.alloc_value(operand_expr),
                    span: *span,
                }
            }

            Expr::ArrayIndex { expr, index, span } => {
                let array_expr = self.lower_expr(expr);
                let _index_expr = self.lower_expr(index);

                // TODO: Implement properly
                // Array indexing is represented as a Get operation
                HirExpr::Get {
                    object: self.ctx.bump.alloc_value(array_expr),
                    field: StrId(self.ctx.context.intern("get")),
                    span: *span,
                }
            }

            Expr::ElseExpr {
                expr,
                pattern: _,
                span: _,
            } => {
                // TODO: Implement error/nullable pattern matching lowering
                // For now, just lower the expression and ignore the pattern
                self.lower_expr(expr)
            }
            Expr::This { span } => HirExpr::This { span: *span },
            Expr::Lambda {
                modifiers,
                params,
                return_type,
                body,
                span,
            } => {
                let lowered_params: Vec<HirLambdaParam<'a, 'bump>> = params
                    .iter()
                    .map(|p| HirLambdaParam {
                        name: p.name,
                        param_type: p.type_annotation.as_ref().map(|t| self.lower_type(t)),
                    })
                    .collect();
                let params_slice = self.ctx.bump.alloc_slice_immutable(&lowered_params);

                let ret = match return_type {
                    Some(t) => self.lower_type(t),
                    None => HirType::Void,
                };
                let ret_ref = self.ctx.bump.alloc_value(ret);

                let lowered_body = self.lower_block(body);
                let body_ref = self.ctx.bump.alloc_value_immutable(lowered_body);

                HirExpr::Lambda {
                    modifier: *modifiers,
                    params: params_slice,
                    return_type: ret_ref,
                    body: body_ref,
                    span: *span,
                }
            }
            Expr::ModulePath { segments, span } => {
                let access = self.ctx.bump.alloc_value_immutable(HirModuleAccess {
                    path: self.ctx.bump.alloc_slice_immutable(segments),
                    member: StrId(self.ctx.context.intern("")),
                    span: *span,
                });
                HirExpr::ModuleAccess(access)
            }

            Expr::ModuleAccess {
                segments,
                member,
                span,
            } => {
                let access = self.ctx.bump.alloc_value_immutable(HirModuleAccess {
                    path: self.ctx.bump.alloc_slice_immutable(segments),
                    member: *member,
                    span: *span,
                });
                HirExpr::ModuleAccess(access)
            }
        }
    }

    pub(super) fn lower_expr_call(
        &self,
        callee: &Expr<'a, 'bump>,
        arguments: &'bump [Expr<'a, 'bump>],
        span: SourceSpan<'a>,
    ) -> HirExpr<'a, 'bump> {
        let lowered_callee = self.lower_expr(&*callee);
        if let Some(value) = self.detect_interface_call(arguments, &lowered_callee) {
            return value;
        }

        if let HirExpr::Ident(func_name, _) = &lowered_callee {
            if let Some(func) = self.ctx.functions.borrow().get(func_name) {
                if let InlineModifier::Inline = func.function_metadata.inline_modifier {
                    if let Some(inlined) = self.try_inline_function(func, arguments) {
                        return inlined;
                    }
                }
            }
        }

        let args_vec: Vec<HirExpr<'a, 'bump>> =
            arguments.iter().map(|a| self.lower_expr(a)).collect();
        let args = self.ctx.bump.alloc_slice(&args_vec);

        HirExpr::Call {
            callee: self.ctx.bump.alloc_value(lowered_callee),
            args,
            span,
        }
    }

    fn detect_interface_call(
        &self,
        arguments: &'bump [Expr],
        lowered_callee: &HirExpr<'a, 'bump>,
    ) -> Option<HirExpr<'a, 'bump>> {
        let HirExpr::FieldAccess {
            object,
            field,
            span,
        } = lowered_callee
        else {
            return None;
        };
        let interface = self.find_interface_method(object, *field)?;

        let args_vec: Vec<HirExpr<'a, 'bump>> =
            arguments.iter().map(|a| self.lower_expr(a)).collect();
        let args = self.ctx.bump.alloc_slice(&args_vec);

        Some(HirExpr::InterfaceCall {
            callee: self.ctx.bump.alloc_value(*lowered_callee),
            args,
            interface,
            span: *span,
        })
    }

    pub(super) fn find_interface_method(&self, object: &HirExpr, method: StrId) -> Option<StrId> {
        let class_name: StrId = match object {
            HirExpr::Ident(var, _span) => {
                if let Some(_) = self.ctx.variable_types.borrow().get(var) {
                    Some(var.clone())
                } else {
                    if self.ctx.classes.borrow().contains_key(var) {
                        Some(var.clone())
                    } else {
                        None
                    }
                }
            }
            _ => None,
        }?;

        let binding = self.ctx.classes.borrow();
        let class = binding.get(&class_name)?;
        let Some(interfaces) = class.interfaces else {
            return None;
        };

        for iface_name in interfaces {
            let if_binding = self.ctx.interfaces.borrow();
            let iface = if_binding.get(iface_name)?;
            let Some(methods) = iface.methods else {
                return None;
            };
            if methods.iter().any(|m| m.name == method) {
                return Some(iface_name.clone());
            }
        }
        None
    }

    pub(super) fn is_assignment_op(op: Op) -> bool {
        matches!(
            op,
            Op::Assign
                | Op::AddAssign
                | Op::SubAssign
                | Op::MulAssign
                | Op::DivAssign
                | Op::ModAssign
                | Op::BitAndAssign
                | Op::BitOrAssign
                | Op::BitXorAssign
                | Op::ShlAssign
                | Op::ShrAssign
        )
    }

    pub(super) fn lower_assignment_operator(op: Op) -> AssignmentOperator {
        match op {
            Op::Assign => AssignmentOperator::Assign,
            Op::AddAssign => AssignmentOperator::AddAssign,
            Op::SubAssign => AssignmentOperator::SubtractAssign,
            Op::MulAssign => AssignmentOperator::MultiplyAssign,
            Op::DivAssign => AssignmentOperator::DivideAssign,
            Op::ModAssign => AssignmentOperator::ModuloAssign,
            Op::BitAndAssign => AssignmentOperator::BitAndAssign,
            Op::BitOrAssign => AssignmentOperator::BitOrAssign,
            Op::BitXorAssign => AssignmentOperator::BitXorAssign,
            Op::ShlAssign => AssignmentOperator::ShiftLeftAssign,
            Op::ShrAssign => AssignmentOperator::ShiftRightAssign,
            _ => unreachable!(),
        }
    }

    pub(super) const fn lower_op(op: Op) -> Operator {
        match op {
            Op::AddAssign => Operator::AddAssign,
            Op::SubAssign => Operator::SubtractAssign,
            Op::MulAssign => Operator::MultiplyAssign,
            Op::DivAssign => Operator::DivideAssign,
            Op::ModAssign => Operator::ModuloAssign,
            Op::BitAndAssign => Operator::BitAndAssign,
            Op::BitOrAssign => Operator::BitOrAssign,
            Op::BitXorAssign => Operator::BitXorAssign,
            Op::ShlAssign => Operator::ShiftLeftAssign,
            Op::ShrAssign => Operator::ShiftRightAssign,
            Op::Add => Operator::Add,
            Op::Sub => Operator::Subtract,
            Op::Mul => Operator::Multiply,
            Op::Div => Operator::Divide,
            Op::Mod => Operator::Modulo,
            Op::BitAnd => Operator::BitAnd,
            Op::BitOr => Operator::BitOr,
            Op::BitXor => Operator::BitXor,
            Op::Shl => Operator::ShiftLeft,
            Op::Shr => Operator::ShiftRight,
            Op::Assign => Operator::Assign,
            Op::Eq => Operator::Equals,
            Op::Neq => Operator::NotEquals,
            Op::Gt => Operator::GreaterThan,
            Op::Lt => Operator::LessThan,
            Op::Gte => Operator::GreaterThanOrEqual,
            Op::Lte => Operator::LessThanOrEqual,
            Op::BitNot => Operator::BitNot,
            Op::LogicalNot => Operator::LogicalNot,
            Op::Range => Operator::Add, // Placeholder: Range will be handled specially
            Op::RangeExcl => Operator::Add, // Placeholder: RangeExcl will be handled specially
            Op::DerefUnsafe => Operator::DerefUnsafe,
            Op::Deref => Operator::Deref,
            Op::Ref => Operator::Ref,
            Op::RefMut => Operator::RefMut,
        }
    }

    pub fn infer_type(&self, expr: &HirExpr<'a, 'bump>) -> HirType<'a, 'bump> {
        match expr {
            HirExpr::Number(_, _) => HirType::I32,
            HirExpr::Decimal(_, _) => HirType::F64,
            HirExpr::Boolean(_, _) => HirType::Boolean,
            HirExpr::String(_, _) => HirType::String,

            HirExpr::Ident(name, _) => self
                .ctx
                .variable_types
                .borrow()
                .get(name)
                .cloned()
                .unwrap_or_else(|| panic!("unknown identifier {:?}", name)),

            HirExpr::Binary {
                left, op, right, ..
            } => {
                let lt = self.infer_type(left);
                let rt = self.infer_type(right);

                if lt != rt {
                    panic!("type mismatch: {:?} vs {:?}", lt, rt);
                }

                match op {
                    Operator::Equals
                    | Operator::NotEquals
                    | Operator::GreaterThan
                    | Operator::LessThan
                    | Operator::GreaterThanOrEqual
                    | Operator::LessThanOrEqual => HirType::Boolean,

                    _ => lt,
                }
            }

            HirExpr::Call { callee, .. } => match **callee {
                HirExpr::Ident(name, _) => {
                    let f = self.ctx.functions.borrow();
                    f.get(&name).expect("unknown function").return_type.unwrap()
                }
                _ => panic!("invalid call target"),
            },

            HirExpr::InterfaceCall {
                interface, callee, ..
            } => {
                let iface = self.ctx.interfaces.borrow();
                let iface = iface.get(interface).unwrap();

                let method = match **callee {
                    HirExpr::FieldAccess { field, .. } => field,
                    _ => unreachable!(),
                };

                iface
                    .methods
                    .unwrap()
                    .iter()
                    .find(|m| m.name == method)
                    .unwrap()
                    .return_type
                    .unwrap()
            }

            HirExpr::StructInit { name, .. } => {
                let HirExpr::Ident(n, _) = **name else {
                    unreachable!()
                };
                let field_slice: &mut [HirType<'a, 'bump>] =
                    if let Some(class) = self.ctx.classes.borrow().get(&n) {
                        let field_types: Vec<HirType<'a, 'bump>> =
                            class.fields.iter().map(|f| f.field_type).collect();
                        self.ctx.bump.alloc_slice(&field_types)
                    } else {
                        &mut []
                    };

                HirType::Struct(n, field_slice)
            }

            HirExpr::FieldAccess { object, field, .. } => {
                self.infer_field_access_type(object, *field)
            }

            HirExpr::Assignment { value, .. } => self.infer_type(value),

            HirExpr::ExprList { list, .. } => list
                .last()
                .map(|e| self.infer_type(e))
                .unwrap_or(HirType::Void),

            HirExpr::Comparison { .. } => HirType::Boolean,

            _ => panic!("infer_type not implemented for {:?}", expr),
        }
    }

    fn infer_field_access_type(
        &self,
        object: &HirExpr<'a, 'bump>,
        field: StrId,
    ) -> HirType<'a, 'bump> {
        let obj_ty = self.infer_type(object);
        match obj_ty {
            HirType::Struct(name, _) => {
                let borrow = self.ctx.classes.borrow();
                let class = borrow.get(&name).unwrap();
                class
                    .fields
                    .iter()
                    .find(|f| f.name == field)
                    .map(|f| f.field_type)
                    .unwrap()
            }
            HirType::DynInterface(_, _) | HirType::Enum(_, _) => {
                panic!("field access on interface/enum")
            }
            _ => panic!("todo"),
        }
    }

    fn try_inline_function(
        &self,
        func: &HirFunc<'a, 'bump>,
        arguments: &'bump [Expr],
    ) -> Option<HirExpr<'a, 'bump>> {
        let body = func.body?;

        let mut param_map: HashMap<StrId, HirExpr<'a, 'bump>, FxHashBuilder> =
            HashMap::with_hasher(FxHashBuilder);

        if let Some(params) = func.params {
            if params.len() != arguments.len() {
                return None;
            }

            for (param, arg) in params.iter().zip(arguments.iter()) {
                match param {
                    ir::hir::HirParam::Normal { name, .. } => {
                        let lowered_arg = self.lower_expr(arg);
                        param_map.insert(*name, lowered_arg);
                    }
                    ir::hir::HirParam::This { .. } => {
                        // Skip 'this' parameter for now
                        return None;
                    }
                }
            }
        }

        let inlined_body = self.inline_stmt_as_expr(&body, &param_map)?;
        Some(inlined_body)
    }

    fn inline_stmt_as_expr(
        &self,
        stmt: &HirStmt<'a, 'bump>,
        param_map: &HashMap<StrId, HirExpr<'a, 'bump>, FxHashBuilder>,
    ) -> Option<HirExpr<'a, 'bump>> {
        match stmt {
            HirStmt::Return(Some(expr)) => Some(self.substitute_expr(expr, param_map)),
            HirStmt::Expr(expr) => Some(self.substitute_expr(expr, param_map)),
            HirStmt::Block { body } => {
                // For blocks, try to find the last expression or return statement
                if let Some(last_stmt) = body.last() {
                    self.inline_stmt_as_expr(last_stmt, param_map)
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    /// Substitute parameter references in an expression using heap-based stack
    fn substitute_expr(
        &self,
        expr: &'a HirExpr<'a, 'bump>,
        param_map: &HashMap<StrId, HirExpr<'a, 'bump>, FxHashBuilder>,
    ) -> HirExpr<'a, 'bump> {
        // For simple cases, handle directly
        match expr {
            HirExpr::Ident(name, _) => {
                return param_map.get(name).copied().unwrap_or(*expr);
            }
            HirExpr::Number(_, _)
            | HirExpr::String(_, _)
            | HirExpr::Boolean(_, _)
            | HirExpr::Decimal(_, _) => {
                return *expr;
            }
            _ => {}
        }

        #[allow(dead_code)]
        enum WorkItem<'a, 'bump> {
            Process(&'a HirExpr<'a, 'bump>),
            BuildBinary {
                left: HirExpr<'a, 'bump>,
                op: Operator,
                right_expr: &'a HirExpr<'a, 'bump>,
                span: SourceSpan<'a>,
            },
            BuildCall {
                callee: HirExpr<'a, 'bump>,
                args: &'bump [HirExpr<'a, 'bump>],
                arg_idx: usize,
                span: SourceSpan<'a>,
            },
            BuildComparison {
                left: HirExpr<'a, 'bump>,
                op: Operator,
                right_expr: &'a HirExpr<'a, 'bump>,
                span: SourceSpan<'a>,
            },
        }

        let mut work_stack: Vec<WorkItem<'a, 'bump>> = vec![WorkItem::Process(expr)];
        let mut result_stack: Vec<HirExpr<'a, 'bump>> = Vec::new();

        while let Some(item) = work_stack.pop() {
            match item {
                WorkItem::Process(e) => match e {
                    HirExpr::Ident(name, _) => {
                        result_stack.push(param_map.get(name).copied().unwrap_or(*e));
                    }
                    HirExpr::Binary {
                        left,
                        op,
                        right,
                        span,
                    } => {
                        work_stack.push(WorkItem::BuildBinary {
                            left: HirExpr::Number(0, *span),
                            op: *op,
                            right_expr: right,
                            span: *span,
                        });
                        work_stack.push(WorkItem::Process(left));
                    }
                    HirExpr::Comparison {
                        left,
                        op,
                        right,
                        span,
                    } => {
                        work_stack.push(WorkItem::BuildComparison {
                            left: HirExpr::Number(0, *span),
                            op: *op,
                            right_expr: right,
                            span: *span,
                        });
                        work_stack.push(WorkItem::Process(left));
                    }
                    HirExpr::Call { callee, args, span } => {
                        work_stack.push(WorkItem::BuildCall {
                            callee: HirExpr::Number(0, *span),
                            args,
                            arg_idx: 0,
                            span: *span,
                        });
                        work_stack.push(WorkItem::Process(callee));
                    }
                    HirExpr::FieldAccess {
                        object,
                        field,
                        span,
                    } => {
                        work_stack.push(WorkItem::Process(object));
                        let obj_result = result_stack.pop().unwrap_or(*e);
                        result_stack.push(HirExpr::FieldAccess {
                            object: self.ctx.bump.alloc_value(obj_result),
                            field: *field,
                            span: *span,
                        });
                    }
                    HirExpr::Assignment {
                        target,
                        op,
                        value,
                        span,
                    } => {
                        let new_target = self.substitute_expr(target, param_map);
                        let new_value = self.substitute_expr(value, param_map);
                        result_stack.push(HirExpr::Assignment {
                            target: self.ctx.bump.alloc_value(new_target),
                            op: *op,
                            value: self.ctx.bump.alloc_value(new_value),
                            span: *span,
                        });
                    }
                    _ => result_stack.push(*e),
                },
                WorkItem::BuildBinary {
                    left: _,
                    op,
                    right_expr,
                    span,
                } => {
                    let left_result = result_stack.pop().unwrap();
                    let right_result = self.substitute_expr(right_expr, param_map);
                    result_stack.push(HirExpr::Binary {
                        left: self.ctx.bump.alloc_value(left_result),
                        op,
                        right: self.ctx.bump.alloc_value(right_result),
                        span,
                    });
                }
                WorkItem::BuildComparison {
                    left: _,
                    op,
                    right_expr,
                    span,
                } => {
                    let left_result = result_stack.pop().unwrap();
                    let right_result = self.substitute_expr(right_expr, param_map);
                    result_stack.push(HirExpr::Comparison {
                        left: self.ctx.bump.alloc_value(left_result),
                        op,
                        right: self.ctx.bump.alloc_value(right_result),
                        span,
                    });
                }
                WorkItem::BuildCall {
                    callee: _,
                    args,
                    arg_idx: _arg_idx,
                    span,
                } => {
                    let callee_result = result_stack.pop().unwrap();
                    let new_args_vec: Vec<HirExpr<'a, 'bump>> = args
                        .iter()
                        .map(|a| self.substitute_expr(a, param_map))
                        .collect();
                    let new_args = self.ctx.bump.alloc_slice(&new_args_vec);
                    result_stack.push(HirExpr::Call {
                        callee: self.ctx.bump.alloc_value(callee_result),
                        args: new_args,
                        span,
                    });
                }
            }
        }

        result_stack.pop().unwrap_or(*expr)
    }

    pub(super) fn lower_pattern(&self, pattern: &Pattern) -> HirPattern<'bump> {
        match pattern {
            Pattern::Ident(name) => HirPattern::Ident(*name),
            Pattern::Number(n) => HirPattern::Number(*n),
            Pattern::String(s) => HirPattern::String(*s),
            Pattern::Tuple(inner) => {
                let tuple_vec: Vec<HirPattern<'bump>> =
                    inner.iter().map(|p| self.lower_pattern(p)).collect();
                let tuple_slice = self.ctx.bump.alloc_slice(&tuple_vec);
                HirPattern::Tuple(tuple_slice)
            }
            Pattern::Wildcard => HirPattern::Wildcard,
            Pattern::Boolean(_) => todo!(),
            Pattern::Array(_) => todo!(),
            Pattern::Struct { .. } => todo!(),
            Pattern::Or(_) => todo!(),
            Pattern::EnumVariant { name, bindings } => {
                // Extract identifier bindings from sub-patterns
                let binding_ids: Vec<ir::hir::StrId> = bindings
                    .iter()
                    .filter_map(|p| {
                        if let Pattern::Ident(id) = p {
                            Some(*id)
                        } else {
                            None
                        }
                    })
                    .collect();
                let bindings_slice = self.ctx.bump.alloc_slice(&binding_ids);
                HirPattern::EnumVariant {
                    enum_name: *name,
                    variant: *name,
                    bindings: bindings_slice,
                }
            }
        }
    }

    pub(super) fn lower_type(&self, t: &Type) -> HirType<'a, 'bump> {
        let ty = self.lower_type_inner(t);
        if t.nullable {
            HirType::Nullable(self.ctx.bump.alloc_value(ty))
        } else {
            ty
        }
    }

    pub(super) fn lower_type_inner(&self, t: &Type) -> HirType<'a, 'bump> {
        match &t.kind {
            TypeKind::I8 => HirType::I8,
            TypeKind::I16 => HirType::I16,
            TypeKind::I32 => HirType::I32,
            TypeKind::I64 => HirType::I64,

            TypeKind::U8 => HirType::U8,
            TypeKind::U16 => HirType::U16,
            TypeKind::U32 => HirType::U32,
            TypeKind::U64 => HirType::U64,

            TypeKind::I128 => HirType::I128,
            TypeKind::U128 => HirType::U128,

            TypeKind::F32 => HirType::F32,
            TypeKind::F64 => HirType::F64,

            TypeKind::String => HirType::String,
            TypeKind::Boolean => HirType::Boolean,
            TypeKind::Void => HirType::Void,

            TypeKind::This => HirType::This,

            TypeKind::Struct { name, generics } => {
                let binding = self.ctx.classes.borrow();
                if let Some(class) = binding.get(name) {
                    if generics.is_empty() {
                        // Non-generic struct: use real field types.
                        let field_types: Vec<HirType<'a, 'bump>> =
                            class.fields.iter().map(|f| f.field_type).collect();
                        let field_slice = self.ctx.bump.alloc_slice_immutable(&field_types);
                        return HirType::Struct(*name, field_slice);
                    }
                }
                HirType::Struct(*name, &[])
            }

            TypeKind::SafePointer { inner, .. } => {
                let inner = self.ctx.bump.alloc_value(self.lower_type(inner));
                HirType::SafePointer(inner)
            }

            TypeKind::UnsafePointer { inner, .. } => {
                let inner = self.ctx.bump.alloc_value(self.lower_type(inner));
                HirType::UnsafePointer(inner)
            }

            TypeKind::Ref {
                inner,
                mutability_state,
            } => {
                let inner = self.ctx.bump.alloc_value(self.lower_type(inner));
                HirType::Ref {
                    inner,
                    mutability_state: *mutability_state,
                }
            }

            TypeKind::Lambda {
                params,
                return_type,
            } => {
                let lowered_params: Vec<HirType<'a, 'bump>> =
                    params.iter().map(|p| self.lower_type(p)).collect();

                let params_slice = self.ctx.bump.alloc_slice_immutable(&lowered_params);

                let ret = self.ctx.bump.alloc_value(self.lower_type(return_type));

                HirType::Lambda {
                    params: params_slice,
                    return_type: ret,
                }
            }

            TypeKind::Infer => {
                panic!("Infer type reached HIR lowering")
            }

            TypeKind::Array { .. } => {
                panic!("Array types not yet represented in HIR")
            }

            TypeKind::Slice { .. } => {
                panic!("Slice types not yet represented in HIR")
            }

            TypeKind::Char => HirType::Char,

            TypeKind::UF32 => {
                panic!("UF32 type not yet represented in HIR")
            }

            TypeKind::UF64 => {
                panic!("UF64 type not yet represented in HIR")
            }
            TypeKind::Dyn { bounds } => HirType::Dyn {
                bounds: self.ctx.bump.alloc_slice(
                    bounds
                        .iter()
                        .map(|p| self.lower_type(p))
                        .collect::<Vec<_>>()
                        .as_slice(),
                ),
            },
        }
    }
}
