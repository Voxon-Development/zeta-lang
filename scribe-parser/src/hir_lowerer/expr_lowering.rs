use super::context::HirLowerer;
use super::utils::lower_cmp_operator;
use ir::ast::{Expr, Op, Pattern, Type};
use ir::hir::{AssignmentOperator, HirExpr, HirFunc, HirPattern, HirStmt, HirType, Operator, StrId};
use ir::ir_hasher::FxHashBuilder;
use ir::span::SourceSpan;
use std::collections::HashMap;

impl<'a, 'bump> HirLowerer<'a, 'bump> {
    // ===============================
    // Expression Lowering
    // ===============================
    pub(super) fn lower_expr(&self, expr: &Expr<'a, 'bump>) -> HirExpr<'a, 'bump> {
        match expr {
            Expr::Call { callee, arguments, .. } => self.lower_expr_call(callee, arguments),
            Expr::FieldAccess { object, field, span } => {
                let obj = self.lower_expr(object);
                HirExpr::FieldAccess {
                    object: self.ctx.bump.alloc_value(obj),
                    field: *field,
                    span: *span,
                }
            }

            Expr::Number { value, .. } => HirExpr::Number(*value),
            Expr::String { value, .. } => HirExpr::String(*value),
            Expr::Boolean { value, .. } => HirExpr::Boolean(*value),
            Expr::Ident { name, .. } => HirExpr::Ident(*name),

            Expr::Decimal { value, .. } => HirExpr::Decimal(*value),

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

            Expr::StructInit { callee, arguments, positional: _, span } => {
                let name = self.lower_expr(callee);
                let args_vec: Vec<HirExpr<'a, 'bump>> = arguments.iter().map(|a| self.lower_expr(a)).collect();
                let args = self.ctx.bump.alloc_slice(&args_vec);
                HirExpr::StructInit {
                    name: self.ctx.bump.alloc_value(name),
                    args,
                    span: *span,
                }
            }

            Expr::Binary { left, op, right, span } => {
                let left_expr = self.lower_expr(left);
                let right_expr = self.lower_expr(right);
                HirExpr::Binary {
                    left: self.ctx.bump.alloc_value(left_expr),
                    op: Self::lower_op(*op),
                    right: self.ctx.bump.alloc_value(right_expr),
                    span: *span,
                }
            }

            Expr::Get { object, field, span } => {
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

            Expr::ExprList { expressions: exprs, span } => {
                let list_vec: Vec<HirExpr<'a, 'bump>> = exprs.iter().map(|e| self.lower_expr(e)).collect();
                let list = self.ctx.bump.alloc_slice(&list_vec);
                HirExpr::ExprList { list, span: *span }
            }

            Expr::Char { value, .. } => {
                // Convert char to its numeric value
                HirExpr::Number(*value as i64)
            }

            Expr::FieldInit { ident, expr, span } => {
                // FieldInit is used in struct initialization and should be lowered to a simple expression
                // The field name is tracked separately in RecordInit, so we just lower the expression
                let lowered_expr = self.lower_expr(expr);
                HirExpr::ExprList {
                    list: self.ctx.bump.alloc_slice(&[lowered_expr]),
                    span: *span,
                }
            }

            Expr::If { if_stmt, span } => {
                // Lower if statement as an expression
                // For now, convert if expression to an empty expression list
                // Proper if-expression support would need HIR changes to support control flow as expressions
                HirExpr::ExprList {
                    list: self.ctx.bump.alloc_slice(&[]),
                    span: *span,
                }
            }

            Expr::Match { match_stmt, span } => {
                // Lower match statement as an expression
                // This is a placeholder - proper match-expression support would need HIR changes
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
                    left: self.ctx.bump.alloc_value(HirExpr::Number(0)),
                    op: hir_op,
                    right: self.ctx.bump.alloc_value(operand_expr),
                    span: *span,
                }
            }

            Expr::ArrayIndex { expr, index, span } => {
                let array_expr = self.lower_expr(expr);
                let index_expr = self.lower_expr(index);
                
                // Array indexing is represented as a Get operation
                // This is a simplification - proper array support would need HIR changes
                HirExpr::Get {
                    object: self.ctx.bump.alloc_value(array_expr),
                    field: StrId(self.ctx.context.intern("index")),
                    span: *span,
                }
            }
            Expr::ElseExpr { expr, pattern, span } => {
                // TODO: Implement error/nullable pattern matching lowering
                // For now, just lower the expression and ignore the pattern
                self.lower_expr(expr)
            }
        }
    }

    pub(super) fn lower_expr_call(&self, callee: &Expr<'a, 'bump>, arguments: &'bump [Expr<'a, 'bump>]) -> HirExpr<'a, 'bump> {
        let lowered_callee = self.lower_expr(&*callee);
        if let Some(value) = self.detect_interface_call(arguments, &lowered_callee) {
            return value;
        }

        if let HirExpr::Ident(func_name) = &lowered_callee {
            if let Some(func) = self.ctx.functions.borrow().get(func_name) {
                if func.inline {
                    if let Some(inlined) = self.try_inline_function(func, arguments) {
                        return inlined;
                    }
                }
            }
        }

        let args_vec: Vec<HirExpr<'a, 'bump>> = arguments.iter().map(|a| self.lower_expr(a)).collect();
        let args = self.ctx.bump.alloc_slice(&args_vec);
        
        HirExpr::Call {
            callee: self.ctx.bump.alloc_value(lowered_callee),
            args,
        }
    }

    fn detect_interface_call(
        &self,
        arguments: &'bump [Expr],
        lowered_callee: &HirExpr<'a, 'bump>,
    ) -> Option<HirExpr<'a, 'bump>> {
        let HirExpr::FieldAccess { object, field, span: _span } = lowered_callee else {
            return None;
        };
        let interface = self.find_interface_method(object, *field)?;

        let args_vec: Vec<HirExpr<'a, 'bump>> = arguments.iter().map(|a| self.lower_expr(a)).collect();
        let args = self.ctx.bump.alloc_slice(&args_vec);

        Some(HirExpr::InterfaceCall {
            callee: self.ctx.bump.alloc_value(*lowered_callee),
            args,
            interface,
        })
    }

    pub(super) fn find_interface_method(&self, object: &HirExpr, method: StrId) -> Option<StrId> {
        let class_name: StrId = match object {
            HirExpr::Ident(var) => {
                if let Some(_) = self.ctx.variable_types.borrow().get(var) {
                    Some(var.clone())
                } else {
                    if self.ctx.classes.borrow().contains_key(var) {
                        Some(var.clone())
                    } else {
                        None
                    }
                }
            },
            _ => None,
        }?;

        let binding = self.ctx.classes.borrow();
        let class = binding.get(&class_name)?;
        let Some(interfaces) = class.interfaces else { return None; };

        for iface_name in interfaces {
            let if_binding = self.ctx.interfaces.borrow();
            let iface = if_binding.get(iface_name)?;
            let Some(methods) = iface.methods else { return None; };
            if methods.iter().any(|m| m.name == method) {
                return Some(iface_name.clone());
            }
        }
        None
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
            Op::ShrAssign => AssignmentOperator::ShiftLeftAssign,
            Op::ShlAssign => AssignmentOperator::ShiftRightAssign,
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
            Op::Range => Operator::Add,  // Placeholder: Range will be handled specially
            Op::RangeExcl => Operator::Add,  // Placeholder: RangeExcl will be handled specially
        }
    }

    // ===============================
    // Type Inference
    // ===============================
    pub fn infer_type(&self, value: &HirExpr<'a, 'bump>) -> HirType<'a, 'bump> {
        match value {
            HirExpr::String(_) => HirType::String,
            HirExpr::Number(_) => HirType::I32,
            HirExpr::Boolean(_) => HirType::Boolean,
            HirExpr::Decimal(_) => HirType::F64,
            HirExpr::StructInit { name, .. } => match **name {
                HirExpr::Ident(n) => HirType::Struct(n, &[]),
                _ => todo!(),
            },
            HirExpr::Binary { left, op: _, right, .. } => {
                let lt = self.infer_type(left);
                let rt = self.infer_type(right);
                if lt == rt { lt } else { panic!("lt != rt") }
            }
            HirExpr::FieldAccess { object, field, span: _ } => self.infer_field_access_type(object, *field),
            _ => todo!(),
        }
    }

    fn infer_field_access_type(&self, object: &HirExpr<'a, 'bump>, field: StrId) -> HirType<'a, 'bump> {
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
            HirType::Interface(_, _) | HirType::Enum(_, _) => panic!("field access on interface/enum"),
            _ => panic!("todo"),
        }
    }

    fn try_inline_function(&self, func: &HirFunc<'a, 'bump>, arguments: &'bump [Expr]) -> Option<HirExpr<'a, 'bump>> {
        let body = func.body?;
        
        let mut param_map: HashMap<StrId, HirExpr<'a, 'bump>, FxHashBuilder> = HashMap::with_hasher(FxHashBuilder);
        
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
    
    fn inline_stmt_as_expr(&self, stmt: &HirStmt<'a, 'bump>, param_map: &HashMap<StrId, HirExpr<'a, 'bump>, FxHashBuilder>) -> Option<HirExpr<'a, 'bump>> {
        match stmt {
            HirStmt::Return(Some(expr)) => {
                Some(self.substitute_expr(expr, param_map))
            }
            HirStmt::Expr(expr) => {
                Some(self.substitute_expr(expr, param_map))
            }
            HirStmt::Block { body } => {
                // For blocks, try to find the last expression or return statement
                if let Some(last_stmt) = body.last() {
                    self.inline_stmt_as_expr(last_stmt, param_map)
                } else {
                    None
                }
            }
            _ => None, // Cannot inline complex control flow yet
        }
    }
    
    /// Substitute parameter references in an expression using heap-based stack
    fn substitute_expr(&self, expr: &'a HirExpr<'a, 'bump>, param_map: &HashMap<StrId, HirExpr<'a, 'bump>, FxHashBuilder>) -> HirExpr<'a, 'bump> {
        // For simple cases, handle directly
        match expr {
            HirExpr::Ident(name) => {
                return param_map.get(name).copied().unwrap_or(*expr);
            }
            HirExpr::Number(_) | HirExpr::String(_) | HirExpr::Boolean(_) | HirExpr::Decimal(_) => {
                return *expr;
            }
            _ => {}
        }

        // For complex expressions, use a work stack
        enum WorkItem<'a, 'bump> {
            Process(&'a HirExpr<'a, 'bump>),
            BuildBinary { left: HirExpr<'a, 'bump>, op: Operator, right_expr: &'a HirExpr<'a, 'bump>, span: SourceSpan<'a> },
            BuildCall { callee: HirExpr<'a, 'bump>, args: &'bump [HirExpr<'a, 'bump>], arg_idx: usize },
            BuildComparison { left: HirExpr<'a, 'bump>, op: Operator, right_expr: &'a HirExpr<'a, 'bump>, span: SourceSpan<'a> },
        }

        let mut work_stack: Vec<WorkItem<'a, 'bump>> = vec![WorkItem::Process(expr)];
        let mut result_stack: Vec<HirExpr<'a, 'bump>> = Vec::new();

        while let Some(item) = work_stack.pop() {
            match item {
                WorkItem::Process(e) => {
                    match e {
                        HirExpr::Ident(name) => {
                            result_stack.push(param_map.get(name).copied().unwrap_or(*e));
                        }
                        HirExpr::Binary { left, op, right, span } => {
                            work_stack.push(WorkItem::BuildBinary { left: HirExpr::Number(0), op: *op, right_expr: right, span: *span });
                            work_stack.push(WorkItem::Process(left));
                        }
                        HirExpr::Comparison { left, op, right, span } => {
                            work_stack.push(WorkItem::BuildComparison { left: HirExpr::Number(0), op: *op, right_expr: right, span: *span });
                            work_stack.push(WorkItem::Process(left));
                        }
                        HirExpr::Call { callee, args } => {
                            work_stack.push(WorkItem::BuildCall { callee: HirExpr::Number(0), args, arg_idx: 0 });
                            work_stack.push(WorkItem::Process(callee));
                        }
                        HirExpr::FieldAccess { object, field, span } => {
                            work_stack.push(WorkItem::Process(object));
                            let obj_result = result_stack.pop().unwrap_or(*e);
                            result_stack.push(HirExpr::FieldAccess {
                                object: self.ctx.bump.alloc_value(obj_result),
                                field: *field,
                                span: *span,
                            });
                        }
                        HirExpr::Assignment { target, op, value, span } => {
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
                    }
                }
                WorkItem::BuildBinary { left: _, op, right_expr, span } => {
                    let left_result = result_stack.pop().unwrap();
                    let right_result = self.substitute_expr(right_expr, param_map);
                    result_stack.push(HirExpr::Binary {
                        left: self.ctx.bump.alloc_value(left_result),
                        op,
                        right: self.ctx.bump.alloc_value(right_result),
                        span,
                    });
                }
                WorkItem::BuildComparison { left: _, op, right_expr, span } => {
                    let left_result = result_stack.pop().unwrap();
                    let right_result = self.substitute_expr(right_expr, param_map);
                    result_stack.push(HirExpr::Comparison {
                        left: self.ctx.bump.alloc_value(left_result),
                        op,
                        right: self.ctx.bump.alloc_value(right_result),
                        span,
                    });
                }
                WorkItem::BuildCall { callee: _, args, arg_idx: _arg_idx } => {
                    let callee_result = result_stack.pop().unwrap();
                    let new_args_vec: Vec<HirExpr<'a, 'bump>> = args.iter()
                        .map(|a| self.substitute_expr(a, param_map))
                        .collect();
                    let new_args = self.ctx.bump.alloc_slice(&new_args_vec);
                    result_stack.push(HirExpr::Call {
                        callee: self.ctx.bump.alloc_value(callee_result),
                        args: new_args,
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
                let tuple_vec: Vec<HirPattern<'bump>> = inner.iter().map(|p| self.lower_pattern(p)).collect();
                let tuple_slice = self.ctx.bump.alloc_slice(&tuple_vec);
                HirPattern::Tuple(tuple_slice)
            }
            Pattern::Wildcard => HirPattern::Wildcard,
            Pattern::Boolean(_) => todo!(),
            Pattern::Array(_) => todo!(),
            Pattern::Struct { .. } => todo!(),
            Pattern::Or(_) => todo!()
        }
    }

    // ===============================
    // Type Lowering
    // ===============================
    pub(super) fn lower_type(&self, t: &Type) -> HirType<'a, 'bump> {
        match &t.kind {
            ir::ast::TypeKind::I32 => HirType::I32,
            ir::ast::TypeKind::I64 => HirType::I64,
            ir::ast::TypeKind::F64 => HirType::F64,
            ir::ast::TypeKind::String => HirType::String,
            ir::ast::TypeKind::Boolean => HirType::Boolean,
            ir::ast::TypeKind::Struct { name, generics } => {
                let generics_vec: Vec<HirType<'a, 'bump>> = generics.iter().map(|g| self.lower_type(g)).collect();
                let generics_slice = self.ctx.bump.alloc_slice(&generics_vec);
                HirType::Struct(*name, generics_slice)
            }
            ir::ast::TypeKind::Void => HirType::Void,
            ir::ast::TypeKind::U32 => HirType::U32,
            ir::ast::TypeKind::F32 => HirType::F32,
            ir::ast::TypeKind::U8 => HirType::U8,
            ir::ast::TypeKind::I8 => HirType::I8,
            ir::ast::TypeKind::U16 => HirType::U16,
            ir::ast::TypeKind::I16 => HirType::I16,
            ir::ast::TypeKind::U64 => HirType::U64,
            ir::ast::TypeKind::I128 => HirType::I128,
            ir::ast::TypeKind::U128 => HirType::U128,
            _ => HirType::Void, // TODO: handle all
        }
    }
}
