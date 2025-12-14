use crate::type_context::TypeContext;
use crate::type_error::{TypeError, TypeCheckResult};
use ir::hir::{
    HirModule, HirFunc, HirInterface, HirExpr, HirStmt, HirType, HirParam, Hir,
    Operator, StrId,
};

pub struct TypeChecker<'a, 'bump> {
    context: TypeContext<'a, 'bump>,
}

impl<'a, 'bump> TypeChecker<'a, 'bump> {
    pub fn new() -> Self {
        Self {
            context: TypeContext::new(),
        }
    }

    /// Check a complete HIR module for type correctness
    pub fn check_module(&mut self, module: &HirModule<'a, 'bump>) -> TypeCheckResult<()> {
        // First pass: register all top-level definitions
        for item in module.items {
            match item {
                Hir::Struct(s) => {
                    let name = self.str_id_to_string(s.name);
                    self.context.add_struct(name, **s);
                }
                Hir::Interface(i) => {
                    let name = self.str_id_to_string(i.name);
                    self.context.add_interface(name, **i);
                }
                Hir::Func(f) => {
                    let name = self.str_id_to_string(f.name);
                    self.context.add_function(name, **f);
                }
                _ => {}
            }
        }

        // Second pass: type check all functions
        for item in module.items {
            if let Hir::Func(func) = item {
                self.check_function(func)?;
            }
        }

        Ok(())
    }

    fn check_function(&mut self, func: &HirFunc<'a, 'bump>) -> TypeCheckResult<()> {
        let mut func_context = self.context.create_child_scope();

        // Add parameters to context
        if let Some(params) = func.params {
            for param in params {
                match param {
                    HirParam::Normal { name, param_type } => {
                        let param_name = self.str_id_to_string(*name);
                        func_context.add_variable(param_name, *param_type);
                    }
                    HirParam::This { param_type } => {
                        if let Some(ty) = param_type {
                            func_context.add_variable("this".to_string(), *ty);
                        }
                    }
                }
            }
        }

        // Set return type for this function
        func_context.current_return_type = func.return_type;

        // Check function body
        if let Some(body) = func.body {
            let old_context = std::mem::replace(&mut self.context, func_context);
            self.check_stmt(&body)?;
            self.context = old_context;
        }

        Ok(())
    }

    fn check_stmt(&mut self, stmt: &HirStmt<'a, 'bump>) -> TypeCheckResult<Option<HirType<'a, 'bump>>> {
        match stmt {
            HirStmt::Let { name, ty, value } => {
                let value_type = self.check_expr(value)?;
                self.types_compatible(ty, &value_type)?;
                let var_name = self.str_id_to_string(*name);
                self.context.add_variable(var_name, *ty);
                Ok(None)
            }
            HirStmt::Return(expr) => {
                if let Some(e) = expr {
                    let expr_type = self.check_expr(e)?;
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
            HirStmt::If { cond, then_block, else_block } => {
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
                    let mut else_context = self.context.create_child_scope();
                    let old_context = std::mem::replace(&mut self.context, else_context);
                    self.check_stmt(else_stmt)?;
                    else_context = self.context.clone();
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
            HirStmt::For { init, condition, increment, body } => {
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
            HirStmt::Break => {
                if !self.context.in_loop {
                    return Err(TypeError::BreakOutsideLoop);
                }
                Ok(None)
            }
            HirStmt::Continue => {
                if !self.context.in_loop {
                    return Err(TypeError::ContinueOutsideLoop);
                }
                Ok(None)
            }
            _ => Ok(None),
        }
    }

    fn check_expr(&mut self, expr: &HirExpr<'a, 'bump>) -> TypeCheckResult<HirType<'a, 'bump>> {
        match expr {
            HirExpr::Number(_) => Ok(HirType::I64),
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
                // For now, return the first type as a simplification
                Ok(types.first().copied().unwrap_or(HirType::Void))
            }
            HirExpr::Binary { left, op, right, .. } => {
                let left_type = self.check_expr(left)?;
                let right_type = self.check_expr(right)?;
                self.check_binary_op(&left_type, op, &right_type)
            }
            HirExpr::Call { callee, args } => {
                if let HirExpr::Ident(func_name) = **callee {
                    let name = self.str_id_to_string(func_name);
                    let func = self.context
                        .get_function(&name)
                        .ok_or_else(|| TypeError::UndefinedFunction(name))?;

                    // Check argument count
                    let expected_args = func.params.map(|p| p.len()).unwrap_or(0);
                    if args.len() != expected_args {
                        return Err(TypeError::InvalidFunctionCall {
                            expected_args,
                            found_args: args.len(),
                        });
                    }

                    // Check argument types
                    if let Some(params) = func.params {
                        for (arg, param) in args.iter().zip(params.iter()) {
                            let arg_type = self.check_expr(arg)?;
                            if let Some(param_type) = param.get_type() {
                                self.types_compatible(param_type, &arg_type)?;
                            }
                        }
                    }

                    Ok(func.return_type.unwrap_or(HirType::Void))
                } else {
                    // For now, assume dynamic calls return Void
                    Ok(HirType::Void)
                }
            }
            HirExpr::FieldAccess { object, field, span: _ } => {
                let obj_type = self.check_expr(object)?;
                match obj_type {
                    HirType::Struct(struct_name, _) => {
                        let struct_name_str = self.str_id_to_string(struct_name);
                        let struct_def = self.context
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
            _ => Ok(HirType::Void),
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
            Ok(())
        } else {
            Err(TypeError::TypeMismatch {
                expected: self.type_to_string(expected),
                found: self.type_to_string(found),
            })
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
            HirType::Interface(name, _) => format!("interface {}", self.str_id_to_string(*name)),
            HirType::Enum(name, _) => format!("enum {}", self.str_id_to_string(*name)),
            HirType::Generic(name) => format!("generic {}", self.str_id_to_string(*name)),
            HirType::Pointer(_) => "pointer".to_string(),
            HirType::Lambda { .. } => "lambda".to_string(),
            HirType::This => "this".to_string(),
            HirType::Null => "null".to_string(),
        }
    }
}

impl<'a, 'bump> Default for TypeChecker<'a, 'bump> {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_type_checker_creation() {
        let checker = TypeChecker::new();
        assert_eq!(checker.context.variables.len(), 0);
    }

    #[test]
    fn test_is_numeric() {
        let checker = TypeChecker::new();
        assert!(checker.is_numeric(&HirType::I32));
        assert!(checker.is_numeric(&HirType::F64));
        assert!(!checker.is_numeric(&HirType::Boolean));
        assert!(!checker.is_numeric(&HirType::String));
    }

    #[test]
    fn test_is_comparable() {
        let checker = TypeChecker::new();
        assert!(checker.is_comparable(&HirType::I32));
        assert!(checker.is_comparable(&HirType::Boolean));
        assert!(checker.is_comparable(&HirType::String));
    }

    #[test]
    fn test_type_to_string() {
        let checker = TypeChecker::new();
        assert_eq!(checker.type_to_string(&HirType::I32), "i32");
        assert_eq!(checker.type_to_string(&HirType::Boolean), "bool");
        assert_eq!(checker.type_to_string(&HirType::String), "string");
        assert_eq!(checker.type_to_string(&HirType::Void), "void");
    }

    #[test]
    fn test_types_compatible_same_type() {
        let checker = TypeChecker::new();
        let result = checker.types_compatible(&HirType::I32, &HirType::I32);
        assert!(result.is_ok());
    }

    #[test]
    fn test_types_compatible_different_type() {
        let checker = TypeChecker::new();
        let result = checker.types_compatible(&HirType::I32, &HirType::I64);
        assert!(result.is_err());
    }

    #[test]
    fn test_binary_op_addition() {
        let checker = TypeChecker::new();
        let result = checker.check_binary_op(&HirType::I32, &Operator::Add, &HirType::I32);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), HirType::I32);
    }

    #[test]
    fn test_binary_op_comparison() {
        let checker = TypeChecker::new();
        let result = checker.check_binary_op(&HirType::I32, &Operator::LessThan, &HirType::I32);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), HirType::Boolean);
    }

    #[test]
    fn test_binary_op_logical() {
        let checker = TypeChecker::new();
        let result = checker.check_binary_op(&HirType::Boolean, &Operator::LogicalAnd, &HirType::Boolean);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), HirType::Boolean);
    }

    #[test]
    fn test_binary_op_type_mismatch() {
        let checker = TypeChecker::new();
        let result = checker.check_binary_op(&HirType::I32, &Operator::Add, &HirType::String);
        assert!(result.is_err());
    }
}
