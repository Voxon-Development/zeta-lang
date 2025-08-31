use std::collections::HashMap;
use std::hash::BuildHasher;
use seahash::SeaHasher;
use ir::ast;
use ir::ast::{ClassDecl, ComparisonOp, ElseBranch, EnumDecl, Expr, FuncDecl, ImplDecl, InterfaceDecl, LetStmt, Op, Pattern, Stmt, Type};
use ir::hir;
use ir::hir::{AssignmentOperator, Hir, HirClass, HirEnum, HirEnumVariant, HirExpr, HirField, HirFunc, HirGeneric, HirImpl, HirInterface, HirMatchArm, HirModule, HirParam, HirPattern, HirStmt, HirType, Operator};
use ir::errors::reporter::ErrorReporter;

// ===============================
// Lowering Context
// ===============================

#[derive(Copy, Clone)]
pub struct SeaHashBuilder {
    hasher: SeaHasher,
}

impl SeaHashBuilder {
    pub fn new() -> Self { Self { hasher: SeaHasher::new() } }
}

impl BuildHasher for SeaHashBuilder {
    type Hasher = SeaHasher;
    fn build_hasher(&self) -> Self::Hasher { self.hasher }
}

pub struct LoweringCtx {
    pub classes: HashMap<String, HirClass, SeaHashBuilder>,
    pub interfaces: HashMap<String, HirInterface, SeaHashBuilder>,
    pub functions: HashMap<String, HirFunc, SeaHashBuilder>,
    pub type_bindings: HashMap<String, HirType, SeaHashBuilder>,
    pub variable_types: HashMap<String, HirType, SeaHashBuilder>,
}

pub struct HirLowerer {
    pub ctx: LoweringCtx,
    pub error_reporter: ErrorReporter
}

impl HirLowerer {
    pub fn new() -> Self {
        let sea_hasher = SeaHashBuilder::new();
        Self {
            ctx: LoweringCtx {
                classes: HashMap::with_hasher(sea_hasher),
                functions: HashMap::with_hasher(sea_hasher),
                interfaces: HashMap::with_hasher(sea_hasher),
                type_bindings: HashMap::with_hasher(sea_hasher),
                variable_types: HashMap::with_hasher(sea_hasher),
            },
            error_reporter: ErrorReporter::new()
        }
    }

    // ===============================
    // Module Lowering
    // ===============================
    pub fn lower_module(&mut self, stmts: Vec<Stmt>) -> HirModule {
        let mut items = Vec::new();
        for stmt in stmts { items.push(self.lower_toplevel(stmt)); }
        HirModule { name: "root".to_string(), imports: vec![], items }
    }

    fn lower_toplevel(&mut self, stmt: Stmt) -> Hir {
        match stmt {
            Stmt::FuncDecl(f) => {
                let func = self.lower_func_decl(f);
                self.ctx.functions.insert(func.name.clone(), func.clone());
                Hir::Func(func)
            }
            Stmt::ClassDecl(c) => {
                let class = self.lower_class_decl(c);
                self.ctx.classes.insert(class.name.clone(), class.clone());
                Hir::Class(class)
            }
            Stmt::InterfaceDecl(i) => {
                let interface = self.lower_interface_decl(i);
                self.ctx.interfaces.insert(interface.name.clone(), interface.clone());
                Hir::Interface(interface)
            }
            Stmt::ImplDecl(i) => Hir::Impl(self.lower_impl_decl(i)),
            Stmt::EnumDecl(e) => Hir::Enum(self.lower_enum_decl(e)),
            Stmt::UnsafeBlock(b) => {
                let body = b.block.into_iter().map(|s| self.lower_stmt(s)).collect();
                Hir::Stmt(Box::new(HirStmt::UnsafeBlock { body: Box::new(body) }))
            }
            other => Hir::Stmt(self.lower_stmt(other)),
        }
    }

    // ===============================
    // Function Lowering
    // ===============================
    fn lower_func_decl(&mut self, func: FuncDecl) -> HirFunc {
        HirFunc {
            name: func.name,
            visibility: lower_visibility(&func.visibility),
            is_static: func.is_static,
            is_unsafe: func.is_unsafe,
            generics: func.generics.unwrap_or_default().into_iter().map(|g| HirGeneric { name: g.type_name, constraints: g.constraints }).collect(),
            params: func.params.into_iter().map(|p| HirParam { name: p.name, ty: self.lower_type(&p.type_annotation) }).collect(),
            return_type: func.return_type.map(|t| self.lower_type(&t)),
            body: func.body.map(|b| b.block.into_iter().map(|s| self.lower_stmt(s)).collect()),
        }
    }

    fn lower_class_decl(&mut self, c: ClassDecl) -> HirClass {
        let methods = c.body.unwrap_or_default().into_iter().filter_map(|s| match s { Stmt::FuncDecl(f) => Some(self.lower_func_decl(f)), _ => None }).collect();
        HirClass {
            name: c.name,
            visibility: lower_visibility(&c.visibility),
            generics: c.generics.unwrap_or_default().into_iter().map(|g| HirGeneric { name: g.type_name, constraints: g.constraints }).collect(),
            fields: c.params.unwrap_or_default().iter().map(|p| self.lower_field(p)).collect(),
            methods,
            interfaces: vec![],
        }
    }

    fn lower_field(&self, p: &ast::Param) -> HirField {
        HirField { name: p.name.clone(), field_type: self.lower_type(&p.type_annotation), visibility: lower_visibility(&p.visibility) }
    }

    fn lower_interface_decl(&mut self, i: InterfaceDecl) -> HirInterface {
        HirInterface {
            name: i.name,
            visibility: lower_visibility(&i.visibility),
            methods: i.methods.unwrap_or_default().into_iter().map(|f| self.lower_func_decl(f)).collect(),
            generics: i.generics.unwrap_or_default().into_iter().map(|g| HirGeneric { name: g.type_name, constraints: g.constraints }).collect(),
        }
    }

    fn lower_impl_decl(&mut self, i: ImplDecl) -> HirImpl {
        let hir_impl = HirImpl {
            generics: i.generics.unwrap_or_default().into_iter().map(|g| HirGeneric { name: g.type_name, constraints: g.constraints }).collect(),
            interface: i.interface.clone(),
            target: i.target.clone(),
            methods: i.methods.unwrap_or_default().into_iter().map(|f| self.lower_func_decl(f)).collect(),
        };
        if let Some(class) = self.ctx.classes.get_mut(&hir_impl.target) {
            if !class.interfaces.contains(&hir_impl.interface) { class.interfaces.push(hir_impl.interface.clone()); }
        }
        hir_impl
    }

    fn lower_enum_decl(&mut self, e: EnumDecl) -> HirEnum {
        let variants = e.variants.into_iter().map(|v| HirEnumVariant { name: v.name, fields: v.fields.into_iter().map(|f| HirField { name: f.name, field_type: self.lower_type(&f.field_type), visibility: lower_visibility(&f.visibility) }).collect() }).collect();
        HirEnum {
            name: e.name,
            visibility: lower_visibility(&e.visibility),
            generics: e.generics.unwrap_or_default().into_iter().map(|g| HirGeneric { name: g.type_name, constraints: g.constraints }).collect(),
            variants,
        }
    }

    // ===============================
    // Statements
    // ===============================
    fn lower_stmt(&mut self, stmt: Stmt) -> Box<HirStmt> {
        match stmt {
            Stmt::Let(l) => self.lower_let_stmt(&l),
            Stmt::Return(r) => Box::new(HirStmt::Return(r.value.map(|v| self.lower_expr(&v))))
            , Stmt::ExprStmt(e) => Box::new(HirStmt::Expr(self.lower_expr(&e.expr)))
            , Stmt::If(i) => self.lower_if_stmt(i)
            , Stmt::While(while_stmt) => Box::new(HirStmt::While { cond: self.lower_expr(&while_stmt.condition), body: Box::new(while_stmt.block.into_iter().map(|s| self.lower_stmt(s)).collect()) })
            , Stmt::For(for_stmt) => Box::new(HirStmt::For { init: for_stmt.let_stmt.map(|s| self.lower_stmt(Stmt::Let(s))), condition: for_stmt.condition.map(|e| self.lower_expr(&e)), increment: for_stmt.increment.map(|e| self.lower_expr(&e)), body: Box::new(for_stmt.block.into_iter().map(|s| self.lower_stmt(s)).collect()) })
            , Stmt::Match(match_stmt) => Box::new(HirStmt::Match { expr: self.lower_expr(&match_stmt.expr), arms: match_stmt.arms.into_iter().map(|a| HirMatchArm { pattern: self.lower_pattern(&a.pattern), body: Box::new(a.block.into_iter().map(|s| self.lower_stmt(s)).collect()) }).collect() })
            , Stmt::Break => Box::new(HirStmt::Break)
            , Stmt::Continue => Box::new(HirStmt::Continue)
            , _ => unreachable!(),
        }
    }

    fn lower_if_stmt(&mut self, i: ast::IfStmt) -> Box<HirStmt> {
        let cond = self.lower_expr(&i.condition);
        let then_block = Box::new(i.then_branch.into_iter().map(|s| self.lower_stmt(s)).collect());
        let else_block = i.else_branch.map(|b| self.lower_else_branch(*b));
        Box::new(HirStmt::If { cond, then_block, else_block })
    }

    fn lower_else_branch(&mut self, branch: ElseBranch) -> Box<HirStmt> {
        match branch {
            ElseBranch::Else(else_block) => Box::new(else_block.into_iter().map(|s| self.lower_stmt(s)).collect()),
            ElseBranch::If(else_if) => self.lower_stmt(Stmt::If(*else_if)),
        }
    }

    fn lower_let_stmt(&mut self, l: &LetStmt) -> Box<HirStmt> {
        let value = self.lower_expr(&l.value);
        let final_type = self.lower_type(&l.type_annotation);

        self.ctx.variable_types.insert(l.ident.clone(), final_type.clone());
        Box::new(HirStmt::Let { name: l.ident.clone(), ty: final_type, value, mutable: l.mutability })
    }

    pub fn infer_type(&self, value: &hir::HirExpr) -> Option<hir::HirType> {
        match value {
            HirExpr::String(_) => Some(HirType::String),
            HirExpr::Number(_) => Some(HirType::U32),
            HirExpr::Boolean(_) => Some(HirType::Boolean),
            HirExpr::Decimal(_) => Some(HirType::F64),
            HirExpr::ClassInit { name, .. } => match &**name { HirExpr::Ident(n) => Some(HirType::Class(n.clone(), vec![])), _ => None },
            HirExpr::Binary { left, op: _, right } => {
                let lt = self.infer_type(left)?; let rt = self.infer_type(right)?; if lt == rt { Some(lt) } else { None }
            }
            HirExpr::FieldAccess { object, field } => self.infer_field_access_type(object, field),
            _ => None,
        }
    }

    fn infer_field_access_type(&self, object: &HirExpr, field: &str) -> Option<HirType> {
        let obj_ty = self.infer_type(object)?;
        match obj_ty {
            HirType::Class(name, _) => {
                let class = self.ctx.classes.get(&name)?;
                class.fields.iter().find(|f| f.name == field).map(|f| f.field_type.clone())
            }
            HirType::Interface(_, _) | HirType::Enum(_, _) => None,
            _ => None,
        }
    }

    fn lower_pattern(&mut self, pattern: &Pattern) -> HirPattern {
        match pattern {
            Pattern::Ident(name) => HirPattern::Ident(name.clone()),
            Pattern::Number(n) => HirPattern::Number(*n),
            Pattern::String(s) => HirPattern::String(s.clone()),
            Pattern::Tuple(inner) => HirPattern::Tuple(inner.iter().map(|p| self.lower_pattern(p)).collect()),
            Pattern::Wildcard => HirPattern::Wildcard,
        }
    }

    fn lower_expr(&mut self, expr: &Expr) -> HirExpr {
        match expr {
            Expr::Call { callee, arguments } => self.lower_expr_call(callee, arguments),
            Expr::FieldAccess { object, field } => HirExpr::FieldAccess { object: Box::new(self.lower_expr(object)), field: field.clone() },
            Expr::Number(n) => HirExpr::Number(*n),
            Expr::String(s) => HirExpr::String(s.clone()),
            Expr::Boolean(b) => HirExpr::Boolean(*b),
            Expr::Ident(name) => HirExpr::Ident(name.clone()),
            Expr::Array { .. } => unimplemented!("Array lowering handled by intrinsics"),
            Expr::ArrayIndex { .. } => unimplemented!("Array index handled by intrinsics"),
            Expr::ArrayInit { .. } => unimplemented!("Array init handled by intrinsics"),
            Expr::Decimal(d) => HirExpr::Decimal(*d),
            Expr::Comparison { lhs, op, rhs } => HirExpr::Comparison { left: Box::new(self.lower_expr(lhs)), op: Self::lower_cmp_operator(op.clone()), right: Box::new(self.lower_expr(rhs)) },
            Expr::ClassInit { callee, arguments } => HirExpr::ClassInit { name: Box::new(self.lower_expr(callee)), args: arguments.iter().map(|a| self.lower_expr(a)).collect() },
            Expr::Binary { left, op, right } => HirExpr::Binary { left: Box::new(self.lower_expr(left)), op: Self::lower_op(op.clone()), right: Box::new(self.lower_expr(right)) },
            Expr::Get { object, field } => HirExpr::Get { object: Box::new(self.lower_expr(object)), field: field.clone() },
            Expr::Assignment { lhs, op, rhs } => HirExpr::Assignment { target: Box::new(self.lower_expr(lhs)), op: Self::lower_assignment_operator(op), value: Box::new(self.lower_expr(rhs)) },
            Expr::ExprList { exprs } => HirExpr::ExprList(exprs.iter().map(|e| self.lower_expr(e)).collect()),
            &Expr::Char(_) => todo!(),
        }
    }

    fn lower_expr_call(&mut self, callee: &Expr, arguments: &Vec<Expr>) -> HirExpr {
        let lowered_callee = self.lower_expr(&*callee);
        if let Some(value) = self.detect_interface_call(arguments, &lowered_callee) { return value; }
        HirExpr::Call { callee: Box::new(lowered_callee), args: arguments.iter().map(|a| self.lower_expr(a)).collect() }
    }

    fn detect_interface_call(&mut self, arguments: &Vec<Expr>, lowered_callee: &HirExpr) -> Option<HirExpr> {
        let HirExpr::FieldAccess { object, field } = lowered_callee else { return None };
        let interface = self.find_interface_method(object, field)?;
        Some(HirExpr::InterfaceCall { callee: Box::new(lowered_callee.clone()), args: arguments.iter().map(|a| self.lower_expr(a)).collect(), interface })
    }

    fn lower_assignment_operator(op: &String) -> AssignmentOperator {
        match op.as_str() {
            "=" => AssignmentOperator::Assign,
            "+=" => AssignmentOperator::AddAssign,
            "-=" => AssignmentOperator::SubtractAssign,
            "*=" => AssignmentOperator::MultiplyAssign,
            "/=" => AssignmentOperator::DivideAssign,
            "%=" => AssignmentOperator::ModuloAssign,
            "&=" => AssignmentOperator::BitAndAssign,
            "|=" => AssignmentOperator::BitOrAssign,
            "^=" => AssignmentOperator::BitXorAssign,
            "<<=" => AssignmentOperator::ShiftLeftAssign,
            ">>=" => AssignmentOperator::ShiftRightAssign,
            _ => unreachable!(),
        }
    }

    fn lower_operator(_op: &String) -> Operator { unreachable!() }

    fn lower_op(op: Op) -> Operator {
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
        }
    }

    fn find_interface_method(&self, object: &HirExpr, method: &str) -> Option<String> {
        // Resolve class name from variable types or direct identifier equal to class name
        let class_name = match object {
            HirExpr::Ident(var) => {
                match self.ctx.variable_types.get(var) {
                    Some(HirType::Class(name, _)) => Some(name.clone()),
                    _ => if self.ctx.classes.contains_key(var) { Some(var.clone()) } else { None }
                }
            }
            _ => None,
        }?;
        let class = self.ctx.classes.get(&class_name)?;
        for iface_name in &class.interfaces {
            let iface = self.ctx.interfaces.get(iface_name)?;
            if iface.methods.iter().any(|m| m.name == method) { return Some(iface_name.clone()); }
        }
        None
    }

    // ===============================
    // Type Lowering
    // ===============================
    fn lower_type(&self, t: &Type) -> HirType {
        match t {
            Type::I32 => HirType::I32,
            Type::I64 => HirType::I64,
            Type::F64 => HirType::F64,
            Type::String => HirType::String,
            Type::Boolean => HirType::Boolean,
            Type::Class(name) => HirType::Class(name.clone(), vec![]),
            Type::Void => HirType::Void,
            Type::U32 => HirType::U32,
            Type::F32 => HirType::F32,
            Type::U8 => HirType::U8,
            Type::I8 => HirType::I8,
            Type::U16 => HirType::U16,
            Type::I16 => HirType::I16,
            Type::U64 => HirType::U64,
            Type::I128 => HirType::I128,
            Type::U128 => HirType::U128,
            _ => HirType::Void, // TODO: handle all
        }
    }

    pub fn monomorphize_function(&mut self, func: &HirFunc, substitutions: &HashMap<String, HirType>) -> HirFunc {
        let mut new_func = func.clone();
        self.apply_substitutions_to_func(&mut new_func, substitutions);
        if let Some(body) = &mut new_func.body { self.monomorphize_body(body, substitutions); }
        new_func
    }

    fn apply_substitutions_to_func(&self, func: &mut HirFunc, substitutions: &HashMap<String, HirType>) {
        for (param_name, ty) in substitutions {
            for param in &mut func.params {
                if let HirType::Generic(ref name) = param.ty { if name == param_name { param.ty = ty.clone(); } }
            }
            if let Some(ret) = &mut func.return_type { if let HirType::Generic(name) = ret { if name == param_name { *ret = ty.clone(); } } }
        }
    }

    fn monomorphize_body(&mut self, body: &mut HirStmt, substitutions: &HashMap<String, HirType>) {
        if let HirStmt::Block { body } = body { for stmt in body.iter_mut() { self.monomorphize_stmt(stmt, substitutions); } }
    }

    fn monomorphize_stmt(&mut self, stmt: &mut HirStmt, subs: &HashMap<String, HirType>) {
        match stmt { HirStmt::Expr(e) | HirStmt::Return(Some(e)) => self.monomorphize_expr(e, subs), _ => {} }
    }

    fn monomorphize_expr(&mut self, expr: &mut HirExpr, subs: &HashMap<String, HirType>) {
        let HirExpr::InterfaceCall { callee, interface, args } = expr else { return };
        let Some(new_expr) = self.direct_method_lookup(subs, callee, interface, &*args) else { return };
        *expr = new_expr;
    }

    fn direct_method_lookup(&mut self, subs: &HashMap<String, HirType>, callee: &mut Box<HirExpr>, interface: &mut String, args: &Vec<HirExpr>) -> Option<HirExpr> {
        let HirExpr::Ident(obj_name) = &**callee else { return None };
        let HirType::Class(class_name, _) = subs.get(obj_name)? else { return None };
        let class = self.ctx.classes.get(class_name)?;
        let method = class.methods.iter().find(|m| m.name == *interface)?;
        Some(HirExpr::Call { callee: Box::new(HirExpr::Ident(method.name.clone())), args: args.clone() })
    }

    fn lower_cmp_operator(op: ComparisonOp) -> Operator {
        match op {
            ComparisonOp::Equal => Operator::Equals,
            ComparisonOp::NotEqual => Operator::NotEquals,
            ComparisonOp::LessThan => Operator::LessThan,
            ComparisonOp::LessThanOrEqual => Operator::LessThanOrEqual,
            ComparisonOp::GreaterThan => Operator::GreaterThan,
            ComparisonOp::GreaterThanOrEqual => Operator::GreaterThanOrEqual,
        }
    }
}

const fn lower_visibility(visibility: &ast::Visibility) -> hir::Visibility {
    match visibility {
        ast::Visibility::Public => hir::Visibility::Public,
        ast::Visibility::Private => hir::Visibility::Private,
        ast::Visibility::Internal => hir::Visibility::Internal,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn iface_has_print() -> InterfaceDecl {
        InterfaceDecl { name: "Printable".into(), visibility: ast::Visibility::Public, methods: Some(vec![
            FuncDecl { visibility: ast::Visibility::Public, is_static: false, is_unsafe: false, is_extern: false, name: "print".into(), generics: None, regions: None, params: vec![], return_type: None, body: None }
        ]), generics: None }
    }

    fn class_foo() -> ClassDecl { ClassDecl { visibility: ast::Visibility::Public, name: "Foo".into(), generics: None, regions: None, params: None, body: None } }

    #[test]
    fn lowers_comparison_operator() {
        let mut lowerer = HirLowerer::new();
        let expr = Expr::Comparison { lhs: Box::new(Expr::Number(1)), op: ComparisonOp::LessThan, rhs: Box::new(Expr::Number(2)) };
        let h = lowerer.lower_expr(&expr);
        match h { HirExpr::Comparison { op, .. } => assert_eq!(op, Operator::LessThan), _ => panic!("wrong lowering") }
    }

    #[test]
    fn detects_interface_call_via_var_type() {
        let mut lowerer = HirLowerer::new();
        let stmts = vec![
            Stmt::InterfaceDecl(iface_has_print()),
            Stmt::ClassDecl(class_foo()),
            Stmt::ImplDecl(ImplDecl { generics: None, interface: "Printable".into(), target: "Foo".into(), methods: None }),
            Stmt::FuncDecl(FuncDecl {
                visibility: ast::Visibility::Public,
                is_static: true,
                is_unsafe: false,
                is_extern: false,
                name: "main".into(),
                generics: None,
                regions: None,
                params: vec![],
                return_type: None,
                body: Some(ast::Block { block: vec![
                    Stmt::Let(LetStmt { mutability: false, ident: "foo".into(), type_annotation: Type::Class("Foo".into()), value: Box::new(Expr::ClassInit { callee: Box::new(Expr::Ident("Foo".into())), arguments: vec![] }) }),
                    Stmt::ExprStmt(ast::InternalExprStmt { expr: Box::new(Expr::Call { callee: Box::new(Expr::FieldAccess { object: Box::new(Expr::Ident("foo".into())), field: "print".into() }), arguments: vec![] }) })
                ] })
            })
        ];
        let hir_mod = lowerer.lower_module(stmts);
        let func = hir_mod.items.into_iter().find_map(|i| if let Hir::Func(f) = i { Some(f) } else { None }).unwrap();
        let body = func.body.unwrap();
        let HirStmt::Block { body } = body else { panic!("expected block") };
        let last = body.last().unwrap();
        match last { HirStmt::Expr(HirExpr::InterfaceCall { interface, .. }) => assert_eq!(interface, "Printable"), _ => panic!("expected interface call") }
    }
}
