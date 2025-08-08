use std::collections::HashMap;
use std::hash::BuildHasher;
use seahash::SeaHasher;
use crate::frontend::ast;
use crate::frontend::ast::{ClassDecl, ElseBranch, EnumDecl, Expr, FuncDecl, ImplDecl, InterfaceDecl, Pattern, Stmt, Type};
use crate::midend::ir::hir;
use crate::midend::ir::hir::{Hir, HirClass, HirEnum, HirEnumVariant, HirExpr, HirField, HirFunc, HirGeneric, HirImpl, HirInterface, HirMatchArm, HirModule, HirParam, HirPattern, HirStmt, HirType};

// ===============================
// Lowering Context
// ===============================

#[derive(Copy, Clone)]
pub struct SeaHashBuilder {
    hasher: SeaHasher,
}

impl SeaHashBuilder {
    pub fn new() -> Self {
        Self { hasher: SeaHasher::new() }
    }
}

impl BuildHasher for SeaHashBuilder {
    type Hasher = SeaHasher;
    fn build_hasher(&self) -> Self::Hasher {
        self.hasher
    }
}

pub struct LoweringCtx {
    pub classes: HashMap<String, HirClass, SeaHashBuilder>,
    pub interfaces: HashMap<String, HirInterface, SeaHashBuilder>,
    pub functions: HashMap<String, HirFunc, SeaHashBuilder>,
    pub type_bindings: HashMap<String, HirType, SeaHashBuilder>, // generic substitutions
}

pub struct HirLowerer {
    pub ctx: LoweringCtx,
}

impl HirLowerer {
    pub fn new() -> Self {
        let sea_hasher = SeaHashBuilder::new();
        Self {
            ctx: LoweringCtx {
                classes: HashMap::with_hasher(sea_hasher),
                interfaces: HashMap::with_hasher(sea_hasher),
                functions: HashMap::with_hasher(sea_hasher),
                type_bindings: HashMap::with_hasher(sea_hasher),
            },
        }
    }

    // ===============================
    // Module Lowering
    // ===============================
    pub fn lower_module(&mut self, stmts: Vec<Stmt>) -> HirModule {
        let mut items = Vec::new();

        for stmt in stmts {
            match stmt {
                Stmt::FuncDecl(f) => {
                    let func = self.lower_func_decl(f);
                    self.ctx.functions.insert(func.name.clone(), func.clone());
                    items.push(Hir::Func(func));
                }
                Stmt::ClassDecl(c) => {
                    let class = self.lower_class_decl(c);
                    self.ctx.classes.insert(class.name.clone(), class.clone());
                    items.push(Hir::Class(class));
                }
                Stmt::InterfaceDecl(i) => {
                    let interface = self.lower_interface_decl(i);
                    self.ctx.interfaces.insert(interface.name.clone(), interface.clone());
                    items.push(Hir::Interface(interface));
                }
                Stmt::ImplDecl(i) => {
                    let imp = self.lower_impl_decl(i);
                    items.push(Hir::Impl(imp));
                }
                Stmt::EnumDecl(e) => {
                    let en = self.lower_enum_decl(e);
                    items.push(Hir::Enum(en));
                }
                Stmt::UnsafeBlock(b) => {
                    items.push(Hir::Stmt(Box::new(HirStmt::UnsafeBlock { body: Box::new(b.block.into_iter().map(|s| self.lower_stmt(s)).collect()) })));
                }
                _ => items.push(Hir::Stmt(self.lower_stmt(stmt))),
            }
        }

        HirModule {
            name: "root".to_string(),
            imports: vec![],
            items,
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
            generics: func.generics.unwrap_or_default()
                .into_iter()
                .map(|g| HirGeneric { name: g.type_name, constraints: g.constraints })
                .collect(),
            params: func.params.into_iter().map(|p| HirParam {
                name: p.name,
                ty: self.lower_type(&p.type_annotation),
            }).collect(),
            return_type: func.return_type.map(|t| self.lower_type(&t)),
            body: func.body.map(|b| b.block.into_iter().map(|s| self.lower_stmt(s)).collect()),
        }
    }

    fn lower_class_decl(&mut self, c: ClassDecl) -> HirClass {
        HirClass {
            name: c.name,
            visibility: lower_visibility(&c.visibility),
            generics: c.generics.unwrap_or_default()
                .into_iter()
                .map(|g| HirGeneric { name: g.type_name, constraints: g.constraints })
                .collect(),
            fields: c.params.unwrap_or_default().iter().map(|p| self.lower_field(p)).collect(),
            methods: c.body.unwrap_or_default()
                .into_iter()
                .filter_map(|s| match s {
                    Stmt::FuncDecl(f) => Some(self.lower_func_decl(f)),
                    _ => None,
                })
                .collect(),
            interfaces: vec![],
        }
    }

    fn lower_field(&self, p: &ast::Param) -> HirField {
        HirField {
            name: p.name.clone(),
            field_type: self.lower_type(&p.type_annotation),
            visibility: lower_visibility(&p.visibility),
        }
    }

    fn lower_interface_decl(&mut self, i: InterfaceDecl) -> HirInterface {
        HirInterface {
            name: i.name,
            visibility: lower_visibility(&i.visibility),
            methods: i.methods.unwrap_or_default()
                .into_iter()
                .map(|f| self.lower_func_decl(f))
                .collect(),
            generics: i.generics.unwrap_or_default()
                .into_iter()
                .map(|g| HirGeneric { name: g.type_name, constraints: g.constraints })
                .collect(),
        }
    }

    fn lower_impl_decl(&mut self, i: ImplDecl) -> HirImpl {
        let hir_impl = HirImpl {
            generics: i.generics.unwrap_or_default()
                .into_iter()
                .map(|g| HirGeneric { name: g.type_name, constraints: g.constraints })
                .collect(),
            interface: i.interface.clone(),
            target: i.target.clone(),
            methods: i.methods.unwrap_or_default()
                .into_iter()
                .map(|f| self.lower_func_decl(f))
                .collect(),
        };

        // Attach this interface to the corresponding class
        if let Some(class) = self.ctx.classes.get_mut(&hir_impl.target) {
            if !class.interfaces.contains(&hir_impl.interface) {
                class.interfaces.push(hir_impl.interface.clone());
            }
        }

        hir_impl
    }


    fn lower_enum_decl(&mut self, e: EnumDecl) -> HirEnum {
        HirEnum {
            name: e.name,
            visibility: lower_visibility(&e.visibility),
            generics: e.generics.unwrap_or_default()
                .into_iter()
                .map(|g| HirGeneric { name: g.type_name, constraints: g.constraints })
                .collect(),
            variants: e.variants.into_iter().map(|v| HirEnumVariant {
                name: v.name,
                fields: v.fields.into_iter().map(|f| HirField {
                    name: f.name,
                    field_type: self.lower_type(&f.field_type),
                    visibility: lower_visibility(&f.visibility),
                }).collect(),
            }).collect(),
        }
    }

    // ===============================
    // Statements
    // ===============================
    fn lower_stmt(&mut self, stmt: Stmt) -> Box<HirStmt> {
        match stmt {
            Stmt::Let(l) => Box::new(HirStmt::Let {
                name: l.ident,
                ty: l.type_annotation.map(|t| self.lower_type(&t)),
                value: self.lower_expr(&l.value),
                mutable: l.mutability,
            }),
            Stmt::Return(r) => Box::new(HirStmt::Return(r.value.map(|v| self.lower_expr(&v)))),
            Stmt::ExprStmt(e) => Box::new(HirStmt::Expr(self.lower_expr(&e.expr))),
            Stmt::If(i) => Box::new(HirStmt::If {
                cond: self.lower_expr(&i.condition),
                then_block: Box::new(i.then_branch.into_iter().map(|s| self.lower_stmt(s)).collect()),
                else_block: i.else_branch.map(|b| match *b {
                    ElseBranch::Else(else_block) => Box::new(else_block.into_iter().map(|s| self.lower_stmt(s)).collect::<HirStmt>()),
                    ElseBranch::If(else_if) => self.lower_stmt(Stmt::If(*else_if))
                })
            }),
            Stmt::While(while_stmt) => Box::new(HirStmt::While {
                cond: self.lower_expr(&while_stmt.condition),
                body: Box::new(while_stmt.block.into_iter().map(|s| self.lower_stmt(s)).collect()),
            }),
            Stmt::For(for_stmt) => Box::new(HirStmt::For {
                init: for_stmt.let_stmt.map(|s| self.lower_stmt(Stmt::Let(s))),
                condition: for_stmt.condition.map(|e| self.lower_expr(&e)),
                increment: for_stmt.increment.map(|e| self.lower_expr(&e)),
                body: Box::new(for_stmt.block.into_iter().map(|s| self.lower_stmt(s)).collect()),
            }),
            Stmt::Match(match_stmt) => Box::new(HirStmt::Match {
                    expr: self.lower_expr(&match_stmt.expr),
                    arms: match_stmt.arms.into_iter().map(|a| HirMatchArm {
                        pattern: self.lower_pattern(&a.pattern),
                        body: Box::new(a.block.into_iter().map(|s| self.lower_stmt(s)).collect()),
                    }).collect(),
            }),
            Stmt::Break => Box::new(HirStmt::Break),
            Stmt::Continue => Box::new(HirStmt::Continue),
            _ => unreachable!(),
        }
    }

    fn lower_pattern(&mut self, pattern: &Pattern) -> HirPattern {
        match pattern {
            Pattern::Ident(name) => HirPattern::Ident(name.clone()),
            Pattern::Number(n) => HirPattern::Number(*n),
            Pattern::String(s) => HirPattern::String(s.clone()),
            Pattern::Tuple(inner) => HirPattern::Tuple(
                inner.iter().map(|p| self.lower_pattern(p)).collect()
            ),
            Pattern::Wildcard => HirPattern::Wildcard,
        }
    }

    // ===============================
    // Expressions with Interface Call Detection
    // ===============================
    fn lower_expr(&mut self, expr: &Expr) -> HirExpr {
        match expr {
            Expr::Call { callee, arguments } => {
                let lowered_callee = self.lower_expr(&**callee);

                // Interface call detection
                if let HirExpr::FieldAccess { object, field } = &lowered_callee {
                    if let Some(interface) = self.find_interface_method(object, field) {
                        return HirExpr::InterfaceCall {
                            callee: Box::new(lowered_callee),
                            args: arguments.into_iter().map(|a| self.lower_expr(a)).collect(),
                            interface,
                        };
                    }
                }

                HirExpr::Call {
                    callee: Box::new(lowered_callee),
                    args: arguments.into_iter().map(|a| self.lower_expr(a)).collect(),
                }
            }
            Expr::FieldAccess { object, field } => HirExpr::FieldAccess {
                object: Box::new(self.lower_expr(&object)),
                field: field.clone(),
            },
            Expr::Number(n) => HirExpr::Number(*n),
            Expr::String(s) => HirExpr::String(s.clone()),
            Expr::Boolean(b) => HirExpr::Boolean(*b),
            Expr::Ident(name) => HirExpr::Ident(name.clone()),
            _ => todo!("other exprs"),
        }
    }

    fn find_interface_method(&self, object: &HirExpr, method: &str) -> Option<String> {
        if let HirExpr::Ident(obj_name) = object {
            if let Some(class) = self.ctx.classes.get(obj_name) {
                for iface_name in &class.interfaces {
                    if let Some(iface) = self.ctx.interfaces.get(iface_name) {
                        if iface.methods.iter().any(|m| m.name == method) {
                            return Some(iface_name.clone());
                        }
                    }
                }
            }
        }
        None
    }

    // ===============================
    // Type Lowering
    // ===============================
    fn lower_type(&self, t: &Type) -> HirType {
        match t {
            Type::I32 => HirType::Primitive("i32".into()),
            Type::I64 => HirType::Primitive("i64".into()),
            Type::F64 => HirType::Primitive("f64".into()),
            Type::String => HirType::Primitive("string".into()),
            Type::Boolean => HirType::Primitive("bool".into()),
            Type::Class(name) => HirType::Class(name.clone(), vec![]),
            Type::Void => HirType::Void,
            _ => HirType::Void, // TODO: handle all
        }
    }

    // ===============================
    // Monomorphization
    // ===============================
    pub fn monomorphize_function(&mut self, func: &HirFunc, substitutions: &HashMap<String, HirType>) -> HirFunc {
        let mut new_func = func.clone();
        for (param_name, ty) in substitutions {
            for param in &mut new_func.params {
                if let HirType::Generic(ref name) = param.ty {
                    if name == param_name {
                        param.ty = ty.clone();
                    }
                }
            }
            if let Some(ret) = &mut new_func.return_type {
                if let HirType::Generic(name) = ret {
                    if name == param_name {
                        *ret = ty.clone();
                    }
                }
            }
        }

        if let Some(body) = &mut new_func.body {
            match body {
                HirStmt::Block { body } => {
                    for stmt in body.iter_mut() {
                        self.monomorphize_stmt(stmt, substitutions);
                    }
                }
                _ => {}
            }
        }
        new_func
    }

    fn monomorphize_stmt(&mut self, stmt: &mut HirStmt, subs: &HashMap<String, HirType>) {
        match stmt {
            HirStmt::Expr(e) | HirStmt::Return(Some(e)) => self.monomorphize_expr(e, subs),
            _ => {}
        }
    }

    fn monomorphize_expr(&mut self, expr: &mut HirExpr, subs: &HashMap<String, HirType>) {
        match expr {
            HirExpr::InterfaceCall { callee, interface, .. } => {
                if let HirExpr::Ident(obj_name) = &**callee {
                    if let Some(HirType::Class(class_name, _)) = subs.get(obj_name) {
                        if let Some(class) = self.ctx.classes.get(class_name) {
                            // Direct method lookup (devirtualization)
                            if let Some(method) = class.methods.iter().find(|m| m.name == *interface) {
                                *expr = HirExpr::Call {
                                    callee: Box::new(HirExpr::Ident(method.name.clone())),
                                    args: vec![],
                                };
                            }
                        }
                    }
                }
            }
            _ => {}
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