use ir::ast::{
    self, ClassDecl, ConstStmt, ElseBranch, EnumDecl, Expr, FuncDecl, ImplDecl, InterfaceDecl,
    LetStmt, Op, Param, Pattern, Stmt, Type,
};
use ir::context::Context;
use ir::hir::{
    self, AssignmentOperator, Hir, HirClass, HirEnum, HirEnumVariant, HirExpr, HirField, HirFunc,
    HirGeneric, HirImpl, HirInterface, HirMatchArm, HirModule, HirParam, HirPattern, HirStmt,
    HirType, Operator, StrId,
};
use smallvec::SmallVec;
use std::cell::RefCell;
use std::collections::HashMap;
use std::collections::hash_map::Entry;
use std::rc::Rc;

use ir::errors::reporter::ErrorReporter;
use ir::ir_hasher::FxHashBuilder;
use zetaruntime::string_pool::StringPool;

// ===============================
// Lowering Context
// ===============================

pub struct LoweringCtx<'a> {
    pub classes: HashMap<StrId, HirClass, FxHashBuilder>,
    pub interfaces: HashMap<StrId, HirInterface, FxHashBuilder>,
    pub functions: HashMap<StrId, HirFunc, FxHashBuilder>,
    pub type_bindings: HashMap<StrId, HirType, FxHashBuilder>,
    pub variable_types: HashMap<StrId, HirType, FxHashBuilder>,

    // caches for instantiated generics:
    pub instantiated_classes: HashMap<(StrId, StrId), StrId, FxHashBuilder>, // (orig_name, suffix) -> new_name
    pub instantiated_functions: HashMap<(StrId, StrId), StrId, FxHashBuilder>, // (orig_name, suffix) -> new_name

    pub context: Rc<RefCell<Context<'a>>>,
}

pub struct HirLowerer<'a> {
    pub ctx: LoweringCtx<'a>,
    pub error_reporter: ErrorReporter,
}

impl<'a> HirLowerer<'a> {
    pub fn new(context: Rc<RefCell<Context<'a>>>) -> Self {
        let sea_hasher = FxHashBuilder;

        Self {
            ctx: LoweringCtx {
                classes: HashMap::with_hasher(sea_hasher),
                functions: HashMap::with_hasher(sea_hasher),
                interfaces: HashMap::with_hasher(sea_hasher),
                type_bindings: HashMap::with_hasher(sea_hasher),
                variable_types: HashMap::with_hasher(sea_hasher),

                instantiated_classes: HashMap::with_hasher(sea_hasher),
                instantiated_functions: HashMap::with_hasher(sea_hasher),

                context,
            },
            error_reporter: ErrorReporter::new(),
        }
    }

    // ===============================
    // Module Lowering
    // ===============================
    pub fn lower_module(&mut self, stmts: Vec<Stmt>) -> HirModule {
        let mut items = Vec::new();
        for stmt in stmts {
            let item = self.lower_toplevel(stmt);
            // If we lowered a function/class/interface, register it for later resolution
            match &item {
                Hir::Func(f) => {
                    self.ctx.functions.insert(f.name.clone(), f.clone());
                }
                Hir::Class(c) => {
                    self.ctx.classes.insert(c.name.clone(), c.clone());
                }
                Hir::Interface(i) => {
                    self.ctx.interfaces.insert(i.name.clone(), i.clone());
                }
                _ => {}
            }
            items.push(item);
        }
        HirModule {
            name: StrId(self.ctx.context.borrow_mut().string_pool.intern("root")),
            imports: vec![],
            items,
        }
    }

    fn lower_toplevel(&mut self, stmt: Stmt) -> Hir {
        match stmt {
            Stmt::FuncDecl(f) => {
                let func = self.lower_func_decl(f);
                Hir::Func(func)
            }
            Stmt::ClassDecl(c) => {
                let class = self.lower_class_decl(c);
                Hir::Class(class)
            }
            Stmt::InterfaceDecl(i) => {
                let interface = self.lower_interface_decl(i);
                Hir::Interface(interface)
            }
            Stmt::ImplDecl(i) => Hir::Impl(self.lower_impl_decl(i)),
            Stmt::EnumDecl(e) => Hir::Enum(self.lower_enum_decl(e)),
            Stmt::UnsafeBlock(b) => {
                let body = b.block.into_iter().map(|s| self.lower_stmt(s)).collect();
                Hir::Stmt(Box::new(HirStmt::UnsafeBlock {
                    body: Box::new(body),
                }))
            }
            other => Hir::Stmt(self.lower_stmt(other)),
        }
    }

    // ===============================
    // Function/Class/Interface/Enum Lowering
    // ===============================
    fn lower_func_decl(&mut self, func: FuncDecl) -> HirFunc {
        HirFunc {
            name: func.name,
            visibility: lower_visibility(&func.visibility),
            is_unsafe: func.is_unsafe,
            generics: func
                .generics
                .unwrap_or_default()
                .into_iter()
                .map(|g| HirGeneric {
                    name: g.type_name,
                    constraints: g.constraints,
                })
                .collect(),

            params: func
                .params
                .into_iter()
                .map(|p| match p {
                    Param::Normal(p) => HirParam::Normal {
                        name: p.name,
                        param_type: self.lower_type(&p.type_annotation),
                    },
                    Param::This(p) => HirParam::This {
                        param_type: p.type_annotation.map(|t| self.lower_type(&t)),
                    },
                })
                .collect(),

            return_type: func.return_type.map(|t| self.lower_type(&t)),
            body: func
                .body
                .map(|b| b.block.into_iter().map(|s| self.lower_stmt(s)).collect()),
        }
    }

    fn lower_class_decl(&mut self, c: ClassDecl) -> HirClass {
        HirClass {
            name: c.name,
            visibility: lower_visibility(&c.visibility),
            generics: c
                .generics
                .unwrap_or_default()
                .into_iter()
                .map(|g| HirGeneric {
                    name: g.type_name,
                    constraints: g.constraints,
                })
                .collect(),
            fields: c
                .params
                .unwrap_or_default()
                .iter()
                .map(|p| self.lower_field(p))
                .collect(),
            interfaces: vec![],
            methods: c
                .body
                .into_iter()
                .map(|s| self.lower_func_decl(s))
                .collect(),
            constants: Vec::from_iter(c.constants.into_iter().map(|b| self.lower_const_stmt(b))),
        }
    }

    fn lower_field(&self, p: &ast::Param) -> HirField {
        match p {
            Param::Normal(p) => HirField {
                name: p.name.clone(),
                field_type: self.lower_type(&p.type_annotation),
                visibility: lower_visibility(&p.visibility),
            },
            Param::This(_) => panic!("`this` parameter is not allowed in a class"),
        }
    }

    fn lower_interface_decl(&mut self, i: InterfaceDecl) -> HirInterface {
        HirInterface {
            name: i.name,
            visibility: lower_visibility(&i.visibility),
            methods: i
                .methods
                .unwrap_or_default()
                .into_iter()
                .map(|f| self.lower_func_decl(f))
                .collect(),
            generics: i
                .generics
                .unwrap_or_default()
                .into_iter()
                .map(|g| HirGeneric {
                    name: g.type_name,
                    constraints: g.constraints,
                })
                .collect(),
        }
    }

    fn lower_impl_decl(&mut self, i: ImplDecl) -> HirImpl {
        let hir_impl = HirImpl {
            generics: i
                .generics
                .unwrap_or_default()
                .into_iter()
                .map(|g| HirGeneric {
                    name: g.type_name,
                    constraints: g.constraints,
                })
                .collect(),

            interface: i.interface.clone(),
            target: i.target.clone(),
            methods: i
                .methods
                .unwrap_or_default()
                .into_iter()
                .map(|f| self.lower_func_decl(f))
                .collect(),
        };

        if let Some(class) = self.ctx.classes.get_mut(&hir_impl.target) {
            if !class.interfaces.contains(&hir_impl.interface) {
                class.interfaces.push(hir_impl.interface.clone());
            }
        }
        hir_impl
    }

    fn lower_enum_decl(&mut self, e: EnumDecl) -> HirEnum {
        let variants = e
            .variants
            .into_iter()
            .map(|v| HirEnumVariant {
                name: v.name,
                fields: v
                    .fields
                    .into_iter()
                    .map(|f| HirField {
                        name: f.name,
                        field_type: self.lower_type(&f.field_type),
                        visibility: lower_visibility(&f.visibility),
                    })
                    .collect(),
            })
            .collect();

        HirEnum {
            name: e.name,
            visibility: lower_visibility(&e.visibility),
            generics: e
                .generics
                .unwrap_or_default()
                .into_iter()
                .map(|g| HirGeneric {
                    name: g.type_name,
                    constraints: g.constraints,
                })
                .collect(),
            variants,
        }
    }

    // ===============================
    // Statements
    // ===============================
    fn lower_stmt(&mut self, stmt: Stmt) -> Box<HirStmt> {
        match stmt {
            Stmt::Let(l) => self.lower_let_stmt(&l),
            Stmt::Return(r) => Box::new(HirStmt::Return(r.value.map(|v| self.lower_expr(&v)))),
            Stmt::ExprStmt(e) => Box::new(HirStmt::Expr(self.lower_expr(&e.expr))),
            Stmt::If(i) => self.lower_if_stmt(i),

            Stmt::While(while_stmt) => Box::new(HirStmt::While {
                cond: self.lower_expr(&while_stmt.condition),
                body: Box::new(
                    while_stmt
                        .block
                        .into_iter()
                        .map(|s| self.lower_stmt(s))
                        .collect(),
                ),
            }),

            Stmt::For(for_stmt) => Box::new(HirStmt::For {
                init: for_stmt.let_stmt.map(|s| self.lower_stmt(Stmt::Let(s))),
                condition: for_stmt.condition.map(|e| self.lower_expr(&e)),
                increment: for_stmt.increment.map(|e| self.lower_expr(&e)),
                body: Box::new(
                    for_stmt
                        .block
                        .into_iter()
                        .map(|s| self.lower_stmt(s))
                        .collect(),
                ),
            }),

            Stmt::Match(match_stmt) => Box::new(HirStmt::Match {
                expr: self.lower_expr(&match_stmt.expr),
                arms: match_stmt
                    .arms
                    .into_iter()
                    .map(|a| HirMatchArm {
                        pattern: self.lower_pattern(&a.pattern),
                        guard: match &a.guard {
                            Some(guard) => Some(self.lower_expr(guard)),
                            _ => None,
                        },
                        body: a.block.into_iter().map(|s| self.lower_stmt(s)).collect(),
                    })
                    .collect(),
            }),

            Stmt::Break => Box::new(HirStmt::Break),
            Stmt::Continue => Box::new(HirStmt::Continue),
            Stmt::Block(block) => Box::new(HirStmt::Block {
                body: block
                    .into_iter()
                    .map(|s| *self.lower_stmt(s))
                    .collect::<Vec<_>>(),
            }),
            _ => panic!("unhandled stmt variant: {:?}", stmt),
        }
    }

    fn lower_if_stmt(&mut self, i: ast::IfStmt) -> Box<HirStmt> {
        let cond = self.lower_expr(&i.condition);
        let then_block = Box::new(
            i.then_branch
                .into_iter()
                .map(|s| self.lower_stmt(s))
                .collect(),
        );
        let else_block = i.else_branch.map(|b| self.lower_else_branch(*b));
        Box::new(HirStmt::If {
            cond,
            then_block,
            else_block,
        })
    }

    fn lower_else_branch(&mut self, branch: ElseBranch) -> Box<HirStmt> {
        match branch {
            ElseBranch::Else(else_block) => {
                Box::new(else_block.into_iter().map(|s| self.lower_stmt(s)).collect())
            }

            ElseBranch::If(else_if) => self.lower_stmt(Stmt::If(*else_if)),
        }
    }

    fn lower_let_stmt(&mut self, l: &LetStmt) -> Box<HirStmt> {
        let value = self.lower_expr(&l.value);
        let final_type = self.lower_type(&l.type_annotation);

        self.ctx
            .variable_types
            .insert(l.ident.clone(), final_type.clone());
        Box::new(HirStmt::Let {
            name: l.ident.clone(),
            ty: final_type,
            value,
            mutable: l.mutability,
        })
    }

    fn lower_const_stmt(&mut self, l: ConstStmt) -> Box<HirStmt> {
        let value = self.lower_expr(&l.value);
        let final_type = self.lower_type(&l.type_annotation);

        self.ctx
            .variable_types
            .insert(l.ident.clone(), final_type.clone());
        Box::new(HirStmt::Const {
            name: l.ident.clone(),
            ty: final_type,
            value,
        })
    }

    pub fn infer_type(&self, value: &HirExpr) -> Option<HirType> {
        match value {
            HirExpr::String(_) => Some(HirType::String),
            HirExpr::Number(_) => Some(HirType::I32),
            HirExpr::Boolean(_) => Some(HirType::Boolean),
            HirExpr::Decimal(_) => Some(HirType::F64),
            HirExpr::ClassInit { name, .. } => match &**name {
                HirExpr::Ident(n) => Some(HirType::Class(n.clone(), vec![])),
                _ => None,
            },
            HirExpr::Binary { left, op: _, right, .. } => {
                let lt = self.infer_type(left)?;
                let rt = self.infer_type(right)?;
                if lt == rt { Some(lt) } else { None }
            }
            HirExpr::FieldAccess { object, field, span: _ } => self.infer_field_access_type(object, *field),
            _ => None,
        }
    }

    fn infer_field_access_type(&self, object: &HirExpr, field: StrId) -> Option<HirType> {
        let obj_ty = self.infer_type(object)?;
        match obj_ty {
            HirType::Class(name, _) => {
                let class = self.ctx.classes.get(&name)?;
                class
                    .fields
                    .iter()
                    .find(|f| f.name == field)
                    .map(|f| f.field_type.clone())
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
            Pattern::Tuple(inner) => {
                HirPattern::Tuple(inner.iter().map(|p| self.lower_pattern(p)).collect())
            }
            Pattern::Wildcard => HirPattern::Wildcard,
        }
    }

    fn lower_expr(&mut self, expr: &Expr) -> HirExpr {
        match expr {
            Expr::Call { callee, arguments, .. } => self.lower_expr_call(callee, arguments),
            Expr::FieldAccess { object, field, span } => HirExpr::FieldAccess {
                object: Box::new(self.lower_expr(object)),
                field: field.clone(),
                span: span.clone(),
            },

            Expr::Number { value, .. } => HirExpr::Number(*value),
            Expr::String { value, .. } => HirExpr::String(value.clone()),
            Expr::Boolean { value, .. } => HirExpr::Boolean(*value),
            Expr::Ident { name, .. } => HirExpr::Ident(name.clone()),

            Expr::Decimal { value, .. } => HirExpr::Decimal(*value),

            Expr::Comparison { lhs, op, rhs, span } => HirExpr::Comparison {
                left: Box::new(self.lower_expr(lhs)),
                op: lower_cmp_operator(op.clone()),
                right: Box::new(self.lower_expr(rhs)),
                span: span.clone(),
            },

            Expr::ClassInit { callee, arguments, positional: _, span } => HirExpr::ClassInit {
                name: Box::new(self.lower_expr(callee)),
                args: arguments.iter().map(|a| self.lower_expr(a)).collect(),
                span: span.clone(),
            },

            Expr::Binary { left, op, right, span } => HirExpr::Binary {
                left: Box::new(self.lower_expr(left)),
                op: Self::lower_op(op.clone()),
                right: Box::new(self.lower_expr(right)),
                span: span.clone(),
            },

            Expr::Get { object, field, span } => HirExpr::Get {
                object: Box::new(self.lower_expr(object)),
                field: field.clone(),
                span: span.clone(),
            },

            Expr::Assignment { lhs, op, rhs, span } => HirExpr::Assignment {
                target: Box::new(self.lower_expr(lhs)),
                op: Self::lower_assignment_operator(*op),
                value: Box::new(self.lower_expr(rhs)),
                span: span.clone(),
            },

            Expr::ExprList { expressions: exprs, span } => {
                HirExpr::ExprList { list: exprs.iter().map(|e| self.lower_expr(e)).collect(), span: span.clone() }
            }

            Expr::Char { value: _, span: _ } => todo!(),
            Expr::FieldInit { ident: _, expr: _, .. } => todo!(),
            Expr::If { if_stmt: _, span: _ } |  Expr::Match { match_stmt: _, span: _ } => todo!(),
        }
    }

    fn lower_expr_call(&mut self, callee: &Expr, arguments: &Vec<Expr>) -> HirExpr {
        let lowered_callee = self.lower_expr(&*callee);
        if let Some(value) = self.detect_interface_call(arguments, &lowered_callee) {
            return value;
        }

        HirExpr::Call {
            callee: Box::new(lowered_callee),
            args: arguments.iter().map(|a| self.lower_expr(a)).collect(),
        }
    }

    fn detect_interface_call(
        &mut self,
        arguments: &Vec<Expr>,
        lowered_callee: &HirExpr,
    ) -> Option<HirExpr> {
        let HirExpr::FieldAccess { object, field, span } = lowered_callee else {
            return None;
        };
        let interface = self.find_interface_method(object, *field)?;

        Some(HirExpr::InterfaceCall {
            callee: Box::new(lowered_callee.clone()),
            args: arguments.iter().map(|a| self.lower_expr(a)).collect(),
            interface,
        })
    }

    fn lower_assignment_operator(op: Op) -> AssignmentOperator {
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

    const fn lower_op(op: Op) -> Operator {
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

    fn find_interface_method(&self, object: &HirExpr, method: StrId) -> Option<StrId> {
        // Resolve class name from variable types or direct identifier equal to class name
        let class_name = match object {
            HirExpr::Ident(var) => match self.ctx.variable_types.get(var) {
                Some(HirType::Class(name, _)) => Some(name.clone()),
                _ => {
                    if self.ctx.classes.contains_key(var) {
                        Some(var.clone())
                    } else {
                        None
                    }
                }
            },
            _ => None,
        }?;
        let class = self.ctx.classes.get(&class_name)?;
        for iface_name in &class.interfaces {
            let iface = self.ctx.interfaces.get(iface_name)?;
            if iface.methods.iter().any(|m| m.name == method) {
                return Some(iface_name.clone());
            }
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
            Type::Class { name, generics } => HirType::Class(
                name.clone(),
                generics.iter().map(|g| self.lower_type(g)).collect(),
            ),
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

    // ===============================
    // Monomorphization
    // ===============================
    // Public entry: monomorphize a function with substitutions mapping generic-name -> concrete HirType
    pub fn monomorphize_function(
        &mut self,
        func: &HirFunc,
        substitutions: &HashMap<StrId, HirType, FxHashBuilder>,
    ) -> HirFunc {
        // clone and apply substitutions to signature
        let mut new_func = func.clone();
        self.apply_substitutions_to_func(&mut new_func, substitutions);

        // rename function to specialized name (cache)
        let suffix = self.suffix_for_subs(substitutions);
        let orig_name: StrId = new_func.name.clone();
        let specialized_name = match self
            .ctx
            .instantiated_functions
            .entry((orig_name.clone(), suffix.clone()))
        {
            Entry::Occupied(e) => e.get().clone(),
            Entry::Vacant(e) => {
                const UNDERSCORE_LEN: usize = 1;
                let mut small_vec: SmallVec<[u8; 32]> =
                    SmallVec::with_capacity(orig_name.len() + UNDERSCORE_LEN + suffix.len());
                small_vec.extend_from_slice(
                    self.ctx
                        .context
                        .borrow_mut()
                        .string_pool
                        .resolve_bytes(&*orig_name),
                );
                small_vec.push(b'_');
                small_vec.extend_from_slice(
                    self.ctx
                        .context
                        .borrow_mut()
                        .string_pool
                        .resolve_bytes(&*suffix),
                );

                let new_name = StrId(
                    self.ctx
                        .context
                        .borrow_mut()
                        .string_pool
                        .intern_bytes(small_vec.as_slice()),
                );
                e.insert(new_name);

                // This is a Copy value.
                new_name
            }
        };
        new_func.name = specialized_name.clone();
        self.ctx
            .functions
            .insert(new_func.name.clone(), new_func.clone());

        // monomorphize body if present
        if let Some(body) = &mut new_func.body {
            self.monomorphize_body(body, substitutions);
        }

        new_func
    }

    fn apply_substitutions_to_func(
        &self,
        func: &mut HirFunc,
        substitutions: &HashMap<StrId, HirType, FxHashBuilder>,
    ) {
        for param in &mut func.params {
            match param {
                HirParam::Normal { param_type, .. } => {
                    *param_type = self.substitute_type(param_type, substitutions);
                }
                HirParam::This { param_type } => {
                    let ty = param_type.clone();
                    let ty = ty.as_ref().unwrap();
                    *param_type = Some(self.substitute_type(ty, substitutions));
                }
            }
        }

        if let Some(ret) = &mut func.return_type {
            *ret = self.substitute_type(ret, substitutions);
        }
    }

    fn monomorphize_body(
        &mut self,
        body: &mut HirStmt,
        substitutions: &HashMap<StrId, HirType, FxHashBuilder>,
    ) {
        match body {
            HirStmt::Block { body } => {
                for stmt in body.iter_mut() {
                    self.monomorphize_stmt(stmt, substitutions);
                }
            }
            HirStmt::If {
                cond,
                then_block,
                else_block,
            } => {
                self.monomorphize_expr(cond, substitutions);
                self.monomorphize_body(then_block, substitutions);
                if let Some(e) = else_block {
                    self.monomorphize_body(e, substitutions);
                }
            }
            HirStmt::While { cond, body } => {
                self.monomorphize_expr(cond, substitutions);
                self.monomorphize_body(body, substitutions);
            }
            HirStmt::For {
                init,
                condition,
                increment,
                body,
            } => {
                if let Some(i) = init {
                    self.monomorphize_stmt(i, substitutions);
                }
                if let Some(c) = condition {
                    let mut c_e = c.clone();
                    self.monomorphize_expr(&mut c_e, substitutions);
                    *c = c_e;
                }
                if let Some(inc) = increment {
                    let mut inc_e = inc.clone();
                    self.monomorphize_expr(&mut inc_e, substitutions);
                    *increment = Some(inc_e);
                }
                self.monomorphize_body(body, substitutions);
            }
            HirStmt::Let {
                name: _,
                ty,
                value,
                mutable: _,
            } => {
                *ty = self.substitute_type(ty, substitutions);
                self.monomorphize_expr(value, substitutions);
            }
            HirStmt::Return(opt) => {
                if let Some(e) = opt {
                    self.monomorphize_expr(e, substitutions);
                }
            }
            HirStmt::Expr(e) => {
                self.monomorphize_expr(e, substitutions);
            }
            HirStmt::Match { expr, arms } => {
                self.monomorphize_expr(expr, substitutions);
                for a in arms {
                    self.monomorphize_body(&mut a.body, substitutions);
                }
            }
            HirStmt::UnsafeBlock { body } => {
                self.monomorphize_body(body, substitutions);
            }
            _ => {}
        }
    }

    fn monomorphize_stmt(
        &mut self,
        stmt: &mut HirStmt,
        subs: &HashMap<StrId, HirType, FxHashBuilder>,
    ) {
        self.monomorphize_body(stmt, subs);
    }

    fn monomorphize_expr(
        &mut self,
        expr: &mut HirExpr,
        subs: &HashMap<StrId, HirType, FxHashBuilder>,
    ) {
        match expr {
            HirExpr::Call { callee, args } => {
                self.monomorphize_expr(callee, subs);
                for a in args {
                    self.monomorphize_expr(a, subs);
                }
                // if callee is a constructor that got instantiated, nothing else to do
            }
            HirExpr::InterfaceCall {
                callee,
                interface: _,
                args,
            } => {
                self.monomorphize_expr(callee, subs);
                for a in args.iter_mut() {
                    self.monomorphize_expr(a, subs);
                }

                // Extract the field access: object.method
                if let HirExpr::FieldAccess { object, field, span } = &**callee {
                    if let Some(HirType::Class(class_name, class_args)) = self.infer_type(object) {
                        if let Some(method_name) =
                            self.direct_method_lookup(&class_name, &class_args, field)
                        {
                            *expr = HirExpr::Call {
                                callee: Box::new(HirExpr::Ident(method_name)),
                                args: args.clone(),
                            };
                        }
                    }
                }
            }
            HirExpr::ClassInit { name, args, span } => {
                // monomorphize args first
                for a in args.iter_mut() {
                    self.monomorphize_expr(a, subs);
                }

                // if name is identifier of generic class (e.g. Foo) and args/types allow instantiation, instantiate
                if let HirExpr::Ident(id) = &**name {
                    let Some(base_class) = self.ctx.classes.get(id) else {
                        return;
                    };
                    if base_class.generics.is_empty() {
                        return;
                    }
                    let mut concrete_args = Vec::new();
                    for a in args.iter() {
                        let inferred = self.infer_type(a).unwrap_or(HirType::Generic(StrId(
                            self.ctx.context.borrow_mut().string_pool.intern("Unknown"),
                        )));
                        let substituted = self.substitute_type(&inferred, subs);
                        concrete_args.push(substituted);
                    }

                    if concrete_args.len() < base_class.generics.len() {
                        base_class
                            .generics
                            .iter()
                            .skip(concrete_args.len())
                            .map(|g| HirType::Generic(g.name))
                            .for_each(|name| concrete_args.push(name));
                    }

                    let new_name_id = self.instantiate_class_for_types(*id, &concrete_args);
                    **name = HirExpr::Ident(new_name_id);
                }
            }
            HirExpr::FieldAccess { object, .. } | HirExpr::Get { object, .. } => {
                self.monomorphize_expr(object, subs);
            }
            HirExpr::Binary { left, right, .. } => {
                self.monomorphize_expr(left, subs);
                self.monomorphize_expr(right, subs);
            }
            HirExpr::Assignment { target, value, .. } => {
                self.monomorphize_expr(target, subs);
                self.monomorphize_expr(value, subs);
            }
            HirExpr::ExprList { list, .. } => {
                for e in list {
                    self.monomorphize_expr(e, subs);
                }
            }
            HirExpr::Comparison { left, right, .. } => {
                self.monomorphize_expr(left, subs);
                self.monomorphize_expr(right, subs);
            }
            _ => {}
        }
    }

    // Tries to resolve an interface call into a direct call to the implementing class' method.
    // Returns Some(HirExpr::Call) on success.
    fn direct_method_lookup(
        &mut self,
        class_name: &StrId,
        class_args: &[HirType],
        method_name: &StrId,
    ) -> Option<StrId> {
        // Instantiate if there are generic args
        let concrete_name = if class_args.is_empty() {
            *class_name
        } else {
            self.instantiate_class_for_types(*class_name, class_args)
        };

        let class = self.ctx.classes.get(&concrete_name)?;

        class
            .methods
            .iter()
            .find(|m| m.name == *method_name)
            .map(|m| m.name)
    }

    // --------------------
    // Utilities for monomorphization
    // --------------------

    // Create a StrId suffix for a set of substitutions, e.g. {"T": i32} -> "T_i32" or "i32" depending on mapping.
    // Create a StrId suffix for a set of substitutions, e.g. {"T": i32} -> "T_i32" or "i32" depending on mapping.
    fn suffix_for_subs(&mut self, subs: &HashMap<StrId, HirType, FxHashBuilder>) -> StrId {
        // Collect into Vec<(StrId, StrId)> so we can sort deterministically.
        let mut pieces: Vec<(StrId, StrId)> = subs
            .iter()
            .map(|(k, v)| {
                // We need to get a suffix for the type using the StringPool.
                // Acquire the pool once (RefMut) to avoid multiple mutable borrows later.
                let mut ctx_ref = self.ctx.context.borrow_mut();
                let suffix = type_suffix_with_pool(&mut ctx_ref.string_pool, v);
                (*k, suffix)
            })
            .collect();

        // sort deterministically by key bytes so result is stable across hashmap order
        pieces.sort_by_key(|(k, _)| {
            // safely look up bytes using a temporary borrow of context
            let ctx_borrow = self.ctx.context.borrow();
            ctx_borrow.string_pool.resolve_string(k).to_string()
        });

        // Build buffer
        let mut buf: SmallVec<[u8; 128]> = SmallVec::new();

        {
            // borrow pool once for resolving names to bytes
            let ctx_ref = self.ctx.context.borrow();
            let pool = &ctx_ref.string_pool;

            for (i, (k, v)) in pieces.iter().enumerate() {
                if i > 0 {
                    buf.push(b'_');
                }
                buf.extend_from_slice(pool.resolve_bytes(&*k));
                buf.extend_from_slice(pool.resolve_bytes(&*v));
            }
        }

        // Convert &[u8] -> &str (we built it from valid UTF-8 interned strings)
        let s = unsafe { std::str::from_utf8_unchecked(&buf) };

        // Intern the final suffix using a mutable borrow of the pool
        let mut ctx_ref = self.ctx.context.borrow_mut();
        StrId(ctx_ref.string_pool.intern(s))
    }

    // Substitute occurrences of Generic types with concrete ones
    fn substitute_type(
        &self,
        ty: &HirType,
        subs: &HashMap<StrId, HirType, FxHashBuilder>,
    ) -> HirType {
        match ty {
            HirType::Generic(name) => subs.get(name).cloned().unwrap_or(ty.clone()),
            HirType::Class(name, args) => {
                if args.is_empty() {
                    HirType::Class(name.clone(), vec![])
                } else {
                    HirType::Class(
                        name.clone(),
                        args.iter().map(|a| self.substitute_type(a, subs)).collect(),
                    )
                }
            }
            HirType::Interface(name, args) => HirType::Interface(
                name.clone(),
                args.iter().map(|a| self.substitute_type(a, subs)).collect(),
            ),
            HirType::Enum(name, args) => HirType::Enum(
                name.clone(),
                args.iter().map(|a| self.substitute_type(a, subs)).collect(),
            ),
            HirType::Lambda {
                params,
                return_type,
                concurrent,
            } => HirType::Lambda {
                params: params
                    .iter()
                    .map(|p| self.substitute_type(p, subs))
                    .collect(),
                return_type: Box::new(self.substitute_type(return_type, subs)),
                concurrent: *concurrent,
            },
            other => other.clone(),
        }
    }

    // Instantiate a generic class for concrete args, caching the result and registering the new class in ctx.classes
    // Instantiate a generic class for concrete args, caching the result and registering the new class in ctx.classes.
    pub fn instantiate_class_for_types(&mut self, base: StrId, concrete_args: &[HirType]) -> StrId {
        let base = self.ctx.classes.get(&base).unwrap();
        // 1) Build mapping from generic params -> concrete args
        let mut map: HashMap<StrId, HirType, FxHashBuilder> = HashMap::with_hasher(FxHashBuilder);
        for (i, generic) in base.generics.iter().enumerate() {
            let ty = concrete_args
                .get(i)
                .cloned()
                .unwrap_or_else(|| HirType::Generic(generic.name));
            map.insert(generic.name, ty);
        }

        // 2) Clone and specialize base class
        let mut new_class = base.clone();

        // 3) Specialize fields by substituting generics
        for field in &mut new_class.fields {
            field.field_type = self.substitute_type(&field.field_type, &map);
        }

        // 4) Drop generics on the specialized class
        new_class.generics.clear();

        {
            let mut ctx_ref = self.ctx.context.borrow_mut();
            let pool = &mut ctx_ref.string_pool;

            let interned = Self::instantiate_class_name(concrete_args, base, pool);
            new_class.name = interned;

            self.ctx.classes.insert(interned, new_class.clone());

            interned
        }
    }

    fn instantiate_class_name(concrete_args: &[HirType], base: &HirClass, pool: &mut &mut StringPool) -> StrId {
        let mut buf: SmallVec<[u8; 128]> = SmallVec::new();
        buf.extend_from_slice(pool.resolve_bytes(&*base.name));
        for arg in concrete_args {
            buf.push(b'_');
            let suf = type_suffix_with_pool(pool, arg);
            buf.extend_from_slice(pool.resolve_bytes(&*suf));
        }
        let s = unsafe { std::str::from_utf8_unchecked(&buf) };
        let interned = StrId(pool.intern(s));
        interned
    }
}

const fn lower_visibility(visibility: &ast::Visibility) -> hir::Visibility {
    match visibility {
        ast::Visibility::Public => hir::Visibility::Public,
        ast::Visibility::Private => hir::Visibility::Private,
        ast::Visibility::Module => hir::Visibility::Module,
        ast::Visibility::Package => hir::Visibility::Package,
    }
}

const fn lower_cmp_operator(op: Op) -> Operator {
    match op {
        Op::Eq => Operator::Equals,
        Op::Neq => Operator::NotEquals,
        Op::Lt => Operator::LessThan,
        Op::Lte => Operator::LessThanOrEqual,
        Op::Gt => Operator::GreaterThan,
        Op::Gte => Operator::GreaterThanOrEqual,
        _ => unreachable!(),
    }
}

pub fn type_suffix_with_pool(pool: &mut StringPool, ty: &HirType) -> StrId {
    StrId(match ty {
        HirType::I32 => pool.intern("i32"),
        HirType::I64 => pool.intern("i64"),
        HirType::U32 => pool.intern("u32"),
        HirType::U64 => pool.intern("u64"),
        HirType::F32 => pool.intern("f32"),
        HirType::F64 => pool.intern("f64"),
        HirType::String => pool.intern("String"),
        HirType::Boolean => pool.intern("boolean"),

        HirType::Class(name, args) => {
            if args.is_empty() {
                **name
            } else {
                let mut buf: SmallVec<[u8; 128]> = SmallVec::new();
                buf.extend_from_slice(pool.resolve_bytes(&*name));
                for arg in args {
                    buf.push(b'_');
                    let suffix = type_suffix_with_pool(pool, arg);
                    let part = pool.resolve_bytes(&*suffix);

                    buf.extend_from_slice(part);
                }
                let s = unsafe { std::str::from_utf8_unchecked(&buf) };
                pool.intern(s)
            }
        }

        HirType::Generic(name) => **name,

        HirType::Void => pool.intern("void"),

        _ => pool.intern("T"),
    })
}
