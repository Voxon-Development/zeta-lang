use crate::ast::{Expr, FuncSafety, InlineModifier, Stmt, Type, Visibility};
use crate::hir::{
    Hir, HirEnum, HirExpr, HirFunc, HirInterface, HirModule, HirParam, HirStmt, HirStruct, HirType,
    Operator, StrId,
};
use std::fmt::{self, Write};
use std::sync::Arc;
use zetaruntime::string_pool::StringPool;

pub struct IrPrettyPrinter {
    string_pool: Arc<StringPool>,
    indent_level: usize,
    indent_size: usize,
}

impl IrPrettyPrinter {
    pub fn new(string_pool: Arc<StringPool>) -> Self {
        Self {
            string_pool,
            indent_level: 0,
            indent_size: 2,
        }
    }

    /// Set the indentation size (default is 2 spaces)
    pub fn with_indent_size(mut self, size: usize) -> Self {
        self.indent_size = size;
        self
    }

    /// Resolve a StrId to its string representation
    fn resolve_str(&self, str_id: StrId) -> &str {
        self.string_pool.resolve_string(&str_id.0)
    }

    /// Get current indentation string
    fn indent(&self) -> String {
        " ".repeat(self.indent_level * self.indent_size)
    }

    /// Increase indentation level
    fn push_indent(&mut self) {
        self.indent_level += 1;
    }

    /// Decrease indentation level
    fn pop_indent(&mut self) {
        if self.indent_level > 0 {
            self.indent_level -= 1;
        }
    }

    pub fn format_hir_module(&mut self, module: &HirModule<'_, '_>) -> Result<String, fmt::Error> {
        let mut output = String::new();

        writeln!(output, "// HIR Module: {}", self.resolve_str(module.name))?;
        writeln!(output)?;

        // Format imports
        if !module.imports.is_empty() {
            for import in module.imports {
                writeln!(output, "{}import {};", self.indent(), import)?;
            }
            writeln!(output)?;
        }

        for item in module.items {
            self.format_hir_item(&mut output, item)?;
            writeln!(output)?;
        }

        Ok(output)
    }

    fn format_hir_item(
        &mut self,
        output: &mut String,
        item: &Hir<'_, '_>,
    ) -> Result<(), fmt::Error> {
        match item {
            Hir::Func(func) => self.format_hir_function(output, func),
            Hir::Struct(struct_def) => self.format_hir_struct(output, struct_def),
            Hir::Enum(enum_def) => self.format_hir_enum(output, enum_def),
            Hir::Interface(interface_def) => self.format_hir_interface(output, interface_def),
            _ => {
                writeln!(output, "{}// Other HIR item", self.indent())?;
                Ok(())
            }
        }
    }

    fn format_hir_function(
        &mut self,
        output: &mut String,
        func: &HirFunc<'_, '_>,
    ) -> Result<(), fmt::Error> {
        let modifiers = func.function_metadata;

        let visibility = match modifiers.visibility {
            Visibility::Public => "public ",
            Visibility::Private => "private ",
            Visibility::Module => "module ",
            Visibility::Internal => "package ",
        };

        let unsafe_kw = match modifiers.func_safety {
            FuncSafety::Safe => "",
            FuncSafety::Unsafe => "unsafe ",
        };

        let inline_attr = match modifiers.inline_modifier {
            InlineModifier::Inline => "inline ",
            InlineModifier::Noinline => "noinline ",
            InlineModifier::None => "",
        };

        write!(
            output,
            "{}{}{}{}fn {}",
            self.indent(),
            visibility,
            unsafe_kw,
            inline_attr,
            self.resolve_str(func.name)
        )?;

        if let Some(generics) = func.generics {
            write!(output, "<")?;
            for (i, generic) in generics.iter().enumerate() {
                if i > 0 {
                    write!(output, ", ")?;
                }
                write!(output, "{}", self.resolve_str(generic.name))?;
            }
            write!(output, ">")?;
        }

        write!(output, "(")?;
        if let Some(params) = func.params {
            for (i, param) in params.iter().enumerate() {
                if i > 0 {
                    write!(output, ", ")?;
                }
                match param {
                    HirParam::Normal {
                        name,
                        param_type,
                        span: _,
                    } => {
                        write!(output, "{}: ", self.resolve_str(*name))?;
                        self.format_hir_type(output, param_type)?;
                    }
                    HirParam::This { kind, span: _ } => {
                        write!(output, "{}", kind)?;
                    }
                }
            }
        }
        write!(output, ")")?;

        if let Some(return_type) = &func.return_type {
            write!(output, " -> ")?;
            self.format_hir_type(output, return_type)?;
        }

        if let Some(body) = &func.body {
            writeln!(output, " {{")?;
            self.push_indent();
            // For function bodies, if it's a Block, print the statements directly
            // to avoid double nesting
            match body {
                HirStmt::Block { body: statements } => {
                    for stmt in *statements {
                        self.format_hir_statement(output, stmt)?;
                    }
                }
                _ => {
                    self.format_hir_statement(output, body)?;
                }
            }
            self.pop_indent();
            writeln!(output, "{}}}", self.indent())?;
        } else {
            writeln!(output, ";")?;
        }

        Ok(())
    }

    fn format_hir_struct(
        &mut self,
        output: &mut String,
        struct_def: &HirStruct<'_, '_>,
    ) -> Result<(), fmt::Error> {
        let visibility = match struct_def.visibility {
            Visibility::Public => "public ",
            Visibility::Private => "private ",
            Visibility::Module => "module ",
            Visibility::Internal => "package ",
        };

        write!(
            output,
            "{}{}struct {}",
            self.indent(),
            visibility,
            self.resolve_str(struct_def.name)
        )?;

        if let Some(generics) = struct_def.generics {
            write!(output, "<")?;
            for (i, generic) in generics.iter().enumerate() {
                if i > 0 {
                    write!(output, ", ")?;
                }
                write!(output, "{}", self.resolve_str(generic.name))?;
            }
            write!(output, ">")?;
        }

        writeln!(output, " {{")?;
        self.push_indent();

        for field in struct_def.fields {
            let field_visibility = match field.visibility {
                Visibility::Public => "public ",
                Visibility::Private => "private ",
                Visibility::Module => "module ",
                Visibility::Internal => "package ",
            };
            write!(
                output,
                "{}{}{}: ",
                self.indent(),
                field_visibility,
                self.resolve_str(field.name)
            )?;
            self.format_hir_type(output, &field.field_type)?;
            writeln!(output, ",")?;
        }

        self.pop_indent();
        writeln!(output, "{}}}", self.indent())?;

        Ok(())
    }

    fn format_hir_enum(
        &mut self,
        output: &mut String,
        enum_def: &HirEnum<'_, '_>,
    ) -> Result<(), fmt::Error> {
        let visibility = match enum_def.visibility {
            Visibility::Public => "public ",
            Visibility::Private => "private ",
            Visibility::Module => "module ",
            Visibility::Internal => "package ",
        };

        write!(
            output,
            "{}{}enum {}",
            self.indent(),
            visibility,
            self.resolve_str(enum_def.name)
        )?;

        if let Some(generics) = enum_def.generics {
            write!(output, "<")?;
            for (i, generic) in generics.iter().enumerate() {
                if i > 0 {
                    write!(output, ", ")?;
                }
                write!(output, "{}", self.resolve_str(generic.name))?;
            }
            write!(output, ">")?;
        }

        writeln!(output, " {{")?;
        self.push_indent();

        for variant in enum_def.variants {
            write!(
                output,
                "{}{}",
                self.indent(),
                self.resolve_str(variant.name)
            )?;
            let fields = variant.fields;
            if !fields.is_empty() {
                write!(output, "(")?;
                for (i, field) in fields.iter().enumerate() {
                    if i > 0 {
                        write!(output, ", ")?;
                    }
                    self.format_hir_type(output, &field.field_type)?;
                }
                write!(output, ")")?;
            }
            writeln!(output, ",")?;
        }

        self.pop_indent();
        writeln!(output, "{}}}", self.indent())?;

        Ok(())
    }

    fn format_hir_interface(
        &mut self,
        output: &mut String,
        interface_def: &HirInterface<'_, '_>,
    ) -> Result<(), fmt::Error> {
        let visibility = match interface_def.visibility {
            Visibility::Public => "public ",
            Visibility::Private => "private ",
            Visibility::Module => "module ",
            Visibility::Internal => "package ",
        };

        write!(
            output,
            "{}{}trait {}",
            self.indent(),
            visibility,
            self.resolve_str(interface_def.name)
        )?;

        if let Some(generics) = interface_def.generics {
            write!(output, "<")?;
            for (i, generic) in generics.iter().enumerate() {
                if i > 0 {
                    write!(output, ", ")?;
                }
                write!(output, "{}", self.resolve_str(generic.name))?;
            }
            write!(output, ">")?;
        }

        writeln!(output, " {{")?;
        self.push_indent();

        // Methods
        if let Some(methods) = interface_def.methods {
            for method in methods {
                self.format_hir_function(output, method)?;
                writeln!(output)?;
            }
        }

        self.pop_indent();
        writeln!(output, "{}}}", self.indent())?;

        Ok(())
    }

    fn format_hir_statement(
        &mut self,
        output: &mut String,
        stmt: &HirStmt<'_, '_>,
    ) -> Result<(), fmt::Error> {
        match stmt {
            HirStmt::Let {
                name, ty, value, ..
            } => {
                write!(output, "{}let {}: ", self.indent(), self.resolve_str(*name))?;
                self.format_hir_type(output, ty)?;
                write!(output, " = ")?;
                self.format_hir_expression(output, value)?;
                writeln!(output, ";")?;
            }
            HirStmt::Return(expr_opt) => {
                write!(output, "{}return", self.indent())?;
                if let Some(expr) = expr_opt {
                    write!(output, " ")?;
                    self.format_hir_expression(output, expr)?;
                }
                writeln!(output, ";")?;
            }
            HirStmt::Expr(expr) => {
                write!(output, "{}", self.indent())?;
                self.format_hir_expression(output, expr)?;
                writeln!(output, ";")?;
            }
            HirStmt::Block { body } => {
                writeln!(output, "{{{}", self.indent())?;
                self.push_indent();
                for stmt in *body {
                    self.format_hir_statement(output, stmt)?;
                }
                self.pop_indent();
                writeln!(output, "{}}}", self.indent())?;
            }
            HirStmt::If {
                cond,
                then_block,
                else_block,
            } => {
                write!(output, "{}if ", self.indent())?;
                self.format_hir_expression(output, cond)?;
                writeln!(output, " {{")?;
                self.push_indent();
                for stmt in *then_block {
                    self.format_hir_statement(output, stmt)?;
                }
                self.pop_indent();
                write!(output, "{}}}", self.indent())?;

                if let Some(else_stmt) = else_block {
                    writeln!(output, " else {{")?;
                    self.push_indent();
                    self.format_hir_statement(output, else_stmt)?;
                    self.pop_indent();
                    writeln!(output, "{}}}", self.indent())?;
                } else {
                    writeln!(output)?;
                }
            }
            HirStmt::While { cond, body } => {
                write!(output, "{}while ", self.indent())?;
                self.format_hir_expression(output, cond)?;
                writeln!(output, " {{")?;
                self.push_indent();
                self.format_hir_statement(output, body)?;
                self.pop_indent();
                writeln!(output, "{}}}", self.indent())?;
            }
            _ => {
                writeln!(output, "{}// Other statement", self.indent())?;
            }
        }
        Ok(())
    }

    fn format_hir_expression(
        &mut self,
        output: &mut String,
        expr: &HirExpr<'_, '_>,
    ) -> Result<(), fmt::Error> {
        match expr {
            HirExpr::Ident(name, _) => {
                write!(output, "{}", self.resolve_str(*name))?;
            }
            HirExpr::Number(n, _) => {
                write!(output, "{}", n)?;
            }
            HirExpr::String(s, _) => {
                write!(output, "\"{}\"", self.resolve_str(*s))?;
            }
            HirExpr::Boolean(b, _) => {
                write!(output, "{}", b)?;
            }
            HirExpr::StructInit { name, args, .. } => {
                self.format_hir_expression(output, name)?;
                write!(output, " {{")?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(output, ", ")?;
                    }
                    write!(output, "{}: ", arg.name)?;
                    self.format_hir_expression(output, &arg.value)?;
                }
                write!(output, "}}")?;
            }
            HirExpr::Call { callee, args, .. } => {
                self.format_hir_expression(output, callee)?;
                write!(output, "(")?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(output, ", ")?;
                    }
                    self.format_hir_expression(output, arg)?;
                }
                write!(output, ")")?;
            }
            HirExpr::FieldAccess { object, field, .. } => {
                self.format_hir_expression(output, object)?;
                write!(output, ".{}", self.resolve_str(*field))?;
            }
            HirExpr::Binary {
                left, op, right, ..
            } => {
                self.format_hir_expression(output, left)?;
                write!(output, " {} ", self.format_operator(*op))?;
                self.format_hir_expression(output, right)?;
            }
            HirExpr::Assignment { target, value, .. } => {
                self.format_hir_expression(output, target)?;
                write!(output, " = ")?;
                self.format_hir_expression(output, value)?;
            }
            HirExpr::Get { object, field, .. } => {
                self.format_hir_expression(output, object)?;
                write!(output, ".{}", self.resolve_str(*field))?;
            }
            _ => {
                write!(output, "/* expr */")?;
            }
        }
        Ok(())
    }

    fn format_hir_type(
        &mut self,
        output: &mut String,
        ty: &HirType<'_, '_>,
    ) -> Result<(), fmt::Error> {
        match ty {
            HirType::I8 => write!(output, "i8"),
            HirType::I16 => write!(output, "i16"),
            HirType::I32 => write!(output, "i32"),
            HirType::I64 => write!(output, "i64"),
            HirType::U8 => write!(output, "u8"),
            HirType::U16 => write!(output, "u16"),
            HirType::U32 => write!(output, "u32"),
            HirType::U64 => write!(output, "u64"),
            HirType::F32 => write!(output, "f32"),
            HirType::F64 => write!(output, "f64"),
            HirType::Boolean => write!(output, "bool"),
            HirType::String => write!(output, "String"),
            HirType::Void => write!(output, "void"),
            HirType::Struct {
                name,
                field_types: _,
                type_args: generics,
            } => {
                write!(output, "{}", self.resolve_str(*name))?;
                if !generics.is_empty() {
                    write!(output, "<")?;
                    for (i, generic) in generics.iter().enumerate() {
                        if i > 0 {
                            write!(output, ", ")?;
                        }
                        self.format_hir_type(output, generic)?;
                    }
                    write!(output, ">")?;
                }
                Ok(())
            }
            HirType::DynInterface(name, generics) => {
                write!(output, "dyn {}", self.resolve_str(*name))?;
                if !generics.is_empty() {
                    write!(output, "<")?;
                    for (i, generic) in generics.iter().enumerate() {
                        if i > 0 {
                            write!(output, ", ")?;
                        }
                        self.format_hir_type(output, generic)?;
                    }
                    write!(output, ">")?;
                }
                Ok(())
            }
            HirType::Generic(name) => {
                write!(output, "{}", self.resolve_str(*name))
            }
            HirType::Lambda {
                params,
                return_type,
            } => {
                write!(output, "fn(")?;
                for (i, param) in params.iter().enumerate() {
                    if i > 0 {
                        write!(output, ", ")?;
                    }
                    self.format_hir_type(output, param)?;
                }
                write!(output, ")")?;

                write!(output, " -> ")?;
                self.format_hir_type(output, return_type)?;
                Ok(())
            }
            _ => write!(output, "/* type */"),
        }
    }

    fn format_operator(&self, op: Operator) -> &'static str {
        match op {
            Operator::Add => "+",
            Operator::Subtract => "-",
            Operator::Multiply => "*",
            Operator::Divide => "/",
            Operator::Modulo => "%",
            Operator::BitAnd => "&",
            Operator::BitOr => "|",
            Operator::BitXor => "^",
            Operator::ShiftLeft => "<<",
            Operator::ShiftRight => ">>",
            Operator::Assign => "=",
            Operator::AddAssign => "+=",
            Operator::SubtractAssign => "-=",
            Operator::MultiplyAssign => "*=",
            Operator::DivideAssign => "/=",
            Operator::ModuloAssign => "%=",
            Operator::BitAndAssign => "&=",
            Operator::BitOrAssign => "|=",
            Operator::BitXorAssign => "^=",
            Operator::ShiftLeftAssign => "<<=",
            Operator::ShiftRightAssign => ">>=",
            Operator::BitNot => "~",
            Operator::LogicalOr => "||",
            Operator::Equals => "==",
            Operator::NotEquals => "!=",
            Operator::GreaterThan => ">",
            Operator::LessThan => "<",
            Operator::GreaterThanOrEqual => ">=",
            Operator::LessThanOrEqual => "<=",
            Operator::LogicalAnd => "&&",
            Operator::DerefUnsafe => "[*]",
            Operator::Deref => "*",
            Operator::Ref => "&",
            Operator::RefMut => "&mut ",
        }
    }

    pub fn format_ast_statements(
        &mut self,
        statements: &[Stmt<'_, '_>],
    ) -> Result<String, fmt::Error> {
        let mut output = String::new();

        writeln!(output, "// AST Statements")?;
        writeln!(output)?;

        for stmt in statements {
            self.format_ast_statement(&mut output, stmt)?;
        }

        Ok(output)
    }

    fn format_ast_statement(
        &mut self,
        output: &mut String,
        stmt: &Stmt<'_, '_>,
    ) -> Result<(), fmt::Error> {
        match stmt {
            Stmt::Let(let_stmt) => {
                write!(
                    output,
                    "{}let {}: ",
                    self.indent(),
                    self.resolve_str(let_stmt.ident)
                )?;
                self.format_ast_type(output, &let_stmt.type_annotation)?;
                write!(output, " = ")?;
                self.format_ast_expression(output, let_stmt.value)?;
                writeln!(output, ";")?;
            }
            Stmt::Return(return_stmt) => {
                write!(output, "{}return", self.indent())?;
                if let Some(expr) = return_stmt.value {
                    write!(output, " ")?;
                    self.format_ast_expression(output, expr)?;
                }
                writeln!(output, ";")?;
            }
            Stmt::FuncDecl(func_decl) => {
                write!(
                    output,
                    "{}fn {}",
                    self.indent(),
                    self.resolve_str(func_decl.name)
                )?;
                writeln!(output, " {{ /* function body */ }}")?;
            }
            Stmt::StructDecl(struct_decl) => {
                write!(
                    output,
                    "{}struct {}",
                    self.indent(),
                    self.resolve_str(struct_decl.name)
                )?;
                writeln!(output, " {{ /* struct body */ }}")?;
            }
            Stmt::Break(expr, _span) => {
                write!(output, "{}break", self.indent())?;
                if let Some(expr) = expr {
                    write!(output, " ")?;
                    self.format_ast_expression(output, expr)?;
                }
                writeln!(output, ";")?;
            }
            Stmt::Continue(_span) => {
                writeln!(output, "{}continue;", self.indent())?;
            }
            _ => {
                writeln!(output, "{}// Other AST statement", self.indent())?;
            }
        }
        Ok(())
    }

    fn format_ast_expression(
        &mut self,
        output: &mut String,
        _expr: &Expr<'_, '_>,
    ) -> Result<(), fmt::Error> {
        // This would need to be implemented based on the actual Expr enum
        // For now, just a placeholder
        write!(output, "/* ast expr */")?;
        Ok(())
    }

    fn format_ast_type(
        &mut self,
        output: &mut String,
        _ty: &Type<'_, '_>,
    ) -> Result<(), fmt::Error> {
        // This would need to be implemented based on the actual Type enum
        // For now, just a placeholder
        write!(output, "/* ast type */")?;
        Ok(())
    }
}

impl IrPrettyPrinter {
    pub fn hir_to_string(
        string_pool: Arc<StringPool>,
        module: &HirModule<'_, '_>,
    ) -> Result<String, fmt::Error> {
        let mut printer = IrPrettyPrinter::new(string_pool);
        printer.format_hir_module(module)
    }

    pub fn ast_to_string(
        string_pool: Arc<StringPool>,
        statements: &[Stmt<'_, '_>],
    ) -> Result<String, fmt::Error> {
        let mut printer = IrPrettyPrinter::new(string_pool);
        printer.format_ast_statements(statements)
    }
}
