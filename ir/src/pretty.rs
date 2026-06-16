use crate::ast::{Expr, FuncSafety, InlineModifier, Stmt, Type, Visibility};
use crate::hir::{
    Hir, HirEnum, HirExpr, HirFunc, HirInterface, HirModule, HirParam, HirStmt, HirStruct, HirType,
    Operator, StrId,
};
use crate::ssa_ir::{BinOp, Function, Instruction, Module, Operand, SsaType, UnOp};
use std::fmt::{self, Write};
use std::sync::Arc;
use zetaruntime::string_pool::StringPool;

/// Pretty printer for Zeta IR with string pool resolution
pub struct IrPrettyPrinter {
    string_pool: Arc<StringPool>,
    indent_level: usize,
    indent_size: usize,
}

impl IrPrettyPrinter {
    /// Create a new pretty printer with the given string pool
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

    /// Format HIR module as readable text
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

        // Format items
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

        // Generics
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

        // Parameters
        write!(output, "(")?;
        if let Some(params) = func.params {
            for (i, param) in params.iter().enumerate() {
                if i > 0 {
                    write!(output, ", ")?;
                }
                match param {
                    HirParam::Normal { name, param_type } => {
                        write!(output, "{}: ", self.resolve_str(*name))?;
                        self.format_hir_type(output, param_type)?;
                    }
                    HirParam::This { kind } => {
                        write!(output, "{}", kind)?;
                    }
                }
            }
        }
        write!(output, ")")?;

        // Return type
        if let Some(return_type) = &func.return_type {
            write!(output, " -> ")?;
            self.format_hir_type(output, return_type)?;
        }

        // Body
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

        // Generics
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

        // Fields
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

        // Generics
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

        // Variants
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

        // Generics
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
            HirExpr::Ident(name) => {
                write!(output, "{}", self.resolve_str(*name))?;
            }
            HirExpr::Number(n) => {
                write!(output, "{}", n)?;
            }
            HirExpr::String(s) => {
                write!(output, "\"{}\"", self.resolve_str(*s))?;
            }
            HirExpr::Boolean(b) => {
                write!(output, "{}", b)?;
            }
            HirExpr::StructInit { name, args, .. } => {
                self.format_hir_expression(output, name)?;
                write!(output, " {{")?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(output, ", ")?;
                    }
                    self.format_hir_expression(output, arg)?;
                }
                write!(output, "}}")?;
            }
            HirExpr::Call { callee, args } => {
                println!("{:?}", callee);
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
            HirType::Struct(name, generics) => {
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
            HirType::Interface(name, generics) => {
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
                concurrent: _,
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

    /// Format SSA IR module as readable text
    pub fn format_ssa_module(&mut self, module: &Module<'_, '_>) -> Result<String, fmt::Error> {
        let mut output = String::new();

        writeln!(output, "// SSA IR Module")?;
        writeln!(output)?;

        // Format functions
        for (name, function) in &module.functions {
            writeln!(output, "// Function: {}", self.resolve_str(*name))?;
            self.format_ssa_function(&mut output, function)?;
            writeln!(output)?;
        }

        Ok(output)
    }

    fn format_ssa_function(
        &mut self,
        output: &mut String,
        func: &Function,
    ) -> Result<(), fmt::Error> {
        writeln!(
            output,
            "{}fn {}(",
            self.indent(),
            self.resolve_str(func.name)
        )?;
        self.push_indent();

        // Parameters
        for (i, param) in func.params.iter().enumerate() {
            if i > 0 {
                writeln!(output, ",")?;
            }
            write!(output, "{}{}: ", self.indent(), param.0.0)?;
            self.format_ssa_type(output, &param.1)?;
        }

        self.pop_indent();
        write!(output, "\n{}) -> ", self.indent())?;
        self.format_ssa_type(output, &func.ret_type)?;
        writeln!(output, " {{")?;

        self.push_indent();

        // Basic blocks
        for block in &func.blocks {
            writeln!(output, "{}bb{}:", self.indent(), block.id.0)?;
            self.push_indent();

            for instruction in &block.instructions {
                self.format_ssa_instruction(output, instruction)?;
            }

            self.pop_indent();
        }

        self.pop_indent();
        writeln!(output, "{}}}", self.indent())?;

        Ok(())
    }

    fn format_ssa_instruction(
        &mut self,
        output: &mut String,
        instr: &Instruction,
    ) -> Result<(), fmt::Error> {
        match instr {
            Instruction::Binary {
                dest,
                op,
                left,
                right,
            } => {
                writeln!(
                    output,
                    "{}{} = {} {} {}",
                    self.indent(),
                    dest.0,
                    self.format_operand(left),
                    self.format_binop(*op),
                    self.format_operand(right)
                )?;
            }
            Instruction::Unary { dest, op, operand } => {
                writeln!(
                    output,
                    "{}{} = {} {}",
                    self.indent(),
                    dest.0,
                    self.format_unop(*op),
                    self.format_operand(operand)
                )?;
            }
            Instruction::Phi {
                dest,
                incoming: incomings,
            } => {
                write!(output, "{}{} = phi [", self.indent(), dest.0)?;
                for (i, (block_id, value)) in incomings.iter().enumerate() {
                    if i > 0 {
                        write!(output, ", ")?;
                    }
                    write!(output, "bb{}: {}", block_id.0, value.0)?;
                }
                writeln!(output, "]")?;
            }
            Instruction::Call { dest, func, args } => {
                if let Some(target) = dest {
                    write!(output, "{}{} = ", self.indent(), target.0)?;
                } else {
                    write!(output, "{}", self.indent())?;
                }
                write!(output, "call {}(", self.format_operand(func))?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(output, ", ")?;
                    }
                    write!(output, "{}", self.format_operand(arg))?;
                }
                writeln!(output, ")")?;
            }
            Instruction::ClassCall {
                dest,
                object,
                method_id,
                args,
            } => {
                if let Some(target) = dest {
                    write!(output, "{}{} = ", self.indent(), target.0)?;
                } else {
                    write!(output, "{}", self.indent())?;
                }
                write!(output, "call_method {}.method{}(", object.0, method_id)?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(output, ", ")?;
                    }
                    write!(output, "{}", self.format_operand(arg))?;
                }
                writeln!(output, ")")?;
            }
            Instruction::InterfaceDispatch {
                dest,
                object,
                method_slot,
                args,
            } => {
                if let Some(target) = dest {
                    write!(output, "{}{} = ", self.indent(), target.0)?;
                } else {
                    write!(output, "{}", self.indent())?;
                }
                write!(
                    output,
                    "call_interface {}.vtable[{}](",
                    object.0, method_slot
                )?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(output, ", ")?;
                    }
                    write!(output, "{}", self.format_operand(arg))?;
                }
                writeln!(output, ")")?;
            }
            Instruction::UpcastToInterface {
                dest,
                object,
                interface_id,
            } => {
                writeln!(
                    output,
                    "{}{} = upcast {} to interface#{}",
                    self.indent(),
                    dest.0,
                    object.0,
                    interface_id
                )?;
            }
            Instruction::StackAlloc { dest, ty, count } => {
                writeln!(
                    output,
                    "{}{} = alloc {} [{}]",
                    self.indent(),
                    dest.0,
                    self.format_ssa_type_inline(ty),
                    count
                )?;
            }
            Instruction::StoreField {
                base,
                offset,
                value,
            } => {
                writeln!(
                    output,
                    "{}store {}.field[{}] = {}",
                    self.indent(),
                    self.format_operand(base),
                    offset,
                    self.format_operand(value)
                )?;
            }
            Instruction::LoadField { dest, base, offset } => {
                writeln!(
                    output,
                    "{}{} = load {}.field[{}]",
                    self.indent(),
                    dest.0,
                    self.format_operand(base),
                    offset
                )?;
            }
            Instruction::Interpolate { dest, parts } => {
                write!(output, "{}{} = interpolate [", self.indent(), dest.0)?;
                for (i, part) in parts.iter().enumerate() {
                    if i > 0 {
                        write!(output, ", ")?;
                    }
                    match part {
                        crate::ssa_ir::InterpolationOperand::Literal(s) => {
                            write!(output, "\"{}\"", self.resolve_str(*s))?;
                        }
                        crate::ssa_ir::InterpolationOperand::Value(v) => {
                            write!(output, "{}", v.0)?;
                        }
                    }
                }
                writeln!(output, "]")?;
            }
            Instruction::EnumConstruct {
                dest,
                enum_name,
                variant,
                args,
            } => {
                write!(
                    output,
                    "{}{} = enum {}::{} {{",
                    self.indent(),
                    dest.0,
                    self.resolve_str(*enum_name),
                    self.resolve_str(*variant)
                )?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(output, ", ")?;
                    }
                    write!(output, "{}", self.format_operand(arg))?;
                }
                writeln!(output, "}}")?;
            }
            Instruction::MatchEnum { value, arms } => {
                writeln!(output, "{}match {} {{", self.indent(), value.0)?;
                self.push_indent();
                for (variant, block_id) in arms.iter() {
                    writeln!(
                        output,
                        "{}{} => bb{},",
                        self.indent(),
                        self.resolve_str(*variant),
                        block_id.0
                    )?;
                }
                self.pop_indent();
                writeln!(output, "{}}}", self.indent())?;
            }
            Instruction::Jump { target } => {
                writeln!(output, "{}jmp bb{}", self.indent(), target.0)?;
            }
            Instruction::Branch {
                cond,
                then_bb,
                else_bb,
            } => {
                writeln!(
                    output,
                    "{}br {} ? bb{} : bb{}",
                    self.indent(),
                    self.format_operand(cond),
                    then_bb.0,
                    else_bb.0
                )?;
            }
            Instruction::Ret { value } => {
                write!(output, "{}ret", self.indent())?;
                if let Some(val) = value {
                    write!(output, " {}", self.format_operand(val))?;
                }
                writeln!(output)?;
            }
            Instruction::Const { dest, ty, value } => {
                writeln!(
                    output,
                    "{}{} = const {} {}",
                    self.indent(),
                    dest.0,
                    self.format_ssa_type_inline(ty),
                    self.format_operand(value)
                )?;
            }
        }
        Ok(())
    }

    fn format_operand(&self, operand: &Operand) -> String {
        match operand {
            Operand::Value(val) => format!("%{}", val.0),
            Operand::ConstInt(n) => n.to_string(),
            Operand::ConstBool(b) => b.to_string(),
            Operand::ConstString(s) => format!("\"{}\"", self.resolve_str(*s)),
            Operand::FunctionRef(name) => format!("@{}", self.resolve_str(*name)),
            Operand::GlobalRef(name) => format!("${}", self.resolve_str(*name)),
            Operand::ConstFloat(n) => n.to_string(),
        }
    }

    fn format_binop(&self, op: BinOp) -> &'static str {
        match op {
            BinOp::Add => "+",
            BinOp::Sub => "-",
            BinOp::Mul => "*",
            BinOp::Div => "/",
            BinOp::Mod => "%",
            BinOp::Eq => "==",
            BinOp::Ne => "!=",
            BinOp::Lt => "<",
            BinOp::Le => "<=",
            BinOp::Gt => ">",
            BinOp::Ge => ">=",
            BinOp::BitAnd => "&",
            BinOp::BitOr => "|",
            BinOp::BitXor => "^",
            BinOp::ShiftLeft => "<<",
            BinOp::ShiftRight => ">>",
        }
    }

    fn format_unop(&self, op: UnOp) -> &'static str {
        match op {
            UnOp::Not => "!",
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
            Operator::LogicalNot => "!",
            Operator::Equals => "==",
            Operator::NotEquals => "!=",
            Operator::GreaterThan => ">",
            Operator::LessThan => "<",
            Operator::GreaterThanOrEqual => ">=",
            Operator::LessThanOrEqual => "<=",
            Operator::LogicalAnd => "&&",
            Operator::LogicalOr => "||",
            Operator::DerefUnsafe => "[*]",
            Operator::Deref => "*",
            Operator::Ref => "&",
            Operator::RefMut => "&mut ",
        }
    }

    fn format_ssa_type_inline(&self, ty: &SsaType) -> String {
        match ty {
            SsaType::I8 => "i8".to_string(),
            SsaType::U8 => "u8".to_string(),
            SsaType::I16 => "i16".to_string(),
            SsaType::U16 => "u16".to_string(),
            SsaType::I32 => "i32".to_string(),
            SsaType::U32 => "u32".to_string(),
            SsaType::I64 => "i64".to_string(),
            SsaType::U64 => "u64".to_string(),
            SsaType::I128 => "i128".to_string(),
            SsaType::F32 => "f32".to_string(),
            SsaType::F64 => "f64".to_string(),
            SsaType::ISize => "isize".to_string(),
            SsaType::USize => "usize".to_string(),
            SsaType::Bool => "bool".to_string(),
            SsaType::String => "String".to_string(),
            SsaType::Void => "()".to_string(),
            SsaType::User(name, generics) => {
                let mut result = self.resolve_str(*name).to_string();
                if !generics.is_empty() {
                    result.push('<');
                    for (i, generic) in generics.iter().enumerate() {
                        if i > 0 {
                            result.push_str(", ");
                        }
                        result.push_str(&self.format_ssa_type_inline(generic));
                    }
                    result.push('>');
                }
                result
            }
            SsaType::Enum(variants) => {
                let mut result = "enum(".to_string();
                for (i, variant) in variants.iter().enumerate() {
                    if i > 0 {
                        result.push_str(" | ");
                    }
                    result.push_str(&self.format_ssa_type_inline(variant));
                }
                result.push(')');
                result
            }
            SsaType::Tuple(types) => {
                let mut result = "(".to_string();
                for (i, ty) in types.iter().enumerate() {
                    if i > 0 {
                        result.push_str(", ");
                    }
                    result.push_str(&self.format_ssa_type_inline(ty));
                }
                result.push(')');
                result
            }
            SsaType::Dyn => "dyn".to_string(),
            SsaType::Slice => "slice".to_string(),
            SsaType::U128 => "u128".to_string(),
            SsaType::Pointer(ty) => format!("*{}", self.format_ssa_type_inline(ty)),
            SsaType::Null => "null".to_string(),
        }
    }

    fn format_ssa_type(&mut self, output: &mut String, ty: &SsaType) -> Result<(), fmt::Error> {
        match ty {
            SsaType::I8 => write!(output, "i8"),
            SsaType::U8 => write!(output, "u8"),
            SsaType::I16 => write!(output, "i16"),
            SsaType::U16 => write!(output, "u16"),
            SsaType::I32 => write!(output, "i32"),
            SsaType::U32 => write!(output, "u32"),
            SsaType::I64 => write!(output, "i64"),
            SsaType::U64 => write!(output, "u64"),
            SsaType::I128 => write!(output, "i128"),
            SsaType::F32 => write!(output, "f32"),
            SsaType::F64 => write!(output, "f64"),
            SsaType::ISize => write!(output, "isize"),
            SsaType::USize => write!(output, "usize"),
            SsaType::Bool => write!(output, "bool"),
            SsaType::String => write!(output, "String"),
            SsaType::Void => write!(output, "()"),
            SsaType::User(name, generics) => {
                write!(output, "{}", self.resolve_str(*name))?;
                if !generics.is_empty() {
                    write!(output, "<")?;
                    for (i, generic) in generics.iter().enumerate() {
                        if i > 0 {
                            write!(output, ", ")?;
                        }
                        self.format_ssa_type(output, generic)?;
                    }
                    write!(output, ">")?;
                }
                Ok(())
            }
            SsaType::Enum(variants) => {
                write!(output, "enum(")?;
                for (i, variant) in variants.iter().enumerate() {
                    if i > 0 {
                        write!(output, " | ")?;
                    }
                    self.format_ssa_type(output, variant)?;
                }
                write!(output, ")")
            }
            SsaType::Tuple(types) => {
                write!(output, "(")?;
                for (i, ty) in types.iter().enumerate() {
                    if i > 0 {
                        write!(output, ", ")?;
                    }
                    self.format_ssa_type(output, ty)?;
                }
                write!(output, ")")
            }
            SsaType::Dyn => write!(output, "dyn"),
            SsaType::Slice => write!(output, "slice"),
            SsaType::U128 => write!(output, "u128"),
            SsaType::Pointer(ty) => write!(output, "*{}", self.format_ssa_type_inline(ty)),
            SsaType::Null => write!(output, "null"),
        }
    }

    /// Format AST as readable text (simplified version)
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
                // Add parameters, return type, and body formatting here
                writeln!(output, " {{ /* function body */ }}")?;
            }
            Stmt::StructDecl(class_decl) => {
                write!(
                    output,
                    "{}class {}",
                    self.indent(),
                    self.resolve_str(class_decl.name)
                )?;
                writeln!(output, " {{ /* class body */ }}")?;
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

/// Convenience functions for quick formatting
impl IrPrettyPrinter {
    /// Format HIR module to string with default settings
    pub fn hir_to_string(
        string_pool: Arc<StringPool>,
        module: &HirModule<'_, '_>,
    ) -> Result<String, fmt::Error> {
        let mut printer = IrPrettyPrinter::new(string_pool);
        printer.format_hir_module(module)
    }

    /// Format SSA module to string with default settings
    pub fn ssa_to_string(
        string_pool: Arc<StringPool>,
        module: &Module<'_, '_>,
    ) -> Result<String, fmt::Error> {
        let mut printer = IrPrettyPrinter::new(string_pool);
        printer.format_ssa_module(module)
    }

    /// Format AST statements to string with default settings
    pub fn ast_to_string(
        string_pool: Arc<StringPool>,
        statements: &[Stmt<'_, '_>],
    ) -> Result<String, fmt::Error> {
        let mut printer = IrPrettyPrinter::new(string_pool);
        printer.format_ast_statements(statements)
    }
}
