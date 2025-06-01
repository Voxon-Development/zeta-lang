use crate::ast::*;
use cranelift::prelude::*;
use cranelift_jit::JITModule;
use cranelift_module::{DataDescription, FuncId, Linkage, Module};

use cranelift::codegen::{ir, CodegenError, Context};
use std::collections::{HashMap, HashSet};
use crate::ast;

use std;
use std::sync::Mutex;
use anyhow::Error;
use cranelift::codegen::ir::stackslot::StackSize;
use lazy_static::lazy_static;

type VariableMap = HashMap<String, Variable>;
type FunctionIds = HashMap<String, FuncId>;

lazy_static! {
    static ref CLASSES: Mutex<HashMap<String, CompiledClass>> = Mutex::new(HashMap::new());
}

pub struct Codegen {
    builder_context: FunctionBuilderContext,
    ctx: Context,
    stmt_compiler: StmtCompiler,
    pub func_ids: FunctionIds,
}

#[derive(Clone)]
pub struct StmtCompiler {
    variables: VariableMap,
    func_ids: FunctionIds,
    string_counter: usize,
    loop_stack: Vec<(ir::Block, ir::Block)>,
    pub variable_types: HashMap<String, String>,
    did_return: bool,
    class_fields: HashSet<String>,
    in_class_scope: bool,
    field_offsets: HashMap<String, i32>
}

impl StmtCompiler {
    pub fn new(func_ids: FunctionIds) -> Self {
        Self {
            variables: HashMap::new(),
            func_ids,
            string_counter: 0,
            loop_stack: Vec::new(),
            variable_types: HashMap::new(),
            did_return: false,
            class_fields: HashSet::new(),
            in_class_scope: false,
            field_offsets: HashMap::new()
        }
    }

    fn lookup_local_variable(&self, name: &str) -> Option<Variable> {
        self.variables.get(name).cloned()
    }

    pub fn compile_stmt(
        &mut self, 
        builder: &mut FunctionBuilder<'_>, 
        stmt: &Stmt, 
        module: &mut JITModule
    ) -> anyhow::Result<()> {
        match stmt {
            Stmt::ExprStmt(expr_stmt) => {
                self.compile_expr(builder, &expr_stmt.expr, module)?;
            }

            Stmt::Let(let_stmt) => {
                let value = self.compile_expr(builder, &let_stmt.value, module)?;
                let variable = Variable::new(self.variables.len());

                // Track the class type
                if let Some(ty) = &let_stmt.type_annotation {
                    if let ast::Type::Class(name) = ty {
                        self.variable_types.insert(let_stmt.ident.clone(), name.clone());
                    }
                } else {
                    match &*let_stmt.value {
                        Expr::Call { callee, .. } => {
                            if let Expr::Ident(class_name) = &**callee {
                                self.variable_types.insert(let_stmt.ident.clone(), class_name.clone());
                            }
                        }

                        Expr::ClassInit { callee, .. } => {
                            match &**callee {
                                Expr::Ident(string) => {
                                    if let Some(class_ty) = self.variable_types.get(string) {
                                        self.variable_types.insert(let_stmt.ident.clone(), class_ty.clone());
                                    }
                                }

                                _ => {}
                            }
                        }

                        _ => {}
                    }
                }

                builder.declare_var(variable, let_stmt.type_annotation.clone()
                    .map(parse_type)
                    .unwrap_or_else(|| builder.func.dfg.value_type(value)));

                builder.def_var(variable, value);
                self.variables.insert(let_stmt.ident.clone(), variable);
            }
            Stmt::Return(return_stmt) => {
                if self.did_return {
                    panic!("Unreachable statement.");
                }

                if let Some(expr) = &return_stmt.value {
                    let val = self.compile_expr(builder, expr, module)?;
                    builder.ins().return_(&[val]);
                } else {
                    builder.ins().return_(&[]);
                }

                self.did_return = true;
            }

            Stmt::Match(match_stmt) => {
                let matched_val = self.compile_expr(builder, &match_stmt.expr, module)?;
                let matched_ty = builder.func.dfg.value_type(matched_val);

                let exit_block = builder.create_block();
                let mut arm_blocks = Vec::new();
                let mut next_test_blocks = Vec::new();

                for _ in &match_stmt.arms {
                    arm_blocks.push(builder.create_block());
                    next_test_blocks.push(builder.create_block());
                }

                // Final fallthrough if no match: jump to exit for now
                next_test_blocks.push(exit_block);

                // === Pattern matching branches ===
                for (i, arm) in match_stmt.arms.iter().enumerate() {
                    let cond = match &arm.pattern {
                        Pattern::Number(n) => {
                            let const_val = builder.ins().iconst(matched_ty, *n);
                            builder.ins().icmp(IntCC::Equal, matched_val, const_val)
                        }
                        Pattern::Wildcard => builder.ins().iconst(types::I8, 1),
                        Pattern::Ident(name) => {
                            let var = Variable::new(self.variables.len());
                            builder.declare_var(var, matched_ty);
                            builder.def_var(var, matched_val);
                            self.variables.insert(name.clone(), var);
                            builder.ins().iconst(types::I8, 1)
                        }
                        _ => unimplemented!("Unsupported pattern variant"),
                    };

                    builder.ins().brif(
                        cond,
                        arm_blocks[i],
                        &[],
                        next_test_blocks[i + 1],
                        &[],
                    );
                    builder.seal_block(builder.current_block().unwrap());
                    builder.switch_to_block(next_test_blocks[i + 1]);
                }

                // === Arm blocks ===
                for (i, arm) in match_stmt.arms.iter().enumerate() {
                    builder.switch_to_block(arm_blocks[i]);

                    for stmt in &arm.block.block {
                        self.compile_stmt(builder, stmt, module)?;
                    }

                    builder.ins().jump(exit_block, &[]);
                    builder.seal_block(arm_blocks[i]);
                }

                builder.switch_to_block(exit_block);
                builder.seal_block(exit_block);
            }

            Stmt::While(while_stmt) => {
                let cond_block = builder.create_block();
                let body_block = builder.create_block();
                let exit_block = builder.create_block();

                // Push loop context
                self.loop_stack.push((cond_block, exit_block));
                builder.ins().jump(cond_block, &[]);

                // === Condition check ===
                builder.switch_to_block(cond_block);
                let cond_val = self.compile_expr(builder, &while_stmt.condition, module)?;
                builder.ins().brif(cond_val, body_block, &[], exit_block, &[]);

                // === Body block ===
                builder.switch_to_block(body_block);
                for stmt in &while_stmt.block.block {
                    self.compile_stmt(builder, stmt, module)?;
                }
                builder.ins().jump(cond_block, &[]);
                builder.seal_block(cond_block);
                builder.seal_block(body_block);

                // === Exit block ===
                builder.switch_to_block(exit_block);
                builder.seal_block(exit_block);

                // Pop loop context
                self.loop_stack.pop();
            }

            Stmt::For(for_stmt) => {
                // === Initialization ===
                if let Some(let_stmt) = &for_stmt.let_stmt {
                    self.compile_stmt(builder, &Stmt::Let(let_stmt.clone()), module)?;
                }

                let cond_block = builder.create_block();
                let body_block = builder.create_block();
                let incr_block = builder.create_block();
                let exit_block = builder.create_block();

                // Push loop context for break/continue
                self.loop_stack.push((incr_block, exit_block));

                // === Jump to condition ===
                builder.ins().jump(cond_block, &[]);

                // === Condition check ===
                builder.switch_to_block(cond_block);
                 if let Some(cond_expr) = &for_stmt.condition {
                    let cond_val = self.compile_expr(builder, cond_expr, module)?;
                    builder.ins().brif(cond_val, body_block, &[], exit_block, &[]);
                } else {
                    // No condition: infinite loop
                    builder.ins().jump(body_block, &[]);
                };
                // Only seal condition block after all jumps to it have been emitted

                // === Body ===
                builder.switch_to_block(body_block);
                for stmt in &for_stmt.block.block {
                    self.compile_stmt(builder, stmt, module)?;
                }
                builder.ins().jump(incr_block, &[]);
                builder.seal_block(body_block); // Seal body after jump to incr

                // === Increment ===
                builder.switch_to_block(incr_block);
                if let Some(incr_expr) = &for_stmt.increment {
                    self.compile_expr(builder, incr_expr, module)?;
                }
                builder.ins().jump(cond_block, &[]);
                builder.seal_block(cond_block);
                builder.seal_block(incr_block); // All predecessors (body) are done

                // === Exit block ===
                builder.switch_to_block(exit_block);
                builder.seal_block(exit_block);

                self.loop_stack.pop();
            }


            Stmt::Break => {
                if let Some((_, break_block)) = self.loop_stack.last() {
                    builder.ins().jump(*break_block, &[]);
                    builder.seal_block(builder.current_block().unwrap());
                } else {
                    anyhow::bail!("`break` used outside of a loop");
                }
            }

            Stmt::Continue => {
                if let Some((continue_block, _)) = self.loop_stack.last() {
                    builder.ins().jump(*continue_block, &[]);
                    builder.seal_block(builder.current_block().unwrap());
                } else {
                    anyhow::bail!("`continue` used outside of a loop");
                }
            }

            Stmt::If(if_stmt) => {
                if if_stmt.condition == Expr::Boolean(false) {
                    let else_block = builder.create_block();

                    builder.switch_to_block(else_block);
                    self.parse_else_block_to_ir(builder, module, &if_stmt)?;
                    builder.seal_block(else_block);
                }
                self.compile_if_stmt(builder, if_stmt, module)?;
            }

            _ => unimplemented!("Unsupported statement: {:?}", stmt),
        }
        Ok(())
    }

    fn parse_else_block_to_ir(&mut self, builder: &mut FunctionBuilder, module: &mut JITModule, if_stmt: &&IfStmt) -> anyhow::Result<()> {
        if let Some(else_branch) = &if_stmt.else_branch {
            match &**else_branch {
                ElseBranch::Else(else_block_stmts) => {
                    for stmt in &else_block_stmts.block {
                        self.compile_stmt(builder, stmt, module)?;
                    }
                }
                ElseBranch::If(nested_if) => {
                    self.compile_if_stmt(builder, nested_if, module)?;
                }
            }
        }
        Ok(())
    }

    fn compile_if_stmt(
        &mut self,
        builder: &mut FunctionBuilder,
        if_stmt: &IfStmt,
        module: &mut JITModule,
    ) -> anyhow::Result<()> {
        let then_block = builder.create_block();
        let else_block = builder.create_block();
        let merge_block = builder.create_block();

        // Compile the condition
        let cond_val = self.compile_expr(builder, &if_stmt.condition, module)?;

        builder.ins().brif(cond_val, then_block, &[], else_block, &[]);

        // === THEN block ===
        builder.switch_to_block(then_block);
        for stmt in &if_stmt.then_branch.block {
            self.compile_stmt(builder, stmt, module)?;
        }
        builder.ins().jump(merge_block, &[]);
        builder.seal_block(then_block);

        // === ELSE block ===
        builder.switch_to_block(else_block);
        self.parse_else_block_to_ir(builder, module, &if_stmt)?;
        builder.ins().jump(merge_block, &[]);
        builder.seal_block(else_block);

        // === Merge block ===
        builder.switch_to_block(merge_block);
        builder.seal_block(merge_block);

        Ok(())
    }


    //noinspection RsUnwrap
    pub fn compile_expr(&mut self, builder: &mut FunctionBuilder, expr: &Expr, module: &mut JITModule) -> anyhow::Result<Value> {
        match expr {
            Expr::Number(n) => Ok(builder.ins().iconst(types::I64, *n)),

            Expr::Comparison { lhs, op, rhs } => {
                let lhs_val = self.compile_expr(builder, lhs, module)?;
                let rhs_val = self.compile_expr(builder, rhs, module)?;

                match op {
                    ComparisonOp::Equal => Ok(builder.ins().icmp(IntCC::Equal, lhs_val, rhs_val)),
                    ComparisonOp::NotEqual => Ok(builder.ins().icmp(IntCC::NotEqual, lhs_val, rhs_val)),
                    ComparisonOp::LessThan => Ok(builder.ins().icmp(IntCC::SignedLessThan, lhs_val, rhs_val)),
                    ComparisonOp::LessThanOrEqual => Ok(builder.ins().icmp(IntCC::SignedLessThanOrEqual, lhs_val, rhs_val)),
                    ComparisonOp::GreaterThan => Ok(builder.ins().icmp(IntCC::SignedGreaterThan, lhs_val, rhs_val)),
                    ComparisonOp::GreaterThanOrEqual => Ok(builder.ins().icmp(IntCC::SignedGreaterThanOrEqual, lhs_val, rhs_val)),
                }
            }

            Expr::Binary { left, op, right } => {
                let left_val = self.compile_expr(builder, left, module)?;
                let right_val = self.compile_expr(builder, right, module)?;

                let ty = builder.func.dfg.value_type(left_val);
                assert_eq!(ty, builder.func.dfg.value_type(right_val), "Mismatched operand types");

                let res = match op {
                    Op::Add => builder.ins().iadd(left_val, right_val),
                    Op::Sub => builder.ins().isub(left_val, right_val),
                    Op::Mul => builder.ins().imul(left_val, right_val),
                    Op::Div => builder.ins().sdiv(left_val, right_val),
                    _ => panic!("Unsupported binary operator: {:?}", op),
                };
                Ok(res)
            }

            Expr::String(s) => {
                // Create a unique symbol name for the string data
                let symbol = format!("str_{}", &self.string_counter);
                self.string_counter += 1;

                // Declare an immutable data object with a given name, not TLS, not writable
                let data_id = module.declare_data(&symbol, Linkage::Local, false, false)?;

                let mut string_bytes = s.clone().into_bytes();
                string_bytes.push(0); // null-terminate the string

                let mut desc = DataDescription::new();
                desc.define(string_bytes.into_boxed_slice());

                // Define data by passing raw bytes
                module.define_data(data_id, &desc).expect(format!("Expected to define a string with id: {}", data_id).as_str());

                let local_id = module.declare_data_in_func(data_id, &mut builder.func);

                let ptr = builder.ins().symbol_value(types::I64, local_id);

                Ok(ptr)
            },

            Expr::Get { object, field } => {
                // Handle `self._field`
                if let Expr::Ident(obj_name) = &**object {
                    if obj_name == "self" && self.in_class_scope {
                        let self_var = self.variables.get("self")
                            .ok_or_else(|| anyhow::anyhow!("`self` not found in class scope"))?;

                        let base_ptr = builder.use_var(*self_var);
                        let offset = *self.field_offsets.get(field)
                            .ok_or_else(|| anyhow::anyhow!("Field `{}` not found", field))?;

                        // Always loading as I64 for now
                        let val = builder.ins().load(types::I64, MemFlags::new(), base_ptr, offset);
                        return Ok(val);
                    }
                }

                if let Expr::Get { object, field: _field } = &**object {
                    return self.compile_expr(builder, object, module);
                }

                anyhow::bail!("Unsupported _field access")
            },

            Expr::ClassInit { callee, arguments } => {
                let class_name = match &**callee {
                    Expr::Ident(name) => name,
                    _ => anyhow::bail!("Expected identifier in class init"),
                };

                let classes = CLASSES.lock().unwrap();
                let compiled_class = classes.get(class_name).ok_or_else(|| anyhow::anyhow!("Class `{}` not found", class_name))?;

                let param_names = compiled_class.field_layout.param_names.clone(); // e.g., ["x", "y"]

                if arguments.len() != param_names.len() {
                    anyhow::bail!("Expected {} arguments for class `{}`, got {}", param_names.len(), class_name, arguments.len());
                }

                let stack_slot = builder.create_sized_stack_slot(StackSlotData::new(
                    StackSlotKind::ExplicitSlot,
                    compiled_class.field_layout.total_size as StackSize,
                ));
                let base_ptr = builder.ins().stack_addr(types::I64, stack_slot, 0);

                for (param_name, arg_expr) in param_names.iter().zip(arguments.iter()) {
                    let offset = match compiled_class.field_layout.offsets.get(param_name) {
                        Some(off) => *off,
                        None => {
                            anyhow::bail!("Missing offset for field `{param_name}`");
                        }
                    };

                    let arg_val = self.compile_expr(builder, arg_expr, module)?;

                    builder.ins().store(MemFlags::new(), arg_val, base_ptr, offset);
                }

                Ok(base_ptr)
            }

            Expr::Call { callee, arguments } => {
                match &**callee {
                    Expr::Get { object, field: _field } => {
                        // object._field(args...) → method call → inject `self`
                        let self_val = self.compile_expr(builder, object, module)?;
                        let func_name = self.get_name(callee);

                        let func_id = *self.func_ids.get(&func_name)
                            .ok_or_else(|| anyhow::anyhow!("Function `{}` not found", func_name))?;

                        let mut all_args = vec![self_val];
                        for arg in arguments {
                            all_args.push(self.compile_expr(builder, arg, module)?);
                        }

                        let func_ref = module.declare_func_in_func(func_id, builder.func);

                        let call = builder.ins().call(func_ref, &all_args);
                        let results = builder.inst_results(call);

                        if results.is_empty() {
                            Ok(builder.ins().iconst(types::I8, 0))
                        } else {
                            Ok(results[0])
                        }
                    }
                    _ => {
                        let func_name = self.get_name(callee);

                        let func_id = *self.func_ids.get(&func_name)
                            .ok_or_else(|| anyhow::anyhow!("Function `{}` not found", func_name))?;

                        let func_ref = module.declare_func_in_func(func_id, &mut builder.func);
                        let mut arg_vals = Vec::new();
                        for arg in arguments {
                            let val = self.compile_expr(builder, arg, module)?;
                            arg_vals.push(val);
                        }

                        let call = builder.ins().call(func_ref, &arg_vals);
                        let results = builder.inst_results(call);

                        if results.is_empty() {
                            Ok(builder.ins().iconst(types::I8, 0))
                        } else {
                            Ok(results[0])
                        }
                    }
                }
            }

            Expr::Ident(name) => {
                if let Some(var) = self.lookup_local_variable(name) {
                    Ok(builder.use_var(var))
                } else if self.in_class_scope {
                    if self.class_fields.contains(name) {
                        // Convert `x` to `Get { object: Ident("self"), field: "x" }`
                        let get_expr = Expr::Get {
                            object: Box::new(Expr::Ident("self".to_string())),
                            field: name.clone(),
                        };
                        self.compile_expr(builder, &get_expr, module)
                    } else {
                        Err(Error::from(CodegenError::CodeTooLarge))
                    }
                } else {
                    Err(Error::from(CodegenError::CodeTooLarge))
                }
            }

            Expr::Boolean(val) => {
                let int_val = if *val { 1 } else { 0 };
                Ok(builder.ins().iconst(types::I8, int_val))
            }

            Expr::Assignment { lhs, op, rhs } => {
                let var_name = match &**lhs {
                    Expr::Ident(name) => name,
                    _ => anyhow::bail!("Only simple variable identifiers can be assigned to"),
                };

                if let Some(&var) = self.variables.get(var_name) {
                    let rhs_val = self.compile_expr(builder, rhs, module)?;

                    // Handle compound assignments
                    let final_val = match op.as_str() {
                        "=" => rhs_val,
                        "+=" => {
                            let current_val = builder.use_var(var);
                            builder.ins().iadd(current_val, rhs_val)
                        }
                        "-=" => {
                            let current_val = builder.use_var(var);
                            builder.ins().isub(current_val, rhs_val)
                        }
                        "*=" => {
                            let current_val = builder.use_var(var);
                            builder.ins().imul(current_val, rhs_val)
                        }
                        "/=" => {
                            let current_val = builder.use_var(var);
                            builder.ins().sdiv(current_val, rhs_val)
                        }
                        _ => anyhow::bail!("Unsupported assignment operator: {}", op),
                    };

                    builder.def_var(var, final_val);
                    return Ok(final_val)
                }


                if let Some(offset) = self.field_offsets.get(var_name) {
                    // Field of the current class instance (`self`)
                    let self_val = self.variables.get("self")
                        .ok_or_else(|| anyhow::anyhow!("No `self` variable in current context"))?;

                    let base_ptr = builder.use_var(*self_val); // pointer to struct
                    let field_addr = builder.ins().iadd_imm(base_ptr, *offset as i64);
                    let rhs_val = self.compile_expr(builder, rhs, module)?;

                    let var_type = builder.func.dfg.value_type(rhs_val);
                    let current_val = builder.ins().load(var_type, MemFlags::new(), field_addr, 0);

                    let final_val = match op.as_str() {
                        "=" => rhs_val,
                        "+=" => builder.ins().iadd(current_val, rhs_val),
                        "-=" => builder.ins().isub(current_val, rhs_val),
                        "*=" => builder.ins().imul(current_val, rhs_val),
                        "/=" => builder.ins().sdiv(current_val, rhs_val),
                        _ => anyhow::bail!("Unsupported assignment operator: {}", op),
                    };

                    builder.ins().store(MemFlags::new(), final_val, field_addr, 0);
                    Ok(final_val)
                } else {
                    panic!("Variable `{}` not found", var_name);
                }

            }


            _ => anyhow::bail!("Unsupported expression"),
        }
    }

    fn get_name(
        &mut self,
        callee: &Box<Expr>,
    ) -> String {
        match &**callee {
            // Direct identifier call like `foo()`
            Expr::Ident(name) => name.clone(),

            // Object method call like `obj.method()`
            Expr::Get { object, field } => {
                if let Expr::Ident(object_name) = &**object {
                    // Look up a type of the object in the symbol table
                    let class_name = self.variable_types.get(object_name).unwrap_or_else(|| {
                        panic!("Unknown type for object '{}'", object_name);
                    });

                    format!("{}_{}", class_name, field)
                } else {
                    panic!("Only simple identifiers are supported for method access (e.g., `obj.method`)");
                }
            }

            _ => panic!("Only direct identifiers or field accesses are valid for function calls"),
        }
    }

}

#[derive(Clone)]
struct CompiledClass {
    field_layout: FieldLayout,
    methods: Vec<FuncDecl>
}

impl Codegen {
    pub fn new() -> Self {
        let map = HashMap::new();
        Self {
            builder_context: FunctionBuilderContext::new(),
            ctx: Context::new(),
            stmt_compiler: StmtCompiler::new(map),
            func_ids: HashMap::new(),
        }
    }

    //noinspection RsUnwrap
    fn compile_class(&mut self, class: &ClassDecl, module: &mut JITModule) -> anyhow::Result<CompiledClass> {
        self.stmt_compiler.class_fields = HashSet::from_iter(class.clone().params.unwrap_or(Vec::new()).iter().map(|p| p.name.clone()));
        self.stmt_compiler.in_class_scope = true;

        let field_layout: FieldLayout = compute_field_offsets(&class.params);

        if let Some(params) = class.params.clone() {
            for param in params {
                match param.type_annotation {
                    Some(ast::Type::Class(class_name)) => {
                        self.stmt_compiler.variable_types.insert(param.name.clone(), class_name);
                    }
                    _ => {}
                }
            }
        }

        self.stmt_compiler.field_offsets = field_layout.offsets.clone();

        let mut compiled_class = CompiledClass {
            field_layout,
            methods: Vec::new()
        };

        for stmt in class.body.clone().into_iter() {
            match stmt {
                Stmt::FuncDecl(f) => {
                    compiled_class.methods.push(FuncDecl {
                        visibility: f.visibility,
                        is_async: f.is_async,
                        is_unsafe: f.is_unsafe,
                        name: class.name.clone() + "_" + &f.name,
                        params: f.params,
                        return_type: f.return_type,
                        body: f.body,
                    });
                }
                _ => unimplemented!()
            }
        }

        self.declare_funcs(compiled_class.methods.as_slice(), module)?;
        self.define_funcs(compiled_class.methods.as_slice(), module)?;

        let mut classes = CLASSES.lock().unwrap();

        classes.insert(class.name.clone(), compiled_class.clone());


        self.stmt_compiler.class_fields.clear();
        self.stmt_compiler.in_class_scope = false;

        Ok(compiled_class)
    }

    pub fn declare_classes(&mut self, class_decls: &[ClassDecl], module: &mut JITModule) -> anyhow::Result<()> {
        for class_decl in class_decls {
            self.compile_class(class_decl, module)?;
        }
        Ok(())
    }

    pub fn declare_funcs(&mut self, funcs: &[FuncDecl], module: &mut JITModule) -> anyhow::Result<()> {
        for func in funcs {

            let mut sig: Signature = module.make_signature();

            // Check if it's an instance method (first param is `self`)
            let is_method = matches!(func.params.first(), Some(p) if p.name == "self");

            for (i, param) in func.params.iter().enumerate() {
                let ty = if is_method && i == 0 {
                    types::I64 // assume self is a pointer
                } else {
                    parse_type(param.type_annotation.clone().unwrap())
                };
                sig.params.push(AbiParam::new(ty));
            }

            if let Some(ret_ty) = &func.return_type {
                sig.returns.push(AbiParam::new(parse_type(ret_ty.clone())));
            }

            let func_id = module.declare_function(&func.name, Linkage::Export, &sig)?;
            self.func_ids.insert(func.name.clone(), func_id);
        }

        self.stmt_compiler.func_ids = self.func_ids.clone();

        Ok(())
    }


    pub fn define_funcs(&mut self, funcs: &[FuncDecl], module: &mut JITModule) -> anyhow::Result<()> {
        for func in funcs {
            let func_id = *self
                .func_ids
                .get(&func.name)
                .ok_or_else(|| anyhow::anyhow!("Function {} not declared", func.name))?;

            let mut sig = module.make_signature();

            // Check if it's an instance method (first param is `self`)
            // Always push all params (including self) into the function signature
            for (i, param) in func.params.iter().enumerate() {
                let ty = if i == 0 && param.name == "self" {
                    types::I64
                } else {
                    parse_type(param.type_annotation.clone().unwrap())
                };

                sig.params.push(AbiParam::new(ty));
            }


            if let Some(ret_ty) = &func.return_type {
                sig.returns.push(AbiParam::new(parse_type(ret_ty.clone())));
            }

            self.ctx.func.signature = sig;

            {
                let mut builder = FunctionBuilder::new(&mut self.ctx.func, &mut self.builder_context);
                let block = builder.create_block();
                builder.append_block_params_for_function_params(block);
                builder.switch_to_block(block);
                builder.seal_block(block);

                self.stmt_compiler.variables.clear();

                let is_method = func.params.len() > 0 && func.params.first().unwrap().name == "self";
                self.stmt_compiler.in_class_scope = is_method;

                // Bind all parameters (including self if present)
                for (i, param) in func.params.iter().enumerate() {
                    let ty = if is_method && i == 0 {
                        types::I64 // assume self is a pointer
                    } else {
                        parse_type(param.type_annotation.clone().unwrap())
                    };

                    let var = Variable::new(i);
                    builder.declare_var(var, ty);

                    let block_params = builder.block_params(block);
                    assert_eq!(
                        block_params.len(),
                        func.params.len(),
                        "Mismatched parameter count: expected {}, got {}",
                        func.params.len(),
                        block_params.len()
                    );

                    let val = block_params[i];

                    builder.def_var(var, val);
                    self.stmt_compiler.variables.insert(param.name.clone(), var);

                    if let Some(type_annotation) = param.type_annotation.clone() {
                        match type_annotation {
                            ast::Type::Class(class_name) => {
                                self.stmt_compiler.variable_types.insert(param.name.clone(), class_name);
                            }
                            _ => {}
                        }
                    }
                }

                for stmt in &func.body.block {
                    self.stmt_compiler.compile_stmt(&mut builder, stmt, module)?;
                }

                if !self.stmt_compiler.did_return {
                    builder.ins().return_(&[]);
                }

                builder.finalize();
                self.stmt_compiler.did_return = false;
            }

            module.define_function(func_id, &mut self.ctx)?;

            module.clear_context(&mut self.ctx);
        }

        Ok(())
    }
}

pub fn parse_type(variable_type: ast::Type) -> types::Type {
    match variable_type {
        ast::Type::F64 => types::F64,
        ast::Type::F32 => types::F32,
        ast::Type::I32 => types::I64,
        ast::Type::I64 => types::I64,
        ast::Type::U32 => types::I64,
        ast::Type::UF64 => types::F64,
        ast::Type::I128 => types::I128,
        ast::Type::U128 => types::I128,
        ast::Type::U64 => types::I64,
        ast::Type::UF32 => types::F32,
        ast::Type::Void => types::I8,
        ast::Type::Boolean => types::I8,
        ast::Type::Class(_) => types::I64,
        _ => panic!("Unexpected type in parse_type: {:?}", variable_type),
    }
}

#[derive(Debug, Clone)]
pub struct FieldLayout {
    pub offsets: HashMap<String, i32>, // field name → offset
    pub param_names: Vec<String>,      // ordered list of field names
    pub total_size: i32,
}

pub fn compute_field_offsets(params: &Option<Vec<Param>>) -> FieldLayout {
    let mut offsets = HashMap::new();
    let mut param_names = Vec::new();
    let mut offset: i32 = 0;

    if let Some(params) = params {
        for param in params {
            offsets.insert(param.name.clone(), offset);
            param_names.push(param.name.clone());
            offset += 8; // TODO: not all offsets are 8 bytes
        }
    }

    FieldLayout {
        offsets,
        param_names,
        total_size: offset,
    }
}
