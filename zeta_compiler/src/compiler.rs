use crate::ast::*;
use cranelift::prelude::*;
use cranelift_jit::JITModule;
use cranelift_module::{DataDescription, FuncId, Linkage, Module};

use cranelift::codegen::{ir, Context};
use std::collections::HashMap;
use crate::ast;

type VariableMap = HashMap<String, Variable>;
type FunctionIds = HashMap<String, FuncId>;

pub struct Codegen {
    builder_context: FunctionBuilderContext,
    ctx: Context,
    stmt_compiler: StmtCompiler,
    pub func_ids: FunctionIds,
}


pub struct StmtCompiler {
    variables: VariableMap,
    func_ids: FunctionIds,
    string_counter: usize,
    loop_stack: Vec<(ir::Block, ir::Block)>,
    did_return: bool
}

impl StmtCompiler {
    pub fn new(func_ids: FunctionIds) -> Self {
        Self {
            variables: HashMap::new(),
            func_ids,
            string_counter: 0,
            loop_stack: Vec::new(),
            did_return: false,
        }
    }

    pub fn compile_stmt(&mut self, builder: &mut FunctionBuilder<'_>, stmt: &Stmt, module: &mut JITModule) -> anyhow::Result<()> {
        match stmt {
            Stmt::ExprStmt(expr_stmt) => {
                let _ = self.compile_expr(builder, &expr_stmt.expr, module)?;
            }
            Stmt::Let(let_stmt) => {
                let value = self.compile_expr(builder, &let_stmt.value, module)?;
                let variable = Variable::new(self.variables.len());
                builder.declare_var(variable, parse_type(let_stmt.type_annotation.clone().unwrap()));
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

                // Final fallthrough if no match: just jump to exit for now
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
                //let current_block = builder.current_block().unwrap();
                builder.ins().jump(cond_block, &[]);
                //builder.seal_block(current_block); // Seal previous block before branching out

                // === Condition check ===
                builder.switch_to_block(cond_block);
                let has_cond = if let Some(cond_expr) = &for_stmt.condition {
                    let cond_val = self.compile_expr(builder, cond_expr, module)?;
                    builder.ins().brif(cond_val, body_block, &[], exit_block, &[]);
                    true
                } else {
                    // No condition: infinite loop
                    builder.ins().jump(body_block, &[]);
                    false
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
        // check if it's a false statement, and if it is, skip the if block or inline the else block

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
                // Attempt constant folding first
                if let (Expr::Number(lhs), Expr::Number(rhs)) = (&**left, &**right) {
                    let folded = match op {
                        Op::Add => Some(lhs + rhs),
                        Op::Sub => Some(lhs - rhs),
                        Op::Mul => Some(lhs * rhs),
                        Op::Div => {
                            if *rhs == 0 { return anyhow::Result::Err(anyhow::anyhow!("Division while RHS is 0")) }
                            Some(lhs / rhs)
                        },
                        _ => None,
                    };

                    if let Some(value) = folded {
                        return Ok(builder.ins().iconst(types::I64, value));
                    }
                }

                // Otherwise, compile normally
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
                module.define_data(data_id, &desc).expect("TODO: panic message");


                // Create a data reference inside the current function
                let local_id = module.declare_data_in_func(data_id, &mut builder.func);

                // Get a pointer to the data object
                let ptr = builder.ins().symbol_value(types::I64, local_id);

                Ok(ptr)
            },

            Expr::Call { callee, arguments } => {
                let func_name = match &**callee {
                    Expr::Ident(name) => name,
                    _ => anyhow::bail!("Only direct function identifiers can be called"),
                };

                let func_id = *self.func_ids.get(func_name)
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
                    // If the call has no return value, return a fake value or bail depending on context
                    // In a real compiler, you'd track type info and ensure this matches expected usage
                    Ok(builder.ins().iconst(types::I8, 0)) // or: bail!("Expected return value from function")
                } else {
                    Ok(results[0])
                }
            }

            Expr::Ident(name) => {
                if let Some(var) = self.variables.get(name) {
                    Ok(builder.use_var(*var))
                } else {
                    anyhow::bail!("Variable `{}` not found", name)
                }
            }

            Expr::Boolean(val) => {
                let int_val = if *val { 1 } else { 0 };
                Ok(builder.ins().iconst(types::I8, int_val))
            }

            Expr::Assignment { lhs, op, rhs } => {
                // Only support variable assignments for now
                let var_name = match &**lhs {
                    Expr::Ident(name) => name,
                    _ => anyhow::bail!("Only simple variable identifiers can be assigned to"),
                };

                let var = *self.variables.get(var_name)
                    .ok_or_else(|| anyhow::anyhow!("Variable not found"))?;

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
                Ok(final_val)
            }


            _ => anyhow::bail!("Unsupported expression"),
        }
    }
}

struct CompiledClass {
    name: String,
    field_layout: FieldLayout,
    methods: Vec<FuncDecl>
}

impl Codegen {
    pub fn new() -> Self {
        let map = HashMap::new();
        Self {
            builder_context: FunctionBuilderContext::new(),
            ctx: Context::new(),
            stmt_compiler: StmtCompiler::new(map), // Temporary placeholder
            func_ids: HashMap::new(),
        }
    }

    fn compile_class(&mut self, class: &ClassDecl, module: &mut JITModule) -> CompiledClass {
        let mut methods = Vec::new();

        for stmt in class.body.clone().into_iter() {
            match stmt {
                Stmt::FuncDecl(f) => {
                    methods.push(f.clone());
                }
                _ => unimplemented!()
            }
        }

        let field_layout = compute_field_offsets(&class.params);

        self.declare_funcs(methods.as_slice(), module).expect("Expected to be able to declare functions");
        self.define_funcs(methods.as_slice(), module).expect("Expected to be able to define functions");

        CompiledClass {
            name: class.name.clone(),
            field_layout,
            methods: methods.clone()
        }
    }

    pub fn declare_funcs(&mut self, funcs: &[FuncDecl], module: &mut JITModule) -> anyhow::Result<()> {
        for func in funcs {
            let mut sig: Signature = module.make_signature();
            for param in &func.params {
                sig.params.push(AbiParam::new(parse_type(param.type_annotation.clone().unwrap())));
            }

            if let Some(ret_ty) = &func.return_type {
                sig.returns.push(AbiParam::new(parse_type(ret_ty.clone())));
            }

            let func_id = module.declare_function(&func.name, Linkage::Export, &sig)?;
            self.func_ids.insert(func.name.clone(), func_id);
        }

        self.stmt_compiler = StmtCompiler::new(self.func_ids.clone());

        Ok(())
    }

    pub fn define_funcs(&mut self, funcs: &[FuncDecl], module: &mut JITModule) -> anyhow::Result<()> {
        for func in funcs {
            let func_id = *self
                .func_ids
                .get(&func.name)
                .ok_or_else(|| anyhow::anyhow!("Function `{}` not declared", func.name))?;

            let mut sig = module.make_signature();
            for param in &func.params {
                println!("Return type: {:?}", param);
                sig.params.push(AbiParam::new(parse_type(param.type_annotation.clone().unwrap())));
            }

            if let Some(ret_ty) = &func.return_type {
                println!("Return type: {:?}", ret_ty);
                sig.returns.push(AbiParam::new(parse_type(ret_ty.clone())));
            }

            self.ctx.func.signature = sig;

            {
                let mut builder = FunctionBuilder::new(&mut self.ctx.func, &mut self.builder_context);
                let block = builder.create_block();
                builder.append_block_params_for_function_params(block);
                builder.switch_to_block(block);
                builder.seal_block(block);

                for (i, param) in func.params.iter().enumerate() {
                    let var = Variable::new(i);

                    println!("Param: {:?}", param);

                    let param_type = parse_type(param.type_annotation.clone().unwrap());
                    builder.declare_var(var, param_type);

                    let val = builder.block_params(block)[i];
                    builder.def_var(var, val);
                    self.stmt_compiler.variables.insert(param.name.clone(), var);
                }

                for stmt in &func.body.block {
                    self.stmt_compiler.compile_stmt(&mut builder, stmt, module)?;
                }

                if !self.stmt_compiler.did_return {
                    builder.ins().return_(&[]);
                }
                builder.finalize();

                println!("Function '{}' registered with return type {:?}", func.name, func.return_type);

                self.stmt_compiler.did_return = false;
            }
            
            println!("Function '{}' registered with return type {:?}", self.ctx.func.name, self.ctx.func.signature.returns);

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
        _ => panic!("Unexpected type in parse_type: {:?}", variable_type),
    }
}

#[derive(Debug, Clone)]
pub struct FieldLayout {
    pub offsets: HashMap<String, usize>,
    pub total_size: usize,
}

fn compute_field_offsets(params: &Option<Vec<Param>>) -> FieldLayout {
    let mut offsets = HashMap::new();
    let mut offset = 0;

    if let Some(fields) = params {
        for param in fields {
            // Assume 8-byte alignment for simplicity (like a pointer)
            let size = type_size(&param.type_annotation.clone().unwrap()); // You define this
            offset = align_to(offset, size);
            offsets.insert(param.name.clone(), offset);
            offset += size;
        }
    }

    FieldLayout {
        offsets,
        total_size: offset,
    }
}

fn align_to(offset: usize, align: usize) -> usize {
    (offset + align - 1) & !(align - 1)
}

fn type_size(ty: &ast::Type) -> usize {
    match ty {
        ast::Type::I64 | ast::Type::F64 => 8,
        ast::Type::I32 | ast::Type::F32 | ast::Type::U32 => 4,
        ast::Type::I16 | ast::Type::U16 => 2,
        ast::Type::Boolean | ast::Type::I8 | ast::Type::U8 | ast::Type::Void => 1,
        _ => 8, // fallback for pointer-sized values
    }
}
