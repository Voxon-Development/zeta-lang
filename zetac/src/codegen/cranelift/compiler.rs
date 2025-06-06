use crate::ast::*;
use cranelift::prelude::*;
use cranelift_module::{FuncId, Linkage, Module};

use crate::ast;
use cranelift::codegen::Context;
use std::collections::{HashMap, HashSet};

use crate::codegen::cranelift::stmt_compiler::StmtCompiler;
use crate::codegen::utils;
use lazy_static::lazy_static;
use std;
use std::sync::Mutex;

pub(crate) type VariableMap = HashMap<String, Variable>;
pub(crate) type FunctionIds = HashMap<String, FuncId>;

lazy_static! {
    pub(crate) static ref CLASSES: Mutex<HashMap<String, CompiledClass>> = Mutex::new(HashMap::new());
}

pub struct Codegen {
    pub builder_context: FunctionBuilderContext,
    pub ctx: Context,
    pub stmt_compiler: StmtCompiler,
    pub func_ids: FunctionIds,
}

#[derive(Clone)]
pub(crate) struct CompiledClass {
    pub field_layout: FieldLayout,
    pub methods: Vec<FuncDecl>
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
    fn compile_class(&mut self, class: &ClassDecl, module: &mut dyn Module) -> anyhow::Result<CompiledClass> {
        self.stmt_compiler.class_fields = HashSet::from_iter(class.clone().params.unwrap_or(Vec::new()).iter().map(|p| p.name.clone()));
        self.stmt_compiler.in_class_scope = true;

        let field_layout: FieldLayout = utils::compute_field_offsets(&class.params);

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

    pub fn declare_classes(&mut self, class_decls: &[ClassDecl], module: &mut dyn Module) -> anyhow::Result<()> {
        for class_decl in class_decls {
            self.compile_class(class_decl, module)?;
        }
        Ok(())
    }

    pub fn declare_funcs(&mut self, funcs: &[FuncDecl], module: &mut dyn Module) -> anyhow::Result<()> {
        for func in funcs {

            let mut sig: Signature = module.make_signature();

            // Check if it's an instance method (first param is `self`)
            let is_method = matches!(func.params.first(), Some(p) if p.name == "self");

            Self::compute_parameters(func, &mut sig, is_method);

            if let Some(ret_ty) = &func.return_type {
                sig.returns.push(AbiParam::new(parse_type(ret_ty.clone())));
            }

            let func_id = module.declare_function(&func.name, Linkage::Export, &sig)?;
            self.func_ids.insert(func.name.clone(), func_id);
        }

        self.stmt_compiler.func_ids = self.func_ids.clone();

        Ok(())
    }

    pub fn declare_funcs_with_ret_type(&mut self, funcs: &[FuncDecl], ret_type: types::Type, module: &mut dyn Module) -> anyhow::Result<()> {
        for func in funcs {

            let mut sig: Signature = module.make_signature();

            // Check if it's an instance method (first param is `self`)
            let is_method = matches!(func.params.first(), Some(p) if p.name == "self");

            Self::compute_parameters(func, &mut sig, is_method);

            sig.returns.push(AbiParam::new(ret_type));

            let func_id = module.declare_function(&func.name, Linkage::Export, &sig)?;
            self.func_ids.insert(func.name.clone(), func_id);
        }

        self.stmt_compiler.func_ids = self.func_ids.clone();

        Ok(())
    }

    fn compute_parameters(func: &FuncDecl, sig: &mut Signature, is_method: bool) {
        for (i, param) in func.params.iter().enumerate() {
            let ty = if is_method && i == 0 {
                types::I64 // assume self is a pointer
            } else {
                parse_type(param.type_annotation.clone().unwrap())
            };
            sig.params.push(AbiParam::new(ty));
        }
    }

    pub fn define_funcs(&mut self, funcs: &[FuncDecl], module: &mut dyn Module) -> anyhow::Result<()> {
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

        // Pointers
        ast::Type::Class(_) => types::I64,
        ast::Type::String => types::I64,
        ast::Type::Array(_, _) => types::I64,
        _ => panic!("Unexpected type in parse_type: {:?}", variable_type),
    }
}

#[derive(Debug, Clone)]
pub struct FieldLayout {
    pub offsets: HashMap<String, i32>, // field name → offset
    pub param_names: Vec<String>,      // ordered list of field names
    pub total_size: i32,
}
