use random;
use random::Source;
use crate::ast;
use std::time;
use std::time::Instant;
use lazy_static::lazy_static;
use crate::codegen::ir::module::{Function, ZetaModule};
use crate::codegen::ir::stmt::ir_stmt_compiler::IRStmtCompiler;

pub struct IrCompiler {
    ir_stmt_compiler: IRStmtCompiler,
    module: &'static mut ZetaModule
}

impl IrCompiler {
    pub fn new(module: &'static mut ZetaModule) -> IrCompiler {
        IrCompiler { 
            ir_stmt_compiler: IRStmtCompiler::new(),
            module
        }
    }
    
    pub fn compile(&mut self, main: ast::FuncDecl, functions: Vec<ast::FuncDecl>, classes: Vec<ast::ClassDecl>) {
        for class in classes {
            self.compile_class(&class);
        }
        for func in functions {
            self.compile_func(&func);
        }
        self.compile_func(&main);
    }
    
    fn compile_class(&mut self, class: &ast::ClassDecl) {
        for stmt in class.body.clone() {
            if let ast::Stmt::FuncDecl(func) = stmt {
                self.compile_func(&func);
                continue;
            }
            
            self.ir_stmt_compiler.compile_stmt(&stmt);
        }
    }
    
    fn compile_func(&mut self, func: &ast::FuncDecl) {
        let bytecode = self.ir_stmt_compiler.compile_stmts(&func.body.block);
        
        let seed: u64 = time::SystemTime::now().duration_since(time::SystemTime::UNIX_EPOCH).unwrap().as_millis() as u64;
        let mut random = random::default(seed);

        let function = Function {
            name: func.name.clone(),
            params: func.params.clone(),
            return_type: func.return_type.clone(),
            visibility: func.visibility.clone(),
            is_method: matches!(func.params.first(), Some(p) if p.name == "self"),
            is_main: func.name == "main",
            locals: vec![],
            is_native: false,
            native_pointer: None,
            id: random.read_u64(),
            code: bytecode
        };
        
        if !self.module.functions.contains_key(&function.id) {
            self.module.functions.insert(function.id, function);
        } else {
            panic!("Function {} already exists", func.name);
        }
    }
}