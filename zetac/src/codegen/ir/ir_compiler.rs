use random;
use random::Source;
use crate::{ast, frontend};
use std::time;
use std::time::Instant;
use crate::codegen::ir::module::{Function, ZetaModule};
use crate::codegen::ir::optimization::pass::OptimizationPassPriority;
use crate::codegen::ir::stmt::ir_stmt_compiler::IRStmtCompiler;

pub struct IrCompiler<'a> {
    ir_stmt_compiler: IRStmtCompiler,
    pub module: &'a mut ZetaModule,
}

impl<'a> IrCompiler<'a> {
    pub fn new(module: &'a mut ZetaModule) -> IrCompiler<'a> {
        IrCompiler {
            ir_stmt_compiler: IRStmtCompiler,
            module
        }
    }
    
    pub fn compile_file(&mut self, file_name: &str) {
        let mut lexer = frontend::lexer::Lexer::from_file(file_name);
        let tokens = lexer.tokenize().unwrap();
        
        let stmts = frontend::parser::parse_program(tokens).unwrap();

        let functions = stmts.iter()
            .filter_map(|stmt| 
                if let ast::Stmt::FuncDecl(f) = stmt { Some(f.clone()) } else { None })
            .collect::<Vec<ast::FuncDecl>>();
        let main = functions.iter()
            .find_map(|func| 
                if func.name == "main" { Some(func.clone()) } else { None })
            .unwrap_or_else(|| panic!("No main function found in file {}", file_name))
            .clone();
        let classes = stmts.iter()
            .filter_map(|stmt| 
                if let ast::Stmt::ClassDecl(c) = stmt { Some(c.clone()) } else { None })
            .collect::<Vec<ast::ClassDecl>>();
        
        self.compile(main, functions, classes);
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
            code: bytecode,
            optimization_level: OptimizationPassPriority::Min
        };

        if func.name == "main" {
            self.module.entry = Some(function.id);
        }
        
        if !self.module.functions.contains_key(&function.id) {
            self.module.functions.insert(function.id, function);
        } else {
            eprintln!("Function {} already exists", func.name);
        }
    }
}