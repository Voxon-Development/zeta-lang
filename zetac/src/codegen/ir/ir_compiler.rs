use crate::ast::{Stmt, FuncDecl, ClassDecl};
use crate::codegen::ir::module::{Function, ZetaModule};
use crate::codegen::ir::optimization::pass::OptimizationPassPriority;
use crate::codegen::ir::stmt::ir_stmt_compiler::IRStmtCompiler;
use crate::frontend;
use rand::rngs::OsRng;
use rand::TryRngCore;
use std::collections::HashMap;

#[derive(Clone, Debug, Default)]
pub struct ClassLayout {
    pub field_indices: HashMap<String, usize>, // field name → index
    pub field_order: Vec<String>,              // deterministic order
}

#[derive(Clone, Debug, Default)]
pub struct ClassTable {
    pub compressed: CompressedClassTable,
    pub name_to_id: HashMap<String, usize>,
}

impl ClassTable {
    #[inline]
    pub fn new() -> ClassTable {
        ClassTable { compressed: CompressedClassTable { layouts: vec![] }, name_to_id: HashMap::new() }
    }
}

#[derive(Clone, Debug, Default)]
pub struct CompressedClassTable {
    pub layouts: Vec<ClassLayout>,
}

pub struct IrCompiler<'a> {
    ir_stmt_compiler: IRStmtCompiler,
    pub module: &'a mut ZetaModule,
    pub classes: ClassTable
}

impl<'a> IrCompiler<'a> {
    pub fn new(module: &'a mut ZetaModule) -> IrCompiler<'a> {
        IrCompiler {
            ir_stmt_compiler: IRStmtCompiler,
            module,
            classes: ClassTable::new()
        }
    }
    
    pub fn compile_file(&mut self, file_name: &str) {
        let mut lexer = frontend::lexer::Lexer::from_file(file_name);
        let tokens = lexer.tokenize().unwrap();
        
        let stmts = frontend::parser::parse_program(tokens).unwrap();

        let functions = stmts.iter()
            .filter_map(|stmt| 
                if let Stmt::FuncDecl(f) = stmt { Some(f.clone()) } else { None })
            .collect::<Vec<FuncDecl>>();

        let classes = stmts.iter()
            .filter_map(|stmt| 
                if let Stmt::ClassDecl(c) = stmt { Some(c.clone()) } else { None })
            .collect::<Vec<ClassDecl>>();
        
        self.compile(functions, classes);
    }
    
    pub fn compile(&mut self, functions: Vec<FuncDecl>, classes: Vec<ClassDecl>) {
        for class in classes {
            self.compile_class(class);
        }
        for func in functions {
            self.compile_func(func);
        }
    }

    fn compile_class(&mut self, class: ClassDecl) {
        let params = class.params.unwrap();

        let layout = ClassLayout {
            field_indices: params.iter().enumerate()
                .map(|(i, param)| (param.name.clone(), i))
                .collect(),
            field_order: params.iter().map(|p| p.name.clone()).collect(),
        };

        let class_id = self.classes.compressed.layouts.len();
        self.classes.compressed.layouts.push(layout);
        self.classes.name_to_id.insert(class.name.clone(), class_id);

        for stmt in class.body.clone() {
            if let Stmt::FuncDecl(func) = stmt {
                self.compile_func(func);
            } else {
                self.ir_stmt_compiler.compile_stmt(&stmt, &self.classes);
            }
        }
    }


    fn compile_func(&mut self, func: FuncDecl) {
        let bytecode = self.ir_stmt_compiler.compile_stmts(&func.body.block, &self.classes);
        
        let mut random = OsRng;

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
            id: random.try_next_u64().unwrap(),
            code: bytecode,
            optimization_level: OptimizationPassPriority::Min
        };

        println!("Function: {:?}", function);

        if func.name == "main" {
            self.module.entry = Some(function.id);
        }
        
        if !self.module.functions.contains_key(&function.id) {
            self.module.add_function(function);
        } else {
            eprintln!("Function {} already exists", func.name);
        }
    }
}