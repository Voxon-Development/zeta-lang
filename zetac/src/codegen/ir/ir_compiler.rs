use crate::ast;

pub struct IrCompiler;

impl IrCompiler {
    pub fn new() -> IrCompiler {
        IrCompiler
    }
    
    pub fn compile(&self, main: ast::FuncDecl, functions: Vec<ast::FuncDecl>, classes: Vec<ast::ClassDecl>) {
        for class in classes {
            self.compile_class(&class);
        }
        for func in functions {
            self.compile_func(&func);
        }
        self.compile_func(&main);
    }
    
    fn compile_class(&self, class: &ast::ClassDecl) {
        
    }
    
    fn compile_func(&self, func: &ast::FuncDecl) {
        
    }
}