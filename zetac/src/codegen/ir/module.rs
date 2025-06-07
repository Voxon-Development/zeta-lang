use std::collections::HashMap;
use crate::ast;
use ir::Bytecode;

pub struct Function {
    pub name: String,
    pub id: u64,
    pub params: Vec<ast::Param>,
    pub return_type: Option<ast::Type>,
    pub visibility: Option<String>,
    pub is_method: bool,
    pub is_main: bool,
    pub is_native: bool,
    pub native_pointer: Option<*const u8>,
    pub locals: Vec<String>,
    pub code: Vec<Bytecode>,
}

pub struct ZetaModule {
    pub functions: HashMap<u64, Function>,
    pub entry: String,
}

impl ZetaModule {
    pub fn new() -> ZetaModule {
        ZetaModule {
            functions: HashMap::new(),
            entry: String::new(),
        }
    }
    
    pub fn add_function(&mut self, function: Function) {
        self.functions.insert(function.id, function);
    }
    
    pub fn get_function(&self, id: u64) -> Option<&Function> {
        self.functions.get(&id)
    }
    
    pub fn get_function_mut(&mut self, id: u64) -> Option<&mut Function> {
        self.functions.get_mut(&id)
    }
}