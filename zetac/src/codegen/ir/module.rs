use std::collections::HashMap;
use std::time;
use crate::ast;
use ir::Bytecode;
use crate::ast::{FuncDecl, Visibility};
use crate::codegen::ir::optimization::pass::OptimizationPassPriority;

use seahash;

// seahash builder
#[derive(Default)]
pub struct SeaHashBuilder;

impl std::hash::BuildHasher for SeaHashBuilder {
    type Hasher = seahash::SeaHasher;

    fn build_hasher(&self) -> Self::Hasher {
        seahash::SeaHasher::new()
    }
}

#[derive(Default, Debug, Clone)]
pub struct Function {
    pub name: String,
    pub id: u64,
    pub params: Vec<ast::Param>,
    pub return_type: Option<ast::Type>,
    pub visibility: Option<Visibility>,
    pub is_method: bool,
    pub is_main: bool,
    pub is_native: bool,
    pub native_pointer: Option<Box<*const u8>>,
    pub locals: Vec<String>,
    pub code: Vec<u8>,
    pub optimization_level: OptimizationPassPriority
}

unsafe impl Send for Function {}
unsafe impl Sync for Function {}


pub struct ZetaModule {
    pub functions: HashMap<u64, Function>,
    pub functions_by_name: HashMap<String, u64>,
    pub entry: Option<u64>,
}

impl ZetaModule {
    pub fn new() -> ZetaModule {
        ZetaModule {
            functions: HashMap::new(),
            functions_by_name: HashMap::new(),
            entry: None,
        }
    }
    
    pub fn println_str(string: &str) {
        println!("{}", string);
    }
    
    pub fn add_function(&mut self, function: Function) {
        self.functions_by_name.insert(function.name.clone(), function.id);
        self.functions.insert(function.id, function);
    }
    
    pub fn get_function_by_name(&self, name: &String) -> Option<Function> {
        let id = self.functions_by_name.get(name).cloned();
        println!("ID: {:?}, Name: {}", id, name);
        match id {
            Some(id) => self.functions.get(&id).cloned(),
            None => None
        }
    }
    
    pub fn replace_function(&mut self, function: Function) {
        self.functions.insert(function.id, function);
    }
    
    pub fn get_function(&self, id: u64) -> Option<&Function> {
        self.functions.get(&id)
    }
    
    #[inline]
    pub fn get_function_mut(&mut self, id: u64) -> Option<&mut Function> {
        self.functions.get_mut(&id)
    }
}