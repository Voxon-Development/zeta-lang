use std::collections::HashMap;
use std::time;
use std::ptr;
use crate::ast;
use ir::{Bytecode, VMValue};
use crate::ast::{FuncDecl, Visibility};
use crate::codegen::ir::optimization::pass::OptimizationPassPriority;

/// Type alias for native function pointers
pub type NativeFnPtr = unsafe extern "C" fn(*const VMValue, usize) -> VMValue;

#[derive(Default, Clone, Debug)]
pub struct Function {
    pub name: String,
    pub id: u64,
    pub params: Vec<ast::Param>,
    pub return_type: Option<ast::Type>,
    pub visibility: Option<Visibility>,
    pub is_method: bool,
    pub is_main: bool,
    pub is_native: bool,
    pub native_pointer: Option<NativeFnPtr>,
    pub locals: Vec<String>,
    pub code: Vec<u8>,
    pub optimization_level: OptimizationPassPriority,
}

impl Function {
    pub fn new_native(name: String, native_pointer: NativeFnPtr) -> Function {
        Function {
            name,
            id: 0,
            params: Vec::new(),
            return_type: None,
            visibility: Some(Visibility::Public),
            is_method: false,
            is_main: false,
            is_native: true,
            native_pointer: Some(native_pointer),
            locals: Vec::new(),
            code: Vec::new(),
            optimization_level: OptimizationPassPriority::Max,
        }
    }
    
    pub fn new_native_with_params(name: String, native_pointer: NativeFnPtr, params: Vec<ast::Param>) -> Function {
        Function {
            name,
            id: 0,
            params,
            return_type: None,
            visibility: None,
            is_method: false,
            is_main: false,
            is_native: true,
            native_pointer: Some(native_pointer),
            locals: Vec::new(),
            code: Vec::new(),
            optimization_level: OptimizationPassPriority::Min,
        }
    }
    
    pub fn new_native_with_params_and_return(name: String, native_pointer: NativeFnPtr, params: Vec<ast::Param>, return_type: ast::Type) -> Function {
        Function {
            name,
            id: 0,
            params,
            return_type: Some(return_type),
            visibility: None,
            is_method: false,
            is_main: false,
            is_native: true,
            native_pointer: Some(native_pointer),
            locals: Vec::new(),
            code: Vec::new(),
            optimization_level: OptimizationPassPriority::Min,
        }
    }
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
    
    pub fn add_function(&mut self, function: Function) {
        self.functions_by_name.insert(function.name.clone(), function.id);
        self.functions.insert(function.id, function);
    }
    
    pub fn get_function_by_name(&self, name: &String) -> Option<&Function> {
        let id = self.functions_by_name.get(name).cloned();
        match id {
            Some(id) => self.functions.get(&id),
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