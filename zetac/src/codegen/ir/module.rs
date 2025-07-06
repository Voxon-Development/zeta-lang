use crate::ast;
use crate::ast::Visibility;
use crate::codegen::ir::optimization::pass::OptimizationPassPriority;
use ir::VMValue;
use std::collections::HashMap;
use dashmap::DashMap;
use ir::bump::{AtomicBump, Bump};

/// Type alias for native function pointers
pub type NativeFnPtr = unsafe extern "C" fn(*const VMValue, usize) -> VMValue;

#[derive(Clone, Debug)]
pub struct Function {
    pub name: String,
    pub id: u64,
    pub params: Vec<ast::Param>,
    pub return_type: Option<ast::Type>,
    pub visibility: Option<Visibility>,
    pub is_method: bool,
    pub is_main: bool,
    pub native_pointer: NativeFnPtr,
    pub code: Vec<u8, AtomicBump>,
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
            native_pointer,
            code: Vec::new_in(AtomicBump::new()),
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
            native_pointer,
            code: Vec::new_in(AtomicBump::new()),
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
            native_pointer,
            code: Vec::new_in(AtomicBump::new()),
            optimization_level: OptimizationPassPriority::Min,
        }
    }
}

unsafe impl Send for Function {}
unsafe impl Sync for Function {}

pub struct ZetaModule {
    pub functions: Vec<Function, AtomicBump>,
    pub functions_by_name: DashMap<String, u64>,
    pub entry: Option<NativeFnPtr>,
}

unsafe impl Send for ZetaModule {}
unsafe impl Sync for ZetaModule {}

impl ZetaModule {
    pub fn new() -> ZetaModule {
        ZetaModule {
            functions: Vec::new_in(AtomicBump::new()),
            functions_by_name: DashMap::new(),
            entry: None,
        }
    }
    
    pub fn add_function(&mut self, function: Function) {
        self.functions_by_name.insert(function.name.clone(), function.id);
        self.functions.insert(function.id as usize, function);
    }
    
    pub fn clear(&mut self) {
        self.functions.clear();
        self.functions_by_name.clear();
        self.entry = None;
    }
    
    pub fn get_function_by_name(&self, name: &String) -> Option<&Function> {
        let id = self.functions_by_name.get(name);
        match id {
            Some(id) => self.functions.get(*id as usize),
            None => None
        }
    }
    
    pub fn replace_function(&mut self, function: Function) {
        self.functions.insert(function.id as usize, function);
    }
    
    pub fn get_function(&self, id: u64) -> Option<&Function> {
        self.functions.get(id as usize)
    }
    
    #[inline]
    pub fn get_function_mut(&mut self, id: u64) -> Option<&mut Function> {
        self.functions.get_mut(id as usize)
    }
}