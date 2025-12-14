use std::collections::HashMap;
use ir::hir::{HirType, StrId, HirStruct, HirInterface, HirFunc};

#[derive(Debug, Clone)]
pub struct TypeContext<'a, 'bump> {
    /// Maps variable names to their types
    pub variables: HashMap<String, HirType<'a, 'bump>>,
    /// Maps struct names to their definitions
    pub structs: HashMap<String, HirStruct<'a, 'bump>>,
    /// Maps interface names to their definitions
    pub interfaces: HashMap<String, HirInterface<'a, 'bump>>,
    /// Maps function names to their definitions
    pub functions: HashMap<String, HirFunc<'a, 'bump>>,
    /// Current function's return type (for return type checking)
    pub current_return_type: Option<HirType<'a, 'bump>>,
    /// Track if we're inside a loop (for break/continue checking)
    pub in_loop: bool,
}

impl<'a, 'bump> TypeContext<'a, 'bump> {
    pub fn new() -> Self {
        Self {
            variables: HashMap::new(),
            structs: HashMap::new(),
            interfaces: HashMap::new(),
            functions: HashMap::new(),
            current_return_type: None,
            in_loop: false,
        }
    }

    pub fn with_return_type(mut self, return_type: HirType<'a, 'bump>) -> Self {
        self.current_return_type = Some(return_type);
        self
    }

    pub fn enter_loop(&mut self) {
        self.in_loop = true;
    }

    pub fn exit_loop(&mut self) {
        self.in_loop = false;
    }

    pub fn add_variable(&mut self, name: String, ty: HirType<'a, 'bump>) {
        self.variables.insert(name, ty);
    }

    pub fn get_variable(&self, name: &str) -> Option<HirType<'a, 'bump>> {
        self.variables.get(name).copied()
    }

    pub fn add_struct(&mut self, name: String, struct_def: HirStruct<'a, 'bump>) {
        self.structs.insert(name, struct_def);
    }

    pub fn get_struct(&self, name: &str) -> Option<HirStruct<'a, 'bump>> {
        self.structs.get(name).copied()
    }

    pub fn add_interface(&mut self, name: String, interface_def: HirInterface<'a, 'bump>) {
        self.interfaces.insert(name, interface_def);
    }

    pub fn get_interface(&self, name: &str) -> Option<HirInterface<'a, 'bump>> {
        self.interfaces.get(name).copied()
    }

    pub fn add_function(&mut self, name: String, func_def: HirFunc<'a, 'bump>) {
        self.functions.insert(name, func_def);
    }

    pub fn get_function(&self, name: &str) -> Option<HirFunc<'a, 'bump>> {
        self.functions.get(name).copied()
    }

    pub fn create_child_scope(&self) -> Self {
        Self {
            variables: self.variables.clone(),
            structs: self.structs.clone(),
            interfaces: self.interfaces.clone(),
            functions: self.functions.clone(),
            current_return_type: self.current_return_type,
            in_loop: self.in_loop,
        }
    }
}

impl<'a, 'bump> Default for TypeContext<'a, 'bump> {
    fn default() -> Self {
        Self::new()
    }
}
