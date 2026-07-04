use codex_dependency_graph::DepGraph;
use ir::{
    hir::{HirEnum, HirFunc, HirInterface, HirStruct, HirType},
    ir_hasher::HashSet,
};
use std::collections::HashMap;
use zetaruntime::bump::GrowableBump;

#[derive(Debug, Clone, Default)]
pub struct TypeMethodTable<'a, 'bump> {
    pub methods: HashMap<String, HirFunc<'a, 'bump>>,
}

impl<'a, 'bump> TypeMethodTable<'a, 'bump> {
    pub fn insert(&mut self, method_name: String, func: HirFunc<'a, 'bump>) {
        self.methods.insert(method_name, func);
    }

    pub fn get(&self, method_name: &str) -> Option<&HirFunc<'a, 'bump>> {
        self.methods.get(method_name)
    }
}

#[derive(Debug, Clone)]
pub struct TypeContext<'a, 'bump> {
    pub variables: HashMap<String, HirType<'a, 'bump>>,
    pub structs: HashMap<String, HirStruct<'a, 'bump>>,
    pub enums: HashMap<String, HirEnum<'a, 'bump>>,
    pub interfaces: HashMap<String, HirInterface<'a, 'bump>>,
    pub module_functions: HashMap<usize, HashMap<String, HirFunc<'a, 'bump>>>,
    pub type_methods: HashMap<String, TypeMethodTable<'a, 'bump>>,
    pub current_return_type: Option<HirType<'a, 'bump>>,
    pub in_loop: bool,
    pub current_module_idx: usize,
    pub dep_graph: &'a DepGraph,
    pub bump: &'bump GrowableBump<'bump>,
    pub dangling_locals: HashSet<String>,
    pub struct_interfaces: HashMap<String, HashSet<String>>,
    pub mutable_variables: HashSet<String>,
}

impl<'a, 'bump> TypeContext<'a, 'bump> {
    pub fn new(dep_graph: &'a DepGraph, bump: &'bump GrowableBump<'bump>) -> Self {
        Self {
            variables: HashMap::new(),
            structs: HashMap::new(),
            enums: HashMap::new(),
            interfaces: HashMap::new(),
            module_functions: HashMap::new(),
            type_methods: HashMap::new(),
            current_return_type: None,
            in_loop: false,
            current_module_idx: usize::MAX, // sentinel
            dep_graph,
            bump,
            dangling_locals: HashSet::default(),
            struct_interfaces: HashMap::default(),
            mutable_variables: HashSet::default(),
        }
    }

    pub fn add_struct_interface(&mut self, struct_name: &str, interface_name: String) {
        self.struct_interfaces
            .entry(struct_name.to_string())
            .or_default()
            .insert(interface_name);
    }

    pub fn struct_implements(&self, struct_name: &str, interface_name: &str) -> bool {
        self.struct_interfaces
            .get(struct_name)
            .is_some_and(|set| set.contains(interface_name))
    }

    pub fn is_local_binding(&self, name: &str) -> bool {
        self.variables.contains_key(name)
    }

    pub fn mark_dangling(&mut self, name: String) {
        self.dangling_locals.insert(name);
    }

    pub fn is_dangling(&self, name: &str) -> bool {
        self.dangling_locals.contains(name)
    }

    pub fn add_function(&mut self, module_idx: usize, name: String, func: HirFunc<'a, 'bump>) {
        self.module_functions
            .entry(module_idx)
            .or_default()
            .insert(name, func);
    }

    pub fn get_module_function(&self, module_idx: usize, name: &str) -> Option<HirFunc<'a, 'bump>> {
        self.module_functions.get(&module_idx)?.get(name).copied()
    }

    pub fn get_function(&self, name: &str) -> Option<HirFunc<'a, 'bump>> {
        self.get_module_function(self.current_module_idx, name)
    }

    pub fn add_struct(&mut self, name: String, struct_def: HirStruct<'a, 'bump>) {
        self.structs.insert(name.clone(), struct_def);
    }

    pub fn get_struct(&self, name: &str) -> Option<HirStruct<'a, 'bump>> {
        self.structs.get(name).copied()
    }

    pub fn add_enum(&mut self, name: String, enum_def: HirEnum<'a, 'bump>) {
        self.enums.insert(name, enum_def);
    }

    pub fn get_enum(&self, name: &str) -> Option<HirEnum<'a, 'bump>> {
        self.enums.get(name).copied()
    }

    pub fn add_impl_methods(&mut self, target: &str, methods: &[HirFunc<'a, 'bump>]) {
        let table = self.type_methods.entry(target.to_string()).or_default();
        for func in methods {
            table.insert(func.unmangled_name.to_string(), *func);
        }
    }

    pub fn add_interface(&mut self, name: String, iface: HirInterface<'a, 'bump>) {
        self.interfaces.insert(name, iface);
    }

    pub fn get_interface(&self, name: &str) -> Option<HirInterface<'a, 'bump>> {
        self.interfaces.get(name).copied()
    }

    pub fn get_method(&self, type_name: &str, method_name: &str) -> Option<&HirFunc<'a, 'bump>> {
        self.type_methods
            .get(type_name)
            .and_then(|t| t.get(method_name))
    }

    pub fn add_variable(&mut self, name: String, ty: HirType<'a, 'bump>) {
        self.variables.insert(name, ty);
    }

    pub fn add_variable_with_mutability(
        &mut self,
        name: String,
        ty: HirType<'a, 'bump>,
        mutable: bool,
    ) {
        if mutable {
            self.mutable_variables.insert(name.clone());
        } else {
            self.mutable_variables.remove(&name);
        }
        self.variables.insert(name, ty);
    }

    pub fn is_mutable(&self, name: &str) -> bool {
        self.mutable_variables.contains(name)
    }

    pub fn get_variable(&self, name: &str) -> Option<HirType<'a, 'bump>> {
        self.variables.get(name).copied()
    }

    pub fn enter_loop(&mut self) {
        self.in_loop = true;
    }
    pub fn exit_loop(&mut self) {
        self.in_loop = false;
    }

    pub fn with_return_type(mut self, return_type: HirType<'a, 'bump>) -> Self {
        self.current_return_type = Some(return_type);
        self
    }

    pub fn create_child_scope(&self) -> Self {
        Self {
            variables: self.variables.clone(),
            structs: self.structs.clone(),
            enums: self.enums.clone(),
            interfaces: self.interfaces.clone(),
            module_functions: self.module_functions.clone(),
            type_methods: self.type_methods.clone(),
            current_return_type: self.current_return_type,
            in_loop: self.in_loop,
            current_module_idx: self.current_module_idx,
            dep_graph: self.dep_graph,
            bump: self.bump,
            dangling_locals: self.dangling_locals.clone(),
            struct_interfaces: self.struct_interfaces.clone(),
            mutable_variables: self.mutable_variables.clone(),
        }
    }
}
