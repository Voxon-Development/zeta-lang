use codex_dependency_graph::DepGraph;
use ir::{
    hir::{HirEnum, HirFunc, HirInterface, HirStruct, HirType, StrId},
    ir_hasher::{HashMap, HashSet},
};
use std::{cell::RefCell, sync::Arc};
use zetaruntime::{bump::GrowableBump, string_pool::StringPool};

use crate::type_checker::SymbolId;

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
    pub variables: HashMap<String, (SymbolId, HirType<'a, 'bump>)>,
    pub structs: HashMap<String, HirStruct<'a, 'bump>>,
    pub struct_owner_module: HashMap<String, usize>,
    pub enums: HashMap<String, HirEnum<'a, 'bump>>,
    pub enum_owner_module: HashMap<String, usize>,
    pub interfaces: HashMap<String, HirInterface<'a, 'bump>>,
    pub interface_owner_module: HashMap<String, usize>,
    pub module_functions: HashMap<usize, HashMap<String, HirFunc<'a, 'bump>>>,
    pub type_methods: HashMap<String, TypeMethodTable<'a, 'bump>>,
    pub current_return_type: Option<HirType<'a, 'bump>>,
    pub in_loop: bool,
    pub current_module_idx: usize,
    pub dep_graph: &'a RefCell<DepGraph>,
    pub bump: &'bump GrowableBump<'bump>,
    pub dangling_locals: HashSet<String>,
    pub struct_interfaces: HashMap<String, HashSet<String>>,
    pub mutable_variables: HashSet<String>,
    pub string_pool: Arc<StringPool>,
    pub generic_struct_instantiations: RefCell<
        Vec<(
            (StrId, Vec<HirType<'a, 'bump>>),
            &'bump [HirType<'a, 'bump>],
        )>,
    >,
    pub generic_enum_instantiations: RefCell<
        Vec<(
            (StrId, Vec<HirType<'a, 'bump>>),
            &'bump [(StrId, &'bump [HirType<'a, 'bump>])],
        )>,
    >,
}

impl<'a, 'bump> TypeContext<'a, 'bump> {
    pub fn new(
        dep_graph: &'a RefCell<DepGraph>,
        bump: &'bump GrowableBump<'bump>,
        string_pool: Arc<StringPool>,
    ) -> Self {
        Self {
            variables: HashMap::default(),
            structs: HashMap::default(),
            enums: HashMap::default(),
            interfaces: HashMap::default(),
            module_functions: HashMap::default(),
            type_methods: HashMap::default(),
            current_return_type: None,
            in_loop: false,
            current_module_idx: usize::MAX, // sentinel
            dep_graph,
            bump,
            dangling_locals: HashSet::default(),
            struct_interfaces: HashMap::default(),
            mutable_variables: HashSet::default(),
            string_pool,
            struct_owner_module: HashMap::default(),
            enum_owner_module: HashMap::default(),
            interface_owner_module: HashMap::default(),
            generic_struct_instantiations: RefCell::new(Vec::new()),
            generic_enum_instantiations: RefCell::new(Vec::new()),
        }
    }

    pub fn add_struct_interface(&mut self, struct_name: &str, interface_name: String) {
        self.struct_interfaces
            .entry(struct_name.to_string())
            .or_default()
            .insert(interface_name);
    }

    pub fn get_enum_instantiation(
        &self,
        name: StrId,
        args: &[HirType<'a, 'bump>],
    ) -> Option<&'bump [(StrId, &'bump [HirType<'a, 'bump>])]> {
        self.generic_enum_instantiations
            .borrow()
            .iter()
            .find(|((n, a), _)| *n == name && a.as_slice() == args)
            .map(|(_, variants)| *variants)
    }

    pub fn cache_enum_instantiation(
        &self,
        name: StrId,
        args: Vec<HirType<'a, 'bump>>,
        variants: &'bump [(StrId, &'bump [HirType<'a, 'bump>])],
    ) {
        self.generic_enum_instantiations
            .borrow_mut()
            .push(((name, args), variants));
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

    pub fn get_struct_instantiation(
        &self,
        name: StrId,
        args: &[HirType<'a, 'bump>],
    ) -> Option<&'bump [HirType<'a, 'bump>]> {
        self.generic_struct_instantiations
            .borrow()
            .iter()
            .find(|((n, a), _)| *n == name && a.as_slice() == args)
            .map(|(_, fields)| *fields)
    }

    pub fn cache_struct_instantiation(
        &self,
        name: StrId,
        args: Vec<HirType<'a, 'bump>>,
        fields: &'bump [HirType<'a, 'bump>],
    ) {
        self.generic_struct_instantiations
            .borrow_mut()
            .push(((name, args), fields));
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

    pub fn get_struct(&self, name: &str) -> Option<HirStruct<'a, 'bump>> {
        self.structs.get(name).copied()
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

    pub fn add_struct(
        &mut self,
        module_idx: usize,
        name: String,
        struct_def: HirStruct<'a, 'bump>,
    ) {
        self.struct_owner_module.insert(name.clone(), module_idx);
        self.structs.insert(name, struct_def);
    }
    pub fn struct_owner(&self, name: &str) -> Option<usize> {
        self.struct_owner_module.get(name).copied()
    }

    pub fn add_enum(&mut self, module_idx: usize, name: String, enum_def: HirEnum<'a, 'bump>) {
        self.enum_owner_module.insert(name.clone(), module_idx);
        self.enums.insert(name, enum_def);
    }
    pub fn enum_owner(&self, name: &str) -> Option<usize> {
        self.enum_owner_module.get(name).copied()
    }

    pub fn add_interface(
        &mut self,
        module_idx: usize,
        name: String,
        iface: HirInterface<'a, 'bump>,
    ) {
        self.interface_owner_module.insert(name.clone(), module_idx);
        self.interfaces.insert(name, iface);
    }
    pub fn interface_owner(&self, name: &str) -> Option<usize> {
        self.interface_owner_module.get(name).copied()
    }

    pub fn get_interface(&self, name: &str) -> Option<HirInterface<'a, 'bump>> {
        self.interfaces.get(name).copied()
    }

    pub fn get_method(&self, type_name: &str, method_name: &str) -> Option<&HirFunc<'a, 'bump>> {
        self.type_methods
            .get(type_name)
            .and_then(|t| t.get(method_name))
    }

    pub fn add_variable(&mut self, name: String, ty: HirType<'a, 'bump>, symbol_id: SymbolId) {
        self.variables.insert(name, (symbol_id, ty));
    }

    pub fn add_variable_with_mutability(
        &mut self,
        name: String,
        ty: HirType<'a, 'bump>,
        mutable: bool,
        symbol_id: SymbolId,
    ) {
        if mutable {
            self.mutable_variables.insert(name.clone());
        } else {
            self.mutable_variables.remove(&name);
        }
        self.variables.insert(name, (symbol_id, ty));
    }

    pub fn is_mutable(&self, name: &str) -> bool {
        self.mutable_variables.contains(name)
    }

    pub fn get_variable(&self, name: &str) -> Option<(SymbolId, HirType<'a, 'bump>)> {
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

    pub fn mangle_function_name(&self, name: StrId) -> StrId {
        let Some(pkg) = self
            .dep_graph
            .borrow()
            .get_module_package(self.current_module_idx)
        else {
            return name;
        };

        let pkg_str = pkg.to_string();
        let mut segments: Vec<StrId> = pkg_str
            .split("::")
            .map(|seg| StrId(self.string_pool.intern(seg)))
            .collect();
        segments.push(name);

        let joined = segments
            .iter()
            .map(|s| s.to_string())
            .collect::<Vec<_>>()
            .join("_");

        StrId(self.string_pool.intern(&joined))
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
            string_pool: self.string_pool.clone(),
            struct_owner_module: self.struct_owner_module.clone(),
            enum_owner_module: self.enum_owner_module.clone(),
            interface_owner_module: self.interface_owner_module.clone(),
            generic_struct_instantiations: self.generic_struct_instantiations.clone(),
            generic_enum_instantiations: self.generic_enum_instantiations.clone(),
        }
    }
}
