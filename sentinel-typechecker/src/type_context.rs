use ir::hir::{HirFunc, HirInterface, HirStruct, HirType};
use std::collections::HashMap;

/// All methods attached to a single named type, gathered from every
/// registration source (struct body, impl blocks, interface impls).
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

/// The name resolution context for a single compilation unit.
///
/// Separation of concerns:
///   - `variables`    - local bindings in the current scope
///   - `structs`      - struct shape (fields, generics)
///   - `interfaces`   - interface shape (method signatures)
///   - `functions`    - free functions (non-method)
///   - `type_methods` - per-type method tables, keyed by the *unmangled*
///                     type name.  Mangling is codegen's problem.
#[derive(Debug, Clone)]
pub struct TypeContext<'a, 'bump> {
    pub variables: HashMap<String, HirType<'a, 'bump>>,
    pub structs: HashMap<String, HirStruct<'a, 'bump>>,
    pub interfaces: HashMap<String, HirInterface<'a, 'bump>>,
    pub functions: HashMap<String, HirFunc<'a, 'bump>>,
    pub type_methods: HashMap<String, TypeMethodTable<'a, 'bump>>,
    pub current_return_type: Option<HirType<'a, 'bump>>,
    pub in_loop: bool,
}

impl<'a, 'bump> TypeContext<'a, 'bump> {
    pub fn new() -> Self {
        Self {
            variables: HashMap::new(),
            structs: HashMap::new(),
            interfaces: HashMap::new(),
            functions: HashMap::new(),
            type_methods: HashMap::new(),
            current_return_type: None,
            in_loop: false,
        }
    }

    pub fn add_function(&mut self, name: String, func: HirFunc<'a, 'bump>) {
        self.functions.insert(name, func);
    }

    pub fn get_function(&self, name: &str) -> Option<HirFunc<'a, 'bump>> {
        self.functions.get(name).copied()
    }

    pub fn add_struct(&mut self, name: String, struct_def: HirStruct<'a, 'bump>) {
        self.structs.insert(name.clone(), struct_def);

        if let Some(methods) = struct_def.methods {
            for func in methods {
                let method_name = func.name.to_string();
                self.type_methods
                    .entry(name.clone())
                    .or_default()
                    .insert(method_name, *func);
            }
        }
    }

    pub fn get_struct(&self, name: &str) -> Option<HirStruct<'a, 'bump>> {
        self.structs.get(name).copied()
    }

    pub fn add_impl_methods(&mut self, target: &str, methods: &[HirFunc<'a, 'bump>]) {
        let table = self.type_methods.entry(target.to_string()).or_default();
        for func in methods {
            table.insert(func.name.to_string(), *func);
        }
    }

    pub fn add_interface(&mut self, name: String, iface: HirInterface<'a, 'bump>) {
        self.interfaces.insert(name, iface);
    }

    pub fn get_interface(&self, name: &str) -> Option<HirInterface<'a, 'bump>> {
        self.interfaces.get(name).copied()
    }

    /// Look up a method on a named type.  This is the only place in the
    /// type checker that should ever need to find methods in structs
    pub fn get_method(&self, type_name: &str, method_name: &str) -> Option<&HirFunc<'a, 'bump>> {
        self.type_methods
            .get(type_name)
            .and_then(|t| t.get(method_name))
    }

    pub fn add_variable(&mut self, name: String, ty: HirType<'a, 'bump>) {
        self.variables.insert(name, ty);
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

    /// Clone everything *except* variables into a child scope.
    /// Variables start fresh so that inner-scope bindings don't leak out,
    /// but type/function knowledge is inherited.
    pub fn create_child_scope(&self) -> Self {
        Self {
            variables: self.variables.clone(), // carry locals into the child
            structs: self.structs.clone(),
            interfaces: self.interfaces.clone(),
            functions: self.functions.clone(),
            type_methods: self.type_methods.clone(),
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
