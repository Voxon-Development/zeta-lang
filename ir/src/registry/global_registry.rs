use crate::hir::{HirFunc, HirInterface, HirStruct, HirType, StrId};
use crate::ir_hasher::FxHashMap;
use std::{cell::RefCell, rc::Rc};

#[derive(Clone, Default)]
pub struct ModuleSymbols {
    pub structs: Vec<StrId>,
    pub interfaces: Vec<StrId>,
    pub functions: Vec<StrId>,
    pub struct_interfaces: Vec<StrId>,
    pub struct_methods: Vec<(StrId, StrId)>, // (struct_name, method_name)
}

/// Cloning this is very cheap as every field is wrapped in Rc
#[derive(Clone)]
pub struct GlobalRegistry<'a, 'bump> {
    pub structs: Rc<RefCell<FxHashMap<StrId, HirStruct<'a, 'bump>>>>,
    pub interfaces: Rc<RefCell<FxHashMap<StrId, HirInterface<'a, 'bump>>>>,
    pub functions: Rc<RefCell<FxHashMap<StrId, HirFunc<'a, 'bump>>>>,
    pub struct_interfaces: Rc<RefCell<FxHashMap<StrId, Vec<StrId>>>>,
    pub struct_methods: Rc<RefCell<FxHashMap<StrId, FxHashMap<StrId, StrId>>>>,
    /// (original_name, type_suffix) -> instantiated function name. Shared
    /// across every module's Monomorphizer so `push` for `ArrayList<i32>`
    /// is specialized exactly once for the whole compilation, not once per
    /// module that happens to call it.
    pub instantiated_functions: Rc<RefCell<FxHashMap<(StrId, StrId), StrId>>>,
    /// (original_name, type_suffix) -> instantiated struct name.
    pub instantiated_structs: Rc<RefCell<FxHashMap<(StrId, StrId), StrId>>>,
    /// instantiated struct name -> (base struct name, concrete args it was
    /// built from). This is the reverse of instantiated_structs
    /// needed so a receiver's concrete type (`ArrayList_..._i32`) can
    /// be traced back to "ArrayList instantiated with [i32]" for method
    /// resolution.
    pub instantiated_struct_origins:
        Rc<RefCell<FxHashMap<StrId, (StrId, Vec<HirType<'a, 'bump>>)>>>,
    owned_by_module: Rc<RefCell<FxHashMap<StrId, ModuleSymbols>>>, // key = module name StrId
}

impl<'a, 'bump> GlobalRegistry<'a, 'bump> {
    pub fn new() -> Self {
        Self {
            structs: Rc::new(RefCell::new(FxHashMap::default())),
            interfaces: Rc::new(RefCell::new(FxHashMap::default())),
            functions: Rc::new(RefCell::new(FxHashMap::default())),
            struct_interfaces: Rc::new(RefCell::new(FxHashMap::default())),
            struct_methods: Rc::new(RefCell::new(FxHashMap::default())),
            owned_by_module: Rc::new(RefCell::new(FxHashMap::default())),
            instantiated_struct_origins: Rc::new(RefCell::new(FxHashMap::default())),
            instantiated_structs: Rc::new(RefCell::new(FxHashMap::default())),
            instantiated_functions: Rc::new(RefCell::new(FxHashMap::default())),
        }
    }

    pub fn unregister_module(&self, module: StrId) {
        let Some(owned) = self.owned_by_module.borrow_mut().remove(&module) else {
            return;
        };
        let mut structs = self.structs.borrow_mut();
        for k in owned.structs {
            structs.remove(&k);
        }
        let mut interfaces = self.interfaces.borrow_mut();
        for k in owned.interfaces {
            interfaces.remove(&k);
        }
        let mut functions = self.functions.borrow_mut();
        for k in owned.functions {
            functions.remove(&k);
        }
        let mut si = self.struct_interfaces.borrow_mut();
        for k in owned.struct_interfaces {
            si.remove(&k);
        }
        let mut sm = self.struct_methods.borrow_mut();
        for (struct_name, method_name) in owned.struct_methods {
            if let Some(methods) = sm.get_mut(&struct_name) {
                methods.remove(&method_name);
            }
        }

        self.instantiated_functions.borrow_mut().clear();
        self.instantiated_structs.borrow_mut().clear();
        self.instantiated_struct_origins.borrow_mut().clear();
    }

    pub fn record_owned(&self, module: StrId, symbols: ModuleSymbols) {
        self.owned_by_module.borrow_mut().insert(module, symbols);
    }
}
