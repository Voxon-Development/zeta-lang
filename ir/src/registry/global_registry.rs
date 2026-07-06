use crate::hir::{HirFunc, HirInterface, HirStruct, StrId};
use crate::ir_hasher::FxHashMap;
use std::{cell::RefCell, rc::Rc};

/// Cloning this is very cheap as every field is wrapped in Rc
#[derive(Clone)]
pub struct GlobalRegistry<'a, 'bump> {
    pub classes: Rc<RefCell<FxHashMap<StrId, HirStruct<'a, 'bump>>>>,
    pub interfaces: Rc<RefCell<FxHashMap<StrId, HirInterface<'a, 'bump>>>>,
    pub functions: Rc<RefCell<FxHashMap<StrId, HirFunc<'a, 'bump>>>>,
    pub struct_interfaces: Rc<RefCell<FxHashMap<StrId, Vec<StrId>>>>,
    pub struct_methods: Rc<RefCell<FxHashMap<StrId, FxHashMap<StrId, StrId>>>>,
}

impl<'a, 'bump> GlobalRegistry<'a, 'bump> {
    pub fn new() -> Self {
        Self {
            classes: Rc::new(RefCell::new(FxHashMap::default())),
            interfaces: Rc::new(RefCell::new(FxHashMap::default())),
            functions: Rc::new(RefCell::new(FxHashMap::default())),
            struct_interfaces: Rc::new(RefCell::new(FxHashMap::default())),
            struct_methods: Rc::new(RefCell::new(FxHashMap::default())),
        }
    }
}
