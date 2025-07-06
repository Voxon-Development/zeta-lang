use ir::Class;
use zetac::codegen::ir::module::Function;

// Fat pointer to interface vtable
pub struct InterfaceObject {
    pub object_ptr: *mut (),
    pub vtable_ptr: *const InterfaceVTable,
}

pub struct VTable {
    pub methods: Vec<Function>
}

pub struct InterfaceVTable {
    pub interface_id: usize,
    /// Class implementing this interface
    pub type_id: usize,
    pub method_table: &'static [Function]
}
