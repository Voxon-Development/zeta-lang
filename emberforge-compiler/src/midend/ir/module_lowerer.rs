use std::collections::HashMap;
use ir::hir::{Hir, HirClass, HirFunc, HirInterface, HirModule, HirParam, HirType};
use ir::sea_hasher::SeaHashBuilder;
use ir::ssa_ir::{Function, Module, SsaType};

pub struct MirModuleLowerer {
    pub module: Module,

    // HIR -> SSA metadata for interfaces / classes
    interface_methods: HashMap<String, Vec<(String, Vec<SsaType>, SsaType)>, SeaHashBuilder>,
    interface_id_map: HashMap<String, usize, SeaHashBuilder>,
    interface_method_slots: HashMap<String, HashMap<String, usize, SeaHashBuilder>, SeaHashBuilder>,

    class_mangled_map: HashMap<String, HashMap<String, String, SeaHashBuilder>, SeaHashBuilder>,
    class_vtable_slots: HashMap<String, Vec<String>, SeaHashBuilder>,
    class_method_slots: HashMap<String, HashMap<String, usize, SeaHashBuilder>, SeaHashBuilder>,
    class_field_offsets: HashMap<String, HashMap<String, usize, SeaHashBuilder>, SeaHashBuilder>,
}

impl MirModuleLowerer {
    pub fn new() -> Self {
        Self {
            module: Module::new(),
            interface_methods: HashMap::with_hasher(SeaHashBuilder),
            interface_id_map: HashMap::with_hasher(SeaHashBuilder),
            interface_method_slots: HashMap::with_hasher(SeaHashBuilder),
            class_mangled_map: HashMap::with_hasher(SeaHashBuilder),
            class_vtable_slots: HashMap::with_hasher(SeaHashBuilder),
            class_method_slots: HashMap::with_hasher(SeaHashBuilder),
            class_field_offsets: HashMap::with_hasher(SeaHashBuilder),
        }
    }

    pub fn lower_module(mut self, hir_mod: &HirModule) -> Module {
        for item in &hir_mod.items { if let Hir::Interface(iface) = item { self.lower_interface(iface); } }
        for item in &hir_mod.items { match item { Hir::Class(class) => self.lower_class(class), _ => {} } }
        for item in &hir_mod.items { match item { Hir::Func(func) => self.lower_function(func), _ => {} } }
        self.module
    }

    fn lower_interface(&mut self, hir_iface: &HirInterface) {
        let iface_id = self.interface_id_map.len();
        self.interface_id_map.insert(hir_iface.name.clone(), iface_id);
        let mut methods = Vec::new();
        let mut slot_map = HashMap::with_hasher(SeaHashBuilder);
        for (i, m) in hir_iface.methods.iter().enumerate() {
            let param_types = m.params.iter().map(|p| crate::midend::ir::lowerer::lower_type_hir(&p.ty)).collect::<Vec<_>>();
            let ret = m.return_type.as_ref().map(|t| crate::midend::ir::lowerer::lower_type_hir(t)).unwrap_or(SsaType::Void);
            methods.push((m.name.clone(), param_types, ret));
            slot_map.insert(m.name.clone(), i);
        }
        self.interface_methods.insert(hir_iface.name.clone(), methods);
        self.interface_method_slots.insert(hir_iface.name.clone(), slot_map);
        self.module.interfaces.insert(hir_iface.name.clone(), hir_iface.clone());
    }

    fn lower_class(&mut self, hir_class: &HirClass) {
        self.compute_field_offsets(hir_class);
        self.lower_class_methods(hir_class);
        self.build_class_vtable(hir_class);
        self.module.classes.insert(hir_class.name.clone(), hir_class.clone());
    }

    fn compute_field_offsets(&mut self, hir_class: &HirClass) {
        let mut offsets = HashMap::with_hasher(SeaHashBuilder);
        let has_interfaces = !hir_class.interfaces.is_empty();
        let field_start = if has_interfaces { 1usize } else { 0usize };
        for (i, f) in hir_class.fields.iter().enumerate() { offsets.insert(f.name.clone(), field_start + i); }
        self.class_field_offsets.insert(hir_class.name.clone(), offsets);
    }

    fn lower_class_methods(&mut self, hir_class: &HirClass) {
        let mut mangle_map = HashMap::with_hasher(SeaHashBuilder);
        for method in &hir_class.methods {
            let mut lowered = method.clone();
            if !lowered.is_static {
                lowered.params.insert(0, HirParam {
                    name: "this".to_string(),
                    ty: HirType::Class(
                        "Ptr".to_string(),
                        vec![HirType::Class(hir_class.name.clone(), vec![])]
                    ),
                });
            }
            let mangled_name = format!("{}::{}", hir_class.name, lowered.name);
            lowered.name = mangled_name.clone();
            let func = self.lower_function_inner(&lowered);
            self.module.funcs.insert(func.name.clone(), func);
            mangle_map.insert(method.name.clone(), mangled_name);
        }
        self.class_mangled_map.insert(hir_class.name.clone(), mangle_map);
    }

    fn build_class_vtable(&mut self, hir_class: &HirClass) {
        let mut vtable_slots: Vec<String> = Vec::new();
        let mut class_slot_map = HashMap::with_hasher(SeaHashBuilder);
        for iface_name in &hir_class.interfaces {
            let iface_methods = self.interface_methods.get(iface_name).unwrap_or_else(|| panic!("Class {} implements unknown interface {}", hir_class.name, iface_name));
            for (midx, (mname, _params, _ret)) in iface_methods.iter().enumerate() {
                let mangled = self.class_mangled_map.get(&hir_class.name).and_then(|map| map.get(mname)).unwrap_or_else(|| panic!("Class {} implements interface {} but does not provide method {}", hir_class.name, iface_name, mname)).clone();
                let slot_index = vtable_slots.len();
                vtable_slots.push(mangled.clone());
                class_slot_map.insert(mname.clone(), slot_index);
                let _ = midx;
            }
        }
        self.class_vtable_slots.insert(hir_class.name.clone(), vtable_slots);
        self.class_method_slots.insert(hir_class.name.clone(), class_slot_map);
    }

    fn lower_function(&mut self, hir_fn: &HirFunc) {
        let func = self.lower_function_inner(hir_fn);
        self.module.funcs.insert(func.name.clone(), func);
    }

    fn lower_function_inner(&self, hir_fn: &HirFunc) -> Function {
        let mut fl = crate::midend::ir::lowerer::FunctionLowerer::new(
            hir_fn,
            &self.class_field_offsets,
            &self.class_method_slots,
            &self.class_mangled_map,
            &self.class_vtable_slots,
            &self.interface_id_map,
            &self.interface_method_slots,
            &self.module.classes,
        );
        fl.lower_body(hir_fn.body.as_ref());
        fl.finish()
    }
}