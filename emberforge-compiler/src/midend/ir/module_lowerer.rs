use crate::midend::ir::ir_conversion::lower_type_hir;
use ir::context::Context;
use ir::hir::{Hir, HirClass, HirFunc, HirInterface, HirModule, HirParam, HirType, StrId};
use ir::ir_hasher::FxHashBuilder;
use ir::ssa_ir::{Function, Module, SsaType};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub struct MirModuleLowerer {
    pub module: Module,

    // HIR -> SSA metadata for interfaces / classes
    interface_methods: HashMap<StrId, Vec<(StrId, Vec<SsaType>, SsaType)>, FxHashBuilder>,
    interface_id_map: HashMap<StrId, usize, FxHashBuilder>,
    interface_method_slots: HashMap<StrId, HashMap<StrId, usize, FxHashBuilder>, FxHashBuilder>,

    class_mangled_map: HashMap<StrId, HashMap<StrId, StrId, FxHashBuilder>, FxHashBuilder>,
    class_vtable_slots: HashMap<StrId, Vec<StrId>, FxHashBuilder>,
    class_method_slots: HashMap<StrId, HashMap<StrId, usize, FxHashBuilder>, FxHashBuilder>,
    class_field_offsets: HashMap<StrId, HashMap<StrId, usize, FxHashBuilder>, FxHashBuilder>,

    context: Rc<RefCell<Context<'static>>>,
}

impl MirModuleLowerer {
    pub fn new(context: Rc<RefCell<Context<'static>>>) -> Self {
        Self {
            module: Module::new(),
            interface_methods: HashMap::with_hasher(FxHashBuilder),
            interface_id_map: HashMap::with_hasher(FxHashBuilder),
            interface_method_slots: HashMap::with_hasher(FxHashBuilder),
            class_mangled_map: HashMap::with_hasher(FxHashBuilder),
            class_vtable_slots: HashMap::with_hasher(FxHashBuilder),
            class_method_slots: HashMap::with_hasher(FxHashBuilder),
            class_field_offsets: HashMap::with_hasher(FxHashBuilder),
            context,
        }
    }

    pub fn lower_module(mut self, hir_mod: &HirModule) -> Module {
        for item in &hir_mod.items {
            if let Hir::Interface(iface) = item {
                self.lower_interface(iface);
            }
        }

        for item in &hir_mod.items {
            match item {
                Hir::Class(class) => self.lower_class(class),
                _ => {}
            }
        }

        for item in &hir_mod.items {
            match item {
                Hir::Func(func) => self.lower_function(func),
                _ => {}
            }
        }

        self.module
    }

    fn lower_interface(&mut self, hir_iface: &HirInterface) {
        let iface_id = self.interface_id_map.len();
        self.interface_id_map
            .insert(hir_iface.name.clone(), iface_id);
        let mut methods = Vec::new();
        let mut slot_map = HashMap::with_hasher(FxHashBuilder);
        for (i, m) in hir_iface.methods.iter().enumerate() {
            let param_types = m
                .params
                .iter()
                .map(|p| match p {
                    HirParam::Normal {
                        name: _,
                        param_type,
                    } => lower_type_hir(param_type),
                    HirParam::This { param_type } => {
                        lower_type_hir(param_type.as_ref().unwrap_or(&HirType::This))
                    }
                })
                .collect::<Vec<_>>();
            let ret = m
                .return_type
                .as_ref()
                .map(|t| lower_type_hir(t))
                .unwrap_or(SsaType::Void);
            methods.push((m.name.clone(), param_types, ret));
            slot_map.insert(m.name.clone(), i);
        }

        self.interface_methods
            .insert(hir_iface.name.clone(), methods);
        self.interface_method_slots.insert(hir_iface.name, slot_map);
        self.module
            .interfaces
            .insert(hir_iface.name, hir_iface.clone());
    }

    fn lower_class(&mut self, hir_class: &HirClass) {
        self.compute_field_offsets(hir_class);
        self.build_class_vtable(hir_class);
        self.module
            .classes
            .insert(hir_class.name, hir_class.clone());
    }

    fn compute_field_offsets(&mut self, hir_class: &HirClass) {
        let mut offsets = HashMap::with_hasher(FxHashBuilder);
        let has_interfaces = !hir_class.interfaces.is_empty();
        let field_start = if has_interfaces { 1usize } else { 0usize };
        for (i, f) in hir_class.fields.iter().enumerate() {
            offsets.insert(f.name, field_start + i);
        }
        self.class_field_offsets.insert(hir_class.name, offsets);
    }

    fn build_class_vtable(&mut self, hir_class: &HirClass) {
        let mut vtable_slots: Vec<StrId> = Vec::new();
        let mut class_slot_map: HashMap<StrId, usize, FxHashBuilder> =
            HashMap::with_hasher(FxHashBuilder);
        for iface_name in &hir_class.interfaces {
            let iface_methods = self.interface_methods.get(iface_name).unwrap_or_else(|| {
                panic!(
                    "Class {} implements unknown interface {}",
                    hir_class.name, iface_name
                )
            });
            for (midx, (mname, _params, _ret)) in iface_methods.iter().enumerate() {
                let mangled = self
                    .class_mangled_map
                    .get(&hir_class.name)
                    .and_then(|map| map.get(mname))
                    .unwrap_or_else(|| {
                        panic!(
                            "Class {} implements interface {} but does not provide method {}",
                            hir_class.name, iface_name, mname
                        )
                    })
                    .clone();

                let slot_index = vtable_slots.len();
                vtable_slots.push(mangled.clone());
                class_slot_map.insert(mname.clone(), slot_index);
                let _ = midx;
            }
        }
        self.class_vtable_slots
            .insert(hir_class.name.clone(), vtable_slots);
        self.class_method_slots
            .insert(hir_class.name.clone(), class_slot_map);
    }

    fn lower_function(&mut self, hir_fn: &HirFunc) {
        let func = self.lower_function_inner(hir_fn);
        self.module.funcs.insert(func.name.clone(), func);
    }

    fn lower_function_inner(&self, hir_fn: &HirFunc) -> Function {
        let mut fl = crate::midend::ir::lowerer::FunctionLowerer::new(
            hir_fn,
            &self.module.funcs,
            &self.class_field_offsets,
            &self.class_method_slots,
            &self.class_mangled_map,
            &self.class_vtable_slots,
            &self.interface_id_map,
            &self.interface_method_slots,
            &self.module.classes,
            self.context.clone(),
        )
        .unwrap();
        fl.lower_body(hir_fn.body.as_ref());
        fl.finish()
    }
}
