use crate::midend::ir::ir_conversion::lower_type_hir;
use crate::midend::ir::lowerer::FunctionLowerer;
use codex_dependency_graph::DepGraph;
use ir::hir::{Hir, HirFunc, HirInterface, HirModule, HirParam, HirStruct, HirType, StrId};
use ir::ir_hasher::{FxHashBuilder, HashSet};
use ir::ssa_ir::{Function, Module, SsaType};
use std::collections::HashMap;
use std::marker::PhantomData;
use std::sync::Arc;
use zetaruntime::string_pool::StringPool;

pub struct MirModuleLowerer<'a, 'cx, 'bump>
where
    'bump: 'a,
    'bump: 'cx,
    'cx: 'a,
{
    pub module: Module<'a, 'bump>,

    // HIR -> SSA metadata for interfaces / classes
    interface_methods: HashMap<StrId, Vec<(StrId, Vec<SsaType>, SsaType)>, FxHashBuilder>,
    interface_id_map: HashMap<StrId, usize, FxHashBuilder>,
    interface_method_slots: HashMap<StrId, HashMap<StrId, usize, FxHashBuilder>, FxHashBuilder>,

    class_mangled_map: HashMap<StrId, HashMap<StrId, StrId, FxHashBuilder>, FxHashBuilder>,
    class_vtable_slots: HashMap<StrId, Vec<StrId>, FxHashBuilder>,
    class_method_slots: HashMap<StrId, HashMap<StrId, usize, FxHashBuilder>, FxHashBuilder>,
    class_field_offsets: HashMap<StrId, HashMap<StrId, usize, FxHashBuilder>, FxHashBuilder>,

    phantom_data: PhantomData<&'cx ()>,

    context: Arc<StringPool>,
    extern_c_names: &'a HashSet<StrId>,

    pub dep_graph: &'a DepGraph,
    pub module_idx: usize,
}

impl<'a, 'cx, 'bump> MirModuleLowerer<'a, 'cx, 'bump>
where
    'bump: 'a,
    'bump: 'cx,
    'cx: 'a,
{
    pub fn new(
        context: Arc<StringPool>,
        extern_c_names: &'a HashSet<StrId>,
        dep_graph: &'a DepGraph,
        module_idx: usize,
    ) -> Self {
        Self {
            module: Module::new(),
            interface_methods: HashMap::with_hasher(FxHashBuilder),
            interface_id_map: HashMap::with_hasher(FxHashBuilder),
            interface_method_slots: HashMap::with_hasher(FxHashBuilder),
            class_mangled_map: HashMap::with_hasher(FxHashBuilder),
            class_vtable_slots: HashMap::with_hasher(FxHashBuilder),
            class_method_slots: HashMap::with_hasher(FxHashBuilder),
            class_field_offsets: HashMap::with_hasher(FxHashBuilder),
            phantom_data: PhantomData,
            context,
            extern_c_names,
            dep_graph,
            module_idx,
        }
    }

    pub fn lower_module(mut self, hir_mod: HirModule<'a, 'bump>) -> Module<'a, 'bump> {
        for item in hir_mod.items {
            match item {
                Hir::Struct(class) => self.lower_class(*class),
                Hir::Interface(iface) => self.lower_interface(*iface),
                Hir::Func(func) => self.lower_function(*func),
                Hir::Impl(impl_block) => {
                    if let Some(methods) = impl_block.methods {
                        for method in methods {
                            self.class_mangled_map
                                .entry(impl_block.target)
                                .or_insert_with(|| HashMap::with_hasher(FxHashBuilder))
                                .insert(method.unmangled_name, method.name);
                        }
                        for method in methods {
                            self.lower_class_method(method, impl_block.target);
                        }
                    }
                }
                _ => {}
            }
        }

        self.module
    }

    fn lower_interface(&mut self, hir_iface: &HirInterface<'a, 'bump>) {
        let iface_id = self.interface_id_map.len();
        self.interface_id_map.insert(hir_iface.name, iface_id);
        let mut methods = Vec::new();
        let mut slot_map = HashMap::with_hasher(FxHashBuilder);
        for (i, m) in hir_iface.methods.unwrap().iter().enumerate() {
            let param_types: Vec<SsaType> = m
                .params
                .unwrap_or_default()
                .iter()
                .map(|p| match p {
                    HirParam::Normal {
                        name: _,
                        param_type,
                    } => lower_type_hir(&param_type),
                    HirParam::This { kind: _ } => lower_type_hir(&HirType::This),
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

    fn lower_class(&mut self, hir_class: &HirStruct<'a, 'bump>) {
        self.compute_field_offsets(hir_class);

        self.build_mangled_map(hir_class);

        self.build_class_vtable(hir_class);

        self.module
            .classes
            .insert(hir_class.name, hir_class.clone());

        if let Some(methods) = hir_class.methods {
            for method in methods {
                self.lower_class_method(method, hir_class.name);
            }
        }
    }

    fn build_mangled_map(&mut self, hir_class: &HirStruct<'a, 'bump>) {
        let mut mmap: HashMap<StrId, StrId, FxHashBuilder> = HashMap::with_hasher(FxHashBuilder);

        if let Some(methods) = hir_class.methods {
            for method in methods {
                mmap.insert(method.unmangled_name, method.name);
            }
        }

        self.class_mangled_map.insert(hir_class.name, mmap);
    }

    fn compute_field_offsets(&mut self, hir_class: &HirStruct<'a, 'bump>) {
        let mut offsets = HashMap::with_hasher(FxHashBuilder);

        for (i, f) in hir_class.fields.iter().enumerate() {
            offsets.insert(f.name, i);
        }

        self.class_field_offsets.insert(hir_class.name, offsets);
    }

    fn build_class_vtable(&mut self, hir_class: &HirStruct<'a, 'bump>) {
        let Some(interfaces) = hir_class.interfaces else {
            return;
        };

        let class_methods = self
            .class_mangled_map
            .get(&hir_class.name)
            .unwrap_or_else(|| panic!("Missing mangled method map for class {}", hir_class.name));

        for iface_name in interfaces {
            let iface_methods = self.interface_methods.get(iface_name).unwrap_or_else(|| {
                panic!(
                    "Class {} implements unknown interface {}",
                    hir_class.name, iface_name
                )
            });

            let mut vtable_slots = Vec::new();
            let mut slot_map = HashMap::with_hasher(FxHashBuilder);

            for (slot, (method_name, _, _)) in iface_methods.iter().enumerate() {
                let mangled = class_methods
                    .get(method_name)
                    .unwrap_or_else(|| {
                        panic!(
                            "Class {} implements interface {} but does not provide method {}",
                            hir_class.name, iface_name, method_name
                        )
                    })
                    .clone();

                vtable_slots.push(mangled);
                slot_map.insert(method_name.clone(), slot);
            }

            self.class_vtable_slots.insert(
                StrId(self.context.intern(&format!(
                    "{}_{}",
                    self.context.resolve_string(&hir_class.name),
                    self.context.resolve_string(iface_name)
                ))),
                vtable_slots,
            );

            self.class_method_slots.insert(hir_class.name, slot_map);
        }
    }

    fn lower_function(&mut self, hir_fn: &HirFunc<'a, 'bump>) {
        let func = self.lower_function_inner(hir_fn);
        self.module.functions.insert(func.name.clone(), func);
    }

    fn lower_function_inner<'s>(&'s self, hir_fn: &HirFunc<'a, 'bump>) -> Function
    where
        's: 'cx,
    {
        let mut fl: FunctionLowerer<'s, 'bump> = FunctionLowerer::new(
            hir_fn,
            &self.module.functions,
            &self.class_field_offsets,
            &self.class_method_slots,
            &self.class_mangled_map,
            &self.class_vtable_slots,
            &self.interface_id_map,
            &self.interface_method_slots,
            &self.module.classes,
            self.context.clone(),
            self.extern_c_names,
            self.dep_graph,
            self.module_idx,
        )
        .unwrap();
        fl.lower_body(hir_fn.body);
        FunctionLowerer::finish(fl)
    }

    fn lower_class_method(&mut self, hir_method: &HirFunc<'a, 'bump>, class_name: StrId) {
        // For class methods, lower with class context
        let func = self.lower_class_method_inner(hir_method, class_name);
        self.module.functions.insert(func.name, func);
    }

    fn lower_class_method_inner<'s>(
        &'s self,
        hir_fn: &HirFunc<'a, 'bump>,
        class_name: StrId,
    ) -> Function
    where
        's: 'cx,
    {
        let mut fl: FunctionLowerer<'s, 'bump> = FunctionLowerer::new_with_class(
            hir_fn,
            class_name,
            &self.module.functions,
            &self.class_field_offsets,
            &self.class_method_slots,
            &self.class_mangled_map,
            &self.class_vtable_slots,
            &self.interface_id_map,
            &self.interface_method_slots,
            &self.module.classes,
            Arc::clone(&self.context),
            self.extern_c_names,
            self.dep_graph,
            self.module_idx,
        )
        .unwrap();
        fl.lower_body(hir_fn.body);
        FunctionLowerer::finish(fl)
    }
}
