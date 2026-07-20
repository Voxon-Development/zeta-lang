use crate::midend::copy_analysis::drop_glue::{DropGlueBuilder, DropGlueRegistry};
use crate::midend::ir::lowerer::FunctionLowerer;
use codex_dependency_graph::DepGraph;
use ir::hir::{Hir, HirFunc, HirInterface, HirModule, HirParam, HirStruct, StrId, ThisPassingKind};
use ir::ir_conversion::lower_type_hir;
use ir::ir_hasher::{FxHashBuilder, HashSet};
use ir::ssa_ir::{Function, Module, SsaType};
use std::cell::RefCell;
use std::collections::HashMap;
use std::marker::PhantomData;
use std::rc::Rc;
use std::sync::Arc;
use zetaruntime::string_pool::StringPool;

pub struct MirModuleLowerer<'a, 'cx, 'bump, 'g>
where
    'bump: 'a,
    'bump: 'cx,
    'cx: 'a,
{
    pub module: Module<'a, 'bump>,

    interface_methods: HashMap<StrId, Vec<(StrId, Vec<SsaType>, SsaType)>, FxHashBuilder>,
    interface_id_map: HashMap<StrId, usize, FxHashBuilder>,
    interface_method_slots: HashMap<StrId, HashMap<StrId, usize, FxHashBuilder>, FxHashBuilder>,

    class_mangled_map: HashMap<StrId, HashMap<StrId, StrId, FxHashBuilder>, FxHashBuilder>,
    class_vtable_slots: HashMap<StrId, Vec<StrId>, FxHashBuilder>,
    class_method_slots: HashMap<StrId, HashMap<StrId, usize, FxHashBuilder>, FxHashBuilder>,
    class_field_offsets: HashMap<StrId, HashMap<StrId, usize, FxHashBuilder>, FxHashBuilder>,

    phantom_data: PhantomData<&'cx ()>,

    context: Arc<StringPool>,
    extern_c_names: Rc<HashSet<StrId>>,

    enum_variant_tags: HashMap<StrId, HashMap<StrId, usize, FxHashBuilder>, FxHashBuilder>,
    struct_interfaces: HashMap<StrId, Vec<StrId>, FxHashBuilder>,
    pub dep_graph: &'a RefCell<DepGraph>,
    pub module_idx: usize,
    g_phantom_data: PhantomData<&'g ()>,
    glue_registry: &'a DropGlueRegistry,
}

impl<'a, 'cx, 'bump, 'g> MirModuleLowerer<'a, 'cx, 'bump, 'g>
where
    'bump: 'a,
    'bump: 'cx,
    'cx: 'a,
{
    pub fn new(
        context: Arc<StringPool>,
        extern_c_names: Rc<HashSet<StrId>>,
        dep_graph: &'a RefCell<DepGraph>,
        module_idx: usize,
        glue_registry: &'a DropGlueRegistry,
    ) -> Self {
        let mut enum_variant_tags: HashMap<
            StrId,
            HashMap<StrId, usize, FxHashBuilder>,
            FxHashBuilder,
        > = HashMap::with_hasher(FxHashBuilder);

        let nullable_enum_name = StrId(context.intern("__nullable"));
        let mut nullable_tags = HashMap::with_hasher(FxHashBuilder);
        nullable_tags.insert(StrId(context.intern("null")), 0usize);
        nullable_tags.insert(StrId(context.intern("some")), 1usize);
        enum_variant_tags.insert(nullable_enum_name, nullable_tags);

        // Tag 0 reserved for "success" (no throw); every distinct thrown error
        // type gets a stable index starting at 1, shared across the whole
        // module so catch-site and call-site tags always agree.
        let throws_enum_name = StrId(context.intern("__throws"));
        let mut throws_tags = HashMap::with_hasher(FxHashBuilder);
        throws_tags.insert(StrId(context.intern("__success")), 0usize);
        enum_variant_tags.insert(throws_enum_name, throws_tags);

        Self {
            module: Module::new(),
            interface_methods: HashMap::with_hasher(FxHashBuilder),
            interface_id_map: HashMap::with_hasher(FxHashBuilder),
            interface_method_slots: HashMap::with_hasher(FxHashBuilder),
            class_mangled_map: HashMap::with_hasher(FxHashBuilder),
            class_vtable_slots: HashMap::with_hasher(FxHashBuilder),
            class_method_slots: HashMap::with_hasher(FxHashBuilder),
            class_field_offsets: HashMap::with_hasher(FxHashBuilder),
            enum_variant_tags,
            struct_interfaces: HashMap::with_hasher(FxHashBuilder),
            phantom_data: PhantomData,
            context,
            extern_c_names,

            dep_graph,
            module_idx,
            g_phantom_data: PhantomData,
            glue_registry,
        }
    }

    fn register_enum(&mut self, hir_enum: &ir::hir::HirEnum<'a, 'bump>) {
        let mut tags = HashMap::with_hasher(FxHashBuilder);
        for (i, variant) in hir_enum.variants.iter().enumerate() {
            tags.insert(variant.name, i);
        }
        self.enum_variant_tags.insert(hir_enum.name, tags);
    }

    pub fn lower_all_modules(
        mut self,
        hir_modules: &[HirModule<'a, 'bump>],
        compilation_order: &[usize],
    ) -> Module<'a, 'bump> {
        for &idx in compilation_order {
            for item in hir_modules[idx].items {
                match item {
                    Hir::Enum(hir_enum) => self.register_enum(*hir_enum),
                    Hir::Interface(iface) => self.lower_interface(*iface),
                    Hir::Impl(impl_block) => {
                        if let Some(iface) = impl_block.interface {
                            self.struct_interfaces
                                .entry(impl_block.target)
                                .or_insert_with(Vec::new)
                                .push(iface);
                        }
                    }
                    _ => {}
                }
            }
        }

        for &idx in compilation_order {
            for item in hir_modules[idx].items {
                if let Hir::Struct(class) = item {
                    self.register_class(*class);
                }
            }
        }

        for &idx in compilation_order {
            for item in hir_modules[idx].items {
                match item {
                    Hir::Func(func) => match func.impl_target {
                        Some(class_name) => {
                            self.class_mangled_map
                                .entry(class_name)
                                .or_insert_with(|| HashMap::with_hasher(FxHashBuilder))
                                .insert(func.unmangled_name, func.name);
                            self.register_method_signature(func, class_name);
                        }
                        None => {
                            let f = Function::from_signature(func);
                            self.module.functions.insert(f.name, f);
                        }
                    },
                    Hir::Impl(impl_block) => {
                        if let Some(methods) = impl_block.methods {
                            for method in methods {
                                self.class_mangled_map
                                    .entry(impl_block.target)
                                    .or_insert_with(|| HashMap::with_hasher(FxHashBuilder))
                                    .insert(method.unmangled_name, method.name);
                                self.register_method_signature(method, impl_block.target);
                            }
                        }
                    }
                    _ => {}
                }
            }
        }

        for &idx in compilation_order {
            self.module_idx = idx;
            for item in hir_modules[idx].items {
                match item {
                    Hir::Func(func) => self.lower_function_body(func),
                    Hir::Impl(impl_block) => {
                        if let Some(methods) = impl_block.methods {
                            for method in methods {
                                self.lower_function_body(method);
                            }
                        }
                    }
                    _ => {}
                }
            }
        }

        for &idx in compilation_order {
            for item in hir_modules[idx].items {
                if let Hir::Struct(ty_struct) = item {
                    self.build_class_vtable(ty_struct);
                }
            }
        }

        let owned_structs: Vec<StrId> = compilation_order
            .iter()
            .flat_map(|&idx| {
                hir_modules[idx].items.iter().filter_map(|item| match item {
                    Hir::Struct(s) => Some(s.name),
                    _ => None,
                })
            })
            .collect();
        for (name, func) in DropGlueBuilder::build_all(
            self.glue_registry,
            &self.module.classes,
            &self.class_mangled_map,
            self.context.clone(),
            &owned_structs,
        ) {
            self.module.functions.insert(name, func);
        }

        self.module
    }

    fn register_method_signature(&mut self, hir_method: &HirFunc<'a, 'bump>, class_name: StrId) {
        if !self.class_field_offsets.contains_key(&class_name) {
            eprintln!(
                "[register_method_signature] WARNING: method {} has impl_target={} but class_field_offsets has no entry. known classes: {:?}",
                hir_method.name,
                class_name,
                self.class_field_offsets.keys().collect::<Vec<_>>()
            );
        }

        let func = Function::from_signature(hir_method);

        self.module.functions.insert(func.name, func);
    }

    fn register_class(&mut self, hir_class: &HirStruct<'a, 'bump>) {
        self.compute_field_offsets(hir_class);

        self.class_mangled_map
            .entry(hir_class.name)
            .or_insert_with(|| HashMap::with_hasher(FxHashBuilder));

        self.module
            .classes
            .insert(hir_class.name, hir_class.clone());
    }

    fn lower_interface(&mut self, hir_iface: &HirInterface<'a, 'bump>) {
        let iface_id = self.interface_id_map.len();
        self.interface_id_map.insert(hir_iface.name, iface_id);
        let mut methods = Vec::new();
        let mut slot_map = HashMap::with_hasher(FxHashBuilder);
        if let Some(hir_methods) = hir_iface.methods {
            for (i, m) in hir_methods.iter().enumerate() {
                let param_types: Vec<SsaType> = m
                    .params
                    .unwrap_or_default()
                    .iter()
                    .map(|p| match p {
                        HirParam::Normal {
                            name: _,
                            param_type,
                            span: _,
                        } => lower_type_hir(&param_type),
                        HirParam::This { kind, span: _ } => match kind {
                            ThisPassingKind::Move | ThisPassingKind::MoveMut => SsaType::Dyn,
                            _ => SsaType::Pointer(Box::new(SsaType::Dyn)),
                        },
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
        }

        self.interface_methods
            .insert(hir_iface.name.clone(), methods);
        self.interface_method_slots.insert(hir_iface.name, slot_map);
        self.module
            .interfaces
            .insert(hir_iface.name, hir_iface.clone());
    }

    fn compute_field_offsets(&mut self, hir_class: &HirStruct<'a, 'bump>) {
        let mut offsets = HashMap::with_hasher(FxHashBuilder);
        let mut current_offset = 0usize;

        for f in hir_class.fields.iter() {
            let field_ssa_ty = lower_type_hir(&f.field_type);
            let align =
                ir::layout::alignof_ssa(&field_ssa_ty, ir::layout::TargetInfo { ptr_bytes: 8 })
                    .unwrap_or_else(|e| {
                        panic!("failed to compute alignment for field {}: {:?}", f.name, e)
                    });

            current_offset = ir::layout::round_up_to_align(current_offset, align);
            offsets.insert(f.name, current_offset);

            let size =
                ir::layout::sizeof_ssa(&field_ssa_ty, ir::layout::TargetInfo { ptr_bytes: 8 })
                    .ok()
                    .unwrap_or_else(|| panic!("failed to compute size for field {}", f.name));
            current_offset += size;
        }

        self.class_field_offsets.insert(hir_class.name, offsets);
    }

    fn build_class_vtable(&mut self, hir_class: &HirStruct<'a, 'bump>) {
        let Some(interfaces) = self.struct_interfaces.get(&hir_class.name).cloned() else {
            return;
        };

        let class_methods = self
            .class_mangled_map
            .get(&hir_class.name)
            .unwrap_or_else(|| panic!("Missing mangled method map for class {}", hir_class.name));

        for iface_name in &interfaces {
            let Some(iface_methods) = self.interface_methods.get(iface_name) else {
                continue;
            };

            let hir_iface = self.module.interfaces.get(iface_name).unwrap_or_else(|| {
                panic!(
                    "Class {} implements unknown interface {}",
                    hir_class.name, iface_name
                )
            });
            let unmangled_names: Vec<StrId> = hir_iface
                .methods
                .unwrap_or(&[])
                .iter()
                .map(|m| m.unmangled_name)
                .collect();

            let mut vtable_slots = Vec::new();
            let mut slot_map = HashMap::with_hasher(FxHashBuilder);

            for (slot, (method_name, _, _)) in iface_methods.iter().enumerate() {
                let unmangled_name = unmangled_names.get(slot).copied().unwrap_or(*method_name);

                let mangled = class_methods
                    .get(&unmangled_name)
                    .unwrap_or_else(|| {
                        panic!(
                            "Class {} implements interface {} but does not provide method {}",
                            hir_class.name, iface_name, unmangled_name
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

    fn lower_function_body(&mut self, hir_fn: &HirFunc<'a, 'bump>) {
        let mut function = self
            .module
            .functions
            .remove(&hir_fn.name)
            .expect("function signature should already be registered");

        let mut fl = FunctionLowerer::new(
            &mut function,
            hir_fn,
            &self.module.functions,
            &self.module.functions,
            &self.class_field_offsets,
            &self.class_method_slots,
            &self.class_mangled_map,
            &self.class_vtable_slots,
            &self.interface_id_map,
            &self.interface_method_slots,
            &self.module.classes,
            &self.enum_variant_tags,
            self.context.clone(),
            &self.extern_c_names,
            self.dep_graph,
            self.module_idx,
            self.glue_registry,
            &self.interface_methods,
        )
        .unwrap();
        fl.lower_body(hir_fn.body);
        fl.finish();

        self.module.functions.insert(hir_fn.name, function);
    }
}
