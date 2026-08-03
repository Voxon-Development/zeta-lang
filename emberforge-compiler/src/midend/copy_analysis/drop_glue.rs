use crate::midend::copy_analysis::drop_emitter::{AllocatorResolver, DropEmitter};
use crate::midend::ir::block_data::CurrentBlockData;
use ir::hir::{self, DropKind, HirStruct, HirType, ProvenanceAnnotation, StrId};
use ir::ir_conversion::lower_type_hir;
use ir::ir_hasher::{FxHashBuilder, FxHashMap};
use ir::registry::global_registry::GlobalRegistry;
use ir::ssa_ir::{
    AllocatorKind, BasicBlock, BlockId, Function, Instruction, Operand, SsaType, Value,
};
use smallvec::SmallVec;
use std::collections::HashMap;
use std::sync::Arc;
use zetaruntime::string_pool::StringPool;

struct GlueAllocatorResolver;

impl AllocatorResolver for GlueAllocatorResolver {
    fn resolve_root(&mut self, root: &hir::ProvenanceRoot) -> Value {
        match root {
            hir::ProvenanceRoot::ThisRoot => Value(0),
            hir::ProvenanceRoot::Var(name) => {
                panic!("[resolve_allocator_value] allocator root {name} is not reachable in glue")
            }
            hir::ProvenanceRoot::Global { .. } => {
                unimplemented!("global-rooted allocator provenance in drop glue")
            }
            hir::ProvenanceRoot::ImplicitParam(_) => {
                unreachable!("resolved by monomorphization before MIR")
            }
        }
    }
}

/// Whole-program registry of which structs need drop glue and what each
/// glue function is called
pub struct DropGlueRegistry {
    is_droppable: FxHashMap<StrId, bool>,
    glue_names: FxHashMap<StrId, StrId>,
    has_own_drop: FxHashMap<StrId, bool>,
}

impl DropGlueRegistry {
    pub fn new<'a, 'bump>(registry: &GlobalRegistry<'a, 'bump>, context: Arc<StringPool>) -> Self {
        let drop_iface = StrId(context.intern("Drop"));
        let struct_names: Vec<StrId> = registry.structs.borrow().keys().copied().collect();

        let mut is_droppable: FxHashMap<StrId, bool> = FxHashMap::default();
        for &name in &struct_names {
            is_droppable.insert(name, false);
        }

        let implements_drop = |name: StrId| -> bool {
            registry
                .struct_interfaces
                .borrow()
                .get(&name)
                .map(|ifaces| ifaces.contains(&drop_iface))
                .unwrap_or(false)
        };

        let mut has_own_drop: FxHashMap<StrId, bool> = FxHashMap::default();
        for &name in &struct_names {
            has_own_drop.insert(name, implements_drop(name));
        }

        let mut changed = true;
        while changed {
            changed = false;
            for &name in &struct_names {
                let computed = if implements_drop(name) {
                    true
                } else {
                    let structs = registry.structs.borrow();
                    match structs.get(&name) {
                        Some(hir_struct) => hir_struct
                            .fields
                            .iter()
                            .any(|f| Self::type_is_droppable(&f.field_type, &is_droppable)),
                        None => false,
                    }
                };
                if is_droppable.get(&name).copied() != Some(computed) {
                    is_droppable.insert(name, computed);
                    changed = true;
                }
            }
        }

        let mut glue_names: FxHashMap<StrId, StrId> = FxHashMap::default();
        for &name in &struct_names {
            if is_droppable.get(&name).copied().unwrap_or(false) {
                let mangled_name = context.resolve_string(&name);
                let glue_name = StrId(context.intern(&format!("{}_drop_glue", mangled_name)));
                glue_names.insert(name, glue_name);
            }
        }

        Self {
            is_droppable,
            glue_names,
            has_own_drop,
        }
    }

    pub fn has_own_drop(&self, struct_name: StrId) -> bool {
        self.has_own_drop
            .get(&struct_name)
            .copied()
            .unwrap_or(false)
    }

    fn type_is_droppable<'a, 'bump>(
        ty: &HirType<'a, 'bump>,
        is_droppable: &FxHashMap<StrId, bool>,
    ) -> bool {
        match ty {
            HirType::Struct { name, .. } => is_droppable.get(name).copied().unwrap_or(false),
            HirType::Nullable(inner) => Self::type_is_droppable(inner, is_droppable),
            // Any owned pointer always needs some glue action (at minimum
            // a free), regardless of whether its pointee is itself
            // droppable, so it always forces the owner to be droppable too.
            HirType::OwnedPointer { .. } => true,
            _ => false,
        }
    }

    pub fn glue_name_for(&self, struct_name: StrId) -> Option<StrId> {
        self.glue_names.get(&struct_name).copied()
    }

    pub fn is_droppable(&self, struct_name: StrId) -> bool {
        self.is_droppable
            .get(&struct_name)
            .copied()
            .unwrap_or(false)
    }
}

enum FieldDrop<'a, 'bump> {
    Type {
        offset: usize,
        glue: StrId,
    },
    OwnedPointer {
        offset: usize,
        pointee: DropKind<'a, 'bump>,
        pointee_ty: HirType<'a, 'bump>,
        allocator: ProvenanceAnnotation<'bump>,
    },
}

/// Builds SSA `Function` bodies for every droppable struct *owned* by one
/// module.
pub struct DropGlueBuilder;

impl DropGlueBuilder {
    pub fn build_all<'a, 'bump>(
        glue_registry: &DropGlueRegistry,
        structs: &HashMap<StrId, HirStruct<'a, 'bump>, FxHashBuilder>,
        struct_mangled_map: &HashMap<StrId, HashMap<StrId, StrId, FxHashBuilder>, FxHashBuilder>,
        struct_field_offsets: &HashMap<StrId, HashMap<StrId, usize, FxHashBuilder>, FxHashBuilder>,
        allocator_kind: &HashMap<StrId, AllocatorKind, FxHashBuilder>,
        context: Arc<StringPool>,
        struct_names_owned_by_this_module: &[StrId],
    ) -> Vec<(StrId, Function)> {
        struct_names_owned_by_this_module
            .iter()
            .filter_map(|&name| {
                Self::build_one(
                    glue_registry,
                    structs,
                    struct_mangled_map,
                    struct_field_offsets,
                    allocator_kind,
                    context.clone(),
                    name,
                )
            })
            .collect()
    }

    fn build_one<'a, 'bump>(
        glue_registry: &DropGlueRegistry,
        structs: &HashMap<StrId, HirStruct<'a, 'bump>, FxHashBuilder>,
        struct_mangled_map: &HashMap<StrId, HashMap<StrId, StrId, FxHashBuilder>, FxHashBuilder>,
        struct_field_offsets: &HashMap<StrId, HashMap<StrId, usize, FxHashBuilder>, FxHashBuilder>,
        allocator_kind: &HashMap<StrId, AllocatorKind, FxHashBuilder>,
        context: Arc<StringPool>,
        struct_name: StrId,
    ) -> Option<(StrId, Function)> {
        let glue_name = glue_registry.glue_name_for(struct_name)?;

        let hir_struct = structs.get(&struct_name).unwrap_or_else(|| {
            panic!(
                "droppable struct {} missing from module.structs",
                struct_name
            )
        });
        let offsets = struct_field_offsets.get(&struct_name);

        let this_ty = SsaType::Pointer(Box::new(SsaType::User(struct_name, vec![])));
        let this_val = Value(0);
        let this_operand = Operand::Value(this_val);

        let mut func = Function {
            name: glue_name,
            params: SmallVec::new(),
            ret_type: SsaType::Void,
            blocks: SmallVec::new(),
            value_types: HashMap::with_hasher(FxHashBuilder),
            entry: BlockId(0),
            function_metadata: Default::default(),
        };
        func.params.push((this_val, this_ty.clone()));

        let mut value_types = HashMap::with_hasher(FxHashBuilder);
        value_types.insert(this_val, this_ty);

        let entry_bb = BlockId(0);
        func.entry = entry_bb;
        func.blocks.push(BasicBlock {
            id: entry_bb,
            instructions: Vec::new(),
        });

        let mut cbd = CurrentBlockData::new(&mut func, entry_bb, 1usize, 1usize, value_types);

        let drop_method_name = StrId(context.intern("drop"));
        if let Some(mangled_drop) = struct_mangled_map
            .get(&struct_name)
            .and_then(|m| m.get(&drop_method_name))
        {
            cbd.bb().instructions.push(Instruction::Call {
                dest: None,
                func: Operand::FunctionRef(*mangled_drop),
                args: SmallVec::from_slice_copy(&[this_operand.clone()]),
            });
        }

        let mut field_drops: Vec<FieldDrop<'a, 'bump>> = Vec::new();
        for field in hir_struct.fields.iter() {
            let offset = offsets
                .and_then(|m| m.get(&field.name))
                .copied()
                .unwrap_or_else(|| {
                    panic!(
                        "build_one: missing field offset for {}.{}",
                        struct_name, field.name
                    )
                });

            match &field.field_type {
                HirType::Struct {
                    name: field_struct_name,
                    ..
                } => {
                    if let Some(field_glue) = glue_registry.glue_name_for(*field_struct_name) {
                        field_drops.push(FieldDrop::Type {
                            offset,
                            glue: field_glue,
                        });
                    }
                }
                HirType::OwnedPointer { inner, allocator } => {
                    let allocator = allocator.unwrap_or_else(|| {
                        panic!(
                            "build_one: owned-pointer field {}.{} has no allocator",
                            struct_name, field.name
                        )
                    });

                    field_drops.push(FieldDrop::OwnedPointer {
                        offset,
                        pointee: inner.drop_kind(),
                        pointee_ty: **inner,
                        allocator,
                    });
                }
                _ => {}
            }
        }

        let mut emitter = DropEmitter::new(
            &mut cbd,
            context.clone(),
            struct_mangled_map,
            struct_field_offsets,
            structs,
            allocator_kind,
            glue_registry,
        );
        let mut resolver = GlueAllocatorResolver;

        for drop in field_drops.into_iter().rev() {
            match drop {
                FieldDrop::Type { offset, glue } => {
                    let field_ptr = emitter.current_block_data.fresh_value();
                    emitter
                        .current_block_data
                        .value_types
                        .insert(field_ptr, SsaType::Pointer(Box::new(SsaType::Void)));
                    emitter.emit(Instruction::FieldAddr {
                        dest: field_ptr,
                        base: this_operand.clone(),
                        offset,
                    });
                    emitter.emit(Instruction::Call {
                        dest: None,
                        func: Operand::FunctionRef(glue),
                        args: SmallVec::from_slice_copy(&[Operand::Value(field_ptr)]),
                    });
                }
                FieldDrop::OwnedPointer {
                    offset,
                    pointee,
                    pointee_ty,
                    allocator,
                } => {
                    let field_ssa_ty = lower_type_hir(&pointee_ty);
                    let field_addr = emitter.current_block_data.fresh_value();
                    emitter.current_block_data.value_types.insert(
                        field_addr,
                        SsaType::Pointer(Box::new(SsaType::Owned(Box::new(field_ssa_ty.clone())))),
                    );
                    emitter.emit(Instruction::FieldAddr {
                        dest: field_addr,
                        base: this_operand.clone(),
                        offset,
                    });

                    let ptr_val = if matches!(pointee_ty, HirType::Slice(_)) {
                        field_addr
                    } else {
                        let loaded = emitter.current_block_data.fresh_value();
                        emitter.emit(Instruction::Load {
                            dest: loaded,
                            ptr: Operand::Value(field_addr),
                        });
                        emitter
                            .current_block_data
                            .value_types
                            .insert(loaded, field_ssa_ty);
                        loaded
                    };

                    emitter.emit_owned_pointer_drop(
                        None,
                        &pointee,
                        &pointee_ty,
                        &allocator,
                        ptr_val,
                        false,
                        None,
                        &mut resolver,
                    );
                }
            }
        }

        cbd.bb().instructions.push(Instruction::Ret { value: None });
        cbd.finish();

        Some((glue_name, func))
    }
}
