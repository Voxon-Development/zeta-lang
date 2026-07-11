use ir::hir::{HirStruct, HirType, StrId};
use ir::ir_conversion::lower_type_hir;
use ir::ir_hasher::{FxHashBuilder, FxHashMap};
use ir::registry::global_registry::GlobalRegistry;
use ir::ssa_ir::{BasicBlock, BlockId, Function, Instruction, Operand, SsaType, Value};
use smallvec::SmallVec;
use std::collections::HashMap;
use std::sync::Arc;
use zetaruntime::string_pool::StringPool;

use crate::midend::ir::block_data::CurrentBlockData;

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
        let struct_names: Vec<StrId> = registry.classes.borrow().keys().copied().collect();

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
                    let classes = registry.classes.borrow();
                    match classes.get(&name) {
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
            HirType::Struct(name, _) => is_droppable.get(name).copied().unwrap_or(false),
            HirType::Nullable(inner) => Self::type_is_droppable(inner, is_droppable),
            HirType::OwnedPointer(_) => true,
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

/// Builds SSA `Function` bodies for every droppable struct *owned* by one
/// module.
pub struct DropGlueBuilder;

impl DropGlueBuilder {
    pub fn build_all<'a, 'bump>(
        glue_registry: &DropGlueRegistry,
        classes: &HashMap<StrId, HirStruct<'a, 'bump>, FxHashBuilder>,
        class_mangled_map: &HashMap<StrId, HashMap<StrId, StrId, FxHashBuilder>, FxHashBuilder>,
        context: Arc<StringPool>,
        struct_names_owned_by_this_module: &[StrId],
    ) -> Vec<(StrId, Function)> {
        struct_names_owned_by_this_module
            .iter()
            .filter_map(|&name| {
                Self::build_one(
                    glue_registry,
                    classes,
                    class_mangled_map,
                    context.clone(),
                    name,
                )
            })
            .collect()
    }

    fn build_one<'a, 'bump>(
        glue_registry: &DropGlueRegistry,
        classes: &HashMap<StrId, HirStruct<'a, 'bump>, FxHashBuilder>,
        class_mangled_map: &HashMap<StrId, HashMap<StrId, StrId, FxHashBuilder>, FxHashBuilder>,
        context: Arc<StringPool>,
        struct_name: StrId,
    ) -> Option<(StrId, Function)> {
        let glue_name = glue_registry.glue_name_for(struct_name)?;

        let hir_struct = classes.get(&struct_name).unwrap_or_else(|| {
            panic!(
                "droppable struct {} missing from module.classes",
                struct_name
            )
        });

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

        let mut cbd = CurrentBlockData::new(func, entry_bb, 1usize, 1usize, value_types);

        let drop_method_name = StrId(context.intern("drop"));
        if let Some(mangled_drop) = class_mangled_map
            .get(&struct_name)
            .and_then(|m| m.get(&drop_method_name))
        {
            cbd.bb().instructions.push(Instruction::Call {
                dest: None,
                func: Operand::FunctionRef(*mangled_drop),
                args: SmallVec::from_slice_copy(&[this_operand.clone()]),
            });
        }

        let mut droppable_fields: Vec<(usize, SsaType, StrId)> = Vec::new();

        for (field_index, field) in hir_struct.fields.iter().enumerate() {
            let HirType::Struct(field_struct_name, _) = &field.field_type else {
                continue;
            };
            let Some(field_glue) = glue_registry.glue_name_for(*field_struct_name) else {
                continue;
            };

            let field_ssa_ty = lower_type_hir(&field.field_type);
            droppable_fields.push((field_index, field_ssa_ty, field_glue));
        }

        for (field_index, field_ssa_ty, field_glue) in droppable_fields.into_iter().rev() {
            let field_ptr_val = cbd.fresh_value();
            cbd.value_types
                .insert(field_ptr_val, SsaType::Pointer(Box::new(field_ssa_ty)));

            cbd.bb().instructions.push(Instruction::FieldAddr {
                dest: field_ptr_val,
                base: this_operand.clone(),
                offset: field_index,
            });
            cbd.bb().instructions.push(Instruction::Call {
                dest: None,
                func: Operand::FunctionRef(field_glue),
                args: SmallVec::from_slice_copy(&[Operand::Value(field_ptr_val)]),
            });
        }

        cbd.bb().instructions.push(Instruction::Ret { value: None });

        Some((glue_name, cbd.finish()))
    }
}
