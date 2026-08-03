use crate::midend::copy_analysis::drop_glue::DropGlueRegistry;
use crate::midend::copy_analysis::drop_tracking::DropMoveState;
use crate::midend::ir::block_data::CurrentBlockData;
use codex_dependency_graph::DepGraph;
use ir::hir::{self, DropKind, HirStruct, HirType, ProvenanceAnnotation, StrId};
use ir::hir_utils::type_suffix_with_pool;
use ir::ir_conversion::lower_type_hir;
use ir::ir_hasher::FxHashBuilder;
use ir::layout::{Layout, TargetInfo, layout_of_ssa, sizeof_ssa};
use ir::ssa_ir::{
    AllocatorKind, BinOp, BlockId, Instruction, IntrinsicOp, Operand, SsaType, Value,
};
use smallvec::SmallVec;
use std::cell::RefCell;
use std::collections::HashMap;
use std::sync::Arc;
use zetaruntime::string_pool::StringPool;

pub trait AllocatorResolver {
    fn resolve_root(&mut self, root: &hir::ProvenanceRoot) -> Value;
    fn lower_global_ref(&mut self, _module_idx: usize, _name: StrId) -> StrId {
        panic!("lower_global_ref not supported by this allocator resolver");
    }
}

pub struct FnAllocatorResolver<'r> {
    pub var_map: &'r HashMap<StrId, Value, FxHashBuilder>,
    pub context: Arc<StringPool>,
    pub dep_graph: &'r RefCell<DepGraph>,
}

impl<'r> AllocatorResolver for FnAllocatorResolver<'r> {
    fn resolve_root(&mut self, root: &hir::ProvenanceRoot) -> Value {
        match root {
            hir::ProvenanceRoot::Var(name) => *self.var_map.get(name).unwrap_or_else(|| {
                panic!(
                    "resolve_allocator_value: allocator root `{:?}` not bound",
                    name
                )
            }),
            hir::ProvenanceRoot::ThisRoot => {
                let this_name = StrId(self.context.intern("this"));
                *self.var_map.get(&this_name).unwrap_or_else(|| {
                    panic!(
                        "resolve_allocator_value: `this` not bound but allocator root is ThisRoot"
                    )
                })
            }
            hir::ProvenanceRoot::Global { .. } => {
                unreachable!(
                    "Global root is handled directly by DropEmitter::resolve_allocator_value"
                )
            }
            hir::ProvenanceRoot::ImplicitParam(_) => {
                unreachable!("resolved by monomorphization before MIR")
            }
        }
    }

    fn lower_global_ref(&mut self, module_idx: usize, name: StrId) -> StrId {
        let pkg = self.dep_graph.borrow().get_module_package(module_idx);
        let segments: Vec<StrId> = match pkg {
            Some(pkg) => {
                let pkg_str = self.context.resolve_string(&pkg);
                pkg_str
                    .split("::")
                    .map(|s| StrId(self.context.intern(s)))
                    .collect()
            }
            None => Vec::new(),
        };
        crate::optimized_string_buffering::build_module_scoped_name(
            &segments,
            name,
            None,
            self.context.clone(),
        )
    }
}

pub struct DropEmitter<'x, 'a, 'bump, 'f> {
    pub current_block_data: &'x mut CurrentBlockData<'f>,
    pub context: Arc<StringPool>,
    pub struct_mangled_map: &'x HashMap<StrId, HashMap<StrId, StrId, FxHashBuilder>, FxHashBuilder>,
    pub struct_field_offsets:
        &'x HashMap<StrId, HashMap<StrId, usize, FxHashBuilder>, FxHashBuilder>,
    pub structs: &'x HashMap<StrId, HirStruct<'a, 'bump>, FxHashBuilder>,
    pub allocator_kind: &'x HashMap<StrId, AllocatorKind, FxHashBuilder>,
    pub glue_registry: &'x DropGlueRegistry,
}

impl<'x, 'a, 'bump, 'f> DropEmitter<'x, 'a, 'bump, 'f> {
    pub fn new(
        current_block_data: &'x mut CurrentBlockData<'f>,
        context: Arc<StringPool>,
        struct_mangled_map: &'x HashMap<StrId, HashMap<StrId, StrId, FxHashBuilder>, FxHashBuilder>,
        struct_field_offsets: &'x HashMap<
            StrId,
            HashMap<StrId, usize, FxHashBuilder>,
            FxHashBuilder,
        >,
        structs: &'x HashMap<StrId, HirStruct<'a, 'bump>, FxHashBuilder>,
        allocator_kind: &'x HashMap<StrId, AllocatorKind, FxHashBuilder>,
        glue_registry: &'x DropGlueRegistry,
    ) -> Self {
        Self {
            current_block_data,
            context,
            struct_mangled_map,
            struct_field_offsets,
            structs,
            allocator_kind,
            glue_registry,
        }
    }

    pub fn emit(&mut self, instruction: Instruction) {
        self.current_block_data.bb().instructions.push(instruction);
    }

    pub fn resolve_allocator_value<R: AllocatorResolver>(
        &mut self,
        allocator: &ProvenanceAnnotation<'bump>,
        resolver: &mut R,
    ) -> Value {
        let root_val = match &allocator.root {
            hir::ProvenanceRoot::Global { module_idx, name } => {
                let mangled = resolver.lower_global_ref(*module_idx, *name);
                let dest = self.current_block_data.fresh_value();
                self.emit(Instruction::Const {
                    dest,
                    ty: SsaType::I64,
                    value: Operand::GlobalRef(mangled),
                });
                self.current_block_data
                    .value_types
                    .insert(dest, SsaType::I64);
                dest
            }
            root => resolver.resolve_root(root),
        };
        self.apply_provenance_path(root_val, allocator.path)
    }

    pub fn apply_provenance_path(
        &mut self,
        mut base: Value,
        path: &[hir::ProvenancePathSegment],
    ) -> Value {
        for seg in path {
            match seg {
                hir::ProvenancePathSegment::Field(field) => {
                    let (addr, _field_ty) = self.field_addr_on_value(base, *field);
                    base = addr;
                }
                hir::ProvenancePathSegment::Deref => {
                    let dest = self.current_block_data.fresh_value();
                    self.emit(Instruction::Load {
                        dest,
                        ptr: Operand::Value(base),
                    });
                    let pointee_ty = match self.current_block_data.value_types.get(&base) {
                        Some(SsaType::Pointer(inner)) => (**inner).clone(),
                        other => panic!("[apply_provenance_path] Deref of non-pointer {:?}", other),
                    };
                    self.current_block_data.value_types.insert(dest, pointee_ty);
                    base = dest;
                }
            }
        }
        base
    }

    pub fn field_addr_on_value(&mut self, obj_val: Value, field: StrId) -> (Value, SsaType) {
        let cls_name = match self.current_block_data.value_types.get(&obj_val) {
            Some(SsaType::User(name, _)) => *name,
            Some(SsaType::Pointer(inner)) => match inner.as_ref() {
                SsaType::User(name, _) => *name,
                other => panic!(
                    "[field_addr_on_value] pointer to non-User type: {:?}",
                    other
                ),
            },
            other => panic!(
                "[field_addr_on_value] could not determine struct type: {:?}",
                other
            ),
        };

        let offsets = self
            .struct_field_offsets
            .get(&cls_name)
            .unwrap_or_else(|| panic!("Unknown struct {} in provenance path", cls_name));
        let offset = *offsets
            .get(&field)
            .unwrap_or_else(|| panic!("Unknown field {} on struct {}", field, cls_name));

        let field_ty = self
            .structs
            .get(&cls_name)
            .and_then(|hc| hc.fields.iter().find(|f| f.name == field))
            .map(|f| lower_type_hir(&f.field_type))
            .unwrap_or(SsaType::I64);

        let addr = self.current_block_data.fresh_value();
        self.emit(Instruction::FieldAddr {
            dest: addr,
            base: Operand::Value(obj_val),
            offset,
        });
        self.current_block_data
            .value_types
            .insert(addr, SsaType::Pointer(Box::new(field_ty.clone())));

        (addr, field_ty)
    }

    pub fn mangled_method_name(&self, struct_name: StrId, method_name: &str) -> StrId {
        let method_id = StrId(self.context.intern(method_name));
        self.struct_mangled_map
            .get(&struct_name)
            .and_then(|m| m.get(&method_id))
            .copied()
            .unwrap_or_else(|| {
                panic!(
                    "mangled_method_name: no mangled entry for `{}::{}`",
                    struct_name, method_name
                )
            })
    }

    pub fn monomorphized_method_name(
        &self,
        struct_name: StrId,
        method_name: &str,
        ty: &HirType,
    ) -> StrId {
        let base = self.mangled_method_name(struct_name, method_name);
        let suffix = type_suffix_with_pool(self.context.clone(), ty);
        StrId(self.context.intern(&format!("{}{}", base.as_str(), suffix)))
    }

    pub fn struct_name_of_value(&self, v: Value) -> Option<StrId> {
        match self.current_block_data.value_types.get(&v)? {
            SsaType::User(name, _) => Some(*name),
            SsaType::Pointer(inner) => match inner.as_ref() {
                SsaType::User(name, _) => Some(*name),
                _ => None,
            },
            _ => None,
        }
    }

    pub fn emit_owning_free_call(
        &mut self,
        alloc_val: Value,
        alloc_cls_name: StrId,
        pointee_ty: &HirType,
        ptr_val: Value,
    ) {
        let free_fn = self.monomorphized_method_name(alloc_cls_name, "free", pointee_ty);
        self.emit(Instruction::Call {
            dest: None,
            func: Operand::FunctionRef(free_fn),
            args: SmallVec::from_slice_copy(&[Operand::Value(alloc_val), Operand::Value(ptr_val)]),
        });
    }

    pub fn emit_free_raw_call(
        &mut self,
        alloc_val: Value,
        alloc_cls_name: StrId,
        pointee_ty: &HirType,
        ptr_val: Value,
    ) {
        let (data_ptr, size_v, align_v) = if let HirType::Slice(inner) = pointee_ty {
            let elem_ssa = lower_type_hir(inner);
            let Layout {
                size: elem_size,
                align: elem_align,
            } = layout_of_ssa(&elem_ssa, TargetInfo { ptr_bytes: 8 })
                .expect("[emit_free_raw_call] layout_of_ssa failed on slice element");

            let data_ptr = self.current_block_data.fresh_value();
            self.emit(Instruction::LoadField {
                dest: data_ptr,
                base: Operand::Value(ptr_val),
                offset: 0,
            });
            self.current_block_data
                .value_types
                .insert(data_ptr, SsaType::Pointer(Box::new(elem_ssa)));

            let cap_v = self.current_block_data.fresh_value();
            self.emit(Instruction::LoadField {
                dest: cap_v,
                base: Operand::Value(ptr_val),
                offset: 16,
            });
            self.current_block_data
                .value_types
                .insert(cap_v, SsaType::Usize);

            let elem_size_v = self.current_block_data.fresh_value();
            self.emit(Instruction::Const {
                dest: elem_size_v,
                ty: SsaType::Usize,
                value: Operand::ConstInt(elem_size as i64),
            });
            self.current_block_data
                .value_types
                .insert(elem_size_v, SsaType::Usize);

            let total_size_v = self.current_block_data.fresh_value();
            self.emit(Instruction::Binary {
                dest: total_size_v,
                op: BinOp::Mul,
                left: Operand::Value(cap_v),
                right: Operand::Value(elem_size_v),
            });
            self.current_block_data
                .value_types
                .insert(total_size_v, SsaType::Usize);

            let align_v = self.current_block_data.fresh_value();
            self.emit(Instruction::Const {
                dest: align_v,
                ty: SsaType::Usize,
                value: Operand::ConstInt(elem_align as i64),
            });
            self.current_block_data
                .value_types
                .insert(align_v, SsaType::Usize);

            (data_ptr, total_size_v, align_v)
        } else {
            let query_ty = lower_type_hir(pointee_ty);
            let size_v = self.current_block_data.fresh_value();
            self.emit(Instruction::Intrinsic {
                dest: Some(size_v),
                op: IntrinsicOp::SizeOf,
                query_ty: Some(query_ty.clone()),
                args: SmallVec::new(),
            });
            self.current_block_data
                .value_types
                .insert(size_v, SsaType::Usize);

            let align_v = self.current_block_data.fresh_value();
            self.emit(Instruction::Intrinsic {
                dest: Some(align_v),
                op: IntrinsicOp::AlignOf,
                query_ty: Some(query_ty),
                args: SmallVec::new(),
            });
            self.current_block_data
                .value_types
                .insert(align_v, SsaType::Usize);

            (ptr_val, size_v, align_v)
        };

        let free_raw_fn = self.mangled_method_name(alloc_cls_name, "free_raw");
        self.emit(Instruction::Call {
            dest: None,
            func: Operand::FunctionRef(free_raw_fn),
            args: SmallVec::from_slice_copy(&[
                Operand::Value(alloc_val),
                Operand::Value(data_ptr),
                Operand::Value(size_v),
                Operand::Value(align_v),
            ]),
        });
    }

    pub fn emit_owned_pointer_drop<R: AllocatorResolver>(
        &mut self,
        owner: Option<StrId>,
        pointee: &DropKind<'a, 'bump>,
        pointee_ty: &HirType<'a, 'bump>,
        allocator: &ProvenanceAnnotation<'bump>,
        ptr_val: Value,
        track_partial_moves: bool,
        drop_state: Option<&DropMoveState<'a, 'bump>>,
        resolver: &mut R,
    ) {
        let alloc_val = self.resolve_allocator_value(allocator, resolver);
        let alloc_cls_name = self.struct_name_of_value(alloc_val).unwrap_or_else(|| {
            panic!("emit_owned_pointer_drop: could not determine allocator's struct type")
        });

        let kind = self
            .allocator_kind
            .get(&alloc_cls_name)
            .copied()
            .unwrap_or_else(|| {
                unreachable!(
                    "struct `{}` used as an allocator but has no AllocatorKind entry",
                    alloc_cls_name
                )
            });

        let partial_move = track_partial_moves
            && owner.is_some()
            && drop_state.map_or(false, |ds| ds.has_any_field_moves(owner.unwrap()));

        match pointee {
            DropKind::Type(struct_name) => match (kind, partial_move) {
                (AllocatorKind::Owning, false) => {
                    self.emit_owning_free_call(alloc_val, alloc_cls_name, pointee_ty, ptr_val);
                }
                (AllocatorKind::Owning, true) | (AllocatorKind::RawOnly, true) => {
                    if let (Some(o), Some(ds)) = (owner, drop_state) {
                        self.emit_partial_struct_field_drops(o, *struct_name, ptr_val, ds);
                    }
                    self.emit_free_raw_call(alloc_val, alloc_cls_name, pointee_ty, ptr_val);
                }
                (AllocatorKind::RawOnly, false) => {
                    if let Some(glue) = self.glue_registry.glue_name_for(*struct_name) {
                        self.emit(Instruction::Call {
                            dest: None,
                            func: Operand::FunctionRef(glue),
                            args: SmallVec::from_slice_copy(&[Operand::Value(ptr_val)]),
                        });
                    }
                    self.emit_free_raw_call(alloc_val, alloc_cls_name, pointee_ty, ptr_val);
                }
            },

            DropKind::Slice {
                element,
                element_ty,
            } => {
                self.emit_slice_loop_drop(element, element_ty, ptr_val, resolver);
                self.emit_free_raw_call(alloc_val, alloc_cls_name, pointee_ty, ptr_val);
            }

            DropKind::Undroppable => {
                self.emit_free_raw_call(alloc_val, alloc_cls_name, pointee_ty, ptr_val);
            }

            DropKind::OwnedPointer {
                pointee: inner_pointee,
                pointee_ty: inner_pointee_ty,
                allocator: inner_allocator,
            } => {
                let inner_val = if matches!(inner_pointee_ty, HirType::Slice(_)) {
                    ptr_val
                } else {
                    let loaded = self.current_block_data.fresh_value();
                    self.emit(Instruction::Load {
                        dest: loaded,
                        ptr: Operand::Value(ptr_val),
                    });
                    self.current_block_data
                        .value_types
                        .insert(loaded, lower_type_hir(pointee_ty));
                    loaded
                };
                self.emit_owned_pointer_drop(
                    owner,
                    inner_pointee,
                    inner_pointee_ty,
                    inner_allocator,
                    inner_val,
                    false,
                    drop_state,
                    resolver,
                );
                self.emit_free_raw_call(alloc_val, alloc_cls_name, pointee_ty, ptr_val);
            }
        }
    }

    pub fn emit_element_drop<R: AllocatorResolver>(
        &mut self,
        kind: &DropKind<'a, 'bump>,
        elem_addr: Value,
        resolver: &mut R,
    ) {
        match kind {
            DropKind::Type(struct_name) => {
                if let Some(glue) = self.glue_registry.glue_name_for(*struct_name) {
                    self.emit(Instruction::Call {
                        dest: None,
                        func: Operand::FunctionRef(glue),
                        args: SmallVec::from_slice_copy(&[Operand::Value(elem_addr)]),
                    });
                }
            }
            DropKind::OwnedPointer {
                pointee,
                pointee_ty,
                allocator,
            } => {
                let loaded = self.current_block_data.fresh_value();
                self.emit(Instruction::Load {
                    dest: loaded,
                    ptr: Operand::Value(elem_addr),
                });
                self.current_block_data
                    .value_types
                    .insert(loaded, lower_type_hir(pointee_ty));
                self.emit_owned_pointer_drop(
                    None, pointee, pointee_ty, allocator, loaded, false, None, resolver,
                );
            }
            DropKind::Slice {
                element,
                element_ty,
            } => {
                self.emit_slice_loop_drop(element, element_ty, elem_addr, resolver);
            }
            DropKind::Undroppable => {}
        }
    }

    pub fn contribute_phi_edge(
        &mut self,
        block_id: BlockId,
        phi_idx: usize,
        from_bb: BlockId,
        val: Value,
    ) {
        let block = self
            .current_block_data
            .func
            .blocks
            .iter_mut()
            .find(|b| b.id == block_id)
            .expect("phi block missing");
        if let Instruction::Phi { incoming, .. } = &mut block.instructions[phi_idx] {
            incoming.push((from_bb, val));
        }
    }

    pub fn emit_slice_loop_drop<R: AllocatorResolver>(
        &mut self,
        element_kind: &DropKind<'a, 'bump>,
        element_ty: &HirType<'a, 'bump>,
        fat_ptr_addr: Value,
        resolver: &mut R,
    ) {
        if !element_kind.is_droppable() {
            return;
        }

        let elem_ssa = lower_type_hir(element_ty);
        let elem_size = sizeof_ssa(&elem_ssa, TargetInfo { ptr_bytes: 8 })
            .expect("slice element type has no known size");

        let data_ptr = self.current_block_data.fresh_value();
        self.emit(Instruction::LoadField {
            dest: data_ptr,
            base: Operand::Value(fat_ptr_addr),
            offset: 0,
        });
        self.current_block_data
            .value_types
            .insert(data_ptr, SsaType::Pointer(Box::new(elem_ssa.clone())));

        let len_v = self.current_block_data.fresh_value();
        self.emit(Instruction::LoadField {
            dest: len_v,
            base: Operand::Value(fat_ptr_addr),
            offset: 8,
        });
        self.current_block_data
            .value_types
            .insert(len_v, SsaType::Usize);

        let i_init = self.current_block_data.fresh_value();
        self.emit(Instruction::Const {
            dest: i_init,
            ty: SsaType::Usize,
            value: Operand::ConstInt(0),
        });
        self.current_block_data
            .value_types
            .insert(i_init, SsaType::Usize);

        let elem_size_v = self.current_block_data.fresh_value();
        self.emit(Instruction::Const {
            dest: elem_size_v,
            ty: SsaType::Usize,
            value: Operand::ConstInt(elem_size as i64),
        });
        self.current_block_data
            .value_types
            .insert(elem_size_v, SsaType::Usize);

        let pre_loop_bb = self.current_block_data.current_block;
        let cond_bb = self.current_block_data.new_block();
        let body_bb = self.current_block_data.new_block();
        let after_bb = self.current_block_data.new_block();
        self.emit(Instruction::Jump { target: cond_bb });

        self.current_block_data.switch_to(cond_bb);
        let i_phi = self.current_block_data.fresh_value();
        self.current_block_data
            .value_types
            .insert(i_phi, SsaType::Usize);
        let phi_idx = self.current_block_data.bb().instructions.len();
        self.emit(Instruction::Phi {
            dest: i_phi,
            incoming: SmallVec::new(),
        });
        self.contribute_phi_edge(cond_bb, phi_idx, pre_loop_bb, i_init);

        let cond = self.current_block_data.fresh_value();
        self.emit(Instruction::Binary {
            dest: cond,
            op: BinOp::Lt,
            left: Operand::Value(i_phi),
            right: Operand::Value(len_v),
        });
        self.current_block_data
            .value_types
            .insert(cond, SsaType::Bool);
        self.emit(Instruction::Branch {
            cond: Operand::Value(cond),
            then_bb: body_bb,
            else_bb: after_bb,
        });

        self.current_block_data.switch_to(body_bb);
        let byte_off = self.current_block_data.fresh_value();
        self.emit(Instruction::Binary {
            dest: byte_off,
            op: BinOp::Mul,
            left: Operand::Value(i_phi),
            right: Operand::Value(elem_size_v),
        });
        self.current_block_data
            .value_types
            .insert(byte_off, SsaType::Usize);

        let elem_addr = self.current_block_data.fresh_value();
        self.emit(Instruction::Binary {
            dest: elem_addr,
            op: BinOp::Add,
            left: Operand::Value(data_ptr),
            right: Operand::Value(byte_off),
        });
        self.current_block_data
            .value_types
            .insert(elem_addr, SsaType::Pointer(Box::new(elem_ssa)));

        self.emit_element_drop(element_kind, elem_addr, resolver);

        let i_next = self.current_block_data.fresh_value();
        self.emit(Instruction::Binary {
            dest: i_next,
            op: BinOp::Add,
            left: Operand::Value(i_phi),
            right: Operand::ConstInt(1),
        });
        self.current_block_data
            .value_types
            .insert(i_next, SsaType::Usize);

        let body_tail = self.current_block_data.current_block;
        self.contribute_phi_edge(cond_bb, phi_idx, body_tail, i_next);
        self.emit(Instruction::Jump { target: cond_bb });

        self.current_block_data.switch_to(after_bb);
    }

    pub fn emit_partial_struct_field_drops(
        &mut self,
        owner: StrId,
        struct_name: StrId,
        base: Value,
        drop_state: &DropMoveState<'a, 'bump>,
    ) {
        let Some(hir_struct) = self.structs.get(&struct_name) else {
            return;
        };
        let Some(offsets) = self.struct_field_offsets.get(&struct_name) else {
            return;
        };

        for field in hir_struct.fields.iter().rev() {
            if drop_state.is_field_moved(owner, field.name) {
                continue;
            }
            let HirType::Struct {
                name: field_struct_name,
                ..
            } = &field.field_type
            else {
                continue;
            };
            let Some(field_glue) = self.glue_registry.glue_name_for(*field_struct_name) else {
                continue;
            };
            let Some(&offset) = offsets.get(&field.name) else {
                continue;
            };
            let field_ptr = self.current_block_data.fresh_value();
            self.current_block_data.value_types.insert(
                field_ptr,
                SsaType::Pointer(Box::new(SsaType::User(*field_struct_name, vec![]))),
            );
            self.emit(Instruction::FieldAddr {
                dest: field_ptr,
                base: Operand::Value(base),
                offset,
            });
            self.emit(Instruction::Call {
                dest: None,
                func: Operand::FunctionRef(field_glue),
                args: SmallVec::from_slice_copy(&[Operand::Value(field_ptr)]),
            });
        }
    }
}
