use std::collections::HashSet;
use std::sync::Arc;

use super::context::HirLowerer;
use crate::optimized_string_buffering::build_module_scoped_name;
use ir::ast::Stmt;
use ir::ast::{FuncDecl, Path};
use ir::hir::HirFunc;
use ir::hir::HirFuncProto;
use ir::hir::{Hir, HirModule, HirStmt, StrId};
use ir::hir::{HirParam, HirType};
use ir::ir_hasher::FxHashMap;
use ir::span::SourceSpan;
use zetaruntime::arena::GrowableAtomicBump;
use zetaruntime::intern_fmt;
use zetaruntime::string_pool::StringPool;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) enum ImplTargetKind {
    UserType,
    Primitive,
    /// element is `None` for the bare `impl []` form and for a generic
    /// element (`impl<T> []T`, `impl []u32`)
    Slice {
        element: Option<StrId>,
    },
}

pub fn primitive_canonical_name(kind: &ir::ast::TypeKind) -> Option<&'static str> {
    use ir::ast::TypeKind::*;
    Some(match kind {
        I8 => "i8",
        I16 => "i16",
        I32 => "i32",
        I64 => "i64",
        I128 => "i128",
        U8 => "u8",
        U16 => "u16",
        U32 => "u32",
        U64 => "u64",
        U128 => "u128",
        Usize => "usize",
        Isize => "isize",
        F32 => "f32",
        F64 => "f64",
        Boolean => "bool",
        String => "str",
        Char => "char",
        _ => return None,
    })
}

pub fn primitive_hir_type<'a, 'bump>(name: StrId, pool: &StringPool) -> Option<HirType<'a, 'bump>> {
    Some(match pool.resolve_string(&name) {
        "i8" => HirType::I8,
        "i16" => HirType::I16,
        "i32" => HirType::I32,
        "i64" => HirType::I64,
        "i128" => HirType::I128,
        "u8" => HirType::U8,
        "u16" => HirType::U16,
        "u32" => HirType::U32,
        "u64" => HirType::U64,
        "u128" => HirType::U128,
        "usize" => HirType::Usize,
        "isize" => HirType::Isize,
        "f32" => HirType::F32,
        "f64" => HirType::F64,
        "bool" => HirType::Boolean,
        "str" => HirType::String,
        "char" => HirType::Char,
        _ => return None,
    })
}

impl<'a, 'bump> HirLowerer<'a, 'bump> {
    fn resolve_imports(&mut self, stmts: &[Stmt<'a, 'bump>]) {
        self.ctx.named_imports.borrow_mut().clear();
        self.ctx.imported_modules.borrow_mut().clear();

        for stmt in stmts {
            if let Stmt::Import(import_stmt) = stmt {
                let segments = import_stmt.path.path;
                match self.ctx.dep_graph.borrow().resolve_module_path(segments) {
                    Some(target_idx) => match import_stmt.path.member {
                        None => {
                            if let Some(&local_name) = segments.last() {
                                self.ctx
                                    .imported_modules
                                    .borrow_mut()
                                    .insert(local_name, target_idx);
                            }
                        }
                        Some(member) => {
                            self.ctx
                                .named_imports
                                .borrow_mut()
                                .insert(member, target_idx);
                        }
                    },
                    None => {
                        let path_str = segments
                            .iter()
                            .map(|s| self.ctx.context.resolve_string(s).to_string())
                            .collect::<Vec<_>>()
                            .join("::");
                        let member_str = import_stmt
                            .path
                            .member
                            .map(|m| format!(".{}", self.ctx.context.resolve_string(&m)))
                            .unwrap_or_default();
                        self.ctx.record_error(
                            format!(
                                "cannot resolve import `{}{}`: no module is registered for path `{}`. \
                                 Either the target file hasn't been compiled yet, or the path is wrong.",
                                path_str, member_str, path_str,
                            ),
                            import_stmt.path.span,
                        );
                    }
                }
            }
        }
    }

    pub fn lower_module_types(
        &mut self,
        stmts: &Vec<Stmt<'a, 'bump>, Arc<GrowableAtomicBump<'bump>>>,
        module_idx: usize,
    ) {
        self.ctx.module_idx = module_idx;
        self.resolve_imports(stmts);
        self.register_auto_import_aliases(module_idx);
        self.collect_type_declarations(stmts);
    }

    pub fn lower_module_prototypes(
        &mut self,
        stmts: &Vec<Stmt<'a, 'bump>, Arc<GrowableAtomicBump<'bump>>>,
        module_idx: usize,
    ) {
        self.ctx.module_idx = module_idx;
        self.resolve_imports(stmts);
        self.register_auto_import_aliases(module_idx);
        self.collect_function_prototypes(stmts);
    }

    pub fn lower_module_bodies(
        &mut self,
        stmts: &Vec<Stmt<'a, 'bump>, Arc<GrowableAtomicBump<'bump>>>,
        module_idx: usize,
    ) -> HirModule<'a, 'bump> {
        self.ctx.module_idx = module_idx;
        self.resolve_imports(stmts);
        self.register_auto_import_aliases(module_idx);

        let (imports, items, pkg_name) = self.lower_function_bodies(stmts);

        HirModule {
            name: pkg_name.unwrap_or_else(|| StrId(self.ctx.context.intern("root"))),
            imports: self.ctx.bump.alloc_slice(&imports),
            items: self.ctx.bump.alloc_slice(&items),
        }
    }

    pub fn lower_module(
        &mut self,
        stmts: &Vec<Stmt<'a, 'bump>, Arc<GrowableAtomicBump<'bump>>>,
        module_idx: usize,
    ) -> HirModule<'a, 'bump> {
        self.lower_module_prototypes(stmts, module_idx);
        self.lower_module_bodies(stmts, module_idx)
    }

    pub fn lower_all_modules(
        &mut self,
        module_stmts: &FxHashMap<usize, Vec<Stmt<'a, 'bump>, Arc<GrowableAtomicBump<'bump>>>>,
        compile_order: &[usize],
    ) -> FxHashMap<usize, HirModule<'a, 'bump>> {
        let mut seen: HashSet<usize> = HashSet::default();
        let mut groups: Vec<Vec<usize>> = Vec::new();

        for &module_idx in compile_order {
            if !seen.insert(module_idx) {
                continue;
            }
            let scc = self.ctx.dep_graph.borrow().modules_in_same_scc(module_idx);
            for &m in &scc {
                seen.insert(m);
            }
            groups.push(scc);
        }

        for group in &groups {
            for &module_idx in group {
                if let Some(stmts) = module_stmts.get(&module_idx) {
                    self.lower_module_prototypes(stmts, module_idx);
                }
            }
        }

        let mut result = FxHashMap::default();
        for group in &groups {
            for &module_idx in group {
                if let Some(stmts) = module_stmts.get(&module_idx) {
                    let hir_module = self.lower_module_bodies(stmts, module_idx);
                    result.insert(module_idx, hir_module);
                }
            }
        }

        result
    }

    fn register_auto_import_aliases(&mut self, module_idx: usize) {
        for auto_path in self.ctx.auto_imports.borrow().paths() {
            let Some(&last) = auto_path.last() else {
                continue;
            };
            let segments: Vec<StrId> = auto_path
                .iter()
                .map(|s| StrId(self.ctx.context.intern(s)))
                .collect();
            let Some(target_idx) = self.ctx.dep_graph.borrow().resolve_module_path(&segments)
            else {
                continue;
            };
            if target_idx == module_idx {
                continue;
            }
            let alias = StrId(self.ctx.context.intern(last));
            self.ctx
                .imported_modules
                .borrow_mut()
                .entry(alias)
                .or_insert(target_idx);
        }
    }

    pub fn struct_type_name_path(ty: &ir::ast::Type<'a, 'bump>) -> Option<(StrId, &'bump [StrId])> {
        match ty.kind {
            ir::ast::TypeKind::Struct { name, path, .. } => Some((name, path)),
            _ => None,
        }
    }

    pub fn collect_type_declarations(&mut self, stmts: &[Stmt<'a, 'bump>]) {
        for stmt in stmts {
            if let Stmt::InterfaceDecl(interface_decl) = stmt {
                let hir_interface = self.lower_interface_decl(**interface_decl);
                self.ctx
                    .interfaces
                    .borrow_mut()
                    .insert(hir_interface.name, hir_interface);
            }
            if let Stmt::StructDecl(struct_decl) = stmt {
                let ty_struct = self.lower_struct_decl(**struct_decl);
                self.ctx
                    .structs
                    .borrow_mut()
                    .insert(ty_struct.name, ty_struct);
            }
            if let Stmt::EnumDecl(enum_decl) = stmt {
                let ty_enum = self.lower_enum_decl(**enum_decl);
                self.ctx.enums.borrow_mut().insert(ty_enum.name, ty_enum);
            }
            if let Stmt::Module(module_decl) = stmt {
                self.collect_type_declarations(module_decl.body);
            }
        }
    }

    pub fn collect_function_prototypes(&mut self, stmts: &[Stmt<'a, 'bump>]) {
        for stmt in stmts {
            if let Stmt::InterfaceDecl(interface_decl) = stmt {
                let Some(methods) = interface_decl.methods else {
                    continue;
                };

                let iface_generics = interface_decl.generics.unwrap_or_default();
                for g in iface_generics {
                    self.add_generic_param(g.type_name);
                }

                for x in methods {
                    self.lower_func_as_proto(x, Some(interface_decl.name));
                }

                for g in iface_generics {
                    self.remove_generic_param(g.type_name);
                }
            }
        }

        for stmt in stmts {
            if let Stmt::FuncDecl(f) = stmt {
                self.lower_func_as_proto(f, None);
            }
            if let Stmt::ImplDecl(impl_decl) = stmt {
                let Some((target_key, _target_kind)) =
                    self.resolve_impl_target(&impl_decl.target, impl_decl.span)
                else {
                    self.ctx.record_error(
                        format!(
                            "invalid `impl` target `{:?}`: expected a struct, primitive, or slice/array type",
                            impl_decl.target.kind
                        ),
                        impl_decl.span,
                    );
                    continue;
                };

                if let Some(iface_ty) = impl_decl.interface {
                    let Some((iface_name, iface_path)) = Self::struct_type_name_path(&iface_ty)
                    else {
                        self.ctx.record_error(
                            format!(
                                "`by` clause must name an interface type, found `{:?}`",
                                iface_ty.kind
                            ),
                            impl_decl.span,
                        );
                        continue;
                    };
                    let iface_key =
                        self.ctx
                            .resolve_type_path_name(iface_path, iface_name, impl_decl.span);
                    self.ctx
                        .struct_interfaces
                        .borrow_mut()
                        .entry(target_key)
                        .or_insert_with(Vec::new)
                        .push(iface_key);
                }

                let Some(methods) = impl_decl.methods else {
                    continue;
                };

                let impl_generics = impl_decl.generics.unwrap_or_default();
                for g in impl_generics {
                    self.add_generic_param(g.type_name);
                }

                for x in methods {
                    let hir_func = self.lower_func_as_proto(x, Some(target_key));
                    self.ctx
                        .struct_methods
                        .borrow_mut()
                        .entry(target_key)
                        .or_insert_with(FxHashMap::default)
                        .insert(hir_func.unmangled_name, hir_func.name);
                }

                for g in impl_generics {
                    self.remove_generic_param(g.type_name);
                }
            }
            if let Stmt::Module(module_decl) = stmt {
                self.collect_function_prototypes(module_decl.body);
            }
        }
    }

    pub fn collect_prototypes(&mut self, stmts: &[Stmt<'a, 'bump>]) {
        self.collect_type_declarations(stmts);
        self.collect_function_prototypes(stmts);
    }

    fn lower_func_as_proto(
        &mut self,
        f: &FuncDecl<'a, 'bump>,
        struct_name: Option<StrId>,
    ) -> HirFunc<'a, 'bump> {
        let mut proto: HirFuncProto = self.lower_func_proto(f);
        let is_extern = matches!(
            proto.function_metadata.extern_modifier,
            ir::ast::ExternModifier::Abi(_)
        );
        let is_main = self.ctx.context.resolve_string(&proto.name) == "main";
        if !is_extern && !is_main {
            proto.name = self.mangle_function_name(self.ctx.module_idx, struct_name, proto.name);
        }

        let hir_func = HirFunc {
            name: proto.name,
            params: proto.params,
            return_type: Some(proto.return_type),
            body: None,
            function_metadata: proto.function_metadata,
            generics: proto.generics,
            unmangled_name: proto.unmangled_name,
            declaring_module_idx: self.ctx.module_idx,
            impl_target: struct_name,
        };

        self.ctx.functions.borrow_mut().insert(proto.name, hir_func);
        hir_func
    }

    pub(super) fn mangle_function_name(
        &self,
        declaring_module_idx: usize,
        struct_name: Option<StrId>,
        name: StrId,
    ) -> StrId {
        let Some(pkg) = self
            .ctx
            .dep_graph
            .borrow()
            .get_module_package(declaring_module_idx)
        else {
            return match struct_name {
                Some(cls) => build_module_scoped_name(&[cls], name, None, self.ctx.context.clone()),
                None => name,
            };
        };

        let pkg_str = self.ctx.context.resolve_string(&pkg);

        let mut segments = Vec::new();

        if let Some(cls) = struct_name {
            segments.push(cls);
        }

        segments.extend(
            pkg_str
                .split("::")
                .map(|seg| StrId(self.ctx.context.intern(seg))),
        );

        build_module_scoped_name(&segments, name, None, self.ctx.context.clone())
    }

    pub(super) fn mangle_with_module_path(&self, name: StrId) -> StrId {
        let Some(pkg) = self
            .ctx
            .dep_graph
            .borrow()
            .get_module_package(self.ctx.module_idx)
        else {
            // No package declaration for this module
            return name;
        };

        let pkg_str = self.ctx.context.resolve_string(&pkg);
        let segments: Vec<StrId> = pkg_str
            .split("_")
            .map(|seg| StrId(self.ctx.context.intern(seg)))
            .collect();

        build_module_scoped_name(&segments, name, None, self.ctx.context.clone())
    }

    pub fn lower_function_bodies(
        &mut self,
        stmts: &Vec<ir::ast::Stmt<'a, 'bump>, Arc<GrowableAtomicBump<'bump>>>,
    ) -> (Vec<Path<'a, 'bump>>, Vec<Hir<'a, 'bump>>, Option<StrId>) {
        let mut imports: Vec<Path<'a, 'bump>> = Vec::with_capacity(64);
        let mut items: Vec<Hir<'a, 'bump>> = Vec::with_capacity(64);
        let mut pkg_name: Option<StrId> = None;

        for stmt in stmts {
            match stmt {
                Stmt::Import(import_stmt) => {
                    imports.push(*import_stmt.path);
                }
                Stmt::Package(package_stmt) => {
                    let joined = format!("{}", package_stmt.path);
                    pkg_name = Some(StrId(self.ctx.context.intern(&joined)));
                }
                Stmt::Module(module_decl) => {
                    for &body_stmt in module_decl.body {
                        match body_stmt {
                            Stmt::FuncDecl(f) => {
                                let lowered_body = f.body.map(|b| self.lower_block(b));

                                let is_extern = matches!(
                                    f.function_metadata.extern_modifier,
                                    ir::ast::ExternModifier::Abi(_)
                                );
                                let is_main = self.ctx.context.resolve_string(&f.name) == "main";
                                let lookup_name = if is_extern || is_main {
                                    f.name
                                } else {
                                    self.mangle_with_module_path(f.name)
                                };

                                let mut func_binding = self.ctx.functions.borrow_mut();
                                let func = func_binding.get_mut(&lookup_name).unwrap();
                                func.body = lowered_body;
                                items.push(Hir::Func(self.ctx.bump.alloc_value(func.clone())));
                            }
                            other => items.push(self.lower_toplevel(other)),
                        }
                    }
                }
                Stmt::FuncDecl(f) => {
                    let lowered_body = f.body.map(|b| self.lower_block(b));

                    let is_extern = matches!(
                        f.function_metadata.extern_modifier,
                        ir::ast::ExternModifier::Abi(_)
                    );
                    let is_main = f.name.eq("main");
                    let lookup_name = if is_extern || is_main {
                        f.name
                    } else {
                        self.mangle_with_module_path(f.name)
                    };
                    let mut func_binding = self.ctx.functions.borrow_mut();
                    let func = func_binding.get_mut(&lookup_name).unwrap();

                    func.body = lowered_body;

                    let hir_func = Hir::Func(self.ctx.bump.alloc_value(func.clone()));
                    items.push(hir_func);
                }
                other => items.push(self.lower_toplevel(*other)),
            }
        }

        (imports, items, pkg_name)
    }

    pub fn lower_func_proto(&self, f: &FuncDecl<'a, 'bump>) -> HirFuncProto<'a, 'bump> {
        let generics = self.lower_generics_slice(f.generics.unwrap_or_default());

        if let Some(gs) = generics {
            for g in gs {
                self.add_generic_param(g.name);
            }
        }

        let params = f.params.map(|ps| {
            let lowered: Vec<HirParam<'a, 'bump>> = self.lower_params(ps);
            self.ctx.bump.alloc_slice_immutable(&lowered)
        });
        let return_type = match f.return_type {
            Some(ty) => self.lower_type(&ty, f.span),
            None => HirType::Void,
        };

        if let Some(gs) = generics {
            for g in gs {
                self.remove_generic_param(g.name);
            }
        }

        HirFuncProto {
            name: f.name,
            params,
            return_type,
            function_metadata: f.function_metadata,
            generics,
            unmangled_name: f.name,
        }
    }

    pub fn lower_block(&self, block: &'a ir::ast::Block<'a, 'bump>) -> HirStmt<'a, 'bump> {
        let mut stmts = Vec::with_capacity(block.block.len());

        for stmt in block {
            stmts.push(self.lower_stmt(*stmt));
        }

        HirStmt::Block {
            body: self.ctx.bump.alloc_slice(&stmts),
        }
    }

    pub(super) fn lower_toplevel(&mut self, stmt: Stmt<'a, 'bump>) -> Hir<'a, 'bump> {
        match stmt {
            Stmt::FuncDecl(f) => {
                let func = self.lower_func_body_from_proto(*f, None);

                let is_extern = matches!(
                    f.function_metadata.extern_modifier,
                    ir::ast::ExternModifier::Abi(_)
                );
                let is_main = self.ctx.context.resolve_string(&f.name) == "main";

                let lookup_name = if is_extern || is_main {
                    f.name
                } else {
                    self.mangle_with_module_path(f.name)
                };

                self.ctx.functions.borrow_mut().insert(lookup_name, func);

                Hir::Func(
                    self.ctx
                        .bump
                        .alloc_value(self.ctx.functions.borrow()[&lookup_name]),
                )
            }
            Stmt::StructDecl(c) => {
                let ty_struct = self.lower_struct_decl(*c);
                Hir::Struct(self.ctx.bump.alloc_value(ty_struct))
            }
            Stmt::InterfaceDecl(i) => {
                let interface = self.lower_interface_decl(*i);
                Hir::Interface(self.ctx.bump.alloc_value(interface))
            }
            Stmt::ImplDecl(i) => {
                let impl_decl = self.lower_impl_decl(*i);
                Hir::Impl(self.ctx.bump.alloc_value(impl_decl))
            }
            Stmt::EnumDecl(e) => {
                let enum_decl = self.lower_enum_decl(*e);
                Hir::Enum(self.ctx.bump.alloc_value(enum_decl))
            }
            Stmt::UnsafeBlock(b) => {
                let body_vec: Vec<HirStmt<'a, 'bump>> =
                    b.block.into_iter().map(|s| self.lower_stmt(*s)).collect();
                let body_slice = self.ctx.bump.alloc_slice(&body_vec);
                let inner_block = self
                    .ctx
                    .bump
                    .alloc_value_immutable(HirStmt::Block { body: body_slice });
                let stmt = HirStmt::UnsafeBlock { body: inner_block };
                Hir::Stmt(self.ctx.bump.alloc_value_immutable(stmt))
            }
            Stmt::Const(c) => Hir::Const(
                self.ctx
                    .bump
                    .alloc_value_immutable(self.lower_const_stmt(*c)),
            ),
            other => {
                let stmt = self.lower_stmt(other);
                Hir::Stmt(self.ctx.bump.alloc_value(stmt))
            }
        }
    }

    pub(super) fn resolve_impl_target(
        &self,
        ty: &ir::ast::Type<'a, 'bump>,
        span: SourceSpan<'a>,
    ) -> Option<(StrId, ImplTargetKind)> {
        use ir::ast::TypeKind;
        match ty.kind {
            TypeKind::Struct { name, path, .. } => Some((
                self.ctx.resolve_type_path_name(path, name, span),
                ImplTargetKind::UserType,
            )),
            TypeKind::AnySlice => Some((
                StrId(self.ctx.context.intern("slice")),
                ImplTargetKind::Slice { element: None },
            )),
            TypeKind::Slice { inner } | TypeKind::Array { inner, .. } => {
                let elem_key = self.slice_element_key(inner, span);
                let key = match elem_key {
                    Some(e) => StrId(intern_fmt!(self.ctx.context, "slice_{}", e)),
                    None => StrId(self.ctx.context.intern("slice")),
                };
                Some((key, ImplTargetKind::Slice { element: elem_key }))
            }
            ref other => {
                let prim = primitive_canonical_name(other)?;
                Some((
                    StrId(self.ctx.context.intern(prim)),
                    ImplTargetKind::Primitive,
                ))
            }
        }
    }

    fn slice_element_key(
        &self,
        elem: &ir::ast::Type<'a, 'bump>,
        span: SourceSpan<'a>,
    ) -> Option<StrId> {
        use ir::ast::TypeKind;
        match elem.kind {
            TypeKind::Struct { name, path, .. } => {
                Some(self.ctx.resolve_type_path_name(path, name, span))
            }
            ref other => primitive_canonical_name(other).map(|s| StrId(self.ctx.context.intern(s))),
        }
    }
}
