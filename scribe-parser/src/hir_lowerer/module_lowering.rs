use super::context::HirLowerer;
use crate::optimized_string_buffering::build_module_scoped_name;
use ir::ast::Stmt;
use ir::ast::{FuncDecl, Path};
use ir::hir::HirFunc;
use ir::hir::HirFuncProto;
use ir::hir::{Hir, HirModule, HirStmt, StrId};
use ir::hir::{HirParam, HirType};
use ir::ir_hasher::FxHashMap;
use zetaruntime::bump::GrowableBump;

impl<'a, 'bump> HirLowerer<'a, 'bump> {
    pub fn lower_module(
        &mut self,
        stmts: Vec<Stmt<'a, 'bump>, &GrowableBump<'bump>>,
        module_idx: usize,
    ) -> HirModule<'a, 'bump> {
        self.ctx.module_idx = module_idx;
        // Register all modules this file imports so expression lowering
        // can resolve module-qualified paths.
        let imported_idxs = self.ctx.dep_graph.get_module_imports(module_idx);
        for imp_idx in imported_idxs {
            if let Some(pkg) = self.ctx.dep_graph.get_module_package(imp_idx) {
                // Last path segment is the local name: std::io -> "io"
                let local_name = pkg.as_str().split("_").last().unwrap_or(pkg.as_str());
                let local_id = StrId(self.ctx.context.intern(local_name));
                self.ctx
                    .imported_modules
                    .borrow_mut()
                    .insert(local_id, imp_idx);
            }
        }

        self.collect_prototypes(&stmts);
        let (imports, items, pkg_name) = self.lower_function_bodies(stmts);

        HirModule {
            name: pkg_name.unwrap_or_else(|| StrId(self.ctx.context.intern("root"))),
            imports: self.ctx.bump.alloc_slice(&imports),
            items: self.ctx.bump.alloc_slice(&items),
        }
    }

    pub fn collect_prototypes(&mut self, stmts: &[Stmt<'a, 'bump>]) {
        // register all struct declarations so that lower_type_inner
        // can look up field types when processing function signatures below.
        for stmt in stmts {
            if let Stmt::StructDecl(struct_decl) = stmt {
                let class = self.lower_struct_decl(**struct_decl);
                self.ctx.classes.borrow_mut().insert(class.name, class);
            }
            if let Stmt::Module(module_decl) = stmt {
                for &body_stmt in module_decl.body {
                    if let Stmt::StructDecl(struct_decl) = body_stmt {
                        let class = self.lower_struct_decl(*struct_decl);
                        self.ctx.classes.borrow_mut().insert(class.name, class);
                    }
                }
            }
        }

        for stmt in stmts {
            if let Stmt::FuncDecl(f) = stmt {
                self.lower_func_as_proto(f, None);
            }
            if let Stmt::InterfaceDecl(interface_decl) = stmt {
                let Some(methods) = interface_decl.methods else {
                    continue;
                };
                for x in methods {
                    self.lower_func_as_proto(x, Some(interface_decl.name));
                }
            }
            if let Stmt::ImplDecl(impl_decl) = stmt {
                if let Some(iface) = impl_decl.interface {
                    self.ctx
                        .struct_interfaces
                        .borrow_mut()
                        .entry(impl_decl.target)
                        .or_insert_with(Vec::new)
                        .push(iface);
                }

                let Some(methods) = impl_decl.methods else {
                    continue;
                };
                for x in methods {
                    let hir_func = self.lower_func_as_proto(x, Some(impl_decl.target));
                    self.ctx
                        .struct_methods
                        .borrow_mut()
                        .entry(impl_decl.target)
                        .or_insert_with(FxHashMap::default)
                        .insert(hir_func.unmangled_name, hir_func.name);
                }
            }
            if let Stmt::Module(module_decl) = stmt {
                self.collect_prototypes(module_decl.body);
            }
        }
    }

    fn lower_func_as_proto(
        &mut self,
        f: &FuncDecl<'a, 'bump>,
        class_name: Option<StrId>,
    ) -> HirFunc<'a, 'bump> {
        let mut proto: HirFuncProto = self.lower_func_proto(f);
        let is_extern = matches!(
            proto.function_metadata.extern_modifier,
            ir::ast::ExternModifier::Abi(_)
        );
        let is_main = self.ctx.context.resolve_string(&proto.name) == "main";
        if !is_extern && !is_main {
            proto.name = self.mangle_function_name(class_name, proto.name);
        }

        let hir_func = HirFunc {
            name: proto.name,
            params: proto.params,
            return_type: Some(proto.return_type),
            body: None,
            function_metadata: proto.function_metadata,
            generics: None,
            unmangled_name: proto.unmangled_name,
        };

        self.ctx.functions.borrow_mut().insert(proto.name, hir_func);
        hir_func
    }

    pub(super) fn mangle_function_name(&self, class_name: Option<StrId>, name: StrId) -> StrId {
        let Some(pkg) = self.ctx.dep_graph.get_module_package(self.ctx.module_idx) else {
            return match class_name {
                Some(cls) => build_module_scoped_name(&[cls], name, None, self.ctx.context.clone()),
                None => name,
            };
        };

        let pkg_str = self.ctx.context.resolve_string(&pkg);
        let mut segments: Vec<StrId> = Vec::new();
        if let Some(cls) = class_name {
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
        let Some(pkg) = self.ctx.dep_graph.get_module_package(self.ctx.module_idx) else {
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
        stmts: Vec<ir::ast::Stmt<'a, 'bump>, &GrowableBump<'bump>>,
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
                other => items.push(self.lower_toplevel(other)),
            }
        }

        (imports, items, pkg_name)
    }

    pub fn lower_func_proto(&self, f: &FuncDecl<'a, 'bump>) -> HirFuncProto<'a, 'bump> {
        let params = f.params.map(|ps| {
            let lowered: Vec<HirParam<'a, 'bump>> = self.lower_params(ps);
            self.ctx.bump.alloc_slice_immutable(&lowered)
        });

        let return_type = match f.return_type {
            Some(ty) => self.lower_type(&ty),
            None => HirType::Void,
        };

        let proto = HirFuncProto {
            name: f.name,
            params,
            return_type,
            function_metadata: f.function_metadata,
            generics: if f.generics.is_none() {
                None
            } else {
                self.lower_generics_slice(f.generics.unwrap())
            },
            unmangled_name: f.name,
        };

        proto
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

                // hydrate the global function table
                self.ctx.functions.borrow_mut().insert(lookup_name, func);

                Hir::Func(
                    self.ctx
                        .bump
                        .alloc_value(self.ctx.functions.borrow()[&lookup_name]),
                )
            }
            Stmt::StructDecl(c) => {
                let class = self.lower_struct_decl(*c);
                Hir::Struct(self.ctx.bump.alloc_value(class))
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
}
