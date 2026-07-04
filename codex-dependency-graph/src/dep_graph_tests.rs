#[cfg(test)]
mod tests {
    use crate::dep_graph::{AstModule, DepGraph, NodeKind};
    use crate::module_collection_builder::ModuleBuilder;
    use ir::ast::{
        ConstStmt, EnumDecl, ExternModifier, FuncDecl, FuncModifiers, FuncSafety, ImplDecl,
        InlineModifier, InterfaceDecl, LetStmt, Path, Stmt, StructDecl, Type, TypeKind, Visibility,
    };
    use ir::hir::StrId;
    use ir::ir_hasher::HashMap;
    use ir::span::SourceSpan;
    use std::sync::Arc;
    use zetaruntime::string_pool::StringPool;

    fn pool() -> Arc<StringPool> {
        Arc::new(StringPool::new().unwrap())
    }

    fn sid(s: &str, p: &StringPool) -> StrId {
        StrId(p.intern(s))
    }

    /// A zero-width source span that satisfies the lifetime but carries no
    /// meaningful position information.
    fn dummy_span<'a>() -> SourceSpan<'a> {
        SourceSpan::default()
    }

    /// Build an `AstModule` with no statements.
    fn empty_module<'a, 'bump>(
        name: StrId,
        stmts: &'bump [Stmt<'a, 'bump>],
    ) -> AstModule<'a, 'bump> {
        AstModule { name, stmts }
    }

    #[test]
    fn test_dep_graph_creation() {
        let graph = DepGraph::new();
        assert_eq!(graph.nodes().len(), 0);
    }

    #[test]
    fn test_register_and_lookup_item_node() {
        let mut graph = DepGraph::new();
        let node_idx = graph.push_node(NodeKind::Module { module_idx: 0 }, None);
        graph.register_item_node(0, 0, "test_tag", node_idx);

        let looked_up = graph.lookup_item_node(0, 0, "test_tag");
        assert_eq!(looked_up, Some(node_idx));
    }

    #[test]
    fn test_add_edge_creates_dependency() {
        let mut graph = DepGraph::new();
        let a = graph.push_node(NodeKind::Module { module_idx: 0 }, None);
        let b = graph.push_node(NodeKind::Module { module_idx: 1 }, None);
        graph.add_edge(a, b);

        assert!(graph.nodes()[a].deps.contains(&b));
        assert!(graph.nodes()[b].rev_deps.contains(&a));
    }

    #[test]
    fn test_add_edge_prevents_duplicates() {
        let mut graph = DepGraph::new();
        let a = graph.push_node(NodeKind::Module { module_idx: 0 }, None);
        let b = graph.push_node(NodeKind::Module { module_idx: 1 }, None);
        graph.add_edge(a, b);
        graph.add_edge(a, b);

        assert_eq!(graph.nodes()[a].deps.len(), 1);
    }

    #[test]
    fn test_add_edge_no_self_loop() {
        let mut graph = DepGraph::new();
        let a = graph.push_node(NodeKind::Module { module_idx: 0 }, None);
        graph.add_edge(a, a);

        assert!(graph.nodes()[a].deps.is_empty());
    }

    #[test]
    fn test_phase_a_empty_module_creates_module_node() {
        let p = pool();
        let name = sid("mymod", &p);
        let stmts: &[Stmt] = &[];
        let modules = [empty_module(name, stmts)];

        let mut graph = DepGraph::new();
        graph.build_from_ast(&modules, &p);

        assert_eq!(graph.nodes().len(), 1);
        assert!(matches!(
            graph.nodes()[0].kind,
            NodeKind::Module { module_idx: 0 }
        ));
    }

    #[test]
    fn test_phase_a_module_node_carries_name_hint() {
        let p = pool();
        let name = sid("com.example", &p);
        let stmts: &[Stmt] = &[];
        let modules = [empty_module(name, stmts)];

        let mut graph = DepGraph::new();
        graph.build_from_ast(&modules, &p);

        let hint = graph.nodes()[0]
            .hint
            .map(|s| p.resolve_string(&s).to_string())
            .unwrap_or_default();
        assert_eq!(hint, "com.example");
    }

    #[test]
    fn test_register_import() {
        let mut graph = DepGraph::new();
        graph.register_import(0, 1);
        assert!(graph.nodes().len() >= 2);
    }

    #[test]
    fn test_get_module_imports() {
        let mut graph = DepGraph::new();
        graph.register_import(0, 1);
        graph.register_import(0, 2);

        let imports = graph.get_module_imports(0);
        assert_eq!(imports.len(), 2);
        assert!(imports.contains(&1));
        assert!(imports.contains(&2));
    }

    #[test]
    fn test_get_module_importers() {
        let mut graph = DepGraph::new();
        graph.register_import(0, 1);
        graph.register_import(2, 1);

        let importers = graph.get_module_importers(1);
        assert_eq!(importers.len(), 2);
        assert!(importers.contains(&0));
        assert!(importers.contains(&2));
    }

    #[test]
    fn test_import_creates_directed_edge() {
        let mut graph = DepGraph::new();
        graph.register_import(0, 1);

        assert!(graph.get_module_imports(0).contains(&1));
        assert!(graph.get_module_importers(1).contains(&0));
    }

    #[test]
    fn test_multiple_imports_from_same_module() {
        let mut graph = DepGraph::new();
        graph.register_import(0, 1);
        graph.register_import(0, 2);
        graph.register_import(0, 3);

        let imports = graph.get_module_imports(0);
        assert_eq!(imports.len(), 3);
        assert!(imports.contains(&1));
        assert!(imports.contains(&2));
        assert!(imports.contains(&3));
    }

    #[test]
    fn test_detect_circular_imports_simple() {
        let mut graph = DepGraph::new();
        graph.register_import(0, 1);
        graph.register_import(1, 0);

        let cycles = graph.detect_circular_imports();
        assert!(!cycles.is_empty());
    }

    #[test]
    fn test_detect_circular_imports_no_cycle() {
        let mut graph = DepGraph::new();
        graph.register_import(0, 1);
        graph.register_import(1, 2);

        let cycles = graph.detect_circular_imports();
        assert!(cycles.is_empty());
    }

    #[test]
    fn test_tarjan_scc_no_cycles() {
        let mut graph = DepGraph::new();
        let n0 = graph.push_node(NodeKind::Module { module_idx: 0 }, None);
        let n1 = graph.push_node(NodeKind::Module { module_idx: 1 }, None);
        let n2 = graph.push_node(NodeKind::Module { module_idx: 2 }, None);
        graph.add_edge(n0, n1);
        graph.add_edge(n1, n2);

        let sccs = graph.tarjan_scc();
        assert_eq!(sccs.len(), 3);
    }

    #[test]
    fn test_tarjan_scc_with_cycle() {
        let mut graph = DepGraph::new();
        let n0 = graph.push_node(NodeKind::Module { module_idx: 0 }, None);
        let n1 = graph.push_node(NodeKind::Module { module_idx: 1 }, None);
        graph.add_edge(n0, n1);
        graph.add_edge(n1, n0);

        let sccs = graph.tarjan_scc();
        assert_eq!(sccs.len(), 1);
        assert_eq!(sccs[0].len(), 2);
    }

    #[test]
    fn test_scc_topo_order() {
        let mut graph = DepGraph::new();
        let n0 = graph.push_node(NodeKind::Module { module_idx: 0 }, None);
        let n1 = graph.push_node(NodeKind::Module { module_idx: 1 }, None);
        let n2 = graph.push_node(NodeKind::Module { module_idx: 2 }, None);
        graph.add_edge(n0, n1);
        graph.add_edge(n1, n2);

        let topo = graph.scc_topo_order();
        assert_eq!(topo.len(), 3);
    }

    #[test]
    fn test_register_package_sets_hierarchy() {
        let p = pool();
        let mut graph = DepGraph::new();

        let mod_node = graph.push_node(NodeKind::Module { module_idx: 0 }, None);
        graph.register_item_node(0, 0, "module", mod_node);

        let segs = vec![sid("com", &p), sid("example", &p), sid("app", &p)];
        let path_strid = sid("com::example::app", &p);
        graph.register_package(0, segs, path_strid);

        assert_eq!(graph.get_module_package(0), Some(path_strid));
    }

    #[test]
    fn test_import_resolved_by_path_index() {
        let p = pool();

        let io_name = sid("io", &p);
        let main_name = sid("main", &p);
        let io_seg = sid("io", &p);

        let io_path_segs: &[StrId] = &[io_seg];
        let io_path = Path {
            path: io_path_segs,
            member: None,
            span: dummy_span(),
        };
        let io_pkg_stmt = Box::leak(Box::new(ir::ast::PackageStmt {
            path: Box::leak(Box::new(io_path)),
            span: dummy_span(),
        }));

        let import_path_segs: &[StrId] = &[io_seg];
        let import_path = Path {
            path: import_path_segs,
            member: None,
            span: dummy_span(),
        };
        let import_stmt_inner = Box::leak(Box::new(ir::ast::ImportStmt {
            path: Box::leak(Box::new(import_path)),
            span: dummy_span(),
        }));

        let io_stmts: &[Stmt] = &[Stmt::Package(io_pkg_stmt)];
        let main_stmts: &[Stmt] = &[Stmt::Import(import_stmt_inner)];

        let modules = [
            AstModule {
                name: io_name,
                stmts: io_stmts,
            },
            AstModule {
                name: main_name,
                stmts: main_stmts,
            },
        ];

        let mut graph = DepGraph::new();
        graph.build_from_ast(&modules, &p);

        assert!(
            graph.unresolved_imports.is_empty(),
            "import of `io` should resolve via path index"
        );
        // main (module 1) should import io (module 0).
        assert!(
            graph.get_module_imports(1).contains(&0),
            "main must import io"
        );
    }

    #[test]
    fn test_unresolved_import_recorded() {
        let p = pool();
        let main_name = sid("main", &p);
        let unknown_seg = sid("unknown_pkg", &p);

        let import_path_segs: &[StrId] = &[unknown_seg];
        let import_path = Path {
            path: import_path_segs,
            member: None,
            span: dummy_span(),
        };
        let import_stmt_inner = Box::leak(Box::new(ir::ast::ImportStmt {
            path: Box::leak(Box::new(import_path)),
            span: dummy_span(),
        }));

        let main_stmts: &[Stmt] = &[Stmt::Import(import_stmt_inner)];
        let modules = [AstModule {
            name: main_name,
            stmts: main_stmts,
        }];

        let mut graph = DepGraph::new();
        graph.build_from_ast(&modules, &p);

        assert_eq!(graph.unresolved_imports.len(), 1);
        assert_eq!(graph.unresolved_imports[0].from_module_idx, 0);
        assert_eq!(graph.unresolved_imports[0].path, vec![unknown_seg]);
    }

    #[test]
    fn test_link_stdlib_to_user() {
        let mut graph = DepGraph::new();
        let _stdlib = graph.push_node(NodeKind::Module { module_idx: 0 }, None);
        let _user1 = graph.push_node(NodeKind::Module { module_idx: 1 }, None);
        let _user2 = graph.push_node(NodeKind::Module { module_idx: 2 }, None);

        graph.link_stdlib_to_user(0, &[1, 2]);

        assert!(graph.get_module_imports(1).contains(&0));
        assert!(graph.get_module_imports(2).contains(&0));
    }

    #[test]
    fn test_build_symbol_table() {
        let p = pool();
        let mut graph = DepGraph::new();

        let node = graph.push_node(
            NodeKind::TypeDecl {
                module_idx: 0,
                item_idx: 0,
            },
            Some(sid("MyType", &p)),
        );
        graph.register_item_node(0, 0, "type", node);

        let table: HashMap<(StrId, usize), (usize, usize, &str)> = graph.build_symbol_table();
        assert!(table.contains_key(&(sid("MyType", &p), 0)));
    }

    #[test]
    fn test_resolve_name_in_current_module() {
        let p = pool();
        let mut graph = DepGraph::new();

        let node = graph.push_node(
            NodeKind::TypeDecl {
                module_idx: 0,
                item_idx: 0,
            },
            Some(sid("MyType", &p)),
        );
        graph.register_item_node(0, 0, "type", node);

        let table = graph.build_symbol_table();
        let resolved = graph.resolve_name(sid("MyType", &p), 0, &table);
        assert_eq!(resolved, Some(node));
    }

    #[test]
    fn test_resolve_name_in_imported_module() {
        let p = pool();
        let mut graph = DepGraph::new();

        let type_node = graph.push_node(
            NodeKind::TypeDecl {
                module_idx: 1,
                item_idx: 0,
            },
            Some(sid("MyType", &p)),
        );
        graph.register_item_node(1, 0, "type", type_node);
        graph.register_import(0, 1);

        let table = graph.build_symbol_table();
        let resolved = graph.resolve_name(sid("MyType", &p), 0, &table);
        assert_eq!(resolved, Some(type_node));
    }

    #[test]
    fn test_can_access_same_module() {
        let graph = DepGraph::new();
        assert!(graph.can_access(0, 0, 0));
    }

    #[test]
    fn test_can_access_imported_module() {
        let mut graph = DepGraph::new();
        graph.register_import(0, 1);
        assert!(graph.can_access(0, 1, 0));
    }

    #[test]
    fn test_cannot_access_non_imported_module() {
        let mut graph = DepGraph::new();
        graph.register_import(0, 1);
        assert!(!graph.can_access(0, 2, 0));
    }

    #[test]
    fn test_can_access_respects_import_chain() {
        let mut graph = DepGraph::new();
        graph.register_import(0, 1);

        assert!(graph.can_access(0, 0, 0), "same module always accessible");
        assert!(graph.can_access(0, 1, 0), "imported module accessible");
        assert!(
            !graph.can_access(0, 2, 0),
            "non-imported module not accessible"
        );
    }

    #[test]
    fn test_get_module_compilation_order() {
        let mut graph = DepGraph::new();
        graph.register_import(0, 1);
        graph.register_import(1, 2);

        let order = graph.get_module_compilation_order();
        assert_eq!(order.len(), 3);

        let pos = |m: usize| order.iter().position(|&x| x == m).unwrap();
        assert!(pos(2) < pos(1));
        assert!(pos(1) < pos(0));
    }

    #[test]
    fn test_module_compilation_order_respects_imports() {
        let mut graph = DepGraph::new();
        graph.register_import(2, 1);
        graph.register_import(1, 0);

        let order = graph.get_module_compilation_order();
        assert!(order.contains(&0));
        assert!(order.contains(&1));
        assert!(order.contains(&2));

        let pos = |m: usize| order.iter().position(|&x| x == m).unwrap();
        assert!(pos(0) < pos(1), "module 0 must compile before module 1");
        assert!(pos(1) < pos(2), "module 1 must compile before module 2");
    }

    #[test]
    fn test_get_compilation_order_functions() {
        let mut graph = DepGraph::new();
        let f0 = graph.push_node(
            NodeKind::FuncBody {
                module_idx: 0,
                item_idx: 0,
            },
            None,
        );
        let f1 = graph.push_node(
            NodeKind::FuncBody {
                module_idx: 0,
                item_idx: 1,
            },
            None,
        );
        let f2 = graph.push_node(
            NodeKind::FuncBody {
                module_idx: 0,
                item_idx: 2,
            },
            None,
        );
        graph.register_item_node(0, 0, "func_body", f0);
        graph.register_item_node(0, 1, "func_body", f1);
        graph.register_item_node(0, 2, "func_body", f2);
        graph.add_edge(f0, f1);
        graph.add_edge(f1, f2);

        let order = graph.get_compilation_order();
        assert!(!order.is_empty());
    }

    #[test]
    fn test_get_function_callers() {
        let mut graph = DepGraph::new();
        let caller_body = graph.push_node(
            NodeKind::FuncBody {
                module_idx: 0,
                item_idx: 0,
            },
            None,
        );
        let callee_body = graph.push_node(
            NodeKind::FuncBody {
                module_idx: 1,
                item_idx: 0,
            },
            None,
        );
        graph.register_item_node(0, 0, "func_body", caller_body);
        graph.register_item_node(1, 0, "func_body", callee_body);
        graph.add_edge(caller_body, callee_body);

        let callers = graph.get_function_callers(1, 0);
        assert_eq!(callers.len(), 1);
        assert_eq!(callers[0], (0, 0));
    }

    #[test]
    fn test_get_function_callees() {
        let mut graph = DepGraph::new();
        let caller_body = graph.push_node(
            NodeKind::FuncBody {
                module_idx: 0,
                item_idx: 0,
            },
            None,
        );
        let callee_body = graph.push_node(
            NodeKind::FuncBody {
                module_idx: 1,
                item_idx: 0,
            },
            None,
        );
        graph.register_item_node(0, 0, "func_body", caller_body);
        graph.register_item_node(1, 0, "func_body", callee_body);
        graph.add_edge(caller_body, callee_body);

        let callees = graph.get_function_callees(0, 0);
        assert_eq!(callees.len(), 1);
        assert_eq!(callees[0], (1, 0));
    }

    #[test]
    fn test_get_function_type_deps() {
        let mut graph = DepGraph::new();
        let func_sig = graph.push_node(
            NodeKind::FuncSig {
                module_idx: 0,
                item_idx: 0,
            },
            None,
        );
        let type_decl = graph.push_node(
            NodeKind::TypeDecl {
                module_idx: 1,
                item_idx: 0,
            },
            None,
        );
        graph.register_item_node(0, 0, "func_sig", func_sig);
        graph.register_item_node(1, 0, "type", type_decl);
        graph.add_edge(func_sig, type_decl);

        let deps = graph.get_function_type_deps(0, 0);
        assert_eq!(deps.len(), 1);
        assert_eq!(deps[0], (1, 0));
    }

    #[test]
    fn test_detect_recursive_cycles() {
        let mut graph = DepGraph::new();
        let fa = graph.push_node(
            NodeKind::FuncBody {
                module_idx: 0,
                item_idx: 0,
            },
            None,
        );
        let fb = graph.push_node(
            NodeKind::FuncBody {
                module_idx: 0,
                item_idx: 1,
            },
            None,
        );
        graph.register_item_node(0, 0, "func_body", fa);
        graph.register_item_node(0, 1, "func_body", fb);
        graph.add_edge(fa, fb);
        graph.add_edge(fb, fa);

        let cycles = graph.detect_recursive_cycles();
        assert!(!cycles.is_empty());
    }

    #[test]
    fn test_debug_node() {
        let p = pool();
        let mut graph = DepGraph::new();
        let node = graph.push_node(
            NodeKind::Module { module_idx: 0 },
            Some(sid("test_module", &p)),
        );
        let s = graph.debug_node(node, &p);
        assert!(s.contains("Module"));
        assert!(s.contains("test_module"));
    }

    fn default_modifiers() -> FuncModifiers {
        FuncModifiers {
            visibility: Visibility::Public,
            extern_modifier: ExternModifier::None,
            inline_modifier: InlineModifier::None,
            func_safety: FuncSafety::Safe,
        }
    }

    #[test]
    fn test_module_builder_collects_func_and_struct() {
        let p = pool();

        let func_name = sid("my_func", &p);
        let struct_name = sid("MyStruct", &p);

        let func_decl = Box::leak(Box::new(FuncDecl {
            function_metadata: default_modifiers(),
            name: func_name,
            generics: None,
            params: None,
            return_type: None,
            body: None,
            span: dummy_span(),
        }));
        let struct_decl = Box::leak(Box::new(StructDecl {
            visibility: Visibility::Public,
            name: struct_name,
            generics: None,
            params: None,
            span: dummy_span(),
        }));

        let stmts: &[Stmt] = &[Stmt::FuncDecl(func_decl), Stmt::StructDecl(struct_decl)];
        let modules = [AstModule {
            name: sid("mymod", &p),
            stmts,
        }];

        let result = ModuleBuilder::run(&modules, p.clone());

        assert_eq!(result.modules.names.len(), 1, "one module");
        assert_eq!(result.modules.symbols[0].names.len(), 2, "two symbols");

        let kinds: Vec<String> = result.modules.symbols[0]
            .kinds
            .iter()
            .map(|k| p.resolve_string(k).to_string())
            .collect();
        assert!(
            kinds.iter().any(|k| k == "function"),
            "should contain function"
        );
        assert!(kinds.iter().any(|k| k == "struct"), "should contain struct");
    }

    #[test]
    fn test_module_builder_collects_interface_and_enum() {
        let p = pool();

        let iface_name = sid("Drawable", &p);
        let enum_name = sid("Color", &p);

        let iface_decl = Box::leak(Box::new(InterfaceDecl {
            name: iface_name,
            visibility: Visibility::Public,
            sealed: false,
            permits: None,
            methods: None,
            generics: None,
            span: dummy_span(),
        }));
        let enum_decl = Box::leak(Box::new(EnumDecl {
            name: enum_name,
            visibility: Visibility::Public,
            generics: None,
            variants: &[],
            span: dummy_span(),
        }));

        let stmts: &[Stmt] = &[Stmt::InterfaceDecl(iface_decl), Stmt::EnumDecl(enum_decl)];
        let modules = [AstModule {
            name: sid("shapes", &p),
            stmts,
        }];

        let result = ModuleBuilder::run(&modules, p.clone());

        let kinds: Vec<String> = result.modules.symbols[0]
            .kinds
            .iter()
            .map(|k| p.resolve_string(k).to_string())
            .collect();
        assert!(
            kinds.iter().any(|k| k == "interface"),
            "should recognise interface"
        );
        assert!(kinds.iter().any(|k| k == "enum"), "should recognise enum");
    }

    #[test]
    fn test_module_builder_collects_const_and_impl() {
        let p = pool();

        let const_name = sid("MAX", &p);
        let impl_target = sid("MyStruct", &p);

        let i32_ty = Type {
            kind: TypeKind::I32,
            nullable: false,
        };
        let value_expr = Box::leak(Box::new(ir::ast::Expr::Number {
            value: 42,
            span: dummy_span(),
        }));

        let const_stmt = Box::leak(Box::new(ConstStmt {
            ident: const_name,
            type_annotation: i32_ty,
            value: value_expr,
            span: dummy_span(),
        }));
        let impl_decl = Box::leak(Box::new(ImplDecl {
            generics: None,
            interface: None,
            target: impl_target,
            methods: None,
            constants: None,
            span: dummy_span(),
        }));

        let stmts: &[Stmt] = &[Stmt::Const(const_stmt), Stmt::ImplDecl(impl_decl)];
        let modules = [AstModule {
            name: sid("mymod", &p),
            stmts,
        }];

        let result = ModuleBuilder::run(&modules, p.clone());

        let kinds: Vec<String> = result.modules.symbols[0]
            .kinds
            .iter()
            .map(|k| p.resolve_string(k).to_string())
            .collect();
        assert!(kinds.iter().any(|k| k == "const"), "should recognise const");
        assert!(kinds.iter().any(|k| k == "impl"), "should recognise impl");
    }

    #[test]
    fn test_module_builder_anonymous_stmts_get_anon_kind() {
        let p = pool();

        let i32_ty = Type {
            kind: TypeKind::I32,
            nullable: false,
        };
        let value_expr = Box::leak(Box::new(ir::ast::Expr::Number {
            value: 0,
            span: dummy_span(),
        }));
        let let_stmt = Box::leak(Box::new(LetStmt {
            ident: sid("x", &p),
            type_annotation: i32_ty,
            value: value_expr,
            mutable: false,
            catch_pattern: None,
            else_block: None,
            is_static: false,
            span: dummy_span(),
        }));

        let stmts: &[Stmt] = &[Stmt::Let(let_stmt)];
        let modules = [AstModule {
            name: sid("anon_mod", &p),
            stmts,
        }];

        let result = ModuleBuilder::run(&modules, p.clone());

        let kinds: Vec<String> = result.modules.symbols[0]
            .kinds
            .iter()
            .map(|k| p.resolve_string(k).to_string())
            .collect();
        assert!(
            kinds.iter().any(|k| k == "unknown"),
            "bare let should be unknown"
        );
    }
}
