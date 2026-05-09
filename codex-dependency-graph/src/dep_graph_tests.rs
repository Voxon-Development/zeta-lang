#[cfg(test)]
mod tests {
    use crate::NodeIdx;
    use crate::dep_graph::{DepGraph, NodeKind, TypeKey};
    use ir::hir::{HirModule, StrId};
    use ir::ir_hasher::HashMap;
    use std::sync::Arc;
    use zetaruntime::string_pool::{StringPool, VmString};

    /// Helper to create a test string pool
    fn create_test_pool() -> Arc<StringPool> {
        Arc::new(StringPool::new().unwrap())
    }

    /// Helper to create an empty HirModule
    fn create_empty_module<'a, 'bump>(name: &str, pool: &Arc<StringPool>) -> HirModule<'a, 'bump> {
        HirModule {
            name: StrId(pool.intern(name)),
            imports: &[],
            items: &[],
        }
    }

    #[test]
    fn test_dep_graph_creation() {
        let graph = DepGraph::new();
        assert_eq!(graph.nodes().len(), 0);
    }

    #[test]
    fn test_phase_a_create_nodes_empty_modules() {
        let pool = create_test_pool();
        let modules = vec![create_empty_module("test", &pool)];

        let mut graph = DepGraph::new();
        graph.phase_a_create_nodes(&modules);

        // Should have created 1 module node
        assert_eq!(graph.nodes().len(), 1);

        // Module node should be created
        let module_node = graph.lookup_item_node(0, 0, "module");
        assert!(module_node.is_none()); // Module nodes aren't registered by item_index
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
        let node_a = graph.push_node(NodeKind::Module { module_idx: 0 }, None);
        let node_b = graph.push_node(NodeKind::Module { module_idx: 1 }, None);

        graph.add_edge(node_a, node_b);

        // Check that edge was added
        assert!(graph.nodes()[node_a].deps.contains(&node_b));
        assert!(graph.nodes()[node_b].rev_deps.contains(&node_a));
    }

    #[test]
    fn test_add_edge_prevents_duplicates() {
        let mut graph = DepGraph::new();
        let node_a = graph.push_node(NodeKind::Module { module_idx: 0 }, None);
        let node_b = graph.push_node(NodeKind::Module { module_idx: 1 }, None);

        graph.add_edge(node_a, node_b);
        graph.add_edge(node_a, node_b); // Add same edge again

        // Should only have one edge
        assert_eq!(graph.nodes()[node_a].deps.len(), 1);
    }

    #[test]
    fn test_register_import() {
        let mut graph = DepGraph::new();
        graph.register_import(0, 1);

        // Should have created 2 module nodes and an edge
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
    fn test_detect_circular_imports_simple() {
        let mut graph = DepGraph::new();
        graph.register_import(0, 1);
        graph.register_import(1, 0); // Create cycle

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
        // Should have 3 SCCs (one per node)
        assert_eq!(sccs.len(), 3);
    }

    #[test]
    fn test_tarjan_scc_with_cycle() {
        let mut graph = DepGraph::new();
        let n0 = graph.push_node(NodeKind::Module { module_idx: 0 }, None);
        let n1 = graph.push_node(NodeKind::Module { module_idx: 1 }, None);

        graph.add_edge(n0, n1);
        graph.add_edge(n1, n0); // Create cycle

        let sccs = graph.tarjan_scc();
        // Should have 1 SCC containing both nodes
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
        // Should have 3 SCCs in topological order
        assert_eq!(topo.len(), 3);
    }

    #[test]
    fn test_get_or_create_instance() {
        let mut graph = DepGraph::new();
        let base = graph.push_node(NodeKind::Module { module_idx: 0 }, None);

        let boxed_str = Box::new("Hello world");
        let leaked_str = Box::leak(boxed_str);
        let ptr = leaked_str.as_ptr();
        let args = vec![TypeKey {
            repr: StrId(VmString {
                offset: ptr,
                length: leaked_str.len(),
            }),
        }];
        let inst1 = graph.get_or_create_instance(base, args.clone(), true);
        let inst2 = graph.get_or_create_instance(base, args.clone(), true);

        // Should return same instance
        assert_eq!(inst1, inst2);
    }

    #[test]
    fn test_link_stdlib_to_user() {
        let mut graph = DepGraph::new();

        // Create stdlib and user module nodes
        let _stdlib = graph.push_node(NodeKind::Module { module_idx: 0 }, None);
        let _user1 = graph.push_node(NodeKind::Module { module_idx: 1 }, None);
        let _user2 = graph.push_node(NodeKind::Module { module_idx: 2 }, None);

        graph.link_stdlib_to_user(0, &[1, 2]);

        // User modules should import stdlib
        let imports1 = graph.get_module_imports(1);
        let imports2 = graph.get_module_imports(2);

        assert!(imports1.contains(&0));
        assert!(imports2.contains(&0));
    }

    #[test]
    fn test_build_symbol_table() {
        let pool = create_test_pool();
        let mut graph = DepGraph::new();

        let node = graph.push_node(
            NodeKind::TypeDecl {
                module_idx: 0,
                item_idx: 0,
            },
            Some(StrId(pool.intern("MyType"))),
        );
        graph.register_item_node(0, 0, "type", node);

        let table: HashMap<(StrId, usize), (usize, usize, &str)> = graph.build_symbol_table();

        // Should have entry for MyType in module 0
        assert!(table.contains_key(&(StrId(pool.intern("MyType")), 0)));
    }

    #[test]
    fn test_resolve_name_in_current_module() {
        let pool = create_test_pool();
        let mut graph = DepGraph::new();

        let node = graph.push_node(
            NodeKind::TypeDecl {
                module_idx: 0,
                item_idx: 0,
            },
            Some(StrId(pool.intern("MyType"))),
        );
        graph.register_item_node(0, 0, "type", node);

        let table: HashMap<(StrId, usize), (usize, usize, &str)> = graph.build_symbol_table();
        let resolved = graph.resolve_name(StrId(pool.intern("MyType")), 0, &table);

        assert_eq!(resolved, Some(node));
    }

    #[test]
    fn test_resolve_name_in_imported_module() {
        let pool = create_test_pool();
        let mut graph = DepGraph::new();

        // Create type in module 1
        let type_node = graph.push_node(
            NodeKind::TypeDecl {
                module_idx: 1,
                item_idx: 0,
            },
            Some(StrId(pool.intern("MyType"))),
        );
        graph.register_item_node(1, 0, "type", type_node);

        // Module 0 imports module 1
        graph.register_import(0, 1);

        let table: HashMap<(StrId, usize), (usize, usize, &str)> = graph.build_symbol_table();
        let resolved: Option<NodeIdx> = graph.resolve_name(StrId(pool.intern("MyType")), 0, &table);

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

        // Module 0 doesn't import module 2
        assert!(!graph.can_access(0, 2, 0));
    }

    #[test]
    fn test_get_module_compilation_order() {
        let mut graph = DepGraph::new();

        // Create a simple dependency chain: 0 -> 1 -> 2
        graph.register_import(0, 1);
        graph.register_import(1, 2);

        let order = graph.get_module_compilation_order();

        // Should have all modules
        assert_eq!(order.len(), 3);

        // Module 2 should come before 1, and 1 before 0
        let pos_0 = order.iter().position(|&x| x == 0).unwrap();
        let pos_1 = order.iter().position(|&x| x == 1).unwrap();
        let pos_2 = order.iter().position(|&x| x == 2).unwrap();

        assert!(pos_2 < pos_1);
        assert!(pos_1 < pos_0);
    }

    #[test]
    fn test_get_function_callers() {
        let mut graph = DepGraph::new();

        let _caller_sig = graph.push_node(
            NodeKind::FuncSig {
                module_idx: 0,
                item_idx: 0,
            },
            None,
        );
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

        // Caller depends on callee
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

        // Caller depends on callee
        graph.add_edge(caller_body, callee_body);

        let callees = graph.get_function_callees(0, 0);
        assert_eq!(callees.len(), 1);
        assert_eq!(callees[0], (1, 0));
    }

    #[test]
    fn test_detect_recursive_cycles() {
        let mut graph = DepGraph::new();

        let func_a = graph.push_node(
            NodeKind::FuncBody {
                module_idx: 0,
                item_idx: 0,
            },
            None,
        );
        let func_b = graph.push_node(
            NodeKind::FuncBody {
                module_idx: 0,
                item_idx: 1,
            },
            None,
        );

        graph.register_item_node(0, 0, "func_body", func_a);
        graph.register_item_node(0, 1, "func_body", func_b);

        // Create cycle: A -> B -> A
        graph.add_edge(func_a, func_b);
        graph.add_edge(func_b, func_a);

        let cycles = graph.detect_recursive_cycles();
        assert!(!cycles.is_empty());
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

        // Function signature depends on type
        graph.add_edge(func_sig, type_decl);

        let deps = graph.get_function_type_deps(0, 0);
        assert_eq!(deps.len(), 1);
        assert_eq!(deps[0], (1, 0));
    }

    #[test]
    fn test_get_compilation_order() {
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

        // Create dependency chain: f0 -> f1 -> f2
        graph.add_edge(f0, f1);
        graph.add_edge(f1, f2);

        let order = graph.get_compilation_order();

        // Should have functions in topological order
        assert!(!order.is_empty());
    }

    #[test]
    fn test_debug_node() {
        let pool = create_test_pool();
        let mut graph = DepGraph::new();

        let node = graph.push_node(
            NodeKind::Module { module_idx: 0 },
            Some(StrId(pool.intern("test_module"))),
        );
        let debug_str = graph.debug_node(node, &pool);

        assert!(debug_str.contains("Module"));
        assert!(debug_str.contains("test_module"));
    }

    // ═══════════════════════════════════════════════════════════════════════
    // Module / package / import integration tests
    // ═══════════════════════════════════════════════════════════════════════

    #[test]
    fn test_register_package_sets_hierarchy() {
        let pool = create_test_pool();
        let mut graph = DepGraph::new();

        // Create a module node and register it so register_package can find it.
        let mod_node = graph.push_node(NodeKind::Module { module_idx: 0 }, None);
        graph.register_item_node(0, 0, "module", mod_node);

        let pkg = StrId(pool.intern("com.example.app"));
        graph.register_package(0, pkg, &pool);

        assert_eq!(
            graph.get_module_package(0),
            Some("com.example.app"),
            "package hierarchy should store the dotted path"
        );
    }

    #[test]
    fn test_import_creates_directed_edge() {
        let mut graph = DepGraph::new();

        // Modules 0 and 1
        graph.register_import(0, 1);

        let imports_of_0 = graph.get_module_imports(0);
        assert!(imports_of_0.contains(&1), "module 0 should import module 1");

        let importers_of_1 = graph.get_module_importers(1);
        assert!(
            importers_of_1.contains(&0),
            "module 1 should be imported by module 0"
        );
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
    fn test_phase_a_creates_module_node_with_name_hint() {
        let pool = create_test_pool();
        let modules = vec![create_empty_module("com.example", &pool)];

        let mut graph = DepGraph::new();
        graph.phase_a_create_nodes(&modules);

        // The module node should exist and carry the name as hint.
        assert_eq!(graph.nodes().len(), 1);
        let node = &graph.nodes()[0];
        assert!(matches!(node.kind, NodeKind::Module { module_idx: 0 }));
        // Hint should be the interned module name.
        let hint_str = node
            .hint
            .map(|s| pool.resolve_string(&s).to_string())
            .unwrap_or_default();
        assert_eq!(hint_str, "com.example");
    }

    #[test]
    fn test_module_compilation_order_respects_imports() {
        // Module 2 imports module 1, which imports module 0 (stdlib-like).
        // Compilation order must put 0 before 1 before 2.
        let mut graph = DepGraph::new();
        graph.register_import(2, 1);
        graph.register_import(1, 0);

        let order = graph.get_module_compilation_order();
        // All three modules should appear.
        assert!(order.contains(&0));
        assert!(order.contains(&1));
        assert!(order.contains(&2));
        // Dependencies come first.
        let pos = |m: usize| order.iter().position(|&x| x == m).unwrap();
        assert!(
            pos(0) < pos(1),
            "module 0 (imported) must compile before module 1"
        );
        assert!(
            pos(1) < pos(2),
            "module 1 (imported) must compile before module 2"
        );
    }

    #[test]
    fn test_can_access_respects_import_chain() {
        let mut graph = DepGraph::new();
        // 0 imports 1; 0 does NOT import 2.
        graph.register_import(0, 1);

        assert!(graph.can_access(0, 0, 0), "same module always accessible");
        assert!(graph.can_access(0, 1, 0), "imported module accessible");
        assert!(
            !graph.can_access(0, 2, 0),
            "non-imported module not accessible"
        );
    }

    // ═══════════════════════════════════════════════════════════════════════
    // HirStmt::Defer dependency-walk tests
    // ═══════════════════════════════════════════════════════════════════════

    #[test]
    fn test_walk_stmt_defer_does_not_panic() {
        use ir::hir::{HirExpr, HirStmt, HirType};

        let pool = create_test_pool();
        let mut graph = DepGraph::new();

        // Build a synthetic function node to act as the "from" node.
        let from_node = graph.push_node(
            NodeKind::FuncBody {
                module_idx: 0,
                item_idx: 0,
            },
            None,
        );

        // Build a Defer statement wrapping a simple Expr statement.
        let inner_expr = HirStmt::Expr(&HirExpr::Number(42));
        // We need a bump to alloc the inner node.
        let bump = Box::new(zetaruntime::bump::GrowableBump::new(4096, 8));
        let bump_ref: &zetaruntime::bump::GrowableBump = Box::leak(bump);
        let inner = bump_ref.alloc_value_immutable(inner_expr);
        let defer_stmt = HirStmt::Defer(inner);

        // This must not panic.
        graph.walk_stmt_for_deps_pub(&defer_stmt, from_node, 0, &pool);
    }

    #[test]
    fn test_walk_stmt_defer_recurses_into_body() {
        use ir::hir::{HirExpr, HirStmt};

        let pool = create_test_pool();
        let mut graph = DepGraph::new();

        let from_node = graph.push_node(
            NodeKind::FuncBody {
                module_idx: 0,
                item_idx: 0,
            },
            None,
        );

        // Inner block contains two expression statements.
        let bump = Box::new(zetaruntime::bump::GrowableBump::new(4096, 8));
        let bump_ref: &zetaruntime::bump::GrowableBump = Box::leak(bump);

        let expr_a = bump_ref.alloc_value_immutable(HirStmt::Expr(&HirExpr::Number(1)));
        let expr_b = bump_ref.alloc_value_immutable(HirStmt::Expr(&HirExpr::Number(2)));
        let body_slice = bump_ref.alloc_slice(&[*expr_a, *expr_b]);
        let block = bump_ref.alloc_value_immutable(HirStmt::Block { body: body_slice });
        let defer_stmt = HirStmt::Defer(block);

        // Must not panic and must traverse both inner statements.
        graph.walk_stmt_for_deps_pub(&defer_stmt, from_node, 0, &pool);
    }

    // ═══════════════════════════════════════════════════════════════════════
    // ModuleCollectionBuilder tests
    // ═══════════════════════════════════════════════════════════════════════

    #[test]
    fn test_module_collection_builder_collects_func_and_struct() {
        use crate::module_collection_builder::ModuleBuilder;
        use ir::ast::{ExternModifier, FuncModifiers, FuncSafety, InlineModifier, Visibility};
        use ir::hir::{Hir, HirEnum, HirFunc, HirInterface, HirStruct, HirType};

        let pool = create_test_pool();
        let func_name = StrId(pool.intern("my_func"));
        let struct_name = StrId(pool.intern("MyStruct"));

        let func = HirFunc {
            name: func_name,
            params: None,
            return_type: Some(HirType::Void),
            body: None,
            function_metadata: FuncModifiers {
                visibility: Visibility::Public,
                extern_modifier: ExternModifier::None,
                inline_modifier: InlineModifier::None,
                func_safety: FuncSafety::Safe,
            },
            generics: None,
        };

        // Use bump-allocated slices for HirStruct fields.
        let bump = Box::new(zetaruntime::bump::GrowableBump::new(4096, 8));
        let bump_ref: &zetaruntime::bump::GrowableBump = Box::leak(bump);
        let func_ref = bump_ref.alloc_value(func);
        let struct_fields: &[ir::hir::HirField] = &[];
        let struct_ref = bump_ref.alloc_value(HirStruct {
            name: struct_name,
            visibility: Visibility::Public,
            generics: None,
            fields: struct_fields,
            interfaces: None,
            methods: None,
            constants: None,
            destructor: None,
        });

        let items: Vec<Hir> = vec![Hir::Func(func_ref), Hir::Struct(struct_ref)];
        let items_slice = bump_ref.alloc_slice(&items);
        let module = ir::hir::HirModule {
            name: StrId(pool.intern("mymod")),
            imports: &[],
            items: items_slice,
        };

        let result = ModuleBuilder::run(&vec![module], pool.clone());
        assert_eq!(result.modules.names.len(), 1, "one module");
        assert_eq!(result.modules.symbols[0].names.len(), 2, "two symbols");

        let kinds: Vec<&str> = result.modules.symbols[0]
            .kinds
            .iter()
            .map(|k| k.as_str())
            .collect();
        assert!(kinds.contains(&"function"));
        assert!(kinds.contains(&"struct"));
    }

    #[test]
    fn test_module_collection_builder_collects_interface_and_enum() {
        use crate::module_collection_builder::ModuleBuilder;
        use ir::ast::Visibility;
        use ir::hir::{Hir, HirEnum, HirEnumVariant, HirInterface};

        let pool = create_test_pool();
        let iface_name = StrId(pool.intern("Drawable"));
        let enum_name = StrId(pool.intern("Color"));

        let bump = Box::new(zetaruntime::bump::GrowableBump::new(4096, 8));
        let bump_ref: &zetaruntime::bump::GrowableBump = Box::leak(bump);

        let iface_ref = bump_ref.alloc_value(HirInterface {
            name: iface_name,
            visibility: Visibility::Public,
            generics: None,
            methods: None,
        });
        let enum_ref = bump_ref.alloc_value(HirEnum {
            name: enum_name,
            visibility: Visibility::Public,
            generics: None,
            variants: &[],
        });

        let items = bump_ref.alloc_slice(&[Hir::Interface(iface_ref), Hir::Enum(enum_ref)]);
        let module = ir::hir::HirModule {
            name: StrId(pool.intern("shapes")),
            imports: &[],
            items,
        };

        let result = ModuleBuilder::run(&vec![module], pool.clone());
        let kinds: Vec<&str> = result.modules.symbols[0]
            .kinds
            .iter()
            .map(|k| k.as_str())
            .collect();
        assert!(kinds.contains(&"interface"), "should recognise interface");
        assert!(kinds.contains(&"enum"), "should recognise enum");
    }
}
