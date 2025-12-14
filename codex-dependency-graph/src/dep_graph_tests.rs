#[cfg(test)]
mod tests {
    use crate::dep_graph::{DepGraph, NodeKind, TypeKey};
    use ir::hir::{HirModule, StrId};
    use zetaruntime::string_pool::{StringPool, VmString};
    use std::sync::Arc;
    use ir::ir_hasher::HashMap;
    use crate::NodeIdx;

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
        let args = vec![TypeKey { repr: StrId(VmString { offset: ptr, length: leaked_str.len() }) }];
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
        
        let node = graph.push_node(NodeKind::TypeDecl { module_idx: 0, item_idx: 0 }, Some(StrId(pool.intern("MyType"))));
        graph.register_item_node(0, 0, "type", node);
        
        let table: HashMap<(StrId, usize), (usize, usize, &str)> = graph.build_symbol_table();
        
        // Should have entry for MyType in module 0
        assert!(table.contains_key(&(StrId(pool.intern("MyType")), 0)));
    }

    #[test]
    fn test_resolve_name_in_current_module() {
        let pool = create_test_pool();
        let mut graph = DepGraph::new();
        
        let node = graph.push_node(NodeKind::TypeDecl { module_idx: 0, item_idx: 0 }, Some(StrId(pool.intern("MyType"))));
        graph.register_item_node(0, 0, "type", node);
        
        let table: HashMap<(StrId, usize), (usize, usize, &str)> = graph.build_symbol_table();
        let resolved = graph.resolve_name(StrId(pool.intern("MyTyped")), 0, &table);
        
        assert_eq!(resolved, Some(node));
    }

    #[test]
    fn test_resolve_name_in_imported_module() {
        let pool = create_test_pool();
        let mut graph = DepGraph::new();
        
        // Create type in module 1
        let type_node = graph.push_node(NodeKind::TypeDecl { module_idx: 1, item_idx: 0 }, Some(StrId(pool.intern("MyType"))));
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
        
        let _caller_sig = graph.push_node(NodeKind::FuncSig { module_idx: 0, item_idx: 0 }, None);
        let caller_body = graph.push_node(NodeKind::FuncBody { module_idx: 0, item_idx: 0 }, None);
        let callee_body = graph.push_node(NodeKind::FuncBody { module_idx: 1, item_idx: 0 }, None);
        
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
        
        let caller_body = graph.push_node(NodeKind::FuncBody { module_idx: 0, item_idx: 0 }, None);
        let callee_body = graph.push_node(NodeKind::FuncBody { module_idx: 1, item_idx: 0 }, None);
        
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
        
        let func_a = graph.push_node(NodeKind::FuncBody { module_idx: 0, item_idx: 0 }, None);
        let func_b = graph.push_node(NodeKind::FuncBody { module_idx: 0, item_idx: 1 }, None);
        
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
        
        let func_sig = graph.push_node(NodeKind::FuncSig { module_idx: 0, item_idx: 0 }, None);
        let type_decl = graph.push_node(NodeKind::TypeDecl { module_idx: 1, item_idx: 0 }, None);
        
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
        
        let f0 = graph.push_node(NodeKind::FuncBody { module_idx: 0, item_idx: 0 }, None);
        let f1 = graph.push_node(NodeKind::FuncBody { module_idx: 0, item_idx: 1 }, None);
        let f2 = graph.push_node(NodeKind::FuncBody { module_idx: 0, item_idx: 2 }, None);
        
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
        
        let node = graph.push_node(NodeKind::Module { module_idx: 0 }, Some(StrId(pool.intern("test_module"))));
        let debug_str = graph.debug_node(node, &pool);
        
        assert!(debug_str.contains("Module"));
        assert!(debug_str.contains("test_module"));
    }
}
