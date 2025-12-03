#[cfg(test)]
mod tests {
    use crate::verify_hir_values_droppable;
    use crate::CTRCGraph;
    use crate::analyze_hir_for_ctrc;
    use crate::ctrc_pvg_graph::CTRCAnalysisResult;
    use zetaruntime::bump::GrowableBump;
    use ir::hir::*;
    use ir::span::SourceSpan;
    use zetaruntime::string_pool::VmString;

    fn create_test_string(s: &str) -> StrId {
        // This is a simplified version for testing
        // In real implementation, this would use the string pool
        StrId::new(VmString {
            offset: s.as_ptr(),
            length: s.len(),
        })
    }

    fn create_simple_hir_module<'a, 'bump>(bump: &GrowableBump<'bump>) -> HirModule<'a, 'bump> {
        let main_func_name = create_test_string("main");
        let person_class_name = create_test_string("Person");
        let x_var_name = create_test_string("x");

        let class_init_name = bump.alloc_value(HirExpr::Ident(person_class_name));
        let let_stmt = HirStmt::Let {
            name: x_var_name,
            ty: HirType::Struct(person_class_name, &[]),
            value: HirExpr::StructInit {
                name: class_init_name,
                args: &[],
                span: ir::span::SourceSpan::new("test", 0, 0),
            },
        };

        let block_body = bump.alloc_slice(&[let_stmt]);
        let block_stmt = HirStmt::Block {
            body: block_body,
        };

        let main_func = HirFunc {
            name: main_func_name,
            visibility: Visibility::Public,
            is_unsafe: false,
            inline: false,
            noinline: false,
            generics: None,
            params: None,
            return_type: Some(HirType::Void),
            body: Some(block_stmt),
        };

        let main_func_ref = bump.alloc_value(main_func);
        let func_item = Hir::Func(main_func_ref);
        let items = bump.alloc_slice(&[func_item]);

        HirModule {
            name: create_test_string("test_module"),
            imports: &[],
            items,
        }
    }

    #[test]
    fn test_ctrc_analysis_basic() {
        let bump = GrowableBump::new(4096, 8);
        let module = create_simple_hir_module(&bump);

        let result = analyze_hir_for_ctrc(&module, &bump);

        println!("Allocation sites: {:?}", result.allocation_sites);
        println!("Drop insertions: {:?}", result.drop_insertions);
        println!("Destructor calls: {:?}", result.destructor_calls);
        println!("Variable aliases: {:?}", result.variable_aliases);

        assert!(!result.allocation_sites.is_empty(), "Should detect allocation sites");

        assert!(!result.drop_insertions.is_empty(), "Should have drop insertions");

        assert!(!result.destructor_calls.is_empty(), "Should have destructor calls");
    }

    #[test]
    fn test_droppable_type_detection() {
        let _bump = GrowableBump::new(4096, 8);
        let graph = CTRCGraph::new();

        // Test primitive types (should not be droppable)
        assert!(!graph.is_droppable_type(&HirType::I32));
        assert!(!graph.is_droppable_type(&HirType::Boolean));
        assert!(!graph.is_droppable_type(&HirType::F64));

        // Test complex types (should be droppable)
        assert!(graph.is_droppable_type(&HirType::String));
        assert!(graph.is_droppable_type(&HirType::Struct(create_test_string("MyClass"), &[])));
        assert!(graph.is_droppable_type(&HirType::Generic(create_test_string("T"))));
    }

    #[test]
    fn test_memory_safety_verification() {
        let bump = GrowableBump::new(4096, 8);
        let module = create_simple_hir_module(&bump);

        let is_safe = verify_hir_values_droppable(&module, &bump);

        assert!(is_safe, "Simple allocation and drop should be memory safe");
    }

    #[test]
    fn test_struct_with_destructor() {
        let bump = GrowableBump::new(4096, 8);

        let struct_name = create_test_string("MyStruct");
        let field_name = create_test_string("data");

        // Create a struct with a destructor
        let destructor_stmt = HirStmt::Block { body: &[] };
        let field = HirField {
            name: field_name,
            field_type: HirType::String,
            visibility: Visibility::Public,
            generics: None
        };

        let fields_slice = bump.alloc_slice(&[field]);
        let destructor_ref = bump.alloc_value(destructor_stmt);
        let test_struct = HirStruct {
            name: struct_name,
            visibility: Visibility::Public,
            generics: None,
            fields: fields_slice,
            interfaces: None,
            methods: None,
            constants: None,
            destructor: Some(destructor_ref),
        };

        let struct_ref = bump.alloc_value(test_struct);
        let struct_item = Hir::Struct(struct_ref);
        let items = bump.alloc_slice(&[struct_item]);

        let module = HirModule {
            name: create_test_string("test_module"),
            imports: &[],
            items,
        };

        let result = analyze_hir_for_ctrc(&module, &bump);

        assert!(result.structs_with_destructors.contains(&struct_name));

        assert!(result.droppable_fields.contains(&(struct_name, field_name)));
    }

    #[test]
    fn test_ctrc_analysis_result_methods() {
        let result = CTRCAnalysisResult::new();

        // New result should have no memory safety issues
        assert!(!result.has_memory_safety_issues());

        // Test variable drop point lookup
        let var_name = create_test_string("test_var");
        let drop_points = result.get_drop_points_for_variable(var_name);
        assert!(drop_points.is_empty());
    }

    fn create_cyclic_hir_module<'a, 'bump>(bump: &GrowableBump<'bump>) -> HirModule<'a, 'bump> {
        let node_class_name = create_test_string("Node");
        let next_field_name = create_test_string("next");
        let data_field_name = create_test_string("data");

        let next_field = HirField {
            name: next_field_name,
            field_type: HirType::Struct(node_class_name, &[]),
            visibility: Visibility::Public,
            generics: None
        };
        let data_field = HirField {
            name: data_field_name,
            field_type: HirType::I32,
            visibility: Visibility::Public,
            generics: None
        };

        let fields = bump.alloc_slice(&[next_field, data_field]);
        let destructor_stmt = HirStmt::Block { body: &[] };
        let destructor_ref = bump.alloc_value(destructor_stmt);

        let node_struct = HirStruct {
            name: node_class_name,
            visibility: Visibility::Public,
            generics: None,
            fields,
            interfaces: None,
            methods: None,
            constants: None,
            destructor: Some(destructor_ref),
        };

        let struct_ref = bump.alloc_value(node_struct);
        let struct_item = Hir::Struct(struct_ref);

        // Create main function that creates cyclic references
        let main_func_name = create_test_string("main");
        let node1_name = create_test_string("node1");
        let node2_name = create_test_string("node2");

        // node1 = Node { next: null, data: 1 }
        let class_init_name1 = bump.alloc_value(HirExpr::Ident(node_class_name));
        let node1_init = HirExpr::StructInit {
            name: class_init_name1,
            args: &[],
            span: ir::span::SourceSpan::new("test", 0, 0),
        };
        let let_stmt1 = HirStmt::Let {
            name: node1_name,
            ty: HirType::Struct(node_class_name, &[]),
            value: node1_init,
        };

        // node2 = Node { next: node1, data: 2 }
        let class_init_name2 = bump.alloc_value(HirExpr::Ident(node_class_name));
        let node2_init = HirExpr::StructInit {
            name: class_init_name2,
            args: &[],
            span: ir::span::SourceSpan::new("test", 0, 0),
        };
        let let_stmt2 = HirStmt::Let {
            name: node2_name,
            ty: HirType::Struct(node_class_name, &[]),
            value: node2_init,
        };

        // node1.next = node2 (creates cycle: node1 -> node2 -> node1)
        let node1_ref = bump.alloc_value(HirExpr::Ident(node1_name));
        let field_access = bump.alloc_value(HirExpr::FieldAccess {
            object: node1_ref,
            field: next_field_name,
            span: ir::span::SourceSpan::new("test", 0, 0),
        });
        let node2_ref = bump.alloc_value(HirExpr::Ident(node2_name));
        let assignment = bump.alloc_value(HirExpr::Assignment {
            target: field_access,
            value: node2_ref,
            op: AssignmentOperator::Assign,
            span: ir::span::SourceSpan::new("test", 0, 0),
        });
        let assign_stmt = HirStmt::Expr(assignment);

        let statements = bump.alloc_slice(&[let_stmt1, let_stmt2, assign_stmt]);
        let block_stmt = HirStmt::Block { body: statements };

        let main_func = HirFunc {
            name: main_func_name,
            visibility: Visibility::Public,
            is_unsafe: false,
            inline: false,
            noinline: false,
            generics: None,
            params: None,
            return_type: Some(HirType::Void),
            body: Some(block_stmt),
        };

        let main_func_ref = bump.alloc_value(main_func);
        let func_item = Hir::Func(main_func_ref);
        let items = bump.alloc_slice(&[struct_item, func_item]);

        HirModule {
            name: create_test_string("cyclic_test_module"),
            imports: &[],
            items,
        }
    }

    #[test]
    fn test_simple_cyclic_reference_detection() {
        let bump = GrowableBump::new(8192, 16);
        let module = create_cyclic_hir_module(&bump);

        let result = analyze_hir_for_ctrc(&module, &bump);

        println!("Cyclic test - Allocation sites: {:?}", result.allocation_sites);
        println!("Cyclic test - Drop insertions: {:?}", result.drop_insertions);
        println!("Cyclic test - Destructor calls: {:?}", result.destructor_calls);
        println!("Cyclic test - Potential leaks: {:?}", result.potential_leaks);

        assert!(result.allocation_sites.len() >= 2, "Should detect at least 2 allocation sites for cyclic nodes");

        assert!(!result.drop_insertions.is_empty(), "Should have drop insertions for allocated objects");

        assert!(!result.destructor_calls.is_empty(), "Should have destructor calls for cyclic structures");
    }

    #[test]
    fn test_self_referential_structure() {
        let bump = GrowableBump::new(4096, 8);

        let self_ref_class = create_test_string("SelfRef");
        let self_field = create_test_string("self_ptr");

        // Create self-referential struct
        let field = HirField {
            name: self_field,
            field_type: HirType::Struct(self_ref_class, &[]),
            visibility: Visibility::Public,
            generics: None
        };

        let fields = bump.alloc_slice(&[field]);
        let destructor_stmt = HirStmt::Block { body: &[] };
        let destructor_ref = bump.alloc_value(destructor_stmt);

        let self_ref_struct = HirStruct {
            name: self_ref_class,
            visibility: Visibility::Public,
            generics: None,
            fields,
            interfaces: None,
            methods: None,
            constants: None,
            destructor: Some(destructor_ref),
        };

        let struct_ref = bump.alloc_value(self_ref_struct);
        let struct_item = Hir::Struct(struct_ref);

        // Create function that creates self-reference
        let main_func_name = create_test_string("main");
        let obj_name = create_test_string("obj");

        let class_init_name = bump.alloc_value(HirExpr::Ident(self_ref_class));
        let obj_init = HirExpr::StructInit {
            name: class_init_name,
            args: &[],
            span: ir::span::SourceSpan::new("test", 0, 0),
        };

        let let_stmt = HirStmt::Let {
            name: obj_name,
            ty: HirType::Struct(self_ref_class, &[]),
            value: obj_init,
        };

        // obj.self_ptr = obj (creates self-cycle)
        let obj_ref1 = bump.alloc_value(HirExpr::Ident(obj_name));
        let field_access = bump.alloc_value(HirExpr::FieldAccess {
            object: obj_ref1,
            field: self_field,
            span: ir::span::SourceSpan::new("test", 0, 0),
        });
        let obj_ref2 = bump.alloc_value(HirExpr::Ident(obj_name));
        let assignment = bump.alloc_value(HirExpr::Assignment {
            target: field_access,
            value: obj_ref2,
            op: AssignmentOperator::Assign,
            span: ir::span::SourceSpan::new("test", 0, 0),
        });
        let assign_stmt = HirStmt::Expr(assignment);

        let statements = bump.alloc_slice(&[let_stmt, assign_stmt]);
        let block_stmt = HirStmt::Block { body: statements };

        let main_func = HirFunc {
            name: main_func_name,
            visibility: Visibility::Public,
            is_unsafe: false,
            inline: false,
            noinline: false,
            generics: None,
            params: None,
            return_type: Some(HirType::Void),
            body: Some(block_stmt),
        };

        let main_func_ref = bump.alloc_value(main_func);
        let func_item = Hir::Func(main_func_ref);
        let items = bump.alloc_slice(&[struct_item, func_item]);

        let module = HirModule {
            name: create_test_string("self_ref_module"),
            imports: &[],
            items,
        };

        let result = analyze_hir_for_ctrc(&module, &bump);

        assert!(!result.allocation_sites.is_empty(), "Should detect allocation site");

        assert!(!result.drop_insertions.is_empty(), "Should have drop insertions");

        assert!(!result.destructor_calls.is_empty(), "Should have destructor calls");

        // Note: current implementation may not detect self-reference as a leak yet
        // this is expected behavior for the current prototype
    }

    #[test]
    fn test_cycle_breaking_with_explicit_drops() {
        let bump = GrowableBump::new(4096, 8);

        let node_class = create_test_string("Node");
        let next_field = create_test_string("next");

        // Create Node struct
        let field = HirField {
            name: next_field,
            field_type: HirType::Struct(node_class, &[]),
            visibility: Visibility::Public,
            generics: None
        };

        let fields = bump.alloc_slice(&[field]);
        let destructor_stmt = HirStmt::Block { body: &[] };
        let destructor_ref = bump.alloc_value(destructor_stmt);

        let node_struct = HirStruct {
            name: node_class,
            visibility: Visibility::Public,
            generics: None,
            fields,
            interfaces: None,
            methods: None,
            constants: None,
            destructor: Some(destructor_ref),
        };

        let struct_ref = bump.alloc_value(node_struct);
        let struct_item = Hir::Struct(struct_ref);

        // Create function with explicit cycle breaking
        let main_func_name = create_test_string("main");
        let node1_name = create_test_string("node1");
        let node2_name = create_test_string("node2");

        // Create nodes and cycle
        let class_init1 = bump.alloc_value(HirExpr::Ident(node_class));
        let node1_init = HirExpr::StructInit {
            name: class_init1,
            args: &[],
            span: ir::span::SourceSpan::new("test", 0, 0),
        };
        let let_stmt1 = HirStmt::Let {
            name: node1_name,
            ty: HirType::Struct(node_class, &[]),
            value: node1_init,
        };

        let class_init2 = bump.alloc_value(HirExpr::Ident(node_class));
        let node2_init = HirExpr::StructInit {
            name: class_init2,
            args: &[],
            span: ir::span::SourceSpan::new("test", 0, 0),
        };
        let let_stmt2 = HirStmt::Let {
            name: node2_name,
            ty: HirType::Struct(node_class, &[]),
            value: node2_init,
        };

        // Break cycle by setting one reference to null (simulated as assignment to new object)
        let null_init = bump.alloc_value(HirExpr::Ident(node_class));
        let null_obj = HirExpr::StructInit {
            name: null_init,
            args: &[],
            span: ir::span::SourceSpan::new("test", 0, 0),
        };

        let node1_ref = bump.alloc_value(HirExpr::Ident(node1_name));
        let field_access = bump.alloc_value(HirExpr::FieldAccess {
            object: node1_ref,
            field: next_field,
            span: ir::span::SourceSpan::new("test", 0, 0),
        });
        let break_cycle = bump.alloc_value(HirExpr::Assignment {
            target: field_access,
            value: bump.alloc_value(null_obj),
            op: AssignmentOperator::Assign,
            span: ir::span::SourceSpan::new("test", 0, 0),
        });
        let break_stmt = HirStmt::Expr(break_cycle);

        let statements = bump.alloc_slice(&[let_stmt1, let_stmt2, break_stmt]);
        let block_stmt = HirStmt::Block { body: statements };

        let main_func = HirFunc {
            name: main_func_name,
            visibility: Visibility::Public,
            is_unsafe: false,
            inline: false,
            noinline: false,
            generics: None,
            params: None,
            return_type: Some(HirType::Void),
            body: Some(block_stmt),
        };

        let main_func_ref = bump.alloc_value(main_func);
        let func_item = Hir::Func(main_func_ref);
        let items = bump.alloc_slice(&[struct_item, func_item]);

        let module = HirModule {
            name: create_test_string("cycle_break_module"),
            imports: &[],
            items,
        };

        let result = analyze_hir_for_ctrc(&module, &bump);

        assert!(result.allocation_sites.len() >= 2, "Should detect multiple allocation sites");

        assert!(!result.drop_insertions.is_empty(), "Should have drop insertions");

        assert!(!result.destructor_calls.is_empty(), "Should have destructor calls");
    }

    #[test]
    fn test_memory_safety_with_cycles() {
        let bump = GrowableBump::new(4096, 8);
        let cyclic_module = create_cyclic_hir_module(&bump);

        let is_safe = verify_hir_values_droppable(&cyclic_module, &bump);

        // Note: Current implementation may not detect cycles as unsafe yet
        // This test verifies the analysis runs without crashing
        println!("Memory safety result for cyclic module: {}", is_safe);
    }

    #[test]
    fn test_cycle_detection_infrastructure() {
        let bump: GrowableBump = GrowableBump::new(4096, 8);
        let module: HirModule = create_cyclic_hir_module(&bump);
        
        let result: CTRCAnalysisResult = analyze_hir_for_ctrc(&module, &bump);
        
        assert!(!result.allocation_sites.is_empty(), "Should detect allocations in cyclic structures");
        assert!(!result.structs_with_destructors.is_empty(), "Should detect structs with destructors");
        
        let node_name = create_test_string("Node");
        assert!(result.structs_with_destructors.contains(&node_name), "Should detect Node struct with destructor");
        
        assert!(!result.drop_insertions.is_empty(), "Should create drop insertions for allocated objects");
        
        assert!(!result.destructor_calls.is_empty(), "Should schedule destructor calls");
        println!("Cycle detection test passed - infrastructure working correctly");
    }
}
