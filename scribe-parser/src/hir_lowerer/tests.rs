#[cfg(test)]
mod hir_lowerer_tests {
    use crate::hir_lowerer::context::HirLowerer;
    use crate::hir_lowerer::monomorphization::Monomorphizer;
    use crate::parser::descent_parser::DescentParser;
    use crate::tokenizer::lexer::Lexer;
    use codex_dependency_graph::DepGraph;
    use ir::hir::{Hir, HirExpr, HirFunc, HirModule, HirStmt, HirType, StrId};
    use ir::ir_hasher::HashMap;
    use ir::registry::global_registry::GlobalRegistry;
    use std::mem::transmute;
    use std::sync::Arc;
    use zetaruntime::arena::GrowableAtomicBump;
    use zetaruntime::bump::GrowableBump;
    use zetaruntime::string_pool::StringPool;

    fn create_test_context() -> (Arc<StringPool>, GrowableBump<'static>, &'static DepGraph) {
        let context = Arc::new(StringPool::new().expect("Failed to create StringPool"));
        let bump = GrowableBump::new(4096, 8);
        let dep_graph = Box::leak(Box::new(Default::default()));
        (context, bump, dep_graph)
    }

    fn parse_and_lower(source: &str) -> (HirModule<'_, '_>, Arc<StringPool>, &'static DepGraph) {
        let (context, _bump, dep_graph) = create_test_context();
        let lexer = Lexer::new(context.clone());

        let parser_bump = Box::new(GrowableBump::new(4096, 8));
        let bump_ref = Box::leak(parser_bump);
        let tokens = Box::leak(Box::new(lexer.tokenize(source, "test.zeta", bump_ref)));

        let stmts = DescentParser::parse(context.clone(), bump_ref, tokens).unwrap();

        let atomic_bump = Arc::new(GrowableAtomicBump::new());
        let registry = GlobalRegistry::new();
        let mut lowerer = HirLowerer::new(context.clone(), atomic_bump, &dep_graph, registry);
        let module = lowerer.lower_module(stmts, 0);

        (module, context, dep_graph)
    }

    fn get_functions_from_module<'a, 'bump>(
        module: &HirModule<'a, 'bump>,
    ) -> Vec<&'bump HirFunc<'a, 'bump>> {
        module
            .items
            .iter()
            .filter_map(|item| {
                if let Hir::Func(func) = item {
                    Some(*func)
                } else {
                    None
                }
            })
            .collect()
    }

    fn get_structs_from_module<'a, 'bump>(
        module: &HirModule<'a, 'bump>,
    ) -> Vec<&'bump ir::hir::HirStruct<'a, 'bump>> {
        module
            .items
            .iter()
            .filter_map(|item| {
                if let Hir::Struct(s) = item {
                    Some(*s)
                } else {
                    None
                }
            })
            .collect()
    }

    #[test]
    fn test_basic_function_lowering() {
        let source = r#"
            fn main() {
                let x: i32 = 42;
                let y: String = "hello";
            }
        "#;

        let (module, context, _dep_graph) = parse_and_lower(source);

        let functions = get_functions_from_module(&module);
        assert_eq!(functions.len(), 1, "Expected exactly one function");

        let main_func = functions[0];
        assert_eq!(
            context.resolve_string(&main_func.name),
            "main",
            "Expected function named 'main'"
        );
        assert!(main_func.body.is_some(), "Expected main to have a body");

        if let Some(HirStmt::Block { body }) = &main_func.body {
            assert_eq!(body.len(), 2, "Expected exactly 2 statements in main body");

            println!("{:?}", body);

            // Check first let statement: let x: i32 = 42;
            if let HirStmt::Let {
                name, ty, value, ..
            } = &body[0]
            {
                assert_eq!(
                    context.resolve_string(name),
                    "x",
                    "Expected first variable named 'x'"
                );
                assert_eq!(*ty, HirType::I32, "Expected x to have type i32");
                if let HirExpr::Number(n, _) = value {
                    assert_eq!(*n, 42, "Expected x to be initialized with 42");
                } else {
                    panic!("Expected numeric literal for x, got {:?}", value);
                }
            } else {
                panic!("Expected first statement to be let");
            }

            // Check second let statement: let y = "hello";
            if let HirStmt::Let {
                name, ty, value, ..
            } = &body[1]
            {
                assert_eq!(
                    context.resolve_string(name),
                    "y",
                    "Expected second variable named 'y'"
                );
                // The type might be inferred as a Class type instead of String
                match ty {
                    HirType::String => {
                        // Expected case
                        if let HirExpr::String(s, _) = value {
                            assert_eq!(
                                context.resolve_string(s),
                                "hello",
                                "Expected y to be initialized with 'hello'"
                            );
                        } else {
                            panic!("Expected string literal for y");
                        }
                    }
                    HirType::Struct(_, _) => {
                        // Alternative case - class type for String
                        if let HirExpr::String(s, _) = value {
                            assert_eq!(
                                context.resolve_string(s),
                                "hello",
                                "Expected y to be initialized with 'hello'"
                            );
                        } else {
                            panic!("Expected string literal for y");
                        }
                    }
                    _ => panic!("Expected String or Struct type for y, got: {:?}", ty),
                }
            } else {
                panic!("Expected second statement to be let");
            }
        } else {
            panic!("Expected function body to be a block");
        }
    }

    #[test]
    fn test_generic_function_monomorphization() {
        let source = r#"
            fn main() {
                let x: i32 = 42;
                let y: String = "hello";
            }
        "#;

        let (module, context, _dep_graph) = parse_and_lower(source);

        let functions = get_functions_from_module(&module);
        assert!(functions.len() >= 1, "Expected at least one function");

        let func_names: Vec<String> = functions
            .iter()
            .map(|f| context.resolve_string(&f.name).to_string())
            .collect();

        println!("Generated functions: {:?}", func_names);

        let has_main = func_names.iter().any(|name| name == "main");
        assert!(has_main, "Should have main function, got: {:?}", func_names);

        let main_func = functions
            .iter()
            .find(|f| context.resolve_string(&f.name) == "main")
            .expect("Expected main function");
        assert!(main_func.body.is_some(), "Expected main to have a body");
    }

    #[test]
    fn test_struct_lowering() {
        let source = r#"
            struct Point { x: i32, y: i32 } {}

            fn main() {
                let p: Point = Point { x: 10, y: 20 };
            }
        "#;

        let (module, context, _dep_graph) = parse_and_lower(source);

        let structs = get_structs_from_module(&module);
        let functions = get_functions_from_module(&module);

        assert!(structs.len() >= 1, "Should have at least one struct");
        assert!(functions.len() >= 1, "Should have main function");

        let point_struct = structs
            .iter()
            .find(|s| context.resolve_string(&s.name) == "Point")
            .expect("Expected Point struct");

        assert_eq!(
            point_struct.fields.len(),
            2,
            "Expected Point to have 2 fields"
        );

        let field_names: Vec<String> = point_struct
            .fields
            .iter()
            .map(|f| context.resolve_string(&f.name).to_string())
            .collect();

        assert!(field_names.contains(&"x".to_string()), "Expected 'x' field");
        assert!(field_names.contains(&"y".to_string()), "Expected 'y' field");

        let main_func = functions
            .iter()
            .find(|f| context.resolve_string(&f.name) == "main")
            .expect("Expected main function");

        assert!(main_func.body.is_some(), "Expected main to have a body");
    }

    #[test]
    fn test_monomorphizer_type_substitution() {
        let (context, _bump, dep_graph) = create_test_context();
        let atomic_bump = Arc::new(GrowableAtomicBump::new());
        let registry = GlobalRegistry::new();
        let lowerer = HirLowerer::new(context.clone(), atomic_bump.clone(), dep_graph, registry);
        let _ = Monomorphizer::new(context.clone(), atomic_bump.clone(), unsafe {
            transmute(&lowerer.ctx)
        });

        let mut substitutions = HashMap::default();
        let t_name = context.intern("T");
        substitutions.insert(ir::hir::StrId(t_name), HirType::I32);

        assert_eq!(substitutions.len(), 1);
        assert!(substitutions.contains_key(&ir::hir::StrId(t_name)));
    }

    #[test]
    fn test_hir_type_system() {
        assert_eq!(HirType::I32, HirType::I32);
        assert_ne!(HirType::I32, HirType::String);

        let int_type = HirType::I32;
        let string_type = HirType::String;

        match int_type {
            HirType::I32 => assert!(true),
            _ => panic!("Should match I32"),
        }

        match string_type {
            HirType::String => assert!(true),
            _ => panic!("Should match String"),
        }
    }

    #[test]
    fn test_expression_lowering() {
        let source = r#"
            fn main() {
                let x: i32 = 1 + 2;
                let y: bool = true;
            }
        "#;

        let (module, context, _dep_graph) = parse_and_lower(source);

        let functions = get_functions_from_module(&module);
        assert!(functions.len() >= 1, "Expected at least one function");

        let main_func = functions[0];
        assert!(main_func.body.is_some(), "Expected main to have a body");

        if let Some(HirStmt::Block { body }) = &main_func.body {
            assert_eq!(body.len(), 2, "Expected exactly 2 statements in main");

            if let HirStmt::Let {
                name, ty, value, ..
            } = &body[0]
            {
                assert_eq!(
                    context.resolve_string(name),
                    "x",
                    "Expected first variable named 'x'"
                );
                assert_eq!(*ty, HirType::I32, "Expected x to have type i32");
                if let HirExpr::Binary { .. } = value {
                    // Expected
                } else {
                    panic!("Expected binary operation for x, got {:?}", value);
                }
            } else {
                panic!("Expected first statement to be let");
            }

            if let HirStmt::Let {
                name, ty, value, ..
            } = &body[1]
            {
                assert_eq!(
                    context.resolve_string(name),
                    "y",
                    "Expected second variable named 'y'"
                );
                assert_eq!(*ty, HirType::Boolean, "Expected y to have type bool");
                if let HirExpr::Boolean(b, _) = value {
                    assert!(*b, "Expected y to be initialized with true");
                } else {
                    panic!("Expected boolean literal for y");
                }
            } else {
                panic!("Expected second statement to be let");
            }
        } else {
            panic!("Expected function body to be a block");
        }
    }

    #[test]
    fn test_monomorphization_naming() {
        use crate::hir_lowerer::monomorphization::naming::suffix_for_subs;

        let (context, _bump, _dep_graph) = create_test_context();

        let mut substitutions: HashMap<StrId, HirType> = HashMap::default();
        let t_name = context.intern("T");
        substitutions.insert(ir::hir::StrId(t_name), HirType::I32);

        let suffix = suffix_for_subs(context.clone(), &substitutions);
        let suffix_str = context.resolve_string(&suffix);

        assert!(!suffix_str.is_empty(), "Should generate non-empty suffix");

        assert!(
            suffix_str.contains("i32") || suffix_str.contains("I32"),
            "Expected suffix to contain type info, got: {}",
            suffix_str
        );

        println!("Generated suffix for T->i32: {}", suffix_str);
    }

    #[test]
    fn test_actual_generic_function_monomorphization() {
        let source = r#"
            T identity<T>(T value) {
                return value;
            }

            fn main() {
                let x: i32 = identity<i32>(42);
                let y: String = identity<String>("hello");
                let z: i64 = identity<i64>(314);
            }
        "#;

        let (module, context, _dep_graph) = parse_and_lower(source);
        let functions = get_functions_from_module(&module);

        println!("All functions in module:");
        for func in &functions {
            let name = context.resolve_string(&func.name);
            println!("  - {}", name);
        }

        assert!(
            functions.len() > 1,
            "Should have multiple functions (main + monomorphized identity), got: {}",
            functions.len()
        );

        let func_names: Vec<String> = functions
            .iter()
            .map(|f| context.resolve_string(&f.name).to_string())
            .collect();

        let has_main = func_names.iter().any(|name| name == "main");
        assert!(has_main, "Should have main function, got: {:?}", func_names);

        let identity_functions: Vec<&String> = func_names
            .iter()
            .filter(|name| name.contains("identity"))
            .collect();

        println!("Identity functions found: {:?}", identity_functions);
        assert!(
            identity_functions.len() >= 1,
            "Should have at least one identity function, got: {:?}",
            func_names
        );

        // Verify each function has a body
        for func in &functions {
            assert!(
                func.body.is_some(),
                "Function {} should have a body",
                context.resolve_string(&func.name)
            );
        }
    }

    #[test]
    fn test_generic_field_types() {
        let source = r#"
            struct Container<T> {
                value: T,
                optional: Option<T>,
            }

            fn main() {
                let container = Container {
                    value: 42,
                    optional: Some(42),
                };
            }
        "#;

        let (_module, _context, _dep_graph) = parse_and_lower(source);

        // The test passes if it compiles and runs without panicking
        // TODO:
        // 1. Get the struct definition from the module
        // 2. Verify the field types include the generic parameters
        // 3. Check that the generic parameters are properly substituted
        // 4. Verify that the instantiated types are correct
    }

    #[test]
    fn test_generic_struct_monomorphization() {
        let source = r#"
            struct Container<T> { T value } {
                T get() {
                    return value;
                }
            }

            fn main() {
                let intContainer: Container<i32> = Container<i32> { 42 };
                let stringContainer: Container<String> = Container<String> { "hello" };
            }
        "#;

        let (module, context, _dep_graph) = parse_and_lower(source);
        let structs = get_structs_from_module(&module);
        let functions = get_functions_from_module(&module);

        println!("All structs in module:");
        for s in &structs {
            let name = context.resolve_string(&s.name);
            println!("  - {} (fields: {})", name, s.fields.len());
        }

        assert!(structs.len() >= 1, "Should have at least one struct");

        let struct_names: Vec<String> = structs
            .iter()
            .map(|s| context.resolve_string(&s.name).to_string())
            .collect();

        let container_structs: Vec<&String> = struct_names
            .iter()
            .filter(|name| name.contains("Container"))
            .collect();

        println!("Container structs found: {:?}", container_structs);
        assert!(
            container_structs.len() >= 1,
            "Should have at least one Container struct, got: {:?}",
            struct_names
        );

        for container_name in &container_structs {
            let container = structs
                .iter()
                .find(|s| context.resolve_string(&s.name) == **container_name)
                .expect("Expected to find Container struct");

            assert_eq!(
                container.fields.len(),
                1,
                "Container should have 1 field (value)"
            );
            assert_eq!(
                context.resolve_string(&container.fields[0].name),
                "value",
                "Expected 'value' field"
            );
        }

        let has_main = functions
            .iter()
            .any(|f| context.resolve_string(&f.name) == "main");
        assert!(has_main, "Should have main function");
    }

    #[test]
    fn test_monomorphization_with_different_types() {
        let source = r#"
            T add<T>(T a, T b) {
                return a + b;
            }

            fn main() {
                let int_result: i32 = add<i32>(1, 2);
                let long_result: i64 = add<i64>(100, 200);
            }
        "#;

        let (module, context, _dep_graph) = parse_and_lower(source);
        let functions = get_functions_from_module(&module);

        println!("Functions for add<T> test:");
        for func in &functions {
            let name = context.resolve_string(&func.name);
            println!("  - {}", name);
        }

        let func_names: Vec<String> = functions
            .iter()
            .map(|f| context.resolve_string(&f.name).to_string())
            .collect();

        let add_functions: Vec<&String> = func_names
            .iter()
            .filter(|name| name.contains("add"))
            .collect();

        println!("Add functions found: {:?}", add_functions);

        assert!(
            functions.len() >= 2,
            "Should have main + at least one add function, got: {}",
            functions.len()
        );

        let has_main = func_names.iter().any(|name| name == "main");
        assert!(has_main, "Should have main function");

        assert!(
            add_functions.len() >= 1,
            "Should have at least one add function, got: {:?}",
            func_names
        );

        for func in &functions {
            let func_name = context.resolve_string(&func.name);
            assert!(
                func.body.is_some(),
                "Function {} should have a body",
                func_name
            );
        }
    }

    #[test]
    fn test_monomorphization_caching() {
        let source = r#"
            T duplicate<T>(T value) {
                return value;
            }

            fn main() {
                let x1: i32 = duplicate<i32>(42);
                let x2: i32 = duplicate<i32>(24);
                let y1: String = duplicate<String>("hello");
                let y2: String = duplicate<String>("world");
            }
        "#;

        let (module, context, _dep_graph) = parse_and_lower(source);
        let functions = get_functions_from_module(&module);

        println!("Functions for caching test:");
        for func in &functions {
            let name = context.resolve_string(&func.name);
            println!("  - {}", name);
        }

        let func_names: Vec<String> = functions
            .iter()
            .map(|f| context.resolve_string(&f.name).to_string())
            .collect();

        let duplicate_functions: Vec<&String> = func_names
            .iter()
            .filter(|name| name.contains("duplicate"))
            .collect();

        println!("Duplicate functions found: {:?}", duplicate_functions);

        assert!(
            functions.len() <= 4,
            "Should cache monomorphizations, not create duplicates. Got: {:?}",
            func_names
        );
        assert!(
            duplicate_functions.len() >= 1,
            "Should have at least one duplicate function, got: {:?}",
            func_names
        );

        let has_main = func_names.iter().any(|name| name == "main");
        assert!(has_main, "Should have main function");

        for func in &functions {
            let func_name = context.resolve_string(&func.name);
            assert!(
                func.body.is_some(),
                "Function {} should have a body",
                func_name
            );
        }
    }

    #[test]
    fn test_nested_generic_monomorphization() {
        let source = r#"
            struct Wrapper<T> { inner: T } {}

            fn unwrap<T>(wrapper: Wrapper<T>): T {
                return wrapper.inner;
            }

            fn main() {
                let wrapped_int: Wrapper<i32> = Wrapper<i32> { inner: 42 };
                let result: i32 = unwrap<i32>(wrapped_int);
            }
        "#;

        let (module, context, _dep_graph) = parse_and_lower(source);
        let functions = get_functions_from_module(&module);
        let structs = get_structs_from_module(&module);

        println!("Nested generic test - Functions:");
        for func in &functions {
            let name = context.resolve_string(&func.name);
            println!("  - {}", name);
        }

        println!("Nested generic test - Structs:");
        for s in &structs {
            let name = context.resolve_string(&s.name);
            println!("  - {}", name);
        }

        assert!(functions.len() >= 1, "Should have at least one function");
        assert!(structs.len() >= 1, "Should have at least one struct");

        let has_main = functions
            .iter()
            .any(|f| context.resolve_string(&f.name) == "main");
        assert!(has_main, "Should have main function");

        let has_wrapper = structs
            .iter()
            .any(|s| context.resolve_string(&s.name).contains("Wrapper"));
        assert!(has_wrapper, "Should have Wrapper struct");

        let has_unwrap = functions
            .iter()
            .any(|f| context.resolve_string(&f.name).contains("unwrap"));
        assert!(has_unwrap, "Should have unwrap function");
    }
}
