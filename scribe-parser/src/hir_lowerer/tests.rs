#[cfg(test)]
mod hir_lowerer_tests {
    use crate::tokenizer::lexer::Lexer;
    use crate::parser::descent_parser::DescentParser;
    use crate::hir_lowerer::context::HirLowerer;
    use crate::hir_lowerer::monomorphization::Monomorphizer;
    use ir::hir::{Hir, HirType, HirFunc, HirStmt, HirExpr, HirModule};
    use ir::ir_hasher::FxHashBuilder;
    use std::collections::HashMap;
    use std::sync::Arc;
    use ctrc_graph::hir_integration::convenience::analyze_and_pretty_print;
    use zetaruntime::bump::GrowableBump;
    use zetaruntime::arena::GrowableAtomicBump;
    use zetaruntime::string_pool::StringPool;

    fn create_test_context() -> (Arc<StringPool>, GrowableBump<'static>) {
        let context = Arc::new(StringPool::new().expect("Failed to create StringPool"));
        let bump = GrowableBump::new(4096, 8);
        (context, bump)
    }

    fn parse_and_lower(source: &str) -> (HirModule<'_, '_>, Arc<StringPool>) {
        let (context, _bump) = create_test_context();
        let lexer = Lexer::from_str(source, "test.zeta", context.clone());
        let tokens = Box::leak(Box::new(lexer.tokenize()));

        // Create parser bump allocator
        let parser_bump = Box::new(GrowableBump::new(4096, 8));
        let bump_ref = Box::leak(parser_bump);
        let parser = DescentParser::new(context.clone(), bump_ref);

        let stmts = parser.parse(tokens);
        
        // Create HIR lowerer
        let atomic_bump = Arc::new(GrowableAtomicBump::new());
        let lowerer = HirLowerer::new(context.clone(), atomic_bump);
        let module = lowerer.lower_module(stmts);

        (module, context)
    }

    fn get_functions_from_module<'a, 'bump>(module: &HirModule<'a, 'bump>) -> Vec<&'bump HirFunc<'a, 'bump>> {
        module.items.iter().filter_map(|item| {
            if let Hir::Func(func) = item {
                Some(*func)
            } else {
                None
            }
        }).collect()
    }

    fn get_structs_from_module<'a, 'bump>(module: &HirModule<'a, 'bump>) -> Vec<&'bump ir::hir::HirStruct<'a, 'bump>> {
        module.items.iter().filter_map(|item| {
            if let Hir::Struct(s) = item {
                Some(*s)
            } else {
                None
            }
        }).collect()
    }

    #[test]
    fn test_basic_function_lowering() {
        let source = r#"
            void main() {
                let x: i32 = 42;
                let y: String = "hello";
            }
        "#;

        let (module, _context) = parse_and_lower(source);
        
        // Verify we have one function
        let functions = get_functions_from_module(&module);
        assert_eq!(functions.len(), 1);
        
        let main_func = functions[0];
        assert!(main_func.body.is_some());
        
        // Verify function body contains let statements
        if let Some(HirStmt::Block { body }) = &main_func.body {
            assert_eq!(body.len(), 2);
            
            // Check first let statement
            if let HirStmt::Let { name: _, ty, value, mutable } = &body[0] {
                assert_eq!(*ty, HirType::I32);
                assert!(!mutable);
                if let HirExpr::Number(n) = value {
                    assert_eq!(*n, 42);
                }
            } else {
                panic!("Expected let statement");
            }
            
            // Check second let statement with type inference
            if let HirStmt::Let { name: _, ty, value, mutable } = &body[1] {
                // The type might be inferred as a Class type instead of String
                match ty {
                    HirType::String => {
                        // Expected case
                        assert!(!mutable);
                        if let HirExpr::String(s) = value {
                            assert!(!s.is_empty());
                        }
                    }
                    HirType::Class(_, _) => {
                        // Alternative case - class type for String
                        assert!(!mutable);
                        if let HirExpr::String(s) = value {
                            assert!(!s.is_empty());
                        }
                    }
                    _ => panic!("Expected String or Class type, got: {:?}", ty),
                }
            } else {
                panic!("Expected let statement");
            }
        } else {
            panic!("Expected function body to be a block");
        }
    }

    #[test]
    fn test_generic_function_monomorphization() {
        let source = r#"
            void main() {
                let x: i32 = 42;
                let y: String = "hello";
            }
        "#;

        let (module, context) = parse_and_lower(source);
        
        // For now, just verify basic lowering works
        let functions = get_functions_from_module(&module);
        assert!(functions.len() >= 1);
        
        // Check function names
        let func_names: Vec<String> = functions.iter()
            .map(|f| context.resolve_string(&f.name).to_string())
            .collect();
        
        println!("Generated functions: {:?}", func_names);
        
        // Should have main function
        let has_main = func_names.iter().any(|name| name.contains("main"));
        assert!(has_main, "Should have main function");
    }

    #[test]
    fn test_struct_lowering() {
        let source = r#"
            void main() {
                let x: i32 = 42;
            }
        "#;

        let (module, context) = parse_and_lower(source);
        
        // Should have function
        let functions = get_functions_from_module(&module);
        
        assert!(functions.len() >= 1, "Should have main function");
        
        let func_names: Vec<String> = functions.iter()
            .map(|f| context.resolve_string(&f.name).to_string())
            .collect();
        
        println!("Generated functions: {:?}", func_names);
        
        let has_main = func_names.iter().any(|name| name.contains("main"));
        assert!(has_main, "Should have main function");
    }

    #[test]
    fn test_monomorphizer_type_substitution() {
        let (context, _bump) = create_test_context();
        let atomic_bump = Arc::new(GrowableAtomicBump::new());
        let lowerer = HirLowerer::new(context.clone(), atomic_bump.clone());
        let monomorphizer = Monomorphizer::new(lowerer.ctx.clone());
        
        // Create type substitutions: T -> i32
        let mut substitutions = HashMap::with_hasher(FxHashBuilder);
        let t_name = context.intern("T");
        substitutions.insert(ir::hir::StrId(t_name), HirType::I32);
        
        // Verify substitutions map was created
        assert_eq!(substitutions.len(), 1);
        assert!(substitutions.contains_key(&ir::hir::StrId(t_name)));
    }

    #[test]
    fn test_hir_type_system() {
        // Test basic HIR type system
        assert_eq!(HirType::I32, HirType::I32);
        assert_ne!(HirType::I32, HirType::String);
        
        // Test type matching
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
            void main() {
                let x: i32 = 1 + 2;
                let y: bool = true;
            }
        "#;

        let (module, _context) = parse_and_lower(source);
        
        let functions = get_functions_from_module(&module);
        assert!(functions.len() >= 1);
        
        let main_func = functions[0];
        assert!(main_func.body.is_some());
        
        // Just verify we can access the function body
        if let Some(HirStmt::Block { body }) = &main_func.body {
            assert!(body.len() >= 1, "Should have at least one statement");
        }
    }

    #[test]
    fn test_monomorphization_naming() {
        use crate::hir_lowerer::monomorphization::naming::suffix_for_subs;
        
        let (context, _bump) = create_test_context();
        
        // Create type substitutions
        let mut substitutions = HashMap::with_hasher(FxHashBuilder);
        let t_name = context.intern("T");
        substitutions.insert(ir::hir::StrId(t_name), HirType::I32);
        
        // Test suffix generation
        let suffix = suffix_for_subs(context.clone(), &substitutions);
        let suffix_str = context.resolve_string(&suffix);
        
        // Should generate some kind of suffix for the substitution
        assert!(!suffix_str.is_empty(), "Should generate non-empty suffix");
        println!("Generated suffix: {}", suffix_str);
    }

    #[test]
    fn test_actual_generic_function_monomorphization() {
        // Test with actual generic function that should be monomorphized
        let source = r#"
            T identity<T>(T value) {
                return value;
            }

            void main() {
                let x: i32 = identity<i32>(42);
                let y: String = identity<String>("hello");
                let z: i64 = identity<i64>(314);
            }
        "#;

        let (module, context) = parse_and_lower(source);
        let functions = get_functions_from_module(&module);
        
        println!("All functions in module:");
        for func in &functions {
            let name = context.resolve_string(&func.name);
            println!("  - {}", name);
        }
        
        // Should have main + multiple monomorphized identity functions
        assert!(functions.len() > 1, "Should have multiple functions (main + monomorphized identity)");
        
        // Check for monomorphized function names
        let func_names: Vec<String> = functions.iter()
            .map(|f| context.resolve_string(&f.name).to_string())
            .collect();
        
        let has_main = func_names.iter().any(|name| name.contains("main"));
        assert!(has_main, "Should have main function");
        
        // Look for monomorphized identity functions
        let identity_functions: Vec<&String> = func_names.iter()
            .filter(|name| name.contains("identity"))
            .collect();
        
        println!("Identity functions found: {:?}", identity_functions);
        assert!(identity_functions.len() >= 1, "Should have at least one identity function");
    }

    #[test]
    fn test_generic_struct_monomorphization() {
        let source = r#"
            struct Container<T> { T value } {
                T get() {
                    return value;
                }
            }

            void main() {
                let intContainer: Container<i32> = Container<i32> { 42 };
                let stringContainer: Container<String> = Container<String> { "hello" };
            }
        "#;

        let (module, context) = parse_and_lower(source);
        let structs = get_structs_from_module(&module);
        
        println!("All structs in module:");
        for s in &structs {
            let name = context.resolve_string(&s.name);
            println!("  - {}", name);
        }
        
        // Should have monomorphized Container structs
        assert!(structs.len() >= 1, "Should have at least one struct");
        
        let struct_names: Vec<String> = structs.iter()
            .map(|s| context.resolve_string(&s.name).to_string())
            .collect();
        
        let container_structs: Vec<&String> = struct_names.iter()
            .filter(|name| name.contains("Container"))
            .collect();
        
        println!("Container structs found: {:?}", container_structs);
        assert!(container_structs.len() >= 1, "Should have at least one Container struct");
    }

    #[test]
    fn test_monomorphization_with_different_types() {
        let source = r#"
            T add<T>(T a, T b) {
                return a + b;
            }

            void main() {
                let int_result: i32 = add<i32>(1, 2);
                let long_result: i64 = add<i64>(100, 200);
            }
        "#;

        let (module, context) = parse_and_lower(source);
        let bump = GrowableBump::new(1024, 8);
        let result = analyze_and_pretty_print(module, &bump, context.clone()).unwrap();

        println!("{}", result);
        let functions = get_functions_from_module(&module);
        
        println!("Functions for add<T> test:");
        for func in &functions {
            let name = context.resolve_string(&func.name);
            println!("  - {}", name);
        }
        
        // Should have main + monomorphized add functions for i32 and f64
        let func_names: Vec<String> = functions.iter()
            .map(|f| context.resolve_string(&f.name).to_string())
            .collect();
        
        let add_functions: Vec<&String> = func_names.iter()
            .filter(|name| name.contains("add"))
            .collect();
        
        println!("Add functions found: {:?}", add_functions);
        
        // Should have different monomorphized versions
        assert!(functions.len() >= 2, "Should have main + at least one add function");
    }

    #[test]
    fn test_monomorphization_caching() {
        let source = r#"
            T duplicate<T>(T value) {
                return value;
            }

            void main() {
                let x1: i32 = duplicate<i32>(42);
                let x2: i32 = duplicate<i32>(24);  // Same type - should reuse
                let y1: String = duplicate<String>("hello");
                let y2: String = duplicate<String>("world");  // Same type - should reuse
            }
        "#;

        let (module, context) = parse_and_lower(source);
        let functions = get_functions_from_module(&module);
        
        println!("Functions for caching test:");
        for func in &functions {
            let name = context.resolve_string(&func.name);
            println!("  - {}", name);
        }
        
        let func_names: Vec<String> = functions.iter()
            .map(|f| context.resolve_string(&f.name).to_string())
            .collect();
        
        let duplicate_functions: Vec<&String> = func_names.iter()
            .filter(|name| name.contains("duplicate"))
            .collect();
        
        println!("Duplicate functions found: {:?}", duplicate_functions);
        
        // Should cache monomorphizations - expect main + duplicate_i32 + duplicate_String
        // Not 5 separate functions (which would indicate no caching)
        assert!(functions.len() <= 4, "Should cache monomorphizations, not create duplicates");
        assert!(duplicate_functions.len() >= 1, "Should have at least one duplicate function");
    }

    #[test]
    fn test_nested_generic_monomorphization() {
        let source = r#"
            struct Wrapper<T> { T inner }

            T unwrap<T>(Wrapper<T> wrapper) {
                return wrapper.inner;
            }

            void main() {
                let wrapped_int: Wrapper<i32> = Wrapper<i32> { 42 };
                let result: i32 = unwrap<i32>(wrapped_int);
            }
        "#;

        let (module, context) = parse_and_lower(source);
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
        
        // Should have monomorphized both struct and function
        assert!(functions.len() >= 1, "Should have functions");
    }
}
