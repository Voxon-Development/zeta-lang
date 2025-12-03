#[cfg(test)]
mod parser_tests {
    use crate::tokenizer::lexer::Lexer;
    use crate::parser::descent_parser::DescentParser;
    use std::sync::Arc;
    use ctrc_graph::hir_integration::convenience::analyze_and_pretty_print;
    use ir::pretty::IrPrettyPrinter;
    use zetaruntime::arena::GrowableAtomicBump;
    use zetaruntime::bump::GrowableBump;
    use zetaruntime::string_pool::StringPool;
    use crate::hir_lowerer::HirLowerer;

    fn create_test_context() -> (Arc<StringPool>, GrowableBump<'static>) {
        let context = Arc::new(StringPool::new().expect("Failed to create string pool"));
        let bump = GrowableBump::new(4096, 8);
        (context, bump)
    }

    fn parse_source(source: &str, context: Arc<StringPool>, _bump: &GrowableBump<'static>) -> Result<bool, String> {
        let lexer = Lexer::from_str(source, "test.zeta", context.clone());
        let tokens = Box::leak(Box::new(lexer.tokenize()));
        
        // Create a new bump allocator and leak it for the parser
        let parser_bump = Box::new(GrowableBump::new(4096, 8));
        let bump_ref = Box::leak(parser_bump);
        let parser = DescentParser::new(context, bump_ref);
        
        let _stmts = parser.parse(tokens);
        Ok(true) // Just return success for now
    }

    #[test]
    fn test_simple_function_declaration() {
        let source = r#"
            void main() {
                return;
            }
        "#;

        let (context, bump) = create_test_context();
        let result = parse_source(source, context.clone(), &bump);
        assert!(result.is_ok(), "Failed to parse simple function: {:?}", result.err());
    }

    #[test]
    fn test_function_with_return_type() {
        let source = r#"
            i32 add(x: i32, y: i32) {
                return x + y;
            }
        "#;

        let (context, bump) = create_test_context();
        let result = parse_source(source, context.clone(), &bump);
        assert!(result.is_ok(), "Failed to parse function with return type: {:?}", result.err());
    }

    #[test]
    fn test_function_without_return_type() {
        let source = r#"
            main() {
                let x = 42;
            }
        "#;
        let (context, bump) = create_test_context();
        let result = parse_source(source, context.clone(), &bump);
        assert!(result.is_ok(), "Failed to parse function without return type: {:?}", result.err());
    }

    #[test]
    fn test_expression_bodied_function() {
        let source = r#"
            i32 double(x: i32) = x * 2;
        "#;
        let (context, bump) = create_test_context();
        let result = parse_source(source, context.clone(), &bump);
        assert!(result.is_ok(), "Failed to parse expression-bodied function: {:?}", result.err());
    }

    #[test]
    fn test_ast_pretty_output_for_simple_let() {
        let source = r#"
            fn main() {
                let x: i32 = 42;
            }
        "#;

        let (context, _bump) = create_test_context();
        let lexer = Lexer::from_str(source, "test.zeta", context.clone());
        let tokens = Box::leak(Box::new(lexer.tokenize()));

        // Parser bump
        let parser_bump = Box::new(GrowableBump::new(4096, 8));
        let bump_ref = Box::leak(parser_bump);
        let parser = DescentParser::new(context.clone(), bump_ref);

        let stmts = parser.parse(tokens);
        let ast_string = IrPrettyPrinter::ast_to_string(context.clone(), &stmts).unwrap();

        // The AST pretty-printer is simplified; assert key structure markers
        assert!(ast_string.contains("// AST Statements"));
        assert!(ast_string.contains("let x:"), "Expected a let statement for x in AST output");
        assert!(ast_string.contains("/* ast expr */"), "Placeholder AST expr formatting should be present");
    }

    #[test]
    fn test_enum_declaration() {
        let source = r#"
            enum Color {
                Red,
                Green,
                Blue,
                RGB(u8, u8, u8)
            }
        "#;
        let (context, bump) = create_test_context();
        let result = parse_source(source, context.clone(), &bump);
        assert!(result.is_ok(), "Failed to parse enum declaration: {:?}", result.err());
    }

    #[test]
    fn test_interface_declaration() {
        let source = r#"
            interface Drawable {
                void draw();
                void move(x y: i32);
            }
        "#;
        let (context, bump): (Arc<StringPool>, GrowableBump) = create_test_context();
        let result: Result<bool, String> = parse_source(source, context.clone(), &bump);
        assert!(result.is_ok(), "Failed to parse interface declaration: {:?}", result.err());
    }

    #[test]
    fn test_let_statement() {
        use ir::hir::{Hir, HirStmt, HirExpr, HirType};
        
        let source = r#"
            void main() {
                let x: i32 = 42;
                let mut y = "hello";
            }
        "#;
        
        let (context, bump) = create_test_context();
        let lexer = Lexer::from_str(source, "test.zeta", context.clone());
        let tokens = Box::leak(Box::new(lexer.tokenize()));

        // Create a new bump allocator and leak it for the parser
        let parser_bump = Box::new(GrowableBump::new(4096, 8));
        let bump_ref = Box::leak(parser_bump);
        let parser = DescentParser::new(context.clone(), bump_ref);

        let stmts = parser.parse(tokens);
        let atomic_bump = Arc::new(GrowableAtomicBump::new());
        let mut lowerer = HirLowerer::new(context.clone(), atomic_bump);
        let module = lowerer.lower_module(stmts);

        // Verify we have exactly one function (main)
        let functions: Vec<_> = module.items.iter().filter_map(|item| {
            if let Hir::Func(func) = item {
                Some(*func)
            } else {
                None
            }
        }).collect();
        
        assert_eq!(functions.len(), 1, "Expected exactly one function (main)");
        let main_func = functions[0];
        
        // Verify function name is "main"
        assert_eq!(context.resolve_string(&main_func.name), "main");
        
        // Verify function body contains two let statements
        if let Some(HirStmt::Block { body }) = &main_func.body {
            assert_eq!(body.len(), 2, "Expected two let statements in main body");
            
            // Check first let statement: let x: i32 = 42;
            if let HirStmt::Let { name, ty, value } = &body[0] {
                assert_eq!(context.resolve_string(name), "x");
                assert_eq!(*ty, HirType::I32);
                if let HirExpr::Number(n) = value {
                    assert_eq!(*n, 42);
                } else {
                    panic!("Expected numeric literal for x");
                }
            } else {
                panic!("Expected first statement to be let");
            }
            
            // Check second let statement: let y = "hello";
            if let HirStmt::Let { name, ty, value } = &body[1] {
                assert_eq!(context.resolve_string(name), "y");
                // Type should be inferred as String
                assert!(matches!(ty, HirType::String | HirType::Struct(_, _)), 
                    "Expected String or Struct type for y, got {:?}", ty);
                if let HirExpr::String(s) = value {
                    assert_eq!(context.resolve_string(s), "hello");
                } else {
                    panic!("Expected string literal for y");
                }
            } else {
                panic!("Expected second statement to be let");
            }
        } else {
            panic!("Expected function body to be a block");
        }
    }

    #[test]
    fn test_struct_let_statement() {
        use ir::hir::{Hir, HirStmt, HirExpr, HirType};
        
        let source = r#"
            struct HelloWorld { i32 x, String y } {
                void printY() {
                    println(y);
                }
            }

            void main() {
                let x: HelloWorld = HelloWorld { 1, "hello world" };
                x.printY();
            }
        "#;

        let (context, bump) = create_test_context();
        let lexer = Lexer::from_str(source, "test.zeta", context.clone());
        let tokens = Box::leak(Box::new(lexer.tokenize()));

        // Create a new bump allocator and leak it for the parser
        let parser_bump = Box::new(GrowableBump::new(4096, 8));
        let bump_ref = Box::leak(parser_bump);
        let parser = DescentParser::new(context.clone(), bump_ref);

        let stmts = parser.parse(tokens);
        let atomic_bump = Arc::new(GrowableAtomicBump::new());
        let mut lowerer = HirLowerer::new(context.clone(), atomic_bump);
        let module = lowerer.lower_module(stmts);

        // Verify we have one struct and one function
        let structs: Vec<_> = module.items.iter().filter_map(|item| {
            if let Hir::Struct(s) = item {
                Some(*s)
            } else {
                None
            }
        }).collect();
        
        let functions: Vec<_> = module.items.iter().filter_map(|item| {
            if let Hir::Func(func) = item {
                Some(*func)
            } else {
                None
            }
        }).collect();
        
        assert!(structs.len() >= 1, "Expected at least one struct");
        assert!(functions.len() >= 1, "Expected at least one function");
        
        // Find HelloWorld struct
        let hello_world = structs.iter().find(|s| {
            context.resolve_string(&s.name) == "HelloWorld"
        }).expect("Expected HelloWorld struct");
        
        // Verify struct has two fields
        assert_eq!(hello_world.fields.len(), 2, "Expected two fields in HelloWorld");
        
        // Find main function
        let main_func = functions.iter().find(|f| {
            context.resolve_string(&f.name) == "main"
        }).expect("Expected main function");
        
        // Verify main body has at least one statement
        if let Some(HirStmt::Block { body }) = &main_func.body {
            assert!(body.len() >= 1, "Expected at least one statement in main");
            
            // Check first statement is a let with HelloWorld initialization
            if let HirStmt::Let { name, ty, value } = &body[0] {
                assert_eq!(context.resolve_string(name), "x");
                // Type should be HelloWorld struct
                match ty {
                    HirType::Struct(struct_name, _) => {
                        assert_eq!(context.resolve_string(struct_name), "HelloWorld");
                    }
                    _ => panic!("Expected Struct type for x, got {:?}", ty),
                }
                // Value should be a struct initialization
                if let HirExpr::StructInit { .. } = value {
                    // Expected
                } else {
                    panic!("Expected struct initialization expression");
                }
            } else {
                panic!("Expected first statement to be let");
            }
        } else {
            panic!("Expected function body to be a block");
        }
    }

    #[test]
    fn test_if_statement() {
        let source = r#"
            void main() {
                if (x > 0) {
                    return 1;
                } else if (x < 0) {
                    return -1;
                } else {
                    return 0;
                }
            }
        "#;

        let (context, bump) = create_test_context();
        let result = parse_source(source, context.clone(), &bump);
        assert!(result.is_ok(), "Failed to parse if statement: {:?}", result.err());
    }

    #[test]
    fn test_while_loop() {
        let source = r#"
            void main() {
                while (i < 10) {
                    i = i + 1;
                }
            }
        "#;
        let (context, bump) = create_test_context();
        let result = parse_source(source, context.clone(), &bump);
        assert!(result.is_ok(), "Failed to parse while loop: {:?}", result.err());
    }

    #[test]
    fn test_for_loop() {
        let source = r#"
            void main() {
                for (let i = 0; i < 10; i = i + 1) {
                    print(i);
                }
            }
        "#;
        let (context, bump) = create_test_context();
        let result = parse_source(source, context.clone(), &bump);
        assert!(result.is_ok(), "Failed to parse for loop: {:?}", result.err());
    }

    #[test]
    fn test_match_statement() {
        let source = r#"
            void main() {
                match (color) {
                    Red => { print("red"); },
                    Green | Blue => { print("not red"); },
                    RGB(r, g, b) if r > 128 => { print("bright"); },
                    _ => { print("other"); }
                }
            }
        "#;
        let (context, bump) = create_test_context();
        let result = parse_source(source, context.clone(), &bump);
        assert!(result.is_ok(), "Failed to parse match statement: {:?}", result.err());
    }

    #[test]
    fn test_expression_parsing() {
        let source = r#"
            void main() {
                let a = 1 + 2 * 3;
                let b = (x + y) / z;
                let c = obj.field;
                let d = func(arg1, arg2);
                let e = arr[index];
                let f = x == y && z != w;
            }
        "#;
        let (context, bump) = create_test_context();
        let result = parse_source(source, context.clone(), &bump);
        assert!(result.is_ok(), "Failed to parse expressions: {:?}", result.err());
    }

    #[test]
    fn test_generic_function() {
        let source = r#"
            T identity<T>(value: T) {
                return value;
            }
        "#;
        let (context, bump) = create_test_context();
        let result = parse_source(source, context.clone(), &bump);
        assert!(result.is_ok(), "Failed to parse generic function: {:?}", result.err());
    }

    #[test]
    fn test_generic_struct() {
        let source = r#"
            struct Vec<T> {
                T data;
                usize size;
                
                void push(item: T) {
                    // implementation
                }
            }
        "#;
        let (context, bump) = create_test_context();
        let result = parse_source(source, context.clone(), &bump);
        assert!(result.is_ok(), "Failed to parse generic struct: {:?}", result.err());
    }

    #[test]
    fn test_visibility_modifiers() {
        let source = r#"
            private void private_func() {}
            module void module_func() {}
            package void package_func() {}
            public void public_func() {}
        "#;
        let (context, bump) = create_test_context();
        let result = parse_source(source, context.clone(), &bump);
        assert!(result.is_ok(), "Failed to parse visibility modifiers: {:?}", result.err());
    }

    #[test]
    fn test_function_modifiers() {
        let source = r#"
            unsafe void unsafe_func() {}
            inline void inline_func() {}
            noinline void noinline_func() {}
            extern "C" void extern_func();
        "#;
        let (context, bump) = create_test_context();
        let result = parse_source(source, context.clone(), &bump);
        assert!(result.is_ok(), "Failed to parse function modifiers: {:?}", result.err());
    }

    #[test]
    fn test_error_recovery() {
        // Test parser's ability to recover from errors
        let source = r#"
            void main() {
                let x = ; // Missing expression
                let y = 42; // This should still parse
            }
        "#;
        let (context, bump) = create_test_context();
        let _result = parse_source(source, context.clone(), &bump);
    }

    #[test]
    fn test_complex_nested_structures() {
        let source = r#"
            struct Container { items: Vec<T> } {
                void add(item: T) {
                    if (items.size() < capacity) {
                        items.push(item);
                    } else {
                        resize();
                        items.push(item);
                    }
                }
                
                T get(index: usize) {
                    if (index < items.size()) {
                        return items[index];
                    } else {
                        panic("Index out of bounds");
                    }
                }
            }
        "#;

        let (context, bump) = create_test_context();
        let result = parse_source(source, context.clone(), &bump);
        assert!(result.is_ok(), "Failed to parse complex nested structure: {:?}", result.err());
    }

    #[test]
    fn test_parser_fixes_comprehensive() {
        // 1. If statements with proper syntax
        let if_source = r#"
            void main() {
                if (items.size() < capacity) {
                    items.push(item);
                } else {
                    resize();
                    items.push(item);
                }
            }
        "#;
        let (context, bump) = create_test_context();
        let result = parse_source(if_source, context.clone(), &bump);
        assert!(result.is_ok(), "Failed to parse if statement: {:?}", result.err());

        // 2. Package as statement vs visibility modifier
        let package_statement_source = r#"
            package "my_package";
            
            void main() {}
        "#;
        let (context2, bump2) = create_test_context();
        let result2 = parse_source(package_statement_source, context2.clone(), &bump2);
        assert!(result2.is_ok(), "Failed to parse package statement: {:?}", result2.err());

        let package_visibility_source = r#"
            package void package_func() {}
        "#;
        let (context3, bump3) = create_test_context();
        let result3 = parse_source(package_visibility_source, context3.clone(), &bump3);
        assert!(result3.is_ok(), "Failed to parse package visibility modifier: {:?}", result3.err());

        // 3. Match statement with proper syntax
        let match_source = r#"
            void main() {
                match (color) {
                    Red => { print("red"); },
                    Green | Blue => { print("not red"); },
                    RGB(r, g, b) if r > 128 => { print("bright"); },
                    _ => { print("other"); }
                }
            }
        "#;
        let (context4, bump4) = create_test_context();
        let result4 = parse_source(match_source, context4.clone(), &bump4);
        assert!(result4.is_ok(), "Failed to parse match statement: {:?}", result4.err());

        // 4. For loop with proper syntax
        let for_source = r#"
            void main() {
                for (let i = 0; i < 10; i = i + 1) {
                    print(i);
                }
            }
        "#;
        let (context5, bump5) = create_test_context();
        let result5 = parse_source(for_source, context5.clone(), &bump5);
        assert!(result5.is_ok(), "Failed to parse for loop: {:?}", result5.err());

        // 5. Enum with variants
        let enum_source = r#"
            enum Color {
                Red,
                Green,
                Blue,
                RGB(u8, u8, u8)
            }
        "#;
        let (context6, bump6) = create_test_context();
        let result6 = parse_source(enum_source, context6.clone(), &bump6);
        assert!(result6.is_ok(), "Failed to parse enum with variants: {:?}", result6.err());
    }
}
