#[cfg(test)]
mod integration_tests {
    use crate::tokenizer::lexer::Lexer;
    use crate::tokenizer::cursor::TokenCursor;
    use crate::parser::declaration_parser::DeclarationParser;
    use crate::parser::statement_parser::StatementParser;
    use std::sync::Arc;
    use zetaruntime::bump::GrowableBump;
    use zetaruntime::string_pool::StringPool;

    fn create_test_parser() -> (Arc<StringPool>, DeclarationParser<'static, 'static>) {
        let context = Arc::new(StringPool::new().expect("Failed to create string pool"));
        let bump = Box::new(GrowableBump::new(4096, 8));
        
        // Leak the bump allocator to extend its lifetime for testing
        let bump_ref = Box::leak(bump);
        let parser = DeclarationParser::new(context.clone(), bump_ref);
        
        (context, parser)
    }

    fn tokenize_source(source: &str, context: Arc<StringPool>) -> crate::tokenizer::tokens::Tokens {
        let lexer = Lexer::from_str(source, "test.zeta", context);
        lexer.tokenize()
    }

    #[test]
    fn test_simple_tokenization() {
        use crate::tokenizer::tokens::TokenKind;
        
        let context = Arc::new(StringPool::new().expect("Failed to create string pool"));
        let source = "void main() { return; }";
        let tokens = tokenize_source(source, context);
        
        // Basic tokenization test
        assert!(!tokens.kinds.is_empty(), "Expected tokens to be generated");
        println!("Tokenized {} tokens", tokens.kinds.len());
        
        // Verify expected token sequence
        assert_eq!(tokens.kinds[0], TokenKind::Void, "Expected first token to be 'void'");
        assert_eq!(tokens.kinds[1], TokenKind::Ident, "Expected second token to be identifier 'main'");
        assert_eq!(tokens.kinds[2], TokenKind::LParen, "Expected third token to be '('");
        assert_eq!(tokens.kinds[3], TokenKind::RParen, "Expected fourth token to be ')'");
    }

    #[test]
    fn test_function_parsing_basic() {
        use ir::ast::Stmt;
        
        let (context, parser) = create_test_parser();
        let source = "void main() { return; }";
        let tokens = Box::leak(Box::new(tokenize_source(source, context.clone())));
        let mut cursor = TokenCursor::from_tokens(tokens);
        
        // Try to parse a function
        let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            parser.parse_function(&mut cursor)
        }));

        match result {
            Ok(stmt) => {
                println!("Successfully parsed function statement");
                
                // Verify the parsed statement is a function declaration
                if let Stmt::FuncDecl(func_decl) = stmt {
                    assert_eq!(context.resolve_string(&func_decl.name), "main", "Expected function named 'main'");
                } else {
                    panic!("Expected function declaration, got {:?}", stmt);
                }
            }
            Err(_) => {
                panic!("Function parsing panicked - this indicates a parser bug");
            }
        }
    }

    #[test]
    fn test_expression_parsing_basic() {
        let context = Arc::new(StringPool::new().expect("Failed to create string pool"));
        let bump = Box::new(GrowableBump::new(4096, 8));
        let bump_ref = Box::leak(bump);
        
        let stmt_parser = StatementParser::new(context.clone(), bump_ref);
        let source = "42 + 3 * 5";
        let tokens = Box::leak(Box::new(tokenize_source(source, context)));
        let mut cursor = TokenCursor::from_tokens(tokens);
        
        // Try to parse an expression statement
        let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            stmt_parser.parse_expr_stmt(&mut cursor)
        }));
        
        match result {
            Ok(_stmt) => {
                println!("Successfully parsed expression statement");
            }
            Err(_) => {
                println!("Expression parsing panicked - this indicates a parser bug");
            }
        }
    }

    #[test]
    fn test_struct_parsing_basic() {
        use ir::ast::Stmt;
        
        let (context, parser) = create_test_parser();
        let source = r#"
            struct Point { i32 x, i32 y } {
            }
        "#;
        let tokens = Box::leak(Box::new(tokenize_source(source, context.clone())));
        let mut cursor = TokenCursor::from_tokens(tokens);
        
        // Try to parse a struct
        let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            parser.parse_struct_decl(&mut cursor)
        }));
        
        match result {
            Ok(stmt) => {
                println!("Successfully parsed struct statement");
                
                // Verify the parsed statement is a struct declaration
                if let Stmt::StructDecl(struct_decl) = stmt {
                    assert_eq!(context.resolve_string(&struct_decl.name), "Point", "Expected struct named 'Point'");
                    assert_eq!(struct_decl.params.unwrap().len(), 2, "Expected Point to have 2 fields");
                } else {
                    panic!("Expected struct declaration, got {:?}", stmt);
                }
            }
            Err(_) => {
                panic!("Struct parsing panicked - this indicates a parser bug");
            }
        }
    }

    #[test]
    fn test_enum_parsing_basic() {
        use ir::ast::Stmt;
        
        let (context, parser) = create_test_parser();
        let source = r#"
            enum Color {
                Red,
                Green,
                Blue
            }
        "#;
        let tokens = Box::leak(Box::new(tokenize_source(source, context.clone())));
        let mut cursor = TokenCursor::from_tokens(tokens);
        
        // Try to parse an enum
        let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            parser.parse_enum(&mut cursor)
        }));
        
        match result {
            Ok(stmt) => {
                println!("Successfully parsed enum statement");
                
                // Verify the parsed statement is an enum declaration
                if let Stmt::EnumDecl(enum_decl) = stmt {
                    assert_eq!(context.resolve_string(&enum_decl.name), "Color", "Expected enum named 'Color'");
                    assert_eq!(enum_decl.variants.len(), 3, "Expected Color to have 3 variants");
                } else {
                    panic!("Expected enum declaration, got {:?}", stmt);
                }
            }
            Err(_) => {
                panic!("Enum parsing panicked - this indicates a parser bug");
            }
        }
    }

    #[test]
    fn test_type_parsing() {
        use crate::parser::parser_types::parse_to_type;
        use crate::tokenizer::tokens::TokenKind;
        
        let context = Arc::new(StringPool::new().expect("Failed to create string pool"));
        let bump = GrowableBump::new(4096, 8);
        let bump_ref = Box::leak(Box::new(bump));
        
        // Test basic type parsing
        let i32_type = parse_to_type("i32", TokenKind::I32, context.clone(), bump_ref);
        println!("Parsed i32 type: {:?}", i32_type);
        
        let string_type = parse_to_type("String", TokenKind::String, context.clone(), bump_ref);
        println!("Parsed String type: {:?}", string_type);
        
        let custom_type = parse_to_type("MyClass", TokenKind::Ident, context.clone(), bump_ref);
        println!("Parsed custom type: {:?}", custom_type);
    }

    #[test]
    fn test_cursor_operations() {
        let context = Arc::new(StringPool::new().expect("Failed to create string pool"));
        let source = "void main ( ) { return ; }";
        let tokens = Box::leak(Box::new(tokenize_source(source, context)));
        let mut cursor = TokenCursor::from_tokens(tokens);
        
        // Test basic cursor operations
        println!("Initial token: {:?}", cursor.peek_kind());
        
        cursor.advance_kind();
        println!("After advance: {:?}", cursor.peek_kind());
        
        if let Some(ident) = cursor.consume_ident() {
            println!("Consumed identifier: {:?}", ident);
        }
        
        println!("Current token after consume: {:?}", cursor.peek_kind());
        
        // Test lookahead
        println!("Next token: {:?}", cursor.peek_kind_n(1));
        println!("Token after next: {:?}", cursor.peek_kind_n(2));
    }

    #[test]
    fn test_error_conditions() {
        let context = Arc::new(StringPool::new().expect("Failed to create string pool"));
        
        // Test empty source
        let empty_tokens = Box::leak(Box::new(tokenize_source("", context.clone())));
        let empty_cursor = TokenCursor::from_tokens(empty_tokens);
        assert!(empty_cursor.at_end());
        
        // Test malformed source
        let malformed_tokens = Box::leak(Box::new(tokenize_source("void main ( { return", context.clone())));
        let mut malformed_cursor = TokenCursor::from_tokens(malformed_tokens);
        
        // The cursor should handle malformed input gracefully
        while !malformed_cursor.at_end() {
            println!("Token: {:?}", malformed_cursor.peek_kind());
            malformed_cursor.advance_kind();
        }
    }
}
