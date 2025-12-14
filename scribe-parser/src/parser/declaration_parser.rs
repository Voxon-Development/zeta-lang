use std::sync::Arc;
use crate::parser::parser_types::parse_to_type;
use crate::parser::statement_parser::StatementParser;
use crate::tokenizer::tokens::TokenKind;
use crate::tokenizer::cursor::TokenCursor;
use ir::ast::*;
use ir::hir::StrId;
use smallvec::SmallVec;
use ir::span::SourceSpan;
use zetaruntime::bump::GrowableBump;
use zetaruntime::string_pool::StringPool;

pub struct DeclarationParser<'a, 'bump>
where
    'bump: 'a,
{
    pub(crate) context: Arc<StringPool>,
    pub(crate) bump: &'bump GrowableBump<'bump>,
    pub(crate) statement_parser: StatementParser<'a, 'bump>,
    phantom: std::marker::PhantomData<&'a ()>,
}

impl<'a, 'bump> DeclarationParser<'a, 'bump>
where
    'bump: 'a,
{
    pub fn new(context: Arc<StringPool>, bump: &'bump GrowableBump) -> Self {
        Self {
            context: context.clone(),
            bump,
            statement_parser: StatementParser::new(context, bump),
            phantom: std::marker::PhantomData,
        }
    }

    /// Parse import statement: import "path"
    pub fn parse_import(&self, cursor: &mut TokenCursor<'a>) -> Stmt<'a, 'bump> {
        cursor.expect_kind(TokenKind::Import);
        let path = cursor.consume_string()
            .expect("Expected string literal for import path");
        
        cursor.expect_kind(TokenKind::Semicolon);
        
        let import_stmt = self.bump.alloc_value(ImportStmt { path });
        Stmt::Import(import_stmt)
    }
    
    /// Parse package statement: package pkg.name
    pub fn parse_path(
        &self,
        cursor: &mut TokenCursor<'a>,
    ) -> Option<&'bump Path<'bump>> {
        let mut segments = Vec::new_in(self.bump);

        // First segment
        let first = cursor.consume_ident()?;
        segments.push(first);

        // (. ident)*
        while cursor.expect_kind(TokenKind::Dot) {
            let ident = cursor
                .consume_ident()
                .expect("Expected identifier after '.' in package path");
            segments.push(ident);
        }

        let segments = self.bump.alloc_slice_copy(segments.as_slice());
        Some(self.bump.alloc_value_immutable(Path { path: segments }))
    }

    pub fn parse_package(&self, cursor: &mut TokenCursor<'a>) -> Stmt<'a, 'bump> {
        cursor.expect_kind(TokenKind::Package);

        let path = self
            .parse_path(cursor)
            .expect("Expected package path");

        cursor.expect_kind(TokenKind::Semicolon);

        let package_stmt = self.bump.alloc_value(PackageStmt { path });
        Stmt::Package(package_stmt)
    }




    pub(crate) fn fetch_visibility(cursor: &mut TokenCursor) -> Visibility {
        match cursor.peek_kind() {
            Some(TokenKind::Private) => {
                cursor.advance_kind();
                Visibility::Private
            }
            Some(TokenKind::Module) => {
                cursor.advance_kind();
                Visibility::Module
            }
            Some(TokenKind::Package) => {
                cursor.advance_kind();
                Visibility::Package
            }
            _ => Visibility::Public,
        }
    }

    /// Parse function declaration using TokenCursor (main entry point)
    pub fn parse_function(&self, cursor: &mut TokenCursor<'a>) -> Stmt<'a, 'bump> {
        let func_decl = self.parse_function_decl(cursor);
        let func_decl_ref = self.bump.alloc_value(func_decl);
        Stmt::FuncDecl(func_decl_ref)
    }

    pub(crate) fn parse_function_decl(&self, cursor: &mut TokenCursor<'a>) -> FuncDecl<'a, 'bump> {
        let visibility = Self::fetch_visibility(cursor);

        let mut is_unsafe = false;
        let mut is_extern = false;
        let mut extern_string = None;
        let mut inline = false;
        let mut noinline = false;
        
        // Skip any whitespace or comments before function declaration
        cursor.skip_comments();

        loop {
            match cursor.peek_kind() {
                Some(TokenKind::Unsafe) => {
                    is_unsafe = true;
                    cursor.advance_kind();
                }
                Some(TokenKind::Inline) => {
                    inline = true;
                    cursor.advance_kind();
                }
                Some(TokenKind::Noinline) => {
                    noinline = true;
                    cursor.advance_kind();
                }
                Some(TokenKind::Extern) => {
                    is_extern = true;
                    cursor.advance_kind();
                    extern_string = cursor.consume_string();
                }
                _ => break,
            }
        }
        
        // Require 'func' keyword for function declarations
        cursor.expect_kind(TokenKind::Fn);
        
        // Parse function name
        let name = match cursor.consume_ident() {
            Some(name) => name,
            None => {
                eprintln!("Warning: Expected function name after 'fn', got: {:?}", cursor.peek_kind());
                // Return a minimal function with a placeholder name
                return FuncDecl {
                    name: StrId(self.context.intern("_error_function")),
                    generics: None,
                    params: None,
                    return_type: None,
                    body: None,
                    extern_string: None,
                    visibility,
                    is_unsafe,
                    is_extern,
                    inline,
                    noinline,
                };
            }
        };

        let generics: Option<&[Generic]> = if cursor.peek_kind() == Some(TokenKind::Lt) {
            cursor.advance_kind(); // consume '<'
            self.parse_generics(cursor)
        } else {
            None
        };

        let params: Option<&[Param]> = if cursor.peek_kind() == Some(TokenKind::LParen) {
            cursor.advance_kind(); // consume '('
            self.parse_function_params(cursor)
        } else {
            None
        };
        
        // Parse return type (after parameters)
        let return_type: Option<Type> = if cursor.peek_kind() == Some(TokenKind::Colon) {
            cursor.advance_kind(); // consume ':'
            
            match cursor.peek_kind() {
                Some(TokenKind::Void) => {
                    cursor.advance_kind();
                    Some(Type::void())
                },
                Some(TokenKind::I8) => {
                    cursor.advance_kind();
                    Some(Type::i8())
                },
                Some(TokenKind::I16) => {
                    cursor.advance_kind();
                    Some(Type::i16())
                },
                Some(TokenKind::I32) => {
                    cursor.advance_kind();
                    Some(Type::i32())
                },
                Some(TokenKind::I64) => {
                    cursor.advance_kind();
                    Some(Type::i64())
                },
                Some(TokenKind::I128) => {
                    cursor.advance_kind();
                    Some(Type::i128())
                },
                Some(TokenKind::U8) => {
                    cursor.advance_kind();
                    Some(Type::u8())
                },
                Some(TokenKind::U16) => {
                    cursor.advance_kind();
                    Some(Type::u16())
                },
                Some(TokenKind::U32) => {
                    cursor.advance_kind();
                    Some(Type::u32())
                },
                Some(TokenKind::U64) => {
                    cursor.advance_kind();
                    Some(Type::u64())
                },
                Some(TokenKind::U128) => {
                    cursor.advance_kind();
                    Some(Type::u128())
                },
                Some(TokenKind::Str) => {
                    cursor.advance_kind();
                    Some(Type::string())
                },
                Some(TokenKind::Ident) => {
                    let type_name = cursor.consume_ident()
                        .expect("Expected return type after colon");
                    let type_str = self.context.resolve_string(&type_name);
                    Some(parse_to_type(type_str, cursor.peek_kind().unwrap_or(TokenKind::EOF), self.context.clone(), self.bump))
                },
                _ => None,
            }
        } else {
            None // No return type specified, defaults to void
        };


        // Parse function body
        let body: Option<&Block> = match cursor.peek_kind() {
            // Block body: { ... }
            Some(TokenKind::LBrace) => {
                self.parse_block(cursor)
            }
            // Expression body: = expr;
            Some(TokenKind::Assign) => {
                cursor.advance_kind(); // consume '='
                let expr = self.statement_parser.parse_expr_placeholder(cursor);
                
                // Create a return statement with the expression
                let return_stmt = self.bump.alloc_value(ReturnStmt {
                    value: Some(expr),
                });
                
                // Create a block with the single return statement
                let stmts = self.bump.alloc_slice_copy(&[Stmt::Return(return_stmt)]);
                Some(self.bump.alloc_value(Block { block: stmts }))
            }
            // No body (e.g., extern functions)
            _ => None,
        };
        
        // Ensure we consume the semicolon for expression-bodied functions
        if body.is_some() && cursor.peek_kind() == Some(TokenKind::Semicolon) {
            cursor.advance_kind();
        }

        FuncDecl {
            visibility,
            is_unsafe,
            is_extern,
            inline,
            noinline,
            extern_string,
            name,
            generics,
            params,
            return_type,
            body,
        }
    }

    pub(crate) fn parse_block(&self, cursor: &mut TokenCursor<'a>) -> Option<&'bump Block<'a, 'bump>> {
        // Skip any whitespace before the opening brace
        cursor.skip_comments();
        
        // Expect opening brace
        if !cursor.expect_kind(TokenKind::LBrace) {
            return None;
        }
        
        // Skip any whitespace after opening brace
        cursor.skip_comments();
        
        let mut stmts = Vec::new();
        
        // Parse until we hit the closing brace or end of input
        while cursor.peek_kind() != Some(TokenKind::RBrace) && !cursor.at_end() {
            // Skip any whitespace before the statement
            cursor.skip_comments();
            
            // Check for empty block or extra semicolons
            if cursor.peek_kind() == Some(TokenKind::Semicolon) {
                cursor.advance_kind(); // skip empty statement
                continue;
            }
            
            // Parse the actual statement
            let stmt = self.statement_parser.parse_stmt(cursor);
            stmts.push(stmt);
            
            // Skip any whitespace after the statement
            cursor.skip_comments();
            
            // Check for semicolon after statement (except for block statements)
            if matches!(cursor.peek_kind(), Some(TokenKind::Semicolon)) {
                cursor.advance_kind();
            }
        }
        
        // Expect and consume the closing brace
        if !cursor.expect_kind(TokenKind::RBrace) {
            // TODO: Add proper error handling for missing closing brace
            return None;
        }
        
        // Allocate block in bump allocator
        let stmts_slice = if stmts.is_empty() {
            &[]
        } else {
            self.bump.alloc_slice_copy(stmts.as_slice())
        };
        
        Some(self.bump.alloc_value(Block { block: stmts_slice }))
    }

    /// Parses generic parameters with better error recovery and support for associated types
    pub(crate) fn parse_generics(&self, cursor: &mut TokenCursor<'a>) -> Option<&'bump [Generic<'a, 'bump>]> {
        let mut generics: SmallVec<Generic, 3> = SmallVec::new();
        let mut errors: SmallVec<(SourceSpan, String), 2> = SmallVec::new();

        // Verify we're actually in a generic parameter list
        if !cursor.expect_kind(TokenKind::Lt) {
            return None;
        }

        // Keep track of the last valid position for error recovery
        let mut last_valid_pos = cursor.position();
        let mut recovery_point = cursor.checkpoint();

        'outer: loop {
            // Skip any stray commas or other delimiters
            while matches!(
            cursor.peek_kind(),
            Some(TokenKind::Comma) | Some(TokenKind::Semicolon) | Some(TokenKind::Colon)
        ) {
                cursor.advance_kind();
            }

            match cursor.peek_kind() {
                Some(TokenKind::Gt) => {
                    cursor.advance_kind();
                    break;
                }
                Some(TokenKind::Const) => {
                    // Save state before parsing const generic
                    cursor.advance_kind(); // consume 'const'

                    let name = match cursor.consume_ident() {
                        Some(name) => name,
                        None => {
                            errors.push((
                                cursor.current_span(),
                                "Expected identifier after 'const' in generic parameter".to_string(),
                            ));
                            // Try to recover by finding the next parameter or end of generics
                            if !self.recover_to_next_parameter(cursor) {
                                break 'outer;
                            }
                            continue;
                        }
                    };

                    // Parse type annotation
                    if !cursor.expect_kind(TokenKind::Colon) {
                        errors.push((
                            cursor.current_span(),
                            format!("Expected ':' after const generic parameter '{}'",
                                    self.context.resolve_string(&name)),
                        ));
                        if !self.recover_to_next_parameter(cursor) {
                            break 'outer;
                        }
                        continue;
                    }

                    let type_span = cursor.current_span();
                    if let Some(type_name) = cursor.peek_text()
                            .map(|str_id| (str_id, cursor.peek_kind().unwrap()))
                            .map(|id| parse_to_type(id.0.as_str(), id.1, self.context.clone(), self.bump)) 
                    {
                        generics.push(Generic {
                            const_generic: true,
                            type_name: name,
                            constraints: self.bump.alloc_slice_copy(&[type_name]),
                        });
                        last_valid_pos = cursor.position();
                        recovery_point = cursor.checkpoint();
                    } else {
                        errors.push((
                            type_span,
                            format!("Expected type after ':' in const generic parameter '{}'",
                                    self.context.resolve_string(&name)),
                        ));
                        if !self.recover_to_next_parameter(cursor) {
                            break 'outer;
                        }
                    }
                }
                Some(TokenKind::Ident) => {
                    let name = match cursor.consume_ident() {
                        Some(name) => name,
                        None => {
                            // Shouldn't happen since we just peeked an ident
                            cursor.advance_kind(); // Skip the invalid token
                            continue;
                        }
                    };

                    // Parse constraints if present
                    let mut constraints: &[Type<'a, 'bump>] = if cursor.peek_kind() == Some(TokenKind::Colon) {
                        cursor.advance_kind();
                        match self.parse_generic_constraints(cursor) {
                            Ok(constraints) => constraints,
                            Err(e) => {
                                errors.push((cursor.current_span(), e));
                                self.bump.alloc_slice(&[])
                            }
                        }
                    } else {
                        self.bump.alloc_slice(&[])
                    };

                    // Parse associated type bounds if present
                    if cursor.peek_kind() == Some(TokenKind::Lt) {
                        if let Some(associated) = self.parse_associated_type_bounds(cursor) {
                            // Merge with existing constraints
                            let mut all_constraints: SmallVec<Type<'a, 'bump>, 4> = SmallVec::new();
                            all_constraints.extend_from_slice(constraints);
                            all_constraints.extend_from_slice(associated);
                            constraints = self.bump.alloc_slice_copy(&all_constraints);
                        }
                    }

                    generics.push(Generic {
                        const_generic: false,
                        type_name: name,
                        constraints,
                    });
                    last_valid_pos = cursor.position();
                    recovery_point = cursor.checkpoint();
                }
                Some(_) => {
                    // Try to recover from unexpected tokens
                    if !self.recover_to_next_parameter(cursor) {
                        break;
                    }
                }
                None => {
                    // End of input reached
                    errors.push((
                        cursor.current_span(),
                        "Unterminated generic parameters".to_string(),
                    ));
                    break;
                }
            }

            // Handle comma or end of parameters
            match cursor.peek_kind() {
                Some(TokenKind::Comma) => {
                    cursor.advance_kind();
                    // Check for trailing comma followed by >
                    if cursor.peek_kind() == Some(TokenKind::Gt) {
                        cursor.advance_kind();
                        break;
                    }
                }
                Some(TokenKind::Gt) => {
                    // Will be handled in the next iteration
                    continue;
                }
                Some(_) => {
                    // Try to recover by inserting a virtual comma
                    errors.push((
                        cursor.current_span(),
                        "Expected ',' or '>' after generic parameter".to_string(),
                    ));
                    // Try to recover by pretending there was a comma
                    if cursor.peek_kind().is_some() {
                        continue;
                    }
                }
                None => break,
            }
        }

        // Report any accumulated errors
        for (span, msg) in errors {
            eprintln!("{} at {}", msg, span);
        }

        if generics.is_empty() {
            None
        } else {
            Some(self.bump.alloc_slice_copy(generics.as_slice()))
        }
    }

    /// Parses generic constraints with support for associated types
    fn parse_generic_constraints(
        &self,
        cursor: &mut TokenCursor<'a>,
    ) -> Result<&'bump [Type<'a, 'bump>], String> {
        let mut constraints: SmallVec<Type<'a, 'bump>, 3> = SmallVec::new();

        // Helper to parse a single constraint
        let parse_constraint = |cursor: &mut TokenCursor| -> Option<Type<'a, 'bump>> {
            let ident: StrId = cursor.consume_ident()?;

            // Check for associated type bounds
            if cursor.peek_kind() == Some(TokenKind::Lt) {
                // For now, just skip the associated type bounds
                // In a full implementation, you'd want to parse these properly
                cursor.advance_kind(); // consume '<'
                let mut depth = 1;
                while let Some(tok) = cursor.peek_kind() {
                    match tok {
                        TokenKind::Lt => depth += 1,
                        TokenKind::Gt => {
                            depth -= 1;
                            if depth == 0 {
                                cursor.advance_kind();
                                break;
                            }
                        }
                        _ => {}
                    }
                    cursor.advance_kind();
                }
            }

            let type_str = self.context.resolve_string(&ident);
            let ty = parse_to_type(
                type_str,
                cursor.peek_kind().unwrap_or(TokenKind::EOF),
                self.context.clone(),
                self.bump,
            );
            Some(ty)

        };

        // Parse the first constraint
        if let Some(constraint) = parse_constraint(cursor) {
            constraints.push(constraint);
        } else {
            return Err("Expected trait or type after ':'".to_string());
        }

        while cursor.peek_kind() == Some(TokenKind::Add) {
            cursor.advance_kind(); // consume '+'

            while cursor.peek_kind() == Some(TokenKind::Add) {
                cursor.advance_kind();
            }

            if let Some(constraint) = parse_constraint(cursor) {
                // Avoid duplicate constraints
                if !constraints.iter().any(|&c| c == constraint) {
                    constraints.push(constraint);
                }
            } else {
                return Err("Expected trait or type after '+' in constraints".to_string());
            }
        }

        if constraints.is_empty() {
            Err("Expected at least one constraint".to_string())
        } else {
            Ok(self.bump.alloc_slice_copy(constraints.as_slice()))
        }
    }

    /// Helper to recover to the next parameter or end of generics
    fn recover_to_next_parameter(&self, cursor: &mut TokenCursor<'a>) -> bool {
        while let Some(tok) = cursor.peek_kind() {
            match tok {
                TokenKind::Comma | TokenKind::Gt => return true,
                TokenKind::Const | TokenKind::Ident => return true,
                _ => cursor.advance_kind(),
            }
        }

        false
    }

    /// Parses associated type bounds like `Iterator<Item = T>`
    fn parse_associated_type_bounds(&self, cursor: &mut TokenCursor<'a>) -> Option<&'bump [Type<'a, 'bump>]> {
        let checkpoint = cursor.checkpoint();
        let mut constraints: SmallVec<Type<'a, 'bump>, 2> = SmallVec::new();

        if !cursor.expect_kind(TokenKind::Lt) {
            return None;
        }

        // Parse associated type bounds
        while cursor.peek_kind() != Some(TokenKind::Gt) {
            if let Some(ident) = cursor.consume_ident() {
                if cursor.expect_kind(TokenKind::Eq) {
                    let type_str = self.context.resolve_string(&ident);
                    let ty: Type<'a, 'bump> = parse_to_type(
                        type_str,
                        cursor.peek_kind().unwrap_or(TokenKind::EOF),
                        self.context.clone(),
                        self.bump,
                    );

                    if constraints.iter().all(|c: &Type<'a, 'bump>| c != &ty) {
                        constraints.push(ty);
                    }

                    while !matches!(cursor.peek_kind(), Some(TokenKind::Comma) | Some(TokenKind::Gt) | None) {
                        cursor.advance_kind();
                    }
                }
            } else {
                // Skip unexpected tokens
                cursor.advance_kind();
            }

            if cursor.peek_kind() == Some(TokenKind::Comma) {
                cursor.advance_kind();
            } else if cursor.peek_kind() != Some(TokenKind::Gt) {
                // Error recovery: skip until comma or closing angle
                while !matches!(
                cursor.peek_kind(),
                Some(TokenKind::Comma) | Some(TokenKind::Gt) | None
            ) {
                    cursor.advance_kind();
                }
            }
        }

        if !cursor.expect_kind(TokenKind::Gt) {
            cursor.restore(checkpoint);
            return None;
        }

        if constraints.is_empty() {
            None
        } else {
            Some(self.bump.alloc_slice_copy(constraints.as_slice()))
        }
    }

    pub(crate) fn parse_struct_params(&self, cursor: &mut TokenCursor<'a>) -> Option<&'bump [Param<'a, 'bump>]> {
        let mut params: SmallVec<Param, 8> = SmallVec::new();
        
        loop {
            match cursor.peek_kind() {
                Some(TokenKind::RParen) => {
                    cursor.advance_kind();
                    break;
                }
                Some(TokenKind::Ident) => {
                    let type_name = cursor.consume_ident()?;

                    let name = cursor.consume_ident()?;
                    let type_str = self.context.resolve_string(&type_name);
                    let param_type: Type<'a, 'bump> = parse_to_type(type_str, cursor.peek_kind().unwrap(), self.context.clone(), self.bump);
                    
                    let normal_param = self.bump.alloc_value(NormalParam {
                        is_mut: false,
                        is_move: false,
                        name,
                        type_annotation: param_type,
                        visibility: Visibility::Public,
                        default_value: None,
                    });
                    
                    params.push(Param::Normal(normal_param));
                    
                    if cursor.peek_kind() == Some(TokenKind::Comma) {
                        cursor.advance_kind();
                    }
                }
                _ => break,
            }
        }
        
        if params.is_empty() {
            None
        } else {
            Some(self.bump.alloc_slice_copy(params.as_slice()))
        }
    }

    fn parse_function_params(&self, cursor: &mut TokenCursor<'a>) -> Option<&'bump [Param<'a, 'bump>]> {
        let mut params: SmallVec<Param, 8> = SmallVec::new();

        loop {
            match cursor.peek_kind() {
                Some(TokenKind::RParen) => {
                    cursor.advance_kind(); // consume ')'
                    break;
                }
                Some(TokenKind::Mut) | Some(TokenKind::Ident) | Some(TokenKind::I32) | Some(TokenKind::I64) | Some(TokenKind::Str) | Some(TokenKind::Void) | Some(TokenKind::This) => {
                    
                    // Check for mut modifier
                    let is_mut = if cursor.peek_kind() == Some(TokenKind::Mut) {
                        cursor.advance_kind(); // consume 'mut'
                        true
                    } else {
                        false
                    };

                    if cursor.peek_kind() == Some(TokenKind::This) {
                        cursor.advance_kind(); // consume 'this'
                        let this_param = self.bump.alloc_value(ThisParam {
                            is_mut,
                            is_move: false,
                            type_annotation: None
                        });
                        params.push(Param::This(this_param));
                        continue;
                    }

                    // Try to parse in different formats
                    let (name, param_type, is_this) = if cursor.peek_kind() == Some(TokenKind::Ident) && cursor.peek_kind_n(1) == Some(TokenKind::Colon) {
                        // Format: name: type
                        let name = cursor.consume_ident()?;
                        cursor.expect_kind(TokenKind::Colon); // consume ':'
                        let type_name = cursor.consume_ident()?;
                        let type_str = self.context.resolve_string(&type_name);
                        let param_type = parse_to_type(type_str, cursor.peek_kind().unwrap_or(TokenKind::EOF), self.context.clone(), self.bump);
                        (name, param_type, false)
                    } else {
                        // Format: type name (or just name for 'this')
                        match cursor.peek_kind() {
                            Some(TokenKind::Ident) => {
                                let first_ident = cursor.consume_ident()?;
                                let first_str = self.context.resolve_string(&first_ident);
                                
                                if first_str == "this" {
                                    // Special case: 'this' parameter (no type specified)
                                    (first_ident, Type::infer(), true)
                                } else if cursor.peek_kind() == Some(TokenKind::Ident) {
                                    // type name format
                                    let name = cursor.consume_ident()?;
                                    let param_type = parse_to_type(first_str, cursor.peek_kind().unwrap_or(TokenKind::EOF), self.context.clone(), self.bump);
                                    (name, param_type, false)
                                } else {
                                    // Just a name, infer type
                                    (first_ident, Type::infer(), false)
                                }
                            }
                            Some(TokenKind::I32) => {
                                cursor.advance_kind(); // consume type
                                let name = cursor.consume_ident()?;
                                (name, Type::i32(), false)
                            }
                            Some(TokenKind::I64) => {
                                cursor.advance_kind(); // consume type
                                let name = cursor.consume_ident()?;
                                (name, Type::i64(), false)
                            }
                            Some(TokenKind::Str) => {
                                cursor.advance_kind(); // consume type
                                let name = cursor.consume_ident()?;
                                (name, Type::string(), false)
                            }
                            Some(TokenKind::Void) => {
                                cursor.advance_kind(); // consume type
                                let name = cursor.consume_ident()?;
                                (name, Type::void(), false)
                            }
                            _ => break, // Can't parse parameter
                        }
                    };

                    if is_this {
                        // Create a ThisParam instead of NormalParam
                        let this_param = self.bump.alloc_value(ThisParam {
                            is_mut,
                            is_move: false,
                            type_annotation: None
                        });
                        params.push(Param::This(this_param));
                    } else {
                        let normal_param = self.bump.alloc_value(NormalParam {
                            is_mut,
                            is_move: false,
                            name,
                            type_annotation: param_type,
                            visibility: Visibility::Public,
                            default_value: None,
                        });
                        params.push(Param::Normal(normal_param));
                    }

                    if cursor.peek_kind() == Some(TokenKind::Comma) {
                        cursor.advance_kind();
                    }
                }
                _ => break,
            }
        }

        if params.is_empty() {
            None
        } else {
            Some(self.bump.alloc_slice_copy(params.as_slice()))
        }
    }
}