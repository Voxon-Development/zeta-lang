use std::sync::Arc;
use crate::parser::parser_types::parse_to_type;
use crate::parser::statement_parser::StatementParser;
use crate::tokenizer::tokens::TokenKind;
use crate::tokenizer::cursor::TokenCursor;
use ir::ast::*;
use ir::hir::StrId;
use smallvec::SmallVec;
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
    
    /// Parse package statement: package "name"
    pub fn parse_package(&self, cursor: &mut TokenCursor<'a>) -> Stmt<'a, 'bump> {
        cursor.expect_kind(TokenKind::Package);
        let path = cursor.consume_string()
            .expect("Expected string literal for package name");
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
        
        let return_type: Option<Type> = match cursor.peek_kind() {
            Some(TokenKind::Void) => {
                // Explicit void return type
                cursor.advance_kind(); // consume 'void'
                Some(Type::Void)
            },
            Some(TokenKind::I32) => {
                cursor.advance_kind(); // consume 'i32'
                Some(Type::I32)
            },
            Some(TokenKind::I64) => {
                cursor.advance_kind(); // consume 'i64'
                Some(Type::I64)
            },
            Some(TokenKind::Str) => {
                cursor.advance_kind(); // consume 'str'
                Some(Type::String)
            },
            Some(TokenKind::Ident) => {
                // Check if this looks like a return type by looking ahead
                if cursor.peek_kind_n(1) == Some(TokenKind::Ident) {
                    // We have two identifiers in a row: return_type function_name
                    let type_name: StrId = cursor.consume_ident()
                        .expect("Expected identifier for return type");
                    let type_str: &str = self.context.resolve_string(&type_name);
                    Some(parse_to_type(type_str, cursor.peek_kind().unwrap(), self.context.clone(), self.bump))
                } else {
                    // Only one identifier, assume no return type (void)
                    None
                }
            },
            _ => None,
        };

        let name = match cursor.consume_ident() {
            Some(name) => name,
            None => {
                panic!("Error: Expected function name at current position");
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

        let body: Option<&Block> = if cursor.peek_kind() == Some(TokenKind::LBrace) {
            self.parse_block(cursor)
        } else if cursor.peek_kind() == Some(TokenKind::Assign) {
            cursor.advance_kind(); // consume '='
            let expr = self.statement_parser.parse_expr_stmt(cursor);
            let return_stmt = match expr {
                Stmt::ExprStmt(expr_stmt) => {
                    let return_stmt = self.bump.alloc_value(ReturnStmt {
                        value: Some(expr_stmt.expr),
                    });
                    Stmt::Return(return_stmt)
                }
                other => other,
            };
            let stmts_slice: &[Stmt] = self.bump.alloc_slice_copy(&[return_stmt]);
            Some(self.bump.alloc_value(Block { block: stmts_slice }))
        } else {
            None
        };

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
        if !cursor.expect_kind(TokenKind::LBrace) {
            return None;
        }
        
        let mut stmts = Vec::new();
        
        while cursor.peek_kind() != Some(TokenKind::RBrace) && !cursor.at_end() {
            let stmt = self.statement_parser.parse_stmt(cursor);
            stmts.push(stmt);
        }
        
        cursor.expect_kind(TokenKind::RBrace);
        
        // Allocate block in bump allocator
        let stmts_slice = if stmts.is_empty() {
            &[]
        } else {
            self.bump.alloc_slice_copy(stmts.as_slice())
        };
        
        Some(self.bump.alloc_value(Block { block: stmts_slice }))
    }

    pub(crate) fn parse_generics(&self, cursor: &mut TokenCursor<'a>) -> Option<&'bump [Generic<'bump>]> {
        let mut generics: SmallVec<[Generic; 3]> = SmallVec::new();
        
        loop {
            match cursor.peek_kind() {
                Some(TokenKind::Gt) => {
                    cursor.advance_kind(); // consume '>'
                    break;
                }
                Some(TokenKind::Ident) => {
                    let name = cursor.consume_ident()?;
                    generics.push(Generic {
                        const_generic: false,
                        type_name: name,
                        constraints: self.bump.alloc_slice(&[]),
                    });
                    
                    if cursor.peek_kind() == Some(TokenKind::Comma) {
                        cursor.advance_kind();
                    }
                }
                _ => break,
            }
        }
        
        if generics.is_empty() {
            None
        } else {
            Some(self.bump.alloc_slice_copy(generics.as_slice()))
        }
    }

    pub(crate) fn parse_struct_params(&self, cursor: &mut TokenCursor<'a>) -> Option<&'bump [Param<'a, 'bump>]> {
        let mut params: SmallVec<[Param; 8]> = SmallVec::new();
        
        loop {
            match cursor.peek_kind() {
                Some(TokenKind::RParen) => {
                    cursor.advance_kind(); // Consume the closing paren
                    break;
                }
                Some(TokenKind::Ident) => {
                    // Parse type
                    let type_name = cursor.consume_ident()?;

                    let name = cursor.consume_ident()?;
                    let type_str = self.context.resolve_string(&type_name);
                    let param_type = parse_to_type(type_str, cursor.peek_kind().unwrap(), self.context.clone(), self.bump);
                    
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
        let mut params: SmallVec<[Param; 8]> = SmallVec::new();

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
                        println!("this is mut = {}", is_mut);
                        let this_param = self.bump.alloc_value(ThisParam {
                            is_mut,
                            is_move: false,
                            type_annotation: None
                        });
                        params.push(Param::This(this_param));
                        continue;
                    }

                    // Try to parse in different formats
                    let (name, param_type) = if cursor.peek_kind() == Some(TokenKind::Ident) && cursor.peek_kind_n(1) == Some(TokenKind::Colon) {
                        // Format: name: type
                        let name = cursor.consume_ident()?;
                        cursor.expect_kind(TokenKind::Colon); // consume ':'
                        let type_name = cursor.consume_ident()?;
                        let type_str = self.context.resolve_string(&type_name);
                        let param_type = parse_to_type(type_str, cursor.peek_kind().unwrap_or(TokenKind::EOF), self.context.clone(), self.bump);
                        (name, param_type)
                    } else {
                        // Format: type name (or just name for 'this')
                        match cursor.peek_kind() {
                            Some(TokenKind::Ident) => {
                                let first_ident = cursor.consume_ident()?;
                                let first_str = self.context.resolve_string(&first_ident);
                                
                                if first_str == "this" {
                                    // Special case: 'this' parameter (no type specified)
                                    (first_ident, Type::Infer)
                                } else if cursor.peek_kind() == Some(TokenKind::Ident) {
                                    // type name format
                                    let name = cursor.consume_ident()?;
                                    let param_type = parse_to_type(first_str, cursor.peek_kind().unwrap_or(TokenKind::EOF), self.context.clone(), self.bump);
                                    (name, param_type)
                                } else {
                                    // Just a name, infer type
                                    (first_ident, Type::Infer)
                                }
                            }
                            Some(TokenKind::I32) => {
                                cursor.advance_kind(); // consume type
                                let name = cursor.consume_ident()?;
                                (name, Type::I32)
                            }
                            Some(TokenKind::I64) => {
                                cursor.advance_kind(); // consume type
                                let name = cursor.consume_ident()?;
                                (name, Type::I64)
                            }
                            Some(TokenKind::Str) => {
                                cursor.advance_kind(); // consume type
                                let name = cursor.consume_ident()?;
                                (name, Type::String)
                            }
                            Some(TokenKind::Void) => {
                                cursor.advance_kind(); // consume type
                                let name = cursor.consume_ident()?;
                                (name, Type::Void)
                            }
                            _ => break, // Can't parse parameter
                        }
                    };

                    let normal_param = self.bump.alloc_value(NormalParam {
                        is_mut,
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
}