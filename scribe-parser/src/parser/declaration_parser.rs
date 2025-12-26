use crate::parser::statement_parser::StatementParser;
use crate::tokenizer::cursor::TokenCursor;
use crate::tokenizer::tokens::TokenKind;
use ir::ast::*;
use ir::hir::StrId;
use ir::span::SourceSpan;
use smallvec::SmallVec;
use std::sync::Arc;
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
        let path = self.parse_path(cursor).expect("Expected package path");

        cursor.expect_kind(TokenKind::Semicolon);

        let import_stmt = self.bump.alloc_value(ImportStmt { path });
        Stmt::Import(import_stmt)
    }

    /// Parse package statement: package pkg.name
    pub fn parse_path(&self, cursor: &mut TokenCursor<'a>) -> Option<&'bump Path<'bump>> {
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

        let path = self.parse_path(cursor).expect("Expected package path");

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

    /// Parse a type annotation from the cursor
    pub fn parse_type(&self, cursor: &mut TokenCursor<'a>) -> Type<'a, 'bump> {
        let mut ty = self.parse_basic_type(cursor);

        // Handle suffixes (nullable ?)
        while cursor.peek_kind() == Some(TokenKind::Question) {
            cursor.advance_kind();
            ty = ty.with_nullable(true);
        }

        ty
    }

    fn parse_basic_type(&self, cursor: &mut TokenCursor<'a>) -> Type<'a, 'bump> {
        match cursor.peek_kind() {
            // Primitives
            Some(TokenKind::U8) => {
                cursor.advance_kind();
                Type::u8()
            }
            Some(TokenKind::U16) => {
                cursor.advance_kind();
                Type::u16()
            }
            Some(TokenKind::U32) => {
                cursor.advance_kind();
                Type::u32()
            }
            Some(TokenKind::U64) => {
                cursor.advance_kind();
                Type::u64()
            }
            Some(TokenKind::U128) => {
                cursor.advance_kind();
                Type::u128()
            }
            Some(TokenKind::I8) => {
                cursor.advance_kind();
                Type::i8()
            }
            Some(TokenKind::I16) => {
                cursor.advance_kind();
                Type::i16()
            }
            Some(TokenKind::I32) => {
                cursor.advance_kind();
                Type::i32()
            }
            Some(TokenKind::I64) => {
                cursor.advance_kind();
                Type::i64()
            }
            Some(TokenKind::I128) => {
                cursor.advance_kind();
                Type::i128()
            }
            Some(TokenKind::F32) => {
                cursor.advance_kind();
                Type::f32()
            }
            Some(TokenKind::F64) => {
                cursor.advance_kind();
                Type::f64()
            }
            Some(TokenKind::Boolean) => {
                cursor.advance_kind();
                Type::boolean()
            }
            Some(TokenKind::Str) => {
                cursor.advance_kind();
                Type::string()
            }
            Some(TokenKind::Void) => {
                cursor.advance_kind();
                Type::void()
            }
            Some(TokenKind::Char) => {
                cursor.advance_kind();
                Type::char()
            }
            Some(TokenKind::This) => {
                cursor.advance_kind();
                Type::this()
            }

            // Pointers *T
            Some(TokenKind::Mul) => {
                cursor.advance_kind(); // *
                let is_mut = if cursor.peek_kind() == Some(TokenKind::Mut) {
                    cursor.advance_kind();
                    true
                } else {
                    false
                };
                let inner = self.parse_type(cursor);
                let inner_ref = self.bump.alloc_value(inner);
                Type {
                    kind: TypeKind::Pointer {
                        inner: inner_ref,
                        mutable: is_mut,
                    },
                    nullable: false,
                    error: false,
                }
            }

            Some(TokenKind::Ident) => {
                let name = cursor.consume_ident().unwrap();

                // Handle generics <T, U>
                if cursor.peek_kind() == Some(TokenKind::Lt) {
                    cursor.advance_kind(); // <
                    let mut args = Vec::new();
                    while cursor.peek_kind() != Some(TokenKind::Gt) && !cursor.at_end() {
                        args.push(self.parse_type(cursor));

                        match cursor.peek_kind() {
                            Some(TokenKind::Comma) => {
                                cursor.advance_kind();
                            }
                            Some(TokenKind::Gt) => break,
                            _ => {
                                // Unexpected token, try to recover
                                // For now just break to avoid infinite loop
                                break;
                            }
                        }
                    }
                    cursor.expect_kind(TokenKind::Gt);

                    Type {
                        kind: TypeKind::Struct {
                            name,
                            generics: self.bump.alloc_slice_copy(&args),
                        },
                        nullable: false,
                        error: false,
                    }
                } else {
                    // Simple named type
                    Type {
                        kind: TypeKind::Struct {
                            name,
                            generics: &[],
                        },
                        nullable: false,
                        error: false,
                    }
                }
            }
            Some(TokenKind::LParen) => {
                // Parenthesized type or Tuple or Lambda
                // For now assuming simple parenthesized or tuple
                cursor.advance_kind(); // (
                // Simplification: parse one type
                let ty = self.parse_type(cursor);
                cursor.expect_kind(TokenKind::RParen);
                ty
            }
            _ => {
                // Unknown type start
                Type::infer().with_error(true)
            }
        }
    }

    /// Parse function declaration using TokenCursor (main entry point)
    pub fn parse_function(&self, cursor: &mut TokenCursor<'a>) -> Stmt<'a, 'bump> {
        let func_decl = self.parse_function_decl(cursor);
        let func_decl_ref = self.bump.alloc_value(func_decl);
        Stmt::FuncDecl(func_decl_ref)
    }

    pub(crate) fn parse_function_decl(
        &self,
        cursor: &mut TokenCursor<'a>,
    ) -> FuncDecl<'a, 'bump> {
        cursor.skip_comments();
        let visibility = Self::fetch_visibility(cursor);
        cursor.skip_comments();

        let mut is_unsafe = false;
        let mut is_extern = false;
        let mut extern_string = None;
        let mut inline = false;
        let mut noinline = false;

        loop {
            match cursor.peek_kind() {
                Some(TokenKind::Unsafe) if !is_unsafe => {
                    is_unsafe = true;
                    cursor.advance_kind();
                }
                Some(TokenKind::Inline) if !inline && !noinline => {
                    inline = true;
                    cursor.advance_kind();
                }
                Some(TokenKind::Noinline) if !noinline && !inline => {
                    noinline = true;
                    cursor.advance_kind();
                }
                Some(TokenKind::Extern) if !is_extern => {
                    is_extern = true;
                    cursor.advance_kind();
                    extern_string = cursor.consume_string().or_else(|| {
                        // default ABI
                        Some(StrId(self.context.intern("C")))
                    });
                }
                _ => break,
            }
        }

        // illegal modifier combinations
        if inline && noinline {
            // emit diagnostic here
        }

        cursor.expect_kind(TokenKind::Fn);

        let name = cursor.consume_ident().unwrap_or_else(|| {
            // emit diagnostic
            StrId(self.context.intern("_error_function"))
        });

        // generics only immediately after name
        let generics = if cursor.peek_kind() == Some(TokenKind::Lt) {
            self.parse_generics(cursor)
        } else {
            None
        };

        let params = if cursor.expect_kind(TokenKind::LParen) {
            self.parse_function_params(cursor)
        } else {
            None
        };

        let return_type = if cursor.peek_kind() == Some(TokenKind::Colon) {
            cursor.advance_kind();
            Some(self.parse_type(cursor))
        } else {
            None
        };

        let body = match cursor.peek_kind() {
            Some(TokenKind::LBrace) => {
                self.parse_block(cursor)
            }
            Some(TokenKind::Assign) => {
                cursor.advance_kind();
                let expr = self.statement_parser.parse_expr_placeholder(cursor);
                let ret = self.bump.alloc_value(ReturnStmt { value: Some(expr) });
                let stmts = self.bump.alloc_slice_copy(&[Stmt::Return(ret)]);
                let block = self.bump.alloc_value_immutable(Block { block: stmts });
                cursor.expect_kind(TokenKind::Semicolon);
                Some(block)
            }
            _ => None,
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

    pub(crate) fn parse_block(
        &self,
        cursor: &mut TokenCursor<'a>,
    ) -> Option<&'bump Block<'a, 'bump>> {
        cursor.skip_comments();

        if !cursor.expect_kind(TokenKind::LBrace) {
            return None;
        }

        cursor.skip_comments();

        let mut stmts: Vec<(Stmt<'a, 'bump>, bool)> = Vec::new();

        while cursor.peek_kind() != Some(TokenKind::RBrace) && !cursor.at_end() {
            cursor.skip_comments();

            if cursor.peek_kind() == Some(TokenKind::Semicolon) {
                cursor.advance_kind();
                continue;
            }

            let stmt = self.statement_parser.parse_stmt(cursor);

            let had_semicolon = matches!(cursor.peek_kind(), Some(TokenKind::Semicolon));
            if had_semicolon {
                cursor.advance_kind();
            }

            stmts.push((stmt, had_semicolon));
        }

        if !cursor.expect_kind(TokenKind::RBrace) {
            return None;
        }

        // Rewrite last expression stmt into return
        let mut final_stmts: Vec<Stmt<'a, 'bump>> = Vec::with_capacity(stmts.len());

        for (i, (stmt, had_semicolon)) in stmts.iter().enumerate() {
            let is_last = i + 1 == stmts.len();

            if is_last && !had_semicolon {
                if let Stmt::ExprStmt(expr) = stmt {
                    let ret = self.bump.alloc_value(ReturnStmt { value: Some(expr.expr) });
                    final_stmts.push(Stmt::Return(ret));
                    continue;
                }
            }

            final_stmts.push(*stmt);
        }

        let stmts_slice = if final_stmts.is_empty() {
            &[]
        } else {
            self.bump.alloc_slice_copy(final_stmts.as_slice())
        };

        Some(self.bump.alloc_value(Block { block: stmts_slice }))
    }

    /// Parses generic parameters with better error recovery and support for associated types
    pub fn parse_generics(
        &self,
        cursor: &mut TokenCursor<'a>,
    ) -> Option<&'bump [Generic<'a, 'bump>]> {
        cursor.expect_kind(TokenKind::Lt);

        let mut generics: SmallVec<Generic<'a, 'bump>, 4> = SmallVec::new();

        loop {
            if cursor.peek_kind() == Some(TokenKind::Gt) {
                break;
            }

            // Handle const generic parameters: const N: usize
            // Skip const generics entirely as Generic struct doesn't support const value types yet
            if cursor.peek_kind() == Some(TokenKind::Const) {
                cursor.advance_kind(); // consume 'const'

                // Skip const generic name (N)
                if cursor.peek_kind() == Some(TokenKind::Ident) {
                    cursor.consume_ident();
                }

                // Skip type annotation (: usize) if present
                if cursor.peek_kind() == Some(TokenKind::Colon) {
                    cursor.advance_kind(); // consume ':'
                    // Skip type annotation tokens
                    while cursor.peek_kind() != Some(TokenKind::Comma)
                        && cursor.peek_kind() != Some(TokenKind::Gt)
                        && !cursor.at_end()
                    {
                        cursor.advance_kind();
                    }
                }

                // Skip to comma or closing angle bracket
                match cursor.peek_kind() {
                    Some(TokenKind::Comma) => {
                        cursor.advance_kind();
                        continue;
                    }
                    Some(TokenKind::Gt) => break,
                    _ => {
                        // Try to recover
                        while cursor.peek_kind() != Some(TokenKind::Comma)
                            && cursor.peek_kind() != Some(TokenKind::Gt)
                            && !cursor.at_end()
                        {
                            cursor.advance_kind();
                        }
                        continue;
                    }
                }
            }

            let name = cursor.consume_ident()
                .unwrap_or_else(|| panic!(
                    "Expected generic parameter name, found {:?}",
                    cursor.peek_kind()
                ));

            generics.push(Generic {
                const_generic: false,
                type_name: name,
                constraints: self.bump.alloc_slice(&[]),
            });

            match cursor.peek_kind() {
                Some(TokenKind::Comma) => cursor.advance_kind(),
                Some(TokenKind::Gt) => break,
                other => panic!("Expected ',' or '>', found {:?}", other),
            }
        }

        cursor.expect_kind(TokenKind::Gt);

        if generics.is_empty() {
            None
        } else {
            Some(self.bump.alloc_slice_copy(&generics))
        }
    }

    /// Parses generic constraints with support for associated types
    fn parse_generic_constraints(
        &self,
        cursor: &mut TokenCursor<'a>,
    ) -> Result<&'bump [Type<'a, 'bump>], String> {
        let mut constraints: SmallVec<Type<'a, 'bump>, 3> = SmallVec::new();

        // Parse the first constraint
        let ty = self.parse_type(cursor);
        if ty.error && matches!(ty.kind, TypeKind::Infer) {
            return Err("Expected trait or type after ':'".to_string());
        }
        constraints.push(ty);

        while cursor.peek_kind() == Some(TokenKind::Add) {
            cursor.advance_kind(); // consume '+'

            while cursor.peek_kind() == Some(TokenKind::Add) {
                cursor.advance_kind();
            }

            let ty = self.parse_type(cursor);
            if ty.error && matches!(ty.kind, TypeKind::Infer) {
                return Err("Expected trait or type after '+' in constraints".to_string());
            }

            // Avoid duplicate constraints
            if !constraints.iter().any(|c| *c == ty) {
                constraints.push(ty);
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
    fn parse_associated_type_bounds(
        &self,
        cursor: &mut TokenCursor<'a>,
    ) -> Option<&'bump [Type<'a, 'bump>]> {
        let checkpoint = cursor.checkpoint();
        let mut constraints: SmallVec<Type<'a, 'bump>, 2> = SmallVec::new();

        if !cursor.expect_kind(TokenKind::Lt) {
            return None;
        }

        // Parse associated type bounds
        while cursor.peek_kind() != Some(TokenKind::Gt) {
            if let Some(ident) = cursor.consume_ident() {
                if cursor.expect_kind(TokenKind::Eq) {
                    let ty = self.parse_type(cursor);

                    if constraints.iter().all(|c: &Type<'a, 'bump>| c != &ty) {
                        constraints.push(ty);
                    }

                    while !matches!(
                        cursor.peek_kind(),
                        Some(TokenKind::Comma) | Some(TokenKind::Gt) | None
                    ) {
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

    pub(crate) fn parse_struct_params(
        &self,
        cursor: &mut TokenCursor<'a>,
    ) -> Option<&'bump [Param<'a, 'bump>]> {
        let mut params: SmallVec<Param, 8> = SmallVec::new();

        loop {
            match cursor.peek_kind() {
                Some(TokenKind::RParen) => {
                    cursor.advance_kind();
                    break;
                }
                Some(TokenKind::Ident) => {
                    let name = cursor.consume_ident()?;
                    let param_type = self.parse_type(cursor);

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

    fn parse_function_params(
        &self,
        cursor: &mut TokenCursor<'a>,
    ) -> Option<&'bump [Param<'a, 'bump>]> {
        let mut params: SmallVec<Param<'a, 'bump>, 8> = SmallVec::new();

            loop {
            match cursor.peek_kind() {
                Some(TokenKind::RParen) => {
                    cursor.advance_kind(); // consume ')'
                    break;
                }
                Some(TokenKind::Mul) => {
                    // Check for pointer types (*mut this, *this)
                    cursor.advance_kind(); // consume '*'
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
                            type_annotation: None,
                        });
                        params.push(Param::This(this_param));
                        // Handle comma after *this or *mut this
                        if cursor.peek_kind() == Some(TokenKind::Comma) {
                            cursor.advance_kind();
                        }
                    }
                    // Skip if we had * but not followed by this
                    continue;
                }
                Some(TokenKind::Mut)
                | Some(TokenKind::Ident)
                | Some(TokenKind::I32)
                | Some(TokenKind::I64)
                | Some(TokenKind::Str)
                | Some(TokenKind::Void)
                | Some(TokenKind::I8)
                | Some(TokenKind::I16)
                | Some(TokenKind::I128)
                | Some(TokenKind::U8)
                | Some(TokenKind::U16)
                | Some(TokenKind::U32)
                | Some(TokenKind::U64)
                | Some(TokenKind::U128)
                | Some(TokenKind::F32)
                | Some(TokenKind::F64)
                | Some(TokenKind::Boolean)
                | Some(TokenKind::Char) => {
                    // Try to parse in different formats
                    let (name, param_type, is_this) = if cursor.peek_kind()
                        == Some(TokenKind::Ident)
                        && cursor.peek_kind_n(1) == Some(TokenKind::Colon)
                    {
                        // Format: name: type
                        let name = cursor.consume_ident()?;
                        cursor.expect_kind(TokenKind::Colon); // consume ':'
                        let param_type = self.parse_type(cursor);
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
                                    // Reconstruct type from first_str (ident)
                                    // This path is tricky because we already consumed the ident.
                                    // Ideally we should use parse_type but we already ate the first token.
                                    // But since we are here, first_ident was a simple identifier type.
                                    // EXCEPT if it was a generic type, we are screwed here too!
                                    // But 'type name' syntax is deprecated/problematic anyway.
                                    // If we support 'type name', we need to parse type first.
                                    // Since we already consumed first_ident, we assume it's a simple type.
                                    let param_type = Type {
                                        kind: TypeKind::Struct {
                                            name: first_ident,
                                            generics: &[],
                                        },
                                        nullable: false,
                                        error: false,
                                    };
                                    (name, param_type, false)
                                } else {
                                    // Just a name, infer type
                                    (first_ident, Type::infer(), false)
                                }
                            }
                            Some(TokenKind::I8) => {
                                cursor.advance_kind(); // consume type
                                let name = cursor.consume_ident()?;
                                (name, Type::i8(), false)
                            }
                            Some(TokenKind::I16) => {
                                cursor.advance_kind(); // consume type
                                let name = cursor.consume_ident()?;
                                (name, Type::i16(), false)
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
                            Some(TokenKind::I128) => {
                                cursor.advance_kind(); // consume type
                                let name = cursor.consume_ident()?;
                                (name, Type::i128(), false)
                            }
                            Some(TokenKind::U8) => {
                                cursor.advance_kind(); // consume type
                                let name = cursor.consume_ident()?;
                                (name, Type::u8(), false)
                            }
                            Some(TokenKind::U16) => {
                                cursor.advance_kind(); // consume type
                                let name = cursor.consume_ident()?;
                                (name, Type::u16(), false)
                            }
                            Some(TokenKind::U32) => {
                                cursor.advance_kind(); // consume type
                                let name = cursor.consume_ident()?;
                                (name, Type::u32(), false)
                            }
                            Some(TokenKind::U64) => {
                                cursor.advance_kind(); // consume type
                                let name = cursor.consume_ident()?;
                                (name, Type::u64(), false)
                            }
                            Some(TokenKind::U128) => {
                                cursor.advance_kind(); // consume type
                                let name = cursor.consume_ident()?;
                                (name, Type::u128(), false)
                            }
                            Some(TokenKind::F32) => {
                                cursor.advance_kind(); // consume type
                                let name = cursor.consume_ident()?;
                                (name, Type::f32(), false)
                            }
                            Some(TokenKind::F64) => {
                                cursor.advance_kind(); // consume type
                                let name = cursor.consume_ident()?;
                                (name, Type::f64(), false)
                            }
                            Some(TokenKind::Str) => {
                                cursor.advance_kind(); // consume type
                                let name = cursor.consume_ident()?;
                                (name, Type::string(), false)
                            }
                            Some(TokenKind::Boolean) => {
                                cursor.advance_kind(); // consume type
                                let name = cursor.consume_ident()?;
                                (name, Type::boolean(), false)
                            }
                            Some(TokenKind::Char) => {
                                cursor.advance_kind(); // consume type
                                let name = cursor.consume_ident()?;
                                (name, Type::char(), false)
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
                            is_mut: false,
                            is_move: false,
                            type_annotation: None,
                        });
                        params.push(Param::This(this_param));
                    } else {
                        let normal_param = self.bump.alloc_value(NormalParam {
                            is_mut: false,
                            is_move: false,
                            name,
                            type_annotation: param_type,
                            visibility: Visibility::Public,
                            default_value: None,
                        });
                        params.push(Param::Normal(normal_param));
                    }

                    // Handle comma separator
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
