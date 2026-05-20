use ir::ast::{Block, ExternModifier, FuncDecl, FuncModifiers, FuncSafety, InlineModifier, Stmt, Visibility};
use ir::errors::error::{DiagnosticError, ParseErrorKind};
use crate::parser::descent_parser::DescentParser;
use ir::tokens::{Cursor, TokenKind};

impl<'a, 'bump> DescentParser<'a, 'bump>
where
    'bump: 'a,
{
    pub fn parse_function_with_visibility(&mut self, visibility: Visibility) -> Result<Stmt<'a, 'bump>, DiagnosticError<'a>> {
        let function_metadata = Self::get_func_metadata(&mut self.cursor, visibility)?;
        self.cursor.expect(TokenKind::Fn)?;
        let name_token = self.cursor.bump();
        let name = match name_token.kind {
            TokenKind::Ident => {
                match name_token.text {
                    Some(text) if text.is_empty() => {
                        return Err(DiagnosticError::new(ParseErrorKind::EmptyIdent, name_token.span))
                    }
                    Some(text) => text,
                    None => return Err(DiagnosticError::new(ParseErrorKind::EmptyIdent, name_token.span))
                }
            }
            _ => return Err(DiagnosticError::new(ParseErrorKind::InvalidFunctionName { found: name_token.kind }, name_token.span))
        };

        let generics = self.parse_generics()?;
        let params = self.parse_params()?;

        let return_type = if self.cursor.consume(TokenKind::Arrow) {
            Some(self.parse_type()?)
        } else {
            None
        };

        let body = self.parse_block()?;

        let func = FuncDecl {
            function_metadata,
            name,
            generics,
            params,
            return_type,
            body: Some(self.bump.alloc_value_immutable(body)),
        };
        Ok(Stmt::FuncDecl(self.bump.alloc_value_immutable(func)))
    }

    /// Parse a function signature with optional body (for interfaces)
    /// Can end with either `;` (just signature) or `{ ... }` (with body)
    pub fn parse_function_signature(&mut self, visibility: Visibility) -> Result<FuncDecl<'a, 'bump>, DiagnosticError<'a>> {
        let function_metadata = Self::get_func_metadata(&mut self.cursor, visibility)?;
        self.cursor.expect(TokenKind::Fn)?;
        let name_token = self.cursor.bump();
        let name = match name_token.kind {
            TokenKind::Ident => {
                match name_token.text {
                    Some(empty) if empty.is_empty() => {
                        return Err(DiagnosticError::new(ParseErrorKind::EmptyIdent, name_token.span))
                    }
                    Some(text) => text,
                    None => return Err(DiagnosticError::new(ParseErrorKind::EmptyIdent, name_token.span))
                }
            }
            _ => return Err(DiagnosticError::new(ParseErrorKind::InvalidFunctionName { found: name_token.kind }, name_token.span))
        };

        let generics = self.parse_generics()?;
        let params = self.parse_params()?;

        let return_type = if self.cursor.consume(TokenKind::Arrow) {
            Some(self.parse_type()?)
        } else {
            None
        };

        // Interface methods can end with just `;` or have a body `{ ... }`
        let body = if self.cursor.consume(TokenKind::Semicolon) {
            None
        } else {
            let block = self.parse_block()?;
            Some(self.bump.alloc_value_immutable(block))
        };

        Ok(FuncDecl {
            function_metadata,
            name,
            generics,
            params,
            return_type,
            body,
        })
    }

    fn get_func_metadata(cursor: &mut Cursor<'a>, visibility: Visibility) -> Result<FuncModifiers, DiagnosticError<'a>> {
        let mut inline_modifier = InlineModifier::None;
        let mut extern_modifier = ExternModifier::None;
        let mut func_safety = FuncSafety::Safe;

        loop {
            match cursor.peek() {
                TokenKind::Inline => {
                    if let InlineModifier::Noinline = inline_modifier {
                        todo!("Handle error at inline modifier.")
                    }
                    if let InlineModifier::Inline = inline_modifier {
                        todo!("Handle error at inline modifier.")
                    }
                    inline_modifier = InlineModifier::Inline;
                    cursor.advance();
                }
                TokenKind::Noinline => {
                    if let InlineModifier::Inline = inline_modifier {
                        todo!("Handle error at noinline modifier.")
                    }
                    if let InlineModifier::Noinline = inline_modifier {
                        todo!("Handle error at noinline modifier.")
                    }
                    inline_modifier = InlineModifier::Noinline;
                    cursor.advance();
                }
                TokenKind::Extern => {
                    cursor.advance();
                    let tok = cursor.peek_token();
                    let abi = match tok.kind {
                        TokenKind::Ident => {
                            let text = tok.text.expect("ident must have text");
                            cursor.advance();
                            text
                        }
                        TokenKind::String => {
                            let text = tok.text.expect("string must have text");
                            cursor.advance();
                            text
                        }
                        _ => return Err(DiagnosticError::new(
                            ParseErrorKind::UnexpectedToken { 
                                expected: TokenKind::Ident, 
                                found: tok.kind 
                            },
                            tok.span
                        )),
                    };
                    extern_modifier = ExternModifier::Abi(abi);
                }
                TokenKind::Unsafe => {
                    if func_safety == FuncSafety::Unsafe {
                        todo!("Handle error at duplicate unsafe modifier.")
                    }
                    func_safety = FuncSafety::Unsafe;
                    cursor.advance();
                }
                _ => break,
            }
        }

        Ok(FuncModifiers {
            visibility,
            extern_modifier,
            inline_modifier,
            func_safety
        })
    }
}