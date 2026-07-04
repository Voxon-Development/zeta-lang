use crate::parser::descent_parser::DescentParser;
use ir::ast::{
    ExternModifier, FuncDecl, FuncModifiers, FuncSafety, InlineModifier, Stmt, Type, Visibility,
};
use ir::errors::error::{DiagnosticError, ParseErrorKind};
use ir::tokens::{Cursor, TokenKind};

impl<'a, 'bump> DescentParser<'a, 'bump>
where
    'bump: 'a,
{
    pub fn parse_function_signature(
        &mut self,
        visibility: Visibility,
    ) -> Result<FuncDecl<'a, 'bump>, DiagnosticError<'a>> {
        let function_metadata = Self::get_func_metadata(&mut self.cursor, visibility)?;
        let fn_token = self.cursor.expect(TokenKind::Fn)?;

        let name_token = self.cursor.bump();
        let name = match name_token.kind {
            TokenKind::Ident => match name_token.text {
                Some(text) if text.is_empty() => {
                    return Err(DiagnosticError::new(
                        ParseErrorKind::EmptyIdent,
                        name_token.span,
                    ));
                }
                Some(text) => text,
                None => {
                    return Err(DiagnosticError::new(
                        ParseErrorKind::EmptyIdent,
                        name_token.span,
                    ));
                }
            },
            _ => {
                return Err(DiagnosticError::new(
                    ParseErrorKind::InvalidFunctionName {
                        found: name_token.kind,
                    },
                    name_token.span,
                ));
            }
        };

        let generics = self.parse_generics()?;

        let params = self.parse_params()?;

        let return_type = if self.cursor.consume(TokenKind::Arrow) {
            self.parse_return_type()
        } else {
            None
        };

        let body = if self.cursor.consume(TokenKind::Semicolon) {
            None
        } else {
            let block = self.parse_block()?;
            Some(self.bump.alloc_value_immutable(block))
        };

        let func_decl = FuncDecl {
            function_metadata,
            name,
            generics,
            params,
            return_type,
            body,
            span: fn_token.span,
        };

        Ok(func_decl)
    }

    fn parse_return_type(&mut self) -> Option<Type<'a, 'bump>> {
        match self.cursor.peek() {
            TokenKind::Semicolon | TokenKind::LBrace | TokenKind::EOF => None,
            _ => self.parse_type().ok(),
        }
    }

    pub fn parse_function_with_visibility(
        &mut self,
        visibility: Visibility,
    ) -> Result<Stmt<'a, 'bump>, DiagnosticError<'a>> {
        let func = self.parse_function_signature(visibility)?;

        let is_extern = !matches!(func.function_metadata.extern_modifier, ExternModifier::None);

        if func.body.is_none() && !is_extern {
            return Err(DiagnosticError::new(
                ParseErrorKind::ExpectedBlock,
                self.cursor.peek_token().span,
            ));
        }

        Ok(Stmt::FuncDecl(self.bump.alloc_value_immutable(func)))
    }

    pub fn get_func_metadata(
        cursor: &mut Cursor<'a>,
        visibility: Visibility,
    ) -> Result<FuncModifiers, DiagnosticError<'a>> {
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
                    cursor.expect(TokenKind::LParen)?;
                    extern_modifier = ExternModifier::Abi(cursor.expect_ident()?.0);
                    cursor.expect(TokenKind::RParen)?;
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
            func_safety,
        })
    }
}
