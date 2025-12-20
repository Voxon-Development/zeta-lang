use crate::tokenizer::tokens::TokenKind;
use ir::ast::Type;
use ir::hir::StrId;
use std::sync::Arc;
use zetaruntime::bump::GrowableBump;
use zetaruntime::string_pool::StringPool;

use crate::tokenizer::cursor::TokenCursor;

pub fn parse_type<'a, 'bump>(
    cursor: &mut TokenCursor<'a>,
    context: Arc<StringPool>,
    bump: &'bump GrowableBump<'bump>,
) -> Type<'a, 'bump> {
    let mut ty = parse_basic_type(cursor, context.clone(), bump);

    // Handle suffixes (nullable ?)
    while cursor.peek_kind() == Some(TokenKind::Question) {
        cursor.advance_kind();
        ty = ty.with_nullable(true);
    }

    ty
}

fn parse_basic_type<'a, 'bump>(
    cursor: &mut TokenCursor<'a>,
    context: Arc<StringPool>,
    bump: &'bump GrowableBump<'bump>,
) -> Type<'a, 'bump> {
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
            let inner = parse_type(cursor, context, bump);
            let inner_ref = bump.alloc_value(inner);
            Type {
                kind: ir::ast::TypeKind::Pointer {
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
                    args.push(parse_type(cursor, context.clone(), bump));

                    match cursor.peek_kind() {
                        Some(TokenKind::Comma) => {
                            cursor.advance_kind();
                        }
                        Some(TokenKind::Gt) => break,
                        _ => {
                            break;
                        }
                    }
                }
                cursor.expect_kind(TokenKind::Gt);

                Type {
                    kind: ir::ast::TypeKind::Struct {
                        name,
                        generics: bump.alloc_slice_copy(&args),
                    },
                    nullable: false,
                    error: false,
                }
            } else {
                // Simple named type
                Type {
                    kind: ir::ast::TypeKind::Struct {
                        name,
                        generics: &[],
                    },
                    nullable: false,
                    error: false,
                }
            }
        }
        Some(TokenKind::LParen) => {
            cursor.advance_kind(); // (
            let ty = parse_type(cursor, context, bump);
            cursor.expect_kind(TokenKind::RParen);
            ty
        }
        _ => Type::infer().with_error(true),
    }
}
