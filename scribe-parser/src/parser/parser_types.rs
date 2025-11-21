use ir::ast::Type;
use ir::hir::StrId;
use std::sync::Arc;
use zetaruntime::bump::GrowableBump;
use zetaruntime::string_pool::StringPool;
use crate::tokenizer::tokens::TokenKind;

pub fn parse_to_type<'a, 'bump>(token_type: &str, token_kind: TokenKind, context: Arc<StringPool>, bump: &'bump GrowableBump) -> Type<'a, 'bump> {
    match token_kind {
        TokenKind::U8 => return Type::U8,
        TokenKind::U16 => return Type::U16,
        TokenKind::U32 => return Type::U32,
        TokenKind::U64 => return Type::U64,
        TokenKind::U128 => return Type::U128,
        TokenKind::I8 => return Type::I8,
        TokenKind::I16 => return Type::I16,
        TokenKind::I32 => return Type::I32,
        TokenKind::I64 => return Type::I64,
        TokenKind::I128 => return Type::I128,
        TokenKind::Boolean => return Type::Boolean,
        TokenKind::String => return Type::String,
        TokenKind::Void => return Type::Void,
        TokenKind::This => return Type::This,
        _ => {}
    }
    
    let token_type = token_type.trim();

    if let Some(open_angle) = token_type.find('<') {
        let close_angle: usize = token_type
            .rfind('>')
            .expect("Unclosed angle bracket in type");
        let type_name: &str = &token_type[..open_angle];
        let inner_types: &str = &token_type[open_angle + 1..close_angle];

        let generic_args: Vec<Type> = inner_types
            .split(',')
            .map(|s| parse_to_type(s.trim(), token_kind, context.clone(), bump))
            .collect();

        return Type::Class {
            name: StrId(context.intern(type_name)),
            generics: bump.alloc_slice_copy(generic_args.as_slice()),
        };
    }

    // Handle lambda types (e.g. "(i32, String) -> boolean")
    if token_type.starts_with('(') {
        let parts: Vec<&str> = token_type.split("->").map(str::trim).collect();

        let params_part = parts[0]; // "(i32, str)"
        let params_str = params_part
            .trim()
            .trim_start_matches('(')
            .trim_end_matches(')');
        let params = if params_str.is_empty() {
            vec![]
        } else {
            params_str
                .split(',')
                .map(|p| parse_to_type(p.trim(), token_kind, context.clone(), bump))
                .collect()
        };

        let return_type = if parts.len() > 1 {
            parse_to_type(parts[1], token_kind, context.clone(), bump)
        } else {
            Type::Void
        };

        return Type::Lambda {
            params: bump.alloc_slice_copy(params.as_slice()),
            return_type: bump.alloc_value(return_type),
        };
    }
    
    match token_type {
        _ => Type::Class {
            name: StrId(context.intern(token_type)),
            generics: &[],
        }
    }
}