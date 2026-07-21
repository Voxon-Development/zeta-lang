use ir::ast::Stmt;
use lsp_types::{SemanticToken, SemanticTokenType, SemanticTokensLegend};
use sentinel_typechecker::type_checker::{SymbolId, TypeChecker};

use crate::state::ServerState;

pub const TOKEN_TYPES: &[SemanticTokenType] = &[
    SemanticTokenType::VARIABLE,  // 0
    SemanticTokenType::PROPERTY,  // 1
    SemanticTokenType::FUNCTION,  // 2
    SemanticTokenType::METHOD,    // 3
    SemanticTokenType::STRUCT,    // 4
    SemanticTokenType::ENUM,      // 5
    SemanticTokenType::INTERFACE, // 6
];

pub fn legend() -> SemanticTokensLegend {
    SemanticTokensLegend {
        token_types: TOKEN_TYPES.to_vec(),
        token_modifiers: vec![],
    }
}

pub fn build_semantic_tokens(
    checker: &TypeChecker,
    state: &ServerState,
    module_idx: usize,
    source: &str,
) -> Vec<SemanticToken> {
    struct Raw {
        line: u32,
        start_char: u32,
        length: u32,
        token_type: u32,
    }
    let mut raw = Vec::new();

    for (span, name, _, m, symbol_id, _) in checker.occurrences() {
        if *m != module_idx {
            continue;
        }
        let token_type = match symbol_id {
            SymbolId::Local(_) => 0,

            SymbolId::Field { .. } => 1,

            SymbolId::Method { .. } => 3,

            SymbolId::Item {
                module_idx,
                item_idx,
                ..
            } => {
                let Some(stmts) = state.compiler.ast_stmts(*module_idx) else {
                    continue;
                };

                match stmts.get(*item_idx) {
                    Some(Stmt::FuncDecl(_)) => 2,
                    Some(Stmt::StructDecl(_)) => 4,
                    Some(Stmt::EnumDecl(_)) => 5,
                    Some(Stmt::InterfaceDecl(_)) => 6,
                    _ => continue,
                }
            }
        };

        let expected = name.to_string();
        if crate::text_utils::span_text(source, span).as_deref() != Some(expected.as_str()) {
            continue;
        }

        let start = crate::text_utils::span_to_position(source, span.line, span.column);
        let line_text = crate::text_utils::line_at(source, start.line).unwrap_or("");
        let end_char =
            crate::text_utils::utf16_offset_from_byte(line_text, span.end_column.saturating_sub(1));
        raw.push(Raw {
            line: start.line,
            start_char: start.character,
            length: end_char - start.character,
            token_type,
        });
    }

    raw.sort_by_key(|t| (t.line, t.start_char));
    raw.dedup_by_key(|t| (t.line, t.start_char));

    let mut tokens = Vec::with_capacity(raw.len());
    let (mut prev_line, mut prev_char) = (0u32, 0u32);
    for t in raw {
        let delta_line = t.line - prev_line;
        let delta_start = if delta_line == 0 {
            t.start_char - prev_char
        } else {
            t.start_char
        };
        tokens.push(SemanticToken {
            delta_line,
            delta_start,
            length: t.length,
            token_type: t.token_type,
            token_modifiers_bitset: 0,
        });
        prev_line = t.line;
        prev_char = t.start_char;
    }
    tokens
}
