use lsp_types::{SemanticToken, SemanticTokenType, SemanticTokensLegend};
use sentinel_typechecker::type_checker::{SymbolId, TypeChecker};

pub const TOKEN_TYPES: &[SemanticTokenType] = &[
    SemanticTokenType::VARIABLE, // 0 — covers both locals and parameters; not distinguished yet
    SemanticTokenType::PROPERTY, // 1 — struct fields
];

pub fn legend() -> SemanticTokensLegend {
    SemanticTokensLegend {
        token_types: TOKEN_TYPES.to_vec(),
        token_modifiers: vec![],
    }
}

pub fn build_semantic_tokens(
    checker: &TypeChecker,
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
            SymbolId::Local(_) => 0u32,
            SymbolId::Field { .. } => 1u32,
            SymbolId::Item { .. } => continue, // usage sites not tracked; skip
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
