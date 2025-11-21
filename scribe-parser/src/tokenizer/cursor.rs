use crate::tokenizer::tokens::{TokenKind, Tokens};
use ir::hir::StrId;
use ir::span::SourceSpan;

pub struct TokenCursor<'a> {
    kinds: &'a [TokenKind],
    texts: &'a [StrId],
    spans: &'a [SourceSpan<'a>],
    kind_idx: usize,
    text_idx: usize,
    span_idx: usize,
}

#[derive(Clone, Copy)]
pub struct TokenCursorCheckpoint {
    kind_idx: usize,
    text_idx: usize,
    span_idx: usize,
}

impl<'a> TokenCursor<'a> {
    pub fn from_tokens(tokens: &'a Tokens<'a>) -> Self {
        TokenCursor {
            kinds: &tokens.kinds,
            texts: &tokens.texts,
            spans: &tokens.spans,
            kind_idx: 0,
            text_idx: 0,
            span_idx: 0,
        }
    }

    #[inline]
    pub fn at_end(&self) -> bool {
        self.kind_idx >= self.kinds.len()
    }

    #[inline]
    pub fn peek_kind(&self) -> Option<TokenKind> {
        self.kinds.get(self.kind_idx).copied()
    }

    #[inline]
    pub fn peek_kind_n(&self, n: usize) -> Option<TokenKind> {
        self.kinds.get(self.kind_idx + n).copied()
    }

    #[inline]
    pub fn peek_text(&self) -> Option<StrId> {
        self.texts.get(self.text_idx).copied()
    }

    #[inline]
    pub fn peek_span(&self) -> Option<SourceSpan<'a>> {
        self.spans.get(self.span_idx).copied()
    }

    #[inline]
    pub fn current_span(&self) -> SourceSpan<'a> {
        self.peek_span().unwrap_or_else(|| SourceSpan::new("", 0, 0))
    }

    /// Advance one token-kind (and if the token had text, advance text index as well).
    /// Note: we don't try to infer which TokenKinds carry text here; callers should only
    /// call `advance_with_text()` when they know a text token was consumed.
    #[inline]
    pub fn advance_kind(&mut self) {
        self.kind_idx = self.kind_idx.saturating_add(1);
        self.span_idx = self.span_idx.saturating_add(1);
    }

    /// Advance kind and text (for tokens that carry a text index).
    #[inline]
    pub fn advance_with_text(&mut self) {
        self.kind_idx = self.kind_idx.saturating_add(1);
        self.text_idx = self.text_idx.saturating_add(1);
        self.span_idx = self.span_idx.saturating_add(1);
    }

    #[inline]
    pub fn expect_kind(&mut self, expected: TokenKind) -> bool {
        match self.peek_kind() {
            Some(k) if k == expected => {
                // If expected is a text-carrying token kind, caller should call `advance_with_text`.
                self.advance_kind();
                true
            }
            _ => false,
        }
    }

    #[inline]
    pub fn consume_ident(&mut self) -> Option<StrId> {
        match self.peek_kind() {
            Some(TokenKind::Ident) => {
                let t = self.peek_text();
                self.advance_with_text();
                t
            }
            _ => None,
        }
    }

    #[inline]
    pub fn consume_string(&mut self) -> Option<StrId> {
        match self.peek_kind() {
            Some(TokenKind::String) => {
                let t = self.peek_text();
                self.advance_with_text();
                t
            }
            _ => None,
        }
    }

    #[inline]
    pub fn consume_number(&mut self) -> Option<StrId> {
        match self.peek_kind() {
            Some(TokenKind::Number) => {
                let t = self.peek_text();
                self.advance_with_text();
                t
            }
            _ => None,
        }
    }

    #[inline]
    pub fn consume_decimal(&mut self) -> Option<StrId> {
        match self.peek_kind() {
            Some(TokenKind::Decimal) => {
                let t = self.peek_text();
                self.advance_with_text();
                t
            }
            _ => None,
        }
    }

    /// Helper: consume a specific kind and advance text index if that kind carries text.
    /// We keep it simple: treat Ident and String as "textful" tokens.
    #[inline]
    pub fn consume_kind(&mut self, kind: TokenKind) -> bool {
        match self.peek_kind() {
            Some(k) if k == kind => {
                match kind {
                    TokenKind::Ident | TokenKind::String | TokenKind::Number | TokenKind::Decimal => self.advance_with_text(),
                    _ => self.advance_kind(),
                }
                true
            }
            _ => false,
        }
    }

    /// Create a checkpoint of the current cursor position
    #[inline]
    pub fn checkpoint(&self) -> TokenCursorCheckpoint {
        TokenCursorCheckpoint {
            kind_idx: self.kind_idx,
            text_idx: self.text_idx,
            span_idx: self.span_idx,
        }
    }

    /// Restore the cursor to a previous checkpoint
    #[inline]
    pub fn restore(&mut self, checkpoint: TokenCursorCheckpoint) {
        self.kind_idx = checkpoint.kind_idx;
        self.text_idx = checkpoint.text_idx;
        self.span_idx = checkpoint.span_idx;
    }
}