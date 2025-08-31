use crate::arena_ast::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum TokenKind {
    Ident,
    Number,
    String,
    Boolean,
    LParen,
    RParen,
    LBrace,
    RBrace,
    Comma,
    Colon,
    Semicolon,
    Arrow,
    Equal,
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Dot,
    EOF
}

pub struct ArenaParser<'a> {
    src: &'a str,
    current: usize,
    tokens: Vec<(TokenKind, std::ops::Range<usize>)>,
}

impl<'a> ArenaParser<'a> {
    pub fn new(src: &'a str) -> Self { Self { src, current: 0, tokens: Vec::new() } }

    fn is_ident_start(c: char) -> bool { c.is_ascii_alphabetic() || c == '_' }
    fn is_ident_continue(c: char) -> bool { c.is_ascii_alphanumeric() || c == '_' }

    pub fn lex(&mut self) {
        let b = self.src.as_bytes();
        let mut i = 0; let n = b.len();
        while i < n {
            let c = b[i] as char;
            match c {
                ' ' | '\t' | '\r' | '\n' => { i += 1; },
                '(' => { self.tokens.push((TokenKind::LParen, i..i + 1)); i += 1; },
                ')' => { self.tokens.push((TokenKind::RParen, i..i + 1)); i += 1; },
                '{' => { self.tokens.push((TokenKind::LBrace, i..i + 1)); i += 1; },
                '}' => { self.tokens.push((TokenKind::RBrace, i..i + 1)); i += 1; },
                ',' => { self.tokens.push((TokenKind::Comma, i..i + 1)); i += 1; },
                ':' => { self.tokens.push((TokenKind::Colon, i..i + 1)); i += 1; },
                ';' => { self.tokens.push((TokenKind::Semicolon, i..i + 1)); i += 1; },
                '.' => { self.tokens.push((TokenKind::Dot, i..i + 1)); i += 1; },
                '+' => { self.tokens.push((TokenKind::Plus, i..i + 1)); i += 1; },
                '-' => { 
                    if i + 1 < n && b[i + 1] == b'>' { 
                        self.tokens.push((TokenKind::Arrow, i..i + 2));
                        i += 2; 
                    } else { 
                        self.tokens.push((TokenKind::Minus, i..i + 1));
                        i += 1; 
                    }
                },
                '*' => { self.tokens.push((TokenKind::Star, i..i + 1)); i += 1; },
                '/' => {
                    if i + 1 < n && b[i + 1] == b'/' { // line comment
                        i += 2;
                        while i < n && b[i] != b'\n' { i += 1; }
                    } else {
                        self.tokens.push((TokenKind::Slash, i..i + 1));
                        i += 1;
                    }
                },
                '%' => { self.tokens.push((TokenKind::Percent, i..i + 1)); i += 1; },
                '=' => { self.tokens.push((TokenKind::Equal, i..i + 1)); i += 1; },
                '"' => {
                    let start = i;
                    i += 1;
                    while i < n {
                        if b[i] == b'"' {
                            i += 1;
                            break;
                        } else if b[i] == b'\\' && i + 1 < n {
                            i += 2;
                        } else {
                            i += 1;
                        }
                    }
                    self.tokens.push((TokenKind::String, start..i));
                }
                _ => {
                    if Self::is_ident_start(c) {
                        let start = i; i += 1; while i<n && Self::is_ident_continue(b[i] as char) { i += 1; }
                        self.tokens.push((TokenKind::Ident, start..i));
                    } else if c.is_ascii_digit() {
                        let start = i; i += 1; while i<n && (b[i] as char).is_ascii_digit() { i += 1; }
                        self.tokens.push((TokenKind::Number, start..i));
                    } else { i += 1; }
                }
            }
        }
        self.tokens.push((TokenKind::EOF, n..n));
    }

    fn peek(&self, k: usize) -> TokenKind {
        self.tokens.get(self.current +k)
            .map(|t| t.0)
            .unwrap_or(TokenKind::EOF)
    }

    fn at(&self) -> TokenKind {
        self.peek(0)
    }

    fn bump(&mut self) {
        if self.current < self.tokens.len() {
            self.current += 1;
        }
    }

    fn text(&self, idx: usize) -> &str {
        &self.src[self.tokens[idx].1.clone()]
    }

    fn cur_text(&self) -> &str {
        self.text(self.current)
    }

    fn expect(&mut self, kind: TokenKind) -> bool { if self.at() == kind { self.bump(); true } else { false } }

    #[inline]
    fn is_kw(&self, kw: &str) -> bool { self.at()==TokenKind::Ident && self.cur_text()==kw }

    #[inline]
    fn consume_kw(&mut self, kw: &str) -> bool { if self.is_kw(kw) { self.bump(); true } else { false } }

    #[inline]
    fn eat_ident_intern(&mut self, arenas: &mut AstArenas) -> Option<zetaruntime::string_pool::VmString> {
        if self.at()==TokenKind::Ident { let s = self.cur_text().to_string(); self.bump(); Some(arenas.intern_str(&s)) } else { None }
    }

    pub fn parse_program(&mut self, arenas: &mut AstArenas) -> Vec<StmtId> {
        self.lex();
        let mut out = Vec::new();
        while self.at()!= TokenKind::EOF {
            match self.at() {
                TokenKind::Semicolon => { self.bump(); }
                _ => {
                    if let Some(s) = self.parse_stmt(arenas) { out.push(s); }
                    else { // sync: skip token
                        self.bump();
                    }
                }
            }
        }
        out
    }

    fn parse_stmt(&mut self, arenas: &mut AstArenas) -> Option<StmtId> {
        // try function decl first when an identifier-like token starts a statement
        if self.at() == TokenKind::Ident {
            if let Some(fun) = self.parse_fun_decl(arenas) { return Some(fun); }
        }
        if self.at()== TokenKind::Ident {
            let t = self.cur_text();
            if t == "let" {
                self.bump();
                let mutable = if self.at()== TokenKind::Ident && self.cur_text()=="mut" { self.bump(); true } else { false };
                let name = if self.at()== TokenKind::Ident { let s = self.cur_text().to_owned(); self.bump(); arenas.intern_str(&s) } else { arenas.intern_str("_") };
                let ty = if self.at()== TokenKind::Colon { self.bump(); self.parse_type(arenas) } else { None };
                let _eq = self.expect(TokenKind::Equal);
                let value = self.parse_expr_bp(arenas, 0)?;
                let _ = self.expect(TokenKind::Semicolon);
                return Some(arenas.push_stmt(StmtNode::Let { mutable, name, ty, value }));
            } else if t == "return" {
                self.bump();
                let value = if self.at() == TokenKind::Semicolon { None } else { Some(self.parse_expr_bp(arenas, 0)?) };
                let _ = self.expect(TokenKind::Semicolon);
                return Some(arenas.push_stmt(StmtNode::Return { value }));
            } else if t == "import" {
                self.bump();
                if self.at() == TokenKind::String { let s = arenas.intern_str(self.cur_text().trim_matches('"')); self.bump(); let _ = self.expect(TokenKind::Semicolon); return Some(arenas.push_stmt(StmtNode::Import { path: s })); }
            }
        }
        // fallback: expression statement until semicolon
        let e = self.parse_expr_bp(arenas, 0)?;
        let _ = self.expect(TokenKind::Semicolon);
        Some(arenas.push_stmt(StmtNode::Expr { expr: e }))
    }

    fn parse_type(&mut self, arenas: &mut AstArenas) -> Option<TypeId> {
        match self.at() {
            TokenKind::Ident => {
                let t = self.cur_text();
                let ty = match t {
                    "i8" => TypeNode::I8,
                    "u8" => TypeNode::U8,
                    "i16" => TypeNode::I16,
                    "u16" => TypeNode::U16,
                    "i32" => TypeNode::I32,
                    "u32" => TypeNode::U32,
                    "i64" => TypeNode::I64,
                    "u64" => TypeNode::U64,
                    "i128" => TypeNode::I128,
                    "u128" => TypeNode::U128,
                    "f64" => TypeNode::F64,
                    "f32" => TypeNode::F32,
                    "boolean" => TypeNode::Boolean,
                    "str" => TypeNode::String,
                    _ => TypeNode::Class(arenas.intern_str(t)),
                };
                self.bump();
                Some(arenas.push_type(ty))
            }
            _ => None,
        }
    }

    // ========== Function Decls ==========
    fn parse_fun_decl(&mut self, arenas: &mut AstArenas) -> Option<StmtId> {
        // visibility? unsafe? static? extern?
        let mut save = self.current;
        let mut visibility = Visibility::Public;
        let mut is_unsafe = false;
        let mut is_static = false;
        let mut is_extern = false;

        // consume modifiers in any order
        loop {
            if self.is_kw("public") { visibility = Visibility::Public; self.bump(); continue; }
            if self.is_kw("private") { visibility = Visibility::Private; self.bump(); continue; }
            if self.is_kw("internal") { visibility = Visibility::Internal; self.bump(); continue; }
            if self.is_kw("unsafe") { is_unsafe = true; self.bump(); continue; }
            if self.is_kw("static") { is_static = true; self.bump(); continue; }
            if self.is_kw("extern") { is_extern = true; self.bump(); /* optional string literal for abi, ignore */ if self.at()==TokenKind::String { self.bump(); } continue; }
            break;
        }

        // function name must follow
        let Some(name) = self.eat_ident_intern(arenas) else { self.current = save; return None; };

        // param list
        if self.at()!=TokenKind::LParen { self.current = save; return None; }
        self.bump();
        let mut params: Vec<ParamId> = Vec::new();
        if self.at()!=TokenKind::RParen {
            loop {
                // param: [visibility] ident [: type]
                let mut p_vis = Visibility::Private;
                if self.is_kw("public") { p_vis=Visibility::Public; self.bump(); }
                else if self.is_kw("private") { p_vis=Visibility::Private; self.bump(); }
                else if self.is_kw("internal") { p_vis=Visibility::Internal; self.bump(); }
                let Some(pname) = self.eat_ident_intern(arenas) else { break; };
                let p_ty = if self.at()==TokenKind::Colon { self.bump(); self.parse_type(arenas) } else { None };
                let pid = arenas.push_param(ParamNode { name: pname, ty: p_ty, visibility: p_vis });
                params.push(pid);
                if self.at()==TokenKind::Comma { self.bump(); continue; } else { break; }
            }
        }
        let _ = self.expect(TokenKind::RParen);

        // return type: optional -> Type
        let mut ret = None;
        if self.at()==TokenKind::Arrow { self.bump(); ret = self.parse_type(arenas); }

        // body: either block {...} or semicolon -> extern/forward decl
        let body: Option<Vec<StmtId>> = if self.at()==TokenKind::LBrace {
            Some(self.parse_block(arenas))
        } else { let _ = self.expect(TokenKind::Semicolon); None };

        let decl = StmtNode::FuncDecl(FuncDecl { visibility, is_static, is_unsafe, is_extern, name, params, return_type: ret, body });
        Some(arenas.push_stmt(decl))
    }

    fn parse_block(&mut self, arenas: &mut AstArenas) -> Vec<StmtId> {
        let mut items = Vec::new();
        let _ = self.expect(TokenKind::LBrace);
        while self.at()!=TokenKind::RBrace && self.at()!=TokenKind::EOF {
            if self.at()==TokenKind::Semicolon { self.bump(); continue; }
            if let Some(s) = self.parse_stmt(arenas) { items.push(s); } else { self.bump(); }
        }
        let _ = self.expect(TokenKind::RBrace);
        items
    }

    fn precedence(tok: TokenKind) -> u8 {
        match tok {
            TokenKind::Plus | TokenKind::Minus => 10,
            TokenKind::Star | TokenKind::Slash | TokenKind::Percent => 20,
            TokenKind::Equal => 1,
            _ => 0,
        }
    }

    fn parse_expr_bp(&mut self, arenas: &mut AstArenas, min_bp: u8) -> Option<ExprId> {
        let mut lhs = self.parse_atom(arenas)?;
        loop {
            let op = self.at();
            let prec = Self::precedence(op);
            if prec < min_bp { break; }
            if op== TokenKind::Equal {
                self.bump();
                let rhs = self.parse_expr_bp(arenas, 1)?;
                let eq = arenas.intern_str("=");
                lhs = arenas.push_expr(ExprNode::Assignment { target: lhs, op: Some(BinaryOp::Eq), value: rhs });
                continue;
            }

            let r_bp = prec + 1;
            self.bump();
            let rhs = self.parse_expr_bp(arenas, r_bp)?;
            let bop = match op { TokenKind::Plus => BinaryOp::Add, TokenKind::Minus => BinaryOp::Sub, TokenKind::Star => BinaryOp::Mul, TokenKind::Slash => BinaryOp::Div, TokenKind::Percent => BinaryOp::Mod, _ => break };
            lhs = arenas.push_expr(ExprNode::Binary { left: lhs, op: bop, right: rhs });
        }
        Some(lhs)
    }

    fn parse_atom(&mut self, arenas: &mut AstArenas) -> Option<ExprId> {
        match self.at() {
            TokenKind::Number => {
                let n: i64 = self.cur_text().parse().ok()?;
                self.bump();
                Some(arenas.push_expr(ExprNode::Number(n)))
            }
            TokenKind::String => {
                let s = arenas.intern_str(self.cur_text().trim_matches('"'));
                self.bump();
                Some(arenas.push_expr(ExprNode::String(s)))
            }
            TokenKind::Ident => {
                let ident_text = self.cur_text().to_owned(); self.bump();
                if ident_text=="true" { return Some(arenas.push_expr(ExprNode::Boolean(true))); }
                if ident_text=="false" { return Some(arenas.push_expr(ExprNode::Boolean(false))); }
                let name = arenas.intern_str(&ident_text);
                let mut e = arenas.push_expr(ExprNode::Ident(name));
                loop {
                    match self.at() {
                        TokenKind::LParen => {
                            self.bump();
                            let mut args = Vec::new();
                            if self.at() != TokenKind::RParen {
                                loop {
                                    if let Some(arg) = self.parse_expr_bp(arenas, 0) {
                                        args.push(arg);
                                    } else if !self.expect(TokenKind::Comma) {
                                        break;
                                    }
                                }
                            }
                            let _ = self.expect(TokenKind::RParen);
                            e = arenas.push_expr(ExprNode::Call { callee: e, args });
                        }
                        TokenKind::Dot => {
                            self.bump();
                            if self.at()== TokenKind::Ident { let f = arenas.intern_str(self.cur_text()); self.bump(); e = arenas.push_expr(ExprNode::Get { object: e, field: f }); }
                        }
                        _ => break,
                    }
                }
                Some(e)
            }
            TokenKind::LParen => { self.bump(); let e = self.parse_expr_bp(arenas, 0)?; let _ = self.expect(TokenKind::RParen); Some(e) }
            _ => None,
        }
    }
}

pub fn parse_program_to_ast(src: &str) -> ProgramAst {
    let mut prog = ProgramAst::new();
    let mut p = ArenaParser::new(src);
    let root = p.parse_program(&mut prog.arenas);
    prog.stmts = root;
    prog
}
