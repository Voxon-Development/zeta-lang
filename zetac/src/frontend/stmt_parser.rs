use std::collections::HashMap;
use std::iter::Peekable;
use std::vec::IntoIter;
use ir::bump::{AtomicBump};
use crate::ast::*;
use crate::frontend::macros::{MacroDef, MacroPatternNode};
use crate::frontend::tokens::{ParserError, Token, TokenType};

pub struct StmtParser {
    tokens: Peekable<IntoIter<Token, AtomicBump>>,
    errors: Vec<ParserError>
}

impl StmtParser {
    pub fn new(tokens: Vec<Token, AtomicBump>) -> StmtParser {
        StmtParser {
            tokens: tokens.into_iter().peekable(),
            errors: Vec::new()
        }
    }

    pub(crate) fn peek(&mut self) -> Option<Token> {
        self.tokens.peek().cloned()
    }

    #[inline]
    pub(crate) fn is_at_end(&mut self) -> bool {
        self.tokens.peek().is_none()
    }

    pub(crate) fn next(&mut self) -> Option<Token> {
        self.tokens.next()
    }

    pub fn parse_stmt(&mut self, statements: &mut Vec<Stmt, AtomicBump>, token: &Token, visibility: Option<Visibility>) {
        if let Some(expanded_stmt) = self.try_expand_macro() {
            statements.push(expanded_stmt);
            return;
        }

        match token.value.as_str() {
            "class" => statements.push(self.parse_class_decl(visibility)),
            "let" => statements.push(self.parse_let_stmt()), // allow `let` for inference
            "const" => statements.push(self.parse_let_stmt()),
            "interface" => {
                let interface_decl = self.parse_interface_decl(visibility);
                statements.push(interface_decl);
            },

            "return" => statements.push(self.parse_return_stmt()),
            "while" => statements.push(self.parse_while_stmt()),
            "if" => statements.push(self.parse_if_stmt()),
            "for" => statements.push(self.parse_for_stmt()),
            "break" => statements.push(self.parse_break_stmt()),
            "continue" => statements.push(self.parse_continue_stmt()),
            "import" => statements.push(self.parse_import_stmt()),
            "match" => statements.push(self.parse_match_stmt()),
            "effect" => statements.push(self.parse_effect_stmt()),

            _ => {
                // At this point, it's either:
                // 1. A function declaration like `public void main() {...}`
                // 2. A variable declaration like `mut Type name = ...`
                // 3. An expression statement

                if let Some(stmt) = self.try_parse_fun_decl(visibility) {
                    statements.push(stmt);
                } else if let Some(stmt) = self.try_parse_typed_var_decl() {
                    statements.push(stmt);
                } else {
                    let expr = self.parse_expr().expect("Expected expression");
                    statements.push(Stmt::ExprStmt(InternalExprStmt {
                        expr: Box::new(expr),
                    }));
                }
            }
        }
    }

    pub fn try_parse_fun_decl(&mut self, visibility: Option<Visibility>) -> Option<Stmt> {
        let return_type_token = self.tokens.peek()?.clone();

        if return_type_token.value_type != TokenType::Identifier {
            return None;
        }

        let return_type = parse_type(self.tokens.next()?); // consume return type

        let name_token = self.tokens.next()?;
        if name_token.value_type != TokenType::Identifier {
            return None;
        }
        let name = name_token.value;

        self.expect_token(TokenType::LParen, "Expected `(` after function name");

        let mut params = Vec::new();
        while let Some(token) = self.tokens.next() {
            if token.value_type == TokenType::RParen {
                break;
            }

            if token.value_type != TokenType::Identifier {
                panic!("Expected parameter name, got `{}`", token.value);
            }

            let param_name = token.value;

            self.expect_token(TokenType::Colon, "Expected `:` after parameter name");

            let param_type_token = self.tokens.next().expect("Expected type after `:`");
            let param_type = parse_type(param_type_token);

            params.push(Param {
                name: param_name,
                type_annotation: Some(param_type),
            });

            if let Some(peek) = self.tokens.peek() {
                if peek.value_type == TokenType::Comma {
                    self.tokens.next();
                } else if peek.value_type != TokenType::RParen {
                    panic!("Expected `,` or `)` after parameter");
                }
            }
        }

        let body = Some(self.parse_block());

        Some(Stmt::FuncDecl(FuncDecl {
            visibility,
            name,
            params,
            return_type: Some(return_type),
            effects: None,
            body,
        }))
    }

    pub fn parse_let_stmt(&mut self) -> Stmt {
        // Optional `mut` before `let`
        let mutability = self.check_token("mut", "Expected `mut` before `let` or a type");

        self.expect_token_str("let", "Expected `let` in variable declaration"); // consume `let`

        // Try to parse a type (optional for inference)
        let peek = self.tokens.peek().expect("Expected type or identifier after `let`");
        let (type_annotation, ident) = if peek.value_type == TokenType::Identifier {
            // Try to parse type and then variable name
            let type_token = self.tokens.next().unwrap(); // type
            let ident_token = self.tokens.next().expect("Expected identifier after type name");

            if ident_token.value_type != TokenType::Identifier {
                panic!("Expected identifier after type, got `{}`", ident_token.value);
            }

            (Some(parse_type(type_token)), ident_token.value)
        } else {
            panic!("Expected type and identifier after `let`");
        };

        // Expect `=`
        self.expect_token(TokenType::Equal, "Expected `=` after variable declaration");

        // Parse expression
        let value = self.parse_expr().expect("Expected expression after `=`");

        Stmt::Let(LetStmt {
            mutability,
            ident,
            type_annotation,
            value: Box::new(value),
        })
    }

    pub fn try_parse_typed_var_decl(&mut self) -> Option<Stmt> {
        // Parse optional `mut`
        let mut_token = self.check_token("mut", "Expected `mut` before `let` or a type");
        let type_token = self.tokens.peek()?.clone();
        if type_token.value_type != TokenType::Identifier {
            return None;
        }
        let type_ = parse_type(self.tokens.next()?);

        // Expect variable name
        let name_token = self.tokens.next()?;
        if name_token.value_type != TokenType::Identifier {
            panic!("Expected variable name after type");
        }
        let ident = name_token.value;

        self.expect_token(TokenType::Equal, "Expected `=` after variable declaration");

        let value = self.parse_expr().expect("Expected expression after `=`");

        Some(Stmt::Let(LetStmt {
            mutability: mut_token,
            ident,
            type_annotation: Some(type_),
            value: Box::new(value),
        }))
    }

    pub(crate) fn parse_const_stmt(&mut self) -> Stmt {
        self.expect_token(TokenType::Keyword, "Expected `const` in `const` statement"); // consume `let`

        let (mutability, ident) = self.get_variable_metadata();

        // Step 3: Optional `: type`
        let type_annotation: Type = if self.check_next_token(TokenType::Colon) {
            self.tokens.next(); // consume `:`
            let type_token = self.tokens.next().expect("Expected type after `:`");
            parse_type(type_token)
        } else {
            panic!("Expected `:` after identifier");
        };

        // Step 4: Expect `=`
        match self.tokens.next() {
            Some(token) if token.value_type == TokenType::Equal => {} // continue
            Some(token) => panic!("Expected `=` after identifier, got {:?}", token),
            None => panic!("Unexpected end of input after identifier"),
        }

        let expr = self.parse_expr().expect("Expected expression after `=`");

        Stmt::Const(ConstStmt {
            mutability,
            ident,
            type_annotation,
            value: Box::new(expr),
        })
    }

    fn get_variable_metadata(&mut self) -> (bool, String) {
        // Step 1: Optional `mut`
        let mut mutability = false;
        if let Some(next) = self.tokens.peek() {
            if next.value_type == TokenType::Keyword && next.value == "mut" {
                mutability = true;
                self.expect_token(TokenType::Keyword, "Expected `mut` in `let` statement"); // consume `mut`
            }
        }

        // Step 2: Identifier
        let ident = match self.tokens.next() {
            Some(token) if token.value_type == TokenType::Identifier => token.value,
            Some(token) => panic!("Expected identifier after `let`, got {:?}", token),
            None => panic!("Unexpected end of input after `let`"),
        };
        (mutability, ident)
    }

    pub(crate) fn parse_return_stmt(&mut self) -> Stmt {
        self.tokens.next(); // consume `return`

        // optional expression
        let value = match self.tokens.peek() {
            Some(token) if token.value_type == TokenType::Semicolon => {
                self.tokens.next(); // consume semicolon
                None
            }
            _ => {
                let expr = self.parse_expr().expect("Expected expression after `return`");
                self.expect_token(TokenType::Semicolon, "Expected `;` after return value");
                Some(Box::new(expr))
            }
        };


        Stmt::Return(ReturnStmt { value })
    }

    pub(crate) fn parse_while_stmt(&mut self) -> Stmt {
        self.tokens.next(); // consume `while`

        self.expect_token(TokenType::LParen, "Expected `(` in condition"); // consume `(`
        let condition = Box::new(self.parse_expr().expect("Expected condition"));
        self.expect_token(TokenType::RParen, "Expected `)` in condition"); // consume `)`

        let block = self.parse_block();

        Stmt::While(WhileStmt { condition, block })
    }

    pub(crate) fn parse_if_stmt(&mut self) -> Stmt {
        self.tokens.next(); // consume `if`

        self.expect_token(TokenType::LParen, "Expected `(` in condition"); // consume `(`
        let condition = self.parse_expr().expect("Expected condition");
        self.expect_token(TokenType::RParen, "Expected `)` in condition"); // consume `)`

        let then_branch = self.parse_block();

        // parse else block, but there could be an if after the else, indicating else if

        Stmt::If(IfStmt { condition, then_branch, else_branch: None })
    }

    pub(crate) fn parse_effect_stmt(&mut self) -> Stmt {
        self.tokens.next(); // consume `effect`
        let name = self.tokens.next().unwrap().value;
        let effect = self.tokens.next().unwrap().value;
        Stmt::EffectDecl(EffectDecl { name, effect })
    }

    pub(crate) fn parse_for_stmt(&mut self) -> Stmt {
        self.tokens.next(); // consume `for`

        // parsing this: for (let i = 0; i < 10; i++) { }
        self.expect_token(TokenType::LParen, "Expected `(` in for loop declaration"); // consume `(`
        // let_stmt could be optional, so we use an Option
        let let_stmt = match self.tokens.peek() {
            Some(token) if token.value_type == TokenType::Identifier => {
                let stmt = self.parse_let_stmt();
                let let_stmt = match stmt {
                    Stmt::Let(let_stmt) => let_stmt,
                    _ => panic!("Expected let statement after `for`"),
                };

                Some(let_stmt)
            },
            _ => None,
        };

        // condition could also be optional
        let condition = self.parse_next_expr();

        self.expect_token(TokenType::Semicolon, "Expected `;` in `for` loop"); // consume `;`
        let increment = self.parse_next_expr();

        self.expect_token(TokenType::RParen, "Expected `(` in for loop declaration"); // consume `)`

        let block = self.parse_block();

        Stmt::For(ForStmt { let_stmt, condition, increment, block })
    }

    fn parse_next_expr(&mut self) -> Option<Box<Expr>> {
        match self.tokens.peek() {
            Some(token) if token.value_type == TokenType::Semicolon => None,
            _ => Some(Box::new(self.parse_expr()?)),
        }
    }

    pub(crate) fn parse_match_stmt(&mut self) -> Stmt {
        self.tokens.next(); // consume `match`
        self.expect_token(TokenType::LParen, "Expected `(` in match statement declaration"); // consume `(`
        let expr = self.parse_expr().expect("Expected expression in match statement");
        self.expect_token(TokenType::RParen, "Expected `)` in match statement declaration"); // consume `)`

        let mut arms: Vec<MatchArm> = Vec::new();

        while let Some(token) = self.tokens.next() {
            if token.value_type == TokenType::RBrace {
                break;
            }

            let pattern = self.parse_pattern()
                .map_err(|e| panic!("Error parsing pattern: {}", e))
                .unwrap();
            self.tokens.next(); // consume `=>`
            let block = self.parse_block();

            arms.push(MatchArm { pattern, block });
        }

        Stmt::Match(MatchStmt { expr, arms })
    }

    pub(crate) fn parse_class_decl(&mut self, visibility: Option<Visibility>) -> Stmt {
        self.tokens.next(); // consume `class`

        let name = match self.tokens.next() {
            Some(token) if token.value_type == TokenType::Identifier => token.value,
            Some(token) => panic!("Expected identifier after `class`, got `{}`", token.value),
            None => panic!("Unexpected end of input after `class`"),
        };

        // we should also expect params after identifier
        self.expect_token(TokenType::LParen, "Expected `(` in class declaration"); // consume `(`
        let params = self.parse_params();

        let body = self.parse_block();

        Stmt::ClassDecl(ClassDecl { visibility, name, params, body })
    }

    pub(crate) fn parse_interface_decl(&mut self, visibility: Option<Visibility>) -> Stmt {
        self.expect_token_str("interface", "Expected `interface`");

        let name = match self.tokens.next() {
            Some(token) if token.value_type == TokenType::Identifier => token.value,
            Some(token) => panic!("Expected identifier after `interface`, got `{}`", token.value),
            None => panic!("Unexpected end of input after `interface`"),
        };

        let mut methods: Vec<FuncDecl> = Vec::new();
        let mut const_stmts: Vec<ConstStmt> = Vec::new();

        // Expect `{`
        match self.tokens.next() {
            Some(token) if token.value_type == TokenType::LBrace => {},
            Some(token) => panic!("Expected '{{', found {}", token.value),
            None => panic!("Unexpected end of input, expected '{{'"),
        }

        // Parse until `}`
        while let Some(token) = self.tokens.peek() {
            if token.value_type == TokenType::RBrace {
                self.tokens.next(); // consume `}`
                break;
            }
            
            // Expect an optional visibility
            let visibility = self.parse_visibility();
            
            // Parse function or const declaration directly
            if let Some(stmt) = self.try_parse_fun_decl(visibility) {
                if let Stmt::FuncDecl(func_decl) = stmt {
                    methods.push(func_decl);
                    continue;
                }
            }

            if let Some(stmt) = self.try_parse_typed_var_decl() {
                if let Stmt::Const(const_stmt) = stmt {
                    const_stmts.push(const_stmt);
                    continue;
                }
            }
            
            // If we get here, we couldn't parse a valid interface member
            panic!("Expected function or const declaration in interface, found {:?}", token);
        }

        Stmt::InterfaceDecl(InterfaceDecl { visibility, name, methods, const_stmts })
    }

    fn parse_params(&mut self) -> Option<Vec<Param>> {
        let mut params: Vec<Param> = Vec::new();

        while let Some(token) = self.tokens.next() {
            if token.value_type == TokenType::RParen {
                break;
            }

            let name = token.value;
            self.expect_token(TokenType::Colon, "Expected `:` in `for` loop"); // consume `:`
            let type_name = match self.tokens.next() {
                Some(token) if token.value_type == TokenType::Identifier  => token,
                Some(token) if token.value_type == TokenType::Int || token.value_type == TokenType::I8
                    || token.value_type == TokenType::I16 || token.value_type == TokenType::I32 || token.value_type == TokenType::I64
                    || token.value_type == TokenType::U8 || token.value_type == TokenType::U16 || token.value_type == TokenType::U32 || token.value_type == TokenType::U64
                    || token.value_type == TokenType::F32 || token.value_type == TokenType::F64 => token,
                Some(token) => panic!("Expected identifier after `:` in param, got `{}`", token),
                None => panic!("Unexpected end of input after `:` in param"),
            };

            params.push(Param {
                name,
                type_annotation: Some(parse_type(type_name))
            });

            // might be comma or might be RBrace
            if let Some(token) = self.tokens.next() {
                if token.value_type == TokenType::RParen {
                    break;
                }
            }
        }

        Some(params)
    }

    pub(crate) fn parse_import_stmt(&mut self) -> Stmt {
        self.tokens.next(); // consume `import`

        let path = match self.tokens.next() {
            Some(token) if token.value_type == TokenType::String => token.value,
            Some(token) => panic!("Expected string literal after `import`, got `{}`", token.value),
            None => panic!("Unexpected end of input after `import`"),
        };

        Stmt::Import(ImportStmt { path })
    }

    pub(crate) fn parse_break_stmt(&mut self) -> Stmt {
        Stmt::Break
    }

    pub(crate) fn parse_continue_stmt(&mut self) -> Stmt {
        Stmt::Continue
    }

    pub(crate) fn parse_block(&mut self) -> Block {
        let mut statements: Vec<Stmt, AtomicBump> = Vec::new_in(AtomicBump::new());

        // Expect `{`
        match self.tokens.next() {
            Some(token) if token.value_type == TokenType::LBrace => {},
            Some(token) => panic!("Expected '{{', found {}", token.value),
            None => panic!("Unexpected end of input, expected '{{'"),
        }

        // Parse until `}`
        while let Some(token) = self.tokens.peek() {
            if token.value_type == TokenType::RBrace {
                self.tokens.next(); // consume `}`
                break;
            }

            // Actually parse a statement
            let token = self.tokens.peek().unwrap().clone(); // clone to not consume yet
            self.parse_stmt(&mut statements, &token, None);
        }

        Block { block: statements }
    }

    #[inline]
    fn expect_token(&mut self, token_type: TokenType, error_message: &str) -> Token {
        match self.tokens.next() {
            Some(token) if token.value_type == token_type => token,
            Some(token) => {
                self.errors.push(ParserError {
                    message: format!("{}, got `{}`", error_message, token.value),
                    token
                });
                panic!("{}", error_message);


            },
            None => panic!("{}", error_message),
        }
    }
    
    #[inline]
    fn expect_token_str(&mut self, token_expected: &str, error_message: &str) -> Token {
        match self.tokens.next() {
            Some(token) if token.value == token_expected => token,
            Some(token) => {
                self.errors.push(ParserError {
                    message: format!("{}, got `{}`", error_message, token.value),
                    token
                });
                panic!("{}", error_message);
            },
            None => panic!("{}", error_message),
        }
    }
    
    #[inline]
    fn check_token(&mut self, token_type: &str, error_message: &str) -> bool {
        match self.tokens.peek() {
            Some(token) if token.value == token_type => {
                self.tokens.next();
                true
            },
            Some(token) => {
                self.errors.push(ParserError {
                    message: format!("{}, got `{}`", error_message, token.value),
                    token: token.clone()
                });
                false
            },
            None => false,
        }
    }

    fn parse_pattern(&mut self) -> Result<Pattern, String> {
        match self.tokens.peek() {
            Some(token) => match token.value_type {
                TokenType::Identifier => {
                    let token = self.tokens.next().unwrap();
                    if token.value == "_" {
                        Ok(Pattern::Wildcard)
                    } else {
                        Ok(Pattern::Ident(token.value))
                    }
                }
                TokenType::Int => {
                    let token = self.tokens.next().unwrap();
                    let number = token.value.parse::<i64>().map_err(|e| e.to_string())?;
                    Ok(Pattern::Number(number))
                }
                TokenType::String => {
                    let token = self.tokens.next().unwrap();
                    Ok(Pattern::String(token.value))
                }
                TokenType::LParen => {
                    self.tokens.next(); // Consume '('
                    let mut patterns = Vec::new();
                    while self.tokens.peek().is_some() && self.tokens.peek().unwrap().value_type != TokenType::RParen {
                        patterns.push(self.parse_pattern()?);
                        if let Some(token) = self.tokens.peek() {
                            if token.value_type == TokenType::Comma {
                                self.tokens.next(); // Consume ','
                            }
                        }
                    }
                    self.tokens.next(); // Consume ')'
                    Ok(Pattern::Tuple(patterns))
                }
                _ => Err(format!("Unexpected token in pattern: {:?}", token)),
            },
            None => Err("Unexpected end of input while parsing pattern".to_string()),
        }
    }

    pub fn parse_expr_internal(&mut self) -> Option<Expr> {
        match self.tokens.next() {
            Some(token) if token.value_type == TokenType::Identifier && self.tokens.peek().map_or(false, |t| t.value_type == TokenType::LBrace) => {
                let ident = token.value;
                self.expect_token(TokenType::LBrace, "Expected `{` after class name");

                let mut arguments = Vec::new();
                if self.tokens.peek().map_or(false, |t| t.value_type != TokenType::RBrace) {
                    loop {
                        arguments.push(self.parse_expr()?);
                        if self.tokens.peek().map_or(false, |t| t.value_type == TokenType::Comma) {
                            self.expect_token(TokenType::Comma, "Expected `,` after class init argument");
                        } else {
                            break;
                        }
                    }
                }

                self.expect_token(TokenType::RBrace, "Expected `}` after class init arguments");

                Some(Expr::ClassInit {
                    callee: Box::new(Expr::Ident(ident)),
                    arguments
                })
            }

            // binary

            Some(token) if self.tokens.peek().map_or(false, |t| t.value_type == TokenType::Plus) => {
                let lhs = match token.value_type {
                    TokenType::Identifier => Expr::Ident(token.value),
                    TokenType::Int | TokenType::I8 | TokenType::I16 | TokenType::I32 | TokenType::I64 | TokenType::U8 | TokenType::U16 | TokenType::U32 | TokenType::U64 => Expr::Number(token.value.parse::<i64>().unwrap()),
                    _ => panic!("Expected identifier or number"),
                };
                self.expect_token(TokenType::Plus, "Expected `+` after expression");
                let rhs = self.parse_expr().expect("Expected expression after `+`");
                Some(Expr::Binary {
                    left: Box::new(lhs),
                    op: Op::Add,
                    right: Box::new(rhs)
                })
            }

            Some(token) if token.value_type == TokenType::LParen => {
                let expr = self.parse_expr();
                self.expect_token(TokenType::RParen, "Expected `)` after expression");
                expr
            },
            Some(token) if token.value_type == TokenType::LBracket => {
                let expr = self.parse_expr();
                self.expect_token(TokenType::RBracket, "Expected `]` after expression");
                expr
            },
            Some(token) if token.value_type == TokenType::LBrace => {
                let expr = self.parse_expr();
                self.expect_token(TokenType::RBrace, "Expected `}` after expression");
                expr
            },

            // comparisons
            Some(token) if token.value_type == TokenType::Identifier && self.tokens.peek().map_or(false, |t| t.value_type == TokenType::IsEqual) => {
                let ident = token.value;
                self.expect_token(TokenType::IsEqual, "Expected `==` after identifier");
                let expr = self.parse_expr().expect("Expected expression after `==`");
                Some(Expr::Comparison {
                    lhs: Box::new(Expr::Ident(ident)),
                    op: ComparisonOp::Equal,
                    rhs: Box::new(expr)
                })
            },
            Some(token) if token.value_type == TokenType::Identifier && self.tokens.peek().map_or(false, |t| t.value_type == TokenType::IsNotEqual) => {
                let ident = token.value;
                self.expect_token(TokenType::IsNotEqual, "Expected `!=` after identifier");
                let expr = self.parse_expr().expect("Expected expression after `!=`");
                Some(Expr::Comparison {
                    lhs: Box::new(Expr::Ident(ident)),
                    op: ComparisonOp::NotEqual,
                    rhs: Box::new(expr)
                })
            },
            Some(token) if token.value_type == TokenType::Identifier && self.tokens.peek().map_or(false, |t| t.value_type == TokenType::LessThan) => {
                let ident = token.value;
                self.expect_token(TokenType::LessThan, "Expected `<` after identifier");
                let expr = self.parse_expr().expect("Expected expression after `<`");
                Some(Expr::Comparison {
                    lhs: Box::new(Expr::Ident(ident)),
                    op: ComparisonOp::LessThan,
                    rhs: Box::new(expr)
                })
            },
            Some(token) if token.value_type == TokenType::Identifier && self.tokens.peek().map_or(false, |t| t.value_type == TokenType::LessThanOrEqualTo) => {
                let ident = token.value;
                self.expect_token(TokenType::LessThanOrEqualTo, "Expected `<=` after identifier");
                let expr = self.parse_expr().expect("Expected expression after `<=`");
                Some(Expr::Comparison {
                    lhs: Box::new(Expr::Ident(ident)),
                    op: ComparisonOp::LessThanOrEqual,
                    rhs: Box::new(expr)
                })
            },
            Some(token) if token.value_type == TokenType::Identifier && self.tokens.peek().map_or(false, |t| t.value_type == TokenType::GreaterThan) => {
                let ident = token.value;
                self.expect_token(TokenType::GreaterThan, "Expected `>` after identifier");
                let expr = self.parse_expr().expect("Expected expression after `>`");
                Some(Expr::Comparison {
                    lhs: Box::new(Expr::Ident(ident)),
                    op: ComparisonOp::GreaterThan,
                    rhs: Box::new(expr)
                })
            },
            Some(token) if token.value_type == TokenType::Identifier && self.tokens.peek().map_or(false, |t| t.value_type == TokenType::GreaterThanOrEqualTo) => {
                let ident = token.value;
                self.expect_token(TokenType::GreaterThanOrEqualTo, "Expected `>=` after identifier");
                let expr = self.parse_expr().expect("Expected expression after `>=`");
                Some(Expr::Comparison {
                    lhs: Box::new(Expr::Ident(ident)),
                    op: ComparisonOp::GreaterThanOrEqual,
                    rhs: Box::new(expr)
                })
            },
            // detect function calls, like `foo()`
            Some(token) if token.value_type == TokenType::Identifier && self.tokens.peek().map_or(false, |t| t.value_type == TokenType::LParen) => {
                let ident = token.value;
                self.expect_token(TokenType::LParen, "Expected `(` after identifier");
                // make a loop to parse arguments, but no trailing comma and no errors if there are no arguments
                let mut arguments = Vec::new();
                if self.tokens.peek().map_or(false, |t| t.value_type != TokenType::RParen) {
                    loop {
                        arguments.push(self.parse_expr()?);
                        if self.tokens.peek().map_or(false, |t| t.value_type == TokenType::Comma) {
                            self.expect_token(TokenType::Comma, "Expected `,` after argument");
                        } else {
                            break;
                        }
                    }
                }
                self.expect_token(TokenType::RParen, "Expected `)` after arguments");
                Some(Expr::Call {
                    callee: Box::new(Expr::Ident(ident)),
                    arguments
                })
            },
            Some(token) if token.value_type == TokenType::LBracket => {
                // parse array literal
                let mut elements = Vec::new();
                loop {
                    elements.push(self.parse_expr()?);
                    if self.tokens.peek().map_or(false, |t| t.value_type == TokenType::Comma) {
                        self.expect_token(TokenType::Comma, "Expected `,` after element");
                    } else {
                        break;
                    }
                }
                self.expect_token(TokenType::RBracket, "Expected `]` after elements");
                Some(Expr::Array { elements })
            },
            // array indexing
            Some(token) if token.value_type == TokenType::Identifier && self.tokens.peek().map_or(false, |t| t.value_type == TokenType::LBracket) => {
                self.expect_token(TokenType::Identifier, "Expected identifier");

                let ident = token.value;
                self.expect_token(TokenType::LBracket, "Expected `[` after identifier");
                let index = Box::new(self.parse_expr()?);
                self.expect_token(TokenType::RBracket, "Expected `]` after index");
                Some(Expr::ArrayIndex { array: Box::new(Expr::Ident(ident)), index })
            },

            // assignments
            Some(token) if token.value_type == TokenType::Identifier && self.tokens.peek().map_or(false, |t| t.value_type == TokenType::AddAssign) => {
                let ident = token.value;
                self.expect_token(TokenType::AddAssign, "Expected `+=` after identifier");
                let value = self.parse_expr();
                Some(Expr::Assignment { lhs: Box::new(Expr::Ident(ident)), op: Op::AddAssign, rhs: Box::new(value?) })
            }
            Some(token) if token.value_type == TokenType::Identifier && self.tokens.peek().map_or(false, |t| t.value_type == TokenType::SubAssign) => {
                let ident = token.value;
                self.expect_token(TokenType::SubAssign, "Expected `-=` after identifier");
                let value = self.parse_expr();
                Some(Expr::Assignment { lhs: Box::new(Expr::Ident(ident)), op: Op::SubAssign, rhs: Box::new(value?) })
            }
            Some(token) if token.value_type == TokenType::Identifier && self.tokens.peek().map_or(false, |t| t.value_type == TokenType::MulAssign) => {
                let ident = token.value;
                self.expect_token(TokenType::MulAssign, "Expected `*=` after identifier");
                let value = self.parse_expr();
                Some(Expr::Assignment { lhs: Box::new(Expr::Ident(ident)), op: Op::MulAssign, rhs: Box::new(value?) })
            }
            Some(token) if token.value_type == TokenType::Identifier && self.tokens.peek().map_or(false, |t| t.value_type == TokenType::ModAssign) => {
                let ident = token.value;
                self.expect_token(TokenType::ModAssign, "Expected `%=` after identifier");
                let value = self.parse_expr();
                Some(Expr::Assignment { lhs: Box::new(Expr::Ident(ident)), op: Op::ModAssign, rhs: Box::new(value?) })
            }
            Some(token) if token.value_type == TokenType::Identifier && self.tokens.peek().map_or(false, |t| t.value_type == TokenType::PowAssign) => {
                let ident = token.value;
                self.expect_token(TokenType::PowAssign, "Expected `**=` after identifier");
                let value = self.parse_expr();
                Some(Expr::Assignment { lhs: Box::new(Expr::Ident(ident)), op: Op::PowAssign, rhs: Box::new(value?) })
            }
            Some(token) if token.value_type == TokenType::Identifier && self.tokens.peek().map_or(false, |t| t.value_type == TokenType::BitAndAssign) => {
                let ident = token.value;
                self.expect_token(TokenType::BitAndAssign, "Expected `&=` after identifier");
                let value = self.parse_expr();
                Some(Expr::Assignment { lhs: Box::new(Expr::Ident(ident)), op: Op::BitAndAssign, rhs: Box::new(value?) })
            }
            Some(token) if token.value_type == TokenType::Identifier && self.tokens.peek().map_or(false, |t| t.value_type == TokenType::BitOrAssign) => {
                let ident = token.value;
                self.expect_token(TokenType::BitOrAssign, "Expected `|=` after identifier");
                let value = self.parse_expr();
                Some(Expr::Assignment { lhs: Box::new(Expr::Ident(ident)), op: Op::BitOrAssign, rhs: Box::new(value?) })
            }
            Some(token) if token.value_type == TokenType::Identifier && self.tokens.peek().map_or(false, |t| t.value_type == TokenType::ShlAssign) => {
                let ident = token.value;
                self.expect_token(TokenType::ShlAssign, "Expected `<<=` after identifier");
                let value = self.parse_expr();
                Some(Expr::Assignment { lhs: Box::new(Expr::Ident(ident)), op: Op::ShlAssign, rhs: Box::new(value?) })
            }
            Some(token) if token.value_type == TokenType::Identifier && self.tokens.peek().map_or(false, |t| t.value_type == TokenType::ShrAssign) => {
                let ident = token.value;
                self.expect_token(TokenType::ShrAssign, "Expected `>>=` after identifier");
                let value = self.parse_expr();
                Some(Expr::Assignment { lhs: Box::new(Expr::Ident(ident)), op: Op::ShrAssign, rhs: Box::new(value?) })
            }
            Some(token) if token.value_type == TokenType::Identifier && self.tokens.peek().map_or(false, |t| t.value_type == TokenType::BitXorAssign) => {
                let ident = token.value;
                self.expect_token(TokenType::BitXorAssign, "Expected `^=` after identifier");
                let value = self.parse_expr();
                Some(Expr::Assignment { lhs: Box::new(Expr::Ident(ident)), op: Op::BitXorAssign, rhs: Box::new(value?) })
            }
            Some(token) if token.value_type == TokenType::Identifier && self.tokens.peek().map_or(false, |t| t.value_type == TokenType::DivAssign) => {
                let ident = token.value;
                self.expect_token(TokenType::DivAssign, "Expected `/=` after identifier");
                let value = self.parse_expr();
                Some(Expr::Assignment { lhs: Box::new(Expr::Ident(ident)), op: Op::DivAssign, rhs: Box::new(value?) })
            }
            Some(token) if token.value_type == TokenType::Identifier && self.tokens.peek().map_or(false, |t| t.value_type == TokenType::Equal) => {
                let ident = token.value;
                self.expect_token(TokenType::Equal, "Expected `=` after identifier");
                let value = self.parse_expr();
                Some(Expr::Assignment { lhs: Box::new(Expr::Ident(ident)), op: Op::Assign, rhs: Box::new(value?) })
            }

            Some(token) if token.value_type == TokenType::AtSymbol => {
                // parse something like @{Region}, then it's Expr::RegionInit
                self.expect_token(TokenType::LBrace, "Expected `{` after `@`");
                match self.tokens.next() {
                    Some(ident) if ident.value == "Region" => {},
                    Some(token) => self.errors.push(ParserError { message: format!("Expected \"Region\" got {}", token.value), token }),
                    None => self.errors.push(ParserError {
                        message: "Expected `Ident` after `@` but token list ended.".to_string(), token
                    })
                }
                self.expect_token(TokenType::RBrace, "Expected `}` after `{`");
                Some(Expr::RegionInit)
            }
            Some(token) if token.value_type == TokenType::Int => Some(Expr::Number(token.value.parse().unwrap())),
            Some(token) if token.value_type == TokenType::String => Some(Expr::String(token.value)),
            Some(token) if token.value_type == TokenType::Identifier => Some(Expr::Ident(token.value)),

            Some(token) => {
                self.errors.push(ParserError {
                    message: format!("Unexpected token: Type: {:?}, Value: {}", token.value_type, token.value),
                    token,
                });
                None
            }
            None => None,
        }
    }

    pub fn parse_expr(&mut self) -> Option<Expr> {
        let mut expr = self.parse_expr_internal()?;

        // postfix loop (e.g. `.field`)
        loop {
            let peek_token = self.tokens.peek()?.clone();

            if peek_token.value_type == TokenType::Dot {
                self.tokens.next(); // consume `.`
                match self.tokens.next() {
                    Some(field_token) if field_token.value_type == TokenType::Identifier => {
                        expr = Expr::FieldAccess {
                            object: Box::new(expr),
                            field: field_token.value,
                        };
                    }
                    Some(other) => {
                        self.errors.push(ParserError {
                            message: "Expected field name after `.`".to_string(),
                            token: other,
                        });
                        return None;
                    }
                    None => {
                        self.errors.push(ParserError {
                            message: "Unexpected end after `.`".to_string(),
                            token: peek_token,
                        });
                        return None;
                    }
                }
            } else {
                break;
            }
        }

        // assignment detection
        let peek_token = self.tokens.peek().cloned();
        if let Some(token) = peek_token {
            let op = match token.value_type {
                TokenType::Equal => Op::Assign,
                TokenType::AddAssign => Op::AddAssign,
                TokenType::SubAssign => Op::SubAssign,
                TokenType::MulAssign => Op::MulAssign,
                TokenType::DivAssign => Op::DivAssign,
                TokenType::ModAssign => Op::ModAssign,
                TokenType::PowAssign => Op::PowAssign,
                TokenType::BitAndAssign => Op::BitAndAssign,
                TokenType::BitOrAssign => Op::BitOrAssign,
                TokenType::BitXorAssign => Op::BitXorAssign,
                TokenType::ShlAssign => Op::ShlAssign,
                TokenType::ShrAssign => Op::ShrAssign,
                _ => return Some(expr),
            };

            self.tokens.next(); // consume the assign token
            let rhs = self.parse_expr()?;
            return Some(Expr::Assignment {
                lhs: Box::new(expr),
                op,
                rhs: Box::new(rhs),
            });
        }

        Some(expr)
    }


    pub fn parse_visibility(&mut self) -> Option<Visibility> {
        match self.tokens.peek() {
            Some(token) if token.value == "public" => {
                self.tokens.next();
                Some(Visibility::Public)
            }
            Some(token) if token.value == "private" => {
                self.tokens.next();
                Some(Visibility::Private)
            }
            Some(token) if token.value == "protected" => {
                self.tokens.next();
                Some(Visibility::Protected)
            }
            _ => None,
        }
    }

    fn check_next_token(&mut self, expected_type: TokenType) -> bool {
        self.tokens.peek().map_or(false, |t| t.value_type == expected_type)
    }

    pub fn try_expand_macro(&mut self) -> Option<Stmt> {
        let checkpoint = self.tokens.clone();

        for m in all_macros() {
            if let Some(bindings) = self.match_macro(&m.pattern) {
                return Some((m.expansion_ast)(bindings));
            } else {
                self.tokens = checkpoint.clone();
            }
        }

        None
    }

    fn match_macro(&mut self, pattern: &MacroPatternNode) -> Option<HashMap<String, Expr>> {
        let mut bindings = HashMap::new();

        match pattern {
            MacroPatternNode::Sequence(seq) => {
                for part in seq {
                    match part {
                        MacroPatternNode::Ident(name) => {
                            if let Some(token) = self.next() {
                                if token.token_type == TokenType::Identifier {
                                    bindings.insert(name.clone(), Expr::Ident(token.lexeme.clone()));
                                } else {
                                    return None;
                                }
                            } else {
                                return None;
                            }
                        }
                        MacroPatternNode::Expr(name) => {
                            if let Some(expr) = self.parse_expr() {
                                bindings.insert(name.clone(), expr);
                            } else {
                                return None;
                            }
                        }
                        MacroPatternNode::Token(expected) => {
                            if let Some(token) = self.next() {
                                if &token.lexeme != expected {
                                    return None;
                                }
                            } else {
                                return None;
                            }
                        }
                        _ => return None,
                    }
                }
                Some(bindings)
            }
            _ => None,
        }
    }
}

#[inline(always)]
fn parse_type(token: Token) -> Type {
    match token.value_type {
        TokenType::U8 => Type::U8,
        TokenType::U16 => Type::U16,
        TokenType::U32 => Type::U32,
        TokenType::U64 => Type::U64,
        TokenType::U128 => Type::U128,
        TokenType::I8 => Type::I8,
        TokenType::I16 => Type::I16,
        TokenType::I32 => Type::I32,
        TokenType::I64 => Type::I64,
        TokenType::I128 => Type::I128,
        TokenType::F32 => Type::F32,
        TokenType::F64 => Type::F64,
        TokenType::String => Type::String,
        TokenType::Boolean => Type::Boolean,
        TokenType::Keyword => {
            match token.value.as_str() {
                "void" => Type::Void,
                _ => todo!()
            }
        }
        TokenType::Identifier => Type::Class(token.value),
        _ => todo!()
    }
}