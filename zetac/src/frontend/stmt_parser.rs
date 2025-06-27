use std::iter::Peekable;
use crate::ast::*;
use crate::frontend::tokens::{ParserError, Token, TokenType};

pub struct StmtParser<I: Iterator<Item = Token> + Clone> {
    tokens: Peekable<I>,
    errors: Vec<ParserError>
}

impl<I: Iterator<Item = Token> + Clone> StmtParser<I> {
    #[inline]
    pub fn new(tokens: I) -> StmtParser<I> {
        StmtParser {
            tokens: tokens.peekable(),
            errors: Vec::new()
        }
    }

    pub(crate) fn peek(&mut self) -> Option<Token> {
        self.tokens.peek().cloned()
    }

    pub(crate) fn is_at_end(&mut self) -> bool {
        self.tokens.peek().is_none()
    }

    pub(crate) fn next(&mut self) -> Option<Token> {
        self.tokens.next()
    }

    pub fn parse_stmt(&mut self, statements: &mut Vec<Stmt>, token: &Token, visibility: Option<Visibility>) {
        match token.value.as_str() {
            "fun" => statements.push(self.parse_fun_decl(visibility)),
            "class" => statements.push(self.parse_class_decl(visibility)),
            "let" => statements.push(self.parse_let_stmt()),
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
                let expr = self.parse_expr();
                match expr {
                    Some(expr) => statements.push(Stmt::ExprStmt(InternalExprStmt {
                        expr: Box::new(expr),
                    })),
                    None => panic!("Unexpected token: {}", token),
                }
            }
        }
    }

    pub(crate) fn parse_fun_decl(&mut self, visibility: Option<Visibility>) -> Stmt {
        self.expect_token(TokenType::Keyword, "Expected `fun` in function declaration"); // consume `fun`
        // The thing to parse:
        // functionName(arg1: type1, arg2: type2, ...): returnType {
        //     body
        // }

        let mut params: Vec<Param> = Vec::new();


        // Step 2: Function name
        let name = match self.tokens.next() {
            Some(token) if token.value_type == TokenType::Identifier => token.value,
            Some(token) => panic!("Expected identifier after `fun`, got {:?}", token),
            None => panic!("Unexpected end of input after `fun`"),
        };

        // Step 3: Parameters
        match self.tokens.next() {
            Some(token) if token.value_type == TokenType::LParen => {} // continue
            Some(token) => panic!("Expected `(` after function name, got `{}`", token.value),
            None => panic!("Unexpected end of input after function name"),
        }

        while let Some(token) = self.tokens.next() {
            if token.value_type == TokenType::RParen {
                break;
            }
            if token.value_type != TokenType::Identifier {
                panic!("Expected identifier after `(`, got `{}`", token.value);
            }
            let param_name = token.value;
            match self.tokens.next() {
                Some(token) if token.value_type == TokenType::Colon => {} // continue
                Some(token) => panic!("Expected `:` after parameter name, got `{}`", token.value),
                None => panic!("Unexpected end of input after parameter name"),
            }
            let param_type = parse_type(self.tokens.next().unwrap());
            params.push(Param {
                name: param_name,
                type_annotation: Some(param_type)
            });
            if let Some(token) = self.tokens.peek() {
                if token.value_type == TokenType::Comma {
                    self.tokens.next(); // consume comma
                } else if token.value_type != TokenType::RParen {
                    panic!("Expected `,` or `)` after parameter, got `{}`", token.value);
                }
            }
        }

        // Step 4: Return type (OPTIONAL)
        let return_type = if self.check_next_token(TokenType::Colon) {
            self.expect_token(TokenType::Colon, "Expected `:` before return type");

            let return_type = match self.tokens.next() {
                Some(token) if token.value_type == TokenType::Identifier => token,
                Some(token) => panic!("Expected identifier after `:` in return type, got `{}`", token.value),
                None => panic!("Unexpected end of input after `:` in return type"),
            };
            Some(parse_type(return_type))
        } else {
            None // Return type is optional
        };

        // Step 6: Body
        let body = self.parse_block();

        Stmt::FuncDecl(FuncDecl { visibility, name, params, return_type, effects: None, body })
    }

    pub(crate) fn parse_let_stmt(&mut self) -> Stmt
    where
        I: Iterator<Item=Token>,
    {
        self.expect_token(TokenType::Keyword, "Expected `let` in `let` statement"); // consume `let`

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

        // Step 3: Optional `: type`
        let mut type_annotation: Option<Type> = None;
        if self.check_next_token(TokenType::Colon) {
            self.tokens.next(); // consume `:`
            let type_token = self.tokens.next().expect("Expected type after `:`");
            type_annotation = Some(parse_type(type_token));
        }

        // Step 4: Expect `=`
        match self.tokens.next() {
            Some(token) if token.value_type == TokenType::Equal => {} // continue
            Some(token) => panic!("Expected `=` after identifier, got {:?}", token),
            None => panic!("Unexpected end of input after identifier"),
        }

        let expr = self.parse_expr().expect("Expected expression after `=`");

        Stmt::Let(LetStmt {
            mutability,
            ident,
            type_annotation,
            value: Box::new(expr),
        })
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
        let mut statements = Vec::new();

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

            Some(token) if token.value_type.is_number() && self.tokens.peek().map_or(false, |t| t.value_type == TokenType::Plus) => {
                let lhs = Expr::Number(token.value.parse().unwrap());
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