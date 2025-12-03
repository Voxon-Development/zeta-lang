use std::sync::Arc;
use ir::ast::*;
use zetaruntime::bump::GrowableBump;
use zetaruntime::string_pool::StringPool;
use crate::tokenizer::cursor::TokenCursor;
use crate::tokenizer::tokens::TokenKind;
use crate::parser::parser_types::parse_to_type;
use crate::parser::pratt_expr_parser::PrattExprParser;

pub struct StatementParser<'a, 'bump>
where
    'bump: 'a,
{
    context: Arc<StringPool>,
    bump: &'bump GrowableBump<'bump>,
    expr_parser: PrattExprParser<'a, 'bump>,
    phantom: std::marker::PhantomData<&'a ()>,
}

impl<'a, 'bump> StatementParser<'a, 'bump>
where
    'bump: 'a,
{
    pub fn new(context: Arc<StringPool>, bump: &'bump GrowableBump<'bump>) -> Self {
        Self {
            context: context.clone(),
            bump,
            expr_parser: PrattExprParser::new(context.clone(), bump),
            phantom: std::marker::PhantomData,
        }
    }
    
    pub fn bump(&self) -> &'bump GrowableBump<'bump> {
        self.bump
    }
    
    pub fn parse_stmt(&self, cursor: &mut TokenCursor<'a>) -> Stmt<'a, 'bump> {
        if let Some(value) = self.check_for_type_inference(cursor) {
            return value;
        }
        
        match cursor.peek_kind() {
            Some(TokenKind::Let) => self.parse_let_or_const(cursor),
            Some(TokenKind::Const) => {
                // Check if this is actually a const statement or something else
                if cursor.peek_kind_n(1) == Some(TokenKind::Ident) {
                    self.parse_let_or_const(cursor)
                } else {
                    // Not a const statement, skip it
                    cursor.advance_kind();
                    self.parse_expr_stmt(cursor)
                }
            }

            Some(TokenKind::Return) => self.parse_return(cursor),

            Some(TokenKind::If) => self.parse_if(cursor),

            Some(TokenKind::While) => self.parse_while(cursor),

            Some(TokenKind::For) => self.parse_for(cursor),

            Some(TokenKind::Match) => self.parse_match(cursor),

            Some(TokenKind::Break) => {
                cursor.advance_kind();
                cursor.expect_kind(TokenKind::Semicolon);
                Stmt::Break
            }

            Some(TokenKind::Continue) => {
                cursor.advance_kind();
                cursor.expect_kind(TokenKind::Semicolon);
                Stmt::Continue
            }

            Some(TokenKind::LBrace) => self.parse_block_stmt(cursor),

            Some(TokenKind::Unsafe) => self.parse_unsafe_block(cursor),

            Some(TokenKind::Defer) => self.parse_defer(cursor),
            _ => self.parse_expr_stmt(cursor),
        }
    }

    fn check_for_type_inference(&self, cursor: &mut TokenCursor<'a>) -> Option<Stmt<'a, 'bump>> {
        // Check for type inference: `x := value`
        if cursor.peek_kind() == Some(TokenKind::Ident) {
            let next = cursor.peek_kind_n(1);
            if next == Some(TokenKind::ColonAssign) {
                return Some(self.parse_type_inference(cursor));
            } else if next == Some(TokenKind::Colon) {
                // Check if it's inside a block like `{ x: 1 }` - if so, parse as expression
                // This happens when we have something like `Vec3f { x: 1 }` where the inner `x: 1` is being parsed
                // In this case, just continue to parse as expression statement
                // Only panic if this is truly a type annotation at statement level
                // For now, we'll let the expression parser handle it
            } else if next == Some(TokenKind::LBrace) {
                // This could be an expression like `Vec3f { ... }` - parse as expression statement
                // Don't panic here
            }
        }
        None
    }

    fn parse_type_inference(&self, cursor: &mut TokenCursor<'a>) -> Stmt<'a, 'bump> {

        // Type inference: `x := value`
        let ident = cursor.consume_ident()
            .expect("Expected variable name");
        cursor.expect_kind(TokenKind::ColonAssign);
        let value = self.parse_expr_placeholder(cursor);
        cursor.expect_kind(TokenKind::Semicolon);
        
        let let_stmt = self.bump.alloc_value(LetStmt {
            ident,
            mutable: false,
            type_annotation: Type::infer(),
            value,
        });
        Stmt::Let(let_stmt)
    }
    
    pub(crate) fn parse_let_or_const(&self, cursor: &mut TokenCursor<'a>) -> Stmt<'a, 'bump> {
        let is_const = cursor.peek_kind() == Some(TokenKind::Const);
        cursor.advance_kind(); // consume 'let' or 'const'

        let mutable = if cursor.peek_kind() == Some(TokenKind::Mut) {
            if is_const {
                eprintln!("Warning: Unexpected 'mut' in const declaration");
                false
            } else {
                cursor.advance_kind();
                true
            }
        } else {
            false
        };
        
        let ident = cursor.consume_ident()
            .expect("Expected identifier in let/const statement");
        
        cursor.expect_kind(TokenKind::Colon);
        
        let type_annotation = match cursor.peek_kind() {
            Some(TokenKind::I8) => { cursor.advance_kind(); Type::i8() }
            Some(TokenKind::I16) => { cursor.advance_kind(); Type::i16() }
            Some(TokenKind::I32) => { cursor.advance_kind(); Type::i32() }
            Some(TokenKind::I64) => { cursor.advance_kind(); Type::i64() }
            Some(TokenKind::I128) => { cursor.advance_kind(); Type::i128() }
            Some(TokenKind::U8) => { cursor.advance_kind(); Type::u8() }
            Some(TokenKind::U16) => { cursor.advance_kind(); Type::u16() }
            Some(TokenKind::U32) => { cursor.advance_kind(); Type::u32() }
            Some(TokenKind::U64) => { cursor.advance_kind(); Type::u64() }
            Some(TokenKind::U128) => { cursor.advance_kind(); Type::u128() }
            Some(TokenKind::F32) => { cursor.advance_kind(); Type::f32() }
            Some(TokenKind::F64) => { cursor.advance_kind(); Type::f64() }
            Some(TokenKind::Boolean) => { cursor.advance_kind(); Type::boolean() }
            Some(TokenKind::String) => { cursor.advance_kind(); Type::string() }
            Some(TokenKind::Void) => { cursor.advance_kind(); Type::void() }
            Some(TokenKind::Ident) => {
                let type_name = cursor.consume_ident().expect("Expected type after colon");
                let type_str = self.context.resolve_string(&type_name);
                parse_to_type(type_str, cursor.peek_kind().unwrap_or(TokenKind::EOF), self.context.clone(), self.bump)
            }
            _ => panic!("Expected type after colon, found: {:?}", cursor.peek_kind()),
        };
        
        // Require '=' for let/const (no ':=' allowed here)
        cursor.expect_kind(TokenKind::Assign);
        
        // Parse value expression (placeholder for now)
        let value = self.parse_expr_placeholder(cursor);
        
        cursor.expect_kind(TokenKind::Semicolon);
        
        if is_const {
            let const_stmt = self.bump.alloc_value(ConstStmt {
                ident,
                type_annotation,
                value,
            });
            Stmt::Const(const_stmt)
        } else {
            let let_stmt = self.bump.alloc_value(LetStmt {
                ident,
                type_annotation,
                value,
                mutable,
            });
            Stmt::Let(let_stmt)
        }
    }
    
    fn parse_return(&self, cursor: &mut TokenCursor<'a>) -> Stmt<'a, 'bump> {
        cursor.expect_kind(TokenKind::Return);
        
        let value = if cursor.peek_kind() != Some(TokenKind::Semicolon) {
            Some(self.parse_expr_placeholder(cursor))
        } else {
            None
        };
        
        cursor.expect_kind(TokenKind::Semicolon);
        
        let return_stmt = self.bump.alloc_value(ReturnStmt { value });
        Stmt::Return(return_stmt)
    }
    
    fn parse_if(&self, cursor: &mut TokenCursor<'a>) -> Stmt<'a, 'bump> {
        cursor.expect_kind(TokenKind::If);
        cursor.expect_kind(TokenKind::LParen);
        
        let condition = self.parse_expr_placeholder(cursor);
        
        cursor.expect_kind(TokenKind::RParen);
        
        let then_branch = match self.parse_block(cursor) {
            Some(block) => block,
            None => {
                eprintln!("Error: Expected block after if condition");
                // Create empty block as fallback
                let empty_stmts = self.bump.alloc_slice(&[]);
                self.bump.alloc_value(Block { block: empty_stmts })
            }
        };
        
        // Handle else/else-if chains iteratively
        let else_branch: Option<&ElseBranch> = if cursor.peek_kind() == Some(TokenKind::Else) {
            cursor.advance_kind();
            
            if cursor.peek_kind() == Some(TokenKind::If) {
                let mut if_chain: Vec<(&Expr, &Block)> = Vec::new();
                
                cursor.expect_kind(TokenKind::If);
                cursor.expect_kind(TokenKind::LParen);
                let elif_condition = self.parse_expr_placeholder(cursor);
                cursor.expect_kind(TokenKind::RParen);
                let elif_then = self.parse_block(cursor).expect("Expected then branch");
                
                if_chain.push((elif_condition, elif_then));
                
                while cursor.peek_kind() == Some(TokenKind::Else) {
                    cursor.advance_kind();
                    
                    if cursor.peek_kind() == Some(TokenKind::If) {
                        cursor.expect_kind(TokenKind::If);
                        cursor.expect_kind(TokenKind::LParen);
                        let elif_condition = self.parse_expr_placeholder(cursor);
                        cursor.expect_kind(TokenKind::RParen);
                        let elif_then = self.parse_block(cursor).expect("Expected then branch");
                        if_chain.push((elif_condition, elif_then));
                    } else {
                        let else_block = self.parse_block(cursor);
                        
                        let mut current_else: Option<&ElseBranch> = else_block.map(|b| self.bump.alloc_value_immutable(ElseBranch::Else(b)));
                        
                        for (elif_condition, elif_then) in if_chain.into_iter().rev() {
                            let elif_stmt = self.bump.alloc_value(IfStmt {
                                condition: elif_condition,
                                then_branch: elif_then,
                                else_branch: current_else,
                            });
                            current_else = Some(self.bump.alloc_value(ElseBranch::If(elif_stmt)));
                        }
                        
                        return Stmt::If(self.bump.alloc_value(IfStmt {
                            condition,
                            then_branch,
                            else_branch: current_else,
                        }));
                    }
                }
                
                // No final else, build chain from right to left
                let mut current_else: Option<&ElseBranch> = None;
                
                for (elif_condition, elif_then) in if_chain.into_iter().rev() {
                    let elif_stmt = self.bump.alloc_value(IfStmt {
                        condition: elif_condition,
                        then_branch: elif_then,
                        else_branch: current_else,
                    });
                    current_else = Some(self.bump.alloc_value(ElseBranch::If(elif_stmt)));
                }
                
                current_else
            } else {
                let else_block = self.parse_block(cursor);
                else_block.map(|b| self.bump.alloc_value_immutable(ElseBranch::Else(b)))
            }
        } else {
            None
        };
        
        let if_stmt = self.bump.alloc_value(IfStmt {
            condition,
            then_branch,
            else_branch,
        });
        
        Stmt::If(if_stmt)
    }
    
    fn parse_while(&self, cursor: &mut TokenCursor<'a>) -> Stmt<'a, 'bump> {
        cursor.expect_kind(TokenKind::While);
        cursor.expect_kind(TokenKind::LParen);
        
        let condition = self.parse_expr_placeholder(cursor);
        
        cursor.expect_kind(TokenKind::RParen);
        
        let block = self.parse_block(cursor)
            .expect("Expected block after while condition");
        
        let while_stmt = self.bump.alloc_value(WhileStmt { condition, block });
        Stmt::While(while_stmt)
    }
    
    fn parse_for(&self, cursor: &mut TokenCursor<'a>) -> Stmt<'a, 'bump> {
        cursor.expect_kind(TokenKind::For);
        
        // Check if it's range-based (for i in ...) or C-style (for (...))
        if cursor.peek_kind() == Some(TokenKind::Ident) {
            // Look ahead to check for 'in' keyword
            let checkpoint = cursor.checkpoint();
            let var_name = cursor.consume_ident();
            let is_range_based = cursor.peek_kind() == Some(TokenKind::In);
            cursor.restore(checkpoint);
            
            if is_range_based {
                return self.parse_range_based_for(cursor);
            }
        }
        
        // C-style for loop
        cursor.expect_kind(TokenKind::LParen);
        
        let let_stmt: Option<&LetStmt> = if cursor.peek_kind() == Some(TokenKind::Let) {
            if let Stmt::Let(ls) = self.parse_let_or_const(cursor) {
                Some(ls)
            } else {
                None
            }
        } else {
            cursor.expect_kind(TokenKind::Semicolon);
            None
        };
        
        let condition = if cursor.peek_kind() != Some(TokenKind::Semicolon) {
            Some(self.parse_expr_placeholder(cursor))
        } else {
            None
        };
        cursor.expect_kind(TokenKind::Semicolon);
        
        let increment = if cursor.peek_kind() != Some(TokenKind::RParen) {
            Some(self.parse_expr_placeholder(cursor))
        } else {
            None
        };
        
        cursor.expect_kind(TokenKind::RParen);
        
        let block = match self.parse_block(cursor) {
            Some(block) => block,
            None => {
                eprintln!("Error: Expected block after for header");
                let empty_stmts = self.bump.alloc_slice(&[]);
                self.bump.alloc_value(Block { block: empty_stmts })
            }
        };
        
        let for_stmt = self.bump.alloc_value(ForStmt {
            kind: ForKind::CStyle {
                let_stmt,
                condition,
                increment,
            },
            block,
        });
        
        Stmt::For(for_stmt)
    }
    
    fn parse_range_based_for(&self, cursor: &mut TokenCursor<'a>) -> Stmt<'a, 'bump> {
        cursor.expect_kind(TokenKind::For);
        
        let variable = cursor.consume_ident().expect("Expected variable name in for loop");
        cursor.expect_kind(TokenKind::In);
        
        let iterable = self.parse_expr_placeholder(cursor);
        
        let block = match self.parse_block(cursor) {
            Some(block) => block,
            None => {
                eprintln!("Error: Expected block after for...in header");
                let empty_stmts = self.bump.alloc_slice(&[]);
                self.bump.alloc_value(Block { block: empty_stmts })
            }
        };
        
        let for_stmt = self.bump.alloc_value(ForStmt {
            kind: ForKind::RangeBased {
                variable,
                iterable,
            },
            block,
        });
        
        Stmt::For(for_stmt)
    }
    
    fn parse_match(&self, cursor: &mut TokenCursor<'a>) -> Stmt<'a, 'bump> {
        cursor.expect_kind(TokenKind::Match);
        cursor.expect_kind(TokenKind::LParen);
        
        let expr: &Expr<'a, 'bump> = self.parse_expr_placeholder(cursor);
        
        cursor.expect_kind(TokenKind::RParen);
        cursor.expect_kind(TokenKind::LBrace);
        
        let mut arms_vec: Vec<MatchArm<'a, 'bump>> = Vec::new();
        
        while cursor.peek_kind() != Some(TokenKind::RBrace) && !cursor.at_end() {
            let pattern: Pattern<'bump> = self.parse_pattern(cursor);
            
            let guard = if cursor.peek_kind() == Some(TokenKind::If) {
                cursor.advance_kind();
                Some(self.parse_expr_placeholder(cursor))
                
            } else {
                None
            };
            
            cursor.expect_kind(TokenKind::FatArrow);
            
            let block = match self.parse_block(cursor) {
                Some(block) => block,
                None => {
                    eprintln!("Error: Expected block in match arm");
                    let empty_stmts = self.bump.alloc_slice(&[]);
                    self.bump.alloc_value(Block { block: empty_stmts })
                }
            };
            
            arms_vec.push(MatchArm {
                pattern,
                guard,
                block,
            });
            
            if cursor.peek_kind() == Some(TokenKind::Comma) {
                cursor.advance_kind();
            }
        }
        
        cursor.expect_kind(TokenKind::RBrace);
        
        let arms = self.bump.alloc_slice(arms_vec.as_slice());
        let stmt = MatchStmt { expr, arms };

        Stmt::Match(self.bump.alloc_value(stmt))
    }
    
    fn parse_block_stmt(&self, cursor: &mut TokenCursor<'a>) -> Stmt<'a, 'bump> {
        let block = self.parse_block(cursor)
            .expect("Expected block");
        Stmt::Block(block)
    }
    
    fn parse_unsafe_block(&self, cursor: &mut TokenCursor<'a>) -> Stmt<'a, 'bump> {
        cursor.expect_kind(TokenKind::Unsafe);
        let block = self.parse_block(cursor)
            .expect("Expected block after unsafe");
        
        let unsafe_block = self.bump.alloc_value(UnsafeBlock { block });
        Stmt::UnsafeBlock(unsafe_block)
    }
    
    pub fn parse_expr_stmt(&self, cursor: &mut TokenCursor<'a>) -> Stmt<'a, 'bump> {
        let expr = self.parse_expr_placeholder(cursor);
        cursor.expect_kind(TokenKind::Semicolon);
        
        let expr_stmt = self.bump.alloc_value(InternalExprStmt { expr });
        Stmt::ExprStmt(expr_stmt)
    }
    
    fn parse_block(&self, cursor: &mut TokenCursor<'a>) -> Option<&'bump Block<'a, 'bump>> {
        if !cursor.expect_kind(TokenKind::LBrace) {
            return None;
        }
        
        let mut stmts: Vec<Stmt> = Vec::new();
        
        while cursor.peek_kind() != Some(TokenKind::RBrace) && !cursor.at_end() {
            stmts.push(self.parse_stmt(cursor));
        }
        
        cursor.expect_kind(TokenKind::RBrace);
        
        let stmts_slice = if stmts.is_empty() {
            &[]
        } else {
            self.bump.alloc_slice_copy(stmts.as_slice())
        };
        
        Some(self.bump.alloc_value(Block { block: stmts_slice }))
    }
    
    /// Parse expression using the expression parser
    pub fn parse_expr_placeholder(&self, cursor: &mut TokenCursor<'a>) -> &'bump Expr<'a, 'bump> {
        self.expr_parser.parse(cursor)
    }
    
    /// Parse patterns with support for:
    /// - Identifiers: `x`
    /// - Literals: `42`, `"hello"`, `true`
    /// - Tuples: `(x, y, z)`
    /// - Structs: `Point { x, y }` or `Point { x: px, y: py }`
    /// - Arrays: `[x, y, z]`
    /// - Or patterns: `1 | 2 | 3`
    /// - Wildcard: `_`
    fn parse_pattern(&self, cursor: &mut TokenCursor<'a>) -> Pattern<'bump> {
        self.parse_or_pattern(cursor)
    }
    
    fn parse_or_pattern(&self, cursor: &mut TokenCursor<'a>) -> Pattern<'bump> {
        let mut patterns: Vec<Pattern<'bump>> = vec![self.parse_primary_pattern(cursor)];
        
        // Check for or patterns: `1 | 2 | 3`
        while cursor.peek_kind() == Some(TokenKind::BitOr) {
            cursor.advance_kind();
            patterns.push(self.parse_primary_pattern(cursor));
        }
        
        if patterns.len() == 1 {
            patterns.into_iter().next().unwrap()
        } else {
            Pattern::Or(self.bump.alloc_slice_copy(patterns.as_slice()))
        }
    }
    
    fn parse_primary_pattern(&self, cursor: &mut TokenCursor<'a>) -> Pattern<'bump> {
        match cursor.peek_kind() {
            Some(TokenKind::Underscore) => {
                cursor.advance_kind();
                Pattern::Wildcard
            }
            
            Some(TokenKind::Number) => {
                let text_id = cursor.consume_number().unwrap();
                let text = self.context.resolve_string(&text_id);
                let value = text.parse::<i64>().unwrap_or(0);
                Pattern::Number(value)
            }
            
            Some(TokenKind::String) => {
                let s = cursor.consume_string().unwrap();
                Pattern::String(s)
            }
            
            Some(TokenKind::BooleanTrue) => {
                cursor.advance_kind();
                Pattern::Boolean(true)
            }
            
            Some(TokenKind::BooleanFalse) => {
                cursor.advance_kind();
                Pattern::Boolean(false)
            }
            
            // Tuple pattern: `(x, y, z)`
            Some(TokenKind::LParen) => {
                cursor.advance_kind();
                let mut patterns: Vec<Pattern> = Vec::new();

                while cursor.peek_kind() != Some(TokenKind::RParen) && !cursor.at_end() {
                    patterns.push(self.parse_or_pattern(cursor));

                    if cursor.peek_kind() == Some(TokenKind::Comma) {
                        cursor.advance_kind();
                    } else {
                        break;
                    }
                }

                cursor.expect_kind(TokenKind::RParen);
                Pattern::Tuple(self.bump.alloc_slice_copy(patterns.as_slice()))
            }
            
            // Array pattern: `[x, y, z]`
            Some(TokenKind::LBracket) => {
                cursor.advance_kind();
                let mut patterns: Vec<Pattern> = Vec::new();
                
                while cursor.peek_kind() != Some(TokenKind::RBracket) && !cursor.at_end() {
                    patterns.push(self.parse_or_pattern(cursor));
                    
                    if cursor.peek_kind() == Some(TokenKind::Comma) {
                        cursor.advance_kind();
                    } else {
                        break;
                    }
                }
                
                cursor.expect_kind(TokenKind::RBracket);
                Pattern::Array(self.bump.alloc_slice_copy(patterns.as_slice()))
            }
            
            // Identifier or Struct pattern
            Some(TokenKind::Ident) => {
                let name = cursor.consume_ident().unwrap();
                
                // Check for struct pattern: `Point { x, y }`
                if cursor.peek_kind() == Some(TokenKind::LBrace) {
                    cursor.advance_kind();
                    let mut fields = Vec::new();
                    
                    while cursor.peek_kind() != Some(TokenKind::RBrace) && !cursor.at_end() {
                        let field_name = cursor.consume_ident().unwrap();
                        
                        // Check for field binding: `x: px` vs shorthand `x`
                        let pattern = if cursor.peek_kind() == Some(TokenKind::Colon) {
                            cursor.advance_kind();
                            self.parse_pattern(cursor)
                        } else {
                            // Shorthand: `x` is same as `x: x`
                            Pattern::Ident(field_name)
                        };
                        
                        fields.push((field_name, pattern));
                        
                        if cursor.peek_kind() == Some(TokenKind::Comma) {
                            cursor.advance_kind();
                        } else {
                            break;
                        }
                    }
                    
                    cursor.expect_kind(TokenKind::RBrace);
                    Pattern::Struct { name, fields: self.bump.alloc_slice_copy(&fields) }
                } else {
                    // Just an identifier
                    Pattern::Ident(name)
                }
            }
            
            _ => {
                // Unknown pattern, consume and return wildcard
                cursor.advance_kind();
                Pattern::Wildcard
            }
        }
    }
    
    fn parse_defer(&self, cursor: &mut TokenCursor<'a>) -> Stmt<'a, 'bump> {
        cursor.expect_kind(TokenKind::Defer);
        
        let action = if cursor.peek_kind() == Some(TokenKind::LBrace) {
            // defer { ... }
            let block = self.parse_block(cursor)
                .expect("Expected block after defer");
            DeferAction::Block(block)
        } else {
            // defer stmt;
            let stmt = self.bump.alloc_value(self.parse_stmt(cursor));
            DeferAction::Stmt(stmt)
        };
        
        let defer_stmt = self.bump.alloc_value(DeferStmt { action });
        Stmt::Defer(defer_stmt)
    }
}