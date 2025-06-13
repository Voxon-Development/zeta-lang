use crate::ast;
use crate::ast::{InternalExprStmt, Stmt};
use crate::frontend::stmt_parser::StmtParser;
use crate::frontend::tokens::{Token, TokenType};

pub struct Parser {
    stmt_parser: StmtParser<<Vec<Token> as IntoIterator>::IntoIter>,
    current: usize
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Parser {
        Parser {
            stmt_parser: StmtParser::new(tokens.into_iter()),
            current: 0
        }
    }

    pub fn parse(mut self) -> Vec<Stmt> {
        let mut stmts = Vec::new();

        while !self.stmt_parser.is_at_end() {
            let mut visibility: Option<ast::Visibility> = None;
            
            let token = self.stmt_parser.peek().unwrap();

            match token.value.as_str() {
                "public" => {
                    self.stmt_parser.next();
                    visibility = Some(ast::Visibility::Public);
                }
                "private" => {
                    self.stmt_parser.next();
                    visibility = Some(ast::Visibility::Private);
                }
                "protected" => {
                    self.stmt_parser.next();
                    visibility = Some(ast::Visibility::Protected);
                }
                _ => {
                    self.stmt_parser.parse_stmt(&mut stmts, &token, visibility);
                }
            }
        }
        
        self.stmt_parser.next();

        stmts
    }
}

pub fn parse_program(tokens: Vec<Token>) -> Result<Vec<ast::Stmt>, String> {
    let parser = Parser::new(tokens);
    Ok(parser.parse())
}