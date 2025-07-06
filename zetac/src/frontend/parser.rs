use ir::bump::AtomicBump;
use crate::ast;
use crate::ast::Stmt;
use crate::frontend::stmt_parser::StmtParser;
use crate::frontend::tokens::Token;

pub struct Parser {
    stmt_parser: StmtParser,
    current: usize
}

impl Parser {
    pub fn new(tokens: Vec<Token, AtomicBump>) -> Parser {
        Parser {
            stmt_parser: StmtParser::new(tokens),
            current: 0
        }
    }

    pub fn parse(mut self) -> Vec<Stmt, AtomicBump> {
        let mut stmts: Vec<Stmt, AtomicBump> = Vec::with_capacity_in(16, AtomicBump::new());

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

pub fn parse_program(tokens: Vec<Token, AtomicBump>) -> Result<Vec<Stmt, AtomicBump>, String> {
    let parser = Parser::new(tokens);
    Ok(parser.parse())
}