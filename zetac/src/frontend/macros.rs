use std::collections::HashMap;
use crate::ast::{Expr, Stmt};
use crate::frontend::tokens::Token;

#[derive(Debug, Clone)]
pub enum MacroPatternNode {
    Ident(String),
    Expr(String),
    Optional(Box<MacroPatternNode>),
    Sequence(Vec<MacroPatternNode>),
    Token(String),
}

#[derive(Debug, Clone)]
pub struct MacroDef {
    pub name: String,
    pub pattern: MacroPatternNode,
    pub expansion_ast: fn(HashMap<String, Expr>) -> Stmt, // AST builder function
}

// Global macro registry
thread_local! {
    static MACRO_REGISTRY: std::cell::RefCell<Vec<MacroDef>> = std::cell::RefCell::new(vec![]);
}

pub fn register_macro(m: MacroDef) {
    MACRO_REGISTRY.with(|reg| {
        reg.borrow_mut().push(m);
    });
}

pub fn all_macros() -> Vec<MacroDef> {
    MACRO_REGISTRY.with(|reg| reg.borrow().clone())
}

pub fn parse_macro(tokens: &[Token]) -> Option<MacroDef> {
    let mut index = 0;

    fn match_token(tokens: &[Token], index: &mut usize, expected: &str) -> bool {
        if *index >= tokens.len() { return false; }
        if tokens[*index].value == expected {
            *index += 1;
            true
        } else {
            false
        }
    }

    if !match_token(tokens, &mut index, "macro") { return None; }

    let name_token = tokens.get(index)?;
    let name = name_token.value.clone();
    *&mut index += 1;

    if !match_token(tokens, &mut index, "{") { return None; }

    // Look for a pattern block ($var:expr, ...) then `expand` block
    // We'll mock this part for now as hardcoded pattern + expansion

    if !tokens.iter().any(|t| t.value == "expand") {
        return None;
    }

    Some(MacroDef {
        name,
        pattern: MacroPatternNode::Sequence(vec![
            MacroPatternNode::Ident("var".into()),
            MacroPatternNode::Token(":".into()),
            MacroPatternNode::Expr("type".into()),
            MacroPatternNode::Token("in".into()),
            MacroPatternNode::Ident("region".into()),
            MacroPatternNode::Token("=".into()),
            MacroPatternNode::Expr("expr".into()),
        ]),
        expansion_ast: |bindings| {
            let var = bindings.get("var").unwrap().clone();
            let ty = bindings.get("type").unwrap().clone();
            let region = bindings.get("region").unwrap().clone();
            let expr = bindings.get("expr").unwrap().clone();

            Stmt::VarDecl {
                mutable: true,
                name: match var {
                    Expr::Ident(s) => s,
                    _ => panic!("Expected identifier for variable name"),
                },
                ty: Some(ty),
                value: Some(Expr::Call {
                    callee: Box::new(Expr::Get {
                        object: Box::new(region),
                        field: "allocate".to_string(),
                    }),
                    args: vec![expr],
                })
            }
        }
    })
}