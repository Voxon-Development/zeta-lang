use pest_derive::Parser;

use pest::iterators::{Pair, Pairs};
use pest::Parser;

use ir::ast::ElseBranch::If;
use ir::ast::Op;
use ir::ast::Stmt::ExprStmt;
use ir::ast::Type::Void;
use ir::ast::*;

#[derive(Parser)]
#[grammar = "grammar.pest"]
pub struct ZetaParser;


pub fn parse_program(input: &str) -> Result<Vec<Stmt>, pest::error::Error<Rule>> {
    let mut pairs = ZetaParser::parse(Rule::program, input)?;
    /*for pair in pairs.clone() {
        println!("{:?}\n", pair);
    }*/

    let program_pair = pairs.clone().into_iter().next().expect("Expected program rule");
    assert_eq!(program_pair.as_rule(), Rule::program);

    let program_pair = pairs.next().unwrap(); // Rule::program

    let mut stmts = Vec::new();
    for pair in program_pair.into_inner() {
        if parse_stmt(&mut stmts, pair) { continue; }
    }

    Ok(stmts)
}

fn parse_stmt(stmts: &mut Vec<Stmt>, pair: Pair<Rule>) -> bool {
    match pair.as_rule() {
        Rule::impl_decl => stmts.push(Stmt::ImplDecl(parse_impl_decl(&pair))),
        Rule::import_stmt => stmts.push(parse_import_stmt(&pair)),
        Rule::let_stmt => stmts.push(parse_let_stmt(&pair)),
        Rule::return_stmt => stmts.push(parse_return_stmt(&pair)),
        Rule::if_stmt => stmts.push(parse_if_stmt(&pair)),
        Rule::while_stmt => stmts.push(parse_while_stmt(&pair)),
        Rule::for_stmt => stmts.push(parse_for_stmt(&pair)),
        Rule::match_stmt => stmts.push(parse_match_stmt(&pair)),
        Rule::unsafe_block => stmts.push(parse_unsafe_block(&pair)),
        Rule::fun_decl => stmts.push(Stmt::FuncDecl(parse_fun_decl(&pair))),
        Rule::struct_decl => stmts.push(parse_class_decl(&pair)),
        Rule::expr_stmt => stmts.push(parse_expr_stmt(&pair)),

        Rule::EOI => return true,
        other => panic!("Unexpected rule in program: {:?}", other),
    }
    false
}

fn parse_impl_decl(pair: &Pair<Rule>) -> ImplDecl {
    let mut inner = pair.clone().into_inner();

    // Parse generics if present
    let (generics, _interface_pair) = if inner.peek().unwrap().as_rule() == Rule::generic_params {
        (parse_generic_params(&mut inner), inner.next().unwrap())
    } else {
        (None, inner.next().unwrap())
    };

    let interface = inner.next().unwrap().as_str().to_string();
    let target = inner.next().unwrap().as_str().to_string();

    let next = inner.next();
    let methods = if let Some(body) = next {
        if body.as_rule() == Rule::fun_decl {
            let mut funcs = vec![parse_fun_decl(&body)];
            for sub in inner {
                funcs.push(parse_fun_decl(&sub));
            }
            Some(funcs)
        } else {
            None
        }
    } else {
        None
    };

    ImplDecl {
        generics,
        interface,
        target,
        methods,
    }
}

fn parse_while_stmt(pair: &Pair<Rule>) -> Stmt {
    let mut inner = pair.clone().into_inner();
    let value: Option<Box<Expr>> = inner.next().map(|p| Box::new(parse_expr(p)));

    match value {
        Some(expr) => Stmt::While(WhileStmt {
            condition: expr,
            block: parse_block(inner.next().unwrap())
        }),
        None => panic!("Expected expression in while loop")
    }
}

fn parse_for_stmt(pair: &Pair<Rule>) -> Stmt {
    let mut inner = pair.clone().into_inner();

    // initializer: optional
    let initializer_pair = inner.next().unwrap();
    let initializer = if initializer_pair.as_rule() == Rule::let_stmt {
        match parse_let_stmt(&initializer_pair) {
            Stmt::Let(let_stmt) => Some(let_stmt),
            _ => panic!("Expected let_stmt in for loop initializer"),
        }
    } else {
        None // e.g., empty initializer (just a semicolon)
    };

    // condition: optional
    let condition_pair = inner.next().unwrap();
    println!("{:?}", condition_pair.as_rule());
    println!("{:?}", condition_pair.as_str());

    let condition = if condition_pair.as_rule() == Rule::expr {
        Some(Box::new(parse_expr(condition_pair)))
    } else {
        None
    };

    // increment: optional
    let increment_pair = inner.next().unwrap();
    let increment = if increment_pair.as_rule() == Rule::expr {
        Some(Box::new(parse_expr(increment_pair)))
    } else {
        None
    };

    let body = parse_block(inner.next().unwrap());

    Stmt::For(ForStmt {
        let_stmt: initializer,
        condition,
        increment,
        block: body,
    })
}

fn parse_unsafe_block(pair: &Pair<Rule>) -> Stmt {
    let mut inner = pair.clone().into_inner();
    let block = parse_block(inner.next().unwrap());  // Parse the matched expression
    Stmt::UnsafeBlock(UnsafeBlock { block })
}

fn parse_expr_stmt(pair: &Pair<Rule>) -> Stmt {
    let expr = parse_expr(pair.clone().into_inner().next().unwrap());
    ExprStmt(InternalExprStmt { expr: Box::new(expr) })
}

fn parse_class_decl(pair: &Pair<Rule>) -> Stmt {
    let mut inner = pair.clone().into_inner();

    // Visibility (public/private/internal)
    let visibility = get_visibility(&mut inner);

    // Optional generic parameters: <T: Constraint, U: …>
    let generics = parse_generic_params(&mut inner);

    // Optional region parameters: [R1 R2 …]
    let regions = parse_region_params(&mut inner);

    // Class keyword already consumed; now the identifier
    let name = inner.next().unwrap().as_str().to_string();

    // Optional constructor parameters (x: T, …)
    let params = if inner.peek().map(|p| p.as_rule()) == Some(Rule::param_list) {
        let param_list = inner.next().unwrap();
        Some(param_list.into_inner().map(parse_param).collect())
    } else {
        None
    };

    // The class body { … }
    let body = Some(parse_block(inner.next().unwrap()));

    Stmt::ClassDecl(ClassDecl {
        visibility,
        name,
        generics,
        regions,
        params,
        body,
    })
}

fn parse_region_params(inner: &mut Pairs<Rule>) -> Option<Vec<RegionParam>> {
    if inner.peek().map(|p| p.as_rule()) == Some(Rule::region_params) {
        let reg_pair = inner.next().unwrap();
        let regs = reg_pair.into_inner()
            .map(|p| RegionParam { name: p.as_str().to_string() })
            .collect();
        Some(regs)
    } else {
        None
    }
}

fn parse_generic_params(inner: &mut Pairs<Rule>) -> Option<Vec<Generic>> {
    if inner.peek().map(|p| p.as_rule()) != Some(Rule::generic_params) {
        return None;
    }

    let gen_pair = inner.next().unwrap();
    let params = gen_pair.into_inner().map(map_pair_to_generic).collect();

    Some(params)
}

fn map_pair_to_generic(p: Pair<Rule>) -> Generic {
    let mut parts = p.into_inner();
    let name = parts.next().unwrap().as_str().to_string();

    // Collect all constraints after ":"
    let mut constraints = Vec::new();
    for part in parts {
        constraints.push(part.as_str().to_string());
    }

    Generic { const_generic: false, type_name: name, constraints }
}

fn get_visibility(inner: &mut Pairs<Rule>) -> Visibility {
    match inner.peek().map(|p| p.as_rule()) {
        Some(Rule::public_keyword | Rule::private_keyword | Rule::protected_keyword) => {
            assert_visibility_modifier(inner)
        }
        _ => Visibility::Public,
    }
}

fn assert_visibility_modifier(inner: &mut Pairs<Rule>) -> Visibility {
    match inner.next().unwrap().as_str() {
        "public" => Visibility::Public,
        "private" => Visibility::Private,
        "internal" => Visibility::Internal,
        fetched => panic!("Expected visibility modifier, got {}", fetched)
    }
}

fn parse_fun_decl(pair: &Pair<Rule>) -> FuncDecl {
    let mut inner = pair.clone().into_inner();

    // Visibility, unsafe, static
    let mut visibility = Visibility::Public;
    let mut is_unsafe = false;
    let mut is_static = false;
    let mut is_extern = false;

    while let Some(peek) = inner.peek() {
        match peek.as_rule() {
            Rule::unsafe_modifier => {
                is_unsafe = true;
                inner.next();
            }
            Rule::visibility_modifier => {
                visibility = assert_visibility_modifier(&mut inner);
            }
            Rule::static_modifier => {
                is_static = true;
                inner.next();
            }
            Rule::extern_modifier => {
                is_extern = true;
                inner.next();
            }
            _ => break,
        }
    }

    // Optional generic parameters: <T: Constraint, U: …>
    let generics = parse_generic_params(&mut inner);

    // Optional region parameters: [R1 R2 …]
    let regions = parse_region_params(&mut inner);

    // Function name
    let name = inner.next().unwrap().as_str().to_string();

    // Param list
    let mut params = Vec::new();
    if inner.peek().map(|p| p.as_rule()) == Some(Rule::param_list) {
        for param in inner.next().unwrap().into_inner() {
            if param.as_rule() == Rule::param {
                params.push(parse_param_no_visibility(param));
            }
        }
    }

    // Return type
    let return_type = get_return_type(&mut inner, Rule::function_type_annotation);

    // Body or arrow expression
    let body = match inner.next().unwrap() {
        p if p.as_rule() == Rule::block => Some(parse_block(p)),
        p if p.as_rule() == Rule::arrow_expr => {
            let expr = parse_expr(p.into_inner().next().unwrap());
            Some(Block {
                block: vec![Stmt::Return(ReturnStmt { value: Some(Box::new(expr)) })],
            })
        }
        p if p.as_str() == ";" => None,
        other => panic!("Unexpected fun_decl item: {:?}", other),
    };

    FuncDecl {
        visibility,
        is_static,
        is_unsafe,
        is_extern,
        name,
        generics,
        regions,
        params,
        return_type,
        body,
    }
}

fn get_return_type(inner: &mut Pairs<Rule>, rule: Rule) -> Option<Type> {
    match inner.peek().filter(|p| p.as_rule() == rule) {
        Some(_) => Some(parse_to_type(inner.next().unwrap().as_str().split_whitespace().last().unwrap())),
        None => None,
    }
}

fn parse_param_no_visibility(pair: Pair<Rule>) -> Param {
    assert_eq!(pair.as_rule(), Rule::param);

    let mut inner = pair.into_inner();

    let type_annotation = get_type_annotation(&mut inner);
    let name = inner.next().unwrap().as_str().to_string();

    Param {
        visibility: Visibility::Private, // default for params
        name,
        type_annotation,
    }
}

fn parse_param(pair: Pair<Rule>) -> Param {
    let mut inner = pair.into_inner();
    let visibility = get_visibility(&mut inner);

    let type_annotation = get_type_annotation(&mut inner);
    let name = inner.next().unwrap().as_str().to_string();

    Param { visibility, name, type_annotation }
}

fn get_type_annotation(inner: &mut Pairs<Rule>) -> Type {
    if let Some(type_str) = inner.next() {
        assert_eq!(type_str.as_rule(), Rule::var_type);
        parse_to_type(type_str.as_str())
    } else {
        panic!("yes")
    }
}

fn parse_if_stmt(pair: &Pair<Rule>) -> Stmt {
    let mut inner = pair.clone().into_inner();

    let condition = parse_expr(inner.next().unwrap());
    let then_branch = parse_block(inner.next().unwrap());

    let mut else_branch = None;

    if let Some(next) = inner.next() {
        let branch = match next.as_rule() {
            Rule::if_stmt => If(Box::new(match parse_if_stmt(&next) {
                Stmt::If(if_stmt) => if_stmt,
                _ => panic!("Expected if_stmt"),
            })),
            Rule::block => {
                let block = parse_block(next);
                ElseBranch::Else(block)
            },
            _ => panic!("Unexpected rule in else branch"),
        };
        else_branch = Some(Box::new(branch));
    }

    Stmt::If(IfStmt {
        condition,
        then_branch,
        else_branch,
    })
}


fn parse_match_stmt(pair: &Pair<Rule>) -> Stmt {
    let mut inner = pair.clone().into_inner();
    let expr = parse_expr(inner.next().unwrap());
    let mut arms = Vec::new();

    for arm in inner {
        arms.push(parse_match_arm(arm));
    }

    Stmt::Match(MatchStmt { expr, arms })
}

fn parse_return_stmt(pair: &Pair<Rule>) -> Stmt {
    let mut inner = pair.clone().into_inner();
    let value = inner.next().map(|p| Box::new(parse_expr(p)));
    Stmt::Return(ReturnStmt { value })
}

fn parse_let_stmt(pair: &Pair<Rule>) -> Stmt {
    let mut inner = pair.clone().into_inner();

    let mutability = match inner.peek().map(|p| p.as_rule()) {
        Some(Rule::mut_keyword) => {
            inner.next();
            true
        },
        _ => false,
    };

    let type_annotation = parse_to_type(inner.next().unwrap().as_str());

    let ident = inner.next().unwrap().as_str().to_string();

    let value = Box::new(parse_expr(inner.next().expect("Expected expression after '='")));

    Stmt::Let(LetStmt {
        mutability,
        ident,
        type_annotation,
        value,
    })
}

fn parse_to_type(token_type: &str) -> Type {
    // Trim whitespace
    let token_type = token_type.trim();

    // Handle lambda types (e.g. "lambda(i32, str) -> boolean")
    if token_type.starts_with("(") {

        // Split into params and return type: "(i32, str) -> boolean"
        let parts: Vec<&str> = token_type.split("->").map(str::trim).collect();

        // Extract parameter list
        let params_part = parts[0]; // "(i32, str)"
        let params_str = params_part.trim().trim_start_matches('(').trim_end_matches(')');
        let params = if params_str.is_empty() {
            vec![]
        } else {
            params_str.split(',')
                .map(|p| parse_to_type(p.trim()))
                .collect()
        };

        // Extract return type (default to void if missing)
        let return_type = if parts.len() > 1 {
            parse_to_type(parts[1])
        } else {
            Void
        };

        return Type::Lambda {
            params,
            return_type: Box::new(return_type),
        };
    }

    // Handle primitive types or class names
    match token_type {
        "u8" => Type::U8,
        "i8" => Type::I8,
        "u16" => Type::U16,
        "i16" => Type::I16,
        "u32" => Type::U32,
        "i32" => Type::I32,
        "u64" => Type::U64,
        "i64" => Type::I64,
        "u128" => Type::U128,
        "i128" => Type::I128,
        "str" => Type::String,
        "boolean" => Type::Boolean,
        name => Type::Class(name.to_string()),
    }
}

fn parse_enum_decl(pair: &Pair<Rule>) -> Stmt {
    let mut inner = pair.clone().into_inner();

    let visibility = get_visibility(&mut inner);
    let name = inner.next().unwrap().as_str().to_string();

    let generics = parse_generic_params(&mut inner);

    let mut variants = Vec::new();
    for variant in inner {
        if variant.as_rule() == Rule::enum_variant {
            let mut variant_inner = variant.into_inner();
            let variant_name = variant_inner.next().unwrap().as_str().to_string();

            let mut fields = Vec::new();
            if let Some(params) = variant_inner.next() {
                for param in params.into_inner() {
                    fields.push(Field {
                        name: "_".to_string(), // unnamed tuple fields
                        field_type: parse_to_type(param.as_str()),
                        visibility: Visibility::Public,
                    });
                }
            }

            variants.push(EnumVariant { name: variant_name, fields });
        }
    }

    Stmt::EnumDecl(EnumDecl {
        name,
        visibility,
        generics,
        variants,
    })
}

fn parse_interface_decl(pair: &Pair<Rule>) -> Stmt {
    let mut inner = pair.clone().into_inner();

    let visibility = get_visibility(&mut inner);
    let name = inner.next().unwrap().as_str().to_string();
    let generics = parse_generic_params(&mut inner);

    let mut methods = Vec::new();
    if let Some(block) = inner.next() {
        for stmt in block.into_inner() {
            if stmt.as_rule() == Rule::fun_decl {
                methods.push(parse_fun_decl(&stmt));
            }
        }
    }

    Stmt::InterfaceDecl(InterfaceDecl {
        name,
        visibility,
        methods: if methods.is_empty() { None } else { Some(methods) },
        generics,
    })
}

fn parse_impl_block(pair: &Pair<Rule>) -> Stmt {
    let mut inner = pair.clone().into_inner();

    let generics = parse_generic_params(&mut inner);
    let interface = inner.next().unwrap().as_str().to_string();
    let target = inner.next().unwrap().as_str().to_string();

    let mut methods = Vec::new();
    let block = inner.next().unwrap();
    for stmt in block.into_inner() {
        if stmt.as_rule() == Rule::fun_decl {
            methods.push(parse_fun_decl(&stmt));
        }
    }

    Stmt::ImplDecl(ImplDecl {
        generics,
        interface,
        target,
        methods: if methods.is_empty() { None } else { Some(methods) },
    })
}

fn parse_import_stmt(pair: &Pair<Rule>) -> Stmt {
    let mut inner = pair.clone().into_inner();
    let path = inner.next().unwrap().as_str().to_string();
    Stmt::Import(ImportStmt { path })
}

fn parse_match_arm(pair: Pair<Rule>) -> MatchArm {
    let mut inner = pair.into_inner();
    let pattern = parse_pattern(inner.next().unwrap()); // Parse the pattern
    let block = parse_block(inner.next().unwrap()); // Parse the block for the match arm
    MatchArm { pattern, block }
}

fn parse_string(raw: &str) -> String {
    let unquoted = &raw[1..raw.len() - 1]; // strip the outer quotes

    let mut result = String::new();
    let mut chars = unquoted.chars().peekable();

    while let Some(c) = chars.next() {
        if c == '\\' {
            match chars.next() {
                Some('"') => result.push('"'),
                Some('\\') => result.push('\\'),
                Some('n') => result.push('\n'),
                Some('t') => result.push('\t'),
                Some(other) => {
                    // Unknown escape: keep it as-is
                    result.push('\\');
                    result.push(other);
                }
                None => break,
            }
        } else {
            result.push(c);
        }
    }

    result
}



fn parse_pattern(pair: Pair<Rule>) -> Pattern {
    match pair.as_rule() {
        Rule::ident => Pattern::Ident(pair.as_str().to_string()),
        Rule::number => Pattern::Number(pair.as_str().parse().unwrap()),
        Rule::string => Pattern::String(parse_string(pair.as_str())),
        Rule::tuple_pattern => {
            let inner = pair.into_inner();
            let mut patterns = Vec::new();
            for p in inner {
                patterns.push(parse_pattern(p));
            }
            Pattern::Tuple(patterns)
        },
        Rule::wildcard_pattern => Pattern::Wildcard,
        _ => panic!("Unexpected pattern: {:?}", pair.as_rule()),
    }
}


pub fn parse_block(pair: Pair<Rule>) -> Block {
    let mut stmts = Vec::new();
    let stmts_ref: &mut Vec<Stmt> = &mut stmts;
    for stmt in pair.into_inner() {
        parse_stmt(stmts_ref, stmt);
    }
    Block { block: stmts }
}

fn parse_expr(pair: Pair<Rule>) -> Expr {
    match pair.as_rule() {
        Rule::boolean => {
            let val = match pair.as_str() {
                "true" => true,
                "false" => false,
                _ => unreachable!(),
            };
            Expr::Boolean(val)
        },
        Rule::number => Expr::Number(pair.as_str().parse().unwrap()),
        Rule::string => Expr::String(parse_string(pair.as_str())),
        Rule::ident => Expr::Ident(pair.as_str().to_string()),

        Rule::assignment => {
            let mut inner = pair.clone().into_inner();
            let first = inner.next().unwrap();

            let lhs = parse_expr(first);
            if let Some(op_pair) = inner.next() {
                let op = op_pair.as_str().to_string();
                let rhs = parse_expr(inner.next().unwrap());
                Expr::Assignment { lhs: Box::new(lhs), op, rhs: Box::new(rhs) }
            } else {
                lhs
            }
        }

        Rule::term => parse_binary_expr(pair.into_inner(), |op| match op {
            "+" => Op::Add,
            "-" => Op::Sub,
            _ => panic!("Unknown term op: {}", op),
        }),

        Rule::factor => parse_binary_expr(pair.into_inner(), |op| match op {
            "*" => Op::Mul,
            "/" => Op::Div,
            "%" => Op::Mod,
            _ => panic!("Unknown factor op: {}", op),
        }),

        Rule::lhs => parse_expr(pair.into_inner().next().unwrap()),

        Rule::expr
        | Rule::logic_or
        | Rule::logic_and
        | Rule::bit_or
        | Rule::bit_xor
        | Rule::bit_and
        | Rule::equality
        | Rule::shift
        | Rule::unary => {
            // descend to the actual operand
            parse_expr(pair.into_inner().next().unwrap())
        }

        Rule::comparison => {
            let mut inner = pair.into_inner();
            let mut lhs = parse_expr(inner.next().unwrap());

            while let Some(op_pair) = inner.next() {
                let rhs = parse_expr(inner.next().unwrap());
                let op = match op_pair.as_str() {
                    "!=" => ComparisonOp::NotEqual,
                    "==" => ComparisonOp::Equal,
                    "<" => ComparisonOp::LessThan,
                    "<=" => ComparisonOp::LessThanOrEqual,
                    ">" => ComparisonOp::GreaterThan,
                    ">=" => ComparisonOp::GreaterThanOrEqual,
                    _ => unreachable!(),
                };

                lhs = Expr::Comparison {
                    lhs: Box::new(lhs),
                    op,
                    rhs: Box::new(rhs),
                };
            }

            lhs
        }

        Rule::parenthesized_expr => {
            let inner = pair.into_inner().next().unwrap();
            parse_expr(inner)
        }

        Rule::class_initialization => {
            let mut inner = pair.into_inner();
            let class_name = parse_expr(inner.next().unwrap());

            let arguments = if let Some(arg_group) = inner.next() {
                arg_group.into_inner().map(parse_expr).collect()
            } else {
                vec![]
            };

            Expr::ClassInit {
                callee: Box::new(class_name),
                arguments,
            }
        }

        Rule::primary => {
            let mut inner = pair.into_inner();
            let mut expr = parse_expr(inner.next().unwrap());

            for next in inner {
                match next.as_rule() {
                    Rule::call_args => {
                        let args = next
                            .into_inner()
                            .map(parse_expr)
                            .collect::<Vec<Expr>>();

                        expr = Expr::Call {
                            callee: Box::new(expr),
                            arguments: args,
                        };
                    }
                    Rule::ident => {
                        // This is the .field part
                        expr = Expr::Get {
                            object: Box::new(expr),
                            field: next.as_str().to_string(),
                        };
                    }
                    _ => unreachable!("Unexpected rule in primary tail: {:?}", next.as_rule())
                }
            }

            expr
        }

        _ => panic!("Unexpected expression rule: {:?}", pair.as_rule()),
    }
}

fn parse_binary_expr(mut inner: Pairs<Rule>, op_from_str: fn(&str) -> Op) -> Expr {
    let mut expr = parse_expr(inner.next().unwrap());

    while let Some(next) = inner.next() {
        match next.as_rule() {
            Rule::operator_add_sub | Rule::operator_mul_div | Rule::comparison_op /* etc */ => {
                let op = op_from_str(next.as_str());
                let rhs = parse_expr(inner.next().expect("Expected right-hand side of binary expr"));
                expr = Expr::Binary {
                    left: Box::new(expr),
                    op,
                    right: Box::new(rhs),
                };
            }

            // This prevents numbers or identifiers from being interpreted as operators
            other => panic!("Expected binary operator, got {:?}", other),
        }
    }

    expr
}
#[cfg(test)]
mod parser_diff_tests {
    // Import only what we need from the parent module to keep scope clean.
    use super::{parse_string, parse_to_type, Type};

    #[test]
    fn parse_string_handles_basic_and_known_escapes() {
        // Basic, no escapes
        assert_eq\!(parse_string("\"hello\""), "hello");

        // Known escapes: \n, \t, \", \\
        assert_eq\!(parse_string("\"a\\n\\t\\\"\\\\b\""), "a\n\t\"\\b");
        // Note: 'b' is an unknown escape; expected to be kept as two chars: backslash + 'b'
    }

    #[test]
    fn parse_string_preserves_unknown_escape_sequences() {
        // Unknown escape \x should remain as backslash + 'x'
        assert_eq\!(parse_string("\"prefix\\\\xsuffix\""), "prefix\\xsuffix");

        // Mixed content with multiple unknown escapes
        assert_eq\!(parse_string("\"_\\\\q_\\\\Z_\""), "_\\q_\\Z_");
    }

    #[test]
    fn parse_to_type_parses_primitives_and_class_names() {
        assert\!(matches\!(parse_to_type("u8"),   Type::U8));
        assert\!(matches\!(parse_to_type("i32"),  Type::I32));
        assert\!(matches\!(parse_to_type("u128"), Type::U128));
        assert\!(matches\!(parse_to_type("str"),  Type::String));
        assert\!(matches\!(parse_to_type("boolean"), Type::Boolean));

        // Unknown tokens map to Class(name)
        match parse_to_type("MyCustomType") {
            Type::Class(name) => assert_eq\!(name, "MyCustomType"),
            _ => panic\!("expected Type::Class for unknown token"),
        }
    }

    #[test]
    fn parse_to_type_trims_whitespace_around_tokens() {
        assert\!(matches\!(parse_to_type("  i64  "), Type::I64));
        match parse_to_type("   FooBar  ") {
            Type::Class(name) => assert_eq\!(name, "FooBar"),
            _ => panic\!("expected Type::Class after trimming"),
        }
    }

    #[test]
    fn parse_to_type_parses_simple_lambda_types() {
        // (i32, str) -> boolean
        let ty = parse_to_type("(i32, str) -> boolean");
        if let Type::Lambda { params, return_type } = ty {
            assert_eq\!(params.len(), 2);
            assert\!(matches\!(params[0], Type::I32));
            assert\!(matches\!(params[1], Type::String));
            assert\!(matches\!(*return_type, Type::Boolean));
        } else {
            panic\!("expected Type::Lambda");
        }
    }

    #[test]
    fn parse_to_type_lambda_no_params_and_whitespace() {
        // ( ) -> i32 with extra spaces
        let ty = parse_to_type(" ( ) ->  i32 ");
        if let Type::Lambda { params, return_type } = ty {
            assert\!(params.is_empty());
            assert\!(matches\!(*return_type, Type::I32));
        } else {
            panic\!("expected Type::Lambda");
        }
    }

    #[test]
    fn parse_to_type_lambda_defaults_to_void_return_when_missing() {
        // (i32, str)  // no '-> return'
        let ty = parse_to_type("(i32, str)");
        if let Type::Lambda { params, return_type } = ty {
            assert_eq\!(params.len(), 2);
            // Default to Void when return type is omitted
            assert\!(matches\!(*return_type, Type::Void));
        } else {
            panic\!("expected Type::Lambda");
        }
    }

    #[test]
    fn parse_to_type_nested_lambda_return_types() {
        // (i32) -> (str) -> boolean
        let ty = parse_to_type("(i32)->(str)->boolean");
        if let Type::Lambda { params, return_type } = ty {
            assert_eq\!(params.len(), 1);
            assert\!(matches\!(params[0], Type::I32));

            if let Type::Lambda { params: inner_params, return_type: inner_ret } = *return_type {
                assert_eq\!(inner_params.len(), 1);
                assert\!(matches\!(inner_params[0], Type::String));
                assert\!(matches\!(*inner_ret, Type::Boolean));
            } else {
                panic\!("expected nested Type::Lambda in return position");
            }
        } else {
            panic\!("expected outer Type::Lambda");
        }
    }
}