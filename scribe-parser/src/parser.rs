use std::cell::RefCell;
use std::panic::{catch_unwind, AssertUnwindSafe};
use std::rc::Rc;
use pest_derive::Parser;

use pest::iterators::{Pair, Pairs};
use pest::Parser;

use ir::ast::ElseBranch::If;
use ir::ast::Op;
use ir::ast::Stmt::ExprStmt;
use ir::ast::Type::Void;
use ir::ast::*;
use ir::context::Context;
use ir::hir::StrId;

pub fn parse_program<'a>(input: &str, context: Rc<RefCell<Context<'a>>>) -> Result<Vec<Stmt>, pest::error::Error<Rule>> {
    let mut pairs = ZetaParser::parse(Rule::program, input)?;
    /*for pair in pairs.clone() {
        println!("{:?}\n", pair);
    }*/

    let program_pair = pairs.clone().into_iter().next().expect("Expected program rule");
    assert_eq!(program_pair.as_rule(), Rule::program);

    let program_pair = pairs.next().unwrap(); // Rule::program
    
    let mut stmts = Vec::new();
    let mut zeta_parser = ZetaParser { context };
    for pair in program_pair.into_inner() {
        if ZetaParser::parse_stmt(&mut zeta_parser, &mut stmts, pair) { continue; }
    }

    Ok(stmts)
}

#[derive(Parser)]
#[grammar = "grammar.pest"]
pub struct ZetaParser<'a> {
    pub context: Rc<RefCell<Context<'a>>>
}

impl<'a> ZetaParser<'a> {
    fn parse_stmt(zeta_parser: &mut ZetaParser, stmts: &mut Vec<Stmt>, pair: Pair<Rule>) -> bool {
        match pair.as_rule() {
            Rule::impl_decl => {
                let stmt = zeta_parser.parse_impl_decl(&pair);
                stmts.push(stmt);
            }
            Rule::package_stmt => {
                let stmt = zeta_parser.parse_package_stmt(&pair);
                stmts.push(stmt);
            }
            Rule::import_stmt => {
                let stmt = zeta_parser.parse_import_stmt(&pair);
                stmts.push(stmt);
            }
            Rule::let_stmt => {
                let stmt = zeta_parser.parse_let_stmt(&pair);
                stmts.push(stmt);
            }
            Rule::return_stmt => {
                let stmt = zeta_parser.parse_return_stmt(&pair);
                stmts.push(stmt);
            }
            Rule::if_stmt => {
                let stmt = zeta_parser.parse_if_stmt(&pair);
                stmts.push(stmt);
            }
            Rule::while_stmt => {
                let stmt = zeta_parser.parse_while_stmt(&pair);
                stmts.push(stmt);
            }
            Rule::for_stmt => {
                let stmt = zeta_parser.parse_for_stmt(&pair);
                stmts.push(stmt);
            }
            Rule::match_stmt => {
                let stmt = zeta_parser.parse_match_stmt(&pair);
                stmts.push(stmt);
            }
            Rule::unsafe_block => {
                let stmt = zeta_parser.parse_unsafe_block(&pair);
                stmts.push(stmt);
            }
            Rule::fun_decl => {
                let stmt = Stmt::FuncDecl(zeta_parser.parse_fun_decl(&pair));
                stmts.push(stmt);
            }
            Rule::struct_decl => {
                let stmt = zeta_parser.parse_class_decl(&pair);
                stmts.push(stmt);
            }
            Rule::expr_stmt => {
                let stmt = zeta_parser.parse_expr_stmt(&pair);
                stmts.push(stmt);
            }

            Rule::EOI => return true,
            other => panic!("Unexpected rule in program: {:?}", other),
        }
        false
    }

    fn parse_impl_decl(&mut self, pair: &Pair<Rule>) -> Stmt  {
        let mut inner = pair.clone().into_inner();

        // Parse generics if present
        let (generics, _interface_pair) = if inner.peek().unwrap().as_rule() == Rule::generic_params {
            (self.parse_generic_params(&mut inner), inner.next().unwrap())
        } else {
            (None, inner.next().unwrap())
        };

        let interface = StrId(self.context.borrow_mut().string_pool.intern(inner.next().unwrap().as_str()));
        let target = StrId(self.context.borrow_mut().string_pool.intern(inner.next().unwrap().as_str()));

        let next = inner.next();
        let methods = if let Some(body) = next {
            if body.as_rule() == Rule::fun_decl {
                let mut funcs = vec![self.parse_fun_decl(&body)];
                for sub in inner {
                    funcs.push(self.parse_fun_decl(&sub));
                }
                Some(funcs)
            } else {
                None
            }
        } else {
            None
        };

        Stmt::ImplDecl(ImplDecl {
            generics,
            interface,
            target,
            methods,
        })
    }

    fn parse_while_stmt(&mut self, pair: &Pair<Rule>) -> Stmt {
        let mut inner = pair.clone().into_inner();
        let value: Option<Box<Expr>> = inner.next().map(|p| Box::new(self.parse_expr(p)));

        match value {
            Some(expr) => Stmt::While(WhileStmt {
                condition: expr,
                block: self.parse_block(inner.next().unwrap())
            }),
            None => panic!("Expected expression in while loop")
        }
    }

    fn parse_package_stmt(&mut self, pair: &Pair<Rule>) -> Stmt {
        let mut inner = pair.clone().into_inner();
        let path = StrId(self.context.borrow_mut().string_pool.intern(inner.next().unwrap().as_str()));
        Stmt::Package(PackageStmt { path })
    }

    fn parse_for_stmt(&mut self, pair: &Pair<Rule>) -> Stmt {
        let mut inner = pair.clone().into_inner();

        // initializer: optional
        let initializer_pair = inner.next().unwrap();
        let initializer = if initializer_pair.as_rule() == Rule::let_stmt {
            match self.parse_let_stmt(&initializer_pair) {
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
            Some(Box::new(self.parse_expr(condition_pair)))
        } else {
            None
        };

        // increment: optional
        let increment_pair = inner.next().unwrap();
        let increment = if increment_pair.as_rule() == Rule::expr {
            Some(Box::new(self.parse_expr(increment_pair)))
        } else {
            None
        };

        let body = self.parse_block(inner.next().unwrap());

        Stmt::For(ForStmt {
            let_stmt: initializer,
            condition,
            increment,
            block: body,
        })
    }

    fn parse_unsafe_block(&mut self, pair: &Pair<Rule>) -> Stmt {
        let mut inner = pair.clone().into_inner();
        let block = self.parse_block(inner.next().unwrap());  // Parse the matched expression
        Stmt::UnsafeBlock(UnsafeBlock { block })
    }

    fn parse_expr_stmt(&mut self, pair: &Pair<Rule>) -> Stmt {
        let expr = self.parse_expr(pair.clone().into_inner().next().unwrap());
        ExprStmt(InternalExprStmt { expr: Box::new(expr) })
    }

    fn parse_class_decl(&mut self, pair: &Pair<Rule>) -> Stmt {
        let mut inner = pair.clone().into_inner();

        // Visibility (public/private/internal)
        let visibility = self.get_visibility(&mut inner);

        // Optional generic parameters: <T: Constraint, U: …>
        let generics = self.parse_generic_params(&mut inner);

        // Class keyword already consumed; now the identifier
        let name: StrId = StrId(self.context.borrow_mut().string_pool.intern(inner.next().unwrap().as_str()));

        // Optional constructor parameters (x: T, …)
        let params = if inner.peek().map(|p| p.as_rule()) == Some(Rule::param_list) {
            let param_list = inner.next().unwrap();
            Some(param_list.into_inner().map(|param| self.parse_param(param)).collect())
        } else {
            None
        };

        Stmt::ClassDecl(ClassDecl {
            visibility,
            name,
            generics,
            params,
        })
    }

    fn parse_generic_params(&mut self, inner: &mut Pairs<Rule>) -> Option<Vec<Generic>> {
        if inner.peek().map(|p| p.as_rule()) != Some(Rule::generic_params) {
            return None;
        }

        let gen_pair = inner.next().unwrap();
        let params = gen_pair.into_inner().map(|pair| self.map_pair_to_generic(pair)).collect();
        
        Some(params)
    }

    fn map_pair_to_generic(&mut self, p: Pair<Rule>) -> Generic {
        let mut parts = p.into_inner();
        let name = StrId(self.context.borrow_mut().string_pool.intern(parts.next().unwrap().as_str()));

        // Collect all constraints after ":"
        let mut constraints = Vec::new();
        for part in parts {
            constraints.push(StrId(self.context.borrow_mut().string_pool.intern(part.as_str())));
        }

        Generic { const_generic: false, type_name: name, constraints }
    }

    fn get_visibility(&mut self, inner: &mut Pairs<Rule>) -> Visibility {
        match inner.peek().map(|p| p.as_rule()) {
            Some(Rule::public_keyword | Rule::private_keyword | Rule::module_keyword | Rule::package_keyword) => {
                Self::assert_visibility_modifier(inner)
            }
            _ => Visibility::Public,
        }
    }

    fn assert_visibility_modifier(inner: &mut Pairs<Rule>) -> Visibility {
        match inner.next().unwrap().as_str() {
            "public" => Visibility::Public,
            "private" => Visibility::Private,
            "module" => Visibility::Module,
            "package" => Visibility::Package,
            fetched => panic!("Expected visibility modifier, got {}", fetched)
        }
    }

    fn parse_fun_decl(&mut self, pair: &Pair<Rule>) -> FuncDecl {
        let mut inner = pair.clone().into_inner();

        // Visibility, unsafe, static
        let mut visibility = Visibility::Public;
        let mut is_unsafe = false;
        let mut is_extern = false;
        let mut extern_string: Option<StrId> = None;
        let mut inline = false;
        let mut no_inline = false;

        while let Some(peek) = inner.peek() {
            match peek.as_rule() {
                Rule::unsafe_modifier => {
                    is_unsafe = true;
                    inner.next();
                }
                Rule::visibility_modifier => {
                    visibility = Self::assert_visibility_modifier(&mut inner);
                }
                Rule::inline_keyword => {
                    inline = true;
                    inner.next();
                }
                Rule::noinline_keyword => {
                    no_inline = true;
                    inner.next();
                }
                Rule::extern_modifier => {
                    is_extern = true;
                    match inner.next() {
                        Some(rule) if rule.as_rule() == Rule::string => {
                            extern_string = Some(StrId(self.context.borrow_mut().string_pool.intern(rule.as_str())));
                            inner.next();
                        }
                        _ => todo!()
                    }
                }
                _ => break,
            }
        }

        debug_assert!(!inline && !no_inline);

        // Optional generic parameters: <T: Constraint, U: …>
        let generics = self.parse_generic_params(&mut inner);

        // Function name
        let name = StrId(self.context.borrow_mut().string_pool.intern(inner.next().unwrap().as_str()));

        // Param list
        let mut params = Vec::new();
        if inner.peek().map(|p| p.as_rule()) == Some(Rule::param_list) {
            for param in inner.next().unwrap().into_inner() {
                if param.as_rule() == Rule::param {
                    params.push(self.parse_param(param));
                }
            }
        }

        // Return type
        let return_type = self.get_return_type(&mut inner, Rule::function_type_annotation);

        // Body or arrow expression
        let body = match inner.next().unwrap() {
            p if p.as_rule() == Rule::block => Some(self.parse_block(p)),
            p if p.as_rule() == Rule::arrow_expr => {
                let expr = self.parse_expr(p.into_inner().next().unwrap());
                Some(Block {
                    block: vec![Stmt::Return(ReturnStmt { value: Some(Box::new(expr)) })],
                })
            }
            p if p.as_str() == ";" => None,
            other => panic!("Unexpected fun_decl item: {:?}", other),
        };

        FuncDecl {
            visibility,
            is_unsafe,
            is_extern,
            extern_string,
            name,
            generics,
            params,
            return_type,
            body,
        }
    }

    fn get_return_type(&mut self, inner: &mut Pairs<Rule>, rule: Rule) -> Option<Type> {
        match inner.peek().filter(|p| p.as_rule() == rule) {
            Some(_) => Some(self.parse_to_type(inner.next().unwrap().as_str().split_whitespace().last().unwrap())),
            None => None,
        }
    }

    fn parse_param(&mut self, pair: Pair<Rule>) -> Param {
        let mut inner = pair.into_inner();
        let visibility = self.get_visibility(&mut inner);

        let is_mut = match inner.peek() {
            Some(rule) if rule.as_rule() == Rule::mut_keyword => {
                inner.next(); // consume the mut keyword
                true
            },
            _ => false
        };

        let is_move = match inner.peek() {
            Some(rule) if rule.as_rule() == Rule::own_keyword => {
                inner.next(); // consume the own keyword
                true
            },
            _ => false
        };

        let type_annotation = self.get_type_annotation(&mut inner);

        if let Some(Type::Class(class_name, generic_args)) = &type_annotation {
            if generic_args.iter().any(|t| matches!(t, Type::This)) {
                return Param::This(ThisParam { 
                    is_mut, 
                    is_move, 
                    type_annotation: Some(Type::Class { name: class_name.clone(), generics: generic_args.clone() })
                });
            }
        }

        if let Some(Type::This) = &type_annotation {
            return Param::This(ThisParam { 
                is_mut, 
                is_move, 
                type_annotation 
            });
        }

        let name = StrId(self.context.borrow_mut().string_pool.intern(inner.next().unwrap().as_str()));

        let default_value = match inner.peek() {
            Some(rule) => {
                inner.next();
                catch_unwind(AssertUnwindSafe(|| self.parse_expr(rule))).ok()
            }
            None => None
        };

        Param::Normal(NormalParam { is_mut, is_move, visibility, name, type_annotation, default_value })
    }

    fn get_type_annotation(&mut self, inner: &mut Pairs<Rule>) -> Option<Type> {
        match inner.peek() {
            Some(type_str) if type_str.as_rule() == Rule::var_type => {
                Some(self.parse_to_type(type_str.as_str()))
            }
            Some(_) => None,
            None => unreachable!()
        }
    }

    fn parse_if_stmt(&mut self, pair: &Pair<Rule>) -> Stmt {
        let mut inner = pair.clone().into_inner();

        let condition = self.parse_expr(inner.next().unwrap());
        let then_branch = self.parse_block(inner.next().unwrap());

        let mut else_branch = None;

        if let Some(next) = inner.next() {
            let branch = match next.as_rule() {
                Rule::if_stmt => If(Box::new(match self.parse_if_stmt(&next) {
                    Stmt::If(if_stmt) => if_stmt,
                    _ => panic!("Expected if_stmt"),
                })),
                Rule::block => {
                    let block = self.parse_block(next);
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


    fn parse_match_stmt(&mut self, pair: &Pair<Rule>) -> Stmt {
        let mut inner = pair.clone().into_inner();
        let expr = self.parse_expr(inner.next().unwrap());
        let mut arms = Vec::new();

        for arm in inner {
            arms.push(self.parse_match_arm(arm));
        }

        Stmt::Match(MatchStmt { expr, arms })
    }

    fn parse_return_stmt(&mut self, pair: &Pair<Rule>) -> Stmt {
        let mut inner = pair.clone().into_inner();
        let value = inner.next().map(|p| Box::new(self.parse_expr(p)));
        Stmt::Return(ReturnStmt { value })
    }

    fn parse_let_stmt(&mut self, pair: &Pair<Rule>) -> Stmt {
        let mut inner = pair.clone().into_inner();

        let mutability = match inner.peek().map(|p| p.as_rule()) {
            Some(Rule::mut_keyword) => {
                inner.next();
                true
            },
            _ => false,
        };

        let type_annotation = self.parse_to_type(inner.next().unwrap().as_str());

        let ident = StrId(self.context.borrow_mut().string_pool.intern(inner.next().unwrap().as_str()));

        let value = Box::new(self.parse_expr(inner.next().expect("Expected expression after '='")));

        Stmt::Let(LetStmt {
            mutability,
            ident,
            type_annotation,
            value,
        })
    }

    fn parse_to_type(&mut self, token_type: &str) -> Type {
        // Trim whitespace
        let token_type = token_type.trim();

        // Handle generic types like "Ptr<this>"
        if let Some(open_angle) = token_type.find('<') {
            let close_angle = token_type.rfind('>').expect("Unclosed angle bracket in type");
            let type_name = &token_type[..open_angle];
            let inner_types = &token_type[open_angle + 1..close_angle];
            
            let generic_args: Vec<Type> = inner_types
                .split(',')
                .map(|s| self.parse_to_type(s.trim()))
                .collect();
                
            return Type::Class(
                StrId(self.context.borrow_mut().string_pool.intern(type_name)),
                generic_args
            );
        }

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
                    .map(|p| self.parse_to_type(p.trim()))
                    .collect()
            };

            // Extract return type (default to void if missing)
            let return_type = if parts.len() > 1 {
                self.parse_to_type(parts[1])
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
            "this" => Type::This,
            "boolean" => Type::Boolean,
            name => Type::Class(
                StrId(self.context.borrow_mut().string_pool.intern(name)),
                Vec::new()
            ),
        }
    }

    fn parse_enum_decl(&mut self, pair: &Pair<Rule>) -> Stmt {
        let mut inner = pair.clone().into_inner();

        let visibility = self.get_visibility(&mut inner);
        let name = StrId(self.context.borrow_mut().string_pool.intern(inner.next().unwrap().as_str()));

        let generics = self.parse_generic_params(&mut inner);

        let mut variants = Vec::new();
        for variant in inner {
            if variant.as_rule() == Rule::enum_variant {
                let mut variant_inner = variant.into_inner();
                let variant_name = StrId(self.context.borrow_mut().string_pool.intern(variant_inner.next().unwrap().as_str()));

                let mut fields = Vec::new();
                if let Some(params) = variant_inner.next() {
                    for param in params.into_inner() {
                        let name = {
                            let mut ctx = self.context.borrow_mut();
                            StrId(ctx.string_pool.intern("_"))
                        };

                        let field_type = self.parse_to_type(param.as_str());

                        fields.push(Field {
                            name,
                            field_type,
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

    fn parse_interface_decl(&mut self, pair: &Pair<Rule>) -> Stmt {
        let mut inner = pair.clone().into_inner();

        let visibility = self.get_visibility(&mut inner);
        let name = StrId(self.context.borrow_mut().string_pool.intern(inner.next().unwrap().as_str()));
        let generics = self.parse_generic_params(&mut inner);

        let mut methods = Vec::new();
        if let Some(block) = inner.next() {
            for stmt in block.into_inner() {
                if stmt.as_rule() == Rule::fun_decl {
                    methods.push(self.parse_fun_decl(&stmt));
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

    fn parse_impl_block(&mut self, pair: &Pair<Rule>) -> Stmt {
        let mut inner = pair.clone().into_inner();

        let generics = self.parse_generic_params(&mut inner);
        let interface = StrId(self.context.borrow_mut().string_pool.intern(inner.next().unwrap().as_str()));
        let target = StrId(self.context.borrow_mut().string_pool.intern(inner.next().unwrap().as_str()));

        let mut methods = Vec::new();
        let block = inner.next().unwrap();
        for stmt in block.into_inner() {
            if stmt.as_rule() == Rule::fun_decl {
                methods.push(self.parse_fun_decl(&stmt));
            }
        }

        Stmt::ImplDecl(ImplDecl {
            generics,
            interface,
            target,
            methods: if methods.is_empty() { None } else { Some(methods) },
        })
    }

    fn parse_import_stmt(&mut self, pair: &Pair<Rule>) -> Stmt {
        let mut inner = pair.clone().into_inner();
        let path = StrId(self.context.borrow_mut().string_pool.intern(inner.next().unwrap().as_str()));
        Stmt::Import(ImportStmt { path })
    }

    fn parse_match_arm(&mut self, pair: Pair<Rule>) -> MatchArm {
        let mut inner = pair.into_inner();
        let pattern = self.parse_pattern(inner.next().unwrap()); // Parse the pattern
        let guard = match inner.peek() {
            Some(pair) => {
                inner.next().unwrap();
                Some(self.parse_expr(pair))
            },
            _ => None
        };
        
        let block = self.parse_block(inner.next().unwrap()); // Parse the block for the match arm
        MatchArm { pattern, guard, block }
    }

    pub fn parse_string(&mut self, raw: &str) -> StrId {
        debug_assert!(raw.len() >= 2 && raw.starts_with('"') && raw.ends_with('"'));

        let unquoted = &raw[1..raw.len() - 1]; // strip outer quotes
        let bytes = unquoted.as_bytes();

        // Worst-case: output is same length as input (escapes never expand beyond 1 char here).
        let mut out = Vec::with_capacity(bytes.len());

        let mut i = 0;
        while i < bytes.len() {
            match bytes[i] {
                b'\\' if i + 1 < bytes.len() => {
                    match bytes[i + 1] {
                        b'"' => out.push(b'"'),
                        b'\\' => out.push(b'\\'),
                        b'n' => out.push(b'\n'),
                        b't' => out.push(b'\t'),
                        other => {
                            out.push(b'\\');
                            out.push(other);
                        }
                    }
                    i += 2;
                }
                c => {
                    out.push(c);
                    i += 1;
                }
            }
        }

        // Write directly into the StringPool buffer
        StrId(self.context.borrow_mut().string_pool.intern_bytes(&out))
    }



    fn parse_pattern(&mut self, pair: Pair<Rule>) -> Pattern {
        match pair.as_rule() {
            Rule::ident => Pattern::Ident(StrId(self.context.borrow_mut().string_pool.intern(pair.as_str()))),
            Rule::number => Pattern::Number(pair.as_str().parse().unwrap()),
            Rule::string => Pattern::String(self.parse_string(pair.as_str())),
            Rule::tuple_pattern => {
                let inner = pair.into_inner();
                let mut patterns = Vec::new();
                for p in inner {
                    patterns.push(self.parse_pattern(p));
                }
                Pattern::Tuple(patterns)
            },
            Rule::wildcard_pattern => Pattern::Wildcard,
            _ => panic!("Unexpected pattern: {:?}", pair.as_rule()),
        }
    }


    pub fn parse_block(&mut self, pair: Pair<Rule>) -> Block {
        let mut stmts = Vec::new();
        for stmt in pair.into_inner() {
            Self::parse_stmt(self, &mut stmts, stmt);
        }
        Block { block: stmts }
    }

    fn parse_expr(&mut self, pair: Pair<Rule>) -> Expr {
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
            Rule::string => Expr::String(self.parse_string(pair.as_str())),
            Rule::ident => Expr::Ident(StrId(self.context.borrow_mut().string_pool.intern(pair.as_str()))),

            Rule::assignment => {
                let mut inner = pair.clone().into_inner();
                let first = inner.next().unwrap();

                let lhs = self.parse_expr(first);
                if let Some(op_pair) = inner.next() {
                    let op = op_pair.as_str().into();
                    let rhs = self.parse_expr(inner.next().unwrap());
                    Expr::Assignment { lhs: Box::new(lhs), op, rhs: Box::new(rhs) }
                } else {
                    lhs
                }
            }

            Rule::term => self.parse_binary_expr(pair.into_inner(), |op| match op {
                "+" => Op::Add,
                "-" => Op::Sub,
                _ => panic!("Unknown term op: {}", op),
            }),

            Rule::factor => self.parse_binary_expr(pair.into_inner(), |op| match op {
                "*" => Op::Mul,
                "/" => Op::Div,
                "%" => Op::Mod,
                _ => panic!("Unknown factor op: {}", op),
            }),

            Rule::lhs => self.parse_expr(pair.into_inner().next().unwrap()),

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
                self.parse_expr(pair.into_inner().next().unwrap())
            }

            Rule::comparison => {
                let mut inner = pair.into_inner();
                let mut lhs = self.parse_expr(inner.next().unwrap());

                while let Some(op_pair) = inner.next() {
                    let rhs = self.parse_expr(inner.next().unwrap());
                    let op = match op_pair.as_str() {
                        "!=" => Op::Neq,
                        "==" => Op::Eq,
                        "<" => Op::Lt,
                        "<=" => Op::Lte,
                        ">" => Op::Gt,
                        ">=" => Op::Gte,
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
                self.parse_expr(inner)
            }

            Rule::class_initialization => {
                let mut inner = pair.into_inner();
                let class_name = self.parse_expr(inner.next().unwrap());

                let arguments = if let Some(arg_group) = inner.next() {
                    arg_group.into_inner().map(|rule| self.parse_expr(rule)).collect()
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
                let mut expr = self.parse_expr(inner.next().unwrap());

                for next in inner {
                    match next.as_rule() {
                        Rule::call_args => {
                            let args = next
                                .into_inner()
                                .map(|rule| self.parse_expr(rule))
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
                                field: StrId(self.context.borrow_mut().string_pool.intern(next.as_str())),
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

    fn parse_binary_expr(&mut self, mut inner: Pairs<Rule>, op_from_str: fn(&str) -> Op) -> Expr {
        let mut expr = self.parse_expr(inner.next().unwrap());

        while let Some(next) = inner.next() {
            match next.as_rule() {
                Rule::operator_add_sub | Rule::operator_mul_div | Rule::comparison_op /* etc */ => {
                    let op = op_from_str(next.as_str());
                    let rhs = self.parse_expr(inner.next().expect("Expected right-hand side of binary expr"));
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
}