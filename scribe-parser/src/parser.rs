use pest_derive::Parser;
use std::cell::RefCell;
use std::panic::{AssertUnwindSafe, catch_unwind};
use std::rc::Rc;
use std::vec;
use pest::Parser;
use pest::iterators::{Pair, Pairs};

use ir::ast::ElseBranch::If;
use ir::ast::Op;
use ir::ast::Stmt::ExprStmt;
use ir::ast::*;
use ir::context::Context;
use ir::hir::StrId;
use ir::span::SourceSpan;

pub fn parse_program<'a>(
    input: &'a str,
    file_name: &'a str,
    context: Rc<RefCell<Context<'a>>>,
) -> Result<Vec<Stmt>, pest::error::Error<Rule>> {
    let mut pairs = ZetaParser::parse(Rule::program, input)?;
    /*for pair in pairs.clone() {
        println!("{:?}\n", pair);
    }*/

    let program_pair = pairs
        .clone()
        .into_iter()
        .next()
        .expect("Expected program rule");
    assert_eq!(program_pair.as_rule(), Rule::program);

    let program_pair = pairs.next().unwrap(); // Rule::program

    let mut stmts = Vec::new();
    let mut zeta_parser = ZetaParser { context, file_name: file_name.to_string() };
    for pair in program_pair.into_inner() {
        if ZetaParser::parse_stmt(&mut zeta_parser, &mut stmts, pair) {
            continue;
        }
    }

    Ok(stmts)
}

#[derive(Parser)]
#[grammar = "grammar.pest"]
pub struct ZetaParser<'a> {
    pub context: Rc<RefCell<Context<'a>>>,
    pub file_name: String
}

impl<'a> ZetaParser<'a> {
    fn span_from_pair(&self, pair: &Pair<Rule>) -> SourceSpan {
        let (line, column) = pair.as_span().start_pos().line_col();
        SourceSpan { file_name: self.file_name.clone(), line, column, }
    }

    fn parse_stmt<'b>(zeta_parser: &mut ZetaParser<'b>, stmts: &mut Vec<Stmt>, pair: Pair<'b, Rule>) -> bool {
        match pair.as_rule() {
            Rule::impl_decl => {
                let stmt = zeta_parser.parse_impl_decl(&pair);
                stmts.push(stmt);
            }
            Rule::enum_decl => {
                let stmt = zeta_parser.parse_enum_decl(&pair);
                stmts.push(stmt);
            }
            Rule::interface_decl => {
                let stmt = zeta_parser.parse_interface_decl(&pair);
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
                let stmt = Stmt::If(zeta_parser.parse_if_stmt(&pair));
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
                let stmt = Stmt::Match(zeta_parser.parse_match_stmt(&pair));
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
                let stmt = zeta_parser.parse_struct_decl(&pair);
                stmts.push(stmt);
            }
            Rule::expr_stmt => {
                let stmt = zeta_parser.parse_expr_stmt(&pair);
                stmts.push(stmt);
            }
            Rule::block => {
                let stmt = Stmt::Block(zeta_parser.parse_block(pair));
                stmts.push(stmt);
            }
            Rule::EOI => return true,
            other => panic!("Unexpected rule in program: {:?}", other),
        }
        false
    }

    fn parse_impl_decl(&mut self, pair: &Pair<'a, Rule>) -> Stmt {
        let mut inner = pair.clone().into_inner();

        // Parse generics if present
        let (generics, _interface_pair) = if inner.peek().unwrap().as_rule() == Rule::generic_params
        {
            (self.parse_generic_params(&mut inner), inner.next().unwrap())
        } else {
            (None, inner.next().unwrap())
        };

        let interface = StrId(
            self.context
                .borrow_mut()
                .string_pool
                .intern(inner.next().unwrap().as_str()),
        );
        let target = StrId(
            self.context
                .borrow_mut()
                .string_pool
                .intern(inner.next().unwrap().as_str()),
        );

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

    fn parse_while_stmt(&mut self, pair: &Pair<'a, Rule>) -> Stmt {
        let mut inner = pair.clone().into_inner();
        let value: Option<Box<Expr>> = inner.next().map(|p| Box::new(self.parse_expr(p)));

        match value {
            Some(expr) => Stmt::While(WhileStmt {
                condition: expr,
                block: self.parse_block(inner.next().unwrap()),
            }),
            None => panic!("Expected expression in while loop"),
        }
    }

    fn parse_package_stmt(&mut self, pair: &Pair<Rule>) -> Stmt {
        let mut inner = pair.clone().into_inner();
        let path = StrId(
            self.context
                .borrow_mut()
                .string_pool
                .intern(inner.next().unwrap().as_str()),
        );
        Stmt::Package(PackageStmt { path })
    }

    fn parse_for_stmt(&mut self, pair: &Pair<'a, Rule>) -> Stmt {
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

    fn parse_unsafe_block(&mut self, pair: &Pair<'a, Rule>) -> Stmt {
        let mut inner = pair.clone().into_inner();
        let block = self.parse_block(inner.next().unwrap()); // Parse the matched expression
        Stmt::UnsafeBlock(UnsafeBlock { block })
    }

    fn parse_expr_stmt(&mut self, pair: &Pair<'a, Rule>) -> Stmt {
        let expr = self.parse_expr(pair.clone().into_inner().next().unwrap());
        ExprStmt(InternalExprStmt {
            expr: Box::new(expr),
        })
    }

    fn parse_struct_decl(&mut self, pair: &Pair<'a, Rule>) -> Stmt {
        let mut inner = pair.clone().into_inner();

        // Visibility (public/private/internal)
        let visibility = self.get_visibility(&mut inner);

        // Optional generic parameters: <T: Constraint, U: …>

        // Class keyword already consumed; now the identifier
        let name: StrId = StrId(
            self.context
                .borrow_mut()
                .string_pool
                .intern(inner.next().unwrap().as_str()),
        );

        let generics = self.parse_generic_params(&mut inner);

        // Optional constructor parameters (x: T, …)
        let params = if inner.peek().map(|p| p.as_rule()) == Some(Rule::field_args) {
            let param_list = inner.next().unwrap();
            Some(
                param_list
                    .into_inner()
                    .map(|param| self.parse_param(param))
                    .collect(),
            )
        } else {
            None
        };

        let block = if inner.peek().map(|p| p.as_rule()) == Some(Rule::block) {
            let block = inner.next().unwrap();
            Some(self.parse_block(block))
        } else {
            None
        }
        .unwrap_or_else(|| Block { block: vec![] });

        let body = block
            .block
            .iter()
            .filter_map(|stmt| {
                if let Stmt::FuncDecl(func_decl) = stmt {
                    Some(func_decl.clone())
                } else {
                    None
                }
            })
            .collect();

        let constants = block
            .block
            .iter()
            .filter_map(|stmt| {
                if let Stmt::Const(const_decl) = stmt {
                    Some(const_decl.clone())
                } else {
                    None
                }
            })
            .collect();

        Stmt::ClassDecl(ClassDecl {
            visibility,
            name,
            generics,
            params,
            body,
            constants,
        })
    }

    fn parse_generic_params(&mut self, inner: &mut Pairs<Rule>) -> Option<Vec<Generic>> {
        if inner.peek().map(|p| p.as_rule()) != Some(Rule::generic_params) {
            return None;
        }

        let gen_pair = inner.next().unwrap();
        let params = gen_pair
            .into_inner()
            .map(|pair| self.map_pair_to_generic(pair))
            .collect();

        Some(params)
    }

    fn map_pair_to_generic(&mut self, p: Pair<Rule>) -> Generic {
        let mut parts = p.into_inner();
        let name = StrId(
            self.context
                .borrow_mut()
                .string_pool
                .intern(parts.next().unwrap().as_str()),
        );

        // Collect all constraints after ":"
        let mut constraints = Vec::new();
        for part in parts {
            constraints.push(StrId(
                self.context.borrow_mut().string_pool.intern(part.as_str()),
            ));
        }

        Generic {
            const_generic: false,
            type_name: name,
            constraints,
        }
    }

    fn get_visibility(&mut self, inner: &mut Pairs<Rule>) -> Visibility {
        match inner.peek().map(|p| p.as_rule()) {
            Some(
                Rule::public_keyword
                | Rule::private_keyword
                | Rule::module_keyword
                | Rule::package_keyword,
            ) => Self::assert_visibility_modifier(inner),
            _ => Visibility::Public,
        }
    }

    fn assert_visibility_modifier(inner: &mut Pairs<Rule>) -> Visibility {
        match inner.next().unwrap().as_str() {
            "public" => Visibility::Public,
            "private" => Visibility::Private,
            "module" => Visibility::Module,
            "package" => Visibility::Package,
            fetched => panic!("Expected visibility modifier, got {}", fetched),
        }
    }

    fn parse_fun_decl(&mut self, pair: &Pair<'a, Rule>) -> FuncDecl {
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
                            extern_string = Some(StrId(
                                self.context.borrow_mut().string_pool.intern(rule.as_str()),
                            ));
                            inner.next();
                        }
                        _ => todo!(),
                    }
                }
                _ => break,
            }
        }

        debug_assert!(!inline && !no_inline);

        let return_type = self
            .get_return_type(&mut inner, Rule::var_type)
            .or(Some(Type::Void));
        
        let name = StrId(
            self.context
                .borrow_mut()
                .string_pool
                .intern(inner.next().unwrap().as_str()),
        );

        let generics = self.parse_generic_params(&mut inner);

        // Param list
        let mut params = Vec::new();
        if inner.peek().map(|p| p.as_rule()) == Some(Rule::param_list) {
            for param in inner.next().unwrap().into_inner() {
                if param.as_rule() == Rule::param {
                    params.push(self.parse_param(param));
                }
            }
        }

        // Body or arrow expression
        let body = match inner.next().unwrap() {
            p if p.as_rule() == Rule::block => Some(self.parse_block(p)),
            p if p.as_rule() == Rule::arrow_expr => {
                let expr = self.parse_expr(p.into_inner().next().unwrap());
                Some(Block {
                    block: vec![Stmt::Return(ReturnStmt {
                        value: Some(Box::new(expr)),
                    })],
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
            Some(_) => Some(
                self.parse_to_type(
                    inner
                        .next()?
                        .as_str()
                        .split_whitespace()
                        .last()?,
                ),
            ),
            None => None,
        }
    }

    fn parse_param(&mut self, pair: Pair<'a, Rule>) -> Param {
        // `pair` is Rule::param → contains either typed_param or this_param
        let mut inner = pair.into_inner();
        let first = inner.next().unwrap();

        match first.as_rule() {
            Rule::this_param => self.parse_this_param(first),
            Rule::typed_param => self.parse_typed_param(first),
            _ => panic!("Expected typed_param or this_param, got {:?}", first.as_rule()),
        }
    }

    fn parse_this_param(&mut self, pair: Pair<Rule>) -> Param {
        let mut inner = pair.into_inner();

        let mut is_mut = false;
        let mut is_move = false;

        if let Some(p) = inner.peek() {
            if p.as_rule() == Rule::mut_keyword {
                inner.next();
                is_mut = true;
            }
        }

        if let Some(p) = inner.peek() {
            if p.as_rule() == Rule::own_keyword {
                inner.next();
                is_move = true;
            }
        }

        let next = inner.next().expect("Expected 'this' token");
        assert_eq!(next.as_rule(), Rule::this_type);

        // Optional type annotation, if your grammar allows e.g. `this: Ptr<this>`
        let type_annotation = if let Some(p) = inner.peek() {
            if p.as_rule() == Rule::var_type {
                let tp = inner.next().unwrap();
                Some(self.parse_to_type(tp.as_str()))
            } else {
                None
            }
        } else {
            None
        };

        Param::This(ThisParam {
            is_mut,
            is_move,
            type_annotation,
        })
    }

    fn parse_typed_param(&mut self, pair: Pair<'a, Rule>) -> Param {
        let mut inner = pair.into_inner();

        let visibility = self.get_visibility(&mut inner);

        let mut is_mut = false;
        if let Some(p) = inner.peek() {
            if p.as_rule() == Rule::mut_keyword {
                inner.next();
                is_mut = true;
            }
        }

        let mut is_move = false;
        if let Some(p) = inner.peek() {
            if p.as_rule() == Rule::own_keyword {
                inner.next();
                is_move = true;
            }
        }

        let type_pair = inner.next().expect("Expected parameter type annotation");
        assert_eq!(type_pair.as_rule(), Rule::var_type);
        let type_annotation = self.parse_to_type(type_pair.as_str());

        let name_pair = inner.next().expect("Expected parameter identifier");
        assert_eq!(name_pair.as_rule(), Rule::ident);

        let name = StrId(
            self.context
                .borrow_mut()
                .string_pool
                .intern(name_pair.as_str()),
        );

        let default_value = if let Some(next) = inner.peek() {
            if next.as_rule() == Rule::expr {
                let expr_pair = inner.next().unwrap();
                catch_unwind(AssertUnwindSafe(|| self.parse_expr(expr_pair))).ok()
            } else {
                None
            }
        } else {
            None
        };

        Param::Normal(NormalParam {
            is_mut,
            is_move,
            visibility,
            name,
            type_annotation,
            default_value,
        })
    }

    fn parse_if_stmt(&mut self, pair: &Pair<'a, Rule>) -> IfStmt {
        let mut inner = pair.clone().into_inner();

        let condition = self.parse_expr(inner.next().unwrap());
        let then_branch = self.parse_block(inner.next().unwrap());

        let mut else_branch = None;

        if let Some(next) = inner.next() {
            let branch = match next.as_rule() {
                Rule::if_stmt => If(Box::new(self.parse_if_stmt(&next))),
                Rule::block => {
                    let block = self.parse_block(next);
                    ElseBranch::Else(block)
                }
                _ => panic!("Unexpected rule in else branch"),
            };
            else_branch = Some(Box::new(branch));
        }

        IfStmt {
            condition,
            then_branch,
            else_branch,
        }
    }

    fn parse_match_stmt(&mut self, pair: &Pair<'a, Rule>) -> MatchStmt {
        let mut inner = pair.clone().into_inner();
        let expr = self.parse_expr(inner.next().unwrap());
        let mut arms = Vec::new();

        for arm in inner {
            arms.push(self.parse_match_arm(arm));
        }

        MatchStmt { expr, arms }
    }

    fn parse_return_stmt(&mut self, pair: &Pair<'a, Rule>) -> Stmt {
        let mut inner = pair.clone().into_inner();
        let value = inner.next().map(|p| Box::new(self.parse_expr(p)));
        Stmt::Return(ReturnStmt { value })
    }

    fn parse_let_stmt(&mut self, pair: &Pair<'a, Rule>) -> Stmt {
        let mut inner = pair.clone().into_inner();

        let mutability = match inner.peek().map(|p| p.as_rule()) {
            Some(Rule::mut_keyword) => {
                inner.next();
                true
            }
            _ => false,
        };

        let type_annotation = self.parse_to_type(inner.next().unwrap().as_str());

        let ident = StrId(
            self.context
                .borrow_mut()
                .string_pool
                .intern(inner.next().unwrap().as_str()),
        );

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
            let close_angle = token_type
                .rfind('>')
                .expect("Unclosed angle bracket in type");
            let type_name = &token_type[..open_angle];
            let inner_types = &token_type[open_angle + 1..close_angle];

            let generic_args: Vec<Type> = inner_types
                .split(',')
                .map(|s| self.parse_to_type(s.trim()))
                .collect();

            return Type::Class {
                name: StrId(self.context.borrow_mut().string_pool.intern(type_name)),
                generics: generic_args,
            };
        }

        // Handle lambda types (e.g. "lambda(i32, str) -> boolean")
        if token_type.starts_with("(") {
            // Split into params and return type: "(i32, str) -> boolean"
            let parts: Vec<&str> = token_type.split("->").map(str::trim).collect();

            // Extract parameter list
            let params_part = parts[0]; // "(i32, str)"
            let params_str = params_part
                .trim()
                .trim_start_matches('(')
                .trim_end_matches(')');
            let params = if params_str.is_empty() {
                vec![]
            } else {
                params_str
                    .split(',')
                    .map(|p| self.parse_to_type(p.trim()))
                    .collect()
            };

            // Extract return type (default to void if missing)
            let return_type = if parts.len() > 1 {
                self.parse_to_type(parts[1])
            } else {
                Type::Void
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
            name => Type::Class {
                name: StrId(self.context.borrow_mut().string_pool.intern(name)),
                generics: Vec::new(),
            },
        }
    }

    fn parse_enum_decl(&mut self, pair: &Pair<Rule>) -> Stmt {
        let mut inner = pair.clone().into_inner();

        let visibility = self.get_visibility(&mut inner);
        let name = StrId(
            self.context
                .borrow_mut()
                .string_pool
                .intern(inner.next().unwrap().as_str()),
        );

        let generics = self.parse_generic_params(&mut inner);

        let mut variants = Vec::new();
        for variant in inner {
            if variant.as_rule() == Rule::enum_variant {
                let mut variant_inner = variant.into_inner();
                let variant_name = StrId(
                    self.context
                        .borrow_mut()
                        .string_pool
                        .intern(variant_inner.next().unwrap().as_str()),
                );

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

                variants.push(EnumVariant {
                    name: variant_name,
                    fields,
                });
            }
        }

        Stmt::EnumDecl(EnumDecl {
            name,
            visibility,
            generics,
            variants,
        })
    }

    fn parse_interface_decl(&mut self, pair: &Pair<'a, Rule>) -> Stmt {
        let mut inner = pair.clone().into_inner();

        let visibility = self.get_visibility(&mut inner);
        let name = StrId(
            self.context
                .borrow_mut()
                .string_pool
                .intern(inner.next().unwrap().as_str()),
        );
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
            methods: if methods.is_empty() {
                None
            } else {
                Some(methods)
            },
            generics,
        })
    }

    fn parse_import_stmt(&mut self, pair: &Pair<Rule>) -> Stmt {
        let mut inner = pair.clone().into_inner();
        let path = StrId(
            self.context
                .borrow_mut()
                .string_pool
                .intern(inner.next().unwrap().as_str()),
        );
        Stmt::Import(ImportStmt { path })
    }

    fn parse_match_arm(&mut self, pair: Pair<'a, Rule>) -> MatchArm {
        let mut inner = pair.into_inner();
        let pattern = self.parse_pattern(inner.next().unwrap()); // Parse the pattern
        let guard = match inner.peek() {
            Some(pair) => {
                inner.next().unwrap();
                Some(self.parse_expr(pair))
            }
            _ => None,
        };

        let block = self.parse_block(inner.next().unwrap()); // Parse the block for the match arm
        MatchArm {
            pattern,
            guard,
            block,
        }
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
            Rule::ident => Pattern::Ident(StrId(
                self.context.borrow_mut().string_pool.intern(pair.as_str()),
            )),
            Rule::number => Pattern::Number(pair.as_str().parse().unwrap()),
            Rule::string => Pattern::String(self.parse_string(pair.as_str())),
            Rule::tuple_pattern => {
                let inner = pair.into_inner();
                let mut patterns = Vec::new();
                for p in inner {
                    patterns.push(self.parse_pattern(p));
                }
                Pattern::Tuple(patterns)
            }
            Rule::wildcard_pattern => Pattern::Wildcard,
            _ => panic!("Unexpected pattern: {:?}", pair.as_rule()),
        }
    }

    pub fn parse_block(&mut self, pair: Pair<'a, Rule>) -> Block {
        let mut stmts = Vec::new();
        for stmt in pair.into_inner() {
            Self::parse_stmt(self, &mut stmts, stmt);
        }
        Block { block: stmts }
    }

    fn parse_expr(&mut self, pair: Pair<'a, Rule>) -> Expr {
        match pair.as_rule() {
            Rule::boolean => {
                let val = match pair.as_str() {
                    "true" => true,
                    "false" => false,
                    _ => unreachable!(),
                };
                Expr::Boolean { value: val, span: self.span_from_pair(&pair) }
            }
            Rule::number => Expr::Number { value: pair.as_str().parse().unwrap(), span: self.span_from_pair(&pair) },
            Rule::string => Expr::String { value: self.parse_string(pair.as_str()), span: self.span_from_pair(&pair) },
            Rule::ident => Expr::Ident { name: StrId(
                self.context.borrow_mut().string_pool.intern(pair.as_str()),
            ), span: self.span_from_pair(&pair) },

            Rule::assignment => {
                let mut inner = pair.clone().into_inner();
                let first = inner.next().unwrap();

                let lhs = self.parse_expr(first);
                if let Some(op_pair) = inner.next() {
                    let op = op_pair.as_str().into();
                    let rhs = self.parse_expr(inner.next().unwrap());
                    Expr::Assignment {
                        lhs: Box::new(lhs),
                        op,
                        rhs: Box::new(rhs),
                        span: self.span_from_pair(&pair),
                    }
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
                        span: self.span_from_pair(&op_pair),
                    };
                }

                lhs
            }

            Rule::parenthesized_expr => {
                let inner = pair.into_inner().next().unwrap();
                self.parse_expr(inner)
            }

            Rule::class_initialization => { self.parse_class_initialization(pair) }

            Rule::primary => {
                let mut inner = pair.clone().into_inner();
                let mut expr = self.parse_expr(inner.next().unwrap());

                for next in inner {
                    match next.as_rule() {
                        Rule::call_args => {
                            let args = next
                                .clone()
                                .into_inner()
                                .map(|rule| self.parse_expr(rule))
                                .collect::<Vec<Expr>>();

                            expr = Expr::Call {
                                callee: Box::new(expr),
                                arguments: args,
                                span: self.span_from_pair(&next),
                            };
                        }
                        Rule::ident => {
                            // This is the .field part
                            expr = Expr::Get {
                                object: Box::new(expr),
                                field: StrId(
                                    self.context.borrow_mut().string_pool.intern(next.as_str()),
                                ),
                                span: self.span_from_pair(&next),
                            };
                        }
                        Rule::if_stmt => {
                            let if_stmt = self.parse_if_stmt(&next);
                            expr = Expr::If { if_stmt: Box::new(if_stmt), span: self.span_from_pair(&next) };
                        }
                        Rule::while_stmt => {
                            let match_stmt = self.parse_match_stmt(&next);
                            expr = Expr::Match { match_stmt: Box::new(match_stmt), span: self.span_from_pair(&next) };
                        }
                        _ => unreachable!("Unexpected rule in primary tail: {:?}", next.as_rule()),
                    }
                }

                expr
            }

            _ => panic!("Unexpected expression rule: {:?}", pair.as_rule()),
        }
    }

    fn parse_class_initialization(&mut self, pair: Pair<'a, Rule>) -> Expr {
        let mut inner = pair.clone().into_inner();

        let class_name = self.parse_expr(inner.next().unwrap());

        let Some(arg_group_raw) = inner.next() else {
            return Expr::ClassInit {
                callee: Box::new(class_name),
                arguments: vec![],
                positional: true,
                span: self.span_from_pair(&pair),
            };
        };

        let arg_group = self.unwrap_expr_layers(arg_group_raw);

        let mut positional = false;

        // Temporary fix until I have a handwritten parser
        let arguments = match arg_group.as_rule() {
            Rule::ctor_args_init => self.parse_ctor_args_init(&mut positional, arg_group),
            Rule::field_args     => self.parse_field_args_init(arg_group),
            Rule::assignment | Rule::expr | Rule::number | Rule::string | Rule::ident => {
                positional = true;
                vec![self.parse_expr(arg_group)]
            }
            other => {
                eprintln!("[warn] unexpected {:?} in class init, coercing to expr", other);
                positional = true;
                vec![self.parse_expr(arg_group)]
            }
        };

        Expr::ClassInit {
            callee: Box::new(class_name),
            arguments,
            positional,
            span: self.span_from_pair(&pair),
        }
    }

    fn unwrap_expr_layers(&self, mut pair: Pair<'a, Rule>) -> Pair<'a, Rule> {
        loop {
            match pair.as_rule() {
                // stop here — we reached what we want
                Rule::ctor_args_init | Rule::field_args => break,

                // unwrap generic expression layers
                Rule::expr
                | Rule::assignment
                | Rule::logic_or
                | Rule::logic_and
                | Rule::bit_or
                | Rule::bit_xor
                | Rule::bit_and
                | Rule::equality
                | Rule::comparison
                | Rule::shift
                | Rule::term
                | Rule::factor
                | Rule::unary
                | Rule::primary
                | Rule::lhs
                | Rule::parenthesized_expr => {
                    if let Some(inner) = pair.clone().into_inner().next() {
                        pair = inner;
                    } else {
                        break;
                    }
                }

                // stop unwrapping when it's anything else (like number)
                _ => break,
            }
        }

        pair
    }


    fn parse_ctor_args_init(&mut self, positional: &mut bool, arg_group: Pair<'a, Rule>) -> Vec<Expr> {
        let args: Vec<Expr> = arg_group
            .into_inner()
            .map(|p| self.parse_expr(p))
            .collect();
        println!("clinit args: {:?}", args);
        *positional = true;
        args
    }

    fn parse_field_args_init(&mut self, arg_group: Pair<'a, Rule>) -> Vec<Expr> {
        let args: Vec<Expr> = arg_group
            .into_inner()
            .map(|p| {
                let span = self.span_from_pair(&p);
                let mut inner = p.into_inner();
                let ident = StrId(self.context.borrow_mut()
                    .string_pool
                    .intern(inner.next().unwrap().as_str()));
                let expr = Box::new(self.parse_expr(inner.next().unwrap()));
                Expr::FieldInit { ident, expr, span }
            })
            .collect();
        args
    }

    fn parse_binary_expr(&mut self, mut inner: Pairs<'a, Rule>, op_from_str: fn(&str) -> Op) -> Expr {
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
                        span: self.span_from_pair(&next),
                    };
                }

                // This prevents numbers or identifiers from being interpreted as operators
                other => panic!("Expected binary operator, got {:?}", other),
            }
        }

        expr
    }
}
