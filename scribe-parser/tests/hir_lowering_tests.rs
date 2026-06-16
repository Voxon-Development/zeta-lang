#![cfg(test)]
#![feature(allocator_api)]

mod parser;

use ir::pretty::IrPrettyPrinter;
use scribe_parser::parser::parse_program;
use std::sync::Arc;
use zetaruntime::arena::GrowableAtomicBump;
use zetaruntime::string_pool::StringPool;

fn make_context() -> (Arc<StringPool>, Arc<GrowableAtomicBump<'static>>) {
    let context = Arc::new(StringPool::new().unwrap());
    let bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(4096, 8).unwrap());
    (context, bump)
}

fn parse_and_lower(code: &str) -> (String, Arc<StringPool>) {
    let (context, atomic_bump) = make_context();
    let result = parse_program(code, "test.zeta", context.clone(), atomic_bump.clone());

    assert!(
        result.diagnostics.errors.is_empty(),
        "Parse errors:\n{:#?}",
        result.diagnostics.errors
    );

    let mut hir_lowerer =
        scribe_parser::hir_lowerer::HirLowerer::new(context.clone(), atomic_bump.clone());
    let module = hir_lowerer.lower_module(result.statements);

    let mut printer = IrPrettyPrinter::new(context.clone());
    let printed = printer
        .format_hir_module(&module)
        .expect("pretty print failed");

    (printed, context)
}

#[cfg(test)]
mod hir_tests {
    use super::*;
    use ir::ast::{Expr, Op, Param, ParamPassingKind, Stmt, Type};
    use ir::hir::{Hir, HirExpr, HirStmt, HirType};
    use scribe_parser::hir_lowerer::HirLowerer;

    #[test]
    fn test_basic_arithmetic_ast() {
        let code = r#"
        fn add(a: i32, b: i32) i32 {
            return a + b;
        }
        fn main() void {
            let x: i32 = 5 + 3 * 2;
            let y: i32 = (10 - 4) / 2;
            let z: i32 = -x + y;
        }
        "#;

        let (context, atomic_bump) = make_context();
        let result = parse_program(code, "test.zeta", context.clone(), atomic_bump.clone());

        assert!(
            result.diagnostics.errors.is_empty(),
            "Expected no parse errors, got:\n{:#?}",
            result.diagnostics.errors
        );

        let stmts = &result.statements;
        assert_eq!(stmts.len(), 2, "Expected 2 top-level statements");

        let add = match &stmts[0] {
            Stmt::FuncDecl(f) => f,
            other => panic!("Expected FuncDecl for `add`, got {other:?}"),
        };

        assert_eq!(context.resolve_string(&add.name), "add");
        assert_eq!(add.return_type, Some(Type::i32()));

        let add_params = add.params.expect("Expected add to have params");
        assert_eq!(add_params.len(), 2, "Expected 2 params on add");

        match &add_params[0] {
            Param::Normal(p) => {
                assert_eq!(context.resolve_string(&p.name), "a");
                assert_eq!(p.type_annotation, Type::i32());
            }
            other => panic!("Expected normal param `a`, got {other:?}"),
        }
        match &add_params[1] {
            Param::Normal(p) => {
                assert_eq!(context.resolve_string(&p.name), "b");
                assert_eq!(p.type_annotation, Type::i32());
            }
            other => panic!("Expected normal param `b`, got {other:?}"),
        }

        let add_body = add.body.expect("Expected add to have a body");
        assert_eq!(add_body.block.len(), 1);

        match &add_body.block[0] {
            Stmt::Return(ret) => match ret.value.expect("Expected return value") {
                Expr::Binary { op: Op::Add, .. } => {}
                other => panic!("Expected `a + b`, got {other:?}"),
            },
            other => panic!("Expected return stmt, got {other:?}"),
        }

        let main = match &stmts[1] {
            Stmt::FuncDecl(f) => f,
            other => panic!("Expected FuncDecl for `main`, got {other:?}"),
        };

        assert_eq!(context.resolve_string(&main.name), "main");
        assert_eq!(main.return_type, Some(Type::void()));
        assert_eq!(main.params.unwrap_or(&[]).len(), 0);

        let body = main.body.expect("Expected main body");
        assert_eq!(body.block.len(), 3, "Expected 3 let stmts");

        // x = 5 + 3 * 2  →  Binary(Add, 5, Binary(Mul, 3, 2))
        match &body.block[0] {
            Stmt::Let(l) => {
                assert_eq!(context.resolve_string(&l.ident), "x");
                assert_eq!(l.type_annotation, Type::i32());
                assert!(!l.mutable);
                match l.value {
                    Expr::Binary { op: Op::Add, .. } => {}
                    other => panic!("Expected Add for x, got {other:?}"),
                }
            }
            other => panic!("Expected let x, got {other:?}"),
        }

        // y = (10 - 4) / 2  →  Binary(Div, ...)
        match &body.block[1] {
            Stmt::Let(l) => {
                assert_eq!(context.resolve_string(&l.ident), "y");
                match l.value {
                    Expr::Binary { op: Op::Div, .. } => {}
                    other => panic!("Expected Div for y, got {other:?}"),
                }
            }
            other => panic!("Expected let y, got {other:?}"),
        }

        // z = -x + y  →  Binary(Add, Unary(Sub, x), y)
        match &body.block[2] {
            Stmt::Let(l) => {
                assert_eq!(context.resolve_string(&l.ident), "z");
                match l.value {
                    Expr::Binary {
                        op: Op::Add, left, ..
                    } => match left {
                        Expr::Unary { op: Op::Sub, .. } => {}
                        other => panic!("Expected Unary(Sub) on lhs of z, got {other:?}"),
                    },
                    other => panic!("Expected Add for z, got {other:?}"),
                }
            }
            other => panic!("Expected let z, got {other:?}"),
        }
    }

    #[test]
    fn test_struct_fields_ast() {
        let code = r#"
        struct Point {
            x: f64,
            y: f64,
        }
        "#;

        let (context, atomic_bump) = make_context();
        let result = parse_program(code, "test.zeta", context.clone(), atomic_bump.clone());

        assert!(
            result.diagnostics.errors.is_empty(),
            "Parse errors:\n{:#?}",
            result.diagnostics.errors
        );

        assert_eq!(result.statements.len(), 1);

        match &result.statements[0] {
            Stmt::StructDecl(s) => {
                assert_eq!(context.resolve_string(&s.name), "Point");
                let params = s.params.expect("Expected Point to have fields");
                assert_eq!(params.len(), 2);
                match &params[0] {
                    Param::Normal(p) => {
                        assert_eq!(context.resolve_string(&p.name), "x");
                        assert_eq!(p.type_annotation, Type::f64());
                    }
                    other => panic!("Expected field x, got {other:?}"),
                }
                match &params[1] {
                    Param::Normal(p) => {
                        assert_eq!(context.resolve_string(&p.name), "y");
                        assert_eq!(p.type_annotation, Type::f64());
                    }
                    other => panic!("Expected field y, got {other:?}"),
                }
            }
            other => panic!("Expected StructDecl, got {other:?}"),
        }
    }

    #[test]
    fn test_if_else_ast() {
        let code = r#"
        fn sign(n: i32) i32 {
            if (n > 0) {
                return 1;
            } else if (n < 0) {
                return -1;
            } else {
                return 0;
            }
        }
        "#;

        let (context, atomic_bump) = make_context();
        let result = parse_program(code, "test.zeta", context.clone(), atomic_bump.clone());

        assert!(
            result.diagnostics.errors.is_empty(),
            "Parse errors:\n{:#?}",
            result.diagnostics.errors
        );

        let func = match &result.statements[0] {
            Stmt::FuncDecl(f) => f,
            other => panic!("Expected FuncDecl, got {other:?}"),
        };

        assert_eq!(context.resolve_string(&func.name), "sign");
        let body = func.body.expect("Expected body");
        assert_eq!(body.block.len(), 1, "Expected 1 if statement");

        match &body.block[0] {
            Stmt::If(if_stmt) => {
                // condition: n > 0
                match if_stmt.condition {
                    Expr::Comparison { op: Op::Gt, .. } => {}
                    other => panic!("Expected Gt comparison, got {other:?}"),
                }
                // has else branch
                assert!(if_stmt.else_branch.is_some(), "Expected else branch");
            }
            other => panic!("Expected if stmt, got {other:?}"),
        }
    }

    #[test]
    fn test_while_loop_ast() {
        let code = r#"
        fn countdown(mut n: i32) void {
            while (n > 0) {
                n -= 1;
            }
        }
        "#;

        let (context, atomic_bump) = make_context();
        let result = parse_program(code, "test.zeta", context.clone(), atomic_bump.clone());

        assert!(
            result.diagnostics.errors.is_empty(),
            "Parse errors:\n{:#?}",
            result.diagnostics.errors
        );

        let func = match &result.statements[0] {
            Stmt::FuncDecl(f) => f,
            other => panic!("Expected FuncDecl, got {other:?}"),
        };

        let body = func.body.expect("Expected body");
        match &body.block[0] {
            Stmt::While(w) => {
                match w.condition {
                    Expr::Comparison { op: Op::Gt, .. } => {}
                    other => panic!("Expected Gt condition, got {other:?}"),
                }
                assert_eq!(w.block.block.len(), 1);
            }
            other => panic!("Expected while stmt, got {other:?}"),
        }
    }

    #[test]
    fn test_for_loop_ast() {
        let code = r#"
        fn sum_to(n: i32) i32 {
            let mut total: i32 = 0;
            for (let mut i: i32 = 0; i <= n; i += 1) {
                total += i;
            }
            return total;
        }
        "#;

        let (context, atomic_bump) = make_context();
        let result = parse_program(code, "test.zeta", context.clone(), atomic_bump.clone());

        assert!(
            result.diagnostics.errors.is_empty(),
            "Parse errors:\n{:#?}",
            result.diagnostics.errors
        );

        let func = match &result.statements[0] {
            Stmt::FuncDecl(f) => f,
            other => panic!("Expected FuncDecl, got {other:?}"),
        };

        let body = func.body.expect("Expected body");
        // block[0] = let total, block[1] = for, block[2] = return
        assert_eq!(body.block.len(), 3);

        match &body.block[1] {
            Stmt::For(f) => match &f.kind {
                ir::ast::ForKind::CStyle {
                    let_stmt,
                    condition,
                    increment,
                } => {
                    assert!(let_stmt.is_some(), "Expected for-init");
                    assert!(condition.is_some(), "Expected for-condition");
                    assert!(increment.is_some(), "Expected for-increment");
                }
                other => panic!("Expected CStyle for, got {other:?}"),
            },
            other => panic!("Expected for stmt, got {other:?}"),
        }
    }

    #[test]
    fn test_match_stmt_ast() {
        let code = r#"
        fn describe(n: i32) void {
            match n {
                case 0 -> { return; }
                case 1 -> { return; }
                case _ -> { return; }
            }
        }
        "#;

        let (context, atomic_bump) = make_context();
        let result = parse_program(code, "test.zeta", context.clone(), atomic_bump.clone());

        assert!(
            result.diagnostics.errors.is_empty(),
            "Parse errors:\n{:#?}",
            result.diagnostics.errors
        );

        let func = match &result.statements[0] {
            Stmt::FuncDecl(f) => f,
            other => panic!("Expected FuncDecl, got {other:?}"),
        };

        let body = func.body.expect("Expected body");
        match &body.block[0] {
            Stmt::Match(m) => {
                assert_eq!(m.arms.len(), 3, "Expected 3 match arms");
                // third arm should be wildcard
                match m.arms[2].pattern {
                    ir::ast::Pattern::Wildcard => {}
                    other => panic!("Expected wildcard arm, got {other:?}"),
                }
            }
            other => panic!("Expected match stmt, got {other:?}"),
        }
    }

    #[test]
    fn test_impl_methods_ast() {
        let code = r#"
        struct Vec3 { x: f64, y: f64, z: f64 }

        impl Vec3 {
            fn dot(&this, other: Vec3) f64 {
                return this.x * other.x + this.y * other.y + this.z * other.z;
            }
            fn scale(&mut this, factor: f64) void {
                this.x *= factor;
                this.y *= factor;
                this.z *= factor;
            }
        }
        "#;

        let (context, atomic_bump) = make_context();
        let result = parse_program(code, "test.zeta", context.clone(), atomic_bump.clone());

        assert!(
            result.diagnostics.errors.is_empty(),
            "Parse errors:\n{:#?}",
            result.diagnostics.errors
        );

        assert_eq!(result.statements.len(), 2);

        let impl_decl = match &result.statements[1] {
            Stmt::ImplDecl(i) => i,
            other => panic!("Expected ImplDecl, got {other:?}"),
        };

        assert_eq!(context.resolve_string(&impl_decl.target), "Vec3");
        let methods = impl_decl.methods.expect("Expected methods");
        assert_eq!(methods.len(), 2);

        assert_eq!(context.resolve_string(&methods[0].name), "dot");
        assert_eq!(context.resolve_string(&methods[1].name), "scale");

        // dot takes &this + 1 normal param
        let dot_params = methods[0].params.expect("Expected dot params");
        assert_eq!(dot_params.len(), 2);
        match &dot_params[0] {
            Param::This(t) => {
                assert_ne!(t.passing_kind, ParamPassingKind::RefMut);
                assert_ne!(t.passing_kind, ParamPassingKind::Move);
            }
            other => panic!("Expected This param for dot, got {other:?}"),
        }

        // scale takes &mut this + 1 normal param
        let scale_params = methods[1].params.expect("Expected scale params");
        println!("{scale_params:?}");
        assert_eq!(scale_params.len(), 2);
        match &scale_params[0] {
            Param::This(t) => {
                assert_eq!(
                    t.passing_kind,
                    ParamPassingKind::RefMut,
                    "Expected mutable this for scale"
                );
            }
            other => panic!("Expected This param for scale, got {other:?}"),
        }
    }

    #[test]
    fn test_defer_ast() {
        let code = r#"
        fn with_cleanup() void {
            defer { return; }
            return;
        }
        "#;

        let (context, atomic_bump) = make_context();
        let result = parse_program(code, "test.zeta", context.clone(), atomic_bump.clone());

        assert!(
            result.diagnostics.errors.is_empty(),
            "Parse errors:\n{:#?}",
            result.diagnostics.errors
        );

        let func = match &result.statements[0] {
            Stmt::FuncDecl(f) => f,
            other => panic!("Expected FuncDecl, got {other:?}"),
        };

        let body = func.body.expect("Expected body");
        match &body.block[0] {
            Stmt::Defer(_) => {}
            other => panic!("Expected defer stmt, got {other:?}"),
        }
    }

    #[test]
    fn test_return_void_and_value_ast() {
        let code = r#"
        fn nothing() void {
            return;
        }
        fn one() i32 {
            return 1;
        }
        "#;

        let (context, atomic_bump) = make_context();
        let result = parse_program(code, "test.zeta", context.clone(), atomic_bump.clone());

        assert!(
            result.diagnostics.errors.is_empty(),
            "Parse errors:\n{:#?}",
            result.diagnostics.errors
        );

        let nothing = match &result.statements[0] {
            Stmt::FuncDecl(f) => f,
            other => panic!("{other:?}"),
        };
        match &nothing.body.unwrap().block[0] {
            Stmt::Return(r) => assert!(r.value.is_none(), "Expected void return"),
            other => panic!("{other:?}"),
        }

        let one = match &result.statements[1] {
            Stmt::FuncDecl(f) => f,
            other => panic!("{other:?}"),
        };
        match &one.body.unwrap().block[0] {
            Stmt::Return(r) => {
                let val = r.value.expect("Expected return value");
                match val {
                    Expr::Number { value: 1, .. } => {}
                    other => panic!("Expected Number(1), got {other:?}"),
                }
            }
            other => panic!("{other:?}"),
        }
    }

    #[test]
    fn test_basic_arithmetic_hir() {
        let code = r#"
        fn add(a: i32, b: i32) i32 {
            return a + b;
        }
        fn main() void {
            let x: i32 = 5 + 3 * 2;
            let y: i32 = (10 - 4) / 2;
            let z: i32 = -x + y;
        }
        "#;

        let (context, atomic_bump) = make_context();
        let result = parse_program(code, "test.zeta", context.clone(), atomic_bump.clone());

        assert!(
            result.diagnostics.errors.is_empty(),
            "Parse errors:\n{:#?}",
            result.diagnostics.errors
        );

        let mut hir_lowerer = HirLowerer::new(context.clone(), atomic_bump.clone());
        let module = hir_lowerer.lower_module(result.statements);

        let functions: Vec<_> = module
            .items
            .iter()
            .filter_map(|item| {
                if let Hir::Func(f) = item {
                    Some(*f)
                } else {
                    None
                }
            })
            .collect();

        assert_eq!(functions.len(), 2, "Expected add and main");

        let add = functions
            .iter()
            .find(|f| context.resolve_string(&f.name) == "add")
            .expect("Expected add");

        let add_params = add.params.expect("Expected add params");
        assert_eq!(add_params.len(), 2);

        let main = functions
            .iter()
            .find(|f| context.resolve_string(&f.name) == "main")
            .expect("Expected main");

        if let Some(HirStmt::Block { body }) = &main.body {
            assert_eq!(body.len(), 3, "Expected 3 let stmts in main");

            if let HirStmt::Let {
                name, ty, value, ..
            } = &body[0]
            {
                assert_eq!(context.resolve_string(name), "x");
                assert_eq!(*ty, HirType::I32);
                assert!(
                    matches!(value, HirExpr::Binary { .. }),
                    "Expected binary for x"
                );
            } else {
                panic!("Expected let x");
            }

            if let HirStmt::Let { name, ty, .. } = &body[1] {
                assert_eq!(context.resolve_string(name), "y");
                assert_eq!(*ty, HirType::I32);
            } else {
                panic!("Expected let y");
            }

            if let HirStmt::Let { name, ty, .. } = &body[2] {
                assert_eq!(context.resolve_string(name), "z");
                assert_eq!(*ty, HirType::I32);
            } else {
                panic!("Expected let z");
            }
        } else {
            panic!("Expected main body to be a block");
        }
    }

    #[test]
    fn test_struct_hir() {
        let code = r#"
        struct Point { x: f64, y: f64 }

        impl Point {
            fn new(x: f64, y: f64) Point {
                return Point { x, y };
            }
            fn distance(&this, other: Point) f64 {
                let dx: f64 = this.x - other.x;
                let dy: f64 = this.y - other.y;
                return dx * dx + dy * dy;
            }
        }

        fn main() void {
            let p1: Point = Point { x: 0.0, y: 0.0 };
            let p2: Point = Point { x: 3.0, y: 4.0 };
        }
        "#;

        let (printed, _) = parse_and_lower(code);
        println!("{printed}");
        assert!(!printed.is_empty());
    }

    #[test]
    fn test_control_flow_hir() {
        let code = r#"
        fn max(a: i32, b: i32) i32 {
            if (a > b) {
                return a;
            } else {
                return b;
            }
        }

        fn factorial(n: i32) i32 {
            let mut result: i32 = 1;
            let mut i: i32 = 1;
            while (i <= n) {
                result *= i;
                i += 1;
            }
            return result;
        }
        "#;

        let (printed, _) = parse_and_lower(code);
        println!("{printed}");
        assert!(!printed.is_empty());
    }

    #[test]
    fn test_enum_decl_hir() {
        let code = r#"
        enum Direction {
            North,
            South,
            East,
            West,
        }

        fn main() void {
            let d: Direction = Direction { };
        }
        "#;

        let (context, atomic_bump) = make_context();
        let result = parse_program(code, "test.zeta", context.clone(), atomic_bump.clone());

        assert!(
            result.diagnostics.errors.is_empty(),
            "Parse errors:\n{:#?}",
            result.diagnostics.errors
        );

        match &result.statements[0] {
            Stmt::EnumDecl(e) => {
                assert_eq!(context.resolve_string(&e.name), "Direction");
                assert_eq!(e.variants.len(), 4);
                assert_eq!(context.resolve_string(&e.variants[0].name), "North");
                assert_eq!(context.resolve_string(&e.variants[3].name), "West");
            }
            other => panic!("Expected EnumDecl, got {other:?}"),
        }
    }

    #[test]
    fn test_generic_function_hir() {
        let code = r#"
        fn identity<T>(x: T) T {
            return x;
        }
        fn main() void {
            let x: i32 = identity(42);
        }
        "#;

        let (printed, _) = parse_and_lower(code);
        println!("{printed}");
        assert!(!printed.is_empty());
    }

    #[test]
    fn test_import_package_ast() {
        let code = r#"
        package com.example.app;
        import std.io;
        import std.collections;
        "#;

        let (context, atomic_bump) = make_context();
        let result = parse_program(code, "test.zeta", context.clone(), atomic_bump.clone());

        assert!(
            result.diagnostics.errors.is_empty(),
            "Parse errors:\n{:#?}",
            result.diagnostics.errors
        );

        assert_eq!(result.statements.len(), 3);

        match &result.statements[0] {
            Stmt::Package(p) => {
                assert_eq!(p.path.path.len(), 3); // com, example, app
            }
            other => panic!("Expected Package, got {other:?}"),
        }

        match &result.statements[1] {
            Stmt::Import(i) => {
                assert_eq!(i.path.path.len(), 2); // std, io
            }
            other => panic!("Expected Import, got {other:?}"),
        }
    }
}
