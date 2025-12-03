#![cfg(test)]

use scribe_parser::parser::parse_program;
use zetaruntime::arena::GrowableAtomicBump;
use zetaruntime::string_pool::StringPool;
use std::sync::Arc;
use ctrc_graph::hir_integration::convenience::analyze_and_pretty_print;
use zetaruntime::bump::GrowableBump;

// Helper function to parse and lower code to HIR
fn parse_and_lower<'bump>(code: &str) -> (String, GrowableBump<'bump>) {
    let context = Arc::new(StringPool::new().unwrap());
    let atomic_bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(1024, 8).unwrap());

    let result = parse_program(code, "test.zeta", context.clone(), atomic_bump.clone());
    // Lower AST to HIR
    let mut hir_lowerer = scribe_parser::hir_lowerer::HirLowerer::new(context.clone(), atomic_bump.clone());
    let new_module = hir_lowerer.lower_module(result.statements);
    println!("{:#?}", new_module);
    let bump = GrowableBump::new(1024, 8);

    (analyze_and_pretty_print(new_module, &bump, context.clone()).expect("REASON"), bump)
}

#[cfg(test)]
mod hir_tests {
    use super::*;

    #[test]
    fn test_basic_arithmetic() {
        use scribe_parser::hir_lowerer::HirLowerer;
        use ir::hir::{Hir, HirStmt, HirExpr, HirType};
        
        let code = r#"
        fn add(a: i32, b: i32): i32 {
            a + b
        }

        fn main() {
            let x: i32 = 5 + 3 * 2;
            let y: i32 = (10 - 4) / 2;
            let z: i32 = -x + y;
        }
        "#;
        
        let context = std::sync::Arc::new(zetaruntime::string_pool::StringPool::new().unwrap());
        let atomic_bump = std::sync::Arc::new(zetaruntime::arena::GrowableAtomicBump::with_capacity_and_aligned(1024, 8).unwrap());

        let result = parse_program(code, "test.zeta", context.clone(), atomic_bump.clone());
        let mut hir_lowerer = HirLowerer::new(context.clone(), atomic_bump.clone());
        let module = hir_lowerer.lower_module(result.statements);
        
        // Find add and main functions
        let functions: Vec<_> = module.items.iter().filter_map(|item| {
            if let Hir::Func(func) = item {
                Some(*func)
            } else {
                None
            }
        }).collect();
        
        assert!(functions.len() >= 2, "Expected at least add and main functions");
        
        // Find add function
        let add_func = functions.iter().find(|f| {
            context.resolve_string(&f.name) == "add"
        }).expect("Expected add function");
        
        assert_eq!(add_func.params.unwrap().len(), 2, "Expected add to have 2 parameters");
        
        // Find main function
        let main_func = functions.iter().find(|f| {
            context.resolve_string(&f.name) == "main"
        }).expect("Expected main function");
        
        // Verify main has 3 let statements
        if let Some(HirStmt::Block { body }) = &main_func.body {
            assert_eq!(body.len(), 3, "Expected 3 let statements in main");
            
            // Check first let: let x: i32 = 5 + 3 * 2;
            if let HirStmt::Let { name, ty, value } = &body[0] {
                assert_eq!(context.resolve_string(name), "x");
                assert_eq!(*ty, HirType::I32);
                if let HirExpr::Binary { .. } = value {
                    // Expected
                } else {
                    panic!("Expected binary operation for x");
                }
            } else {
                panic!("Expected let statement");
            }
        } else {
            panic!("Expected main body to be a block");
        }
    }
    }

    #[test]
    fn test_control_flow() {
        let code = r#"
        fn max(a: i32, b: i32) -> i32 {
            if a > b {
                a
            } else {
                b
            }
        }
        
        fn factorial(n: i32) -> i32 {
            let result: i32 = 9;
            result
        }
        "#;
        
        let result = parse_and_lower(code);
        println!("{}", result.0);
    }

    #[test]
    fn test_struct_usage() {
        let code = r#"
        record Point { x: f64, y: f64 } {
            fn new(x: f64, y: f64): Self {
                Point { x, y }
            }

            fn distance(this, other: Point): f64 {
                let dx: f64 = self.x - other.x;
                let dy: f64 = self.y - other.y;
                sqrt(dx * dx + dy * dy)
            }
        }
        
        fn main() {
            let p1 = Point.new(0.0, 0.0);
            let p2 = Point { x: 3.0, y: 4.0 };
            let d = p1.distance(p2);
        }
        "#;
        
        let result = parse_and_lower(code);
        println!("{}", result.0);
    }

    #[test]
    fn test_generic_functions() {
        let code = r#"
        fn identity<T>(x: T): T {
            x
        }
        
        fn max<T: Ord>(a: T, b: T): T {
            if a > b { a } else { b }
        }
        
        fn main() {
            let x: i32 = identity(42);
            let y: i32 = max(10, 20);
        }
        "#;
        
        let result = parse_and_lower(code);
        println!("{}", result.0);
    }

    #[test]
    fn test_pattern_matching() {
        let code = r#"
        enum Option<T> {
            Some(T),
            None
        }
        
        fn unwrap_or<T>(opt: Option<T>, default: T) -> T {
            match opt {
                Option::Some(x) => x,
                Option::None => default
            }
        }
        
        fn main() {
            let x = Option::Some(42);
            let y = unwrap_or(x, 0);
        }
        "#;
        
        let result = parse_and_lower(code);
        println!("{}", result.0);
    }

    #[test]
    fn test_error_handling() {
        let code = r#"
        enum Result<T, E> {
            Ok(T),
            Err(E)
        }
        
        fn divide(a: i32, b: i32) -> Result<i32, String> {
            if b == 0 {
                Result::Err("division by zero".to_string())
            } else {
                Result::Ok(a / b)
            }
        }
        
        fn main() {
            match divide(10, 2) {
                Result::Ok(result) => println!("Result: {}", result),
                Result::Err(err) => println!("Error: {}", err)
            }
        }
        "#;
        
        let result = parse_and_lower(code);
        println!("{}", result.0);
    }

