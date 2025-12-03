#![cfg(test)]
use std::sync::Arc;
use scribe_parser::parser::parse_program;
use zetaruntime::arena::GrowableAtomicBump;
use zetaruntime::string_pool::StringPool;

#[cfg(test)]
mod struct_init_tests {
    use super::*;

    #[test]
    fn test_struct_init_syntax() {
        let code = r#"
        struct Point { x: i32, y: i32 }
        struct Vec3f { x: f32, y: f32, z: f32 } {}
        struct Vec9f { a: Vec3f, b: Vec3f, c: Vec3f } {}

        fn main() {
            // Named field initialization
            let p1: Point = Point { x: 1, y: 2 };
            
            // With trailing comma
            let p2: Point = Point { x: 3, y: 4, };
            
            // Nested struct initialization
            let v1: Vec3f = Vec3f { x: 1.0, y: 2.0, z: 3.0 };
            
            // Deeply nested
            let v9: Vec9f = Vec9f {
                a: Vec3f { x: 1.0, y: 0.0, z: 0.0 },
                b: Vec3f { x: 0.0, y: 1.0, z: 0.0 },
                c: Vec3f { x: 0.0, y: 0.0, z: 1.0 },
            };
            
            // Mixed with other expressions
            let x: i32 = 10;
            let p3: Point = Point { x, y: x + 1 };
        }
        "#;

        let context = Arc::new(StringPool::new().unwrap());
        let atomic_bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(1024, 8).unwrap());
        let tokens = parse_program(code, "test.zeta", context.clone(), atomic_bump.clone());

        //println!("{:#?}", tokens);
    }

    #[test]
    fn test_named_struct_init() {
        let code = r#"
        struct Point { x: i32, y: i32 }
        
        fn main() {
            // Named field initialization
            let p1: Point = Point { x: 1, y: 2 };
            
            // With reordered fields
            let p2: Point = Point { y: 4, x: 3 };
            
            // With trailing comma
            let p3: Point = Point { x: 5, y: 6, };
        }
        "#;

        let context = Arc::new(StringPool::new().unwrap());
        let atomic_bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(1024, 8).unwrap());
        let tokens = parse_program(code, "test.zeta", context.clone(), atomic_bump.clone());

        //println!("{:#?}", tokens);
    }

    #[test]
    fn test_mixed_struct_init() {
        let code = r#"
        struct Person { name: String, age: i32, active: bool }
        
        fn create_person() {
            // Mixed initialization with expressions
            let name: String = "Alice".to_string();
            let age: i32 = 30;
            let p: Person = Person { name, age, active: true };
        }
        "#;

        let context = Arc::new(StringPool::new().unwrap());
        let atomic_bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(1024, 8).unwrap());
        let tokens = parse_program(code, "test.zeta", context.clone(), atomic_bump.clone());

        //println!("{:#?}", tokens);
    }

    #[test]
    fn test_empty_struct_init() {
        let code = r#"
        struct Empty {}
        
        fn main() {
            let e: Empty = Empty {};
        }
        "#;

        let context: Arc<StringPool> = Arc::new(StringPool::new().unwrap());
        let atomic_bump: Arc<GrowableAtomicBump> = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(1024, 8).unwrap());
        let tokens = parse_program(code, "test.zeta", context.clone(), atomic_bump.clone());

        //println!("{:#?}", tokens);
    }

}