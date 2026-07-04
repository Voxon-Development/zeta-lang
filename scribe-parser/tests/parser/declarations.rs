#[cfg(test)]
mod tests {
    use ir::ast::{Stmt, Visibility};
    use ir::hir::StrId;
    use scribe_parser::parser::parse_program;
    use std::sync::Arc;
    use zetaruntime::arena::GrowableAtomicBump;
    use zetaruntime::bump::GrowableBump;
    use zetaruntime::string_pool::StringPool;

    fn parse<'a, 'bump>(
        src: &'bump str,
        bump: Arc<GrowableAtomicBump<'bump>>,
    ) -> Vec<Stmt<'a, 'bump>, &'bump GrowableBump<'bump>> {
        let ctx = Arc::new(StringPool::new().unwrap());
        let ctx_for_parse = Arc::clone(&ctx);
        parse_program(
            StrId(ctx_for_parse.clone().intern(src)),
            "<test>",
            ctx_for_parse,
            bump,
        )
        .statements
    }

    macro_rules! first_stmt {
        ($stmts:expr) => {{ $stmts.into_iter().next().expect("no statements") }};
    }

    #[test]
    fn test_struct_simple() {
        let bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(4096, 8).unwrap());
        let stmts = parse("struct Point { x: i32, y: i32 }", bump);
        match first_stmt!(stmts) {
            Stmt::StructDecl(s) => {
                assert_eq!(s.name.as_str(), "Point");
                assert!(matches!(s.visibility, Visibility::Public));
                assert!(s.generics.is_none());
                assert!(s.params.is_some());
                let fields = s.params.unwrap();
                assert_eq!(fields.len(), 2);
            }
            _ => panic!("Expected StructDecl"),
        }
    }

    #[test]
    fn test_struct_with_generics() {
        let bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(4096, 8).unwrap());
        let stmts = parse("struct Vec2<T> { x: T, y: T }", bump);
        match first_stmt!(stmts) {
            Stmt::StructDecl(s) => {
                assert_eq!(s.name.as_str(), "Vec2");
                assert!(s.generics.is_some());
                assert_eq!(s.generics.unwrap().len(), 1);
            }
            _ => panic!("Expected StructDecl"),
        }
    }

    #[test]
    fn test_struct_empty() {
        let bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(4096, 8).unwrap());
        let stmts = parse("struct Unit {}", bump);
        match first_stmt!(stmts) {
            Stmt::StructDecl(s) => {
                assert_eq!(s.name.as_str(), "Unit");
                assert!(s.params.is_none() || s.params.unwrap().is_empty());
            }
            _ => panic!("Expected StructDecl"),
        }
    }

    #[test]
    fn test_tuple_struct() {
        let bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(4096, 8).unwrap());
        let stmts = parse("struct Point(i32, i32);", bump);
        match first_stmt!(stmts) {
            Stmt::StructDecl(s) => {
                assert_eq!(s.name.as_str(), "Point");
                assert!(s.params.is_some());
                assert_eq!(s.params.unwrap().len(), 2);
            }
            _ => panic!("Expected StructDecl"),
        }
    }

    #[test]
    fn test_unit_struct() {
        let bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(4096, 8).unwrap());
        let stmts = parse("struct Unit;", bump);
        match first_stmt!(stmts) {
            Stmt::StructDecl(s) => {
                assert_eq!(s.name.as_str(), "Unit");
            }
            _ => panic!("Expected StructDecl"),
        }
    }

    #[test]
    fn test_struct_private_fields() {
        let bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(4096, 8).unwrap());
        let stmts = parse("struct Counter { private count: i32 }", bump);
        match first_stmt!(stmts) {
            Stmt::StructDecl(s) => {
                assert_eq!(s.name.as_str(), "Counter");
                let fields = s.params.unwrap();
                assert_eq!(fields.len(), 1);
                // The field should be private
            }
            _ => panic!("Expected StructDecl"),
        }
    }

    #[test]
    fn test_struct_with_methods() {
        let bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(4096, 8).unwrap());
        let stmts = parse(
            r#"
            struct Point {
                x: i32,
                y: i32,
            }

            impl Point {
                fn new(x: i32, y: i32) -> Point {
                    return Point { x, y };
                }

                fn distance(&this) -> i32 {
                    return x + y;
                }
            }
        "#,
            bump,
        );

        // Check struct
        let mut iter = stmts.into_iter();
        match iter.next().expect("no statements") {
            Stmt::StructDecl(s) => {
                assert_eq!(s.name.as_str(), "Point");
                assert!(s.params.is_some());
                assert_eq!(s.params.unwrap().len(), 2); // x and y fields
                assert!(s.body.is_empty()); // no methods in struct
            }
            _ => panic!("Expected StructDecl"),
        }

        // Check impl
        match iter.next().expect("no impl") {
            Stmt::ImplDecl(i) => {
                assert_eq!(i.target.as_str(), "Point");
                assert!(i.interface.is_none()); // not a trait impl
                assert!(i.methods.is_some());
                assert_eq!(i.methods.unwrap().len(), 2); // new and distance
            }
            _ => panic!("Expected ImplDecl"),
        }
    }

    #[test]
    fn test_interface_simple() {
        let bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(4096, 8).unwrap());
        let stmts = parse("interface Drawable { fn draw(&this); }", bump);
        match first_stmt!(stmts) {
            Stmt::InterfaceDecl(i) => {
                assert_eq!(i.name.as_str(), "Drawable");
                assert!(matches!(i.visibility, Visibility::Public));
                assert!(!i.sealed);
                assert!(i.methods.is_some());
                assert_eq!(i.methods.unwrap().len(), 1);
            }
            _ => panic!("Expected InterfaceDecl"),
        }
    }

    #[test]
    fn test_interface_multiple_methods() {
        let bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(4096, 8).unwrap());
        let stmts = parse(
            r#"
            interface Comparable {
                fn compare(&this, other: i32) -> i32;
                fn equals(&this, other: i32) -> bool;
            }
        "#,
            bump,
        );
        match first_stmt!(stmts) {
            Stmt::InterfaceDecl(i) => {
                assert_eq!(i.name.as_str(), "Comparable");
                assert!(i.methods.is_some());
                assert_eq!(i.methods.unwrap().len(), 2);
            }
            _ => panic!("Expected InterfaceDecl"),
        }
    }

    #[test]
    fn test_interface_with_generics() {
        let bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(4096, 8).unwrap());
        let stmts = parse("interface Container<T> { fn get(&this) -> T; }", bump);
        match first_stmt!(stmts) {
            Stmt::InterfaceDecl(i) => {
                assert_eq!(i.name.as_str(), "Container");
                assert!(i.generics.is_some());
                assert_eq!(i.generics.unwrap().len(), 1);
            }
            _ => panic!("Expected InterfaceDecl"),
        }
    }

    #[test]
    fn test_interface_sealed() {
        let bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(4096, 8).unwrap());
        let stmts = parse(
            "sealed interface SealedTrait permits A, B, C { fn method(&this); }",
            bump,
        );
        match first_stmt!(stmts) {
            Stmt::InterfaceDecl(i) => {
                assert_eq!(i.name.as_str(), "SealedTrait");
                assert!(i.sealed);
                assert!(i.permits.is_some());
            }
            _ => panic!("Expected InterfaceDecl"),
        }
    }

    #[test]
    fn test_interface_empty() {
        let bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(4096, 8).unwrap());
        let stmts = parse("interface Marker {}", bump);
        match first_stmt!(stmts) {
            Stmt::InterfaceDecl(i) => {
                assert_eq!(i.name.as_str(), "Marker");
                assert!(i.methods.is_none() || i.methods.unwrap().is_empty());
            }
            _ => panic!("Expected InterfaceDecl"),
        }
    }
}
