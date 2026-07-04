#[cfg(test)]
mod tests {
    use ir::ast::{Stmt, TypeKind};
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
        parse_program(StrId(ctx.intern(src)), "<test>", ctx_for_parse, bump).statements
    }

    macro_rules! first_stmt {
        ($stmts:expr) => {
            $stmts.into_iter().next().expect("no statements")
        };
    }

    #[test]
    fn test_aligned_pointer_simple() {
        let bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(4096, 8).unwrap());
        let stmts = parse("fn main() { let x: *i32 = 0; }", bump);
        match first_stmt!(stmts) {
            Stmt::FuncDecl(f) => {
                let body = f.body.unwrap();
                match body.block[0] {
                    Stmt::Let(let_stmt) => {
                        let ty = let_stmt.type_annotation;
                        assert!(!ty.nullable);
                        match ty.kind {
                            TypeKind::SafePointer { .. } => {}
                            _ => panic!("Expected Pointer type"),
                        }
                    }
                    _ => panic!("Expected Let"),
                }
            }
            _ => panic!("Expected FuncDecl"),
        }
    }

    #[test]
    fn test_aligned_pointer_nullable() {
        let bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(4096, 8).unwrap());
        let stmts = parse("fn main() { let x: *i32? = 0; }", bump);
        match first_stmt!(stmts) {
            Stmt::FuncDecl(f) => {
                let body = f.body.unwrap();
                match body.block[0] {
                    Stmt::Let(let_stmt) => {
                        let ty = let_stmt.type_annotation;
                        assert!(ty.nullable);
                        match ty.kind {
                            TypeKind::SafePointer { .. } => {}
                            _ => panic!("Expected Pointer type"),
                        }
                    }
                    _ => panic!("Expected Let"),
                }
            }
            _ => panic!("Expected FuncDecl"),
        }
    }

    #[test]
    fn test_raw_pointer_simple() {
        let bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(4096, 8).unwrap());
        let stmts = parse("fn main() { let x: **i32 = 0; }", bump);
        match first_stmt!(stmts) {
            Stmt::FuncDecl(f) => {
                let body = f.body.unwrap();
                match body.block[0] {
                    Stmt::Let(let_stmt) => {
                        let ty = let_stmt.type_annotation;
                        assert!(!ty.nullable);
                        match ty.kind {
                            TypeKind::SafePointer { .. } => {}
                            _ => panic!("Expected Pointer type"),
                        }
                    }
                    _ => panic!("Expected Let"),
                }
            }
            _ => panic!("Expected FuncDecl"),
        }
    }

    #[test]
    fn test_raw_pointer_nullable() {
        let bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(4096, 8).unwrap());
        let stmts = parse("fn main() { let x: **i32? = 0; }", bump);
        match first_stmt!(stmts) {
            Stmt::FuncDecl(f) => {
                let body = f.body.unwrap();
                match body.block[0] {
                    Stmt::Let(let_stmt) => {
                        let ty = let_stmt.type_annotation;
                        assert!(ty.nullable);
                        match ty.kind {
                            TypeKind::SafePointer { .. } => {}
                            _ => panic!("Expected Pointer type"),
                        }
                    }
                    _ => panic!("Expected Let"),
                }
            }
            _ => panic!("Expected FuncDecl"),
        }
    }
}
