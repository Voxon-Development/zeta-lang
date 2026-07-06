#[cfg(test)]
mod tests {
    use ir::ast::{Stmt, Visibility};
    use ir::hir::StrId;
    use ir::registry::global_registry::GlobalRegistry;
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
        let ctx_clone = Arc::clone(&ctx);
        std::mem::forget(ctx);
        parse_program(StrId(ctx_clone.intern(src)), "<test>", ctx_clone, bump).statements
    }

    #[test]
    fn test_import_single_segment() {
        let bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(4096, 8).unwrap());
        let stmts = parse("import foo;", bump);
        match stmts.into_iter().next().expect("no stmts") {
            Stmt::Import(i) => {
                assert_eq!(i.path.path.len(), 1);
                assert_eq!(i.path.path[0].as_str(), "foo");
            }
            other => panic!("expected Import, got {:?}", other),
        }
    }

    #[test]
    fn test_import_two_segments() {
        let bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(4096, 8).unwrap());
        let stmts = parse("import foo.bar;", bump);
        match stmts.into_iter().next().expect("no stmts") {
            Stmt::Import(i) => {
                assert_eq!(i.path.path.len(), 2);
                assert_eq!(i.path.path[0].as_str(), "foo");
                assert_eq!(i.path.path[1].as_str(), "bar");
            }
            other => panic!("expected Import, got {:?}", other),
        }
    }

    #[test]
    fn test_import_dotted_path() {
        let bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(4096, 8).unwrap());
        let stmts = parse("import foo.bar.Baz;", bump);
        match stmts.into_iter().next().expect("no stmts") {
            Stmt::Import(i) => {
                let segs: Vec<&str> = i.path.path.iter().map(|s| s.as_str()).collect();
                assert_eq!(segs, vec!["foo", "bar", "Baz"]);
            }
            other => panic!("expected Import, got {:?}", other),
        }
    }

    #[test]
    fn test_multiple_imports() {
        let bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(4096, 8).unwrap());
        let stmts = parse("import a; import b.c;", bump);
        assert_eq!(stmts.len(), 2);
        assert!(matches!(stmts[0], Stmt::Import(_)));
        assert!(matches!(stmts[1], Stmt::Import(_)));
    }

    #[test]
    fn test_package_single_segment() {
        let bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(4096, 8).unwrap());
        let stmts = parse("package com;", bump);
        match stmts.into_iter().next().expect("no stmts") {
            Stmt::Package(p) => {
                assert_eq!(p.path.path.len(), 1);
                assert_eq!(p.path.path[0].as_str(), "com");
            }
            other => panic!("expected Package, got {:?}", other),
        }
    }

    #[test]
    fn test_package_dotted_path() {
        let bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(4096, 8).unwrap());
        let stmts = parse("package com.example.myapp;", bump);
        match stmts.into_iter().next().expect("no stmts") {
            Stmt::Package(p) => {
                let segs: Vec<&str> = p.path.path.iter().map(|s| s.as_str()).collect();
                assert_eq!(segs, vec!["com", "example", "myapp"]);
            }
            other => panic!("expected Package, got {:?}", other),
        }
    }

    #[test]
    fn test_package_sets_module_name_via_lowering() {
        use scribe_parser::hir_lowerer::context::HirLowerer;

        let src = "package com.example;\nfn main() {}";
        let bump_arc = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(4096, 8).unwrap());
        let ctx = Arc::new(StringPool::new().unwrap());
        let ctx2 = Arc::clone(&ctx);
        let result = parse_program(StrId(ctx.clone().intern(src)), "<test>", ctx2, bump_arc);

        let pool2 = Arc::new(StringPool::new().unwrap());
        let hir_bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(4096, 8).unwrap());
        let dep_graph = Default::default();
        let registry = GlobalRegistry::new();
        let mut lowerer = HirLowerer::new(pool2, hir_bump, &dep_graph, registry);
        let module = lowerer.lower_module(result.statements, 0);
        assert_eq!(
            module.name.as_str(),
            "com.example",
            "HirModule name should be the package path"
        );
    }

    #[test]
    fn test_module_decl_empty() {
        let bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(4096, 8).unwrap());
        let stmts = parse("module utils {}", bump);
        match stmts.into_iter().next().expect("no stmts") {
            Stmt::Module(m) => {
                assert_eq!(m.name.as_str(), "utils");
                assert_eq!(m.body.len(), 0);
            }
            other => panic!("expected Module, got {:?}", other),
        }
    }

    #[test]
    fn test_module_decl_with_function() {
        let bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(4096, 8).unwrap());
        let stmts = parse("module utils { fn helper() -> i32 { return 42; } }", bump);
        match stmts.into_iter().next().expect("no stmts") {
            Stmt::Module(m) => {
                assert_eq!(m.name.as_str(), "utils");
                assert_eq!(m.body.len(), 1);
                assert!(matches!(m.body[0], Stmt::FuncDecl(_)));
            }
            other => panic!("expected Module, got {:?}", other),
        }
    }

    #[test]
    fn test_module_decl_with_multiple_items() {
        let bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(4096, 8).unwrap());
        let stmts = parse(
            "module math { \
                fn add(a: i32, b: i32) -> i32 { return a; } \
                struct Vec2 { x: f32, y: f32 } \
             }",
            bump,
        );
        match stmts.into_iter().next().expect("no stmts") {
            Stmt::Module(m) => {
                assert_eq!(m.name.as_str(), "math");
                assert_eq!(m.body.len(), 2);
                assert!(matches!(m.body[0], Stmt::FuncDecl(_)));
                assert!(matches!(m.body[1], Stmt::StructDecl(_)));
            }
            other => panic!("expected Module, got {:?}", other),
        }
    }

    #[test]
    fn test_module_decl_nested() {
        let bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(4096, 8).unwrap());
        let stmts = parse("module outer { module inner { fn foo() {} } }", bump);
        match stmts.into_iter().next().expect("no stmts") {
            Stmt::Module(outer) => {
                assert_eq!(outer.name.as_str(), "outer");
                assert_eq!(outer.body.len(), 1);
                match outer.body[0] {
                    Stmt::Module(inner) => {
                        assert_eq!(inner.name.as_str(), "inner");
                        assert_eq!(inner.body.len(), 1);
                        assert!(matches!(inner.body[0], Stmt::FuncDecl(_)));
                    }
                    other => panic!("expected inner Module, got {:?}", other),
                }
            }
            other => panic!("expected Module, got {:?}", other),
        }
    }

    #[test]
    fn test_module_visibility_modifier_vs_decl() {
        let bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(4096, 8).unwrap());
        let stmts = parse("module fn foo() {}", bump);
        match stmts.into_iter().next().expect("no stmts") {
            Stmt::FuncDecl(f) => {
                assert_eq!(f.name.as_str(), "foo");
                assert_eq!(
                    f.function_metadata.visibility,
                    Visibility::Module,
                    "fn should have module visibility"
                );
            }
            other => panic!("expected FuncDecl with module visibility, got {:?}", other),
        }
    }

    #[test]
    fn test_module_decl_visibility_stored() {
        let bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(4096, 8).unwrap());
        let stmts = parse("module helpers {}", bump);
        match stmts.into_iter().next().expect("no stmts") {
            Stmt::Module(m) => {
                assert_eq!(m.visibility, Visibility::Module);
            }
            other => panic!("expected Module, got {:?}", other),
        }
    }

    #[test]
    fn test_package_import_and_module_together() {
        let bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(4096, 8).unwrap());
        let src = "package com.example;\n\
                   import std.io;\n\
                   module utils { fn helper() {} }\n\
                   fn main() {}";
        let stmts = parse(src, bump);
        assert_eq!(stmts.len(), 4);
        assert!(matches!(stmts[0], Stmt::Package(_)));
        assert!(matches!(stmts[1], Stmt::Import(_)));
        assert!(matches!(stmts[2], Stmt::Module(_)));
        assert!(matches!(stmts[3], Stmt::FuncDecl(_)));
    }

    #[test]
    fn test_import_path_display_roundtrip() {
        let bump = Arc::new(GrowableAtomicBump::with_capacity_and_aligned(4096, 8).unwrap());
        let stmts = parse("import alpha.beta.gamma;", bump);
        match stmts.into_iter().next().expect("no stmts") {
            Stmt::Import(i) => {
                let displayed = format!("{}", i.path);
                assert_eq!(displayed, "alpha.beta.gamma");
            }
            other => panic!("expected Import, got {:?}", other),
        }
    }
}
