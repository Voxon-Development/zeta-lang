#[cfg(test)]
mod prettify_tests {
    use ir::pretty::IrPrettyPrinter;
    use ctrc_graph::hir_integration::convenience;
    use ctrc_graph::hir_integration::CTRCHirIntegration;
    use ir::span::SourceSpan;
    use std::sync::Arc;
    use zetaruntime::string_pool::StringPool;
    use zetaruntime::bump::GrowableBump;
    use ir::hir::*;

    fn create_demo_string_pool() -> Arc<StringPool> {
        Arc::new(StringPool::new().expect("Failed to create string pool"))
    }

    fn create_demo_str_id(pool: &StringPool, s: &str) -> StrId {
        StrId::new(pool.intern(s))
    }

    #[test]
    fn test_ctrc_with_ir_prints() {
        let string_pool: Arc<StringPool> = create_demo_string_pool();
        let bump = GrowableBump::new(8192, 16);

        // Create string IDs
        let module_name = create_demo_str_id(&string_pool, "ctrc_demo");
        let person_class = create_demo_str_id(&string_pool, "Person");
        let database_class = create_demo_str_id(&string_pool, "Database");

        // Field names
        let name_field = create_demo_str_id(&string_pool, "name");
        let age_field = create_demo_str_id(&string_pool, "age");
        let connection_field = create_demo_str_id(&string_pool, "connection");

        // Function names
        let main_func_name = create_demo_str_id(&string_pool, "main");
        let create_person_func = create_demo_str_id(&string_pool, "create_person");

        // Variable names
        let person_var = create_demo_str_id(&string_pool, "person");
        let db_var = create_demo_str_id(&string_pool, "db");

        // Create structs with destructors
        let person_destructor = bump.alloc_value(HirStmt::Block { body: &[] });
        let database_destructor = bump.alloc_value(HirStmt::Block { body: &[] });

        let person_struct = HirStruct {
            name: person_class,
            visibility: Visibility::Public,
            generics: None,
            fields: bump.alloc_slice(&[
                HirField {
                    name: name_field,
                    field_type: HirType::String,
                    visibility: Visibility::Public,
                },
                HirField {
                    name: age_field,
                    field_type: HirType::U32,
                    visibility: Visibility::Private,
                },
            ]),
            interfaces: None,
            methods: None,
            constants: None,
            destructor: Some(person_destructor), // Has destructor
        };

        let database_struct = HirStruct {
            name: database_class,
            visibility: Visibility::Public,
            generics: None,
            fields: bump.alloc_slice(&[
                HirField {
                    name: connection_field,
                    field_type: HirType::String,
                    visibility: Visibility::Private,
                },
            ]),
            interfaces: None,
            methods: None,
            constants: None,
            destructor: Some(database_destructor), // Has destructor
        };

        // Create function that allocates objects (will trigger CTRC analysis)
        let create_person_func_def = HirFunc {
            name: create_person_func,
            visibility: Visibility::Public,
            is_unsafe: false,
            inline: false,
            noinline: false,
            generics: None,
            params: None,
            return_type: Some(HirType::Class(person_class, &[])),
            body: Some(HirStmt::Block {
                body: bump.alloc_slice(&[
                    HirStmt::Let {
                        name: person_var,
                        ty: HirType::Class(person_class, &[]),
                        value: HirExpr::ClassInit {
                            name: bump.alloc_value(HirExpr::Ident(person_class)),
                            args: bump.alloc_slice(&[
                                HirExpr::String(create_demo_str_id(&string_pool, "Alice")),
                                HirExpr::Number(30),
                            ]),
                            span: SourceSpan::default(),
                        },
                        mutable: false,
                    },
                    HirStmt::Return(Some(bump.alloc_value(HirExpr::Ident(person_var)))),
                ]),
            }),
        };

        // Create main function that uses the objects
        let main_func_def = HirFunc {
            name: main_func_name,
            visibility: Visibility::Public,
            is_unsafe: false,
            inline: false,
            noinline: false,
            generics: None,
            params: None,
            return_type: Some(HirType::I32),
            body: Some(HirStmt::Block {
                body: bump.alloc_slice(&[
                    HirStmt::Let {
                        name: person_var,
                        ty: HirType::Class(person_class, &[]),
                        value: HirExpr::Call {
                            callee: bump.alloc_value(HirExpr::Ident(create_person_func)),
                            args: &[],
                        },
                        mutable: false,
                    },
                    HirStmt::Let {
                        name: db_var,
                        ty: HirType::Class(database_class, &[]),
                        value: HirExpr::ClassInit {
                            name: bump.alloc_value(HirExpr::Ident(database_class)),
                            args: bump.alloc_slice(&[
                                HirExpr::String(create_demo_str_id(&string_pool, "localhost:5432")),
                            ]),
                            span: SourceSpan::default(),
                        },
                        mutable: false,
                    },
                    HirStmt::Return(Some(bump.alloc_value(HirExpr::Number(0)))),
                ]),
            }),
        };

        // Create HIR module
        let module = HirModule {
            name: module_name,
            imports: &[],
            items: bump.alloc_slice(&[
                Hir::Struct(bump.alloc_value(person_struct)),
                Hir::Struct(bump.alloc_value(database_struct)),
                Hir::Func(bump.alloc_value(create_person_func_def)),
                Hir::Func(bump.alloc_value(main_func_def)),
            ]),
        };

        println!("Original HIR Module:");
        let mut printer = IrPrettyPrinter::new(string_pool.clone());
        let original_output = printer.format_hir_module(&module).unwrap();
        println!("{}", original_output);

        println!("\nRunning CTRC Analysis...\n");

        // Analyze HIR module with CTRC
        let module_with_ctrc = CTRCHirIntegration::analyze_hir_module(module, &bump);

        // Get analysis summary
        let summary = CTRCHirIntegration::get_analysis_summary(&module_with_ctrc);
        println!("CTRC Analysis Summary:");
        println!("  Structs with destructors: {}", summary.structs_with_destructors);
        println!("  Drop insertions: {}", summary.drop_insertions);
        println!("  Destructor calls: {}", summary.destructor_calls);
        println!("  Allocation sites: {}", summary.allocation_sites);
        println!("  Variable aliases: {}", summary.variable_aliases);
        println!("  Memory safe: {}", if summary.memory_safe { "Yes" } else { "No" });
        println!("  Has analysis data: {}", if summary.has_analysis_data() { "Yes" } else { "No" });

        // Verify memory safety
        let is_memory_safe = CTRCHirIntegration::verify_memory_safety(&module_with_ctrc);
        println!("  Memory safety verified: {}", if is_memory_safe { "✅ Pass" } else { "❌ Fail" });

        println!("\nHIR Module with CTRC Analysis:");
        let ctrc_output = IrPrettyPrinter::hir_with_ctrc_to_string(string_pool.clone(), &module_with_ctrc).unwrap();
        println!("{}", ctrc_output);

        println!("\nUsing Convenience API:");
        let convenience_output = convenience::analyze_and_pretty_print(module, &bump, string_pool).unwrap();
        println!("{}", convenience_output);
    }
}