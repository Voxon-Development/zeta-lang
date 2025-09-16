// Integration tests for Cranelift-related intrinsics and layout utilities.
//
// Testing framework: Rust built-in #[test] (cargo test).
//
// These tests focus on pure functions and enum resolution, covering happy paths,
// edge cases, and failure-like conditions where applicable.

#[cfg(test)]
mod tests {
    use super::*;
    // Testing framework: Rust's built-in test harness with #[test].

    // --- resolve_intrinsic tests ---
    #[test]
    fn resolve_intrinsic_known_symbols() {
        assert\!(matches\!(resolve_intrinsic("ptr_add"), Some(Intrinsic::PtrAdd)));
        assert\!(matches\!(resolve_intrinsic("ptr_deref"), Some(Intrinsic::PtrDeref)));
        assert\!(matches\!(resolve_intrinsic("ptr_store"), Some(Intrinsic::PtrStore)));
        assert\!(matches\!(resolve_intrinsic("ptr_from_addr"), Some(Intrinsic::PtrFromAddr)));
        assert\!(matches\!(resolve_intrinsic("ptr_addr"), Some(Intrinsic::PtrAddr)));
        assert\!(matches\!(resolve_intrinsic("ptr_cast_to_raw"), Some(Intrinsic::PtrCastToRaw)));
    }

    #[test]
    fn resolve_intrinsic_unknown_symbol_returns_none() {
        for name in ["", "unknown", "ptr_Add", "PTR_ADD", "ptradd", "ptr-cast-to-raw"] {
            assert\!(resolve_intrinsic(name).is_none(), "expected None for {:?}", name);
        }
    }

    // --- round_up tests ---
    #[test]
    fn round_up_no_align_or_align_one_is_noop() {
        assert_eq\!(round_up(0, 0), 0); // degenerate: align <= 1 is identity
        assert_eq\!(round_up(0, 1), 0);
        assert_eq\!(round_up(7, 1), 7);
        assert_eq\!(round_up(123456, 1), 123456);
    }

    #[test]
    fn round_up_power_of_two_alignments() {
        // 8-byte alignment
        assert_eq\!(round_up(0, 8), 0);
        assert_eq\!(round_up(1, 8), 8);
        assert_eq\!(round_up(7, 8), 8);
        assert_eq\!(round_up(8, 8), 8);
        assert_eq\!(round_up(9, 8), 16);

        // 16-byte alignment
        assert_eq\!(round_up(1, 16), 16);
        assert_eq\!(round_up(15, 16), 16);
        assert_eq\!(round_up(16, 16), 16);
        assert_eq\!(round_up(17, 16), 32);
    }

    #[test]
    fn round_up_large_values() {
        let near_u64_max = u64::MAX - 7; // keep some headroom
        // With align 8, rounding up a value already divisible should be identity.
        let aligned = near_u64_max & \!7;
        assert_eq\!(round_up(aligned, 8), aligned);
    }

    // --- tag_bytes tests ---
    #[test]
    fn tag_bytes_thresholds() {
        // According to the encoding scheme in tag_bytes:
        // 0..=0x100 => 1
        assert_eq\!(tag_bytes(0), 1);
        assert_eq\!(tag_bytes(1), 1);
        assert_eq\!(tag_bytes(0x100), 1);

        // 0x101..=0x1_0000 => 2
        assert_eq\!(tag_bytes(0x101), 2);
        assert_eq\!(tag_bytes(0x1_0000), 2);

        // 0x1_0001..=0x1_0000_0000 => 4
        assert_eq\!(tag_bytes(0x1_0001), 4);
        assert_eq\!(tag_bytes(0x1_0000_0000), 4);

        // beyond => 8
        assert_eq\!(tag_bytes(0x1_0000_0000usize.saturating_add(1)), 8);
    }

    // --- clif_type_of tests ---
    #[test]
    fn clif_type_of_primitives() {
        use ir::ssa_ir::SsaType;
        use ir::hir::HirType;

        assert_eq\!(clif_type_of(&HirType::I8),    SsaType::I8);
        assert_eq\!(clif_type_of(&HirType::I16),   SsaType::I16);
        assert_eq\!(clif_type_of(&HirType::I32),   SsaType::I32);
        assert_eq\!(clif_type_of(&HirType::I64),   SsaType::I64);
        assert_eq\!(clif_type_of(&HirType::I128),  SsaType::I128);

        assert_eq\!(clif_type_of(&HirType::U8),    SsaType::U8);
        assert_eq\!(clif_type_of(&HirType::U16),   SsaType::U16);
        assert_eq\!(clif_type_of(&HirType::U32),   SsaType::U32);
        assert_eq\!(clif_type_of(&HirType::U64),   SsaType::U64);

        assert_eq\!(clif_type_of(&HirType::F32),   SsaType::F32);
        assert_eq\!(clif_type_of(&HirType::F64),   SsaType::F64);

        assert_eq\!(clif_type_of(&HirType::Boolean), SsaType::Bool);
        assert_eq\!(clif_type_of(&HirType::String),  SsaType::String);
        assert_eq\!(clif_type_of(&HirType::Void),    SsaType::Void);
    }

    #[test]
    fn clif_type_of_composites_user_and_tuple_shape() {
        use ir::ssa_ir::SsaType;
        use ir::hir::HirType;

        let user = HirType::Class("MyStruct".to_string(), vec\![HirType::I32, HirType::U8]);
        // SsaType::User(name, args mapped recursively)
        let mapped = clif_type_of(&user);
        match mapped {
            SsaType::User(n, args) => {
                assert_eq\!(n, "MyStruct");
                assert_eq\!(args.len(), 2);
                assert_eq\!(args[0], SsaType::I32);
                assert_eq\!(args[1], SsaType::U8);
            }
            _ => panic\!("expected SsaType::User"),
        }
    }

    // --- layout_of / sizeof tests ---
    #[test]
    fn layout_of_scalars_and_unsized() {
        use ir::ssa_ir::SsaType;

        let target = TargetInfo { ptr_bytes: 8 };

        // Scalars
        let l_i8 = layout_of(&SsaType::I8, &target).unwrap();
        assert_eq\!(l_i8.size, 1);
        assert_eq\!(l_i8.align, 1);

        let l_i64 = layout_of(&SsaType::I64, &target).unwrap();
        assert_eq\!(l_i64.size, 8);
        assert_eq\!(l_i64.align, 8);

        // Unsized
        let err_slice = layout_of(&SsaType::Slice, &target).unwrap_err();
        match err_slice {
            LayoutError::Unsized(_) => {}
            e => panic\!("expected Unsized, got {:?}", e),
        }

        let err_dyn = layout_of(&SsaType::Dyn, &target).unwrap_err();
        match err_dyn {
            LayoutError::Unsized(_) => {}
            e => panic\!("expected Unsized, got {:?}", e),
        }
    }

    #[test]
    fn layout_of_tuple_and_user_alignment_and_padding() {
        use ir::ssa_ir::SsaType;

        let target = TargetInfo { ptr_bytes: 8 };

        // Tuple(User) with mixed alignments: (U8, U64, U16)
        let tup = SsaType::Tuple(vec\![SsaType::U8, SsaType::U64, SsaType::U16]);
        let lt = layout_of(&tup, &target).unwrap();
        // Compute expected:
        // U8 at off 0..1, align 1 => off becomes 1
        // Round up to U64 align 8 => off becomes 8, then +8 => 16
        // Round up to U16 align 2 => off is 16 (already multiple of 2), then +2 => 18
        // Final round up to max_align (8) => size becomes 24, align 8
        assert_eq\!(lt.align, 8);
        assert_eq\!(lt.size, 24);

        // User mirrors tuple behavior for layout calculation
        let usr = SsaType::User("S".into(), vec\![SsaType::U8, SsaType::U64, SsaType::U16]);
        let lu = layout_of(&usr, &target).unwrap();
        assert_eq\!(lu.align, 8);
        assert_eq\!(lu.size, 24);
    }

    #[test]
    fn layout_of_enum_tagging_and_max_variant() {
        use ir::ssa_ir::SsaType;

        let target = TargetInfo { ptr_bytes: 8 };

        // Variants: one small (U8), one larger (U64)
        let en = SsaType::Enum(vec\![SsaType::U8, SsaType::U64]);
        let le = layout_of(&en, &target).unwrap();
        // max_variant: size 8, align 8
        // union_size = round_up(8, 8) = 8
        // tag size = tag_bytes(2) = 1, align 1
        // total = round_up(8, 1) + 1 = 8 + 1 = 9
        // align = max(8, 1) = 8
        assert_eq\!(le.align, 8);
        assert_eq\!(le.size, 9);
    }

    #[test]
    fn sizeof_delegates_to_layout_of() {
        use ir::ssa_ir::SsaType;

        let target = TargetInfo { ptr_bytes: 8 };
        let l = layout_of(&SsaType::F32, &target).unwrap();
        let s = sizeof(&SsaType::F32, &target).unwrap();
        assert_eq\!(l.size, s);
    }
}