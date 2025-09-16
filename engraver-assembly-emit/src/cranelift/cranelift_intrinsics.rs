use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::{types, InstBuilder, MemFlags, Signature, Type, Value};
use cranelift_codegen::isa::TargetIsa;
use cranelift_frontend::FunctionBuilder;
use ir::hir::HirType;
use std::io::Read;

#[derive(Debug, Clone, Copy)]
pub enum Intrinsic {
    PtrAdd,
    PtrDeref,
    PtrStore,
    PtrFromAddr,
    PtrAddr,
    PtrCastToRaw
}

/// Resolve by symbol name to intrinsic enum
pub fn resolve_intrinsic(name: &str) -> Option<Intrinsic> {
    use Intrinsic::*;
    match name {
        "ptr_add" => Some(PtrAdd),
        "ptr_deref" => Some(PtrDeref),
        "ptr_store" => Some(PtrStore),
        "ptr_from_addr" => Some(PtrFromAddr),
        "ptr_addr" => Some(PtrAddr),
        "ptr_cast_to_raw" => Some(PtrCastToRaw),
        _ => None,
    }
}/*

/// Helper: try to extract the inner T from `Ptr<T>` HirType.
/// Adjust to match how your HirType encodes generics.
pub fn as_ptr_inner(hir_ty: &HirType) -> Option<&HirType> {
    if let HirType::Class(name, args) = hir_ty {
        if name == "Ptr" {
            return args.get(0);
        }
    }
    None
}*/

/// Ensure `val` has `ptr_ty` as its CLIF type, extending/truncating as necessary.
/// Returns a Value guaranteed to be `ptr_ty`.
fn ensure_value_is_ptr_width(
    builder: &mut FunctionBuilder,
    val: Value,
    ptr_ty: Type,
    val_ty: Type,
) -> Value {
    if val_ty == ptr_ty {
        val
    } else if builder.func.dfg.value_type(val).bits() < ptr_ty.bits() {
        // Zero-extend smaller ints to pointer width
        builder.ins().uextend(ptr_ty, val)
    } else if builder.func.dfg.value_type(val).bits() > ptr_ty.bits() {
        // Truncate larger integer to pointer width
        builder.ins().ireduce(ptr_ty, val)
    } else {
        // same size but different type - just return
        val
    }
}

use cranelift::prelude::AbiParam;
use cranelift_module::{Linkage, Module};
use cranelift_object::object::read::elf::Dyn;
use ir::ssa_ir::{Function, SsaType};

fn push_param_for_hir_type(sig: &mut Signature, hir_ty: &HirType, isa: &dyn TargetIsa) {
    match hir_ty {
        // detect Array<T> - adapt to how your HirType encodes generics
        HirType::Class(name, args) if name == "Array" && args.len() == 1 => {
            let pt = AbiParam::new(ptr_clif_ty(isa));
            let len = AbiParam::new(usize_clif_ty(isa));
            sig.params.push(pt);
            sig.params.push(len);
        }
        // Ptr<T> is a single pointer param
        HirType::Class(name, _args) if name == "Ptr" => {
            sig.params.push(AbiParam::new(ptr_clif_ty(isa)));
        }
        // primitives
        _ => {
            let clif_t = clif_type_of(hir_ty);
            sig.params.push(AbiParam::new(super::clif_type(&clif_t)));
        }
    }
}

// For returns, same idea: push one or two return types depending on Array<T>.
fn push_return_for_hir_type(sig: &mut Signature, hir_ty: &HirType, isa: &dyn TargetIsa) {
    match hir_ty {
        HirType::Class(name, args) if name == "Array" && args.len() == 1 => {
            sig.returns.push(AbiParam::new(ptr_clif_ty(isa)));
            sig.returns.push(AbiParam::new(usize_clif_ty(isa)));
        }
        HirType::Class(name, _) if name == "Ptr" => {
            sig.returns.push(AbiParam::new(ptr_clif_ty(isa)));
        }
        _ => {
            sig.returns.push(AbiParam::new(super::clif_type(&clif_type_of(hir_ty))));
        }
    }
}

fn ptr_clif_ty(isa: &dyn TargetIsa) -> Type {
    isa.pointer_type()
}

fn usize_clif_ty(isa: &dyn TargetIsa) -> Type {
    // If you want isize vs usize semantics, change accordingly.
    // Usually `usize` is same bitwidth as pointer type but an integer type:
    // we will map usize to the pointer-width integer type.
    ptr_clif_ty(isa)
}

/// Ensure `val` has CLIF type `target_ty`. Widen/truncate if necessary.
fn ensure_val_ty(builder: &mut FunctionBuilder, val: Value, target_ty: Type) -> Value {
    let cur_ty = builder.func.dfg.value_type(val);
    if cur_ty == target_ty {
        val
    } else if cur_ty.bits() < target_ty.bits() {
        builder.ins().uextend(target_ty, val)
    } else if cur_ty.bits() > target_ty.bits() {
        builder.ins().ireduce(target_ty, val)
    } else {
        val
    }
}
pub(super) fn clif_type_of(ty: &HirType) -> SsaType {
    match ty {
        HirType::I8 => SsaType::I8,
        HirType::I16 => SsaType::I16,
        HirType::I32 => SsaType::I32,
        HirType::I64 => SsaType::I64,
        HirType::I128 => SsaType::I128,
        HirType::U8 => SsaType::U8,
        HirType::U16 => SsaType::U16,
        HirType::U32 => SsaType::U32,
        HirType::U64 => SsaType::U64,
        HirType::U128 => SsaType::U64,
        HirType::F32 => SsaType::F32,
        HirType::F64 => SsaType::F64,
        HirType::Boolean => SsaType::Bool,
        HirType::String => SsaType::String,
        HirType::Class(name, args) | HirType::Interface(name, args) | HirType::Enum(name, args) 
            => SsaType::User(name.clone(), args.iter().map(clif_type_of).collect()),
        HirType::Void => SsaType::Void,
        _ => unimplemented!("Unsupported type {:?}", ty),
    }
}

// In your codegen lowering for intrinsics:
fn lower_syscall(
    builder: &mut FunctionBuilder, 
    call: &mut cranelift_codegen::ir::function::Function,
    ssa_call: &mut Function,
    module: &mut impl Module
) -> Value {
    let (num_val, arg_vals) = extract_args(ssa_call); // all are machine-size ints
    let sig = runtime_syscall_signature(arg_vals.len()); // e.g., fn(i64,i64,...)->i64

    // create/import external function symbol:
    let callee = module.declare_function(&format!("syscall{}", arg_vals.len()),
                                         Linkage::Import, &sig)
        .map(|id| module.declare_func_in_func(id, call)).unwrap();
    
    

    // build call operands
    let mut operands = Vec::new();
    operands.push(num_val); // syscall number first argument
    operands.extend(arg_vals);

    // emit call
    let call_inst = builder.ins().call(callee, &operands);

    // get return value
    let results = builder.inst_results(call_inst);
    results[0]
    // return or store ret as the intrinsic result
}

fn runtime_syscall_signature(args_len: usize) -> Signature {
    todo!()
}

fn extract_args(function: &Function) -> (Value, Vec<Value>) {
    todo!()
}

#[derive(Clone, Copy, Debug)]
pub struct Layout { pub size: u64, pub align: u64 }

#[derive(Debug)]
pub enum LayoutError {
    Unsized(&'static str),
    Recursive,
    Unknown,
}

pub fn sizeof(ty: &SsaType, target: &TargetInfo) -> Result<u64, LayoutError> {
    Ok(layout_of(ty, target)?.size)
}

pub fn layout_of(ty: &SsaType, target: &TargetInfo) -> Result<Layout, LayoutError> {
    match ty {
        SsaType::Void            => Ok(Layout { size: 0, align: 1 }),
        SsaType::Bool | SsaType::I8 | SsaType::U8  => Ok(Layout { size: 1, align: 1 }),
        SsaType::I16 | SsaType::U16                => Ok(Layout { size: 2, align: 2 }),
        SsaType::I32 | SsaType::U32 | SsaType::F32 => Ok(Layout { size: 4, align: 4 }),
        SsaType::I64 | SsaType::U64 | SsaType::F64 => Ok(Layout { size: 8, align: 8 }),

        SsaType::Slice | SsaType::Dyn => Err(LayoutError::Unsized("unsized type")),

        // Tuples/structs: sequential fields with padding between and at end to struct align.
        SsaType::Tuple(fields) | SsaType::User(_, fields) => {
            let mut off = 0u64;
            let mut max_align = 1u64;
            for fty in fields {
                let f = layout_of(fty, target)?;
                max_align = max_align.max(f.align);
                off = round_up(off, f.align);
                off = off.checked_add(f.size).ok_or(LayoutError::Unknown)?;
            }
            let size = round_up(off, max_align);
            Ok(Layout { size, align: max_align })
        }

        // Enums/sum types: choose your scheme. Simple tagged union:
        SsaType::Enum(variants) => {
            if variants.is_empty() { return Ok(Layout { size: 0, align: 1 }); }
            let mut max_variant = Layout { size: 0, align: 1 };
            for v in variants {
                let l = layout_of(v, target)?;
                max_variant.size  = max_variant.size.max(l.size);
                max_variant.align = max_variant.align.max(l.align);
            }
            let tag = Layout { size: tag_bytes(variants.len()), align: 1 }; // define tag width
            let union_size = round_up(max_variant.size, max_variant.align);
            let total = round_up(union_size, tag.align).checked_add(tag.size).ok_or(LayoutError::Unknown)?;
            Ok(Layout { size: total, align: max_variant.align.max(tag.align) })
        }
        SsaType::I128 => Ok(Layout { size: 16, align: 16 }),
        SsaType::ISize => Ok(Layout { size: 8, align: 8 }),
        SsaType::USize => Ok(Layout { size: 8, align: 8 }),
        SsaType::String => Ok(Layout { size: 16, align: 16 }),
    }
}

#[inline]
fn round_up(x: u64, align: u64) -> u64 {
    if align <= 1 { return x; }
    let m = align - 1;
    (x + m) & !m
}

fn tag_bytes(variants: usize) -> u64 {
    // pick an encoding; here is a simple power-of-two step
    match variants {
        0..=0x100      => 1,
        0x101..=0x1_0000 => 2,
        0x1_0001..=0x1_0000_0000 => 4,
        _ => 8,
    }
}

pub struct TargetInfo { pub ptr_bytes: u64 /* plus anything else you need */ }

/// Codegen an intrinsic. Returns `Some(Value)` if the intrinsic produced a value,
/// or `None` if it produced no value (like store).
///
/// * `name`: intrinsic symbol like "__ptr_null"
/// * `args`: evaluated argument Values (in the CLIF function)
/// * `ret_hir_types`: expected return HirType(s). For single-return intrinsics you can index [0].
/// * `builder`: FunctionBuilder for the current function
/// * `isa`: target ISA to get pointer width
pub fn codegen_intrinsic(
    intr: Intrinsic,
    args: &[Value],
    ret_hir_types: &[HirType],
    builder: &mut FunctionBuilder,
    isa: &dyn TargetIsa,
) -> Option<Value> {
    let ptr_ty = isa.pointer_type();
    match intr {

        Intrinsic::PtrCastToRaw => {
            // args: [value]
            // If value is a Ptr<T> at the HIR level, or an Array<T> lowered as (ptr,len),
            // then test ptr != 0 and return boolean. If it's an integer, test != 0.
            //
            // Caller must pass the `ret_hir_types` so we could assert it's boolean, but
            // we'll just produce the Value here.

            let val = args[0];
            let val_ty = builder.func.dfg.value_type(val);
            let ptr_ty = isa.pointer_type();

            // Strategy:
            // 1) if incoming SSA value is pointer-sized already, compare against zero
            // 2) else if it's integer, up/down-cast to ptr width then compare
            // 3) else (non-pointer) return constant false

            let nonzero_test = if val_ty == ptr_ty {
                let zero = builder.ins().iconst(ptr_ty, 0);
                builder.ins().icmp(IntCC::NotEqual, val, zero)
            } else if val_ty.is_int() {
                // widen/truncate to pointer width then compare
                let normalized = ensure_value_is_ptr_width(builder, val, ptr_ty, val_ty);
                let zero = builder.ins().iconst(ptr_ty, 0);
                builder.ins().icmp(IntCC::NotEqual, normalized, zero)
            } else {
                // Not a pointer-ish or integer-ish typed SSA value: produce constant false.
                // Make sure to return a boolean-typed value consistent with your convention.
                let zero_bool = builder.ins().iconst(types::I8, 0); // boolean false
                return Some(zero_bool);
            };

            // `nonzero_test` is an i1/icmp predicate represented as an int by CLIF.
            // Return it directly (we used I8 earlier as our boolean conv).
            Some(nonzero_test)
        }

        Intrinsic::PtrAdd => {
            // args: [ptr, bytes:u64]
            // We add bytes (an integer) to the pointer.
            let p = args[0];
            let bytes = args[1];

            // Normalize arg types to pointer width
            let p_ty = builder.func.dfg.value_type(p);
            let b_ty = builder.func.dfg.value_type(bytes);

            let p = ensure_value_is_ptr_width(builder, p, ptr_ty, p_ty);
            let bytes = ensure_value_is_ptr_width(builder, bytes, ptr_ty, b_ty);

            let res = builder.ins().iadd(p, bytes);
            Some(res)
        }

        Intrinsic::PtrDeref => {
            // args: [ptr]
            // ret_hir_types[0] tells what to load
            let p = args[0];
            let p_ty = builder.func.dfg.value_type(p);
            let p = ensure_value_is_ptr_width(builder, p, ptr_ty, p_ty);

            // Determine CLIF load type from return HirType
            let ret_hir = &ret_hir_types[0];
            let clif_ret = clif_type_of(&ret_hir);

            // Use default MemFlags. Tune if you need volatile/readonly.
            let mem_flags = MemFlags::new();

            Some(builder.ins().load(super::clif_type(&clif_ret), mem_flags, p, 0))
        }

        Intrinsic::PtrStore => {
            // args: [ptr, value]
            let p = args[0];
            let val = args[1];

            let p_ty = builder.func.dfg.value_type(p);
            let p = ensure_value_is_ptr_width(builder, p, ptr_ty, p_ty);

            // We need the value type for store; assume val already has right CLIF type
            // (the caller should have produced correct typed value).
            let val_ty = builder.func.dfg.value_type(val);

            let mem_flags = MemFlags::new();
            builder.ins().store(mem_flags, val, p, 0);
            None
        }

        Intrinsic::PtrFromAddr => {
            // args: [addr: u64]
            // Cast integer address to pointer type.
            let addr = args[0];
            let addr_ty = builder.func.dfg.value_type(addr);
            let ptr = ensure_value_is_ptr_width(builder, addr, ptr_ty, addr_ty);
            Some(ptr)
        }

        Intrinsic::PtrAddr => {
            // args: [ptr] -> return u64 (or your 'usize' representation)
            let p = args[0];
            let p_ty = builder.func.dfg.value_type(p);
            let p = ensure_value_is_ptr_width(builder, p, ptr_ty, p_ty);

            // Cast pointer to a target-sized integer. We'll use I64 here for u64.
            // If you want `usize` semantics, map to pointer width integer (ptr_ty bits).
            let target_int_ty = if ptr_ty == types::I64 { types::I64 } else { ptr_ty };
            let casted = if builder.func.dfg.value_type(p) == target_int_ty {
                p
            } else if builder.func.dfg.value_type(p).bits() < target_int_ty.bits() {
                builder.ins().uextend(target_int_ty, p)
            } else {
                builder.ins().ireduce(target_int_ty, p)
            };
            Some(casted)
        }
    }
}
#[cfg(test)]
mod tests {
    use super::*;
    // Testing library/framework note:
    // These tests use Rust's built-in test framework (cargo test) with `#[test]`.
    // No external testing libraries are introduced to keep alignment with typical project setup.

    // Minimal stand-ins/imports for IR types used by this module.
    // If these modules exist elsewhere in the repository, these uses will resolve.
    use ir::ssa_ir::SsaType;
    use ir::hir::HirType;

    // Helper: safely construct a CLIF function builder sufficient for simple integer ops.
    // We avoid creating a full ISA; many type checks work at the IR level.
    fn make_builder() -> (cranelift_codegen::ir::function::Function, FunctionBuilder, Type) {
        use cranelift_codegen::ir::{Function as ClifFunction, Signature};
        use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext};

        // Build an empty function with a single block.
        let mut func = ClifFunction::with_name_signature(
            cranelift_codegen::ir::ExternalName::user(0, 0),
            Signature::new(cranelift_codegen::isa::CallConv::SystemV),
        );
        let mut fb_ctx = FunctionBuilderContext::new();
        let mut builder = FunctionBuilder::new(&mut func, &mut fb_ctx);
        let block = builder.create_block();
        builder.append_block_params_for_function_params(block);
        builder.switch_to_block(block);
        builder.seal_block(block);
        // Pointer type from a minimal fake; cranelift IR uses opaque pointer type from func's ISA later.
        let ptr_ty = types::I64; // assume 64-bit for IR shape tests (type relations only)
        (builder.func.clone(), builder, ptr_ty)
    }

    #[test]
    fn test_resolve_intrinsic_mappings() {
        assert_eq\!(resolve_intrinsic("ptr_add"), Some(Intrinsic::PtrAdd));
        assert_eq\!(resolve_intrinsic("ptr_deref"), Some(Intrinsic::PtrDeref));
        assert_eq\!(resolve_intrinsic("ptr_store"), Some(Intrinsic::PtrStore));
        assert_eq\!(resolve_intrinsic("ptr_from_addr"), Some(Intrinsic::PtrFromAddr));
        assert_eq\!(resolve_intrinsic("ptr_addr"), Some(Intrinsic::PtrAddr));
        assert_eq\!(resolve_intrinsic("ptr_cast_to_raw"), Some(Intrinsic::PtrCastToRaw));
        assert_eq\!(resolve_intrinsic("unknown_intrinsic"), None);
    }

    #[test]
    fn test_clif_type_of_primitives_and_user() {
        // primitives
        assert_eq\!(clif_type_of(&HirType::I8),   SsaType::I8);
        assert_eq\!(clif_type_of(&HirType::I16),  SsaType::I16);
        assert_eq\!(clif_type_of(&HirType::I32),  SsaType::I32);
        assert_eq\!(clif_type_of(&HirType::I64),  SsaType::I64);
        assert_eq\!(clif_type_of(&HirType::U8),   SsaType::U8);
        assert_eq\!(clif_type_of(&HirType::U16),  SsaType::U16);
        assert_eq\!(clif_type_of(&HirType::U32),  SsaType::U32);
        assert_eq\!(clif_type_of(&HirType::U64),  SsaType::U64);
        assert_eq\!(clif_type_of(&HirType::F32),  SsaType::F32);
        assert_eq\!(clif_type_of(&HirType::F64),  SsaType::F64);
        assert_eq\!(clif_type_of(&HirType::Boolean), SsaType::Bool);
        assert_eq\!(clif_type_of(&HirType::String),  SsaType::String);
        assert_eq\!(clif_type_of(&HirType::Void),    SsaType::Void);

        // user types (Class/Interface/Enum funnel to SsaType::User)
        let inner = HirType::I32;
        let user = HirType::Class("Vec".to_string(), vec\![inner.clone()]);
        let mapped = clif_type_of(&user);
        match mapped {
            SsaType::User(name, args) => {
                assert_eq\!(name, "Vec");
                assert_eq\!(args, vec\![SsaType::I32]);
            }
            other => panic\!("Expected SsaType::User, got {:?}", other),
        }

        let iface = HirType::Interface("Display".to_string(), vec\![]);
        let mapped_iface = clif_type_of(&iface);
        match mapped_iface {
            SsaType::User(name, args) => {
                assert_eq\!(name, "Display");
                assert\!(args.is_empty());
            }
            _ => panic\!("Expected User for interface"),
        }
    }

    #[test]
    fn test_round_up_and_tag_bytes_edges() {
        // round_up
        assert_eq\!(round_up(0, 1), 0);
        assert_eq\!(round_up(1, 1), 1);
        assert_eq\!(round_up(1, 2), 2);
        assert_eq\!(round_up(15, 8), 16);
        assert_eq\!(round_up(16, 8), 16);
        assert_eq\!(round_up(17, 8), 24);

        // tag_bytes thresholds
        assert_eq\!(tag_bytes(0), 1);
        assert_eq\!(tag_bytes(1), 1);
        assert_eq\!(tag_bytes(0x100), 1);
        assert_eq\!(tag_bytes(0x101), 2);
        assert_eq\!(tag_bytes(0x1_0000), 2);
        assert_eq\!(tag_bytes(0x1_0001), 4);
        assert_eq\!(tag_bytes(0x1_0000_0000), 4);
        assert_eq\!(tag_bytes(0x1_0000_0001), 8);
    }

    #[test]
    fn test_layout_of_scalars_and_structs() {
        let tgt = TargetInfo { ptr_bytes: 8 };

        // Scalars
        assert_eq\!(layout_of(&SsaType::Void, &tgt).unwrap(), Layout { size: 0, align: 1 });
        assert_eq\!(layout_of(&SsaType::Bool, &tgt).unwrap(), Layout { size: 1, align: 1 });
        assert_eq\!(layout_of(&SsaType::I8, &tgt).unwrap(),   Layout { size: 1, align: 1 });
        assert_eq\!(layout_of(&SsaType::U16, &tgt).unwrap(),  Layout { size: 2, align: 2 });
        assert_eq\!(layout_of(&SsaType::I32, &tgt).unwrap(),  Layout { size: 4, align: 4 });
        assert_eq\!(layout_of(&SsaType::U64, &tgt).unwrap(),  Layout { size: 8, align: 8 });
        assert_eq\!(layout_of(&SsaType::F64, &tgt).unwrap(),  Layout { size: 8, align: 8 });
        assert_eq\!(layout_of(&SsaType::I128, &tgt).unwrap(), Layout { size: 16, align: 16 });
        assert_eq\!(layout_of(&SsaType::String, &tgt).unwrap(), Layout { size: 16, align: 16 });

        // Unsized
        let err = layout_of(&SsaType::Slice, &tgt).unwrap_err();
        matches\!(err, LayoutError::Unsized(_));

        // Tuple with padding and final alignment
        // Tuple(I8, I32, U16) => offsets: I8@0, pad to 4 -> I32@4, then pad to 2 -> U16@8; end pad to max_align(4) => size 12 rounded to 12
        let tup = SsaType::Tuple(vec\![SsaType::I8, SsaType::I32, SsaType::U16]);
        let l = layout_of(&tup, &tgt).unwrap();
        assert_eq\!(l.align, 4);
        assert_eq\!(l.size, 12);

        // Nested User type reuses same tuple rule
        let user = SsaType::User("MyStruct".into(), vec\![SsaType::U64, SsaType::I8]);
        let l2 = layout_of(&user, &tgt).unwrap();
        // U64@0, align=8; I8@8, align stays 8; size rounds up to 16
        assert_eq\!(l2.align, 8);
        assert_eq\!(l2.size, 16);
    }

    #[test]
    fn test_layout_of_enums_tagged_union() {
        let tgt = TargetInfo { ptr_bytes: 8 };

        // Empty enum => size 0 align 1
        let empty = SsaType::Enum(vec\![]);
        assert_eq\!(layout_of(&empty, &tgt).unwrap(), Layout { size: 0, align: 1 });

        // Two variants: (I8) or (U64)
        let e = SsaType::Enum(vec\![SsaType::I8, SsaType::U64]);
        let l = layout_of(&e, &tgt).unwrap();
        // Union size rounds up largest variant (8) to its align(8)=8; tag is 1 byte; total sizes = 8 + 1 rounded to max align (8) => should be 16
        assert_eq\!(l.align, 8);
        assert_eq\!(l.size, 16);
    }

    #[test]
    fn test_sizeof_delegates_to_layout() {
        let tgt = TargetInfo { ptr_bytes: 8 };
        assert_eq\!(sizeof(&SsaType::U32, &tgt).unwrap(), 4);
        assert_eq\!(sizeof(&SsaType::Tuple(vec\![SsaType::U32, SsaType::U32])), &tgt).unwrap(), 8);
    }

    #[test]
    fn test_codegen_intrinsic_ptraddr_and_fromaddr_and_add_types() {
        // Build a simple function builder
        use cranelift_frontend::FunctionBuilder;
        use cranelift_frontend::FunctionBuilderContext;
        use cranelift_codegen::ir::{Function as ClifFunction, Signature};

        let mut func = ClifFunction::with_name_signature(
            cranelift_codegen::ir::ExternalName::user(0, 1),
            Signature::new(cranelift_codegen::isa::CallConv::SystemV),
        );
        let mut fb_ctx = FunctionBuilderContext::new();
        let mut builder = FunctionBuilder::new(&mut func, &mut fb_ctx);
        let block = builder.create_block();
        builder.switch_to_block(block);
        builder.seal_block(block);

        // Assume 64-bit ptr in IR for testing types
        let ptr_ty = types::I64;

        // Prepare args
        let p = builder.ins().iconst(ptr_ty, 0x1000);
        let b = builder.ins().iconst(types::I64, 16);

        // PtrAdd => pointer-typed result
        let res_add = super::codegen_intrinsic(Intrinsic::PtrAdd, &[p, b], &[], &mut builder, &fake_isa::FakeIsa::new());
        let res_add = res_add.expect("PtrAdd should return a value");
        let ty_add = builder.func.dfg.value_type(res_add);
        assert_eq\!(ty_add, ptr_ty);

        // PtrFromAddr => pointer-typed result
        let from = super::codegen_intrinsic(Intrinsic::PtrFromAddr, &[p], &[], &mut builder, &fake_isa::FakeIsa::new());
        let from = from.expect("PtrFromAddr should return a value");
        let ty_from = builder.func.dfg.value_type(from);
        assert_eq\!(ty_from, ptr_ty);

        // PtrAddr => integer-typed (usize) result
        let addr = super::codegen_intrinsic(Intrinsic::PtrAddr, &[p], &[], &mut builder, &fake_isa::FakeIsa::new());
        let addr = addr.expect("PtrAddr should return a value");
        let ty_addr = builder.func.dfg.value_type(addr);
        assert_eq\!(ty_addr, ptr_ty); // on 64-bit, usize == I64 in this IR setting

        builder.finalize();
    }

    // A very small fake ISA to satisfy &dyn TargetIsa requirements for tests that only need pointer_type()
    // Without pulling cranelift-native as a dev-dependency.
    mod fake_isa {
        use cranelift_codegen::isa::{TargetIsa, CallConv};
        use cranelift_codegen::settings::{self, Flags};
        use cranelift_codegen::ir::types;
        use target_lexicon::Triple;
        use std::fmt::Debug;

        pub struct FakeIsa {
            flags: Flags,
        }

        impl FakeIsa {
            pub fn new() -> Self {
                let builder = settings::builder();
                let flags = Flags::new(builder);
                Self { flags }
            }
        }

        impl TargetIsa for FakeIsa {
            fn name(&self) -> &'static str { "fake" }
            fn triple(&self) -> &Triple {
                // Use a static triple; details don't matter for pointer_type width in these tests
                static T: once_cell::sync::Lazy<Triple> = once_cell::sync::Lazy::new(|| "x86_64-unknown-unknown".parse().unwrap());
                &T
            }
            fn flags(&self) -> &Flags { &self.flags }
            fn pointer_type(&self) -> cranelift_codegen::ir::Type { types::I64 }
            fn default_call_conv(&self) -> CallConv { CallConv::SystemV }
            // Provide minimal required methods with unimplemented\!() if not used by tests
        }
    }
}