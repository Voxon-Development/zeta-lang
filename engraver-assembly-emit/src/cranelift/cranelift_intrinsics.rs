use std::io::Read;
use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::{types, InstBuilder, MemFlags, Signature, Type, Value};
use cranelift_codegen::isa::TargetIsa;
use cranelift_frontend::FunctionBuilder;
use ir::hir::HirType;

#[derive(Debug, Clone, Copy)]
pub enum Intrinsic {
    PtrNull,
    PtrIsNull,
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
        "__ptr_null" => Some(PtrNull),
        "__ptr_is_null" => Some(PtrIsNull),
        "__ptr_add" => Some(PtrAdd),
        "__ptr_deref" => Some(PtrDeref),
        "__ptr_store" => Some(PtrStore),
        "__ptr_from_addr" => Some(PtrFromAddr),
        "__ptr_addr" => Some(PtrAddr),
        "__ptr_cast_to_raw" => Some(PtrCastToRaw),
        _ => None,
    }
}

/// Helper: try to extract the inner T from `Ptr<T>` HirType.
/// Adjust to match how your HirType encodes generics.
pub fn as_ptr_inner(hir_ty: &HirType) -> Option<&HirType> {
    if let HirType::Class(name, args) = hir_ty {
        if name == "Ptr" {
            return args.get(0);
        }
    }
    None
}

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

pub enum ArrayIntrinsic {
    Len,
    Ptr,
    Get,
    Set,
}

pub fn resolve_array_intrinsic(name: &str) -> Option<ArrayIntrinsic> {
    match name {
        "__array_len" => Some(ArrayIntrinsic::Len),
        "__array_ptr" => Some(ArrayIntrinsic::Ptr),
        "__array_get" => Some(ArrayIntrinsic::Get),
        "__array_set" => Some(ArrayIntrinsic::Set),
        _ => None,
    }
}

pub fn codegen_array_get(
    arg_arr: LoweredValue,
    arg_index: Value,
    result_hir_ty: &HirType,
    builder: &mut FunctionBuilder,
    isa: &dyn TargetIsa,
) -> Value {
    // Resolve base pointer
    let base_ptr = match arg_arr {
        LoweredValue::Pair(p, _len) => p,
        LoweredValue::Single(v) => v,
    };
    let base_ptr = ensure_val_ty(builder, base_ptr, ptr_clif_ty(isa));

    // index should be usize compatible
    let idx = ensure_val_ty(builder, arg_index, usize_clif_ty(isa));

    // element CLIF type
    let elem_clif = clif_type_of(result_hir_ty);
    vec![]

    // compute byte offset = idx * sizeof(elem)
    let size_bytes = elem_clif.bytes() as i64;
    let idx_scaled = if size_bytes == 1 {
        idx
    } else {
        // idx * size_bytes
        builder.ins().imul_imm(idx, size_bytes)
    };

    // ptr + offset
    let addr = builder.ins().iadd(base_ptr, idx_scaled);

    // load element
    let loaded = builder.ins().load(elem_clif, MemFlags::new(), addr, 0);
    loaded
}

use cranelift::prelude::AbiParam;
use ir::ssa_ir::SsaType;

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
        HirType::Class(name, _) if name == "Ptr" => {
            sig.params.push(AbiParam::new(ptr_clif_ty(isa)));
        }
        // primitives
        _ => {
            let clif_t = clif_type_of(hir_ty, isa);
            sig.params.push(AbiParam::new(clif_t));
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
            sig.returns.push(AbiParam::new(clif_type_of(hir_ty, isa)));
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
        HirType::Class(name, _) | HirType::Interface(name, _) | HirType::Enum(name, _) => SsaType::User(name.clone()),
        HirType::Void => SsaType::Void,
        _ => unimplemented!("Unsupported type {:?}", ty),
    }
}

// __array_set(arr, index, value)
pub fn codegen_array_set(
    arg_arr: LoweredValue,
    arg_index: Value,
    value: Value,
    builder: &mut FunctionBuilder,
    isa: &dyn TargetIsa,
) {
    let base_ptr = match arg_arr {
        LoweredValue::Pair(p, _len) => p,
        LoweredValue::Single(v) => v,
    };
    let base_ptr = ensure_val_ty(builder, base_ptr, ptr_clif_ty(isa));
    let idx = ensure_val_ty(builder, arg_index, usize_clif_ty(isa));

    let val_ty = builder.func.dfg.value_type(value);
    let size_bytes = val_ty.bytes() as i64;
    let idx_scaled = if size_bytes == 1 {
        idx
    } else {
        builder.ins().imul_imm(idx, size_bytes)
    };

    let addr = builder.ins().iadd(base_ptr, idx_scaled);
    builder.ins().store(MemFlags::new(), value, addr, 0);
}

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
        Intrinsic::PtrNull => {
            // return a null pointer
            let zero = builder.ins().iconst(ptr_ty, 0);
            Some(zero)
        }

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

        Intrinsic::PtrIsNull => {
            // args: [ptr]
            let p = args[0];
            let p_ty = builder.func.dfg.value_type(p);
            let p = ensure_value_is_ptr_width(builder, p, ptr_ty, p_ty);
            let zero = builder.ins().iconst(ptr_ty, 0);
            let cmp = builder.ins().icmp(IntCC::Equal, p, zero);
            // cmp is an integer boolean (I8). If your boolean type differs, convert here.
            Some(cmp)
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
            let clif_ret = clif_type_of(ret_hir, isa);

            // Use default MemFlags. Tune if you need volatile/readonly.
            let mem_flags = MemFlags::new();

            // Cranelift `load` expects the base to be pointer typed. Good.
            let loaded = builder.ins().load(clif_ret, mem_flags, p, 0);
            Some(loaded)
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