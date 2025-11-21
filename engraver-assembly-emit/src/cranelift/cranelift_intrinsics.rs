use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::{types, InstBuilder, MemFlags, Signature, StackSlotData, StackSlotKind, Type, Value, Function as ClFunction};
use cranelift_codegen::isa::TargetIsa;
use cranelift_frontend::FunctionBuilder;
use ir::hir::HirType;

#[derive(Debug, Clone, Copy)]
pub enum Intrinsic {
    PtrAdd,
    PtrDeref,
    PtrStore,
    PtrFromAddr,
    PtrAddr,
    PtrCastToRaw,
    StackAlloc,
    StackAllocZeroed,
    SizeOf,
    AlignOf,
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
        "stack_alloc" => Some(StackAlloc),
        "stack_alloc_zeroed" => Some(StackAllocZeroed),
        "sizeof" => Some(SizeOf),
        "alignof" => Some(AlignOf),
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
use cranelift_module::{Linkage, Module};
use ir::ssa_ir::{Function, SsaType};

/*fn push_param_for_hir_type(sig: &mut Signature, hir_ty: &HirType, isa: &dyn TargetIsa, string_pool: &StringPool) {
    match hir_ty {
        HirType::Class(name, args) if string_pool.resolve_string(name) == "Array" && args.len() == 1 => {
            let pt = AbiParam::new(ptr_clif_ty(isa));
            let len = AbiParam::new(usize_clif_ty(isa));
            sig.params.push(pt);
            sig.params.push(len);
        }
        HirType::Class(name, _args) if string_pool.resolve_string(name) == "Ptr" => {
            sig.params.push(AbiParam::new(ptr_clif_ty(isa)));
        }
        _ => {
            let clif_t = clif_type_of(hir_ty);
            sig.params.push(AbiParam::new(super::clif_type(&clif_t)));
        }
    }
}*/

// For returns, same idea: push one or two return types depending on Array<T>.
/*fn push_return_for_hir_type(sig: &mut Signature, hir_ty: &HirType, isa: &dyn TargetIsa, string_pool: &StringPool) {
    match hir_ty {
        HirType::Class(name, args) if string_pool.resolve_string(name) == "Array" && args.len() == 1 => {
            sig.returns.push(AbiParam::new(ptr_clif_ty(isa)));
            sig.returns.push(AbiParam::new(usize_clif_ty(isa)));
        }
        HirType::Class(name, _) if string_pool.resolve_string(name) == "Ptr" => {
            sig.returns.push(AbiParam::new(ptr_clif_ty(isa)));
        }
        _ => {
            sig.returns.push(AbiParam::new(super::clif_type(&clif_type_of(hir_ty))));
        }
    }
}*/

pub fn stack_alloc(
    builder: &mut FunctionBuilder,
    module: &impl Module,
    size_bytes: usize,
) -> Value {
    assert!(size_bytes > 0, "stack_alloc: size must be > 0");

    let isa = module.isa();
    let ptr_ty = isa.pointer_type();
    let aligned_size = round_to_eight_align(size_bytes);
    let slot = builder.create_sized_stack_slot(StackSlotData::new(
        StackSlotKind::ExplicitSlot,
        aligned_size as u32,
        0
    ));

    builder.ins().stack_addr(ptr_ty, slot, 0)
}

#[inline(always)]
const fn round_to_eight_align(size_bytes: usize) -> usize {
    ((size_bytes + 7) / 8) * 8
}

/*fn ptr_clif_ty(isa: &dyn TargetIsa) -> Type {
    isa.pointer_type()
}

fn usize_clif_ty(isa: &dyn TargetIsa) -> Type {
    ptr_clif_ty(isa)
}*/

/*
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
}*/
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

fn lower_syscall(
    builder: &mut FunctionBuilder, 
    call: &mut ClFunction,
    ssa_call: &mut Function,
    module: &mut impl Module
) -> Value {
    let (num_val, arg_vals) = extract_args(ssa_call);
    let sig = runtime_syscall_signature(arg_vals.len());
    let name = &format!("syscall{}", arg_vals.len());

    let callee = module
        .declare_function(name, Linkage::Import, &sig)
        .map(|id| module.declare_func_in_func(id, call)).unwrap();

    let mut operands = Vec::new();
    operands.push(num_val);
    operands.extend(arg_vals);

    let call_inst = builder.ins().call(callee, &operands);

    let results = builder.inst_results(call_inst);
    results[0]
}

fn runtime_syscall_signature(_args_len: usize) -> Signature {
    todo!()
}

fn extract_args(_function: &Function) -> (Value, Vec<Value>) {
    todo!()
}

#[derive(Clone, Copy, Debug)]
pub struct Layout { pub size: usize, pub align: usize }

#[derive(Debug)]
pub enum LayoutError {
    Unsized(&'static str),
    Recursive,
    Unknown,
}

#[derive(Clone, Copy)]
pub struct TargetInfo { pub ptr_bytes: u64 }

pub fn sizeof(ty: &SsaType, target: &TargetInfo) -> Result<usize, LayoutError> {
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
            let mut off = 0usize;
            let mut max_align = 1usize;
            for fty in fields {
                let f = layout_of(fty, target)?;
                max_align = max_align.max(f.align);
                off = round_up(off, f.align);
                off = off.checked_add(f.size).ok_or(LayoutError::Unknown)?;
            }
            let size = round_up(off, max_align);
            Ok(Layout { size, align: max_align })
        }

        // Enums/sum types: Simple tagged union:
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

#[inline(always)]
const fn round_up(x: usize, align: usize) -> usize {
    if align <= 1 { return x; }
    let m = align - 1;
    (x + m) & !m
}

fn tag_bytes(variants: usize) -> usize {
    match variants {
        0..=0x100      => 1,
        0x101..=0x1_0000 => 2,
        0x1_0001..=0x1_0000_0000 => 4,
        _ => 8,
    }
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
    func_name: Option<&str>,
) -> Option<Value> {
    let ptr_ty = isa.pointer_type();
    match intr {

        Intrinsic::StackAlloc | Intrinsic::StackAllocZeroed => {
            // Parse const generic N from monomorphized function name suffix, e.g., "..._N10_..."
            let mut n: i64 = 1;
            if let Some(fname) = func_name {
                if let Some(pos) = fname.rfind("_N") {
                    let digits = &fname[pos + 2..];
                    let mut end = digits.len();
                    for (i, ch) in digits.char_indices() {
                        if !ch.is_ascii_digit() { end = i; break; }
                    }
                    if end > 0 {
                        if let Ok(parsed) = digits[..end].parse::<i64>() {
                            n = parsed.max(1);
                        }
                    }
                }
            }
            let elem_size: i64 = 8; // TODO: compute sizeof(T) precisely once type info is threaded through
            let total_size = n.saturating_mul(elem_size);

            use cranelift_codegen::ir::{StackSlotData, StackSlotKind};
            let slot = builder.create_sized_stack_slot(StackSlotData::new(StackSlotKind::ExplicitSlot, total_size as u32, 3));
            let ptr_ty = isa.pointer_type();
            let addr = builder.ins().stack_addr(ptr_ty, slot, 0);

            if let Intrinsic::StackAllocZeroed = intr {
                // best-effort: zero first word
                let zero = builder.ins().iconst(types::I64, 0);
                let flags = MemFlags::new();
                builder.ins().store(flags, zero, addr, 0);
            }

            Some(addr)
        }

        Intrinsic::SizeOf => {
            // Fallback: return pointer width in bytes (until full type layout is wired here)
            let bytes = (ptr_ty.bits() / 8) as i64;
            let c = builder.ins().iconst(ptr_ty, bytes as i64);
            Some(c)
        }

        Intrinsic::AlignOf => {
            // Fallback: return pointer alignment in bytes (same as width on most targets)
            let bytes = (ptr_ty.bits() / 8) as i64;
            let c = builder.ins().iconst(ptr_ty, bytes as i64);
            Some(c)
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