use ir::layout::{layout_of_ssa, sizeof_ssa};
use cranelift_codegen::ir::condcodes::{FloatCC, IntCC};
use cranelift_codegen::ir::{types, InstBuilder, MemFlags, Signature, StackSlotData, StackSlotKind, Type, Value, Function as ClFunction};
use cranelift_codegen::isa::TargetIsa;
use cranelift_frontend::{FuncInstBuilder, FunctionBuilder};
use ir::hir::HirType;
use ir::ssa_ir::SsaType;
use ir::layout::TargetInfo;

/// Represents an intrinsic operation that can be lowered to Cranelift IR
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Intrinsic {
    // Pointer operations
    PtrAdd,          // ptr + offset -> ptr
    PtrDeref,        // *ptr -> T
    PtrStore,        // *ptr = value
    PtrFromAddr,     // addr as *const T
    PtrAddr,         // ptr as usize
    PtrCastToRaw,    // &T as *const T
    PtrEq,           // ptr1 == ptr2 -> bool
    PtrNe,           // ptr1 != ptr2 -> bool
    PtrLt,           // ptr1 < ptr2 -> bool
    PtrLe,           // ptr1 <= ptr2 -> bool
    PtrGt,           // ptr1 > ptr2 -> bool
    PtrGe,           // ptr1 >= ptr2 -> bool
    
    // Memory operations
    StackAlloc,      // Allocate stack memory
    StackAllocZeroed,// Allocate and zero-initialize stack memory
    MemCopy,         // Copy memory regions
    MemSet,          // Set memory to a value
    
    // Type information
    SizeOf,          // size_of::<T>()
    AlignOf,         // align_of::<T>()
    TypeId,          // Get type ID at runtime
    
    // Atomic operations
    AtomicLoad,      // Atomic load
    AtomicStore,     // Atomic store
    AtomicRmw,       // Atomic read-modify-write
    AtomicCmpXchg,   // Atomic compare-and-swap
}

/// Resolves a function name to its corresponding intrinsic operation
/// 
/// # Arguments
/// * `name` - The name of the intrinsic to resolve
/// 
/// # Returns
/// `Some(Intrinsic)` if the name matches a known intrinsic, `None` otherwise
pub fn resolve_intrinsic(name: &str) -> Option<Intrinsic> {
    use Intrinsic::*;
    
    match name {
        // Pointer operations
        "ptr_add" | "__ptr_add" => Some(PtrAdd),
        "ptr_deref" | "__ptr_deref" => Some(PtrDeref),
        "ptr_store" | "__ptr_store" => Some(PtrStore),
        "ptr_from_addr" | "__ptr_from_addr" => Some(PtrFromAddr),
        "ptr_addr" | "__ptr_addr" => Some(PtrAddr),
        "ptr_cast_to_raw" | "__ptr_cast_to_raw" => Some(PtrCastToRaw),
        "ptr_eq" | "__ptr_eq" => Some(PtrEq),
        "ptr_ne" | "__ptr_ne" => Some(PtrNe),
        "ptr_lt" | "__ptr_lt" => Some(PtrLt),
        "ptr_le" | "__ptr_le" => Some(PtrLe),
        "ptr_gt" | "__ptr_gt" => Some(PtrGt),
        "ptr_ge" | "__ptr_ge" => Some(PtrGe),
        
        // Memory operations
        "stack_alloc" | "__stack_alloc" => Some(StackAlloc),
        "stack_alloc_zeroed" | "__stack_alloc_zeroed" => Some(StackAllocZeroed),
        "mem_copy" | "__mem_copy" => Some(MemCopy),
        "mem_set" | "__mem_set" => Some(MemSet),
        
        // Type information
        "size_of" | "__size_of" | "core::mem::size_of" => Some(SizeOf),
        "align_of" | "__align_of" | "core::mem::align_of" => Some(AlignOf),
        "type_id" | "__type_id" => Some(TypeId),
        
        // Atomic operations
        "atomic_load" | "__atomic_load" => Some(AtomicLoad),
        "atomic_store" | "__atomic_store" => Some(AtomicStore),
        "atomic_rmw" | "__atomic_rmw" => Some(AtomicRmw),
        "atomic_cmpxchg" | "__atomic_cmpxchg" => Some(AtomicCmpXchg),
        
        _ => None,
    }
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
        builder.ins().uextend(ptr_ty, val)
    } else if builder.func.dfg.value_type(val).bits() > ptr_ty.bits() {
        builder.ins().ireduce(ptr_ty, val)
    } else {
        val
    }
}
use cranelift_module::{Linkage, Module};
use ir::ssa_ir::{Function};

pub fn stack_alloc(
    builder: &mut FunctionBuilder,
    module: &dyn Module,
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

    FuncInstBuilder::stack_addr(builder.ins(), ptr_ty, slot, 0)
}

#[inline(always)]
const fn round_to_eight_align(size_bytes: usize) -> usize {
    ((size_bytes + 7) / 8) * 8
}

fn ptr_clif_ty(isa: &dyn TargetIsa) -> Type {
    isa.pointer_type()
}

fn usize_clif_ty(isa: &dyn TargetIsa) -> Type {
    ptr_clif_ty(isa)
}

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
        HirType::Struct(name, args) | HirType::Interface(name, args) | HirType::Enum(name, args)
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

pub fn codegen_intrinsic(
    intr: Intrinsic,
    args: &[Value],
    ret_hir_types: &[HirType],
    builder: &mut FunctionBuilder,
    module: &dyn Module,
    func_name: Option<&str>,
    target_info: TargetInfo
) -> Option<Value> {
    let isa = module.isa();
    let ptr_ty = isa.pointer_type();
    
    let arg_ty = args.first().map(|&val| {
        let ty = builder.func.dfg.value_type(val);
        match ty {
            types::I8 => SsaType::I8,
            types::I16 => SsaType::I16,
            types::I32 => SsaType::I32,
            types::I64 => SsaType::I64,
            types::I128 => SsaType::I128,
            _ => SsaType::Void, // Default to void if type is not recognized
        }
    }).unwrap_or(SsaType::Void);
    
    match intr {
        Intrinsic::StackAlloc => {
            let elem_size = sizeof_ssa(&arg_ty, target_info).ok()?;
            Some(stack_alloc(builder, module, elem_size as usize))
        }

        Intrinsic::StackAllocZeroed => {
            stack_alloc_zeroed_intr(&arg_ty, builder, module, target_info)
        }

        Intrinsic::SizeOf => {
            layout_of_ssa(&arg_ty, target_info).ok().map(|l| builder.ins().iconst(ptr_ty, l.size as i64))
        }

        Intrinsic::AlignOf => {
            layout_of_ssa(&arg_ty, target_info).ok().map(|l| builder.ins().iconst(ptr_ty, l.align as i64))
        }

        Intrinsic::PtrCastToRaw => {
            match ptr_cast_to_raw(args, builder, module) {
                Ok(value) => value,
                Err(value) => return value,
            }
        }

        Intrinsic::PtrAdd => {
            ptr_add(args, builder, ptr_ty)
        }

        Intrinsic::PtrDeref => {
            ptr_deref(args, &ret_hir_types, builder, ptr_ty)
        }

        Intrinsic::PtrStore => {
            ptr_store(args, &ret_hir_types, builder, ptr_ty);
            None
        }

        Intrinsic::PtrFromAddr => {
            get_ptr_from_addr(args, builder, ptr_ty)
        }
        
        Intrinsic::PtrAddr => {
            get_ptr_addr(args, builder, module)
        },
        _ => todo!()
    }
}

fn stack_alloc_zeroed_intr(ty: &SsaType, builder: &mut FunctionBuilder, isa: &dyn Module, target_info: TargetInfo) -> Option<Value> {
    let elem_size: usize = sizeof_ssa(ty, target_info).ok()?;
    let addr: Value = stack_alloc(builder, isa, elem_size);

    let zero = builder.ins().iconst(types::I64, 0);
    let flags = MemFlags::new();
    builder.ins().store(flags, zero, addr, 0);

    Some(addr)
}

fn ptr_cast_to_raw(args: &[Value], builder: &mut FunctionBuilder, module: &dyn Module) -> Result<Option<Value>, Option<Value>> {
    let val = args[0];
    let val_ty = builder.func.dfg.value_type(val);
    let ptr_ty = module.isa().pointer_type();

    let nonzero_test = if val_ty == ptr_ty {
        let zero = builder.ins().iconst(ptr_ty, 0);
        builder.ins().icmp(IntCC::NotEqual, val, zero)
    } else if val_ty.is_int() {
        let normalized = ensure_value_is_ptr_width(builder, val, ptr_ty, val_ty);
        let zero = builder.ins().iconst(ptr_ty, 0);
        builder.ins().icmp(IntCC::NotEqual, normalized, zero)
    } else {
        let zero_bool = builder.ins().iconst(types::I8, 0); // boolean false
        return Err(Some(zero_bool));
    };

    Ok(Some(nonzero_test))
}

fn ptr_add(args: &[Value], builder: &mut FunctionBuilder, ptr_ty: Type) -> Option<Value> {
    let base_ptr = args[0];
    let offset = args[1];

    let base_ty = builder.func.dfg.value_type(base_ptr);
    let offset_ty = builder.func.dfg.value_type(offset);

    let base_ptr = ensure_value_is_ptr_width(builder, base_ptr, ptr_ty, base_ty);
    let offset = ensure_value_is_ptr_width(builder, offset, ptr_ty, offset_ty);

    let result = builder.ins().iadd(base_ptr, offset);
    
    #[cfg(debug_assertions)]
    {
        // TODO: Add bounds checking in debug mode
    }
    
    Some(result)
}

fn ptr_deref(args: &[Value], ret_hir_types: &&[HirType], builder: &mut FunctionBuilder, ptr_ty: Type) -> Option<Value> {
    let ptr = args[0];
    let ptr_val_ty = builder.func.dfg.value_type(ptr);
    
    let ptr_val = ensure_value_is_ptr_width(builder, ptr, ptr_ty, ptr_val_ty);
    
    let ret_hir = &ret_hir_types[0];
    let clif_ret = clif_type_of(&ret_hir);
    let load_ty = super::clif_type(&clif_ret);
    
    let load_ty = match clif_ret {
        SsaType::Pointer(_) => ptr_ty,
        _ => load_ty,
    };

    // Use default memory flags (non-volatile, not atomic)
    // TODO: Add support for volatile/atomic operations
    let mem_flags = MemFlags::new();
    
    let loaded = builder.ins().load(load_ty, mem_flags, ptr_val, 0);
    
    #[cfg(debug_assertions)]
    {
        // TODO: Add null pointer checking in debug mode
    }
    
    if let SsaType::Pointer(inner) = clif_ret {
        Some(loaded)
    } else {
        Some(loaded)
    }
}

fn ptr_store(args: &[Value], ret_hir_types: &&[HirType], builder: &mut FunctionBuilder, ptr_ty: Type) {
    let p = args[0];
    let mut val = args[1];

    let p_ty = builder.func.dfg.value_type(p);
    let p = ensure_value_is_ptr_width(builder, p, ptr_ty, p_ty);

    if !ret_hir_types.is_empty() {
        let val_ty = builder.func.dfg.value_type(val);
        let expected_ty = match ret_hir_types[0] {
            HirType::Pointer(_) => ptr_ty,
            _ => super::clif_type(&clif_type_of(&ret_hir_types[0])),
        };

        if val_ty != expected_ty {
            val = ensure_value_is_ptr_width(builder, val, expected_ty, val_ty);
        }
    }

    let mem_flags = MemFlags::new();
    builder.ins().store(mem_flags, val, p, 0);
}

fn get_ptr_addr(args: &[Value], builder: &mut FunctionBuilder, module: &dyn Module) -> Option<Value> {
    let ptr = args[0];
    let ptr_ty = builder.func.dfg.value_type(ptr);

    let ptr_val = ensure_value_is_ptr_width(builder, ptr, module.isa().pointer_type(), ptr_ty);

    Some(ptr_val)
}

fn get_ptr_from_addr(args: &[Value], builder: &mut FunctionBuilder, ptr_ty: Type) -> Option<Value> {
    let addr = args[0];
    let addr_ty = builder.func.dfg.value_type(addr);

    let ptr_val = ensure_value_is_ptr_width(builder, addr, ptr_ty, addr_ty);
    Some(ptr_val)
}