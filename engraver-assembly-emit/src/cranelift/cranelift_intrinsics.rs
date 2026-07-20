use cranelift_codegen::ir::{InstBuilder, StackSlotData, StackSlotKind, Value};
use cranelift_frontend::FunctionBuilder;
use cranelift_module::Module;

pub fn stack_alloc(builder: &mut FunctionBuilder, module: &dyn Module, size_bytes: usize) -> Value {
    assert!(size_bytes > 0, "stack_alloc: size must be > 0");

    let isa = module.isa();
    let ptr_ty = isa.pointer_type();
    let aligned_size = round_to_eight_align(size_bytes);
    let slot = builder.create_sized_stack_slot(StackSlotData::new(
        StackSlotKind::ExplicitSlot,
        aligned_size as u32,
        0,
    ));

    builder.ins().stack_addr(ptr_ty, slot, 0)
}

#[inline(always)]
const fn round_to_eight_align(size_bytes: usize) -> usize {
    ((size_bytes + 7) / 8) * 8
}
