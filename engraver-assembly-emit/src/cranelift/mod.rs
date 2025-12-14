use cranelift::prelude::types;
use ir::ssa_ir::SsaType;
use cranelift_codegen::ir::Type;

pub mod cranelift_backend;
mod cranelift_intrinsics;

fn clif_type(ty: &SsaType) -> Type {
    match ty {
        SsaType::I8 => types::I8,
        SsaType::U8 => types::I8,  // Cranelift uses i8 for both signed and unsigned
        SsaType::I16 => types::I16,
        SsaType::U16 => types::I16,
        SsaType::I32 => types::I32,
        SsaType::U32 => types::I32,
        SsaType::I64 => types::I64,
        SsaType::U64 => types::I64,
        SsaType::I128 => types::I128,
        SsaType::U128 => types::I128,
        SsaType::ISize => types::I64,  // Assuming 64-bit
        SsaType::USize => types::I64,  // Assuming 64-bit
        SsaType::F32 => types::F32,
        SsaType::F64 => types::F64,
        SsaType::Bool => types::I8,    // Using i8 for bool
        SsaType::String => types::I64, // String is a fat pointer
        SsaType::Void => types::I8,    // Using i8 as placeholder for void
        SsaType::User(_, _) => types::I64, // User types passed by reference
        SsaType::Enum(_) => types::I64,    // Tagged union, passed by reference
        SsaType::Tuple(_) => types::I64,   // Tuples passed by reference
        SsaType::Pointer(_) => types::I64, // Pointers are 64-bit
        SsaType::Dyn => types::I64,    // Trait object (fat pointer)
        SsaType::Slice => types::I64,  // Slice (fat pointer)
    }
}