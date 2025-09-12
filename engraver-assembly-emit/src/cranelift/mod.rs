use cranelift::prelude::types;
use ir::ssa_ir::SsaType;
use cranelift_codegen::ir::Type;

pub mod cranelift_backend;
mod cranelift_intrinsics;

pub fn clif_type(param: &SsaType) -> Type {
    match param {
        SsaType::I8 | SsaType::Bool => types::I8,
        SsaType::I16 => types::I16,
        SsaType::I32 => types::I32,
        SsaType::I64 => types::I64,
        SsaType::U8 => types::I8,
        SsaType::U16 => types::I16,
        SsaType::U32 => types::I32,
        SsaType::U64 => types::I64,
        SsaType::I128 => types::I128,
        SsaType::F32 => types::F32,
        SsaType::F64 => types::F64,
        SsaType::Void => types::I8,
        _ => types::I64
    }
}