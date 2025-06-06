use std::collections::HashMap;
use crate::ast;
use crate::ast::Param;
use crate::codegen::cranelift::compiler::FieldLayout;

pub fn compute_field_offsets(params: &Option<Vec<Param>>) -> FieldLayout {
    let mut offsets = HashMap::new();
    let mut param_names = Vec::new();
    let mut offset: i32 = 0;

    if let Some(params) = params {
        for param in params {
            offsets.insert(param.name.clone(), offset);
            param_names.push(param.name.clone());
            offset += 8; // TODO: not all offsets are 8 bytes
        }
    }

    FieldLayout {
        offsets,
        param_names,
        total_size: offset,
    }
}

#[inline]
pub fn type_size(ty: &ast::Type) -> u8 {
    match ty {
        ast::Type::F64 => 8,
        ast::Type::F32 => 4,
        ast::Type::I32 => 4,
        ast::Type::I64 => 8,
        ast::Type::U32 => 4,
        ast::Type::UF64 => 8,
        ast::Type::I128 => 16,
        ast::Type::U128 => 16,
        ast::Type::U64 => 8,
        ast::Type::UF32 => 4,
        ast::Type::Void => 1,
        ast::Type::Boolean => 1,
        ast::Type::Class(_) => 8,
        ast::Type::Array(_, _) => 8,
        _ => panic!("Unexpected type in type_size: {:?}", ty),
    }
}