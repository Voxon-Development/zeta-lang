use ir::hir::{AssignmentOperator, HirType, Operator};
use ir::ssa_ir::{BinOp, SsaType};

pub fn assign_op_to_bin_op(op: AssignmentOperator) -> BinOp {
    let bin_op = match op {
        AssignmentOperator::AddAssign => BinOp::Add,
        AssignmentOperator::SubtractAssign => BinOp::Sub,
        AssignmentOperator::MultiplyAssign => BinOp::Mul,
        AssignmentOperator::DivideAssign => BinOp::Div,
        AssignmentOperator::ModuloAssign => BinOp::Mod,
        AssignmentOperator::BitAndAssign => BinOp::BitAnd,
        AssignmentOperator::BitOrAssign => BinOp::BitOr,
        AssignmentOperator::BitXorAssign => BinOp::BitXor,
        AssignmentOperator::ShiftLeftAssign => BinOp::ShiftLeft,
        AssignmentOperator::ShiftRightAssign => BinOp::ShiftRight,
        _ => unreachable!(),
    };
    bin_op
}

pub fn lower_type_hir(ty: &HirType) -> SsaType {
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
        HirType::Struct(name, args)
        | HirType::Interface(name, args)
        | HirType::Enum(name, args) =>
            SsaType::User(*name, args.iter().map(lower_type_hir).collect()),
        HirType::Void => SsaType::Void,
        HirType::Pointer(inner) => SsaType::Pointer(Box::new(lower_type_hir(inner))),
        HirType::Lambda { params, return_type, .. } => {
            let param_types: Vec<SsaType> = params.iter().map(lower_type_hir).collect();
            let ret_type = lower_type_hir(return_type);
            // Represent lambda as a function pointer type (Dyn for now, could be improved)
            SsaType::Dyn
        }
        HirType::Generic(name) => {
            // Generics are typically resolved at monomorphization time
            // For now, represent as a user type with the generic name
            SsaType::User(*name, vec![])
        }
        HirType::This => {
            // This should have been replaced with the actual class type before lowering
            // If we get here, treat it as a generic user type
            SsaType::Dyn
        }
        HirType::Null => {
            // Null type - represent as void for now (could be a special pointer type)
            SsaType::Void
        }
    }
}

pub(super) fn lower_operator_bin(operator: &Operator) -> BinOp {
    match operator {
        Operator::Add => BinOp::Add,
        Operator::Subtract => BinOp::Sub,
        Operator::Multiply => BinOp::Mul,
        Operator::Divide => BinOp::Div,
        Operator::Modulo => BinOp::Mod,
        Operator::Equals => BinOp::Eq,
        Operator::NotEquals => BinOp::Ne,
        Operator::LessThan => BinOp::Lt,
        Operator::LessThanOrEqual => BinOp::Le,
        Operator::GreaterThan => BinOp::Gt,
        Operator::GreaterThanOrEqual => BinOp::Ge,
        _ => unimplemented!("Operator {:?} not yet lowered", operator),
    }
}