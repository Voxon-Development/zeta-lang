use ir::ast;
use ir::hir::{self, HirType, Operator, StrId};
use smallvec::SmallVec;
use std::sync::Arc;
use zetaruntime::string_pool::StringPool;

pub const fn lower_visibility(visibility: &ast::Visibility) -> hir::Visibility {
    match visibility {
        ast::Visibility::Public => hir::Visibility::Public,
        ast::Visibility::Private => hir::Visibility::Private,
        ast::Visibility::Module => hir::Visibility::Module,
        ast::Visibility::Internal => hir::Visibility::Internal,
    }
}

pub const fn lower_cmp_operator(op: ast::Op) -> Operator {
    match op {
        ast::Op::Eq => Operator::Equals,
        ast::Op::Neq => Operator::NotEquals,
        ast::Op::Lt => Operator::LessThan,
        ast::Op::Lte => Operator::LessThanOrEqual,
        ast::Op::Gt => Operator::GreaterThan,
        ast::Op::Gte => Operator::GreaterThanOrEqual,
        _ => unreachable!(),
    }
}

#[allow(unused)] // TODO: probably finish, lol
pub fn type_suffix_with_pool(pool: Arc<StringPool>, ty: &HirType) -> StrId {
    StrId(match ty {
        HirType::I32 => pool.intern("i32"),
        HirType::I64 => pool.intern("i64"),
        HirType::U32 => pool.intern("u32"),
        HirType::U64 => pool.intern("u64"),
        HirType::F32 => pool.intern("f32"),
        HirType::F64 => pool.intern("f64"),
        HirType::String => pool.intern("str"),
        HirType::Boolean => pool.intern("boolean"),

        HirType::Struct {
            name, type_args, ..
        } => {
            if type_args.is_empty() {
                // name is already the fully-resolved/mangled identity of this struct
                **name
            } else {
                // only real generic params recurse
                let mut buf: SmallVec<u8, 64> = SmallVec::new();
                buf.extend_from_slice(pool.resolve_bytes(&*name));
                for arg in type_args.iter() {
                    buf.push(b'_');
                    let suf = type_suffix_with_pool(pool.clone(), arg);
                    buf.extend_from_slice(pool.resolve_bytes(&*suf));
                }
                let s = std::str::from_utf8(&buf).expect("valid utf8");
                pool.intern(s)
            }
        }

        HirType::Generic(name) => unreachable!(),

        HirType::Void => pool.intern("void"),
        HirType::I8 => pool.intern("i8"),
        HirType::I16 => pool.intern("i16"),
        HirType::U8 => pool.intern("u8"),
        HirType::U16 => pool.intern("u16"),
        HirType::I128 => pool.intern("i128"),
        HirType::U128 => pool.intern("u128"),
        HirType::SafePointer(hir_type) => todo!(),
        HirType::Ref {
            inner,
            mutability_state,
            provenance,
        } => todo!(),
        HirType::UnsafePointer(hir_type) => todo!(),
        HirType::OwnedPointer(hir_type) => todo!(),
        HirType::Lambda {
            params,
            return_type,
        } => todo!(),
        HirType::This => todo!(),
        HirType::Null => todo!(),
        HirType::Char => pool.intern("char"),
        HirType::Unknown => todo!(),
        HirType::Nullable(hir_type) => todo!(),
        HirType::Dyn { bounds } => todo!(),
        HirType::Tuple(hir_types) => todo!(),
        HirType::Array(hir_type, _) => todo!(),
        HirType::Slice(hir_type) => todo!(),
        HirType::Usize => pool.intern("usize"),
        HirType::Isize => pool.intern("isize"),
        HirType::DynInterface(str_id, hir_types) => todo!(),
        HirType::Enum(str_id, hir_types) => todo!(),
    })
}
