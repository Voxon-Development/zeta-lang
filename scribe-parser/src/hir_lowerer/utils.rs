use std::sync::Arc;
use ir::ast;
use ir::hir::{self, HirType, Operator, StrId};
use smallvec::SmallVec;
use zetaruntime::string_pool::StringPool;

pub const fn lower_visibility(visibility: &ast::Visibility) -> hir::Visibility {
    match visibility {
        ast::Visibility::Public => hir::Visibility::Public,
        ast::Visibility::Private => hir::Visibility::Private,
        ast::Visibility::Module => hir::Visibility::Module,
        ast::Visibility::Package => hir::Visibility::Package,
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

pub fn type_suffix_with_pool(pool: Arc<StringPool>, ty: &HirType) -> StrId {
    StrId(match ty {
        HirType::I32 => pool.intern("i32"),
        HirType::I64 => pool.intern("i64"),
        HirType::U32 => pool.intern("u32"),
        HirType::U64 => pool.intern("u64"),
        HirType::F32 => pool.intern("f32"),
        HirType::F64 => pool.intern("f64"),
        HirType::String => pool.intern("String"),
        HirType::Boolean => pool.intern("boolean"),

        HirType::Class(name, args) => {
            if args.is_empty() {
                **name
            } else {
                let mut buf: SmallVec<[u8; 128]> = SmallVec::new();
                buf.extend_from_slice(pool.resolve_bytes(&*name));
                for arg in *args {
                    buf.push(b'_');
                    let suffix = type_suffix_with_pool(pool.clone(), arg);
                    let part = pool.resolve_bytes(&*suffix);

                    buf.extend_from_slice(part);
                }
                let s = std::str::from_utf8(&buf).expect("Generated string should be valid UTF-8");
                pool.intern(s)
            }
        }

        HirType::Generic(name) => **name,

        HirType::Void => pool.intern("void"),

        _ => pool.intern("T"),
    })
}
