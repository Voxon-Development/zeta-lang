use crate::hir_lowerer::utils::type_suffix_with_pool;
use ir::hir::{HirStruct, HirType, StrId};
use smallvec::SmallVec;
use std::sync::Arc;
use ir::ir_hasher::HashMap;
use zetaruntime::string_pool::StringPool;

/// Create a StrId suffix for a set of substitutions, e.g. {"T": i32} -> "T_i32"
pub fn suffix_for_subs(
    pool: Arc<StringPool>,
    subs: &HashMap<StrId, HirType>,
) -> StrId {
    // Collect into Vec<(StrId, StrId)> so we can sort deterministically.
    let mut pieces: Vec<(StrId, StrId)> = subs
        .iter()
        .map(|(k, v)| {
            let suffix = type_suffix_with_pool(pool.clone(), v);
            (*k, suffix)
        })
        .collect();

    // sort deterministically by key bytes so result is stable across hashmap order
    pieces.sort_by_key(|(k, _)| pool.resolve_string(k).to_string());

    // Build buffer
    let mut buf: SmallVec<[u8; 128]> = SmallVec::new();

    for (i, (k, v)) in pieces.iter().enumerate() {
        if i > 0 {
            buf.push(b'_');
        }
        buf.extend_from_slice(pool.resolve_bytes(&*k));
        buf.extend_from_slice(pool.resolve_bytes(&*v));
    }

    // Convert &[u8] -> &str (we built it from valid UTF-8 interned strings)
    let s = std::str::from_utf8(&buf).expect("Generated string should be valid UTF-8");

    StrId(pool.intern(s))
}

/// Generate a specialized class name from concrete type arguments
pub fn instantiate_class_name(
    concrete_args: &[HirType],
    base: &HirStruct,
    pool: Arc<StringPool>,
) -> StrId {
    let mut buf: SmallVec<[u8; 128]> = SmallVec::new();
    buf.extend_from_slice(pool.resolve_bytes(&*base.name));
    for arg in concrete_args {
        buf.push(b'_');
        let suf = type_suffix_with_pool(pool.clone(), arg);
        buf.extend_from_slice(pool.resolve_bytes(&*suf));
    }
    let s = std::str::from_utf8(&buf).expect("Generated string should be valid UTF-8");
    let interned = StrId(pool.intern(s));
    interned
}
