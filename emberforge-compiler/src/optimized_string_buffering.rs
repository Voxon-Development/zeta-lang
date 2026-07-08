use arrayvec::ArrayVec;
use codex_dependency_graph::DepGraph;
use ir::hir::StrId;
use smallvec::SmallVec;
use std::ptr;
use std::sync::Arc;
use zetaruntime::string_pool::StringPool;

pub fn mangle_method_name(
    dep_graph: &DepGraph,
    module_idx: usize,
    class_name: StrId,
    method_name: StrId,
    context: Arc<StringPool>,
) -> StrId {
    let mut segments: Vec<StrId> = Vec::with_capacity(4);
    segments.push(class_name);

    if let Some(pkg) = dep_graph.get_module_package(module_idx) {
        let pkg_str = context.resolve_string(&pkg);
        segments.extend(pkg_str.split("::").map(|seg| StrId(context.intern(seg))));
    }

    build_module_scoped_name(&segments, method_name, None, context)
}

const VTABLE_LEN: usize = 8;

pub fn make_vtable_name(name: StrId, string_pool: Arc<StringPool>) -> StrId {
    let name = string_pool.resolve_string(&*name);

    let vtable = b"vtable::";
    let nlen = name.len();
    let total_len = VTABLE_LEN + nlen;

    let mut string: SmallVec<u8, 32> = SmallVec::with_capacity(total_len);

    unsafe {
        let dst = string.as_mut_ptr();
        ptr::copy_nonoverlapping(vtable.as_ptr(), dst, VTABLE_LEN);
        ptr::copy_nonoverlapping(name.as_ptr(), dst.add(VTABLE_LEN), nlen);
        string.set_len(total_len);
    }

    StrId(string_pool.intern_bytes(string.as_slice()))
}

pub fn get_type(name: StrId, string_pool: Arc<StringPool>) -> StrId {
    let mut parts: ArrayVec<&str, 2> = ArrayVec::<&str, 2>::new();
    for part in string_pool.resolve_string(&*name).split('_') {
        if parts.try_push(part).is_err() {
            panic!(
                "Unexpected type name with too many parts: {}",
                string_pool.resolve_string(&*name)
            );
        }
    }

    if parts.len() == 1 {
        return StrId(string_pool.intern(parts.first().unwrap()));
    }

    if let Some(inner_value) = parts.last().copied() {
        StrId(string_pool.intern(inner_value))
    } else {
        panic!("FieldAccess on non-user pointer type")
    }
}

pub fn build_module_scoped_name(
    path: &[StrId],
    member: StrId,
    extra: Option<StrId>,
    context: Arc<StringPool>,
) -> StrId {
    let mut parts: Vec<String> = path
        .iter()
        .map(|s| context.resolve_string(s).to_string())
        .collect();
    parts.push(context.resolve_string(&member).to_string());
    if let Some(e) = extra {
        parts.push(context.resolve_string(&e).to_string());
    }
    let joined = parts.join("_");
    StrId(context.intern(&joined))
}

pub fn mangle_function_name(
    dep_graph: &DepGraph,
    module_idx: usize,
    class_name: Option<StrId>,
    func_name: StrId,
    is_extern_c: bool,
    context: Arc<StringPool>,
) -> StrId {
    if is_extern_c {
        return match class_name {
            Some(cls) => build_module_scoped_name(&[cls], func_name, None, context),
            None => func_name,
        };
    }

    let pkg_segments: Vec<StrId> = match dep_graph.get_module_package(module_idx) {
        Some(pkg) => {
            let pkg_str = context.resolve_string(&pkg);
            pkg_str
                .split("::")
                .map(|seg| StrId(context.intern(seg)))
                .collect()
        }
        None => Vec::new(),
    };

    let mut segments: Vec<StrId> = Vec::with_capacity(pkg_segments.len() + 1);
    if let Some(cls) = class_name {
        segments.push(cls);
    }
    segments.extend(pkg_segments);

    build_module_scoped_name(&segments, func_name, None, context)
}
