use std::ptr;
use std::sync::Arc;
use smallvec::SmallVec;
use ir::hir::StrId;
use zetaruntime::string_pool::StringPool;

pub fn build_scoped_name(maybe_cls_name: Option<&str>, field: StrId, pool: Arc<StringPool>) -> StrId {
    let cls = maybe_cls_name.unwrap_or("Unknown");
    let sep = "::";

    let total_len = cls.len() + sep.len() + field.len();

    let mut buf: SmallVec<[u8; 64]> = SmallVec::with_capacity(total_len);

    unsafe {
        // copy class name
        ptr::copy_nonoverlapping(cls.as_ptr(), buf.as_mut_ptr(), cls.len());

        // copy "::"
        ptr::copy_nonoverlapping(sep.as_ptr(), buf.as_mut_ptr().add(cls.len()), sep.len());

        // copy field
        ptr::copy_nonoverlapping(pool.resolve_bytes(&*field).as_ptr(), buf.as_mut_ptr().add(cls.len() + sep.len()), field.len());

        // mark the length
        buf.set_len(total_len);
    }

    StrId(pool.intern_bytes(buf.as_slice()))
}

const VTABLE_LEN: usize = 8;

pub fn make_vtable_name(name: StrId, string_pool: Arc<StringPool>) -> StrId {
    let name = string_pool.resolve_string(&*name);

    let vtable = b"vtable::";
    let nlen = name.len();
    let total_len = VTABLE_LEN + nlen;

    let mut string: SmallVec<[u8; 32]> = SmallVec::with_capacity(total_len);

    unsafe {
        let dst = string.as_mut_ptr();
        ptr::copy_nonoverlapping(vtable.as_ptr(), dst, VTABLE_LEN);
        ptr::copy_nonoverlapping(name.as_ptr(), dst.add(VTABLE_LEN), nlen);
        string.set_len(total_len);
    }

    StrId(string_pool.intern_bytes(string.as_slice()))
}

pub fn get_type(name: StrId, string_pool: Arc<StringPool>) -> StrId {
    let mut parts = arrayvec::ArrayVec::<&str, 2>::new();
    for part in string_pool.resolve_string(&*name).split('_') {
        if parts.try_push(part).is_err() {
            panic!("Unexpected type name with too many parts: {}", string_pool.resolve_string(&*name));
        }
    }

    if parts.len() == 1 {
        return StrId(string_pool.intern(parts.first().unwrap()));
    }

    //let i: &str = parts.next().unwrap();
    if let Some(inner_value) = parts.last().copied() {
        StrId(string_pool.intern(inner_value))
    } else {
        panic!("FieldAccess on non-user pointer type")
    }
}