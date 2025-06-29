use std::collections::HashMap;
use lazy_static::lazy_static;

#[derive(Debug, Clone)]
pub struct StringPool {
    pub(crate) data_buffer: Vec<u8>,
    interned_strings: HashMap<u64, Vec<ir::VmString>>,
}

lazy_static! {
    static ref HASHER: ahash::RandomState = ahash::RandomState::new();
}

impl StringPool {
    #[inline(always)]
    pub fn new() -> Self {
        StringPool {
            data_buffer: Vec::new(),
            interned_strings: HashMap::new(),
        }
    }

    #[inline(always)]
    pub fn intern(&mut self, s: &str) -> ir::VmString {
        let s_hash = HASHER.hash_one(s);

        if let Some(collision_list) = self.interned_strings.get(&s_hash) {
            // Hash collision: iterate through the list of VmStrings
            // that share this hash to find an exact content match.
            for vm_string in collision_list {
                if self.resolve_string(vm_string) == s {
                    return *vm_string;
                }
            }
        }

        let offset = self.data_buffer.len();
        self.data_buffer.extend_from_slice(s.as_bytes());

        let new_vm_string = ir::VmString {
            offset,
            length: s.len(),
            hash: s_hash,
        };

        self.interned_strings.entry(s_hash).or_default().push(new_vm_string);
        new_vm_string
    }

    #[inline(always)]
    pub fn resolve_string(&self, vm_string: &ir::VmString) -> &str {
        let start = vm_string.offset;
        let end = start + vm_string.length;

        assert!(start <= end, "Invalid string offset");
        assert!(start < self.data_buffer.len(), "String offset out of bounds");
        assert!(end < self.data_buffer.len(), "String offset out of bounds");

        // SAFETY: We trust the VM to pass valid pointers
        unsafe { std::str::from_utf8_unchecked(&self.data_buffer[start..end]) }
    }
}