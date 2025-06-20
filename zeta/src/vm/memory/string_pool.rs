use std::collections::HashMap;

#[derive(Debug)]
pub struct StringPool {
    pub(crate) data_buffer: Vec<u8>,
    interned_strings: HashMap<u64, Vec<ir::VmString>>,
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
        let s_hash = ahash::RandomState::new().hash_one(s);

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

        unsafe { std::str::from_utf8_unchecked(&self.data_buffer[start..end]) }
    }
}