use std::ptr::NonNull;
use libc::{mmap, munmap, MAP_ANON, MAP_PRIVATE, PROT_READ, PROT_WRITE, MAP_FAILED, c_void, size_t};

#[inline]
pub const fn align_up(value: usize, align: usize) -> usize {
    debug_assert!(align.is_power_of_two());
    (value + align - 1) & !(align - 1)
}

const DEFAULT_CAPACITY: usize = 4 * 1024; // 4 KB bump default

#[repr(C)]
pub struct Bump {
    ptr: NonNull<u8>,
    capacity: usize,
    offset: usize,
}

impl Bump {
    pub fn new() -> Self {
        Self::with_capacity(DEFAULT_CAPACITY)
    }

    pub fn with_capacity(capacity: usize) -> Self {
        unsafe {
            let ptr = mmap(
                std::ptr::null_mut(),
                capacity as size_t,
                PROT_READ | PROT_WRITE,
                MAP_PRIVATE | MAP_ANON,
                -1,
                0,
            );

            if ptr == MAP_FAILED {
                oom();
            }

            Self {
                ptr: NonNull::new(ptr as *mut u8).unwrap(),
                capacity,
                offset: 0,
            }
        }
    }

    #[inline]
    pub fn reset(&mut self) {
        self.offset = 0;
    }

    #[inline]
    pub fn alloc_aligned(&mut self, size: usize, align: usize) -> Option<NonNull<u8>> {
        debug_assert!(align.is_power_of_two());

        let base_ptr = self.ptr.as_ptr() as usize;
        let base = base_ptr + self.offset;
        let aligned = align_up(base, align);

        let aligned_end = aligned.checked_add(size)?;
        let new_offset = aligned_end.checked_sub(base_ptr)?;

        if new_offset > self.capacity {
            return None;
        }

        self.offset = new_offset;
        Some(unsafe { NonNull::new_unchecked(aligned as *mut u8) })
    }

    #[inline]
    pub fn alloc_struct<'a, T>(&'a mut self, val: T) -> Option<&'a mut T> {
        let layout = std::alloc::Layout::new::<T>();
        let ptr = self.alloc_aligned(layout.size(), layout.align())?;
        unsafe {
            let typed = ptr.as_ptr() as *mut T;
            typed.write(val);
            Some(&mut *typed)
        }
    }
}

impl Drop for Bump {
    fn drop(&mut self) {
        unsafe {
            if munmap(self.ptr.as_ptr() as *mut c_void, self.capacity as size_t) != 0 {
                oom(); // Rare, but good to catch
            }
        }
    }
}

#[cold]
#[inline(never)]
fn oom() -> ! {
    panic!("Out of memory or mmap/munmap failed");
}