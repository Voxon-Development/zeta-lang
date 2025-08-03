use std::alloc::{handle_alloc_error, Layout};
use std::ptr::NonNull;
use std::{ptr, slice};
use std::cell::UnsafeCell;
use std::sync::atomic::{AtomicUsize, Ordering};

#[inline(always)]
pub const fn align_up(value: usize, align: usize) -> usize {
    debug_assert!(align.is_power_of_two());
    (value + align - 1) & !(align - 1)
}


#[repr(C)]
pub struct Bump {
    ptr: NonNull<u8>,
    capacity: usize,
    offset: UnsafeCell<usize>, // Interior mutability for offset
    align: usize,
}

unsafe impl Send for Bump {}
unsafe impl Sync for Bump {}

impl Bump {
    pub fn new() -> Self {
        Self::with_capacity(4096)
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Self::with_capacity_and_aligned(capacity, 8)
    }

    pub fn with_capacity_and_aligned(capacity: usize, align: usize) -> Self {
        let layout = unsafe { Layout::from_size_align_unchecked(capacity, align) };
        let ptr = unsafe { std::alloc::alloc(layout) };
        if ptr.is_null() {
            handle_alloc_error(layout);
        }

        Self {
            ptr: unsafe { NonNull::new_unchecked(ptr) },
            capacity,
            offset: UnsafeCell::new(0),
            align,
        }
    }

    #[inline(always)]
    fn alloc_raw(&self, size: usize, align: usize) -> Option<*mut u8> {
        unsafe {
            let offset = &mut *self.offset.get();
            let base = self.ptr.as_ptr() as usize + *offset;
            let aligned = align_up(base, align);
            let end = aligned + size;
            let new_offset = end - self.ptr.as_ptr() as usize;

            if new_offset > self.capacity {
                return None;
            }

            *offset = new_offset;
            Some(aligned as *mut u8)
        }
    }

    #[inline(always)]
    pub fn alloc<T>(&self, val: T) -> Option<&mut T> {
        let align = align_of::<T>();
        let size = size_of::<T>();

        let ptr = self.alloc_raw(size, align)?;
        unsafe {
            ptr.cast::<T>().write(val);
            Some(&mut *ptr.cast::<T>())
        }
    }

    #[inline(always)]
    pub fn alloc_str(&self, s: &str) -> &mut str {
        let len = s.len();
        let ptr = self
            .alloc_raw(len, align_of::<u8>())
            .unwrap_or_else(|| panic!("Bump out of memory for string"));

        unsafe {
            ptr::copy_nonoverlapping(s.as_ptr(), ptr, len);
            let slice = slice::from_raw_parts_mut(ptr, len);
            str::from_utf8_unchecked_mut(slice)
        }
    }

    #[inline(always)]
    pub fn reset(&self) {
        unsafe {
            *self.offset.get() = 0;
        }
    }
}

impl Drop for Bump {
    fn drop(&mut self) {
        let layout = unsafe { Layout::from_size_align_unchecked(self.capacity, self.align) };
        unsafe { std::alloc::dealloc(self.ptr.as_ptr(), layout) }
    }
}

#[repr(C)]
pub struct AtomicBump {
    ptr: NonNull<u8>,
    capacity: usize,
    offset: AtomicUsize,
    align: usize,
}

impl AtomicBump {
    pub fn new() -> Self {
        Self::with_capacity(4096) // default capacity
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Self::with_capacity_and_aligned(capacity, 8)
    }

    pub fn with_capacity_and_aligned(capacity: usize, align: usize) -> Self {
        let layout = unsafe { Layout::from_size_align_unchecked(capacity, align) };
        let ptr = unsafe { std::alloc::alloc(layout) };
        if ptr.is_null() {
            handle_alloc_error(layout);
        }

        Self { ptr: unsafe { NonNull::new_unchecked(ptr) }, capacity, offset: AtomicUsize::new(0), align, }
    }

    #[inline(always)]
    pub fn alloc<T>(&self, val: T) -> Option<&mut T> {
        let size = size_of::<T>();
        let align = align_of::<T>();
        let ptr_base = self.ptr.as_ptr() as usize;

        loop {
            let old = self.offset.load(Ordering::Relaxed);
            let aligned = unsafe { align_up(ptr_base + old, align) };
            let end = aligned + size;
            let new_offset = end - ptr_base;

            if new_offset > self.capacity {
                return None;
            }

            if self.offset
                .compare_exchange(old, new_offset, Ordering::AcqRel, Ordering::Relaxed)
                .is_ok()
            {
                let ptr = aligned as *mut T;
                unsafe {
                    ptr.write(val);
                    return Some(&mut *ptr);
                }
            }
        }
    }

    #[inline(always)]
    pub fn reset(&mut self) {
        self.offset.store(0, Ordering::Relaxed);
    }
}

impl Drop for AtomicBump {
    fn drop(&mut self) {
        self.reset();
        let layout = unsafe { Layout::from_size_align_unchecked(self.capacity, self.align) };
        unsafe { std::alloc::dealloc(self.ptr.as_ptr(), layout) }
    }
}

#[repr(C)]
pub struct Arena {
    chunks: UnsafeCell<Vec<Bump>>,
    chunk_index: UnsafeCell<usize>,
    capacity: usize,
}

impl Arena {
    #[inline(always)]
    pub fn new() -> Self {
        Self::with_capacity(4096)
    }

    #[inline(always)]
    pub fn with_capacity(capacity: usize) -> Self {
        let mut chunks = Vec::with_capacity(2);
        chunks.push(Bump::with_capacity(capacity));
        Self {
            chunks: UnsafeCell::new(chunks),
            chunk_index: UnsafeCell::new(0),
            capacity,
        }
    }

    #[inline(always)]
    fn alloc_raw(&self, size: usize, align: usize) -> *mut u8 {
        let chunks = unsafe { &mut *self.chunks.get() };
        let chunk_index = unsafe { &mut *self.chunk_index.get() };

        let chunk = &mut chunks[*chunk_index];

        let ptr = chunk.ptr.as_ptr() as usize;
        let offset_ref = unsafe { chunk.offset.get().as_mut().unwrap() };
        let base = ptr + *offset_ref;
        let aligned = align_up(base, align);
        let end = aligned + size;

        if core::hint::likely(end <= chunk.ptr.as_ptr() as usize + chunk.capacity) {
            *offset_ref = end - ptr;
            return aligned as *mut u8;
        }

        // Slow path: allocate a new chunk
        *chunk_index += 1;
        if *chunk_index == chunks.len() {
            chunks.push(Bump::with_capacity(self.capacity));
        }

        let new_chunk = &mut chunks[*chunk_index];
        let ptr = new_chunk.ptr.as_ptr() as usize;
        let aligned = align_up(ptr, align);
        let end = aligned + size;

        debug_assert!(end <= new_chunk.ptr.as_ptr() as usize + new_chunk.capacity);

        *unsafe { new_chunk.offset.get().as_mut().unwrap() } = end - ptr;
        aligned as *mut u8
    }

    #[inline(always)]
    pub fn alloc_str(&self, s: &str) -> &mut str {
        let len = s.len();
        let ptr = self.alloc_raw(len, align_of::<u8>());
        unsafe {
            ptr::copy_nonoverlapping(s.as_ptr(), ptr, len);
            let slice = slice::from_raw_parts_mut(ptr, len);
            str::from_utf8_unchecked_mut(slice)
        }
    }

    #[inline(always)]
    pub fn alloc<T>(&self, val: T) -> &mut T {
        let chunks = unsafe { &mut *self.chunks.get() };
        let chunk_index = unsafe { &mut *self.chunk_index.get() };

        let chunk = &mut chunks[*chunk_index];

        let align = align_of::<T>();
        let size = size_of::<T>();
        let ptr = chunk.ptr.as_ptr() as usize;
        let base = ptr + *unsafe { chunk.offset.as_ref_unchecked() };
        let aligned = align_up(base, align);
        let end = aligned + size;

        if core::hint::likely(end <= chunk.capacity) {
            unsafe { chunk.offset.replace(end - ptr); }
            let ptr = aligned as *mut T;
            unsafe {
                ptr.write(val);
                return &mut *ptr;
            }
        }

        // Slow path: new chunk
        *chunk_index += 1;
        if *chunk_index == chunks.len() {
            chunks.push(Bump::with_capacity(self.capacity));
        }

        let new_chunk = &mut chunks[*chunk_index];
        let ptr = new_chunk.ptr.as_ptr() as usize;
        let aligned = align_up(ptr, align);
        let end = aligned + size;

        debug_assert!(end <= new_chunk.capacity);

        unsafe { new_chunk.offset.replace(end - ptr); }
        let ptr = aligned as *mut T;
        unsafe {
            ptr.write(val);
            &mut *ptr
        }
    }
}