use std::alloc::{handle_alloc_error, AllocError, Allocator, Layout};
use std::cell::RefCell;
use std::ptr::NonNull;
use std::rc::Rc;
use std::sync::atomic::{AtomicUsize, Ordering};

pub(crate) const DEFAULT_CHUNK_SIZE: usize = 4096;

#[inline(always)]
pub const fn align_up(value: usize, align: usize) -> usize {
    debug_assert!(align.is_power_of_two());
    (value + align - 1) & !(align - 1)
}


#[repr(C)]
pub struct Bump {
    pub(crate) ptr: NonNull<u8>,
    pub(crate) capacity: usize,
    pub(crate) offset: usize,
    pub(crate) align: usize,
}

impl Default for Bump {
    fn default() -> Self {
        Self::new()
    }
}

impl Bump {
    pub fn new() -> Self {
        Self::with_capacity(DEFAULT_CHUNK_SIZE) // default capacity
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
            offset: 0,
            align,
        }
    }

    #[inline(always)]
    pub fn alloc<T>(&mut self, val: T) -> Option<&mut T> {
        let align = align_of::<T>();
        let size = size_of::<T>();

        let ptr = self.ptr.as_ptr() as usize;
        let base = ptr + self.offset;
        let aligned = align_up(base, align);
        let end = aligned + size;
        let new_offset = end - ptr;

        if core::hint::unlikely(new_offset > self.capacity) {
            return Self::allocation_failed();
        }

        self.offset = new_offset;
        let ptr = aligned as *mut T;
        unsafe {
            ptr.write(val);
            Some(&mut *ptr)
        }
    }

    #[cold]
    #[inline(never)]
    fn allocation_failed<'a, T>() -> Option<&'a mut T> {
        None
    }

    #[inline(always)]
    pub fn get_slice(&self) -> &[u8] {
        let ptr = self.ptr.as_ptr();
        unsafe { std::slice::from_raw_parts(ptr, self.offset) }
    }

    #[inline(always)]
    pub fn reset(&mut self) {
        self.offset = 0;
    }
}

impl Drop for Bump {
    fn drop(&mut self) {
        let layout = unsafe { Layout::from_size_align_unchecked(self.capacity, self.align) };
        unsafe { std::alloc::dealloc(self.ptr.as_ptr(), layout) }
    }
}

struct Chunk {
    ptr: NonNull<u8>,
    capacity: usize,
    offset: usize,
    align: usize,
}

impl Chunk {
    fn new(capacity: usize, align: usize) -> Self {
        let layout = unsafe { Layout::from_size_align_unchecked(capacity, align) };
        let ptr = unsafe { std::alloc::alloc(layout) };
        if ptr.is_null() {
            std::alloc::handle_alloc_error(layout);
        }
        Self {
            ptr: unsafe { NonNull::new_unchecked(ptr) },
            capacity,
            offset: 0,
            align,
        }
    }

    fn try_alloc(&mut self, layout: Layout) -> Option<NonNull<u8>> {
        let ptr = self.ptr.as_ptr() as usize;
        let base = ptr + self.offset;
        let aligned = align_up(base, layout.align());
        let end = aligned + layout.size();
        let new_offset = end - ptr;

        if new_offset > self.capacity {
            return None;
        }

        self.offset = new_offset;
        Some(unsafe { NonNull::new_unchecked(aligned as *mut u8) })
    }
}

impl Drop for Chunk {
    fn drop(&mut self) {
        let layout = unsafe { Layout::from_size_align_unchecked(self.capacity, self.align) };
        unsafe { std::alloc::dealloc(self.ptr.as_ptr(), layout) }
    }
}

pub struct GrowableBump {
    chunks: Rc<RefCell<Vec<Chunk>>>,
    align: usize,
}

impl GrowableBump {
    pub fn new(initial_capacity: usize, align: usize) -> Self {
        Self {
            chunks: Rc::new(RefCell::new(vec![Chunk::new(initial_capacity, align)])),
            align,
        }
    }

    /// Allocate and initialize a value of type `T` inside the bump.
    pub fn alloc_value<T>(&mut self, val: T) -> &mut T {
        let layout = Layout::new::<T>();
        let ptr = self.allocate(layout).unwrap().as_non_null_ptr().cast::<T>();
        unsafe {
            ptr.as_ptr().write(val);
            &mut *ptr.as_ptr()
        }
    }

    /// Reset all but the first chunk.
    pub fn reset(&mut self) {
        let mut chunks = self.chunks.borrow_mut();
        if let Some(first) = chunks.first_mut() {
            first.offset = 0;
        }
        chunks.truncate(1);
    }
}

unsafe impl Allocator for GrowableBump {
    fn allocate(&self, layout: Layout) -> Result<NonNull<[u8]>, AllocError> {
        let mut chunks = self.chunks.borrow_mut();

        if let Some(last) = chunks.last_mut() {
            if let Some(ptr) = last.try_alloc(layout) {
                return Ok(NonNull::slice_from_raw_parts(ptr, layout.size()));
            }
        }

        // Grow: double the last capacity or at least layout.size()
        let new_cap = chunks
            .last()
            .map(|c| c.capacity.max(layout.size()) * 2)
            .unwrap_or(layout.size().max(1024));

        chunks.push(Chunk::new(new_cap, self.align));
        let last = chunks.last_mut().unwrap();
        let ptr = last
            .try_alloc(layout)
            .ok_or(AllocError)?; // must succeed after growing

        Ok(NonNull::slice_from_raw_parts(ptr, layout.size()))
    }

    unsafe fn deallocate(&self, _ptr: NonNull<u8>, _layout: Layout) {
        // bump allocator does not support freeing individual allocations
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

    pub fn len(&self) -> usize {
        self.offset.load(Ordering::Relaxed)
    }

    #[inline(always)]
    pub fn capacity(&self) -> usize {
        self.capacity
    }

    #[inline(always)]
    pub fn alloc_many(&self, data: &[u8]) -> Option<*mut u8> {
        let size = data.len();
        let ptr_base = self.ptr.as_ptr() as usize;

        loop {
            let old = self.offset.load(Ordering::Relaxed);
            let aligned = align_up(ptr_base + old, self.align);
            let end = aligned + size;
            let new_offset = end - ptr_base;

            if new_offset > self.capacity {
                return None;
            }

            if self.offset
                .compare_exchange(old, new_offset, Ordering::AcqRel, Ordering::Relaxed)
                .is_ok()
            {
                return Some(aligned as *mut u8);
            }
        }
    }

    #[inline(always)]
    #[allow(clippy::mut_from_ref)]
    pub fn alloc<T>(&self, val: T) -> Option<&mut T> {
        let size = size_of::<T>();
        let ptr_base = self.ptr.as_ptr() as usize;
        loop {
            let old = self.offset.load(Ordering::Relaxed);
            let aligned = align_up(ptr_base + old, self.align);
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
    pub fn get_slice(&self, start: usize, end: usize) -> &[u8] {
        let ptr = self.ptr.as_ptr();

        // use start, end to get the slice
        unsafe { std::slice::from_raw_parts(ptr.add(start), ptr as usize + end) }
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