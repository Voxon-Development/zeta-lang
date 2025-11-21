use std::alloc::{handle_alloc_error, AllocError, Allocator, Layout};
use std::cell::{RefCell, RefMut};
use std::hint::likely;
use std::marker::PhantomData;
use std::mem::{align_of, size_of};
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
pub struct Bump<'bump> {
    pub(crate) ptr: NonNull<u8>,
    pub(crate) capacity: usize,
    pub(crate) offset: usize,
    pub(crate) align: usize,
    phantom_data: PhantomData<&'bump ()>
}

impl<'bump> Default for Bump<'bump> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'bump> Bump<'bump> {
    pub fn new() -> Self {
        Self::with_capacity(DEFAULT_CHUNK_SIZE) // default capacity
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Self::with_capacity_and_aligned(capacity, 8)
    }

    pub fn with_capacity_and_aligned(
        capacity: usize,
        align: usize
    ) -> Self {
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
            phantom_data: PhantomData
        }
    }

    pub fn alloc<T>(
        &mut self,
        val: T
    ) -> Option<&'bump mut T> {
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
    pub fn get_slice(&self) -> &'bump [u8] {
        let ptr = self.ptr.as_ptr();
        unsafe { std::slice::from_raw_parts(ptr, self.offset) }
    }

    #[inline(always)]
    pub fn reset(&mut self) {
        self.offset = 0;
    }
}

impl<'bump> Drop for Bump<'bump> {
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
    fn new(
        capacity: usize,
        align: usize
    ) -> Self {
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

    fn try_alloc(
        &mut self,
        layout: Layout
    ) -> Option<NonNull<u8>> {
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

#[derive(Clone)]
pub struct GrowableBump<'bump> {
    chunks: Rc<RefCell<Vec<Chunk>>>,
    align: usize,
    phantom_data: PhantomData<&'bump ()>
}

impl<'bump> GrowableBump<'bump> {
    pub fn new(
        initial_capacity: usize,
        align: usize
    ) -> Self {
        Self {
            chunks: Rc::new(RefCell::new(vec![Chunk::new(initial_capacity, align)])),
            align,
            phantom_data: PhantomData
        }
    }

    /// Allocate and initialize a value of type `T` inside the bump.
    pub fn alloc_value<T>(
        &self,
        val: T
    ) -> &'bump mut T {
        let layout = Layout::new::<T>();
        let ptr: NonNull<T> = self.allocate(layout)
            .unwrap()
            .as_non_null_ptr()
            .cast::<T>();
        unsafe {
            ptr.as_ptr().write(val);
            &mut *ptr.as_ptr()
        }
    }

    /// Allocate and initialize a value of type `T` inside the bump.
    pub fn alloc_value_immutable<T>(
        &self,
        val: T
    ) -> &'bump T {
        let layout = Layout::new::<T>();
        let ptr: NonNull<T> = self.allocate(layout)
            .unwrap()
            .as_non_null_ptr()
            .cast::<T>();
        unsafe {
            ptr.as_ptr().write(val);
            &*ptr.as_ptr()
        }
    }

    pub fn alloc_bytes(&self, len: usize) -> &'bump mut [u8] {
        if len == 0 {
            return &mut []; // no allocation needed
        }

        let layout = Layout::array::<u8>(len).expect("invalid layout for bytes");
        let ptr = self
            .allocate(layout)
            .unwrap_or_else(|_| panic!("bump allocator out of memory for {len} bytes"))
            .as_non_null_ptr()
            .cast::<u8>();

        unsafe { std::slice::from_raw_parts_mut(ptr.as_ptr(), len) }
    }

    /// Allocate a copy of an existing byte slice inside the bump.
    pub fn alloc_bytes_copy(&self, src: &[u8]) -> &mut [u8] {
        let dst = self.alloc_bytes(src.len());
        dst.copy_from_slice(src);
        dst
    }

    /// Reset all but the first chunk.
    pub fn reset(&mut self) {
        let mut chunks = self.chunks.borrow_mut();
        if let Some(first) = chunks.first_mut() {
            first.offset = 0;
        }
        chunks.truncate(1);
    }

    pub fn alloc_slice_copy<T: Copy>(&self, slice: &[T]) -> &'bump [T] {
        if slice.is_empty() {
            return &[];
        }

        let layout = Layout::array::<T>(slice.len())
            .expect("Failed to create layout for slice");

        // allocate raw bytes large enough and properly aligned for T
        let ptr = self.allocate(layout)
            .expect("Failed to allocate slice in bump allocator")
            .as_ptr() as *mut T;

        unsafe {
            std::ptr::copy_nonoverlapping(slice.as_ptr(), ptr, slice.len());
            std::slice::from_raw_parts(ptr, slice.len())
        }
    }

    pub fn alloc_slice<T>(&self, slice: &[T]) -> &'bump [T] {
        if slice.is_empty() {
            return &[];
        }

        let layout = Layout::array::<T>(slice.len())
            .expect("Failed to create layout for slice");

        // allocate raw bytes large enough and properly aligned for T
        let ptr = self.allocate(layout)
            .expect("Failed to allocate slice in bump allocator")
            .as_ptr() as *mut T;

        unsafe {
            std::ptr::copy_nonoverlapping(slice.as_ptr(), ptr, slice.len());
            std::slice::from_raw_parts(ptr, slice.len())
        }
    }

    #[cold]
    fn grow_allocator(&self, layout: Layout, chunks: &mut RefMut<Vec<Chunk>>) -> Result<NonNull<[u8]>, AllocError> {
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
}

unsafe impl<'bump> Allocator for GrowableBump<'bump> {
    fn allocate(&self, layout: Layout) -> Result<NonNull<[u8]>, AllocError> {
        let mut chunks = self.chunks.borrow_mut();

        if let Some(last) = chunks.last_mut() {
            let ptr = last.try_alloc(layout);
            if likely(ptr.is_some()) {
                return Ok(NonNull::slice_from_raw_parts(ptr.unwrap(), layout.size()));
            }
        }

        // Grow: double the last capacity or at least layout.size()
        self.grow_allocator(layout, &mut chunks)
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
        let align = align_of::<T>();
        let ptr_base = self.ptr.as_ptr() as usize;
        loop {
            let old = self.offset.load(Ordering::Relaxed);
            let aligned = align_up(ptr_base + old, align);
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

        // Validate bounds
        if start > end || end > self.offset.load(Ordering::Relaxed) {
            return &[];
        }

        // use start, end to get the slice
        unsafe { std::slice::from_raw_parts(ptr.add(start), end - start) }
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