use crate::bump::align_up;
use std::alloc::{Layout};
use std::ptr::{slice_from_raw_parts_mut, NonNull};
use std::sync::atomic::{AtomicPtr, AtomicUsize, Ordering};
use std::mem::size_of;
use std::mem::align_of;
use std::ptr;
use std::hint::likely;
use std::marker::PhantomData;

/// Lock-free growable bump allocator with a small cached free-list of chunks.
/// Highly unsafe but optimized for speed. Use only in contexts that guarantee
/// chunk lifetimes are valid for as long as pointers are used.
#[repr(C)]
#[derive(Debug)]
struct Chunk {
    ptr: NonNull<u8>,
    capacity: usize,
    offset: AtomicUsize, // atomic offset inside chunk
    align: usize,
    next: AtomicPtr<Chunk>, // intrusive lock-free next pointer
}

impl Clone for Chunk {
    fn clone(&self) -> Self {
        Chunk {
            ptr: self.ptr,
            capacity: self.capacity,
            offset: AtomicUsize::new(self.offset.load(Ordering::Acquire)),
            align: self.align,
            next: AtomicPtr::new(self.next.load(Ordering::Acquire)),
        }
    }
}

unsafe impl Send for Chunk {}
unsafe impl Sync for Chunk {}

impl Chunk {
    unsafe fn new_raw(capacity: usize, align: usize) -> Result<*mut Chunk, std::alloc::AllocError> {
        if capacity == 0 || !align.is_power_of_two() || capacity > isize::MAX as usize {
            return Err(std::alloc::AllocError);
        }

        let layout = Layout::from_size_align(capacity, align).map_err(|_| std::alloc::AllocError)?;
        let mem: *mut u8 = unsafe { std::alloc::alloc(layout) };
        if mem.is_null() {
            return Err(std::alloc::AllocError);
        }

        let boxed = Box::new(Chunk {
            ptr: NonNull::new(mem).ok_or(std::alloc::AllocError)?,
            capacity,
            offset: AtomicUsize::new(0),
            align,
            next: AtomicPtr::new(ptr::null_mut()),
        });

        Ok(Box::into_raw(boxed))
    }

    unsafe fn dealloc_raw(chunk: *mut Chunk) {
        if chunk.is_null() {
            return;
        }
        unsafe {
            let c = &*chunk;
            let layout = Layout::from_size_align_unchecked(c.capacity, c.align);
            std::alloc::dealloc(c.ptr.as_ptr(), layout);
            drop(Box::from_raw(chunk));
        }
    }
}

#[derive(Debug)]
pub struct GrowableAtomicBump<'bump> {
    head: AtomicPtr<Chunk>,
    base_capacity: usize,
    align: usize,

    cache_head: AtomicPtr<Chunk>,
    cache_len: AtomicUsize,
    cache_len_limit: usize,
    
    phantom_data: PhantomData<&'bump ()>
}

unsafe impl<'bump> Send for GrowableAtomicBump<'bump> {}
unsafe impl<'bump> Sync for GrowableAtomicBump<'bump> {}

impl<'bump> GrowableAtomicBump<'bump> {
    pub fn new() -> Self {
        Self::with_capacity_and_aligned_and_cache(4096, 8, 8).expect("Failed to allocate arena")
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Self::with_capacity_and_aligned_and_cache(capacity, 8, 8).expect("Failed to allocate arena")
    }

    pub fn with_capacity_and_aligned(
        capacity: usize,
        align: usize,
    ) -> Result<Self, std::alloc::AllocError> {
        Self::with_capacity_and_aligned_and_cache(capacity, align, 8)
    }

    pub fn with_capacity_and_aligned_and_cache(
        capacity: usize,
        align: usize,
        cache_limit: usize,
    ) -> Result<Self, std::alloc::AllocError> {
        let head = unsafe { Chunk::new_raw(capacity, align)? };
        Ok(GrowableAtomicBump {
            head: AtomicPtr::new(head),
            base_capacity: capacity,
            align,
            cache_head: AtomicPtr::new(ptr::null_mut()),
            cache_len: AtomicUsize::new(0),
            cache_len_limit: cache_limit.max(1),
            phantom_data: PhantomData
        })
    }

    #[inline]
    fn current_head(&self) -> *mut Chunk {
        self.head.load(Ordering::Acquire)
    }

    #[inline]
    pub fn get_slice(&self, start: usize, end: usize) -> &[u8] {
        let head = self.current_head();
        if head.is_null() || start > end {
            return &[];
        }
        unsafe {
            let h = &*head;
            let cur = h.offset.load(Ordering::Acquire);
            if end > cur {
                return &[];
            }
            let base = h.ptr.as_ptr() as usize;
            std::slice::from_raw_parts((base + start) as *const u8, end - start)
        }
    }

    fn chunk_containing_ptr(&self, ptr: *const u8) -> Option<*mut Chunk> {
        let mut cur = self.head.load(Ordering::Acquire);
        while !cur.is_null() {
            unsafe {
                let c = &*cur;
                let base = c.ptr.as_ptr() as usize;
                if (ptr as usize) >= base && (ptr as usize) < base + c.capacity {
                    return Some(cur);
                }
                cur = c.next.load(Ordering::Acquire);
            }
        }
        None
    }

    /// Returns slice from an absolute pointer allocated by this arena.
    /// Caller must ensure pointer belongs to this allocator.
    pub unsafe fn get_slice_from_ptr_unchecked<'a>(
        &self,
        ptr: *const u8,
        len: usize,
    ) -> &'a [u8] {
        debug_assert!(self.chunk_containing_ptr(ptr).is_some());
        unsafe { std::slice::from_raw_parts(ptr, len) }
    }

    fn alloc_aligned_bytes(&self, size: usize, align: usize) -> &mut [u8] {
        if size == 0 {
            return &mut [];
        }
        
        if size > isize::MAX as usize / 2 {
            panic!("ERROR: Attempted to allocate {} bytes (exceeds limit)", size);
        }

        let allocation: &mut [u8] = self.try_alloc_in_head_aligned(size, align);
        if likely(!allocation.is_empty()) {
            return allocation;
        }

        self.allocate_new_chunk_aligned(size, align)
    }

    #[inline]
    fn try_alloc_in_head(&self, size: usize) -> &mut [u8] {
        self.try_alloc_in_head_aligned(size, self.align)
    }

    #[inline]
    fn try_alloc_in_head_aligned(&self, size: usize, align: usize) -> &mut [u8] {
        let head = self.current_head();
        if head.is_null() {
            return &mut [];
        }

        let h = unsafe { &*head };
        let ptr_base = h.ptr.as_ptr() as usize;
        loop {
            let old = h.offset.load(Ordering::Relaxed);
            let aligned_addr = align_up(ptr_base + old, align);
            let end = aligned_addr.checked_add(size).unwrap_or(usize::MAX);
            let new_offset = end - ptr_base;
            if new_offset > h.capacity {
                return &mut [];
            }

            if h.offset
                .compare_exchange(old, new_offset, Ordering::AcqRel, Ordering::Relaxed)
                .is_ok()
            {
                return unsafe { &mut *slice_from_raw_parts_mut(aligned_addr as *mut u8, size) };
            }
        }
    }

    pub fn alloc_bytes(&self, size: usize) -> &mut [u8] {
        if size == 0 {
            return &mut [];
        }

        if size > isize::MAX as usize / 2 {
            panic!("ERROR: Attempted to allocate {} bytes (exceeds limit)", size);
        }

        let allocation: &mut [u8] = self.try_alloc_in_head(size);
        if likely(!allocation.is_empty()) {
            return allocation;
        }

        self.allocate_new_chunk(size)
    }

    #[cold]
    fn allocate_new_chunk_aligned(&self, size: usize, align: usize) -> &mut [u8] {
        let mut cap = self.base_capacity;
        while cap < size {
            cap = cap.checked_mul(2).unwrap_or(isize::MAX as usize / 2);
            if cap > isize::MAX as usize / 2 {
                cap = size.max(self.base_capacity);
                break;
            }
        }

        if let Some(reuse) = self.pop_cached_chunk_with_capacity(cap) {
            unsafe {
                (*reuse).offset.store(0, Ordering::Relaxed);
                self.push_new_head(reuse);
            }
            return self.try_alloc_in_head_aligned(size, align);
        }

        let new_chunk = unsafe { Chunk::new_raw(cap, self.align).ok().expect("Failed to allocate chunk") };
        unsafe { self.push_new_head(new_chunk) };
        self.try_alloc_in_head_aligned(size, align)
    }

    #[cold]
    fn allocate_new_chunk(&self, size: usize) -> &mut [u8] {
        self.allocate_new_chunk_aligned(size, self.align)
    }

    unsafe fn push_new_head(&self, new_chunk: *mut Chunk) {
        loop {
            let old_head = self.head.load(Ordering::Acquire);
            unsafe { (*new_chunk).next.store(old_head, Ordering::Relaxed); }
            if self
                .head
                .compare_exchange(old_head, new_chunk, Ordering::AcqRel, Ordering::Acquire)
                .is_ok()
            {
                break;
            }
        }
    }

    pub fn alloc_value<T>(&self, val: T) -> &'bump mut T {
        let size = size_of::<T>();
        let align = align_of::<T>();

        let ptr: *mut T = self.alloc_aligned_bytes(size, align).as_mut_ptr() as *mut T;
        unsafe {
            ptr.write(val);
            &mut *ptr
        }
    }

    pub fn alloc_value_immutable<T>(&self, val: T) -> &'bump T {
        let size = size_of::<T>();
        let align = align_of::<T>();

        let ptr = self.alloc_aligned_bytes(size, align).as_mut_ptr() as *mut T;
        unsafe {
            ptr.write(val);
            &*ptr
        }
    }

    pub fn alloc_slice<T>(&self, data: &[T]) -> &'bump mut [T] {
        if data.is_empty() {
            return &mut [];
        }

        let len = data.len();
        let type_size = size_of::<T>();
        let size = type_size.checked_mul(len).unwrap_or(usize::MAX);
        let align = align_of::<T>();

        let ptr: *mut T = self.alloc_aligned_bytes(size, align).as_mut_ptr() as *mut T;

        unsafe {
            ptr::copy_nonoverlapping(data.as_ptr(), ptr, len);
            std::slice::from_raw_parts_mut(ptr, len)
        }
    }

    pub fn alloc_slice_immutable<T>(&self, data: &[T]) -> Option<&'bump [T]> {
        if data.is_empty() {
            return Some(&[]);
        }

        let len = data.len();
        let size = size_of::<T>().checked_mul(len)?;
        let align = align_of::<T>();

        let ptr: *mut T = self.alloc_aligned_bytes(size, align).as_mut_ptr() as *mut T;

        unsafe {
            ptr::copy_nonoverlapping(data.as_ptr(), ptr, len);
            Some(std::slice::from_raw_parts(ptr, len))
        }
    }

    pub fn alloc_many(&self, data: &[u8]) -> Option<&mut [u8]> {
        let size = data.len();
        let p: &mut [u8] = self.alloc_bytes(size);
        unsafe { ptr::copy_nonoverlapping(data.as_ptr(), p.as_mut_ptr(), size) };
        Some(p)
    }

    pub fn len(&self) -> usize {
        let mut sum: usize = 0;
        let mut cur = self.head.load(Ordering::Acquire);
        while !cur.is_null() {
            unsafe {
                let c = &*cur;
                sum = sum.saturating_add(c.offset.load(Ordering::Acquire));
                cur = c.next.load(Ordering::Acquire);
            }
        }
        sum
    }

    pub fn head_used(&self) -> usize {
        let h = self.head.load(Ordering::Acquire);
        if h.is_null() {
            return 0;
        }
        unsafe { (&*h).offset.load(Ordering::Acquire) }
    }

    pub fn reset(&mut self) {
        let old_head = self.head.swap(ptr::null_mut(), Ordering::AcqRel);
        let mut keep: *mut Chunk = ptr::null_mut();
        let mut cur = old_head;
        let mut to_cache = Vec::new();

        while !cur.is_null() {
            unsafe {
                let next = (*cur).next.load(Ordering::Acquire);
                (*cur).next.store(ptr::null_mut(), Ordering::Relaxed);
                (*cur).offset.store(0, Ordering::Relaxed);

                if keep.is_null() && (*cur).capacity >= self.base_capacity {
                    keep = cur;
                } else {
                    to_cache.push(cur);
                }
                cur = next;
            }
        }

        if keep.is_null() {
            if let Some(reuse) = self.pop_cached_chunk_with_capacity(self.base_capacity) {
                unsafe {
                    (*reuse).offset.store(0, Ordering::Relaxed);
                    (*reuse).next.store(ptr::null_mut(), Ordering::Relaxed);
                }
                keep = reuse;
            } else if let Ok(newc) = unsafe { Chunk::new_raw(self.base_capacity, self.align) } {
                keep = newc;
            } else {
                for &c in &to_cache {
                    let _ = self.push_cached_chunk(c);
                }
                return;
            }
        }

        self.head.store(keep, Ordering::Release);

        for c in to_cache.into_iter() {
            if !self.push_cached_chunk(c) {
                unsafe { Chunk::dealloc_raw(c) };
            }
        }
    }

    fn push_cached_chunk(&self, chunk: *mut Chunk) -> bool {
        let mut len = self.cache_len.load(Ordering::Acquire);
        loop {
            if len >= self.cache_len_limit {
                return false;
            }
            if self
                .cache_len
                .compare_exchange(len, len + 1, Ordering::AcqRel, Ordering::Acquire)
                .is_ok()
            {
                break;
            }
            len = self.cache_len.load(Ordering::Acquire);
        }

        loop {
            let old = self.cache_head.load(Ordering::Acquire);
            unsafe { (*chunk).next.store(old, Ordering::Relaxed) };
            if self
                .cache_head
                .compare_exchange(old, chunk, Ordering::AcqRel, Ordering::Acquire)
                .is_ok()
            {
                return true;
            }
        }
    }

    fn pop_cached_chunk_with_capacity(&self, min_capacity: usize) -> Option<*mut Chunk> {
        let mut attempts = 0usize;
        let max_attempts = 16usize;

        while attempts < max_attempts {
            let head = self.cache_head.load(Ordering::Acquire);
            if head.is_null() {
                return None;
            }
            let next = unsafe { (*head).next.load(Ordering::Acquire) };
            if self
                .cache_head
                .compare_exchange(head, next, Ordering::AcqRel, Ordering::Acquire)
                .is_ok()
            {
                self.cache_len.fetch_sub(1, Ordering::AcqRel);
                unsafe {
                    if (*head).capacity >= min_capacity {
                        (*head).next.store(ptr::null_mut(), Ordering::Relaxed);
                        return Some(head);
                    } else {
                        if min_capacity <= self.base_capacity {
                            let _ = self.push_cached_chunk(head);
                        } else {
                            Chunk::dealloc_raw(head);
                        }
                        return None;
                    }
                }
            }
            attempts += 1;
        }

        None
    }
}

impl<'bump> Drop for GrowableAtomicBump<'bump> {
    fn drop(&mut self) {
        let mut head = self.head.load(Ordering::Acquire);
        while !head.is_null() {
            unsafe {
                let next = (*head).next.load(Ordering::Acquire);
                Chunk::dealloc_raw(head);
                head = next;
            }
        }

        let mut c = self.cache_head.load(Ordering::Acquire);
        while !c.is_null() {
            unsafe {
                let next = (*c).next.load(Ordering::Acquire);
                Chunk::dealloc_raw(c);
                c = next;
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::Arc;
    use std::thread;

    #[test]
    fn basic_alloc_and_reset() {
        let mut a = GrowableAtomicBump::with_capacity(128);
        let r = a.alloc_value(42u32);
        assert_eq!(*r, 42u32);

        let p = a.alloc_many(&[1u8, 2, 3]).unwrap();
        let s = unsafe { a.get_slice_from_ptr_unchecked(p.as_ptr(), 3) };
        assert_eq!(s, &[1u8, 2, 3]);

        a.reset();
        assert_eq!(a.head_used(), 0);
    }

    #[test]
    fn concurrent_allocs() {
        let a = Arc::new(GrowableAtomicBump::with_capacity(1024));
        let mut handles = Vec::new();
        for _ in 0..8 {
            let ca = a.clone();
            handles.push(thread::spawn(move || {
                for _ in 0..1000 {
                    let _ = ca.alloc_value(42u32);
                }
            }));
        }
        for h in handles {
            h.join().unwrap();
        }
        assert!(a.head_used() > 0);
    }
}