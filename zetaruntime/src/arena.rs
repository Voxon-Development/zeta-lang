use crate::bump::align_up;
use std::alloc::Layout;
use std::ptr::NonNull;
use std::sync::atomic::{AtomicPtr, AtomicUsize, Ordering};
use std::{mem::size_of, ptr};

/// Lock-free growable bump allocator with a small cached free-list of chunks.
/// This version is maximally "unsafe" to reduce overhead: no `Arc` inside `Chunk`,
/// no `Mutex` on the cache. Use with care.
#[repr(C)]
#[derive(Debug)]
struct Chunk {
    ptr: NonNull<u8>,
    capacity: usize,
    offset: AtomicUsize,         // atomic offset inside chunk
    align: usize,
    next: AtomicPtr<Chunk>,      // intrusive lock-free next pointer
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

// Safety: We ensure that Chunk is only accessed via atomic pointers and controlled
// lifetimes. The allocator ensures chunks live until deallocated in Drop.
unsafe impl Send for Chunk {}
unsafe impl Sync for Chunk {}

impl Chunk {
    /// Allocate a chunk and return a raw pointer. Caller owns the pointer and must
    /// eventually call `dealloc`.
    unsafe fn new_raw(capacity: usize, align: usize) -> Result<*mut Chunk, std::alloc::AllocError> {
        if capacity == 0 || !align.is_power_of_two() || capacity > isize::MAX as usize {
            return Err(std::alloc::AllocError);
        }

        let layout = Layout::from_size_align(capacity, align).map_err(|_| std::alloc::AllocError)?;
        let mem = unsafe { std::alloc::alloc(layout) };
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

    /// Deallocate a chunk previously returned by `new_raw`.
    unsafe fn dealloc_raw(chunk: *mut Chunk) {
        if chunk.is_null() { return; }
        // Reconstruct the box so Rust drops it
        let c = unsafe { &*chunk };
        // Use unchecked layout because we validated on allocation
        unsafe {
            let layout = Layout::from_size_align_unchecked(c.capacity, c.align);
            std::alloc::dealloc(c.ptr.as_ptr(), layout);
            // drop the Box itself
            drop(Box::from_raw(chunk));
        }
    }
}

#[derive(Debug)]
pub struct GrowableAtomicBump {
    head: AtomicPtr<Chunk>,          // current head chunk (for fast-path allocations)
    base_capacity: usize,
    align: usize,

    // lock-free cache of chunks (single-linked list). Bounded by cache_len_limit.
    cache_head: AtomicPtr<Chunk>,
    cache_len: AtomicUsize,
    cache_len_limit: usize,
}

unsafe impl Send for GrowableAtomicBump {}
unsafe impl Sync for GrowableAtomicBump {}

impl GrowableAtomicBump {
    /// Default: base capacity 4096, align 8, cache limit 8
    pub fn new() -> Self {
        Self::with_capacity_and_aligned_and_cache(4096, 8, 8).expect("Failed to allocate arena")
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Self::with_capacity_and_aligned_and_cache(capacity, 8, 8).expect("Failed to allocate arena")
    }
    
    pub fn with_capacity_and_aligned(capacity: usize, align: usize) -> Result<Self, std::alloc::AllocError> {
        Self::with_capacity_and_aligned_and_cache(capacity, align, 8)
    }

    pub fn with_capacity_and_aligned_and_cache(capacity: usize, align: usize, cache_limit: usize) -> Result<Self, std::alloc::AllocError> {
        // allocate initial head chunk
        let head = unsafe { Chunk::new_raw(capacity, align)? };
        Ok(GrowableAtomicBump {
            head: AtomicPtr::new(head),
            base_capacity: capacity,
            align,
            cache_head: AtomicPtr::new(ptr::null_mut()),
            cache_len: AtomicUsize::new(0),
            cache_len_limit: cache_limit.max(1),
        })
    }

    #[inline]
    fn current_head(&self) -> *mut Chunk {
        self.head.load(Ordering::Acquire)
    }

    /// Fast, checked slice view into the head chunk (safe routine with bounds-checks).
    /// For hot-path you can expose an unsafe unchecked variant.
    #[inline]
    pub fn get_slice(&self, start: usize, end: usize) -> &[u8] {
        let head = self.current_head();
        if head.is_null() { return &[]; }
        if start > end { return &[]; }

        unsafe {
            let h = &*head;
            let cur = h.offset.load(Ordering::Acquire);
            if end > cur { return &[]; }

            let base = h.ptr.as_ptr() as usize;
            let start_ptr = base + start;
            let len = end - start;
            std::slice::from_raw_parts(start_ptr as *const u8, len)
        }
    }

    /// Unsafe unchecked slice (caller must ensure bounds).
    pub unsafe fn get_slice_unchecked<'a>(&self, start: usize, len: usize) -> &'a [u8] {
        let head = self.current_head();
        let h = unsafe { &*head };
        let base = h.ptr.as_ptr() as usize;
        let start_ptr = base + start;
        unsafe { std::slice::from_raw_parts(start_ptr  as *const u8, len) }
    }

    /// Try to allocate `size` bytes in the current head chunk (fast path).
    #[inline]
    fn try_alloc_in_head(&self, size: usize) -> Option<*mut u8> {
        let head = self.current_head();
        if head.is_null() { return None; }
        unsafe {
            let h = &*head;
            let ptr_base = h.ptr.as_ptr() as usize;
            loop {
                let old = h.offset.load(Ordering::Relaxed);
                // align address: align_up(ptr_base + old, h.align)
                let aligned_addr = align_up(ptr_base + old, h.align);
                let end = aligned_addr.checked_add(size)?;
                let new_offset = end - ptr_base;
                if new_offset > h.capacity {
                    return None;
                }
                if h.offset.compare_exchange(old, new_offset, Ordering::AcqRel, Ordering::Relaxed).is_ok() {
                    return Some(aligned_addr as *mut u8);
                }
                // else retry
            }
        }
    }

    /// Allocate raw bytes. Returns pointer or None on OOM/invalid size.
    pub fn alloc_bytes(&self, size: usize) -> Option<*mut u8> {
        if size == 0 || size > isize::MAX as usize / 2 {
            return None;
        }

        // Fast path
        if let Some(p) = self.try_alloc_in_head(size) { return Some(p); }

        // Slow path: allocate or reuse a chunk and push it as new head.
        let needed = size;
        let mut cap = self.base_capacity;
        while cap < needed {
            cap = cap.checked_mul(2)?;
            if cap > isize::MAX as usize / 2 {
                cap = needed.max(self.base_capacity);
                break;
            }
        }
        if cap < size { cap = size; }

        // Try to pop a cached chunk of adequate capacity
        if let Some(reuse) = self.pop_cached_chunk_with_capacity(cap) {
            unsafe {
                // Reset its offset
                (*reuse).offset.store(0, Ordering::Relaxed);
                // Push as new head
                self.alloc_new_bytes(reuse);
                // try to allocate in new head (should succeed)
                if let Some(p) = self.try_alloc_in_head(size) { return Some(p); }
            }
        }

        // Otherwise allocate a fresh chunk
        let new_chunk = unsafe { Chunk::new_raw(cap, self.align).ok()? };
        unsafe {
            self.alloc_new_bytes(new_chunk);
        }

        // now try allocate in head
        self.try_alloc_in_head(size)
    }

    unsafe fn alloc_new_bytes(&self, new_chunk: *mut Chunk) {
        loop {
            let old_head = self.head.load(Ordering::Acquire);
            unsafe { (*new_chunk).next.store(old_head, Ordering::Relaxed); }
            if self.head.compare_exchange(old_head, new_chunk, Ordering::AcqRel, Ordering::Acquire).is_ok() {
                break;
            }
        }
    }

    /// Allocate and return a typed mutable reference (writes value into arena).
    /// This returns `&'static mut T` in practice because the arena is assumed to outlive callers.
    /// Use with caution: the returned reference will be invalid after `reset` or drop.
    pub fn alloc<T>(&self, val: T) -> Option<&'static mut T> {
        let size = size_of::<T>();
        let align = align_of::<T>().max(self.align);
        // compute worst-case needed bytes to align within chunk: size + align
        let bytes = self.alloc_bytes(size + align)?;
        let addr = bytes as usize;
        let aligned = align_up(addr, align);
        let ptr = aligned as *mut T;
        unsafe {
            ptr.write(val);
            Some(&mut *ptr)
        }
    }

    /// Allocate and copy `data` into arena; returns pointer to start.
    pub fn alloc_many(&self, data: &[u8]) -> Option<*mut u8> {
        let size = data.len();
        let p = self.alloc_bytes(size)?;

        // SAFETY: Can never overlap
        unsafe { ptr::copy_nonoverlapping(data.as_ptr(), p, size); }
        Some(p)
    }

    /// Return approximate used bytes in the entire chunk list (sums offsets). This reads offsets atomically.
    pub fn len(&self) -> usize {
        let mut sum = 0usize;
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

    /// Approximate used bytes in head chunk.
    pub fn head_used(&self) -> usize {
        let h = self.head.load(Ordering::Acquire);
        if h.is_null() { return 0; }
        unsafe { (&*h).offset.load(Ordering::Acquire) }
    }

    /// Exclusive reset: caller must have mutable access; this function reclaims chunks into cache and
    /// sets head to a single chunk of at least base_capacity (reusing cached if possible).
    pub fn reset(&mut self) {
        // Take entire current head list
        let old_head = self.head.swap(ptr::null_mut(), Ordering::AcqRel);

        // Walk list; keep one chunk to be the new head (prefer capacity >= base_capacity),
        // others go into local vector for caching or immediate deallocation.
        let mut keep: *mut Chunk = ptr::null_mut();
        let mut cur = old_head;
        let mut to_cache: Vec<*mut Chunk> = Vec::new();

        while !cur.is_null() {
            unsafe {
                let next = (*cur).next.load(Ordering::Acquire);
                // reset next pointer for detached list nodes
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

        // If we didn't find a keep chunk, try to take one from cache
        if keep.is_null() {
            if let Some(reuse) = self.pop_cached_chunk_with_capacity(self.base_capacity) {
                unsafe {
                    (*reuse).offset.store(0, Ordering::Relaxed);
                    (*reuse).next.store(ptr::null_mut(), Ordering::Relaxed);
                }
                keep = reuse;
            } else {
                // try to allocate fresh chunk
                if let Ok(newc) = unsafe { Chunk::new_raw(self.base_capacity, self.align) } {
                    keep = newc;
                } else {
                    // cannot allocate: push everything back into cache (best-effort) and return
                    for &c in &to_cache { let _ = self.push_cached_chunk(c); }
                    return;
                }
            }
        }

        // Set head to keep
        self.head.store(keep, Ordering::Release);

        // Push other chunks into shared cache (bounded)
        for c in to_cache.into_iter() {
            if !self.push_cached_chunk(c) {
                // cache full -> deallocate
                unsafe { Chunk::dealloc_raw(c); }
            }
        }
    }

    /// Try to push a chunk onto the lock-free cache. Returns true if cached, false if cache is full.
    fn push_cached_chunk(&self, chunk: *mut Chunk) -> bool {
        // Bound cache size
        let mut len = self.cache_len.load(Ordering::Acquire);
        loop {
            if len >= self.cache_len_limit {
                return false;
            }
            if self.cache_len.compare_exchange(len, len + 1, Ordering::AcqRel, Ordering::Acquire).is_ok() {
                break;
            }
            len = self.cache_len.load(Ordering::Acquire);
        }

        // Insert at head using CAS
        loop {
            let old = self.cache_head.load(Ordering::Acquire);
            unsafe { (*chunk).next.store(old, Ordering::Relaxed); }
            if self.cache_head.compare_exchange(old, chunk, Ordering::AcqRel, Ordering::Acquire).is_ok() {
                return true;
            }
        }
    }

    /// Pop a cached chunk with capacity >= min_capacity. If none found, returns None.
    fn pop_cached_chunk_with_capacity(&self, min_capacity: usize) -> Option<*mut Chunk> {
        // We perform a linear scan by popping heads until we find one that fits, pushing back the others.
        // To avoid infinite loops and contention, we do a bounded number of attempts.
        let mut attempts = 0usize;
        let max_attempts = 16usize;

        while attempts < max_attempts {
            // Pop one
            let head = self.cache_head.load(Ordering::Acquire);
            if head.is_null() { return None; }
            // Try to pop
            let next = unsafe { (*head).next.load(Ordering::Acquire) };
            if self.cache_head.compare_exchange(head, next, Ordering::AcqRel, Ordering::Acquire).is_ok() {
                // we popped head; decrement cache_len
                self.cache_len.fetch_sub(1, Ordering::AcqRel);
                unsafe {
                    return if (*head).capacity >= min_capacity {
                        (*head).next.store(ptr::null_mut(), Ordering::Relaxed);
                        Some(head)
                    } else {
                        // Not big enough: either keep it for reuse if min_capacity small, or dealloc
                        // For simplicity: if min_capacity is small (< base_capacity) we'll reuse; otherwise dealloc
                        if min_capacity <= self.base_capacity {
                            // push it back
                            let _ = self.push_cached_chunk(head);
                            None
                        } else {
                            // deallocate smaller chunk
                            Chunk::dealloc_raw(head);
                            None
                        }
                    }
                }
            }

            attempts += 1;
        }

        None
    }
}

impl Drop for GrowableAtomicBump {
    fn drop(&mut self) {
        // Drain head list
        let mut head = self.head.load(Ordering::Acquire);
        while !head.is_null() {
            unsafe {
                let next = (*head).next.load(Ordering::Acquire);
                Chunk::dealloc_raw(head);
                head = next;
            }
        }

        // Drain cache list
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
        let r = a.alloc(42u32).unwrap();
        assert_eq!(*r, 42u32);

        let p = a.alloc_many(&[1u8, 2, 3]).unwrap();
        unsafe {
            let s = a.get_slice_unchecked(p as usize, 3);
            assert_eq!(s, &[1u8, 2, 3]);
        }

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
                    let _ = ca.alloc(42u32);
                }
            }));
        }
        for h in handles { h.join().unwrap(); }
        assert!(a.head_used() > 0);
    }
}