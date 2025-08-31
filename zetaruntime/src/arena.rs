use crate::bump::align_up;
use std::alloc::Layout;
use std::ptr::NonNull;
use std::sync::atomic::{AtomicPtr, AtomicUsize, Ordering};
use std::sync::Mutex;
use std::{mem::size_of, ptr};

/// A single bump chunk. Each chunk is independently bump-allocated using an atomic offset
/// so concurrent allocations can proceed in parallel as long as they target the same chunk.
#[repr(C)]
struct Chunk {
    ptr: NonNull<u8>,
    capacity: usize,
    offset: AtomicUsize,
    align: usize,
    next: *mut Chunk,
}

// I can do this because Chunk is actually only shared in thread-safe contexts
unsafe impl Send for Chunk {}
unsafe impl Sync for Chunk {}

impl Chunk {
    unsafe fn new(capacity: usize, align: usize) -> Result<AtomicPtr<Chunk>, std::alloc::AllocError> {
        // Validate inputs to prevent overflow
        if capacity == 0 || !align.is_power_of_two() || capacity > isize::MAX as usize {
            return Err(std::alloc::AllocError);
        }
        
        let layout = Layout::from_size_align(capacity, align)
            .map_err(|_| std::alloc::AllocError)?;
        
        // SAFETY: Layout is valid as we just created it successfully
        let mem = unsafe { std::alloc::alloc(layout) };
        if mem.is_null() {
            return Err(std::alloc::AllocError);
        }

        // allocate the Chunk on the heap so we can store a pointer to it
        let boxed = Box::new(Chunk {
            ptr: NonNull::new(mem).ok_or(std::alloc::AllocError)?,
            capacity,
            offset: AtomicUsize::new(0),
            align,
            next: ptr::null_mut(),
        });
        Ok(AtomicPtr::new(Box::into_raw(boxed)))
    }

    unsafe fn dealloc(chunk: *mut Chunk) {
        if chunk.is_null() {
            return;
        }
        // SAFETY: chunk is non-null and was allocated by us
        let c = unsafe { &*chunk };

        // SAFETY: capacity and align were validated when the chunk was created
        let layout = unsafe { Layout::from_size_align_unchecked(c.capacity, c.align) };

        // SAFETY: ptr was allocated with this layout
        unsafe { std::alloc::dealloc(c.ptr.as_ptr(), layout) };
        // SAFETY: chunk was allocated by Box::into_raw

        unsafe { drop(Box::from_raw(chunk)) };
    }
}

#[derive(Debug)]
pub struct GrowableAtomicBump {
    head: AtomicPtr<Chunk>,
    base_capacity: usize,
    align: usize,
    cache: Mutex<Vec<*mut Chunk>>,
}

// Everything is thread-safe smh
unsafe impl Send for GrowableAtomicBump {}
unsafe impl Sync for GrowableAtomicBump {}

impl GrowableAtomicBump {
    pub fn new() -> Self {
        Self::with_capacity_and_aligned(4096, 8).expect("Failed to allocate arena")
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Self::with_capacity_and_aligned(capacity, 8).expect("Failed to allocate arena")
    }

    pub fn with_capacity_and_aligned(capacity: usize, align: usize) -> Result<Self, std::alloc::AllocError> {
        let head = unsafe { Chunk::new(capacity, align)? };
        Ok(Self {
            head,
            base_capacity: capacity,
            align,
            cache: Mutex::new(Vec::new()),
        })
    }

    #[inline]
    fn current_head(&self) -> *mut Chunk {
        self.head.load(Ordering::Acquire)
    }

    #[inline]
    pub fn get_slice(&self, start: usize, end: usize) -> &[u8] {
        let head = self.current_head();
        if head.is_null() {
            return &[];
        }
        
        // Validate bounds
        if start > end {
            return &[];
        }
        
        unsafe {
            let h = &*head;
            let current_offset = h.offset.load(Ordering::Relaxed);
            
            // Ensure we don't read beyond allocated memory
            if start >= current_offset || end > current_offset {
                return &[];
            }
            
            let ptr_base = h.ptr.as_ptr() as usize;
            let start_ptr = ptr_base + start;
            let len = end - start;
            
            // Additional bounds check
            if start_ptr.checked_add(len).map_or(true, |end_ptr| end_ptr > ptr_base + current_offset) {
                return &[];
            }
            
            std::slice::from_raw_parts(start_ptr as *const u8, len)
        }
    }

    #[inline]
    fn try_alloc_in_head(&self, size: usize) -> Option<*mut u8> {
        let head = self.current_head();
        if head.is_null() {
            return None;
        }
        unsafe {
            let h = &*head;
            let ptr_base = h.ptr.as_ptr() as usize;
            loop {
                let old = h.offset.load(Ordering::Relaxed);
                let aligned = align_up(ptr_base + old, h.align);
                let end = aligned.checked_add(size)?;
                let new_offset = end - ptr_base;
                if new_offset > h.capacity {
                    return None;
                }

                if h.offset
                    .compare_exchange(old, new_offset, Ordering::AcqRel, Ordering::Relaxed)
                    .is_ok()
                {
                    return Some(aligned as *mut u8);
                }
                // else retry
            }
        }
    }

    pub fn alloc_bytes(&self, size: usize) -> Option<*mut u8> {
        // Validate size to prevent overflow
        if size == 0 || size > isize::MAX as usize / 2 {
            return None;
        }

        // fast path: try current head
        if let Some(p) = self.try_alloc_in_head(size) {
            return Some(p);
        }

        // slow path: need to grow. We'll allocate a new chunk sized to fit `size` and usually larger.
        // Try to reuse a cached chunk first.
        let needed = size;
        // Choose new capacity: at least double the base_capacity until it fits `size` OR at least base_capacity * 2.
        let mut cap = self.base_capacity;
        while cap < needed {
            cap = cap.checked_mul(2)?;
            if cap > isize::MAX as usize / 2 {
                cap = needed.max(self.base_capacity);
                break;
            }
        }

        if cap < size {
            cap = size;
        }

        if let Some(reuse) = self.take_cached_chunk_with_capacity(cap) {
            unsafe {
                (*reuse).offset.store(0, Ordering::Relaxed);
                loop {
                    let old_head = self.head.load(Ordering::Acquire);
                    (*reuse).next = old_head;
                    if self.head
                        .compare_exchange(old_head, reuse, Ordering::AcqRel, Ordering::Acquire)
                        .is_ok()
                    {
                        break;
                    }
                }
                // try to allocate in new head (should succeed)
                if let Some(p) = self.try_alloc_in_head(size) {
                    return Some(p);
                }
            }
        }

        let new_chunk = unsafe { Chunk::new(cap, self.align).ok()? };
        unsafe {
            loop {
                let old_head = self.head.load(Ordering::Acquire);
                let new_chunk_ptr = new_chunk.load(Ordering::Acquire);
                (*new_chunk_ptr).next = old_head;
                if self.head
                    .compare_exchange(old_head, new_chunk_ptr, Ordering::AcqRel, Ordering::Acquire)
                    .is_ok()
                {
                    break;
                }
            }
        }

        self.try_alloc_in_head(size)
    }

    #[allow(clippy::mut_from_ref)]
    pub fn alloc<T>(&self, val: T) -> Option<&mut T> {
        let size = size_of::<T>();
        let align = std::mem::align_of::<T>().max(self.align);
        // naive: ensure allocation respects type alignment by using a sized allocation with align-up
        // our chunks were created with `self.align` granularity; to be safe we request enough space
        // and then align within the chunk manually.
        let bytes = self.alloc_bytes(size + align)?;

        // align the returned pointer
        let addr = bytes as usize;
        let aligned = align_up(addr, align);
        let ptr = aligned as *mut T;
        unsafe {
            ptr.write(val);
            Some(&mut *ptr)
        }
    }

    pub fn len(&self) -> usize {
        let mut len = 0;
        let mut head = self.head.load(Ordering::Relaxed);
        while !head.is_null() {
            unsafe {
                let h = &*head;
                len += h.offset.load(Ordering::Relaxed);
                head = h.next;
            }
        }
        len
    }

    /// Allocate and copy the bytes from `data` into the arena and return a raw pointer.
    pub fn alloc_many(&self, data: &[u8]) -> Option<*mut u8> {
        let size = data.len();
        let p = self.alloc_bytes(size)?;
        unsafe {
            std::ptr::copy_nonoverlapping(data.as_ptr(), p, size);
        }
        Some(p)
    }

    /// Given a pointer returned by `alloc_many`/`alloc_bytes` and a length, create a slice.
    /// This is `unsafe` because the caller must ensure `ptr` was allocated from this arena and `len` is valid.
    pub unsafe fn get_slice_from_ptr<'a>(&self, ptr: *const u8, len: usize) -> &'a [u8] {
        // SAFETY: Caller must ensure ptr is valid and len is correct
        unsafe { std::slice::from_raw_parts(ptr, len) }
    }

    /// Returns an approximate "used" size of the head chunk. For the total used across all chunks
    /// you would need to iterate over the chunk list. This is fast and useful for quick metrics.
    pub fn head_used(&self) -> usize {
        let head = self.current_head();
        if head.is_null() { return 0; }
        unsafe { (&*head).offset.load(Ordering::Relaxed) }
    }

    /// Reset the arena. This is exclusive (requires &mut self) so it's safe to reclaim or cache chunks.
    /// Behavior: keep one chunk as the head (the base capacity chunk) and move any other chunks into the
    /// cache for reuse to reduce future allocations.
    pub fn reset(&mut self) {
        // Swap out the head and rebuild a fresh head chunk (try to reuse the base-capacity one)
        let old_head = self.head.swap(ptr::null_mut(), Ordering::AcqRel);

        // walk the list and cache chunks except for one which we will use as the new head
        let mut keep: AtomicPtr<Chunk> = AtomicPtr::new(ptr::null_mut());
        let mut cur = old_head;
        let mut cached: Vec<*mut Chunk> = Vec::new();
        while !cur.is_null() {
            unsafe {
                let next = (*cur).next;
                // choose a chunk to keep that is at least base_capacity if possible
                if (*cur).capacity >= self.base_capacity {
                    keep.store(cur, Ordering::Relaxed);
                } else {
                    // reset offset before caching
                    (*cur).offset.store(0, Ordering::Relaxed);
                    (*cur).next = ptr::null_mut();
                    cached.push(cur);
                }
                cur = next;
            }
        }

        // if we didn't find a chunk to keep, create a fresh one (try cache first)
        let x = keep.load(Ordering::Acquire);
        if x.is_null() {
            if let Some(reuse) = self.take_cached_chunk_with_capacity(self.base_capacity) {
                keep.store(reuse, Ordering::Release);
            } else if let Ok(new_chunk) = unsafe { Chunk::new(self.base_capacity, self.align) } {
                keep = new_chunk;
            } else {
                // If we can't allocate a new chunk, just return without setting a head
                // This will make the arena unusable but won't crash
                return;
            }
        }

        // reset the kept chunk's offset and set head to it
        let final_chunk = keep.load(Ordering::Acquire);
        if !final_chunk.is_null() {
            unsafe {
                (*final_chunk).offset.store(0, Ordering::Relaxed);
                (*final_chunk).next = ptr::null_mut();
            }
            self.head.store(final_chunk, Ordering::Release);
        }

        // push cached chunks into our global cache (bounded to avoid unbounded memory growth)
        if !cached.is_empty() {
            let mut global = self.cache.lock().unwrap();
            // keep cache small: at most 8 chunks
            for c in cached.drain(..) {
                if global.len() < 8 {
                    global.push(c);
                } else {
                    unsafe { Chunk::dealloc(c) }
                }
            }
        }
    }

    /// Try to grab a cached chunk with capacity >= requested. Returns Some(*mut Chunk) or None.
    fn take_cached_chunk_with_capacity(&self, min_capacity: usize) -> Option<*mut Chunk> {
        let mut cache = self.cache.lock().unwrap();
        // find an index of a chunk with capacity >= min_capacity
        if let Some(pos) = cache.iter().position(|&c| unsafe { (&*c).capacity } >= min_capacity) {
            Some(cache.swap_remove(pos))
        } else {
            // if none large enough, but there are small ones and min_capacity is small, maybe reuse any
            if !cache.is_empty() {
                Some(cache.pop().unwrap())
            } else {
                None
            }
        }
    }
}

impl Drop for GrowableAtomicBump {
    fn drop(&mut self) {
        // take head and deallocate all chunks
        let mut head = self.head.load(Ordering::Acquire);
        while !head.is_null() {
            unsafe {
                let next = (*head).next;
                Chunk::dealloc(head);
                head = next;
            }
        }

        // deallocate any cached chunks
        let mut cache = self.cache.lock().unwrap();
        while let Some(c) = cache.pop() {
            unsafe { Chunk::dealloc(c) }
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
            let s = a.get_slice_from_ptr(p, 3);
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
                    ca.alloc(42u32).unwrap();
                }
            }));
        }
        for h in handles { h.join().unwrap(); }
        // ensure we allocated something
        assert!(a.head_used() > 0);
    }
}