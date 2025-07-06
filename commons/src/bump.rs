use std::alloc::{alloc, dealloc, AllocError, Allocator, Layout};
use std::cell::UnsafeCell;
use std::ptr::NonNull;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::{Arc, RwLock};
use trc::SharedTrc;

const DEFAULT_CHUNK_SIZE: usize = 4096;

macro_rules! reset_atomic_chunks_unrolled {
    ($chunks:expr) => {{
        let len = $chunks.len();
        let mut i = 0;
        while i + 4 <= len {
            $chunks[i].offset.store(0, Ordering::Relaxed);
            $chunks[i + 1].offset.store(0, Ordering::Relaxed);
            $chunks[i + 2].offset.store(0, Ordering::Relaxed);
            $chunks[i + 3].offset.store(0, Ordering::Relaxed);
            i += 4;
        }
        while i < len {
            $chunks[i].offset.store(0, Ordering::Relaxed);
            i += 1;
        }
    }};
}

macro_rules! reset_atomic_chunks {
    ($chunks:expr) => {
        let len = $chunks.len();
        let mut i = 0;
        while i + 4 <= len {
            $chunks[i].offset = 0;
            $chunks[i + 1].offset = 0;
            $chunks[i + 2].offset = 0;
            $chunks[i + 3].offset = 0;
            i += 4;
        }
        while i < len {
            $chunks[i].offset = 0;
            i += 1;
        }
    }
}

const ALIGNMENT: usize = 16;
const INITIAL_CHUNK_SIZE: usize = 16 * 1024; // 16 KB

struct Chunk {
    memory: *mut u8,
    capacity: usize,
    offset: usize,
}

impl Chunk {
    #[inline(always)]
    fn new(size: usize) -> Self {
        let layout = unsafe { Layout::from_size_align_unchecked(size, ALIGNMENT) };
        let memory = unsafe { alloc(layout) };
        if memory.is_null() {
            panic!("Failed to allocate memory");
        }

        Self {
            memory,
            capacity: size,
            offset: 0,
        }
    }

    #[inline(always)]
    fn try_alloc(&mut self, size: usize, align: usize) -> Option<*mut u8> {
        let aligned = (self.offset + align - 1) & !(align - 1);
        let next = aligned + size;
        if next > self.capacity {
            return None;
        }
        self.offset = next;
        Some(unsafe { self.memory.add(aligned) })
    }
}

impl Clone for Chunk {
    fn clone(&self) -> Self {
        Self {
            memory: self.memory,
            capacity: self.capacity,
            offset: self.offset,
        }
    }
}

impl Drop for Chunk {
    fn drop(&mut self) {
        let layout = unsafe { Layout::from_size_align_unchecked(self.capacity, ALIGNMENT) };
        unsafe { dealloc(self.memory, layout) };
    }
}

#[repr(transparent)]
pub struct Bump {
    inner: UnsafeCell<BumpInner>,
}

#[derive(Clone)]
struct BumpInner {
    chunks: Vec<Chunk>,
    current: usize,
}

impl Default for BumpInner {
    fn default() -> Self {
        Self {
            chunks: vec![Chunk::new(INITIAL_CHUNK_SIZE)],
            current: 0,
        }
    }
}

impl Clone for Bump {
    fn clone(&self) -> Self {
        Self {
            inner: UnsafeCell::new(self.get_mut().clone()),
        }
    }
}

impl Default for Bump {
    fn default() -> Self {
        Self::new()
    }
}

impl Bump {
    #[inline(always)]
    pub fn new() -> Self {
        Self {
            inner: UnsafeCell::new(BumpInner::default()),
        }
    }

    #[inline(always)]
    fn get_mut(&self) -> &mut BumpInner {
        unsafe { &mut *self.inner.get() }
    }

    #[inline(always)]
    pub fn alloc(&self, size: usize, align: usize) -> *mut u8 {
        let inner = self.get_mut();

        // Try current chunk
        if let Some(ptr) = inner.chunks[inner.current].try_alloc(size, align) {
            return ptr;
        }

        // Fallback: try other existing chunks
        for (i, chunk) in inner.chunks.iter_mut().enumerate() {
            if let Some(ptr) = chunk.try_alloc(size, align) {
                inner.current = i;
                return ptr;
            }
        }

        // Need a new chunk
        let new_capacity = size.max(INITIAL_CHUNK_SIZE).next_power_of_two();
        let mut new_chunk = Chunk::new(new_capacity);
        let ptr = new_chunk
            .try_alloc(size, align)
            .expect("Allocation failed in new chunk");

        inner.chunks.push(new_chunk);
        inner.current = inner.chunks.len() - 1;

        ptr
    }

    #[inline(always)]
    pub fn reset(&self) {
        let inner = self.get_mut();
        for chunk in &mut inner.chunks {
            chunk.offset = 0;
        }
        inner.current = 0;
    }
}

impl Drop for Bump {
    fn drop(&mut self) {
        let inner = self.get_mut();
        for chunk in inner.chunks.drain(..) {
            drop(chunk);
        }
    }
}

unsafe impl Allocator for Bump {
    #[inline(always)]
    fn allocate(&self, layout: Layout) -> Result<NonNull<[u8]>, AllocError> {
        let ptr = self.alloc(layout.size(), layout.align());

        let slice = NonNull::new(std::ptr::slice_from_raw_parts_mut(ptr, layout.size())).ok_or(AllocError)?;
        Ok(slice)
    }

    #[inline(always)]
    unsafe fn deallocate(&self, _ptr: NonNull<u8>, _layout: Layout) {
        // No-op
    }
}


//
// ─── ATOMIC CHUNK ──────────────────────────────────────────────────────────────
//

pub struct AtomicChunk {
    memory: SharedTrc<UnsafeCell<Box<[u8]>>>,
    capacity: usize,
    offset: AtomicUsize,
}

impl Clone for AtomicChunk {
    fn clone(&self) -> Self {
        Self {
            memory: self.memory.clone(),
            capacity: self.capacity,
            offset: AtomicUsize::new(self.offset.load(Ordering::Relaxed)),
        }
    }
}

unsafe impl Send for AtomicChunk {}
unsafe impl Sync for AtomicChunk {}

impl Default for AtomicChunk {
    fn default() -> Self {
        Self::new(DEFAULT_CHUNK_SIZE)
    }
}

impl AtomicChunk {
    fn new(size: usize) -> Self {
        let mut vec: Vec<u8> = Vec::with_capacity(size);
        unsafe { vec.set_len(size); } // SAFETY: We'll use UnsafeCell
        let memory: SharedTrc<UnsafeCell<Box<[u8]>>> = SharedTrc::new(UnsafeCell::new(vec.into_boxed_slice()));

        Self {
            memory,
            capacity: size,
            offset: AtomicUsize::new(0),
        }
    }

    #[inline(always)]
    fn try_alloc(&self, size: usize, align: usize) -> Option<*mut u8> {
        loop {
            let current = self.offset.load(Ordering::Relaxed);
            let aligned = (current + align - 1) & !(align - 1);
            let next = aligned + size;

            if next > self.capacity {
                return None;
            }

            if self.offset.compare_exchange_weak(
                current,
                next,
                Ordering::SeqCst,
                Ordering::Relaxed,
            ).is_ok() {
                unsafe {
                    let ptr = (*self.memory.get()).as_mut_ptr().add(aligned);
                    return Some(ptr);
                }
            }
        }
    }
}

//
// ─── ATOMIC BUMP ALLOCATOR (THREAD SAFE, RESIZABLE) ────────────────────────────
//

pub struct AtomicBump {
    chunks: RwLock<Vec<SharedTrc<AtomicChunk>>>,
}



impl Clone for AtomicBump {
    fn clone(&self) -> Self {
        let chunks = self.chunks.read().unwrap(); // RwLock read guard
        Self {
            chunks: RwLock::new(chunks.clone()),
        }
    }
}

unsafe impl Send for AtomicBump {}
unsafe impl Sync for AtomicBump {}

impl Default for AtomicBump {
    fn default() -> Self {
        Self::new()
    }
}

impl AtomicBump {
    #[inline(always)]
    #[allow(clippy::arc_with_non_send_sync)]
    pub fn new() -> Self {
        let first_chunk = SharedTrc::new(AtomicChunk::new(DEFAULT_CHUNK_SIZE));
        Self {
            chunks: RwLock::new(vec![first_chunk]),
        }
    }

    #[inline(always)]
    #[allow(clippy::arc_with_non_send_sync)]
    pub fn with_capacity(cap: usize) -> Self {
        let first_chunk = SharedTrc::new(AtomicChunk::new(cap));
        Self {
            chunks: RwLock::new(vec![first_chunk]),
        }
    }

    #[inline(always)]
    pub fn get_slice(&self, offset: usize, length: usize) -> &[u8] {
        let chunks = self.chunks.read().unwrap();
        let chunk: SharedTrc<AtomicChunk> = chunks.last().unwrap().clone();
        unsafe { std::slice::from_raw_parts((*chunk.memory.get()).as_mut_ptr().add(offset), length) }
    }

    #[inline(always)]
    #[allow(clippy::arc_with_non_send_sync)]
    pub fn alloc(&self, size: usize, align: usize) -> *mut u8 {
        {
            let chunks = self.chunks.read().unwrap();
            for chunk in chunks.iter().rev() {
                if let Some(ptr) = chunk.try_alloc(size, align) {
                    return ptr;
                }
            }
        }

        let new_capacity = size.max(DEFAULT_CHUNK_SIZE).next_power_of_two();
        let new_chunk = SharedTrc::new(AtomicChunk::new(new_capacity));
        {
            let mut chunks = self.chunks.write().unwrap();
            chunks.push(new_chunk.clone());
        }

        new_chunk
            .try_alloc(size, align)
            .expect("Failed to allocate in new atomic chunk")
    }

    #[inline(always)]
    pub fn reset(&self) {
        let chunks = self.chunks.read().unwrap();
        reset_atomic_chunks_unrolled!(chunks);
    }
}

// The following code can't be implemented as a macro because
// `the size for values of type `[u8]` cannot be known at compilation time [E0277]`
// Manual implementation is required to prevent such errors.
unsafe impl Allocator for AtomicBump {
    fn allocate(&self, layout: Layout) -> Result<NonNull<[u8]>, AllocError> {
        let ptr: *mut u8 = self.alloc(layout.size(), layout.align());
        if ptr.is_null() {
            Err(AllocError)
        } else {
            // SAFETY: `ptr` is non-null and points to at least `layout.size()` bytes
            let slice = std::ptr::slice_from_raw_parts_mut(ptr, layout.size());
            Ok(NonNull::new(slice).unwrap())
        }
    }

    #[inline(always)] // This is a no-op and we should just inline so it is as if we never even called this :P
    unsafe fn deallocate(&self, _ptr: NonNull<u8>, _layout: Layout) {
        // Empty, deallocation is handled by `reset`
    }
}