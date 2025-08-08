use crate::bump::{align_up, Bump, DEFAULT_CHUNK_SIZE};

#[repr(C)]
pub struct Arena {
    chunks: Vec<Bump>,
    chunk_index: usize,
    capacity: usize,
}

impl Arena {
    #[inline(always)]
    pub fn new() -> Self {
        Self::with_capacity(DEFAULT_CHUNK_SIZE)
    }

    #[inline(always)]
    pub fn with_capacity(capacity: usize) -> Self {
        let mut chunks = Vec::with_capacity(1);
        chunks.push(Bump::with_capacity(capacity));
        Self { chunks, chunk_index: 0, capacity }
    }

    #[inline(always)]
    pub fn alloc<T>(&mut self, val: T) -> &mut T {
        let chunk = &mut self.chunks[self.chunk_index];

        // Inline fits check
        let align = core::mem::align_of::<T>();
        let size = core::mem::size_of::<T>();
        let ptr = chunk.ptr.as_ptr() as usize;
        let base = ptr + chunk.offset;
        let aligned = align_up(base, align);
        let end = aligned + size;

        if core::hint::likely(end <= chunk.capacity) {
            // Fast path: allocate in current chunk
            chunk.offset = end - ptr;
            let ptr = aligned as *mut T;
            unsafe {
                ptr.write(val);
                return &mut *ptr;
            }
        }

        // Slow path: allocate a new chunk and retry
        self.chunk_index += 1;
        if self.chunk_index == self.chunks.len() {
            self.chunks.push(Bump::with_capacity(self.capacity * 2));
        }

        let new_chunk = &mut self.chunks[self.chunk_index];
        let ptr = new_chunk.ptr.as_ptr() as usize;
        let base = ptr;
        let aligned = align_up(base, align);
        let end = aligned + size;

        debug_assert!(end <= new_chunk.capacity, "Chunk too small for allocation");

        new_chunk.offset = end - ptr;
        let ptr = aligned as *mut T;
        unsafe {
            ptr.write(val);
            &mut *ptr
        }
    }
}
