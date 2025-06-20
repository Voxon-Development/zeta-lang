use std::alloc::{alloc, dealloc, Layout};
use std::collections::{HashMap, HashSet};
use std::ptr::NonNull;

mod benches;

const BLOCKS: usize = 100_000;
const SIZE: usize = 64;

struct DefaultAllocator;

impl DefaultAllocator {
    fn alloc(&self, size: usize) -> *mut u8 {
        let layout = Layout::from_size_align(size, 8).unwrap();
        unsafe { alloc(layout) }
    }

    fn free(&self, ptr: *mut u8, size: usize) {
        let layout = Layout::from_size_align(size, 8).unwrap();
        unsafe { dealloc(ptr, layout) };
    }
}

#[derive(Default)]
struct MeshAllocator {
    blocks: HashMap<usize, NonNull<u8>>,
    graph: HashMap<usize, HashSet<usize>>,
    next_id: usize,
}

impl MeshAllocator {
    fn alloc(&mut self, size: usize, near: Option<usize>) -> usize {
        let layout = Layout::from_size_align(size, 8).unwrap();
        let ptr = unsafe { alloc(layout) };
        let id = self.next_id;
        self.next_id += 1;

        self.blocks.insert(id, NonNull::new(ptr).unwrap());

        if let Some(n) = near {
            self.graph.entry(n).or_default().insert(id);
        }

        id
    }

    fn access(&self, id: usize) -> *mut u8 {
        self.blocks.get(&id).unwrap().as_ptr()
    }

    fn free(&mut self, id: usize, size: usize) {
        if let Some(ptr) = self.blocks.remove(&id) {
            let layout = Layout::from_size_align(size, 8).unwrap();
            unsafe { dealloc(ptr.as_ptr(), layout) };
        }
        self.graph.remove(&id);
    }
}

fn main() {
    let start = std::time::Instant::now();
    let allocator = DefaultAllocator;
    let mut ptrs = Vec::with_capacity(BLOCKS);
    for _ in 0..BLOCKS {
        let ptr = allocator.alloc(SIZE);
        unsafe { std::ptr::write_bytes(ptr, 0xaa, SIZE) }; // simulate access
        ptrs.push(ptr);
    }
    for ptr in ptrs {
        allocator.free(ptr, SIZE);
    }
    println!("{:?}", start.elapsed().as_nanos());
    
    let start = std::time::Instant::now();
    let mut mesh = MeshAllocator::default();
    let mut ids = Vec::with_capacity(BLOCKS);
    for i in 0..BLOCKS {
        let near = if i > 0 { Some(i - 1) } else { None };
        let id = mesh.alloc(SIZE, near);
        unsafe {
            std::ptr::write_bytes(mesh.access(id), 0xaa, SIZE); // simulate access
        }
        ids.push(id);
    }
    for id in ids {
        mesh.free(id, SIZE);
    }
    println!("{:?}", start.elapsed().as_nanos());
}