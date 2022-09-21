use crate::{Allocator, GarbageCollector, Half, Int, Ptr, RecordSignature};

/// number of words used for the header of allocated memory blocks
pub const BLOCK_HEADER_SIZE: usize = 1;

/// heap address 0 is unused and always set to a constant value
const HEAP_ADDR0_VALUE: Int = 1337;

/// reserved type signature that indicates relocated blocks
const RELOC_MARKER: Int = Int::MAX;

/// Allocator to complement the CopyCollector
pub struct CopyAllocator;

impl Allocator for CopyAllocator {
    fn init_heap(&self, size: usize) -> Vec<Int> {
        let mut heap = Vec::with_capacity(size);
        heap.push(HEAP_ADDR0_VALUE);
        heap
    }

    fn size_of(&self, n_int: Half, n_ptr: Half) -> usize {
        BLOCK_HEADER_SIZE + n_int as usize + n_ptr as usize
    }

    /** Allocate a block of the types size + BLOCK_HEADER_SIZE.
    The header stores how many primitive and pointer fields the data contains.
    Return pointer to the next word, where the data starts: First all primitives, then all pointers.
    **/
    fn alloc(&self, n_int: Half, n_ptr: Half, heap: &mut Vec<Int>) -> Int {
        let size = n_int + n_ptr;
        let ptr = heap.len();

        // block header
        heap.push(RecordSignature::new(n_int, n_ptr).as_int());

        // block data
        for _ in 0..size {
            heap.push(0);
        }

        (BLOCK_HEADER_SIZE + ptr) as Int
    }
}

#[derive(Debug)]
pub struct CopyCollector;

impl GarbageCollector for CopyCollector {
    fn collect(&self, roots: &mut [Ptr], heap: &mut Vec<Int>) {
        let mut target = Vec::with_capacity(heap.capacity());
        target.push(HEAP_ADDR0_VALUE);

        let mut cc = CollectionContext {
            source: heap,
            target: &mut target,
        };

        cc.copy_reachable(roots);

        *heap = target;
    }
}

struct CollectionContext<'a> {
    source: &'a mut Vec<Int>,
    target: &'a mut Vec<Int>,
}

impl<'s> CollectionContext<'s> {
    fn copy_reachable(&mut self, roots: &mut [Ptr]) {
        let mut trace_ptr = self.target.len();

        for x in roots {
            let p = *x;
            *x = self.relocate(p);
        }

        while trace_ptr < self.target.len() {
            let rs = RecordSignature::from_int(self.target[trace_ptr]);
            trace_ptr += BLOCK_HEADER_SIZE;
            trace_ptr += rs.n_primitive() as usize;

            for _ in 0..rs.n_pointer() {
                self.target[trace_ptr] = self.relocate(self.target[trace_ptr]);
                trace_ptr += 1;
            }
        }
    }

    fn relocate(&mut self, ptr: Int) -> Int {
        let ptr = ptr as usize;

        if ptr == 0 {
            return 0;
        }

        if self.source[ptr - BLOCK_HEADER_SIZE] == RELOC_MARKER {
            return self.source[ptr];
        }

        let new_ptr = (self.target.len() + BLOCK_HEADER_SIZE) as Int;
        let start = ptr - BLOCK_HEADER_SIZE;
        let size = RecordSignature::from_int(self.source[start]).size();
        self.target
            .extend_from_slice(&self.source[start..ptr + size]);

        self.source[start] = RELOC_MARKER;
        self.source[ptr] = new_ptr;

        new_ptr
    }
}

#[derive(Debug)]
pub struct ChattyCollector<T: GarbageCollector> {
    collector: T,
}

impl<T: GarbageCollector> ChattyCollector<T> {
    pub fn new(collector: T) -> Self {
        ChattyCollector { collector }
    }
}

impl<T: GarbageCollector> GarbageCollector for ChattyCollector<T> {
    fn collect(&self, roots: &mut [Ptr], heap: &mut Vec<Int>) {
        let used_before = heap.len();
        self.collector.collect(roots, heap);

        let capacity = heap.capacity();
        let freed = used_before - heap.len();
        let available = capacity - heap.len();
        println!("GC -- Heap size: {capacity}, {available} available ({freed} freed)");
    }
}
