use crate::vm::{Allocator, GarbageCollector, Int, Ptr, RecordSignature};

/// number of words used for the header of allocated memory blocks
pub const BLOCK_HEADER_SIZE: usize = 1;

/// heap address 0 is unused and always set to a constant value
const HEAP_ADDR0_VALUE: Int = 1337;

/// reserved type signature that indicates relocated blocks
const RELOC_MARKER: Int = Int::MAX;

/// Allocator to complement the CopyCollector
#[derive(Debug)]
pub struct CopyAllocator;

impl Allocator for CopyAllocator {
    fn init_heap(&self, size: usize) -> Vec<Int> {
        let mut heap = Vec::with_capacity(size);
        heap.push(HEAP_ADDR0_VALUE);
        heap
    }

    fn size_of(&self, rs: RecordSignature) -> usize {
        BLOCK_HEADER_SIZE + rs.size()
    }

    /** Allocate a block of the types size + BLOCK_HEADER_SIZE.
    The header stores how many primitive and pointer fields the data contains.
    Return pointer to the next word, where the data starts: First all primitives, then all pointers.
    **/
    fn alloc(&self, rs: RecordSignature, heap: &mut Vec<Int>) -> Int {
        if rs.size() == 0 {
            return 0;
        }

        let ptr = heap.len();

        // block header
        heap.push(rs.as_int());

        // block data
        // we make sure to initialize newly allocated memory to 0 so the GC won't try to
        // preserve any uninitialized pointers.
        heap.extend(vec![0; rs.size()]);

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

        if size == 0 {
            return 0;
        }

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
        let used_after = heap.len();

        let capacity = heap.capacity();
        let freed = used_before - heap.len();
        let available = capacity - heap.len();
        println!("GC -- Heap size: {capacity}, {used_after} used, {freed} collected");
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn dont_allocate_empty_records() {
        let mut heap = vec![];

        let ptr = CopyAllocator.alloc(RecordSignature::new(0, 0), &mut heap);

        assert_eq!(ptr, 0);
        assert!(heap.is_empty());
    }

    #[test]
    fn allocate_at_end_of_heap() {
        let mut heap = vec![1, 2, 3];
        let rs = RecordSignature::new(1, 1);

        let ptr = CopyAllocator.alloc(rs, &mut heap);

        assert_eq!(ptr, 4);
        assert_eq!(heap, [1, 2, 3, rs.as_int(), 0, 0]);
    }

    #[test]
    fn dont_relocate_null_pointers() {
        let mut cc = CollectionContext {
            source: &mut vec![],
            target: &mut vec![],
        };

        let ptr = cc.relocate(0);

        assert_eq!(ptr, 0);
    }

    #[test]
    fn follow_previously_relocated_pointer() {
        const RELOCATED_TO: Int = 7;
        let mut cc = CollectionContext {
            source: &mut vec![RELOC_MARKER, RELOCATED_TO],
            target: &mut vec![],
        };

        let ptr = cc.relocate(1);

        assert_eq!(ptr, RELOCATED_TO);
    }

    #[test]
    fn relocate_to_end_of_target() {
        const TARGET_LEN: usize = 3;
        let rs = RecordSignature::new(3, 0).as_int();
        let mut cc = CollectionContext {
            source: &mut vec![rs, 41, 42, 43],
            target: &mut vec![0; TARGET_LEN],
        };

        let ptr = cc.relocate(1) as usize;

        assert_eq!(cc.target[TARGET_LEN..], [rs, 41, 42, 43]);
        assert_eq!(cc.target[ptr], 41)
    }

    #[test]
    fn point_to_new_location() {
        let rs = RecordSignature::new(1, 0).as_int();
        let mut cc = CollectionContext {
            source: &mut vec![0, rs, 42],
            target: &mut vec![],
        };

        cc.relocate(2);

        assert_eq!(cc.source, &[0, RELOC_MARKER, 1])
    }

    #[test]
    fn empty_structures_dont_relocate() {
        let rs = RecordSignature::new(0, 0).as_int();
        let mut cc = CollectionContext {
            source: &mut vec![0, rs, rs],
            target: &mut vec![],
        };

        let ptr1 = cc.relocate(2);
        let ptr2 = cc.relocate(3);

        assert_eq!(ptr1, 0);
        assert_eq!(ptr2, 0);

        assert_eq!(cc.source, &[0, rs, rs]);
        assert_eq!(cc.target, &[]);
    }

    #[test]
    fn no_roots_empty_heap() {
        let mut cc = CollectionContext {
            source: &mut vec![1, 2, 3, 4, 5, 6, 7],
            target: &mut vec![],
        };

        cc.copy_reachable(&mut []);

        assert_eq!(cc.target, &[]);
    }

    #[test]
    fn copy_roots() {
        let rs = RecordSignature::new(1, 0).as_int();
        let mut cc = CollectionContext {
            source: &mut vec![rs, 11, rs, 22],
            target: &mut vec![],
        };
        let mut roots = [3, 1];

        cc.copy_reachable(&mut roots);

        assert_eq!(roots, [1, 3]);
        assert_eq!(cc.target, &[rs, 22, rs, 11]);
    }

    #[test]
    fn copy_reachable_objects() {
        let rs = RecordSignature::new(0, 2).as_int();
        let mut cc = CollectionContext {
            source: &mut vec![
                rs, 0, 0, rs, 0, 10, rs, 19, 0, rs, 0, 16, rs, 1, 19, rs, 0, 4, rs, 1, 7,
            ],
            target: &mut vec![],
        };
        let mut roots = [13];

        cc.copy_reachable(&mut roots);

        assert_eq!(roots, [1]);
        assert_eq!(cc.target, &[rs, 4, 7, rs, 0, 0, rs, 4, 10, rs, 7, 0]);
    }
}
