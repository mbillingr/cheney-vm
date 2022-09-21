use std::collections::HashMap;
use std::hash::Hash;

pub type Int = u64;
pub type Ptr = u64;
pub type Half = u32;

const HEAP_ADDR0_VALUE: Int = 1337; // heap address 0 is unused and always set to a constant value

const RELOC_MARKER: Int = Int::MAX;

/// number of words used for the header of allocated memory blocks
const BLOCK_HEADER_SIZE: usize = 1;

const INITIAL_HEAP_SIZE: usize = 8;

/// Registers
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum R {
    Val,
    Ptr,
    Obj,
    Arg,
    Lcl,
    Cls,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Op<T> {
    Halt,
    Label(T),
    GetAddr(T),
    Goto(T),
    Jump,

    Alloc(RecordSignature),
    GetVal(R, Half),
    PutVal(R, Half),

    Const(Int),
    Copy(R, R),
}

impl<T> Op<T> {
    pub fn is_label(&self) -> bool {
        match self {
            Op::Label(_) => true,
            _ => false,
        }
    }

    fn convert<U>(&self) -> Op<U> {
        match self {
            Op::Label(_) | Op::GetAddr(_) | Op::Goto(_) => panic!("Invalid conversion"),
            Op::Halt => Op::Halt,
            Op::Jump => Op::Jump,
            Op::Alloc(s) => Op::Alloc(*s),
            Op::GetVal(r, idx) => Op::GetVal(*r, *idx),
            Op::PutVal(r, idx) => Op::PutVal(*r, *idx),
            Op::Const(x) => Op::Const(*x),
            Op::Copy(a, b) => Op::Copy(*a, *b),
        }
    }
}

pub fn transform_labels<T: Eq + Hash>(code: &[Op<T>]) -> impl Iterator<Item = Op<Int>> + '_ {
    let labels = find_label_offsets(code);
    code.iter().filter_map(move |op| match op {
        Op::Label(_) => None,
        Op::Goto(label) => Some(Op::Goto(labels[label])),
        Op::GetAddr(label) => Some(Op::GetAddr(labels[label])),
        _ => Some(op.convert()),
    })
}

fn find_label_offsets<T: Eq + Hash>(code: &[Op<T>]) -> HashMap<&T, Int> {
    let mut offset = 0;
    let mut labels = HashMap::new();
    for op in code {
        match op {
            Op::Label(label) => {
                labels.insert(label, offset);
            }
            _ => offset += 1,
        }
    }
    labels
}

#[derive(Debug)]
pub struct Vm<GC: GarbageCollector> {
    gc: GC,
    heap: Vec<Int>,

    // registers
    val: Int,
    ptr: Ptr,
    obj: Ptr,
    arg: Ptr,
    lcl: Ptr,
    cls: Ptr,
}

impl Default for Vm<CopyCollector> {
    fn default() -> Self {
        Self::new(CopyCollector)
    }
}

impl<GC: GarbageCollector> Vm<GC> {
    pub fn new(gc: GC) -> Self {
        let mut heap = Vec::with_capacity(INITIAL_HEAP_SIZE);
        heap.push(HEAP_ADDR0_VALUE);

        Vm {
            gc,
            heap,
            val: 0,
            ptr: 0,
            obj: 0,
            arg: 0,
            lcl: 0,
            cls: 0,
        }
    }

    pub fn run(&mut self, program: &[Op<Int>]) -> Int {
        let mut ip = 0;
        loop {
            let op = program[ip];
            ip += 1;
            match op {
                Op::Halt => return self.val,
                Op::GetAddr(pos) => self.val = pos,
                Op::Goto(pos) => ip = pos as usize,
                Op::Jump => ip = self.val as usize,
                Op::Alloc(s) => self.ptr = self.alloc(s.n_primitive(), s.n_pointer()),
                Op::GetVal(r, idx) => self.val = self.get_field(r, idx),
                Op::PutVal(r, idx) => self.set_field(r, idx, self.val),
                Op::Const(x) => self.val = x,
                Op::Copy(R::Arg, R::Lcl) => self.lcl = self.arg,
                Op::Copy(R::Obj, R::Val) => self.val = self.obj,
                Op::Copy(R::Obj, R::Arg) => self.arg = self.obj,
                Op::Copy(R::Obj, R::Cls) => self.cls = self.obj,
                Op::Copy(R::Val, R::Obj) => self.obj = self.val,
                Op::Copy(R::Ptr, R::Arg) => self.arg = self.ptr,
                Op::Copy(R::Ptr, R::Obj) => self.obj = self.ptr,
                _ => todo!("{:?}", op),
            }
        }
    }

    fn set_field(&mut self, r: R, offset: Half, val: Int) {
        let obj = self.get_pointer(r);
        self.heap[obj as usize + offset as usize] = val
    }

    fn get_field(&mut self, r: R, offset: Half) -> Int {
        let obj = self.get_pointer(r);
        self.heap[obj as usize + offset as usize]
    }

    fn get_pointer(&mut self, r: R) -> Ptr {
        match r {
            R::Val => panic!("VAL accessed as pointer"),
            R::Ptr => self.ptr,
            R::Obj => self.obj,
            R::Arg => self.arg,
            R::Lcl => self.lcl,
            R::Cls => self.cls,
        }
    }

    /** Allocate a block of the types size + BLOCK_HEADER_SIZE.
    The header stores how many primitive and pointer fields the data contains.
    Return pointer to the next word, where the data starts: First all primitives, then all pointers.
    **/
    fn alloc(&mut self, n_int: Half, n_ptr: Half) -> Int {
        let size = n_int + n_ptr;
        let total_size = BLOCK_HEADER_SIZE + size as usize;

        if self.available_heap() < total_size {
            self.collect_garbage();

            if self.available_heap() < total_size {
                let wanted_cap = self.heap.capacity() * 2;
                let needed_cap = self.heap.len() + total_size;
                let target_cap = std::cmp::max(wanted_cap, needed_cap);
                self.heap.reserve(target_cap - self.heap.len());
            }
        }

        let ptr = self.heap.len();

        // block header
        self.heap.push(RecordSignature::new(n_int, n_ptr).as_int());

        // block data
        for _ in 0..size {
            self.heap.push(0);
        }

        (BLOCK_HEADER_SIZE + ptr) as Int
    }

    fn collect_garbage(&mut self) {
        /*println!(
            "Before collect: {} total, {} used, {} free",
            self.heap.capacity(),
            self.heap.len(),
            self.available_heap()
        );*/
        let mut roots = [self.ptr, self.obj, self.arg, self.lcl];

        self.gc.collect(&mut roots, &mut self.heap);

        self.ptr = roots[0];
        self.obj = roots[1];
        self.arg = roots[2];
        self.lcl = roots[3];
        /*println!(
            "After collect: {} total, {} used, {} free",
            self.heap.capacity(),
            self.heap.len(),
            self.available_heap()
        );*/
    }

    fn available_heap(&self) -> usize {
        self.heap.capacity() - self.heap.len()
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct RecordSignature {
    n_primitive: Half,
    n_pointer: Half,
}

impl RecordSignature {
    fn new(n_primitive: Half, n_pointer: Half) -> Self {
        RecordSignature {
            n_primitive,
            n_pointer,
        }
    }

    fn from_int(x: Int) -> Self {
        RecordSignature::new(
            (x / (Half::MAX as Int)) as Half,
            (x % (Half::MAX as Int)) as Half,
        )
    }

    fn as_int(&self) -> Int {
        (self.n_primitive as Int) * (Half::MAX as Int) + (self.n_pointer as Int)
    }

    fn n_primitive(&self) -> Half {
        self.n_primitive
    }

    fn n_pointer(&self) -> Half {
        self.n_pointer
    }

    fn size(&self) -> usize {
        self.n_primitive as usize + self.n_pointer as usize
    }
}

pub trait GarbageCollector {
    fn collect(&mut self, roots: &mut [Ptr], heap: &mut Vec<Int>);
}

#[derive(Debug)]
pub struct CopyCollector;
impl GarbageCollector for CopyCollector {
    fn collect(&mut self, roots: &mut [Ptr], heap: &mut Vec<Int>) {
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
            *x = self.reloc_value(*x);
        }

        while trace_ptr < self.target.len() {
            let rs = RecordSignature::from_int(self.target[trace_ptr]);
            trace_ptr += BLOCK_HEADER_SIZE;
            trace_ptr += rs.n_primitive() as usize;

            for _ in 0..rs.n_pointer() {
                self.reloc_field(trace_ptr);
                trace_ptr += 1;
            }
        }
    }

    fn reloc_value(&mut self, p: Ptr) -> Ptr {
        self.reloc(p)
    }

    fn reloc_field(&mut self, ptr: usize) {
        self.target[ptr] = self.reloc(self.target[ptr])
    }

    fn reloc(&mut self, p: Int) -> Int {
        if p == 0 {
            return 0;
        }

        let new_ptr = self.relocate_pointer(p as usize);
        new_ptr as Int
    }

    fn relocate_pointer(&mut self, ptr: usize) -> usize {
        if self.source[ptr - BLOCK_HEADER_SIZE] == RELOC_MARKER {
            return self.source[ptr] as usize;
        }

        let new_ptr = self.target.len() + BLOCK_HEADER_SIZE;
        let start = ptr as usize - BLOCK_HEADER_SIZE;
        let size = RecordSignature::from_int(self.source[start]).size();
        self.target
            .extend_from_slice(&self.source[start..ptr + size]);

        self.source[start] = RELOC_MARKER;
        self.source[ptr] = new_ptr as Int;

        new_ptr
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_transform_string_labels_to_offsets() {
        assert_eq!(transform_labels::<()>(&[]).collect::<Vec<_>>(), vec![]);
        assert_eq!(
            transform_labels(&[Op::Label("A")]).collect::<Vec<_>>(),
            vec![]
        );
        assert_eq!(
            transform_labels(&[Op::Label("A"), Op::Goto("A")]).collect::<Vec<_>>(),
            vec![Op::Goto(0)]
        );
        assert_eq!(
            transform_labels(&[Op::Goto("C"), Op::Label("B"), Op::Label("C"), Op::Goto("B")])
                .collect::<Vec<_>>(),
            vec![Op::Goto(1), Op::Goto(1)]
        );
    }

    #[test]
    fn test_run_trivial_program() {
        let mut vm = Vm::default();

        let res = vm.run(&[Op::Const(42), Op::Halt]);

        assert_eq!(res, 42);
    }

    #[test]
    fn test_set_obj_to_val() {
        let mut vm = Vm::default();

        vm.run(&[Op::Const(123), Op::Copy(R::Val, R::Obj), Op::Halt]);

        assert_eq!(vm.obj, 123);
    }

    #[test]
    fn test_set_val_to_obj() {
        let mut vm = Vm::default();

        vm.obj = 456;
        vm.run(&[Op::Copy(R::Obj, R::Val), Op::Halt]);

        assert_eq!(vm.val, 456);
    }

    #[test]
    fn test_alloc_on_top_of_heap() {
        let mut vm = Vm::default();
        let heap_size_before_alloc = vm.heap.len();
        let rs = RecordSignature::new(2, 0);

        vm.run(&[Op::Alloc(rs), Op::Halt]);

        assert_eq!(vm.heap[heap_size_before_alloc], rs.as_int());
        assert_eq!(vm.heap.len() - heap_size_before_alloc, 3);
    }

    #[test]
    fn test_initialize_obj() {
        let mut vm = Vm::default();

        vm.run(&[
            Op::Alloc(RecordSignature::new(2, 0)),
            Op::Copy(R::Ptr, R::Obj),
            Op::Const(12),
            Op::PutVal(R::Obj, 0),
            Op::Const(34),
            Op::PutVal(R::Obj, 1),
            Op::Halt,
        ]);

        assert_eq!(vm.heap.pop().unwrap(), 34);
        assert_eq!(vm.heap.pop().unwrap(), 12);
    }

    #[test]
    fn test_gc_object_reachable_through_val() {
        let mut vm = Vm::default();
        let rs = RecordSignature::new(2, 0);
        vm.run(&[Op::Alloc(rs), Op::Halt]);
        let irs = rs.as_int();
        assert_eq!(&vm.heap[1..], &[irs, 0, 0]);

        vm.collect_garbage();

        assert_eq!(&vm.heap[1..], &[irs, 0, 0]);
    }

    #[test]
    fn test_gc_object_not_reachable() {
        let mut vm = Vm::default();
        let rs = RecordSignature::new(2, 0);
        let irs = rs.as_int();
        vm.run(&[Op::Alloc(rs), Op::Halt]);
        vm.ptr = 0;
        assert_eq!(&vm.heap[1..], &[irs, 0, 0]);

        vm.collect_garbage();

        assert_eq!(&vm.heap[1..], &[]);
    }

    #[test]
    fn test_gc_nested_objects_not_reachable() {
        let mut vm = Vm::default();
        let rs = RecordSignature::new(0, 1);
        let irs = rs.as_int();
        vm.run(&[
            Op::Alloc(rs),
            Op::Copy(R::Ptr, R::Obj),
            Op::Const(0),
            Op::PutVal(R::Obj, 0),
            Op::Copy(R::Obj, R::Val),
            Op::Alloc(rs),
            Op::Copy(R::Ptr, R::Obj),
            Op::PutVal(R::Obj, 0),
            Op::Halt,
        ]);
        vm.ptr = 0;
        vm.obj = 0;
        assert_eq!(&vm.heap[1..], &[irs, 0, irs, 2]);

        vm.collect_garbage();

        assert_eq!(&vm.heap[1..], &[]);
    }

    #[test]
    fn test_gc_nested_objects_reachable() {
        let mut vm = Vm::default();
        let rs = RecordSignature::new(0, 1);
        let irs = rs.as_int();
        vm.run(&[
            Op::Alloc(rs),
            Op::Copy(R::Ptr, R::Obj),
            Op::Const(0),
            Op::PutVal(R::Obj, 0),
            Op::Copy(R::Obj, R::Val),
            Op::Alloc(rs),
            Op::Copy(R::Ptr, R::Obj),
            Op::PutVal(R::Obj, 0),
            Op::Halt,
        ]);
        vm.val = 0;

        // the second object comes later on the heap
        assert_eq!(vm.obj, 4);
        assert_eq!(&vm.heap[1..], &[irs, 0, irs, 2]);

        vm.collect_garbage();

        // the second object is moved to the front of the heap
        assert_eq!(vm.obj, 2);
        assert_eq!(&vm.heap[1..], &[irs, 4, irs, 0]);
    }

    #[test]
    fn test_gc_multiple_pointers_to_same_object() {
        let mut vm = Vm::default();
        let rs = RecordSignature::new(0, 2);
        let irs = rs.as_int();
        vm.run(&[
            Op::Alloc(rs),
            Op::Copy(R::Ptr, R::Obj),
            Op::Const(0),
            Op::PutVal(R::Obj, 0),
            Op::PutVal(R::Obj, 1),
            Op::Copy(R::Obj, R::Val),
            Op::Alloc(rs),
            Op::Copy(R::Ptr, R::Obj),
            Op::PutVal(R::Obj, 0),
            Op::PutVal(R::Obj, 1),
            Op::Halt,
        ]);

        assert_eq!(vm.obj, 5);
        assert_eq!(&vm.heap[1..], &[irs, 0, 0, irs, 2, 2]);

        vm.collect_garbage();

        assert_eq!(vm.obj, 2);
        assert_eq!(&vm.heap[1..], &[irs, 5, 5, irs, 0, 0]);
    }

    #[test]
    fn test_gc_self_referential_objects() {
        let mut vm = Vm::default();
        let rs = RecordSignature::new(1, 1);
        let irs = rs.as_int();
        vm.run(&[
            Op::Alloc(rs),
            Op::Copy(R::Ptr, R::Obj),
            Op::Copy(R::Obj, R::Val),
            Op::PutVal(R::Obj, 1),
            Op::Const(111),
            Op::PutVal(R::Obj, 0),
            Op::Alloc(rs),
            Op::Copy(R::Ptr, R::Obj),
            Op::Copy(R::Obj, R::Val),
            Op::PutVal(R::Obj, 1),
            Op::Const(222),
            Op::PutVal(R::Obj, 0),
            Op::Halt,
        ]);

        assert_eq!(&vm.heap[1..], &[irs, 111, 2, irs, 222, 5]);
        assert_eq!(vm.obj, 5);

        // the object with 111 is not reachable from outside and should be collected
        vm.collect_garbage();

        assert_eq!(&vm.heap[1..], &[irs, 222, 2]);
        assert_eq!(vm.obj, 2);
    }

    #[test]
    fn test_heap_capacity_stabilizes_with_automatic_gc() {
        let mut vm = Vm::default();
        vm.heap.reserve(1000);
        let rs = RecordSignature::new(100, 0);

        let mut heap_capacities = vec![];

        for _ in 0..100 {
            vm.run(&[Op::Alloc(rs), Op::Halt]);
            heap_capacities.push(vm.heap.capacity());
        }

        assert_eq!(heap_capacities.pop(), heap_capacities.pop());
    }

    #[test]
    fn test_alloc_resizes_the_heap_if_data_cant_fit() {
        let mut vm = Vm::default();

        vm.alloc(1000, 0);

        assert!(vm.heap.capacity() >= 1000);
    }

    #[test]
    fn test_alloc_triggers_garbage_collection_when_heap_is_full() {
        let mut vm = Vm::default();
        let free = vm.available_heap();
        assert!(free > BLOCK_HEADER_SIZE); // just to make sure the initial heap size is sane
        vm.alloc((free - BLOCK_HEADER_SIZE) as Half, 0);

        vm.alloc(0, 0);

        assert_eq!(vm.available_heap(), free - BLOCK_HEADER_SIZE);
    }

    #[test]
    fn test_function_call_semantic() {
        let mut vm = Vm::default();

        // (define (func x) (halt x)
        // (func 99)
        let code = transform_labels(&[
            Op::Goto("main"),
            // func
            Op::Label("func"),
            Op::Copy(R::Arg, R::Lcl),
            Op::GetVal(R::Lcl, 0),
            Op::Halt,
            // main
            Op::Label("main"),
            Op::Alloc(RecordSignature::new(1, 0)),
            Op::Copy(R::Ptr, R::Arg),
            Op::Const(99),
            Op::PutVal(R::Arg, 0),
            Op::Goto("func"),
        ])
        .collect::<Vec<_>>();

        let res = vm.run(&code);

        assert_eq!(res, 99);
    }

    #[test]
    fn test_first_class_functions_semantic() {
        let mut vm = Vm::default();

        // (define (func x) (halt x)
        // (define (invoke f) (f 42))
        // (invoke f)
        let code = transform_labels(&[
            Op::Goto("main"),
            // func
            Op::Label("func"),
            Op::Copy(R::Arg, R::Lcl),
            Op::GetVal(R::Lcl, 0),
            Op::Halt,
            // func
            Op::Label("invoke"),
            Op::Copy(R::Arg, R::Lcl),
            Op::Alloc(RecordSignature::new(1, 0)),
            Op::Copy(R::Ptr, R::Arg),
            Op::Const(42),
            Op::PutVal(R::Arg, 0),
            Op::GetVal(R::Lcl, 0),
            Op::Jump,
            // main
            Op::Label("main"),
            Op::Alloc(RecordSignature::new(1, 0)),
            Op::Copy(R::Ptr, R::Arg),
            Op::GetAddr("func"),
            Op::PutVal(R::Arg, 0),
            Op::Goto("invoke"),
        ])
        .collect::<Vec<_>>();

        let res = vm.run(&code);

        assert_eq!(res, 42);
    }

    #[test]
    fn test_closure_semantic() {
        let mut vm = Vm::default();

        // (define (outer x y z)
        //   (define (inner)
        //     (halt y)
        //   (inner))
        // (outer 1 2 3)
        let code = transform_labels(&[
            Op::Goto("main"),
            // inner
            Op::Label("inner"),
            Op::Copy(R::Arg, R::Lcl),
            Op::GetVal(R::Cls, 0),
            Op::Halt,
            // outer
            Op::Label("outer"),
            Op::Copy(R::Arg, R::Lcl),
            Op::Alloc(RecordSignature::new(1, 0)),
            Op::Copy(R::Ptr, R::Obj),
            Op::GetVal(R::Lcl, 1),
            Op::PutVal(R::Obj, 0),
            Op::Alloc(RecordSignature::new(0, 0)),
            Op::Copy(R::Ptr, R::Arg),
            Op::Copy(R::Obj, R::Cls),
            Op::Goto("inner"),
            // main
            Op::Label("main"),
            Op::Alloc(RecordSignature::new(3, 0)),
            Op::Copy(R::Ptr, R::Arg),
            Op::Const(1),
            Op::PutVal(R::Arg, 0),
            Op::Const(2),
            Op::PutVal(R::Arg, 1),
            Op::Const(3),
            Op::PutVal(R::Arg, 2),
            Op::Goto("outer"),
        ])
        .collect::<Vec<_>>();

        let res = vm.run(&code);

        assert_eq!(res, 2);
    }
}
