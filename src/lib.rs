use std::collections::HashMap;
use std::hash::Hash;

pub type Int = u64;

const HEAP_ADDR0_VALUE: Int = 1337; // heap address 0 is unused and always set to a constant value

const TID_TOMBSTONE: TypeId = TypeId(0);
const TID_PRIMITIVE: TypeId = TypeId(1);
const TID_RELOC_PTR: TypeId = TypeId(2);

const RESERVED_TIDS: [TypeId; 3] = [TID_TOMBSTONE, TID_PRIMITIVE, TID_RELOC_PTR];

/// number of words used for the header of allocated memory blocks
const BLOCK_HEADER_SIZE: usize = 1;

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Op<T> {
    Halt,
    Label(T),
    Goto(T),

    Alloc(TypeId),
    SetField(Int),

    Const(Int),
    GetObj,
    SetObj,
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
            Op::Label(_) | Op::Goto(_) => panic!("Invalid conversion"),
            Op::Halt => Op::Halt,
            Op::Alloc(t) => Op::Alloc(*t),
            Op::SetField(idx) => Op::SetField(*idx),
            Op::Const(x) => Op::Const(*x),
            Op::SetObj => Op::SetObj,
            Op::GetObj => Op::GetObj,
        }
    }
}

pub fn transform_labels<T: Eq + Hash>(code: &[Op<T>]) -> impl Iterator<Item = Op<Int>> + '_ {
    let labels = find_label_offsets(code);
    code.iter().filter_map(move |op| match op {
        Op::Label(_) => None,
        Op::Goto(label) => Some(Op::Goto(labels[label])),
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

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct TypeId(Int);

impl TypeId {
    pub fn as_int(&self) -> Int {
        self.0
    }
}

impl From<Int> for TypeId {
    fn from(id: Int) -> Self {
        TypeId(id)
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum Type {
    Primitive,
    Pointer(TypeId),
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct TypedValue(Int, Type);

impl TypedValue {
    pub fn int(x: Int) -> Self {
        TypedValue(x, Type::Primitive)
    }

    pub fn ptr(p: Int, t: TypeId) -> Self {
        TypedValue(p, Type::Pointer(t))
    }

    pub fn raw(&self) -> Int {
        self.0
    }
}

pub struct Vm<GC: GarbageCollector> {
    gc: GC,
    types: TypeRegistry,
    heap: Vec<Int>,

    // registers
    val: TypedValue,
    obj: TypedValue,
}

impl Default for Vm<CopyCollector> {
    fn default() -> Self {
        Self::new(CopyCollector)
    }
}

impl<GC: GarbageCollector> Vm<GC> {
    pub fn new(gc: GC) -> Self {
        Vm {
            gc,
            types: TypeRegistry::new(),
            heap: vec![HEAP_ADDR0_VALUE],
            val: TypedValue::int(0),
            obj: TypedValue::int(0),
        }
    }

    pub fn register_type(&mut self, id: TypeId, fields: Vec<Type>) {
        self.types.register_type(id, fields)
    }

    pub fn run(&mut self, program: &[Op<Int>]) -> TypedValue {
        let mut ip = 0;
        loop {
            let op = program[ip];
            ip += 1;
            match op {
                Op::Halt => return self.val,
                Op::Alloc(tid) => self.obj = TypedValue::ptr(self.alloc(tid), tid),
                Op::SetField(offset) => self.set_field(offset),
                Op::Const(x) => self.val = TypedValue::int(x),
                Op::SetObj => self.obj = self.val,
                Op::GetObj => self.val = self.obj,
                _ => todo!("{:?}", op),
            }
        }
    }

    /** Allocate a block of the types size + BLOCK_DATA_OFFSET.
    The first word of the block stores the type id.
    Return pointer to the next word, where the data starts.
    **/
    fn alloc(&mut self, tid: TypeId) -> Int {
        let size = self.types.size(tid);
        let ptr = self.heap.len();

        // block header
        self.heap.push(tid.as_int());

        // block data
        for _ in 0..size {
            self.heap.push(0);
        }

        (BLOCK_HEADER_SIZE + ptr) as Int
    }

    fn set_field(&mut self, offset: Int) {
        self.heap[(self.obj.raw() + offset) as usize] = self.val.raw()
    }

    fn collect_garbage(&mut self) {
        let mut roots = [self.val, self.obj];

        self.gc.collect(&mut roots, &mut self.heap, &self.types);

        self.val = roots[0];
        self.obj = roots[1];
    }
}

pub struct TypeRegistry {
    types: HashMap<TypeId, Vec<Type>>,
}

impl TypeRegistry {
    fn new() -> Self {
        TypeRegistry {
            types: HashMap::new(),
        }
    }

    fn register_type(&mut self, id: TypeId, fields: Vec<Type>) {
        assert!(!RESERVED_TIDS.contains(&id));
        self.types.insert(id, fields);
    }

    fn size(&self, id: TypeId) -> usize {
        self.types[&id].len()
    }

    fn fields(&self, id: TypeId) -> &[Type] {
        &self.types[&id]
    }
}

pub trait GarbageCollector {
    fn collect(&mut self, roots: &mut [TypedValue], heap: &mut Vec<Int>, types: &TypeRegistry);
}

pub struct CopyCollector;
impl GarbageCollector for CopyCollector {
    fn collect(&mut self, roots: &mut [TypedValue], heap: &mut Vec<Int>, types: &TypeRegistry) {
        let mut target = vec![HEAP_ADDR0_VALUE];

        let mut cc = CollectionContext {
            source: heap,
            target: &mut target,
            types,
        };

        cc.copy_reachable(roots);

        *heap = target;
    }
}

struct CollectionContext<'a> {
    source: &'a mut Vec<Int>,
    target: &'a mut Vec<Int>,
    types: &'a TypeRegistry,
}

impl<'s> CollectionContext<'s> {
    fn copy_reachable(&mut self, roots: &mut [TypedValue]) {
        let mut trace_ptr = self.target.len();

        for x in roots {
            *x = self.reloc_value(*x);
        }

        while trace_ptr < self.target.len() {
            let block_t = TypeId(self.target[trace_ptr]);
            trace_ptr += BLOCK_HEADER_SIZE;

            for field_type in self.types.fields(block_t) {
                self.reloc_field(trace_ptr, *field_type);
                trace_ptr += 1;
            }
        }
    }

    fn reloc_value(&mut self, x: TypedValue) -> TypedValue {
        let TypedValue(p, t) = x;
        TypedValue(self.reloc(p, t), t)
    }

    fn reloc_field(&mut self, ptr: usize, field_type: Type) {
        self.target[ptr] = self.reloc(self.target[ptr], field_type)
    }

    fn reloc(&mut self, p: Int, t: Type) -> Int {
        if p == 0 {
            return 0;
        }

        if let Type::Pointer(tid) = t {
            let new_ptr = self.relocate_pointer(p as usize, self.types.size(tid));
            new_ptr as Int
        } else {
            p
        }
    }

    fn relocate_pointer(&mut self, ptr: usize, size: usize) -> usize {
        if self.source[ptr - BLOCK_HEADER_SIZE] == TID_RELOC_PTR.0 {
            return self.source[ptr] as usize;
        }

        let new_ptr = self.target.len() + BLOCK_HEADER_SIZE;
        let start = ptr as usize - BLOCK_HEADER_SIZE;
        self.target
            .extend_from_slice(&self.source[start..ptr + size]);

        self.source[start] = TID_RELOC_PTR.0;
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
    fn test_register_an_empty_type() {
        let mut vm = Vm::default();
        vm.register_type(42.into(), vec![]);
        assert_eq!(vm.types.size(42.into()), 0);
    }

    #[test]
    fn test_register_types_with_primitive_fields() {
        let mut vm = Vm::default();
        let (a, b) = (10.into(), 11.into());

        vm.register_type(a, vec![Type::Primitive]);
        vm.register_type(b, vec![Type::Primitive, Type::Primitive]);

        assert_eq!(vm.types.size(a), 1);
        assert_eq!(vm.types.size(b), 2);
    }

    #[test]
    fn test_register_types_with_pointer_fields() {
        let mut vm = Vm::default();
        let (a, b, c) = (10.into(), 11.into(), 12.into());

        vm.register_type(a, vec![Type::Primitive]);
        vm.register_type(b, vec![Type::Pointer(a)]);
        vm.register_type(c, vec![Type::Pointer(b), Type::Pointer(b)]);

        assert_eq!(vm.types.size(a), 1);
        assert_eq!(vm.types.size(b), 1);
        assert_eq!(vm.types.size(c), 2);
    }

    #[test]
    fn test_register_recursive_type() {
        let mut vm = Vm::default();
        let a = 10.into();

        vm.register_type(a, vec![Type::Pointer(a)]);

        assert_eq!(vm.types.size(a), 1);
    }

    #[test]
    fn test_run_trivial_program() {
        let mut vm = Vm::default();

        let res = vm.run(&[Op::Const(42), Op::Halt]);

        assert_eq!(res, TypedValue::int(42));
    }

    #[test]
    fn test_set_obj_to_val() {
        let mut vm = Vm::default();

        vm.run(&[Op::Const(123), Op::SetObj, Op::Halt]);

        assert_eq!(vm.obj, TypedValue::int(123));
    }

    #[test]
    fn test_set_val_to_obj() {
        let mut vm = Vm::default();

        vm.obj = TypedValue::int(456);
        vm.run(&[Op::GetObj, Op::Halt]);

        assert_eq!(vm.val, TypedValue::int(456));
    }

    #[test]
    fn test_alloc_on_top_of_heap() {
        let mut vm = Vm::default();
        let tid = 11.into();
        vm.register_type(tid, vec![Type::Primitive, Type::Primitive]);
        let heap_size_before_alloc = vm.heap.len();

        vm.run(&[Op::Alloc(tid), Op::Halt]);

        assert_eq!(vm.heap[heap_size_before_alloc], tid.0);
        assert_eq!(vm.heap.len() - heap_size_before_alloc, 3);
    }

    #[test]
    fn test_initialize_obj() {
        let mut vm = Vm::default();
        let tid = 11.into();
        vm.register_type(tid, vec![Type::Primitive, Type::Primitive]);

        vm.run(&[
            Op::Alloc(tid),
            Op::Const(12),
            Op::SetField(0),
            Op::Const(34),
            Op::SetField(1),
            Op::Halt,
        ]);

        assert_eq!(vm.heap.pop().unwrap(), 34);
        assert_eq!(vm.heap.pop().unwrap(), 12);
    }

    #[test]
    fn test_gc_object_reachable_through_val() {
        let mut vm = Vm::default();
        let tid = 11.into();
        vm.register_type(tid, vec![Type::Primitive, Type::Primitive]);
        vm.run(&[Op::Alloc(tid), Op::Halt]);
        assert_eq!(&vm.heap[1..], &[tid.0, 0, 0]);

        vm.collect_garbage();

        assert_eq!(&vm.heap[1..], &[tid.0, 0, 0]);
    }

    #[test]
    fn test_gc_object_not_reachable() {
        let mut vm = Vm::default();
        let tid = 11.into();
        vm.register_type(tid, vec![Type::Primitive, Type::Primitive]);
        vm.run(&[Op::Alloc(tid), Op::Halt]);
        vm.obj = TypedValue::int(0);
        assert_eq!(&vm.heap[1..], &[tid.0, 0, 0]);

        vm.collect_garbage();

        assert_eq!(&vm.heap[1..], &[]);
    }

    #[test]
    fn test_gc_nested_objects_not_reachable() {
        let mut vm = Vm::default();
        let tid = 11.into();
        vm.register_type(tid, vec![Type::Pointer(tid)]);
        vm.run(&[
            Op::Alloc(tid),
            Op::Const(0),
            Op::SetField(0),
            Op::GetObj,
            Op::Alloc(tid),
            Op::SetField(0),
            Op::Halt,
        ]);
        vm.val = TypedValue::int(0);
        vm.obj = TypedValue::int(0);
        assert_eq!(&vm.heap[1..], &[tid.0, 0, tid.0, 2]);

        vm.collect_garbage();

        assert_eq!(&vm.heap[1..], &[]);
    }

    #[test]
    fn test_gc_nested_objects_reachable() {
        let mut vm = Vm::default();
        let tid = 11.into();
        vm.register_type(tid, vec![Type::Pointer(tid)]);
        vm.run(&[
            Op::Alloc(tid),
            Op::Const(0),
            Op::SetField(0),
            Op::GetObj,
            Op::Alloc(tid),
            Op::SetField(0),
            Op::Halt,
        ]);
        vm.val = TypedValue::int(0);

        // the second object comes later on the heap
        assert_eq!(vm.obj, TypedValue::ptr(4, tid));
        assert_eq!(&vm.heap[1..], &[tid.0, 0, tid.0, 2]);

        vm.collect_garbage();

        // the second object is moved to the front of the heap
        assert_eq!(vm.obj, TypedValue::ptr(2, tid));
        assert_eq!(&vm.heap[1..], &[tid.0, 4, tid.0, 0]);
    }

    #[test]
    fn test_gc_multiple_pointers_to_same_object() {
        let mut vm = Vm::default();
        let tid = 11.into();
        vm.register_type(tid, vec![Type::Pointer(tid), Type::Pointer(tid)]);
        vm.run(&[
            Op::Alloc(tid),
            Op::Const(0),
            Op::SetField(0),
            Op::SetField(1),
            Op::GetObj,
            Op::Alloc(tid),
            Op::SetField(0),
            Op::SetField(1),
            Op::Halt,
        ]);

        assert_eq!(vm.obj, TypedValue::ptr(5, tid));
        assert_eq!(vm.val, TypedValue::ptr(2, tid));
        assert_eq!(&vm.heap[1..], &[tid.0, 0, 0, tid.0, 2, 2]);

        vm.collect_garbage();

        assert_eq!(vm.obj, TypedValue::ptr(5, tid));
        assert_eq!(vm.val, TypedValue::ptr(2, tid));
        assert_eq!(&vm.heap[1..], &[tid.0, 0, 0, tid.0, 2, 2]);
    }

    #[test]
    fn test_gc_self_referential_objects() {
        let mut vm = Vm::default();
        let tid = 11.into();
        vm.register_type(tid, vec![Type::Pointer(tid), Type::Primitive]);
        vm.run(&[
            Op::Alloc(tid),
            Op::GetObj,
            Op::SetField(0),
            Op::Const(111),
            Op::SetField(1),
            Op::Alloc(tid),
            Op::GetObj,
            Op::SetField(0),
            Op::Const(222),
            Op::SetField(1),
            Op::Halt,
        ]);

        assert_eq!(&vm.heap[1..], &[tid.0, 2, 111, tid.0, 5, 222]);
        assert_eq!(vm.obj, TypedValue::ptr(5, tid));
        assert_eq!(vm.val, TypedValue::int(222));

        // the object with 111 is not reachable from outside and should be collected
        vm.collect_garbage();

        assert_eq!(&vm.heap[1..], &[tid.0, 2, 222]);
        assert_eq!(vm.obj, TypedValue::ptr(2, tid));
        assert_eq!(vm.val, TypedValue::int(222));
    }
}
