use std::collections::HashMap;
use std::hash::Hash;

pub type Int = u64;

const TID_TOMBSTONE: TypeId = TypeId(0);
const TID_PRIMITIVE: TypeId = TypeId(1);
const TID_RELOC_PTR: TypeId = TypeId(2);

const RESERVED_TIDS: [TypeId; 3] = [TID_TOMBSTONE, TID_PRIMITIVE, TID_RELOC_PTR];

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Op<T> {
    Halt,
    Label(T),
    Goto(T),

    Alloc(TypeId),

    Const(Int),
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
            Op::Const(x) => Op::Const(*x),
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
}

pub struct Vm<GC: GarbageCollector> {
    gc: GC,
    types: TypeRegistry,
    heap: Vec<Int>,

    // registers
    val: TypedValue,
}

impl Default for Vm<NullCollector> {
    fn default() -> Self {
        Self::new(NullCollector)
    }
}

impl<GC: GarbageCollector> Vm<GC> {
    pub fn new(gc: GC) -> Self {
        Vm {
            gc,
            types: TypeRegistry::new(),
            heap: vec![],
            val: TypedValue::int(0),
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
                Op::Alloc(tid) => self.val = TypedValue::ptr(self.alloc(tid), tid),
                Op::Const(x) => self.val = TypedValue::int(x),
                _ => todo!("{:?}", op),
            }
        }
    }

    fn alloc(&mut self, tid: TypeId) -> Int {
        /** Allocate a block of the types size + 1.
        The first word of the block stores the type id.
        Return pointer to the next word, where the data starts.
        **/
        let size = self.types.size(tid);
        let ptr = self.heap.len();

        self.heap.push(tid.as_int());
        for _ in 0..size {
            self.heap.push(0);
        }

        1 + ptr as Int
    }

    fn collect_garbage(&mut self) {
        let old_heap = std::mem::replace(&mut self.heap, vec![]);
        let new_heap = self.gc.collect(&[self.val], old_heap);
        self.heap = new_heap;
    }
}

struct TypeRegistry {
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
}

pub trait GarbageCollector {
    fn collect(&mut self, roots: &[TypedValue], heap: Vec<Int>) -> Vec<Int>;
}

pub struct NullCollector;
impl GarbageCollector for NullCollector {
    fn collect(&mut self, _roots: &[TypedValue], heap: Vec<Int>) -> Vec<Int> {
        heap
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
    fn test_alloc_on_top_of_heap() {
        let mut vm = Vm::default();
        let tid = 11.into();
        vm.register_type(tid, vec![Type::Primitive, Type::Primitive]);

        let TypedValue(ptr, t) = vm.run(&[Op::Alloc(tid), Op::Halt]);
        let ptr = ptr as usize;

        assert_eq!(t, Type::Pointer(tid));
        assert_eq!(vm.heap[ptr - 1], tid.0);
        assert_eq!(vm.heap.len() - ptr, 2);
    }

    #[test]
    fn test_gc_object_reachable_through_val() {
        let mut vm = Vm::default();
        let tid = 11.into();
        vm.register_type(tid, vec![Type::Primitive, Type::Primitive]);
        let TypedValue(ptr, t) = vm.run(&[Op::Alloc(tid), Op::Halt]);
        let heap_size_before_gc = vm.heap.len();

        vm.collect_garbage();

        assert_eq!(vm.heap.len(), heap_size_before_gc);
    }

    #[test]
    fn test_gc_object_not_reachable() {
        let mut vm = Vm::default();
        let tid = 11.into();
        vm.register_type(tid, vec![Type::Primitive, Type::Primitive]);
        let heap_size_before_alloc = vm.heap.len();
        let TypedValue(ptr, t) = vm.run(&[Op::Alloc(tid), Op::Halt]);
        vm.val = TypedValue::int(0);

        vm.collect_garbage();

        assert_eq!(vm.heap.len(), heap_size_before_alloc);
    }
}
