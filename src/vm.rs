use crate::memory::{ChattyCollector, CopyAllocator, CopyCollector};
use std::collections::HashMap;
use std::fmt::{Debug, Formatter};
use std::hash::Hash;

pub type Int = u64;
pub type Ptr = u64;
pub type Half = u32;

const INITIAL_HEAP_SIZE: usize = 8;

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Op<T> {
    Comment(T),
    Halt,
    Label(T),
    PushAddr(T),
    Goto(T),
    GoIfZero(T),
    Jump,
    CallBuiltin(Int),

    Alloc(RecordSignature),

    Const(Int),

    PtrPushEnv,
    PtrPopEnv,
    PushLocal(Int),
    PopLocal(Int),
    PtrPushLocal(Int),
    PtrPopLocal(Int),

    SetClosure,
    PushClosed(Int),
    PtrPushClosed(Int),

    PushFrom(Int),
    PopInto(Int),
    PtrPushFrom(Int),
    PtrPopInto(Int),

    PushFromDyn,
    PtrPushFromDyn,
    PopFromDyn,
    PtrPopFromDyn,

    DupVal,

    PtrDup,
    PtrDrop(Int),
    PtrNip(Int),
    PtrPeek(Int),

    PtrToVal,
    ValToPtr,
}

impl<T> Op<T> {
    pub fn comment(text: impl Into<T>) -> Self {
        Self::Comment(text.into())
    }
    pub fn label(label: impl Into<T>) -> Self {
        Self::Label(label.into())
    }

    pub fn goto(label: impl Into<T>) -> Self {
        Self::Goto(label.into())
    }
    pub fn goto_zero(label: impl Into<T>) -> Self {
        Self::GoIfZero(label.into())
    }
    pub fn push_addr(label: impl Into<T>) -> Self {
        Self::PushAddr(label.into())
    }

    pub fn is_label(&self) -> bool {
        match self {
            Op::Label(_) => true,
            _ => false,
        }
    }

    fn convert<U>(&self) -> Op<U> {
        match self {
            Op::Comment(_) | Op::Label(_) | Op::PushAddr(_) | Op::Goto(_) | Op::GoIfZero(_) => {
                panic!("Invalid conversion")
            }
            Op::Halt => Op::Halt,
            Op::Jump => Op::Jump,
            Op::CallBuiltin(idx) => Op::CallBuiltin(*idx),
            Op::Alloc(s) => Op::Alloc(*s),
            Op::Const(x) => Op::Const(*x),
            Op::PtrPushEnv => Op::PtrPushEnv,
            Op::PtrPopEnv => Op::PtrPopEnv,
            Op::PushLocal(idx) => Op::PushLocal(*idx),
            Op::PopLocal(idx) => Op::PopLocal(*idx),
            Op::SetClosure => Op::SetClosure,
            Op::PushClosed(idx) => Op::PushClosed(*idx),
            Op::PtrPushClosed(idx) => Op::PtrPushClosed(*idx),
            Op::PtrPushLocal(idx) => Op::PtrPushLocal(*idx),
            Op::PtrPopLocal(idx) => Op::PtrPopLocal(*idx),
            Op::PushFrom(idx) => Op::PushFrom(*idx),
            Op::PopInto(idx) => Op::PopInto(*idx),
            Op::PtrPushFrom(idx) => Op::PtrPushFrom(*idx),
            Op::PtrPopInto(idx) => Op::PtrPopInto(*idx),
            Op::PushFromDyn => Op::PushFromDyn,
            Op::PtrPushFromDyn => Op::PtrPushFromDyn,
            Op::PopFromDyn => Op::PopFromDyn,
            Op::PtrPopFromDyn => Op::PtrPopFromDyn,
            Op::DupVal => Op::DupVal,
            Op::PtrDup => Op::PtrDup,
            Op::PtrDrop(i) => Op::PtrDrop(*i),
            Op::PtrNip(i) => Op::PtrNip(*i),
            Op::PtrPeek(i) => Op::PtrPeek(*i),
            Op::PtrToVal => Op::PtrToVal,
            Op::ValToPtr => Op::ValToPtr,
        }
    }
}

pub fn strip_comments<T: Clone>(code: &[Op<T>]) -> impl Iterator<Item = Op<T>> + '_ {
    code.iter()
        .filter(|op| match op {
            Op::Comment(_) => false,
            _ => true,
        })
        .cloned()
}

pub fn transform_labels<T: Eq + Hash>(code: &[Op<T>]) -> impl Iterator<Item = Op<Int>> + '_ {
    let labels = find_label_offsets(code);
    code.iter().filter_map(move |op| match op {
        Op::Comment(_) => None,
        Op::Label(_) => None,
        Op::Goto(label) => Some(Op::Goto(labels[label])),
        Op::GoIfZero(label) => Some(Op::GoIfZero(labels[label])),
        Op::PushAddr(label) => Some(Op::PushAddr(labels[label])),
        _ => Some(op.convert()),
    })
}

fn find_label_offsets<T: Eq + Hash>(code: &[Op<T>]) -> HashMap<&T, Int> {
    let mut offset = 0;
    let mut labels = HashMap::new();
    for op in code {
        match op {
            Op::Comment(_) => {}
            Op::Label(label) => {
                labels.insert(label, offset);
            }
            _ => offset += 1,
        }
    }
    labels
}

pub struct VmContext<'a, AC, GC> {
    vm: &'a mut Vm<AC, GC>,
    may_alloc: bool,
}

impl<'a, AC: Allocator, GC: GarbageCollector> VmContext<'a, AC, GC> {
    pub fn pop_val(&mut self) -> Int {
        self.vm.val_stack.pop().unwrap()
    }
    pub fn pop_ptr(&mut self) -> Ptr {
        self.vm.ptr_stack.pop().unwrap()
    }

    /// Warning: Calling this may invalidate pointers (because it may trigger GC).
    ///   Implies that calling this more than once in a context is unsafe. The pointer returned
    ///   by the first call is not rooted and will be subject to garbage collection.
    pub fn alloc(&mut self, size: usize) -> Ptr {
        assert!(self.may_alloc);
        self.may_alloc = false;
        self.vm.alloc(size as Half, 0)
    }

    pub fn as_slice(&self, ptr: Ptr) -> &[Int] {
        let ptr = ptr as usize;
        let rs = RecordSignature::from_int(self.vm.heap[ptr - 1]);
        &self.vm.heap[ptr..ptr + rs.n_primitive() as usize]
    }

    pub fn as_slice_mut(&mut self, ptr: Ptr) -> &mut [Int] {
        let ptr = ptr as usize;
        let rs = RecordSignature::from_int(self.vm.heap[ptr - 1]);
        &mut self.vm.heap[ptr..ptr + rs.n_primitive() as usize]
    }
}

pub type BuiltinFunctionType<AC, GC> = fn(VmContext<AC, GC>) -> Int;

pub struct BuiltinFunction<AC, GC>(BuiltinFunctionType<AC, GC>);

impl<AC, GC> std::fmt::Debug for BuiltinFunction<AC, GC> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "<builtin>")
    }
}

#[derive(Debug)]
pub struct Vm<AC, GC> {
    ac: AC,
    gc: GC,
    heap: Vec<Int>,

    val_stack: Vec<Int>,
    ptr_stack: Vec<Ptr>,
    env: Ptr,
    cls: Ptr,

    builtins: Vec<BuiltinFunction<AC, GC>>,
}

impl Default for Vm<CopyAllocator, ChattyCollector<CopyCollector>> {
    fn default() -> Self {
        Self::new(CopyAllocator, ChattyCollector::new(CopyCollector))
    }
}

impl<AC: Allocator, GC: GarbageCollector> Vm<AC, GC> {
    pub fn new(ac: AC, gc: GC) -> Self {
        let heap = ac.init_heap(INITIAL_HEAP_SIZE);
        Vm {
            ac,
            gc,
            heap,
            val_stack: vec![],
            ptr_stack: vec![],
            env: 0,
            cls: 0,
            builtins: vec![],
        }
    }

    pub fn val_stack(&self) -> &[Int] {
        &self.val_stack
    }

    pub fn ptr_stack(&self) -> &[Ptr] {
        &self.ptr_stack
    }

    pub fn register_builtin(
        &mut self,
        idx: Int,
        _info: impl ToString,
        func: BuiltinFunctionType<AC, GC>,
    ) {
        assert_eq!(idx as usize, self.builtins.len());
        self.builtins.push(BuiltinFunction(func));
    }

    fn make_context(&mut self) -> VmContext<AC, GC> {
        VmContext {
            vm: self,
            may_alloc: true,
        }
    }

    pub fn run(&mut self, program: &[Op<Int>]) -> Int {
        let mut ip = 0;
        loop {
            let op = program[ip];
            ip += 1;
            /*println!(
                "ENV: {:?}, VS: {:?}, PS: {:?}, next OP: {:?}",
                self.peek(self.env),
                self.val_stack,
                self.ptr_stack
                    .iter()
                    .map(|&p| self.peek(p))
                    .collect::<Vec<_>>(),
                op
            );*/
            match op {
                Op::Halt => return self.val_stack.pop().unwrap_or(123456789),
                Op::PushAddr(pos) => self.val_stack.push(pos),
                Op::Goto(pos) => ip = pos as usize,
                Op::GoIfZero(pos) => {
                    if self.val_stack.pop().unwrap() == 0 {
                        ip = pos as usize
                    }
                }
                Op::Jump => ip = self.val_stack.pop().unwrap() as usize,
                Op::CallBuiltin(idx) => {
                    let res = self.builtins[idx as usize].0(self.make_context());
                    self.val_stack.push(res);
                }
                Op::Alloc(s) => {
                    let ptr = self.alloc(s.n_primitive(), s.n_pointer());
                    self.ptr_stack.push(ptr);
                }
                Op::Const(x) => self.val_stack.push(x),
                Op::PtrPushEnv => self.ptr_stack.push(self.env),
                Op::PtrPopEnv => self.env = self.ptr_stack.pop().unwrap(),
                Op::PushLocal(idx) => self.val_stack.push(self.get_ptr_offset(self.env, idx)),
                Op::PopLocal(idx) => {
                    let val = self.val_stack.pop().unwrap();
                    self.set_local(idx, val);
                }
                Op::PtrPushLocal(idx) => self.ptr_stack.push(self.get_ptr_offset(self.env, idx)),
                Op::PtrPopLocal(idx) => {
                    let val = self.ptr_stack.pop().unwrap();
                    self.set_local(idx, val);
                }
                Op::SetClosure => self.cls = self.ptr_stack.pop().unwrap(),
                Op::PushClosed(idx) => self.val_stack.push(self.get_ptr_offset(self.cls, idx)),
                Op::PtrPushClosed(idx) => self.ptr_stack.push(self.get_ptr_offset(self.cls, idx)),
                Op::PushFrom(idx) => self.val_stack.push(self.get_field(idx)),
                Op::PopInto(idx) => {
                    let val = self.val_stack.pop().unwrap();
                    self.set_field(idx, val);
                }
                Op::PtrPushFrom(idx) => self.ptr_stack.push(self.get_field(idx)),
                Op::PtrPopInto(idx) => {
                    let val = self.ptr_stack.pop().unwrap();
                    self.set_field(idx, val);
                }
                Op::PushFromDyn => {
                    let idx = self.val_stack.pop().unwrap();
                    self.val_stack.push(self.get_field(idx));
                }
                Op::PtrPushFromDyn => {
                    let idx = self.val_stack.pop().unwrap();
                    self.ptr_stack.push(self.get_field(idx));
                }
                Op::PopFromDyn => {
                    let idx = self.val_stack.pop().unwrap();
                    let val = self.val_stack.pop().unwrap();
                    self.set_field(idx, val);
                }
                Op::PtrPopFromDyn => {
                    let idx = self.val_stack.pop().unwrap();
                    let val = self.ptr_stack.pop().unwrap();
                    self.set_field(idx, val);
                }
                Op::DupVal => {
                    let val = self.val_stack.pop().unwrap();
                    self.val_stack.push(val);
                    self.val_stack.push(val);
                }
                Op::PtrDup => {
                    let ptr = self.ptr_stack.pop().unwrap();
                    self.ptr_stack.push(ptr);
                    self.ptr_stack.push(ptr);
                }
                Op::PtrDrop(i) => {
                    self.ptr_stack.remove(self.ptr_stack.len() - 1 - i as usize);
                }
                Op::PtrNip(i) => {
                    let ptr = self.ptr_stack.remove(self.ptr_stack.len() - 1 - i as usize);
                    self.ptr_stack.push(ptr);
                }
                Op::PtrPeek(i) => {
                    self.ptr_stack
                        .push(self.ptr_stack[self.ptr_stack.len() - 1 - i as usize]);
                }
                Op::PtrToVal => self.val_stack.push(self.ptr_stack.pop().unwrap()),
                Op::ValToPtr => self.ptr_stack.push(self.val_stack.pop().unwrap()),
                _ => todo!("{:?}", op),
            }
        }
    }

    fn set_local(&mut self, offset: Int, val: Int) {
        self.heap[self.env as usize + offset as usize] = val
    }

    fn get_ptr_offset(&self, ptr: Int, offset: Int) -> Int {
        self.heap[ptr as usize + offset as usize]
    }

    fn set_field(&mut self, offset: Int, val: Int) {
        let obj = *self.ptr_stack.last().unwrap();
        self.heap[obj as usize + offset as usize] = val
    }

    fn get_field(&self, offset: Int) -> Int {
        let obj = *self.ptr_stack.last().unwrap();
        self.heap[obj as usize + offset as usize]
    }

    fn alloc(&mut self, n_int: Half, n_ptr: Half) -> Int {
        let rs = RecordSignature::new(n_int, n_ptr);
        let total_size = self.ac.size_of(rs);

        if self.available_heap() < total_size {
            self.collect_garbage();

            if self.available_heap() < total_size {
                let wanted_cap = self.heap.capacity() * 2;
                let needed_cap = self.heap.len() + total_size;
                let target_cap = std::cmp::max(wanted_cap, needed_cap);
                self.heap.reserve(target_cap - self.heap.len());
            }
        }

        self.ac.alloc(rs, &mut self.heap)
    }

    fn collect_garbage(&mut self) {
        self.ptr_stack.push(self.env);
        self.ptr_stack.push(self.cls);
        self.gc.collect(&mut self.ptr_stack, &mut self.heap);
        self.cls = self.ptr_stack.pop().unwrap();
        self.env = self.ptr_stack.pop().unwrap();
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
    pub fn new(n_primitive: Half, n_pointer: Half) -> Self {
        RecordSignature {
            n_primitive,
            n_pointer,
        }
    }

    pub fn from_int(x: Int) -> Self {
        RecordSignature::new(
            (x / (Half::MAX as Int)) as Half,
            (x % (Half::MAX as Int)) as Half,
        )
    }

    pub fn as_int(&self) -> Int {
        (self.n_primitive as Int) * (Half::MAX as Int) + (self.n_pointer as Int)
    }

    pub fn n_primitive(&self) -> Half {
        self.n_primitive
    }

    pub fn n_pointer(&self) -> Half {
        self.n_pointer
    }

    pub fn size(&self) -> usize {
        self.n_primitive as usize + self.n_pointer as usize
    }
}

pub trait Allocator: Debug {
    fn init_heap(&self, size: usize) -> Vec<Int>;
    fn size_of(&self, rs: RecordSignature) -> usize;
    fn alloc(&self, rs: RecordSignature, heap: &mut Vec<Int>) -> Int;
}

pub trait GarbageCollector: Debug {
    fn collect(&self, roots: &mut [Ptr], heap: &mut Vec<Int>);
}

impl<AC: Allocator, GC: GarbageCollector> Vm<AC, GC> {
    fn peek(&self, ptr: Int) -> Val {
        let mut ptr = ptr as usize;
        if ptr == 0 {
            return Val::Rec(vec![]);
        }
        let rs = RecordSignature::from_int(self.heap[ptr - 1]);
        let mut data = Vec::with_capacity(rs.size());

        for _ in 0..rs.n_primitive() {
            data.push(Val::Int(self.heap[ptr]));
            ptr += 1;
        }

        for _ in 0..rs.n_pointer() {
            data.push(self.peek(self.heap[ptr]));
            ptr += 1;
        }

        Val::Rec(data)
    }

    fn poke(&mut self, ptr: Int, data: &[Int]) {
        let ptr = ptr as usize;
        let rs = RecordSignature::from_int(self.heap[ptr - 1]);
        assert!(data.len() <= rs.size());
        self.heap[ptr..ptr + data.len()].copy_from_slice(data);
    }
}

#[derive(Eq, PartialEq)]
enum Val {
    Int(Int),
    Rec(Vec<Val>),
}

impl Debug for Val {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Val::Int(x) => x.fmt(f),
            Val::Rec(xs) => {
                write!(f, "[")?;
                let mut xs = xs.iter();
                if let Some(x) = xs.next() {
                    x.fmt(f)?;
                }
                for x in xs {
                    write!(f, ", {:?}", x)?;
                }
                write!(f, "]")
            }
        }
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

    macro_rules! rec {
        ($($x:tt),*) => {
            Val::Rec(vec![$(val![$x]),*])
        };
    }

    macro_rules! val {
        ([$($x:tt),*]) => {
            rec![$($x),*]
        };

        ($x:expr) => {
            Val::Int($x)
        };
    }

    #[test]
    fn test_run_trivial_program() {
        let mut vm = Vm::default();

        let res = vm.run(&[Op::Const(42), Op::Halt]);

        assert_eq!(res, 42);
    }

    #[test]
    fn test_push_local() {
        let mut vm = Vm::default();
        vm.env = vm.alloc(3, 0);
        vm.poke(vm.env, &[10, 11, 12]);

        let res = vm.run(&[Op::PushLocal(1), Op::Halt]);

        assert_eq!(res, 11);
    }

    #[test]
    fn test_pop_local() {
        let mut vm = Vm::default();
        vm.env = vm.alloc(3, 0);
        vm.poke(vm.env, &[10, 11, 12]);
        vm.val_stack = vec![42];

        vm.run(&[Op::PopLocal(2), Op::Halt]);

        assert_eq!(vm.val_stack, []);
        assert_eq!(vm.peek(vm.env), rec![10, 11, 42]);
    }

    #[test]
    fn test_push_from() {
        let mut vm = Vm::default();
        let ptr = vm.alloc(3, 0);
        vm.ptr_stack.push(ptr);
        vm.poke(ptr, &[10, 11, 12]);

        let res = vm.run(&[Op::PushFrom(1), Op::Halt]);

        assert_eq!(res, 11);
    }

    #[test]
    fn test_pop_into() {
        let mut vm = Vm::default();
        let ptr = vm.alloc(3, 0);
        vm.ptr_stack.push(ptr);
        vm.poke(ptr, &[10, 11, 12]);
        vm.val_stack = vec![42];

        vm.run(&[Op::PopInto(2), Op::Halt]);

        assert_eq!(vm.val_stack, []);
        assert_eq!(vm.peek(ptr), rec![10, 11, 42]);
    }

    #[test]
    fn test_pointer_push_local() {
        let mut vm = Vm::default();
        vm.env = vm.alloc(2, 1);
        let ptr = vm.alloc(1, 0);
        vm.poke(ptr, &[33]);
        vm.poke(vm.env, &[11, 22, ptr]);

        vm.run(&[Op::PtrPushLocal(2), Op::Halt]);

        assert_eq!(vm.ptr_stack, [ptr]);
    }

    #[test]
    fn test_pointer_pop_local() {
        let mut vm = Vm::default();
        vm.env = vm.alloc(2, 1);
        let ptr = vm.alloc(1, 0);
        vm.ptr_stack.push(ptr);

        vm.run(&[Op::PtrPopLocal(2), Op::Halt]);

        assert_eq!(vm.ptr_stack, []);
        assert_eq!(vm.peek(vm.env), rec![0, 0, [0]]);
    }

    #[test]
    fn test_pointer_push_from() {
        let mut vm = Vm::default();
        let ptr1 = vm.alloc(0, 1);
        vm.ptr_stack.push(ptr1);
        let ptr2 = vm.alloc(0, 1);
        vm.poke(ptr1, &[ptr2]);

        vm.run(&[Op::PtrPushFrom(0), Op::Halt]);

        assert_eq!(vm.ptr_stack, [ptr1, ptr2]);
    }

    #[test]
    fn test_pointer_pop_into() {
        let mut vm = Vm::default();
        let ptr1 = vm.alloc(2, 2);
        vm.ptr_stack.push(ptr1);
        let ptr2 = vm.alloc(3, 0);
        vm.ptr_stack.push(ptr2);
        vm.poke(ptr2, &[1, 2, 3]);

        vm.run(&[Op::PtrPopInto(2), Op::Halt]);

        assert_eq!(vm.ptr_stack, [ptr1]);
        assert_eq!(vm.peek(ptr1), rec![0, 0, [1, 2, 3], []]);
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
            Op::Const(12),
            Op::PopInto(0),
            Op::Const(34),
            Op::PopInto(1),
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
        vm.ptr_stack.pop();
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
            Op::Const(0),
            Op::PopInto(0),
            Op::PtrToVal,
            Op::Alloc(rs),
            Op::PopInto(0),
            Op::Halt,
        ]);
        vm.ptr_stack.clear();
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
            Op::Const(0),
            Op::PopInto(0),
            Op::Alloc(rs),
            Op::PtrNip(1),
            Op::PtrToVal,
            Op::PopInto(0),
            Op::Halt,
        ]);

        // the second object comes later on the heap
        assert_eq!(vm.ptr_stack, &[4]);
        assert_eq!(&vm.heap[1..], &[irs, 0, irs, 2]);

        vm.collect_garbage();

        // the second object is moved to the front of the heap
        assert_eq!(vm.ptr_stack, &[2]);
        assert_eq!(&vm.heap[1..], &[irs, 4, irs, 0]);
    }

    #[test]
    fn test_gc_multiple_pointers_to_same_object() {
        let mut vm = Vm::default();
        let rs = RecordSignature::new(0, 2);
        let irs = rs.as_int();
        vm.run(&[
            Op::Alloc(rs),
            Op::Const(0),
            Op::PopInto(0),
            Op::Const(0),
            Op::PopInto(1),
            Op::Alloc(rs),
            Op::PtrNip(1),
            Op::PtrToVal,
            Op::DupVal,
            Op::PopInto(0),
            Op::PopInto(1),
            Op::Halt,
        ]);

        assert_eq!(vm.ptr_stack, &[5]);
        assert_eq!(&vm.heap[1..], &[irs, 0, 0, irs, 2, 2]);

        vm.collect_garbage();

        assert_eq!(vm.ptr_stack, &[2]);
        assert_eq!(&vm.heap[1..], &[irs, 5, 5, irs, 0, 0]);
    }

    #[test]
    fn test_gc_self_referential_objects() {
        let mut vm = Vm::default();
        let rs = RecordSignature::new(1, 1);
        let irs = rs.as_int();
        vm.run(&[
            Op::Alloc(rs),
            Op::PtrDup,
            Op::PtrToVal,
            Op::PopInto(1),
            Op::Const(111),
            Op::PopInto(0),
            Op::PtrToVal, // "drop" pointer
            Op::Alloc(rs),
            Op::PtrDup,
            Op::PtrToVal,
            Op::PopInto(1),
            Op::Const(222),
            Op::PopInto(0),
            Op::Halt,
        ]);

        assert_eq!(&vm.heap[1..], &[irs, 111, 2, irs, 222, 5]);
        assert_eq!(vm.ptr_stack, &[5]);

        // the object with 111 is not reachable from outside and should be collected
        vm.collect_garbage();

        assert_eq!(&vm.heap[1..], &[irs, 222, 2]);
        assert_eq!(vm.ptr_stack, &[2]);
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
        const DATA: Half = 1000;
        const NEW_SIZE: Half = 1;
        // make sure the heap is filled
        vm.alloc(DATA, 0);
        assert_eq!(vm.available_heap(), 0);

        vm.alloc(NEW_SIZE, 0);

        // old object collected, new object only header, data space of old object now free
        assert_eq!(vm.available_heap(), (DATA - NEW_SIZE) as usize);
    }

    #[test]
    fn test_function_tailcall_semantic() {
        let mut vm = Vm::default();

        // (define (func x) (halt x)
        // (func 99)
        let code = transform_labels(&[
            Op::Goto("main"),
            // func
            Op::Label("func"),
            Op::Halt,
            // main
            Op::Label("main"),
            Op::Const(99),
            Op::Goto("func"),
        ])
        .collect::<Vec<_>>();

        let res = vm.run(&code);

        assert_eq!(res, 99);
    }

    #[test]
    fn test_function_call_semantic() {
        let mut vm = Vm::default();

        // (define (func x) x)
        // (halt (func 99))
        let code = transform_labels(&[
            Op::Goto("main"),
            // func
            Op::Label("func"),
            // load x
            Op::PushLocal(1),
            // return
            Op::PushLocal(0),
            Op::PtrPushLocal(2),
            Op::PtrPopEnv,
            Op::Jump,
            // main
            Op::Label("main"),
            Op::Alloc(RecordSignature::new(2, 1)),
            // Return address ("continuation")
            Op::PushAddr("after-func"),
            Op::PopInto(0),
            // Current frame
            Op::PtrPushEnv,
            Op::PtrPopInto(2),
            // first arg
            Op::Const(99),
            Op::PopInto(1),
            // call func
            Op::PtrPopEnv,
            Op::Goto("func"),
            Op::Label("after-func"),
            Op::Halt,
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
            Op::Halt,
            // func
            Op::Label("invoke"),
            Op::Alloc(RecordSignature::new(1, 0)),
            Op::PtrPopEnv,
            Op::PopLocal(0),
            Op::Const(42),
            Op::PushLocal(0),
            Op::Jump,
            // main
            Op::Label("main"),
            Op::PushAddr("func"),
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
            Op::PushFrom(1),
            Op::Halt,
            // outer
            Op::Label("outer"),
            Op::Alloc(RecordSignature::new(3, 0)),
            Op::PopInto(2),
            Op::PopInto(1),
            Op::PopInto(0),
            Op::Goto("inner"),
            // main
            Op::Label("main"),
            Op::Const(1),
            Op::Const(2),
            Op::Const(3),
            Op::Goto("outer"),
        ])
        .collect::<Vec<_>>();

        let res = vm.run(&code);

        assert_eq!(res, 2);
    }

    #[test]
    fn test_run_builtin() {
        let mut vm = Vm::default();
        vm.register_builtin(0, "add", |mut ctx| ctx.pop_val() + ctx.pop_val());

        let res = vm.run(&[Op::Const(10), Op::Const(20), Op::CallBuiltin(0), Op::Halt]);

        assert_eq!(res, 30);
    }

    #[test]
    fn test_string_lib() {
        let mut vm = Vm::default();
        vm.register_builtin(0, "make_str", |mut ctx| -> Int {
            let s = "Hello, World!";
            let ptr = ctx.alloc(s.len());

            let data = ctx.as_slice_mut(ptr);
            for (i, ch) in s.bytes().enumerate() {
                data[i] = ch as Int;
            }

            ptr
        });
        vm.register_builtin(1, "print_str", |mut ctx| -> Int {
            let ptr = ctx.pop_ptr();
            for &ch in ctx.as_slice(ptr) {
                print!("{}", ch as u8 as char);
            }
            println!();
            0
        });

        vm.run(&[
            Op::CallBuiltin(0),
            Op::ValToPtr,
            Op::CallBuiltin(1),
            Op::Halt,
        ]);
    }

    #[test]
    fn conditional() {
        let mut vm = Vm::default();

        let res = vm.run(&[
            Op::Const(0),
            Op::goto_zero(4 as Int),
            Op::Const(1),
            Op::Halt,
            Op::Const(2),
            Op::Halt,
        ]);

        assert_eq!(res, 2);

        let res = vm.run(&[
            Op::Const(Int::MAX),
            Op::goto_zero(4 as Int),
            Op::Const(1),
            Op::Halt,
            Op::Const(2),
            Op::Halt,
        ]);

        assert_eq!(res, 1);
    }
}
