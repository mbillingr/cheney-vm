use std::collections::HashMap;
use std::hash::Hash;

pub type Int = u64;

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Op<T> {
    Halt,
    Label(T),
    Goto(T),

    Alloc(TypeId),
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

pub type TypeId = Int;

pub enum Type {
    Primitive,
    Pointer(TypeId),
}

pub struct Vm {
    types: TypeRegistry,
}

impl Vm {
    pub fn new() -> Self {
        Vm {
            types: TypeRegistry::new(),
        }
    }

    pub fn register_type(&mut self, id: TypeId, fields: Vec<Type>) {
        self.types.register_type(id, fields)
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
        self.types.insert(id, fields);
    }

    fn size(&self, id: TypeId) -> usize {
        self.types[&id].len()
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
        let mut vm = Vm::new();
        vm.register_type(42, vec![]);
        assert_eq!(vm.types.size(42), 0);
    }

    #[test]
    fn test_register_types_with_primitive_fields() {
        let mut vm = Vm::new();
        let (a, b) = (0, 1);

        vm.register_type(a, vec![Type::Primitive]);
        vm.register_type(b, vec![Type::Primitive, Type::Primitive]);

        assert_eq!(vm.types.size(a), 1);
        assert_eq!(vm.types.size(b), 2);
    }

    #[test]
    fn test_register_types_with_pointer_fields() {
        let mut vm = Vm::new();
        let (a, b, c) = (0, 1, 2);

        vm.register_type(a, vec![Type::Primitive]);
        vm.register_type(b, vec![Type::Pointer(a)]);
        vm.register_type(c, vec![Type::Pointer(b), Type::Pointer(b)]);

        assert_eq!(vm.types.size(a), 1);
        assert_eq!(vm.types.size(b), 1);
        assert_eq!(vm.types.size(c), 2);
    }

    #[test]
    fn test_register_recursive_type() {
        let mut vm = Vm::new();
        let a = 0;

        vm.register_type(0, vec![Type::Pointer(a)]);

        assert_eq!(vm.types.size(a), 1);
    }
}
