use crate::vm::{Half, Int, Op, RecordSignature, R};
use std::collections::HashMap;
use std::hash::Hash;

#[derive(Debug, Clone)]
pub enum Type {
    Integer,
    Record(String),
    Continuation,
    Function(FunType),
}

#[derive(Debug, Clone)]
pub struct FunType(Vec<Type>, Box<Type>);

#[derive(Debug, Clone)]
enum Binding {
    Static,
    Local(Half),
}

type Env = HashMap<String, (Binding, Type)>;

trait AstNode {
    fn compile(&self, env: &Env) -> Vec<Op<String>>;
}

trait Typed {
    fn type_<'a>(&self, env: &'a Env) -> &'a Type;
}

trait ToplevelDefinition: AstNode {}
trait Expression: TailExpression {}
trait TailExpression: AstNode + Typed {}

impl<T: Expression> TailExpression for T {}

struct Program {
    defs: Vec<Box<dyn ToplevelDefinition>>,
}

struct FunctionDefinition {
    name: String,
    params: Vec<(String, Type)>,
    body: Box<dyn TailExpression>,
}

struct RecordDefinition {
    name: String,
    fields: Vec<(String, Type)>,
}

struct Constant(Int);
struct Reference(String);

struct FunCall {
    name: String,
    args: Vec<Box<dyn Expression>>,
}

struct ContCall {
    name: String,
    val: Box<dyn Expression>,
}

impl AstNode for Program {
    fn compile(&self, env: &Env) -> Vec<Op<String>> {
        let env = assoc(
            "__halt".to_string(),
            (Binding::Static, Type::Continuation),
            env,
        );

        let mut code = FunCall {
            name: "main".to_string(),
            args: vec![Box::new(Reference::new("__halt"))],
        }
        .compile(&env);

        code.extend([Op::label("__halt"), Op::Halt]);

        for def in &self.defs {
            code.extend(def.compile(&env));
        }

        code
    }
}

impl ToplevelDefinition for FunctionDefinition {}

impl AstNode for FunctionDefinition {
    fn compile(&self, env: &Env) -> Vec<Op<String>> {
        let mut local_env = env.clone();

        let (_, _, idxmap) = map_types_to_record_indices(self.params.iter().map(|(_, t)| t));

        for ((p, t), idx) in self.params.iter().zip(idxmap) {
            local_env = assoc(p.clone(), (Binding::Local(idx), t.clone()), &local_env);
        }

        let mut code = vec![Op::label(self.name.clone())];
        if !self.params.is_empty() {
            code.push(Op::Copy(R::Arg, R::Lcl));
        }
        code.extend(self.body.compile(&local_env));
        code
    }
}

impl ToplevelDefinition for RecordDefinition {}

impl AstNode for RecordDefinition {
    fn compile(&self, _env: &Env) -> Vec<Op<String>> {
        let (n_primitive, n_pointer, _idxmap) =
            map_types_to_record_indices(self.fields.iter().map(|(_, t)| t));

        let rs = RecordSignature::new(n_primitive, n_pointer);

        let mut code = vec![Op::label(self.name.clone())];
        code.push(Op::Alloc(rs));
        code.push(Op::Copy(R::Ptr, R::Val));
        code
    }
}

impl Expression for Constant {}

impl AstNode for Constant {
    fn compile(&self, _env: &Env) -> Vec<Op<String>> {
        vec![Op::Const(self.0)]
    }
}

impl Typed for Constant {
    fn type_<'a>(&self, _env: &'a Env) -> &'a Type {
        &Type::Integer
    }
}

impl Expression for Reference {}

impl Reference {
    fn new(name: impl ToString) -> Self {
        Reference(name.to_string())
    }
}

impl AstNode for Reference {
    fn compile(&self, env: &Env) -> Vec<Op<String>> {
        match env[&self.0] {
            (Binding::Static, _) => vec![Op::GetAddr(self.0.clone())],
            (Binding::Local(idx), _) => vec![Op::GetVal(R::Lcl, idx)],
        }
    }
}

impl Typed for Reference {
    fn type_<'a>(&self, env: &'a Env) -> &'a Type {
        &env[&self.0].1
    }
}

impl TailExpression for FunCall {}

impl AstNode for FunCall {
    fn compile(&self, env: &Env) -> Vec<Op<String>> {
        let (n_primitive, n_pointer, idxmap) =
            map_types_to_record_indices(self.args.iter().map(|a| a.type_(env)));

        let mut code = vec![];
        code.push(Op::Alloc(RecordSignature::new(n_primitive, n_pointer)));
        code.push(Op::Copy(R::Ptr, R::Arg));

        for (arg, idx) in self.args.iter().zip(idxmap) {
            code.extend(arg.compile(env));
            code.push(Op::PutVal(R::Arg, idx));
        }

        code.push(Op::Goto(self.name.clone()));
        code
    }
}

impl Typed for FunCall {
    fn type_<'a>(&self, _env: &'a Env) -> &'a Type {
        todo!()
    }
}

impl TailExpression for ContCall {}

impl AstNode for ContCall {
    fn compile(&self, env: &Env) -> Vec<Op<String>> {
        let mut code = vec![];

        match env.get(&self.name) {
            None => panic!("{}", self.name),
            Some((Binding::Static, Type::Continuation)) => {
                code.extend(self.val.compile(env));
                code.push(Op::Goto(self.name.clone()))
            }
            Some((Binding::Local(idx), Type::Continuation)) => {
                code.extend([Op::GetVal(R::Lcl, *idx), Op::Copy(R::Val, R::Fun)]);
                code.extend(self.val.compile(env));
                code.push(Op::Jump);
            }
            _ => todo!(),
        }

        code
    }
}

impl Typed for ContCall {
    fn type_<'a>(&self, _env: &'a Env) -> &'a Type {
        todo!()
    }
}

fn map_types_to_record_indices<'a>(
    types: impl Iterator<Item = &'a Type>,
) -> (Half, Half, Vec<Half>) {
    let types: Vec<_> = types.collect();

    let n_primitive = types
        .iter()
        .filter(|t| match t {
            Type::Integer | Type::Continuation => true,
            Type::Record(_) => false,
            _ => todo!(),
        })
        .count() as Half;

    let n_record = types.len() as Half - n_primitive;

    let mut primitive_idx = 0;
    let mut record_idx = n_primitive;

    let mut indices = Vec::with_capacity(types.len());
    for t in types {
        match t {
            Type::Integer | Type::Continuation => {
                indices.push(primitive_idx);
                primitive_idx += 1;
            }
            Type::Record(_) => {
                indices.push(record_idx);
                record_idx += 1;
            }
            _ => todo!(),
        }
    }
    (n_primitive, n_record, indices)
}

fn assoc<K: Eq + Hash + Clone, V: Clone>(k: K, v: V, map: &HashMap<K, V>) -> HashMap<K, V> {
    let mut map = map.clone();
    map.insert(k, v);
    map
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vm::{transform_labels, Vm};

    #[test]
    fn run_program() {
        let program = Program {
            defs: vec![
                /*Box::new(RecordDefinition {
                    name: "Data".to_string(),
                    fields: vec![("x".to_string(), Type::Integer)],
                }),*/
                Box::new(FunctionDefinition {
                    name: "foo".to_string(),
                    params: vec![
                        ("x".to_string(), Type::Integer),
                        ("k".to_string(), Type::Continuation),
                    ],
                    body: Box::new(ContCall {
                        name: "k".to_string(),
                        val: Box::new(Reference::new("x")),
                    }),
                }),
                Box::new(FunctionDefinition {
                    name: "main".to_string(),
                    params: vec![("k".to_string(), Type::Continuation)],
                    body: Box::new(FunCall {
                        name: "foo".to_string(),
                        args: vec![Box::new(Constant(42)), Box::new(Reference::new("k"))],
                    }),
                }),
            ],
        };

        let code = program.compile(&HashMap::new());
        for op in &code {
            println!("{:?}", op);
        }

        let code = transform_labels(&code).collect::<Vec<_>>();

        let mut vm = Vm::default();
        let res = vm.run(&code);

        assert_eq!(res, 42);
    }
}
