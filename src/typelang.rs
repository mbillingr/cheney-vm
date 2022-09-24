use crate::vm::{Half, Int, Op, RecordSignature, R};
use std::collections::HashMap;
use std::hash::Hash;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Type {
    Empty,
    Integer,
    Record(String),
    Continuation(Box<Type>),
    Function(FunType),
}

impl Type {
    pub fn continuation(t: Type) -> Self {
        Type::Function(FunType(vec![t]))
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct FunType(Vec<Type>); // no return type because functions never return

#[derive(Debug, Copy, Clone)]
enum Binding {
    Static,
    Local(Half),
    Register(R),
}

type Env = HashMap<String, (Binding, Type)>;

trait AstNode {
    fn compile(&self, env: &Env) -> Vec<Op<String>>;
    fn check(&self, env: &Env);
}

trait Typed {
    fn type_<'a>(&self, env: &'a Env) -> &'a Type;
}

trait ToplevelDefinition: AstNode {
    fn name(&self) -> &str;
    fn type_(&self) -> &Type;
}
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
    type_: Type,
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

impl AstNode for Program {
    fn compile(&self, env: &Env) -> Vec<Op<String>> {
        let env = self.make_env(env);

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

    fn check(&self, env: &Env) {
        let env = self.make_env(env);
        for def in &self.defs {
            def.check(&env);
        }
    }
}

impl Program {
    fn make_env(&self, env: &Env) -> Env {
        let mut env = assoc(
            "__halt".to_string(),
            (Binding::Static, Type::Continuation(Box::new(Type::Integer))),
            env,
        );

        for def in &self.defs {
            env = assoc(
                def.name().to_string(),
                (Binding::Static, def.type_().clone()),
                &env,
            );
        }

        env
    }
}

impl ToplevelDefinition for FunctionDefinition {
    fn name(&self) -> &str {
        &self.name
    }

    fn type_(&self) -> &Type {
        &self.type_
    }
}

impl AstNode for FunctionDefinition {
    fn compile(&self, env: &Env) -> Vec<Op<String>> {
        // calling convention:
        //   unary function: VAL -> P00
        //   n-ary function: ARG -> LCL

        let mut code = vec![Op::label(self.name.clone())];
        match self.params.len() {
            0 => {
                code.extend(self.body.compile(env));
            }
            1 => {
                code.push(Op::Copy(R::Val, R::P00));
                code.extend(self.body.compile(&self.extend_env_unary(R::P00, env)));
            }
            _ => {
                code.push(Op::Copy(R::Arg, R::Lcl));
                code.extend(self.body.compile(&self.extend_env(env)));
            }
        }
        code
    }

    fn check(&self, env: &Env) {
        let local_env = self.extend_env(env);
        self.body.check(&local_env);
        assert_eq!(self.body.type_(&local_env), &Type::Empty);
    }
}

impl FunctionDefinition {
    fn extend_env(&self, env: &Env) -> Env {
        let mut env = env.clone();

        let (_, _, idxmap) = map_types_to_record_indices(self.params.iter().map(|(_, t)| t));

        for ((p, t), idx) in self.params.iter().zip(idxmap) {
            env = assoc(p.clone(), (Binding::Local(idx), t.clone()), &env);
        }

        env
    }
    fn extend_env_unary(&self, r: R, env: &Env) -> Env {
        let (p, t) = &self.params[0];
        assoc(p.clone(), (Binding::Register(r), t.clone()), &env)
    }
}

impl ToplevelDefinition for RecordDefinition {
    fn name(&self) -> &str {
        &self.name
    }

    fn type_(&self) -> &Type {
        todo!()
    }
}

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

    fn check(&self, _env: &Env) {
        todo!()
    }
}

impl Expression for Constant {}

impl AstNode for Constant {
    fn compile(&self, _env: &Env) -> Vec<Op<String>> {
        vec![Op::Const(self.0)]
    }

    fn check(&self, _: &Env) {}
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
            (Binding::Register(R::Val), _) => vec![],
            (Binding::Register(r), _) => vec![Op::Copy(r, R::Val)],
        }
    }

    fn check(&self, _: &Env) {}
}

impl Typed for Reference {
    fn type_<'a>(&self, env: &'a Env) -> &'a Type {
        &env[&self.0].1
    }
}

impl TailExpression for FunCall {}

impl AstNode for FunCall {
    fn compile(&self, env: &Env) -> Vec<Op<String>> {
        match env.get(&self.name) {
            None => panic!("{}", self.name),
            Some((binding, Type::Continuation(_))) => {
                return compile_unary_call(*binding, self.name.clone(), &*self.args[0], env)
            }
            Some((binding, Type::Function(f))) if f.0.len() == 1 => {
                return compile_unary_call(*binding, self.name.clone(), &*self.args[0], env)
            }
            _ => {}
        }

        let (n_primitive, n_pointer, idxmap) =
            map_types_to_record_indices(self.args.iter().map(|a| a.type_(env)));

        let mut code = vec![];
        code.push(Op::Alloc(RecordSignature::new(n_primitive, n_pointer)));
        code.push(Op::Copy(R::Ptr, R::Arg));

        for (arg, idx) in self.args.iter().zip(idxmap) {
            code.extend(arg.compile(env));
            code.push(Op::PutVal(R::Arg, idx));
        }

        match env.get(&self.name) {
            None => panic!("{}", self.name),
            Some((Binding::Static, _)) => code.push(Op::Goto(self.name.clone())),
            Some((Binding::Local(idx), _)) => {
                code.extend([Op::GetVal(R::Lcl, *idx), Op::Copy(R::Val, R::Fun), Op::Jump]);
            }
            _ => todo!(),
        }

        code
    }

    fn check(&self, env: &Env) {
        match env.get(&self.name) {
            None => panic!("Unknown function {}", self.name),
            Some((_, Type::Function(ft))) => {
                assert_eq!(self.args.len(), ft.0.len());
                for (param_type, arg) in ft.0.iter().zip(&self.args) {
                    arg.check(env);
                    assert_eq!(arg.type_(env), param_type);
                }
            }
            Some((_, Type::Continuation(t))) => {
                assert_eq!(self.args.len(), 1);
                self.args[0].check(env);
                assert_eq!(self.args[0].type_(env), &**t);
            }
            _ => todo!(),
        }
    }
}

impl Typed for FunCall {
    fn type_<'a>(&self, _env: &'a Env) -> &'a Type {
        &Type::Empty
    }
}


fn compile_unary_call(
    binding: Binding,
    name: String,
    val: &dyn Expression,
    env: &Env,
) -> Vec<Op<String>> {
    match binding {
        Binding::Static => {
            let mut code = val.compile(env);
            code.push(Op::Goto(name));
            code
        }
        Binding::Local(idx) => {
            let mut code = vec![Op::GetVal(R::Lcl, idx), Op::Copy(R::Val, R::Fun)];
            code.extend(val.compile(env));
            code.push(Op::Jump);
            code
        }
        _ => todo!(),
    }
}

fn map_types_to_record_indices<'a>(
    types: impl Iterator<Item = &'a Type>,
) -> (Half, Half, Vec<Half>) {
    let types: Vec<_> = types.collect();

    let n_primitive = types
        .iter()
        .filter(|t| match t {
            Type::Integer | Type::Continuation(_) | Type::Function(_) => true,
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
            Type::Integer | Type::Continuation(_) | Type::Function(_) => {
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

    macro_rules! tl {
        (@expr $x:ident) => {
            Reference(stringify!($x).to_string())
        };

        (@expr $x:expr) => {
            Constant($x)
        };

        (@tail ($name:ident $($arg:tt)*)) => {
            FunCall {
                name: stringify!($name).to_string(),
                args: vec![$(Box::new(tl!(@expr $arg))),*],
            }
        };

        (@topdef (define ($name:ident $($a:ident:$t:expr)*) $body:tt)) => {
            FunctionDefinition {
                name: stringify!($name).to_string(),
                params: vec![$((stringify!($a).to_string(), $t)),*],
                body: Box::new(tl![@tail $body]),
                type_: Type::Function(FunType(vec![$($t),*]))
            }
        };

        ($($topdef:tt)*) => {
            Program {
                defs: vec![$(
                    Box::new(tl!(@topdef $topdef))
                ),*],
            }
        };
    }

    #[test]
    fn run_program() {
        use Type::*;
        let program = tl! {
            /*Box::new(RecordDefinition {
                name: "Data".to_string(),
                fields: vec![("x".to_string(), Type::Integer)],
            }),*/
            (define (foo x:Integer k:Type::continuation(Integer))
                (k x))
            (define (main k:Type::continuation(Integer))
                (foo 42 k))
        };
        program.check(&HashMap::new());

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
