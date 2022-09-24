use crate::vm::{Allocator, GarbageCollector, Half, Int, Op, RecordSignature, Vm, R};
use std::collections::HashMap;
use std::hash::Hash;
use std::sync::atomic::AtomicU64;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Type {
    Empty,
    Boolean,
    Integer,
    Record(String),
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
}

type Env = HashMap<String, (Binding, Type)>;

trait AstNode {
    fn compile(&self, env: &Env) -> Vec<Op<String>>;
    fn check(&self, env: &Env);
}

trait Typed {
    fn type_<'a, 'b: 'a>(&'b self, env: &'a Env) -> &'a Type;
}

trait ToplevelDefinition: AstNode {
    fn name(&self) -> &str;
    fn type_(&self) -> &Type;
}
trait Expression: AstNode + Typed {}
trait TailStatement: AstNode + Typed {}

impl<T: TailStatement> Typed for T {
    fn type_<'a, 'b: 'a>(&'b self, _env: &'a Env) -> &'a Type {
        &Type::Empty
    }
}

struct Program {
    defs: Vec<Box<dyn ToplevelDefinition>>,
}

struct FunctionDefinition<B: TailStatement> {
    name: String,
    params: Vec<(String, Type)>,
    body: B,
    type_: Type,
}

struct RecordDefinition {
    name: String,
    fields: Vec<(String, Type)>,
}

struct Constant(Int, Type);
struct Reference(String);
struct ConditionalExpression<A: Expression, B: Expression, C: Expression> {
    condition: A,
    consequence: B,
    alternative: C,
}
struct ConditionalStatement<A: Expression, B: TailStatement, C: TailStatement> {
    condition: A,
    consequence: B,
    alternative: C,
}

struct FunCall {
    name: String,
    args: Vec<Box<dyn Expression>>,
}

struct Operation<A: Expression, B: Expression>(Operator, A, B);

pub enum Operator {
    LessThan,
}

impl AstNode for Program {
    fn compile(&self, env: &Env) -> Vec<Op<String>> {
        let env = self.make_env(env);

        let mut code = FunCall {
            name: "main".to_string(),
            args: vec![Box::new(Reference::new("__halt"))],
        }
        .compile(&env);

        code.extend([Op::label("__halt"), Op::PushFrom(R::Arg, 0), Op::Halt]);

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
            (Binding::Static, Type::continuation(Type::Integer)),
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

impl<B: TailStatement> ToplevelDefinition for FunctionDefinition<B> {
    fn name(&self) -> &str {
        &self.name
    }

    fn type_(&self) -> &Type {
        &self.type_
    }
}

impl<B: TailStatement> AstNode for FunctionDefinition<B> {
    fn compile(&self, env: &Env) -> Vec<Op<String>> {
        let mut code = vec![Op::label(self.name.clone())];
        match self.params.len() {
            0 => {
                code.extend(self.body.compile(env));
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

impl<B: TailStatement> FunctionDefinition<B> {
    fn extend_env(&self, env: &Env) -> Env {
        let mut env = env.clone();

        let (_, _, idxmap) = map_types_to_record_indices(self.params.iter().map(|(_, t)| t));

        for ((p, t), idx) in self.params.iter().zip(idxmap) {
            env = assoc(p.clone(), (Binding::Local(idx), t.clone()), &env);
        }

        env
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

        let mut code: Vec<Op<String>> = vec![Op::label(self.name.clone())];
        code.push(Op::Alloc(rs));
        todo!()
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
    fn type_<'a, 'b: 'a>(&'b self, _env: &'a Env) -> &'a Type {
        &self.1
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
            (Binding::Static, _) => vec![Op::PushAddr(self.0.clone())],
            (Binding::Local(idx), _) => vec![Op::PushFrom(R::Lcl, idx)],
        }
    }

    fn check(&self, _: &Env) {}
}

impl Typed for Reference {
    fn type_<'a, 'b: 'a>(&'b self, env: &'a Env) -> &'a Type {
        &env[&self.0].1
    }
}

impl<A: Expression, B: Expression, C: Expression> Expression for ConditionalExpression<A, B, C> {}

impl<A: Expression, B: Expression, C: Expression> AstNode for ConditionalExpression<A, B, C> {
    fn compile(&self, env: &Env) -> Vec<Op<String>> {
        let cond = self.condition.compile(env);
        let cons = self.consequence.compile(env);
        let alt = self.alternative.compile(env);

        let elif = unique_label("elif");
        let endif = unique_label("endif");

        let mut code = cond;
        code.push(Op::goto_zero(elif.clone()));
        code.extend(cons);
        code.push(Op::goto(endif.clone()));
        code.push(Op::label(elif));
        code.extend(alt);
        code.push(Op::label(endif));
        code
    }

    fn check(&self, env: &Env) {
        self.condition.check(env);
        self.consequence.check(env);
        self.alternative.check(env);
        assert_eq!(self.condition.type_(env), &Type::Boolean);
        assert_eq!(self.consequence.type_(env), self.alternative.type_(env));
    }
}

impl<A: Expression, B: Expression, C: Expression> Typed for ConditionalExpression<A, B, C> {
    fn type_<'a, 'b: 'a>(&'b self, env: &'a Env) -> &'a Type {
        self.consequence.type_(env)
    }
}

impl<A: Expression, B: TailStatement, C: TailStatement> TailStatement
    for ConditionalStatement<A, B, C>
{
}

impl<A: Expression, B: TailStatement, C: TailStatement> AstNode for ConditionalStatement<A, B, C> {
    fn compile(&self, env: &Env) -> Vec<Op<String>> {
        let cond = self.condition.compile(env);
        let cons = self.consequence.compile(env);
        let alt = self.alternative.compile(env);

        let elif = unique_label("else");

        let mut code = cond;
        code.push(Op::goto_zero(elif.clone()));
        code.extend(cons); // no need to explicitly jump because the tailstatements will
        code.push(Op::label(elif));
        code.extend(alt);
        code
    }

    fn check(&self, env: &Env) {
        self.condition.check(env);
        self.consequence.check(env);
        self.alternative.check(env);
        assert_eq!(self.condition.type_(env), &Type::Boolean);
    }
}

impl TailStatement for FunCall {}

impl AstNode for FunCall {
    fn compile(&self, env: &Env) -> Vec<Op<String>> {
        let (n_primitive, n_pointer, idxmap) =
            map_types_to_record_indices(self.args.iter().map(|a| a.type_(env)));

        let mut code = vec![];
        code.push(Op::Alloc(RecordSignature::new(n_primitive, n_pointer)));
        code.push(Op::Copy(R::Ptr, R::Arg));

        for (arg, idx) in self.args.iter().zip(idxmap) {
            code.extend(arg.compile(env));
            code.push(Op::PopInto(R::Arg, idx));
        }

        match env.get(&self.name) {
            None => panic!("{}", self.name),
            Some((Binding::Static, _)) => code.push(Op::Goto(self.name.clone())),
            Some((Binding::Local(idx), _)) => {
                code.extend([Op::PushFrom(R::Lcl, *idx), Op::Jump]);
            }
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
            _ => todo!(),
        }
    }
}

impl<A: Expression, B: Expression> Expression for Operation<A, B> {}

impl<A: Expression, B: Expression> AstNode for Operation<A, B> {
    fn compile(&self, env: &Env) -> Vec<Op<String>> {
        let mut code = self.1.compile(env);
        code.extend(self.2.compile(env));
        match self.0 {
            Operator::LessThan => code.push(Op::CallBuiltin(BUILTIN_LT)),
        }
        code
    }

    fn check(&self, env: &Env) {
        self.1.check(env);
        self.2.check(env);
        match self.0 {
            Operator::LessThan => assert_eq!(
                (self.1.type_(env), self.2.type_(env)),
                (&Type::Integer, &Type::Integer)
            ),
        }
    }
}

impl<A: Expression, B: Expression> Typed for Operation<A, B> {
    fn type_<'a, 'b: 'a>(&'b self, _env: &'a Env) -> &'a Type {
        match self.0 {
            Operator::LessThan => &Type::Boolean,
        }
    }
}

fn map_types_to_record_indices<'a>(
    types: impl Iterator<Item = &'a Type>,
) -> (Half, Half, Vec<Half>) {
    let types: Vec<_> = types.collect();

    let n_primitive = types
        .iter()
        .filter(|t| match t {
            Type::Integer | Type::Function(_) => true,
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
            Type::Integer | Type::Function(_) => {
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

fn unique_label(name: &str) -> String {
    let n = LABEL_COUNTER.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
    format!("{}-{}", name, n)
}

static LABEL_COUNTER: AtomicU64 = AtomicU64::new(0);

const BUILTIN_LT: Int = 0;

pub fn register_builtins<AC: Allocator, GC: GarbageCollector>(vm: &mut Vm<AC, GC>) {
    vm.register_builtin(BUILTIN_LT, "<", |mut ctx| {
        if ctx.pop_val() >= ctx.pop_val() {
            Int::MAX
        } else {
            0
        }
    });
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vm::{transform_labels, Vm};

    macro_rules! tl {
        (@expr (if $cond:tt $cons:tt $alt:tt)) => {
            ConditionalExpression {
                condition: tl!(@expr $cond),
                consequence: tl!(@expr $cons),
                alternative: tl!(@expr $alt),
            }
        };

        (@expr (< $a:tt $b:tt)) => {
            Operation(
                Operator::LessThan,
                tl!(@expr $a),
                tl!(@expr $b)
            )
        };

        (@expr true) => {
            Constant(Int::MAX, Type::Boolean)
        };

        (@expr false) => {
            Constant(0, Type::Boolean)
        };

        (@expr $x:ident) => {
            Reference(stringify!($x).to_string())
        };

        (@expr $x:expr) => {
            Constant($x, Type::Integer)
        };

        (@tail (if $cond:tt $cons:tt $alt:tt)) => {
            ConditionalStatement {
                condition: tl!(@expr $cond),
                consequence: tl!(@tail $cons),
                alternative: tl!(@tail $alt),
            }
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
                body: tl![@tail $body],
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

    #[test]
    fn fibonacci() {
        use Type::*;
        let program = tl! {
            (define (fib n:Integer k:Type::continuation(Integer))
                (if (< n 2)
                    (k 1)
                    (k 8)))
            (define (main k:Type::continuation(Integer))
                (fib 5 k))
        };
        program.check(&HashMap::new());

        let code = program.compile(&HashMap::new());
        for op in &code {
            println!("{:?}", op);
        }

        let code = transform_labels(&code).collect::<Vec<_>>();

        let mut vm = Vm::default();
        register_builtins(&mut vm);
        let res = vm.run(&code);

        assert_eq!(res, 8);
    }
}

/*  CPS fibonacci
(define (fib n k)
  (if (< n 2)
      (k 1)
      (fib (- n 1)
           (lambda (n1)
                   (fib (- n 2)
                        (lambda (n2)
                                (k (+ n1 n2))))))))

 */
