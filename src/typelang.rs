use crate::vm::{Allocator, GarbageCollector, Half, Int, Op, RecordSignature, Vm};
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

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct FunType(Vec<Type>); // no return type because functions never return

#[derive(Debug, Copy, Clone)]
enum Binding {
    Static,
    Local(Int),
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

struct Lambda<B: TailStatement> {
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
    Add,
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
            (
                Binding::Static,
                Type::Function(FunType(vec![Type::Integer])),
            ),
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
        compile_function(self.name.clone(), &self.params, &self.body, env)
    }

    fn check(&self, env: &Env) {
        let local_env = extend_env(&self.params, env);
        self.body.check(&local_env);
        assert_eq!(self.body.type_(&local_env), &Type::Empty);
    }
}

fn extend_env(params: &[(String, Type)], env: &Env) -> Env {
    let mut env = env.clone();

    let (_, _, idxmap) = map_types_to_record_indices(params.iter().map(|(_, t)| t));

    for ((p, t), idx) in params.iter().zip(idxmap) {
        env = assoc(p.clone(), (Binding::Local(idx as Int), t.clone()), &env);
    }

    env
}

fn compile_function(
    name: String,
    params: &[(String, Type)],
    body: &impl TailStatement,
    env: &Env,
) -> Vec<Op<String>> {
    let mut code = vec![Op::label(name.clone())];
    match params.len() {
        0 => {
            code.extend(body.compile(env));
        }
        _ => {
            let (n_primitive, n_pointer, idxmap) =
                map_types_to_record_indices(params.iter().map(|(_, t)| t));
            code.push(Op::Alloc(RecordSignature::new(n_primitive, n_pointer)));
            code.push(Op::SetLocals);
            for ((_, t), idx) in params.iter().zip(idxmap).rev() {
                match t {
                    Type::Record(_) => code.push(Op::PtrToVal),
                    _ => {}
                }
                code.push(Op::PopLocal(idx as Int))
            }
            code.extend(body.compile(&extend_env(params, env)));
        }
    }
    code
}

impl<B: TailStatement> AstNode for Lambda<B> {
    fn compile(&self, env: &Env) -> Vec<Op<String>> {
        let lambda = unique_label("lambda");
        let end = format!("end-{}", lambda);

        let mut code = vec![];
        code.push(Op::goto(end.clone()));
        code.extend(compile_function(
            lambda.clone(),
            &self.params,
            &self.body,
            env,
        ));
        code.push(Op::label(end));
        code.push(Op::PushAddr(lambda));
        code
    }

    fn check(&self, env: &Env) {
        let local_env = extend_env(&self.params, env);
        self.body.check(&local_env);
        assert_eq!(self.body.type_(&local_env), &Type::Empty);
    }
}

impl<B: TailStatement> Expression for Lambda<B> {}

impl<B: TailStatement> Typed for Lambda<B> {
    fn type_<'a, 'b: 'a>(&'b self, _env: &'a Env) -> &'a Type {
        &self.type_
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
            (Binding::Local(idx), _) => vec![Op::PushLocal(idx)],
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
        let mut code = vec![];

        for arg in self.args.iter() {
            code.extend(arg.compile(env));
        }

        match env.get(&self.name) {
            None => panic!("{}", self.name),
            Some((Binding::Static, _)) => code.push(Op::Goto(self.name.clone())),
            Some((Binding::Local(idx), _)) => {
                code.extend([Op::PushLocal(*idx), Op::Jump]);
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
            Operator::Add => code.push(Op::CallBuiltin(BUILTIN_ADD)),
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
            Operator::Add => assert_eq!(
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
            Operator::Add => &Type::Integer,
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
const BUILTIN_ADD: Int = 1;

pub fn register_builtins<AC: Allocator, GC: GarbageCollector>(vm: &mut Vm<AC, GC>) {
    vm.register_builtin(BUILTIN_LT, "<", |mut ctx| {
        if ctx.pop_val() >= ctx.pop_val() {
            Int::MAX
        } else {
            0
        }
    });
    vm.register_builtin(BUILTIN_ADD, "+", |mut ctx| ctx.pop_val() + ctx.pop_val());
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vm::{transform_labels, Vm};
    use Type::*;

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

        (@expr (+ $a:tt $b:tt)) => {
            Operation(
                Operator::Add,
                tl!(@expr $a),
                tl!(@expr $b)
            )
        };

        (@expr (lambda ($($a:ident:$t:tt)*) $body:tt)) => {
            Lambda{
                params: vec![$((stringify!($a).to_string(), tl![@type $t])),*],
                body: tl![@tail $body],
                type_: Type::Function(FunType(vec![$(tl![@type $t]),*]))
            }
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

        (@topdef (define ($name:ident $($a:ident:$t:tt)*) $body:tt)) => {
            FunctionDefinition {
                name: stringify!($name).to_string(),
                params: vec![$((stringify!($a).to_string(), tl![@type $t])),*],
                body: tl![@tail $body],
                type_: Type::Function(FunType(vec![$(tl![@type $t]),*]))
            }
        };

        (@type (continuation $t:tt)) => {
            tl![@type (-> $t)]
        };

        (@type (->  $($t:tt)*)) => {
            Type::Function(FunType(vec![$(tl![@type $t]),*]))
        };

        (@type $t:expr) => {
            $t
        };

        ($($topdef:tt)*) => {
            Program {
                defs: vec![$(
                    Box::new(tl!(@topdef $topdef))
                ),*],
            }
        };
    }

    fn run(prog: &Program) -> Int {
        let env = HashMap::new();
        prog.check(&env);
        let code = prog.compile(&HashMap::new());
        /*for op in &code {
            println!("{:?}", op);
        }*/

        let code = transform_labels(&code).collect::<Vec<_>>();
        let mut vm = Vm::default();
        register_builtins(&mut vm);
        let res = vm.run(&code);
        assert!(vm.val_stack().is_empty());
        assert!(vm.ptr_stack().is_empty());
        res
    }

    #[test]
    fn function_without_parameters() {
        let program = tl! {
            (define (func) (__halt 123))
            (define (main k:(continuation Integer))
                (func))
        };

        assert_eq!(run(&program), 123);
    }

    #[test]
    fn function_with_one_parameter() {
        let program = tl! {
            (define (func x:Integer) (__halt x))
            (define (main k:(continuation Integer))
                (func 99))
        };

        assert_eq!(run(&program), 99);
    }

    #[test]
    fn function_with_multiple_parameters() {
        let program = tl! {
            (define (func a:Integer b:Integer c:Integer d:Integer)
                (__halt (+ (+ a b) (+ c d))))
            (define (main k:(continuation Integer))
                (func 1 2 4 8))
        };

        assert_eq!(run(&program), 15);
    }

    #[test]
    fn function_with_continuation_passed() {
        let program = tl! {
            (define (add a:Integer b:Integer k:(continuation Integer))
                (k (+ a b)))
            (define (main k:(continuation Integer))
                (add 1 2 k))
        };

        assert_eq!(run(&program), 3);
    }

    #[test]
    fn anonymous_first_class_function() {
        let program = tl! {
            (define (func f:(->)) (f))
            (define (main k:(continuation Integer))
                (func (lambda () (__halt 666))))
        };

        assert_eq!(run(&program), 666);
    }

    #[test]
    fn run_program() {
        let program = tl! {
            /*Box::new(RecordDefinition {
                name: "Data".to_string(),
                fields: vec![("x".to_string(), Type::Integer)],
            }),*/
            (define (foo x:Integer k:(continuation Integer))
                (k x))
            (define (main k:(continuation Integer))
                (foo 42 k))
        };

        assert_eq!(run(&program), 42);
    }

    #[test]
    fn fibonacci() {
        let program = tl! {
            (define (fib n:Integer k:(continuation Integer))
                (if (< n 2)
                    (k 1)
                    (k 8)))
            (define (main k:(continuation Integer))
                (fib 5 k))
        };
        assert_eq!(run(&program), 8);
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
