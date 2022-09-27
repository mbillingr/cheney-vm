//! super-simple functional semantics on top of the VM

use crate::vm::{Half, Int, Op, RecordSignature};
use std::rc::Rc;

#[derive(Debug)]
enum IntExpression {
    Const(Int),
    Ref(String),
    If(Box<IntExpression>, Box<IntExpression>, Box<IntExpression>),
    Lambda(Vec<IntExpression>, Vec<PtrExpression>, Box<TailStatement>),
}

#[derive(Debug)]
enum PtrExpression {
    Null,
    Record(Vec<IntExpression>, Vec<PtrExpression>),
    Ref(String),
    If(IntExpression, Box<PtrExpression>, Box<PtrExpression>),
}

#[derive(Debug)]
enum TailStatement {
    Halt(IntExpression),
    //Call(Expression, Vec<Expression>),
    StaticCall(String, Vec<IntExpression>, Vec<PtrExpression>),
    If(IntExpression, Box<TailStatement>, Box<TailStatement>),
}

macro_rules! concat {
    () => { vec![] };
    ($x:expr) => { $x };
    ($first:expr, $($more:expr),*) => {
        {
            let mut items = $first;
            $(
                items.extend($more);
            )*
            items
        }
    };
}

impl Compilable for IntExpression {
    fn compile(&self, env: &Env, compiler: &mut Compiler) -> Vec<Op<String>> {
        match self {
            IntExpression::Const(x) => vec![Op::Const(*x)],
            IntExpression::If(condition, consequence, alternative) => {
                let else_label = compiler.unique_label("elif");
                let end_label = compiler.unique_label("endif");
                let cond = condition.compile(env, compiler);
                let cons = consequence.compile(env, compiler);
                let alt = alternative.compile(env, compiler);
                concat!(
                    cond,
                    [Op::GoIfZero(else_label.clone())],
                    cons,
                    [Op::Goto(end_label.clone()), Op::Label(else_label)],
                    alt,
                    [Op::Label(end_label)]
                )
            }
            IntExpression::Ref(ident) => match env.lookup(ident) {
                None => panic!("unbound identifier {ident}"),
                Some(Binding::LocalVal(idx)) => vec![Op::PushLocal(*idx)],
                Some(_) => panic!("{ident} is not a value variable"),
            },
            IntExpression::Lambda(int_args, ptr_args, body) => {
                let local_env = env.clone();

                let lam_label = compiler.unique_label("lambda");
                let end_label = compiler.unique_label("end-lambda");
                concat!(
                    vec![Op::Goto(end_label.clone()), Op::Label(lam_label.clone())],
                    body.compile(&local_env, compiler),
                    [
                        Op::Label(end_label.clone()),
                        Op::PushAddr(lam_label.clone())
                    ]
                )
            }
            _ => todo!("{self:?}"),
        }
    }
}

impl Compilable for PtrExpression {
    fn compile(&self, env: &Env, compiler: &mut Compiler) -> Vec<Op<String>> {
        match self {
            PtrExpression::Null => vec![Op::Const(0), Op::ValToPtr],
            PtrExpression::Record(ints, ptrs) => compiler.compile_record(ints, ptrs, env),
            PtrExpression::Ref(ident) => match env.lookup(ident) {
                None => panic!("unbound identifier {ident}"),
                Some(Binding::LocalPtr(idx)) => vec![Op::PtrPushLocal(*idx)],
                Some(_) => panic!("{ident} is not a pointer variable"),
            },
            _ => todo!("{self:?}"),
        }
    }
}

impl Compilable for TailStatement {
    fn compile(&self, env: &Env, compiler: &mut Compiler) -> Vec<Op<String>> {
        match self {
            TailStatement::Halt(expr) => concat!(expr.compile(env, compiler), [Op::Halt]),
            _ => todo!("{self:?}"),
        }
    }
}

trait Compilable {
    fn compile(&self, env: &Env, compiler: &mut Compiler) -> Vec<Op<String>>;
}

struct Compiler {
    unique_counter: u64,
}

impl Compiler {
    fn new() -> Self {
        Compiler { unique_counter: 0 }
    }

    fn compile(&mut self, item: impl Compilable, env: &Env) -> Vec<Op<String>> {
        item.compile(env, self)
    }

    fn compile_record(
        &mut self,
        ints: &[IntExpression],
        ptrs: &[PtrExpression],
        env: &Env,
    ) -> Vec<Op<String>> {
        let mut idx = 0;
        let mut code = vec![Op::Alloc(RecordSignature::new(
            ints.len() as Half,
            ptrs.len() as Half,
        ))];
        for ix in ints {
            code.extend(ix.compile(env, self));
            code.extend([Op::PopInto(idx)]);
            idx += 1;
        }
        for px in ptrs {
            code.extend(px.compile(env, self));
            code.extend([Op::PtrPopInto(idx)]);
            idx += 1;
        }
        code
    }

    fn unique_label(&mut self, s: &str) -> String {
        let n = self.unique_counter;
        self.unique_counter += 1;
        format!("{s}-{n}")
    }
}

#[derive(Debug)]
enum Binding {
    LocalVal(Int),
    LocalPtr(Int),
}

#[derive(Debug, Clone)]
enum Env {
    Empty,
    Entry(Rc<(String, Binding, Env)>),
}

impl Env {
    fn assoc(&self, name: impl ToString, thing: Binding) -> Self {
        Env::Entry(Rc::new((name.to_string(), thing, self.clone())))
    }

    fn lookup(&self, name: &str) -> Option<&Binding> {
        match self {
            Env::Empty => None,
            Env::Entry(e) => {
                if e.0 == name {
                    Some(&e.1)
                } else {
                    e.2.lookup(name)
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vm::RecordSignature;

    #[test]
    fn compile_int_constant() {
        assert_eq!(
            Compiler::new().compile(IntExpression::Const(42), &Env::Empty),
            vec![Op::Const(42)]
        );
    }

    #[test]
    fn compile_int_conditional() {
        let code = Compiler::new().compile(
            IntExpression::If(
                Box::new(IntExpression::Const(0)),
                Box::new(IntExpression::Const(1)),
                Box::new(IntExpression::Const(2)),
            ),
            &Env::Empty,
        );
        match code.as_slice() {
            [Op::Const(0), Op::GoIfZero(else_target), Op::Const(1), Op::Goto(end_target), Op::Label(else_label), Op::Const(2), Op::Label(end_label)]
                if else_target == else_label
                    && end_target == end_label
                    && else_label != end_label => {}
            _ => panic!("{code:?}"),
        }
    }

    #[test]
    fn compile_int_reference() {
        let env = Env::Empty.assoc("foo", Binding::LocalVal(7));
        assert_eq!(
            Compiler::new().compile(IntExpression::Ref("foo".to_string()), &env),
            vec![Op::PushLocal(7)]
        );
    }

    #[test]
    fn compile_ptr_reference() {
        let env = Env::Empty.assoc("foo", Binding::LocalPtr(5));
        assert_eq!(
            Compiler::new().compile(PtrExpression::Ref("foo".to_string()), &env),
            vec![Op::PtrPushLocal(5)]
        );
    }

    #[test]
    #[should_panic]
    fn compile_wrong_reference1() {
        let env = Env::Empty
            .assoc("foo", Binding::LocalVal(0))
            .assoc("bar", Binding::LocalPtr(1));
        Compiler::new().compile(PtrExpression::Ref("foo".to_string()), &env);
    }

    #[test]
    #[should_panic]
    fn compile_wrong_reference2() {
        let env = Env::Empty
            .assoc("foo", Binding::LocalVal(0))
            .assoc("bar", Binding::LocalPtr(1));
        Compiler::new().compile(IntExpression::Ref("bar".to_string()), &env);
    }

    #[test]
    fn compile_null_ptr() {
        assert_eq!(
            Compiler::new().compile(PtrExpression::Null, &Env::Empty),
            vec![Op::Const(0), Op::ValToPtr]
        );
    }

    #[test]
    fn compile_record_initialization() {
        assert_eq!(
            Compiler::new().compile(
                PtrExpression::Record(
                    vec![IntExpression::Const(1), IntExpression::Const(2)],
                    vec![PtrExpression::Null],
                ),
                &Env::Empty
            ),
            vec![
                Op::Alloc(RecordSignature::new(2, 1)),
                Op::Const(1),
                Op::PopInto(0),
                Op::Const(2),
                Op::PopInto(1),
                Op::Const(0),
                Op::ValToPtr,
                Op::PtrPopInto(2),
            ]
        );
    }

    #[test]
    fn compile_nullary_lambda() {
        let code = Compiler::new().compile(
            IntExpression::Lambda(
                vec![],
                vec![],
                Box::new(TailStatement::Halt(IntExpression::Const(42))),
            ),
            &Env::Empty,
        );

        assert_eq!(
            code,
            vec![
                Op::goto("end-lambda-1"),
                Op::label("lambda-0"),
                Op::Const(42),
                Op::Halt,
                Op::label("end-lambda-1"),
                Op::PushAddr("lambda-0".to_string()),
            ]
        );

        match &code[..] {
            [Op::Goto(goto_end), Op::Label(label_def), Op::Const(42), Op::Halt, Op::Label(label_end), Op::PushAddr(get_def)]
                if goto_end == label_end && get_def == label_def => {} // Ok
            _ => panic!("{:?}", code),
        }
    }
}
