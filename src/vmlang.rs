//! super-simple functional semantics on top of the VM

use crate::vm::{Half, Int, Op, RecordSignature};
use std::rc::Rc;

#[derive(Debug)]
enum IntExpression {
    Const(Int),
    Ref(String),
    If(Box<IntExpression>, Box<IntExpression>, Box<IntExpression>),
}

#[derive(Debug)]
enum PtrExpression {
    Record(Vec<IntExpression>, Vec<PtrExpression>),
    Ref(String),
    If(IntExpression, Box<PtrExpression>, Box<PtrExpression>),
}

#[derive(Debug)]
enum TailStatement {
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
                Some(Binding::Local(idx)) => vec![Op::PushLocal(*idx)],
                x => todo!("{x:?}"),
            },
            _ => todo!("{self:?}"),
        }
    }
}

impl Compilable for PtrExpression {
    fn compile(&self, env: &Env, compiler: &mut Compiler) -> Vec<Op<String>> {
        match self {
            PtrExpression::Record(ints, ptrs) => compiler.compile_record(ints, ptrs, env),
            PtrExpression::Ref(ident) => match env.lookup(ident) {
                None => panic!("unbound identifier {ident}"),
                Some(Binding::Local(idx)) => vec![todo!("need ptr-stack equivalent of PushLocal")],
                x => todo!("{x:?}"),
            },
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
            code.extend([todo!("need ptr-stack equivalent of PopInto")]);
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
    Local(Int),
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
        let env = Env::Empty.assoc("foo", Binding::Local(7));
        assert_eq!(
            Compiler::new().compile(IntExpression::Ref("foo".to_string()), &env),
            vec![Op::PushLocal(7)]
        );
    }

    #[test]
    fn compile_ptr_reference() {
        let env = Env::Empty.assoc("foo", Binding::Local(7));
        assert_eq!(
            Compiler::new().compile(PtrExpression::Ref("foo".to_string()), &env),
            vec![todo!("need ptr-stack equivalent of PushLocal")]
        );
    }

    #[test]
    fn compile_record_initialization() {
        assert_eq!(
            Compiler::new().compile(
                PtrExpression::Record(
                    vec![IntExpression::Const(1), IntExpression::Const(2)],
                    vec![]
                ),
                &Env::Empty
            ),
            vec![
                Op::Alloc(RecordSignature::new(2, 0)),
                Op::Const(1),
                Op::PopInto(0),
                Op::Const(2),
                Op::PopInto(1)
            ]
        );
    }
}
