use crate::vm::Int;
use crate::vmlang::{
    self, Const, Environment, PtrExpression, PtrNull, TailStatement, ValExpression,
};
use std::fmt::Debug;

pub trait Expression:
    Debug
    + Infer
    + MaybeIdentifier
    + Compilable<Box<dyn ValExpression>>
    + Compilable<Box<dyn PtrExpression>>
{
}

pub trait Compilable<T>: Debug {
    fn compile(&self, env: &Env, compiler: &mut Compiler) -> T {
        panic!("Don't know how to compile {self:?} to {}", stringify!(T))
    }
}

pub trait Infer {
    fn infer_type(&self, env: &Env) -> Type;
}

pub trait MaybeIdentifier {
    fn as_identifier(&self) -> Option<&str> {
        None
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Type {
    Val,
    Ptr,
    StaticFn(FnSig),
    Fn(FnSig),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct FnSig(Vec<Type>);

impl FnSig {
    fn check(&self, args: &[Box<dyn Expression>], env: &Env) {
        assert_eq!(args.len(), self.0.len());
        for (pt, at) in self.0.iter().zip(args) {
            assert_eq!(&at.infer_type(env), pt);
        }
    }
}

type Env = Environment<(Binding, Type)>;

#[derive(Debug, Clone)]
pub enum Binding {
    Static,
    Local(Int),
}

pub struct Compiler;

impl Compiler {
    fn distribute_args(
        &mut self,
        args: &[Box<dyn Expression>],
        env: &Env,
    ) -> (Vec<Box<dyn ValExpression>>, Vec<Box<dyn PtrExpression>>) {
        let mut val_args = vec![];
        let mut ptr_args = vec![];

        for arg in args {
            match arg.infer_type(env) {
                Type::Val => val_args.push(arg.compile(env, self)),
                Type::Ptr => ptr_args.push(arg.compile(env, self)),
                _ => todo!(),
            }
        }

        (val_args, ptr_args)
    }
}

mark! { Expression:
    Const,
    PtrNull,
    Ref
}

impl MaybeIdentifier for Const {}
impl Infer for Const {
    fn infer_type(&self, _env: &Env) -> Type {
        Type::Val
    }
}
impl Compilable<Box<dyn ValExpression>> for Const {
    fn compile(&self, env: &Env, compiler: &mut Compiler) -> Box<dyn ValExpression> {
        Box::new(self.clone())
    }
}
impl Compilable<Box<dyn PtrExpression>> for Const {}

impl MaybeIdentifier for PtrNull {}
impl Infer for PtrNull {
    fn infer_type(&self, _env: &Env) -> Type {
        Type::Ptr
    }
}
impl Compilable<Box<dyn ValExpression>> for PtrNull {}
impl Compilable<Box<dyn PtrExpression>> for PtrNull {
    fn compile(&self, env: &Env, compiler: &mut Compiler) -> Box<dyn PtrExpression> {
        Box::new(self.clone())
    }
}

#[derive(Debug)]
struct Ref(String);

impl Ref {
    pub fn new(identifier: impl ToString) -> Self {
        Ref(identifier.to_string())
    }
}

impl Infer for Ref {
    fn infer_type(&self, env: &Env) -> Type {
        match env.lookup(&self.0) {
            None => panic!("unbound name {}", self.0),
            Some((_, t)) => t.clone(),
        }
    }
}

impl MaybeIdentifier for Ref {
    fn as_identifier(&self) -> Option<&str> {
        Some(&self.0)
    }
}

impl Compilable<Box<dyn ValExpression>> for Ref {
    fn compile(&self, env: &Env, compiler: &mut Compiler) -> Box<dyn ValExpression> {
        Box::new(vmlang::ValRef::new(&self.0))
    }
}

impl Compilable<Box<dyn PtrExpression>> for Ref {
    fn compile(&self, env: &Env, compiler: &mut Compiler) -> Box<dyn PtrExpression> {
        Box::new(vmlang::PtrRef::new(&self.0))
    }
}

#[derive(Debug)]
struct Call {
    function: Box<dyn Expression>,
    args: Vec<Box<dyn Expression>>,
}

impl Call {
    pub fn new(function: impl Expression + 'static, args: Vec<Box<dyn Expression>>) -> Self {
        Call {
            function: Box::new(function),
            args,
        }
    }
}

impl Compilable<Box<dyn TailStatement>> for Call {
    fn compile(&self, env: &Env, compiler: &mut Compiler) -> Box<dyn TailStatement> {
        let t = self.function.infer_type(env);
        match t {
            Type::StaticFn(sig) => {
                sig.check(&self.args, env);
                let (val_args, ptr_args) = compiler.distribute_args(&self.args, env);
                Box::new(vmlang::CallStatic::new(
                    self.function
                        .as_identifier()
                        .expect("static function expression should be convertible to identifier"),
                    val_args,
                    ptr_args,
                ))
            }
            Type::Fn(sig) => {
                sig.check(&self.args, env);
                let (val_args, ptr_args) = compiler.distribute_args(&self.args, env);
                let function: Box<dyn ValExpression> = self.function.compile(env, compiler);
                Box::new(vmlang::CallDynamic {
                    function,
                    val_args,
                    ptr_args,
                })
            }
            Type::Val | Type::Ptr => panic!("{t:?} is not callable"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vmlang::ValRef;
    use crate::Serialize;

    #[test]
    fn static_call() {
        let env = Env::Empty.assoc("foo", (Binding::Static, Type::StaticFn(FnSig(vec![]))));
        assert_eq!(
            Call::new(Ref::new("foo"), vec![])
                .compile(&env, &mut Compiler)
                .serialize(),
            vmlang::CallStatic::new("foo", vec![], vec![]).serialize(),
        );
    }

    #[test]
    fn dynamic_call() {
        let env = Env::Empty.assoc("foo", (Binding::Local(0), Type::Fn(FnSig(vec![]))));
        assert_eq!(
            Call::new(Ref::new("foo"), vec![])
                .compile(&env, &mut Compiler)
                .serialize(),
            vmlang::CallDynamic::new(ValRef::new("foo"), vec![], vec![]).serialize(),
        );
    }

    #[test]
    #[should_panic]
    fn call_sig_mismatch() {
        let env = Env::Empty.assoc(
            "foo",
            (
                Binding::Static,
                Type::StaticFn(FnSig(vec![Type::Val, Type::Ptr])),
            ),
        );
        Call::new(Ref::new("foo"), vec![]).compile(&env, &mut Compiler);
    }

    #[test]
    fn static_call_with_args() {
        let env = Env::Empty.assoc(
            "foo",
            (
                Binding::Static,
                Type::StaticFn(FnSig(vec![Type::Val, Type::Ptr, Type::Val, Type::Ptr])),
            ),
        );
        assert_eq!(
            Call::new(
                Ref::new("foo"),
                boxvec![Const(1), PtrNull, Const(2), PtrNull]
            )
            .compile(&env, &mut Compiler)
            .serialize(),
            vmlang::CallStatic::new(
                "foo",
                boxvec![Const(1), Const(2)],
                boxvec![PtrNull, PtrNull]
            )
            .serialize(),
        );
    }
}
