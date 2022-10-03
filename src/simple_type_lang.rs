use crate::vm::Int;
use crate::vmlang::{self, Const, Environment, PtrExpression, PtrNull, ValExpression};
use std::fmt::Debug;

pub trait Expression:
    Debug
    + Infer
    + MaybeIdentifier
    + Compilable<Box<dyn ValExpression>>
    + Compilable<Box<dyn PtrExpression>>
{
}

pub trait TailStatement: Compilable<Box<dyn vmlang::TailStatement>> {}

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
    Ptr(Vec<Type>),
    StaticFn(FnSig),
    Fn(FnSig),
    Closure(FnSig),
}

impl Type {
    pub fn is_val(&self) -> bool {
        match self {
            Type::Val | Type::Fn(_) => true,
            Type::Ptr(_) | Type::StaticFn(_) | Type::Closure(_) => false,
        }
    }

    pub fn is_ptr(&self) -> bool {
        match self {
            Type::Val | Type::Fn(_) | Type::StaticFn(_) => false,
            Type::Ptr(_) | Type::Closure(_) => true,
        }
    }
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
                Type::Ptr(_) => ptr_args.push(arg.compile(env, self)),
                _ => todo!(),
            }
        }

        (val_args, ptr_args)
    }

    fn map_index(&mut self, index: Int, field_types: &[Type]) -> Int {
        let mut n_val = 0;
        let mut n_ptr = 0;
        let mut nth = 0;
        for (i, t) in field_types.iter().enumerate() {
            let i = i as Int;
            if t.is_val() {
                if i == index {
                    return n_val;
                }
                n_val += 1;
            } else if t.is_ptr() {
                if i == index {
                    nth = n_ptr;
                }
                n_ptr += 1;
            } else {
                panic!("invalid type")
            }
        }

        n_val + nth
    }
}

mark! { Expression:
    Const,
    PtrNull,
    Ref,
    Record,
    GetField,
    IfExpr
}

mark! { TailStatement:
    Halt,
    Call,
    IfStmt
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
        Type::Ptr(vec![])
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
        assert!(self.infer_type(env).is_val());
        Box::new(vmlang::ValRef::new(&self.0))
    }
}

impl Compilable<Box<dyn PtrExpression>> for Ref {
    fn compile(&self, env: &Env, compiler: &mut Compiler) -> Box<dyn PtrExpression> {
        assert!(self.infer_type(env).is_ptr());
        Box::new(vmlang::PtrRef::new(&self.0))
    }
}

#[derive(Debug)]
struct Record(Vec<Box<dyn Expression>>);

impl Infer for Record {
    fn infer_type(&self, env: &Env) -> Type {
        Type::Ptr(self.0.iter().map(|x| x.infer_type(env)).collect())
    }
}

impl MaybeIdentifier for Record {}

impl Compilable<Box<dyn ValExpression>> for Record {}
impl Compilable<Box<dyn PtrExpression>> for Record {
    fn compile(&self, env: &Env, compiler: &mut Compiler) -> Box<dyn PtrExpression> {
        let (val_args, ptr_args) = compiler.distribute_args(&self.0, env);
        Box::new(vmlang::Record::new(val_args, ptr_args))
    }
}

#[derive(Debug)]
struct GetField {
    index: Int,
    record: Box<dyn Expression>,
}

impl GetField {
    pub fn new(index: Int, record: impl Expression + 'static) -> Self {
        GetField {
            index,
            record: Box::new(record),
        }
    }
}

impl Infer for GetField {
    fn infer_type(&self, env: &Env) -> Type {
        match self.record.infer_type(env) {
            Type::Ptr(ts) => ts[self.index as usize].clone(),
            t => panic!("wrong type: {t:?}"),
        }
    }
}

impl MaybeIdentifier for GetField {}

impl Compilable<Box<dyn ValExpression>> for GetField {
    fn compile(&self, env: &Env, compiler: &mut Compiler) -> Box<dyn ValExpression> {
        assert!(self.infer_type(env).is_val());
        let index = match self.record.infer_type(env) {
            Type::Ptr(ts) => compiler.map_index(self.index, &ts),
            t => panic!("wrong type: {t:?}"),
        };
        let rec: Box<dyn PtrExpression> = self.record.compile(env, compiler);
        Box::new(vmlang::ValGetField {
            idx: Box::new(Const(index)),
            rec,
        })
    }
}

impl Compilable<Box<dyn PtrExpression>> for GetField {
    fn compile(&self, env: &Env, compiler: &mut Compiler) -> Box<dyn PtrExpression> {
        assert!(self.infer_type(env).is_ptr());
        let index = match self.record.infer_type(env) {
            Type::Ptr(ts) => compiler.map_index(self.index, &ts),
            t => panic!("wrong type: {t:?}"),
        };
        let rec: Box<dyn PtrExpression> = self.record.compile(env, compiler);
        Box::new(vmlang::PtrGetField {
            idx: Box::new(Const(index)),
            rec,
        })
    }
}

#[derive(Debug)]
struct Halt {
    value: Box<dyn Expression>,
}

impl Halt {
    pub fn new(value: impl Expression + 'static) -> Self {
        Halt {
            value: Box::new(value),
        }
    }
}

impl Compilable<Box<dyn vmlang::TailStatement>> for Halt {
    fn compile(&self, env: &Env, compiler: &mut Compiler) -> Box<dyn vmlang::TailStatement> {
        Box::new(vmlang::Halt(self.value.compile(env, compiler)))
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

impl Compilable<Box<dyn vmlang::TailStatement>> for Call {
    fn compile(&self, env: &Env, compiler: &mut Compiler) -> Box<dyn vmlang::TailStatement> {
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
            Type::Closure(sig) => {
                sig.check(&self.args, env);
                let (val_args, ptr_args) = compiler.distribute_args(&self.args, env);
                let closure: Box<dyn PtrExpression> = self.function.compile(env, compiler);
                Box::new(vmlang::CallClosure {
                    closure,
                    val_args,
                    ptr_args,
                })
            }
            Type::Val | Type::Ptr(_) => panic!("{t:?} is not callable"),
        }
    }
}

#[derive(Debug)]
struct IfExpr {
    condition: Box<dyn Expression>,
    consequence: Box<dyn Expression>,
    alternative: Box<dyn Expression>,
}

impl IfExpr {
    pub fn new(
        condition: impl Expression + 'static,
        consequence: impl Expression + 'static,
        alternative: impl Expression + 'static,
    ) -> Self {
        IfExpr {
            condition: Box::new(condition),
            consequence: Box::new(consequence),
            alternative: Box::new(alternative),
        }
    }
}

impl MaybeIdentifier for IfExpr {}

impl Infer for IfExpr {
    fn infer_type(&self, env: &Env) -> Type {
        assert_eq!(self.condition.infer_type(env), Type::Val);
        let t = self.consequence.infer_type(env);
        assert_eq!(t, self.alternative.infer_type(env));
        t
    }
}

impl Compilable<Box<dyn ValExpression>> for IfExpr {
    fn compile(&self, env: &Env, compiler: &mut Compiler) -> Box<dyn ValExpression> {
        Box::new(vmlang::ValIf {
            condition: self.condition.compile(env, compiler),
            consequence: self.consequence.compile(env, compiler),
            alternative: self.alternative.compile(env, compiler),
        })
    }
}

impl Compilable<Box<dyn PtrExpression>> for IfExpr {
    fn compile(&self, env: &Env, compiler: &mut Compiler) -> Box<dyn PtrExpression> {
        Box::new(vmlang::PtrIf {
            condition: self.condition.compile(env, compiler),
            consequence: self.consequence.compile(env, compiler),
            alternative: self.alternative.compile(env, compiler),
        })
    }
}

#[derive(Debug)]
struct IfStmt {
    condition: Box<dyn Expression>,
    consequence: Box<dyn TailStatement>,
    alternative: Box<dyn TailStatement>,
}

impl IfStmt {
    pub fn new(
        condition: impl Expression + 'static,
        consequence: impl TailStatement + 'static,
        alternative: impl TailStatement + 'static,
    ) -> Self {
        IfStmt {
            condition: Box::new(condition),
            consequence: Box::new(consequence),
            alternative: Box::new(alternative),
        }
    }
}

impl MaybeIdentifier for IfStmt {}

impl Compilable<Box<dyn vmlang::TailStatement>> for IfStmt {
    fn compile(&self, env: &Env, compiler: &mut Compiler) -> Box<dyn vmlang::TailStatement> {
        assert_eq!(self.condition.infer_type(env), Type::Val);
        Box::new(vmlang::TailIf {
            condition: self.condition.compile(env, compiler),
            consequence: self.consequence.compile(env, compiler),
            alternative: self.alternative.compile(env, compiler),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vmlang::{PtrRef, ValRef};
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
    fn closure_call() {
        let env = Env::Empty.assoc("foo", (Binding::Local(0), Type::Closure(FnSig(vec![]))));
        assert_eq!(
            Call::new(Ref::new("foo"), vec![])
                .compile(&env, &mut Compiler)
                .serialize(),
            vmlang::CallClosure::new(PtrRef::new("foo"), vec![], vec![]).serialize(),
        );
    }

    #[test]
    #[should_panic]
    fn call_sig_mismatch() {
        let env = Env::Empty.assoc(
            "foo",
            (
                Binding::Static,
                Type::StaticFn(FnSig(vec![Type::Val, Type::Ptr(vec![])])),
            ),
        );
        Call::new(Ref::new("foo"), vec![]).compile(&env, &mut Compiler);
    }

    #[test]
    #[should_panic]
    fn call_sig_mismatch_wrong_ptr_type() {
        let env = Env::Empty.assoc(
            "foo",
            (
                Binding::Static,
                Type::StaticFn(FnSig(vec![Type::Ptr(vec![Type::Val])])),
            ),
        );
        Call::new(Ref::new("foo"), boxvec![PtrNull]).compile(&env, &mut Compiler);
    }

    #[test]
    fn static_call_with_args() {
        let env = Env::Empty.assoc(
            "foo",
            (
                Binding::Static,
                Type::StaticFn(FnSig(vec![
                    Type::Val,
                    Type::Ptr(vec![]),
                    Type::Val,
                    Type::Ptr(vec![]),
                ])),
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

    #[test]
    fn if_val_expr() {
        let vml: Box<dyn ValExpression> =
            IfExpr::new(Const(0), Const(1), Const(2)).compile(&Env::Empty, &mut Compiler);
        assert_eq!(
            vml.serialize(),
            vmlang::ValIf::new(Const(0), Const(1), Const(2)).serialize(),
        );
    }

    #[test]
    fn if_ptr_expr() {
        let vml: Box<dyn PtrExpression> =
            IfExpr::new(Const(0), PtrNull, PtrNull).compile(&Env::Empty, &mut Compiler);
        assert_eq!(
            vml.serialize(),
            vmlang::PtrIf::new(Const(0), PtrNull, PtrNull).serialize(),
        );
    }

    #[test]
    fn if_tail() {
        let vml: Box<dyn vmlang::TailStatement> =
            IfStmt::new(Const(0), Halt::new(Const(1)), Halt::new(Const(2)))
                .compile(&Env::Empty, &mut Compiler);
        assert_eq!(
            vml.serialize(),
            vmlang::TailIf::new(
                Const(0),
                vmlang::Halt::new(Const(1)),
                vmlang::Halt::new(Const(2))
            )
            .serialize(),
        );
    }

    #[test]
    fn allocate() {
        let vml: Box<dyn PtrExpression> =
            Record(boxvec![Const(1), PtrNull, Const(2), Record(vec![])])
                .compile(&Env::Empty, &mut Compiler);
        assert_eq!(
            vml.serialize(),
            vmlang::Record::new(
                boxvec![Const(1), Const(2)],
                boxvec![PtrNull, vmlang::Record::new(vec![], vec![])]
            )
            .serialize(),
        );
    }

    #[test]
    fn get_field() {
        let expr = GetField::new(0, Record(boxvec![PtrNull, Const(42)]));
        let vml: Box<dyn PtrExpression> = expr.compile(&Env::Empty, &mut Compiler);
        assert_eq!(
            vml.serialize(),
            vmlang::PtrGetField::new(
                Const(1),
                vmlang::Record::new(boxvec![Const(42)], boxvec![PtrNull])
            )
            .serialize()
        );

        let expr = GetField::new(1, Record(boxvec![PtrNull, Const(42)]));
        let vml: Box<dyn ValExpression> = expr.compile(&Env::Empty, &mut Compiler);
        assert_eq!(
            vml.serialize(),
            vmlang::ValGetField::new(
                Const(0),
                vmlang::Record::new(boxvec![Const(42)], boxvec![PtrNull])
            )
            .serialize()
        );

        let expr = GetField::new(1, Record(boxvec![Const(42), PtrNull]));
        let vml: Box<dyn PtrExpression> = expr.compile(&Env::Empty, &mut Compiler);
        assert_eq!(
            vml.serialize(),
            vmlang::PtrGetField::new(
                Const(1),
                vmlang::Record::new(boxvec![Const(42)], boxvec![PtrNull])
            )
            .serialize()
        );

        let expr = GetField::new(0, Record(boxvec![Const(42), PtrNull]));
        let vml: Box<dyn ValExpression> = expr.compile(&Env::Empty, &mut Compiler);
        assert_eq!(
            vml.serialize(),
            vmlang::ValGetField::new(
                Const(0),
                vmlang::Record::new(boxvec![Const(42)], boxvec![PtrNull])
            )
            .serialize()
        );
    }
}
