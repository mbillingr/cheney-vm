pub mod types;

use crate::env::Environment;
use crate::str::Str;
use crate::tier02_vmlang as t2;
use crate::tier03_types::types::{Builtin, Closure, Function, RecordType, Value};
use crate::vm::Int;
use std::any::Any;
use std::borrow::Borrow;
use std::collections::HashMap;
use std::rc::Rc;
use types::FunctionSignature;

pub trait Type: std::fmt::Debug {
    fn is_value(&self, env: &Env) -> bool;
    fn is_pointer(&self, env: &Env) -> bool;

    fn is_equal(&self, other: &dyn Type, env: &Env) -> bool;

    fn resolve<'a>(&'a self, env: &'a Env) -> &'a dyn Type;
    fn as_any(&self) -> &dyn Any;

    /// callable types should implement this
    fn callable_signature(&self) -> Option<(&[Rc<dyn Type>], &dyn Type)> {
        None
    }
}

#[derive(Debug)]
pub enum Expression {
    Null,
    Const(Int),
    Record(Vec<Expression>),
    Ref(Str),
    If(Box<Expression>, Box<Expression>, Box<Expression>),
    Call(Box<Expression>, Vec<Expression>),
    Lambda(Vec<Str>, Vec<Rc<dyn Type>>, Box<Expression>),
    GetField(Int, Box<Expression>),
}

#[derive(Debug)]
pub struct FunctionDefinition {
    name: Str,
    params: Vec<Str>,
    body: Expression,
    param_types: Vec<Rc<dyn Type>>,
    return_type: Rc<dyn Type>,
}

#[derive(Debug)]
pub struct Program(Vec<FunctionDefinition>);

type Env = Environment<Rc<dyn Type>>;

impl Program {
    pub fn check(&self, env: &Env) -> t2::Program {
        let program_env = self.extend(env);
        t2::Program(self.0.iter().map(|def| def.check(&program_env)).collect())
    }

    fn extend(&self, env: &Env) -> Env {
        let mut program_env = env.clone();

        for def in &self.0 {
            let t = Function::new(def.param_types.clone(), def.return_type.clone());
            program_env = program_env.assoc(&def.name, t);
        }

        program_env
    }
}

impl FunctionDefinition {
    fn check(&self, env: &Env) -> t2::Definition {
        let local_env = self.extend(env);
        self.body.check(&*self.return_type, &local_env);

        let (vparams, pparams) = distribute(&self.params, &*self.param_types, env);
        let vparams = vparams.cloned().collect();
        let pparams = pparams.cloned().collect();

        match &self.return_type {
            t if t.is_value(env) => t2::Definition::ValFunc(
                self.name.clone(),
                vparams,
                pparams,
                self.body.to_valexpr(&local_env),
            ),
            t if t.is_pointer(env) => t2::Definition::PtrFunc(
                self.name.clone(),
                vparams,
                pparams,
                self.body.to_ptrexpr(&local_env),
            ),
            t => panic!("Invalid return type {t:?}"),
        }
    }

    fn extend(&self, env: &Env) -> Env {
        let mut local_env = env.clone();
        for (p, t) in self.params.iter().zip(&self.param_types) {
            //assert!(!TypeEnum::equal(t, &TypeEnum::Empty, env));
            assert!(t.is_value(env) || t.is_pointer(env));
            local_env = local_env.assoc(p.clone(), t.clone());
        }
        local_env
    }
}

impl Expression {
    fn check(&self, t: &dyn Type, env: &Env) {
        match self {
            Expression::Null => assert!(t.is_pointer(env)),
            Expression::Const(_) => assert!(Value.is_equal(t, env)),
            Expression::Record(xs) => match t.resolve(env).as_any().downcast_ref() {
                Some(RecordType { fields }) => check_expressions(xs, fields, env),
                _ => panic!("{self:?} is not a {t:?}"),
            },
            Expression::Ref(ident) => assert!(env.lookup(ident).unwrap().is_equal(t, env)),
            Expression::If(c, a, b) => {
                c.check(&Value, env);
                a.check(t, env);
                b.check(t, env);
            }
            Expression::Call(f, args) => {
                let ft = f.infer(env);
                match ft.callable_signature() {
                    Some((params, returns)) => {
                        assert!(returns.is_equal(&*t, env));
                        check_expressions(args, &params, env);
                    }
                    t => panic!("can't call expression of type {t:?}"),
                }
            }
            Expression::Lambda(_, _, _) => assert!(self.infer(env).is_equal(t, env)),
            Expression::GetField(idx, rec) => {
                let t_rec = rec.infer(env);
                match t_rec.resolve(env).as_any().downcast_ref() {
                    Some(RecordType { fields }) => {
                        assert!(fields[*idx as usize].is_equal(t, env))
                    }
                    _ => panic!("expected record type, but got {t_rec:?}"),
                }
            }
        }
    }

    fn infer(&self, env: &Env) -> Rc<dyn Type> {
        match self {
            Expression::Null => panic!("can't infer type of Null"),
            Expression::Const(_) => Value::new(),
            Expression::Ref(ident) => env.lookup(ident).unwrap().clone(),
            Expression::Record(args) => RecordType::new(infer_expressions(args, env)),
            Expression::If(c, a, b) => {
                c.check(&Value, env);
                let t = a.infer(env);
                b.check(&*t, env);
                t
            }
            Expression::Call(func, _) => {
                let t = func.infer(env);
                if let Some(Function(FunctionSignature { returns, .. })) = t.as_any().downcast_ref()
                {
                    return returns.clone();
                }
                if let Some(Closure(FunctionSignature { returns, .. }, _)) =
                    t.as_any().downcast_ref()
                {
                    return returns.clone();
                }
                panic!("can't call expression of type {t:?}")
            }
            Expression::Lambda(_, ptypes, body) => {
                let functype = FunctionSignature {
                    ptypes: ptypes.clone(),
                    returns: body.infer(env),
                };

                let fv = self.free_vars(env);
                if fv.is_empty() {
                    Rc::new(Function(functype))
                } else {
                    Rc::new(Closure(functype, fv))
                }
            }
            Expression::GetField(idx, rec) => match rec.infer(env).as_any().downcast_ref() {
                Some(RecordType { fields }) => fields[*idx as usize].clone(),
                o => panic!("expected record type, but got {o:?}"),
            },
        }
    }

    fn free_vars(&self, env: &Env) -> HashMap<Str, Rc<dyn Type>> {
        match self {
            Expression::Null | Expression::Const(_) => HashMap::new(),
            Expression::Ref(ident) => {
                HashMap::from([(ident.clone(), env.lookup(ident).unwrap().clone())])
            }
            Expression::Record(args) => free_vars(args, env),
            Expression::If(c, a, b) => free_vars(&[&**a, &**b, &**c], env),
            Expression::Call(func, args) => {
                let mut fv = free_vars(args, env);
                fv.extend(func.free_vars(env));
                fv
            }
            Expression::Lambda(params, _, body) => {
                let mut fv = body.free_vars(env);
                for p in params {
                    fv.remove(p);
                }
                fv
            }
            Expression::GetField(_, rec) => rec.free_vars(env),
        }
    }

    fn to_valexpr(&self, env: &Env) -> t2::ValExpr {
        match self {
            Expression::Const(x) => t2::ValExpr::Const(*x),
            Expression::Ref(ident) => t2::ValExpr::Ref(ident.clone()),
            Expression::If(condition, consequence, alternative) => t2::ValExpr::If(
                Box::new(condition.to_valexpr(env)),
                Box::new(consequence.to_valexpr(env)),
                Box::new(alternative.to_valexpr(env)),
            ),
            Expression::Call(func, args) => {
                let ft = func.infer(env);
                if let Some(Function(FunctionSignature { ptypes, .. })) = ft.as_any().downcast_ref()
                {
                    let (vargs, pargs) = distribute(args, &ptypes, env);
                    let vargs = vargs.map(|x| x.to_valexpr(env)).collect();
                    let pargs = pargs.map(|x| x.to_ptrexpr(env)).collect();
                    return t2::ValExpr::CallFun(Box::new(func.to_valexpr(env)), vargs, pargs);
                }
                if let Some(Closure(FunctionSignature { ptypes, .. }, _)) =
                    ft.as_any().downcast_ref()
                {
                    let (vargs, pargs) = distribute(args, &ptypes, env);
                    let vargs = vargs.map(|x| x.to_valexpr(env)).collect();
                    let pargs = pargs.map(|x| x.to_ptrexpr(env)).collect();
                    return t2::ValExpr::CallCls(Box::new(func.to_ptrexpr(env)), vargs, pargs);
                }
                if let Some(Builtin(FunctionSignature { ptypes, .. })) = ft.as_any().downcast_ref()
                {
                    let (vargs, pargs) = distribute(args, &ptypes, env);
                    let vargs = vargs.map(|x| x.to_valexpr(env)).collect();
                    let pargs = pargs.map(|x| x.to_ptrexpr(env)).collect();

                    if let Expression::Ref(name) = &**func {
                        return t2::ValExpr::Builtin(name.clone(), vargs, pargs);
                    } else {
                        unimplemented!("builtin must be a direct ref (no first-class builtins)")
                    }
                }
                panic!("can't call a {ft:?}")
            }
            Expression::Lambda(params, ptypes, body) => {
                let (vparams, pparams) = distribute(params, ptypes, env);
                let vparams = vparams.cloned().collect();
                let pparams = pparams.cloned().collect();

                let local_env = env.extend(params, ptypes);

                match body.infer(env) {
                    rt if rt.is_value(&local_env) => t2::ValExpr::LambdaVal(
                        vparams,
                        pparams,
                        Box::new(body.to_valexpr(&local_env)),
                    ),
                    //rt if rt.is_pointer() => todo!(),
                    rt => panic!("Don't know how to deal with return type {rt:?}"),
                }
            }
            Expression::GetField(idx, rec) => t2::ValExpr::GetField(
                Box::new(t2::ValExpr::Const(map_index(
                    *idx,
                    &infer_recargs(rec, env),
                    env,
                ))),
                Box::new(rec.to_ptrexpr(env)),
            ),
            _ => panic!("expected value, got {self:?}"),
        }
    }

    fn to_ptrexpr(&self, env: &Env) -> t2::PtrExpr {
        match self {
            Expression::Null => t2::PtrExpr::Null,
            Expression::Record(xs) => {
                let arg_types = infer_expressions(xs, env);
                let (vargs, pargs) = distribute(xs, &arg_types, env);
                let vargs = vargs.map(|x| x.to_valexpr(env)).collect();
                let pargs = pargs.map(|x| x.to_ptrexpr(env)).collect();
                t2::PtrExpr::Record(vargs, pargs)
            }
            Expression::Ref(ident) => t2::PtrExpr::Ref(ident.clone()),
            Expression::If(condition, consequence, alternative) => t2::PtrExpr::If(
                Box::new(condition.to_valexpr(env)),
                Box::new(consequence.to_ptrexpr(env)),
                Box::new(alternative.to_ptrexpr(env)),
            ),
            Expression::Call(func, args) => {
                let ft = func.infer(env);
                if let Some(Function(FunctionSignature { ptypes, .. })) = ft.as_any().downcast_ref()
                {
                    let (vargs, pargs) = distribute(args, &ptypes, env);
                    let vargs = vargs.map(|x| x.to_valexpr(env)).collect();
                    let pargs = pargs.map(|x| x.to_ptrexpr(env)).collect();
                    return t2::PtrExpr::CallFun(Box::new(func.to_valexpr(env)), vargs, pargs);
                }
                if let Some(Closure(FunctionSignature { ptypes, .. }, _)) =
                    ft.as_any().downcast_ref()
                {
                    let (vargs, pargs) = distribute(args, &ptypes, env);
                    let vargs = vargs.map(|x| x.to_valexpr(env)).collect();
                    let pargs = pargs.map(|x| x.to_ptrexpr(env)).collect();
                    return t2::PtrExpr::CallCls(Box::new(func.to_ptrexpr(env)), vargs, pargs);
                }
                panic!("can't call a {ft:?}")
            }
            Expression::Lambda(params, ptypes, body) => {
                let free_vars = self.free_vars(env);
                let frees = free_vars.keys().cloned().collect::<Vec<_>>();
                let ftypes = free_vars.values().cloned().collect::<Vec<_>>();

                let (vfree, pfree) = distribute(&frees, &ftypes, env);
                let vfree = vfree.cloned().collect();
                let pfree = pfree.cloned().collect();

                let (vparams, pparams) = distribute(params, ptypes, env);
                let vparams = vparams.cloned().collect();
                let pparams = pparams.cloned().collect();

                let local_env = env.extend(params, ptypes);

                match body.infer(&local_env) {
                    rt if rt.is_value(&local_env) => t2::PtrExpr::ClosureVal(
                        vfree,
                        pfree,
                        vparams,
                        pparams,
                        Box::new(body.to_valexpr(&local_env)),
                    ),
                    //rt if rt.is_pointer() => todo!(),
                    rt => panic!("Don't know how to deal with return type {rt:?}"),
                }
            }
            Expression::GetField(idx, rec) => t2::PtrExpr::GetField(
                Box::new(t2::ValExpr::Const(map_index(
                    *idx,
                    &infer_recargs(rec, env),
                    env,
                ))),
                Box::new(rec.to_ptrexpr(env)),
            ),
            _ => panic!("expected pointer, got {self:?}"),
        }
    }
}

fn distribute<'a, T>(
    things: &'a [T],
    types: &'a [Rc<dyn Type>],
    env: &'a Env,
) -> (impl Iterator<Item = &'a T>, impl Iterator<Item = &'a T>) {
    assert_eq!(things.len(), types.len());
    let vals = things
        .iter()
        .zip(types)
        .filter(|(_, t)| t.is_value(env))
        .map(|(x, _)| x);
    let ptrs = things
        .iter()
        .zip(types)
        .filter(|(_, t)| t.is_pointer(env))
        .map(|(x, _)| x);
    (vals, ptrs)
}

fn map_index(idx: Int, types: &[Rc<dyn Type>], env: &Env) -> Int {
    let mut n_val = 0;
    let mut n_ptr = 0;
    let mut ith_ptr = 0;
    for (i, t) in types.iter().enumerate() {
        if t.is_value(env) {
            if i == idx as usize {
                return n_val;
            }
            n_val += 1;
        }
        if t.is_pointer(env) {
            if i == idx as usize {
                ith_ptr = n_ptr;
            }
            n_ptr += 1;
        }
    }
    n_val + ith_ptr
}

fn check_expressions(xs: &[Expression], ts: &[Rc<dyn Type>], env: &Env) {
    assert_eq!(xs.len(), ts.len());
    for (x, t) in xs.iter().zip(ts) {
        x.check(&**t, env)
    }
}

fn infer_expressions(xs: &[Expression], env: &Env) -> Vec<Rc<dyn Type>> {
    xs.iter().map(|x| x.infer(env)).collect()
}

fn infer_recargs(expr: &Expression, env: &Env) -> Vec<Rc<dyn Type>> {
    let trec = expr.infer(env);
    match trec.resolve(env).as_any().downcast_ref() {
        Some(RecordType { fields }) => fields.clone(),
        _ => panic!("not a record expression {expr:?} has type {trec:?}"),
    }
}

fn free_vars<T: Borrow<Expression>>(xs: &[T], env: &Env) -> HashMap<Str, Rc<dyn Type>> {
    xs.iter()
        .map(Borrow::borrow)
        .map(|x| x.free_vars(env))
        .fold(HashMap::new(), |mut acc, fv| {
            acc.extend(fv);
            acc
        })
}

pub fn builtin_env() -> Env {
    let val = Rc::new(Value);
    let vvv = Builtin::new(vec![val.clone(), val.clone()], val);

    Env::Empty
        .assoc("<", vvv.clone())
        .assoc("+", vvv.clone())
        .assoc("-", vvv.clone())
        .assoc("*", vvv)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tier02_vmlang;
    use crate::tier03_types::types::{Empty, Named, Value};

    fn run(program: &Program) -> Int {
        println!("T3: {program:?}");
        let env = builtin_env();
        tier02_vmlang::tests::full_stack_tests::run(program.check(&env))
    }

    #[test]
    fn structurally_same_types_are_equal() {
        assert!(Empty.is_equal(&Empty, &Env::Empty));
        assert!(Value.is_equal(&Value, &Env::Empty));
        assert!(RecordType { fields: vec![] }.is_equal(&RecordType { fields: vec![] }, &Env::Empty));
        assert!(RecordType {
            fields: rcvec![Value]
        }
        .is_equal(
            &RecordType {
                fields: rcvec![Value]
            },
            &Env::Empty
        ));
    }

    #[test]
    fn structurally_different_types_are_not_equal() {
        assert!(!Empty.is_equal(&Value, &Env::Empty));
        assert!(!RecordType {
            fields: rcvec![Value]
        }
        .is_equal(&RecordType { fields: rcvec![] }, &Env::Empty));
        assert!(!RecordType {
            fields: rcvec![Empty]
        }
        .is_equal(
            &RecordType {
                fields: rcvec![Value]
            },
            &Env::Empty
        ));
    }

    #[test]
    fn named_types_equality_based_on_name() {
        assert!(Named::new("Foo").is_equal(&*Named::new("Foo"), &Env::Empty));
        assert!(!Named::new("Foo").is_equal(&*Named::new("Bar"), &Env::Empty));
    }

    #[test]
    fn foo() {
        let empty = RecordType::new(vec![]);
        let value = Rc::new(Value);
        let fndef = FunctionDefinition {
            name: "foo".into(),
            params: vec!["a".into(), "b".into(), "c".into()],
            return_type: RecordType::new(vec![empty.clone(), value.clone(), value.clone()]),
            param_types: vec![value.clone(), empty.clone(), value.clone()],
            body: Expression::Record(vec![
                Expression::Ref("b".into()),
                Expression::Ref("a".into()),
                Expression::Ref("c".into()),
            ]),
        };

        let env = Env::Empty;

        assert_eq!(
            fndef.check(&env),
            t2::Definition::PtrFunc(
                "foo".into(),
                intovec!["a", "c"],
                intovec!["b"],
                t2::PtrExpr::Record(
                    vec![t2::ValExpr::Ref("a".into()), t2::ValExpr::Ref("c".into())],
                    vec![t2::PtrExpr::Ref("b".into())]
                )
            )
        );
    }

    #[test]
    fn simple_prog() {
        let prog = Program(vec![
            FunctionDefinition {
                name: "main".into(),
                params: vec![],
                return_type: Rc::new(Value),
                param_types: vec![],
                body: Expression::Call(Box::new(Expression::Ref("foo".into())), vec![]),
            },
            FunctionDefinition {
                name: "foo".into(),
                params: vec![],
                return_type: Rc::new(Value),
                param_types: vec![],
                body: Expression::Const(42),
            },
        ]);

        assert_eq!(run(&prog), 42);
    }

    #[test]
    fn simple_closure() {
        let value: Rc<dyn Type> = Rc::new(Value);
        let prog = Program(vec![
            FunctionDefinition {
                name: "main".into(),
                params: vec![],
                return_type: value.clone(),
                param_types: vec![],
                body: Expression::Call(
                    Box::new(Expression::Call(
                        Box::new(Expression::Ref("make-fn".into())),
                        vec![Expression::Const(42)],
                    )),
                    vec![],
                ),
            },
            FunctionDefinition {
                name: "make-fn".into(),
                params: vec!["n".into()],
                return_type: Rc::new(Closure(
                    FunctionSignature {
                        ptypes: vec![],
                        returns: value.clone(),
                    },
                    HashMap::from([("n".into(), value.clone())]),
                )),
                param_types: rcvec![Value],
                body: Expression::Lambda(vec![], vec![], Box::new(Expression::Ref("n".into()))),
            },
        ]);

        assert_eq!(run(&prog), 42);
    }

    #[test]
    fn fib() {
        let prog = Program(vec![
            FunctionDefinition {
                name: "main".into(),
                params: vec![],
                return_type: Rc::new(Value),
                param_types: vec![],
                body: Expression::Call(
                    Box::new(Expression::Ref("fib".into())),
                    vec![Expression::Const(6)],
                ),
            },
            FunctionDefinition {
                name: "fib".into(),
                params: vec!["n".into()],
                return_type: Rc::new(Value),
                param_types: rcvec![Value],
                body: Expression::If(
                    Box::new(Expression::Call(
                        Box::new(Expression::Ref("<".into())),
                        vec![Expression::Ref("n".into()), Expression::Const(2)],
                    )),
                    Box::new(Expression::Const(1)),
                    Box::new(Expression::Call(
                        Box::new(Expression::Ref("+".into())),
                        vec![
                            Expression::Call(
                                Box::new(Expression::Ref("fib".into())),
                                vec![Expression::Call(
                                    Box::new(Expression::Ref("-".into())),
                                    vec![Expression::Ref("n".into()), Expression::Const(1)],
                                )],
                            ),
                            Expression::Call(
                                Box::new(Expression::Ref("fib".into())),
                                vec![Expression::Call(
                                    Box::new(Expression::Ref("-".into())),
                                    vec![Expression::Ref("n".into()), Expression::Const(2)],
                                )],
                            ),
                        ],
                    )),
                ),
            },
        ]);

        assert_eq!(run(&prog), 13);
    }

    #[test]
    fn list_type() {
        let env = Env::Empty.assoc(
            "List",
            RecordType::new(vec![Named::new("List"), Value::new()]), // store the cdr first, to check correct record indexing
        );

        let nildef = FunctionDefinition {
            name: "nil".into(),
            params: vec![],
            param_types: vec![],
            return_type: Named::new("List"),
            body: Expression::Null,
        };

        assert_eq!(
            nildef.check(&env),
            t2::Definition::PtrFunc("nil".into(), vec![], vec![], t2::PtrExpr::Null)
        );

        let consdef = FunctionDefinition {
            name: "cons".into(),
            params: vec!["car".into(), "cdr".into()],
            param_types: vec![Value::new(), Named::new("List")],
            return_type: Named::new("List"),
            body: Expression::Record(vec![
                Expression::Ref("cdr".into()),
                Expression::Ref("car".into()),
            ]),
        };

        assert_eq!(
            consdef.check(&env),
            t2::Definition::PtrFunc(
                "cons".into(),
                vec!["car".into()],
                vec!["cdr".into()],
                t2::PtrExpr::Record(
                    vec![t2::ValExpr::Ref("car".into())],
                    vec![t2::PtrExpr::Ref("cdr".into())],
                )
            )
        );

        let cardef = FunctionDefinition {
            name: "car".into(),
            params: vec!["list".into()],
            param_types: vec![Named::new("List")],
            return_type: Rc::new(Value),
            body: Expression::GetField(1, Box::new(Expression::Ref("list".into()))),
        };

        assert_eq!(
            cardef.check(&env),
            t2::Definition::ValFunc(
                "car".into(),
                vec![],
                vec!["list".into()],
                t2::ValExpr::GetField(
                    Box::new(t2::ValExpr::Const(0)),
                    Box::new(t2::PtrExpr::Ref("list".into()))
                )
            )
        );

        let cdrdef = FunctionDefinition {
            name: "cdr".into(),
            params: vec!["list".into()],
            param_types: vec![Named::new("List")],
            return_type: Named::new("List"),
            body: Expression::GetField(0, Box::new(Expression::Ref("list".into()))),
        };

        assert_eq!(
            cdrdef.check(&env),
            t2::Definition::PtrFunc(
                "cdr".into(),
                vec![],
                vec!["list".into()],
                t2::PtrExpr::GetField(
                    Box::new(t2::ValExpr::Const(1)),
                    Box::new(t2::PtrExpr::Ref("list".into()))
                )
            )
        );
    }
}
