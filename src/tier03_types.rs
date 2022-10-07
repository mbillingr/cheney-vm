use crate::env::Environment;
use crate::str::Str;
use crate::tier02_vmlang as t2;
use crate::vm::Int;
use std::borrow::Borrow;
use std::collections::HashMap;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Type {
    Empty,
    Value,
    Record(RecordType),
    Builtin(FunctionType),
    Function(FunctionType),
    Closure(FunctionType, HashMap<Str, Type>),
}

impl Type {
    fn is_value(&self) -> bool {
        match self {
            Type::Value | Type::Function(_) => true,
            Type::Record(_) | Type::Closure(_, _) => false,
            Type::Builtin(_) => false,
            Type::Empty => false,
        }
    }

    fn is_pointer(&self) -> bool {
        match self {
            Type::Value | Type::Function(_) => false,
            Type::Record(_) | Type::Closure(_, _) => true,
            Type::Builtin(_) => false,
            Type::Empty => false,
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct RecordType {
    fields: Vec<Type>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct FunctionType {
    ptypes: Vec<Type>,
    returns: Box<Type>,
}

#[derive(Debug)]
pub enum Expression {
    Const(Int),
    Record(Vec<Expression>, Vec<Type>),
    Ref(Str),
    If(Box<Expression>, Box<Expression>, Box<Expression>),
    Call(Box<Expression>, Vec<Expression>),
    Lambda(Vec<Str>, Vec<Type>, Box<Expression>),
    GetField(Int, Box<Expression>),
}

#[derive(Debug)]
pub struct FunctionDefinition {
    name: Str,
    params: Vec<Str>,
    body: Expression,
    param_types: Vec<Type>,
    return_type: Type,
}

#[derive(Debug)]
pub struct Program(Vec<FunctionDefinition>);

type Env = Environment<Type>;

impl Program {
    pub fn check(&self, env: &Env) -> t2::Program {
        let program_env = self.extend(env);
        t2::Program(self.0.iter().map(|def| def.check(&program_env)).collect())
    }

    fn extend(&self, env: &Env) -> Env {
        let mut program_env = env.clone();

        for def in &self.0 {
            let t = Type::Function(FunctionType {
                ptypes: def.param_types.clone(),
                returns: Box::new(def.return_type.clone()),
            });
            program_env = program_env.assoc(&def.name, t);
        }

        program_env
    }
}

impl FunctionDefinition {
    fn check(&self, env: &Env) -> t2::Definition {
        let local_env = self.extend(env);
        self.body.check(&self.return_type, &local_env);

        let (vparams, pparams) = distribute(&self.params, &self.param_types);
        let vparams = vparams.cloned().collect();
        let pparams = pparams.cloned().collect();

        match &self.return_type {
            Type::Empty => todo!("Wanna support without return type? Not sure how..."),
            Type::Builtin(_) => unimplemented!("No first-class builtins"),
            Type::Value | Type::Function(_) => t2::Definition::ValFunc(
                self.name.clone(),
                vparams,
                pparams,
                self.body.to_valexpr(&local_env),
            ),
            Type::Record(_) | Type::Closure(_, _) => t2::Definition::PtrFunc(
                self.name.clone(),
                vparams,
                pparams,
                self.body.to_ptrexpr(&local_env),
            ),
        }
    }

    fn extend(&self, env: &Env) -> Env {
        let mut local_env = env.clone();
        for (p, t) in self.params.iter().zip(&self.param_types) {
            assert_ne!(t, &Type::Empty);
            local_env = local_env.assoc(p.clone(), t.clone());
        }
        local_env
    }
}

impl Expression {
    fn check(&self, t: &Type, env: &Env) {
        match self {
            Expression::Const(_) => assert_eq!(t, &Type::Value),
            Expression::Record(xs, ts) => {
                assert_eq!(t, &Type::Record(RecordType { fields: ts.clone() }));
                check_expressions(xs, ts, env);
            }
            Expression::Ref(ident) => assert_eq!(env.lookup(ident), Some(t)),
            Expression::If(c, a, b) => {
                c.check(&Type::Value, env);
                a.check(t, env);
                b.check(t, env);
            }
            Expression::Call(f, args) => {
                let (params, returns) = match f.infer(env) {
                    Type::Function(FunctionType {
                        ptypes: params,
                        returns,
                    })
                    | Type::Closure(
                        FunctionType {
                            ptypes: params,
                            returns,
                        },
                        _,
                    )
                    | Type::Builtin(FunctionType {
                        ptypes: params,
                        returns,
                    }) => (params, returns),
                    t => panic!("can't call expression of type {t:?}"),
                };
                assert_eq!(&*returns, t);
                check_expressions(args, &params, env);
            }
            Expression::Lambda(_, _, _) => assert_eq!(t, &self.infer(env)),
            Expression::GetField(idx, rec) => match rec.infer(env) {
                Type::Record(RecordType { fields }) => assert_eq!(&fields[*idx as usize], t),
                o => panic!("expected record type, but got {o:?}"),
            },
        }
    }

    fn infer(&self, env: &Env) -> Type {
        match self {
            Expression::Const(_) => Type::Value,
            Expression::Ref(ident) => env.lookup(ident).unwrap().clone(),
            Expression::Record(_, ts) => Type::Record(RecordType { fields: ts.clone() }),
            Expression::If(c, a, b) => {
                c.check(&Type::Value, env);
                let t = a.infer(env);
                b.check(&t, env);
                t
            }
            Expression::Call(func, _) => match func.infer(env) {
                Type::Function(FunctionType { returns, .. })
                | Type::Closure(FunctionType { returns, .. }, _) => *returns,
                t => panic!("can't call expression of type {t:?}"),
            },
            Expression::Lambda(_, ptypes, body) => {
                let functype = FunctionType {
                    ptypes: ptypes.clone(),
                    returns: Box::new(body.infer(env)),
                };

                let fv = self.free_vars(env);
                if fv.is_empty() {
                    Type::Function(functype)
                } else {
                    Type::Closure(functype, fv)
                }
            }
            Expression::GetField(idx, rec) => match rec.infer(env) {
                Type::Record(RecordType { fields }) => fields[*idx as usize].clone(),
                o => panic!("expected record type, but got {o:?}"),
            },
        }
    }

    fn free_vars(&self, env: &Env) -> HashMap<Str, Type> {
        match self {
            Expression::Const(_) => HashMap::new(),
            Expression::Ref(ident) => {
                HashMap::from([(ident.clone(), env.lookup(ident).unwrap().clone())])
            }
            Expression::Record(args, _) => free_vars(args, env),
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
                match ft {
                    Type::Function(FunctionType { ptypes, .. }) => {
                        let (vargs, pargs) = distribute(args, &ptypes);
                        let vargs = vargs.map(|x| x.to_valexpr(env)).collect();
                        let pargs = pargs.map(|x| x.to_ptrexpr(env)).collect();
                        t2::ValExpr::CallFun(Box::new(func.to_valexpr(env)), vargs, pargs)
                    }
                    Type::Closure(FunctionType { ptypes, .. }, _) => {
                        let (vargs, pargs) = distribute(args, &ptypes);
                        let vargs = vargs.map(|x| x.to_valexpr(env)).collect();
                        let pargs = pargs.map(|x| x.to_ptrexpr(env)).collect();
                        t2::ValExpr::CallCls(Box::new(func.to_ptrexpr(env)), vargs, pargs)
                    }
                    Type::Builtin(FunctionType { ptypes, .. }) => {
                        let (vargs, pargs) = distribute(args, &ptypes);
                        let vargs = vargs.map(|x| x.to_valexpr(env)).collect();
                        let pargs = pargs.map(|x| x.to_ptrexpr(env)).collect();

                        if let Expression::Ref(name) = &**func {
                            t2::ValExpr::Builtin(name.clone(), vargs, pargs)
                        } else {
                            unimplemented!("builtin must be a direct ref (no first-class builtins)")
                        }
                    }
                    _ => todo!("{ft:?}"),
                }
            }
            Expression::Lambda(params, ptypes, body) => {
                let local_env = env.extend(params, ptypes);

                let (vparams, pparams) = distribute(params, ptypes);
                let vparams = vparams.cloned().collect();
                let pparams = pparams.cloned().collect();

                match body.infer(env) {
                    rt if rt.is_value() => t2::ValExpr::LambdaVal(
                        vparams,
                        pparams,
                        Box::new(body.to_valexpr(&local_env)),
                    ),
                    //rt if rt.is_pointer() => todo!(),
                    rt => panic!("Don't know how to deal with return type {rt:?}"),
                }
            }
            _ => panic!("expected value, got {self:?}"),
        }
    }

    fn to_ptrexpr(&self, env: &Env) -> t2::PtrExpr {
        match self {
            Expression::Record(xs, ts) => {
                let (vargs, pargs) = distribute(xs, ts);
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
                match ft {
                    Type::Function(FunctionType { ptypes, .. }) => {
                        let (vargs, pargs) = distribute(args, &ptypes);
                        let vargs = vargs.map(|x| x.to_valexpr(env)).collect();
                        let pargs = pargs.map(|x| x.to_ptrexpr(env)).collect();
                        t2::PtrExpr::CallFun(Box::new(func.to_valexpr(env)), vargs, pargs)
                    }
                    Type::Closure(FunctionType { ptypes, .. }, _) => {
                        let (vargs, pargs) = distribute(args, &ptypes);
                        let vargs = vargs.map(|x| x.to_valexpr(env)).collect();
                        let pargs = pargs.map(|x| x.to_ptrexpr(env)).collect();
                        t2::PtrExpr::CallCls(Box::new(func.to_ptrexpr(env)), vargs, pargs)
                    }
                    _ => todo!("{ft:?}"),
                }
            }
            Expression::Lambda(params, ptypes, body) => {
                let local_env = env.extend(params, ptypes);

                let free_vars = self.free_vars(env);
                let frees = free_vars.keys().cloned().collect::<Vec<_>>();
                let ftypes = free_vars.values().cloned().collect::<Vec<_>>();

                let (vfree, pfree) = distribute(&frees, &ftypes);
                let vfree = vfree.cloned().collect();
                let pfree = pfree.cloned().collect();

                let (vparams, pparams) = distribute(params, ptypes);
                let vparams = vparams.cloned().collect();
                let pparams = pparams.cloned().collect();

                match body.infer(env) {
                    rt if rt.is_value() => t2::PtrExpr::ClosureVal(
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
            _ => panic!("expected pointer, got {self:?}"),
        }
    }
}

fn distribute<'a, T>(
    things: &'a [T],
    types: &'a [Type],
) -> (impl Iterator<Item = &'a T>, impl Iterator<Item = &'a T>) {
    assert_eq!(things.len(), types.len());
    let vals = things
        .iter()
        .zip(types)
        .filter(|(_, t)| t.is_value())
        .map(|(x, _)| x);
    let ptrs = things
        .iter()
        .zip(types)
        .filter(|(_, t)| t.is_pointer())
        .map(|(x, _)| x);
    (vals, ptrs)
}

fn check_expressions(xs: &[Expression], ts: &[Type], env: &Env) {
    assert_eq!(xs.len(), ts.len());
    for (x, t) in xs.iter().zip(ts) {
        x.check(t, env)
    }
}

fn free_vars<T: Borrow<Expression>>(xs: &[T], env: &Env) -> HashMap<Str, Type> {
    xs.iter()
        .map(Borrow::borrow)
        .map(|x| x.free_vars(env))
        .fold(HashMap::new(), |mut acc, fv| {
            acc.extend(fv);
            acc
        })
}

pub fn builtin_env() -> Env {
    let vvv = Type::Builtin(FunctionType {
        ptypes: vec![Type::Value, Type::Value],
        returns: Box::new(Type::Value),
    });

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

    fn run(program: &Program) -> Int {
        println!("T3: {program:?}");
        let env = builtin_env();
        tier02_vmlang::tests::full_stack_tests::run(program.check(&env))
    }

    #[test]
    fn foo() {
        let empty = Type::Record(RecordType { fields: vec![] });
        let fndef = FunctionDefinition {
            name: "foo".into(),
            params: vec!["a".into(), "b".into(), "c".into()],
            return_type: Type::Record(RecordType {
                fields: vec![empty.clone(), Type::Value, Type::Value],
            }),
            param_types: vec![Type::Value, empty.clone(), Type::Value],
            body: Expression::Record(
                vec![
                    Expression::Ref("b".into()),
                    Expression::Ref("a".into()),
                    Expression::Ref("c".into()),
                ],
                vec![empty.clone(), Type::Value, Type::Value],
            ),
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
                return_type: Type::Value,
                param_types: vec![],
                body: Expression::Call(Box::new(Expression::Ref("foo".into())), vec![]),
            },
            FunctionDefinition {
                name: "foo".into(),
                params: vec![],
                return_type: Type::Value,
                param_types: vec![],
                body: Expression::Const(42),
            },
        ]);

        assert_eq!(run(&prog), 42);
    }

    #[test]
    fn simple_closure() {
        let prog = Program(vec![
            FunctionDefinition {
                name: "main".into(),
                params: vec![],
                return_type: Type::Value,
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
                return_type: Type::Closure(
                    FunctionType {
                        ptypes: vec![],
                        returns: Box::new(Type::Value),
                    },
                    HashMap::from([("n".into(), Type::Value)]),
                ),
                param_types: vec![Type::Value],
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
                return_type: Type::Value,
                param_types: vec![],
                body: Expression::Call(
                    Box::new(Expression::Ref("fib".into())),
                    vec![Expression::Const(6)],
                ),
            },
            FunctionDefinition {
                name: "fib".into(),
                params: vec!["n".into()],
                return_type: Type::Value,
                param_types: vec![Type::Value],
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
}
