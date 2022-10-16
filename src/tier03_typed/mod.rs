pub mod types;

use crate::env::Environment;
use crate::memory::{ChattyCollector, CopyAllocator, CopyCollector};
use crate::str::Str;
use crate::tier02_vmlang as t2;
use crate::tier03_typed::types::{Builtin, Closure, Function, RecordType, Value};
use crate::vm::{BuiltinFunctionType, Int};
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

pub trait Expression: std::fmt::Debug {
    fn check(&self, t: &dyn Type, env: &Env);
    fn get_type(&self, env: &Env) -> Rc<dyn Type>;
    fn free_vars(&self, env: &Env) -> HashMap<Str, Rc<dyn Type>>;
    fn to_valexpr(&self, env: &Env) -> t2::ValExpr;
    fn to_ptrexpr(&self, env: &Env) -> t2::PtrExpr;
}

#[derive(Debug)]
pub enum ExprEnum {
    Null,
    Const(Int),
    Record(Vec<Rc<dyn Expression>>),
    Ref(Str),
    If(Rc<dyn Expression>, Rc<dyn Expression>, Rc<dyn Expression>),
    Call(Box<ExprEnum>, Vec<Rc<dyn Expression>>),
    Lambda(Vec<Str>, Vec<Rc<dyn Type>>, Rc<dyn Expression>),
    GetField(Int, Box<ExprEnum>),
}

#[derive(Debug)]
pub struct FunctionDefinition {
    name: Str,
    params: Vec<Str>,
    body: Rc<dyn Expression>,
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

impl Expression for ExprEnum {
    fn check(&self, t: &dyn Type, env: &Env) {
        match self {
            ExprEnum::Null => assert!(t.is_pointer(env)),
            ExprEnum::Const(_) => assert!(Value.is_equal(t, env)),
            ExprEnum::Record(xs) => match t.resolve(env).as_any().downcast_ref() {
                Some(RecordType { fields }) => check_expressions(xs, fields, env),
                _ => panic!("{self:?} is not a {t:?}"),
            },
            ExprEnum::Ref(ident) => assert!(env.lookup(ident).unwrap().is_equal(t, env)),
            ExprEnum::If(c, a, b) => {
                c.check(&Value, env);
                a.check(t, env);
                b.check(t, env);
            }
            ExprEnum::Call(f, args) => {
                let ft = f.get_type(env);
                match ft.callable_signature() {
                    Some((params, returns)) => {
                        assert!(returns.is_equal(&*t, env));
                        check_expressions(args, &params, env);
                    }
                    t => panic!("can't call expression of type {t:?}"),
                }
            }
            ExprEnum::Lambda(_, _, _) => assert!(self.get_type(env).is_equal(t, env)),
            ExprEnum::GetField(idx, rec) => {
                let t_rec = rec.get_type(env);
                match t_rec.resolve(env).as_any().downcast_ref() {
                    Some(RecordType { fields }) => {
                        assert!(fields[*idx as usize].is_equal(t, env))
                    }
                    _ => panic!("expected record type, but got {t_rec:?}"),
                }
            }
        }
    }

    fn get_type(&self, env: &Env) -> Rc<dyn Type> {
        match self {
            ExprEnum::Null => panic!("can't infer type of Null"),
            ExprEnum::Const(_) => Value::new(),
            ExprEnum::Ref(ident) => env.lookup(ident).unwrap().clone(),
            ExprEnum::Record(args) => RecordType::new(get_expression_types(args, env)),
            ExprEnum::If(c, a, b) => {
                c.check(&Value, env);
                let t = a.get_type(env);
                b.check(&*t, env);
                t
            }
            ExprEnum::Call(func, _) => {
                let t = func.get_type(env);
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
            ExprEnum::Lambda(_, ptypes, body) => {
                let functype = FunctionSignature {
                    ptypes: ptypes.clone(),
                    returns: body.get_type(env),
                };

                let fv = self.free_vars(env);
                if fv.is_empty() {
                    Rc::new(Function(functype))
                } else {
                    Rc::new(Closure(functype, fv))
                }
            }
            ExprEnum::GetField(idx, rec) => match rec.get_type(env).as_any().downcast_ref() {
                Some(RecordType { fields }) => fields[*idx as usize].clone(),
                o => panic!("expected record type, but got {o:?}"),
            },
        }
    }

    fn free_vars(&self, env: &Env) -> HashMap<Str, Rc<dyn Type>> {
        match self {
            ExprEnum::Null | ExprEnum::Const(_) => HashMap::new(),
            ExprEnum::Ref(ident) => {
                HashMap::from([(ident.clone(), env.lookup(ident).unwrap().clone())])
            }
            ExprEnum::Record(args) => free_vars(args, env),
            ExprEnum::If(c, a, b) => free_vars(&[&**a, &**b, &**c], env),
            ExprEnum::Call(func, args) => {
                let mut fv = free_vars(args, env);
                fv.extend(func.free_vars(env));
                fv
            }
            ExprEnum::Lambda(params, _, body) => {
                let mut fv = body.free_vars(env);
                for p in params {
                    fv.remove(p);
                }
                fv
            }
            ExprEnum::GetField(_, rec) => rec.free_vars(env),
        }
    }

    fn to_valexpr(&self, env: &Env) -> t2::ValExpr {
        match self {
            ExprEnum::Const(x) => t2::ValExpr::Const(*x),
            ExprEnum::Ref(ident) => t2::ValExpr::Ref(ident.clone()),
            ExprEnum::If(condition, consequence, alternative) => t2::ValExpr::If(
                Box::new(condition.to_valexpr(env)),
                Box::new(consequence.to_valexpr(env)),
                Box::new(alternative.to_valexpr(env)),
            ),
            ExprEnum::Call(func, args) => {
                let ft = func.get_type(env);
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

                    if let ExprEnum::Ref(name) = &**func {
                        return t2::ValExpr::Builtin(name.clone(), vargs, pargs);
                    } else {
                        unimplemented!("builtin must be a direct ref (no first-class builtins)")
                    }
                }
                panic!("can't call a {ft:?}")
            }
            ExprEnum::Lambda(params, ptypes, body) => {
                let (vparams, pparams) = distribute(params, ptypes, env);
                let vparams = vparams.cloned().collect();
                let pparams = pparams.cloned().collect();

                let local_env = env.extend(params, ptypes);

                match body.get_type(env) {
                    rt if rt.is_value(&local_env) => t2::ValExpr::LambdaVal(
                        vparams,
                        pparams,
                        Box::new(body.to_valexpr(&local_env)),
                    ),
                    //rt if rt.is_pointer() => todo!(),
                    rt => panic!("Don't know how to deal with return type {rt:?}"),
                }
            }
            ExprEnum::GetField(idx, rec) => t2::ValExpr::GetField(
                Box::new(t2::ValExpr::Const(map_index(
                    *idx,
                    &get_recarg_types(rec, env),
                    env,
                ))),
                Box::new(rec.to_ptrexpr(env)),
            ),
            _ => panic!("expected value, got {self:?}"),
        }
    }

    fn to_ptrexpr(&self, env: &Env) -> t2::PtrExpr {
        match self {
            ExprEnum::Null => t2::PtrExpr::Null,
            ExprEnum::Record(xs) => {
                let arg_types = get_expression_types(xs, env);
                let (vargs, pargs) = distribute(xs, &arg_types, env);
                let vargs = vargs.map(|x| x.to_valexpr(env)).collect();
                let pargs = pargs.map(|x| x.to_ptrexpr(env)).collect();
                t2::PtrExpr::Record(vargs, pargs)
            }
            ExprEnum::Ref(ident) => t2::PtrExpr::Ref(ident.clone()),
            ExprEnum::If(condition, consequence, alternative) => t2::PtrExpr::If(
                Box::new(condition.to_valexpr(env)),
                Box::new(consequence.to_ptrexpr(env)),
                Box::new(alternative.to_ptrexpr(env)),
            ),
            ExprEnum::Call(func, args) => {
                let ft = func.get_type(env);
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
            ExprEnum::Lambda(params, ptypes, body) => {
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

                match body.get_type(&local_env) {
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
            ExprEnum::GetField(idx, rec) => t2::PtrExpr::GetField(
                Box::new(t2::ValExpr::Const(map_index(
                    *idx,
                    &get_recarg_types(rec, env),
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

fn check_expressions(xs: &[Rc<dyn Expression>], ts: &[Rc<dyn Type>], env: &Env) {
    assert_eq!(xs.len(), ts.len());
    for (x, t) in xs.iter().zip(ts) {
        x.check(&**t, env)
    }
}

fn get_expression_types(xs: &[Rc<dyn Expression>], env: &Env) -> Vec<Rc<dyn Type>> {
    xs.iter().map(|x| x.get_type(env)).collect()
}

fn get_recarg_types(expr: &ExprEnum, env: &Env) -> Vec<Rc<dyn Type>> {
    let trec = expr.get_type(env);
    match trec.resolve(env).as_any().downcast_ref() {
        Some(RecordType { fields }) => fields.clone(),
        _ => panic!("not a record expression {expr:?} has type {trec:?}"),
    }
}

fn free_vars<T: Borrow<dyn Expression>>(xs: &[T], env: &Env) -> HashMap<Str, Rc<dyn Type>> {
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

pub struct LanguageContext {
    t2_ctx: t2::LanguageContext,
    builtin_env: Env,
}

impl Default for LanguageContext {
    fn default() -> Self {
        let val = Rc::new(Value);
        let vvv: Rc<dyn Type> = Builtin::new(vec![val.clone(), val.clone()], val);
        let vvv = &vvv;

        let mut ctx = Self::empty();
        ctx.register_builtin("<", vvv, |mut ctx| {
            if ctx.pop_val() > ctx.pop_val() {
                Int::MAX
            } else {
                0
            }
        });
        ctx.register_builtin("+", vvv, |mut ctx| ctx.pop_val() + ctx.pop_val());
        ctx.register_builtin("-", vvv, |mut ctx| {
            let b = ctx.pop_val();
            ctx.pop_val() - b
        });
        ctx.register_builtin("*", vvv, |mut ctx| ctx.pop_val() * ctx.pop_val());
        ctx
    }
}

impl LanguageContext {
    pub fn empty() -> Self {
        LanguageContext {
            t2_ctx: t2::LanguageContext::empty(),
            builtin_env: Env::Empty,
        }
    }

    pub fn register_builtin(
        &mut self,
        name: impl Into<Str>,
        fntype: &Rc<dyn Type>,
        func: BuiltinFunctionType<CopyAllocator, ChattyCollector<CopyCollector>>,
    ) -> Int {
        let name = name.into();
        self.builtin_env = self.builtin_env.assoc(name.clone(), fntype.clone());
        self.t2_ctx.register_builtin(name, func)
    }

    pub fn run(&mut self, program: &Program) -> Int {
        println!("T3: {program:?}");
        self.t2_ctx.run(&program.check(&self.builtin_env))
    }
}

pub use parsing::parse;
mod parsing {
    use super::*;
    use lrlex::lrlex_mod;
    use lrpar::lrpar_mod;
    // Using `lrlex_mod!` brings the lexer for `calc.l` into scope. By default the
    // module name will be `calc_l` (i.e. the file name, minus any extensions,
    // with a suffix of `_l`).
    lrlex_mod!("tier03_typed/lexer.l");
    // Using `lrpar_mod!` brings the parser for `calc.y` into scope. By default the
    // module name will be `calc_y` (i.e. the file name, minus any extensions,
    // with a suffix of `_y`).
    lrpar_mod!("tier03_typed/parser.y");

    pub fn parse(src: &str) -> Program {
        let lexerdef = lexer_l::lexerdef();
        let lexer = lexerdef.lexer(src);
        let (prog, errs) = parser_y::parse(&lexer);
        for e in errs {
            println!("{}", e);
        }
        prog.unwrap().unwrap()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tier03_typed::types::{Empty, Named, Value};

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
            body: Rc::new(ExprEnum::Record(rcvec![
                ExprEnum::Ref("b".into()),
                ExprEnum::Ref("a".into()),
                ExprEnum::Ref("c".into())
            ])),
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
    fn trivial_prog() {
        let prog = parse("main : -> Int main = 42");
        assert_eq!(LanguageContext::default().run(&prog), 42);
    }

    #[test]
    fn simple_prog() {
        let prog = Program(vec![
            FunctionDefinition {
                name: "main".into(),
                params: vec![],
                return_type: Rc::new(Value),
                param_types: vec![],
                body: Rc::new(ExprEnum::Call(
                    Box::new(ExprEnum::Ref("foo".into())),
                    vec![],
                )),
            },
            FunctionDefinition {
                name: "foo".into(),
                params: vec![],
                return_type: Rc::new(Value),
                param_types: vec![],
                body: Rc::new(ExprEnum::Const(42)),
            },
        ]);

        assert_eq!(LanguageContext::default().run(&prog), 42);
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
                body: Rc::new(ExprEnum::Call(
                    Box::new(ExprEnum::Call(
                        Box::new(ExprEnum::Ref("make-fn".into())),
                        rcvec![ExprEnum::Const(42)],
                    )),
                    vec![],
                )),
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
                body: Rc::new(ExprEnum::Lambda(
                    vec![],
                    vec![],
                    Rc::new(ExprEnum::Ref("n".into())),
                )),
            },
        ]);

        assert_eq!(LanguageContext::default().run(&prog), 42);
    }

    #[test]
    fn fib() {
        let prog = Program(vec![
            FunctionDefinition {
                name: "main".into(),
                params: vec![],
                return_type: Rc::new(Value),
                param_types: vec![],
                body: Rc::new(ExprEnum::Call(
                    Box::new(ExprEnum::Ref("fib".into())),
                    rcvec![ExprEnum::Const(6)],
                )),
            },
            FunctionDefinition {
                name: "fib".into(),
                params: vec!["n".into()],
                return_type: Rc::new(Value),
                param_types: rcvec![Value],
                body: Rc::new(ExprEnum::If(
                    Rc::new(ExprEnum::Call(
                        Box::new(ExprEnum::Ref("<".into())),
                        rcvec![ExprEnum::Ref("n".into()), ExprEnum::Const(2)],
                    )),
                    Rc::new(ExprEnum::Const(1)),
                    Rc::new(ExprEnum::Call(
                        Box::new(ExprEnum::Ref("+".into())),
                        rcvec![
                            ExprEnum::Call(
                                Box::new(ExprEnum::Ref("fib".into())),
                                rcvec![ExprEnum::Call(
                                    Box::new(ExprEnum::Ref("-".into())),
                                    rcvec![ExprEnum::Ref("n".into()), ExprEnum::Const(1)],
                                )],
                            ),
                            ExprEnum::Call(
                                Box::new(ExprEnum::Ref("fib".into())),
                                rcvec![ExprEnum::Call(
                                    Box::new(ExprEnum::Ref("-".into())),
                                    rcvec![ExprEnum::Ref("n".into()), ExprEnum::Const(2)],
                                )],
                            )
                        ],
                    )),
                )),
            },
        ]);

        assert_eq!(LanguageContext::default().run(&prog), 13);
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
            body: Rc::new(ExprEnum::Null),
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
            body: Rc::new(ExprEnum::Record(rcvec![
                ExprEnum::Ref("cdr".into()),
                ExprEnum::Ref("car".into()),
            ])),
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
            body: Rc::new(ExprEnum::GetField(
                1,
                Box::new(ExprEnum::Ref("list".into())),
            )),
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
            body: Rc::new(ExprEnum::GetField(
                0,
                Box::new(ExprEnum::Ref("list".into())),
            )),
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
