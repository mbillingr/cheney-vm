use crate::env::Environment;
use crate::str::Str;
use crate::vm::{Half, Int, Op, RecordSignature};
use std::collections::HashMap;

pub type Env = Environment<Binding>;

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Binding {
    Static,
    LocalVal(Int),
    LocalPtr(Int),
    ClosedVal(Int, Int),
    ClosedPtr(Int, Int),
}

#[derive(Debug)]
enum ValExpr {
    Const(Int),
    Ref(Str),
    If(Box<ValExpr>, Box<ValExpr>, Box<ValExpr>),
    CallFun(Box<ValExpr>, Vec<ValExpr>, Vec<PtrExpr>),
    CallCls(Box<PtrExpr>, Vec<ValExpr>, Vec<PtrExpr>),
    LambdaVal(Vec<Str>, Vec<Str>, Box<ValExpr>),
    LambdaPtr(Vec<Str>, Vec<Str>, Box<PtrExpr>),
}

#[derive(Debug)]
enum PtrExpr {
    Null,
    Ref(Str),
    If(Box<ValExpr>, Box<PtrExpr>, Box<PtrExpr>),
    CallFun(Box<ValExpr>, Vec<ValExpr>, Vec<PtrExpr>),
    CallCls(Box<PtrExpr>, Vec<ValExpr>, Vec<PtrExpr>),
    ClosureVal(Vec<Str>, Vec<Str>, Vec<Str>, Vec<Str>, Box<ValExpr>),
    ClosurePtr(Vec<Str>, Vec<Str>, Vec<Str>, Vec<Str>, Box<PtrExpr>),
}

trait Compile {
    fn compile(&self, env: &Env, compiler: &mut Compiler) -> Vec<Op<Str>>;
}

impl Compile for ValExpr {
    fn compile(&self, env: &Env, compiler: &mut Compiler) -> Vec<Op<Str>> {
        match self {
            ValExpr::Const(c) => vec![Op::Const(*c)],
            ValExpr::Ref(ident) => match env.lookup(ident) {
                None => panic!("Unbound identifier: {ident}"),
                Some(Binding::Static) => vec![Op::PushAddr(ident.clone())],
                Some(Binding::LocalVal(idx)) => vec![Op::PushLocal(*idx)],
                Some(Binding::LocalPtr(_) | Binding::ClosedPtr(_, _)) => {
                    panic!("expected value, but {ident} is a pointer")
                }
                Some(Binding::ClosedVal(cls, idx)) => {
                    vec![Op::PtrPushLocal(*cls), Op::PushFrom(*idx), Op::PtrDrop(0)]
                }
            },
            ValExpr::If(a, b, c) => compiler.compile_if(a, &**b, &**c, env),
            ValExpr::CallFun(fun, vargs, pargs) => {
                compiler.compile_function_call(fun, vargs, pargs, env)
            }
            ValExpr::CallCls(cls, vargs, pargs) => {
                compiler.compile_closure_call(cls, vargs, pargs, env)
            }
            ValExpr::LambdaVal(vargs, pargs, body) => {
                let name = compiler.unique_label("lambda");
                compiler.compile_inline_function(name, vargs, pargs, &**body, env)
            }
            ValExpr::LambdaPtr(vargs, pargs, body) => {
                let name = compiler.unique_label("lambda");
                compiler.compile_inline_function(name, vargs, pargs, &**body, env)
            }
            x => todo!("{x:?}"),
        }
    }
}

impl Compile for PtrExpr {
    fn compile(&self, env: &Env, compiler: &mut Compiler) -> Vec<Op<Str>> {
        match self {
            PtrExpr::Null => vec![Op::Const(0), Op::ValToPtr],
            PtrExpr::Ref(ident) => match env.lookup(ident) {
                None => panic!("Unbound identifier: {ident}"),
                Some(Binding::Static) => todo!("static pointers??"),
                Some(Binding::LocalVal(_) | Binding::ClosedVal(_, _)) => {
                    panic!("expected pointer, but {ident} is a value")
                }
                Some(Binding::LocalPtr(idx)) => vec![Op::PtrPushLocal(*idx)],
                Some(Binding::ClosedPtr(cls, idx)) => vec![
                    Op::PtrPushLocal(*cls),
                    Op::PtrPushFrom(*idx),
                    Op::PtrDrop(1),
                ],
            },
            PtrExpr::If(a, b, c) => compiler.compile_if(a, &**b, &**c, env),
            PtrExpr::CallFun(fun, vargs, pargs) => {
                compiler.compile_function_call(fun, vargs, pargs, env)
            }
            PtrExpr::CallCls(cls, vargs, pargs) => {
                compiler.compile_closure_call(cls, vargs, pargs, env)
            }
            PtrExpr::ClosureVal(vfree, pfree, vargs, pargs, body) => {
                let name = compiler.unique_label("closure");
                compiler.compile_closure(name, vfree, pfree, vargs, pargs, &**body, env)
            }
            PtrExpr::ClosurePtr(vfree, pfree, vargs, pargs, body) => {
                let name = compiler.unique_label("closure");
                compiler.compile_closure(name, vfree, pfree, vargs, pargs, &**body, env)
            }
            x => todo!("{x:?}"),
        }
    }
}

pub struct Compiler {
    unique_counters: HashMap<Str, u64>,
}

impl Compiler {
    pub fn new() -> Self {
        Compiler {
            unique_counters: HashMap::new(),
        }
    }

    fn unique_label(&mut self, name: &str) -> Str {
        /*if let Some(n) = self.unique_counters.get_mut(name) {
            *n += 1;
            return format!("{name}-{n}").into()
        }
        todo!()*/
        format!("{name}-1").into()
    }

    fn compile_if(
        &mut self,
        condition: &ValExpr,
        consequence: &impl Compile,
        alternative: &impl Compile,
        env: &Env,
    ) -> Vec<Op<Str>> {
        let label = self.unique_label("");
        let else_label = Str::from("else".to_string() + &label);
        let endif_label = Str::from("endif".to_string() + &label);

        let mut code = condition.compile(env, self);
        code.push(Op::GoIfZero(else_label.clone()));
        code.extend(consequence.compile(env, self));
        code.extend([Op::Goto(endif_label.clone()), Op::Label(else_label)]);
        code.extend(alternative.compile(env, self));
        code.extend([Op::Label(endif_label)]);
        code
    }

    fn compile_function_call(
        &mut self,
        fun: &ValExpr,
        vargs: &Vec<ValExpr>,
        pargs: &Vec<PtrExpr>,
        env: &Env,
    ) -> Vec<Op<Str>> {
        let return_label = self.unique_label("return");
        let mut code = vec![Op::PushAddr(return_label.clone())];
        for va in vargs {
            code.extend(va.compile(env, self));
        }
        for pa in pargs {
            code.extend(pa.compile(env, self));
        }
        code.extend(fun.compile(env, self));
        code.extend([Op::PtrPushEnv, Op::Jump, Op::Label(return_label)]);
        code
    }

    fn compile_closure_call(
        &mut self,
        cls: &PtrExpr,
        vargs: &Vec<ValExpr>,
        pargs: &Vec<PtrExpr>,
        env: &Env,
    ) -> Vec<Op<Str>> {
        let return_label = self.unique_label("return");
        let mut code = vec![Op::PushAddr(return_label.clone())];
        for va in vargs {
            code.extend(va.compile(env, self));
        }
        for pa in pargs {
            code.extend(pa.compile(env, self));
        }
        code.extend(cls.compile(env, self));
        code.extend([
            Op::PushFrom(0),
            Op::PtrPushEnv,
            Op::Jump,
            Op::Label(return_label),
        ]);
        code
    }

    fn compile_inline_function(
        &mut self,
        name: Str,
        vargs: &[Str],
        pargs: &[Str],
        body: &impl Compile,
        env: &Env,
    ) -> Vec<Op<Str>> {
        let continue_execution = Str::from("after-".to_string() + &name);
        let mut code = vec![Op::goto(continue_execution.clone())];
        code.extend(self.compile_function(name.clone(), vargs, pargs, body, env));
        code.extend([Op::label(continue_execution), Op::push_addr(name)]);
        code
    }

    fn compile_function(
        &mut self,
        name: Str,
        vargs: &[Str],
        pargs: &[Str],
        body: &impl Compile,
        env: &Env,
    ) -> Vec<Op<Str>> {
        let local_env = env.without_locals().extend_local(vargs, pargs);

        let mut code = vec![Op::Label(name)];

        code.extend(self.compile_prologue(vargs.len(), pargs.len()));
        code.extend(body.compile(&local_env, self));
        code.extend(self.compile_epilogue(0, (vargs.len() + pargs.len() + 1) as Int));

        code
    }

    fn compile_prologue(&mut self, n_vargs: usize, n_pargs: usize) -> Vec<Op<Str>> {
        // store return address, previous env and args in new env structure
        let mut code = vec![
            Op::comment("new env structure"),
            Op::Alloc(RecordSignature::new(
                (n_vargs + 1) as Half,
                (n_pargs + 1) as Half,
            )),
            Op::PtrPopEnv,
        ];

        let mut idx = (n_vargs + n_pargs + 2) as Int;
        code.push(Op::comment("previous env and pointer args"));
        for _ in 0..n_pargs + 1 {
            idx -= 1;
            code.push(Op::PtrPopLocal(idx));
        }
        code.push(Op::comment("value args and return address"));
        for _ in 0..n_vargs + 1 {
            idx -= 1;
            code.push(Op::PopLocal(idx));
        }

        code
    }

    fn compile_closure_prologue(&mut self, n_vargs: usize, n_pargs: usize) -> Vec<Op<Str>> {
        // store return address, previous env and args in new env structure
        let mut code = vec![
            Op::comment("new env structure"),
            Op::Alloc(RecordSignature::new(
                (n_vargs + 1) as Half,
                (n_pargs + 2) as Half,
            )),
            Op::PtrPopEnv,
        ];

        let mut idx = (n_vargs + n_pargs + 3) as Int;
        code.push(Op::comment("previous env, closure and pointer args"));
        for _ in 0..n_pargs + 2 {
            idx -= 1;
            code.push(Op::PtrPopLocal(idx));
        }
        code.push(Op::comment("value args and return address"));
        for _ in 0..n_vargs + 1 {
            idx -= 1;
            code.push(Op::PopLocal(idx));
        }

        code
    }

    fn compile_epilogue(&mut self, retaddr_idx: Int, retenv_idx: Int) -> Vec<Op<Str>> {
        vec![
            Op::comment("restore previous env"),
            Op::PtrPushLocal(retenv_idx),
            Op::PtrPopEnv,
            Op::comment("return to address"),
            Op::PushLocal(retaddr_idx),
            Op::Jump,
        ]
    }

    fn compile_closure(
        &mut self,
        name: Str,
        vfree: &[Str],
        pfree: &[Str],
        vargs: &[Str],
        pargs: &[Str],
        body: &impl Compile,
        env: &Env,
    ) -> Vec<Op<Str>> {
        let cls_idx = (vargs.len() + 1 + pargs.len()) as Int;
        let global_env = env.without_locals();
        let closure_env = global_env.extend_closed(vfree, pfree, cls_idx);
        let local_env = closure_env.extend_local(vargs, pargs);

        let continue_execution = Str::from("after-".to_string() + &name);

        let mut code = vec![
            Op::goto(continue_execution.clone()),
            Op::Label(name.clone()),
        ];

        code.extend(self.compile_closure_prologue(vargs.len(), pargs.len()));
        code.extend(body.compile(&local_env, self));
        code.extend(self.compile_epilogue(0, (vargs.len() + pargs.len() + 2) as Int));

        code.extend([
            Op::label(continue_execution),
            Op::Alloc(RecordSignature::new(
                (vfree.len() + 1) as Half,
                pfree.len() as Half,
            )),
            Op::PushAddr(name),
            Op::PopInto(0),
        ]);

        let mut idx = 1;
        for vf in vfree {
            code.extend(ValExpr::Ref(vf.clone()).compile(env, self));
            code.push(Op::PopInto(idx));
            idx += 1;
        }
        for pf in pfree {
            code.extend(PtrExpr::Ref(pf.clone()).compile(env, self));
            code.push(Op::PtrPopInto(idx));
            idx += 1;
        }

        code
    }
}

impl Env {
    fn extend_local(&self, vargs: &[Str], pargs: &[Str]) -> Env {
        let mut env = self.clone();
        let mut idx = 1;
        for va in vargs.iter().cloned() {
            env = env.assoc(va, Binding::LocalVal(idx));
            idx += 1;
        }
        for pa in pargs.iter().cloned() {
            env = env.assoc(pa, Binding::LocalPtr(idx));
            idx += 1;
        }
        env
    }

    fn extend_closed(&self, vfree: &[Str], pfree: &[Str], cls_idx: Int) -> Env {
        let mut env = self.clone();
        let mut idx = 1;
        for va in vfree.iter().cloned() {
            env = env.assoc(va, Binding::ClosedVal(cls_idx, idx));
            idx += 1;
        }
        for pa in pfree.iter().cloned() {
            env = env.assoc(pa, Binding::ClosedPtr(cls_idx, idx));
            idx += 1;
        }
        env
    }

    fn without_locals(&self) -> Env {
        match self {
            Env::Empty => Env::Empty,
            Env::Entry(entry) => match &**entry {
                (
                    _,
                    Binding::LocalPtr(_)
                    | Binding::LocalVal(_)
                    | Binding::ClosedPtr(_, _)
                    | Binding::ClosedVal(_, _),
                    next,
                ) => next.without_locals(),
                (name, b @ Binding::Static, next) => next.without_locals().assoc(name, *b),
            },
        }
    }
}

macro_rules! vmlang {
    (($($x:tt)*)) => {
        vmlang!($($x)*)
    };

    (null) => {
        $crate::tier02_vmlang::PtrExpr::Null
    };

    (val $s:ident) => {
        $crate::tier02_vmlang::ValExpr::Ref(stringify!($s).into())
    };

    (ptr $s:ident) => {
        $crate::tier02_vmlang::PtrExpr::Ref(stringify!($s).into())
    };

    (val-if $a:tt $b:tt $c:tt) => {
        $crate::tier02_vmlang::ValExpr::If(
            Box::new(vmlang!($a)),
            Box::new(vmlang!($b)),
            Box::new(vmlang!($c))
        )
    };

    (ptr-if $a:tt $b:tt $c:tt) => {
        $crate::tier02_vmlang::PtrExpr::If(
            Box::new(vmlang!($a)),
            Box::new(vmlang!($b)),
            Box::new(vmlang!($c))
        )
    };

    (lambda->val ($($a:ident)*) ($($p:ident)*) $body:tt) => {
        $crate::tier02_vmlang::ValExpr::LambdaVal(
            vec![$(stringify!($a).into()),*],
            vec![$(stringify!($p).into()),*],
            Box::new(vmlang!($body))
        )
    };

    (lambda->ptr ($($a:ident)*) ($($p:ident)*) $body:tt) => {
        $crate::tier02_vmlang::ValExpr::LambdaPtr(
            vec![$(stringify!($a).into()),*],
            vec![$(stringify!($p).into()),*],
            Box::new(vmlang!($body))
        )
    };

    (closure->val ($($vc:ident)*) ($($pc:ident)*) ($($va:ident)*) ($($pa:ident)*) $body:tt) => {
        $crate::tier02_vmlang::PtrExpr::ClosureVal(
            vec![$(stringify!($vc).into()),*],
            vec![$(stringify!($pc).into()),*],
            vec![$(stringify!($va).into()),*],
            vec![$(stringify!($pa).into()),*],
            Box::new(vmlang!($body))
        )
    };

    (closure->ptr ($($vc:ident)*) ($($pc:ident)*) ($($va:ident)*) ($($pa:ident)*) $body:tt) => {
        $crate::tier02_vmlang::PtrExpr::ClosurePtr(
            vec![$(stringify!($vc).into()),*],
            vec![$(stringify!($pc).into()),*],
            vec![$(stringify!($va).into()),*],
            vec![$(stringify!($pa).into()),*],
            Box::new(vmlang!($body))
        )
    };

    (fun->val $f:ident ($($va:tt)*) ($($pa:tt)*)) => {
        vmlang!(fun->val (val $f) ($($va)*) ($($pa)*))
    };

    (fun->val $f:tt ($($va:tt)*) ($($pa:tt)*)) => {
        $crate::tier02_vmlang::ValExpr::CallFun(
            Box::new(vmlang!($f)),
            vec![$(vmlang!($va)),*],
            vec![$(vmlang!($pa)),*]
        )
    };

    (fun->ptr $f:ident ($($va:tt)*) ($($pa:tt)*)) => {
        vmlang!(fun->ptr (val $f) ($($va)*) ($($pa)*))
    };

    (fun->ptr $f:tt ($($va:tt)*) ($($pa:tt)*)) => {
        $crate::tier02_vmlang::PtrExpr::CallFun(
            Box::new(vmlang!($f)),
            vec![$(vmlang!($va)),*],
            vec![$(vmlang!($pa)),*]
        )
    };

    (cls->val $f:ident ($($va:tt)*) ($($pa:tt)*)) => {
        vmlang!(cls->val (ptr $f) ($($va)*) ($($pa)*))
    };

    (cls->val $f:tt ($($va:tt)*) ($($pa:tt)*)) => {
        $crate::tier02_vmlang::ValExpr::CallCls(
            Box::new(vmlang!($f)),
            vec![$(vmlang!($va)),*],
            vec![$(vmlang!($pa)),*]
        )
    };

    (cls->ptr $f:ident ($($va:tt)*) ($($pa:tt)*)) => {
        vmlang!(cls->ptr (ptr $f) ($($va)*) ($($pa)*))
    };

    (cls->ptr $f:tt ($($va:tt)*) ($($pa:tt)*)) => {
        $crate::tier02_vmlang::PtrExpr::CallCls(
            Box::new(vmlang!($f)),
            vec![$(vmlang!($va)),*],
            vec![$(vmlang!($pa)),*]
        )
    };

    ($x:expr) => {
        $crate::tier02_vmlang::ValExpr::Const($x)
    };
}

#[cfg(test)]
mod tests {
    use super::*;

    mod unit_tests {
        use super::*;
        use crate::vm::strip_comments;

        #[test]
        fn compile_ptr_null() {
            assert_eq!(
                vmlang!(null).compile(&Env::Empty, &mut Compiler::new()),
                vec![Op::Const(0), Op::ValToPtr]
            )
        }

        #[test]
        fn compile_const() {
            assert_eq!(
                vmlang!(42).compile(&Env::Empty, &mut Compiler::new()),
                vec![Op::Const(42)]
            )
        }

        #[test]
        fn compile_val_if() {
            assert_eq!(
                vmlang!(val-if 0 1 2).compile(&Env::Empty, &mut Compiler::new()),
                vec![
                    Op::Const(0),
                    Op::goto_zero("else-1"),
                    Op::Const(1),
                    Op::goto("endif-1"),
                    Op::label("else-1"),
                    Op::Const(2),
                    Op::label("endif-1")
                ]
            )
        }

        #[test]
        fn compile_ptr_if() {
            assert_eq!(
                vmlang!(ptr-if 0 null null).compile(&Env::Empty, &mut Compiler::new()),
                vec![
                    Op::Const(0),
                    Op::goto_zero("else-1"),
                    Op::Const(0),
                    Op::ValToPtr,
                    Op::goto("endif-1"),
                    Op::label("else-1"),
                    Op::Const(0),
                    Op::ValToPtr,
                    Op::label("endif-1")
                ]
            )
        }

        #[test]
        fn compile_nullary_val_lambda() {
            assert_eq!(
                strip_comments(
                    &vmlang!(lambda->val () () 123).compile(&Env::Empty, &mut Compiler::new())
                )
                .collect::<Vec<_>>(),
                vec![
                    Op::goto("after-lambda-1"),
                    Op::label("lambda-1"),
                    // prologue
                    Op::Alloc(RecordSignature::new(1, 1)),
                    Op::PtrPopEnv,
                    Op::PtrPopLocal(1),
                    Op::PopLocal(0),
                    // body
                    Op::Const(123),
                    // epilogue
                    Op::PtrPushLocal(1),
                    Op::PtrPopEnv,
                    Op::PushLocal(0),
                    Op::Jump,
                    Op::label("after-lambda-1"),
                    Op::push_addr("lambda-1"),
                ]
            )
        }

        #[test]
        fn compile_nary_val_lambda() {
            assert_eq!(
                strip_comments(
                    &vmlang!(lambda->val (a b) (x y z) (val b))
                        .compile(&Env::Empty, &mut Compiler::new())
                )
                .collect::<Vec<_>>(),
                vec![
                    Op::goto("after-lambda-1"),
                    Op::label("lambda-1"),
                    // prologue
                    Op::Alloc(RecordSignature::new(3, 4)),
                    Op::PtrPopEnv,
                    Op::PtrPopLocal(6),
                    Op::PtrPopLocal(5),
                    Op::PtrPopLocal(4),
                    Op::PtrPopLocal(3),
                    Op::PopLocal(2),
                    Op::PopLocal(1),
                    Op::PopLocal(0),
                    // body
                    Op::PushLocal(2),
                    // epilogue
                    Op::PtrPushLocal(6),
                    Op::PtrPopEnv,
                    Op::PushLocal(0),
                    Op::Jump,
                    Op::label("after-lambda-1"),
                    Op::push_addr("lambda-1"),
                ]
            )
        }

        #[test]
        fn compile_nary_ptr_lambda() {
            assert_eq!(
                strip_comments(
                    &vmlang!(lambda->ptr (a b c) (y z) (ptr y))
                        .compile(&Env::Empty, &mut Compiler::new())
                )
                .collect::<Vec<_>>(),
                vec![
                    Op::goto("after-lambda-1"),
                    Op::label("lambda-1"),
                    // prologue
                    Op::Alloc(RecordSignature::new(4, 3)),
                    Op::PtrPopEnv,
                    Op::PtrPopLocal(6),
                    Op::PtrPopLocal(5),
                    Op::PtrPopLocal(4),
                    Op::PopLocal(3),
                    Op::PopLocal(2),
                    Op::PopLocal(1),
                    Op::PopLocal(0),
                    // body
                    Op::PtrPushLocal(4),
                    // epilogue
                    Op::PtrPushLocal(6),
                    Op::PtrPopEnv,
                    Op::PushLocal(0),
                    Op::Jump,
                    Op::label("after-lambda-1"),
                    Op::push_addr("lambda-1"),
                ]
            )
        }

        #[test]
        fn compile_call_static() {
            assert_eq!(
                vmlang!(fun->val foo () ()).compile(
                    &Env::Empty.assoc("foo", Binding::Static),
                    &mut Compiler::new()
                ),
                vec![
                    Op::push_addr("return-1"),
                    Op::push_addr("foo"),
                    Op::PtrPushEnv,
                    Op::Jump,
                    Op::label("return-1"),
                ]
            )
        }

        #[test]
        fn compile_call_with_args() {
            assert_eq!(
                vmlang!(fun->val foo (1 2) (null)).compile(
                    &Env::Empty.assoc("foo", Binding::Static),
                    &mut Compiler::new()
                ),
                vec![
                    Op::push_addr("return-1"),
                    Op::Const(1),
                    Op::Const(2),
                    Op::Const(0),
                    Op::ValToPtr,
                    Op::push_addr("foo"),
                    Op::PtrPushEnv,
                    Op::Jump,
                    Op::label("return-1"),
                ]
            )
        }

        #[test]
        fn compile_call_dynamic() {
            assert_eq!(
                vmlang!(fun->val foo () ()).compile(
                    &Env::Empty.assoc("foo", Binding::LocalVal(7)),
                    &mut Compiler::new()
                ),
                vec![
                    Op::push_addr("return-1"),
                    Op::PushLocal(7),
                    Op::PtrPushEnv,
                    Op::Jump,
                    Op::label("return-1"),
                ]
            )
        }

        #[test]
        fn compile_call_ptrfunc() {
            assert_eq!(
                strip_comments(&vmlang!(fun->ptr foo () ()).compile(
                    &Env::Empty.assoc("foo", Binding::Static),
                    &mut Compiler::new()
                ))
                .collect::<Vec<_>>(),
                vec![
                    Op::push_addr("return-1"),
                    Op::push_addr("foo"),
                    Op::PtrPushEnv,
                    Op::Jump,
                    Op::label("return-1"),
                ]
            )
        }

        #[test]
        fn compile_call_val_closure() {
            assert_eq!(
                vmlang!(cls->val bar () ()).compile(
                    &Env::Empty.assoc("bar", Binding::LocalPtr(5)),
                    &mut Compiler::new()
                ),
                vec![
                    Op::push_addr("return-1"),
                    Op::PtrPushLocal(5),
                    Op::PushFrom(0),
                    Op::PtrPushEnv,
                    // Imminent Call
                    //  value stack: [RET-ADDR, V-ARGS*, FUN-ADDR]
                    //  pointer stack: [P-ARGS*, CLS=[FUN-ADDR, VARS*], ENV]
                    Op::Jump,
                    Op::label("return-1"),
                ]
            )
        }

        #[test]
        fn compile_call_ptr_closure() {
            assert_eq!(
                vmlang!(cls->ptr bar () ()).compile(
                    &Env::Empty.assoc("bar", Binding::LocalPtr(5)),
                    &mut Compiler::new()
                ),
                vec![
                    Op::push_addr("return-1"),
                    Op::PtrPushLocal(5),
                    Op::PushFrom(0),
                    Op::PtrPushEnv,
                    // Imminent Call
                    //  value stack: [RET-ADDR, V-ARGS*, FUN-ADDR]
                    //  pointer stack: [P-ARGS*, CLS=[FUN-ADDR, VARS*], ENV]
                    Op::Jump,
                    Op::label("return-1"),
                ]
            )
        }

        #[test]
        fn compile_nullary_val_closure() {
            assert_eq!(
                strip_comments(
                    &vmlang!(closure->val (v) (p) () () (val v)).compile(
                        &Env::Empty
                            .assoc("p", Binding::LocalPtr(7))
                            .assoc("v", Binding::LocalVal(3)),
                        &mut Compiler::new()
                    )
                )
                .collect::<Vec<_>>(),
                vec![
                    Op::goto("after-closure-1"),
                    Op::label("closure-1"),
                    // prologue
                    Op::Alloc(RecordSignature::new(1, 2)),
                    Op::PtrPopEnv,
                    Op::PtrPopLocal(2),
                    Op::PtrPopLocal(1),
                    Op::PopLocal(0),
                    // ENV: [RET-ADDR [@closure-1 v p] RET-ENV]
                    // body
                    Op::PtrPushLocal(1),
                    Op::PushFrom(1),
                    Op::PtrDrop(0),
                    // epilogue
                    Op::PtrPushLocal(2),
                    Op::PtrPopEnv,
                    Op::PushLocal(0),
                    Op::Jump,
                    Op::label("after-closure-1"),
                    // create closure
                    Op::Alloc(RecordSignature::new(2, 1)),
                    Op::push_addr("closure-1"),
                    Op::PopInto(0),
                    Op::PushLocal(3),
                    Op::PopInto(1),
                    Op::PtrPushLocal(7),
                    Op::PtrPopInto(2),
                ]
            )
        }

        #[test]
        fn compile_nary_val_closure() {
            assert_eq!(
                strip_comments(
                    &vmlang!(closure->val (v) (p) (a) (z) (val v)).compile(
                        &Env::Empty
                            .assoc("p", Binding::LocalPtr(7))
                            .assoc("v", Binding::LocalVal(3)),
                        &mut Compiler::new()
                    )
                )
                .collect::<Vec<_>>(),
                vec![
                    Op::goto("after-closure-1"),
                    Op::label("closure-1"),
                    // prologue
                    Op::Alloc(RecordSignature::new(2, 3)),
                    Op::PtrPopEnv,
                    Op::PtrPopLocal(4),
                    Op::PtrPopLocal(3),
                    Op::PtrPopLocal(2),
                    Op::PopLocal(1),
                    Op::PopLocal(0),
                    // ENV: [RET-ADDR a z [@closure-1 v p] RET-ENV]
                    // body
                    Op::PtrPushLocal(3),
                    Op::PushFrom(1),
                    Op::PtrDrop(0),
                    // epilogue
                    Op::PtrPushLocal(4),
                    Op::PtrPopEnv,
                    Op::PushLocal(0),
                    Op::Jump,
                    Op::label("after-closure-1"),
                    // create closure
                    Op::Alloc(RecordSignature::new(2, 1)),
                    Op::push_addr("closure-1"),
                    Op::PopInto(0),
                    Op::PushLocal(3),
                    Op::PopInto(1),
                    Op::PtrPushLocal(7),
                    Op::PtrPopInto(2),
                ]
            )
        }

        #[test]
        fn compile_nullary_ptr_closure() {
            assert_eq!(
                strip_comments(
                    &vmlang!(closure->ptr (v) (p) () () (ptr p)).compile(
                        &Env::Empty
                            .assoc("p", Binding::LocalPtr(7))
                            .assoc("v", Binding::LocalVal(3)),
                        &mut Compiler::new()
                    )
                )
                .collect::<Vec<_>>(),
                vec![
                    Op::goto("after-closure-1"),
                    Op::label("closure-1"),
                    // prologue
                    Op::Alloc(RecordSignature::new(1, 2)),
                    Op::PtrPopEnv,
                    Op::PtrPopLocal(2),
                    Op::PtrPopLocal(1),
                    Op::PopLocal(0),
                    // ENV: [RET-ADDR [@closure-1 v p] RET-ENV]
                    // body
                    Op::PtrPushLocal(1),
                    Op::PtrPushFrom(2),
                    Op::PtrDrop(1),
                    // epilogue
                    Op::PtrPushLocal(2),
                    Op::PtrPopEnv,
                    Op::PushLocal(0),
                    Op::Jump,
                    Op::label("after-closure-1"),
                    // create closure
                    Op::Alloc(RecordSignature::new(2, 1)),
                    Op::push_addr("closure-1"),
                    Op::PopInto(0),
                    Op::PushLocal(3),
                    Op::PopInto(1),
                    Op::PtrPushLocal(7),
                    Op::PtrPopInto(2),
                ]
            )
        }
    }

    mod full_stack_tests {
        /*#[test]
        fn fib() {
            let mut code = vmlang!(program
                (define (main () () -> val) (fib 5))
                (define (fib (n) () -> val)
                    (if (< n 2)
                        (+ (fib (- n 1))
                           (fib (- n 2)))))
            );
        }*/
    }
}
