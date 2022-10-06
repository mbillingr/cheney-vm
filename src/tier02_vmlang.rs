use crate::env::Environment;
use crate::str::Str;
use crate::vm::{Half, Int, Op, RecordSignature};
use std::collections::HashMap;

pub type Env = Environment<Binding>;

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Binding {
    Local(Int),
    Static,
}

#[derive(Debug)]
enum ValExpr {
    Const(Int),
    Ref(Str),
    CallFun(Box<ValExpr>, Vec<ValExpr>, Vec<PtrExpr>),
    //CallCls(Box<PtrExpr>, Vec<ValExpr>, Vec<PtrExpr>),
    LambdaVal(Vec<Str>, Vec<Str>, Box<ValExpr>),
    LambdaPtr(Vec<Str>, Vec<Str>, Box<PtrExpr>),
}

#[derive(Debug)]
enum PtrExpr {
    Null,
    Ref(Str),
    CallFun(Box<ValExpr>, Vec<ValExpr>, Vec<PtrExpr>),
    //CallCls(Box<PtrExpr>, Vec<ValExpr>, Vec<PtrExpr>),
    //ClosureVal(Vec<Str>, Vec<Str>, Box<ValExpr>),
    //ClosurePtr(Vec<Str>, Vec<Str>, Box<PtrExpr>),
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
                Some(Binding::Local(idx)) => vec![Op::PushLocal(*idx)],
            },
            ValExpr::CallFun(fun, vargs, pargs) => compiler.compile_call(fun, vargs, pargs, env),
            ValExpr::LambdaVal(vargs, pargs, body) => {
                compiler.compile_lambda(vargs, pargs, &**body, env)
            }
            ValExpr::LambdaPtr(vargs, pargs, body) => {
                compiler.compile_lambda(vargs, pargs, &**body, env)
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
                Some(Binding::Local(idx)) => vec![Op::PtrPushLocal(*idx)],
            },
            PtrExpr::CallFun(fun, vargs, pargs) => compiler.compile_call(fun, vargs, pargs, env),
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

    fn compile_call(
        &mut self,
        fun: &Box<ValExpr>,
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
        code.push(Op::PtrPushLocals);
        code.extend(fun.compile(env, self));
        code.extend([Op::Jump, Op::Label(return_label)]);
        code
    }

    fn compile_lambda(
        &mut self,
        vargs: &[Str],
        pargs: &[Str],
        body: &impl Compile,
        env: &Env,
    ) -> Vec<Op<Str>> {
        let local_env = env.extend(vargs, pargs);

        let label = self.unique_label("lambda");
        let mut code = vec![Op::Label(label)];

        code.extend(self.compile_prologue(vargs.len(), pargs.len()));
        code.extend(body.compile(&local_env, self));
        code.extend(self.compile_epilogue(vargs.len() + pargs.len()));

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
            Op::PtrPopLocals,
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

    fn compile_epilogue(&mut self, n_locals: usize) -> Vec<Op<Str>> {
        vec![
            Op::comment("restore previous env"),
            Op::PtrPushLocal((n_locals + 1) as Int),
            Op::PtrPopLocals,
            Op::comment("return to address"),
            Op::PushLocal(0),
            Op::Jump,
        ]
    }
}

impl Env {
    fn extend(&self, vargs: &[Str], pargs: &[Str]) -> Env {
        let mut env = self.clone();
        let mut idx = 1;
        for va in vargs.iter().cloned() {
            env = env.assoc(va, Binding::Local(idx));
            idx += 1;
        }
        for pa in pargs.iter().cloned() {
            env = env.assoc(pa, Binding::Local(idx));
            idx += 1;
        }
        env
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
        fn compile_nullary_val_lambda() {
            assert_eq!(
                strip_comments(
                    &vmlang!(lambda->val () () 123).compile(&Env::Empty, &mut Compiler::new())
                )
                .collect::<Vec<_>>(),
                vec![
                    Op::label("lambda-1"),
                    // prologue
                    Op::Alloc(RecordSignature::new(1, 1)),
                    Op::PtrPopLocals,
                    Op::PtrPopLocal(1),
                    Op::PopLocal(0),
                    // body
                    Op::Const(123),
                    // epilogue
                    Op::PtrPushLocal(1),
                    Op::PtrPopLocals,
                    Op::PushLocal(0),
                    Op::Jump,
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
                    Op::label("lambda-1"),
                    // prologue
                    Op::Alloc(RecordSignature::new(3, 4)),
                    Op::PtrPopLocals,
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
                    Op::PtrPopLocals,
                    Op::PushLocal(0),
                    Op::Jump,
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
                    Op::label("lambda-1"),
                    // prologue
                    Op::Alloc(RecordSignature::new(4, 3)),
                    Op::PtrPopLocals,
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
                    Op::PtrPopLocals,
                    Op::PushLocal(0),
                    Op::Jump,
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
                    Op::PtrPushLocals,
                    Op::push_addr("foo"),
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
                    Op::PtrPushLocals,
                    Op::push_addr("foo"),
                    Op::Jump,
                    Op::label("return-1"),
                ]
            )
        }

        #[test]
        fn compile_call_dynamic() {
            assert_eq!(
                vmlang!(fun->val foo () ()).compile(
                    &Env::Empty.assoc("foo", Binding::Local(7)),
                    &mut Compiler::new()
                ),
                vec![
                    Op::push_addr("return-1"),
                    Op::PtrPushLocals,
                    Op::PushLocal(7),
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
                    Op::PtrPushLocals,
                    Op::push_addr("foo"),
                    Op::Jump,
                    Op::label("return-1"),
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
