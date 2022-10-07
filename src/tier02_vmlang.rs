use crate::env::Environment;
use crate::str::Str;
use crate::vm::{Allocator, GarbageCollector, Half, Int, Op, RecordSignature, Vm};
use std::collections::HashMap;

pub type Env = Environment<Binding>;

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Binding {
    Static,
    Builtin(Int),
    LocalVal(Int),
    LocalPtr(Int),
    ClosedVal(Int, Int),
    ClosedPtr(Int, Int),
}

#[derive(Debug)]
pub struct Program(Vec<Definition>);

#[derive(Debug)]
pub enum Definition {
    ValFunc(Str, Vec<Str>, Vec<Str>, ValExpr),
    PtrFunc(Str, Vec<Str>, Vec<Str>, PtrExpr),
}

#[derive(Debug)]
pub enum Statement {
    SetValField(ValExpr, PtrExpr, ValExpr),
    SetPtrField(ValExpr, PtrExpr, PtrExpr),
    DropVal(ValExpr),
    DropPtr(PtrExpr),
}

#[derive(Debug)]
pub enum ValExpr {
    Const(Int),
    Ref(Str),
    GetField(Box<ValExpr>, Box<PtrExpr>),
    If(Box<ValExpr>, Box<ValExpr>, Box<ValExpr>),
    Builtin(Str, Vec<ValExpr>, Vec<PtrExpr>),
    CallFun(Box<ValExpr>, Vec<ValExpr>, Vec<PtrExpr>),
    CallCls(Box<PtrExpr>, Vec<ValExpr>, Vec<PtrExpr>),
    LambdaVal(Vec<Str>, Vec<Str>, Box<ValExpr>),
    LambdaPtr(Vec<Str>, Vec<Str>, Box<PtrExpr>),
    Sequence(Box<Statement>, Box<ValExpr>),
}

#[derive(Debug)]
pub enum PtrExpr {
    Null,
    Ref(Str),
    Record(Vec<ValExpr>, Vec<PtrExpr>),
    GetField(Box<ValExpr>, Box<PtrExpr>),
    If(Box<ValExpr>, Box<PtrExpr>, Box<PtrExpr>),
    CallFun(Box<ValExpr>, Vec<ValExpr>, Vec<PtrExpr>),
    CallCls(Box<PtrExpr>, Vec<ValExpr>, Vec<PtrExpr>),
    ClosureVal(Vec<Str>, Vec<Str>, Vec<Str>, Vec<Str>, Box<ValExpr>),
    ClosurePtr(Vec<Str>, Vec<Str>, Vec<Str>, Vec<Str>, Box<PtrExpr>),
    Sequence(Box<Statement>, Box<PtrExpr>),
}

impl Definition {
    pub fn name(&self) -> &Str {
        match self {
            Definition::ValFunc(name, _, _, _) => name,
            Definition::PtrFunc(name, _, _, _) => name,
        }
    }
}

impl From<ValExpr> for Box<Statement> {
    fn from(x: ValExpr) -> Self {
        Box::new(Statement::DropVal(x))
    }
}

impl From<PtrExpr> for Box<Statement> {
    fn from(x: PtrExpr) -> Self {
        Box::new(Statement::DropPtr(x))
    }
}

trait Compile {
    fn compile(&self, env: &Env, compiler: &mut Compiler) -> Vec<Op<Str>>;
}

impl Compile for Program {
    fn compile(&self, env: &Env, compiler: &mut Compiler) -> Vec<Op<Str>> {
        let mut code = vec![
            Op::push_addr("  halt  "),
            Op::Const(0),
            Op::ValToPtr,
            Op::goto("main"),
            Op::label("  halt  "),
            Op::Halt,
        ];

        let mut env = env.clone();

        for def in &self.0 {
            env = env.assoc(def.name(), Binding::Static);
        }

        for def in &self.0 {
            code.extend(def.compile(&env, compiler));
        }

        code
    }
}

impl Compile for Definition {
    fn compile(&self, env: &Env, compiler: &mut Compiler) -> Vec<Op<Str>> {
        match self {
            Definition::ValFunc(name, vargs, pargs, body) => compiler.compile_function(
                name.clone(),
                ParamPair {
                    vals: vargs,
                    ptrs: pargs,
                },
                body,
                env,
            ),
            Definition::PtrFunc(name, vargs, pargs, body) => compiler.compile_function(
                name.clone(),
                ParamPair {
                    vals: vargs,
                    ptrs: pargs,
                },
                body,
                env,
            ),
        }
    }
}

impl Compile for Statement {
    fn compile(&self, env: &Env, compiler: &mut Compiler) -> Vec<Op<Str>> {
        match self {
            Statement::SetValField(idx, rec, val) => {
                join!(
                    rec.compile(env, compiler),
                    val.compile(env, compiler),
                    idx.compile(env, compiler),
                    [Op::PopFromDyn, Op::PtrDrop(0)]
                )
            }
            Statement::SetPtrField(idx, rec, val) => {
                join!(
                    rec.compile(env, compiler),
                    val.compile(env, compiler),
                    idx.compile(env, compiler),
                    [Op::PtrPopFromDyn, Op::PtrDrop(0)]
                )
            }
            Statement::DropVal(val) => join!(val.compile(env, compiler), [Op::Drop(0)]),
            Statement::DropPtr(ptr) => join!(ptr.compile(env, compiler), [Op::PtrDrop(0)]),
        }
    }
}

impl Compile for ValExpr {
    fn compile(&self, env: &Env, compiler: &mut Compiler) -> Vec<Op<Str>> {
        match self {
            ValExpr::Const(c) => vec![Op::Const(*c)],
            ValExpr::Ref(ident) => match env.lookup(ident) {
                None => panic!("Unbound identifier: {ident}"),
                Some(Binding::Builtin(_)) => panic!("Can't dereference builtin {ident}"),
                Some(Binding::Static) => vec![Op::PushAddr(ident.clone())],
                Some(Binding::LocalVal(idx)) => vec![Op::PushLocal(*idx)],
                Some(Binding::LocalPtr(_) | Binding::ClosedPtr(_, _)) => {
                    panic!("expected value, but {ident} is a pointer")
                }
                Some(Binding::ClosedVal(cls, idx)) => {
                    vec![Op::PtrPushLocal(*cls), Op::PushFrom(*idx), Op::PtrDrop(0)]
                }
            },
            ValExpr::GetField(idx, rec) => {
                join!(
                    rec.compile(env, compiler),
                    idx.compile(env, compiler),
                    [Op::PushFromDyn, Op::PtrDrop(0)]
                )
            }
            ValExpr::If(a, b, c) => compiler.compile_if(a, &**b, &**c, env),
            ValExpr::Builtin(op, vargs, pargs) => {
                compiler.compile_builtin_call(op, vargs, pargs, env)
            }
            ValExpr::CallFun(fun, vargs, pargs) => {
                compiler.compile_function_call(fun, vargs, pargs, env)
            }
            ValExpr::CallCls(cls, vargs, pargs) => {
                compiler.compile_closure_call(cls, vargs, pargs, env)
            }
            ValExpr::LambdaVal(vargs, pargs, body) => {
                let name = compiler.unique_label("lambda");
                compiler.compile_inline_function(
                    name,
                    ParamPair {
                        vals: vargs,
                        ptrs: pargs,
                    },
                    &**body,
                    env,
                )
            }
            ValExpr::LambdaPtr(vargs, pargs, body) => {
                let name = compiler.unique_label("lambda");
                compiler.compile_inline_function(
                    name,
                    ParamPair {
                        vals: vargs,
                        ptrs: pargs,
                    },
                    &**body,
                    env,
                )
            }
            ValExpr::Sequence(first, next) => {
                join!(first.compile(env, compiler), next.compile(env, compiler))
            }
        }
    }
}

impl Compile for PtrExpr {
    fn compile(&self, env: &Env, compiler: &mut Compiler) -> Vec<Op<Str>> {
        match self {
            PtrExpr::Null => vec![Op::Const(0), Op::ValToPtr],
            PtrExpr::Ref(ident) => match env.lookup(ident) {
                None => panic!("Unbound identifier: {ident}"),
                Some(Binding::Builtin(_)) => panic!("Can't dereference builtin {ident}"),
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
            PtrExpr::Record(vargs, pargs) => compiler.compile_record(vargs, pargs, env),
            PtrExpr::GetField(idx, rec) => {
                join!(
                    rec.compile(env, compiler),
                    idx.compile(env, compiler),
                    [Op::PtrPushFromDyn, Op::PtrDrop(1)]
                )
            }
            PtrExpr::If(a, b, c) => compiler.compile_if(a, &**b, &**c, env),
            PtrExpr::CallFun(fun, vargs, pargs) => {
                compiler.compile_function_call(fun, vargs, pargs, env)
            }
            PtrExpr::CallCls(cls, vargs, pargs) => {
                compiler.compile_closure_call(cls, vargs, pargs, env)
            }
            PtrExpr::ClosureVal(vfree, pfree, vargs, pargs, body) => {
                let name = compiler.unique_label("closure");
                compiler.compile_closure(
                    name,
                    ParamPair {
                        vals: vfree,
                        ptrs: pfree,
                    },
                    ParamPair {
                        vals: vargs,
                        ptrs: pargs,
                    },
                    &**body,
                    env,
                )
            }
            PtrExpr::ClosurePtr(vfree, pfree, vargs, pargs, body) => {
                let name = compiler.unique_label("closure");
                compiler.compile_closure(
                    name,
                    ParamPair {
                        vals: vfree,
                        ptrs: pfree,
                    },
                    ParamPair {
                        vals: vargs,
                        ptrs: pargs,
                    },
                    &**body,
                    env,
                )
            }
            PtrExpr::Sequence(first, next) => {
                join!(first.compile(env, compiler), next.compile(env, compiler))
            }
        }
    }
}

pub struct Compiler {
    unique_counters: HashMap<Str, u64>,
}

impl Default for Compiler {
    fn default() -> Self {
        Self::new()
    }
}

impl Compiler {
    pub fn new() -> Self {
        Compiler {
            unique_counters: HashMap::new(),
        }
    }

    fn unique_label(&mut self, name: &str) -> Str {
        if let Some(n) = self.unique_counters.get_mut(name) {
            *n += 1;
            return format!("{name}-{n}").into();
        }
        self.unique_counters.insert(name.into(), 1);
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

    fn compile_record(
        &mut self,
        vargs: &Vec<ValExpr>,
        pargs: &Vec<PtrExpr>,
        env: &Env,
    ) -> Vec<Op<Str>> {
        let mut code = vec![Op::Alloc(RecordSignature::new(
            vargs.len() as Half,
            pargs.len() as Half,
        ))];
        code.extend(self.compile_fill_record(0, vargs.iter(), pargs.iter(), env));
        code
    }

    fn compile_builtin_call(
        &mut self,
        op: &Str,
        vargs: &Vec<ValExpr>,
        pargs: &Vec<PtrExpr>,
        env: &Env,
    ) -> Vec<Op<Str>> {
        let mut code = vec![];
        for va in vargs {
            code.extend(va.compile(env, self));
        }
        for pa in pargs {
            code.extend(pa.compile(env, self));
        }
        let idx = match env.lookup(op) {
            None => panic!("unbound identifier: {op}"),
            Some(Binding::Builtin(idx)) => *idx,
            Some(_) => panic!("not a builtin operation: {op}"),
        };
        code.extend([Op::CallBuiltin(idx)]);
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
        args: ParamPair,
        body: &impl Compile,
        env: &Env,
    ) -> Vec<Op<Str>> {
        let continue_execution = Str::from("after-".to_string() + &name);
        let mut code = vec![Op::goto(continue_execution.clone())];
        code.extend(self.compile_function(name.clone(), args, body, env));
        code.extend([Op::label(continue_execution), Op::push_addr(name)]);
        code
    }

    fn compile_function(
        &mut self,
        name: Str,
        args: ParamPair,
        body: &impl Compile,
        env: &Env,
    ) -> Vec<Op<Str>> {
        let local_env = env.without_locals().extend_local(args);

        let mut code = vec![Op::Label(name)];

        code.extend(self.compile_prologue(args.vals.len(), args.ptrs.len()));
        code.extend(body.compile(&local_env, self));
        code.extend(self.compile_epilogue(0, (args.vals.len() + args.ptrs.len() + 1) as Int));

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
            Op::comment("return to address"),
            Op::PushLocal(retaddr_idx),
            Op::comment("restore previous env"),
            Op::PtrPushLocal(retenv_idx),
            Op::PtrPopEnv,
            Op::Jump,
        ]
    }

    fn compile_closure(
        &mut self,
        name: Str,
        free: ParamPair,
        args: ParamPair,
        body: &impl Compile,
        env: &Env,
    ) -> Vec<Op<Str>> {
        let cls_idx = (args.vals.len() + 1 + args.ptrs.len()) as Int;
        let global_env = env.without_locals();
        let closure_env = global_env.extend_closed(free, cls_idx);
        let local_env = closure_env.extend_local(args);

        let continue_execution = Str::from("after-".to_string() + &name);

        let mut code = vec![
            Op::goto(continue_execution.clone()),
            Op::Label(name.clone()),
        ];

        code.extend(self.compile_closure_prologue(args.vals.len(), args.ptrs.len()));
        code.extend(body.compile(&local_env, self));
        code.extend(self.compile_epilogue(0, (args.vals.len() + args.ptrs.len() + 2) as Int));

        code.extend([
            Op::label(continue_execution),
            Op::Alloc(RecordSignature::new(
                (free.vals.len() + 1) as Half,
                free.ptrs.len() as Half,
            )),
            Op::PushAddr(name),
            Op::PopInto(0),
        ]);

        code.extend(self.compile_fill_record(
            1,
            free.vals.iter().cloned().map(ValExpr::Ref),
            free.ptrs.iter().cloned().map(PtrExpr::Ref),
            env,
        ));

        code
    }

    fn compile_fill_record<V: std::borrow::Borrow<ValExpr>, P: std::borrow::Borrow<PtrExpr>>(
        &mut self,
        mut idx: Int,
        vals: impl Iterator<Item = V>,
        ptrs: impl Iterator<Item = P>,
        env: &Env,
    ) -> Vec<Op<Str>> {
        let mut code = vec![];

        for v in vals {
            code.extend(v.borrow().compile(env, self));
            code.push(Op::PopInto(idx));
            idx += 1;
        }
        for p in ptrs {
            code.extend(p.borrow().compile(env, self));
            code.push(Op::PtrPopInto(idx));
            idx += 1;
        }

        code
    }
}

#[derive(Copy, Clone)]
struct ParamPair<'a> {
    vals: &'a [Str],
    ptrs: &'a [Str],
}

impl Env {
    fn extend_local(&self, args: ParamPair) -> Env {
        let mut env = self.clone();
        let mut idx = 1;
        for va in args.vals.iter().cloned() {
            env = env.assoc(va, Binding::LocalVal(idx));
            idx += 1;
        }
        for pa in args.ptrs.iter().cloned() {
            env = env.assoc(pa, Binding::LocalPtr(idx));
            idx += 1;
        }
        env
    }

    fn extend_closed(&self, free: ParamPair, cls_idx: Int) -> Env {
        let mut env = self.clone();
        let mut idx = 1;
        for va in free.vals.iter().cloned() {
            env = env.assoc(va, Binding::ClosedVal(cls_idx, idx));
            idx += 1;
        }
        for pa in free.ptrs.iter().cloned() {
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
                (name, b @ (Binding::Static | Binding::Builtin(_)), next) => {
                    next.without_locals().assoc(name, *b)
                }
            },
        }
    }
}

const BUILTIN_LT: Int = 0;
const BUILTIN_ADD: Int = 1;
const BUILTIN_SUB: Int = 2;
const BUILTIN_MUL: Int = 3;

pub fn register_builtins<AC: Allocator, GC: GarbageCollector>(vm: &mut Vm<AC, GC>) {
    vm.register_builtin(BUILTIN_LT, "<", |mut ctx| {
        if ctx.pop_val() > ctx.pop_val() {
            Int::MAX
        } else {
            0
        }
    });
    vm.register_builtin(BUILTIN_ADD, "+", |mut ctx| ctx.pop_val() + ctx.pop_val());
    vm.register_builtin(BUILTIN_SUB, "-", |mut ctx| {
        let b = ctx.pop_val();
        ctx.pop_val() - b
    });
    vm.register_builtin(BUILTIN_MUL, "*", |mut ctx| ctx.pop_val() * ctx.pop_val());
}

pub fn builtin_env() -> Env {
    let mut env = Env::Empty;
    env = env.assoc("<", Binding::Builtin(BUILTIN_LT));
    env = env.assoc("+", Binding::Builtin(BUILTIN_ADD));
    env = env.assoc("-", Binding::Builtin(BUILTIN_SUB));
    env = env.assoc("*", Binding::Builtin(BUILTIN_MUL));
    env
}

#[macro_export]
macro_rules! vmlang {
    (($($x:tt)*)) => {
        vmlang!($($x)*)
    };

    (program $($def:tt)*) => {
        $crate::tier02_vmlang::Program(vec![$(vmlang!($def)),*])
    };

    (define ($name:ident($($a:ident)*) ($($p:ident)*) -> val) $body:tt) => {
        $crate::tier02_vmlang::Definition::ValFunc(
            stringify!($name).into(),
            vec![$(stringify!($a).into()),*],
            vec![$(stringify!($p).into()),*],
            vmlang!($body)
        )
    };

    (define ($name:ident($($a:ident)*) ($($p:ident)*) -> ptr) $body:tt) => {
        $crate::tier02_vmlang::Definition::PtrFunc(
            stringify!($name).into(),
            vec![$(stringify!($a).into()),*],
            vec![$(stringify!($p).into()),*],
            vmlang!($body)
        )
    };

    (begin-val $x:tt) => {
        vmlang!($x)
    };

    (begin-val $x:tt $($rest:tt)+) => {
        $crate::tier02_vmlang::ValExpr::Sequence(
            vmlang!($x).into(),
            Box::new(vmlang!(begin-val $($rest)+))
        )
    };

    (begin-ptr $x:tt) => {
        vmlang!($x)
    };

    (begin-ptr $x:tt $($rest:tt)+) => {
        $crate::tier02_vmlang::PtrExpr::Sequence(
            vmlang!($x).into(),
            Box::new(vmlang!(begin-ptr $($rest)+))
        )
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

    (record ($($v:tt)*) ($($p:tt)*)) => {
        $crate::tier02_vmlang::PtrExpr::Record(
            vec![$(vmlang!($v)),*],
            vec![$(vmlang!($p)),*]
        )
    };

    (get-val $idx:tt $rec:tt) => {
        $crate::tier02_vmlang::ValExpr::GetField(
            Box::new(vmlang!($idx)),
            Box::new(vmlang!($rec))
        )
    };

    (get-ptr $idx:tt $rec:tt) => {
        $crate::tier02_vmlang::PtrExpr::GetField(
            Box::new(vmlang!($idx)),
            Box::new(vmlang!($rec))
        )
    };

    (set-val! $idx:tt $rec:tt $val:tt) => {
        $crate::tier02_vmlang::Statement::SetValField(vmlang!($idx), vmlang!($rec), vmlang!($val))
    };

    (set-ptr! $idx:tt $rec:tt $val:tt) => {
        $crate::tier02_vmlang::Statement::SetPtrField(vmlang!($idx), vmlang!($rec), vmlang!($val))
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

    ($op:tt ($($va:tt)*) ($($pa:tt)*)) => {
        $crate::tier02_vmlang::ValExpr::Builtin(
            Str::from(stringify!($op)),
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
        fn compile_record() {
            assert_eq!(
                vmlang!(record (1 2) (null (record (3) ())))
                    .compile(&Env::Empty, &mut Compiler::new()),
                vec![
                    Op::Alloc(RecordSignature::new(2, 2)),
                    Op::Const(1),
                    Op::PopInto(0),
                    Op::Const(2),
                    Op::PopInto(1),
                    Op::Const(0),
                    Op::ValToPtr,
                    Op::PtrPopInto(2),
                    Op::Alloc(RecordSignature::new(1, 0)),
                    Op::Const(3),
                    Op::PopInto(0),
                    Op::PtrPopInto(3)
                ]
            )
        }

        #[test]
        fn compile_get_valfield() {
            assert_eq!(
                vmlang!(get-val 99 null).compile(&Env::Empty, &mut Compiler::new()),
                vec![
                    Op::Const(0),
                    Op::ValToPtr,
                    Op::Const(99),
                    Op::PushFromDyn,
                    Op::PtrDrop(0),
                ]
            )
        }

        #[test]
        fn compile_get_ptrfield() {
            assert_eq!(
                vmlang!(get-ptr 5 null).compile(&Env::Empty, &mut Compiler::new()),
                vec![
                    Op::Const(0),
                    Op::ValToPtr,
                    Op::Const(5),
                    Op::PtrPushFromDyn,
                    Op::PtrDrop(1),
                ]
            )
        }

        #[test]
        fn compile_set_valfield() {
            assert_eq!(
                vmlang!(set-val! 99 null 42).compile(&Env::Empty, &mut Compiler::new()),
                vec![
                    Op::Const(0),
                    Op::ValToPtr,
                    Op::Const(42),
                    Op::Const(99),
                    Op::PopFromDyn,
                    Op::PtrDrop(0),
                ]
            )
        }

        #[test]
        fn compile_set_ptrfield() {
            assert_eq!(
                vmlang!(set-ptr! 99 null null).compile(&Env::Empty, &mut Compiler::new()),
                vec![
                    Op::Const(0),
                    Op::ValToPtr,
                    Op::Const(0),
                    Op::ValToPtr,
                    Op::Const(99),
                    Op::PtrPopFromDyn,
                    Op::PtrDrop(0),
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
                    Op::PushLocal(0),
                    Op::PtrPushLocal(1),
                    Op::PtrPopEnv,
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
                    Op::PushLocal(0),
                    Op::PtrPushLocal(6),
                    Op::PtrPopEnv,
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
                    Op::PushLocal(0),
                    Op::PtrPushLocal(6),
                    Op::PtrPopEnv,
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
        fn compile_pass_call_result_to_call() {
            assert_eq!(
                vmlang!(fun->val foo ((fun->val bar () ())) ()).compile(
                    &Env::Empty
                        .assoc("foo", Binding::Static)
                        .assoc("bar", Binding::Static),
                    &mut Compiler::new()
                ),
                vec![
                    Op::push_addr("return-1"),
                    Op::push_addr("return-2"),
                    Op::push_addr("bar"),
                    Op::PtrPushEnv,
                    Op::Jump,
                    Op::label("return-2"),
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
                    Op::PushLocal(0),
                    Op::PtrPushLocal(2),
                    Op::PtrPopEnv,
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
                    Op::PushLocal(0),
                    Op::PtrPushLocal(4),
                    Op::PtrPopEnv,
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
                    Op::PushLocal(0),
                    Op::PtrPushLocal(2),
                    Op::PtrPopEnv,
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
        fn compile_val_sequence() {
            assert_eq!(
                vmlang!(begin-val 1 2 3).compile(&Env::Empty, &mut Compiler::new()),
                vec![
                    Op::Const(1),
                    Op::Drop(0),
                    Op::Const(2),
                    Op::Drop(0),
                    Op::Const(3),
                ]
            )
        }

        #[test]
        fn compile_ptr_sequence() {
            assert_eq!(
                vmlang!(begin-ptr null null).compile(&Env::Empty, &mut Compiler::new()),
                vec![
                    Op::Const(0),
                    Op::ValToPtr,
                    Op::PtrDrop(0),
                    Op::Const(0),
                    Op::ValToPtr,
                ]
            )
        }

        #[test]
        fn compile_empty_program() {
            assert_eq!(
                vmlang!(program).compile(&Env::Empty, &mut Compiler::new()),
                vec![
                    Op::push_addr("  halt  "),
                    Op::Const(0),
                    Op::ValToPtr,
                    Op::goto("main"),
                    Op::label("  halt  "),
                    Op::Halt
                ]
            )
        }

        #[test]
        fn compile_simple_program() {
            assert_eq!(
                strip_comments(
                    &vmlang!(program (define (main () () -> val) 42))
                        .compile(&Env::Empty, &mut Compiler::new())
                )
                .collect::<Vec<_>>(),
                vec![
                    Op::push_addr("  halt  "),
                    Op::Const(0),
                    Op::ValToPtr,
                    Op::goto("main"),
                    Op::label("  halt  "),
                    Op::Halt,
                    Op::label("main"),
                    Op::Alloc(RecordSignature::new(1, 1)),
                    Op::PtrPopEnv,
                    Op::PtrPopLocal(1),
                    Op::PopLocal(0),
                    Op::Const(42),
                    Op::PushLocal(0),
                    Op::PtrPushLocal(1),
                    Op::PtrPopEnv,
                    Op::Jump,
                ]
            )
        }
    }

    mod full_stack_tests {
        use super::*;
        use crate::vm::{transform_labels, Vm};

        fn run(program: Program) -> Int {
            let code = program.compile(&builtin_env(), &mut Compiler::new());
            for op in &code {
                println!("{op:?}");
            }
            let code: Vec<_> = transform_labels(&code).collect();
            let mut vm = Vm::default();
            register_builtins(&mut vm);
            vm.run(&code)
        }

        #[test]
        fn simple_program() {
            assert_eq!(
                run(vmlang!(program
                    (define (main () () -> val) 42))),
                42
            );
        }

        #[test]
        fn multiple_calls() {
            assert_eq!(
                run(vmlang!(program
                    (define (main () () -> val) (fun->val foo () ()))
                    (define (foo () () -> val) 42)
                )),
                42
            );
        }

        #[test]
        fn add_numbers() {
            assert_eq!(
                run(vmlang!(program
                    (define (main () () -> val) (+ (1 2) ())))),
                3
            );
        }

        #[test]
        fn closure() {
            assert_eq!(
                run(vmlang!(program
                    (define (main () () -> val)
                        (cls->val (fun->ptr store (123) ()) () ()))
                    (define (store (n) () -> ptr)
                        (closure->val (n) () () () (val n)))
                )),
                123
            );
        }

        #[test]
        fn fibonacci() {
            assert_eq!(
                run(vmlang!(program
                    (define (main () () -> val) (fun->val fib (5) ()))
                    (define (fib (n) () -> val)
                        (val-if (< ((val n) 2) ())
                            1
                            (+ ((fun->val fib ((- ((val n) 1)())) ())
                                (fun->val fib ((- ((val n) 2)())) ()))
                               ())))
                )),
                8
            );
        }

        #[test]
        fn side_effects() {
            assert_eq!(
                run(vmlang!(program
                    (define (main () () -> val)
                        // pass new record to foo
                        (fun->val foo () ((record (0) ()))))
                    (define (foo () (rec) -> val)
                        // call bar with rec and return first field of rec
                        (begin-val
                            (fun->val bar () ((ptr rec)))
                            (get-val 0 (ptr rec))))
                    (define (bar () (rec) -> val)
                        // set first field of rec and return arbitrary number
                        (begin-val
                            (set-val! 0 (ptr rec) 42)
                            123))
                )),
                42
            );
        }
    }
}
