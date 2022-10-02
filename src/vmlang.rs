//! super-simple functional semantics on top of the VM

use crate::vm::{Allocator, GarbageCollector, Half, Int, Op, RecordSignature, Vm};
use crate::{Serialize, StrStruct};
use std::fmt::Debug;
use std::rc::Rc;

pub trait Ast: Debug + Compilable + Serialize {}
pub trait ValExpression: Ast {}
pub trait PtrExpression: Ast {}
pub trait TailStatement: Ast {}

pub trait Compilable {
    fn compile(&self, env: &Env, compiler: &mut Compiler) -> Vec<Op<String>>;
}

mark!(
    Ast: Program,
    FuncDef,
    Const,
    ValRef,
    Lambda,
    ValOperation,
    PtrNull,
    PtrRef,
    Record,
    Closure,
    Halt,
    CallStatic,
    CallDynamic,
    CallClosure,
    ValIf,
    PtrIf,
    TailIf
);
mark!(ValExpression: Const, ValRef, Lambda, ValOperation, ValIf);
mark!(PtrExpression: PtrNull, PtrRef, Record, Closure, PtrIf);
mark!(
    TailStatement: Halt,
    CallStatic,
    CallDynamic,
    CallClosure,
    TailIf
);

macro_rules! vm_ast {
    ((program $($def:tt)*)) => {
        Program::new(vec![$(vm_ast!{$def}),*])
    };

    ((define ($name:ident ($($vparam:ident)*) ($($pparam:ident)*)) $body:tt)) => {
        FuncDef::new(
            stringify!($name),
            vec![$(stringify!($vparam).to_string()),*],
            vec![$(stringify!($pparam).to_string()),*],
            vm_ast!{$body}
        )
    };

    ((const $x:expr)) => { Const($x) };

    ((val-ref $i:ident)) => { ValRef(stringify!($i).to_string()) };

    ((lambda ($($vparam:ident)*) ($($pparam:ident)*) $body:tt)) => {
        Lambda::new(
            vec![$(stringify!($vparam).to_string()),*],
            vec![$(stringify!($pparam).to_string()),*],
            vm_ast!{$body}
        )
    };

    ((val-op $op:tt ($($vals:tt)*) ($($ptrs:tt)*))) => {
        ValOperation::new(
            stringify!($op),
            boxvec![$(vm_ast!{$vals}),*],
            boxvec![$(vm_ast!{$ptrs}),*]
        )
    };

    ((ptr-null)) => { PtrNull };

    ((ptr-ref $i:ident)) => { PtrRef(stringify!($i).to_string()) };

    ((record ($($vals:tt)*) ($($ptrs:tt)*))) => {
        Record::new(
            boxvec![$(vm_ast!{$vals}),*],
            boxvec![$(vm_ast!{$ptrs}),*]
        )
    };

    ((closure ($($var:ident)*) $lambda:tt)) => {
        Closure::new(
            vec![$(stringify!($var).to_string()),*],
            vm_ast!{$lambda}
        )
    };

    ((halt! $val:tt)) => { Halt::new(vm_ast!{$val}) };

    ((call-static $func:ident ($($vals:tt)*) ($($ptrs:tt)*))) => {
        CallStatic::new(
            stringify!($func),
            boxvec![$(vm_ast!{$vals}),*],
            boxvec![$(vm_ast!{$ptrs}),*]
        )
    };

    ((call-dynamic $func:tt ($($vals:tt)*) ($($ptrs:tt)*))) => {
        CallDynamic::new(
            vm_ast!{$func},
            boxvec![$(vm_ast!{$vals}),*],
            boxvec![$(vm_ast!{$ptrs}),*]
        )
    };

    ((call-closure $cls:tt ($($vals:tt)*) ($($ptrs:tt)*))) => {
        CallClosure::new(
            vm_ast!{$cls},
            boxvec![$(vm_ast!{$vals}),*],
            boxvec![$(vm_ast!{$ptrs}),*]
        )
    };

    ((val-if $a:tt $b:tt $c:tt)) => { ValIf::new(vm_ast!{$a}, vm_ast!{$b}, vm_ast!{$c}) };

    ((ptr-if $a:tt $b:tt $c:tt)) => { PtrIf::new(vm_ast!{$a}, vm_ast!{$b}, vm_ast!{$c}) };

    ((tail-if $a:tt $b:tt $c:tt)) => { TailIf::new(vm_ast!{$a}, vm_ast!{$b}, vm_ast!{$c}) };
}

impl Serialize for Box<dyn ValExpression> {
    fn serialize(&self) -> StrStruct {
        (**self).serialize()
    }
}

impl Serialize for Box<dyn PtrExpression> {
    fn serialize(&self) -> StrStruct {
        (**self).serialize()
    }
}

#[derive(Debug)]
struct Program {
    defs: Vec<FuncDef>,
}

impl Program {
    pub fn new(defs: Vec<FuncDef>) -> Self {
        Program { defs }
    }
}

impl Serialize for Program {
    fn serialize(&self) -> StrStruct {
        strx!(("program", &self.defs))
    }
}

impl Compilable for Program {
    fn compile(&self, env: &Env, compiler: &mut Compiler) -> Vec<Op<String>> {
        let mut global_env = env.clone();
        for def in &self.defs {
            global_env = global_env.assoc(def.name.clone(), Binding::Static);
        }

        let mut code = vec![Op::goto("main")];

        for def in &self.defs {
            code.extend(def.compile(&global_env, compiler))
        }

        code
    }
}

#[derive(Debug)]
struct FuncDef {
    name: String,
    val_params: Vec<String>,
    ptr_params: Vec<String>,
    body: Box<dyn TailStatement>,
}

impl FuncDef {
    pub fn new(
        name: impl ToString,
        val_params: Vec<String>,
        ptr_params: Vec<String>,
        body: impl TailStatement + 'static,
    ) -> Self {
        FuncDef {
            name: name.to_string(),
            val_params,
            ptr_params,
            body: Box::new(body),
        }
    }
}

impl Serialize for FuncDef {
    fn serialize(&self) -> StrStruct {
        strx!((
            "define",
            self.name,
            self.val_params,
            self.ptr_params,
            self.body,
        ))
    }
}

impl Compilable for FuncDef {
    fn compile(&self, env: &Env, compiler: &mut Compiler) -> Vec<Op<String>> {
        let local_env = env.without_locals();
        join!(
            vec![Op::label(&self.name)],
            compiler.compile_function(&self.val_params, &self.ptr_params, &*self.body, local_env)
        )
    }
}

#[derive(Debug, Clone)]
pub struct Const(pub Int);

impl Compilable for Const {
    fn compile(&self, _env: &Env, _compiler: &mut Compiler) -> Vec<Op<String>> {
        vec![Op::Const(self.0)]
    }
}

impl Serialize for Const {
    fn serialize(&self) -> StrStruct {
        strx!(("const", self.0))
    }
}

#[derive(Debug)]
pub struct ValRef(String);

impl ValRef {
    pub fn new(identifier: impl ToString) -> Self {
        ValRef(identifier.to_string())
    }
}

impl Serialize for ValRef {
    fn serialize(&self) -> StrStruct {
        strx!(("val-ref", self.0))
    }
}

impl Compilable for ValRef {
    fn compile(&self, env: &Env, compiler: &mut Compiler) -> Vec<Op<String>> {
        compiler.compile_valref(&self.0, env)
    }
}

#[derive(Debug)]
struct Lambda {
    val_params: Vec<String>,
    ptr_params: Vec<String>,
    body: Box<dyn TailStatement>,
}

impl Lambda {
    pub fn new(
        val_params: Vec<String>,
        ptr_params: Vec<String>,
        body: impl TailStatement + 'static,
    ) -> Self {
        Lambda {
            val_params,
            ptr_params,
            body: Box::new(body),
        }
    }
}

impl Serialize for Lambda {
    fn serialize(&self) -> StrStruct {
        strx!(("lambda", self.val_params, self.ptr_params))
    }
}

impl Compilable for Lambda {
    fn compile(&self, env: &Env, compiler: &mut Compiler) -> Vec<Op<String>> {
        let lam_label = compiler.unique_label("lambda");
        let end_label = compiler.unique_label("end-lambda");
        let code = vec![Op::Goto(end_label.clone()), Op::Label(lam_label.clone())];

        let local_env = env.without_locals();

        join!(
            code,
            compiler.compile_function(&self.val_params, &self.ptr_params, &*self.body, local_env),
            [
                Op::Label(end_label.clone()),
                Op::PushAddr(lam_label.clone())
            ]
        )
    }
}

#[derive(Debug)]
struct ValOperation {
    operator: String,
    var_args: Vec<Box<dyn ValExpression>>,
    ptr_args: Vec<Box<dyn PtrExpression>>,
}

impl ValOperation {
    pub fn new(
        operator: impl ToString,
        var_args: Vec<Box<dyn ValExpression>>,
        ptr_args: Vec<Box<dyn PtrExpression>>,
    ) -> Self {
        ValOperation {
            operator: operator.to_string(),
            var_args,
            ptr_args,
        }
    }
}

impl Serialize for ValOperation {
    fn serialize(&self) -> StrStruct {
        self.var_args.serialize()
        //strx!(("val-op", self.operator, self.var_args, self.ptr_args))
    }
}

impl Compilable for ValOperation {
    fn compile(&self, env: &Env, compiler: &mut Compiler) -> Vec<Op<String>> {
        let builtin_idx = match env.lookup(&self.operator) {
            None => panic!("unbound identifier {}", self.operator),
            Some(Binding::Builtin(idx)) => *idx,
            Some(_) => panic!("{} is not a builtin operator", self.operator),
        };
        let mut code = compiler.compile_args(&self.var_args, &self.ptr_args, env);
        code.extend([Op::CallBuiltin(builtin_idx)]);
        code
    }
}

#[derive(Debug, Clone)]
pub struct PtrNull;

impl Serialize for PtrNull {
    fn serialize(&self) -> StrStruct {
        strx!("ptr-null")
    }
}

impl Compilable for PtrNull {
    fn compile(&self, _env: &Env, _compiler: &mut Compiler) -> Vec<Op<String>> {
        vec![Op::Const(0), Op::ValToPtr]
    }
}

#[derive(Debug)]
pub struct PtrRef(String);

impl PtrRef {
    pub fn new(identifier: impl ToString) -> Self {
        PtrRef(identifier.to_string())
    }
}

impl Serialize for PtrRef {
    fn serialize(&self) -> StrStruct {
        strx!(("ptr-ref", self.0))
    }
}

impl Compilable for PtrRef {
    fn compile(&self, env: &Env, compiler: &mut Compiler) -> Vec<Op<String>> {
        compiler.compile_ptrref(&self.0, env)
    }
}

#[derive(Debug)]
struct Record {
    val_fields: Vec<Box<dyn ValExpression>>,
    ptr_fields: Vec<Box<dyn PtrExpression>>,
}

impl Record {
    pub fn new(
        val_fields: Vec<Box<dyn ValExpression>>,
        ptr_fields: Vec<Box<dyn PtrExpression>>,
    ) -> Self {
        Record {
            val_fields,
            ptr_fields,
        }
    }
}

impl Serialize for Record {
    fn serialize(&self) -> StrStruct {
        strx!(("record", self.val_fields, self.ptr_fields))
    }
}

impl Compilable for Record {
    fn compile(&self, env: &Env, compiler: &mut Compiler) -> Vec<Op<String>> {
        let mut idx = 0;
        let mut code = vec![Op::Alloc(RecordSignature::new(
            self.val_fields.len() as Half,
            self.ptr_fields.len() as Half,
        ))];
        for ix in &self.val_fields {
            code.extend(ix.compile(env, compiler));
            code.extend([Op::PopInto(idx)]);
            idx += 1;
        }
        for px in &self.ptr_fields {
            code.extend(px.compile(env, compiler));
            code.extend([Op::PtrPopInto(idx)]);
            idx += 1;
        }
        code
    }
}

#[derive(Debug)]
struct Closure {
    closed_vars: Vec<String>,
    lambda: Lambda,
}

impl Closure {
    pub fn new(closed_vars: Vec<String>, lambda: Lambda) -> Self {
        Closure {
            closed_vars,
            lambda,
        }
    }
}

impl Serialize for Closure {
    fn serialize(&self) -> StrStruct {
        strx!(("closure", self.closed_vars, self.lambda))
    }
}

impl Compilable for Closure {
    fn compile(&self, env: &Env, compiler: &mut Compiler) -> Vec<Op<String>> {
        let mut n_val = 0;
        let mut n_ptr = 0;
        for cv in &self.closed_vars {
            match env.lookup(cv) {
                None => panic!("unbound variable {cv}"),
                Some(b) if b.is_value() => n_val += 1,
                Some(b) if b.is_pointer() => n_ptr += 1,
                Some(b) => panic!("can't capture {b:?} in closure"),
            }
        }

        let mut code = vec![];
        code.extend([
            Op::Alloc(RecordSignature::new(1, 1)),
            Op::Alloc(RecordSignature::new(n_val, n_ptr)),
        ]);

        let mut closed_env = env.without_locals();
        let mut idx = 0;
        for cv in self
            .closed_vars
            .iter()
            .filter(|cv| env.lookup(cv).unwrap().is_value())
        {
            code.extend(compiler.compile_valref(cv, env));
            code.extend([Op::PopInto(idx)]);
            closed_env = closed_env.assoc(cv, Binding::ClosedVal(idx));
            idx += 1;
        }
        for cv in self
            .closed_vars
            .iter()
            .filter(|cv| env.lookup(cv).unwrap().is_pointer())
        {
            code.extend(compiler.compile_ptrref(cv, env));
            code.extend([Op::PtrPopInto(idx)]);
            closed_env = closed_env.assoc(cv, Binding::ClosedPtr(idx));
            idx += 1;
        }
        code.extend([Op::PtrPopInto(1)]);

        code.extend(self.lambda.compile(&closed_env, compiler));
        code.extend([Op::PopInto(0)]);

        code
    }
}

#[derive(Debug)]
struct Halt(Box<dyn ValExpression>);

impl Halt {
    pub fn new(return_value: impl ValExpression + 'static) -> Self {
        Halt(Box::new(return_value))
    }
}

impl Serialize for Halt {
    fn serialize(&self) -> StrStruct {
        strx!(("halt", self.0))
    }
}

impl Compilable for Halt {
    fn compile(&self, env: &Env, compiler: &mut Compiler) -> Vec<Op<String>> {
        join!(self.0.compile(env, compiler), [Op::Halt])
    }
}

#[derive(Debug)]
pub struct CallStatic {
    function: String,
    var_args: Vec<Box<dyn ValExpression>>,
    ptr_args: Vec<Box<dyn PtrExpression>>,
}

impl CallStatic {
    pub fn new(
        f: impl ToString,
        var_args: Vec<Box<dyn ValExpression>>,
        ptr_args: Vec<Box<dyn PtrExpression>>,
    ) -> Self {
        CallStatic {
            function: f.to_string(),
            var_args,
            ptr_args,
        }
    }
}

impl Serialize for CallStatic {
    fn serialize(&self) -> StrStruct {
        strx!(("call-static", self.function, self.var_args, self.ptr_args))
    }
}

impl Compilable for CallStatic {
    fn compile(&self, env: &Env, compiler: &mut Compiler) -> Vec<Op<String>> {
        if env.lookup(&self.function) != Some(&Binding::Static) {
            println!("{env:?}");
            panic!("{} is not a static function", self.function)
        }
        let mut code = compiler.compile_args(&self.var_args, &self.ptr_args, env);
        code.extend([Op::Goto(self.function.clone())]);
        code
    }
}

#[derive(Debug)]
struct CallDynamic {
    function: Box<dyn ValExpression>,
    var_args: Vec<Box<dyn ValExpression>>,
    ptr_args: Vec<Box<dyn PtrExpression>>,
}

impl CallDynamic {
    pub fn new(
        f: impl ValExpression + 'static,
        var_args: Vec<Box<dyn ValExpression>>,
        ptr_args: Vec<Box<dyn PtrExpression>>,
    ) -> Self {
        CallDynamic {
            function: Box::new(f),
            var_args,
            ptr_args,
        }
    }
}

impl Serialize for CallDynamic {
    fn serialize(&self) -> StrStruct {
        strx!(("call-dynamic", self.function, self.var_args, self.ptr_args))
    }
}

impl Compilable for CallDynamic {
    fn compile(&self, env: &Env, compiler: &mut Compiler) -> Vec<Op<String>> {
        let mut code = compiler.compile_args(&self.var_args, &self.ptr_args, env);
        code.extend(self.function.compile(env, compiler));
        code.extend([Op::Jump]);
        code
    }
}

#[derive(Debug)]
struct CallClosure {
    closure: Box<dyn PtrExpression>,
    var_args: Vec<Box<dyn ValExpression>>,
    ptr_args: Vec<Box<dyn PtrExpression>>,
}

impl CallClosure {
    pub fn new(
        f: impl PtrExpression + 'static,
        var_args: Vec<Box<dyn ValExpression>>,
        ptr_args: Vec<Box<dyn PtrExpression>>,
    ) -> Self {
        CallClosure {
            closure: Box::new(f),
            var_args,
            ptr_args,
        }
    }
}

impl Serialize for CallClosure {
    fn serialize(&self) -> StrStruct {
        strx!(("call-closure", self.closure, self.var_args, self.ptr_args))
    }
}

impl Compilable for CallClosure {
    fn compile(&self, env: &Env, compiler: &mut Compiler) -> Vec<Op<String>> {
        let mut code = compiler.compile_args(&self.var_args, &self.ptr_args, env);
        code.extend(self.closure.compile(env, compiler));
        code.extend(compiler.gen_destructure(1, 1));
        code.extend([Op::SetClosure, Op::Jump]);
        code
    }
}

macro_rules! define_if {
    ($tname:ident, $t:path, $ser:expr, tail=false) => {
        define_if!(@struct $tname, $t, $ser);
        define_if!(@compile $tname, $t, tail=false);
    };
    ($tname:ident, $t:path, $ser:expr, tail=true) => {
        define_if!(@struct $tname, $t, $ser);
        define_if!(@compile $tname, $t, tail=true);
    };

    (@struct $tname:ident, $t:path, $ser:expr) => {
        #[derive(Debug)]
        struct $tname {
            condition: Box<dyn ValExpression>,
            consequence: Box<dyn $t>,
            alternative: Box<dyn $t>,
        }

        impl $tname {
            fn new(
                condition: impl ValExpression + 'static,
                consequence: impl $t + 'static,
                alternative: impl $t + 'static,
            ) -> Self {
                $tname {
                    condition: Box::new(condition),
                    consequence: Box::new(consequence),
                    alternative: Box::new(alternative),
                }
            }
        }

        impl Serialize for $tname {
            fn serialize(&self) -> StrStruct {
                strx!(($ser, self.condition, self.consequence, self.alternative))
            }
        }
    };

    (@compile $tname:ident, $t:path, tail=false) => {
        impl Compilable for $tname {
            fn compile(&self, env: &Env, compiler: &mut Compiler) -> Vec<Op<String>> {
                let else_label = compiler.unique_label("elif");
                let end_label = compiler.unique_label("endif");
                let cond = self.condition.compile(env, compiler);
                let cons = self.consequence.compile(env, compiler);
                let alt = self.alternative.compile(env, compiler);
                join!(
                    cond,
                    [Op::GoIfZero(else_label.clone())],
                    cons,
                    [Op::Goto(end_label.clone()), Op::Label(else_label)],
                    alt,
                    [Op::Label(end_label)]
                )
            }
        }
    };

    (@compile $tname:ident, $t:path, tail=true) => {
        impl Compilable for $tname {
            fn compile(&self, env: &Env, compiler: &mut Compiler) -> Vec<Op<String>> {
                let else_label = compiler.unique_label("elif");
                let cond = self.condition.compile(env, compiler);
                let cons = self.consequence.compile(env, compiler);
                let alt = self.alternative.compile(env, compiler);
                join!(
                    cond,
                    [Op::GoIfZero(else_label.clone())],
                    cons,
                    [Op::Label(else_label)],
                    alt
                )
            }
        }
    }
}

define_if!(ValIf, ValExpression, "val-if", tail = false);
define_if!(PtrIf, PtrExpression, "ptr-if", tail = false);
define_if!(TailIf, TailStatement, "tail-if", tail = true);

pub struct Compiler {
    unique_counter: u64,
}

impl Compiler {
    pub fn new() -> Self {
        Compiler { unique_counter: 0 }
    }

    fn compile(&mut self, item: impl Compilable, env: &Env) -> Vec<Op<String>> {
        item.compile(env, self)
    }

    fn unique_label(&mut self, s: &str) -> String {
        let n = self.unique_counter;
        self.unique_counter += 1;
        format!("{s}-{n}")
    }

    fn compile_valref(&self, name: &str, env: &Env) -> Vec<Op<String>> {
        match env.lookup(name) {
            None => panic!("unbound identifier {name}"),
            Some(Binding::Static) => vec![Op::push_addr(name)],
            Some(Binding::LocalVal(idx)) => vec![Op::PushLocal(*idx)],
            Some(Binding::ClosedVal(idx)) => vec![Op::PushClosed(*idx)],
            Some(_) => panic!("{} is not a value variable", name),
        }
    }

    fn compile_ptrref(&self, name: &str, env: &Env) -> Vec<Op<String>> {
        match env.lookup(name) {
            None => panic!("unbound identifier {name}"),
            Some(Binding::LocalPtr(idx)) => vec![Op::PtrPushLocal(*idx)],
            Some(Binding::ClosedPtr(idx)) => vec![Op::PtrPushClosed(*idx)],
            Some(_) => panic!("{} is not a pointer variable", name),
        }
    }

    fn compile_args(
        &mut self,
        val_args: &[Box<dyn ValExpression>],
        ptr_args: &[Box<dyn PtrExpression>],
        env: &Env,
    ) -> Vec<Op<String>> {
        let mut code = vec![];
        for va in val_args {
            code.extend(va.compile(env, self));
        }
        for pa in ptr_args {
            code.extend(pa.compile(env, self));
        }
        code
    }

    fn compile_function(
        &mut self,
        val_params: &[String],
        ptr_params: &[String],
        body: &dyn TailStatement,
        mut local_env: Env,
    ) -> Vec<Op<String>> {
        let mut code = vec![];

        let mut idx = (val_params.len() + ptr_params.len()) as Int;
        if idx > 0 {
            code.push(Op::Alloc(RecordSignature::new(
                val_params.len() as Half,
                ptr_params.len() as Half,
            )));
            code.push(Op::SetLocals);
        }

        for pa in ptr_params.iter().rev() {
            idx -= 1;
            local_env = local_env.assoc(pa, Binding::LocalPtr(idx));
            code.extend([Op::PtrPopLocal(idx)]);
        }

        for pa in val_params.iter().rev() {
            idx -= 1;
            local_env = local_env.assoc(pa, Binding::LocalVal(idx));
            code.extend([Op::PopLocal(idx)]);
        }

        join!(code, body.compile(&local_env, self))
    }

    /// generate code to effectively pop the top record from the ptr_stack, push its value fields on
    /// val_stack and its pointer fields on ptr_stack.
    fn gen_destructure(&mut self, n_vals: Half, n_ptrs: Half) -> Vec<Op<String>> {
        let mut code = vec![];
        let mut idx = 0;
        for _ in 0..n_vals {
            code.push(Op::PushFrom(idx));
            idx += 1;
        }
        for _ in 0..n_vals {
            code.push(Op::PtrPushFrom(idx));
            idx += 1;
        }
        code.push(Op::PtrDrop(n_ptrs as Int));
        code
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Binding {
    Builtin(Int),
    Static,
    LocalVal(Int),
    LocalPtr(Int),
    ClosedVal(Int),
    ClosedPtr(Int),
}

impl Binding {
    fn is_value(&self) -> bool {
        match self {
            Binding::Builtin(_) | Binding::Static => false,
            Binding::LocalVal(_) | Binding::ClosedVal(_) => true,
            Binding::LocalPtr(_) | Binding::ClosedPtr(_) => false,
        }
    }

    fn is_pointer(&self) -> bool {
        match self {
            Binding::Builtin(_) | Binding::Static => false,
            Binding::LocalVal(_) | Binding::ClosedVal(_) => false,
            Binding::LocalPtr(_) | Binding::ClosedPtr(_) => true,
        }
    }
}

pub type Env = Environment<Binding>;

#[derive(Debug, Clone)]
pub enum Environment<T: Clone> {
    Empty,
    Entry(Rc<(String, T, Environment<T>)>),
}

impl<T: Clone> Environment<T> {
    pub fn assoc(&self, name: impl ToString, thing: T) -> Self {
        Environment::Entry(Rc::new((name.to_string(), thing, self.clone())))
    }

    pub fn lookup(&self, name: &str) -> Option<&T> {
        match self {
            Environment::Empty => None,
            Environment::Entry(e) => {
                if e.0 == name {
                    Some(&e.1)
                } else {
                    e.2.lookup(name)
                }
            }
        }
    }
}

impl Env {
    fn without_locals(&self) -> Self {
        match self {
            Environment::Empty => Environment::Empty,
            Environment::Entry(entry) => match &**entry {
                (_, Binding::LocalPtr(_) | Binding::LocalVal(_), next) => next.without_locals(),
                (name, binding, next) => next.without_locals().assoc(name, *binding),
            },
        }
    }
}

const BUILTIN_LT: Int = 0;
const BUILTIN_ADD: Int = 1;
const BUILTIN_SUB: Int = 2;
const BUILTIN_MUL: Int = 3;

fn register_builtins<AC: Allocator, GC: GarbageCollector>(vm: &mut Vm<AC, GC>) {
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

fn builtin_env() -> Env {
    let mut env = Env::Empty;
    env = env.assoc("<", Binding::Builtin(BUILTIN_LT));
    env = env.assoc("+", Binding::Builtin(BUILTIN_ADD));
    env = env.assoc("-", Binding::Builtin(BUILTIN_SUB));
    env = env.assoc("*", Binding::Builtin(BUILTIN_MUL));
    env
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vm::{transform_labels, RecordSignature};
    use std::collections::HashMap;

    #[test]
    fn compile_int_constant() {
        assert_eq!(
            Compiler::new().compile(vm_ast! {(const 42)}, &Env::Empty),
            vec![Op::Const(42)]
        );
    }

    #[test]
    fn compile_int_conditional() {
        let code = Compiler::new().compile(
            vm_ast! {(val-if (const 0) (const 1) (const 2))},
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
    fn compile_ptr_conditional() {
        let code = Compiler::new().compile(
            vm_ast! {(ptr-if (const 1) (ptr-null) (ptr-null))},
            &Env::Empty,
        );
        match code.as_slice() {
            [Op::Const(1), Op::GoIfZero(else_target), Op::Const(0), Op::ValToPtr, Op::Goto(end_target), Op::Label(else_label), Op::Const(0), Op::ValToPtr, Op::Label(end_label)]
                if else_target == else_label
                    && end_target == end_label
                    && else_label != end_label => {}
            _ => panic!("{code:?}"),
        }
    }

    #[test]
    fn compile_tail_conditional() {
        let code = Compiler::new().compile(
            vm_ast! {(tail-if (const 1) (halt! (const 2)) (halt! (const 3)))},
            &Env::Empty,
        );
        match code.as_slice() {
            [Op::Const(1), Op::GoIfZero(else_target), Op::Const(2), Op::Halt, Op::Label(else_label), Op::Const(3), Op::Halt]
                if else_target == else_label => {}
            _ => panic!("{code:?}"),
        }
    }

    #[test]
    fn compile_int_reference() {
        let env = Env::Empty.assoc("foo", Binding::LocalVal(7));
        assert_eq!(
            Compiler::new().compile(vm_ast! {(val-ref foo)}, &env),
            vec![Op::PushLocal(7)]
        );
    }

    #[test]
    fn compile_ptr_reference() {
        let env = Env::Empty.assoc("foo", Binding::LocalPtr(5));
        assert_eq!(
            Compiler::new().compile(vm_ast! {(ptr-ref foo)}, &env),
            vec![Op::PtrPushLocal(5)]
        );
    }

    #[test]
    #[should_panic]
    fn compile_wrong_reference1() {
        let env = Env::Empty
            .assoc("foo", Binding::LocalVal(0))
            .assoc("bar", Binding::LocalPtr(1));
        Compiler::new().compile(vm_ast! {(ptr-ref foo)}, &env);
    }

    #[test]
    #[should_panic]
    fn compile_wrong_reference2() {
        let env = Env::Empty
            .assoc("foo", Binding::LocalVal(0))
            .assoc("bar", Binding::LocalPtr(1));
        Compiler::new().compile(vm_ast! {(val-ref bar)}, &env);
    }

    #[test]
    fn compile_builtin_val_op() {
        let env = builtin_env();
        assert_eq!(
            Compiler::new().compile(vm_ast! {(val-op + ((const 1) (const 2)) ())}, &env),
            vec![Op::Const(1), Op::Const(2), Op::CallBuiltin(BUILTIN_ADD)]
        );
    }

    #[test]
    fn compile_null_ptr() {
        assert_eq!(
            Compiler::new().compile(vm_ast! {(ptr-null)}, &Env::Empty),
            vec![Op::Const(0), Op::ValToPtr]
        );
    }

    #[test]
    fn compile_record_initialization() {
        assert_eq!(
            Compiler::new().compile(
                vm_ast! {(record ((const 1) (const 2)) ((ptr-null)))},
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
        let code =
            Compiler::new().compile(vm_ast! {(lambda () () (halt! (const 42)))}, &Env::Empty);

        match &code[..] {
            [Op::Goto(goto_end), Op::Label(label_def), Op::Const(42), Op::Halt, Op::Label(label_end), Op::PushAddr(get_def)]
                if goto_end == label_end && get_def == label_def => {} // Ok
            _ => panic!("{:?}", code),
        }
    }

    #[test]
    fn compile_nary_lambda() {
        let code = Compiler::new().compile(
            vm_ast! {(lambda (a b) (x y) (halt! (const 42)))},
            &Env::Empty,
        );

        match &code[..] {
            [Op::Goto(goto_end), Op::Label(label_def), Op::Alloc(rs), Op::SetLocals, Op::PtrPopLocal(3), Op::PtrPopLocal(2), Op::PopLocal(1), Op::PopLocal(0), Op::Const(42), Op::Halt, Op::Label(label_end), Op::PushAddr(get_def)]
                if goto_end == label_end
                    && get_def == label_def
                    && rs.n_primitive() == 2
                    && rs.n_pointer() == 2 => {} // Ok
            _ => panic!("{:?}", code),
        }
    }

    #[test]
    fn compile_static_call() {
        assert_eq!(
            Compiler::new().compile(
                vm_ast! {(call-static foo ((const 1) (const 2)) ((ptr-null)))},
                &Env::Empty.assoc("foo", Binding::Static)
            ),
            vec![
                Op::Const(1),
                Op::Const(2),
                Op::Const(0),
                Op::ValToPtr,
                Op::goto("foo"),
            ]
        );
    }

    #[test]
    fn compile_dynamic_call() {
        // Calling a number is invalid, but it compiles
        assert_eq!(
            Compiler::new().compile(
                vm_ast! {(call-dynamic (const 0) ((const 1) (const 2)) ((ptr-null)))},
                &Env::Empty
            ),
            vec![
                Op::Const(1),
                Op::Const(2),
                Op::Const(0),
                Op::ValToPtr,
                Op::Const(0),
                Op::Jump,
            ]
        );
    }

    #[test]
    fn compile_closure_call() {
        // Calling a Null pointer is invalid, but it compiles
        assert_eq!(
            Compiler::new().compile(
                vm_ast! {(call-closure (ptr-null) ((const 1) (const 2)) ((ptr-null)))},
                &Env::Empty
            ),
            vec![
                // args
                Op::Const(1),
                Op::Const(2),
                Op::Const(0),
                Op::ValToPtr,
                // fake ptr to a closure (record with two fields)
                Op::Const(0),
                Op::ValToPtr,
                // prepare closure
                Op::PushFrom(0),    // callable is in the first field
                Op::PtrPushFrom(1), // free-var record is in the second field
                Op::PtrDrop(1),     // get rid of of the closure record
                Op::SetClosure,
                Op::Jump,
            ]
        );
    }

    #[test]
    #[should_panic]
    fn lambda_cant_access_outer_vars() {
        let code = Compiler::new().compile(
            vm_ast! {
                (lambda (a) ()
                    (halt!
                        (lambda () ()
                            (halt! (val-ref a)))))
            },
            &Env::Empty,
        );
    }

    #[test]
    fn lambda_can_access_closed_vars() {
        let code = Compiler::new().compile(
            vm_ast! {(closure (p a) (lambda () () (halt! (val-ref a))))},
            &Env::Empty
                .assoc("a", Binding::LocalVal(7))
                .assoc("p", Binding::LocalPtr(11)),
        );

        for op in &code {
            println!("{op:?}");
        }

        assert_eq!(
            code,
            [
                Op::Alloc(RecordSignature::new(1, 1)), // closure record
                Op::Alloc(RecordSignature::new(1, 1)), // env record
                Op::PushLocal(7),
                Op::PopInto(0),
                Op::PtrPushLocal(11),
                Op::PtrPopInto(1),
                Op::PtrPopInto(1), // put env into closure
                Op::goto("end-lambda-1"),
                Op::label("lambda-0"),
                Op::PushClosed(0),
                Op::Halt,
                Op::label("end-lambda-1"),
                Op::push_addr("lambda-0"),
                Op::PopInto(0), // store function pointer in closure
            ]
        );
    }

    #[test]
    fn compile_nullary_funcdef() {
        let code = Compiler::new().compile(
            vm_ast! {(define (foo () ()) (halt! (const 42)))},
            &Env::Empty,
        );

        assert_eq!(
            code,
            [Op::Label("foo".to_string()), Op::Const(42), Op::Halt]
        );
    }

    #[test]
    fn compile_empty_program() {
        let code = Compiler::new().compile(vm_ast! {(program)}, &Env::Empty);

        assert_eq!(code, [Op::goto("main")]);
    }

    #[test]
    fn compile_simplest_program() {
        let code = Compiler::new().compile(
            vm_ast! {
                (program
                    (define (main () ()) (halt! (const 0))))
            },
            &Env::Empty,
        );

        assert_eq!(
            code,
            [Op::goto("main"), Op::label("main"), Op::Const(0), Op::Halt]
        );
    }

    #[test]
    fn compile_program() {
        let code = Compiler::new().compile(
            vm_ast! {
                (program
                    (define (main () ()) (call-static foo () ()))
                    (define (foo () ()) (halt! (const 0)))
                )
            },
            &Env::Empty,
        );

        assert_eq!(
            code,
            [
                Op::goto("main"),
                Op::label("main"),
                Op::goto("foo"),
                Op::label("foo"),
                Op::Const(0),
                Op::Halt
            ]
        );
    }

    #[test]
    fn factorial() {
        let code = Compiler::new().compile(
            vm_ast! {
                (program
                    (define (stop (r) ())
                        (halt! (val-ref r)))
                    (define (main () ())
                        (call-static fact
                            ((const 5))
                            ((closure ()
                                (lambda (r) ()
                                    (call-static stop ((val-ref r)) ()))))))
                    (define (fact (n) (k))
                        (tail-if (val-op < ((val-ref n) (const 1)) ())
                            (call-closure (ptr-ref k) ((const 1)) ())
                            (call-static fact
                                ((val-op - ((val-ref n) (const 1)) ()))
                                ((closure (n k)
                                    (lambda (m) ()
                                        (call-closure (ptr-ref k) ((val-op * ((val-ref n) (val-ref m)) ())) ())))))
                        ))

                )
            },
            &builtin_env(),
        );

        let code: Vec<_> = transform_labels(&code).collect();

        let mut vm = Vm::default();
        register_builtins(&mut vm);
        let res = vm.run(&code);
        assert_eq!(res, 1 * 2 * 3 * 4 * 5);
    }

    #[test]
    fn fibonacci() {
        let code = Compiler::new().compile(
            vm_ast! {
                (program
                    (define (stop (r) ())
                        (halt! (val-ref r)))
                    (define (main () ())
                        (call-static fib
                            ((const 5))
                            ((closure ()
                                (lambda (r) ()
                                    (call-static stop ((val-ref r)) ()))))))
                    (define (fib (n) (k))
                        (tail-if (val-op < ((val-ref n) (const 2)) ())
                            (call-closure (ptr-ref k) ((const 1)) ())
                            (call-static fib
                                ((val-op - ((val-ref n) (const 1)) ()))
                                ((closure (n k)
                                    (lambda (f1) ()
                                        (call-static fib
                                            ((val-op - ((val-ref n) (const 2)) ()))
                                            ((closure (f1 n k)
                                                (lambda (f2) ()
                                                    (call-closure (ptr-ref k) ((val-op + ((val-ref f1) (val-ref f2)) ())) ())))))))))))

                )
            },
            &builtin_env(),
        );

        let code: Vec<_> = transform_labels(&code).collect();

        let mut vm = Vm::default();
        register_builtins(&mut vm);
        let res = vm.run(&code);
        assert_eq!(res, 8); // 1 1 2 3 5 8 13 21
    }
}
