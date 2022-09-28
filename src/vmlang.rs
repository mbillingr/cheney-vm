//! super-simple functional semantics on top of the VM

use crate::vm::{Half, Int, Op, RecordSignature};
use std::fmt::Debug;
use std::rc::Rc;

macro_rules! mark {
    ($trait:path: $($t:ty),*) => {
        $(
            impl $trait for $t {}
        )*
    }
}

macro_rules! boxvec {
    ($($x:expr),*) => {
        vec![$(Box::new($x)),*]
    }
}

macro_rules! join {
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

trait Ast: Debug + Compilable {}
trait ValExpression: Ast {}
trait PtrExpression: Ast {}
trait TailStatement_: Ast {}

trait Compilable {
    fn compile(&self, env: &Env, compiler: &mut Compiler) -> Vec<Op<String>>;
}

mark!(
    Ast: Const,
    ValRef,
    Lambda,
    ValIf,
    PtrNull,
    PtrRef,
    Record,
    Halt,
    CallStatic,
    CallDynamic,
    CallClosure
);
mark!(ValExpression: Const, ValRef, Lambda, ValIf);
mark!(PtrExpression: PtrNull, PtrRef, Record);
mark!(TailStatement_: Halt, CallStatic, CallDynamic, CallClosure);

#[derive(Debug)]
struct Const(Int);

impl Compilable for Const {
    fn compile(&self, _env: &Env, _compiler: &mut Compiler) -> Vec<Op<String>> {
        vec![Op::Const(self.0)]
    }
}

#[derive(Debug)]
struct ValRef(String);

impl ValRef {
    pub fn new(identifier: impl ToString) -> Self {
        ValRef(identifier.to_string())
    }
}

impl Compilable for ValRef {
    fn compile(&self, env: &Env, _compiler: &mut Compiler) -> Vec<Op<String>> {
        match env.lookup(&self.0) {
            None => panic!("unbound identifier {}", self.0),
            Some(Binding::LocalVal(idx)) => vec![Op::PushLocal(*idx)],
            Some(_) => panic!("{} is not a value variable", self.0),
        }
    }
}

#[derive(Debug)]
struct Lambda {
    val_params: Vec<String>,
    ptr_params: Vec<String>,
    body: Box<dyn TailStatement_>,
}

impl Lambda {
    pub fn new(
        val_params: Vec<String>,
        ptr_params: Vec<String>,
        body: impl TailStatement_ + 'static,
    ) -> Self {
        Lambda {
            val_params,
            ptr_params,
            body: Box::new(body),
        }
    }
}

impl Compilable for Lambda {
    fn compile(&self, env: &Env, compiler: &mut Compiler) -> Vec<Op<String>> {
        let lam_label = compiler.unique_label("lambda");
        let end_label = compiler.unique_label("end-lambda");
        let mut code = vec![Op::Goto(end_label.clone()), Op::Label(lam_label.clone())];

        let mut local_env = env.clone();
        let mut idx = (self.val_params.len() + self.ptr_params.len()) as Int;

        if idx > 0 {
            code.push(Op::Alloc(RecordSignature::new(
                self.val_params.len() as Half,
                self.ptr_params.len() as Half,
            )));
        }

        for pa in self.ptr_params.iter().rev() {
            idx -= 1;
            local_env = local_env.assoc(pa, Binding::LocalPtr(idx));
            code.extend([Op::PtrPeek(1), Op::PtrPopInto(idx)]);
        }

        for pa in self.val_params.iter().rev() {
            idx -= 1;
            local_env = local_env.assoc(pa, Binding::LocalVal(idx));
            code.extend([Op::PopInto(idx)]);
        }

        join!(
            code,
            self.body.compile(&local_env, compiler),
            [
                Op::Label(end_label.clone()),
                Op::PushAddr(lam_label.clone())
            ]
        )
    }
}

#[derive(Debug)]
struct PtrNull;

impl Compilable for PtrNull {
    fn compile(&self, _env: &Env, _compiler: &mut Compiler) -> Vec<Op<String>> {
        vec![Op::Const(0), Op::ValToPtr]
    }
}

#[derive(Debug)]
struct PtrRef(String);

impl PtrRef {
    pub fn new(identifier: impl ToString) -> Self {
        PtrRef(identifier.to_string())
    }
}

impl Compilable for PtrRef {
    fn compile(&self, env: &Env, _compiler: &mut Compiler) -> Vec<Op<String>> {
        match env.lookup(&self.0) {
            None => panic!("unbound identifier {}", self.0),
            Some(Binding::LocalPtr(idx)) => vec![Op::PtrPushLocal(*idx)],
            Some(_) => panic!("{} is not a pointer variable", self.0),
        }
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
struct Halt(Box<dyn ValExpression>);

impl Halt {
    pub fn new(return_value: impl ValExpression + 'static) -> Self {
        Halt(Box::new(return_value))
    }
}

impl Compilable for Halt {
    fn compile(&self, env: &Env, compiler: &mut Compiler) -> Vec<Op<String>> {
        join!(self.0.compile(env, compiler), [Op::Halt])
    }
}

#[derive(Debug)]
struct CallStatic {
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

impl Compilable for CallStatic {
    fn compile(&self, env: &Env, compiler: &mut Compiler) -> Vec<Op<String>> {
        let mut code = compiler.compile_args(&self.var_args, &self.ptr_args, env);
        code.extend([Op::PushAddr(self.function.clone()), Op::Jump]);
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

impl Compilable for CallClosure {
    fn compile(&self, env: &Env, compiler: &mut Compiler) -> Vec<Op<String>> {
        let mut code = compiler.compile_args(&self.var_args, &self.ptr_args, env);
        code.extend(self.closure.compile(env, compiler));
        code.extend(compiler.gen_destructure(1, 1));
        code.extend([Op::Jump]);
        code
    }
}

macro_rules! define_if {
    ($tname:ident, $t:path, tail=false) => {
        define_if!(@struct $tname, $t);
        define_if!(@compile $tname, $t, tail=false);
    };
    ($tname:ident, $t:path, tail=true) => {
        define_if!(@struct $tname, $t);
        define_if!(@compile $tname, $t, tail=true);
    };

    (@struct $tname:ident, $t:path) => {
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

define_if!(ValIf, ValExpression, tail = false);
define_if!(PtrIf, PtrExpression, tail = false);
define_if!(TailIf, TailStatement_, tail = true);

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

    fn unique_label(&mut self, s: &str) -> String {
        let n = self.unique_counter;
        self.unique_counter += 1;
        format!("{s}-{n}")
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

#[derive(Debug)]
enum Binding {
    LocalVal(Int),
    LocalPtr(Int),
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
    use std::collections::HashMap;

    #[test]
    fn compile_int_constant() {
        assert_eq!(
            Compiler::new().compile(Const(42), &Env::Empty),
            vec![Op::Const(42)]
        );
    }

    #[test]
    fn compile_int_conditional() {
        let code = Compiler::new().compile(ValIf::new(Const(0), Const(1), Const(2)), &Env::Empty);
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
        let code = Compiler::new().compile(PtrIf::new(Const(1), PtrNull, PtrNull), &Env::Empty);
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
            TailIf::new(Const(1), Halt::new(Const(2)), Halt::new(Const(3))),
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
            Compiler::new().compile(ValRef("foo".to_string()), &env),
            vec![Op::PushLocal(7)]
        );
    }

    #[test]
    fn compile_ptr_reference() {
        let env = Env::Empty.assoc("foo", Binding::LocalPtr(5));
        assert_eq!(
            Compiler::new().compile(PtrRef::new("foo"), &env),
            vec![Op::PtrPushLocal(5)]
        );
    }

    #[test]
    #[should_panic]
    fn compile_wrong_reference1() {
        let env = Env::Empty
            .assoc("foo", Binding::LocalVal(0))
            .assoc("bar", Binding::LocalPtr(1));
        Compiler::new().compile(PtrRef::new("foo"), &env);
    }

    #[test]
    #[should_panic]
    fn compile_wrong_reference2() {
        let env = Env::Empty
            .assoc("foo", Binding::LocalVal(0))
            .assoc("bar", Binding::LocalPtr(1));
        Compiler::new().compile(ValRef("bar".to_string()), &env);
    }

    #[test]
    fn compile_null_ptr() {
        assert_eq!(
            Compiler::new().compile(PtrNull, &Env::Empty),
            vec![Op::Const(0), Op::ValToPtr]
        );
    }

    #[test]
    fn compile_record_initialization() {
        assert_eq!(
            Compiler::new().compile(
                Record::new(boxvec![Const(1), Const(2)], boxvec![PtrNull],),
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
        let code = Compiler::new().compile(
            Lambda::new(vec![], vec![], Halt::new(Const(42))),
            &Env::Empty,
        );

        match &code[..] {
            [Op::Goto(goto_end), Op::Label(label_def), Op::Const(42), Op::Halt, Op::Label(label_end), Op::PushAddr(get_def)]
                if goto_end == label_end && get_def == label_def => {} // Ok
            _ => panic!("{:?}", code),
        }
    }

    #[test]
    fn compile_nary_lambda() {
        let code = Compiler::new().compile(
            Lambda::new(
                vec!["a".to_string(), "b".to_string()],
                vec!["x".to_string(), "y".to_string()],
                Halt::new(Const(42)),
            ),
            &Env::Empty,
        );

        match &code[..] {
            [Op::Goto(goto_end), Op::Label(label_def), Op::Alloc(rs), Op::PtrPeek(1), Op::PtrPopInto(3), Op::PtrPeek(1), Op::PtrPopInto(2), Op::PopInto(1), Op::PopInto(0), Op::Const(42), Op::Halt, Op::Label(label_end), Op::PushAddr(get_def)]
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
                CallStatic::new("foo", boxvec![Const(1), Const(2)], boxvec!(PtrNull)),
                &Env::Empty
            ),
            vec![
                Op::Const(1),
                Op::Const(2),
                Op::Const(0),
                Op::ValToPtr,
                Op::PushAddr("foo".to_string()),
                Op::Jump,
            ]
        );
    }

    #[test]
    fn compile_dynamic_call() {
        // Calling a number is invalid, but it compiles
        assert_eq!(
            Compiler::new().compile(
                CallDynamic::new(Const(0), boxvec![Const(1), Const(2)], boxvec!(PtrNull)),
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
                CallClosure::new(PtrNull, boxvec![Const(1), Const(2)], boxvec!(PtrNull)),
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
                Op::Jump,
            ]
        );
    }
}
