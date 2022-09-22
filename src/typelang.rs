use crate::vm::{Int, Op, RecordSignature, R};

pub enum Type {
    Integer,
    Record(String),
}

trait AstNode {
    fn compile(&self) -> Vec<Op<String>>;
}

trait Expression: AstNode {}
trait ToplevelDefinition: AstNode {}

struct Program {
    defs: Vec<Box<dyn ToplevelDefinition>>,
}

struct FunctionDefinition {
    name: String,
    body: Box<dyn Expression>,
}

struct RecordDefinition {
    name: String,
    fields: Vec<(String, Type)>,
}

struct Constant(Int);

struct FunCall {
    name: String,
}

impl AstNode for Program {
    fn compile(&self) -> Vec<Op<String>> {
        let mut code = vec![Op::goto("main")];

        for def in &self.defs {
            code.extend(def.compile());
        }

        code
    }
}

impl ToplevelDefinition for FunctionDefinition {}

impl AstNode for FunctionDefinition {
    fn compile(&self) -> Vec<Op<String>> {
        let mut code = vec![Op::label(self.name.clone())];
        code.extend(self.body.compile());
        code.push(Op::Halt);
        code
    }
}

impl ToplevelDefinition for RecordDefinition {}

impl AstNode for RecordDefinition {
    fn compile(&self) -> Vec<Op<String>> {
        let mut n_primitive = 0;
        let mut n_pointer = 0;

        for (_, t) in &self.fields {
            match t {
                Type::Integer => n_primitive += 1,
                Type::Record(_) => n_pointer += 1,
            }
        }

        let rs = RecordSignature::new(n_primitive, n_pointer);

        let mut code = vec![Op::label(self.name.clone())];
        code.push(Op::Alloc(rs));
        code.push(Op::Copy(R::Ptr, R::Val));
        code.push(Op::Halt);
        code
    }
}

impl Expression for Constant {}

impl AstNode for Constant {
    fn compile(&self) -> Vec<Op<String>> {
        vec![Op::Const(self.0)]
    }
}

impl Expression for FunCall {}

impl AstNode for FunCall {
    fn compile(&self) -> Vec<Op<String>> {
        vec![Op::Goto(self.name.clone())]
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vm::{transform_labels, Vm};

    #[test]
    fn run_program() {
        let program = Program {
            defs: vec![
                Box::new(RecordDefinition {
                    name: "Data".to_string(),
                    fields: vec![("x".to_string(), Type::Integer)],
                }),
                Box::new(FunctionDefinition {
                    name: "foo".to_string(),
                    body: Box::new(Constant(42)),
                }),
                Box::new(FunctionDefinition {
                    name: "main".to_string(),
                    body: Box::new(FunCall {
                        name: "Data".to_string(),
                    }),
                }),
            ],
        };

        let code = program.compile();
        for op in &code {
            println!("{:?}", op);
        }

        let code = transform_labels(&code).collect::<Vec<_>>();

        let mut vm = Vm::default();
        let res = vm.run(&code);
        panic!("{res}");
    }
}
