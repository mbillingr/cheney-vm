use crate::vm::{Int, Op};

trait AstNode {
    fn compile(&self) -> Vec<Op<String>>;
}

trait Expression: AstNode {}

struct Program {
    defs: Vec<ToplevelDefinition>,
}

struct ToplevelDefinition {
    name: String,
    body: Box<dyn Expression>,
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

impl AstNode for ToplevelDefinition {
    fn compile(&self) -> Vec<Op<String>> {
        let mut code = vec![Op::label(self.name.clone())];
        code.extend(self.body.compile());
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
                ToplevelDefinition {
                    name: "foo".to_string(),
                    body: Box::new(Constant(42)),
                },
                ToplevelDefinition {
                    name: "main".to_string(),
                    body: Box::new(FunCall {
                        name: "foo".to_string(),
                    }),
                },
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
