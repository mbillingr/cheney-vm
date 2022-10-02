use crate::vmlang;
use std::fmt::Debug;

trait RtAst: Debug + ToVmlang {}
trait RtTailExpression: RtAst + ToTailStatement {}

trait ToVmlang {
    fn to_vmlang(&self) -> Box<dyn vmlang::VmAst>;
}

trait ToTailStatement {
    fn to_tail_statement(&self) -> Box<dyn vmlang::TailStatement>;
}

// ValExpression: Const, ValRef, ValIf, Lambda, ValCallStatic, ValCallDynamic, ValCallClosure
// PtrExpression: PtrNull, PtrRef PtrIf, Closure, ValCallStatic, ValCallDynamic, ValCallClosure

#[derive(Debug)]
struct Program {
    defs: Vec<FuncDef>,
}

impl Program {
    pub fn new(defs: Vec<FuncDef>) -> Self {
        Program { defs }
    }
}

impl From<Program> for vmlang::Program {
    fn from(program: Program) -> Self {
        vmlang::Program::new(program.defs.into_iter().map(Into::into).collect())
    }
}

#[derive(Debug)]
struct FuncDef {
    name: String,
    val_params: Vec<String>,
    ptr_params: Vec<String>,
    body: Box<dyn RtTailExpression>,
}

impl From<FuncDef> for vmlang::FuncDef {
    fn from(program: FuncDef) -> Self {
        vmlang::FuncDef::new(
            program.name,
            program.val_params.clone(),
            program.ptr_params,
            program.body.to_tail_statement()
        )
    }
}
