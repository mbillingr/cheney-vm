use lrlex::lrlex_mod;
use lrpar::lrpar_mod;

// Using `lrlex_mod!` brings the lexer for `calc.l` into scope. By default the
// module name will be `calc_l` (i.e. the file name, minus any extensions,
// with a suffix of `_l`).
lrlex_mod!("tier01_asm/lexer.l");
// Using `lrpar_mod!` brings the parser for `calc.y` into scope. By default the
// module name will be `calc_y` (i.e. the file name, minus any extensions,
// with a suffix of `_y`).
lrpar_mod!("tier01_asm/parser.y");

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vm::Op;

    #[test]
    fn parse() {
        let lexerdef = lexer_l::lexerdef();
        let lexer = lexerdef.lexer("foo: (halt)  // simple, right? \n (halt)");
        let (prog, errs) = parser_y::parse(&lexer);
        for e in errs {
            eprintln!("{}", e.pp(&lexer, &parser_y::token_epp));
        }
        assert_eq!(
            prog.unwrap().unwrap(),
            vec![
                Op::Label("foo"),
                Op::Halt,
                Op::Comment("simple, right?"),
                Op::Halt
            ]
        );
    }
}
