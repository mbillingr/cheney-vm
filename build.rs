use cfgrammar::yacc::YaccKind;
use lrlex::CTLexerBuilder;
use std::error::Error;

fn main() -> Result<(), Box<dyn Error>> {
    build_parser("tier01_asm/lexer.l", "tier01_asm/parser.y")?;
    build_parser("tier02_vmlang/lexer.l", "tier02_vmlang/parser.y")
}

fn build_parser(lexer_spec: &'static str, parser_spec: &'static str) -> Result<(), Box<dyn Error>> {
    CTLexerBuilder::new()
        .lrpar_config(move |ctp| {
            ctp.yacckind(YaccKind::Grmtools)
                .grammar_in_src_dir(parser_spec)
                .unwrap()
        })
        .lexer_in_src_dir(lexer_spec)?
        .build()?;
    Ok(())
}
