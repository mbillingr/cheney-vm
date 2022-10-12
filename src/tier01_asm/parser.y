%start Prog
%%
Prog -> Result<Vec<Op<&'input str>>, Box<dyn Error>>:
      Opcode { Ok(vec![$1?]) }
    | Prog Opcode
      {
        let mut $1 = $1?;
        $1.push($2?);
        Ok($1)
      }
    ;

Opcode -> Result<Op<&'input str>, Box<dyn Error>>:
      'COMMENT' { Ok(Op::Comment($lexer.span_str($1?.span())[2..].trim())) }
    | 'IDENT' ':' { Ok(Op::Label($lexer.span_str($1?.span()))) }
    | 'LPAREN' 'IDENT' 'RPAREN'
      {
        match $lexer.span_str($2?.span()) {
            "halt" => Ok(Op::Halt),
            ident => Err(format!("unknown opcode: {}", ident).into())
        }
      }
    ;
%%

use std::error::Error;
use crate::vm::Op;
