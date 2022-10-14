%start Prog
%%
Prog -> Result<t2::Program, Box<dyn Error>>:
      TopDef { Ok(t2::Program(vec![$1?])) }
    | Prog TopDef
      {
        let mut $1 = $1?;
        $1.0.push($2?);
        Ok($1)
      }
    ;

TopDef -> Result<t2::Definition, Box<dyn Error>>:
    'LPAREN' 'DEFVAL' 'LPAREN' Ident IdentList IdentList 'RPAREN' ValExpr 'RPAREN'
    {
        Ok(t2::Definition::ValFunc($4?, $5?, $6?, $8?))
    }
    ;

ValExpr -> Result<t2::ValExpr, Box<dyn Error>>:
    Number { Ok(t2::ValExpr::Const($1?)) }
    ;

IdentList -> Result<Vec<Str>, Box<dyn Error>>:
      'LPAREN' 'RPAREN' { Ok(vec![]) }
    | 'LPAREN' Idents 'RPAREN' { Ok($2?) }
    ;

Idents -> Result<Vec<Str>, Box<dyn Error>>:
      Ident { Ok(vec![$1?]) }
    | Idents Ident { flatten($1, $2) }
    ;

Ident -> Result<Str, Box<dyn Error>>:
    'IDENT' { Ok(Str::interned($lexer.span_str($1?.span()))) }
    ;

Number -> Result<Int, Box<dyn Error>>:
    'NUMBER' { parse_int($lexer.span_str($1?.span())) }
    ;
%%

use std::error::Error;
use crate::str::Str;
use crate::tier02_vmlang as t2;
use crate::vm::Int;

fn flatten<T>(lhs: Result<Vec<T>, Box<dyn Error>>, rhs: Result<T, Box<dyn Error>>) -> Result<Vec<T>, Box<dyn Error>>
{
    let mut flt = lhs?;
    flt.push(rhs?);
    Ok(flt)
}

fn parse_int(s: &str) -> Result<Int, Box<dyn Error>> {
    match s.parse::<u64>() {
        Ok(val) => Ok(val),
        Err(_) => {
            Err(format!("{} cannot be represented as a u64", s).into())
        }
    }
}
