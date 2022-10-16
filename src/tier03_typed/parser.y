%start Prog
%%
Prog -> Result<t3::Program, Box<dyn Error>>:
      TopDef { Ok(t3::Program(vec![$1?])) }
    | Prog TopDef
      {
        let mut $1 = $1?;
        $1.0.push($2?);
        Ok($1)
      }
    ;

TopDef -> Result<t3::FunctionDefinition, Box<dyn Error>>:
      Ident 'COLON' Type
      Ident Idents 'EQUALS' Expr { todo!("fndef with params") }
    | Ident 'COLON' Type
      Ident 'EQUALS' Expr
    {
        let sig = &*$3?;
        let sig = t3::types::get_fnsignature(sig).ok_or_else(||"expected function type")?;
        Ok(t3::FunctionDefinition {
            name: $1?,
            params: vec![],
            body: $6?,
            param_types: sig.ptypes.clone(),
            return_type: sig.returns.clone(),
        })
    }
    ;

Expr -> Result<Rc<dyn t3::Expression>, Box<dyn Error>>:
    Number { Ok(Rc::new(t3::ExprEnum::Const($1?))) }
    ;

Type -> Result<Rc<dyn t3::Type>, Box<dyn Error>>:
      NonFnType { $1  }
    | NonFnType 'RARROW' Type { todo!("n ary fn types") }
    | 'RARROW' Type { Ok(t3::types::Function::new(vec![], $2?)) }
    ;

NonFnType -> Result<Rc<dyn t3::Type>, Box<dyn Error>>:
      'INT' { Ok(t3::types::Value::new()) }
    | 'LPAREN' Type 'RPAREN' { $2 }
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
use std::rc::Rc;
use crate::str::Str;
use crate::tier03_typed as t3;
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
