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
      Ident Idents 'EQUALS' Expr {
            let name = $4?;
            if name != $1? { Err(format!("Undeclared name: {name}"))? }

            let sig = &*$3?;
            let sig = t3::types::get_fnsignature(sig).ok_or_else(||"expected function type")?;

            Ok(t3::FunctionDefinition {
                name,
                params: $5?,
                body: $7?,
                param_types: sig.ptypes.clone(),
                return_type: sig.returns.clone(),
            })
        }
    | Ident 'COLON' Type
      Ident 'EQUALS' Expr {
            let name = $4?;
            if name != $1? { Err(format!("Undeclared name: {name}"))? }

            let sig = &*$3?;
            let sig = t3::types::get_fnsignature(sig).ok_or_else(||"expected function type")?;

            Ok(t3::FunctionDefinition {
                name,
                params: vec![],
                body: $6?,
                param_types: sig.ptypes.clone(),
                return_type: sig.returns.clone(),
            })
        }
    ;

Type -> Result<Rc<dyn t3::Type>, Box<dyn Error>>:
      NonFnType { $1  }
    | Types 'RARROW' Type { Ok(t3::types::Callable::new($1?, $3?)) }
    | 'RARROW' Type { Ok(t3::types::Callable::new(vec![], $2?)) }
    ;

Types -> Result<Vec<Rc<dyn t3::Type>>, Box<dyn Error>>:
      NonFnType { Ok(vec![$1?]) }
    | Types NonFnType { flatten($1, $2) }
    ;

NonFnType -> Result<Rc<dyn t3::Type>, Box<dyn Error>>:
      'INT' { Ok(t3::types::Value::new()) }
    | 'LPAREN' Type 'RPAREN' { $2 }
    ;

Idents -> Result<Vec<Str>, Box<dyn Error>>:
      Ident { Ok(vec![$1?]) }
    | Idents Ident { flatten($1, $2) }
    ;

TypedIdents -> Result<Vec<(Str, Rc<dyn t3::Type>)>, Box<dyn Error>>:
      Ident 'COLON' Type { Ok(vec![($1?, $3?)]) }
    | TypedIdents Ident 'COLON' Type { flatten($1, $2.and_then(|ident| Ok((ident, $4?)))) }
    ;

Ident -> Result<Str, Box<dyn Error>>:
    'IDENT' { Ok(Str::interned($lexer.span_str($1?.span()))) }
    ;

Expr -> Result<Rc<dyn t3::Expression>, Box<dyn Error>>:
      Number { Ok(Rc::new(t3::ExprEnum::Const($1?))) }
    | Ident  { Ok(Rc::new(t3::ExprEnum::Ref($1?))) }
    | Lambda { Ok(Rc::new($1?)) }
    | 'LPAREN' Exprs 'RPAREN'
        {   // function call
            let mut args = $2?;
            let fun = args.remove(0);
            Ok(Rc::new(t3::ExprEnum::Call(fun, args)))
        }
    ;

Exprs -> Result<Vec<Rc<dyn t3::Expression>>, Box<dyn Error>>:
      Expr { Ok(vec![$1?]) }
    | Exprs Expr { flatten($1, $2) }
    ;

Lambda -> Result<t3::ExprEnum, Box<dyn Error>>:
      'LPAREN' 'LAMBDA' 'EQUALS' Expr 'RPAREN'
        {   // 0-ary lambda special form
            Ok(t3::ExprEnum::Lambda(vec![], vec![], $4?))
        }
    | 'LPAREN' 'LAMBDA' TypedIdents 'EQUALS' Expr 'RPAREN'
        {   // n-ary lambda special form
            let (params, ptypes) = $3?.into_iter().unzip();
            Ok(t3::ExprEnum::Lambda(params, ptypes, $5?))
        }
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
