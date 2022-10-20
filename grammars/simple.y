%start TopExpr
%%
TopExpr -> Result<(), ()>:
    Expr 'NEWLINE' { }
    ;

Expr -> Result<(), ()>:
    AtomicExprs { }
    ;

AtomicExpr -> Result<(), ()>:
      'NUMBER' { }
      'IDENT' { }
    | '(' Expr ')' { }
    ;

AtomicExprs -> Result<(), ()>:
      AtomicExpr {  }
    | AtomicExprs AtomicExpr { }
    ;

%%
