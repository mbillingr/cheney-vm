%%
:       "COLON"
\(      "LPAREN"
\)      "RPAREN"
->      "FTYPE"
=>      "CTYPE"
~>      "BTYPE"
=       "EQUALS"

\n      "NEWLINE"

Int     "INT"

cast    "CAST"
closure "CLOSURE"
if      "IF"
lambda  "LAMBDA"

[0-9]+  "NUMBER"
(\w|[-+<>!?])+  "IDENT"
[\t \n]+ ;