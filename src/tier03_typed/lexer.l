%%
:       "COLON"
\(      "LPAREN"
\)      "RPAREN"
->      "FTYPE"
=>      "CTYPE"
~>      "BTYPE"
=       "EQUALS"

\n      "NEWLINE"

Int     "INT_T"
Record  "RECORD_T"

cast    "CAST"
closure "CLOSURE"
if      "IF"
lambda  "LAMBDA"
record  "RECORD"
rec-ref "RECREF"
type    "TYPE"

[0-9]+  "NUMBER"
(\w|[-+<>!?])+  "IDENT"
[\t \n]+ ;