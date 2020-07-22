lexer grammar CommonLex;

Number
    : Digit+
    ;

Temp
    : '_T' Number
    ;

Ident
    : IdentLead WordChar*
    ;

fragment
IdentLead
    : [a-zA-Z_]
    ;

fragment
WordChar
    : [0-9a-zA-Z_]
    ;

fragment
Digit
    : [0-9]
    ;

Whitespace
    : [ \t]+ -> skip
    ;

Newline
    : ( '\r' '\n'? | '\n') -> skip
    ;

StrLit
    : '"' StrChar* '"'
    ;

fragment
StrChar
    :   ~["\\\r\n]
    |   EscapeChar
    ;

fragment
EscapeChar
    :   '\\' ['"?abfnrtv\\]
    ;
