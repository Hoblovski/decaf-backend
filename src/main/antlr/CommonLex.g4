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

StrLit
    : '"' SChar* '"'
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

fragment
SChar
    : ~["\\\r\n]
    | EscapeSequence
    | '\\\n'   // Added line
    | '\\\r\n' // Added line
    ;

fragment
EscapeSequence
    : SimpleEscapeSequence
    ;

fragment
SimpleEscapeSequence
    : '\\' ['"?abfnrtv\\]
    ;

Whitespace
    : [ \t]+ -> skip
    ;

Newline
    : ( '\r' '\n'? | '\n') -> skip
    ;
