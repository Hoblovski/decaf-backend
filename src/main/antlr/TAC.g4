grammar TAC;

import CommonLex;

prog
    : vtabDef* funcDef*
    ;

vtabDef
    : vtabLabel ':' parentLabel className=StrLit funcLabel*
    ;

funcDef
    : funcLabel ':' cmd+
    ;

cmd
    : Temp '=' Temp   # Assign
    | Temp '=' vtabLabel  # LoadVTbl
    | Temp '=' Number # LoadImm
    | Temp '=' StrLit # LoadStrLit
    | Temp '=' unaryOp Temp # Unary
    | Temp '=' '(' Temp binaryOp Temp ')' # Binary
    | 'branch' label # Branch
    | 'if' '(' Temp condBrOp ')' 'branch' label # CondBranch
    | 'return' Temp? # Return
    | 'parm' Temp # Parm
    | (Temp '=')? 'call' Temp # IndirectCall
    | (Temp '=')? 'call' label # DirectCall
    | Temp '=' memOperand # Load
    | memOperand '=' Temp # Store
    | 'memo' StrLit # Memo
    | label ':' # Mark
    ;

condBrOp
    : '== 0' # EQZ
    | '!= 0' # NEZ
    ;

unaryOp
    : '-' | '!'
    ;

binaryOp
    : '+' | '-' | '*' | '/' | '%'
    | '==' | '!=' | '<' | '<=' | '>' | '>='
    | '&&' | '||'
    ;

label
    : Ident
    ;

memOperand
    : '*' '(' Temp Op=('+'|'-') Number ')'
    ;

vtabLabel
    : 'VTABLE' '<' className=Ident '>'
    ;

funcLabel
    : 'FUNCTION' '<' className=Ident '.' funcName=Ident '>'
    ;

parentLabel
    : vtabLabel
    | 'NULL'
    ;
