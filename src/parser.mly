%{ open Ast %}

%token READ WRITE 
%token PLUS MINUS TIMES DIVIDES MOD CONV ASSIGN
%token NEQ LT LEQ GT GEQ EQ AND OR NOT QUES
%token BITAND BITOR BITNOT
%token LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE SEMICOLON COLON COMMA
%token BOOL INT UINT FLOAT HIST IMAGE
%token TRUE FALSE IF ELSE FOR IN WHILE RETURN BREAK FUN KERNEL
%token <bool> BLITERAL
%token <int> ILITERAL
%token <float> FLITERAL
%token <string> ID
%token EOF

%nonassoc NOELSE
%nonassoc ELSE

%right ASSIGN

%left EQ NEQ
%left LT LEQ GT GEQ
%left AND OR
%left NOT

%left PLUS MINUS
%left TIMES DIVIDES MOD
%left UMINUS

%left BITAND BITOR
%left BITNOT
%left CONV

%start program
%type <Ast.program> program

%%

program:
   /* nothing */ { [], [] }
 | program vdecl { ($2 :: fst $1), snd $1 }
 | program fdecl { fst $1, ($2 :: snd $1) }
 | program kdecl { fst $1, ($2 :: snd $1) }

fdecl:
   FUN ID LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
     { { fname   = $2;
	     formals = $4;
	     locals  = List.rev $7;
	     body    = List.rev $8 } }

formals_opt:
    /* nothing */ { [] }
  | formal_list   { List.rev $1 }

formal_list:
    fparam                   { [$1] }
  | formal_list COMMA fparam { $3 :: $1 }

fparam:
    BOOL ID  { { vname = $2; vtype = $1} }
  | INT ID   { { vname = $2; vtype = $1} }
  | UINT ID  { { vname = $2; vtype = $1} }
  | FLOAT ID { { vname = $2; vtype = $1} }
  | HIST ID  { { vname = $2; vtype = $1} }
  | IMAGE ID { { vname = $2; vtype = $1} }

kdecl:
   KERNEL ID LPAREN kformal_list RPAREN LBRACE vdecl_list stmt_list RBRACE
     { { kname    = $2;
  	     kformals = $4;
  	     klocals  = List.rev $7;
  	     kbody    = List.rev $8 } }

kformal_list:
    ID                      { [$1] }
  | kformal_list COMMA ID   { $3 :: $1 }

vdecl_list:
    /* nothing */    { [] }
  | vdecl_list vdecl { $2 :: $1 }

vdecl:
    BOOL ID SEMI  { { vname = $2; vtype = $1} }
  | INT ID SEMI   { { vname = $2; vtype = $1} }
  | UINT ID SEMI  { { vname = $2; vtype = $1} }
  | FLOAT ID SEMI { { vname = $2; vtype = $1} }
  | HIST ID SEMI  { { vname = $2; vtype = $1} }
  | IMAGE ID SEMI { { vname = $2; vtype = $1} }

stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }

stmt:
    expr SEMI { Expr($1) }
  | RETURN expr SEMI { Return($2) }
  | LBRACE stmt_list RBRACE { Block(List.rev $2) }
  | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7) }
  | LPAREN expr RPAREN QUES stmt COLON stmt    { If($2, $5, $7) }
  | FOR LPAREN expr_opt SEMI expr_opt SEMI expr_opt RPAREN stmt
     { For($3, $5, $7, $9) }
  | WHILE LPAREN expr RPAREN stmt { While($3, $5) }
  | ID IN LPAREN channels RPAREN SEMI FOR LBRACE channels_expr_opt RBRACE { In($1, $4, $9) }

channels:
    ID                      { [$1] }
  | channels COMMA ID       { $3 :: $1 }

channel_expr:
    ID COLON expr           { Assign($1, $3) }

channels_expr_opt:
    /* nothing */       { [] }
  | channels_expr_list  { List.rev $1 }

channels_expr_list:
    channel_expr                          { [$1] }
  | channels_expr_list COMMA channel_expr { $3 :: $1 }

expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }

expr:
    ILITERAL                { IntLiteral($1) }
  | FLITERAL                { FloatLiteral($1) }
  | ID                      { Id($1) }
  | expr PLUS   expr        { Binop($1, Add,   $3) }
  | expr MINUS  expr        { Binop($1, Sub,   $3) }
  | MINUS expr %prec UMINUS { Unop(Neg, $2) }
  | expr TIMES  expr        { Binop($1, Mult,  $3) }
  | expr DIVIDE expr        { Binop($1, Div,   $3) }
  | expr EQ     expr        { Binop($1, Equal, $3) }
  | expr NEQ    expr        { Binop($1, Neq,   $3) }
  | expr LT     expr        { Binop($1, Less,  $3) }
  | expr LEQ    expr        { Binop($1, Leq,   $3) }
  | expr GT     expr        { Binop($1, Greater,  $3) }
  | expr GEQ    expr        { Binop($1, Geq,   $3) }
  | ID ASSIGN   expr        { Assign($1, $3) }
  | ID LPAREN actuals_opt RPAREN { Call($1, $3) }
  | LPAREN expr RPAREN      { $2 }

actuals_opt:
    /* nothing */ { [] }
  | actuals_list  { List.rev $1 }

actuals_list:
    expr                    { [$1] }
  | actuals_list COMMA expr { $3 :: $1 }
