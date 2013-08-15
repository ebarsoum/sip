%{(*
    Columbia University

    PLT 4115 Course - SIP Compiler Project

    Under the Supervision of: Prof. Stephen A. Edwards
    Name: Emad Barsoum
    UNI: eb2871

    parser.mly for SIP language
*)
open Ast %}

%token READ WRITE 
%token PLUS MINUS TIMES DIVIDES MOD CONV ASSIGN
%token NEQ LT LEQ GT GEQ EQ AND OR NOT QUES
%token BITAND BITOR BITNOT
%token LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE SEMICOLON COLON COMMA SEMI
%token ARROW RANGE
%token BOOL INT UINT FLOAT HIST IMAGE
%token TRUE FALSE IF ELSE FOR IN WHILE RETURN BREAK FUN KERNEL
%token <bool> BLITERAL
%token <int> ILITERAL
%token <float> FLITERAL
%token <string> ID
%token <string> SLITERAL
%token EOF

%nonassoc NOELSE
%nonassoc ELSE

%right ASSIGN

%left EQ NEQ
%left LT LEQ GT GEQ
%left AND OR
%right NOT

%left PLUS MINUS
%left TIMES DIVIDES MOD
%right UMINUS

%left BITAND BITOR
%right BITNOT

%start program
%type <Ast.program> program

%%

program:
   /* nothing */ { [], [] }
 | program vdef  { ($2 :: fst $1), snd $1 }
 | program fdecl { fst $1, ($2 :: snd $1) }

vdef_list:
    /* nothing */    { [] }
  | vdef_list vdef { $2 :: $1 }

vdef:
    vdecl    { VarDecl($1) }
  | vinit    { Varinit($1) }
  
vdecl:
    basic_type ID SEMI              { { vname = $2; vtype = $1} }
  | img_type ID SEMI                { { vname = $2; vtype = $1} }

basic_type:
    BOOL  { Bool      }
  | INT   { Int       }
  | UINT  { UInt      }
  | FLOAT { Float     }

img_type:
    HIST  { Histogram }
  | IMAGE { Image     }

vinit:
    basic_type ID ASSIGN expr SEMI   { Vinit ({ vname = $2; vtype = $1}, $4) }
  | img_type ID ASSIGN img_expr SEMI { Iminit({ vname = $2; vtype = $1}, $4) }
  | img_type ID ASSIGN LBRACKET row3 row3 row3 RBRACKET SEMI { Immatrix3x3({ vname = $2; vtype = Matrix3x3 }, $5, $6, $7) }

row3:
    LBRACKET expr COMMA expr COMMA expr RBRACKET  { Row($2, $4, $6) }

fdecl:
    FUN ID LPAREN formals_opt RPAREN ftype_opt LBRACE vdef_list stmt_list RBRACE
     { { fname    = $2;
	     fparams  = $4;
	     flocals  = List.rev $8;
	     fbody    = List.rev $9;
		 freturn  = $6;
		 fgpu     = false } }
  | KERNEL ID LPAREN formals_opt RPAREN LBRACE vdef_list stmt_list RBRACE
     { { fname    = $2;
  	     fparams  = $4;
  	     flocals  = List.rev $7;
  	     fbody    = List.rev $8;
  		 freturn  = Void;
		 fgpu     = true } }

formals_opt:
    /* nothing */ { [] }
  | formal_list   { List.rev $1 }

formal_list:
    fparam                   { [$1] }
  | formal_list COMMA fparam { $3 :: $1 }
  
fparam:
    basic_type ID  { { vname = $2; vtype = $1} }
  | img_type ID    { { vname = $2; vtype = $1} }

ftype_opt:
  /* nothing */ { Void }
| ftype         { $1 }

ftype:
    BOOL  { Bool      }
  | INT   { Int       }
  | UINT  { UInt      }
  | FLOAT { Float     }

stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }

stmt:
    expr SEMI { Expr($1) }
  | img_expr { Imexpr($1) }
  | LBRACE stmt_list RBRACE { Block(List.rev $2) }
  | ID READ SLITERAL SEMI { Imread($1, $3) }
  | ID WRITE SLITERAL SEMI { Imwrite($1, $3) }
  | RETURN expr SEMI { Return($2) }
  | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7) }
  | FOR LPAREN expr_opt SEMI expr_opt SEMI expr_opt RPAREN stmt
      { For($3, $5, $7, $9) }
  | WHILE LPAREN expr RPAREN stmt { While($3, $5) }
  | BREAK { Break }

img_expr:
    ID IN LPAREN ID RPAREN SEMI { In($1, [Channel($1,$4)], []) }
  | ID IN LPAREN ID COMMA ID COMMA ID RPAREN SEMI { In($1, [Channel($1,$4); Channel($1,$6); Channel($1,$8)], []) }
  | ID IN LPAREN ID RPAREN FOR LBRACE ID COLON expr RBRACE SEMI { In($1, [Channel($1,$4)], [Assign($8 ^ "_out", $10)]) }
  | ID IN LPAREN ID COMMA ID COMMA ID RPAREN FOR LBRACE ID COLON expr RBRACE SEMI 
      { In($1, [Channel($1,$4); Channel($1,$6); Channel($1,$8)], [Assign($12 ^ "_out", $14)]) }
  | ID IN LPAREN ID COMMA ID COMMA ID RPAREN FOR LBRACE ID COLON expr COMMA ID COLON expr COMMA ID COLON expr RBRACE SEMI 
      { In($1, [Channel($1,$4); Channel($1,$6); Channel($1,$8)], [Assign($12 ^ "_out", $14); Assign($16 ^ "_out", $18); Assign($20 ^ "_out", $22)]) }
  | ID CONV ID SEMI    { Imop($1, Conv, $3) }
  | ID ASSIGN img_expr { Imassign($1, $3) }

expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }

expr:
    BLITERAL                     { BoolLiteral($1) }
  | ILITERAL                     { IntLiteral($1) }
  | FLITERAL                     { FloatLiteral($1) }
  | ID                           { Id($1) }
  | expr PLUS   expr             { Binop($1, Add,   $3) }
  | expr MINUS  expr             { Binop($1, Sub,   $3) }
  | expr TIMES  expr             { Binop($1, Mult,  $3) }
  | expr DIVIDES expr            { Binop($1, Div,   $3) }
  | expr MOD expr                { Binop($1, Mod,   $3) }
  | expr NEQ expr                { Binop($1, Neq,   $3) }
  | expr LT expr                 { Binop($1, Lt,    $3) }
  | expr LEQ expr                { Binop($1, Leq,   $3) }
  | expr GT expr                 { Binop($1, Gt,    $3) }
  | expr GEQ expr                { Binop($1, Geq,   $3) }
  | expr EQ expr                 { Binop($1, Eq,    $3) }
  | expr AND expr                { Binop($1, And,   $3) }
  | expr OR expr                 { Binop($1, Or,    $3) }
  | expr NOT expr                { Binop($1, Not,   $3) }
  | expr BITAND expr             { Binop($1, BitAnd,$3) }
  | expr BITOR expr              { Binop($1, BitOr, $3) }
  | expr BITNOT expr             { Binop($1, BitNot,$3) }
  | MINUS expr %prec UMINUS      { Unop(Neg, $2) }
  | ID ASSIGN expr               { Assign($1, $3) }
  | ID LPAREN actuals_opt RPAREN { Call($1, $3) }
  | LPAREN expr RPAREN           { Bracket($2) }
  | expr QUES expr COLON expr    { Ques($1, $3, $5) }
  | ID LBRACKET expr COMMA expr RBRACKET ARROW ID { Imaccessor($1, $3, $5, $8) }

actuals_opt:
    /* nothing */ { [] }
  | actuals_list  { List.rev $1 }

actuals_list:
    expr                    { [$1] }
  | actuals_list COMMA expr { $3 :: $1 }
