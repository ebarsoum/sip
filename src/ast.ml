(*
    Columbia University

    PLT 4115 Course - SIP Compiler Project

    Under the Supervision of: Prof. Stephen A. Edwards
    Name: Emad Barsoum
    UNI: eb2871

    ast.ml for SIP language
*)

type binary_op = Add | Sub | Mult | Div | Mod | 
                 Neq | Lt | Leq | Gt | Geq | Eq | And | Or | Not |
                 BitAnd | BitOr | BitNot

type unary_op = Neg

type image_op = Conv

type var_type = Void | Bool | Int | UInt | Float | Matrix3x3 | Histogram | Image
type var_decl = { vname : string; vtype : var_type }

type expr =
    BoolLiteral of bool
  | IntLiteral of int
  | FloatLiteral of float
  | Id of string
  | Unop of unary_op * expr
  | Binop of expr * binary_op * expr
  | Assign of string * expr
  | Call of string * expr list
  | Ques of expr * expr * expr
  | Bracket of expr
  | Imaccessor of string * expr * expr * string
  | Noexpr

type channel =
    Channel of string * string

type row3 = 
    Row of expr * expr * expr

type img_expr =
    Imop of string * image_op * string
  | In of string * channel list * expr list
  | Imassign of string * img_expr

type var_init =
    Iminit of var_decl * img_expr
  | Vinit of var_decl * expr
  | Immatrix3x3 of var_decl * row3 * row3 * row3

type var_def = 
    VarDecl of var_decl
  | Varinit of var_init

type stmt =
    Block of stmt list
  | Expr of expr
  | Imexpr of img_expr
  | Imread of string * string
  | Imwrite of string * string
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt
  | Break

type func_decl = {
    fname   : string;
    fparams : var_decl list;
    flocals : var_def list;
    fbody   : stmt list;
	freturn : var_type;
	fgpu    : bool
  }

type program = var_def list * func_decl list

(* 
   The below are some helper functions, some of them are used in the translate 
   unit and other for testing 
*)

let string_of_vartype = function
    Void -> "void"
  | Bool -> "bool"
  | Int -> "int"
  | UInt -> "unsigned int"
  | Float -> "float"
  | Matrix3x3 -> "float"
  | Histogram -> "Histogram"
  | Image -> "Image"

let rec string_of_expr = function
    BoolLiteral(l) -> string_of_bool l
  | IntLiteral(l) -> string_of_int l
  | FloatLiteral(l) -> string_of_float l
  | Id(s) -> s
  | Unop(o, e) ->
      (match o with
	Neg -> "-") ^ string_of_expr e
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^
      (match o with
	Add -> "+" | Sub -> "-" | Mult -> "*" | Div -> "/" | Mod -> "%"
      | Neq -> "!=" | Lt -> "<" | Leq -> "<=" | Gt -> ">" | Geq -> ">=" | Eq -> "=="
      | And -> "&&" | Or -> "||" | Not -> "!"
      | BitAnd -> "&" | BitOr -> "|" | BitNot -> "~") ^ " " ^
      string_of_expr e2
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Ques (e1, e2, e3) -> "(" ^ string_of_expr e1 ^ ") ? " ^
      string_of_expr e2 ^ ":" ^ string_of_expr e3
  | Bracket (e) -> "(" ^ string_of_expr e ^ ")"
  | Imaccessor (i, r, c, a) -> i ^ "(" ^ string_of_expr r ^ "," ^ string_of_expr c ^ ")->" ^ a
  | Noexpr -> ""

let string_of_row3 = function
    Row(e1, e2, e3) -> "{" ^ string_of_expr e1 ^ ", " 
	                       ^ string_of_expr e2 ^ ", " 
						   ^ string_of_expr e3 ^ "}"

let get_channel = function
    Channel(_, c) -> c

let string_of_channel = function
    Channel(i, c) -> i ^ "(row, col)->" ^ String.capitalize c

let string_of_channels c =
  if ((List.length c) != 0)
  then 
    (string_of_channel (List.hd c) ^
     String.concat "" (List.map (fun f -> ", " ^ string_of_channel f) (List.tl c)))
  else ""

let rec string_of_img_expr = function
    Imop(s, o, k) -> "conv(" ^ s ^ "' " ^ k ^ ");\n";
  | In (v, a, el) -> "in (" ^ string_of_channels a ^ ")\n{\n" ^ 
      String.concat ";\n" (List.map string_of_expr el) ^ ";}\n"
  | Imassign(v, e) -> v ^ " = " ^ string_of_img_expr e

let string_of_vdecl var = (string_of_vartype var.vtype) ^ " " ^ var.vname

let string_of_vinit = function
    Iminit(v, e) -> string_of_vdecl v ^ " = " ^ string_of_img_expr e
  | Vinit(v, e) -> string_of_vdecl v ^ " = " ^ string_of_expr e
  | Immatrix3x3(v, r1, r2, r3) -> string_of_vdecl v ^ "[3][3] = " ^
	                              "{" ^ string_of_row3 r1 ^ ", " 
                                      ^ string_of_row3 r2 ^ ", " 
							          ^ string_of_row3 r3 ^ "}"

let string_of_vdef = function
    VarDecl(v) -> string_of_vdecl v ^ ";\n"
  | Varinit(vi) -> string_of_vinit vi ^ ";\n"

let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Imexpr(imexpr) -> string_of_img_expr imexpr
  | Imread(i, p) -> i ^ " = imread(" ^ p ^ ");\n";
  | Imwrite(i, p) -> i ^ " = imwrite(" ^ p ^ ");\n";  
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | For(e1, e2, e3, s) ->
      "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") " ^ string_of_stmt s
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s
  | Break -> "break;\n"

let string_of_fdecl fdecl =
  (string_of_vartype fdecl.freturn) ^ " " ^
  fdecl.fname ^ "(" ^ String.concat ", " (List.map string_of_vdecl fdecl.fparams) ^ ")\n{\n" ^
  String.concat "" (List.map string_of_vdef fdecl.flocals) ^
  String.concat "" (List.map string_of_stmt fdecl.fbody) ^
  "}\n"

let string_of_program (global_vars, funcs) =
  String.concat "" (List.map string_of_vdef global_vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs)
