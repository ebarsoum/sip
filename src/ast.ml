(*
    ast.ml for SIP language
*)

type op = Add | Sub | Mult | Div | Mod | Equal | Neq | Less | Leq | Greater | Geq
type unary_op = Neg
type var_type = Bool | Int | UInt | Float | Histogram | Image

type var_decl = { vname : string; vtype : string }

type expr =
    BoolLiteral of bool
  | IntLiteral of int
  | FloatLiteral of float
  | Id of string
  | Unop of unary_op * expr
  | Binop of expr * op * expr
  | Assign of string * expr
  | Call of string * expr list
  | Noexpr

type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt
  | In of string * string list * expr list
  | Ques of expr * stmt * stmt

type func_decl = {
    fname   : string;
    formals : var_decl list;
    locals  : var_decl list;
    body    : stmt list;
  }

type kernel_decl = {
    kname    : string;
    kformals : string list;
    klocals  : var_decl list;
    kbody    : stmt list;
  }

type program = var_decl list * func_decl list

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
      | Equal -> "==" | Neq -> "!="
      | Less -> "<" | Leq -> "<=" | Greater -> ">" | Geq -> ">=") ^ " " ^
      string_of_expr e2
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Noexpr -> ""

let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | For(e1, e2, e3, s) ->
      "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") " ^ string_of_stmt s
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s
  | In (v, a, el) -> "in (" ^ v ^ ", " ^ String.concat ", " a ^ ")\n{\n" ^ 
      String.concat ";\n" (List.map string_of_expr el) ^ "}\n"
  | Ques (e, s1, s2) -> "(" ^ string_of_expr e ^ ") ? " ^
      string_of_stmt s1 ^ ":" ^ string_of_stmt s2

let string_of_vdecl var = var.vtype ^ " " ^ var.vname ^ ";\n"

let string_of_formal var = var.vtype ^ " " ^ var.vname

let string_of_fdecl fdecl =
  fdecl.fname ^ "(" ^ String.concat ", " (List.map string_of_formal fdecl.formals) ^ ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.locals) ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_program (vars, funcs) =
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs)
