open Ast

module StringMap = Map.Make(String)

(* Symbol table: Information about all the names in scope *)
type env = {
    function_decl : func_type StringMap.t; (* Index for each function *)
    global_var    : var_type StringMap.t; (* "Address" for global variables *)
    local_var     : var_type StringMap.t; (* FP offset for args, locals *)
  }

(* val enum : int -> 'a list -> (int * 'a) list *)
let rec enum_vdecl = function
    [] -> []
  | hd::tl -> (hd.vtype, hd.vname) :: enum_vdecl tl

let rec enum_func = function
    [] -> []
 |  hd:: tl -> (hd.ftype, hd.fname) :: enum_func tl  
  
(* val string_map_pairs StringMap 'a -> (int * 'a) list -> StringMap 'a *)
let string_map_pairs map pairs =
  List.fold_left (fun m (i, n) -> StringMap.add n i m) map pairs
  
(** Translate a program in AST form into a bytecode program.  Throw an
    exception if something is wrong, e.g., a reference to an unknown
    variable or function *)
let translate_to_cc (globals, functions) =

  (* Allocate "addresses" for each global variable *)
  let global_variables = string_map_pairs StringMap.empty (enum_vdecl globals) in
  let function_decls = string_map_pairs StringMap.empty (enum_func functions) in

  (* Translate a function in AST form into a list of bytecode statements *)
  let translate env fdecl =
    (* Bookkeeping: FP offsets for locals and arguments *)
    let num_formals = List.length fdecl.formals
    and num_locals = List.length fdecl.locals
    and local_var = enum_vdecl fdecl.locals
    and formal_var = enum_vdecl fdecl.formals in
    let env = { env with local_var = string_map_pairs
		  StringMap.empty (local_var @ formal_var) } in

    let rec expr = function
      BoolLiteral(l) -> string_of_bool l
      | IntLiteral(l) -> string_of_int l
      | FloatLiteral(l) -> string_of_float l
      | Id(s) -> 
		  if ((StringMap.mem s env.local_var) || (StringMap.mem s env.global_var))
            then s
			else raise (Failure ("undeclared variable " ^ s))
      | Binop (e1, op, e2) -> 
		  expr e1 ^ " " ^
          (match op with
	    Add -> "+" | Sub -> "-" | Mult -> "*" | Div -> "/" | Mod -> "%"
          | Neq -> "!=" | Lt -> "<" | Leq -> "<=" | Gt -> ">" | Geq -> ">=" | Eq -> "=="
          | And -> "&&" | Or -> "||" | Not -> "!"
          | BitAnd -> "&" | BitOr -> "|" | BitNot -> "~") ^ " " ^
          expr e2
      | Assign (s, e) -> 
		  if ((StringMap.mem s env.local_var) || (StringMap.mem s env.global_var))
		    then s ^ " = " ^ expr e
		 	else raise (Failure ("undeclared variable " ^ s))
      | Call (fname, actuals) -> 
		  if (StringMap.mem fname env.function_decl)
		    then fname ^ "(" ^ String.concat ", " (List.map expr (List.rev actuals)) ^ ")"
		 	else raise (Failure ("undefined function " ^ fname))
      | Noexpr -> ""

    in let rec stmt = function
	    Block(sl) -> 
          String.concat "" (List.map stmt sl) ^ "\n"
	  | Expr(e) -> expr e ^ ";\n";
	  | Imop(s, o, k) -> "conv(" ^ s ^ "' " ^ k ^ ");\n";
	  | Imread(i, p) -> i ^ " = imread(" ^ p ^ ");\n";
	  | Imwrite(i, p) -> i ^ " = imwrite(" ^ p ^ ");\n";  
	  | Return(e) -> "return " ^ expr e ^ ";\n";
	  | If(e, s, Block([])) -> "if (" ^ expr e ^ ")\n" ^ stmt s
      | If(e, s1, s2) ->  "if (" ^ expr e ^ ")\n" ^
	      stmt s1 ^ "else\n" ^ stmt s2
	  | For(e1, e2, e3, s) ->
	      "for (" ^ expr e1  ^ " ; " ^ expr e2 ^ " ; " ^
	      expr e3  ^ ") " ^ stmt s
	  | While(e, s) -> "while (" ^ expr e ^ ") " ^ stmt s
	  | In (v, a, el) -> "in (" ^ v ^ ", " ^ String.concat ", " a ^ ")\n{\n" ^ 
	      String.concat ";\n" (List.map expr el) ^ ";}\n"
	  | Ques (e1, e2, e3) -> "(" ^ expr e1 ^ ") ? " ^
	      expr e2 ^ ":" ^ expr e3 ^ ";\n"
  in let functype = function
		FVoid -> "void"
	  | FBool -> "bool"
	  | FInt -> "int"
	  | FUInt -> "unsigned int"
	  | FFloat -> "float"

  in let vartype = function
		Bool -> "bool"
	  | Int -> "int"
	  | UInt -> "unsigned int"
	  | Float -> "float"
	  | Histogram -> "Histogram"
	  | Image -> "Image"
  in let name = fdecl.fname 
  in (if ((String.compare name "main") == 0) 
      then "int main(char* args)\n"
      else (functype fdecl.ftype) ^ " " ^ fdecl.fname
      ^ if ((List.length fdecl.formals) != 0)
        then ("("
      ^ vartype (List.hd fdecl.formals).vtype ^ " " ^ (List.hd fdecl.formals).vname ^ " "
      ^ String.concat "" (List.map (fun formal -> ", " ^ vartype formal.vtype ^ " " ^ formal.vname) (List.tl fdecl.formals)) ^ ")\n")
        else "()\n"
        )
      ^ "{\n" ^String.concat "" (List.map Ast.string_of_vdecl (List.rev fdecl.locals)) ^ "\n"
      ^ stmt (Block fdecl.body) ^ "}\n" 

  in let env = { 
         function_decl = function_decls;
		 global_var = global_variables;
		 local_var = StringMap.empty } in

  (* Code executed to start the program: Jsr main; halt *)
  let entry_function = try
    (StringMap.find "main" function_decls)
  with Not_found -> raise (Failure ("no \"main\" function"))
    
  (* Compile the functions *)
  in String.concat "" (List.map Ast.string_of_vdecl (List.rev globals)) ^ "\n" ^
	(String.concat "\n" (List.map (translate env) (List.rev functions))) ^ "\n"
