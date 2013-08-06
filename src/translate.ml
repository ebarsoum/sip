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

  (* Translate a function in AST form into a list of bytecode statements *)
  let translate env fdecl =
    (* Bookkeeping: FP offsets for locals and arguments *)
    let num_formals = List.length fdecl.formals
    and num_locals = List.length fdecl.locals
    and local_var = enum_var fdecl.locals
    and formal_var = enum_var fdecl.formals in
    let env = { env with local_var = string_map_pairs
		  StringMap.empty (local_var @ formal_var) } in

    let rec expr = function
      BoolLiteral(l) -> string_of_bool l
      | IntLiteral(l) -> string_of_int l
      | FloatLiteral(l) -> string_of_float l
      | Id(s) -> 
		  if (StringMap.mem s env.local_var) || (StringMap.mem s env.global_var)
            then s
			else raise (Failure ("undeclared variable " ^ s)))
      | Binop (e1, op, e2) -> 
		  expr e1 ^ " " ^
          (match op with
	    Add -> "+" | Sub -> "-" | Mult -> "*" | Div -> "/" | Mod -> "%"
          | Neq -> "!=" | Lt -> "<" | Leq -> "<=" | Gt -> ">" | Geq -> ">=" | Eq -> "=="
          | And -> "&&" | Or -> "||" | Not -> "!"
          | BitAnd -> "&" | BitOr -> "|" | BitNot -> "~") ^ " " ^
          expr e2
      | Assign (s, e) -> 
		  if (StringMap.mem s env.local_var) || (StringMap.mem s env.global_var)
		    then s ^ " = " ^ expr e
		 	else raise (Failure ("undeclared variable " ^ s)))
      | Call (fname, actuals) -> 
		  if (StringMap.mem fname env.function_decl)
		    then fname ^ "(" ^ String.concat ", " (List.map expr (List.rev actuals)) ^ ")"
		 	else raise (Failure ("undefined function " ^ fname)))
      | Noexpr -> ""

    in let rec stmt = function
	    Block(sl) -> 
          "{\n" ^ String.concat "" (List.map stmt sl) ^ "}\n"
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

    in [Ent num_locals] @      (* Entry: allocate space for locals *)
    stmt (Block fdecl.body) @  (* Body *)
    [Lit 0; Rts num_formals]   (* Default = return 0 *)

  in let env = { 
         function_decl = function_indexes;
		 global_var = global_indexes;
		 local_var = StringMap.empty } in

  (* Code executed to start the program: Jsr main; halt *)
  let entry_function = try
    [Jsr (StringMap.find "main" function_indexes); Hlt]
  with Not_found -> raise (Failure ("no \"main\" function"))
  in
    
  (* Compile the functions *)
  let func_bodies = entry_function :: List.map (translate env) functions in

  (* Calculate function entry points by adding their lengths *)
  let (fun_offset_list, _) = List.fold_left
      (fun (l,i) f -> (i :: l, (i + List.length f))) ([],0) func_bodies in
  let func_offset = Array.of_list (List.rev fun_offset_list) in

  { num_globals = List.length globals;
    (* Concatenate the compiled functions and replace the function
       indexes in Jsr statements with PC values *)
    text = Array.of_list (List.map (function
	Jsr i when i > 0 -> Jsr func_offset.(i)
      | _ as s -> s) (List.concat func_bodies))
  }
