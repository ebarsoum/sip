(*
    translate.ml for SIP language

    Name: Emad Barsoum
    UNI: eb2871
*)

open Ast
open Printf
  
module StringMap = Map.Make(String)

(* Symbol table: Information about all the names in scope *)
type env = {
    function_decl : string StringMap.t;   (* Signature for each function *)
    global_var    : var_type StringMap.t; (* global variables and their types*)
    local_var     : var_type StringMap.t; (* locals + function params and their types *)
  }

(* Begining of the C++ file. *)
let cc_headers = "#include \"sip.h\"\n"         ^
                 "using namespace Sip;\n\n"     ^
			     "ClProgram g_clProgram;\n"     ^
				 "Image g__sip_temp__;\n\n"

(* Return a string represntation of function signature *)
let fsig fdecl =
  fdecl.fname ^ "_" ^ String.concat "_" (List.map Ast.string_of_vdecl fdecl.fparams)

(* Extract the declared type from the fully qualified initializer *)
let vdecl_of_vinit = function
    Iminit(v, _) -> (v.vtype, v.vname)
  | Vinit(v, _) -> (v.vtype, v.vname)
  | Immatrix3x3(v, _, _, _) -> (v.vtype, v.vname)

(* Extract the declared type from variable definition. Variable definition is a declared variable
   or declared + initialized *)
let vdecl_of_vdef = function
    VarDecl(v) -> (v.vtype, v.vname)
  | Varinit(vi) -> vdecl_of_vinit vi

(* val enum : int -> 'a list -> (int * 'a) list *)
let rec enum_vdecl = function
    [] -> []
  | hd::tl -> (hd.vtype, hd.vname) :: enum_vdecl tl

let rec enum_vdef = function
    [] -> []
  | hd::tl -> (vdecl_of_vdef hd) :: enum_vdef tl

let rec enum_func = function
    [] -> []
 |  hd:: tl -> (fsig hd, hd.fname) :: enum_func tl  
  
(* val string_map_pairs StringMap 'a -> (int * 'a) list -> StringMap 'a *)
let string_map_pairs map pairs =
  List.fold_left (fun m (i, n) -> StringMap.add n i m) map pairs
  
(* Translate the AST tree into a C++ program *)
let translate_to_cc (globals, functions) out_name =

  (* Allocate "addresses" for each global variable *)
  let global_variables = string_map_pairs StringMap.empty (enum_vdef globals) in
  let function_decls = string_map_pairs StringMap.empty (enum_func functions) in

  (* Translate a function in AST form into a list of bytecode statements *)
  let translate env fdecl =
    (* Bookkeeping: FP offsets for locals and arguments *)
    let local_var = enum_vdef fdecl.flocals
    and formal_var = enum_vdecl fdecl.fparams in
    let env = { env with local_var = string_map_pairs StringMap.empty (local_var @ formal_var) } in
    let dynamic_var = ref StringMap.empty in

    let rec expr e = 
	  (match e with
      BoolLiteral(l) -> string_of_bool l
      | IntLiteral(l) -> string_of_int l
      | FloatLiteral(l) -> string_of_float l
      | Id(s) -> 
		  if ((StringMap.mem s env.local_var) || (StringMap.mem s env.global_var) || (StringMap.mem s !dynamic_var))
            then s
			else raise (Failure ("undeclared variable " ^ s))
      | Unop(o, e) ->
          (match o with
        Neg -> "-") ^ expr e
      | Binop (e1, op, e2) -> 
		  expr e1 ^ " " ^
          (match op with
	    Add -> "+" | Sub -> "-" | Mult -> "*" | Div -> "/" | Mod -> "%"
          | Neq -> "!=" | Lt -> "<" | Leq -> "<=" | Gt -> ">" | Geq -> ">=" | Eq -> "=="
          | And -> "&&" | Or -> "||" | Not -> "!"
          | BitAnd -> "&" | BitOr -> "|" | BitNot -> "~") ^ " " ^
          expr e2
      | Assign (s, e) ->
		  if ((StringMap.mem s env.local_var) || (StringMap.mem s env.global_var) || (StringMap.mem s !dynamic_var))
		    then s ^ " = " ^ expr e
		 	else raise (Failure ("undeclared variable " ^ s))
      | Call (fname, actuals) -> 
		  if (StringMap.mem fname env.function_decl)
		    then fname ^ "(" ^ String.concat ", " (List.map expr (List.rev actuals)) ^ ")"
		 	else raise (Failure ("undefined function " ^ fname))
  	  | Ques (e1, e2, e3) -> "(" ^ expr e1 ^ ") ? " ^
  	      expr e2 ^ ":" ^ expr e3
      | Bracket (e) -> "(" ^ expr e ^ ")"
      | Noexpr -> "")

   in let add_channels_var c =
      if ((List.length c) != 0)
      then
        (String.concat "" (List.map (fun f ->
		  (* print_string ((Ast.get_channel f) ^ "\n"); *)
		  ignore(dynamic_var := StringMap.add (Ast.get_channel f) Ast.UInt !dynamic_var);
		  ignore(dynamic_var := StringMap.add ((Ast.get_channel f) ^ "_out") Ast.UInt !dynamic_var);
		  "") c))
 	else raise (Failure ("empty channel list in an \"In\" statement "))

   in let expand_channels c =
	    if ((List.length c) != 0)
	    then
	      (String.concat "" (List.map (fun f ->
			  "        unsigned int " ^ Ast.get_channel f ^ " = " ^ Ast.string_of_channel f ^ ";\n" ^
		      "        unsigned int " ^ Ast.get_channel f ^ "_out = " ^ Ast.string_of_channel f ^ ";\n") c))
	 	else raise (Failure ("empty channel list in an \"In\" statement "))

    in let rec img_expr = function
	      Imop(s, o, k) -> 
			  if ((StringMap.mem s env.local_var) || (StringMap.mem s env.global_var)) then begin
			        if ((StringMap.mem k env.local_var) || (StringMap.mem k env.global_var)) then
			           "g_clProgram.ApplyFilter(" ^ s ^ ", g__sip_temp__, (float*)&" ^ k ^ ");\n"
					else raise (Failure ("undeclared variable " ^ k))
				end
			 	else raise (Failure ("undeclared variable " ^ s))
	    | In (v, a, el) -> ignore(add_channels_var a); (* To force the order, we need to add the variable before evluating the expr. *)
            "g__sip_temp__.clone(" ^ v ^ ");\n" ^
            "for (int row = 0; row <" ^ v ^ ".height(); ++row)\n{\n"        ^
            "    for (int col = 0; col <" ^ v ^ ".width(); ++col)\n    {\n" ^
            expand_channels a ^ "\n" ^
	        String.concat ";\n" (List.map expr el) ^ ";\n\n"  ^
	        "        g__sip_temp__(row, col)->Red   = (char)red_out;\n"   ^
	        "        g__sip_temp__(row, col)->Green = (char)green_out;\n" ^
	        "        g__sip_temp__(row, col)->Blue  = (char)blue_out;\n"  ^
			"        g__sip_temp__(row, col)->Alpha = " ^ v ^ "(row, col)->Alpha;\n" ^
			"    }\n}\n"
        | Imassign(v, e) -> img_expr e ^ "\n" ^ v ^ " = g__sip_temp__;\n"

    in let rec stmt = function
	    Block(sl) -> 
          String.concat "" (List.map stmt sl) ^ "\n"
	  | Expr(e) -> expr e ^ ";\n";
	  | Imexpr(imexpr) -> img_expr imexpr
	  | Imread(i, p) -> i ^ ".read(" ^ p ^ ");\n";
	  | Imwrite(i, p) -> i ^ ".write(" ^ p ^ ");\n";  
	  | Return(e) -> "return " ^ expr e ^ ";\n";
	  | If(e, s, Block([])) -> "if (" ^ expr e ^ ")\n" ^ stmt s
      | If(e, s1, s2) ->  "if (" ^ expr e ^ ")\n" ^
	      stmt s1 ^ "else\n" ^ stmt s2
	  | For(e1, e2, e3, s) ->
	      "for (" ^ expr e1  ^ " ; " ^ expr e2 ^ " ; " ^
	      expr e3  ^ ") " ^ stmt s
	  | While(e, s) -> "while (" ^ expr e ^ ") " ^ stmt s
      | Break -> "break;\n"

    in let vartype = function
        Void -> "void"
	  | Bool -> "bool"
	  | Int -> "int"
	  | UInt -> "unsigned int"
	  | Float -> "float"
      | Matrix3x3 -> "float"
	  | Histogram -> "Histogram"
	  | Image -> "Image"

    in let func_params_type = function
        Void -> "void"
      | Bool -> "bool"
      | Int -> "int"
      | UInt -> "unsigned int"
      | Float -> "float"
      | Matrix3x3 -> "float"
      | Histogram -> "Histogram&"
      | Image -> "Image&"
	  
  in (if ((String.compare fdecl.fname "main") == 0)
      then "int main()\n{\n" ^ "    g_clProgram.CompileClFile(\"./" ^ out_name ^ ".cl\");\n\n"
      else (vartype fdecl.freturn) ^ " " ^ fdecl.fname
      ^ if ((List.length fdecl.fparams) != 0)
        then ("("
      ^ func_params_type (List.hd fdecl.fparams).vtype ^ " " ^ (List.hd fdecl.fparams).vname ^ " "
      ^ String.concat "" (List.map (fun formal -> ", " ^ func_params_type formal.vtype ^ " " ^ formal.vname) (List.tl fdecl.fparams)) ^ ")\n{\n")
        else "()\n{\n"
        )
      ^ String.concat "" (List.map Ast.string_of_vdef (List.rev fdecl.flocals)) ^ "\n"
      ^ stmt (Block fdecl.fbody) ^ "\n" ^ 
      if ((String.compare fdecl.fname "main") == 0)
	  then "    return 0;\n}\n"
      else "\n}\n"
	  
  in let env = { 
         function_decl = function_decls;
		 global_var = global_variables;
		 local_var = StringMap.empty } in

  (* Code executed to start the program: Jsr main; halt *)
  let _ = try
    (StringMap.find "main" function_decls)
  with Not_found -> raise (Failure ("no \"main\" function"))
    
  (* Compile the functions *)
  in cc_headers ^
    String.concat "" (List.map Ast.string_of_vdef (List.rev globals)) ^ "\n" ^
	(String.concat "\n" (List.map (translate env) (List.rev functions))) ^ "\n"

(* Translate the AST tree into a OpenCL shader program *)
let translate_to_cl (globals, functions) out_name =
	"__constant sampler_t sampler =  CLK_NORMALIZED_COORDS_FALSE | CLK_ADDRESS_CLAMP_TO_EDGE | CLK_FILTER_NEAREST;

    __kernel void apply_filter(__read_only image2d_t in_image, __write_only image2d_t out_image, __constant float* filter)
    {
        const int2 pos = {get_global_id(0), get_global_id(1)};

        float4 sum = (float4)(0.0f);
        for (int y = -1; y <= 1; y++)
	    {
            for (int x = -1; x <= 1; x++)
		    {
                sum.x += filter[(y + 1) * 3 + (x + 1)] * read_imagef(in_image, sampler, pos + (int2)(x,y)).x;
                sum.y += filter[(y + 1) * 3 + (x + 1)] * read_imagef(in_image, sampler, pos + (int2)(x,y)).y;
                sum.z += filter[(y + 1) * 3 + (x + 1)] * read_imagef(in_image, sampler, pos + (int2)(x,y)).z;
            }
       }
	
       write_imagef (out_image, (int2)(pos.x, pos.y), sum);
    }\n"

