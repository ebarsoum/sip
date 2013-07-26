open Printf

type action = Ast | Compile | None

let out_ast_file = "a.ast"
let out_cc_file  = "a.cc"
let out_cl_file  = "a.cl"

let fwrite name content =
	let out = open_out name in
	  fprintf out "%s\n" content;
	  close_out out

let main () =
  let action = 
    if Array.length Sys.argv > 2 then
      List.assoc Sys.argv.(1) [ ("-a", Ast);
                                ("-c", Compile) ]
    else None in
    let lexbuf = Lexing.from_channel (open_in Sys.argv.(2)) in
    let program = Parser.program Scanner.token lexbuf in
    match action with
      Ast -> let listing = Ast.string_of_program program in
               fwrite out_ast_file listing
    | Compile -> fwrite out_cc_file "test"
    | None -> print_string "None\n"

let _ = main ()

