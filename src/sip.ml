open Printf

type action = Ast | Compile | None

let out_filename = "a"

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
               fwrite (out_filename ^ ".ast") listing
    | Compile -> let listing = Translate.translate_to_cc program in
               fwrite (out_filename ^ ".cc") listing
    | None -> print_string "None\n"

let _ = main ()
