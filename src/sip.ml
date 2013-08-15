(* 
    Columbia University

    PLT 4115 Course - SIP Compiler Project

    Under the Supervision of: Prof. Stephen A. Edwards
    Name: Emad Barsoum
    UNI: eb2871

    sip.ml for SIP Compiler entry point
*)

open Printf

type action = Ast | Compile | TestCpp | TestOpenCL | Error

let out_name = ref "a"

(* Helper function that write "content" into a file named "name" *)
let fwrite name content =
    let out = open_out name in
      fprintf out "%s\n" content;
      close_out out

(* Return filename without extension or path in Unix like system. *)
let get_filename path =
    let i = (String.rindex path '/') in
    String.sub path (i + 1) ((String.length path) - i - 5)

let right_string = 
    "\n             Columbia University\n\n" ^
    "Simple Image Processing Compiler Version 1.0 Preview\n\n" ^
	"Under the Supervision of: Prof. Stephen A. Edwards\n" ^
	"Name: Emad Barsoum, UNI: eb2871\n\n"

let usage_string =
    right_string ^ 
    "Usage:\n\n" ^
    "    Compiling to C++ and OpenCL  : sip -c <filename>\n" ^
    "    Compiling to AST Tree        : sip -a <filename>\n" ^
    "    Compiling to C++ to stdin    : sip -tcc <filename>\n" ^
    "    Compiling to OpenCL to stdin : sip -tcl <filename>\n"

let main () =
  let action = 
    if Array.length Sys.argv > 2 then
	  try
        List.assoc Sys.argv.(1) [ ("-a", Ast);
                                  ("-c", Compile);
                                  ("-tcc", TestCpp);
                                  ("-tcl", TestOpenCL) ]
      with
	    _ -> Error
    else Error in
        let lexbuf = ignore(out_name := get_filename Sys.argv.(2)); 
                     Lexing.from_channel (open_in Sys.argv.(2)) in
        let program = Parser.program Scanner.token lexbuf in
        match action with
          Ast -> let listing = Ast.string_of_program program in
                   fwrite ("./out/" ^ !out_name ^ ".ast") listing
        | Compile -> let listing = Translate.translate_to_cc program !out_name in
                     let cllisting = Translate.translate_to_cl program !out_name in
                   Makefile.gen_makefile !out_name; 
                   fwrite ("./out/" ^ !out_name ^ ".cpp") listing;
                   fwrite ("./out/" ^ !out_name ^ ".cl") cllisting
        | TestCpp -> let listing = Translate.translate_to_cc program !out_name in
                     print_endline listing
        | TestOpenCL -> let listing = Translate.translate_to_cl program !out_name in
                     print_endline listing
        | Error -> print_string usage_string

let _ = main ()
