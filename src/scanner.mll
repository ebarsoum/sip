(* 
    Scanner.mll for SIP language
*)

{ 
  open Parser
  open Printf
}

let newline    = '\n' | "\r\n"
let whitespace = [' ' '\t']
let digit   = ['0'-'9']
let exp     = 'e' ['-' '+']? digit+
let int_t   = digit+
let float_t = digit+ '.' digit* exp? | digit+ exp | '.' digit+ exp?
let string_r = '\\'+ | [^ '"']
let string_t = '"' string_r* '"'

rule token = parse  
  newline				{ Lexing.new_line lexbuf; token lexbuf }
  | whitespace			{ token lexbuf }
  
  (* Read and write image files *)
  | "<<"                 { READ  }
  | ">>"                 { WRITE }

  (* Arithmetic operations *)
  | '+'                 { PLUS        }
  | '-'                 { MINUS       }
  | '*'                 { TIMES       }
  | '/'                 { DIVIDES     }
  | '%'                 { MOD         }
  | '^'                 { CONV        }
  | '='                 { ASSIGN      }

  (* Logic operations *)
  | "!="                { NEQ     }
  | '<'                 { LT      }
  | "<="                { LEQ     }
  | '>'                 { GT      }
  | ">="                { GEQ     }
  | "=="                { EQ      }
  | "&&"                { AND     }
  | "||"                { OR      }
  | '!'                 { NOT     }
  | '?'                 { QUES    }

  (* Bit operations *)
  | '&'                { BITAND  }
  | '|'                { BITOR   }
  | '~'                { BITNOT  }

  (* Scoping, accessors, and sequences *)
  | '('                 { LPAREN    }
  | ')'                 { RPAREN    }
  | '['                 { LBRACKET  }
  | ']'                 { RBRACKET  }
  | '{'                 { LBRACE    }
  | '}'                 { RBRACE    }
  | ';'                 { SEMI      }
  | ':'                 { COLON     }
  | ','                 { COMMA     }
  | "->"                { ARROW     }
  | ".."                { RANGE     }

  (* Supported types *)
  | "bool"            { BOOL    }
  | "int"             { INT     }
  | "uint"            { UINT    }
  | "float"           { FLOAT   }
  | "histogram"       { HIST    }
  | "image"           { IMAGE   }
    
  (* Control flow and loop *)
  | "if"               { IF      }
  | "else"             { ELSE    }
  | "for"              { FOR     }
  | "in"               { IN      }
  | "while"            { WHILE   }
  | "return"           { RETURN  }
  | "break"            { BREAK   }

  (* function *)
  | "fun"              { FUN     }
  | "kernel"           { KERNEL  }
  
  (* Identifier, types, comments and EOF. *)
  | "true"             { BLITERAL(true) }
  | "false"            { BLITERAL(false)}
  | int_t  as lxm      { ILITERAL(int_of_string lxm)   }
  | float_t as flt     { FLITERAL(float_of_string flt) }  
  | string_t as lxm    { SLITERAL(lxm) }
  | ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
  | "/*"               { comment lexbuf        }
  | "//"               { line_comment lexbuf   }
  | eof                { EOF }
  | _ as char          { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
    "*/"   { token lexbuf   }
  | _      { comment lexbuf }

and line_comment = parse
    '\n' | "\r\n" { token lexbuf        }
  | _             { line_comment lexbuf }
