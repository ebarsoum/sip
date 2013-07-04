{ 
  open Parser
  open Printf
}

let newline    = '\n' | "\r\n"
let whitespace = [' ' '\t']
let digit      = ['0'-'9']
let exp = 'e' ['-' '+']? digit+
let float = digit+ '.' digit* exp? | digit+ exp | '.' digit+ exp?

rule token = parse
  newline               { Lexing.new_line lexbuf; token lexbuf }
  | whitespace          { token lexbuf }
  
  (* Arithmetic operations *)
  | '+'                 { PLUS    }
  | '-'                 { MINUS   }
  | '*'                 { TIMES   }
  | '/'                 { DIVIDES }
  | '%'                 { MODE    }
  | '^'                 { CONVOLUTION }
  | '='                 { ASSIGN  }

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
  | ';'                 { SEMICOLON }
  | ':'                 { COLON     }
  | ','                 { COMMA     }
    
 (* Supported types *)
    | "bool"            { BOOL    }
    | "int"             { INT     }
    | "uint"            { UINT    }
    | "float"           { FLOAT   }
    | "histogram"       { HIST    }
    | "image"           { IMAGE   }
    
 (* Control flow and loop *)
  | "true"             { TRUE    }
  | "false"            { FALSE   }
  | "if"               { IF      }
  | "else"             { ELSE    }
  | "for"              { FOR     }
  | "in"               { IN      }
  | "while"            { WHILE   }
  | "return"           { RETURN  }

  | ['0'-'9']+ as lxm { LITERAL(int_of_string lxm) }
  | ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
  | float   as flt     { FLOAT_LITERAL(float_of_string flt) }
  | "/*"                { comment lexbuf    }
  | "//"                { line_comment lexbuf   }
  | eof                { EOF }
  | _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
     "*/" { token lexbuf }
    | _     { comment lexbuf }

and line_comment = parse
    newline { token lexbuf }
    | _     { line_comment lexbuf }
