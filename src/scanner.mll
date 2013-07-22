(* 
    Scanner.mll for SIP language
*)

{ 
  open Parser
  open Printf
}

let digit = ['0'-'9']
let exp   = 'e' ['-' '+']? digit+
let float = digit+ '.' digit* exp? | digit+ exp | '.' digit+ exp?

rule token = parse  
    [' ' '\t' '\r' '\n']  { token lexbuf } (* Whitespace *)
  
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
  | ';'                 { SEMICOLON }
  | ':'                 { COLON     }
  | ','                 { COMMA     }
  | "->"                { ARROW     }

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
  | "break"            { BREAK   }

  (* function *)
  | "fun"              { FUN     }
  | "kernel"           { KERNEL  }
  
  (* Identifier, types, comments and EOF. *)
  | digit+  as lxm     { ILITERAL(int_of_string lxm)   }
  | float   as flt     { FLITERAL(float_of_string flt) }  
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
