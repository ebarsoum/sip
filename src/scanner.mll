{ open Parser
  open Printf
 }

let newline    = '\n' | "\r\n"
let whitespace = [' ' '\t']
let digit      = ['0'-'9']
let integer    = digit+
let letter     = ['_' 'a'-'z' 'A'-'Z']
let letdig     = letter | digit
let identifier = letter letdig*
let exp = 'e' ['-' '+']? digit+
let float1 = digit+ '.' digit* exp?
let float2 = digit+ exp
let float3 = '.' digit+ exp?


rule token = parse
  newline				{ Lexing.new_line lexbuf; token lexbuf }
  | whitespace			{ token lexbuf }
  
  (* compute_operators *)
  | '+'					{ PLUS    }
  | '-'					{ MINUS   }
  | '*'					{ TIMES   }
  | '/'					{ DIVIDES  }
  | "*."				{ MTIMES  }
  | '%'					{ MODE    }
 
 (* arithmetic_operator *)
  | "!="				{ NEQ     }
  | '<'					{ LT      }
  | "<="				{ LEQ     }
  | '>'					{ GT      }
  | ">="				{ GEQ     }
  | "=="				{ EQ      }
 
 (* matrix_operator *)
  | ".*"				{ CROPROD }
  | "**"				{ CONVOLUTION }
  | '''					{ TRANSPO }
  | '"'					{ INVERSE }
  
 (* assignment_operator *)
 	| '='				{ ASSIGN  }
 	| "+="				{ PLUSASS }
 	| "-="				{ MINUASS }
 	
 (* boolean_operator *)
 	| "&&"				{ AND     }
 	| "||"				{ OR      }
 	| '!'				{ NOT     }
 
 (* bar&trace *)
 	| "|"			{ BAR }
 	| "tr"		{ TRACE }
 	
 (* build-in fun *)
  | "submat" { SUBMAT }
  | "sum"    { SUM }
  | "Init"	{ INIT	}
 	
 (* punctuation *)
  | '('					{ LPAREN  }
  | ')'					{ RPAREN  }
  | '['					{ LBRACKET   }
  | ']'					{ RBRACKET   }
  | '{'					{ LBRACE  }
  | '}'					{ RBRACE  }
  | ';'					{ SEMICOLON    }
  | ':'					{ COLON   }
  | ','					{ COMMA   }
  | '?'					{ QUES	}
  | "[["					{ SLASH }
  | "]]"					{ BACKSLASH }
 	
 (* build-in_type *)
 	| "int"				{ INT     }
 	| "float"			{ FLOAT   }
	| "intRowVec"		{ INTROWVEC	}
	| "floatRowVec"		{ FLOATROWVEC	}
	| "intColVec"		{ INTCOLVEC	}
	| "floatColVec"		{ FLOATCOLVEC	}
	| "intMat"			{ INTMAT	}
	| "floatMat"		{ FLOATMAT	}
 	
 (* control_flow *)
	| "if"               { IF      }
	| "else"             { ELSE    }
	| "while"            { WHILE   }
	| "for"              { FOR     }
	| "in"				{ IN      }
	| "endif"			{ ENDIF		}
	| "return"			{ RETURN	}
  
 (* selection *)
  | "switch"           { SWITCH  }
  
 (* I/O *)
 	| "import"					{ IMPORT }
 	| "export"					{ EXPORT }
 		
 (* boolean_true_false *) 
  | "true" as lxm            { TRUE(lxm)    }
  | "false" as lxm            { FALSE(lxm)   } 	
  | integer as lit     { INT_LITERAL(int_of_string lit) }
  | (float1 | float2 | float3) as fl { FLOAT_LITERAL(float_of_string fl) }
  | identifier as lxm { IDENTIFIER(lxm) }
  | eof                { EOF }
  
  | "/*"				{ comment lexbuf	}
  
and comment = parse
 	 "*/" { token lexbuf }
	| _     { comment lexbuf }
