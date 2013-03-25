signature LEXER =
sig

  exception InternalError

  datatype token = ID of string       | NUM of int        | VAL 
                 | EQUAL              | NEQUAL            | LPAREN
                 | RPAREN             | DATATYPE          | TYPE
                 | COMMA              | PIPE              | LBRACKET
                 | RBRACKET           | LBRACE            | RBRACE

  val scan : string -> token list

end
