signature PARSER_COMBINATOR =
sig

  exception InternalError
  exception SyntaxError of string

  datatype token = ID of string  | NUM of int  | STRING of string
                 | VAL           | EQUAL       | NEQUAL
                 | LPAREN        | RPAREN      | DATATYPE
                 | TYPE          | COMMA       | PIPE
                 | LBRACKET      | RBRACKET    | LBRACE
                 | RBRACE

  datatype partree = Decl of decl | NA

  and decl = Datatype of id * typeDef list
           | Value of id * expr

  and typeDef = Enum of string

  and id = Id of string 

  and expr = Num of int
           | Str of string
           | Tuple of expr list
           | List of expr list
           | Record of (id * expr) list

  val scan : string -> token list

  val parse : token list -> partree list

end
