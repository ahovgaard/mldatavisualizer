signature PARSER =
sig

  exception InternalError 
  exception SyntaxError of string

  datatype token = KEY of string
                 | ID of string
                 | INT of int
                 | REAL of real
                 | STRING of string
                 | CHAR of char

  datatype partree = Value of string * expr
                   | Datatype of string * typeDef list

  and expr = Int of int
           | Real of real
           | String of string
           | Char of char
           | Tuple of expr list
           | List of expr list
           | Record of (string * expr) list
           | NullaryCon of string
           | MultaryCon of string * expr

  and typeDef = NullaryTyCon of string
              | MultaryTyCon of string * typ

  and typ = IntTyp
          | RealTyp
          | StringTyp
          | TupleTyp of typ list
          | Tyvar of string

  val scan : string -> token list

  val parse : token list -> partree list

end
