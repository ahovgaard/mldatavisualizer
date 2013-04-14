signature PARSER_COMBINATOR =
sig

  exception InternalError 
  exception SyntaxError of string

  datatype token = KEY of string
                 | ID of string
                 | INT of int
                 | REAL of real

  datatype partree = Value of string * expr
                   | Datatype of string * typeDef list

  and expr = Int of int
           | Real of real
           | Tuple of expr list
           | List of expr list
           | Record of (string * expr) list

  and typeDef = NullaryCon of string
              | UnaryCon of string * typ
              | MultaryCon of string * typ list

  and typ = IntTyp
          | RealTyp
          | Tyvar of string

  val scan : string -> token list

  val parse : token list -> partree list

end
