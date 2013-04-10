structure ParserCombinator :> PARSER_COMBINATOR =
struct

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

  fun alphaTok str =
    case str of
         "val"      => VAL
       | "datatype" => DATATYPE
       | "type"     => TYPE
       | id         => ID id

  fun symbolic str =
    case str of
         "("  => SOME LPAREN
       | ")"  => SOME RPAREN
       | ","  => SOME COMMA
       | "="  => SOME EQUAL
       | "<>" => SOME NEQUAL
       | "|"  => SOME PIPE
       | "["  => SOME LBRACKET
       | "]"  => SOME RBRACKET
       | "{"  => SOME LBRACE
       | "}"  => SOME RBRACE
       | _    => NONE

  fun symbTok (str, ss) =
    let val symb = symbolic str
        val isSymb = isSome symb
    in case Substring.getc ss of
            NONE          => if isSymb then (valOf symb, ss)
                             else (ID str, ss)
          | SOME (c, ss1) => if isSymb orelse not (Char.isPunct c)
                             then if isSymb then (valOf symb, ss)
                                  else (ID str, ss)
                             else symbTok (str ^ String.str c, ss1)
    end

  fun scanning (toks, ss) =
    case Substring.getc ss of
         NONE => rev toks (* nothing left to scan *)
       | SOME (c, ss1) =>
           if Char.isDigit c
           then (* numerals, just integers for now *)
                let val (num, ss2) = Substring.splitl Char.isDigit ss
                    val tok = (case Int.fromString(Substring.string num) of
                                    NONE   => raise InternalError
                                  | SOME n => NUM n)
                in scanning (tok::toks, ss2) end
           else if Char.isAlphaNum c
           then (* identifier or keyword *)
                let val (id, ss2) = Substring.splitl Char.isAlphaNum ss
                    val tok       = alphaTok (Substring.string id)
                in scanning (tok::toks, ss2) end
           else if c = #"\""
           then (* string *)
                let val (ssStr, ss2) = Substring.position "\"" ss1
                    val ss3          = Substring.dropl (fn c => c = #"\"") ss2
                    val tok          = STRING (Substring.string ssStr)
                in scanning (tok::toks, ss3) end
           else if Char.isPunct c
           then (* special symbol *)
                let val (tok, ss2) = symbTok (String.str c, ss1)
                in scanning (tok::toks, ss2) end
           else (* ignore spaces, line breaks, control characters *)
                scanning (toks, Substring.dropl (not o Char.isGraph) ss)

  fun scan str = scanning ([], Substring.all str)


  (* Tests *)
  val test_symb0 = scan "val test = (123, 42)" =
    [VAL, ID "test", EQUAL, LPAREN, NUM 123, COMMA, NUM 42, RPAREN]
  val test_symb1 = scan "val ? = (hej, 43, __)"  =
    [VAL, ID "?", EQUAL, LPAREN, ID "hej", COMMA, NUM 43, COMMA, ID "__",
     RPAREN]


  (* The parser combinators *)
  infix 6 $-
  infix 6 -$
  infix 5 --
  infix 3 >>
  infix 0 ||

  fun empty toks = ([], toks)

  fun (ph1 || ph2) toks = ph1 toks handle SyntaxError _ => ph2 toks

  fun !! ph toks = ph toks handle SyntaxError msg =>
                           raise Fail ("Syntax error: " ^ msg)

  fun (ph1 -- ph2) toks =
    let val (a, toks2) = ph1 toks
        val (b, toks3) = ph2 toks2
    in ((a, b), toks3) end

  fun (ph >> f) toks =
    let val (x, toks2) = ph toks
    in (f x, toks2) end

  (* Parse with ph on toks zero or more times *)
  fun repeat ph toks = (ph -- repeat ph >> (op::) || empty) toks

  fun ph1 $- ph2 = (ph1 -- ph2) >> (fn (_, y) => y)

  fun ph1 -$ ph2 = (ph1 -- ph2) >> (fn (x, _) => x)


  (* Parser functions *)
  fun id (ID s :: toks) = (Id s, toks)
    | id _ = raise SyntaxError "Identifier expected"

  fun dType (DATATYPE :: toks) = (NA, toks)
    | dType _ = raise SyntaxError "Datatype declaration expected"

  fun equal (EQUAL :: toks) = (NA, toks)
    | equal _ = raise SyntaxError "Equal sign expected"

  fun pipe (PIPE :: toks) = (NA, toks)
    | pipe _ = raise SyntaxError "Pipe expected"

  fun value (VAL :: toks) = (NA, toks)
    | value _ = raise SyntaxError "Value declaration expected"

  fun num (NUM n :: toks) = (Num n, toks)
    | num _ = raise SyntaxError "Number expected"

  fun str (STRING s :: toks) = (Str s, toks)
    | str _ = raise SyntaxError "String expected"

  fun lparen (LPAREN :: toks) = (NA, toks)
    | lparen _ = raise SyntaxError "Left parenthesis expected"

  fun rparen (RPAREN :: toks) = (NA, toks)
    | rparen _ = raise SyntaxError "Right parenthesis expected"

  fun comma (COMMA :: toks) = (NA, toks)
    | comma _ = raise SyntaxError "Comma expected"

  fun lbracket (LBRACKET :: toks) = (NA, toks)
    | lbracket _ = raise SyntaxError "Left bracket expected"

  fun rbracket (RBRACKET :: toks) = (NA, toks)
    | rbracket _ = raise SyntaxError "Right bracket expected"

  fun lbrace (LBRACE :: toks) = (NA, toks)
    | lbrace _ = raise SyntaxError "Left brace expected"

  fun rbrace (RBRACE:: toks) = (NA, toks)
    | rbrace _ = raise SyntaxError "Right brace expected"

  (* Functions for contructing partree types *)
  fun makeDatatype ((n, c), cs) =
    let val enums = map (fn Id n => Enum n) (c :: cs)
    in Decl(Datatype(n, enums)) end

  fun makeValue (n, exp) = Decl (Value (n, exp))

  fun makeTuple (exp, exps) = Tuple (exp :: exps)

  fun makeList (exp, exps) = List (exp :: exps)

  fun makeRecord ((id, exp), idExps) = Record ((id, exp) :: idExps)

  (* Grammar definition *)
  fun decl toks =
    (    dType $- id -- equal $- id -- repeat (pipe $- id) >> makeDatatype
      || value $- id -- equal $- expr                      >> makeValue
    ) toks

  and expr toks =
    (    num
      || str
      || lparen $- expr -- repeat (comma $- expr) -$ rparen     >> makeTuple
      || lbracket $- expr -- repeat (comma $- expr) -$ rbracket >> makeList
      || lbrace $- id -- equal $- expr -- repeat
           (comma $- id -- equal $- expr) -$ rbrace             >> makeRecord
    ) toks

  (* Parsing function *)
  fun parse toks =
    case decl toks of
         (tree, []) => [tree]
       | (tree, ls) => tree :: (parse ls)

end
