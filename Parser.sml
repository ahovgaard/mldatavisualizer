structure Parser :> PARSER =
struct

  exception InternalError 
  exception SyntaxError of string

  (* Alphanumeric and symbolic keywords *)
  val keywords = ["datatype", "withtype", "and", "of", "val", "int", "real",
                  "string"]
  val symbols  = ["=", "|", "*", "(", ")", "[", "]", "{", "}", ","]

  (* Datatype for lexer tokens *)
  datatype token = KEY of string
                 | ID of string
                 | INT of int
                 | REAL of real
                 | STRING of string
                 | CHAR of char

  (* Parse tree datatypes *)
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

  (* Check if s is a member of the list ls *)
  fun member s ls = List.exists (fn n => n = s) ls


  (* Given an alphanumeical string a, construct a Key type token if a is member
     of the list keywords, else construct an Id type token. *)
  fun alphaTok a = if member a keywords then KEY a else ID a 

  (* Construct a symbolic keyword or identifier *)
  fun symbTok (str, ss) =
    case Substring.getc ss of
         NONE          => if member str symbols
                          then (KEY str, ss) else (ID str, ss)
       | SOME (c, ss1) => if member str symbols orelse not (Char.isPunct c)
                          then (KEY str, ss)
                          else symbTok (str ^ String.str c, ss1)

  fun numTok str = if Char.contains str #"." orelse Char.contains str #"E"
                   then case Real.fromString str of
                             NONE   => raise InternalError
                           | SOME n => REAL n
                   else case Int.fromString str of
                             NONE   => raise InternalError
                           | SOME n => INT n

  (* Scan a character from the substring ss *)
  fun charTok ss = case Substring.getc ss of
                         NONE => raise InternalError
                       | SOME (c, ss1) => (CHAR c, ss1)

  fun scanning (toks, ss) =
    case Substring.getc ss of
         NONE => rev toks (* end of substring, ie. nothing left to scan *)
       | SOME (c, ss1) =>
           if Char.isDigit c orelse c = #"~"
           then (* numerals (reals and ints) *)
                let val (num, ss2) = Substring.splitl
                    (fn c => Char.isDigit c orelse member c (explode ".E~")) ss
                  val tok = numTok (Substring.string num)
                in scanning (tok::toks, ss2) end
           else if Char.isAlphaNum c
           then (* keyword or identifier *)
                let val (id, ss2) = Substring.splitl Char.isAlphaNum ss
                    val tok       = alphaTok (Substring.string id)
                in scanning (tok::toks, ss2) end
           else if c = #"\""
           then (* string *)
                let val (ssStr, ss2) = Substring.position "\"" ss1
                    val ss3          = Substring.dropl (fn c => c = #"\"") ss2
                    val tok          = STRING (Substring.string ssStr)
                in scanning (tok::toks, ss3) end
           else if c = #"#"
           then (* char *)
                let val (tok, ss2) = charTok ss1
                in scanning (tok::toks, ss2) end
           else if Char.isPunct c
           then (* symbol *)
                let val (tok, ss2) = symbTok (String.str c, ss1)
                in scanning (tok::toks, ss2) end
           else (* ignore spaces, line breaks, control characters *)
                scanning (toks, Substring.dropl (not o Char.isGraph) ss)

  fun scan str = scanning ([], Substring.all str)

  (** The parser combinators *)
  infix 6 $- -$
  infix 5 --
  infix 3 >>
  infix 0 ||

  fun empty toks = ([], toks)

  fun (ph1 || ph2) toks = ph1 toks handle SyntaxError _ => ph2 toks

  fun (ph1 -- ph2) toks =
    let val (x, toks')  = ph1 toks
        val (y, toks'') = ph2 toks'
    in ((x, y), toks'') end

  fun (ph >> f) toks =
    let val (x, toks') = ph toks
    in (f x, toks') end

  fun ph1 $- ph2 = ph1 -- ph2 >> #2

  fun ph1 -$ ph2 = ph1 -- ph2 >> #1

  (* Parse with ph on toks zero or more times *)
  fun repeat ph toks = (ph -- repeat ph >> (op::) || empty) toks

  (** Simple parsers *)
  fun id (ID s :: toks) = (s, toks)
    | id _              = raise SyntaxError "Identifier expected"

  fun $ s1 (KEY s2 :: toks) = if s1 = s2 then (s2, toks) else
                              raise SyntaxError ("Keyword " ^ s1 ^ " expected")
    | $ _ _                 = raise SyntaxError "Keyword expected"

  fun num (INT n :: toks)  = (Int n, toks)
    | num (REAL n :: toks) = (Real n, toks)
    | num _                = raise SyntaxError "Number expected"

  fun str (STRING s :: toks) = (String s, toks)
    | str _                  = raise SyntaxError "String expected"

  fun chr (CHAR c :: toks) = (Char c, toks)
    | chr _                = raise SyntaxError "Character expected"

  (* Misc. parsing functions *)
  val parens = fn ph => $"(" $- ph -$ $")"

  (** Grammar definitions *)
  (* Declarations *)
  fun decl toks =
    (    $"val" $- id -$ $"=" -- expr >> Value
      || $"datatype" $- id -$ $"=" -- (datbind -- repeat ($"|" $- datbind))
           >> (fn (str, (ty, tys)) => Datatype (str, ty::tys))
           (* FIXME: ^ could be prettier, Datatype (#1, op:: #2)*)
    ) toks

  (* Expressions *)
  and expr toks =
    (    parens expr 
      || num
      || str
      || chr
      || $"(" $- expr -- repeat ($"," $- expr) -$ $")" >> (Tuple o op::)
      || $"[" $- expr -- repeat ($"," $- expr) -$ $"]" >> (List o op::)
      || $"{" $- id -$ $"=" -- expr --
           repeat ($"," $- id -$ $"=" -- expr) -$ $"}" >> (Record o op::)
      || id -- expr                                    >> MultaryCon
      || id                                            >> NullaryCon
    ) toks

  (* Datatype binding *)
  and datbind toks =
    (    id -$ $"of" -- typ >> MultaryTyCon
      || id                 >> NullaryTyCon
    ) toks

  (* Type expressions *)
  and typ toks =
    (    parens typ
      || $"int"    >> (fn _ => IntTyp)
      || $"real"   >> (fn _ => RealTyp)
      || $"string" >> (fn _ => StringTyp)
      || $"(" $- typ -- repeat ($"," $- typ) -$ $")" >> (TupleTyp o op::)
      || id        >> Tyvar
    ) toks

  (** Parsing function *)
  fun parse toks =
    case decl toks of
         (tree, []) => [tree]
       | (tree, ls) => tree :: parse ls

end
