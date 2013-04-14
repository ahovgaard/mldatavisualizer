structure ParserCombinator :> PARSER_COMBINATOR =
struct

  exception InternalError 
  exception SyntaxError of string

  (* Alphanumeric and symbolic keywords *)
  val keywords = ["datatype", "withtype", "and", "of", "val", "int", "real"]
  val symbols  = ["=", "|", "*", "(", ")", "[", "]", "{", "}", ","]

  (* Datatype for lexer tokens *)
  datatype token = KEY of string
                 | ID of string
                 | INT of int
                 | REAL of real

  (* Parse tree datatypes *)
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

  fun ph1 $- ph2 = ph1 -- ph2 >> #2 (*(fn (_, y) => y)*)

  fun ph1 -$ ph2 = ph1 -- ph2 >> #1 (*(fn (x, _) => x)*)

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
    (    num
      || $"(" $- expr -- repeat ($"," $- expr) -$ $")" >> (Tuple o op::)
      || $"[" $- expr -- repeat ($"," $- expr) -$ $"]" >> (List o op::)
      || $"{" $- id -$ $"=" -- expr --
           repeat ($"," $- id -$ $"=" -- expr) -$ $"}" >> (Record o op::)
    ) toks

  (* Datatype binding *)
  and datbind toks =
    (    id -$ $"of" -- (typ -$ $"*" -- typ -- repeat ( $"*" $- typ))
           >> (fn (str, ((t0, t1), ts)) => MultaryCon (str, t0::t1::ts))
      || id -$ $"of" -- typ >> UnaryCon
      || id                 >> NullaryCon
    ) toks

  (* Type expressions *)
  and typ toks =
    (    $"int"  >> (fn s => IntTyp) (*FIXME: improve weird looking fn*)
      || $"real" >> (fn s => RealTyp)
      || id      >> Tyvar
    ) toks

  (** Parsing function *)
  fun parse toks =
    case decl toks of
         (tree, []) => [tree]
       | (tree, ls) => tree :: parse ls

end
