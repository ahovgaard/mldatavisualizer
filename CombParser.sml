(*
 * Parser combinator for Standard ML data structures and datatype declarations.
 *)

(* Exception for catching internal errors in the lexer *)
exception InternalError

(* Datatype of tokens for lexing *)
datatype token = ID of string       | NUM of int        | VAL 
               | EQUAL              | NEQUAL            | LPAREN
               | RPAREN             | DATATYPE          | TYPE
               | COMMA              | PIPE              | LBRACKET
               | RBRACKET           | LBRACE            | RBRACE

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
         else if Char.isPunct c
         then (* special symbol *)
              let val (tok, ss2) = symbTok (String.str c, ss1)
              in scanning (tok::toks, ss2) end
         else (* ignore spaces, line breaks, control characters *)
              scanning (toks, Substring.dropl (not o Char.isGraph) ss)

fun scan str = scanning ([], Substring.all str)

val test_symb0 = scan "val test = (123, 42)" =
  [VAL, ID "test", EQUAL, LPAREN, NUM 123, COMMA, NUM 42, RPAREN]
val test_symb1 = scan "val ? = (hej, 43, __)"  =
  [VAL, ID "?", EQUAL, LPAREN, ID "hej", COMMA, NUM 43, COMMA, ID "__",
   RPAREN]

exception SyntaxError of string

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

(*fun infixes (ph, prec_of, apply) =
  let fun over k toks = next k (ph toks)
      and next k (x, Lex.Key(a)::toks) =
             if prec_of a < k then (x, Lex.Key a :: toks)
             else next k ((over (prec_of a) >> apply a x) toks)
        | next k (x, toks) = (x, toks)
  in over 0 end

fun reader ph a =
  (case ph (Lex.scan a) of
        (x, [])   => x
      | (_, _::_) => raise SyntaxError "Extra characters in phrase")*)


datatype partree = Decl of decl | NA

and decl = Datatype of id * typeDef list
         | Value of id * expr

and typeDef = Enum of string
            (*| Tuple of string * string list*)

and id = Id of string 

and expr = Num of int
         | Tuple of expr list
         | List of expr list


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


fun makeDatatype ((n, c), cs) =
  let val enums = map (fn Id n => Enum n) (c :: cs)
  in Decl(Datatype(n, enums)) end

fun makeValue (n, exp) = Decl (Value (n, exp))

fun makeTuple (exp, exps) = Tuple (exp :: exps)

fun makeList (exp, exps) = List (exp :: exps)

fun decl toks =
  (    dType $- id -- equal $- id -- repeat (pipe $- id) >> makeDatatype
    || value $- id -- equal $- expr                      >> makeValue
  ) toks

and expr toks =
  (    num
    || lparen $- expr -- repeat (comma $- expr) -$ rparen     >> makeTuple
    || lbracket $- expr -- repeat (comma $- expr) -$ rbracket >> makeList
  ) toks
