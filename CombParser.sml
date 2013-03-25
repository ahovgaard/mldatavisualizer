(* Parser combinator for Standard ML data structures and datatype declarations.
 * TODO:
 * - Substring.all / Substring.full, in scan function, depending on the SML
 *   implementation (I think).
 * (- Handle symbols not defined in symbTok function, might return Option
 *   exception.)
 * - Handle parsing of symbols like "__)" as [ID "__", RPAREN].
 *)

(*datatype exp = Id of string     | Num of int
             | Add of exp * exp | Sub of exp * exp
             | Mul of exp * exp*)

(* exception for catching internal errors in the lexer *)
exception InternalError

(* datatype of tokens for lexing *)
datatype token = ID of string       | NUM of int        | VAL 
               | EQUAL              | NEQUAL            | LPAREN
               | RPAREN             | DATATYPE          | TYPE
               | COMMA              | PIPE

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
val test_symb1 = scan "val ? = (hej, 43, __)" (* =
  [VAL, ID "?", EQUAL, LPAREN, ID "hej", COMMA, NUM 43, COMMA, ID "__",
  RPAREN]*)



(******************************************************************************)
(* Parser combinator :: The actual parser *)
(*exception Punt (* backtracking *)
exception ParserError (* failed to parse input stream *)

datatype id = Id of string

datatype typeDef = Enum of string

datatype decl = Datatype of string

(*datatype decl = Datatype of id * typeDef list*)

fun id (ID s :: toks) = (Id s, toks)
  | id toks           = raise Punt

fun dType (DATATYPE :: ID s :: EQUAL :: toks) = (Datatype s, toks)
  | dType toks                                = raise Punt

fun enumPipe (ID s :: PIPE :: toks) = (Enum s, toks)
  | enumPipe _                      = raise Punt

fun enum (ID s :: toks) = (Enum s, toks)
  | enum _              = raise Punt


(*fun dType (DATATYPE :: ID s :: EQUAL :: ID s1 :: rest) =
    (Datatype (Id s, [Enum s1]), rest)
  | dType _ = raise Punt*)

fun empty toks = ([], toks)

fun (ph1 ||| ph2) toks = ph1 toks handle Punt => ph2 toks

fun (ph1 --- ph2) toks =
  let val (x, toks2) = ph1 toks
      val (y, toks3) = ph2 toks2
  in ((x, y), toks3) end

fun (ph >>> f) toks =
  let val (x, toks2) = ph toks
  in (f x, toks2) end*)


(*
fun dType ph =
  case ph of
       (DATATYPE :: ID s :: EQUAL :: rest) => (Datatype (Id s, parseTypeDef rest))
   | _                                   => raise Punt

and parseTypeDef ls =
  case ls of
       (ID s :: PIPE :: rest) => (Enum s :: parseTypeDef rest, rest)
     | (ID s :: rest)         => ([Enum s], rest)

and parseTypeDef' ls =
  case ls of
       (ID s :: rest) => (Enum s, rest)*)

(*****************************************************************************)

exception SyntaxError of string

(* The parser combinators *)
(*infix 6 $-*)
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

datatype id = Id of string

datatype typeDef = Enum of string
                 | Tuple of string * string list

datatype decl = Datatype of id * typeDef list


fun dType (DATATYPE :: ID s :: EQUAL :: toks) = (Id s, toks)
  | dType _ = raise SyntaxError "Datatype declaration expected"


fun enum (ID s :: PIPE :: toks) = (Enum s, toks)
  | enum (ID s :: toks)         = (Enum s, toks)
  | enum _ = raise SyntaxError "enum datatype constructor expected"

fun 

fun makeDatatype (n, ls) = Datatype (n, ls)

fun decl toks =
  (   dType -- repeat enum  >> makeDatatype
  ) toks


(*fun idPipe (ID s :: PIPE :: toks) = (Id s, toks)
  | idPipe _ = raise SyntaxError "| Identifier expected"

fun id (ID s :: toks) = (Id s, toks)
  | id _ = raise SyntaxError "Identifier expected"*)
