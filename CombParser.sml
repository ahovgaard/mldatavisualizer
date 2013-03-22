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
exception Punt (* backtracking *)
exception ParserError (* failed to parse input stream *)

datatype id = Id of string

datatype typeDef = Enum of string

datatype decl = Datatype of id * typeDef list

(* The parser combinators *)
infix 6 $--
infix 5 ---
infix 3 >>>
infix 0 |||


fun dType ph =
  case ph of
       (DATATYPE :: ID s :: EQUAL :: rest) => (Datatype (Id s, parseTypeDef rest))
     | _                                   => raise Punt

and parseTypeDef ls =
  case ls of
       (ID s :: PIPE :: rest) => (Enum s :: parseTypeDef rest, rest)
     | (ID s :: rest)         => ([Enum s], rest)



(*datatype partree = Val of string * partree | Num of int | Id of string*)

(*
fun (ph1 ||| ph2) toks = ph1 toks handle Punt => ph2 toks

fun (ph1 --- ph2) toks =
  let val (a, toks2) = ph1 toks
      val (b, toks3) = ph2 toks2
  in ((a,b), toks3) end

fun num ph =
  case ph of
       (NUM n :: rest) => (Num n, rest)
     | _               => raise Punt

fun id ph =
  case ph of
       (ID s :: rest) => (Id s, rest)
     | _              => raise Punt
     *)
(* Checks that there are no excess data when the parse finishes and handles
   top-level punts, raising ParserError exception. *)
fun parse_list parser stream =
  (case parser stream of
        (result, []) => result
      | _            => raise ParserError) handle Punt => raise ParserError


(*
(* The 'or'-combinator: ph1 ||| ph2 will first try to parse with ph1. If ph1
   succedes, this result is returned, else it tries ph2. *)
fun ph1 ||| ph2 =
  fn stream => ph1 stream
    handle Punt => ph2 stream

(* The 'and'-combinator: ph1 -- ph2 first parses with ph1 and then with ph2 on
   the stream remining from ph1. It returns the pair of the result. *)
fun ph1 -- ph2 = (fn stream =>
  let val (a, stream1) = ph1 stream
      val (b, stream2) = ph2 stream1
  in
    ((a, b), stream2)
  end)

(* The application operator: ph >>> f first uses ph to parse the stream into 
   (x, rest), but the final result will be (f x, rest). *)
fun ph >>> f = fn stream =>
  let val (x, stream) = ph stream
  in (f x, rest) end
*)
