structure Lexer :> LEXER =
struct

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

end
