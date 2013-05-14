(* Useful syntactic sugar for function composition, as in Haskell *)
infixr 0 $
fun f $ x = f x

open Parser

exception ProcessingError of string

(* Goal datatype *)
datatype tree = Node of string * tree list


fun procVal dVal =
  case dVal of
       Value (id, expr) => procExpr expr
     | _                => raise ProcessingError "Invalid value declaration"

and procExpr expr =
  case expr of
       Int n             => Node (Int.toString n, [])
     | Real n            => Node (Real.toString n, [])
     | String n          => Node (n, [])
     | Char c            => Node (Char.toString c, [])
     | Tuple ls          => Node $ procTuple ls
     | NullaryCon s      => Node (s, [])
     | MultaryCon (s, e) => procExpr e
     | _                 => raise ProcessingError "error" (*TODO*)

and procTuple exprs =
  let fun aux exprs (strs, exps) =
        case exprs of
             Int n    :: ls => aux ls (Int.toString n :: strs, exps)
           | Real n   :: ls => aux ls (Real.toString n :: strs, exps)
           | String s :: ls => aux ls (s :: strs, exps)
           | Char c   :: ls => aux ls ("#\"" ^ Char.toString c ^ "\"" :: strs, exps)
           | e        :: ls => aux ls (strs, procExpr e :: exps)
           | []             => (String.concatWith ", " $ rev strs, rev exps)
           (*| NullaryCon s :: ls      => aux ls (str, Node (s, []) :: exps)
           | MultaryCon (s, e) :: ls => aux ls (str, procExpr MultaryCon (s, e) :: exps)*)
  in
    aux exprs ([], [])
  end

(* Testing: *)
val test = List.last $ parse $ scan
  "datatype tree = Node of string * int * tree * tree | Null \
  \val a = Node(\"a\", 1, Node(\"b\", 2, Null, Null), Node(\"c\", 3, Null, Null))"

val res = procVal test
