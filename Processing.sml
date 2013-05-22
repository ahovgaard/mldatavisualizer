structure Processing :> PROCESSING =
struct

  exception ProcessingError of string

  datatype tree = Node of string * tree list

  fun procVal dVal =
    case dVal of
         Parser.Value (id, expr) => procExpr expr
       | _ => raise ProcessingError "Invalid value declaration"

  and procExpr expr =
    case expr of
         Parser.Int n             => Node (Int.toString n, [])
       | Parser.Real n            => Node (Real.toString n, [])
       | Parser.String n          => Node (n, [])
       | Parser.Char c            => Node (Char.toString c, [])
       | Parser.Tuple ls          => Node (procTuple ls)
       | Parser.NullaryCon s      => Node (s, [])
       | Parser.MultaryCon (s, e) => procExpr e
       | _                        => raise ProcessingError "error" (*TODO*)

  and procTuple exprs =
    let fun aux exprs (strs, exps) =
          case exprs of
               Parser.Int n    :: ls => aux ls (Int.toString n :: strs, exps)
             | Parser.Real n   :: ls => aux ls (Real.toString n :: strs, exps)
             | Parser.String s :: ls => aux ls (s :: strs, exps)
             | Parser.Char c   :: ls =>
                 aux ls ("#\"" ^ Char.toString c ^ "\"" :: strs, exps)
             | e :: ls => aux ls (strs, procExpr e :: exps)
             | [] => (String.concatWith ", " (rev strs), rev exps)
             (*| NullaryCon s :: ls      => aux ls (str, Node (s, []) :: exps)
             | MultaryCon (s, e) :: ls => aux ls (str, procExpr MultaryCon (s, e) :: exps)*)
    in
      aux exprs ([], [])
    end

  (* Testing: *)
  (*val test = (List.last o parse o scan)
    "datatype tree = Node of string * int * tree * tree | Null \
    \val a = Node(\"a\", 1, Node(\"b\", 2, Null, Null), Node(\"c\", 3, Null, Null))"

  val res = procVal test*)

end
