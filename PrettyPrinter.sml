signature PRETTY_PRINTER =
sig

  val show: ParserCombinator.partree list -> unit

end

structure PrettyPrinter :> PRETTY_PRINTER =
struct

  open ParserCombinator

  fun showPartree tree =
    case tree of
         Value (str, exp)       => "val " ^ str ^ " = " ^ showExpr exp ^ "\n"
       | Datatype (str, tydefs) => "datatype " ^ str ^ " = " ^ 
           foldr op^ "" (map (fn tydef => showTydef tydef ^ " | ") tydefs)
           ^ "\n"

  and showExpr tree =
    case tree of
         Int n     => Int.toString n
       | Real n    => Real.toString n
       | String s  => "\"" ^ s ^ "\""
       | Char c    => "#\"" ^ Char.toString c ^ "\""
       | Tuple es  =>
           "(" ^ foldr op^ "" (map (fn x => showExpr x ^ ", ") es) ^ ")"
       | List es   =>
           "[" ^ foldr op^ "" (map (fn x => showExpr x ^ ", ") es) ^ "]"
       | Record es =>
           "{" ^ foldr op^ "" (map (fn (n,e) => n ^ " = " ^ showExpr e) es) ^ "}"

  and showTydef tree =
    case tree of
         NullaryCon s       => s
       | UnaryCon (s, t)    => s ^ " of " ^ showTyp t
       | MultaryCon (s, ts) =>
           s ^ " of " ^ foldr op^ ""(map (fn t => showTyp t ^ " * ") ts)

  and showTyp tree =
    case tree of
         IntTyp  => "int"
       | RealTyp => "real"
       | Tyvar s => s

  fun show ptree = app print (map showPartree ptree)

end
