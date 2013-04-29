(* pt = partree *)
exception ProcessingError

open ParserCombinator

(* Goal datatype *)
datatype tree = Node of string * tree list
(* More general, which might be more neat:
   datatype 'a tree = Node of 'a * ('a tree list)*)

(*fun dtyp pt =
  case pt of
       MultaryCon(str, tys)*)

(*fun inDtype (tDef, dTyp) = List.exists (fn e => e = tDef) (List.last dTyp)

fun procDTyp dTyp =
  case dTyp of
       Datatype (s, tDefs) => procTypDef tDefs

and procTypDef tDef =
  case tDef of*)

fun getId (Datatype (s, _))      = s
fun getCons (Datatype (_, cons)) = cons

fun existsCon id tyDefs =
  case tyDefs of
       (NullaryTyCon id :: _)      => true
     | (MultaryTyCon (id, _) :: _) => true
     | _ :: ts                     => existsCon id ts
     | []                          => false


fun procVal pt dTyp =
  case pt of
       Value (id, expr) => procExpr expr dTyp

and procExpr exp dTyp =
  case exp of
       (* Simple expressions like "val a = 5" etc. *) 
       Int n             => Node (Int.toString n, [])
     | Real n            => Node (Real.toString n, [])
     | String s          => Node (s, [])
     | Char c            => Node (Char.toString c, [])
       (* Nullary type constructors defined in dTyp *)
     | NullaryCon s    =>
         if List.exists (fn e => e = NullaryTyCon s) (getCons dTyp)
         then Node (s, [])
         else raise ProcessingError
       (* Unary type constructors defined in dTyp *)
     | MultaryCon (s, e) =>
         (case e of
              Int n =>
                if List.exists (fn e => e = UnaryTyCon (s, IntTyp))
                   (getCons dTyp)
                then Node (s ^ " " ^ Int.toString n, [])
                else raise ProcessingError
            | Real n =>
                if List.exists (fn e => e = UnaryTyCon (s, RealTyp))
                   (getCons dTyp)
                then Node (s ^ " " ^ Real.toString n, [])
                else raise ProcessingError
            | String s1 =>
                if List.exists (fn e => e = UnaryTyCon (s, StringTyp))
                   (getCons dTyp)
                then Node (s ^ " " ^ s1, [])
                else raise ProcessingError
             | Tuple es =>

             | NullaryCon s1 =>
                 let val id = getId dTyp
                 in if List.exists (fn e => e = UnaryTyCon (s, Tyvar id))
                       (getCons dTyp)
                       andalso List.exists (fn e => e = NullaryTyCon s1)
                       (getCons dTyp)
                    (*then Node (s ^ " " ^ s1, [])*)
                    then Node (s, [Node (s1, [])])
                    else raise ProcessingError
                 end
             | UnaryCon (s1, e1) =>
                 let val id = getId dTyp
                 in if List.exists (fn e => e = UnaryTyCon (s, Tyvar id))
                      (getCons dTyp)
                      andalso existsCon s1 (getCons dTyp)
                      (*andalso List.exists (fn e => e = UnaryCon (s1, e2))
                      (getCons dTyp)*)
                    then Node (s, [procExpr e1 dTyp])
                    else raise ProcessingError
                 end
             | MultaryCon (s1, es) =>
                 let val id = getId dTyp
                 in if List.exists (fn e => e = UnaryCon (s, Tyvar id))
                       (getCons dTyp)
                       andalso existsCon s1 (getCons dTyp)
                    then Node (s, map (fn x => procExpr x dTyp) es)
                    else raise ProcessingError
                 end)
       (* Multary type constructors defined in dTyp, not even close yet... *)
     | MultaryTyCon (s, es) => Node(s, map (fn e => procExpr e dTyp) es)

         (*if List.exists (fn e => e = UnaryCon (s, t)) (getCons dTyp)
         then Node (s, [(*procTyp t*)])
         else raise ProcessingError*)

val dtScan0  = ParserCombinator.scan "datatype tree = UnaryNode of tree | Null"
val dtParse0 = hd (ParserCombinator.parse dtScan0)
val valScan0 =
  ParserCombinator.scan "val a = UnaryNode (UnaryNode (UnaryNode (Null)))"
val valParse0 = hd (ParserCombinator.parse valScan0)

val dt1  = (hd o ParserCombinator.parse o ParserCombinator.scan)
  "datatype tree = UnaryNode of tree | MultaryNode of int * tree | Null"
val val1 = (hd o ParserCombinator.parse o ParserCombinator.scan)
  "val a = UnaryNode (MultaryNode (5, Null))"
