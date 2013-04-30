exception ProcessingError

open Parser

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


fun partitionTuplesAux []    p1 p2 = (p1, p2)
  | partitionTuplesAux pairs p1 p2 =
  case pairs of
       (Int n, IntTyp) :: ts       => partitionTuplesAux ts (Int n :: p1) p2
     | (Real n, RealTyp) :: ts     => partitionTuplesAux ts (Real n :: p1) p2
     | (String s, StringTyp) :: ts => partitionTuplesAux ts (String s :: p1) p2
     (* TODO: | (Char c, CharTyp *)
     | (NullaryCon s1, NullaryTyCon s2) :: ts
         => if s1 = s2 then partitionTuplesAux ts p1 (NullaryCon s1 :: p2)
            else raise ProcessingError
     | (MultaryCon (s1, e), MultaryTyCon (s2, _)) :: ts
         => if s1 = s2 then partitionTuplesAux ts p1 (MultaryCon (s1, e) :: p2)
            else raise ProcessingError
     | _ => raise ProcessingError

fun partitionTuples tupVal tupTyp =
  let val pairs = ListPair.zip (tupVal, tupTyp)
  in partitionTuplesAux pairs [] []
  end

fun getContent exprs =
  let fun aux e = case e of
                       Int n    => Int.toString n
                     | Real n   => Real.toString n
                     | String s => s
                     | Char c   => Char.toString c
  in map (fn e => aux e ^ " ") exprs
  end

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
                if List.exists (fn e => e = MultaryTyCon (s, IntTyp))
                   (getCons dTyp)
                then Node (s ^ " " ^ Int.toString n, [])
                else raise ProcessingError
            | Real n =>
                if List.exists (fn e => e = MultaryTyCon (s, RealTyp))
                   (getCons dTyp)
                then Node (s ^ " " ^ Real.toString n, [])
                else raise ProcessingError
            | String s1 =>
                if List.exists (fn e => e = MultaryTyCon (s, StringTyp))
                   (getCons dTyp)
                then Node (s ^ " " ^ s1, [])
                else raise ProcessingError
            | Tuple es =>
                 if List.exists (fn e => e = MultaryTyCon (s, TupleTyp es1))
                    (getCons dTyp)
                 then let val (content, branches) = partitionTuples es es1
                      in Node (getContent content,
                               map (fn e => procExpr e dTyp) branches)
                      end
                 else raise ProcessingError)

             (*| NullaryCon s1 =>
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
     | MultaryTyCon (s, es) => Node(s, map (fn e => procExpr e dTyp) es)*)

         (*if List.exists (fn e => e = UnaryCon (s, t)) (getCons dTyp)
         then Node (s, [(*procTyp t*)])
         else raise ProcessingError*)

val dtScan0  = Parser.scan "datatype tree = UnaryNode of tree | Null"
val dtParse0 = hd (Parser.parse dtScan0)
val valScan0 =
  Parser.scan "val a = UnaryNode (UnaryNode (UnaryNode (Null)))"
val valParse0 = hd (Parser.parse valScan0)

val dt1  = (hd o Parser.parse o Parser.scan)
  "datatype tree = UnaryNode of tree | MultaryNode of int * tree | Null"
val val1 = (hd o Parser.parse o Parser.scan)
  "val a = UnaryNode (MultaryNode (5, Null))"
