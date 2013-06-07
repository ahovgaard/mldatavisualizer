(*structure Processing :> PROCESSING =
struct*)

  exception ProcessingError of string

  datatype 'a tree = Node of 'a * ('a tree list)

  type extent = (int * int) list

  val minNodeSep = 30

  (** Processing of Parser.partree into string tree *)
  (*fun procVal dVal =
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
    end*)


  val testTree = Node ("a",
                  [Node ("b",
                    [Node ("c",
                      [Node ("f", [Node ("h", []), Node ("i", [])]),
                       Node ("g", [])]),
                     Node ("d", [])]),
                   Node ("e", [])])

  (** Tree positioning *)
  (* Displace a given tree horizontally by x' *)
  fun moveTree (Node ((label, x), subts), x') = Node ((label, x + x'), subts)

  (* Move an extent horizontally by a distance of x *)
  fun moveExtent (e, x) =  map (fn (p, q) => (p + x, q + x)) e

<<<<<<< HEAD
  fun mergelist es = List.foldl merge [] es
=======
  (* Merge two non-overlapping extents, filling in the gap between them, by
   * picking the leftmost position of the first and the rightmost position
   * the second *)
  fun merge ([], qs)                     = qs
    | merge (ps, [])                     = ps
    | merge ((p, _) :: ps, (_, q) :: qs) = (p, q) :: merge (ps, qs)
>>>>>>> 7dc8182a198fc820b93af87fad5c63e97196ce44

  (* Merge a list of extents *)
  fun mergeList es = foldl merge [] es

  (* Determine how close to each other two trees may be places, assuming a
   * minimum node separation of minNodeSep. Takes two extents and returns the
   * minimum posible distance. *)
  fun fit ((_,p) :: ps) ((q,_) :: qs) = Int.max (fit ps qs, p - q + minNodeSep)
    | fit _             _             = 0

  (* Calculate a list of positions for each subtree relative to the leftmost
   * subtree which has position tree. *)
  fun fitListl es = 
    let
      fun fitListl' acc [] = []
        | fitListl' acc (e :: es) =
        let val x = fit acc e
        in
          x :: fitListl' (merge (acc, moveExtent (e, x))) es
        end
    in
      fitListl' [] es
    end

  (* Flip/negate extent *)
  val flipExtent = map (fn (p, q) => (~q, ~p))

  (* Opposite of fitListl *)
  val fitListr = rev o map ~ o fitListl o map flipExtent o rev

  fun mean (x, y) = (x + y) div 2

  (* Symmetric combination of fitListl and fitListr *)
  fun fitList es = map mean (ListPair.zip (fitListl es, fitListr es))

  fun design tree =
    let
<<<<<<< HEAD
      val (trees, extents)
          = ListPair.unzip (map design' subtrees)
      val positions
          = fitlist extents
      val ptrees
          = map movetree (ListPair.zip (trees, positions))
      val pextents
          = map moveextent (ListPair.zip (extents, positions))
      val resultextent
          = (0, 0) :: mergelist pextents
      val resulttree
          = Node((i, 0), ptrees)
=======
      fun design' (Node (label, subtrees), i) =
        let
          val (trees, exts) = ListPair.unzip (map (fn t => design' (t, i + 1))
                                                  subtrees)
          val positions     = fitList exts
          val ptrees        = map moveTree (ListPair.zip (trees, positions))
          val pexts         = map moveExtent (ListPair.zip (exts, positions))
          val resultextent  = (i * minNodeSep, i * minNodeSep) :: mergeList pexts
          val resulttree    = Node ((label, i * minNodeSep), ptrees)
        in
          (resulttree, resultextent)
        end
>>>>>>> 7dc8182a198fc820b93af87fad5c63e97196ce44
    in
      #1 (design' (tree, 0))
    end

  (*fun position tree =
    let fun positionAux (Node ((s, x), ts), i) =
          Node ((s, x (*10 * (x + Int.abs(x)) + 100*), i),
                map (fn t => positionAux (t, i - 30)) ts)
    in positionAux (design tree, 0)
    end

  val proc = position o procVal   

end*)
