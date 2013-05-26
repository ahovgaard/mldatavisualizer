type hpos = int
type vpos = int

datatype 'a Tree = Node of 'a * ('a Tree list)
(*datatype posTree = Node of vpos * posTree list*)

type Extent = (hpos*vpos) list

fun movetree (Node((label, x), subtrees), x' : int) =
       Node((label, x+x'), subtrees)

fun moveextent (e : Extent, x) =  map (fn (p, q) => (p+x, q+x)) e

fun merge ([], qs) = qs
  | merge (ps, []) = ps
  | merge ((p, _) :: ps, (_, q) :: qs) = (p, q) :: merge (ps, qs)

fun mergelist es = List.foldl merge es []

fun fit ((_, p) :: ps) ((q, _) :: qs) = Int.max(fit ps qs, p - q + 10)
  | fit _              _              = 0

fun fitlistl es = 
let
  fun fitlistl' acc [] = []
    | fitlistl' acc (e :: es) =
              let val x = fit acc e
              in
                x :: fitlistl' (merge (acc, moveextent (e, x))) es
              end
in
  fitlistl' [] es
end

fun fitlistr es =
let
  fun fitlistr' acc [] = []
    | fitlistr' acc (e::es) =
    let val x = ~(fit e acc)
    in
      x :: fitlistr' (merge (moveextent (e, x), acc)) es
    end
in
  rev (fitlistr' [] (rev es))
end

val flipextent : Extent -> Extent = map (fn (p, q) => (~q, ~p))

val fitlistr = rev o map ~ o fitlistl o map flipextent o rev

fun mean (x, y) = (x+y) div 2

fun fitlist es = map mean (ListPair.zip (fitlistl es, fitlistr es))

fun design tree =
let
  fun design' (Node(i, subtrees)) =
  let
    val (trees, extents)
        = ListPair.unzip (map design' subtrees)
    val positions
        = fitlist extents
    val ptrees
        = map movetree (ListPair.zip (trees, positions))
    val pextents
        = List.concat(map moveextent (ListPair.zip (extents, positions)))
    val resultextent
        = (0, 0) :: mergelist pextents
    val resulttree
        = Node((i, 0), ptrees)
  in
    (resulttree, resultextent)
  end
in
  #1 (design' tree)
end

fun position tree =
  let fun positionAux (Node ((s, x), ts), i) =
        Node ((s, x+Int.abs(x), i), map (fn t => positionAux (t, i+5)) ts)
  in positionAux (tree, 0)
  end
