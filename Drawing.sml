functor Drawing (structure P : PICTURE) =
struct

  open P

  type hpos = int
  type vpos = int

  datatype posTree = Node of string * hpos * vpos * posTree list

  (* Draw a node and, if applicaple, a line to the parent node, i.e. return a
   * list of O.t type items, from the external 'draw' library, representing the
   * items to be drawn. *)
  fun drawNode (s, x, y) parentPos =
    case parentPos of
         NONE => let val rect = O.rect (x-10, y-4) (x+10, y+4)
                     val text = O.text (x, y) s
                 in [rect, text] end
       | SOME (x1, y1) => let val rect = O.rect (x-10, y-4) (x+10, y+4)
                              val text = O.text (x, y) s
                              val line = O.line (x, y) (x1, y1)
                          in [rect, text, line] end

  (* Return O.t type items of given posTree, initial call has parentPos = NONE *)
  fun drawTree (Node (s, x, y, ls)) parentPos =
    drawNode (s, x, y) parentPos
    @
    List.concat (map (fn e => drawTree e (SOME (x, y))) ls)

  (* Main interface function. Draw a given postree and output to file. *)
  fun draw posTree file =
    let
      val pict = O.all (drawTree posTree NONE)
      val p = picture {unitlength = MM 1.0, dim = (100, 100)} pict
    in
      toFile A4 file p
    end

end

structure DrawingSvg :> DRAWING = Drawing (structure P = SvgPicture)

structure DrawingLatex :> DRAWING = Drawing (structure P = LatexPicture)

(* Testing:
open DrawingSvg

type hpos = int
type vpos = int

datatype posTree = Node of string * hpos * vpos * posTree list

val test = Node("a", 100, 30, [Node ("b", 60, 50,
                                 [Node ("b1", 40, 70, []),
                                  Node ("b2", 70, 70, [])]),
                               Node ("c", 100, 50,
                                 [Node ("c1", 100, 70, [])]),
                               Node ("d", 140, 50, [])])

val () = DrawingSvg.draw test "test.svg"*)
