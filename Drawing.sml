functor Drawing (structure P : PICTURE) =
struct

  open P

  open Processing

  val h = 4
  val w = 14
  val sep = 30

  (* Draw a node and, if applicaple, a line to the parent node, i.e. return a
   * list of O.t type items, from the external 'draw' library, representing the
   * items to be drawn. *)
  fun drawNode (s, x, y) parentPos =
    case parentPos of
         NONE => let val rect = O.rect (x - w, y - h) (x + w, y + h)
                     val text = O.text (x, y) s
                 in [rect, text] end
       | SOME (x1, y1) => let val rect = O.rect (x - w, y - h) (x + w, y + h)
                              val text = O.text (x, y)
                                (if size s >= 10
                                 then (String.substring (s, 0, 10)) ^ " .."
                                 else s)
                              val line = O.line (x, y + h) (x1, y1 - h)
                          in [rect, text, line] end

  (* Return O.t type items of given posTree, initial call has parentPos = NONE *)
  fun drawTree' (Node ((s, x), ls), curX, y, parentPos) =
    drawNode (s, curX + x, y) parentPos
    @
    List.concat (map (fn e => drawTree' (e, curX + x, y - sep, SOME (x, y))) ls)

  fun drawTree tree = drawTree' (tree, 0, 0, NONE)

  (* Draw a given postree and output to file. *)
  fun drawFile posTree file =
    let
      val pict = O.all (drawTree posTree)
      val p = picture {unitlength = MM 1.0, dim = (100, 100)} pict
    in
      toFile A4 file p
    end

  (* Main interface function. Draw a given postree and ouput as a string. *)
  fun draw posTree =
    let
      val pict = O.all (drawTree posTree)
      val p = picture {unitlength = MM 1.0, dim = (100, 100)} pict
    in
      toString A4 p
    end

end

structure DrawingSvg   :> DRAWING = Drawing (structure P = SvgPicture)
structure DrawingLatex :> DRAWING = Drawing (structure P = LatexPicture)


