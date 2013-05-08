(*structure Drawing :> DRAWING =
struct*)
  (*open SvgPicture*)
  (*open LatexPicture*)

  type hpos = int
  type vpos = int

  datatype posTree = Node of string * hpos * vpos * posTree list


  fun drawNode (s, x, y) parentPos =
    case parentPos of
         NONE => let val rect = SvgPicture.O.rect (x-10, y-4) (x+10, y+4)
                     val text = SvgPicture.O.text (x, y) s
                 in [rect, text] end
       | SOME (x1, y1) => let val rect = SvgPicture.O.rect (x-10, y-4) (x+10, y+4)
                              val text = SvgPicture.O.text (x, y) s
                              val line = SvgPicture.O.line (x, y) (x1, y1)
                          in [rect, text, line] end

  fun draw (Node (s, x, y, ls)) parentPos =
    drawNode (s, x, y) parentPos
    @
    List.concat (map (fn e => draw e (SOME (x, y))) ls)
    (*drawChildren ls (SOME (x, y))*)

  (*and drawChildren ls parentPos =
    List.concat (map (fn x => draw x parentPos) ls)*)

  val test = Node("a", 100, 30, [Node ("b", 60, 50,
                                   [Node ("b1", 40, 70, []),
                                    Node ("b2", 70, 70, [])]),
                                 Node ("c", 100, 50,
                                   [Node ("c1", 100, 70, [])]),
                                 Node ("d", 140, 50, [])])

  fun drawPict ls file =
    let val pict = SvgPicture.O.all ls
        val p = SvgPicture.picture
          {unitlength=SvgPicture.MM 1.0, dim=(100,100)} pict
    in SvgPicture.toFile SvgPicture.A4 file p
    end

  (*val () = drawPict (draw test NONE) "test.svg"*)

(*end*)
