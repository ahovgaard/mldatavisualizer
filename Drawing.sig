signature DRAWING =
sig

  type hpos = int
  type vpos = int

  datatype posTree = Node of string * hpos * vpos * posTree list

  (* Draw a given posTree, positioned tree, and output to a specified file.
   * E.g.:
   * DrawingSvg.draw (Node ("a", 10, 20, [Node ("b", 20, 10, [])])) "test.svg"
   * or similar with DrawingLatex.draw for LaTeX output. *)
  val draw : posTree -> string -> unit

end
