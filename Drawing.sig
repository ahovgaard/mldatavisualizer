signature DRAWING =
sig

  (* Draw a given posTree, positioned tree, and output to a specified file.
<<<<<<< HEAD
* E.g.:
* DrawingSvg.draw (Node ("a", 10, 20, [Node ("b", 20, 10, [])])) "test.svg"
* or similar with DrawingLatex.draw for LaTeX output. *)
  val drawFile : (string * int) Processing.tree -> string -> unit
=======
   * E.g.:
   * DrawingSvg.draw (Node ("a", 10, 20, [Node ("b", 20, 10, [])])) "test.svg"
   * or similar with DrawingLatex.draw for LaTeX output. *)
  val drawFile : (string * int * int) Processing.tree -> string -> unit
>>>>>>> parent of c0d2356... Merge branch 'master' of https://github.com/ahovgaard/mldatavisualizer

  (* Draw a given positioned tree and output as a string. *)
  val draw : (string * int * int) Processing.tree -> string

end
