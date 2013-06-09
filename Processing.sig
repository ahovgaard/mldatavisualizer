signature PROCESSING =
sig

  datatype 'a tree = Node of 'a * ('a tree list)

  (* Process a Parser.partree, as returned by Parser.parse, into a positioned
     tree of the polymorphic tree type, with each node containing a string of
     content, an int horizontal position and an int vertical position. *)
  val proc: Parser.partree -> (string * int) tree

end
