signature PROCESSING =
sig

  datatype 'a Tree = Node of 'a * ('a Tree list)

  val proc: Parser.partree -> (string * int * int) Tree

end
