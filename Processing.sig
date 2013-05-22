signature PROCESSING =
sig

  datatype tree = Node of string * tree list

  val procVal : Parser.partree -> tree

end
