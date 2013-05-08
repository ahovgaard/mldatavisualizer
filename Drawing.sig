signature DRAWING = sig

  type hpos = int
  type vpos = int

  datatype posTree = Node of string * hpos * vpos * posTree list

end
