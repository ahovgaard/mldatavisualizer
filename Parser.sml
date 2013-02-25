structure Parser :> Parser =
struct

  fun parseTypeDef typeDef = print("Type definition: \n" ^ typeDef ^ "\n")

  fun parseStructure data = print("Data structure: \n" ^ data ^ "\n")

end
