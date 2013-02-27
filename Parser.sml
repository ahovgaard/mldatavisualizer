structure Parser :> Parser =
struct

  datatype typeName = TypeName of string

  datatype typeDef  = Enum of typeName
                    | Record of typeName * (string * string list)
                    | Tuple of typeName * string list
                    | Simple of typeName * string

  datatype dataType = Type of typeName * typeDef list

  fun parseTypeDef typeDef = print("Type definition: \n" ^ typeDef ^ "\n")

  fun parseStructure data = print("Data structure: \n" ^ data ^ "\n")

end
