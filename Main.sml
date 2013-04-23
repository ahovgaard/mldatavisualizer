(*
 * MLDataVisualizer main file containing the main function that is 
 * called automatically on program execution.
 *)

exception InvalidArguments

fun parseArguments args =
  case args of
       [a1, a2] =>
       let
         val dType = ParserCombinator.parse (ParserCombinator.scan a1)
         val dVal  = ParserCombinator.parse (ParserCombinator.scan a2)
       in
         print "Datatype defintion:\n";
         PrettyPrinter.show dType;
         print "Datatype structure:\n";
         PrettyPrinter.show dVal
       end
     | _        => raise InvalidArguments

(*fun parseArguments args =
  case args of
       [arg] =>
         let
           val ptrees = ParserCombinator.parse (ParserCombinator.scan arg)
         in
           PrettyPrinter.show ptrees
         end
     | _                     => raise InvalidArguments*)

fun main () = parseArguments (CommandLine.arguments())
  handle InvalidArguments =>
           print "InvalidArguments: Do somthing smart here!\n"
       | ParserCombinator.SyntaxError str =>
           print ("Syntax error: " ^ str ^ "\n")

val _ = main ()
