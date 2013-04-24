(*
 * MLDataVisualizer main file containing the main function that is 
 * called automatically on program execution.
 *)

exception InvalidArguments

fun cgiArgs args =
  case args of
       (("dtype", arg) :: args1) =>
       let val dType = ParserCombinator.parse (ParserCombinator.scan arg)
       in print "Datatype definition:\n";
          PrettyPrinter.show dType;
          cgiArgs args1
       end
     | (("dval", arg) :: args1) =>
       let val dVal = ParserCombinator.parse (ParserCombinator.scan arg)
       in print "Datatype structure:\n";
          PrettyPrinter.show dVal;
          cgiArgs args1
       end
     | _                   => ()

fun parseArguments args =
  cgiArgs (CGI.getParams()) handle CGI.CGI_Error _ =>
  (case args of
       [a1, a2] =>
       let val dType = ParserCombinator.parse (ParserCombinator.scan a1)
           val dVal  = ParserCombinator.parse (ParserCombinator.scan a2)
       in
         print "Datatype defintion:\n";
         PrettyPrinter.show dType;
         print "Datatype structure:\n";
         PrettyPrinter.show dVal
       end
     | _        => raise InvalidArguments)

fun main () = parseArguments (CommandLine.arguments())
  handle InvalidArguments =>
           print "InvalidArguments: Do somthing smart here!\n"
       | ParserCombinator.SyntaxError str =>
           print ("Syntax error: " ^ str ^ "\n")

val _ = main () handle _ => print "Unknown exception thrown!"
