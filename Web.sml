(*
 * MLDataVisualizer CGI web interface
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
     | _                        => ()

val head = "<!DOCTYPE html><html lang=\"en\"><head><meta charset=\"utf-8\">"
         ^ "<meta name=\"description\" content=\"Visualization of Standard"
         ^ " ML data structures\"><title>MLDataVisualizer :: Visualization "
         ^ "of Standard ML data structures</title><style>label { margin: 1em"
         ^ " 2em 1em 0.5em; }</style></head>"

val bodyTop =
    "<body><h1>MLDataVisualizer</h1><p>Datatype definition:</p> <form"
  ^ " method=\"GET\" action=\"mldv\"><textarea rows=\"10\" "
  ^ "cols=\"80\" name=\"dtype\" placeholder=\"datatype exampleTree = "
  ^ "Node of exampleTree * int * exampleTree | Null\">"

val bodyMid =
  "</textarea><p>Data structure:</p><textarea rows=\"10\" cols=\"80\" "
  ^ "name=\"dval\" placeholder=\"val tree = Node(Null, 42, Node(Null,"
  ^ " 12, Null))\">"

val bodyBot =
  "</textarea><br /><br /><input type=\"checkbox\" "
  ^ "id=\"svg\" name=\"svg\" value=\"svg\"><label for=\"svg\">SVG "
  ^ "(scalable image)</label><input type=\"checkbox\" id=\"latex\" "
  ^ "name=\"latex\" value=\"latex\"><label for=\"latex\">LaTeX</label>"
  ^ "<input type=\"submit\" value=\"Visualize!\" /></form>"

val bottom = "</body></html>"

fun main () =
  let val dtyp = case CGI.getParam "dtype" of
                      NONE   => ""
                    | SOME s => s
      val dval = case CGI.getParam "dval" of
                      NONE   => ""
                    | SOME s => s
  in
    print (head ^ bodyTop ^ dtyp ^ bodyMid ^ dval ^ bodyBot);
    (cgiArgs (CGI.getParams())
    handle ParserCombinator.SyntaxError s => print ("Syntax error: " ^ s ^ "\n")
         | CGI.CGI_Error _                => ());
    print bottom
  end

(*fun main () =
  print head;
  print body;
  (cgiArgs (CGI.getParams())
  handle ParserCombinator.SyntaxError s => print ("Syntax error: " ^ s ^ "\n")
       | CGI.CGI_Error _                => ());
  print bottom;*)

val _ = main ()
