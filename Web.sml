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

val htmlTop = let val is = TextIO.openIn "htmlTop.html"
              in (TextIO.inputAll is before TextIO.closeIn is)
                 handle e => (TextIO.closeIn is; raise e)
              end

val htmlMid = let val is = TextIO.openIn "htmlMid.html"
              in (TextIO.inputAll is before TextIO.closeIn is)
                 handle e => (TextIO.closeIn is; raise e)
              end

val htmlMidBot = let val is = TextIO.openIn "htmlMidBot.html"
                 in (TextIO.inputAll is before TextIO.closeIn is)
                    handle e => (TextIO.closeIn is; raise e)
                 end

val htmlBot = let val is = TextIO.openIn "htmlBot.html"
              in (TextIO.inputAll is before TextIO.closeIn is)
                 handle e => (TextIO.closeIn is; raise e)
              end

fun main () =
  let val dtyp = case CGI.getParam "dtype" of
                      NONE   => ""
                    | SOME s => s
      val dval = case CGI.getParam "dval" of
                      NONE   => ""
                    | SOME s => s
  in
    print (htmlTop ^ dtyp ^ htmlMid ^ dval ^ htmlMidBot);
    (cgiArgs (CGI.getParams())
    handle ParserCombinator.SyntaxError s => print ("Syntax error: " ^ s ^ "\n")
         | CGI.CGI_Error _                => ());
    print htmlBot
  end

(*fun main () =
  print head;
  print body;
  (cgiArgs (CGI.getParams())
  handle ParserCombinator.SyntaxError s => print ("Syntax error: " ^ s ^ "\n")
       | CGI.CGI_Error _                => ());
  print bottom;*)

val _ = main () handle e => print "Unknown exception thrown!"
