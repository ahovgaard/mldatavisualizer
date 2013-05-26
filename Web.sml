(*
 * MLDataVisualizer CGI web interface
 *)

(*exception InvalidArguments*)

val htmlTop = let val is = TextIO.openIn "htmlTop.html"
              in (TextIO.inputAll is before TextIO.closeIn is)
                 handle e => (TextIO.closeIn is; raise e)
              end

val htmlMid = let val is = TextIO.openIn "htmlMid.html"
              in (TextIO.inputAll is before TextIO.closeIn is)
                 handle e => (TextIO.closeIn is; raise e)
              end

val htmlBot = let val is = TextIO.openIn "htmlBot.html"
              in (TextIO.inputAll is before TextIO.closeIn is)
                 handle e => (TextIO.closeIn is; raise e)
              end

(*val svgInline0 = "<svg xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\">"
val svgInline1 = "</svg>"*)

fun main () = 
  case CGI.getParam "input" of
       SOME s => let val res = (DrawingSvg.draw o Processing.proc o List.last o
                                Parser.parse o Parser.scan) s
                 in print (htmlTop ^ s ^ htmlMid ^ res ^ htmlBot)
                 end
     | NONE   => print (htmlTop ^ htmlMid ^ htmlBot)

val _ = main ()
  handle Empty => print "Exception 'Empty' thrown"
       | e     => print "Unknown exception thrown!"
