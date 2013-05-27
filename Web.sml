(*
 * MLDataVisualizer web interface (CGI)
 *)

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

fun main () =
  let val cgiParams = CGI.getParams ()
      val (inputSet, input) =
        case List.find (fn (x, _) => x = "input") cgiParams of
             SOME (_, y) => (true, y)
           | NONE        => (false, "")
      val svgSel   = List.exists (fn (x, _) => x = "svg") cgiParams
      val latexSel = List.exists (fn (x, _) => x = "latex") cgiParams
  in
    if inputSet
    then let val procRes = (Processing.proc o List.last o Parser.parse o
                            Parser.scan) input
         in print (htmlTop ^ input ^ htmlMid);
            (if svgSel andalso latexSel
             then print (DrawingSvg.draw procRes ^ DrawingLatex.draw procRes)
             else if latexSel then print (DrawingLatex.draw procRes)
             else print (DrawingSvg.draw procRes));
            print htmlBot
         end
    else print (htmlTop ^ htmlMid ^ htmlBot)
  end

val _ = main ()
  handle CGI.Error s => print ("Error recieving input, error message is: " ^ s)
       | e           => print "Unknown exception thrown!" (* FIXME *)
