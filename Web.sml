(*
 * MLDataVisualizer web interface (CGI)
 *)

val htmlTop = let val is = TextIO.openIn "htmlTop.html"
              in (TextIO.inputAll is before TextIO.closeIn is)
                 handle e => (TextIO.closeIn is; raise e)
              end

fun mid svgBool latexBool =
  "</textarea>\n\
  \<br /><br />\n\
  \<input type=\"checkbox\" id=\"svg\" name=\"svg\" value=\"svg\""
  ^ (if svgBool then " checked />\n" else " />\n") ^
  "<label for=\"svg\">SVG (scalable image)</label>\n\
  \<input type=\"checkbox\" id=\"latex\" name=\"latex\" value=\"latex\""
  ^ (if latexBool then " checked />\n" else " />\n") ^
  "<label for=\"latex\">LaTeX</label>\n\
  \<input type=\"submit\" value=\"Visualize!\" />\n\
  \</form>"

fun latexBox s = "<textarea cols=\"40\" rows=\"10\">" ^ s ^ "</textarea>"

val htmlBot = let val is = TextIO.openIn "htmlBot.html"
              in (TextIO.inputAll is before TextIO.closeIn is)
                 handle e => (TextIO.closeIn is; raise e)
              end

(* Full default page string with possible error message string s *)
val stdPage = fn s => htmlTop ^ mid true false ^ s ^ htmlBot

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
    then (let val procRes = (Processing.proc o List.last o Parser.parse o
                             Parser.scan) input
         in print (htmlTop ^ input);
            (if svgSel andalso latexSel
             then print (mid true true ^
                         latexBox (DrawingLatex.draw procRes) ^
                         DrawingSvg.draw procRes)
             else if latexSel
             then print (mid false true ^
                         latexBox (DrawingLatex.draw procRes))
             else print (mid true false ^
                         DrawingSvg.draw procRes));
            print htmlBot
         end) handle List.Empty => print (htmlTop ^ mid true false ^ htmlBot)
    else print (stdPage "")
  end

val _ = main ()
  handle CGI.Error s          => print (stdPage ("CGI error: " ^ s))
       | Parser.InternalError => print (stdPage "Parser internal error")
       | Parser.SyntaxError s => print (stdPage ("Parser syntax error: " ^ s))
       | Processing.Error s   => print (stdPage ("Processing error: " ^ s))
       | e                    => print (stdPage "Unknown exception thrown!")


