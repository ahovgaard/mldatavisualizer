(*
 * MLDataVisualizer main file containing the main function that is 
 * called automatically on program execution.
 *)

(* basis library modules *)
open CommandLine

(* project modules *)
open Parser

exception InvalidArguments

fun parseArguments args =
  case args of
       ("-t" :: arg :: args) => Parser.parseTypeDef arg before
                                parseArguments args
     | ("-s" :: arg :: args) => Parser.parseStructure arg before
                                parseArguments args
     | []                    => ()
     | _                     => raise InvalidArguments

fun main () = parseArguments (CommandLine.arguments())
  handle InvalidArguments => print "InvalidArguments: Do somthing smart here!\n"

val _ = main ()