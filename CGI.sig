signature CGI =
sig

  (* Return CGI parameters as a list of (id * value) pairs. *)
  val getParams : unit -> (string * string) list

end
