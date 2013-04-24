signature CGI =
sig

  exception CGI_Error of string

  (* Return CGI parameters as a list of (id * value) pairs. *)
  val getParams : unit -> (string * string) list

end
