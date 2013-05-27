signature CGI =
sig

  exception Error of string

  (* Return CGI parameters as a list of (id * value) pairs. *)
  val getParams : unit -> (string * string) list

end
