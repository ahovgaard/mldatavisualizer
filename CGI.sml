structure CGI :> CGI =
struct

  exception CGI_Error of string

  local
    (* Convert hex char to int *)
    fun fromHex c =
      if #"0" <= c andalso c <= #"9"
      then Char.ord c - Char.ord #"0"
      else if #"a" <= c andalso c <= #"f"
      then Char.ord c - Char.ord #"a" + 10
      else if #"A" <= c andalso c <= #"F"
      then Char.ord c - Char.ord #"A" + 10
      else raise CGI_Error "Incorrect URL percent-encoding"

    (* Decode URL percent-encoding of char list *)
    fun urlDecodeAux ls =
      case ls of
           (#"%" :: c1 :: c2 :: cs) =>
             Char.chr ((fromHex c1) * 16 + fromHex c2) :: urlDecodeAux cs
         | (#"+" :: cs)             => #" " :: urlDecodeAux cs
         | (x :: cs)                => urlDecodeAux cs
         | []                       => []
  in
    (* Decode URL encoding of string s *)
    fun urlDecode s = implode (urlDecodeAux (explode s))
  end

  (* Convert [a, b, c, d] to [(a, b), (c, d)] *)
  fun listToPairs (l1 :: l2 :: ls) = ((l1, l2) :: listToPairs ls)
    | listToPairs (l  :: ls)       = raise CGI_Error "Malformed CGI call"
    | listToPairs []               = []

  (* Receive the value of the QUERY_STRING environment variable and return the
   * value of this as a list of (id * value) pairs. *)
  fun getParams () =
    case OS.Process.getEnv "QUERY_STRING" of
         NONE   => raise CGI_Error "Malformed QUERY_STRING environment variable"
       | SOME s =>
           let val qs0 = String.fields (fn c => c = #"&" orelse c = #"=") s
               val qs1 = listToPairs (map urlDecode qs0)
           in qs1 end

end
