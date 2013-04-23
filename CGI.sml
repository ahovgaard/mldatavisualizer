structure CGI =
struct

  fun uriDecode' (#"%"::c1::c2::t) =
  let
      fun frmhex c =
          if #"0" <= c andalso c <= #"9" then
              Char.ord c - Char.ord #"0" else
          if #"a" <= c andalso c <= #"f" then
              Char.ord c - Char.ord #"a" + 10 else
          if #"A" <= c andalso c <= #"F" then
              Char.ord c - Char.ord #"A" + 10
          else Char.ord #"?"
  in
      Char.chr ((frmhex c1) * 16 + frmhex c2) :: uriDecode' t
  end
    | uriDecode' (#"+"::t) = #" " :: uriDecode' t
    | uriDecode' (h::t) = h :: uriDecode' t
    | uriDecode' [] = []
      
  fun uriDecode s = String.implode (uriDecode' (String.explode s))

  fun getParams () =
    case OS.Process.getEnv "QUERY_STRING" of
         NONE   => raise Fail "Malformed QUERY_STRING environment variable"
       | SOME s =>
           let val qs0 = String.fields (fn c => c = #"&") s
               val qs1 = map uriDecode qs0
           in qs1
           end

  val _ = app (fn s => print (s ^ "\n"))  (getParams())

end
