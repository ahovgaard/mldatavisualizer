fun inputNChars n = let val is = TextIO.openIn "input.sml"
                    in (TextIO.inputN(is, n) before TextIO.closeIn is)
                       handle e => (TextIO.closeIn is; raise e)
                    end
