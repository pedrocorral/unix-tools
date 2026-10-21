-- lam/main.rg — BSD lam(1) file laminator
-- @implements FR-FILTER-002

import posix
import filter_lib

fn :: drop prog [String]    => [String]
      drop prog []          =  []
      drop prog (prog:rest) =  rest

fn :: main => ()
      main =
        args = drop prog (cli args)
        parsed = parse lam args args
        result = laminate columns (fst parsed) with (snd parsed)
        echo result
