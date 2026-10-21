-- lam/main.rg — BSD lam(1) file laminator
-- @implements FR-FILTER-002

import filter_lib

fn :: drop_prog [String]    => [String]
      drop_prog []          =  []
      drop_prog (prog:rest) =  rest

fn :: main => ()
      main =
        args = drop_prog cli_args
        parsed = parse lam args args
        result = laminate columns (fst parsed) with (snd parsed)
        echo result
