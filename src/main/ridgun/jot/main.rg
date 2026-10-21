-- jot/main.rg — BSD jot(1) sequential/repetitive data generator
-- @implements FR-FILTER-001

import posix
import filter_lib

fn :: drop prog [String]    => [String]
      drop prog []          =  []
      drop prog (prog:rest) =  rest

fn :: emit jot JotSpec String => ()
      emit jot spec    result =
        | j_no_newline spec =
          _ = posix write 1 result
          ()
        | otherwise         = echo result

fn :: main => ()
      main =
        args = drop prog (cli args)
        spec = parse jot args args
        result = evaluate jot spec
        emit jot spec result
