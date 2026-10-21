-- jot/main.rg — BSD jot(1) sequential/repetitive data generator
-- @implements FR-FILTER-001

import filter_lib

fn :: drop_prog [String]    => [String]
      drop_prog []          =  []
      drop_prog (prog:rest) =  rest

fn :: emit_jot JotSpec String => ()
      emit_jot spec    result =
        | j_no_newline spec = write_file "/dev/stdout" result
        | otherwise         = echo result

fn :: main => ()
      main =
        args = drop_prog cli_args
        spec = parse jot args args
        result = evaluate jot spec
        emit_jot spec result
