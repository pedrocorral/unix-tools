-- find/main.rg — BSD find(1) directory tree boolean evaluator in Pure Ridgun
-- @implements FR-FS-002

import fs_lib

fn :: drop_prog [String]    => [String]
      drop_prog []          =  []
      drop_prog (prog:rest) =  rest

fn :: emit_stdout String => ()
      emit_stdout ""     =  ()
      emit_stdout text   =
        | (str_char_at text ((str_len text) - 1)) == 10 = echo (str_slice text 0 ((str_len text) - 1))
        | otherwise                                     = write_file "/dev/stdout" text

fn :: main => ()
      main =
        args = drop_prog cli_args
        output = run find args
        emit_stdout output
