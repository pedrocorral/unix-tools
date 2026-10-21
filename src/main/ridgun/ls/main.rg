-- ls/main.rg — BSD ls(1) directory listing utility in Pure Ridgun
-- @implements FR-FS-001

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
        output = run ls args
        emit_stdout output
