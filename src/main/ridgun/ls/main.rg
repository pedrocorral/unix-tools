-- ls/main.rg — BSD ls(1) directory listing utility in Pure Ridgun
-- @implements FR-FS-001

import posix
import fs_lib

fn :: drop prog [String]    => [String]
      drop prog []          =  []
      drop prog (prog:rest) =  rest

fn :: emit stdout String => ()
      emit stdout ""     =  ()
      emit stdout text   =
        | (str char at text ((str len text) - 1)) == 10 = echo (str slice text 0 ((str len text) - 1))
        | otherwise                                     =
          _ = posix write 1 text
          ()

fn :: main => ()
      main =
        args = drop prog (cli args)
        output = run ls args
        emit stdout output
