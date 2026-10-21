-- timeout/main.rg — BSD timeout(1) command execution deadline utility in Pure Ridgun
-- @implements FR-PROC-003

import io
import socket
import proc_lib

fn :: drop prog [String] => [String]
      drop prog []       =  []
      drop prog (_:rest) =  rest

fn :: emit stdout String => ()
      emit stdout ""     =  ()
      emit stdout text   =
        | (str char at text ((str len text) - 1)) == 10 = echo (str slice text 0 ((str len text) - 1))
        | otherwise                                     =
          _ = posix write 1 text
          ()

fn :: exit timeout Int  => ()
      exit timeout 0    =  ()
      exit timeout code =  sys exit code

fn :: main => ()
      main =
        args = drop prog (cli args)
        spec = parse timeout args args
        res = execute timeout spec
        _ = emit stdout (fst res)
        exit timeout (snd res)
