-- nc/main.rg — BSD nc(1) TCP/Unix socket client, listener & port probe in Pure Ridgun
-- @implements FR-PROC-003

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

fn :: finish nc (Int, String)  => ()
      finish nc (0, out)       =  emit stdout out
      finish nc (rc, err_text) =
        _ = posix write 2 err_text
        sys exit rc

fn :: main => ()
      main =
        args = drop prog (cli args)
        spec = parse nc args args
        res = execute nc spec with ""
        finish nc res
