-- nc/main.rg — BSD nc(1) TCP/Unix socket client, listener & port probe in Pure Ridgun
-- @implements FR-PROC-003

import proc_lib

fn :: drop prog [String] => [String]
      drop prog []       =  []
      drop prog (_:rest) =  rest

fn :: read stdin nonblocking String => String
      read stdin nonblocking acc    =
        read stdin after poll (sys poll 0 1 50) acc

fn :: read stdin after poll Int String => String
      read stdin after poll prc acc =
        | prc > 0   = read stdin chunk (posix read 0 65536) acc
        | otherwise = acc

fn :: read stdin chunk String String => String
      read stdin chunk ""     acc    =  acc
      read stdin chunk chunk  acc    =  read stdin nonblocking (acc + chunk)

fn :: select nc stdin NcSpec => String
      select nc stdin spec   =
        | nc_zero spec = ""
        | otherwise    = read stdin nonblocking ""

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
        payload = select nc stdin spec
        res = execute nc spec with payload
        finish nc res
