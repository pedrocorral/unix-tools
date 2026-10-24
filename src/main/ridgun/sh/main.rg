-- sh/main.rg — BSD sh(1) Almquist-style command interpreter in Pure Ridgun
-- @implements FR-PROC-002

import proc_lib

fn :: drop prog [String]    => [String]
      drop prog []          =  []
      drop prog (prog:rest) =  rest

fn :: read sh stdin chunk String String => String
      read sh stdin chunk acc "" = acc
      read sh stdin chunk acc chunk = read sh stdin loop (acc + chunk)

fn :: read sh stdin loop String => String
      read sh stdin loop acc    =
        read sh stdin chunk acc (posix read 0 65536)

fn :: extract sh invocation [String]           => String [String]
      extract sh invocation []                 =  (read sh stdin loop "", [])
      extract sh invocation ("-c":[])          =  ("", [])
      extract sh invocation ("-c":cmd_str:rest) = (cmd_str, rest)
      extract sh invocation (script_path:rest) =  (read file script_path, rest)

fn :: emit stdout String => ()
      emit stdout ""     =  ()
      emit stdout text   =
        | (str char at text ((str len text) - 1)) == 10 = echo (str slice text 0 ((str len text) - 1))
        | otherwise                                     =
          _ = posix write 1 text
          ()

fn :: finish sh (String, Int)  => ()
      finish sh (text, status) =
        _ = emit stdout text
        exit sh status

fn :: exit sh Int  => ()
      exit sh 0    =  ()
      exit sh code =  sys exit code

fn :: main => ()
      main =
        args = drop prog (cli args)
        inv = extract sh invocation args
        src = fst inv
        pos = snd inv
        res = run shell script src with pos
        finish sh res
