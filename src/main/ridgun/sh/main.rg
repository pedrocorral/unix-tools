-- sh/main.rg — BSD sh(1) Almquist-style command interpreter in Pure Ridgun
-- @implements FR-PROC-002

import proc_lib

fn :: drop_prog [String]    => [String]
      drop_prog []          =  []
      drop_prog (prog:rest) =  rest

fn :: extract_sh_invocation [String]           => (String, [String])
      extract_sh_invocation []                 =  (read_file "/dev/stdin", [])
      extract_sh_invocation ("-c":[])          =  ("", [])
      extract_sh_invocation ("-c":cmd_str:rest) = (cmd_str, rest)
      extract_sh_invocation (script_path:rest) =  (read_file script_path, rest)

fn :: emit_stdout String => ()
      emit_stdout ""     =  ()
      emit_stdout text   =
        | (str_char_at text ((str_len text) - 1)) == 10 = echo (str_slice text 0 ((str_len text) - 1))
        | otherwise                                     = write_file "/dev/stdout" text

fn :: finish_sh (String, Int)  => ()
      finish_sh (text, status) =
        _ = emit_stdout text
        exit_sh status

fn :: exit_sh Int  => ()
      exit_sh 0    =  ()
      exit_sh code =  sys_exit code

fn :: main => ()
      main =
        args = drop_prog cli_args
        inv = extract_sh_invocation args
        src = fst inv
        pos = snd inv
        res = run shell script src with pos
        finish_sh res
