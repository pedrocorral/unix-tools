-- apply/main.rg — BSD apply(1) command applicator in Pure Ridgun
-- @implements FR-PROC-001

import proc_lib

fn :: drop_prog [String]    => [String]
      drop_prog []          =  []
      drop_prog (prog:rest) =  rest

fn :: emit_apply_lines [String] => ()
      emit_apply_lines []       =  ()
      emit_apply_lines lines    =  echo (join_with lines (char_from_code 10))

fn :: finish_apply ([String], Int) => ()
      finish_apply (lines, rc)     =
        _ = emit_apply_lines lines
        exit_if_nonzero rc

fn :: exit_if_nonzero Int => ()
      exit_if_nonzero 0   =  ()
      exit_if_nonzero rc  =  sys_exit rc

fn :: main => ()
      main =
        args = drop_prog cli_args
        spec = parse apply args args
        res = evaluate apply spec
        finish_apply res
