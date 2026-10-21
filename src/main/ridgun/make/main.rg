-- make/main.rg — BSD make(1) DAG dependency build tool in Pure Ridgun
-- @implements FR-PROC-001

import proc_lib

fn :: drop_prog [String]    => [String]
      drop_prog []          =  []
      drop_prog (prog:rest) =  rest

fn :: emit_make_lines [String] => ()
      emit_make_lines []       =  ()
      emit_make_lines lines    =  echo (join_with lines (char_from_code 10))

fn :: finish_make (Bool, [String]) => ()
      finish_make (True, lines)    =  emit_make_lines lines
      finish_make (False, lines)   =
        _ = emit_make_lines lines
        sys_exit 2

fn :: main => ()
      main =
        args = drop_prog cli_args
        spec = parse make args args
        raw_mk = read_file (mk_file spec)
        res = evaluate make raw_mk with spec
        finish_make res
