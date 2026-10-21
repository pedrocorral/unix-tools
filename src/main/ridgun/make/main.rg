-- make/main.rg — BSD make(1) DAG dependency build tool in Pure Ridgun
-- @implements FR-PROC-001

import proc_lib

fn :: drop prog [String]    => [String]
      drop prog []          =  []
      drop prog (prog:rest) =  rest

fn :: emit make lines [String] => ()
      emit make lines []       =  ()
      emit make lines lines    =  echo (join with lines (char from code 10))

fn :: finish make (Bool, [String]) => ()
      finish make (True, lines)    =  emit make lines lines
      finish make (False, lines)   =
        _ = emit make lines lines
        sys exit 2

fn :: main => ()
      main =
        args = drop prog (cli args)
        spec = parse make args args
        raw_mk = read file (mk_file spec)
        res = evaluate make raw_mk with spec
        finish make res
