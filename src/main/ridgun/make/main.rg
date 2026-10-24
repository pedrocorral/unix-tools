-- make/main.rg — BSD make(1) DAG dependency build tool in Pure Ridgun
-- @implements FR-PROC-001

import proc_lib

fn :: drop prog [String]    => [String]
      drop prog []          =  []
      drop prog (prog:rest) =  rest

fn :: emit make lines [String] => ()
      emit make lines []       =  ()
      emit make lines lines    =  echo (join with lines (char from code 10))

fn :: apply make chdir String => Int
      apply make chdir ""     =  0
      apply make chdir dir    =  sys chdir dir

fn :: finish make (Bool, [String]) Bool  => ()
      finish make (True, lines)    _     =  emit make lines lines
      finish make (False, lines)   True  =
        _ = emit make lines lines
        sys exit 1
      finish make (False, lines)   False =
        _ = emit make lines lines
        sys exit 2

fn :: main => ()
      main =
        args = drop prog (cli args)
        spec = parse make args args
        _ = apply make chdir (mk_dir spec)
        raw_mk = read file (mk_file spec)
        res = evaluate make raw_mk with spec
        finish make res (mk_question spec)
