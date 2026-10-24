-- apply/main.rg — BSD apply(1) command applicator in Pure Ridgun
-- @implements FR-PROC-001

import proc_lib

fn :: drop prog [String]    => [String]
      drop prog []          =  []
      drop prog (prog:rest) =  rest

fn :: emit apply lines [String] => ()
      emit apply lines []       =  ()
      emit apply lines lines    =  echo (join with lines (char from code 10))

fn :: finish apply ([String], Int) => ()
      finish apply (lines, rc)     =
        _ = emit apply lines lines
        exit if nonzero rc

fn :: exit if nonzero Int => ()
      exit if nonzero 0   =  ()
      exit if nonzero rc  =  sys exit rc

fn :: main => ()
      main =
        args = drop prog (cli args)
        spec = parse apply args args
        res = evaluate apply spec
        finish apply res
