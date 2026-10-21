-- rs/main.rg — BSD rs(1) 2D data array reshaper
-- @implements FR-FILTER-001

import filter_lib

fn :: drop_prog [String]    => [String]
      drop_prog []          =  []
      drop_prog (prog:rest) =  rest

fn :: read_rs_files [String] => String
      read_rs_files []       =  read_file "/dev/stdin"
      read_rs_files (f:[])   =  read_file f
      read_rs_files (f:rest) =  (read_file f) + " " + (read_rs_files rest)

fn :: main => ()
      main =
        args = drop_prog cli_args
        spec = parse rs args args
        raw = read_rs_files (r_files spec)
        result = reshape text raw with spec
        echo result
