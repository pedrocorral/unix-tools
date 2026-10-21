-- rs/main.rg — BSD rs(1) 2D data array reshaper
-- @implements FR-FILTER-001

import filter_lib

fn :: drop prog [String]    => [String]
      drop prog []          =  []
      drop prog (prog:rest) =  rest

fn :: read rs files [String] => String
      read rs files []       =  read file "/dev/stdin"
      read rs files (f:[])   =  read file f
      read rs files (f:rest) =  (read file f) + " " + (read rs files rest)

fn :: main => ()
      main =
        args = drop prog (cli args)
        spec = parse rs args args
        raw = read rs files (r_files spec)
        result = reshape text raw with spec
        echo result
