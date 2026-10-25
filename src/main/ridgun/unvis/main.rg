-- unvis/main.rg — BSD unvis(1) visual character decoder
-- @implements FR-FILTER-002

import filter_lib

fn :: drop prog [String]    => [String]
      drop prog []          =  []
      drop prog (prog:rest) =  rest

fn :: read unvis files [String] => String
      read unvis files []       =  read stdin all
      read unvis files (f:[])   =  read file f
      read unvis files (f:rest) =  (read file f) + (read unvis files rest)

fn :: main => ()
      main =
        args = drop prog (cli args)
        spec = parse unvis args args
        raw = read unvis files (v_files spec)
        result = unvis decode bytes with raw spec
        write bytes "/dev/stdout" result
