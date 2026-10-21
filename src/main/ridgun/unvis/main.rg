-- unvis/main.rg — BSD unvis(1) visual character decoder
-- @implements FR-FILTER-002

import filter_lib

fn :: drop_prog [String]    => [String]
      drop_prog []          =  []
      drop_prog (prog:rest) =  rest

fn :: read_unvis_files [String] => String
      read_unvis_files []       =  read_file "/dev/stdin"
      read_unvis_files (f:[])   =  read_file f
      read_unvis_files (f:rest) =  (read_file f) + (read_unvis_files rest)

fn :: main => ()
      main =
        args = drop_prog cli_args
        raw = read_unvis_files args
        result = unvis decode bytes raw
        write_bytes "/dev/stdout" result
