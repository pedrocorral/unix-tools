-- vis/main.rg — BSD vis(1) visual character encoder
-- @implements FR-FILTER-002

import filter_lib

fn :: drop prog [String]    => [String]
      drop prog []          =  []
      drop prog (prog:rest) =  rest

fn :: read vis files [String] => [Int]
      read vis files []       =  read bytes "/dev/stdin"
      read vis files (f:[])   =  read bytes f
      read vis files (f:rest) =  (read bytes f) :: (read vis files rest)

fn :: main => ()
      main =
        args = drop prog (cli args)
        spec = parse vis args args
        raw = read vis files (v_files spec)
        result = vis encode bytes raw with spec
        _ = posix write 1 result
        ()
