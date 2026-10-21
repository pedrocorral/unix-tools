-- vis/main.rg — BSD vis(1) visual character encoder
-- @implements FR-FILTER-002

import filter_lib

fn :: drop_prog [String]    => [String]
      drop_prog []          =  []
      drop_prog (prog:rest) =  rest

fn :: read_vis_files [String] => [Int]
      read_vis_files []       =  read_bytes "/dev/stdin"
      read_vis_files (f:[])   =  read_bytes f
      read_vis_files (f:rest) =  (read_bytes f) :: (read_vis_files rest)

fn :: main => ()
      main =
        args = drop_prog cli_args
        spec = parse vis args args
        raw = read_vis_files (v_files spec)
        result = vis encode bytes raw with spec
        write_file "/dev/stdout" result
