-- mktemp/main.rg — BSD mktemp(1) temporary file and directory creator in Pure Ridgun
-- @implements FR-FS-004

import posix
import fs_lib

fn :: drop prog [String] => [String]
      drop prog []       =  []
      drop prog (_:rest) =  rest

fn :: finish mktemp (Bool, String) MktempSpec => ()
      finish mktemp (True, path)   _          =  echo path
      finish mktemp (False, path)  spec       =
        | mkt_quiet spec = sys exit 1
        | otherwise      =
          _ = posix write 2 ("mktemp: failed to create " + path + (char from code 10))
          sys exit 1

fn :: main => ()
      main =
        args = drop prog (cli args)
        spec = parse mktemp args args
        res = execute mktemp spec
        finish mktemp res spec
