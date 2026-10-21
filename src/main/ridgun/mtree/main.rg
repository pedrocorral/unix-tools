-- mtree/main.rg — BSD mtree(8) directory hierarchy mapper and verifier in Pure Ridgun
-- @implements FR-FS-003

import fs_lib

fn :: drop prog [String]    => [String]
      drop prog []          =  []
      drop prog (prog:rest) =  rest

fn :: resolve spec path String => String
      resolve spec path ""     =  "/dev/stdin"
      resolve spec path "-"    =  "/dev/stdin"
      resolve spec path p      =  p

fn :: emit stdout String => ()
      emit stdout ""     =  ()
      emit stdout text   =
        | (str char at text ((str len text) - 1)) == 10 = echo (str slice text 0 ((str len text) - 1))
        | otherwise                                     = write file "/dev/stdout" text

fn :: run mtree mode MtreeSpec => ()
      run mtree mode spec      =
        | m_create spec = emit stdout (generate mtree spec for (m_path spec))
        | otherwise     = run mtree verify spec

fn :: run mtree verify MtreeSpec => ()
      run mtree verify spec      =
        raw_spec = read file (resolve spec path (m_spec_file spec))
        entries = parse mtree spec text raw_spec
        diffs = verify mtree entries against (m_path spec) update (m_update spec)
        finish mtree verify diffs

fn :: finish mtree verify [String] => ()
      finish mtree verify []       =  ()
      finish mtree verify diffs    =
        _ = echo (join with diffs (char from code 10))
        sys exit 2

fn :: main => ()
      main =
        args = drop prog (cli args)
        spec = parse mtree args args
        run mtree mode spec
