-- mtree/main.rg — BSD mtree(8) directory hierarchy mapper and verifier in Pure Ridgun
-- @implements FR-FS-003

import fs_lib

fn :: drop prog [String]    => [String]
      drop prog []          =  []
      drop prog (prog:rest) =  rest

fn :: read mtree stdin chunk String String => String
      read mtree stdin chunk acc "" = acc
      read mtree stdin chunk acc chunk = read mtree stdin loop (acc + chunk)

fn :: read mtree stdin loop String => String
      read mtree stdin loop acc    =
        read mtree stdin chunk acc (posix read 0 65536)

fn :: read mtree spec String => String
      read mtree spec ""     =  read mtree stdin loop ""
      read mtree spec "-"    =  read mtree stdin loop ""
      read mtree spec p      =  read file p

fn :: emit stdout String => ()
      emit stdout ""     =  ()
      emit stdout text   =
        | (str char at text ((str len text) - 1)) == 10 = echo (str slice text 0 ((str len text) - 1))
        | otherwise                                     =
          _ = posix write 1 text
          ()

fn :: run mtree mode MtreeSpec => ()
      run mtree mode spec      =
        | m_create spec = emit stdout (generate mtree spec for (m_path spec))
        | otherwise     = run mtree verify spec

fn :: run mtree verify MtreeSpec => ()
      run mtree verify spec      =
        raw_spec = read mtree spec (m_spec_file spec)
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
