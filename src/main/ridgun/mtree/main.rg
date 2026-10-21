-- mtree/main.rg — BSD mtree(8) directory hierarchy mapper and verifier in Pure Ridgun
-- @implements FR-FS-003

import fs_lib

fn :: drop_prog [String]    => [String]
      drop_prog []          =  []
      drop_prog (prog:rest) =  rest

fn :: resolve_spec_path String => String
      resolve_spec_path ""     =  "/dev/stdin"
      resolve_spec_path "-"    =  "/dev/stdin"
      resolve_spec_path p      =  p

fn :: emit_stdout String => ()
      emit_stdout ""     =  ()
      emit_stdout text   =
        | (str_char_at text ((str_len text) - 1)) == 10 = echo (str_slice text 0 ((str_len text) - 1))
        | otherwise                                     = write_file "/dev/stdout" text

fn :: run_mtree_mode MtreeSpec => ()
      run_mtree_mode spec      =
        | m_create spec = emit_stdout (generate mtree spec for (m_path spec))
        | otherwise     = run_mtree_verify spec

fn :: run_mtree_verify MtreeSpec => ()
      run_mtree_verify spec      =
        raw_spec = read_file (resolve_spec_path (m_spec_file spec))
        entries = parse mtree spec text raw_spec
        diffs = verify mtree entries against (m_path spec) update (m_update spec)
        finish_mtree_verify diffs

fn :: finish_mtree_verify [String] => ()
      finish_mtree_verify []       =  ()
      finish_mtree_verify diffs    =
        _ = echo (join_with diffs (char_from_code 10))
        sys_exit 2

fn :: main => ()
      main =
        args = drop_prog cli_args
        spec = parse mtree args args
        run_mtree_mode spec
