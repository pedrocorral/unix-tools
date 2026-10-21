-- mdrr/main.rg — MD Ridgun Reader: Terminal Markdown Formatter & Reader in Pure Ridgun
-- @implements FR-FILTER-003

import md_lib

fn :: drop_prog [String] => [String]
      drop_prog []       =  []
      drop_prog (_:rest) =  rest

fn :: read_md_inputs [String] [String] => String
      read_md_inputs []       _        =  read_file "/dev/stdin"
      read_md_inputs files    acc      =
        loaded = load_each_md files acc
        join_with loaded ((char_from_code 10) + (char_from_code 10))

fn :: load_each_md [String]    [String] => [String]
      load_each_md []          acc      =  acc
      load_each_md ("-":rest)  acc      =
        load_each_md rest (acc :: [read_file "/dev/stdin"])
      load_each_md (path:rest) acc      =
        load_each_md rest (acc :: [read_file path])

fn :: emit_stdout String => ()
      emit_stdout ""     =  ()
      emit_stdout text   =
        | (str_char_at text ((str_len text) - 1)) == 10 = echo (str_slice text 0 ((str_len text) - 1))
        | otherwise                                     = write_file "/dev/stdout" text

fn :: main => ()
      main =
        args = drop_prog cli_args
        spec = parse mdrr args args
        raw = read_md_inputs (md_files spec) []
        rendered = render markdown raw with spec
        emit_stdout rendered
