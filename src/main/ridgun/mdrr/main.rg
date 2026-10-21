-- mdrr/main.rg — MD Ridgun Reader: Terminal Markdown Formatter & Reader in Pure Ridgun
-- @implements FR-FILTER-003

import posix
import md_lib

fn :: drop prog [String] => [String]
      drop prog []       =  []
      drop prog (_:rest) =  rest

fn :: read md stdin chunk String String => String
      read md stdin chunk acc "" = acc
      read md stdin chunk acc chunk = read md stdin loop (acc + chunk)

fn :: read md stdin loop String => String
      read md stdin loop acc    =
        read md stdin chunk acc (posix read 0 65536)

fn :: read md inputs [String] [String] => String
      read md inputs []       _        =  read md stdin loop ""
      read md inputs files    acc      =
        loaded = load each md files acc
        join with loaded ((char from code 10) + (char from code 10))

fn :: load each md [String]    [String] => [String]
      load each md []          acc      =  acc
      load each md ("-":rest)  acc      =
        load each md rest (acc :: [read md stdin loop ""])
      load each md (path:rest) acc      =
        load each md rest (acc :: [read file path])

fn :: emit stdout String => ()
      emit stdout ""     =  ()
      emit stdout text   =
        | (str char at text ((str len text) - 1)) == 10 = echo (str slice text 0 ((str len text) - 1))
        | otherwise                                     =
          _ = posix write 1 text
          ()

fn :: main => ()
      main =
        args = drop prog (cli args)
        spec = parse mdrr args args
        raw = read md inputs (md_files spec) []
        rendered = render markdown raw with spec
        emit stdout rendered
