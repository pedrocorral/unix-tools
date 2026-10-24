-- md_lib.rg — Pure Ridgun Terminal Markdown Parser, Formatter & Reader (mdrr)
-- @implements FR-FILTER-003

-- ============================================================================
-- Algebraic Data Types & Configuration Record (with FR-COMP-005 expand type)
-- ============================================================================

type ColAlign = AlignLeft | AlignCenter | AlignRight

type MdListEntry = mle_ind: Int, mle_kind: String, mle_txt: String

type MdBlock = BFrontMatter [String] | BHeading Int String | BParagraph [String] | BCode String [String]

expand type MdBlock = BTable [String] [ColAlign] [[String]] | BQuote String [String] | BListItem Int String String | BHr

type MdSpec = md_plain: Bool, md_ascii: Bool, md_line_nums: Bool, md_toc_only: Bool, md_no_frontmatter: Bool, md_width: Int, md_section: String, md_files: [String]

-- ============================================================================
-- Foundational Polymorphic & String Helpers
-- ============================================================================

fn :: list len [A]    => Int
      list len []     =  0
      list len (_:xs) =  1 + (list len xs)

fn :: fst (A, B) => A
      fst (a, _) =  a

fn :: snd (A, B) => B
      snd (_, b) =  b

fn :: fst3 (A, B, C) => A
      fst3 (a, _, _) =  a

fn :: snd3 (A, B, C) => B
      snd3 (_, b, _) =  b

fn :: trd3 (A, B, C) => C
      trd3 (_, _, c) =  c

fn :: max int Int Int => Int
      max int a   b   =
        | a >= b    = a
        | otherwise = b

fn :: is ws Int => Bool
      is ws c   =  (c == 32) or (c == 9) or (c == 10) or (c == 13)

fn :: trim ws String => String
      trim ws s      =
        left = trim left idx s 0 (str len s)
        right = trim right idx s left (str len s)
        str slice s left right

fn :: trim left idx String Int Int => Int
      trim left idx s idx len =
        | idx >= len                = len
        | is ws (str char at s idx) = trim left idx s (idx + 1) len
        | otherwise                 = idx

fn :: trim right idx String Int Int => Int
      trim right idx s left right =
        | right <= left                     = left
        | is ws (str char at s (right - 1)) = trim right idx s left (right - 1)
        | otherwise                         = right

fn :: repeat str String Int => String
      repeat str s      n   =
        | n <= 0    = ""
        | otherwise = s + (repeat str s (n - 1))

fn :: join with [String] String => String
      join with []       _      =  ""
      join with (x:[])   _      =  x
      join with (x:xs)   sep    =  x + sep + (join with xs sep)

fn :: str list nth [String] Int => String
      str list nth []       _   =  ""
      str list nth (x:xs)   n   =
        | n <= 0    = x
        | otherwise = str list nth xs (n - 1)

fn :: align list nth [ColAlign] Int => ColAlign
      align list nth []         _   =  AlignLeft
      align list nth (x:xs)     n   =
        | n <= 0    = x
        | otherwise = align list nth xs (n - 1)

fn :: int list nth [Int]  Int => Int
      int list nth []     _   =  0
      int list nth (x:xs) n   =
        | n <= 0    = x
        | otherwise = int list nth xs (n - 1)

fn :: list contains str [String] String => Bool
      list contains str []       _      =  False
      list contains str (x:xs)   target =
        | x == target = True
        | otherwise   = list contains str xs target

fn :: to lower char Int => Int
      to lower char c   =
        | (c >= 65) and (c <= 90) = c + 32
        | otherwise               = c

fn :: to lower str String => String
      to lower str s      =  to lower loop s 0 (str len s) ""

fn :: to lower loop String Int Int String => String
      to lower loop s idx len acc =
        | idx >= len = acc
        | otherwise  =
          ch = char from code (to lower char (str char at s idx))
          to lower loop s (idx + 1) len (acc + ch)

fn :: str contains ci String String => Bool
      str contains ci hay    needle =  str contains h n
        || to lower str hay         -> h
        || to lower str needle      -> n

op :: split lines String => [String]
      split lines s      =
        | (str len s) == 0 = []
        | otherwise        = scan lines loop s 0 (str len s) "" []

fn :: scan lines loop String Int Int String [String] => [String]
      scan lines loop s idx len cur acc =
        | idx >= len                = finalize lines cur acc
        | (str char at s idx) == 10 = scan lines loop s (idx + 1) len "" (acc :: [strip cr cur])
        | otherwise                 = scan lines loop s (idx + 1) len (cur + (str slice s idx (idx + 1))) acc

fn :: strip cr String => String
      strip cr s      =
        | (str len s) == 0                        = s
        | (str char at s ((str len s) - 1)) == 13 = str slice s 0 ((str len s) - 1)
        | otherwise                               = s

fn :: finalize lines String [String] => [String]
      finalize lines cur    acc      =
        | (str len cur) == 0 = acc
        | otherwise          = acc :: [strip cr cur]

op :: split whitespace String => [String]
      split whitespace s      =  split ws loop s 0 (str len s) "" []

fn :: split ws loop String Int Int String [String] => [String]
      split ws loop s idx len cur acc =
        | idx >= len                = push nonempty cur acc
        | is ws (str char at s idx) = split ws loop s (idx + 1) len "" (push nonempty cur acc)
        | otherwise                 = split ws loop s (idx + 1) len (cur + (str slice s idx (idx + 1))) acc

fn :: push nonempty String [String] => [String]
      push nonempty cur    acc      =
        | (str len cur) == 0 = acc
        | otherwise          = acc :: [cur]

-- ============================================================================
-- 1. CLI Argument Parsing — FR-FILTER-003
-- ============================================================================

fn :: default md spec => MdSpec
      default md spec =  MdSpec False False False False False 80 "" []

op :: parse mdrr args [String] => MdSpec
      parse mdrr args args     =  parse mdrr loop args (default md spec)

fn :: parse mdrr loop [String]   MdSpec => MdSpec
      parse mdrr loop []         spec   =  spec
      parse mdrr loop (arg:rest) spec   =
        | (arg == "-p") or (arg == "--plain") or (arg == "--no-color") = (parse mdrr loop rest (spec with md_plain = True))
        | (arg == "-a") or (arg == "--ascii")                          = (parse mdrr loop rest (spec with md_ascii = True))
        | (arg == "-n") or (arg == "--line-numbers")                   = (parse mdrr loop rest (spec with md_line_nums = True))
        | (arg == "-t") or (arg == "--toc")                            = (parse mdrr loop rest (spec with md_toc_only = True))
        | (arg == "-H") or (arg == "--no-frontmatter")                 = (parse mdrr loop rest (spec with md_no_frontmatter = True))
        | (arg == "-w") or (arg == "--width")                          = parse mdrr width rest spec
        | (arg == "-s") or (arg == "--section")                        = parse mdrr section rest spec
        | arg == "--"                                                  = spec with md_files = (md_files spec) :: rest
        | otherwise                                                    = (parse mdrr loop rest (spec with md_files = (md_files spec) :: [arg]))

fn :: parse mdrr width [String] MdSpec => MdSpec
      parse mdrr width []       spec   =  spec
      parse mdrr width (w:rest) spec   =  parse mdrr loop rest (spec with md_width = (max int 20 (str to int w)))

fn :: parse mdrr section [String]   MdSpec => MdSpec
      parse mdrr section []         spec   =  spec
      parse mdrr section (sec:rest) spec   =  parse mdrr loop rest (spec with md_section = sec)

-- ============================================================================
-- 2. ANSI SGR Styling & Visible Display Width Measurement
-- ============================================================================

fn :: ansi String Bool  => String
      ansi _      True  =  ""
      ansi code   False =  (char from code 27) + "[" + code + "m"

fn :: reset ansi Bool  => String
      reset ansi plain =  ansi "0" plain

fn :: visible len String => Int
      visible len s      =  scan visible len s 0 (str len s) False 0

fn :: scan visible len String Int Int Bool Int => Int
      scan visible len s idx len in_esc count =
        | idx >= len                                                  = count
        | in_esc and ((str char at s idx) == 109)                     = (scan visible len s (idx + 1) len False count)
        | in_esc                                                      = (scan visible len s (idx + 1) len True count)
        | (str char at s idx) == 27                                   = (scan visible len s (idx + 1) len True count)
        | ((str char at s idx) >= 128) and ((str char at s idx) < 192) = (scan visible len s (idx + 1) len False count)
        | otherwise                                                   = (scan visible len s (idx + 1) len False (count + 1))

-- ============================================================================
-- 3. Inline Markdown Parser & Renderer (**bold**, *italic*, `code`, [link](url))
-- ============================================================================

op :: format inline String plain Bool     => String
      format inline s      plain is_plain =
        format inline loop s 0 (str len s) is_plain ""

fn :: format inline loop String Int Int Bool String => String
      format inline loop s idx len plain acc =
        | idx >= len                                        = acc
        | ((str char at s idx) == 92) and ((idx + 1) < len) = (format inline loop s (idx + 2) len plain (acc + (str slice s (idx + 1) (idx + 2))))
        | ((idx + 1) < len) and ((str slice s idx (idx + 2)) == "``") =
          format inline code delim s (idx + 2) len "``" (find substr idx s (idx + 2) len "``") plain acc
        | (str char at s idx) == 96 =
          format inline code delim s (idx + 1) len "`" (find substr idx s (idx + 1) len "`") plain acc
        | ((idx + 1) < len) and ((str slice s idx (idx + 2)) == "![") =
          format inline image s (idx + 2) len (find substr idx s (idx + 2) len "](") plain acc
        | (str char at s idx) == 91 =
          format inline link s (idx + 1) len (find substr idx s (idx + 1) len "](") plain acc
        | ((idx + 2) < len) and ((str slice s idx (idx + 3)) == "***") =
          format inline delim s (idx + 3) len "***" "1;3" "22;23" (find delim outside code s (idx + 3) len "***" False) plain acc
        | ((idx + 1) < len) and ((str slice s idx (idx + 2)) == "**") =
          format inline delim s (idx + 2) len "**" "1" "22" (find delim outside code s (idx + 2) len "**" False) plain acc
        | ((idx + 1) < len) and ((str slice s idx (idx + 2)) == "__") =
          format inline delim s (idx + 2) len "__" "1" "22" (find delim outside code s (idx + 2) len "__" False) plain acc
        | ((idx + 1) < len) and ((str slice s idx (idx + 2)) == "~~") =
          format inline delim s (idx + 2) len "~~" "9" "29" (find delim outside code s (idx + 2) len "~~" False) plain acc
        | (str char at s idx) == 42 =
          format inline delim s (idx + 1) len "*" "3" "23" (find delim outside code s (idx + 1) len "*" False) plain acc
        | is italic underscore s idx len =
          format inline delim s (idx + 1) len "_" "3" "23" (find delim outside code s (idx + 1) len "_" False) plain acc
        | otherwise =
          (format inline loop s (idx + 1) len plain (acc + (str slice s idx (idx + 1))))

fn :: is alnum ascii Int => Bool
      is alnum ascii c   =
        ((c >= 48) and (c <= 57)) or ((c >= 65) and (c <= 90)) or ((c >= 97) and (c <= 122))

fn :: is italic underscore String Int Int => Bool
      is italic underscore s idx _ =
        | (str char at s idx) != 95                                = False
        | (idx > 0) and (is alnum ascii (str char at s (idx - 1))) = False
        | otherwise                                                = True

fn :: find substr idx String Int Int String => Int
      find substr idx s idx len pat =
        plen = str len pat
        find substr loop s idx len pat plen

fn :: find substr loop String Int Int String Int => Int
      find substr loop s idx len pat plen =
        | (idx + plen) > len                    = 0 - 1
        | (str slice s idx (idx + plen)) == pat = idx
        | otherwise                             = find substr loop s (idx + 1) len pat plen

fn :: find delim outside code String Int Int String Bool => Int
      find delim outside code s idx len pat in_code =
        | (idx + (str len pat)) > len                                        = 0 - 1
        | (str char at s idx) == 96                                          = (find delim outside code s (idx + 1) len pat (not in_code))
        | (not in_code) and ((str slice s idx (idx + (str len pat))) == pat) = idx
        | otherwise                                                          = find delim outside code s (idx + 1) len pat in_code

fn :: format inline code delim String Int Int String Int Bool String => String
      format inline code delim s idx len delim end_i plain acc =
        | end_i < 0 = format inline loop s idx len plain (acc + delim)
        | plain     =
          dlen = str len delim
          code_txt = trim ws (str slice s idx end_i)
          format inline loop s (end_i + dlen) len plain (acc + "`" + code_txt + "`")
        | otherwise =
          dlen = str len delim
          code_txt = trim ws (str slice s idx end_i)
          styled = (ansi "36;1" False) + code_txt + (ansi "22;39" False)
          format inline loop s (end_i + dlen) len plain (acc + styled)

fn :: format inline delim String Int Int String String String Int Bool String => String
      format inline delim s idx len delim open_sgr close_sgr end_i plain acc =
        | end_i < 0 = format inline loop s idx len plain (acc + delim)
        | otherwise =
          dlen = str len delim
          inner_raw = str slice s idx end_i
          inner_fmt = format inline inner_raw plain plain
          styled = (ansi open_sgr plain) + inner_fmt + (ansi close_sgr plain)
          format inline loop s (end_i + dlen) len plain (acc + styled)

fn :: format inline link String Int Int Int Bool String => String
      format inline link s idx len close_b plain acc =
        | close_b < 0 = format inline loop s idx len plain (acc + "[")
        | otherwise   =
          | (find substr idx s (close_b + 2) len ")") < 0 = format inline loop s idx len plain (acc + "[")
          | otherwise =
            url_start = close_b + 2
            close_p = find substr idx s url_start len ")"
            label = format inline (str slice s idx close_b) plain plain
            url = str slice s url_start close_p
            rendered = render link span label url plain
            format inline loop s (close_p + 1) len plain (acc + rendered)

fn :: render link span String String Bool => String
      render link span label url True = label + " (" + url + ")"
      render link span label url False =
        (ansi "4;34;1" False) + label + (ansi "0" False) + " " + (ansi "2;37" False) + "(" + url + ")" + (ansi "0" False)

fn :: format inline image String Int Int Int Bool String => String
      format inline image s idx len close_b plain acc =
        | close_b < 0 = format inline loop s idx len plain (acc + "![")
        | otherwise   =
          | (find substr idx s (close_b + 2) len ")") < 0 = format inline loop s idx len plain (acc + "![")
          | otherwise =
            url_start = close_b + 2
            close_p = find substr idx s url_start len ")"
            alt = str slice s idx close_b
            src = str slice s url_start close_p
            badge = (ansi "35;1" plain) + "[IMAGE: " + alt + " (" + src + ")]" + (reset ansi plain)
            format inline loop s (close_p + 1) len plain (acc + badge)

-- ============================================================================
-- 4. Block-Level Markdown Parser (`parse markdown String => [MdBlock]`)
-- ============================================================================

op :: parse markdown String => [MdBlock]
      parse markdown raw    =
        lines = split lines raw
        parse frontmatter or blocks lines

fn :: parse frontmatter or blocks [String]     => [MdBlock]
      parse frontmatter or blocks []           =  []
      parse frontmatter or blocks (first:rest) =
        | (trim ws first) == "---" =
          fm_res = collect frontmatter rest []
          prepend frontmatter (snd3 fm_res) (fst3 fm_res) (trd3 fm_res) rest
        | otherwise =
          parse blocks loop ([first] :: rest) [] []

fn :: prepend frontmatter Bool [String] [String] [String] => [MdBlock]
      prepend frontmatter False _ _ orig_rest =
        parse blocks loop (["---"] :: orig_rest) [] []
      prepend frontmatter True fm_lines rem _ =
        [BFrontMatter fm_lines] :: (parse blocks loop rem [] [])

fn :: collect frontmatter [String]    [String] => [String] Bool [String]
      collect frontmatter []          acc      =  (acc, False, [])
      collect frontmatter (line:rest) acc      =
        | ((trim ws line) == "---") or ((trim ws line) == "...") = (acc, True, rest)
        | otherwise                                              = (collect frontmatter rest (acc :: [line]))

fn :: flush para [String]   [MdBlock] => [MdBlock]
      flush para []         blocks    =  blocks
      flush para para_lines blocks    =  blocks :: [BParagraph para_lines]

fn :: parse blocks loop [String] [String] [MdBlock] => [MdBlock]
      parse blocks loop [] cur_para blocks = flush para cur_para blocks
      parse blocks loop (line:rest) cur_para blocks =
        | (str len (trim ws line)) == 0 = (parse blocks loop rest [] (flush para cur_para blocks))
        | is fence start (trim ws line) =
          b1 = flush para cur_para blocks
          fence_res = parse fenced code (trim ws line) rest []
          (parse blocks loop (snd fence_res) [] (b1 :: [fst fence_res]))
        | is hr line (trim ws line) =
          (parse blocks loop rest [] ((flush para cur_para blocks) :: [BHr]))
        | is atx heading (trim ws line) =
          (parse blocks loop rest [] ((flush para cur_para blocks) :: [parse atx heading (trim ws line)]))
        | str starts with (trim ws line) ">" =
          b1 = flush para cur_para blocks
          q_res = collect blockquote ([trim ws line] :: rest) []
          (parse blocks loop (snd q_res) [] (b1 :: [build quote block (fst q_res)]))
        | is table start (trim ws line) rest =
          b1 = flush para cur_para blocks
          t_res = parse gfm table (trim ws line) rest
          (parse blocks loop (snd t_res) [] (b1 :: [fst t_res]))
        | is list line line =
          (parse blocks loop rest [] ((flush para cur_para blocks) :: [parse list item line]))
        | otherwise =
          (parse blocks loop rest (cur_para :: [trim ws line]) blocks)

fn :: is fence start String => Bool
      is fence start t      =  (str_starts_with t "```") or (str_starts_with t "~~~")

fn :: parse fenced code String [String] [String] => MdBlock [String]
      parse fenced code open_line [] acc =
        lang = trim ws (str slice open_line 3 (str len open_line))
        (BCode lang acc, [])
      parse fenced code open_line (line:rest) acc =
        | str starts with (trim ws line) (str slice open_line 0 3) =
          lang = trim ws (str slice open_line 3 (str len open_line))
          (BCode lang acc, rest)
        | otherwise =
          (parse fenced code open_line rest (acc :: [line]))

fn :: is hr line String => Bool
      is hr line t      =
        | (t == "---") or (t == "***") or (t == "___") or (t == "- - -") or (t == "* * *") = True
        | ((str len t) >= 3) and (all same char t 0 (str len t) 45)                        = True
        | ((str len t) >= 3) and (all same char t 0 (str len t) 42)                        = True
        | otherwise                                                                        = False

fn :: all same char String Int Int Int => Bool
      all same char s idx len target_c =
        | idx >= len                      = True
        | (str char at s idx) == target_c = all same char s (idx + 1) len target_c
        | otherwise                       = False

fn :: count leading hashes String Int Int => Int
      count leading hashes s idx len =
        | idx >= len                = idx
        | (str char at s idx) == 35 = count leading hashes s (idx + 1) len
        | otherwise                 = idx

fn :: is atx heading String => Bool
      is atx heading t      =
        n = count leading hashes t 0 (str len t)
        (n >= 1) and (n <= 6) and ((n < (str len t)) and ((str char at t n) == 32))

fn :: parse atx heading String => MdBlock
      parse atx heading t      =
        n = count leading hashes t 0 (str len t)
        raw_title = trim ws (str slice t n (str len t))
        BHeading n (strip trailing hashes raw_title)

fn :: strip trailing hashes String => String
      strip trailing hashes s      =
        | (str len s) == 0                        = s
        | (str char at s ((str len s) - 1)) == 35 = (strip trailing hashes (trim ws (str slice s 0 ((str len s) - 1))))
        | otherwise                               = s

fn :: strip quote prefix String => String
      strip quote prefix t      =
        | str starts with t "> " = str slice t 2 (str len t)
        | str starts with t ">"  = str slice t 1 (str len t)
        | otherwise              = t

fn :: collect blockquote [String]    [String] => [String] [String]
      collect blockquote []          acc      =  (acc, [])
      collect blockquote (line:rest) acc      =
        | str starts with (trim ws line) ">" = (collect blockquote rest (acc :: [strip quote prefix (trim ws line)]))
        | otherwise                          = (acc, [line] :: rest)

fn :: build quote block [String]     => MdBlock
      build quote block []           =  BQuote "" []
      build quote block (first:rest) =
        | str starts with (trim ws first) "[!NOTE]" =
          tf = trim ws first
          BQuote "NOTE" (prepend nonempty (trim ws (str slice tf 7 (str len tf))) rest)
        | str starts with (trim ws first) "[!TIP]" =
          tf = trim ws first
          BQuote "TIP" (prepend nonempty (trim ws (str slice tf 6 (str len tf))) rest)
        | str starts with (trim ws first) "[!IMPORTANT]" =
          tf = trim ws first
          BQuote "IMPORTANT" (prepend nonempty (trim ws (str slice tf 12 (str len tf))) rest)
        | str starts with (trim ws first) "[!WARNING]" =
          tf = trim ws first
          BQuote "WARNING" (prepend nonempty (trim ws (str slice tf 10 (str len tf))) rest)
        | str starts with (trim ws first) "[!CAUTION]" =
          tf = trim ws first
          BQuote "CAUTION" (prepend nonempty (trim ws (str slice tf 10 (str len tf))) rest)
        | otherwise =
          BQuote "" ([first] :: rest)

fn :: prepend nonempty String [String] => [String]
      prepend nonempty ""     xs       =  xs
      prepend nonempty s      xs       =  [s] :: xs

-- ============================================================================
-- 5. GFM Table Parser (`| Col 1 | Col 2 |` + `| :--- | ---: |`)
-- ============================================================================

fn :: is table start String      [String]     => Bool
      is table start _           []           =  False
      is table start header_line (sep_line:_) =
        (has_pipe_char header_line) and (is table delimiter row (trim ws sep_line))

fn :: has pipe char String => Bool
      has pipe char s      =  str contains s "|"

fn :: is table delimiter row String => Bool
      is table delimiter row s      =
        | not (has pipe char s) = False
        | otherwise             =
          cells = split table cells s
          ((list len cells) >= 1) and (all cells are delims cells)

fn :: all cells are delims [String] => Bool
      all cells are delims []       =  True
      all cells are delims (c:rest) =
        | is delim cell (trim ws c) = all cells are delims rest
        | otherwise                 = False

fn :: is delim cell String => Bool
      is delim cell c      =
        | (str len c) == 0 = False
        | otherwise        = is delim chars c 0 (str len c) False

fn :: is delim chars String Int Int Bool => Bool
      is delim chars c idx len seen_dash =
        | idx >= len                = seen_dash
        | (str char at c idx) == 45 = is delim chars c (idx + 1) len True
        | (str char at c idx) == 58 = is delim chars c (idx + 1) len seen_dash
        | otherwise                 = False

fn :: split table cells String => [String]
      split table cells line   =
        stripped = strip outer pipes (trim ws line)
        split pipe loop stripped 0 (str len stripped) "" []

fn :: strip outer pipes String => String
      strip outer pipes s      =
        s1 = strip leading pipe s
        strip trailing pipe s1

fn :: strip leading pipe String => String
      strip leading pipe s      =
        | str starts with s "|" = str slice s 1 (str len s)
        | otherwise             = s

fn :: strip trailing pipe String => String
      strip trailing pipe s      =
        | ((str len s) > 0) and ((str char at s ((str len s) - 1)) == 124) = str slice s 0 ((str len s) - 1)
        | otherwise                                                        = s

fn :: split pipe loop String Int Int String [String] => [String]
      split pipe loop s idx len cur acc =
        | idx >= len = acc :: [trim ws cur]
        | ((str char at s idx) == 92) and (((idx + 1) < len) and ((str char at s (idx + 1)) == 124)) =
          (split pipe loop s (idx + 2) len (cur + "|") acc)
        | (str char at s idx) == 124 =
          (split pipe loop s (idx + 1) len "" (acc :: [trim ws cur]))
        | otherwise =
          (split pipe loop s (idx + 1) len (cur + (str slice s idx (idx + 1))) acc)

fn :: parse col aligns [String] [ColAlign] => [ColAlign]
      parse col aligns []       acc        =  acc
      parse col aligns (c:rest) acc        =
        tc = trim ws c
        left_col = str starts with tc ":"
        right_col = ((str len tc) > 0) and ((str char at tc ((str len tc) - 1)) == 58)
        align =
          | left_col and right_col = AlignCenter
          | right_col              = AlignRight
          | otherwise              = AlignLeft |.
        parse col aligns rest (acc :: [align])

fn :: parse gfm table String      [String]        => MdBlock [String]
      parse gfm table header_line []              =  (BTable (split table cells header_line) [] [], [])
      parse gfm table header_line (sep_line:rest) =
        || split table cells header_line                    -> headers
        || parse col aligns (split table cells sep_line) [] -> aligns
        || collect table rows rest []                       -> rows_res ||.
        (BTable headers aligns (fst rows_res), snd rows_res)

fn :: collect table rows [String]    [[String]] => [[String]] [String]
      collect table rows []          acc        =  (acc, [])
      collect table rows (line:rest) acc        =
        | ((str len (trim ws line)) > 0) and (has pipe char (trim ws line)) =
          (collect table rows rest (acc :: [split table cells (trim ws line)]))
        | otherwise =
          (acc, [line] :: rest)

-- ============================================================================
-- 6. List & GFM Task List Parser (`-`, `*`, `+`, `1.`, `- [ ]`, `- [x]`)
-- ============================================================================

fn :: count indent String Int Int => Int
      count indent s      idx len =
        | idx >= len                = idx
        | (str char at s idx) == 32 = count indent s (idx + 1) len
        | (str char at s idx) == 9  = count indent s (idx + 1) len
        | otherwise                 = idx

fn :: is list line String => Bool
      is list line line   =
        | (str_starts_with (trim ws line) "- ") or (str_starts_with (trim ws line) "* ") or (str_starts_with (trim ws line) "+ ") = True
        | otherwise =
          t = trim ws line
          (ordered marker len t 0 (str len t)) > 0

fn :: ordered marker len String Int Int => Int
      ordered marker len t idx len =
        | idx >= len                                                  = 0
        | ((str char at t idx) >= 48) and ((str char at t idx) <= 57) = (ordered marker len t (idx + 1) len)
        | (idx > 0) and (((str char at t idx) == 46) or ((str char at t idx) == 41)) and (((idx + 1) < len) and ((str char at t (idx + 1)) == 32)) = idx + 1
        | otherwise                                                   = 0

fn :: parse list item String => MdBlock
      parse list item line   =
        | (str_starts_with (trim ws line) "- [ ] ") or (str_starts_with (trim ws line) "* [ ] ") =
          indent = count indent line 0 (str len line)
          t = trim ws line
          BListItem indent "todo" (str slice t 6 (str len t))
        | (str_starts_with (trim ws line) "- [x] ") or (str_starts_with (trim ws line) "- [X] ") or (str_starts_with (trim ws line) "* [x] ") or (str_starts_with (trim ws line) "* [X] ") =
          indent = count indent line 0 (str len line)
          t = trim ws line
          BListItem indent "done" (str slice t 6 (str len t))
        | (str_starts_with (trim ws line) "- ") or (str_starts_with (trim ws line) "* ") or (str_starts_with (trim ws line) "+ ") =
          indent = count indent line 0 (str len line)
          t = trim ws line
          BListItem indent "bullet" (str slice t 2 (str len t))
        | otherwise =
          indent = count indent line 0 (str len line)
          t = trim ws line
          mlen = ordered marker len t 0 (str len t)
          marker = str slice t 0 mlen
          body = trim ws (str slice t mlen (str len t))
          BListItem indent marker body

-- ============================================================================
-- 7. Section Filtering (`-s`) & Table of Contents (`-t`) — FR-FILTER-003
-- ============================================================================

op :: extract section [MdBlock] matching String     => [MdBlock]
      extract section blocks    matching ""         =  blocks
      extract section blocks    matching target_sec =
        scan section loop blocks target_sec False 0 []

fn :: scan section loop [MdBlock] String Bool Int [MdBlock] => [MdBlock]
      scan section loop [] _ _ _ acc = acc
      scan section loop ((BHeading lvl title):rest) target_sec False _ acc =
        | str contains ci title target_sec = (scan section loop rest target_sec True lvl (acc :: [BHeading lvl title]))
        | otherwise                        = (scan section loop rest target_sec False 0 acc)
      scan section loop ((BHeading lvl title):rest) target_sec True start_lvl acc =
        | lvl <= start_lvl = acc
        | otherwise        = (scan section loop rest target_sec True start_lvl (acc :: [BHeading lvl title]))
      scan section loop (_:rest) target_sec False start_lvl acc =
        scan section loop rest target_sec False start_lvl acc
      scan section loop (b:rest) target_sec True start_lvl acc =
        scan section loop rest target_sec True start_lvl (acc :: [b])

op :: render toc [MdBlock] with MdSpec => String
      render toc blocks    with spec   =
        lines = collect toc lines blocks spec []
        res =
          | (list len lines) == 0 = ""
          | otherwise             = (join with lines (char from code 10)) + (char from code 10)
        res

fn :: collect toc lines [MdBlock] MdSpec [String] => [String]
      collect toc lines [] _ acc = acc
      collect toc lines ((BHeading lvl title):rest) spec acc =
        indent = repeat str "  " (lvl - 1)
        bullet = select toc bullet lvl (md_ascii spec)
        clean_title = format inline title plain (md_plain spec)
        styled = indent + (ansi "36;1" (md_plain spec)) + bullet + (reset ansi (md_plain spec)) + " " + clean_title
        collect toc lines rest spec (acc :: [styled])
      collect toc lines (_:rest) spec acc =
        collect toc lines rest spec acc

fn :: select toc bullet Int Bool  => String
      select toc bullet _   True  =  "*"
      select toc bullet 1   False =  "▸"
      select toc bullet 2   False =  "•"
      select toc bullet _   False =  "◦"

-- ============================================================================
-- 8. Visible-Width Word Wrapper & Block Renderer
-- ============================================================================

fn :: wrap ansi words [String] Int String String String Int [String] => [String]
      wrap ansi words [] _ _ _ cur_line _ acc =
        | (str len cur_line) == 0 = acc
        | otherwise               = acc :: [cur_line]
      wrap ansi words (w:rest) max_w first_pref cont_pref cur_line cur_vis acc =
        | cur_vis == 0 =
          w_vis = visible len w
          pref_vis = visible len first_pref
          (wrap ansi words rest max_w first_pref cont_pref (first_pref + w) (pref_vis + w_vis) acc)
        | (cur_vis + 1 + (visible len w)) <= max_w =
          w_vis = visible len w
          (wrap ansi words rest max_w first_pref cont_pref (cur_line + " " + w) (cur_vis + 1 + w_vis) acc)
        | otherwise =
          w_vis = visible len w
          cont_vis = visible len cont_pref
          (wrap ansi words rest max_w first_pref cont_pref (cont_pref + w) (cont_vis + w_vis) (acc :: [cur_line]))

fn :: wrap formatted text String Int String String => [String]
      wrap formatted text text max_w first_pref cont_pref =
        words = split whitespace text
        res =
          | (list len words) == 0 = [first_pref]
          | otherwise             = wrap ansi words words max_w first_pref cont_pref "" 0 []
        res

-- ============================================================================
-- 9. Syntax Highlighting for Fenced Code Blocks
-- ============================================================================

fn :: is code keyword String => Bool
      is code keyword w      =
        list contains str ["fn", "op", "type", "expand", "with", "import", "otherwise", "and", "or", "not", "xor", "mut", "region", "map", "filter", "fold", "True", "False", "Int", "String", "Bool", "if", "then", "else", "elif", "fi", "for", "in", "do", "done", "while", "return", "let", "const", "var", "pub", "struct", "enum", "match", "def", "class", "permit", "deny"] w

fn :: is comment line String => Bool
      is comment line line   =
        t = trim ws line
        (str_starts_with t "--") or (str_starts_with t "#") or (str_starts_with t "//")

fn :: highlight code line String Bool  => String
      highlight code line line   True  =  line
      highlight code line line   False =
        | is comment line line = (ansi "2;32" False) + line + (reset ansi False)
        | otherwise            = highlight tokens loop line 0 (str len line) "" ""

fn :: is ident char Int => Bool
      is ident char c   =  (is_alnum_ascii c) or (c == 95)

fn :: flush ident tok String => String
      flush ident tok ""     =  ""
      flush ident tok tok    =
        | is code keyword tok = (ansi "35;1" False) + tok + (reset ansi False)
        | otherwise           = tok

fn :: highlight tokens loop String Int Int String String => String
      highlight tokens loop s idx len cur_id acc =
        | idx >= len = acc + (flush ident tok cur_id)
        | (str char at s idx) == 34 =
          acc1 = acc + (flush ident tok cur_id)
          str_res = read quoted literal s (idx + 1) len "\""
          styled_s = (ansi "33" False) + (fst str_res) + (reset ansi False)
          (highlight tokens loop s (snd str_res) len "" (acc1 + styled_s))
        | is ident char (str char at s idx) =
          (highlight tokens loop s (idx + 1) len (cur_id + (str slice s idx (idx + 1))) acc)
        | otherwise =
          acc1 = acc + (flush ident tok cur_id) + (str slice s idx (idx + 1))
          (highlight tokens loop s (idx + 1) len "" acc1)

fn :: read quoted literal String Int Int String => String Int
      read quoted literal s idx len acc =
        | idx >= len                                        = (acc, len)
        | ((str char at s idx) == 92) and ((idx + 1) < len) = (read quoted literal s (idx + 2) len (acc + (str slice s idx (idx + 2))))
        | (str char at s idx) == 34                         = (acc + "\"", idx + 1)
        | otherwise                                         = (read quoted literal s (idx + 1) len (acc + (str slice s idx (idx + 1))))

-- ============================================================================
-- 10. Rendering Blocks (Headings, Code Boxes, GFM Tables, Alerts, Lists)
-- ============================================================================

op :: render markdown String with MdSpec => String
      render markdown raw    with spec   =
        all_b = parse markdown raw
        blocks = extract section all_b matching (md_section spec)
        render blocks or toc blocks spec

fn :: render blocks or toc [MdBlock] MdSpec => String
      render blocks or toc blocks spec =
        | md_toc_only spec = render toc blocks with spec
        | otherwise        =
          rendered = render blocks loop blocks spec []
          res =
            | (list len rendered) == 0 = ""
            | otherwise                = (join with rendered ((char from code 10) + (char from code 10))) + (char from code 10)
          res

fn :: render blocks loop [MdBlock] MdSpec [String] => [String]
      render blocks loop [] _ acc = acc
      render blocks loop ((BFrontMatter fm):rest) spec acc =
        | md_no_frontmatter spec = render blocks loop rest spec acc
        | otherwise              = render blocks loop rest spec (acc :: [render frontmatter fm spec])
      render blocks loop ((BHeading lvl title):rest) spec acc =
        render blocks loop rest spec (acc :: [render heading lvl title spec])
      render blocks loop ((BParagraph lines):rest) spec acc =
        render blocks loop rest spec (acc :: [render paragraph lines spec])
      render blocks loop ((BCode lang lines):rest) spec acc =
        render blocks loop rest spec (acc :: [render code block lang lines spec])
      render blocks loop ((BTable hdrs aligns rows):rest) spec acc =
        render blocks loop rest spec (acc :: [render table block hdrs aligns rows spec])
      render blocks loop ((BQuote kind lines):rest) spec acc =
        render blocks loop rest spec (acc :: [render quote block kind lines spec])
      render blocks loop ((BListItem ind kind txt):rest) spec acc =
        list_res = collect consecutive list ([MdListEntry ind kind txt]) rest
        items_str = render list items (fst list_res) spec []
        render blocks loop (snd list_res) spec (acc :: [items_str])
      render blocks loop (BHr:rest) spec acc =
        render blocks loop rest spec (acc :: [render hr spec])

fn :: collect consecutive list [MdListEntry] [MdBlock] => [MdListEntry] [MdBlock]
      collect consecutive list acc [] = (acc, [])
      collect consecutive list acc ((BListItem ind kind txt):rest) =
        collect consecutive list (acc :: [MdListEntry ind kind txt]) rest
      collect consecutive list acc other = (acc, other)

fn :: render frontmatter [String] MdSpec => String
      render frontmatter lines    spec   =
        || select box char (md_ascii spec) "-" "─" -> h_ch
        || select box char (md_ascii spec) "+" "╭" -> tl
        || select box char (md_ascii spec) "+" "╰" -> bl
        || select box char (md_ascii spec) "|" "│" -> v_ch ||.
        plain = md_plain spec
        top = (ansi "2;36" plain) + tl + h_ch + " front-matter " + (repeat str h_ch 24) + (reset ansi plain)
        bot = (ansi "2;36" plain) + bl + (repeat str h_ch 39) + (reset ansi plain)
        body = format fm lines lines v_ch plain []
        join with ([top] :: body :: [bot]) (char from code 10)

fn :: format fm lines [String] String Bool [String] => [String]
      format fm lines [] _ _ acc = acc
      format fm lines (l:rest) v_ch plain acc =
        line_s = (ansi "2;36" plain) + v_ch + " " + (reset ansi plain) + (ansi "37" plain) + l + (reset ansi plain)
        format fm lines rest v_ch plain (acc :: [line_s])

fn :: select box char Bool String String => String
      select box char True a _ = a
      select box char False _ u = u

fn :: render heading Int String    MdSpec => String
      render heading lvl raw_title spec   =
        | lvl == 1 =
          nl = char from code 10
          plain = md_plain spec
          ascii = md_ascii spec
          fmt_title = format inline raw_title plain plain
          rule_ch = select box char ascii "=" "═"
          rlen = max int 8 (visible len fmt_title)
          (ansi "1;36" plain) + fmt_title + (reset ansi plain) + nl + (ansi "36" plain) + (repeat str rule_ch rlen) + (reset ansi plain)
        | lvl == 2 =
          nl = char from code 10
          plain = md_plain spec
          ascii = md_ascii spec
          fmt_title = format inline raw_title plain plain
          rule_ch = select box char ascii "-" "─"
          rlen = max int 6 (visible len fmt_title)
          (ansi "1;32" plain) + fmt_title + (reset ansi plain) + nl + (ansi "32" plain) + (repeat str rule_ch rlen) + (reset ansi plain)
        | lvl == 3 =
          plain = md_plain spec
          fmt_title = format inline raw_title plain plain
          (ansi "1;33" plain) + "### " + fmt_title + (reset ansi plain)
        | otherwise =
          plain = md_plain spec
          fmt_title = format inline raw_title plain plain
          prefix = repeat str "#" lvl
          (ansi "1;34" plain) + prefix + " " + fmt_title + (reset ansi plain)

fn :: render paragraph [String] MdSpec => String
      render paragraph lines    spec   =
        joined = join with lines " "
        fmt = format inline joined plain (md_plain spec)
        wrapped = wrap formatted text fmt (md_width spec) "" ""
        join with wrapped (char from code 10)

fn :: render hr MdSpec => String
      render hr spec   =
        ch = select box char (md_ascii spec) "-" "─"
        (ansi "2;37" (md_plain spec)) + (repeat str ch (md_width spec)) + (reset ansi (md_plain spec))

fn :: render code block String [String] MdSpec => String
      render code block lang lines spec =
        || select box char (md_ascii spec) "-" "─" -> h_ch
        || select box char (md_ascii spec) "+" "╭" -> tl
        || select box char (md_ascii spec) "+" "╰" -> bl
        || select box char (md_ascii spec) "|" "│" -> v_ch
        || select code label lang                  -> label ||.
        plain = md_plain spec
        top = (ansi "2;36" plain) + tl + h_ch + label + (repeat str h_ch (max int 4 (44 - (str len label)))) + (reset ansi plain)
        bot = (ansi "2;36" plain) + bl + (repeat str h_ch 45) + (reset ansi plain)
        body = render code lines lines 1 (md_line_nums spec) v_ch plain []
        join with ([top] :: body :: [bot]) (char from code 10)

fn :: select code label String => String
      select code label ""     =  " code "
      select code label lang   =  " " + lang + " "

fn :: pad left num Int Int   => String
      pad left num n   width =
        s = int to str n
        pad = max int 0 (width - (str len s))
        (repeat str " " pad) + s

fn :: render code lines [String] Int Bool String Bool [String] => [String]
      render code lines [] _ _ _ _ acc = acc
      render code lines (l:rest) idx show_nums v_ch plain acc =
        || format code prefix idx show_nums v_ch plain -> prefix
        || highlight code line l plain                 -> hl ||.
        render code lines rest (idx + 1) show_nums v_ch plain (acc :: [prefix + hl])

fn :: format code prefix Int Bool String Bool => String
      format code prefix _ False v_ch plain =
        (ansi "2;36" plain) + v_ch + " " + (reset ansi plain)
      format code prefix idx True v_ch plain =
        num_s = pad left num idx 3
        (ansi "2;36" plain) + v_ch + " " + (ansi "2;37" plain) + num_s + " │ " + (reset ansi plain)

-- ============================================================================
-- 11. Blockquotes & GitHub Alerts (`NOTE`, `TIP`, `IMPORTANT`, `WARNING`, `CAUTION`)
-- ============================================================================

fn :: alert color and title String Bool => String String
      alert color and title "NOTE" ascii = ("34;1", (select box char ascii "[i] NOTE" "ℹ NOTE"))
      alert color and title "TIP" ascii = ("32;1", (select box char ascii "[*] TIP" "💡 TIP"))
      alert color and title "IMPORTANT" ascii = ("35;1", (select box char ascii "[!] IMPORTANT" "❗ IMPORTANT"))
      alert color and title "WARNING" ascii = ("33;1", (select box char ascii "[!] WARNING" "⚠ WARNING"))
      alert color and title "CAUTION" ascii = ("31;1", (select box char ascii "[X] CAUTION" "🛑 CAUTION"))
      alert color and title _ _ = ("2;37", "")

fn :: render quote block String [String] MdSpec => String
      render quote block kind lines spec =
        || select box char (md_ascii spec) "|" "┃"    -> bar_ch
        || alert color and title kind (md_ascii spec) -> meta
        || join with lines " "                        -> joined ||.
        plain = md_plain spec
        sgr = fst meta
        badge = snd meta
        bar = (ansi sgr plain) + bar_ch + " " + (reset ansi plain)
        fmt_body = format inline joined plain plain
        wrapped = wrap formatted text fmt_body (max int 20 ((md_width spec) - 4)) bar bar
        hdr = bar + (ansi sgr plain) + badge + (reset ansi plain)
        res =
          | (str len badge) == 0 = join with wrapped (char from code 10)
          | otherwise            = join with ([hdr] :: wrapped) (char from code 10)
        res

-- ============================================================================
-- 12. List & Task List Rendering
-- ============================================================================

fn :: render list items [MdListEntry] MdSpec [String] => String
      render list items [] _ acc = join with acc (char from code 10)
      render list items (ent:rest) spec acc =
        ind = mle_ind ent
        kind = mle_kind ent
        txt = mle_txt ent
        plain = md_plain spec
        ascii = md_ascii spec
        pad = repeat str " " ind
        marker = format list marker kind ascii plain
        first_pref = pad + marker + " "
        cont_pref = pad + "  "
        fmt_txt = format inline txt plain plain
        lines = wrap formatted text fmt_txt (md_width spec) first_pref cont_pref
        render list items rest spec (acc :: lines)

fn :: format list marker String Bool Bool => String
      format list marker "bullet" True plain = (ansi "36;1" plain) + "*" + (reset ansi plain)
      format list marker "bullet" False plain = (ansi "36;1" plain) + "•" + (reset ansi plain)
      format list marker "todo" True plain = (ansi "33;1" plain) + "[ ]" + (reset ansi plain)
      format list marker "todo" False plain = (ansi "33;1" plain) + "☐" + (reset ansi plain)
      format list marker "done" True plain = (ansi "32;1" plain) + "[x]" + (reset ansi plain)
      format list marker "done" False plain = (ansi "32;1" plain) + "☑" + (reset ansi plain)
      format list marker num_m _ plain = (ansi "36;1" plain) + num_m + (reset ansi plain)

-- ============================================================================
-- 13. GFM Table Box Rendering & Cell Alignment
-- ============================================================================

fn :: render table block [String] [ColAlign] [[String]] MdSpec => String
      render table block hdrs aligns rows spec =
        || format row cells hdrs (md_plain spec) [] -> fmt_hdrs
        || format all rows rows (md_plain spec) []  -> fmt_rows
        || list len hdrs                            -> num_cols ||.
        plain = md_plain spec
        ascii = md_ascii spec
        widths = compute table widths 0 num_cols fmt_hdrs fmt_rows []
        top_b = render table border widths ascii plain "┌" "┬" "┐" "─"
        sep_b = render table border widths ascii plain "├" "┼" "┤" "─"
        bot_b = render table border widths ascii plain "└" "┴" "┘" "─"
        hdr_line = render table row fmt_hdrs aligns widths ascii plain True
        row_lines = render table body rows fmt_rows aligns widths ascii plain []
        join with ([top_b, hdr_line, sep_b] :: row_lines :: [bot_b]) (char from code 10)

fn :: format row cells [String] Bool [String] => [String]
      format row cells [] _ acc = acc
      format row cells (c:rest) plain acc =
        format row cells rest plain (acc :: [format inline c plain plain])

fn :: format all rows [[String]] Bool [[String]] => [[String]]
      format all rows [] _ acc = acc
      format all rows (r:rest) plain acc =
        format all rows rest plain (acc :: [format row cells r plain []])

fn :: compute table widths Int Int [String] [[String]] [Int] => [Int]
      compute table widths col_i num_cols hdrs rows acc =
        | col_i >= num_cols = acc
        | otherwise         =
          h_w = visible len (str list nth hdrs col_i)
          max_w = max col in rows rows col_i (max int 3 h_w)
          (compute table widths (col_i + 1) num_cols hdrs rows (acc :: [max_w]))

fn :: max col in rows [[String]] Int Int => Int
      max col in rows [] _ best = best
      max col in rows (r:rest) col_i best =
        w = visible len (str list nth r col_i)
        max col in rows rest col_i (max int best w)

fn :: render table border [Int] Bool Bool String String String String => String
      render table border widths ascii plain u_left u_mid u_right u_horz =
        || select box char ascii "+" u_left  -> l_ch
        || select box char ascii "+" u_mid   -> m_ch
        || select box char ascii "+" u_right -> r_ch
        || select box char ascii "-" u_horz  -> h_ch ||.
        segs = build border segments widths h_ch []
        (ansi "2;37" plain) + l_ch + (join with segs m_ch) + r_ch + (reset ansi plain)

fn :: build border segments [Int] String [String] => [String]
      build border segments [] _ acc = acc
      build border segments (w:rest) h_ch acc =
        build border segments rest h_ch (acc :: [repeat str h_ch (w + 2)])

fn :: align cell text String Int ColAlign => String
      align cell text cell width AlignLeft =
        pad = max int 0 (width - (visible len cell))
        cell + (repeat str " " pad)
      align cell text cell width AlignRight =
        pad = max int 0 (width - (visible len cell))
        (repeat str " " pad) + cell
      align cell text cell width AlignCenter =
        pad = max int 0 (width - (visible len cell))
        left_p = pad / 2
        right_p = pad - left_p
        (repeat str " " left_p) + cell + (repeat str " " right_p)

fn :: render table row [String] [ColAlign] [Int] Bool Bool Bool => String
      render table row cells aligns widths ascii plain is_hdr =
        || (ansi "2;37" plain) + (select box char ascii "|" "│") + (reset ansi plain) -> v_ch
        || render row cells 0 (list len widths) cells aligns widths plain is_hdr []   -> padded ||.
        v_ch + " " + (join with padded (" " + v_ch + " ")) + " " + v_ch

fn :: render row cells Int Int [String] [ColAlign] [Int] Bool Bool [String] => [String]
      render row cells idx total cells aligns widths plain is_hdr acc =
        | idx >= total = acc
        | otherwise    =
          raw_c = str list nth cells idx
          al = align list nth aligns idx
          w = int list nth widths idx
          aligned = align cell text raw_c w al
          styled =
            | is_hdr    = (ansi "1;36" plain) + aligned + (reset ansi plain)
            | otherwise = aligned |.
          (render row cells (idx + 1) total cells aligns widths plain is_hdr (acc :: [styled]))

fn :: render table body rows [[String]] [ColAlign] [Int] Bool Bool [String] => [String]
      render table body rows [] _ _ _ _ acc = acc
      render table body rows (r:rest) aligns widths ascii plain acc =
        line = render table row r aligns widths ascii plain False
        render table body rows rest aligns widths ascii plain (acc :: [line])
