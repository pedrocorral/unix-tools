-- md_lib.rg — Pure Ridgun Terminal Markdown Parser, Formatter & Reader (mdrr)
-- @implements FR-FILTER-003

-- ============================================================================
-- Algebraic Data Types & Configuration Record
-- ============================================================================

type ColAlign = AlignLeft | AlignCenter | AlignRight

type MdListEntry = mle_ind: Int, mle_kind: String, mle_txt: String

type MdBlock = BFrontMatter [String] | BHeading Int String | BParagraph [String] | BCode String [String] | BTable [String] [ColAlign] [[String]] | BQuote String [String] | BListItem Int String String | BHr

type MdSpec = md_plain: Bool, md_ascii: Bool, md_line_nums: Bool, md_toc_only: Bool, md_no_frontmatter: Bool, md_width: Int, md_section: String, md_files: [String]

-- ============================================================================
-- Foundational Polymorphic & String Helpers
-- ============================================================================

fn :: list_len [A]    => Int
      list_len []     =  0
      list_len (_:xs) =  1 + (list_len xs)

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

fn :: max_int Int Int => Int
      max_int a   b   =
        | a >= b    = a
        | otherwise = b

fn :: is_ws Int => Bool
      is_ws c   =  (c == 32) or (c == 9) or (c == 10) or (c == 13)

fn :: trim_ws String => String
      trim_ws s      =
        left = trim_left_idx s 0 (str_len s)
        right = trim_right_idx s left (str_len s)
        str_slice s left right

fn :: trim_left_idx String Int Int => Int
      trim_left_idx s      idx len =
        | idx >= len                = len
        | is_ws (str_char_at s idx) = trim_left_idx s (idx + 1) len
        | otherwise                 = idx

fn :: trim_right_idx String Int  Int   => Int
      trim_right_idx s      left right =
        | right <= left                     = left
        | is_ws (str_char_at s (right - 1)) = trim_right_idx s left (right - 1)
        | otherwise                         = right

fn :: repeat_str String Int => String
      repeat_str s      n   =
        | n <= 0    = ""
        | otherwise = s + (repeat_str s (n - 1))

fn :: join_with [String] String => String
      join_with []       _      =  ""
      join_with (x:[])   _      =  x
      join_with (x:xs)   sep    =  x + sep + (join_with xs sep)

fn :: str_list_nth [String] Int => String
      str_list_nth []       _   =  ""
      str_list_nth (x:xs)   n   =
        | n <= 0    = x
        | otherwise = str_list_nth xs (n - 1)

fn :: align_list_nth [ColAlign] Int => ColAlign
      align_list_nth []         _   =  AlignLeft
      align_list_nth (x:xs)     n   =
        | n <= 0    = x
        | otherwise = align_list_nth xs (n - 1)

fn :: int_list_nth [Int]  Int => Int
      int_list_nth []     _   =  0
      int_list_nth (x:xs) n   =
        | n <= 0    = x
        | otherwise = int_list_nth xs (n - 1)

fn :: list_contains_str [String] String => Bool
      list_contains_str []       _      =  False
      list_contains_str (x:xs)   target =
        | x == target = True
        | otherwise   = list_contains_str xs target

fn :: to_lower_char Int => Int
      to_lower_char c   =
        | (c >= 65) and (c <= 90) = c + 32
        | otherwise               = c

fn :: to_lower_str String => String
      to_lower_str s      =  to_lower_loop s 0 (str_len s) ""

fn :: to_lower_loop String Int Int String => String
      to_lower_loop s      idx len acc    =
        | idx >= len = acc
        | otherwise  =
          ch = char_from_code (to_lower_char (str_char_at s idx))
          to_lower_loop s (idx + 1) len (acc + ch)

fn :: str_contains_ci String String => Bool
      str_contains_ci hay    needle =  str_contains h n
        || to_lower_str hay         -> h
        || to_lower_str needle      -> n

op :: split lines String => [String]
      split lines s      =
        | (str_len s) == 0 = []
        | otherwise        = split_lines_loop s 0 (str_len s) "" []

fn :: split_lines_loop String Int Int String [String] => [String]
      split_lines_loop s idx len cur acc =
        | idx >= len                = finalize_lines cur acc
        | (str_char_at s idx) == 10 = split_lines_loop s (idx + 1) len "" (acc :: [strip_cr cur])
        | otherwise                 = split_lines_loop s (idx + 1) len (cur + (str_slice s idx (idx + 1))) acc

fn :: strip_cr String => String
      strip_cr s      =
        | (str_len s) == 0                        = s
        | (str_char_at s ((str_len s) - 1)) == 13 = str_slice s 0 ((str_len s) - 1)
        | otherwise                               = s

fn :: finalize_lines String [String] => [String]
      finalize_lines cur    acc      =
        | (str_len cur) == 0 = acc
        | otherwise          = acc :: [strip_cr cur]

op :: split whitespace String => [String]
      split whitespace s      =  split_ws_loop s 0 (str_len s) "" []

fn :: split_ws_loop String Int Int String [String] => [String]
      split_ws_loop s idx len cur acc =
        | idx >= len                = push_nonempty cur acc
        | is_ws (str_char_at s idx) = split_ws_loop s (idx + 1) len "" (push_nonempty cur acc)
        | otherwise                 = split_ws_loop s (idx + 1) len (cur + (str_slice s idx (idx + 1))) acc

fn :: push_nonempty String [String] => [String]
      push_nonempty cur    acc      =
        | (str_len cur) == 0 = acc
        | otherwise          = acc :: [cur]

-- ============================================================================
-- 1. CLI Argument Parsing — FR-FILTER-003
-- ============================================================================

fn :: default_md_spec => MdSpec
      default_md_spec =  MdSpec False False False False False 80 "" []

op :: parse mdrr args [String] => MdSpec
      parse mdrr args args     =  parse_mdrr_loop args default_md_spec

fn :: parse_mdrr_loop [String]   MdSpec => MdSpec
      parse_mdrr_loop []         spec   =  spec
      parse_mdrr_loop (arg:rest) spec   =
        | (arg == "-p") or (arg == "--plain") or (arg == "--no-color") = (parse_mdrr_loop rest (spec with md_plain: True))
        | (arg == "-a") or (arg == "--ascii")                          = (parse_mdrr_loop rest (spec with md_ascii: True))
        | (arg == "-n") or (arg == "--line-numbers")                   = (parse_mdrr_loop rest (spec with md_line_nums: True))
        | (arg == "-t") or (arg == "--toc")                            = (parse_mdrr_loop rest (spec with md_toc_only: True))
        | (arg == "-H") or (arg == "--no-frontmatter")                 = (parse_mdrr_loop rest (spec with md_no_frontmatter: True))
        | (arg == "-w") or (arg == "--width")                          = parse_mdrr_width rest spec
        | (arg == "-s") or (arg == "--section")                        = parse_mdrr_section rest spec
        | arg == "--"                                                  = spec with md_files: (md_files spec) :: rest
        | otherwise                                                    = (parse_mdrr_loop rest (spec with md_files: (md_files spec) :: [arg]))

fn :: parse_mdrr_width [String] MdSpec => MdSpec
      parse_mdrr_width []       spec   =  spec
      parse_mdrr_width (w:rest) spec   =
        w_val = max_int 20 (str_to_int w)
        parse_mdrr_loop rest (spec with md_width: w_val)

fn :: parse_mdrr_section [String]   MdSpec => MdSpec
      parse_mdrr_section []         spec   =  spec
      parse_mdrr_section (sec:rest) spec   =
        parse_mdrr_loop rest (spec with md_section: sec)

-- ============================================================================
-- 2. ANSI SGR Styling & Visible Display Width Measurement
-- ============================================================================

fn :: ansi String Bool  => String
      ansi _      True  =  ""
      ansi code   False =  (char_from_code 27) + "[" + code + "m"

fn :: ansi_reset Bool  => String
      ansi_reset plain =  ansi "0" plain

fn :: visible_len String => Int
      visible_len s      =  visible_len_loop s 0 (str_len s) False 0

fn :: visible_len_loop String Int Int Bool Int => Int
      visible_len_loop s idx len in_esc count =
        | idx >= len                                                  = count
        | in_esc and ((str_char_at s idx) == 109)                     = (visible_len_loop s (idx + 1) len False count)
        | in_esc                                                      = (visible_len_loop s (idx + 1) len True count)
        | (str_char_at s idx) == 27                                   = (visible_len_loop s (idx + 1) len True count)
        | ((str_char_at s idx) >= 128) and ((str_char_at s idx) < 192) = (visible_len_loop s (idx + 1) len False count)
        | otherwise                                                   = (visible_len_loop s (idx + 1) len False (count + 1))

-- ============================================================================
-- 3. Inline Markdown Parser & Renderer (**bold**, *italic*, `code`, [link](url))
-- ============================================================================

op :: format inline String plain Bool     => String
      format inline s      plain is_plain =
        format_inline_loop s 0 (str_len s) is_plain ""

fn :: format_inline_loop String Int Int Bool String => String
      format_inline_loop s idx len plain acc =
        | idx >= len                                        = acc
        | ((str_char_at s idx) == 92) and ((idx + 1) < len) = (format_inline_loop s (idx + 2) len plain (acc + (str_slice s (idx + 1) (idx + 2))))
        | ((idx + 1) < len) and ((str_slice s idx (idx + 2)) == "``") =
          format_inline_code_delim s (idx + 2) len "``" (find_substr_idx s (idx + 2) len "``") plain acc
        | (str_char_at s idx) == 96 =
          format_inline_code_delim s (idx + 1) len "`" (find_substr_idx s (idx + 1) len "`") plain acc
        | ((idx + 1) < len) and ((str_slice s idx (idx + 2)) == "![") =
          format_inline_image s (idx + 2) len (find_substr_idx s (idx + 2) len "](") plain acc
        | (str_char_at s idx) == 91 =
          format_inline_link s (idx + 1) len (find_substr_idx s (idx + 1) len "](") plain acc
        | ((idx + 2) < len) and ((str_slice s idx (idx + 3)) == "***") =
          format_inline_delim s (idx + 3) len "***" "1;3" "22;23" (find_delim_outside_code s (idx + 3) len "***" False) plain acc
        | ((idx + 1) < len) and ((str_slice s idx (idx + 2)) == "**") =
          format_inline_delim s (idx + 2) len "**" "1" "22" (find_delim_outside_code s (idx + 2) len "**" False) plain acc
        | ((idx + 1) < len) and ((str_slice s idx (idx + 2)) == "__") =
          format_inline_delim s (idx + 2) len "__" "1" "22" (find_delim_outside_code s (idx + 2) len "__" False) plain acc
        | ((idx + 1) < len) and ((str_slice s idx (idx + 2)) == "~~") =
          format_inline_delim s (idx + 2) len "~~" "9" "29" (find_delim_outside_code s (idx + 2) len "~~" False) plain acc
        | (str_char_at s idx) == 42 =
          format_inline_delim s (idx + 1) len "*" "3" "23" (find_delim_outside_code s (idx + 1) len "*" False) plain acc
        | is_italic_underscore s idx len =
          format_inline_delim s (idx + 1) len "_" "3" "23" (find_delim_outside_code s (idx + 1) len "_" False) plain acc
        | otherwise =
          (format_inline_loop s (idx + 1) len plain (acc + (str_slice s idx (idx + 1))))

fn :: is_alnum_ascii Int => Bool
      is_alnum_ascii c   =
        ((c >= 48) and (c <= 57)) or ((c >= 65) and (c <= 90)) or ((c >= 97) and (c <= 122))

fn :: is_italic_underscore String Int Int => Bool
      is_italic_underscore s      idx _   =
        | (str_char_at s idx) != 95                                = False
        | (idx > 0) and (is_alnum_ascii (str_char_at s (idx - 1))) = False
        | otherwise                                                = True

fn :: find_substr_idx String Int Int String => Int
      find_substr_idx s      idx len pat    =
        plen = str_len pat
        find_substr_loop s idx len pat plen

fn :: find_substr_loop String Int Int String Int => Int
      find_substr_loop s idx len pat plen =
        | (idx + plen) > len                    = 0 - 1
        | (str_slice s idx (idx + plen)) == pat = idx
        | otherwise                             = find_substr_loop s (idx + 1) len pat plen

fn :: find_delim_outside_code String Int Int String Bool => Int
      find_delim_outside_code s idx len pat in_code =
        | (idx + (str_len pat)) > len                                        = 0 - 1
        | (str_char_at s idx) == 96                                          = (find_delim_outside_code s (idx + 1) len pat (not in_code))
        | (not in_code) and ((str_slice s idx (idx + (str_len pat))) == pat) = idx
        | otherwise                                                          = find_delim_outside_code s (idx + 1) len pat in_code

fn :: format_inline_code_delim String Int Int String Int Bool String => String
      format_inline_code_delim s idx len delim end_i plain acc =
        | end_i < 0 = format_inline_loop s idx len plain (acc + delim)
        | plain     =
          dlen = str_len delim
          code_txt = trim_ws (str_slice s idx end_i)
          format_inline_loop s (end_i + dlen) len plain (acc + "`" + code_txt + "`")
        | otherwise =
          dlen = str_len delim
          code_txt = trim_ws (str_slice s idx end_i)
          styled = (ansi "36;1" False) + code_txt + (ansi "22;39" False)
          format_inline_loop s (end_i + dlen) len plain (acc + styled)

fn :: format_inline_delim String Int Int String String String Int Bool String => String
      format_inline_delim s idx len delim open_sgr close_sgr end_i plain acc =
        | end_i < 0 = format_inline_loop s idx len plain (acc + delim)
        | otherwise =
          dlen = str_len delim
          inner_raw = str_slice s idx end_i
          inner_fmt = format inline inner_raw plain plain
          styled = (ansi open_sgr plain) + inner_fmt + (ansi close_sgr plain)
          format_inline_loop s (end_i + dlen) len plain (acc + styled)

fn :: format_inline_link String Int Int Int Bool String => String
      format_inline_link s idx len close_b plain acc =
        | close_b < 0 = format_inline_loop s idx len plain (acc + "[")
        | otherwise   =
          | (find_substr_idx s (close_b + 2) len ")") < 0 = format_inline_loop s idx len plain (acc + "[")
          | otherwise =
            url_start = close_b + 2
            close_p = find_substr_idx s url_start len ")"
            label = format inline (str_slice s idx close_b) plain plain
            url = str_slice s url_start close_p
            rendered = render_link_span label url plain
            format_inline_loop s (close_p + 1) len plain (acc + rendered)

fn :: render_link_span String String Bool  => String
      render_link_span label  url    True  =  label + " (" + url + ")"
      render_link_span label  url    False =
        (ansi "4;34;1" False) + label + (ansi "0" False) + " " + (ansi "2;37" False) + "(" + url + ")" + (ansi "0" False)

fn :: format_inline_image String Int Int Int Bool String => String
      format_inline_image s idx len close_b plain acc =
        | close_b < 0 = format_inline_loop s idx len plain (acc + "![")
        | otherwise   =
          | (find_substr_idx s (close_b + 2) len ")") < 0 = format_inline_loop s idx len plain (acc + "![")
          | otherwise =
            url_start = close_b + 2
            close_p = find_substr_idx s url_start len ")"
            alt = str_slice s idx close_b
            src = str_slice s url_start close_p
            badge = (ansi "35;1" plain) + "[IMAGE: " + alt + " (" + src + ")]" + (ansi_reset plain)
            format_inline_loop s (close_p + 1) len plain (acc + badge)

-- ============================================================================
-- 4. Block-Level Markdown Parser (`parse markdown String => [MdBlock]`)
-- ============================================================================

op :: parse markdown String => [MdBlock]
      parse markdown raw    =
        lines = split lines raw
        parse_frontmatter_or_blocks lines

fn :: parse_frontmatter_or_blocks [String]     => [MdBlock]
      parse_frontmatter_or_blocks []           =  []
      parse_frontmatter_or_blocks (first:rest) =
        | (trim_ws first) == "---" =
          fm_res = collect_frontmatter rest []
          prepend_frontmatter (snd3 fm_res) (fst3 fm_res) (trd3 fm_res) rest
        | otherwise =
          parse_blocks_loop ([first] :: rest) [] []

fn :: prepend_frontmatter Bool  [String] [String] [String]  => [MdBlock]
      prepend_frontmatter False _        _        orig_rest =
        parse_blocks_loop (["---"] :: orig_rest) [] []
      prepend_frontmatter True  fm_lines rem      _         =
        [BFrontMatter fm_lines] :: (parse_blocks_loop rem [] [])

fn :: collect_frontmatter [String]    [String] => ([String], Bool, [String])
      collect_frontmatter []          acc      =  (acc, False, [])
      collect_frontmatter (line:rest) acc      =
        | ((trim_ws line) == "---") or ((trim_ws line) == "...") = (acc, True, rest)
        | otherwise                                              = (collect_frontmatter rest (acc :: [line]))

fn :: flush_para [String]   [MdBlock] => [MdBlock]
      flush_para []         blocks    =  blocks
      flush_para para_lines blocks    =  blocks :: [BParagraph para_lines]

fn :: parse_blocks_loop [String]    [String] [MdBlock] => [MdBlock]
      parse_blocks_loop []          cur_para blocks    =  flush_para cur_para blocks
      parse_blocks_loop (line:rest) cur_para blocks    =
        | (str_len (trim_ws line)) == 0 = (parse_blocks_loop rest [] (flush_para cur_para blocks))
        | is_fence_start (trim_ws line) =
          b1 = flush_para cur_para blocks
          fence_res = parse_fenced_code (trim_ws line) rest []
          (parse_blocks_loop (snd fence_res) [] (b1 :: [fst fence_res]))
        | is_hr_line (trim_ws line) =
          (parse_blocks_loop rest [] ((flush_para cur_para blocks) :: [BHr]))
        | is_atx_heading (trim_ws line) =
          (parse_blocks_loop rest [] ((flush_para cur_para blocks) :: [parse_atx_heading (trim_ws line)]))
        | str_starts_with (trim_ws line) ">" =
          b1 = flush_para cur_para blocks
          q_res = collect_blockquote ([trim_ws line] :: rest) []
          (parse_blocks_loop (snd q_res) [] (b1 :: [build_quote_block (fst q_res)]))
        | is_table_start (trim_ws line) rest =
          b1 = flush_para cur_para blocks
          t_res = parse_gfm_table (trim_ws line) rest
          (parse_blocks_loop (snd t_res) [] (b1 :: [fst t_res]))
        | is_list_line line =
          (parse_blocks_loop rest [] ((flush_para cur_para blocks) :: [parse_list_item line]))
        | otherwise =
          (parse_blocks_loop rest (cur_para :: [trim_ws line]) blocks)

fn :: is_fence_start String => Bool
      is_fence_start t      =  (str_starts_with t "```") or (str_starts_with t "~~~")

fn :: parse_fenced_code String    [String]    [String] => (MdBlock, [String])
      parse_fenced_code open_line []          acc      =
        lang = trim_ws (str_slice open_line 3 (str_len open_line))
        (BCode lang acc, [])
      parse_fenced_code open_line (line:rest) acc      =
        | str_starts_with (trim_ws line) (str_slice open_line 0 3) =
          lang = trim_ws (str_slice open_line 3 (str_len open_line))
          (BCode lang acc, rest)
        | otherwise =
          (parse_fenced_code open_line rest (acc :: [line]))

fn :: is_hr_line String => Bool
      is_hr_line t      =
        | (t == "---") or (t == "***") or (t == "___") or (t == "- - -") or (t == "* * *") = True
        | ((str_len t) >= 3) and (all_same_char t 0 (str_len t) 45)                        = True
        | ((str_len t) >= 3) and (all_same_char t 0 (str_len t) 42)                        = True
        | otherwise                                                                        = False

fn :: all_same_char String Int Int Int      => Bool
      all_same_char s      idx len target_c =
        | idx >= len                      = True
        | (str_char_at s idx) == target_c = all_same_char s (idx + 1) len target_c
        | otherwise                       = False

fn :: count_leading_hashes String Int Int => Int
      count_leading_hashes s      idx len =
        | idx >= len                = idx
        | (str_char_at s idx) == 35 = count_leading_hashes s (idx + 1) len
        | otherwise                 = idx

fn :: is_atx_heading String => Bool
      is_atx_heading t      =
        n = count_leading_hashes t 0 (str_len t)
        (n >= 1) and (n <= 6) and ((n < (str_len t)) and ((str_char_at t n) == 32))

fn :: parse_atx_heading String => MdBlock
      parse_atx_heading t      =
        n = count_leading_hashes t 0 (str_len t)
        raw_title = trim_ws (str_slice t n (str_len t))
        BHeading n (strip_trailing_hashes raw_title)

fn :: strip_trailing_hashes String => String
      strip_trailing_hashes s      =
        | (str_len s) == 0                        = s
        | (str_char_at s ((str_len s) - 1)) == 35 = (strip_trailing_hashes (trim_ws (str_slice s 0 ((str_len s) - 1))))
        | otherwise                               = s

fn :: strip_quote_prefix String => String
      strip_quote_prefix t      =
        | str_starts_with t "> " = str_slice t 2 (str_len t)
        | str_starts_with t ">"  = str_slice t 1 (str_len t)
        | otherwise              = t

fn :: collect_blockquote [String]    [String] => ([String], [String])
      collect_blockquote []          acc      =  (acc, [])
      collect_blockquote (line:rest) acc      =
        | str_starts_with (trim_ws line) ">" = (collect_blockquote rest (acc :: [strip_quote_prefix (trim_ws line)]))
        | otherwise                          = (acc, [line] :: rest)

fn :: build_quote_block [String]     => MdBlock
      build_quote_block []           =  BQuote "" []
      build_quote_block (first:rest) =
        | str_starts_with (trim_ws first) "[!NOTE]" =
          tf = trim_ws first
          BQuote "NOTE" (prepend_nonempty (trim_ws (str_slice tf 7 (str_len tf))) rest)
        | str_starts_with (trim_ws first) "[!TIP]" =
          tf = trim_ws first
          BQuote "TIP" (prepend_nonempty (trim_ws (str_slice tf 6 (str_len tf))) rest)
        | str_starts_with (trim_ws first) "[!IMPORTANT]" =
          tf = trim_ws first
          BQuote "IMPORTANT" (prepend_nonempty (trim_ws (str_slice tf 12 (str_len tf))) rest)
        | str_starts_with (trim_ws first) "[!WARNING]" =
          tf = trim_ws first
          BQuote "WARNING" (prepend_nonempty (trim_ws (str_slice tf 10 (str_len tf))) rest)
        | str_starts_with (trim_ws first) "[!CAUTION]" =
          tf = trim_ws first
          BQuote "CAUTION" (prepend_nonempty (trim_ws (str_slice tf 10 (str_len tf))) rest)
        | otherwise =
          BQuote "" ([first] :: rest)

fn :: prepend_nonempty String [String] => [String]
      prepend_nonempty ""     xs       =  xs
      prepend_nonempty s      xs       =  [s] :: xs

-- ============================================================================
-- 5. GFM Table Parser (`| Col 1 | Col 2 |` + `| :--- | ---: |`)
-- ============================================================================

fn :: is_table_start String      [String]     => Bool
      is_table_start _           []           =  False
      is_table_start header_line (sep_line:_) =
        (has_pipe_char header_line) and (is_table_delimiter_row (trim_ws sep_line))

fn :: has_pipe_char String => Bool
      has_pipe_char s      =  str_contains s "|"

fn :: is_table_delimiter_row String => Bool
      is_table_delimiter_row s      =
        | not (has_pipe_char s) = False
        | otherwise             =
          cells = split_table_cells s
          ((list_len cells) >= 1) and (all_cells_are_delims cells)

fn :: all_cells_are_delims [String] => Bool
      all_cells_are_delims []       =  True
      all_cells_are_delims (c:rest) =
        | is_delim_cell (trim_ws c) = all_cells_are_delims rest
        | otherwise                 = False

fn :: is_delim_cell String => Bool
      is_delim_cell c      =
        | (str_len c) == 0 = False
        | otherwise        = is_delim_chars c 0 (str_len c) False

fn :: is_delim_chars String Int Int Bool      => Bool
      is_delim_chars c      idx len seen_dash =
        | idx >= len                = seen_dash
        | (str_char_at c idx) == 45 = is_delim_chars c (idx + 1) len True
        | (str_char_at c idx) == 58 = is_delim_chars c (idx + 1) len seen_dash
        | otherwise                 = False

fn :: split_table_cells String => [String]
      split_table_cells line   =
        stripped = strip_outer_pipes (trim_ws line)
        split_pipe_loop stripped 0 (str_len stripped) "" []

fn :: strip_outer_pipes String => String
      strip_outer_pipes s      =
        s1 = strip_leading_pipe s
        strip_trailing_pipe s1

fn :: strip_leading_pipe String => String
      strip_leading_pipe s      =
        | str_starts_with s "|" = str_slice s 1 (str_len s)
        | otherwise             = s

fn :: strip_trailing_pipe String => String
      strip_trailing_pipe s      =
        | ((str_len s) > 0) and ((str_char_at s ((str_len s) - 1)) == 124) = str_slice s 0 ((str_len s) - 1)
        | otherwise                                                        = s

fn :: split_pipe_loop String Int Int String [String] => [String]
      split_pipe_loop s idx len cur acc =
        | idx >= len = acc :: [trim_ws cur]
        | ((str_char_at s idx) == 92) and (((idx + 1) < len) and ((str_char_at s (idx + 1)) == 124)) =
          (split_pipe_loop s (idx + 2) len (cur + "|") acc)
        | (str_char_at s idx) == 124 =
          (split_pipe_loop s (idx + 1) len "" (acc :: [trim_ws cur]))
        | otherwise =
          (split_pipe_loop s (idx + 1) len (cur + (str_slice s idx (idx + 1))) acc)

fn :: parse_col_aligns [String] [ColAlign] => [ColAlign]
      parse_col_aligns []       acc        =  acc
      parse_col_aligns (c:rest) acc        =
        tc = trim_ws c
        left_col = str_starts_with tc ":"
        right_col = ((str_len tc) > 0) and ((str_char_at tc ((str_len tc) - 1)) == 58)
        align =
          | left_col and right_col = AlignCenter
          | right_col              = AlignRight
          | otherwise              = AlignLeft |.
        parse_col_aligns rest (acc :: [align])

fn :: parse_gfm_table String      [String]        => (MdBlock, [String])
      parse_gfm_table header_line []              =  (BTable (split_table_cells header_line) [] [], [])
      parse_gfm_table header_line (sep_line:rest) =
        || split_table_cells header_line                    -> headers
        || parse_col_aligns (split_table_cells sep_line) [] -> aligns
        || collect_table_rows rest []                       -> rows_res ||.
        (BTable headers aligns (fst rows_res), snd rows_res)

fn :: collect_table_rows [String]    [[String]] => ([[String]], [String])
      collect_table_rows []          acc        =  (acc, [])
      collect_table_rows (line:rest) acc        =
        | ((str_len (trim_ws line)) > 0) and (has_pipe_char (trim_ws line)) =
          (collect_table_rows rest (acc :: [split_table_cells (trim_ws line)]))
        | otherwise =
          (acc, [line] :: rest)

-- ============================================================================
-- 6. List & GFM Task List Parser (`-`, `*`, `+`, `1.`, `- [ ]`, `- [x]`)
-- ============================================================================

fn :: count_indent String Int Int => Int
      count_indent s      idx len =
        | idx >= len                = idx
        | (str_char_at s idx) == 32 = count_indent s (idx + 1) len
        | (str_char_at s idx) == 9  = count_indent s (idx + 1) len
        | otherwise                 = idx

fn :: is_list_line String => Bool
      is_list_line line   =
        | (str_starts_with (trim_ws line) "- ") or (str_starts_with (trim_ws line) "* ") or (str_starts_with (trim_ws line) "+ ") = True
        | otherwise =
          t = trim_ws line
          (ordered_marker_len t 0 (str_len t)) > 0

fn :: ordered_marker_len String Int Int => Int
      ordered_marker_len t      idx len =
        | idx >= len                                                  = 0
        | ((str_char_at t idx) >= 48) and ((str_char_at t idx) <= 57) = (ordered_marker_len t (idx + 1) len)
        | (idx > 0) and (((str_char_at t idx) == 46) or ((str_char_at t idx) == 41)) and (((idx + 1) < len) and ((str_char_at t (idx + 1)) == 32)) = idx + 1
        | otherwise                                                   = 0

fn :: parse_list_item String => MdBlock
      parse_list_item line   =
        | (str_starts_with (trim_ws line) "- [ ] ") or (str_starts_with (trim_ws line) "* [ ] ") =
          indent = count_indent line 0 (str_len line)
          t = trim_ws line
          BListItem indent "todo" (str_slice t 6 (str_len t))
        | (str_starts_with (trim_ws line) "- [x] ") or (str_starts_with (trim_ws line) "- [X] ") or (str_starts_with (trim_ws line) "* [x] ") or (str_starts_with (trim_ws line) "* [X] ") =
          indent = count_indent line 0 (str_len line)
          t = trim_ws line
          BListItem indent "done" (str_slice t 6 (str_len t))
        | (str_starts_with (trim_ws line) "- ") or (str_starts_with (trim_ws line) "* ") or (str_starts_with (trim_ws line) "+ ") =
          indent = count_indent line 0 (str_len line)
          t = trim_ws line
          BListItem indent "bullet" (str_slice t 2 (str_len t))
        | otherwise =
          indent = count_indent line 0 (str_len line)
          t = trim_ws line
          mlen = ordered_marker_len t 0 (str_len t)
          marker = str_slice t 0 mlen
          body = trim_ws (str_slice t mlen (str_len t))
          BListItem indent marker body

-- ============================================================================
-- 7. Section Filtering (`-s`) & Table of Contents (`-t`) — FR-FILTER-003
-- ============================================================================

op :: extract section [MdBlock] matching String     => [MdBlock]
      extract section blocks    matching ""         =  blocks
      extract section blocks    matching target_sec =
        filter_section_loop blocks target_sec False 0 []

fn :: filter_section_loop [MdBlock] String Bool Int [MdBlock] => [MdBlock]
      filter_section_loop [] _ _ _ acc = acc
      filter_section_loop ((BHeading lvl title):rest) target_sec False _ acc =
        | str_contains_ci title target_sec = (filter_section_loop rest target_sec True lvl (acc :: [BHeading lvl title]))
        | otherwise                        = (filter_section_loop rest target_sec False 0 acc)
      filter_section_loop ((BHeading lvl title):rest) target_sec True start_lvl acc =
        | lvl <= start_lvl = acc
        | otherwise        = (filter_section_loop rest target_sec True start_lvl (acc :: [BHeading lvl title]))
      filter_section_loop (_:rest) target_sec False start_lvl acc =
        filter_section_loop rest target_sec False start_lvl acc
      filter_section_loop (b:rest) target_sec True start_lvl acc =
        filter_section_loop rest target_sec True start_lvl (acc :: [b])

op :: render toc [MdBlock] with MdSpec => String
      render toc blocks    with spec   =
        lines = collect_toc_lines blocks spec []
        res =
          | (list_len lines) == 0 = ""
          | otherwise             = (join_with lines (char_from_code 10)) + (char_from_code 10)
        res

fn :: collect_toc_lines [MdBlock] MdSpec [String] => [String]
      collect_toc_lines []        _      acc      =  acc
      collect_toc_lines ((BHeading lvl title):rest) spec acc =
        indent = repeat_str "  " (lvl - 1)
        bullet = select_toc_bullet lvl (md_ascii spec)
        clean_title = format inline title plain (md_plain spec)
        styled = indent + (ansi "36;1" (md_plain spec)) + bullet + (ansi_reset (md_plain spec)) + " " + clean_title
        collect_toc_lines rest spec (acc :: [styled])
      collect_toc_lines (_:rest)  spec   acc      =
        collect_toc_lines rest spec acc

fn :: select_toc_bullet Int Bool  => String
      select_toc_bullet _   True  =  "*"
      select_toc_bullet 1   False =  "▸"
      select_toc_bullet 2   False =  "•"
      select_toc_bullet _   False =  "◦"

-- ============================================================================
-- 8. Visible-Width Word Wrapper & Block Renderer
-- ============================================================================

fn :: wrap_ansi_words [String] Int String String String Int [String] => [String]
      wrap_ansi_words [] _ _ _ cur_line _ acc =
        | (str_len cur_line) == 0 = acc
        | otherwise               = acc :: [cur_line]
      wrap_ansi_words (w:rest) max_w first_pref cont_pref cur_line cur_vis acc =
        | cur_vis == 0 =
          w_vis = visible_len w
          pref_vis = visible_len first_pref
          (wrap_ansi_words rest max_w first_pref cont_pref (first_pref + w) (pref_vis + w_vis) acc)
        | (cur_vis + 1 + (visible_len w)) <= max_w =
          w_vis = visible_len w
          (wrap_ansi_words rest max_w first_pref cont_pref (cur_line + " " + w) (cur_vis + 1 + w_vis) acc)
        | otherwise =
          w_vis = visible_len w
          cont_vis = visible_len cont_pref
          (wrap_ansi_words rest max_w first_pref cont_pref (cont_pref + w) (cont_vis + w_vis) (acc :: [cur_line]))

fn :: wrap_formatted_text String Int   String     String    => [String]
      wrap_formatted_text text   max_w first_pref cont_pref =
        words = split whitespace text
        res =
          | (list_len words) == 0 = [first_pref]
          | otherwise             = wrap_ansi_words words max_w first_pref cont_pref "" 0 []
        res

-- ============================================================================
-- 9. Syntax Highlighting for Fenced Code Blocks
-- ============================================================================

fn :: is_code_keyword String => Bool
      is_code_keyword w      =
        list_contains_str ["fn", "op", "type", "with", "import", "otherwise", "and", "or", "not", "xor", "mut", "region", "map", "filter", "fold", "True", "False", "Int", "String", "Bool", "if", "then", "else", "elif", "fi", "for", "in", "do", "done", "while", "return", "let", "const", "var", "pub", "struct", "enum", "match", "def", "class", "permit", "deny"] w

fn :: is_comment_line String => Bool
      is_comment_line line   =
        t = trim_ws line
        (str_starts_with t "--") or (str_starts_with t "#") or (str_starts_with t "//")

fn :: highlight_code_line String Bool  => String
      highlight_code_line line   True  =  line
      highlight_code_line line   False =
        | is_comment_line line = (ansi "2;32" False) + line + (ansi_reset False)
        | otherwise            = highlight_tokens_loop line 0 (str_len line) "" ""

fn :: is_ident_char Int => Bool
      is_ident_char c   =  (is_alnum_ascii c) or (c == 95)

fn :: flush_ident_tok String => String
      flush_ident_tok ""     =  ""
      flush_ident_tok tok    =
        | is_code_keyword tok = (ansi "35;1" False) + tok + (ansi_reset False)
        | otherwise           = tok

fn :: highlight_tokens_loop String Int Int String String => String
      highlight_tokens_loop s idx len cur_id acc =
        | idx >= len = acc + (flush_ident_tok cur_id)
        | (str_char_at s idx) == 34 =
          acc1 = acc + (flush_ident_tok cur_id)
          str_res = read_quoted_literal s (idx + 1) len "\""
          styled_s = (ansi "33" False) + (fst str_res) + (ansi_reset False)
          (highlight_tokens_loop s (snd str_res) len "" (acc1 + styled_s))
        | is_ident_char (str_char_at s idx) =
          (highlight_tokens_loop s (idx + 1) len (cur_id + (str_slice s idx (idx + 1))) acc)
        | otherwise =
          acc1 = acc + (flush_ident_tok cur_id) + (str_slice s idx (idx + 1))
          (highlight_tokens_loop s (idx + 1) len "" acc1)

fn :: read_quoted_literal String Int Int String => (String, Int)
      read_quoted_literal s      idx len acc    =
        | idx >= len                                        = (acc, len)
        | ((str_char_at s idx) == 92) and ((idx + 1) < len) = (read_quoted_literal s (idx + 2) len (acc + (str_slice s idx (idx + 2))))
        | (str_char_at s idx) == 34                         = (acc + "\"", idx + 1)
        | otherwise                                         = (read_quoted_literal s (idx + 1) len (acc + (str_slice s idx (idx + 1))))

-- ============================================================================
-- 10. Rendering Blocks (Headings, Code Boxes, GFM Tables, Alerts, Lists)
-- ============================================================================

op :: render markdown String with MdSpec => String
      render markdown raw    with spec   =
        all_b = parse markdown raw
        blocks = extract section all_b matching (md_section spec)
        render_blocks_or_toc blocks spec

fn :: render_blocks_or_toc [MdBlock] MdSpec => String
      render_blocks_or_toc blocks    spec   =
        | md_toc_only spec = render toc blocks with spec
        | otherwise        =
          rendered = render_blocks_loop blocks spec []
          out =
            | (list_len rendered) == 0 = ""
            | otherwise                = (join_with rendered ((char_from_code 10) + (char_from_code 10))) + (char_from_code 10)
          out

fn :: render_blocks_loop [MdBlock]  MdSpec [String] => [String]
      render_blocks_loop []         _      acc      =  acc
      render_blocks_loop ((BFrontMatter fm):rest) spec acc =
        | md_no_frontmatter spec = render_blocks_loop rest spec acc
        | otherwise              = render_blocks_loop rest spec (acc :: [render_frontmatter fm spec])
      render_blocks_loop ((BHeading lvl title):rest) spec acc =
        render_blocks_loop rest spec (acc :: [render_heading lvl title spec])
      render_blocks_loop ((BParagraph lines):rest) spec acc =
        render_blocks_loop rest spec (acc :: [render_paragraph lines spec])
      render_blocks_loop ((BCode lang lines):rest) spec acc =
        render_blocks_loop rest spec (acc :: [render_code_block lang lines spec])
      render_blocks_loop ((BTable hdrs aligns rows):rest) spec acc =
        render_blocks_loop rest spec (acc :: [render_table_block hdrs aligns rows spec])
      render_blocks_loop ((BQuote kind lines):rest) spec acc =
        render_blocks_loop rest spec (acc :: [render_quote_block kind lines spec])
      render_blocks_loop ((BListItem ind kind txt):rest) spec acc =
        list_res = collect_consecutive_list ([MdListEntry ind kind txt]) rest
        items_str = render_list_items (fst list_res) spec []
        render_blocks_loop (snd list_res) spec (acc :: [items_str])
      render_blocks_loop (BHr:rest) spec   acc      =
        render_blocks_loop rest spec (acc :: [render_hr spec])

fn :: collect_consecutive_list [MdListEntry] [MdBlock] => ([MdListEntry], [MdBlock])
      collect_consecutive_list acc [] = (acc, [])
      collect_consecutive_list acc ((BListItem ind kind txt):rest) =
        collect_consecutive_list (acc :: [MdListEntry ind kind txt]) rest
      collect_consecutive_list acc other = (acc, other)

fn :: render_frontmatter [String] MdSpec => String
      render_frontmatter lines    spec   =
        || select_box_char (md_ascii spec) "-" "─" -> h_ch
        || select_box_char (md_ascii spec) "+" "╭" -> tl
        || select_box_char (md_ascii spec) "+" "╰" -> bl
        || select_box_char (md_ascii spec) "|" "│" -> v_ch ||.
        plain = md_plain spec
        top = (ansi "2;36" plain) + tl + h_ch + " front-matter " + (repeat_str h_ch 24) + (ansi_reset plain)
        bot = (ansi "2;36" plain) + bl + (repeat_str h_ch 39) + (ansi_reset plain)
        body = format_fm_lines lines v_ch plain []
        join_with ([top] :: body :: [bot]) (char_from_code 10)

fn :: format_fm_lines [String] String Bool  [String] => [String]
      format_fm_lines []       _      _     acc      =  acc
      format_fm_lines (l:rest) v_ch   plain acc      =
        line_s = (ansi "2;36" plain) + v_ch + " " + (ansi_reset plain) + (ansi "37" plain) + l + (ansi_reset plain)
        format_fm_lines rest v_ch plain (acc :: [line_s])

fn :: select_box_char Bool  String String => String
      select_box_char True  a      _      =  a
      select_box_char False _      u      =  u

fn :: render_heading Int String    MdSpec => String
      render_heading lvl raw_title spec   =
        | lvl == 1 =
          nl = char_from_code 10
          plain = md_plain spec
          ascii = md_ascii spec
          fmt_title = format inline raw_title plain plain
          rule_ch = select_box_char ascii "=" "═"
          rlen = max_int 8 (visible_len fmt_title)
          (ansi "1;36" plain) + fmt_title + (ansi_reset plain) + nl + (ansi "36" plain) + (repeat_str rule_ch rlen) + (ansi_reset plain)
        | lvl == 2 =
          nl = char_from_code 10
          plain = md_plain spec
          ascii = md_ascii spec
          fmt_title = format inline raw_title plain plain
          rule_ch = select_box_char ascii "-" "─"
          rlen = max_int 6 (visible_len fmt_title)
          (ansi "1;32" plain) + fmt_title + (ansi_reset plain) + nl + (ansi "32" plain) + (repeat_str rule_ch rlen) + (ansi_reset plain)
        | lvl == 3 =
          plain = md_plain spec
          fmt_title = format inline raw_title plain plain
          (ansi "1;33" plain) + "### " + fmt_title + (ansi_reset plain)
        | otherwise =
          plain = md_plain spec
          fmt_title = format inline raw_title plain plain
          prefix = repeat_str "#" lvl
          (ansi "1;34" plain) + prefix + " " + fmt_title + (ansi_reset plain)

fn :: render_paragraph [String] MdSpec => String
      render_paragraph lines    spec   =
        joined = join_with lines " "
        fmt = format inline joined plain (md_plain spec)
        wrapped = wrap_formatted_text fmt (md_width spec) "" ""
        join_with wrapped (char_from_code 10)

fn :: render_hr MdSpec => String
      render_hr spec   =
        ch = select_box_char (md_ascii spec) "-" "─"
        (ansi "2;37" (md_plain spec)) + (repeat_str ch (md_width spec)) + (ansi_reset (md_plain spec))

fn :: render_code_block String [String] MdSpec => String
      render_code_block lang   lines    spec   =
        || select_box_char (md_ascii spec) "-" "─" -> h_ch
        || select_box_char (md_ascii spec) "+" "╭" -> tl
        || select_box_char (md_ascii spec) "+" "╰" -> bl
        || select_box_char (md_ascii spec) "|" "│" -> v_ch
        || select_code_label lang                  -> label ||.
        plain = md_plain spec
        top = (ansi "2;36" plain) + tl + h_ch + label + (repeat_str h_ch (max_int 4 (44 - (str_len label)))) + (ansi_reset plain)
        bot = (ansi "2;36" plain) + bl + (repeat_str h_ch 45) + (ansi_reset plain)
        body = render_code_lines lines 1 (md_line_nums spec) v_ch plain []
        join_with ([top] :: body :: [bot]) (char_from_code 10)

fn :: select_code_label String => String
      select_code_label ""     =  " code "
      select_code_label lang   =  " " + lang + " "

fn :: pad_left_num Int Int   => String
      pad_left_num n   width =
        s = int_to_str n
        pad = max_int 0 (width - (str_len s))
        (repeat_str " " pad) + s

fn :: render_code_lines [String] Int Bool String Bool [String] => [String]
      render_code_lines [] _ _ _ _ acc = acc
      render_code_lines (l:rest) idx show_nums v_ch plain acc =
        || format_code_prefix idx show_nums v_ch plain -> prefix
        || highlight_code_line l plain                 -> hl ||.
        render_code_lines rest (idx + 1) show_nums v_ch plain (acc :: [prefix + hl])

fn :: format_code_prefix Int Bool  String Bool  => String
      format_code_prefix _   False v_ch   plain =
        (ansi "2;36" plain) + v_ch + " " + (ansi_reset plain)
      format_code_prefix idx True  v_ch   plain =
        num_s = pad_left_num idx 3
        (ansi "2;36" plain) + v_ch + " " + (ansi "2;37" plain) + num_s + " │ " + (ansi_reset plain)

-- ============================================================================
-- 11. Blockquotes & GitHub Alerts (`NOTE`, `TIP`, `IMPORTANT`, `WARNING`, `CAUTION`)
-- ============================================================================

fn :: alert_color_and_title String      Bool  => (String, String)
      alert_color_and_title "NOTE"      ascii =  ("34;1", (select_box_char ascii "[i] NOTE" "ℹ NOTE"))
      alert_color_and_title "TIP"       ascii =  ("32;1", (select_box_char ascii "[*] TIP" "💡 TIP"))
      alert_color_and_title "IMPORTANT" ascii =  ("35;1", (select_box_char ascii "[!] IMPORTANT" "❗ IMPORTANT"))
      alert_color_and_title "WARNING"   ascii =  ("33;1", (select_box_char ascii "[!] WARNING" "⚠ WARNING"))
      alert_color_and_title "CAUTION"   ascii =  ("31;1", (select_box_char ascii "[X] CAUTION" "🛑 CAUTION"))
      alert_color_and_title _           _     =  ("2;37", "")

fn :: render_quote_block String [String] MdSpec => String
      render_quote_block kind   lines    spec   =
        || select_box_char (md_ascii spec) "|" "┃"  -> bar_ch
        || alert_color_and_title kind (md_ascii spec) -> meta
        || join_with lines " "                      -> joined ||.
        plain = md_plain spec
        sgr = fst meta
        badge = snd meta
        bar = (ansi sgr plain) + bar_ch + " " + (ansi_reset plain)
        fmt_body = format inline joined plain plain
        wrapped = wrap_formatted_text fmt_body (max_int 20 ((md_width spec) - 4)) bar bar
        hdr = bar + (ansi sgr plain) + badge + (ansi_reset plain)
        out =
          | (str_len badge) == 0 = join_with wrapped (char_from_code 10)
          | otherwise            = join_with ([hdr] :: wrapped) (char_from_code 10)
        out

-- ============================================================================
-- 12. List & Task List Rendering
-- ============================================================================

fn :: render_list_items [MdListEntry] MdSpec [String] => String
      render_list_items []            _      acc      =  join_with acc (char_from_code 10)
      render_list_items (ent:rest)    spec   acc      =
        ind = mle_ind ent
        kind = mle_kind ent
        txt = mle_txt ent
        plain = md_plain spec
        ascii = md_ascii spec
        pad = repeat_str " " ind
        marker = format_list_marker kind ascii plain
        first_pref = pad + marker + " "
        cont_pref = pad + "  "
        fmt_txt = format inline txt plain plain
        lines = wrap_formatted_text fmt_txt (md_width spec) first_pref cont_pref
        render_list_items rest spec (acc :: lines)

fn :: format_list_marker String   Bool  Bool  => String
      format_list_marker "bullet" True  plain =  (ansi "36;1" plain) + "*" + (ansi_reset plain)
      format_list_marker "bullet" False plain =  (ansi "36;1" plain) + "•" + (ansi_reset plain)
      format_list_marker "todo"   True  plain =  (ansi "33;1" plain) + "[ ]" + (ansi_reset plain)
      format_list_marker "todo"   False plain =  (ansi "33;1" plain) + "☐" + (ansi_reset plain)
      format_list_marker "done"   True  plain =  (ansi "32;1" plain) + "[x]" + (ansi_reset plain)
      format_list_marker "done"   False plain =  (ansi "32;1" plain) + "☑" + (ansi_reset plain)
      format_list_marker num_m    _     plain =  (ansi "36;1" plain) + num_m + (ansi_reset plain)

-- ============================================================================
-- 13. GFM Table Box Rendering & Cell Alignment
-- ============================================================================

fn :: render_table_block [String] [ColAlign] [[String]] MdSpec => String
      render_table_block hdrs     aligns     rows       spec   =
        || format_row_cells hdrs (md_plain spec) [] -> fmt_hdrs
        || format_all_rows rows (md_plain spec) []  -> fmt_rows
        || list_len hdrs                            -> num_cols ||.
        plain = md_plain spec
        ascii = md_ascii spec
        widths = compute_table_widths 0 num_cols fmt_hdrs fmt_rows []
        top_b = render_table_border widths ascii plain "┌" "┬" "┐" "─"
        sep_b = render_table_border widths ascii plain "├" "┼" "┤" "─"
        bot_b = render_table_border widths ascii plain "└" "┴" "┘" "─"
        hdr_line = render_table_row fmt_hdrs aligns widths ascii plain True
        row_lines = render_table_body_rows fmt_rows aligns widths ascii plain []
        join_with ([top_b, hdr_line, sep_b] :: row_lines :: [bot_b]) (char_from_code 10)

fn :: format_row_cells [String] Bool  [String] => [String]
      format_row_cells []       _     acc      =  acc
      format_row_cells (c:rest) plain acc      =
        format_row_cells rest plain (acc :: [format inline c plain plain])

fn :: format_all_rows [[String]] Bool  [[String]] => [[String]]
      format_all_rows []         _     acc        =  acc
      format_all_rows (r:rest)   plain acc        =
        format_all_rows rest plain (acc :: [format_row_cells r plain []])

fn :: compute_table_widths Int Int [String] [[String]] [Int] => [Int]
      compute_table_widths col_i num_cols hdrs rows acc =
        | col_i >= num_cols = acc
        | otherwise         =
          h_w = visible_len (str_list_nth hdrs col_i)
          max_w = max_col_in_rows rows col_i (max_int 3 h_w)
          (compute_table_widths (col_i + 1) num_cols hdrs rows (acc :: [max_w]))

fn :: max_col_in_rows [[String]] Int   Int  => Int
      max_col_in_rows []         _     best =  best
      max_col_in_rows (r:rest)   col_i best =
        w = visible_len (str_list_nth r col_i)
        max_col_in_rows rest col_i (max_int best w)

fn :: render_table_border [Int] Bool Bool String String String String => String
      render_table_border widths ascii plain u_left u_mid u_right u_horz =
        || select_box_char ascii "+" u_left  -> l_ch
        || select_box_char ascii "+" u_mid   -> m_ch
        || select_box_char ascii "+" u_right -> r_ch
        || select_box_char ascii "-" u_horz  -> h_ch ||.
        segs = build_border_segments widths h_ch []
        (ansi "2;37" plain) + l_ch + (join_with segs m_ch) + r_ch + (ansi_reset plain)

fn :: build_border_segments [Int]    String [String] => [String]
      build_border_segments []       _      acc      =  acc
      build_border_segments (w:rest) h_ch   acc      =
        build_border_segments rest h_ch (acc :: [repeat_str h_ch (w + 2)])

fn :: align_cell_text String Int   ColAlign    => String
      align_cell_text cell   width AlignLeft   =
        pad = max_int 0 (width - (visible_len cell))
        cell + (repeat_str " " pad)
      align_cell_text cell   width AlignRight  =
        pad = max_int 0 (width - (visible_len cell))
        (repeat_str " " pad) + cell
      align_cell_text cell   width AlignCenter =
        pad = max_int 0 (width - (visible_len cell))
        left_p = pad / 2
        right_p = pad - left_p
        (repeat_str " " left_p) + cell + (repeat_str " " right_p)

fn :: render_table_row [String] [ColAlign] [Int] Bool Bool Bool => String
      render_table_row cells aligns widths ascii plain is_hdr =
        || (ansi "2;37" plain) + (select_box_char ascii "|" "│") + (ansi_reset plain) -> v_ch
        || render_row_cells 0 (list_len widths) cells aligns widths plain is_hdr []   -> padded ||.
        v_ch + " " + (join_with padded (" " + v_ch + " ")) + " " + v_ch

fn :: render_row_cells Int Int [String] [ColAlign] [Int] Bool Bool [String] => [String]
      render_row_cells idx total cells aligns widths plain is_hdr acc =
        | idx >= total = acc
        | otherwise    =
          raw_c = str_list_nth cells idx
          al = align_list_nth aligns idx
          w = int_list_nth widths idx
          aligned = align_cell_text raw_c w al
          styled =
            | is_hdr    = (ansi "1;36" plain) + aligned + (ansi_reset plain)
            | otherwise = aligned |.
          (render_row_cells (idx + 1) total cells aligns widths plain is_hdr (acc :: [styled]))

fn :: render_table_body_rows [[String]] [ColAlign] [Int] Bool Bool [String] => [String]
      render_table_body_rows [] _ _ _ _ acc = acc
      render_table_body_rows (r:rest) aligns widths ascii plain acc =
        line = render_table_row r aligns widths ascii plain False
        render_table_body_rows rest aligns widths ascii plain (acc :: [line])
