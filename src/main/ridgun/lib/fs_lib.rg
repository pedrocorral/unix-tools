-- fs_lib.rg — Pure Ridgun BSD Filesystem Library (ls, find, mtree, mktemp)
-- @implements FR-FS-001
-- @implements FR-FS-002
-- @implements FR-FS-003
-- @implements FR-FS-004

import posix

-- ============================================================================
-- Algebraic Data Types (ADTs)
-- ============================================================================

type FileKind = FReg | FDir | FSymLink | FFifo | FSock | FBlock | FChar

type FileEntry = Entry String String FileKind Int Int Int String String Int String

type LsSpec = l_one_per_line: Bool, l_multi_col: Bool, l_long: Bool, l_all: Bool, l_almost_all: Bool, l_classify: Bool, l_human: Bool, l_sort_size: Bool, l_sort_time: Bool, l_reverse: Bool, l_recursive: Bool, l_color: Bool, l_paths: [String]

type FindExpr = ETrue | EName String | EIname String | EPath String | EType String | ESize Int Int | EMaxDepth Int | EMinDepth Int

expand type FindExpr = EEmpty | ENot FindExpr | EAnd FindExpr FindExpr | EOr FindExpr FindExpr

type FindSpec = f_paths: [String], f_expr: FindExpr, f_max_depth: Int, f_min_depth: Int, f_print0: Bool, f_exec_cmd: [String]

type MtreeEntry = me_rel: String, me_type: String, me_mode: Int, me_size: Int, me_sha256: String, me_link: String

type MtreeSpec = m_create: Bool, m_spec_file: String, m_path: String, m_update: Bool, m_keywords: [String]

type MktempSpec = mkt_dir: Bool, mkt_unsafe: Bool, mkt_quiet: Bool, mkt_tmpdir: String, mkt_prefix: String, mkt_template: String

-- ============================================================================
-- Foundational String & List Helpers
-- ============================================================================

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

fn :: is ws Int => Bool
      is ws c   =  (c == 32) or (c == 9) or (c == 10) or (c == 13)

fn :: split ws loop String Int Int String [String] => [String]
      split ws loop s idx len cur acc =
        | idx >= len                = push nonempty cur acc
        | is ws (str char at s idx) = split ws loop s (idx + 1) len "" (push nonempty cur acc)
        | otherwise                 = split ws loop s (idx + 1) len (cur + (str slice s idx (idx + 1))) acc

fn :: push nonempty String [String] => [String]
      push nonempty cur    acc      =
        | (str len cur) == 0 = acc
        | otherwise          = acc :: [cur]

fn :: join with [String] String => String
      join with []       sep    =  ""
      join with (x:[])   sep    =  x
      join with (x:xs)   sep    =  x + sep + (join with xs sep)

fn :: repeat str String Int   => String
      repeat str s      count =
        | count <= 0 = ""
        | otherwise  = s + (repeat str s (count - 1))

fn :: pad left String Int   => String
      pad left s      width =
        | (str len s) >= width = s
        | otherwise            = (repeat str " " (width - (str len s))) + s

fn :: pad right String Int   => String
      pad right s      width =
        | (str len s) >= width = s
        | otherwise            = s + (repeat str " " (width - (str len s)))

fn :: max int Int Int => Int
      max int a   b   =
        | a >= b    = a
        | otherwise = b

fn :: min int Int Int => Int
      min int a   b   =
        | a <= b    = a
        | otherwise = b

fn :: str ends with String String => Bool
      str ends with s      suffix =
        slen = str len s
        suflen = str len suffix
        check suffix s suffix slen suflen

fn :: check suffix String String Int Int => Bool
      check suffix s suffix slen suflen =
        | slen < suflen = False
        | otherwise     = (str slice s (slen - suflen) slen) == suffix

fn :: str list nth [String] Int => String
      str list nth []       n   =  ""
      str list nth (x:xs)   n   =
        | n <= 0    = x
        | otherwise = str list nth xs (n - 1)

fn :: int list nth [Int]  Int => Int
      int list nth []     n   =  0
      int list nth (x:xs) n   =
        | n <= 0    = x
        | otherwise = int list nth xs (n - 1)

fn :: list len [A]    => Int
      list len []     =  0
      list len (_:xs) =  1 + (list len xs)

fn :: fst (A, B) => A
      fst (a, _) =  a

fn :: snd (A, B) => B
      snd (_, b) =  b

fn :: join_path String String => String
      join_path dir    name   =
        | (str_len dir) == 0 = name
        | dir == "." = "./" + name
        | dir == "/" = "/" + name
        | str_ends_with dir "/" = dir + name
        | otherwise = dir + "/" + name

fn :: join_clean_path String String => String
      join_clean_path dir    name   =
        | (str_len dir) == 0 = name
        | dir == "." = name
        | dir == "/" = "/" + name
        | str_ends_with dir "/" = dir + name
        | otherwise = dir + "/" + name

fn :: basename_of String => String
      basename_of path   =  basename_loop path ((str_len path) - 1)

fn :: basename_loop String Int => String
      basename_loop path   idx =
        | idx < 0 = path
        | (str_char_at path idx) == 47 = str_slice path (idx + 1) (str_len path)
        | otherwise = basename_loop path (idx - 1)

fn :: to_lower_char Int => Int
      to_lower_char c   =
        | (c >= 65) and (c <= 90) = c + 32
        | otherwise = c

fn :: to_lower_str String => String
      to_lower_str s      =  to_lower_loop s 0 (str_len s) ""

fn :: to_lower_loop String Int Int String => String
      to_lower_loop s      idx len acc    =
        | idx >= len = acc
        | otherwise = to_lower_loop s (idx + 1) len (acc + (char_from_code (to_lower_char (str_char_at s idx))))

fn :: str_cmp String String => Int
      str_cmp a      b      =  str_cmp_loop a b 0 (str_len a) (str_len b)

fn :: str_cmp_loop String String Int Int Int => Int
      str_cmp_loop a b idx lena lenb =
        | (idx >= lena) and (idx >= lenb) = 0
        | idx >= lena = 0 - 1
        | idx >= lenb = 1
        | (str_char_at a idx) < (str_char_at b idx) = 0 - 1
        | (str_char_at a idx) > (str_char_at b idx) = 1
        | otherwise = str_cmp_loop a b (idx + 1) lena lenb

-- ============================================================================
-- FileEntry Accessors & Metadata Loading
-- ============================================================================

fn :: fe_name FileEntry => String
      fe_name (Entry n fp k m nl sz ow gr mt lt) = n

fn :: fe_path FileEntry => String
      fe_path (Entry n fp k m nl sz ow gr mt lt) = fp

fn :: fe_kind FileEntry => FileKind
      fe_kind (Entry n fp k m nl sz ow gr mt lt) = k

fn :: fe_mode FileEntry => Int
      fe_mode (Entry n fp k m nl sz ow gr mt lt) = m

fn :: fe_nlink FileEntry => Int
      fe_nlink (Entry n fp k m nl sz ow gr mt lt) = nl

fn :: fe_size FileEntry => Int
      fe_size (Entry n fp k m nl sz ow gr mt lt) = sz

fn :: fe_owner FileEntry => String
      fe_owner (Entry n fp k m nl sz ow gr mt lt) = ow

fn :: fe_group FileEntry => String
      fe_group (Entry n fp k m nl sz ow gr mt lt) = gr

fn :: fe_mtime FileEntry => Int
      fe_mtime (Entry n fp k m nl sz ow gr mt lt) = mt

fn :: fe_link_target FileEntry => String
      fe_link_target (Entry n fp k m nl sz ow gr mt lt) = lt

fn :: parse_file_kind String => FileKind
      parse_file_kind s      =
        | s == "dir" = FDir
        | s == "link" = FSymLink
        | s == "fifo" = FFifo
        | s == "sock" = FSock
        | s == "block" = FBlock
        | s == "char" = FChar
        | otherwise = FReg

fn :: is_dir_kind FileKind => Bool
      is_dir_kind FDir     =  True
      is_dir_kind other    =  False

fn :: is_reg_kind FileKind => Bool
      is_reg_kind FReg     =  True
      is_reg_kind other    =  False

fn :: is_link_kind FileKind => Bool
      is_link_kind FSymLink =  True
      is_link_kind other    =  False

fn :: stat_to_entry String String (Bool, String, Int, Int, Int, String, String, Int, String) => FileEntry
      stat_to_entry name full_path (ok, kstr, mode, nlink, size, owner, group, mtime, link_target) =
        Entry name full_path (parse_file_kind kstr) mode nlink size owner group mtime link_target

fn :: lstat_ok (Bool, String, Int, Int, Int, String, String, Int, String) => Bool
      lstat_ok (ok, kstr, mode, nlink, size, owner, group, mtime, link_target) = ok

op :: stat file entry String    named String => FileEntry
      stat file entry full_path named name   =
        raw = file_lstat full_path
        stat_to_entry name full_path raw

-- ============================================================================
-- 1. BSD ls(1) — FR-FS-001
-- ============================================================================

fn :: default_ls_spec => LsSpec
      default_ls_spec =  LsSpec False True False False False False False False False False False False []

op :: parse ls args [String] => LsSpec
      parse ls args args     =  parse_ls_loop args default_ls_spec False

fn :: apply_ls_flag Int LsSpec => LsSpec
      apply_ls_flag c   spec   =
        | c == 49 = spec with l_one_per_line: True, l_multi_col: False, l_long: False
        | c == 67 = spec with l_one_per_line: False, l_multi_col: True, l_long: False
        | c == 108 = spec with l_one_per_line: False, l_multi_col: False, l_long: True
        | c == 97 = spec with l_all: True
        | c == 65 = spec with l_almost_all: True
        | c == 70 = spec with l_classify: True
        | c == 104 = spec with l_human: True
        | c == 83 = spec with l_sort_size: True, l_sort_time: False
        | c == 116 = spec with l_sort_size: False, l_sort_time: True
        | c == 114 = spec with l_reverse: True
        | c == 82 = spec with l_recursive: True
        | c == 71 = spec with l_color: True
        | otherwise = spec

fn :: apply_ls_cluster String Int Int LsSpec => LsSpec
      apply_ls_cluster s      idx len spec   =
        | idx >= len = spec
        | otherwise = apply_ls_cluster s (idx + 1) len (apply_ls_flag (str_char_at s idx) spec)

fn :: parse_ls_loop [String]   LsSpec Bool      => LsSpec
      parse_ls_loop []         spec   stop_opts =  finalize_ls_paths spec
      parse_ls_loop (arg:rest) spec   True      =
        parse_ls_loop rest (spec with l_paths: (l_paths spec) :: [arg]) True
      parse_ls_loop (arg:rest) spec   False     =
        | arg == "--" = parse_ls_loop rest spec True
        | (str_starts_with arg "-") and ((str_len arg) > 1) = parse_ls_loop rest (apply_ls_cluster arg 1 (str_len arg) spec) False
        | otherwise = parse_ls_loop rest (spec with l_paths: (l_paths spec) :: [arg]) False

fn :: finalize_ls_paths LsSpec => LsSpec
      finalize_ls_paths spec   =
        | (list_len (l_paths spec)) == 0 = spec with l_paths: ["."]
        | otherwise = spec

fn :: is_exec_mode Int  => Bool
      is_exec_mode mode =
        u_x = (mode / 64) % 2
        g_x = (mode / 8) % 2
        o_x = mode % 2
        (u_x == 1) or (g_x == 1) or (o_x == 1)

op :: classify suffix FileEntry => String
      classify suffix entry     =  classify_kind_mode (fe_kind entry) (fe_mode entry)

fn :: classify_kind_mode FileKind Int  => String
      classify_kind_mode FDir     mode =  "/"
      classify_kind_mode FSymLink mode =  "@"
      classify_kind_mode FFifo    mode =  "|"
      classify_kind_mode FSock    mode =  "="
      classify_kind_mode FReg     mode =
        | is_exec_mode mode = "*"
        | otherwise = ""
      classify_kind_mode other    mode =  ""

fn :: kind_type_char FileKind => String
      kind_type_char FDir     =  "d"
      kind_type_char FSymLink =  "l"
      kind_type_char FFifo    =  "p"
      kind_type_char FSock    =  "s"
      kind_type_char FBlock   =  "b"
      kind_type_char FChar    =  "c"
      kind_type_char FReg     =  "-"

fn :: rwx_triplet Int Int Bool String String => String
      rwx_triplet bits special_bit is_special sp_exec sp_noexec =
        r_ch = select_bit_char ((bits / 4) % 2) "r" "-"
        w_ch = select_bit_char ((bits / 2) % 2) "w" "-"
        x_bit = bits % 2
        x_ch = select_exec_char x_bit is_special sp_exec sp_noexec
        r_ch + w_ch + x_ch

fn :: select_bit_char Int   String String => String
      select_bit_char 1     yes    no     =  yes
      select_bit_char other yes    no     =  no

fn :: select_exec_char Int   Bool  String  String    => String
      select_exec_char 1     True  sp_exec sp_noexec =  sp_exec
      select_exec_char 0     True  sp_exec sp_noexec =  sp_noexec
      select_exec_char 1     False sp_exec sp_noexec =  "x"
      select_exec_char other False sp_exec sp_noexec =  "-"

op :: format mode string FileKind Int  => String
      format mode string kind     mode =
        suid = ((mode / 2048) % 2) == 1
        sgid = ((mode / 1024) % 2) == 1
        sticky = ((mode / 512) % 2) == 1
        u_bits = (mode / 64) % 8
        g_bits = (mode / 8) % 8
        o_bits = mode % 8
        (kind_type_char kind) + (rwx_triplet u_bits 0 suid "s" "S") + (rwx_triplet g_bits 0 sgid "s" "S") + (rwx_triplet o_bits 0 sticky "t" "T")

op :: format human size Int   => String
      format human size bytes =
        | bytes < 1000 = (int_to_str bytes) + "B"
        | bytes < 10240 = format_tenth_unit bytes 1024 "K"
        | bytes < 1048576 = (int_to_str ((bytes + 512) / 1024)) + "K"
        | bytes < 10485760 = format_tenth_unit bytes 1048576 "M"
        | bytes < 1073741824 = (int_to_str ((bytes + 524288) / 1048576)) + "M"
        | otherwise = format_tenth_unit bytes 1073741824 "G"

fn :: format_tenth_unit Int   Int  String => String
      format_tenth_unit bytes unit suffix =
        tenths = ((bytes * 10) + (unit / 2)) / unit
        whole = tenths / 10
        frac = tenths % 10
        (int_to_str whole) + "." + (int_to_str frac) + suffix

fn :: resolve_lscolors => String
      resolve_lscolors =
        env_val = sys_getenv "LSCOLORS"
        select_lscolors env_val

fn :: select_lscolors String  => String
      select_lscolors env_val =
        | (str_len env_val) >= 22 = env_val
        | otherwise = "exfxcxdxbxegedabagacad"

fn :: lscolors_slot FileKind Int  => Int
      lscolors_slot FDir     mode =
        | (((mode / 512) % 2) == 1) and (((mode / 2) % 2) == 1) = 9
        | ((mode / 2) % 2) == 1 = 10
        | otherwise = 0
      lscolors_slot FSymLink mode =  1
      lscolors_slot FSock    mode =  2
      lscolors_slot FFifo    mode =  3
      lscolors_slot FBlock   mode =  5
      lscolors_slot FChar    mode =  6
      lscolors_slot FReg     mode =
        | (((mode / 2048) % 2) == 1) and (is_exec_mode mode) = 7
        | (((mode / 1024) % 2) == 1) and (is_exec_mode mode) = 8
        | is_exec_mode mode = 4
        | otherwise = 0 - 1

fn :: ansi_fg_for_bsd Int => String
      ansi_fg_for_bsd c   =
        | c == 97 = "30"
        | c == 98 = "31"
        | c == 99 = "32"
        | c == 100 = "33"
        | c == 101 = "34"
        | c == 102 = "35"
        | c == 103 = "36"
        | c == 104 = "37"
        | c == 65 = "1;30"
        | c == 66 = "1;31"
        | c == 67 = "1;32"
        | c == 68 = "1;33"
        | c == 69 = "1;34"
        | c == 70 = "1;35"
        | c == 71 = "1;36"
        | c == 72 = "1;37"
        | otherwise = ""

fn :: colorize_name String FileEntry Bool  => String
      colorize_name name   entry     False =  name
      colorize_name name   entry     True  =
        slot = lscolors_slot (fe_kind entry) (fe_mode entry)
        apply_color_slot name slot (resolve_lscolors)

fn :: apply_color_slot String Int  String => String
      apply_color_slot name   slot colors =
        | slot < 0 = name
        | otherwise = wrap_ansi_fg name (ansi_fg_for_bsd (str_char_at colors (slot * 2)))

fn :: wrap_ansi_fg String String => String
      wrap_ansi_fg name   code   =
        | (str_len code) == 0 = name
        | otherwise = (char_from_code 27) + "[" + code + "m" + name + (char_from_code 27) + "[0m"

fn :: format_entry_display_name FileEntry LsSpec => String
      format_entry_display_name entry     spec   =
        colored = colorize_name (fe_name entry) entry (l_color spec)
        suf = select_classify_suffix entry (l_classify spec)
        colored + suf

fn :: select_classify_suffix FileEntry Bool  => String
      select_classify_suffix entry     False =  ""
      select_classify_suffix entry     True  =  classify suffix entry

fn :: compare_entries FileEntry FileEntry LsSpec => Int
      compare_entries a         b         spec   =
        raw_cmp = compare_entries_primary a b spec
        apply_reverse_cmp raw_cmp (l_reverse spec)

fn :: apply_reverse_cmp Int Bool  => Int
      apply_reverse_cmp cmp False =  cmp
      apply_reverse_cmp cmp True  =  0 - cmp

fn :: compare_entries_primary FileEntry FileEntry LsSpec => Int
      compare_entries_primary a         b         spec   =
        | l_sort_size spec = compare_int_desc (fe_size a) (fe_size b) (fe_name a) (fe_name b)
        | l_sort_time spec = compare_int_desc (fe_mtime a) (fe_mtime b) (fe_name a) (fe_name b)
        | otherwise = str_cmp (fe_name a) (fe_name b)

fn :: compare_int_desc Int Int String String => Int
      compare_int_desc va  vb  na     nb     =
        | va > vb = 0 - 1
        | va < vb = 1
        | otherwise = str_cmp na nb

op :: sort entries [FileEntry] with LsSpec => [FileEntry]
      sort entries []          with spec   =  []
      sort entries (x:xs)      with spec   =  insert_sorted_entry x (sort entries xs with spec) spec

fn :: insert_sorted_entry FileEntry [FileEntry] LsSpec => [FileEntry]
      insert_sorted_entry item      []          spec   =  [item]
      insert_sorted_entry item      (y:ys)      spec   =
        | (compare_entries item y spec) <= 0 = [item, y] :: ys
        | otherwise = [y] :: (insert_sorted_entry item ys spec)

fn :: filter_ls_names [String] LsSpec => [String]
      filter_ls_names names    spec   =
        | l_all spec = [".", ".."] :: names
        | l_almost_all spec = names
        | otherwise = filter_visible_names names []

fn :: filter_visible_names [String] [String] => [String]
      filter_visible_names []       acc      =  acc
      filter_visible_names (n:rest) acc      =
        | str_starts_with n "." = filter_visible_names rest acc
        | otherwise = filter_visible_names rest (acc :: [n])

fn :: load_dir_entries String   [String]    [FileEntry] => [FileEntry]
      load_dir_entries dir_path []          acc         =  acc
      load_dir_entries dir_path (name:rest) acc         =
        fp = join_path dir_path name
        entry = stat file entry fp named name
        load_dir_entries dir_path rest (acc :: [entry])

fn :: format_size_field Int Bool  => String
      format_size_field sz  False =  int_to_str sz
      format_size_field sz  True  =  format human size sz

fn :: format_mtime_short Int => String
      format_mtime_short mt  =
        day_sec = mt % 86400
        hh = (day_sec / 3600) % 24
        mm = (day_sec / 60) % 60
        "Oct 18 " + (pad_2_zero hh) + ":" + (pad_2_zero mm)

fn :: pad_2_zero Int => String
      pad_2_zero n   =
        | n < 10 = "0" + (int_to_str n)
        | otherwise = int_to_str n

fn :: format_long_entry FileEntry LsSpec Int Int Int Int => String
      format_long_entry entry spec w_nlink w_owner w_group w_size =
        m_str = format mode string (fe_kind entry) (fe_mode entry)
        nl_str = pad_left (int_to_str (fe_nlink entry)) w_nlink
        ow_str = pad_right (fe_owner entry) w_owner
        gr_str = pad_right (fe_group entry) w_group
        sz_str = pad_left (format_size_field (fe_size entry) (l_human spec)) w_size
        dt_str = format_mtime_short (fe_mtime entry)
        disp_name = format_entry_display_name entry spec
        link_suffix = format_symlink_target entry
        m_str + " " + nl_str + " " + ow_str + " " + gr_str + " " + sz_str + " " + dt_str + " " + disp_name + link_suffix

fn :: format_symlink_target FileEntry => String
      format_symlink_target entry     =
        | (is_link_kind (fe_kind entry)) and ((str_len (fe_link_target entry)) > 0) = " -> " + (fe_link_target entry)
        | otherwise = ""

fn :: max_entry_widths [FileEntry] Bool Int Int Int Int => (Int, Int, Int, Int)
      max_entry_widths [] human wn wo wg ws = (wn, wo, wg, ws)
      max_entry_widths (e:rest) human wn wo wg ws =
        n_len = str_len (int_to_str (fe_nlink e))
        o_len = str_len (fe_owner e)
        g_len = str_len (fe_group e)
        s_len = str_len (format_size_field (fe_size e) human)
        max_entry_widths rest human (max_int wn n_len) (max_int wo o_len) (max_int wg g_len) (max_int ws s_len)

fn :: render_long_entries [FileEntry] LsSpec (Int, Int, Int, Int) [String] => [String]
      render_long_entries [] spec widths acc = acc
      render_long_entries (e:rest) spec (wn, wo, wg, ws) acc =
        line = format_long_entry e spec wn wo wg ws
        render_long_entries rest spec (wn, wo, wg, ws) (acc :: [line])

fn :: render_one_per_line [FileEntry] LsSpec [String] => [String]
      render_one_per_line []          spec   acc      =  acc
      render_one_per_line (e:rest)    spec   acc      =
        render_one_per_line rest spec (acc :: [format_entry_display_name e spec])

fn :: visible_entry_len FileEntry LsSpec => Int
      visible_entry_len entry     spec   =
        (str_len (fe_name entry)) + (str_len (select_classify_suffix entry (l_classify spec)))

fn :: max_visible_len [FileEntry] LsSpec Int  => Int
      max_visible_len []          spec   best =  best
      max_visible_len (e:rest)    spec   best =
        max_visible_len rest spec (max_int best (visible_entry_len e spec))

fn :: render_multi_col [FileEntry] LsSpec => [String]
      render_multi_col []          spec   =  []
      render_multi_col entries     spec   =
        total = list_len entries
        max_w = max_int 1 (max_visible_len entries spec 0)
        col_w = max_w + 2
        num_cols = max_int 1 (80 / col_w)
        num_rows = ((total + num_cols - 1) / num_cols)
        render_mc_rows entries spec total num_rows num_cols col_w 0 []

fn :: entry_list_nth [FileEntry] Int => FileEntry
      entry_list_nth (x:xs)      n   =
        | n <= 0 = x
        | otherwise = entry_list_nth xs (n - 1)

fn :: render_mc_rows [FileEntry] LsSpec Int Int Int Int Int [String] => [String]
      render_mc_rows entries spec total num_rows num_cols col_w r acc =
        | r >= num_rows = acc
        | otherwise = render_mc_rows entries spec total num_rows num_cols col_w (r + 1) (acc :: [render_mc_row entries spec total num_rows num_cols col_w r 0 ""])

fn :: render_mc_row [FileEntry] LsSpec Int Int Int Int Int Int String => String
      render_mc_row entries spec total num_rows num_cols col_w r c acc =
        | c >= num_cols = acc
        | ((c * num_rows) + r) >= total = acc
        | otherwise =
          idx = (c * num_rows) + r
          ent = entry_list_nth entries idx
          disp = format_entry_display_name ent spec
          vlen = visible_entry_len ent spec
          next_idx = ((c + 1) * num_rows) + r
          has_more = ((c + 1) < num_cols) and (next_idx < total)
          padded = pad_mc_cell disp vlen col_w has_more
          (render_mc_row entries spec total num_rows num_cols col_w r (c + 1) (acc + padded))

fn :: pad_mc_cell String Int  Int   Bool  => String
      pad_mc_cell disp   vlen col_w False =  disp
      pad_mc_cell disp   vlen col_w True  =  disp + (repeat_str " " (max_int 1 (col_w - vlen)))

op :: format ls entries [FileEntry] with LsSpec => [String]
      format ls entries entries     with spec   =
        | l_long spec = render_long_entries entries spec (max_entry_widths entries (l_human spec) 1 1 1 1) []
        | l_one_per_line spec = render_one_per_line entries spec []
        | otherwise = render_multi_col entries spec

fn :: collect_subdirs [FileEntry] [String] => [String]
      collect_subdirs []          acc      =  acc
      collect_subdirs (e:rest)    acc      =
        | (is_dir_kind (fe_kind e)) and ((fe_name e) != ".") and ((fe_name e) != "..") = collect_subdirs rest (acc :: [fe_path e])
        | otherwise = collect_subdirs rest acc

fn :: list_single_dir String   LsSpec Bool        => [String]
      list_single_dir dir_path spec   show_header =
        raw_names = filter_ls_names (list_dir dir_path) spec
        loaded = load_dir_entries dir_path raw_names []
        sorted = sort entries loaded with spec
        lines = format ls entries sorted with spec
        header_lines = prepend_dir_header dir_path show_header lines
        recurse_subdirs sorted spec header_lines

fn :: prepend_dir_header String   Bool  [String] => [String]
      prepend_dir_header dir_path False lines    =  lines
      prepend_dir_header dir_path True  lines    =  [dir_path + ":"] :: lines

fn :: recurse_subdirs [FileEntry] LsSpec [String] => [String]
      recurse_subdirs sorted      spec   lines    =
        | not (l_recursive spec) = lines
        | otherwise = recurse_subdir_list (collect_subdirs sorted []) spec lines

fn :: recurse_subdir_list [String]   LsSpec [String] => [String]
      recurse_subdir_list []         spec   acc      =  acc
      recurse_subdir_list (sub:rest) spec   acc      =
        sub_lines = list_single_dir sub spec True
        next_acc = acc :: [""] :: sub_lines
        recurse_subdir_list rest spec next_acc

fn :: run_ls_paths [String] LsSpec Bool  [String] => [String]
      run_ls_paths []       spec   multi acc      =  acc
      run_ls_paths (p:rest) spec   multi acc      =
        ent = stat file entry p named p
        part = render_ls_target p ent spec (multi or (l_recursive spec))
        next_acc = append_ls_section acc part
        run_ls_paths rest spec multi next_acc

fn :: render_ls_target String FileEntry LsSpec Bool        => [String]
      render_ls_target p      ent       spec   show_header =
        | is_dir_kind (fe_kind ent) = list_single_dir p spec show_header
        | otherwise = format ls entries [ent] with spec

fn :: append_ls_section [String] [String] => [String]
      append_ls_section []       part     =  part
      append_ls_section acc      []       =  acc
      append_ls_section acc      part     =  acc :: [""] :: part

op :: run ls [String] => String
      run ls args     =
        spec = parse ls args args
        paths = l_paths spec
        multi = (list_len paths) > 1
        lines = run_ls_paths paths spec multi []
        finalize_output_lines lines

fn :: finalize_output_lines [String] => String
      finalize_output_lines []       =  ""
      finalize_output_lines lines    =  (join_with lines (char_from_code 10)) + (char_from_code 10)

-- ============================================================================
-- 2. BSD find(1) — FR-FS-002
-- ============================================================================

op :: glob match String  against String => Bool
      glob match pattern against text   =  glob_loop pattern 0 (str_len pattern) text 0 (str_len text)

fn :: glob_loop String Int Int String Int Int => Bool
      glob_loop pat pi plen txt ti tlen =
        | (pi >= plen) and (ti >= tlen) = True
        | (pi < plen) and ((str_char_at pat pi) == 42) = glob_star pat (pi + 1) plen txt ti tlen
        | (pi >= plen) or (ti >= tlen) = False
        | (str_char_at pat pi) == 63 = glob_loop pat (pi + 1) plen txt (ti + 1) tlen
        | (str_char_at pat pi) == 91 = glob_bracket pat (pi + 1) plen txt ti tlen
        | (str_char_at pat pi) == (str_char_at txt ti) = glob_loop pat (pi + 1) plen txt (ti + 1) tlen
        | otherwise = False

fn :: glob_star String Int Int String Int Int => Bool
      glob_star pat pi plen txt ti tlen =
        | glob_loop pat pi plen txt ti tlen = True
        | ti < tlen = glob_star pat pi plen txt (ti + 1) tlen
        | otherwise = False

fn :: glob_bracket String Int Int String Int Int => Bool
      glob_bracket pat pi plen txt ti tlen =
        neg = (pi < plen) and (((str_char_at pat pi) == 33) or ((str_char_at pat pi) == 94))
        start_idx = select_bracket_start pi neg
        res = match_bracket_chars pat start_idx plen (str_char_at txt ti) False
        matched = fst res
        end_pi = snd res
        check_bracket_result pat end_pi plen txt ti tlen matched neg

fn :: select_bracket_start Int Bool  => Int
      select_bracket_start pi  False =  pi
      select_bracket_start pi  True  =  pi + 1

fn :: match_bracket_chars String Int Int Int Bool => (Bool, Int)
      match_bracket_chars pat idx plen ch matched =
        | idx >= plen = (False, plen)
        | (str_char_at pat idx) == 93 = (matched, idx + 1)
        | ((idx + 2) < plen) and ((str_char_at pat (idx + 1)) == 45) and ((str_char_at pat (idx + 2)) != 93) = match_bracket_range pat idx plen ch matched
        | (str_char_at pat idx) == ch = match_bracket_chars pat (idx + 1) plen ch True
        | otherwise = match_bracket_chars pat (idx + 1) plen ch matched

fn :: match_bracket_range String Int Int Int Bool => (Bool, Int)
      match_bracket_range pat idx plen ch matched =
        lo = str_char_at pat idx
        hi = str_char_at pat (idx + 2)
        in_rng = (ch >= lo) and (ch <= hi)
        match_bracket_chars pat (idx + 3) plen ch (matched or in_rng)

fn :: check_bracket_result String Int Int String Int Int Bool Bool => Bool
      check_bracket_result pat end_pi plen txt ti tlen matched neg =
        | end_pi > plen = False
        | (not neg) and matched = glob_loop pat end_pi plen txt (ti + 1) tlen
        | neg and (not matched) = glob_loop pat end_pi plen txt (ti + 1) tlen
        | otherwise = False

fn :: is_find_token String => Bool
      is_find_token s      =
        | (s == "(") or (s == ")") or (s == "!") = True
        | (str_starts_with s "-") and ((str_len s) > 1) = True
        | otherwise = False

fn :: split_find_paths [String] [String] => ([String], [String])
      split_find_paths []       acc      =
        | (list_len acc) == 0 = (["."], [])
        | otherwise = (acc, [])
      split_find_paths (x:xs)   acc      =
        | is_find_token x = (default_if_empty acc, [x] :: xs)
        | otherwise = split_find_paths xs (acc :: [x])

fn :: default_if_empty [String] => [String]
      default_if_empty []       =  ["."]
      default_if_empty xs       =  xs

fn :: parse_size_arg String => (Int, Int)
      parse_size_arg s      =
        | (str_len s) == 0 = (0, 0)
        | (str_char_at s 0) == 43 = (1, parse_size_bytes (str_slice s 1 (str_len s)))
        | (str_char_at s 0) == 45 = (0 - 1, parse_size_bytes (str_slice s 1 (str_len s)))
        | otherwise = (0, parse_size_bytes s)

fn :: parse_size_bytes String => Int
      parse_size_bytes s      =
        len = str_len s
        last_ch = select_last_char s len
        parse_size_with_unit s len last_ch

fn :: select_last_char String Int => Int
      select_last_char s      len =
        | len <= 0 = 0
        | otherwise = str_char_at s (len - 1)

fn :: parse_size_with_unit String Int Int     => Int
      parse_size_with_unit s      len last_ch =
        | last_ch == 99 = str_to_int (str_slice s 0 (len - 1))
        | last_ch == 107 = (str_to_int (str_slice s 0 (len - 1))) * 1024
        | last_ch == 77 = (str_to_int (str_slice s 0 (len - 1))) * 1048576
        | last_ch == 71 = (str_to_int (str_slice s 0 (len - 1))) * 1073741824
        | otherwise = (str_to_int s) * 512

fn :: extract_find_options [String] Int Int Bool [String] [String] => (Int, Int, Bool, [String], [String])
      extract_find_options [] max_d min_d p0 exec_c rem = (max_d, min_d, p0, exec_c, rem)
      extract_find_options (t:rest) max_d min_d p0 exec_c rem =
        | t == "-maxdepth" = extract_maxdepth rest max_d min_d p0 exec_c rem
        | t == "-mindepth" = extract_mindepth rest max_d min_d p0 exec_c rem
        | t == "-print" = extract_find_options rest max_d min_d p0 exec_c rem
        | t == "-print0" = extract_find_options rest max_d min_d True exec_c rem
        | t == "-exec" = extract_exec_tokens rest max_d min_d p0 [] rem
        | otherwise = extract_find_options rest max_d min_d p0 exec_c (rem :: [t])

fn :: extract_maxdepth [String] Int Int Bool [String] [String] => (Int, Int, Bool, [String], [String])
      extract_maxdepth [] max_d min_d p0 exec_c rem = (max_d, min_d, p0, exec_c, rem)
      extract_maxdepth (v:rest) max_d min_d p0 exec_c rem =
        extract_find_options rest (str_to_int v) min_d p0 exec_c rem

fn :: extract_mindepth [String] Int Int Bool [String] [String] => (Int, Int, Bool, [String], [String])
      extract_mindepth [] max_d min_d p0 exec_c rem = (max_d, min_d, p0, exec_c, rem)
      extract_mindepth (v:rest) max_d min_d p0 exec_c rem =
        extract_find_options rest max_d (str_to_int v) p0 exec_c rem

fn :: extract_exec_tokens [String] Int Int Bool [String] [String] => (Int, Int, Bool, [String], [String])
      extract_exec_tokens [] max_d min_d p0 acc rem = (max_d, min_d, p0, acc, rem)
      extract_exec_tokens (t:rest) max_d min_d p0 acc rem =
        | t == ";" = extract_find_options rest max_d min_d p0 acc rem
        | otherwise = extract_exec_tokens rest max_d min_d p0 (acc :: [t]) rem

fn :: parse_find_expr [String] => FindExpr
      parse_find_expr []       =  ETrue
      parse_find_expr toks     =  fst (parse_or_expr toks)

fn :: parse_or_expr [String] => (FindExpr, [String])
      parse_or_expr toks     =
        left_res = parse_and_expr toks
        parse_or_tail (fst left_res) (snd left_res)

fn :: parse_or_tail FindExpr [String] => (FindExpr, [String])
      parse_or_tail left     []       =  (left, [])
      parse_or_tail left     (t:rest) =
        | (t == "-or") or (t == "-o") = combine_or_branch left (parse_and_expr rest)
        | otherwise = (left, [t] :: rest)

fn :: combine_or_branch FindExpr (FindExpr, [String]) => (FindExpr, [String])
      combine_or_branch left     (right, rem)         =  parse_or_tail (EOr left right) rem

fn :: parse_and_expr [String] => (FindExpr, [String])
      parse_and_expr toks     =
        left_res = parse_unary_expr toks
        parse_and_tail (fst left_res) (snd left_res)

fn :: parse_and_tail FindExpr [String] => (FindExpr, [String])
      parse_and_tail left     []       =  (left, [])
      parse_and_tail left     (t:rest) =
        | (t == "-and") or (t == "-a") = combine_and_branch left (parse_unary_expr rest)
        | (t == "-or") or (t == "-o") or (t == ")") = (left, [t] :: rest)
        | otherwise = combine_and_branch left (parse_unary_expr ([t] :: rest))

fn :: combine_and_branch FindExpr (FindExpr, [String]) => (FindExpr, [String])
      combine_and_branch left     (right, rem)         =  parse_and_tail (EAnd left right) rem

fn :: parse_unary_expr [String] => (FindExpr, [String])
      parse_unary_expr []       =  (ETrue, [])
      parse_unary_expr (t:rest) =
        | (t == "!") or (t == "-not") = wrap_not_branch (parse_unary_expr rest)
        | t == "(" = parse_paren_expr rest
        | t == "-name" = parse_arg_primary 1 rest
        | t == "-iname" = parse_arg_primary 2 rest
        | t == "-path" = parse_arg_primary 3 rest
        | t == "-type" = parse_arg_primary 4 rest
        | t == "-size" = parse_arg_primary 5 rest
        | t == "-empty" = (EEmpty, rest)
        | otherwise = (ETrue, rest)

fn :: wrap_not_branch (FindExpr, [String]) => (FindExpr, [String])
      wrap_not_branch (sub, rem)           =  (ENot sub, rem)

fn :: parse_paren_expr [String] => (FindExpr, [String])
      parse_paren_expr toks     =
        inner = parse_or_expr toks
        strip_closing_paren (fst inner) (snd inner)

fn :: strip_closing_paren FindExpr [String] => (FindExpr, [String])
      strip_closing_paren e        []       =  (e, [])
      strip_closing_paren e        (t:rest) =
        | t == ")" = (e, rest)
        | otherwise = (e, [t] :: rest)

fn :: parse_arg_primary Int  [String]   => (FindExpr, [String])
      parse_arg_primary kind []         =  (ETrue, [])
      parse_arg_primary kind (arg:rest) =
        | kind == 1 = (EName arg, rest)
        | kind == 2 = (EIname arg, rest)
        | kind == 3 = (EPath arg, rest)
        | kind == 4 = (EType arg, rest)
        | kind == 5 = build_size_expr (parse_size_arg arg) rest
        | otherwise = (ETrue, rest)

fn :: build_size_expr (Int, Int)    [String] => (FindExpr, [String])
      build_size_expr (cmp, target) rest     =  (ESize cmp target, rest)

op :: parse find args [String] => FindSpec
      parse find args args     =
        split_res = split_find_paths args []
        paths = fst split_res
        raw_toks = snd split_res
        opt_res = extract_find_options raw_toks (0 - 1) 0 False [] []
        build_find_spec paths opt_res

fn :: build_find_spec [String] (Int, Int, Bool, [String], [String]) => FindSpec
      build_find_spec paths    (max_d, min_d, p0, exec_c, expr_toks) =
        FindSpec paths (parse_find_expr expr_toks) max_d min_d p0 exec_c

fn :: match_find_type FileKind String => Bool
      match_find_type FReg     "f"    =  True
      match_find_type FDir     "d"    =  True
      match_find_type FSymLink "l"    =  True
      match_find_type FFifo    "p"    =  True
      match_find_type FSock    "s"    =  True
      match_find_type FBlock   "b"    =  True
      match_find_type FChar    "c"    =  True
      match_find_type k        t      =  False

fn :: match_find_size Int    Int Int    => Bool
      match_find_size actual cmp target =
        | cmp > 0 = actual > target
        | cmp < 0 = actual < target
        | otherwise = actual == target

fn :: match_find_empty FileEntry => Bool
      match_find_empty entry     =
        | is_reg_kind (fe_kind entry) = (fe_size entry) == 0
        | is_dir_kind (fe_kind entry) = (list_len (list_dir (fe_path entry))) == 0
        | otherwise = False

op :: evaluate find expr FindExpr           on FileEntry at Int   => Bool
      evaluate find expr ETrue              on entry     at depth =  True
      evaluate find expr (EName pat)        on entry     at depth =  glob match pat against (fe_name entry)
      evaluate find expr (EIname pat)       on entry     at depth =  glob match (to_lower_str pat) against (to_lower_str (fe_name entry))
      evaluate find expr (EPath pat)        on entry     at depth =  glob match pat against (fe_path entry)
      evaluate find expr (EType t)          on entry     at depth =  match_find_type (fe_kind entry) t
      evaluate find expr (ESize cmp target) on entry     at depth =  match_find_size (fe_size entry) cmp target
      evaluate find expr (EMaxDepth d)      on entry     at depth =  depth <= d
      evaluate find expr (EMinDepth d)      on entry     at depth =  depth >= d
      evaluate find expr EEmpty             on entry     at depth =  match_find_empty entry
      evaluate find expr (ENot sub)         on entry     at depth =
        ok = evaluate find expr sub on entry at depth
        not ok
      evaluate find expr (EAnd a b)         on entry     at depth =
        l = evaluate find expr a on entry at depth
        r = evaluate find expr b on entry at depth
        l and r
      evaluate find expr (EOr a b)          on entry     at depth =
        l = evaluate find expr a on entry at depth
        r = evaluate find expr b on entry at depth
        l or r

fn :: walk_find_tree String Int   FindSpec [String] => [String]
      walk_find_tree path   depth spec     acc      =
        | ((f_max_depth spec) >= 0) and (depth > (f_max_depth spec)) = acc
        | otherwise = walk_find_node path depth spec acc

fn :: walk_find_node String Int   FindSpec [String] => [String]
      walk_find_node path   depth spec     acc      =
        bname = basename_of path
        entry = stat file entry path named bname
        expr_ok = evaluate find expr (f_expr spec) on entry at depth
        matched = (depth >= (f_min_depth spec)) and expr_ok
        acc1 = record_find_match path matched spec acc
        descend_find_children path entry depth spec acc1

fn :: record_find_match String Bool  FindSpec [String] => [String]
      record_find_match path   False spec     acc      =  acc
      record_find_match path   True  spec     acc      =
        _ = run_find_exec (f_exec_cmd spec) path
        acc :: [path]

fn :: run_find_exec [String]    String => Int
      run_find_exec []          path   =  0
      run_find_exec (prog:args) path   =
        sub_prog = subst_braces prog path
        sub_args = subst_braces_list args path []
        sys_exec sub_prog sub_args []

fn :: subst_braces String String => String
      subst_braces tok    path   =
        | tok == "\{\}" = path
        | otherwise = tok

fn :: subst_braces_list [String] String [String] => [String]
      subst_braces_list []       path   acc      =  acc
      subst_braces_list (x:xs)   path   acc      =  subst_braces_list xs path (acc :: [subst_braces x path])

fn :: descend_find_children String FileEntry Int FindSpec [String] => [String]
      descend_find_children path entry depth spec acc =
        | not (is_dir_kind (fe_kind entry)) = acc
        | ((f_max_depth spec) >= 0) and ((depth + 1) > (f_max_depth spec)) = acc
        | otherwise = walk_find_children_list path (list_dir path) (depth + 1) spec acc

fn :: walk_find_children_list String [String] Int FindSpec [String] => [String]
      walk_find_children_list parent [] depth spec acc = acc
      walk_find_children_list parent (child:rest) depth spec acc =
        child_path = join_path parent child
        acc1 = walk_find_tree child_path depth spec acc
        walk_find_children_list parent rest depth spec acc1

fn :: walk_find_roots [String] FindSpec [String] => [String]
      walk_find_roots []       spec     acc      =  acc
      walk_find_roots (r:rest) spec     acc      =
        acc1 = walk_find_tree r 0 spec acc
        walk_find_roots rest spec acc1

op :: run find [String] => String
      run find args     =
        spec = parse find args args
        matches = walk_find_roots (f_paths spec) spec []
        format_find_matches matches (f_print0 spec) (list_len (f_exec_cmd spec))

fn :: format_find_matches [String] Bool   Int      => String
      format_find_matches matches  print0 exec_len =
        | exec_len > 0 = ""
        | (list_len matches) == 0 = ""
        | print0 = (join_with matches (char_from_code 0)) + (char_from_code 0)
        | otherwise = (join_with matches (char_from_code 10)) + (char_from_code 10)

-- ============================================================================
-- 3. BSD mtree(8) — FR-FS-003
-- ============================================================================

fn :: default_mtree_spec => MtreeSpec
      default_mtree_spec =  MtreeSpec False "" "." False ["type", "mode", "size", "sha256", "link"]

op :: parse mtree args [String] => MtreeSpec
      parse mtree args args     =  parse_mtree_loop args default_mtree_spec

fn :: parse_mtree_loop [String]   MtreeSpec => MtreeSpec
      parse_mtree_loop []         spec      =  spec
      parse_mtree_loop (arg:rest) spec      =
        | arg == "-c" = parse_mtree_loop rest (spec with m_create: True)
        | arg == "-u" = parse_mtree_loop rest (spec with m_update: True)
        | arg == "-f" = parse_mtree_flag_f rest spec
        | arg == "-p" = parse_mtree_flag_p rest spec
        | arg == "-K" = parse_mtree_flag_k rest spec
        | otherwise = parse_mtree_loop rest spec

fn :: parse_mtree_flag_f [String] MtreeSpec => MtreeSpec
      parse_mtree_flag_f []       spec      =  spec
      parse_mtree_flag_f (f:rest) spec      =
        parse_mtree_loop rest (spec with m_spec_file: f)

fn :: parse_mtree_flag_p [String] MtreeSpec => MtreeSpec
      parse_mtree_flag_p []       spec      =  spec
      parse_mtree_flag_p (p:rest) spec      =
        parse_mtree_loop rest (spec with m_path: p)

fn :: parse_mtree_flag_k [String] MtreeSpec => MtreeSpec
      parse_mtree_flag_k []       spec      =  spec
      parse_mtree_flag_k (k:rest) spec      =
        kws = split_comma k 0 (str_len k) "" []
        parse_mtree_loop rest (spec with m_keywords: (m_keywords spec) :: kws)

fn :: split_comma String Int Int String [String] => [String]
      split_comma s idx len cur acc =
        | idx >= len = push_nonempty cur acc
        | (str_char_at s idx) == 44 = split_comma s (idx + 1) len "" (push_nonempty cur acc)
        | otherwise = split_comma s (idx + 1) len (cur + (str_slice s idx (idx + 1))) acc

fn :: int_to_octal_4 Int => String
      int_to_octal_4 m   =
        d3 = (m / 512) % 8
        d2 = (m / 64) % 8
        d1 = (m / 8) % 8
        d0 = m % 8
        (int_to_str d3) + (int_to_str d2) + (int_to_str d1) + (int_to_str d0)

fn :: octal_str_to_int String => Int
      octal_str_to_int s      =  octal_loop s 0 (str_len s) 0

fn :: octal_loop String Int Int Int => Int
      octal_loop s      idx len acc =
        | idx >= len = acc
        | ((str_char_at s idx) >= 48) and ((str_char_at s idx) <= 55) = octal_loop s (idx + 1) len ((acc * 8) + ((str_char_at s idx) - 48))
        | otherwise = acc

fn :: mtree_kind_str FileKind => String
      mtree_kind_str FDir     =  "dir"
      mtree_kind_str FSymLink =  "link"
      mtree_kind_str other    =  "file"

fn :: walk_mtree_create String    String   [String] => [String]
      walk_mtree_create root_path rel_path acc      =
        full_p = resolve_mtree_full root_path rel_path
        entry = stat file entry full_p named (basename_of full_p)
        line = format_mtree_spec_line rel_path full_p entry
        acc1 = acc :: [line]
        walk_mtree_children root_path rel_path full_p entry acc1

fn :: resolve_mtree_full String    String   => String
      resolve_mtree_full root_path "."      =  root_path
      resolve_mtree_full root_path rel_path =  join_clean_path root_path rel_path

fn :: format_mtree_spec_line String   String FileEntry => String
      format_mtree_spec_line rel_path full_p entry     =
        kstr = mtree_kind_str (fe_kind entry)
        m_oct = int_to_octal_4 (fe_mode entry)
        base = rel_path + " type=" + kstr + " mode=" + m_oct
        append_mtree_kind_attrs base full_p entry

fn :: append_mtree_kind_attrs String String FileEntry => String
      append_mtree_kind_attrs base   full_p entry     =
        | is_reg_kind (fe_kind entry) = base + " size=" + (int_to_str (fe_size entry)) + " sha256=" + (sha256_file full_p)
        | is_link_kind (fe_kind entry) = base + " link=" + (fe_link_target entry)
        | otherwise = base

fn :: walk_mtree_children String String String FileEntry [String] => [String]
      walk_mtree_children root_path rel_path full_p entry acc =
        | not (is_dir_kind (fe_kind entry)) = acc
        | otherwise = walk_mtree_child_list root_path rel_path (list_dir full_p) acc

fn :: walk_mtree_child_list String    String   [String]     [String] => [String]
      walk_mtree_child_list root_path rel_path []           acc      =  acc
      walk_mtree_child_list root_path rel_path (child:rest) acc      =
        child_rel = join_clean_path rel_path child
        acc1 = walk_mtree_create root_path child_rel acc
        walk_mtree_child_list root_path rel_path rest acc1

op :: generate mtree spec for String    => String
      generate mtree spec for root_path =
        header = ["#\t   mtree specification", "/set type=file mode=0644"]
        lines = walk_mtree_create root_path "." header
        finalize_output_lines lines

op :: parse mtree spec text String => [MtreeEntry]
      parse mtree spec text raw    =
        lines = split lines raw
        parse_mtree_lines lines "file" 420 []

fn :: parse_mtree_lines [String] String Int [MtreeEntry] => [MtreeEntry]
      parse_mtree_lines [] def_type def_mode acc = acc
      parse_mtree_lines (line:rest) def_type def_mode acc =
        toks = split whitespace line
        parse_mtree_tokens toks rest def_type def_mode acc

fn :: parse_mtree_tokens [String] [String] String Int [MtreeEntry] => [MtreeEntry]
      parse_mtree_tokens [] rest def_type def_mode acc = parse_mtree_lines rest def_type def_mode acc
      parse_mtree_tokens (first:kvs) rest def_type def_mode acc =
        | str_starts_with first "#" = parse_mtree_lines rest def_type def_mode acc
        | first == "/set" = apply_mtree_set kvs rest def_type def_mode acc
        | first == "/unset" = parse_mtree_lines rest "file" 420 acc
        | first == ".." = parse_mtree_lines rest def_type def_mode acc
        | otherwise = parse_mtree_lines rest def_type def_mode (acc :: [build_mtree_entry first kvs def_type def_mode])

fn :: apply_mtree_set [String] [String] String Int [MtreeEntry] => [MtreeEntry]
      apply_mtree_set [] rest def_type def_mode acc = parse_mtree_lines rest def_type def_mode acc
      apply_mtree_set (kv:more) rest def_type def_mode acc =
        | str_starts_with kv "type=" = apply_mtree_set more rest (str_slice kv 5 (str_len kv)) def_mode acc
        | str_starts_with kv "mode=" = apply_mtree_set more rest def_type (octal_str_to_int (str_slice kv 5 (str_len kv))) acc
        | otherwise = apply_mtree_set more rest def_type def_mode acc

fn :: build_mtree_entry String [String] String   Int      => MtreeEntry
      build_mtree_entry rel    kvs      def_type def_mode =
        apply_entry_kvs kvs (MtreeEntry rel def_type def_mode (0 - 1) "" "")

fn :: apply_entry_kvs [String]  MtreeEntry => MtreeEntry
      apply_entry_kvs []        ent        =  ent
      apply_entry_kvs (kv:rest) ent        =
        | str_starts_with kv "type=" = apply_entry_kvs rest (ent with me_type: str_slice kv 5 (str_len kv))
        | str_starts_with kv "mode=" = apply_entry_kvs rest (ent with me_mode: octal_str_to_int (str_slice kv 5 (str_len kv)))
        | str_starts_with kv "size=" = apply_entry_kvs rest (ent with me_size: str_to_int (str_slice kv 5 (str_len kv)))
        | str_starts_with kv "sha256=" = apply_entry_kvs rest (ent with me_sha256: str_slice kv 7 (str_len kv))
        | str_starts_with kv "link=" = apply_entry_kvs rest (ent with me_link: str_slice kv 5 (str_len kv))
        | otherwise = apply_entry_kvs rest ent

fn :: collect_live_rels String    String   [String] => [String]
      collect_live_rels root_path rel_path acc      =
        full_p = resolve_mtree_full root_path rel_path
        raw = file_lstat full_p
        collect_live_if_ok root_path rel_path full_p raw acc

fn :: collect_live_if_ok String String String (Bool, String, Int, Int, Int, String, String, Int, String) [String] => [String]
      collect_live_if_ok root_path rel_path full_p (False, kstr, m, nl, sz, ow, gr, mt, lt) acc = acc
      collect_live_if_ok root_path rel_path full_p (True, kstr, m, nl, sz, ow, gr, mt, lt) acc =
        | kstr == "dir" = collect_live_children root_path rel_path (list_dir full_p) (acc :: [rel_path])
        | otherwise = acc :: [rel_path]

fn :: collect_live_children String    String   [String] [String] => [String]
      collect_live_children root_path rel_path []       acc      =  acc
      collect_live_children root_path rel_path (c:rest) acc      =
        c_rel = join_clean_path rel_path c
        acc1 = collect_live_rels root_path c_rel acc
        collect_live_children root_path rel_path rest acc1

fn :: verify_single_entry MtreeEntry String    Bool   => [String]
      verify_single_entry spec_ent   root_path update =
        rel = me_rel spec_ent
        full_p = resolve_mtree_full root_path rel
        raw = file_lstat full_p
        verify_lstat_result spec_ent full_p raw update

fn :: verify_lstat_result MtreeEntry String (Bool, String, Int, Int, Int, String, String, Int, String) Bool => [String]
      verify_lstat_result spec_ent full_p (False, kstr, m, nl, sz, ow, gr, mt, lt) update =
        _ = heal_missing_dir spec_ent full_p update
        [(me_rel spec_ent) + ": missing"]
      verify_lstat_result spec_ent full_p (True, kstr, m, nl, sz, ow, gr, mt, lt) update =
        m1 = check_mode_diff spec_ent full_p m update []
        m2 = check_size_diff spec_ent kstr sz m1
        check_sha256_diff spec_ent full_p kstr m2

fn :: heal_missing_dir MtreeEntry String Bool  => Int
      heal_missing_dir spec_ent   full_p False =  0
      heal_missing_dir spec_ent   full_p True  =
        | (me_type spec_ent) == "dir" = sys_mkdir full_p (me_mode spec_ent)
        | otherwise = 0

fn :: check_mode_diff MtreeEntry String Int Bool [String] => [String]
      check_mode_diff spec_ent full_p actual_mode update acc =
        | (me_mode spec_ent) == actual_mode = acc
        | otherwise =
          _ = heal_mode_diff full_p (me_mode spec_ent) update
          msg = (me_rel spec_ent) + ": permissions (" + (int_to_octal_4 (me_mode spec_ent)) + ", " + (int_to_octal_4 actual_mode) + ")"
          acc :: [msg]

fn :: heal_mode_diff String Int      Bool  => Int
      heal_mode_diff full_p exp_mode False =  0
      heal_mode_diff full_p exp_mode True  =  sys_chmod full_p exp_mode

fn :: check_size_diff MtreeEntry String Int       [String] => [String]
      check_size_diff spec_ent   kstr   actual_sz acc      =
        | (kstr != "file") or ((me_size spec_ent) < 0) = acc
        | (me_size spec_ent) == actual_sz = acc
        | otherwise = acc :: [(me_rel spec_ent) + ": size (" + (int_to_str (me_size spec_ent)) + ", " + (int_to_str actual_sz) + ")"]

fn :: check_sha256_diff MtreeEntry String String [String] => [String]
      check_sha256_diff spec_ent   full_p kstr   acc      =
        | (kstr != "file") or ((str_len (me_sha256 spec_ent)) == 0) = acc
        | otherwise = compare_sha256_val (me_rel spec_ent) (me_sha256 spec_ent) (sha256_file full_p) acc

fn :: compare_sha256_val String String   String   [String] => [String]
      compare_sha256_val rel    exp_hash act_hash acc      =
        | exp_hash == act_hash = acc
        | otherwise = acc :: [rel + ": sha256 (" + exp_hash + ", " + act_hash + ")"]

fn :: verify_spec_entries [MtreeEntry] String    Bool   [String] => [String]
      verify_spec_entries []           root_path update acc      =  acc
      verify_spec_entries (e:rest)     root_path update acc      =
        diffs = verify_single_entry e root_path update
        verify_spec_entries rest root_path update (acc :: diffs)

fn :: has_spec_rel [MtreeEntry] String => Bool
      has_spec_rel []           rel    =  False
      has_spec_rel (e:rest)     rel    =
        | (me_rel e) == rel = True
        | otherwise = has_spec_rel rest rel

fn :: find_extra_entries [String]   [MtreeEntry] [String] => [String]
      find_extra_entries []         spec_entries acc      =  acc
      find_extra_entries (rel:rest) spec_entries acc      =
        | has_spec_rel spec_entries rel = find_extra_entries rest spec_entries acc
        | otherwise = find_extra_entries rest spec_entries (acc :: [rel + ": extra"])

op :: verify mtree [MtreeEntry] against String    update Bool => [String]
      verify mtree spec_entries against root_path update upd  =
        spec_diffs = verify_spec_entries spec_entries root_path upd []
        live_rels = collect_live_rels root_path "." []
        extra_diffs = find_extra_entries live_rels spec_entries []
        spec_diffs :: extra_diffs

-- ============================================================================
-- 4. BSD mktemp(1) Implementation (FR-FS-004)
-- ============================================================================

fn :: default_mktemp_spec => MktempSpec
      default_mktemp_spec =  MktempSpec False False False "/tmp" "" ""

fn :: parse_mktemp_loop [String]        MktempSpec => MktempSpec
      parse_mktemp_loop []              spec       =  spec
      parse_mktemp_loop ("-d":rest)     spec       =  parse_mktemp_loop rest (spec with mkt_dir: True)
      parse_mktemp_loop ("-u":rest)     spec       =  parse_mktemp_loop rest (spec with mkt_unsafe: True)
      parse_mktemp_loop ("-q":rest)     spec       =  parse_mktemp_loop rest (spec with mkt_quiet: True)
      parse_mktemp_loop ("-p":dir:rest) spec       =  parse_mktemp_loop rest (spec with mkt_tmpdir: dir)
      parse_mktemp_loop ("-t":pfx:rest) spec       =  parse_mktemp_loop rest (spec with mkt_prefix: pfx)
      parse_mktemp_loop (arg:rest)      spec       =  parse_mktemp_loop rest (spec with mkt_template: arg)

op :: parse mktemp args [String] => MktempSpec
      parse mktemp args args     =  parse_mktemp_loop args default_mktemp_spec

fn :: mktemp_alnum_table => String
      mktemp_alnum_table =  "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"

fn :: mktemp_char_from_byte Int => String
      mktemp_char_from_byte b   =
        idx = ((b + 256) % 62)
        str_slice mktemp_alnum_table idx (idx + 1)

fn :: count_trailing_x String Int => Int
      count_trailing_x tmpl   pos =
        | pos <= 0 = 0
        | (str_char_at tmpl (pos - 1)) == 88 = 1 + (count_trailing_x tmpl (pos - 1))
        | otherwise = 0

fn :: build_random_suffix [Int]    Int Int  => String
      build_random_suffix _        0   _    =  ""
      build_random_suffix []       rem seed =
        ch = mktemp_char_from_byte ((seed * 31) + rem)
        ch + (build_random_suffix [] (rem - 1) ((seed * 17) + 7))
      build_random_suffix (b:rest) rem seed =
        ch = mktemp_char_from_byte (b + seed)
        ch + (build_random_suffix rest (rem - 1) (seed + 13))

fn :: resolve_mktemp_template MktempSpec => String
      resolve_mktemp_template spec       =
        | (str_len (mkt_template spec)) > 0 = mkt_template spec
        | (str_len (mkt_prefix spec)) > 0   = (mkt_tmpdir spec) + (path separator (sys os)) + (mkt_prefix spec) + ".XXXXXXXX"
        | otherwise                         = (mkt_tmpdir spec) + (path separator (sys os)) + "tmp.XXXXXXXXXX"

fn :: expand_mktemp_with String Int  Int Int     => String
      expand_mktemp_with tmpl   tlen nx  attempt =
        | nx <= 0   = expand_mktemp_with (tmpl + ".XXXXXXXX") (tlen + 9) 8 attempt
        | otherwise =
          rng = snd (sys random bytes list nx)
          suf = build_random_suffix rng nx ((sys getpid) + (attempt * 97))
          (str_slice tmpl 0 (tlen - nx)) + suf

op :: expand mktemp template MktempSpec => String
expand mktemp template spec       =
  tmpl = resolve_mktemp_template spec
  tlen = str_len tmpl
  expand_mktemp_with tmpl tlen (count_trailing_x tmpl tlen) 0

fn :: expand_mktemp_attempt MktempSpec Int     => String
      expand_mktemp_attempt spec       attempt =
        tmpl = resolve_mktemp_template spec
        tlen = str_len tmpl
        expand_mktemp_with tmpl tlen (count_trailing_x tmpl tlen) attempt

fn :: finish_mktemp_fd String Int => Bool
      finish_mktemp_fd path   fd  =
        | fd < 0    = False
        | otherwise =
          _ = posix close fd
          _ = posix chmod path 384
          True

fn :: create_mktemp_file String => Bool
      create_mktemp_file path   =
        finish_mktemp_fd path (posix open path ((o wronly) + (o creat) + (o excl)) 384)

fn :: create_mktemp_dir String => Bool
      create_mktemp_dir path   =
        | not (posix mkdir path) = False
        | otherwise              = posix chmod path 448

fn :: dispatch_mktemp_cand MktempSpec String => (Bool, String)
      dispatch_mktemp_cand spec       cand   =
        | mkt_unsafe spec = (not (lstat exists (file_lstat cand)), cand)
        | mkt_dir spec    = (create_mktemp_dir cand, cand)
        | otherwise       = (create_mktemp_file cand, cand)

fn :: format_mktemp_result MktempSpec (Bool, String) => (Bool, String)
      format_mktemp_result spec       (True, cand)   =  (True, cand)
      format_mktemp_result spec       (False, cand)  =
        | mkt_quiet spec = (False, "")
        | otherwise      = (False, "mktemp: cannot create temp file " + cand)

fn :: execute_mktemp_loop MktempSpec Int     => (Bool, String)
      execute_mktemp_loop spec       attempt =
        | attempt >= 16 = format_mktemp_result spec (False, expand_mktemp_attempt spec attempt)
        | otherwise     =
          res = dispatch_mktemp_cand spec (expand_mktemp_attempt spec attempt)
          check_mktemp_attempt spec attempt res

fn :: check_mktemp_attempt MktempSpec Int     (Bool, String) => (Bool, String)
      check_mktemp_attempt _          _       (True, cand)   =  (True, cand)
      check_mktemp_attempt spec       attempt (False, _)     =  execute_mktemp_loop spec (attempt + 1)

op :: execute mktemp MktempSpec => (Bool, String)
      execute mktemp spec       =
        execute_mktemp_loop spec 0
