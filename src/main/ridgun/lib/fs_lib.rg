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

type LsSpec = l_one_per_line: Bool, l_multi_col: Bool, l_across: Bool, l_comma: Bool, l_long: Bool, l_dir_self: Bool, l_all: Bool, l_almost_all: Bool, l_unsorted: Bool, l_classify: Bool, l_slash_dir: Bool, l_nonprint_q: Bool, l_human: Bool, l_sort_size: Bool, l_sort_time: Bool, l_reverse: Bool, l_recursive: Bool, l_color: Bool, l_inode: Bool, l_numeric: Bool, l_blocks: Bool, l_paths: [String]

type FindExpr = ETrue | EName String | EIname String | EPath String | EType String | ESize Int Int | EMaxDepth Int | EMinDepth Int

expand type FindExpr = EEmpty | EPrune | EPrint | ENewer Int Int | EPerm Int Int | ELinks Int Int | EInum Int | ERegex String | ENot FindExpr | EAnd FindExpr FindExpr | EOr FindExpr FindExpr

type FindSpec = f_paths: [String], f_expr: FindExpr, f_max_depth: Int, f_min_depth: Int, f_print0: Bool, f_exec_cmd: [String], f_exec_plus: Bool, f_follow: Bool, f_delete: Bool, f_has_print: Bool

type MtreeEntry = me_rel: String, me_type: String, me_mode: Int, me_size: Int, me_sha256: String, me_link: String

type MtreeSpec = m_create: Bool, m_spec_file: String, m_path: String, m_update: Bool, m_ignore_extra: Bool, m_remove_extra: Bool, m_keywords: [String]

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

fn :: join path String String => String
      join path dir    name   =
        | (str len dir) == 0 = name
        | dir == "." = "./" + name
        | dir == "/" = "/" + name
        | str ends with dir "/" = dir + name
        | otherwise = dir + "/" + name

fn :: join clean path String String => String
      join clean path dir    name   =
        | (str len dir) == 0 = name
        | dir == "." = name
        | dir == "/" = "/" + name
        | str ends with dir "/" = dir + name
        | otherwise = dir + "/" + name

fn :: basename of String => String
      basename of path   =  basename loop path ((str len path) - 1)

fn :: basename loop String Int => String
      basename loop path   idx =
        | idx < 0 = path
        | (str char at path idx) == 47 = str slice path (idx + 1) (str len path)
        | otherwise = basename loop path (idx - 1)

fn :: to lower char Int => Int
      to lower char c   =
        | (c >= 65) and (c <= 90) = c + 32
        | otherwise = c

fn :: to lower str String => String
      to lower str s      =  to lower loop s 0 (str len s) ""

fn :: to lower loop String Int Int String => String
      to lower loop s idx len acc =
        | idx >= len = acc
        | otherwise = to lower loop s (idx + 1) len (acc + (char from code (to lower char (str char at s idx))))

fn :: str cmp String String => Int
      str cmp a      b      =  str cmp loop a b 0 (str len a) (str len b)

fn :: str cmp loop String String Int Int Int => Int
      str cmp loop a b idx lena lenb =
        | (idx >= lena) and (idx >= lenb) = 0
        | idx >= lena = 0 - 1
        | idx >= lenb = 1
        | (str char at a idx) < (str char at b idx) = 0 - 1
        | (str char at a idx) > (str char at b idx) = 1
        | otherwise = str cmp loop a b (idx + 1) lena lenb

-- ============================================================================
-- FileEntry Accessors & Metadata Loading
-- ============================================================================

fn :: fe name FileEntry => String
      fe name (Entry n fp k m nl sz ow gr mt lt) = n

fn :: fe path FileEntry => String
      fe path (Entry n fp k m nl sz ow gr mt lt) = fp

fn :: fe kind FileEntry => FileKind
      fe kind (Entry n fp k m nl sz ow gr mt lt) = k

fn :: fe mode FileEntry => Int
      fe mode (Entry n fp k m nl sz ow gr mt lt) = m

fn :: fe nlink FileEntry => Int
      fe nlink (Entry n fp k m nl sz ow gr mt lt) = nl

fn :: fe size FileEntry => Int
      fe size (Entry n fp k m nl sz ow gr mt lt) = sz

fn :: fe owner FileEntry => String
      fe owner (Entry n fp k m nl sz ow gr mt lt) = ow

fn :: fe group FileEntry => String
      fe group (Entry n fp k m nl sz ow gr mt lt) = gr

fn :: fe mtime FileEntry => Int
      fe mtime (Entry n fp k m nl sz ow gr mt lt) = mt

fn :: fe link target FileEntry => String
      fe link target (Entry n fp k m nl sz ow gr mt lt) = lt

fn :: parse file kind String => FileKind
      parse file kind s      =
        | s == "dir" = FDir
        | s == "link" = FSymLink
        | s == "fifo" = FFifo
        | s == "sock" = FSock
        | s == "block" = FBlock
        | s == "char" = FChar
        | otherwise = FReg

fn :: is dir kind FileKind => Bool
      is dir kind FDir     =  True
      is dir kind other    =  False

fn :: is reg kind FileKind => Bool
      is reg kind FReg     =  True
      is reg kind other    =  False

fn :: is link kind FileKind => Bool
      is link kind FSymLink =  True
      is link kind other    =  False

fn :: select stat syscall Bool  => Int
      select stat syscall True  =  4
      select stat syscall False =  6

fn :: raw kernel stat String Bool   => Bool Int Int Int Int Int Int Int Int Int
      raw kernel stat path   follow =
        | (str len path) == 0 = (False, 0, 1, 0, 0, 0, 0, 0, 0, 0)
        | otherwise =
          ptr = px alloc page 4096
          mfd = px alloc mfd
          _ = px poke mfd ptr (path + (px raw byte 0))
          sys_nr = select stat syscall follow
          rc = sys call sys_nr ptr (ptr + 2048) 0 0 0 0
          raw = px peek mfd (ptr + 2048) 144
          _ = px free page mfd ptr 4096
          decode raw stat rc raw

fn :: decode raw stat Int String => Bool Int Int Int Int Int Int Int Int Int
      decode raw stat rc  raw    =
        | (rc != 0) or ((str len raw) < 112) = (False, 0, 1, 0, 0, 0, 0, 0, 0, 0)
        | otherwise =
          ino = px decode u64 le raw 8
          nlink = max int 1 (px decode u64 le raw 16)
          mode = px decode u32 le raw 24
          uid = px decode u32 le raw 28
          gid = px decode u32 le raw 32
          sz = px decode u64 le raw 48
          blks = px decode u64 le raw 64
          mt_sec = px decode u64 le raw 88
          mt_nsec = px decode u64 le raw 96
          (True, ino, nlink, mode, uid, gid, sz, blks, mt_sec, mt_nsec)

fn :: kstat ok (Bool, Int, Int, Int, Int, Int, Int, Int, Int, Int) => Bool
      kstat ok (ok, _, _, _, _, _, _, _, _, _)                     =  ok

fn :: kstat ino (Bool, Int, Int, Int, Int, Int, Int, Int, Int, Int) => Int
      kstat ino (_, ino, _, _, _, _, _, _, _, _)                    =  ino

fn :: kstat nlink (Bool, Int, Int, Int, Int, Int, Int, Int, Int, Int) => Int
      kstat nlink (_, _, nl, _, _, _, _, _, _, _)                     =  nl

fn :: kstat uid (Bool, Int, Int, Int, Int, Int, Int, Int, Int, Int) => Int
      kstat uid (_, _, _, _, uid, _, _, _, _, _)                    =  uid

fn :: kstat gid (Bool, Int, Int, Int, Int, Int, Int, Int, Int, Int) => Int
      kstat gid (_, _, _, _, _, gid, _, _, _, _)                    =  gid

fn :: kstat blocks (Bool, Int, Int, Int, Int, Int, Int, Int, Int, Int) => Int
      kstat blocks (_, _, _, _, _, _, _, blks, _, _)                   =  blks

fn :: kstat mtime sec (Bool, Int, Int, Int, Int, Int, Int, Int, Int, Int) => Int
      kstat mtime sec (_, _, _, _, _, _, _, _, ms, _)                     =  ms

fn :: kstat mtime nsec (Bool, Int, Int, Int, Int, Int, Int, Int, Int, Int) => Int
      kstat mtime nsec (_, _, _, _, _, _, _, _, _, mn) = mn

fn :: stat to entry String String (Bool, String, Int, Int, Int, String, String, Int, String) => FileEntry
      stat to entry name full_path (ok, kstr, mode, nlink, size, owner, group, mtime, link_target) =
        ks = raw kernel stat full_path False
        real_nl = select real nlink (kstat ok ks) (kstat nlink ks) nlink
        Entry name full_path (parse file kind kstr) mode real_nl size owner group mtime link_target

fn :: select real nlink Bool Int Int => Int
      select real nlink True knl _ = knl
      select real nlink False _ fnl = fnl

fn :: lstat ok (Bool, String, Int, Int, Int, String, String, Int, String) => Bool
      lstat ok (ok, kstr, mode, nlink, size, owner, group, mtime, link_target) = ok

op :: stat file entry String    named String => FileEntry
      stat file entry full_path named name   =
        raw = file lstat full_path
        stat to entry name full_path raw

-- ============================================================================
-- 1. BSD ls(1) — FR-FS-001
-- ============================================================================

fn :: default ls spec => LsSpec
      default ls spec =  LsSpec False True False False False False False False False False False False False False False False False False False False False []

op :: parse ls args [String] => LsSpec
      parse ls args args     =  parse ls loop args (default ls spec) False

fn :: apply ls flag Int LsSpec => LsSpec
      apply ls flag c   spec   =
        | c == 49 = spec with l_one_per_line = True, l_multi_col = False, l_across = False, l_comma = False, l_long = False
        | c == 67 = spec with l_one_per_line = False, l_multi_col = True, l_across = False, l_comma = False, l_long = False
        | c == 120 = spec with l_one_per_line = False, l_multi_col = False, l_across = True, l_comma = False, l_long = False
        | c == 109 = spec with l_one_per_line = False, l_multi_col = False, l_across = False, l_comma = True, l_long = False
        | c == 108 = spec with l_one_per_line = False, l_multi_col = False, l_across = False, l_comma = False, l_long = True
        | c == 110 = spec with l_one_per_line = False, l_multi_col = False, l_across = False, l_comma = False, l_long = True, l_numeric = True
        | c == 105 = spec with l_inode = True
        | c == 115 = spec with l_blocks = True
        | c == 100 = spec with l_dir_self = True
        | c == 97 = spec with l_all = True
        | c == 65 = spec with l_almost_all = True
        | c == 102 = spec with l_all = True, l_unsorted = True
        | c == 70 = spec with l_classify = True, l_slash_dir = False
        | c == 112 = spec with l_slash_dir = True, l_classify = False
        | c == 113 = spec with l_nonprint_q = True
        | c == 104 = spec with l_human = True
        | c == 83 = spec with l_sort_size = True, l_sort_time = False
        | c == 116 = spec with l_sort_size = False, l_sort_time = True
        | c == 114 = spec with l_reverse = True
        | c == 82 = spec with l_recursive = True
        | c == 71 = spec with l_color = True
        | otherwise = spec

fn :: apply ls cluster String Int Int LsSpec => LsSpec
      apply ls cluster s idx len spec =
        | idx >= len = spec
        | otherwise = apply ls cluster s (idx + 1) len (apply ls flag (str char at s idx) spec)

fn :: parse ls loop [String] LsSpec Bool => LsSpec
      parse ls loop [] spec stop_opts = finalize ls paths spec
      parse ls loop (arg:rest) spec True = parse ls loop rest (spec with l_paths = ((l_paths spec) :: [arg])) True
      parse ls loop (arg:rest) spec False =
        | arg == "--" = parse ls loop rest spec True
        | (str starts with arg "-") and ((str len arg) > 1) = parse ls loop rest (apply ls cluster arg 1 (str len arg) spec) False
        | otherwise = parse ls loop rest (spec with l_paths = ((l_paths spec) :: [arg])) False

fn :: finalize ls paths LsSpec => LsSpec
      finalize ls paths spec   =
        | (list len (l_paths spec)) == 0 = spec with l_paths = ["."]
        | otherwise = spec

fn :: is exec mode Int  => Bool
      is exec mode mode =
        u_x = (mode / 64) % 2
        g_x = (mode / 8) % 2
        o_x = mode % 2
        (u_x == 1) or (g_x == 1) or (o_x == 1)

op :: classify suffix FileEntry => String
      classify suffix entry     =  classify kind mode (fe kind entry) (fe mode entry)

fn :: classify kind mode FileKind Int  => String
      classify kind mode FDir     mode =  "/"
      classify kind mode FSymLink mode =  "@"
      classify kind mode FFifo    mode =  "|"
      classify kind mode FSock    mode =  "="
      classify kind mode FReg     mode =
        | is exec mode mode = "*"
        | otherwise = ""
      classify kind mode other    mode =  ""

fn :: kind type char FileKind => String
      kind type char FDir     =  "d"
      kind type char FSymLink =  "l"
      kind type char FFifo    =  "p"
      kind type char FSock    =  "s"
      kind type char FBlock   =  "b"
      kind type char FChar    =  "c"
      kind type char FReg     =  "-"

fn :: rwx triplet Int Int Bool String String => String
      rwx triplet bits special_bit is_special sp_exec sp_noexec =
        r_ch = select bit char ((bits / 4) % 2) "r" "-"
        w_ch = select bit char ((bits / 2) % 2) "w" "-"
        x_bit = bits % 2
        x_ch = select exec char x_bit is_special sp_exec sp_noexec
        r_ch + w_ch + x_ch

fn :: select bit char Int String String => String
      select bit char 1 yes no = yes
      select bit char other yes no = no

fn :: select exec char Int Bool String String => String
      select exec char 1 True sp_exec sp_noexec = sp_exec
      select exec char 0 True sp_exec sp_noexec = sp_noexec
      select exec char 1 False sp_exec sp_noexec = "x"
      select exec char other False sp_exec sp_noexec = "-"

op :: format mode string FileKind Int  => String
      format mode string kind     mode =
        suid = ((mode / 2048) % 2) == 1
        sgid = ((mode / 1024) % 2) == 1
        sticky = ((mode / 512) % 2) == 1
        u_bits = (mode / 64) % 8
        g_bits = (mode / 8) % 8
        o_bits = mode % 8
        (kind type char kind) + (rwx triplet u_bits 0 suid "s" "S") + (rwx triplet g_bits 0 sgid "s" "S") + (rwx triplet o_bits 0 sticky "t" "T")

op :: format human size Int   => String
      format human size bytes =
        | bytes < 1000 = (int to str bytes) + "B"
        | bytes < 10240 = format tenth unit bytes 1024 "K"
        | bytes < 1048576 = (int to str ((bytes + 512) / 1024)) + "K"
        | bytes < 10485760 = format tenth unit bytes 1048576 "M"
        | bytes < 1073741824 = (int to str ((bytes + 524288) / 1048576)) + "M"
        | otherwise = format tenth unit bytes 1073741824 "G"

fn :: format tenth unit Int Int String => String
      format tenth unit bytes unit suffix =
        tenths = ((bytes * 10) + (unit / 2)) / unit
        whole = tenths / 10
        frac = tenths % 10
        (int to str whole) + "." + (int to str frac) + suffix

fn :: resolve lscolors => String
      resolve lscolors =
        env_val = sys getenv "LSCOLORS"
        select lscolors env_val

fn :: select lscolors String  => String
      select lscolors env_val =
        | (str len env_val) >= 22 = env_val
        | otherwise = "exfxcxdxbxegedabagacad"

fn :: lscolors slot FileKind Int  => Int
      lscolors slot FDir     mode =
        | (((mode / 512) % 2) == 1) and (((mode / 2) % 2) == 1) = 9
        | ((mode / 2) % 2) == 1 = 10
        | otherwise = 0
      lscolors slot FSymLink mode =  1
      lscolors slot FSock    mode =  2
      lscolors slot FFifo    mode =  3
      lscolors slot FBlock   mode =  5
      lscolors slot FChar    mode =  6
      lscolors slot FReg     mode =
        | (((mode / 2048) % 2) == 1) and (is exec mode mode) = 7
        | (((mode / 1024) % 2) == 1) and (is exec mode mode) = 8
        | is exec mode mode = 4
        | otherwise = 0 - 1

fn :: ansi fg for bsd Int => String
      ansi fg for bsd c   =
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

fn :: colorize name String FileEntry Bool  => String
      colorize name name   entry     False =  name
      colorize name name   entry     True  =
        slot = lscolors slot (fe kind entry) (fe mode entry)
        apply color slot name slot (resolve lscolors)

fn :: apply color slot String Int String => String
      apply color slot name slot colors =
        | slot < 0 = name
        | otherwise = wrap ansi fg name (ansi fg for bsd (str char at colors (slot * 2)))

fn :: wrap ansi fg String String => String
      wrap ansi fg name   code   =
        | (str len code) == 0 = name
        | otherwise = (char from code 27) + "[" + code + "m" + name + (char from code 27) + "[0m"

fn :: sanitize nonprint loop String Int Int String => String
      sanitize nonprint loop s idx len acc =
        | idx >= len = acc
        | ((str char at s idx) < 32) or ((str char at s idx) == 127) = sanitize nonprint loop s (idx + 1) len (acc + "?")
        | otherwise = sanitize nonprint loop s (idx + 1) len (acc + (str slice s idx (idx + 1)))

fn :: sanitize ls name String Bool  => String
      sanitize ls name s      False =  s
      sanitize ls name s      True  =  sanitize nonprint loop s 0 (str len s) ""

fn :: select entry suffix FileEntry LsSpec => String
      select entry suffix entry     spec   =
        | l_classify spec                        = classify suffix entry
        | (l_slash_dir spec) and (is dir kind (fe kind entry)) = "/"
        | otherwise                              = ""

fn :: format entry display name FileEntry LsSpec => String
      format entry display name entry spec =
        clean_n = sanitize ls name (fe name entry) (l_nonprint_q spec)
        colored = colorize name clean_n entry (l_color spec)
        suf = select entry suffix entry spec
        colored + suf

fn :: select classify suffix FileEntry Bool  => String
      select classify suffix entry     False =  ""
      select classify suffix entry     True  =  classify suffix entry

fn :: compare entries FileEntry FileEntry LsSpec => Int
      compare entries a         b         spec   =
        raw_cmp = compare entries primary a b spec
        apply reverse cmp raw_cmp (l_reverse spec)

fn :: apply reverse cmp Int Bool  => Int
      apply reverse cmp cmp False =  cmp
      apply reverse cmp cmp True  =  0 - cmp

fn :: compare entries primary FileEntry FileEntry LsSpec => Int
      compare entries primary a b spec =
        | l_sort_size spec = compare int desc (fe size a) (fe size b) (fe name a) (fe name b)
        | l_sort_time spec = compare int desc (fe mtime a) (fe mtime b) (fe name a) (fe name b)
        | otherwise = str cmp (fe name a) (fe name b)

fn :: compare int desc Int Int String String => Int
      compare int desc va vb na nb =
        | va > vb = 0 - 1
        | va < vb = 1
        | otherwise = str cmp na nb

op :: sort entries [FileEntry] with LsSpec => [FileEntry]
      sort entries []          with spec   =  []
      sort entries (x:xs)      with spec   =
        | l_unsorted spec = [x] :: xs
        | otherwise       = insert sorted entry x (sort entries xs with spec) spec

fn :: insert sorted entry FileEntry [FileEntry] LsSpec => [FileEntry]
      insert sorted entry item [] spec = [item]
      insert sorted entry item (y:ys) spec =
        | (compare entries item y spec) <= 0 = [item, y] :: ys
        | otherwise = [y] :: (insert sorted entry item ys spec)

fn :: filter ls names [String] LsSpec => [String]
      filter ls names names    spec   =
        | l_all spec = [".", ".."] :: names
        | l_almost_all spec = names
        | otherwise = filter visible names names []

fn :: filter visible names [String] [String] => [String]
      filter visible names []       acc      =  acc
      filter visible names (n:rest) acc      =
        | str starts with n "." = filter visible names rest acc
        | otherwise = filter visible names rest (acc :: [n])

fn :: load dir entries String [String] [FileEntry] => [FileEntry]
      load dir entries dir_path [] acc = acc
      load dir entries dir_path (name:rest) acc =
        fp = join path dir_path name
        entry = stat file entry fp named name
        load dir entries dir_path rest (acc :: [entry])

fn :: format size field Int Bool  => String
      format size field sz  False =  int to str sz
      format size field sz  True  =  format human size sz

fn :: format mtime short Int => String
      format mtime short mt  =
        day_sec = mt % 86400
        hh = (day_sec / 3600) % 24
        mm = (day_sec / 60) % 60
        "Oct 18 " + (pad2 zero hh) + ":" + (pad2 zero mm)

fn :: pad2 zero Int => String
      pad2 zero n   =
        | n < 10 = "0" + (int to str n)
        | otherwise = int to str n

fn :: resolve entry prefix FileEntry LsSpec => String
      resolve entry prefix entry     spec   =
        | (not (l_inode spec)) and (not (l_blocks spec)) = ""
        | otherwise =
          ks = raw kernel stat (fe path entry) False
          ino_part = format opt inode (l_inode spec) (kstat ino ks)
          blk_part = format opt blocks (l_blocks spec) (kstat blocks ks)
          ino_part + blk_part

fn :: format opt inode Bool  Int => String
      format opt inode False _   =  ""
      format opt inode True  ino =  (int to str ino) + " "

fn :: format opt blocks Bool  Int  => String
      format opt blocks False _    =  ""
      format opt blocks True  blks =  (int to str blks) + " "

fn :: resolve owner str FileEntry Bool  => String
      resolve owner str entry     False =  fe owner entry
      resolve owner str entry     True  =
        ks = raw kernel stat (fe path entry) False
        select uid str (kstat ok ks) (kstat uid ks) (fe owner entry)

fn :: select uid str Bool Int String => String
      select uid str True uid _ = int to str uid
      select uid str False _ fallback = fallback

fn :: resolve group str FileEntry Bool  => String
      resolve group str entry     False =  fe group entry
      resolve group str entry     True  =
        ks = raw kernel stat (fe path entry) False
        select uid str (kstat ok ks) (kstat gid ks) (fe group entry)

fn :: format long entry FileEntry LsSpec Int Int Int Int => String
      format long entry entry spec w_nlink w_owner w_group w_size =
        pfx = resolve entry prefix entry spec
        m_str = format mode string (fe kind entry) (fe mode entry)
        nl_str = pad left (int to str (fe nlink entry)) w_nlink
        ow_str = pad right (resolve owner str entry (l_numeric spec)) w_owner
        gr_str = pad right (resolve group str entry (l_numeric spec)) w_group
        sz_str = pad left (format size field (fe size entry) (l_human spec)) w_size
        dt_str = format mtime short (fe mtime entry)
        disp_name = format entry display name entry spec
        link_suffix = format symlink target entry
        pfx + m_str + " " + nl_str + " " + ow_str + " " + gr_str + " " + sz_str + " " + dt_str + " " + disp_name + link_suffix

fn :: format symlink target FileEntry => String
      format symlink target entry     =
        | (is link kind (fe kind entry)) and ((str len (fe link target entry)) > 0) = " -> " + (fe link target entry)
        | otherwise = ""

fn :: max entry widths [FileEntry] LsSpec Int Int Int Int => Int Int Int Int
      max entry widths [] spec wn wo wg ws = (wn, wo, wg, ws)
      max entry widths (e:rest) spec wn wo wg ws =
        n_len = str len (int to str (fe nlink e))
        o_len = str len (resolve owner str e (l_numeric spec))
        g_len = str len (resolve group str e (l_numeric spec))
        s_len = str len (format size field (fe size e) (l_human spec))
        max entry widths rest spec (max int wn n_len) (max int wo o_len) (max int wg g_len) (max int ws s_len)

fn :: render long entries [FileEntry] LsSpec (Int, Int, Int, Int) [String] => [String]
      render long entries [] spec widths acc = acc
      render long entries (e:rest) spec (wn, wo, wg, ws) acc =
        line = format long entry e spec wn wo wg ws
        render long entries rest spec (wn, wo, wg, ws) (acc :: [line])

fn :: render one per line [FileEntry] LsSpec [String] => [String]
      render one per line [] spec acc = acc
      render one per line (e:rest) spec acc =
        pfx = resolve entry prefix e spec
        render one per line rest spec (acc :: [pfx + (format entry display name e spec)])

fn :: visible entry len FileEntry LsSpec => Int
      visible entry len entry     spec   =
        (str len (fe name entry)) + (str len (select entry suffix entry spec))

fn :: max visible len [FileEntry] LsSpec Int => Int
      max visible len [] spec best = best
      max visible len (e:rest) spec best =
        max visible len rest spec (max int best (visible entry len e spec))

fn :: render multi col [FileEntry] LsSpec => [String]
      render multi col []          spec   =  []
      render multi col entries     spec   =
        total = list len entries
        max_w = max int 1 (max visible len entries spec 0)
        col_w = max_w + 2
        num_cols = max int 1 (80 / col_w)
        num_rows = ((total + num_cols - 1) / num_cols)
        render mc rows entries spec total num_rows num_cols col_w 0 []

fn :: render across col [FileEntry] LsSpec => [String]
      render across col []          spec   =  []
      render across col entries     spec   =
        total = list len entries
        max_w = max int 1 (max visible len entries spec 0)
        col_w = max_w + 2
        num_cols = max int 1 (80 / col_w)
        num_rows = ((total + num_cols - 1) / num_cols)
        render across rows entries spec total num_rows num_cols col_w 0 []

fn :: render across rows [FileEntry] LsSpec Int Int Int Int Int [String] => [String]
      render across rows entries spec total num_rows num_cols col_w r acc =
        | r >= num_rows = acc
        | otherwise     = render across rows entries spec total num_rows num_cols col_w (r + 1) (acc :: [render across row entries spec total num_cols col_w r 0 ""])

fn :: render across row [FileEntry] LsSpec Int Int Int Int Int String => String
      render across row entries spec total num_cols col_w r c acc =
        | c >= num_cols                  = acc
        | ((r * num_cols) + c) >= total  = acc
        | otherwise                      =
          idx = (r * num_cols) + c
          ent = entry list nth entries idx
          disp = format entry display name ent spec
          vlen = visible entry len ent spec
          next_idx = idx + 1
          has_more = ((c + 1) < num_cols) and (next_idx < total)
          padded = pad mc cell disp vlen col_w has_more
          render across row entries spec total num_cols col_w r (c + 1) (acc + padded)

fn :: render comma stream [FileEntry] LsSpec => [String]
      render comma stream []          spec   =  []
      render comma stream entries     spec   =
        items = render one per line entries spec []
        [join with items ", "]

fn :: entry list nth [FileEntry] Int => FileEntry
      entry list nth (x:xs)      n   =
        | n <= 0 = x
        | otherwise = entry list nth xs (n - 1)

fn :: render mc rows [FileEntry] LsSpec Int Int Int Int Int [String] => [String]
      render mc rows entries spec total num_rows num_cols col_w r acc =
        | r >= num_rows = acc
        | otherwise = render mc rows entries spec total num_rows num_cols col_w (r + 1) (acc :: [render mc row entries spec total num_rows num_cols col_w r 0 ""])

fn :: render mc row [FileEntry] LsSpec Int Int Int Int Int Int String => String
      render mc row entries spec total num_rows num_cols col_w r c acc =
        | c >= num_cols = acc
        | ((c * num_rows) + r) >= total = acc
        | otherwise =
          idx = (c * num_rows) + r
          ent = entry list nth entries idx
          disp = format entry display name ent spec
          vlen = visible entry len ent spec
          next_idx = ((c + 1) * num_rows) + r
          has_more = ((c + 1) < num_cols) and (next_idx < total)
          padded = pad mc cell disp vlen col_w has_more
          (render mc row entries spec total num_rows num_cols col_w r (c + 1) (acc + padded))

fn :: pad mc cell String Int Int Bool => String
      pad mc cell disp vlen col_w False = disp
      pad mc cell disp vlen col_w True = disp + (repeat str " " (max int 1 (col_w - vlen)))

op :: format ls entries [FileEntry] with LsSpec => [String]
      format ls entries entries     with spec   =
        | l_long spec         = render long entries entries spec (max entry widths entries spec 1 1 1 1) []
        | l_one_per_line spec = render one per line entries spec []
        | l_comma spec        = render comma stream entries spec
        | l_across spec       = render across col entries spec
        | otherwise           = render multi col entries spec

fn :: collect subdirs [FileEntry] [String] => [String]
      collect subdirs []          acc      =  acc
      collect subdirs (e:rest)    acc      =
        | (is dir kind (fe kind e)) and ((fe name e) != ".") and ((fe name e) != "..") = collect subdirs rest (acc :: [fe path e])
        | otherwise = collect subdirs rest acc

fn :: list single dir String LsSpec Bool => [String]
      list single dir dir_path spec show_header =
        raw_names = filter ls names (list dir dir_path) spec
        loaded = load dir entries dir_path raw_names []
        sorted = sort entries loaded with spec
        lines = format ls entries sorted with spec
        header_lines = prepend dir header dir_path show_header lines
        recurse subdirs sorted spec header_lines

fn :: prepend dir header String Bool [String] => [String]
      prepend dir header dir_path False lines = lines
      prepend dir header dir_path True lines = [dir_path + ":"] :: lines

fn :: recurse subdirs [FileEntry] LsSpec [String] => [String]
      recurse subdirs sorted      spec   lines    =
        | not (l_recursive spec) = lines
        | otherwise = recurse subdir list (collect subdirs sorted []) spec lines

fn :: recurse subdir list [String] LsSpec [String] => [String]
      recurse subdir list [] spec acc = acc
      recurse subdir list (sub:rest) spec acc =
        sub_lines = list single dir sub spec True
        next_acc = acc :: [""] :: sub_lines
        recurse subdir list rest spec next_acc

fn :: run ls paths [String] LsSpec Bool [String] => [String]
      run ls paths [] spec multi acc = acc
      run ls paths (p:rest) spec multi acc =
        ent = stat file entry p named p
        part = render ls target p ent spec (multi or (l_recursive spec))
        next_acc = append ls section acc part
        run ls paths rest spec multi next_acc

fn :: render ls target String FileEntry LsSpec Bool => [String]
      render ls target p ent spec show_header =
        | l_dir_self spec           = format ls entries [ent] with spec
        | is dir kind (fe kind ent) = list single dir p spec show_header
        | otherwise                 = format ls entries [ent] with spec

fn :: append ls section [String] [String] => [String]
      append ls section []       part     =  part
      append ls section acc      []       =  acc
      append ls section acc      part     =  acc :: [""] :: part

op :: evaluate ls LsSpec => String
      evaluate ls spec   =
        paths = l_paths spec
        multi = (list len paths) > 1
        lines = run ls paths paths spec multi []
        finalize output lines lines

op :: run ls [String] => String
      run ls args     =
        evaluate ls (parse ls args args)

fn :: finalize output lines [String] => String
      finalize output lines []       =  ""
      finalize output lines lines    =  (join with lines (char from code 10)) + (char from code 10)

-- ============================================================================
-- 2. BSD find(1) — FR-FS-002
-- ============================================================================

op :: glob match String  against String => Bool
      glob match pattern against text   =  glob loop pattern 0 (str len pattern) text 0 (str len text)

fn :: glob loop String Int Int String Int Int => Bool
      glob loop pat pi plen txt ti tlen =
        | (pi >= plen) and (ti >= tlen) = True
        | (pi < plen) and ((str char at pat pi) == 42) = glob star pat (pi + 1) plen txt ti tlen
        | (pi >= plen) or (ti >= tlen) = False
        | (str char at pat pi) == 63 = glob loop pat (pi + 1) plen txt (ti + 1) tlen
        | (str char at pat pi) == 91 = glob bracket pat (pi + 1) plen txt ti tlen
        | (str char at pat pi) == (str char at txt ti) = glob loop pat (pi + 1) plen txt (ti + 1) tlen
        | otherwise = False

fn :: glob star String Int Int String Int Int => Bool
      glob star pat pi plen txt ti tlen =
        | glob loop pat pi plen txt ti tlen = True
        | ti < tlen = glob star pat pi plen txt (ti + 1) tlen
        | otherwise = False

fn :: glob bracket String Int Int String Int Int => Bool
      glob bracket pat pi plen txt ti tlen =
        neg = (pi < plen) and (((str char at pat pi) == 33) or ((str char at pat pi) == 94))
        start_idx = select bracket start pi neg
        res = match bracket chars pat start_idx plen (str char at txt ti) False
        matched = fst res
        end_pi = snd res
        check bracket result pat end_pi plen txt ti tlen matched neg

fn :: select bracket start Int Bool  => Int
      select bracket start pi  False =  pi
      select bracket start pi  True  =  pi + 1

fn :: match bracket chars String Int Int Int Bool => Bool Int
      match bracket chars pat idx plen ch matched =
        | idx >= plen = (False, plen)
        | (str char at pat idx) == 93 = (matched, idx + 1)
        | ((idx + 2) < plen) and ((str char at pat (idx + 1)) == 45) and ((str char at pat (idx + 2)) != 93) = match bracket range pat idx plen ch matched
        | (str char at pat idx) == ch = match bracket chars pat (idx + 1) plen ch True
        | otherwise = match bracket chars pat (idx + 1) plen ch matched

fn :: match bracket range String Int Int Int Bool => Bool Int
      match bracket range pat idx plen ch matched =
        lo = str char at pat idx
        hi = str char at pat (idx + 2)
        in_rng = (ch >= lo) and (ch <= hi)
        match bracket chars pat (idx + 3) plen ch (matched or in_rng)

fn :: check bracket result String Int Int String Int Int Bool Bool => Bool
      check bracket result pat end_pi plen txt ti tlen matched neg =
        | end_pi > plen = False
        | (not neg) and matched = glob loop pat end_pi plen txt (ti + 1) tlen
        | neg and (not matched) = glob loop pat end_pi plen txt (ti + 1) tlen
        | otherwise = False

fn :: extract lead follow [String] Bool   => Bool [String]
      extract lead follow []       follow =  (follow, [])
      extract lead follow (x:xs)   follow =
        | x == "-L" = extract lead follow xs True
        | (x == "-P") or (x == "-H") = extract lead follow xs False
        | otherwise = (follow, [x] :: xs)

fn :: is find token String => Bool
      is find token s      =
        | (s == "(") or (s == ")") or (s == "!") = True
        | (str starts with s "-") and ((str len s) > 1) = True
        | otherwise = False

fn :: split find paths [String] [String] => [String] [String]
      split find paths []       acc      =
        | (list len acc) == 0 = (["."], [])
        | otherwise = (acc, [])
      split find paths (x:xs)   acc      =
        | is find token x = (default if empty acc, [x] :: xs)
        | otherwise = split find paths xs (acc :: [x])

fn :: default if empty [String] => [String]
      default if empty []       =  ["."]
      default if empty xs       =  xs

fn :: parse size arg String => Int Int
      parse size arg s      =
        | (str len s) == 0 = (0, 0)
        | (str char at s 0) == 43 = (1, parse size bytes (str slice s 1 (str len s)))
        | (str char at s 0) == 45 = (0 - 1, parse size bytes (str slice s 1 (str len s)))
        | otherwise = (0, parse size bytes s)

fn :: parse links arg String => Int Int
      parse links arg s      =
        | (str len s) == 0 = (0, 0)
        | (str char at s 0) == 43 = (1, str to int (str slice s 1 (str len s)))
        | (str char at s 0) == 45 = (0 - 1, str to int (str slice s 1 (str len s)))
        | otherwise = (0, str to int s)

fn :: parse perm arg String => Int Int
      parse perm arg s      =
        | (str len s) == 0 = (0, 0)
        | (str char at s 0) == 45 = (1, octal str to int (str slice s 1 (str len s)))
        | ((str char at s 0) == 47) or ((str char at s 0) == 43) = (2, octal str to int (str slice s 1 (str len s)))
        | otherwise = (0, octal str to int s)

fn :: parse size bytes String => Int
      parse size bytes s      =
        len = str len s
        last_ch = select last char s len
        parse size with unit s len last_ch

fn :: select last char String Int => Int
      select last char s      len =
        | len <= 0 = 0
        | otherwise = str char at s (len - 1)

fn :: parse size with unit String Int Int => Int
      parse size with unit s len last_ch =
        | last_ch == 99 = str to int (str slice s 0 (len - 1))
        | last_ch == 107 = (str to int (str slice s 0 (len - 1))) * 1024
        | last_ch == 77 = (str to int (str slice s 0 (len - 1))) * 1048576
        | last_ch == 71 = (str to int (str slice s 0 (len - 1))) * 1073741824
        | otherwise = (str to int s) * 512

fn :: extract find options [String] Int Int Bool [String] Bool Bool Bool [String] => Int Int Bool [String] Bool Bool Bool [String]
      extract find options [] max_d min_d p0 exec_c exec_pl del has_pr rem = (max_d, min_d, p0, exec_c, exec_pl, del, has_pr, rem)
      extract find options (t:rest) max_d min_d p0 exec_c exec_pl del has_pr rem =
        | t == "-maxdepth" = extract maxdepth rest max_d min_d p0 exec_c exec_pl del has_pr rem
        | t == "-mindepth" = extract mindepth rest max_d min_d p0 exec_c exec_pl del has_pr rem
        | t == "-print" = extract find options rest max_d min_d p0 exec_c exec_pl del True (rem :: ["-print"])
        | t == "-print0" = extract find options rest max_d min_d True exec_c exec_pl del has_pr rem
        | t == "-delete" = extract find options rest max_d min_d p0 exec_c exec_pl True has_pr rem
        | t == "-exec" = extract exec tokens rest max_d min_d p0 [] exec_pl del has_pr rem
        | otherwise = extract find options rest max_d min_d p0 exec_c exec_pl del has_pr (rem :: [t])

fn :: extract maxdepth [String] Int Int Bool [String] Bool Bool Bool [String] => Int Int Bool [String] Bool Bool Bool [String]
      extract maxdepth [] max_d min_d p0 exec_c exec_pl del has_pr rem = (max_d, min_d, p0, exec_c, exec_pl, del, has_pr, rem)
      extract maxdepth (v:rest) max_d min_d p0 exec_c exec_pl del has_pr rem =
        extract find options rest (str to int v) min_d p0 exec_c exec_pl del has_pr rem

fn :: extract mindepth [String] Int Int Bool [String] Bool Bool Bool [String] => Int Int Bool [String] Bool Bool Bool [String]
      extract mindepth [] max_d min_d p0 exec_c exec_pl del has_pr rem = (max_d, min_d, p0, exec_c, exec_pl, del, has_pr, rem)
      extract mindepth (v:rest) max_d min_d p0 exec_c exec_pl del has_pr rem =
        extract find options rest max_d (str to int v) p0 exec_c exec_pl del has_pr rem

fn :: extract exec tokens [String] Int Int Bool [String] Bool Bool Bool [String] => Int Int Bool [String] Bool Bool Bool [String]
      extract exec tokens [] max_d min_d p0 acc exec_pl del has_pr rem = (max_d, min_d, p0, acc, exec_pl, del, has_pr, rem)
      extract exec tokens (t:rest) max_d min_d p0 acc exec_pl del has_pr rem =
        | t == ";" = extract find options rest max_d min_d p0 acc False del has_pr rem
        | (t == "+") and ((list len acc) > 0) and ((str list nth acc ((list len acc) - 1)) == "\{\}") = extract find options rest max_d min_d p0 acc True del has_pr rem
        | otherwise = extract exec tokens rest max_d min_d p0 (acc :: [t]) exec_pl del has_pr rem

fn :: parse find expr [String] => FindExpr
      parse find expr []       =  ETrue
      parse find expr toks     =  fst (parse or expr toks)

fn :: parse or expr [String] => FindExpr [String]
      parse or expr toks     =
        left_res = parse and expr toks
        parse or tail (fst left_res) (snd left_res)

fn :: parse or tail FindExpr [String] => FindExpr [String]
      parse or tail left     []       =  (left, [])
      parse or tail left     (t:rest) =
        | (t == "-or") or (t == "-o") = combine or branch left (parse and expr rest)
        | otherwise = (left, [t] :: rest)

fn :: combine or branch FindExpr (FindExpr, [String]) => FindExpr [String]
      combine or branch left     (right, rem)         =  parse or tail (EOr left right) rem

fn :: parse and expr [String] => FindExpr [String]
      parse and expr toks     =
        left_res = parse unary expr toks
        parse and tail (fst left_res) (snd left_res)

fn :: parse and tail FindExpr [String] => FindExpr [String]
      parse and tail left     []       =  (left, [])
      parse and tail left     (t:rest) =
        | (t == "-and") or (t == "-a") = combine and branch left (parse unary expr rest)
        | (t == "-or") or (t == "-o") or (t == ")") = (left, [t] :: rest)
        | otherwise = combine and branch left (parse unary expr ([t] :: rest))

fn :: combine and branch FindExpr (FindExpr, [String]) => FindExpr [String]
      combine and branch left     (right, rem)         =  parse and tail (EAnd left right) rem

fn :: parse unary expr [String] => FindExpr [String]
      parse unary expr []       =  (ETrue, [])
      parse unary expr (t:rest) =
        | (t == "!") or (t == "-not") = wrap not branch (parse unary expr rest)
        | t == "(" = parse paren expr rest
        | t == "-name" = parse arg primary 1 rest
        | t == "-iname" = parse arg primary 2 rest
        | t == "-path" = parse arg primary 3 rest
        | t == "-type" = parse arg primary 4 rest
        | t == "-size" = parse arg primary 5 rest
        | t == "-newer" = parse arg primary 6 rest
        | t == "-perm" = parse arg primary 7 rest
        | t == "-links" = parse arg primary 8 rest
        | t == "-inum" = parse arg primary 9 rest
        | t == "-regex" = parse arg primary 10 rest
        | t == "-empty" = (EEmpty, rest)
        | t == "-prune" = (EPrune, rest)
        | t == "-print" = (EPrint, rest)
        | otherwise = (ETrue, rest)

fn :: wrap not branch (FindExpr, [String]) => FindExpr [String]
      wrap not branch (sub, rem)           =  (ENot sub, rem)

fn :: parse paren expr [String] => FindExpr [String]
      parse paren expr toks     =
        inner = parse or expr toks
        strip closing paren (fst inner) (snd inner)

fn :: strip closing paren FindExpr [String] => FindExpr [String]
      strip closing paren e        []       =  (e, [])
      strip closing paren e        (t:rest) =
        | t == ")" = (e, rest)
        | otherwise = (e, [t] :: rest)

fn :: parse arg primary Int  [String]   => FindExpr [String]
      parse arg primary kind []         =  (ETrue, [])
      parse arg primary kind (arg:rest) =
        | kind == 1 = (EName arg, rest)
        | kind == 2 = (EIname arg, rest)
        | kind == 3 = (EPath arg, rest)
        | kind == 4 = (EType arg, rest)
        | kind == 5 = build size expr (parse size arg arg) rest
        | kind == 6 = build newer expr arg rest
        | kind == 7 = build perm expr (parse perm arg arg) rest
        | kind == 8 = build links expr (parse links arg arg) rest
        | kind == 9 = (EInum (str to int arg), rest)
        | kind == 10 = (ERegex arg, rest)
        | otherwise = (ETrue, rest)

fn :: build size expr (Int, Int)    [String] => FindExpr [String]
      build size expr (cmp, target) rest     =  (ESize cmp target, rest)

fn :: build newer expr String   [String] => FindExpr [String]
      build newer expr ref_path rest     =
        ks = raw kernel stat ref_path True
        (ENewer (kstat mtime sec ks) (kstat mtime nsec ks), rest)

fn :: build perm expr (Int, Int)  [String] => FindExpr [String]
      build perm expr (pk, pmask) rest     =  (EPerm pk pmask, rest)

fn :: build links expr (Int, Int)    [String] => FindExpr [String]
      build links expr (cmp, target) rest     =  (ELinks cmp target, rest)

op :: parse find args [String] => FindSpec
      parse find args args     =
        lead_res = extract lead follow args False
        follow = fst lead_res
        rem_args = snd lead_res
        split_res = split find paths rem_args []
        paths = fst split_res
        raw_toks = snd split_res
        opt_res = extract find options raw_toks (0 - 1) 0 False [] False False False []
        build find spec paths follow opt_res

fn :: build find spec [String] Bool (Int, Int, Bool, [String], Bool, Bool, Bool, [String]) => FindSpec
      build find spec paths follow (max_d, min_d, p0, exec_c, exec_pl, del, has_pr, expr_toks) =
        FindSpec paths (parse find expr expr_toks) max_d min_d p0 exec_c exec_pl follow del has_pr

fn :: match find type FileKind String => Bool
      match find type FReg     "f"    =  True
      match find type FDir     "d"    =  True
      match find type FSymLink "l"    =  True
      match find type FFifo    "p"    =  True
      match find type FSock    "s"    =  True
      match find type FBlock   "b"    =  True
      match find type FChar    "c"    =  True
      match find type k        t      =  False

fn :: match find size Int Int Int => Bool
      match find size actual cmp target =
        | cmp > 0 = actual > target
        | cmp < 0 = actual < target
        | otherwise = actual == target

fn :: match find perm Int Int Int => Bool
      match find perm actual 0 mask = (actual % 4096) == mask
      match find perm actual 1 mask = ((actual % 4096) and mask) == mask
      match find perm actual _ mask = ((actual % 4096) and mask) != 0

fn :: match find newer FileEntry Int Int => Bool
      match find newer entry ref_sec ref_nsec =
        ks = raw kernel stat (fe path entry) False
        esec = select newer sec (kstat ok ks) (kstat mtime sec ks) (fe mtime entry)
        ensec = kstat mtime nsec ks
        (esec > ref_sec) or ((esec == ref_sec) and (ensec > ref_nsec))

fn :: select newer sec Bool Int Int => Int
      select newer sec True s _ = s
      select newer sec False _ fb = fb

fn :: match find inum FileEntry Int        => Bool
      match find inum entry     target_ino =
        ks = raw kernel stat (fe path entry) False
        (kstat ino ks) == target_ino

fn :: regex match String against String => Bool
      regex match pat    against txt    =
        clean_p = strip regex anchors pat
        regex loop clean_p 0 (str len clean_p) txt 0 (str len txt)

fn :: strip regex anchors String => String
      strip regex anchors p      =
        p1 = strip lead caret p
        strip trail dollar p1

fn :: strip lead caret String => String
      strip lead caret p      =
        | ((str len p) > 0) and ((str char at p 0) == 94) = str slice p 1 (str len p)
        | otherwise = p

fn :: strip trail dollar String => String
      strip trail dollar p      =
        | ((str len p) > 0) and ((str char at p ((str len p) - 1)) == 36) = str slice p 0 ((str len p) - 1)
        | otherwise = p

fn :: regex atom len String Int Int => Int
      regex atom len pat pi plen =
        | pi >= plen = 0
        | ((str char at pat pi) == 92) and ((pi + 1) < plen) = 2
        | otherwise = 1

fn :: regex char match String Int Int Int => Bool
      regex char match pat pi plen tc =
        | (str char at pat pi) == 46 = True
        | ((str char at pat pi) == 92) and ((pi + 1) < plen) = (str char at pat (pi + 1)) == tc
        | otherwise = (str char at pat pi) == tc

fn :: regex loop String Int Int String Int Int => Bool
      regex loop pat pi plen txt ti tlen =
        | (pi >= plen) and (ti >= tlen) = True
        | pi >= plen = False
        | otherwise =
          alen = regex atom len pat pi plen
          q_idx = pi + alen
          has_star = (q_idx < plen) and ((str char at pat q_idx) == 42)
          has_plus = (q_idx < plen) and ((str char at pat q_idx) == 43)
          has_qmark = (q_idx < plen) and ((str char at pat q_idx) == 63)
          regex step quant pat pi plen txt ti tlen alen has_star has_plus has_qmark

fn :: regex step quant String Int Int String Int Int Int Bool Bool Bool => Bool
      regex step quant pat pi plen txt ti tlen alen True _ _ =
        regex star pat pi (pi + alen + 1) plen txt ti tlen
      regex step quant pat pi plen txt ti tlen alen False True _ =
        | (ti < tlen) and (regex char match pat pi plen (str char at txt ti)) = regex star pat pi (pi + alen + 1) plen txt (ti + 1) tlen
        | otherwise = False
      regex step quant pat pi plen txt ti tlen alen False False True =
        | regex loop pat (pi + alen + 1) plen txt ti tlen = True
        | (ti < tlen) and (regex char match pat pi plen (str char at txt ti)) = regex loop pat (pi + alen + 1) plen txt (ti + 1) tlen
        | otherwise = False
      regex step quant pat pi plen txt ti tlen alen False False False =
        | (ti < tlen) and (regex char match pat pi plen (str char at txt ti)) = regex loop pat (pi + alen) plen txt (ti + 1) tlen
        | otherwise = False

fn :: regex star String Int Int Int String Int Int => Bool
      regex star pat atom_pi next_pi plen txt ti tlen =
        | regex loop pat next_pi plen txt ti tlen = True
        | (ti < tlen) and (regex char match pat atom_pi plen (str char at txt ti)) = regex star pat atom_pi next_pi plen txt (ti + 1) tlen
        | otherwise = False

fn :: match find empty FileEntry => Bool
      match find empty entry     =
        | is reg kind (fe kind entry) = (fe size entry) == 0
        | is dir kind (fe kind entry) = (list len (list dir (fe path entry))) == 0
        | otherwise = False

fn :: tuple3 c1 (A, B, C) => A
      tuple3 c1 (a, _, _) =  a

fn :: tuple3 c2 (A, B, C) => B
      tuple3 c2 (_, b, _) =  b

fn :: tuple3 c3 (A, B, C) => C
      tuple3 c3 (_, _, c) =  c

fn :: evaluate find full FindExpr FileEntry Int => Bool Bool Bool
      evaluate find full ETrue entry depth = (True, False, False)
      evaluate find full EPrune entry depth = (True, True, False)
      evaluate find full EPrint entry depth = (True, False, True)
      evaluate find full (EName pat) entry depth = (glob match pat against (fe name entry), False, False)
      evaluate find full (EIname pat) entry depth = (glob match (to lower str pat) against (to lower str (fe name entry)), False, False)
      evaluate find full (EPath pat) entry depth = (glob match pat against (fe path entry), False, False)
      evaluate find full (EType t) entry depth = (match find type (fe kind entry) t, False, False)
      evaluate find full (ESize cmp target) entry depth = (match find size (fe size entry) cmp target, False, False)
      evaluate find full (ENewer rsec rnsec) entry depth = (match find newer entry rsec rnsec, False, False)
      evaluate find full (EPerm pk pmask) entry depth = (match find perm (fe mode entry) pk pmask, False, False)
      evaluate find full (ELinks cmp target) entry depth = (match find size (fe nlink entry) cmp target, False, False)
      evaluate find full (EInum target_ino) entry depth = (match find inum entry target_ino, False, False)
      evaluate find full (ERegex pat) entry depth = (regex match pat against (fe path entry), False, False)
      evaluate find full (EMaxDepth d) entry depth = (depth <= d, False, False)
      evaluate find full (EMinDepth d) entry depth = (depth >= d, False, False)
      evaluate find full EEmpty entry depth = (match find empty entry, False, False)
      evaluate find full (ENot sub) entry depth =
        r = evaluate find full sub entry depth
        (not (tuple3 c1 r), tuple3 c2 r, tuple3 c3 r)
      evaluate find full (EAnd a b) entry depth =
        ra = evaluate find full a entry depth
        eval find and tail (tuple3 c1 ra) (tuple3 c2 ra) (tuple3 c3 ra) b entry depth
      evaluate find full (EOr a b) entry depth =
        ra = evaluate find full a entry depth
        eval find or tail (tuple3 c1 ra) (tuple3 c2 ra) (tuple3 c3 ra) b entry depth

fn :: eval find and tail Bool Bool Bool FindExpr FileEntry Int => Bool Bool Bool
      eval find and tail False pa pra _ _ _ = (False, pa, pra)
      eval find and tail True pa pra b entry depth =
        rb = evaluate find full b entry depth
        (tuple3 c1 rb, pa or (tuple3 c2 rb), pra or (tuple3 c3 rb))

fn :: eval find or tail Bool Bool Bool FindExpr FileEntry Int => Bool Bool Bool
      eval find or tail True pa pra _ _ _ = (True, pa, pra)
      eval find or tail False pa pra b entry depth =
        rb = evaluate find full b entry depth
        (tuple3 c1 rb, pa or (tuple3 c2 rb), pra or (tuple3 c3 rb))

op :: evaluate find expr FindExpr on FileEntry at Int   => Bool
      evaluate find expr expr     on entry     at depth =
        tuple3 c1 (evaluate find full expr entry depth)

fn :: stat find entry String String Bool => FileEntry
      stat find entry path bname False = stat file entry path named bname
      stat find entry path bname True =
        base_ent = stat file entry path named bname
        upgrade followed symlink path bname base_ent

fn :: upgrade followed symlink String String FileEntry => FileEntry
      upgrade followed symlink path bname (Entry n fp FSymLink m nl sz ow gr mt lt) =
        ks = raw kernel stat path True
        upgrade symlink kind n fp m nl sz ow gr mt lt ks
      upgrade followed symlink _ _ ent = ent

fn :: upgrade symlink kind String String Int Int Int String String Int String (Bool, Int, Int, Int, Int, Int, Int, Int, Int, Int) => FileEntry
      upgrade symlink kind n fp m nl sz ow gr mt lt (False, _, _, _, _, _, _, _, _, _) = Entry n fp FSymLink m nl sz ow gr mt lt
      upgrade symlink kind n fp m nl sz ow gr mt lt (True, _, knl, km, _, _, ksz, _, kmt, _) =
        ftype = (km / 4096) % 16
        fkind = select followed kind ftype
        Entry n fp fkind (km % 4096) knl ksz ow gr kmt lt

fn :: select followed kind Int => FileKind
      select followed kind 4   =  FDir
      select followed kind _   =  FReg

fn :: select find emit Bool Bool Bool => Bool
      select find emit True _ printed = printed
      select find emit False expr_ok _ = expr_ok

fn :: walk find tree String Int FindSpec [String] => [String]
      walk find tree path depth spec acc =
        | ((f_max_depth spec) >= 0) and (depth > (f_max_depth spec)) = acc
        | otherwise = walk find node path depth spec acc

fn :: walk find node String Int FindSpec [String] => [String]
      walk find node path depth spec acc =
        bname = basename of path
        entry = stat find entry path bname (f_follow spec)
        res = evaluate find full (f_expr spec) entry depth
        expr_ok = tuple3 c1 res
        pruned = tuple3 c2 res
        printed = tuple3 c3 res
        emit_ok = select find emit (f_has_print spec) expr_ok printed
        matched = (depth >= (f_min_depth spec)) and emit_ok
        acc1 = record find match path matched spec acc
        acc2 = descend find children path entry depth pruned spec acc1
        _ = delete find if matched path entry (matched and (f_delete spec))
        acc2

fn :: delete find if matched String FileEntry Bool => Bool
      delete find if matched _ _ False = False
      delete find if matched path entry True =
        | is dir kind (fe kind entry) = sys rmdir path
        | otherwise                   = sys unlink path

fn :: record find match String Bool FindSpec [String] => [String]
      record find match path False spec acc = acc
      record find match path True spec acc =
        _ = run find single exec (f_exec_cmd spec) (f_exec_plus spec) path
        acc :: [path]

fn :: run find single exec [String] Bool String => Int
      run find single exec _ True _ = 0
      run find single exec cmd False path = run find exec cmd path

fn :: resolve find prog String [String] => String
      resolve find prog prog   []       =  prog
      resolve find prog prog   (d:rest) =
        | lstat exists (file lstat (d + "/" + prog)) = d + "/" + prog
        | otherwise                                  = resolve find prog prog rest

fn :: resolve find cmd String => String
      resolve find cmd prog   =
        | str contains prog "/" = prog
        | otherwise             = resolve find prog prog ["/usr/local/bin", "/usr/bin", "/bin"]

fn :: run find fork Int String [String] => Int
      run find fork 0 resolved_prog argv =
        _ = posix execve resolved_prog argv ["PATH=/usr/local/bin:/usr/bin:/bin"]
        posix exit 127
      run find fork pid _ _ =
        | pid < 0   = 1
        | otherwise = snd (posix waitpid pid 0)

fn :: run find exec [String]    String => Int
      run find exec []          path   =  0
      run find exec (prog:args) path   =
        sub_prog = resolve find cmd (subst braces prog path)
        sub_args = subst braces list args path []
        run find fork (posix fork) sub_prog ([sub_prog] :: sub_args)

fn :: expand braces batch [String] [String] [String] => [String]
expand braces batch [] _ acc = acc
expand braces batch (x:xs) paths acc =
  | x == "\{\}" = expand braces batch xs paths (acc :: paths)
        | otherwise   = expand braces batch xs paths (acc :: [x])

fn :: run find exec batch [String] [String] Bool => Int
      run find exec batch _ [] _ = 0
      run find exec batch [] _ _ = 0
      run find exec batch (prog:args) matches False = 0
      run find exec batch (prog:args) matches True =
        sub_prog = resolve find cmd prog
        sub_args = expand braces batch args matches []
        run find fork (posix fork) sub_prog ([sub_prog] :: sub_args)

fn :: subst braces String String => String
      subst braces tok    path   =
        | tok == "\{\}" = path
        | otherwise = tok

fn :: subst braces list [String] String [String] => [String]
      subst braces list [] path acc = acc
      subst braces list (x:xs) path acc = subst braces list xs path (acc :: [subst braces x path])

fn :: descend find children String FileEntry Int Bool FindSpec [String] => [String]
      descend find children path entry depth True spec acc = acc
      descend find children path entry depth False spec acc =
        | not (is dir kind (fe kind entry)) = acc
        | ((f_max_depth spec) >= 0) and ((depth + 1) > (f_max_depth spec)) = acc
        | otherwise = walk find children list path (list dir path) (depth + 1) spec acc

fn :: walk find children list String [String] Int FindSpec [String] => [String]
      walk find children list parent [] depth spec acc = acc
      walk find children list parent (child:rest) depth spec acc =
        child_path = join path parent child
        acc1 = walk find tree child_path depth spec acc
        walk find children list parent rest depth spec acc1

fn :: walk find roots [String] FindSpec [String] => [String]
      walk find roots [] spec acc = acc
      walk find roots (r:rest) spec acc =
        acc1 = walk find tree r 0 spec acc
        walk find roots rest spec acc1

op :: evaluate find FindSpec => String
      evaluate find spec     =
        matches = walk find roots (f_paths spec) spec []
        _ = run find exec batch (f_exec_cmd spec) matches (f_exec_plus spec)
        format find matches matches (f_print0 spec) ((list len (f_exec_cmd spec)) > 0 or (f_delete spec))

op :: run find [String] => String
      run find args     =
        evaluate find (parse find args args)

fn :: format find matches [String] Bool Bool => String
      format find matches matches print0 suppress =
        | suppress = ""
        | (list len matches) == 0 = ""
        | print0 = (join with matches (char from code 0)) + (char from code 0)
        | otherwise = (join with matches (char from code 10)) + (char from code 10)

-- ============================================================================
-- 3. BSD mtree(8) — FR-FS-003
-- ============================================================================

fn :: default mtree spec => MtreeSpec
      default mtree spec =  MtreeSpec False "" "." False False False ["type", "mode", "size", "sha256", "link"]

op :: parse mtree args [String] => MtreeSpec
      parse mtree args args     =  parse mtree loop args (default mtree spec)

fn :: parse mtree loop [String]   MtreeSpec => MtreeSpec
      parse mtree loop []         spec      =  spec
      parse mtree loop (arg:rest) spec      =
        | arg == "-c" = parse mtree loop rest (spec with m_create = True)
        | arg == "-u" = parse mtree loop rest (spec with m_update = True)
        | arg == "-e" = parse mtree loop rest (spec with m_ignore_extra = True)
        | arg == "-r" = parse mtree loop rest (spec with m_remove_extra = True)
        | arg == "-f" = parse mtree flag f rest spec
        | arg == "-p" = parse mtree flag p rest spec
        | arg == "-K" = parse mtree flag k rest spec
        | otherwise = parse mtree loop rest spec

fn :: parse mtree flag f [String] MtreeSpec => MtreeSpec
      parse mtree flag f [] spec = spec
      parse mtree flag f (f:rest) spec = parse mtree loop rest (spec with m_spec_file = f)

fn :: parse mtree flag p [String] MtreeSpec => MtreeSpec
      parse mtree flag p [] spec = spec
      parse mtree flag p (p:rest) spec = parse mtree loop rest (spec with m_path = p)

fn :: parse mtree flag k [String] MtreeSpec => MtreeSpec
      parse mtree flag k [] spec = spec
      parse mtree flag k (k:rest) spec =
        kws = split comma k 0 (str len k) "" []
        res = parse mtree loop rest (spec with m_keywords = ((m_keywords spec) :: kws))
        res

fn :: split comma String Int Int String [String] => [String]
      split comma s idx len cur acc =
        | idx >= len = push nonempty cur acc
        | (str char at s idx) == 44 = split comma s (idx + 1) len "" (push nonempty cur acc)
        | otherwise = split comma s (idx + 1) len (cur + (str slice s idx (idx + 1))) acc

fn :: int to octal4 Int => String
      int to octal4 m   =
        d3 = (m / 512) % 8
        d2 = (m / 64) % 8
        d1 = (m / 8) % 8
        d0 = m % 8
        (int to str d3) + (int to str d2) + (int to str d1) + (int to str d0)

fn :: octal str to int String => Int
      octal str to int s      =  octal loop s 0 (str len s) 0

fn :: octal loop String Int Int Int => Int
      octal loop s idx len acc =
        | idx >= len = acc
        | ((str char at s idx) >= 48) and ((str char at s idx) <= 55) = octal loop s (idx + 1) len ((acc * 8) + ((str char at s idx) - 48))
        | otherwise = acc

fn :: mtree kind str FileKind => String
      mtree kind str FDir     =  "dir"
      mtree kind str FSymLink =  "link"
      mtree kind str other    =  "file"

fn :: walk mtree create String String [String] => [String]
      walk mtree create root_path rel_path acc =
        full_p = resolve mtree full root_path rel_path
        entry = stat file entry full_p named (basename of full_p)
        line = format mtree spec line rel_path full_p entry
        acc1 = acc :: [line]
        walk mtree children root_path rel_path full_p entry acc1

fn :: resolve mtree full String    String   => String
      resolve mtree full root_path "."      =  root_path
      resolve mtree full root_path rel_path =  join clean path root_path rel_path

fn :: format mtree spec line String String FileEntry => String
      format mtree spec line rel_path full_p entry =
        kstr = mtree kind str (fe kind entry)
        m_oct = int to octal4 (fe mode entry)
        base = rel_path + " type=" + kstr + " mode=" + m_oct
        append mtree kind attrs base full_p entry

fn :: append mtree kind attrs String String FileEntry => String
      append mtree kind attrs base full_p entry =
        | is reg kind (fe kind entry) = base + " size=" + (int to str (fe size entry)) + " sha256=" + (sha256 file full_p)
        | is link kind (fe kind entry) = base + " link=" + (fe link target entry)
        | otherwise = base

fn :: walk mtree children String String String FileEntry [String] => [String]
      walk mtree children root_path rel_path full_p entry acc =
        | not (is dir kind (fe kind entry)) = acc
        | otherwise = walk mtree child list root_path rel_path (list dir full_p) acc

fn :: walk mtree child list String String [String] [String] => [String]
      walk mtree child list root_path rel_path [] acc = acc
      walk mtree child list root_path rel_path (child:rest) acc =
        child_rel = join clean path rel_path child
        acc1 = walk mtree create root_path child_rel acc
        walk mtree child list root_path rel_path rest acc1

op :: generate mtree spec for String    => String
      generate mtree spec for root_path =
        header = ["#\t   mtree specification", "/set type=file mode=0644"]
        lines = walk mtree create root_path "." header
        finalize output lines lines

fn :: parent dir of String => String
      parent dir of path   =  parent dir loop path ((str len path) - 1)

fn :: parent dir loop String Int => String
      parent dir loop path   idx =
        | idx < 0 = "."
        | (str char at path idx) == 47 = str slice path 0 idx
        | otherwise = parent dir loop path (idx - 1)

fn :: is dotdot line [String]  => Bool
      is dotdot line ("..":[]) =  True
      is dotdot line _         =  False

fn :: lines has dotdot [String]    => Bool
      lines has dotdot []          =  False
      lines has dotdot (line:rest) =
        | is dotdot line (split whitespace line) = True
        | otherwise                              = lines has dotdot rest

op :: parse mtree spec text String => [MtreeEntry]
      parse mtree spec text raw    =
        lines = split lines raw
        use_blocks = lines has dotdot lines
        parse mtree lines lines "." use_blocks "file" 420 []

fn :: parse mtree lines [String] String Bool String Int [MtreeEntry] => [MtreeEntry]
      parse mtree lines [] cur_dir use_blocks def_type def_mode acc = acc
      parse mtree lines (line:rest) cur_dir use_blocks def_type def_mode acc =
        toks = split whitespace line
        parse mtree tokens toks rest cur_dir use_blocks def_type def_mode acc

fn :: parse mtree tokens [String] [String] String Bool String Int [MtreeEntry] => [MtreeEntry]
      parse mtree tokens [] rest cur_dir use_blocks def_type def_mode acc = parse mtree lines rest cur_dir use_blocks def_type def_mode acc
      parse mtree tokens (first:kvs) rest cur_dir use_blocks def_type def_mode acc =
        | str starts with first "#" = parse mtree lines rest cur_dir use_blocks def_type def_mode acc
        | first == "/set" = apply mtree set kvs rest cur_dir use_blocks def_type def_mode acc
        | first == "/unset" = apply mtree unset kvs rest cur_dir use_blocks def_type def_mode acc
        | first == ".." = parse mtree lines rest (parent dir of cur_dir) use_blocks def_type def_mode acc
        | otherwise =
          rel_name = resolve mtree rel cur_dir first use_blocks
          ent = build mtree entry rel_name kvs def_type def_mode
          next_dir = advance mtree dir cur_dir first (me_type ent) use_blocks
          (parse mtree lines rest next_dir use_blocks def_type def_mode (acc :: [ent]))

fn :: resolve mtree rel String String Bool => String
      resolve mtree rel _ "." _ = "."
      resolve mtree rel _ first False = first
      resolve mtree rel cur_dir first True =
        | str contains first "/" = first
        | otherwise              = join clean path cur_dir first

fn :: advance mtree dir String String String Bool => String
      advance mtree dir cur_dir _ _ False = cur_dir
      advance mtree dir cur_dir "." _ True = cur_dir
      advance mtree dir cur_dir first "dir" True =
        | str contains first "/" = cur_dir
        | otherwise              = join clean path cur_dir first
      advance mtree dir cur_dir _ _ True = cur_dir

fn :: apply mtree set [String] [String] String Bool String Int [MtreeEntry] => [MtreeEntry]
      apply mtree set [] rest cur_dir use_blocks def_type def_mode acc = parse mtree lines rest cur_dir use_blocks def_type def_mode acc
      apply mtree set (kv:more) rest cur_dir use_blocks def_type def_mode acc =
        | str starts with kv "type=" = apply mtree set more rest cur_dir use_blocks (str slice kv 5 (str len kv)) def_mode acc
        | str starts with kv "mode=" = apply mtree set more rest cur_dir use_blocks def_type (octal str to int (str slice kv 5 (str len kv))) acc
        | otherwise = apply mtree set more rest cur_dir use_blocks def_type def_mode acc

fn :: apply mtree unset [String] [String] String Bool String Int [MtreeEntry] => [MtreeEntry]
      apply mtree unset [] rest cur_dir use_blocks def_type def_mode acc = parse mtree lines rest cur_dir use_blocks "file" (0 - 1) acc
      apply mtree unset (kw:more) rest cur_dir use_blocks def_type def_mode acc =
        | (kw == "all") = parse mtree lines rest cur_dir use_blocks "file" (0 - 1) acc
        | (kw == "type") = apply mtree unset more rest cur_dir use_blocks "file" def_mode acc
        | (kw == "mode") = apply mtree unset more rest cur_dir use_blocks def_type (0 - 1) acc
        | otherwise = apply mtree unset more rest cur_dir use_blocks def_type def_mode acc

fn :: has kv prefix [String]  String => Bool
      has kv prefix []        _      =  False
      has kv prefix (kv:rest) pfx    =
        | str starts with kv pfx = True
        | otherwise              = has kv prefix rest pfx

fn :: build mtree entry String [String] String Int => MtreeEntry
      build mtree entry rel kvs def_type def_mode =
        base_ent = apply entry kvs kvs (MtreeEntry rel def_type def_mode (0 - 1) "" "")
        clear mismatched dir mode base_ent kvs def_type

fn :: clear mismatched dir mode MtreeEntry [String] String => MtreeEntry
      clear mismatched dir mode ent kvs def_type =
        | ((me_type ent) != def_type) and (not (has kv prefix kvs "mode=")) = ent with me_mode = (0 - 1)
        | otherwise = ent

fn :: apply entry kvs [String]  MtreeEntry => MtreeEntry
      apply entry kvs []        ent        =  ent
      apply entry kvs (kv:rest) ent        =
        | str starts with kv "type=" = apply entry kvs rest (ent with me_type = (str slice kv 5 (str len kv)))
        | str starts with kv "mode=" = apply entry kvs rest (ent with me_mode = (octal str to int (str slice kv 5 (str len kv))))
        | str starts with kv "size=" = apply entry kvs rest (ent with me_size = (str to int (str slice kv 5 (str len kv))))
        | str starts with kv "sha256=" = apply entry kvs rest (ent with me_sha256 = (str slice kv 7 (str len kv)))
        | str starts with kv "link=" = apply entry kvs rest (ent with me_link = (str slice kv 5 (str len kv)))
        | otherwise = apply entry kvs rest ent

fn :: collect live rels String String [String] => [String]
      collect live rels root_path rel_path acc =
        full_p = resolve mtree full root_path rel_path
        raw = file lstat full_p
        collect live if ok root_path rel_path full_p raw acc

fn :: collect live if ok String String String (Bool, String, Int, Int, Int, String, String, Int, String) [String] => [String]
      collect live if ok root_path rel_path full_p (False, kstr, m, nl, sz, ow, gr, mt, lt) acc = acc
      collect live if ok root_path rel_path full_p (True, kstr, m, nl, sz, ow, gr, mt, lt) acc =
        | kstr == "dir" = collect live children root_path rel_path (list dir full_p) (acc :: [rel_path])
        | otherwise = acc :: [rel_path]

fn :: collect live children String String [String] [String] => [String]
      collect live children root_path rel_path [] acc = acc
      collect live children root_path rel_path (c:rest) acc =
        c_rel = join clean path rel_path c
        acc1 = collect live rels root_path c_rel acc
        collect live children root_path rel_path rest acc1

fn :: verify single entry MtreeEntry String Bool => [String]
      verify single entry spec_ent root_path update =
        rel = me_rel spec_ent
        full_p = resolve mtree full root_path rel
        raw = file lstat full_p
        verify lstat result spec_ent full_p raw update

fn :: verify lstat result MtreeEntry String (Bool, String, Int, Int, Int, String, String, Int, String) Bool => [String]
      verify lstat result spec_ent full_p (False, kstr, m, nl, sz, ow, gr, mt, lt) update =
        _ = heal missing dir spec_ent full_p update
        [(me_rel spec_ent) + ": missing"]
      verify lstat result spec_ent full_p (True, kstr, m, nl, sz, ow, gr, mt, lt) update =
        m1 = check mode diff spec_ent full_p m update []
        m2 = check size diff spec_ent kstr sz m1
        check sha256 diff spec_ent full_p kstr m2

fn :: heal missing dir MtreeEntry String Bool => Int
      heal missing dir spec_ent full_p False = 0
      heal missing dir spec_ent full_p True =
        | (me_type spec_ent) == "dir" = sys mkdir full_p (max int 493 (me_mode spec_ent))
        | otherwise = 0

fn :: check mode diff MtreeEntry String Int Bool [String] => [String]
      check mode diff spec_ent full_p actual_mode update acc =
        | (me_mode spec_ent) < 0 = acc
        | (me_mode spec_ent) == actual_mode = acc
        | otherwise =
          _ = heal mode diff full_p (me_mode spec_ent) update
          msg = (me_rel spec_ent) + ": permissions (" + (int to octal4 (me_mode spec_ent)) + ", " + (int to octal4 actual_mode) + ")"
          acc :: [msg]

fn :: heal mode diff String Int Bool => Int
      heal mode diff full_p exp_mode False = 0
      heal mode diff full_p exp_mode True = sys chmod full_p exp_mode

fn :: check size diff MtreeEntry String Int [String] => [String]
      check size diff spec_ent kstr actual_sz acc =
        | (kstr != "file") or ((me_size spec_ent) < 0) = acc
        | (me_size spec_ent) == actual_sz = acc
        | otherwise = acc :: [(me_rel spec_ent) + ": size (" + (int to str (me_size spec_ent)) + ", " + (int to str actual_sz) + ")"]

fn :: check sha256 diff MtreeEntry String String [String] => [String]
      check sha256 diff spec_ent full_p kstr acc =
        | (kstr != "file") or ((str len (me_sha256 spec_ent)) == 0) = acc
        | otherwise = compare sha256 val (me_rel spec_ent) (me_sha256 spec_ent) (sha256 file full_p) acc

fn :: compare sha256 val String String String [String] => [String]
      compare sha256 val rel exp_hash act_hash acc =
        | exp_hash == act_hash = acc
        | otherwise = acc :: [rel + ": sha256 (" + exp_hash + ", " + act_hash + ")"]

fn :: verify spec entries [MtreeEntry] String Bool [String] => [String]
      verify spec entries [] root_path update acc = acc
      verify spec entries (e:rest) root_path update acc =
        diffs = verify single entry e root_path update
        verify spec entries rest root_path update (acc :: diffs)

fn :: has spec rel [MtreeEntry] String => Bool
      has spec rel []           rel    =  False
      has spec rel (e:rest)     rel    =
        | (me_rel e) == rel = True
        | otherwise = has spec rel rest rel

fn :: find extra entries [String] [MtreeEntry] [String] => [String]
      find extra entries [] spec_entries acc = acc
      find extra entries (rel:rest) spec_entries acc =
        | has spec rel spec_entries rel = find extra entries rest spec_entries acc
        | otherwise = find extra entries rest spec_entries (acc :: [rel + ": extra"])

fn :: remove extra entries [String] [MtreeEntry] String => ()
      remove extra entries [] _ _ = ()
      remove extra entries (rel:rest) spec_entries root_path =
        | has spec rel spec_entries rel = remove extra entries rest spec_entries root_path
        | otherwise =
          full_p = resolve mtree full root_path rel
          _ = sys unlink full_p
          _ = sys rmdir full_p
          remove extra entries rest spec_entries root_path

op :: verify mtree [MtreeEntry] against String    update Bool => [String]
      verify mtree spec_entries against root_path update upd  =
        spec_diffs = verify spec entries spec_entries root_path upd []
        live_rels = collect live rels root_path "." []
        extra_diffs = find extra entries live_rels spec_entries []
        spec_diffs :: extra_diffs

op :: verify mtree String   at String    update Bool => Bool [String]
      verify mtree raw_spec at root_path update upd  =
        entries = parse mtree spec text raw_spec
        diffs = verify mtree entries against root_path update upd
        ((list len diffs) == 0, diffs)

op :: verify mtree full [MtreeEntry] with MtreeSpec => [String]
      verify mtree full spec_entries with spec      =
        root_path = m_path spec
        spec_diffs = verify spec entries spec_entries root_path (m_update spec) []
        live_rels = collect live rels root_path "." []
        handle mtree extras live_rels spec_entries root_path (m_ignore_extra spec) (m_remove_extra spec) spec_diffs

fn :: handle mtree extras [String] [MtreeEntry] String Bool Bool [String] => [String]
      handle mtree extras live_rels spec_entries root_path True False spec_diffs = spec_diffs
      handle mtree extras live_rels spec_entries root_path _ True spec_diffs =
        _ = remove extra entries live_rels spec_entries root_path
        spec_diffs
      handle mtree extras live_rels spec_entries root_path False False spec_diffs =
        extra_diffs = find extra entries live_rels spec_entries []
        spec_diffs :: extra_diffs

-- ============================================================================
-- 4. BSD mktemp(1) Implementation (FR-FS-004)
-- ============================================================================

fn :: default mktemp spec => MktempSpec
      default mktemp spec =  MktempSpec False False False "/tmp" "" ""

fn :: parse mktemp loop [String]        MktempSpec => MktempSpec
      parse mktemp loop []              spec       =  spec
      parse mktemp loop ("-d":rest)     spec       =  parse mktemp loop rest (spec with mkt_dir = True)
      parse mktemp loop ("-u":rest)     spec       =  parse mktemp loop rest (spec with mkt_unsafe = True)
      parse mktemp loop ("-q":rest)     spec       =  parse mktemp loop rest (spec with mkt_quiet = True)
      parse mktemp loop ("-p":dir:rest) spec       =  parse mktemp loop rest (spec with mkt_tmpdir = dir)
      parse mktemp loop ("-t":pfx:rest) spec       =  parse mktemp loop rest (spec with mkt_prefix = pfx)
      parse mktemp loop (arg:rest)      spec       =  parse mktemp loop rest (spec with mkt_template = arg)

op :: parse mktemp args [String] => MktempSpec
      parse mktemp args args     =  parse mktemp loop args (default mktemp spec)

fn :: mktemp alnum table => String
      mktemp alnum table =  "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"

fn :: mktemp char from byte Int => String
      mktemp char from byte b   =
        idx = ((b + 256) % 62)
        str slice (mktemp alnum table) idx (idx + 1)

fn :: count trailing x String Int => Int
      count trailing x tmpl   pos =
        | pos <= 0 = 0
        | (str char at tmpl (pos - 1)) == 88 = 1 + (count trailing x tmpl (pos - 1))
        | otherwise = 0

fn :: build random suffix [Int] Int Int => String
      build random suffix _ 0 _ = ""
      build random suffix [] rem seed =
        ch = mktemp char from byte ((seed * 31) + rem)
        ch + (build random suffix [] (rem - 1) ((seed * 17) + 7))
      build random suffix (b:rest) rem seed =
        ch = mktemp char from byte (b + seed)
        ch + (build random suffix rest (rem - 1) (seed + 13))

fn :: resolve mktemp template MktempSpec => String
      resolve mktemp template spec       =
        | (str len (mkt_template spec)) > 0 = mkt_template spec
        | (str len (mkt_prefix spec)) > 0   = (mkt_tmpdir spec) + (path separator (sys os)) + (mkt_prefix spec) + ".XXXXXXXX"
        | otherwise                         = (mkt_tmpdir spec) + (path separator (sys os)) + "tmp.XXXXXXXXXX"

fn :: expand mktemp with String Int Int Int => String
expand mktemp with tmpl   tlen nx  attempt =
  | nx <= 0   = expand mktemp with (tmpl + ".XXXXXXXX") (tlen + 9) 8 attempt
        | otherwise =
          rng = snd (sys random bytes list nx)
          suf = build random suffix rng nx ((sys getpid) + (attempt * 97))
          (str slice tmpl 0 (tlen - nx)) + suf

op :: expand mktemp template MktempSpec => String
expand mktemp template spec       =
  tmpl = resolve mktemp template spec
  tlen = str len tmpl
expand mktemp with tmpl tlen (count trailing x tmpl tlen) 0

fn :: expand mktemp attempt MktempSpec Int => String
expand mktemp attempt spec       attempt =
  tmpl = resolve mktemp template spec
  tlen = str len tmpl
expand mktemp with tmpl tlen (count trailing x tmpl tlen) attempt

fn :: finish mktemp fd String Int => Bool
      finish mktemp fd path   fd  =
        | fd < 0    = False
        | otherwise =
          _ = posix close fd
          _ = posix chmod path 384
          True

fn :: create mktemp file String => Bool
      create mktemp file path   =
        finish mktemp fd path (posix open path ((o wronly) + (o creat) + (o excl)) 384)

fn :: create mktemp dir String => Bool
      create mktemp dir path   =
        | (sys mkdir path 448) != 0 = False
        | otherwise                 = posix chmod path 448

fn :: dispatch mktemp cand MktempSpec String => Bool String
      dispatch mktemp cand spec       cand   =
        | mkt_unsafe spec = (not (lstat exists (file lstat cand)), cand)
        | mkt_dir spec    = (create mktemp dir cand, cand)
        | otherwise       = (create mktemp file cand, cand)

fn :: format mktemp result MktempSpec (Bool, String) => Bool String
      format mktemp result spec       (True, cand)   =  (True, cand)
      format mktemp result spec       (False, cand)  =
        | mkt_quiet spec = (False, "")
        | otherwise      = (False, "mktemp: cannot create temp file " + cand)

fn :: execute mktemp loop MktempSpec Int     => Bool String
      execute mktemp loop spec       attempt =
        | attempt >= 16 = format mktemp result spec (False, expand mktemp attempt spec attempt)
        | otherwise     =
          res = dispatch mktemp cand spec (expand mktemp attempt spec attempt)
          check mktemp attempt spec attempt res

fn :: check mktemp attempt MktempSpec Int (Bool, String) => Bool String
      check mktemp attempt _ _ (True, cand) = (True, cand)
      check mktemp attempt spec attempt (False, _) = execute mktemp loop spec (attempt + 1)

op :: execute mktemp MktempSpec => Bool String
      execute mktemp spec       =
        execute mktemp loop spec 0
