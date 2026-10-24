-- filter_lib.rg — Exhaustive pure Ridgun implementation for Tier 1 BSD Filters
-- @implements FR-FILTER-001
-- @implements FR-FILTER-002

import posix

type JotParam = Omitted | Given Int

type JotSpec = j_reps: JotParam, j_begin: JotParam, j_end: JotParam, j_step: JotParam, j_word: String, j_has_word: Bool, j_fmt: String, j_char_mode: Bool, j_sep: String, j_no_newline: Bool, j_random: Bool, j_prec: Int

type RsSpec = r_rows: Int, r_cols: Int, r_transpose: Bool, r_in_sep: String, r_out_sep: String, r_has_out_sep: Bool, r_gutter: Int, r_right_align: Bool, r_width: Int, r_line_entries: Bool, r_skip_lines: Int, r_recycle: Bool, r_shape_only: Bool, r_squeeze: Bool, r_files: [String]

type LamCol = l_sep: String, l_min: Int, l_max: Int, l_pad_eof: Bool, l_lines: [String]

type VisSpec = v_octal: Bool, v_cstyle: Bool, v_tab: Bool, v_space: Bool, v_newline: Bool, v_nobs: Bool, v_mark_nl: Bool, v_none: Bool, v_fold: Int, v_files: [String]

-- ============================================================================
-- Generic List, Integer & String Helpers
-- ============================================================================

fn :: list len [A]    => Int
      list len []     =  0
      list len (_:xs) =  1 + (list len xs)

fn :: fst (A, B) => A
      fst (a, _) =  a

fn :: snd (A, B) => B
      snd (_, b) =  b

fn :: str list nth [String] Int => String
      str list nth []       idx =  ""
      str list nth (x:xs)   n   =
        | n <= 0    = x
        | otherwise = str list nth xs (n - 1)

fn :: int list nth [Int]  Int => Int
      int list nth []     idx =  0
      int list nth (x:xs) n   =
        | n <= 0    = x
        | otherwise = int list nth xs (n - 1)

fn :: drop str list [A]    Int => [A]
      drop str list []     n   =  []
      drop str list (x:xs) n   =
        | n <= 0    = [x] :: xs
        | otherwise = drop str list xs (n - 1)

fn :: join with [String] String => String
      join with []       sep    =  ""
      join with (x:[])   sep    =  x
      join with (x:xs)   sep    =  x + sep + (join with xs sep)

fn :: repeat str String Int => String
      repeat str s      n   =
        | n <= 0    = ""
        | otherwise = s + (repeat str s (n - 1))

fn :: pad right String Int   => String
      pad right s      width =
        | (str len s) >= width = s
        | otherwise            = s + (repeat str " " (width - (str len s)))

fn :: pad left String Int   => String
      pad left s      width =
        | (str len s) >= width = s
        | otherwise            = (repeat str " " (width - (str len s))) + s

fn :: pad left char String Int String => String
      pad left char s width ch =
        | (str len s) >= width = s
        | otherwise            = (repeat str ch (width - (str len s))) + s

fn :: pow10 Int => Int
      pow10 n   =
        | n <= 0    = 1
        | otherwise = 10 * (pow10 (n - 1))

fn :: abs int Int => Int
      abs int n   =
        | n < 0     = 0 - n
        | otherwise = n

fn :: max int Int Int => Int
      max int a   b   =
        | a >= b    = a
        | otherwise = b

fn :: is digits only String => Bool
      is digits only s      =
        | (str len s) == 0 = False
        | otherwise = is digits loop s 0 (str len s)

fn :: is digits loop String Int Int => Bool
      is digits loop s idx len =
        | idx >= len = True
        | ((str char at s idx) >= 48) and ((str char at s idx) <= 57) = is digits loop s (idx + 1) len
        | otherwise = False

fn :: parse signed int String => Int
      parse signed int s      =
        | (str len s) == 0 = 0
        | ((str char at s 0) == 45) and ((str len s) > 1) = 0 - (str to int (str slice s 1 (str len s)))
        | ((str char at s 0) == 43) and ((str len s) > 1) = str to int (str slice s 1 (str len s))
        | otherwise = str to int s

fn :: is signed int str String => Bool
      is signed int str s      =
        | (str len s) == 0 = False
        | ((str char at s 0) == 45) and ((str len s) > 1) = is digits only (str slice s 1 (str len s))
        | ((str char at s 0) == 43) and ((str len s) > 1) = is digits only (str slice s 1 (str len s))
        | otherwise = is digits only s

fn :: find char idx String Int Int Int => Int
      find char idx s target idx len =
        | idx >= len = 0 - 1
        | (str char at s idx) == target = idx
        | otherwise = find char idx s target (idx + 1) len

fn :: is numeric arg String => Bool
      is numeric arg s      =
        | is signed int str s = True
        | otherwise = is decimal str s

fn :: is decimal str String => Bool
      is decimal str s      =
        dot_idx = find char idx s 46 0 (str len s)
        check decimal parts s dot_idx

fn :: check decimal parts String Int     => Bool
      check decimal parts s      dot_idx =
        | dot_idx <= 0 = False
        | (dot_idx + 1) >= (str len s) = False
        | otherwise = (is signed int str (str slice s 0 dot_idx)) and (is digits only (str slice s (dot_idx + 1) (str len s)))

fn :: decimal places String => Int
      decimal places s      =
        dot_idx = find char idx s 46 0 (str len s)
        calc decimal places s dot_idx

fn :: calc decimal places String Int     => Int
      calc decimal places s      dot_idx =
        | dot_idx < 0 = 0
        | otherwise = (str len s) - dot_idx - 1

fn :: parse scaled int String Int  => Int
      parse scaled int s      prec =
        | prec <= 0 = parse signed int s
        | otherwise = parse scaled dot s prec (find char idx s 46 0 (str len s))

fn :: parse scaled dot String Int Int => Int
      parse scaled dot s prec dot_idx =
        | dot_idx < 0 = (parse signed int s) * (pow10 prec)
        | otherwise = combine scaled parts (str slice s 0 dot_idx) (str slice s (dot_idx + 1) (str len s)) prec

fn :: combine scaled parts String String Int => Int
      combine scaled parts int_part frac_part prec =
        is_neg = ((str len int_part) > 0) and ((str char at int_part 0) == 45)
        abs_i = abs int (parse signed int int_part)
        padded_frac = pad or trunc frac frac_part prec
        frac_val = str to int padded_frac
        mag = (abs_i * (pow10 prec)) + frac_val
        apply sign is_neg mag

fn :: apply sign Bool  Int => Int
      apply sign True  mag =  0 - mag
      apply sign False mag =  mag

fn :: pad or trunc frac String Int => String
      pad or trunc frac frac prec =
        | (str len frac) == prec = frac
        | (str len frac) > prec = str slice frac 0 prec
        | otherwise = frac + (repeat str "0" (prec - (str len frac)))

fn :: format scaled int Int Int  => String
      format scaled int val prec =
        | prec <= 0 = int to str val
        | val < 0 = "-" + (format scaled pos (0 - val) prec)
        | otherwise = format scaled pos val prec

fn :: format scaled pos Int Int  => String
      format scaled pos mag prec =
        scale = pow10 prec
        int_part = mag / scale
        frac_part = mag % scale
        (int to str int_part) + "." + (pad left char (int to str frac_part) prec "0")

fn :: is ws code Int => Bool
      is ws code c   =
        | c == 32 = True
        | c == 9 = True
        | c == 10 = True
        | c == 13 = True
        | otherwise = False

fn :: split ws loop String Int Int String [String] => [String]
      split ws loop s idx len cur acc =
        | idx >= len = finalize token cur acc
        | is ws code (str char at s idx) = split ws after space s (idx + 1) len cur acc
        | otherwise = split ws loop s (idx + 1) len (cur + (str slice s idx (idx + 1))) acc

fn :: split ws after space String Int Int String [String] => [String]
      split ws after space s next_idx len cur acc =
        | (str len cur) == 0 = split ws loop s next_idx len "" acc
        | otherwise = split ws loop s next_idx len "" (acc :: [cur])

fn :: finalize token String [String] => [String]
      finalize token cur    acc      =
        | (str len cur) == 0 = acc
        | otherwise = acc :: [cur]

op :: split whitespace String => [String]
      split whitespace s      =  split ws loop s 0 (str len s) "" []

fn :: split char loop String Int Int Int String [String] => [String]
      split char loop s idx len delim cur acc =
        | idx >= len = finalize token cur acc
        | (str char at s idx) == delim = split ws after delim s (idx + 1) len delim cur acc
        | is ws code (str char at s idx) and (delim != 10) and ((str char at s idx) == 10) = split ws after delim s (idx + 1) len delim cur acc
        | otherwise = split char loop s (idx + 1) len delim (cur + (str slice s idx (idx + 1))) acc

fn :: split ws after delim String Int Int Int String [String] => [String]
      split ws after delim s next_idx len delim cur acc =
        | (str len cur) == 0 = split char loop s next_idx len delim "" acc
        | otherwise = split char loop s next_idx len delim "" (acc :: [cur])

fn :: split lines delim String Int Int Int String [String] => [String]
      split lines delim s idx len term_code cur acc =
        | idx >= len = finalize line cur acc
        | (str char at s idx) == term_code = split lines delim s (idx + 1) len term_code "" (acc :: [cur])
        | (str char at s idx) == 13 = split lines delim s (idx + 1) len term_code cur acc
        | (term_code != 10) and ((str char at s idx) == 10) = split lines delim s (idx + 1) len term_code cur acc
        | otherwise = split lines delim s (idx + 1) len term_code (cur + (str slice s idx (idx + 1))) acc

fn :: finalize line String [String] => [String]
      finalize line cur    acc      =
        | (str len cur) == 0 = acc
        | otherwise = acc :: [cur]

op :: split lines String => [String]
      split lines s      =  split lines delim s 0 (str len s) 10 "" []

-- ============================================================================
-- 1. BSD jot(1) — FR-FILTER-001
-- ============================================================================

fn :: is omitted JotParam  => Bool
      is omitted Omitted   =  True
      is omitted (Given x) =  False

fn :: param val JotParam  Int     => Int
      param val Omitted   def_val =  def_val
      param val (Given x) def_val =  x

op :: resolve jot params JotParam JotParam JotParam JotParam Int => Int Int Int Int
      resolve jot params p_r p_b p_e p_s prec =
        scale = pow10 prec
        def_b = 1 * scale
        def_e = 100 * scale
        def_s = 1 * scale
        resolve jot flags (not (is omitted p_r)) (not (is omitted p_b)) (not (is omitted p_e)) (not (is omitted p_s)) (param val p_r 100) (param val p_b def_b) (param val p_e def_e) (param val p_s def_s) scale

fn :: resolve jot flags Bool Bool Bool Bool Int Int Int Int Int => Int Int Int Int
      resolve jot flags False False False False r b e s scale = (100, 1 * scale, 100 * scale, 1 * scale)
      resolve jot flags True False False False r b e s scale = (r, 1 * scale, r * scale, 1 * scale)
      resolve jot flags True True False False r b e s scale = (r, b, b + ((r - 1) * scale), 1 * scale)
      resolve jot flags True True True False r b e s scale = (r, b, e, infer step r b e)
      resolve jot flags False True True True r b e s scale = (infer reps b e s, b, e, s)
      resolve jot flags True True False True r b e s scale = (r, b, b + (s * (r - 1)), s)
      resolve jot flags False True True False r b e s scale = (infer reps b e ((default dir b e) * scale), b, e, (default dir b e) * scale)
      resolve jot flags True False True False r b e s scale = (r, e - ((r - 1) * scale), e, 1 * scale)
      resolve jot flags True False True True r b e s scale = (r, e - (s * (r - 1)), e, s)
      resolve jot flags False True False True r b e s scale = (100, b, b + (s * 99), s)
      resolve jot flags False False True True r b e s scale = (100, e - (s * 99), e, s)
      resolve jot flags False False False True r b e s scale = (100, 1 * scale, (1 * scale) + (s * 99), s)
      resolve jot flags False True False False r b e s scale = (100, b, b + (99 * scale), 1 * scale)
      resolve jot flags False False True False r b e s scale = (e / scale, 1 * scale, e, 1 * scale)
      resolve jot flags True True True True r b e s scale = (r, b, e, s)

fn :: default dir Int Int => Int
      default dir b   e   =
        | e >= b = 1
        | otherwise = 0 - 1

fn :: infer step Int Int Int => Int
      infer step r   b   e   =
        | r <= 1 = 0
        | otherwise = (e - b) / (r - 1)

fn :: infer reps Int Int Int => Int
      infer reps b   e   s   =
        | s == 0 = 100
        | ((e > b) and (s < 0)) or ((e < b) and (s > 0)) = 0
        | otherwise = ((e - b) / s) + 1

fn :: int to hex Int Bool  => String
      int to hex 0   upper =  "0"
      int to hex n   upper =
        | n < 0 = "-" + (int to hex loop (0 - n) upper "")
        | otherwise = int to hex loop n upper ""

fn :: int to hex loop Int Bool String => String
      int to hex loop 0 upper acc = acc
      int to hex loop n upper acc =
        d = n % 16
        int to hex loop (n / 16) upper ((hex digit d upper) + acc)

fn :: hex digit Int Bool  => String
      hex digit d   upper =
        | d < 10 = int to str d
        | upper = char from code (55 + d)
        | otherwise = char from code (87 + d)

fn :: int to octal Int => String
      int to octal 0   =  "0"
      int to octal n   =
        | n < 0 = "-" + (int to octal loop (0 - n) "")
        | otherwise = int to octal loop n ""

fn :: int to octal loop Int String => String
      int to octal loop 0 acc = acc
      int to octal loop n acc = int to octal loop (n / 8) ((int to str (n % 8)) + acc)

fn :: format jot item Int JotSpec => String
      format jot item val spec    =
        | j_has_word spec = j_word spec
        | (j_char_mode spec) and ((str len (j_fmt spec)) > 0) = format printf jot (j_fmt spec) val (j_prec spec) True
        | j_char_mode spec = char from code val
        | (str len (j_fmt spec)) > 0 = format printf jot (j_fmt spec) val (j_prec spec) False
        | otherwise = format scaled int val (j_prec spec)

fn :: format printf jot String Int Int Bool => String
      format printf jot fmt val prec char_mode =
        | (str len fmt) == 0 = format scaled int val prec
        | not (str contains fmt "%") = fmt + (format scaled int val prec)
        | otherwise = printf jot loop fmt 0 (str len fmt) val prec char_mode ""

fn :: printf jot loop String Int Int Int Int Bool String => String
      printf jot loop fmt idx len val prec char_mode acc =
        | idx >= len = acc
        | ((str char at fmt idx) == 37) and ((idx + 1) < len) and ((str char at fmt (idx + 1)) == 37) = printf jot loop fmt (idx + 2) len val prec char_mode (acc + "%")
        | ((str char at fmt idx) == 37) and ((idx + 1) < len) = printf parse spec fmt (idx + 1) len val prec char_mode acc
        | otherwise = printf jot loop fmt (idx + 1) len val prec char_mode (acc + (str slice fmt idx (idx + 1)))

fn :: printf parse spec String Int Int Int Int Bool String => String
      printf parse spec fmt idx len val prec char_mode acc =
        left_align = (idx < len) and ((str char at fmt idx) == 45)
        idx1 = advance if left_align idx
        zero_pad = (idx1 < len) and ((str char at fmt idx1) == 48)
        idx2 = advance if zero_pad idx1
        width_pair = read digits at fmt idx2 len ""
        width_str = fst width_pair
        idx3 = snd width_pair
        prec_pair = read opt prec fmt idx3 len prec
        spec_prec = fst prec_pair
        idx4 = snd prec_pair
        conv_code = char at or fmt idx4 100
        rendered = render printf conv conv_code val prec spec_prec char_mode
        padded = apply printf pad rendered (str to int width_str) left_align zero_pad
        printf jot loop fmt (idx4 + 1) len val prec char_mode (acc + padded)

fn :: advance if Bool  Int => Int
      advance if True  idx =  idx + 1
      advance if False idx =  idx

fn :: read digits at String Int Int String => String Int
      read digits at s idx len acc =
        | idx >= len = (acc, idx)
        | ((str char at s idx) >= 48) and ((str char at s idx) <= 57) = read digits at s (idx + 1) len (acc + (str slice s idx (idx + 1)))
        | otherwise = (acc, idx)

fn :: read opt prec String Int Int Int => Int Int
      read opt prec s idx len def_prec =
        | (idx < len) and ((str char at s idx) == 46) = read opt prec digits s (idx + 1) len
        | otherwise = (def_prec, idx)

fn :: read opt prec digits String Int Int => Int Int
      read opt prec digits s idx len =
        pair = read digits at s idx len ""
        (str to int (fst pair), snd pair)

fn :: char at or String Int Int => Int
      char at or s idx fallback =
        | idx >= (str len s) = fallback
        | otherwise = str char at s idx

fn :: render printf conv Int Int Int Int Bool => String
      render printf conv conv val orig_prec spec_prec char_mode =
        | (conv == 99) or char_mode = char from code (val / (pow10 orig_prec))
        | conv == 120 = int to hex (val / (pow10 orig_prec)) False
        | conv == 88 = int to hex (val / (pow10 orig_prec)) True
        | conv == 111 = int to octal (val / (pow10 orig_prec))
        | conv == 102 = format rescaled val orig_prec spec_prec
        | otherwise = format scaled int val orig_prec

fn :: format rescaled Int Int       Int         => String
      format rescaled val orig_prec target_prec =
        | target_prec == orig_prec = format scaled int val orig_prec
        | target_prec > orig_prec = format scaled int (val * (pow10 (target_prec - orig_prec))) target_prec
        | otherwise = format scaled int (val / (pow10 (orig_prec - target_prec))) target_prec

fn :: apply printf pad String Int Bool Bool => String
      apply printf pad s width left_align zero_pad =
        | width <= 0 = s
        | left_align = pad right s width
        | zero_pad = pad zero signed s width
        | otherwise = pad left s width

fn :: pad zero signed String Int   => String
      pad zero signed s      width =
        | (str len s) >= width = s
        | ((str len s) > 0) and ((str char at s 0) == 45) = "-" + (pad left char (str slice s 1 (str len s)) (width - 1) "0")
        | otherwise = pad left char s width "0"

fn :: gen jot items Int Int Int Int JotSpec [String] => [String]
      gen jot items idx reps cur step spec acc =
        | idx >= reps = acc
        | otherwise = gen jot items (idx + 1) reps (cur + step) step spec (acc :: [format jot item cur spec])

fn :: next prng Int   => Int
      next prng state =  ((state * 1103515245) + 12345) % 2147483648

fn :: gen jot random Int Int Int Int Int JotSpec [String] => [String]
      gen jot random idx reps lo hi state spec acc =
        | idx >= reps = acc
        | otherwise =
          st1 = abs int (next prng state)
          span = max int 1 ((hi - lo) + 1)
          val = lo + (st1 % span)
          (gen jot random (idx + 1) reps lo hi st1 spec (acc :: [format jot item val spec]))

op :: evaluate jot JotSpec => String
      evaluate jot spec    =
        tuple = resolve jot params (j_reps spec) (j_begin spec) (j_end spec) (j_step spec) (j_prec spec)
        reps = tuple4 c1 tuple
        begin_val = tuple4 c2 tuple
        end_val = tuple4 c3 tuple
        step_val = tuple4 c4 tuple
        items = select jot items (j_random spec) reps begin_val end_val step_val spec
        join with items (j_sep spec)

fn :: select jot items Bool Int Int Int Int JotSpec => [String]
      select jot items False reps begin_val end_val step_val spec = gen jot items 0 reps begin_val step_val spec []
      select jot items True reps begin_val end_val step_val spec =
        lo = min int begin_val end_val
        hi = max int begin_val end_val
        seed = max int 1 (abs int step_val)
        gen jot random 0 reps lo hi seed spec []

fn :: min int Int Int => Int
      min int a   b   =
        | a <= b = a
        | otherwise = b

fn :: tuple4 c1 (A, B, C, D) => A
      tuple4 c1 (a, _, _, _) =  a

fn :: tuple4 c2 (A, B, C, D) => B
      tuple4 c2 (_, b, _, _) =  b

fn :: tuple4 c3 (A, B, C, D) => C
      tuple4 c3 (_, _, c, _) =  c

fn :: tuple4 c4 (A, B, C, D) => D
      tuple4 c4 (_, _, _, d) =  d

fn :: parse jot param String Bool Int => JotParam
      parse jot param s char_mode prec =
        | s == "-" = Omitted
        | char_mode and ((str len s) == 1) and (not (is digits only s)) = Given (str char at s 0)
        | otherwise = Given (parse scaled int s prec)

fn :: default jot spec => JotSpec
      default jot spec =  JotSpec Omitted Omitted Omitted Omitted "" False "" False (char from code 10) False False (0 - 1)

op :: parse jot args [String] => JotSpec
      parse jot args args     =  parse jot loop args False (default jot spec) []

fn :: is jot flag cluster String => Bool
      is jot flag cluster s      =
        | (str len s) <= 1 = False
        | (str char at s 0) != 45 = False
        | otherwise = is jot cluster loop s 1 (str len s)

fn :: is jot cluster loop String Int Int => Bool
      is jot cluster loop s idx len =
        | idx >= len = True
        | ((str char at s idx) == 99) or ((str char at s idx) == 110) or ((str char at s idx) == 114) = is jot cluster loop s (idx + 1) len
        | otherwise = False

fn :: apply jot cluster String Int Int JotSpec => JotSpec
      apply jot cluster s idx len spec =
        | idx >= len = spec
        | (str char at s idx) == 99 = apply jot cluster s (idx + 1) len (spec with j_char_mode = True)
        | (str char at s idx) == 110 = apply jot cluster s (idx + 1) len (spec with j_no_newline = True)
        | (str char at s idx) == 114 = apply jot cluster s (idx + 1) len (spec with j_random = True)
        | otherwise = apply jot cluster s (idx + 1) len spec

fn :: parse jot loop [String] Bool JotSpec [String] => JotSpec
      parse jot loop [] stop_opts spec pos = assign jot pos pos spec
      parse jot loop (arg:rest) True spec pos = parse jot loop rest True spec (pos :: [arg])
      parse jot loop (arg:rest) False spec pos =
        | arg == "--" = parse jot loop rest True spec pos
        | arg == "-" = parse jot loop rest False spec (pos :: [arg])
        | is numeric arg arg = parse jot loop rest False spec (pos :: [arg])
        | is jot flag cluster arg = parse jot loop rest False (apply jot cluster arg 1 (str len arg) spec) pos
        | arg == "-b" = parse jot flag b rest spec pos
        | arg == "-w" = parse jot flag w rest spec pos
        | arg == "-s" = parse jot flag s rest spec pos
        | arg == "-p" = parse jot flag p rest spec pos
        | str starts with arg "-b" = parse jot inline b (str slice arg 2 (str len arg)) rest spec pos
        | str starts with arg "-w" = parse jot inline w (str slice arg 2 (str len arg)) rest spec pos
        | str starts with arg "-s" = parse jot inline s (str slice arg 2 (str len arg)) rest spec pos
        | str starts with arg "-p" = parse jot inline p (str slice arg 2 (str len arg)) rest spec pos
        | otherwise = parse jot loop rest False spec (pos :: [arg])

fn :: parse jot flag b [String] JotSpec [String] => JotSpec
      parse jot flag b [] spec pos = assign jot pos pos spec
      parse jot flag b (word:rest) spec pos = parse jot inline b word rest spec pos

fn :: parse jot inline b String [String] JotSpec [String] => JotSpec
      parse jot inline b word rest spec pos = parse jot loop rest False (spec with j_word = word, j_has_word = True) pos

fn :: parse jot flag w [String] JotSpec [String] => JotSpec
      parse jot flag w [] spec pos = assign jot pos pos spec
      parse jot flag w (fmt:rest) spec pos = parse jot inline w fmt rest spec pos

fn :: parse jot inline w String [String] JotSpec [String] => JotSpec
      parse jot inline w fmt rest spec pos = parse jot loop rest False (spec with j_fmt = fmt) pos

fn :: parse jot flag s [String] JotSpec [String] => JotSpec
      parse jot flag s [] spec pos = assign jot pos pos spec
      parse jot flag s (sep:rest) spec pos = parse jot inline s sep rest spec pos

fn :: parse jot inline s String [String] JotSpec [String] => JotSpec
      parse jot inline s sep rest spec pos = parse jot loop rest False (spec with j_sep = sep) pos

fn :: parse jot flag p [String] JotSpec [String] => JotSpec
      parse jot flag p [] spec pos = assign jot pos pos spec
      parse jot flag p (p:rest) spec pos = parse jot inline p p rest spec pos

fn :: parse jot inline p String [String] JotSpec [String] => JotSpec
      parse jot inline p p rest spec pos = parse jot loop rest False (spec with j_prec = (str to int p)) pos

fn :: extract fmt prec String Int Int => Int
      extract fmt prec fmt idx len =
        | (idx + 2) >= len = 0
        | ((str char at fmt idx) == 37) and ((str char at fmt (idx + 1)) == 46) = str to int (fst (read digits at fmt (idx + 2) len ""))
        | otherwise = extract fmt prec fmt (idx + 1) len

fn :: infer pos prec [String] Int  => Int
      infer pos prec []       best =  best
      infer pos prec (x:xs)   best =  infer pos prec xs (max int best (decimal places x))

fn :: resolve jot prec [String] Int String => Int
      resolve jot prec pos explicit_prec fmt =
        | explicit_prec >= 0 = explicit_prec
        | otherwise = max int (infer pos prec pos 0) (extract fmt prec fmt 0 (str len fmt))

fn :: assign jot pos [String]           JotSpec => JotSpec
      assign jot pos []                 spec    =  spec with j_prec = (max int 0 (j_prec spec))
      assign jot pos (p1:[])            spec    =
        pr = resolve jot prec [p1] (j_prec spec) (j_fmt spec)
        res = spec with j_reps = (parse jot param p1 False 0), j_prec = pr
        res
      assign jot pos (p1:p2:[])         spec    =
        pr = resolve jot prec [p2] (j_prec spec) (j_fmt spec)
        res = spec with j_reps = (parse jot param p1 False 0), j_begin = (parse jot param p2 (j_char_mode spec) pr), j_prec = pr
        res
      assign jot pos (p1:p2:p3:[])      spec    =
        pr = resolve jot prec [p2, p3] (j_prec spec) (j_fmt spec)
        res = spec with j_reps = (parse jot param p1 False 0), j_begin = (parse jot param p2 (j_char_mode spec) pr), j_end = (parse jot param p3 (j_char_mode spec) pr), j_prec = pr
        res
      assign jot pos (p1:p2:p3:p4:rest) spec    =
        pr = resolve jot prec [p2, p3, p4] (j_prec spec) (j_fmt spec)
        step_pr = select step prec (j_random spec) pr
        res = spec with j_reps = (parse jot param p1 False 0), j_begin = (parse jot param p2 (j_char_mode spec) pr), j_end = (parse jot param p3 (j_char_mode spec) pr), j_step = (parse jot param p4 False step_pr), j_prec = pr
        res

fn :: select step prec Bool  Int => Int
      select step prec True  pr  =  0
      select step prec False pr  =  pr

op :: run jot [String] => String
      run jot args     =  evaluate jot (parse jot args args)

-- ============================================================================
-- 2. BSD rs(1) — FR-FILTER-001
-- ============================================================================

fn :: default rs spec => RsSpec
      default rs spec =  RsSpec 0 0 False "" "" False 2 False 0 False 0 False False False []

op :: parse rs args [String] => RsSpec
      parse rs args args     =  parse rs loop args (default rs spec) []

fn :: is rs flag cluster String => Bool
      is rs flag cluster s      =
        | (str len s) <= 1 = False
        | (str char at s 0) != 45 = False
        | otherwise = is rs cluster loop s 1 (str len s)

fn :: is rs cluster loop String Int Int => Bool
      is rs cluster loop s idx len =
        | idx >= len = True
        | ((str char at s idx) == 116) or ((str char at s idx) == 106) or ((str char at s idx) == 101) or ((str char at s idx) == 121) or ((str char at s idx) == 104) or ((str char at s idx) == 72) or ((str char at s idx) == 122) = is rs cluster loop s (idx + 1) len
        | otherwise = False

fn :: apply rs cluster String Int Int RsSpec => RsSpec
      apply rs cluster s idx len spec =
        | idx >= len = spec
        | (str char at s idx) == 116 = apply rs cluster s (idx + 1) len (spec with r_transpose = True)
        | (str char at s idx) == 106 = apply rs cluster s (idx + 1) len (spec with r_right_align = True)
        | (str char at s idx) == 101 = apply rs cluster s (idx + 1) len (spec with r_line_entries = True)
        | (str char at s idx) == 121 = apply rs cluster s (idx + 1) len (spec with r_recycle = True)
        | ((str char at s idx) == 104) or ((str char at s idx) == 72) = apply rs cluster s (idx + 1) len (spec with r_shape_only = True)
        | (str char at s idx) == 122 = apply rs cluster s (idx + 1) len (spec with r_squeeze = True)
        | otherwise = apply rs cluster s (idx + 1) len spec

fn :: parse rs loop [String] RsSpec [String] => RsSpec
      parse rs loop [] spec pos = assign rs pos pos spec
      parse rs loop (arg:rest) spec pos =
        | is rs flag cluster arg = parse rs loop rest (apply rs cluster arg 1 (str len arg) spec) pos
        | (arg == "-c") or (arg == "-s") = parse rs flag c rest spec pos
        | (arg == "-C") or (arg == "-S") = parse rs flag cap c rest spec pos
        | arg == "-g" = parse rs flag g rest spec pos
        | arg == "-w" = parse rs flag w rest spec pos
        | arg == "-k" = parse rs flag k rest spec pos
        | (str starts with arg "-c") or (str starts with arg "-s") = parse rs inline c (str slice arg 2 (str len arg)) rest spec pos
        | (str starts with arg "-C") or (str starts with arg "-S") = parse rs inline cap c (str slice arg 2 (str len arg)) rest spec pos
        | str starts with arg "-g" = parse rs inline g (str slice arg 2 (str len arg)) rest spec pos
        | str starts with arg "-w" = parse rs inline w (str slice arg 2 (str len arg)) rest spec pos
        | str starts with arg "-k" = parse rs inline k (str slice arg 2 (str len arg)) rest spec pos
        | otherwise = parse rs loop rest spec (pos :: [arg])

fn :: parse rs flag c [String] RsSpec [String] => RsSpec
      parse rs flag c [] spec pos = parse rs inline c (char from code 9) [] spec pos
      parse rs flag c (sep:rest) spec pos = parse rs inline c sep rest spec pos

fn :: parse rs flag cap c [String] RsSpec [String] => RsSpec
      parse rs flag cap c [] spec pos = parse rs inline cap c (char from code 9) [] spec pos
      parse rs flag cap c (sep:rest) spec pos = parse rs inline cap c sep rest spec pos

fn :: parse rs flag g [String] RsSpec [String] => RsSpec
      parse rs flag g [] spec pos = assign rs pos pos spec
      parse rs flag g (g:rest) spec pos = parse rs inline g g rest spec pos

fn :: parse rs inline g String [String] RsSpec [String] => RsSpec
      parse rs inline g g rest spec pos = parse rs loop rest (spec with r_gutter = (str to int g)) pos

fn :: parse rs flag w [String] RsSpec [String] => RsSpec
      parse rs flag w [] spec pos = assign rs pos pos spec
      parse rs flag w (w:rest) spec pos = parse rs inline w w rest spec pos

fn :: parse rs inline w String [String] RsSpec [String] => RsSpec
      parse rs inline w w rest spec pos = parse rs loop rest (spec with r_width = (str to int w)) pos

fn :: parse rs flag k [String] RsSpec [String] => RsSpec
      parse rs flag k [] spec pos = assign rs pos pos spec
      parse rs flag k (k:rest) spec pos = parse rs inline k k rest spec pos

fn :: parse rs inline k String [String] RsSpec [String] => RsSpec
      parse rs inline k k rest spec pos = parse rs loop rest (spec with r_skip_lines = (str to int k)) pos

fn :: parse rs inline c String [String] RsSpec [String] => RsSpec
      parse rs inline c sep rest spec pos = parse rs loop rest (spec with r_in_sep = sep) pos

fn :: parse rs inline cap c String [String] RsSpec [String] => RsSpec
      parse rs inline cap c sep rest spec pos = parse rs loop rest (spec with r_out_sep = sep, r_has_out_sep = True) pos

fn :: assign rs pos [String]     RsSpec => RsSpec
      assign rs pos []           spec   =  spec
      assign rs pos (p1:[])      spec   =
        | is digits only p1 = spec with r_rows = (str to int p1), r_cols = 0, r_files = []
        | otherwise = spec with r_files = [p1]
      assign rs pos (p1:p2:rest) spec   =
        | (is digits only p1) and (is digits only p2) = spec with r_rows = (str to int p1), r_cols = (str to int p2), r_files = rest
        | is digits only p1 = spec with r_rows = (str to int p1), r_cols = 0, r_files = ([p2] :: rest)
        | otherwise = spec with r_files = ([p1, p2] :: rest)

fn :: ceil div Int Int => Int
      ceil div n   d   =
        | d <= 0 = 1
        | n <= 0 = 0
        | otherwise = ((n + d - 1) / d)

fn :: max item len [String] Int  => Int
      max item len []       best =  best
      max item len (x:xs)   best =  max item len xs (max int best (str len x))

fn :: infer rs dims [String] Int Int Int Int Int => Int Int
      infer rs dims items total r c width gutter =
        | total <= 0 = (0, 0)
        | (r > 0) and (c > 0) = (r, c)
        | (r <= 0) and (c > 0) = (ceil div total c, c)
        | (r > 0) and (c <= 0) = (r, ceil div total r)
        | width > 0 = infer rs width dims items total width gutter
        | otherwise = (1, total)

fn :: infer rs width dims [String] Int Int Int => Int Int
      infer rs width dims items total width gutter =
        m_len = max int 1 (max item len items 0)
        fit_cols = max int 1 ((width + gutter) / (m_len + gutter))
        cols = min int total fit_cols
        (ceil div total cols, cols)

fn :: grid cell index Int Int Int Int Bool => Int
      grid cell index orig_rows orig_cols r c False = (r * orig_cols) + c
      grid cell index orig_rows orig_cols r c True = (c * orig_cols) + r

fn :: grid cell [String] Int Int Int Int Int Bool Bool => String
      grid cell items total orig_rows orig_cols r c trans recycle =
        idx = grid cell index orig_rows orig_cols r c trans
        resolve grid item items total idx recycle

fn :: resolve grid item [String] Int Int Bool => String
      resolve grid item items total idx False = str list nth items idx
      resolve grid item items total idx True =
        | total <= 0 = ""
        | otherwise = str list nth items (idx % total)

fn :: max col width [String] Int Int Int Int Int Int Bool Bool Int => Int
      max col width items total orig_rows orig_cols out_rows c row_idx trans recycle best =
        | row_idx >= out_rows = best
        | otherwise =
          cell = grid cell items total orig_rows orig_cols row_idx c trans recycle
          w = str len cell
          next_best = max int best w
          (max col width items total orig_rows orig_cols out_rows c (row_idx + 1) trans recycle next_best)

fn :: compute col widths [String] Int Int Int Int Int Int Bool Bool [Int] => [Int]
      compute col widths items total orig_rows orig_cols out_rows out_cols c trans recycle acc =
        | c >= out_cols = acc
        | otherwise =
          w = max col width items total orig_rows orig_cols out_rows c 0 trans recycle 0
          (compute col widths items total orig_rows orig_cols out_rows out_cols (c + 1) trans recycle (acc :: [w]))

fn :: row active cols Int Int Int Int Int Bool Bool Int Int => Int
      row active cols total orig_rows orig_cols r out_cols trans True c last_valid = out_cols
      row active cols total orig_rows orig_cols r out_cols trans False c last_valid =
        | c >= out_cols = last_valid + 1
        | (grid cell index orig_rows orig_cols r c trans) < total = row active cols total orig_rows orig_cols r out_cols trans False (c + 1) c
        | otherwise = row active cols total orig_rows orig_cols r out_cols trans False (c + 1) last_valid

fn :: format rs cell String Int Bool Bool Bool => String
      format rs cell cell width right_align squeeze is_last =
        | squeeze = cell
        | right_align = pad left cell width
        | is_last = cell
        | otherwise = pad right cell width

fn :: render rs row [String] Int Int Int Int Int Bool [Int] RsSpec Int [String] => String
      render rs row items total orig_rows orig_cols r active_cols trans widths spec c acc =
        | c >= active_cols = join rs cells acc spec
        | otherwise =
          raw_cell = grid cell items total orig_rows orig_cols r c trans (r_recycle spec)
          col_w = int list nth widths c
          is_last = (c + 1) >= active_cols
          formatted = format rs cell raw_cell col_w (r_right_align spec) (r_squeeze spec) (is_last or (r_has_out_sep spec))
          (render rs row items total orig_rows orig_cols r active_cols trans widths spec (c + 1) (acc :: [formatted]))

fn :: join rs cells [String] RsSpec => String
      join rs cells cells    spec   =
        | r_has_out_sep spec = join with cells (r_out_sep spec)
        | otherwise = join with cells (repeat str " " (r_gutter spec))

fn :: render rs rows [String] Int Int Int Int Int Int Bool [Int] RsSpec [String] => [String]
      render rs rows items total orig_rows orig_cols out_rows out_cols r trans widths spec acc =
        | r >= out_rows = acc
        | otherwise =
          active_cols = row active cols total orig_rows orig_cols r out_cols trans (r_recycle spec) 0 (0 - 1)
          row_str = render rs row items total orig_rows orig_cols r active_cols trans widths spec 0 []
          (render rs rows items total orig_rows orig_cols out_rows out_cols (r + 1) trans widths spec (acc :: [row_str]))

fn :: preprocess rs lines String Int    => String
      preprocess rs lines raw    skip_n =
        | skip_n <= 0 = raw
        | otherwise = join with (drop str list (split lines raw) skip_n) (char from code 10)

fn :: tokenize rs input String RsSpec => [String]
      tokenize rs input raw    spec   =
        trimmed = preprocess rs lines raw (r_skip_lines spec)
        tokenize rs trimmed trimmed spec

fn :: tokenize rs trimmed String  RsSpec => [String]
      tokenize rs trimmed trimmed spec   =
        | r_line_entries spec = split lines trimmed
        | (str len (r_in_sep spec)) > 0 = split char loop trimmed 0 (str len trimmed) (str char at (r_in_sep spec) 0) "" []
        | otherwise = split whitespace trimmed

op :: reshape text String with RsSpec => String
      reshape text raw    with spec   =
        items = tokenize rs input raw spec
        total = list len items
        dims = infer rs dims items total (r_rows spec) (r_cols spec) (r_width spec) (r_gutter spec)
        orig_rows = fst dims
        orig_cols = snd dims
        out_dims = select out dims orig_rows orig_cols (r_transpose spec)
        out_rows = fst out_dims
        out_cols = snd out_dims
        render rs or shape items total orig_rows orig_cols out_rows out_cols spec

fn :: render rs or shape [String] Int Int Int Int Int RsSpec => String
      render rs or shape items total orig_rows orig_cols out_rows out_cols spec =
        | r_shape_only spec = (int to str out_rows) + " by " + (int to str out_cols)
        | otherwise = render rs full grid items total orig_rows orig_cols out_rows out_cols spec

fn :: render rs full grid [String] Int Int Int Int Int RsSpec => String
      render rs full grid items total orig_rows orig_cols out_rows out_cols spec =
        widths = compute col widths items total orig_rows orig_cols out_rows out_cols 0 (r_transpose spec) (r_recycle spec) []
        rows = render rs rows items total orig_rows orig_cols out_rows out_cols 0 (r_transpose spec) widths spec []
        join with rows (char from code 10)

fn :: select out dims Int Int Bool => Int Int
      select out dims r c False = (r, c)
      select out dims r c True = (c, r)

-- ============================================================================
-- 3. BSD lam(1) — FR-FILTER-002
-- ============================================================================

fn :: parse min max String => Int Int
      parse min max spec   =  parse min max loop spec 0 (str len spec) ""

fn :: parse min max loop String Int Int String => Int Int
      parse min max loop s idx len left =
        | idx >= len = (parse opt int left, 0)
        | (str char at s idx) == 46 = (parse opt int left, parse opt int (str slice s (idx + 1) len))
        | otherwise = parse min max loop s (idx + 1) len (left + (str slice s idx (idx + 1)))

fn :: parse opt int String => Int
      parse opt int s      =
        | (str len s) == 0 = 0
        | s == "-" = 0
        | otherwise = parse signed int s

fn :: format lam cell String Int Int => String
      format lam cell s min_w max_w =
        truncated = truncate lam s max_w
        pad lam truncated min_w

fn :: truncate lam String Int   => String
      truncate lam s      max_w =
        | (max_w > 0) and ((str len s) > max_w) = str slice s 0 max_w
        | otherwise = s

fn :: pad lam String Int   => String
      pad lam s      min_w =
        | min_w > 0 = pad left s min_w
        | min_w < 0 = pad right s (0 - min_w)
        | otherwise = s

fn :: max lam lines [LamCol]   Int  => Int
      max lam lines []         best =  best
      max lam lines (col:rest) best =
        n = list len (l_lines col)
        max lam lines rest (max int best n)

fn :: render lam line [LamCol] Int String => String
      render lam line [] row_idx trail_sep = trail_sep
      render lam line (col:rest) row_idx trail_sep =
        in_bounds = row_idx < (list len (l_lines col))
        raw_line = str list nth (l_lines col) row_idx
        cell_body = select lam cell body in_bounds (l_pad_eof col) raw_line (l_min col) (l_max col)
        cell = (l_sep col) + cell_body
        cell + (render lam line rest row_idx trail_sep)

fn :: select lam cell body Bool Bool String Int Int => String
      select lam cell body True pad_eof raw_line min_w max_w = format lam cell raw_line min_w max_w
      select lam cell body False True raw_line min_w max_w = format lam cell "" min_w max_w
      select lam cell body False False raw_line min_w max_w = format lam cell "" min_w max_w

fn :: render lam all [LamCol] Int Int String [String] => [String]
      render lam all cols row_idx total_rows trail_sep acc =
        | row_idx >= total_rows = acc
        | otherwise =
          line = render lam line cols row_idx trail_sep
          (render lam all cols (row_idx + 1) total_rows trail_sep (acc :: [line]))

op :: laminate columns [LamCol] with String    => String
      laminate columns cols     with trail_sep =
        total_rows = max lam lines cols 0
        lines = render lam all cols 0 total_rows trail_sep []
        join with lines (char from code 10)

op :: parse lam args [String] => [LamCol] String
      parse lam args args     =  parse lam loop args "" "" 0 0 0 0 False False 10 10 []

fn :: parse lam loop [String] String String Int Int Int Int Bool Bool Int Int [LamCol] => [LamCol] String
      parse lam loop [] cur_s def_s cur_min cur_max def_min def_max cur_p def_p cur_t def_t acc = (acc, select nonempty cur_s def_s)
      parse lam loop (arg:rest) cur_s def_s cur_min cur_max def_min def_max cur_p def_p cur_t def_t acc =
        | arg == "-s" = parse lam flag s rest False cur_s def_s cur_min cur_max def_min def_max cur_p def_p cur_t def_t acc
        | arg == "-S" = parse lam flag s rest True cur_s def_s cur_min cur_max def_min def_max cur_p def_p cur_t def_t acc
        | arg == "-f" = parse lam flag f rest False False cur_s def_s cur_min cur_max def_min def_max cur_p def_p cur_t def_t acc
        | arg == "-F" = parse lam flag f rest True False cur_s def_s cur_min cur_max def_min def_max cur_p def_p cur_t def_t acc
        | arg == "-p" = parse lam flag f rest False True cur_s def_s cur_min cur_max def_min def_max cur_p def_p cur_t def_t acc
        | arg == "-P" = parse lam flag f rest True True cur_s def_s cur_min cur_max def_min def_max cur_p def_p cur_t def_t acc
        | arg == "-t" = parse lam flag t rest False cur_s def_s cur_min cur_max def_min def_max cur_p def_p cur_t def_t acc
        | arg == "-T" = parse lam flag t rest True cur_s def_s cur_min cur_max def_min def_max cur_p def_p cur_t def_t acc
        | str starts with arg "-s" = parse lam loop rest (str slice arg 2 (str len arg)) def_s cur_min cur_max def_min def_max cur_p def_p cur_t def_t acc
        | str starts with arg "-S" = parse lam loop rest (str slice arg 2 (str len arg)) (str slice arg 2 (str len arg)) cur_min cur_max def_min def_max cur_p def_p cur_t def_t acc
        | str starts with arg "-f" = parse lam inline f (str slice arg 2 (str len arg)) rest False False cur_s def_s cur_min cur_max def_min def_max cur_p def_p cur_t def_t acc
        | str starts with arg "-F" = parse lam inline f (str slice arg 2 (str len arg)) rest True False cur_s def_s cur_min cur_max def_min def_max cur_p def_p cur_t def_t acc
        | str starts with arg "-p" = parse lam inline f (str slice arg 2 (str len arg)) rest False True cur_s def_s cur_min cur_max def_min def_max cur_p def_p cur_t def_t acc
        | str starts with arg "-P" = parse lam inline f (str slice arg 2 (str len arg)) rest True True cur_s def_s cur_min cur_max def_min def_max cur_p def_p cur_t def_t acc
        | str starts with arg "-t" = parse lam inline t (str slice arg 2 (str len arg)) rest False cur_s def_s cur_min cur_max def_min def_max cur_p def_p cur_t def_t acc
        | str starts with arg "-T" = parse lam inline t (str slice arg 2 (str len arg)) rest True cur_s def_s cur_min cur_max def_min def_max cur_p def_p cur_t def_t acc
        | otherwise = parse lam file arg rest cur_s def_s cur_min cur_max def_min def_max cur_p def_p cur_t def_t acc

fn :: select nonempty String String => String
      select nonempty a      b      =
        | (str len a) > 0 = a
        | otherwise = b

fn :: parse lam flag s [String] Bool String String Int Int Int Int Bool Bool Int Int [LamCol] => [LamCol] String
      parse lam flag s [] persist cur_s def_s cur_min cur_max def_min def_max cur_p def_p cur_t def_t acc = (acc, select nonempty cur_s def_s)
      parse lam flag s (sep:rest) False cur_s def_s cur_min cur_max def_min def_max cur_p def_p cur_t def_t acc =
        parse lam loop rest sep def_s cur_min cur_max def_min def_max cur_p def_p cur_t def_t acc
      parse lam flag s (sep:rest) True cur_s def_s cur_min cur_max def_min def_max cur_p def_p cur_t def_t acc =
        parse lam loop rest sep sep cur_min cur_max def_min def_max cur_p def_p cur_t def_t acc

fn :: parse lam flag f [String] Bool Bool String String Int Int Int Int Bool Bool Int Int [LamCol] => [LamCol] String
      parse lam flag f [] persist pad_eof cur_s def_s cur_min cur_max def_min def_max cur_p def_p cur_t def_t acc = (acc, select nonempty cur_s def_s)
      parse lam flag f (mm:rest) persist pad_eof cur_s def_s cur_min cur_max def_min def_max cur_p def_p cur_t def_t acc =
        parse lam inline f mm rest persist pad_eof cur_s def_s cur_min cur_max def_min def_max cur_p def_p cur_t def_t acc

fn :: parse lam inline f String [String] Bool Bool String String Int Int Int Int Bool Bool Int Int [LamCol] => [LamCol] String
      parse lam inline f mm rest False pad_eof cur_s def_s cur_min cur_max def_min def_max cur_p def_p cur_t def_t acc =
        bounds = parse min max mm
        parse lam loop rest cur_s def_s (fst bounds) (snd bounds) def_min def_max pad_eof def_p cur_t def_t acc
      parse lam inline f mm rest True pad_eof cur_s def_s cur_min cur_max def_min def_max cur_p def_p cur_t def_t acc =
        bounds = parse min max mm
        b1 = fst bounds
        b2 = snd bounds
        parse lam loop rest cur_s def_s b1 b2 b1 b2 pad_eof pad_eof cur_t def_t acc

fn :: parse lam flag t [String] Bool String String Int Int Int Int Bool Bool Int Int [LamCol] => [LamCol] String
      parse lam flag t [] persist cur_s def_s cur_min cur_max def_min def_max cur_p def_p cur_t def_t acc = (acc, select nonempty cur_s def_s)
      parse lam flag t (tc:rest) persist cur_s def_s cur_min cur_max def_min def_max cur_p def_p cur_t def_t acc =
        parse lam inline t tc rest persist cur_s def_s cur_min cur_max def_min def_max cur_p def_p cur_t def_t acc

fn :: parse lam inline t String [String] Bool String String Int Int Int Int Bool Bool Int Int [LamCol] => [LamCol] String
      parse lam inline t tc rest False cur_s def_s cur_min cur_max def_min def_max cur_p def_p cur_t def_t acc =
        code = char at or tc 0 10
        parse lam loop rest cur_s def_s cur_min cur_max def_min def_max cur_p def_p code def_t acc
      parse lam inline t tc rest True cur_s def_s cur_min cur_max def_min def_max cur_p def_p cur_t def_t acc =
        code = char at or tc 0 10
        parse lam loop rest cur_s def_s cur_min cur_max def_min def_max cur_p def_p code code acc

fn :: read stdin chunk String String => String
      read stdin chunk acc    ""     =  acc
      read stdin chunk acc    chunk  =  read stdin loop (acc + chunk)

fn :: read stdin loop String => String
      read stdin loop acc    =
        read stdin chunk acc (posix read 0 65536)

fn :: read stdin all => String
      read stdin all =  read stdin loop ""

fn :: read lam file String => String
      read lam file path   =
        | path == "-" = read stdin all
        | otherwise = read file path

fn :: parse lam file String [String] String String Int Int Int Int Bool Bool Int Int [LamCol] => [LamCol] String
      parse lam file path rest cur_s def_s cur_min cur_max def_min def_max cur_p def_p cur_t def_t acc =
        content = read lam file path
        lines = split lines delim content 0 (str len content) cur_t "" []
        col = LamCol cur_s cur_min cur_max cur_p lines
        parse lam loop rest def_s def_s def_min def_max def_min def_max def_p def_p def_t def_t (acc :: [col])

-- ============================================================================
-- 4. BSD vis(1) & unvis(1) — FR-FILTER-002
-- ============================================================================

fn :: default vis spec => VisSpec
      default vis spec =  VisSpec False True False False False False False False 0 []

op :: parse vis args [String] => VisSpec
      parse vis args args     =  parse vis loop args (default vis spec)

fn :: apply vis flag char Int VisSpec => VisSpec
      apply vis flag char c spec =
        | c == 111 = spec with v_octal = True
        | c == 99 = spec with v_cstyle = True
        | c == 116 = spec with v_tab = True
        | c == 119 = spec with v_tab = True, v_space = True, v_newline = True
        | c == 115 = spec with v_space = True
        | c == 98 = spec with v_nobs = True
        | c == 108 = spec with v_mark_nl = True
        | c == 110 = spec with v_none = True
        | c == 102 = spec with v_fold = 80
        | otherwise = spec

fn :: apply vis flags str String Int Int VisSpec => VisSpec
      apply vis flags str s idx len spec =
        | idx >= len = spec
        | otherwise = apply vis flags str s (idx + 1) len (apply vis flag char (str char at s idx) spec)

fn :: parse vis loop [String]   VisSpec => VisSpec
      parse vis loop []         spec    =  spec
      parse vis loop (arg:rest) spec    =
        | arg == "-F" = parse vis flag fold rest spec
        | (str starts with arg "-F") and ((str len arg) > 2) = parse vis inline fold (str slice arg 2 (str len arg)) rest spec
        | (str starts with arg "-") and ((str len arg) > 1) = parse vis loop rest (apply vis flags str arg 1 (str len arg) spec)
        | otherwise = parse vis loop rest (spec with v_files = ((v_files spec) :: [arg]))

fn :: parse vis flag fold [String] VisSpec => VisSpec
      parse vis flag fold [] spec = spec
      parse vis flag fold (w:rest) spec = parse vis inline fold w rest spec

fn :: parse vis inline fold String [String] VisSpec => VisSpec
      parse vis inline fold w rest spec = parse vis loop rest (spec with v_fold = (str to int w))

fn :: bslash => String
      bslash =  char from code 92

fn :: vis prefix VisSpec => String
      vis prefix spec    =
        | v_nobs spec = ""
        | otherwise = bslash

fn :: to octal3 Int VisSpec => String
      to octal3 c   spec    =
        d2 = (c / 64) % 8
        d1 = (c / 8) % 8
        d0 = c % 8
        (vis prefix spec) + (int to str d2) + (int to str d1) + (int to str d0)

fn :: encode vis code Int VisSpec => String
      encode vis code c   spec    =
        | (c == 92) and (v_nobs spec) = bslash
        | c == 92 = bslash + bslash
        | v_none spec = char from code c
        | (c == 9) and (not (v_tab spec)) = char from code 9
        | (c == 9) and (v_octal spec) = to octal3 9 spec
        | c == 9 = (vis prefix spec) + "t"
        | (c == 10) and (v_mark_nl spec) and (not (v_newline spec)) = (vis prefix spec) + "$" + (char from code 10)
        | (c == 10) and (not (v_newline spec)) = char from code 10
        | (c == 10) and (v_octal spec) = to octal3 10 spec
        | c == 10 = (vis prefix spec) + "n"
        | (c == 32) and (not (v_space spec)) = " "
        | (c == 32) and (v_octal spec) = to octal3 32 spec
        | c == 32 = (vis prefix spec) + "s"
        | (c == 13) and (v_octal spec) = to octal3 13 spec
        | c == 13 = (vis prefix spec) + "r"
        | (c == 8) and (v_octal spec) = to octal3 8 spec
        | c == 8 = (vis prefix spec) + "b"
        | (c == 7) and (v_octal spec) = to octal3 7 spec
        | c == 7 = (vis prefix spec) + "a"
        | (c == 11) and (v_octal spec) = to octal3 11 spec
        | c == 11 = (vis prefix spec) + "v"
        | (c == 12) and (v_octal spec) = to octal3 12 spec
        | c == 12 = (vis prefix spec) + "f"
        | (c == 127) and (v_octal spec) = to octal3 127 spec
        | c == 127 = (vis prefix spec) + "^?"
        | (c >= 0) and (c < 32) and (v_octal spec) = to octal3 c spec
        | (c >= 0) and (c < 32) = (vis prefix spec) + "^" + (char from code (c + 64))
        | c >= 128 = to octal3 c spec
        | otherwise = char from code c

fn :: vis encode loop String Int Int VisSpec String => String
      vis encode loop s idx len spec acc =
        | idx >= len = acc
        | otherwise =
          c = str char at s idx
          (vis encode loop s (idx + 1) len spec (acc + (encode vis code c spec)))

fn :: fold vis lines String Int Int Int Int String => String
      fold vis lines s idx len max_col cur_col acc =
        | idx >= len = acc
        | (str char at s idx) == 10 = fold vis lines s (idx + 1) len max_col 0 (acc + (char from code 10))
        | (cur_col + 1) >= max_col = fold vis lines s idx len max_col 0 (acc + bslash + (char from code 10))
        | otherwise = fold vis lines s (idx + 1) len max_col (cur_col + 1) (acc + (str slice s idx (idx + 1)))

op :: vis encode String with VisSpec => String
      vis encode s      with spec    =
        encoded = vis encode loop s 0 (str len s) spec ""
        apply vis fold encoded (v_fold spec)

fn :: apply vis fold String  Int    => String
      apply vis fold encoded fold_w =
        | fold_w <= 1 = encoded
        | otherwise = fold vis lines encoded 0 (str len encoded) fold_w 0 ""

fn :: is octal digit Int => Bool
      is octal digit c   =  (c >= 48) and (c <= 55)

fn :: hex val Int => Int
      hex val c   =
        | (c >= 48) and (c <= 57) = c - 48
        | (c >= 65) and (c <= 70) = c - 55
        | (c >= 97) and (c <= 102) = c - 87
        | otherwise = 0 - 1

fn :: is hex digit Int => Bool
      is hex digit c   =  (hex val c) >= 0

fn :: decode c escape Int => String
      decode c escape c   =
        | c == 110 = char from code 10
        | c == 116 = char from code 9
        | c == 114 = char from code 13
        | c == 98 = char from code 8
        | c == 97 = char from code 7
        | c == 118 = char from code 11
        | c == 102 = char from code 12
        | c == 115 = " "
        | c == 69 = char from code 27
        | c == 92 = char from code 92
        | c == 36 = ""
        | c == 10 = ""
        | otherwise = char from code c

fn :: decode ctrl code Int => Int
      decode ctrl code c   =
        | c == 63 = 127
        | otherwise = c - 64

fn :: decode ctrl char Int => String
      decode ctrl char c   =  char from code (decode ctrl code c)

fn :: unvis loop String Int Int String => String
      unvis loop s idx len acc =
        | idx >= len = acc
        | ((str char at s idx) == 92) and ((idx + 1) < len) = unvis after bslash s idx len acc
        | otherwise = unvis loop s (idx + 1) len (acc + (str slice s idx (idx + 1)))

fn :: unvis after bslash String Int Int String => String
      unvis after bslash s idx len acc =
        | ((str char at s (idx + 1)) == 94) and ((idx + 2) < len) = unvis loop s (idx + 3) len (acc + (decode ctrl char (str char at s (idx + 2))))
        | ((str char at s (idx + 1)) == 77) and ((idx + 3) < len) and ((str char at s (idx + 2)) == 45) = unvis loop s (idx + 4) len (acc + (char from code (128 + (str char at s (idx + 3)))))
        | ((str char at s (idx + 1)) == 77) and ((idx + 3) < len) and ((str char at s (idx + 2)) == 94) = unvis loop s (idx + 4) len (acc + (char from code (128 + (decode ctrl code (str char at s (idx + 3))))))
        | ((str char at s (idx + 1)) == 120) and ((idx + 3) < len) and (is hex digit (str char at s (idx + 2))) and (is hex digit (str char at s (idx + 3))) = unvis hex2 s idx len acc
        | is octal digit (str char at s (idx + 1)) = unvis octal var s idx len acc
        | otherwise = unvis loop s (idx + 2) len (acc + (decode c escape (str char at s (idx + 1))))

fn :: unvis hex2 String Int Int String => String
      unvis hex2 s idx len acc =
        h1 = hex val (str char at s (idx + 2))
        h0 = hex val (str char at s (idx + 3))
        unvis loop s (idx + 4) len (acc + (char from code ((h1 * 16) + h0)))

fn :: unvis octal var String Int Int String => String
      unvis octal var s idx len acc =
        | ((idx + 3) < len) and (is octal digit (str char at s (idx + 2))) and (is octal digit (str char at s (idx + 3))) = unvis octal3 s idx len acc
        | ((idx + 2) < len) and (is octal digit (str char at s (idx + 2))) = unvis octal2 s idx len acc
        | otherwise = unvis octal1 s idx len acc

fn :: unvis octal3 String Int Int String => String
      unvis octal3 s idx len acc =
        d2 = (str char at s (idx + 1)) - 48
        d1 = (str char at s (idx + 2)) - 48
        d0 = (str char at s (idx + 3)) - 48
        code = (d2 * 64) + (d1 * 8) + d0
        unvis loop s (idx + 4) len (acc + (char from code code))

fn :: unvis octal2 String Int Int String => String
      unvis octal2 s idx len acc =
        d1 = (str char at s (idx + 1)) - 48
        d0 = (str char at s (idx + 2)) - 48
        code = (d1 * 8) + d0
        unvis loop s (idx + 3) len (acc + (char from code code))

fn :: unvis octal1 String Int Int String => String
      unvis octal1 s idx len acc =
        d0 = (str char at s (idx + 1)) - 48
        unvis loop s (idx + 2) len (acc + (char from code d0))

op :: unvis decode String => String
      unvis decode s      =  unvis loop s 0 (str len s) ""

fn :: vis encode bytes loop [Int] VisSpec String => String
      vis encode bytes loop [] spec acc = acc
      vis encode bytes loop (b:rest) spec acc =
        vis encode bytes loop rest spec (acc + (encode vis code b spec))

op :: vis encode bytes [Int] with VisSpec => String
      vis encode bytes bs    with spec    =
        encoded = vis encode bytes loop bs spec ""
        apply vis fold encoded (v_fold spec)

fn :: decode c escape bytes Int => [Int]
      decode c escape bytes c   =
        | c == 110 = [10]
        | c == 116 = [9]
        | c == 114 = [13]
        | c == 98 = [8]
        | c == 97 = [7]
        | c == 118 = [11]
        | c == 102 = [12]
        | c == 115 = [32]
        | c == 69 = [27]
        | c == 92 = [92]
        | c == 36 = []
        | c == 10 = []
        | otherwise = [c]

fn :: unvis bytes loop String Int Int [Int] => [Int]
      unvis bytes loop s idx len acc =
        | idx >= len = acc
        | ((str char at s idx) == 92) and ((idx + 1) < len) = unvis bytes after bslash s idx len acc
        | otherwise = unvis bytes loop s (idx + 1) len (acc :: [str char at s idx])

fn :: unvis bytes after bslash String Int Int [Int] => [Int]
      unvis bytes after bslash s idx len acc =
        | ((str char at s (idx + 1)) == 94) and ((idx + 2) < len) = unvis bytes loop s (idx + 3) len (acc :: [decode ctrl code (str char at s (idx + 2))])
        | ((str char at s (idx + 1)) == 77) and ((idx + 3) < len) and ((str char at s (idx + 2)) == 45) = unvis bytes loop s (idx + 4) len (acc :: [128 + (str char at s (idx + 3))])
        | ((str char at s (idx + 1)) == 77) and ((idx + 3) < len) and ((str char at s (idx + 2)) == 94) = unvis bytes loop s (idx + 4) len (acc :: [128 + (decode ctrl code (str char at s (idx + 3)))])
        | ((str char at s (idx + 1)) == 120) and ((idx + 3) < len) and (is hex digit (str char at s (idx + 2))) and (is hex digit (str char at s (idx + 3))) = unvis bytes hex2 s idx len acc
        | is octal digit (str char at s (idx + 1)) = unvis bytes octal var s idx len acc
        | otherwise = unvis bytes loop s (idx + 2) len (acc :: (decode c escape bytes (str char at s (idx + 1))))

fn :: unvis bytes hex2 String Int Int [Int] => [Int]
      unvis bytes hex2 s idx len acc =
        h1 = hex val (str char at s (idx + 2))
        h0 = hex val (str char at s (idx + 3))
        unvis bytes loop s (idx + 4) len (acc :: [(h1 * 16) + h0])

fn :: unvis bytes octal var String Int Int [Int] => [Int]
      unvis bytes octal var s idx len acc =
        | ((idx + 3) < len) and (is octal digit (str char at s (idx + 2))) and (is octal digit (str char at s (idx + 3))) = unvis bytes octal3 s idx len acc
        | ((idx + 2) < len) and (is octal digit (str char at s (idx + 2))) = unvis bytes octal2 s idx len acc
        | otherwise = unvis bytes octal1 s idx len acc

fn :: unvis bytes octal3 String Int Int [Int] => [Int]
      unvis bytes octal3 s idx len acc =
        d2 = (str char at s (idx + 1)) - 48
        d1 = (str char at s (idx + 2)) - 48
        d0 = (str char at s (idx + 3)) - 48
        code = (d2 * 64) + (d1 * 8) + d0
        unvis bytes loop s (idx + 4) len (acc :: [code])

fn :: unvis bytes octal2 String Int Int [Int] => [Int]
      unvis bytes octal2 s idx len acc =
        d1 = (str char at s (idx + 1)) - 48
        d0 = (str char at s (idx + 2)) - 48
        code = (d1 * 8) + d0
        unvis bytes loop s (idx + 3) len (acc :: [code])

fn :: unvis bytes octal1 String Int Int [Int] => [Int]
      unvis bytes octal1 s idx len acc =
        d0 = (str char at s (idx + 1)) - 48
        unvis bytes loop s (idx + 2) len (acc :: [d0])

op :: unvis decode bytes String => [Int]
      unvis decode bytes s      =  unvis bytes loop s 0 (str len s) []
