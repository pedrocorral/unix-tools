-- filter_lib.rg — Exhaustive pure Ridgun implementation for Tier 1 BSD Filters
-- @implements FR-FILTER-001
-- @implements FR-FILTER-002

type JotParam = Omitted | Given Int

type JotSpec = j_reps: JotParam, j_begin: JotParam, j_end: JotParam, j_step: JotParam, j_word: String, j_has_word: Bool, j_fmt: String, j_char_mode: Bool, j_sep: String, j_no_newline: Bool, j_random: Bool, j_prec: Int

type RsSpec = r_rows: Int, r_cols: Int, r_transpose: Bool, r_in_sep: String, r_out_sep: String, r_has_out_sep: Bool, r_gutter: Int, r_right_align: Bool, r_width: Int, r_line_entries: Bool, r_skip_lines: Int, r_recycle: Bool, r_shape_only: Bool, r_squeeze: Bool, r_files: [String]

type LamCol = l_sep: String, l_min: Int, l_max: Int, l_pad_eof: Bool, l_lines: [String]

type VisSpec = v_octal: Bool, v_cstyle: Bool, v_tab: Bool, v_space: Bool, v_newline: Bool, v_nobs: Bool, v_mark_nl: Bool, v_none: Bool, v_fold: Int, v_files: [String]

-- ============================================================================
-- Generic List, Integer & String Helpers
-- ============================================================================

fn :: list_len [A]    => Int
      list_len []     =  0
      list_len (_:xs) =  1 + (list_len xs)

fn :: fst (A, B) => A
      fst (a, _) =  a

fn :: snd (A, B) => B
      snd (_, b) =  b

fn :: str_list_nth [String] Int => String
      str_list_nth []       idx =  ""
      str_list_nth (x:xs)   n   =
        | n <= 0 = x
        | otherwise = str_list_nth xs (n - 1)

fn :: int_list_nth [Int]  Int => Int
      int_list_nth []     idx =  0
      int_list_nth (x:xs) n   =
        | n <= 0 = x
        | otherwise = int_list_nth xs (n - 1)

fn :: drop_str_list [A]    Int => [A]
      drop_str_list []     n   =  []
      drop_str_list (x:xs) n   =
        | n <= 0 = [x] :: xs
        | otherwise = drop_str_list xs (n - 1)

fn :: join_with [String] String => String
      join_with []       sep    =  ""
      join_with (x:[])   sep    =  x
      join_with (x:xs)   sep    =  x + sep + (join_with xs sep)

fn :: repeat_str String Int => String
      repeat_str s      n   =
        | n <= 0 = ""
        | otherwise = s + (repeat_str s (n - 1))

fn :: pad_right String Int   => String
      pad_right s      width =
        | (str_len s) >= width = s
        | otherwise = s + (repeat_str " " (width - (str_len s)))

fn :: pad_left String Int   => String
      pad_left s      width =
        | (str_len s) >= width = s
        | otherwise = (repeat_str " " (width - (str_len s))) + s

fn :: pad_left_char String Int   String => String
      pad_left_char s      width ch     =
        | (str_len s) >= width = s
        | otherwise = (repeat_str ch (width - (str_len s))) + s

fn :: pow10 Int => Int
      pow10 n   =
        | n <= 0 = 1
        | otherwise = 10 * (pow10 (n - 1))

fn :: abs_int Int => Int
      abs_int n   =
        | n < 0 = 0 - n
        | otherwise = n

fn :: max_int Int Int => Int
      max_int a   b   =
        | a >= b = a
        | otherwise = b

fn :: is_digits_only String => Bool
      is_digits_only s      =
        | (str_len s) == 0 = False
        | otherwise = is_digits_loop s 0 (str_len s)

fn :: is_digits_loop String Int Int => Bool
      is_digits_loop s      idx len =
        | idx >= len = True
        | ((str_char_at s idx) >= 48) and ((str_char_at s idx) <= 57) = is_digits_loop s (idx + 1) len
        | otherwise = False

fn :: parse_signed_int String => Int
      parse_signed_int s      =
        | (str_len s) == 0 = 0
        | ((str_char_at s 0) == 45) and ((str_len s) > 1) = 0 - (str_to_int (str_slice s 1 (str_len s)))
        | ((str_char_at s 0) == 43) and ((str_len s) > 1) = str_to_int (str_slice s 1 (str_len s))
        | otherwise = str_to_int s

fn :: is_signed_int_str String => Bool
      is_signed_int_str s      =
        | (str_len s) == 0 = False
        | ((str_char_at s 0) == 45) and ((str_len s) > 1) = is_digits_only (str_slice s 1 (str_len s))
        | ((str_char_at s 0) == 43) and ((str_len s) > 1) = is_digits_only (str_slice s 1 (str_len s))
        | otherwise = is_digits_only s

fn :: find_char_idx String Int    Int Int => Int
      find_char_idx s      target idx len =
        | idx >= len = 0 - 1
        | (str_char_at s idx) == target = idx
        | otherwise = find_char_idx s target (idx + 1) len

fn :: is_numeric_arg String => Bool
      is_numeric_arg s      =
        | is_signed_int_str s = True
        | otherwise = is_decimal_str s

fn :: is_decimal_str String => Bool
      is_decimal_str s      =
        dot_idx = find_char_idx s 46 0 (str_len s)
        check_decimal_parts s dot_idx

fn :: check_decimal_parts String Int     => Bool
      check_decimal_parts s      dot_idx =
        | dot_idx <= 0 = False
        | (dot_idx + 1) >= (str_len s) = False
        | otherwise = (is_signed_int_str (str_slice s 0 dot_idx)) and (is_digits_only (str_slice s (dot_idx + 1) (str_len s)))

fn :: decimal_places String => Int
      decimal_places s      =
        dot_idx = find_char_idx s 46 0 (str_len s)
        calc_decimal_places s dot_idx

fn :: calc_decimal_places String Int     => Int
      calc_decimal_places s      dot_idx =
        | dot_idx < 0 = 0
        | otherwise = (str_len s) - dot_idx - 1

fn :: parse_scaled_int String Int  => Int
      parse_scaled_int s      prec =
        | prec <= 0 = parse_signed_int s
        | otherwise = parse_scaled_dot s prec (find_char_idx s 46 0 (str_len s))

fn :: parse_scaled_dot String Int  Int     => Int
      parse_scaled_dot s      prec dot_idx =
        | dot_idx < 0 = (parse_signed_int s) * (pow10 prec)
        | otherwise = combine_scaled_parts (str_slice s 0 dot_idx) (str_slice s (dot_idx + 1) (str_len s)) prec

fn :: combine_scaled_parts String   String    Int  => Int
      combine_scaled_parts int_part frac_part prec =
        is_neg = ((str_len int_part) > 0) and ((str_char_at int_part 0) == 45)
        abs_i = abs_int (parse_signed_int int_part)
        padded_frac = pad_or_trunc_frac frac_part prec
        frac_val = str_to_int padded_frac
        mag = (abs_i * (pow10 prec)) + frac_val
        apply_sign is_neg mag

fn :: apply_sign Bool  Int => Int
      apply_sign True  mag =  0 - mag
      apply_sign False mag =  mag

fn :: pad_or_trunc_frac String Int  => String
      pad_or_trunc_frac frac   prec =
        | (str_len frac) == prec = frac
        | (str_len frac) > prec = str_slice frac 0 prec
        | otherwise = frac + (repeat_str "0" (prec - (str_len frac)))

fn :: format_scaled_int Int Int  => String
      format_scaled_int val prec =
        | prec <= 0 = int_to_str val
        | val < 0 = "-" + (format_scaled_pos (0 - val) prec)
        | otherwise = format_scaled_pos val prec

fn :: format_scaled_pos Int Int  => String
      format_scaled_pos mag prec =
        scale = pow10 prec
        int_part = mag / scale
        frac_part = mag % scale
        (int_to_str int_part) + "." + (pad_left_char (int_to_str frac_part) prec "0")

fn :: is_ws_code Int => Bool
      is_ws_code c   =
        | c == 32 = True
        | c == 9 = True
        | c == 10 = True
        | c == 13 = True
        | otherwise = False

fn :: split_ws_loop String Int Int String [String] => [String]
      split_ws_loop s idx len cur acc =
        | idx >= len = finalize_token cur acc
        | is_ws_code (str_char_at s idx) = split_ws_after_space s (idx + 1) len cur acc
        | otherwise = split_ws_loop s (idx + 1) len (cur + (str_slice s idx (idx + 1))) acc

fn :: split_ws_after_space String Int Int String [String] => [String]
      split_ws_after_space s next_idx len cur acc =
        | (str_len cur) == 0 = split_ws_loop s next_idx len "" acc
        | otherwise = split_ws_loop s next_idx len "" (acc :: [cur])

fn :: finalize_token String [String] => [String]
      finalize_token cur    acc      =
        | (str_len cur) == 0 = acc
        | otherwise = acc :: [cur]

op :: split whitespace String => [String]
      split whitespace s      =  split_ws_loop s 0 (str_len s) "" []

fn :: split_char_loop String Int Int Int String [String] => [String]
      split_char_loop s idx len delim cur acc =
        | idx >= len = finalize_token cur acc
        | (str_char_at s idx) == delim = split_ws_after_delim s (idx + 1) len delim cur acc
        | is_ws_code (str_char_at s idx) and (delim != 10) and ((str_char_at s idx) == 10) = split_ws_after_delim s (idx + 1) len delim cur acc
        | otherwise = split_char_loop s (idx + 1) len delim (cur + (str_slice s idx (idx + 1))) acc

fn :: split_ws_after_delim String Int Int Int String [String] => [String]
      split_ws_after_delim s next_idx len delim cur acc =
        | (str_len cur) == 0 = split_char_loop s next_idx len delim "" acc
        | otherwise = split_char_loop s next_idx len delim "" (acc :: [cur])

fn :: split_lines_delim String Int Int Int String [String] => [String]
      split_lines_delim s idx len term_code cur acc =
        | idx >= len = finalize_line cur acc
        | (str_char_at s idx) == term_code = split_lines_delim s (idx + 1) len term_code "" (acc :: [cur])
        | (str_char_at s idx) == 13 = split_lines_delim s (idx + 1) len term_code cur acc
        | (term_code != 10) and ((str_char_at s idx) == 10) = split_lines_delim s (idx + 1) len term_code cur acc
        | otherwise = split_lines_delim s (idx + 1) len term_code (cur + (str_slice s idx (idx + 1))) acc

fn :: finalize_line String [String] => [String]
      finalize_line cur    acc      =
        | (str_len cur) == 0 = acc
        | otherwise = acc :: [cur]

op :: split lines String => [String]
      split lines s      =  split_lines_delim s 0 (str_len s) 10 "" []

-- ============================================================================
-- 1. BSD jot(1) — FR-FILTER-001
-- ============================================================================

fn :: is_omitted JotParam  => Bool
      is_omitted Omitted   =  True
      is_omitted (Given x) =  False

fn :: param_val JotParam  Int     => Int
      param_val Omitted   def_val =  def_val
      param_val (Given x) def_val =  x

op :: resolve jot params JotParam JotParam JotParam JotParam Int => (Int, Int, Int, Int)
      resolve jot params p_r p_b p_e p_s prec =
        scale = pow10 prec
        def_b = 1 * scale
        def_e = 100 * scale
        def_s = 1 * scale
        resolve_jot_flags (not (is_omitted p_r)) (not (is_omitted p_b)) (not (is_omitted p_e)) (not (is_omitted p_s)) (param_val p_r 100) (param_val p_b def_b) (param_val p_e def_e) (param_val p_s def_s) scale

fn :: resolve_jot_flags Bool Bool Bool Bool Int Int Int Int Int => (Int, Int, Int, Int)
      resolve_jot_flags False False False False r b e s scale = (100, 1 * scale, 100 * scale, 1 * scale)
      resolve_jot_flags True False False False r b e s scale = (r, 1 * scale, r * scale, 1 * scale)
      resolve_jot_flags True True False False r b e s scale = (r, b, b + ((r - 1) * scale), 1 * scale)
      resolve_jot_flags True True True False r b e s scale = (r, b, e, infer_step r b e)
      resolve_jot_flags False True True True r b e s scale = (infer_reps b e s, b, e, s)
      resolve_jot_flags True True False True r b e s scale = (r, b, b + (s * (r - 1)), s)
      resolve_jot_flags False True True False r b e s scale = (infer_reps b e ((default_dir b e) * scale), b, e, (default_dir b e) * scale)
      resolve_jot_flags True False True False r b e s scale = (r, e - ((r - 1) * scale), e, 1 * scale)
      resolve_jot_flags True False True True r b e s scale = (r, e - (s * (r - 1)), e, s)
      resolve_jot_flags False True False True r b e s scale = (100, b, b + (s * 99), s)
      resolve_jot_flags False False True True r b e s scale = (100, e - (s * 99), e, s)
      resolve_jot_flags False False False True r b e s scale = (100, 1 * scale, (1 * scale) + (s * 99), s)
      resolve_jot_flags False True False False r b e s scale = (100, b, b + (99 * scale), 1 * scale)
      resolve_jot_flags False False True False r b e s scale = (e / scale, 1 * scale, e, 1 * scale)
      resolve_jot_flags True True True True r b e s scale = (r, b, e, s)

fn :: default_dir Int Int => Int
      default_dir b   e   =
        | e >= b = 1
        | otherwise = 0 - 1

fn :: infer_step Int Int Int => Int
      infer_step r   b   e   =
        | r <= 1 = 0
        | otherwise = (e - b) / (r - 1)

fn :: infer_reps Int Int Int => Int
      infer_reps b   e   s   =
        | s == 0 = 100
        | ((e > b) and (s < 0)) or ((e < b) and (s > 0)) = 0
        | otherwise = ((e - b) / s) + 1

fn :: int_to_hex Int Bool  => String
      int_to_hex 0   upper =  "0"
      int_to_hex n   upper =
        | n < 0 = "-" + (int_to_hex_loop (0 - n) upper "")
        | otherwise = int_to_hex_loop n upper ""

fn :: int_to_hex_loop Int Bool  String => String
      int_to_hex_loop 0   upper acc    =  acc
      int_to_hex_loop n   upper acc    =
        d = n % 16
        int_to_hex_loop (n / 16) upper ((hex_digit d upper) + acc)

fn :: hex_digit Int Bool  => String
      hex_digit d   upper =
        | d < 10 = int_to_str d
        | upper = char_from_code (55 + d)
        | otherwise = char_from_code (87 + d)

fn :: int_to_octal Int => String
      int_to_octal 0   =  "0"
      int_to_octal n   =
        | n < 0 = "-" + (int_to_octal_loop (0 - n) "")
        | otherwise = int_to_octal_loop n ""

fn :: int_to_octal_loop Int String => String
      int_to_octal_loop 0   acc    =  acc
      int_to_octal_loop n   acc    =  int_to_octal_loop (n / 8) ((int_to_str (n % 8)) + acc)

fn :: format_jot_item Int JotSpec => String
      format_jot_item val spec    =
        | j_has_word spec = j_word spec
        | (j_char_mode spec) and ((str_len (j_fmt spec)) > 0) = format_printf_jot (j_fmt spec) val (j_prec spec) True
        | j_char_mode spec = char_from_code val
        | (str_len (j_fmt spec)) > 0 = format_printf_jot (j_fmt spec) val (j_prec spec) False
        | otherwise = format_scaled_int val (j_prec spec)

fn :: format_printf_jot String Int Int  Bool      => String
      format_printf_jot fmt    val prec char_mode =
        | (str_len fmt) == 0 = format_scaled_int val prec
        | not (str_contains fmt "%") = fmt + (format_scaled_int val prec)
        | otherwise = printf_jot_loop fmt 0 (str_len fmt) val prec char_mode ""

fn :: printf_jot_loop String Int Int Int Int Bool String => String
      printf_jot_loop fmt idx len val prec char_mode acc =
        | idx >= len = acc
        | ((str_char_at fmt idx) == 37) and ((idx + 1) < len) and ((str_char_at fmt (idx + 1)) == 37) = printf_jot_loop fmt (idx + 2) len val prec char_mode (acc + "%")
        | ((str_char_at fmt idx) == 37) and ((idx + 1) < len) = printf_parse_spec fmt (idx + 1) len val prec char_mode acc
        | otherwise = printf_jot_loop fmt (idx + 1) len val prec char_mode (acc + (str_slice fmt idx (idx + 1)))

fn :: printf_parse_spec String Int Int Int Int Bool String => String
      printf_parse_spec fmt idx len val prec char_mode acc =
        left_align = (idx < len) and ((str_char_at fmt idx) == 45)
        idx1 = advance_if left_align idx
        zero_pad = (idx1 < len) and ((str_char_at fmt idx1) == 48)
        idx2 = advance_if zero_pad idx1
        width_pair = read_digits_at fmt idx2 len ""
        width_str = fst width_pair
        idx3 = snd width_pair
        prec_pair = read_opt_prec fmt idx3 len prec
        spec_prec = fst prec_pair
        idx4 = snd prec_pair
        conv_code = char_at_or fmt idx4 100
        rendered = render_printf_conv conv_code val prec spec_prec char_mode
        padded = apply_printf_pad rendered (str_to_int width_str) left_align zero_pad
        printf_jot_loop fmt (idx4 + 1) len val prec char_mode (acc + padded)

fn :: advance_if Bool  Int => Int
      advance_if True  idx =  idx + 1
      advance_if False idx =  idx

fn :: read_digits_at String Int Int String => (String, Int)
      read_digits_at s      idx len acc    =
        | idx >= len = (acc, idx)
        | ((str_char_at s idx) >= 48) and ((str_char_at s idx) <= 57) = read_digits_at s (idx + 1) len (acc + (str_slice s idx (idx + 1)))
        | otherwise = (acc, idx)

fn :: read_opt_prec String Int Int Int      => (Int, Int)
      read_opt_prec s      idx len def_prec =
        | (idx < len) and ((str_char_at s idx) == 46) = read_opt_prec_digits s (idx + 1) len
        | otherwise = (def_prec, idx)

fn :: read_opt_prec_digits String Int Int => (Int, Int)
      read_opt_prec_digits s      idx len =
        pair = read_digits_at s idx len ""
        (str_to_int (fst pair), snd pair)

fn :: char_at_or String Int Int      => Int
      char_at_or s      idx fallback =
        | idx >= (str_len s) = fallback
        | otherwise = str_char_at s idx

fn :: render_printf_conv Int Int Int Int Bool => String
      render_printf_conv conv val orig_prec spec_prec char_mode =
        | (conv == 99) or char_mode = char_from_code (val / (pow10 orig_prec))
        | conv == 120 = int_to_hex (val / (pow10 orig_prec)) False
        | conv == 88 = int_to_hex (val / (pow10 orig_prec)) True
        | conv == 111 = int_to_octal (val / (pow10 orig_prec))
        | conv == 102 = format_rescaled val orig_prec spec_prec
        | otherwise = format_scaled_int val orig_prec

fn :: format_rescaled Int Int       Int         => String
      format_rescaled val orig_prec target_prec =
        | target_prec == orig_prec = format_scaled_int val orig_prec
        | target_prec > orig_prec = format_scaled_int (val * (pow10 (target_prec - orig_prec))) target_prec
        | otherwise = format_scaled_int (val / (pow10 (orig_prec - target_prec))) target_prec

fn :: apply_printf_pad String Int   Bool       Bool     => String
      apply_printf_pad s      width left_align zero_pad =
        | width <= 0 = s
        | left_align = pad_right s width
        | zero_pad = pad_zero_signed s width
        | otherwise = pad_left s width

fn :: pad_zero_signed String Int   => String
      pad_zero_signed s      width =
        | (str_len s) >= width = s
        | ((str_len s) > 0) and ((str_char_at s 0) == 45) = "-" + (pad_left_char (str_slice s 1 (str_len s)) (width - 1) "0")
        | otherwise = pad_left_char s width "0"

fn :: gen_jot_items Int Int Int Int JotSpec [String] => [String]
      gen_jot_items idx reps cur step spec acc =
        | idx >= reps = acc
        | otherwise = gen_jot_items (idx + 1) reps (cur + step) step spec (acc :: [format_jot_item cur spec])

fn :: next_prng Int   => Int
      next_prng state =  ((state * 1103515245) + 12345) % 2147483648

fn :: gen_jot_random Int Int Int Int Int JotSpec [String] => [String]
      gen_jot_random idx reps lo hi state spec acc =
        | idx >= reps = acc
        | otherwise =
          st1 = abs_int (next_prng state)
          span = max_int 1 ((hi - lo) + 1)
          val = lo + (st1 % span)
          (gen_jot_random (idx + 1) reps lo hi st1 spec (acc :: [format_jot_item val spec]))

op :: evaluate jot JotSpec => String
      evaluate jot spec    =
        tuple = resolve jot params (j_reps spec) (j_begin spec) (j_end spec) (j_step spec) (j_prec spec)
        reps = tuple_4_1 tuple
        begin_val = tuple_4_2 tuple
        end_val = tuple_4_3 tuple
        step_val = tuple_4_4 tuple
        items = select_jot_items (j_random spec) reps begin_val end_val step_val spec
        join_with items (j_sep spec)

fn :: select_jot_items Bool Int Int Int Int JotSpec => [String]
      select_jot_items False reps begin_val end_val step_val spec = gen_jot_items 0 reps begin_val step_val spec []
      select_jot_items True reps begin_val end_val step_val spec =
        lo = min_int begin_val end_val
        hi = max_int begin_val end_val
        seed = max_int 1 (abs_int step_val)
        gen_jot_random 0 reps lo hi seed spec []

fn :: min_int Int Int => Int
      min_int a   b   =
        | a <= b = a
        | otherwise = b

fn :: tuple_4_1 (A, B, C, D) => A
      tuple_4_1 (a, _, _, _) =  a

fn :: tuple_4_2 (A, B, C, D) => B
      tuple_4_2 (_, b, _, _) =  b

fn :: tuple_4_3 (A, B, C, D) => C
      tuple_4_3 (_, _, c, _) =  c

fn :: tuple_4_4 (A, B, C, D) => D
      tuple_4_4 (_, _, _, d) =  d

fn :: parse_jot_param String Bool      Int  => JotParam
      parse_jot_param s      char_mode prec =
        | s == "-" = Omitted
        | char_mode and ((str_len s) == 1) and (not (is_digits_only s)) = Given (str_char_at s 0)
        | otherwise = Given (parse_scaled_int s prec)

fn :: default_jot_spec => JotSpec
      default_jot_spec =  JotSpec Omitted Omitted Omitted Omitted "" False "" False (char_from_code 10) False False (0 - 1)

op :: parse jot args [String] => JotSpec
      parse jot args args     =  parse_jot_loop args False default_jot_spec []

fn :: is_jot_flag_cluster String => Bool
      is_jot_flag_cluster s      =
        | (str_len s) <= 1 = False
        | (str_char_at s 0) != 45 = False
        | otherwise = is_jot_cluster_loop s 1 (str_len s)

fn :: is_jot_cluster_loop String Int Int => Bool
      is_jot_cluster_loop s      idx len =
        | idx >= len = True
        | ((str_char_at s idx) == 99) or ((str_char_at s idx) == 110) or ((str_char_at s idx) == 114) = is_jot_cluster_loop s (idx + 1) len
        | otherwise = False

fn :: apply_jot_cluster String Int Int JotSpec => JotSpec
      apply_jot_cluster s      idx len spec    =
        | idx >= len = spec
        | (str_char_at s idx) == 99 = apply_jot_cluster s (idx + 1) len (spec with j_char_mode: True)
        | (str_char_at s idx) == 110 = apply_jot_cluster s (idx + 1) len (spec with j_no_newline: True)
        | (str_char_at s idx) == 114 = apply_jot_cluster s (idx + 1) len (spec with j_random: True)
        | otherwise = apply_jot_cluster s (idx + 1) len spec

fn :: parse_jot_loop [String]   Bool      JotSpec [String] => JotSpec
      parse_jot_loop []         stop_opts spec    pos      =  assign_jot_pos pos spec
      parse_jot_loop (arg:rest) True      spec    pos      =  parse_jot_loop rest True spec (pos :: [arg])
      parse_jot_loop (arg:rest) False     spec    pos      =
        | arg == "--" = parse_jot_loop rest True spec pos
        | arg == "-" = parse_jot_loop rest False spec (pos :: [arg])
        | is_numeric_arg arg = parse_jot_loop rest False spec (pos :: [arg])
        | is_jot_flag_cluster arg = parse_jot_loop rest False (apply_jot_cluster arg 1 (str_len arg) spec) pos
        | arg == "-b" = parse_jot_flag_b rest spec pos
        | arg == "-w" = parse_jot_flag_w rest spec pos
        | arg == "-s" = parse_jot_flag_s rest spec pos
        | arg == "-p" = parse_jot_flag_p rest spec pos
        | str_starts_with arg "-b" = parse_jot_inline_b (str_slice arg 2 (str_len arg)) rest spec pos
        | str_starts_with arg "-w" = parse_jot_inline_w (str_slice arg 2 (str_len arg)) rest spec pos
        | str_starts_with arg "-s" = parse_jot_inline_s (str_slice arg 2 (str_len arg)) rest spec pos
        | str_starts_with arg "-p" = parse_jot_inline_p (str_slice arg 2 (str_len arg)) rest spec pos
        | otherwise = parse_jot_loop rest False spec (pos :: [arg])

fn :: parse_jot_flag_b [String]    JotSpec [String] => JotSpec
      parse_jot_flag_b []          spec    pos      =  assign_jot_pos pos spec
      parse_jot_flag_b (word:rest) spec    pos      =  parse_jot_inline_b word rest spec pos

fn :: parse_jot_inline_b String [String] JotSpec [String] => JotSpec
      parse_jot_inline_b word   rest     spec    pos      =
        parse_jot_loop rest False (spec with j_word: word, j_has_word: True) pos

fn :: parse_jot_flag_w [String]   JotSpec [String] => JotSpec
      parse_jot_flag_w []         spec    pos      =  assign_jot_pos pos spec
      parse_jot_flag_w (fmt:rest) spec    pos      =  parse_jot_inline_w fmt rest spec pos

fn :: parse_jot_inline_w String [String] JotSpec [String] => JotSpec
      parse_jot_inline_w fmt    rest     spec    pos      =
        parse_jot_loop rest False (spec with j_fmt: fmt) pos

fn :: parse_jot_flag_s [String]   JotSpec [String] => JotSpec
      parse_jot_flag_s []         spec    pos      =  assign_jot_pos pos spec
      parse_jot_flag_s (sep:rest) spec    pos      =  parse_jot_inline_s sep rest spec pos

fn :: parse_jot_inline_s String [String] JotSpec [String] => JotSpec
      parse_jot_inline_s sep    rest     spec    pos      =
        parse_jot_loop rest False (spec with j_sep: sep) pos

fn :: parse_jot_flag_p [String] JotSpec [String] => JotSpec
      parse_jot_flag_p []       spec    pos      =  assign_jot_pos pos spec
      parse_jot_flag_p (p:rest) spec    pos      =  parse_jot_inline_p p rest spec pos

fn :: parse_jot_inline_p String [String] JotSpec [String] => JotSpec
      parse_jot_inline_p p      rest     spec    pos      =
        parse_jot_loop rest False (spec with j_prec: str_to_int p) pos

fn :: extract_fmt_prec String Int Int => Int
      extract_fmt_prec fmt    idx len =
        | (idx + 2) >= len = 0
        | ((str_char_at fmt idx) == 37) and ((str_char_at fmt (idx + 1)) == 46) = str_to_int (fst (read_digits_at fmt (idx + 2) len ""))
        | otherwise = extract_fmt_prec fmt (idx + 1) len

fn :: infer_pos_prec [String] Int  => Int
      infer_pos_prec []       best =  best
      infer_pos_prec (x:xs)   best =  infer_pos_prec xs (max_int best (decimal_places x))

fn :: resolve_jot_prec [String] Int           String => Int
      resolve_jot_prec pos      explicit_prec fmt    =
        | explicit_prec >= 0 = explicit_prec
        | otherwise = max_int (infer_pos_prec pos 0) (extract_fmt_prec fmt 0 (str_len fmt))

fn :: assign_jot_pos [String]           JotSpec => JotSpec
      assign_jot_pos []                 spec    =
        spec with j_prec: max_int 0 (j_prec spec)
      assign_jot_pos (p1:[])            spec    =
        pr = resolve_jot_prec [p1] (j_prec spec) (j_fmt spec)
        spec with j_reps: parse_jot_param p1 False 0, j_prec: pr
      assign_jot_pos (p1:p2:[])         spec    =
        pr = resolve_jot_prec [p2] (j_prec spec) (j_fmt spec)
        spec with j_reps: parse_jot_param p1 False 0, j_begin: parse_jot_param p2 (j_char_mode spec) pr, j_prec: pr
      assign_jot_pos (p1:p2:p3:[])      spec    =
        pr = resolve_jot_prec [p2, p3] (j_prec spec) (j_fmt spec)
        spec with j_reps: parse_jot_param p1 False 0, j_begin: parse_jot_param p2 (j_char_mode spec) pr, j_end: parse_jot_param p3 (j_char_mode spec) pr, j_prec: pr
      assign_jot_pos (p1:p2:p3:p4:rest) spec    =
        pr = resolve_jot_prec [p2, p3, p4] (j_prec spec) (j_fmt spec)
        step_pr = select_step_prec (j_random spec) pr
        spec with j_reps: parse_jot_param p1 False 0, j_begin: parse_jot_param p2 (j_char_mode spec) pr, j_end: parse_jot_param p3 (j_char_mode spec) pr, j_step: parse_jot_param p4 False step_pr, j_prec: pr

fn :: select_step_prec Bool  Int => Int
      select_step_prec True  pr  =  0
      select_step_prec False pr  =  pr

op :: run jot [String] => String
      run jot args     =  evaluate jot (parse jot args args)

-- ============================================================================
-- 2. BSD rs(1) — FR-FILTER-001
-- ============================================================================

fn :: default_rs_spec => RsSpec
      default_rs_spec =  RsSpec 0 0 False "" "" False 2 False 0 False 0 False False False []

op :: parse rs args [String] => RsSpec
      parse rs args args     =  parse_rs_loop args default_rs_spec []

fn :: is_rs_flag_cluster String => Bool
      is_rs_flag_cluster s      =
        | (str_len s) <= 1 = False
        | (str_char_at s 0) != 45 = False
        | otherwise = is_rs_cluster_loop s 1 (str_len s)

fn :: is_rs_cluster_loop String Int Int => Bool
      is_rs_cluster_loop s      idx len =
        | idx >= len = True
        | ((str_char_at s idx) == 116) or ((str_char_at s idx) == 106) or ((str_char_at s idx) == 101) or ((str_char_at s idx) == 121) or ((str_char_at s idx) == 104) or ((str_char_at s idx) == 72) or ((str_char_at s idx) == 122) = is_rs_cluster_loop s (idx + 1) len
        | otherwise = False

fn :: apply_rs_cluster String Int Int RsSpec => RsSpec
      apply_rs_cluster s      idx len spec   =
        | idx >= len = spec
        | (str_char_at s idx) == 116 = apply_rs_cluster s (idx + 1) len (spec with r_transpose: True)
        | (str_char_at s idx) == 106 = apply_rs_cluster s (idx + 1) len (spec with r_right_align: True)
        | (str_char_at s idx) == 101 = apply_rs_cluster s (idx + 1) len (spec with r_line_entries: True)
        | (str_char_at s idx) == 121 = apply_rs_cluster s (idx + 1) len (spec with r_recycle: True)
        | ((str_char_at s idx) == 104) or ((str_char_at s idx) == 72) = apply_rs_cluster s (idx + 1) len (spec with r_shape_only: True)
        | (str_char_at s idx) == 122 = apply_rs_cluster s (idx + 1) len (spec with r_squeeze: True)
        | otherwise = apply_rs_cluster s (idx + 1) len spec

fn :: parse_rs_loop [String]   RsSpec [String] => RsSpec
      parse_rs_loop []         spec   pos      =  assign_rs_pos pos spec
      parse_rs_loop (arg:rest) spec   pos      =
        | is_rs_flag_cluster arg = parse_rs_loop rest (apply_rs_cluster arg 1 (str_len arg) spec) pos
        | (arg == "-c") or (arg == "-s") = parse_rs_flag_c rest spec pos
        | (arg == "-C") or (arg == "-S") = parse_rs_flag_cap_c rest spec pos
        | arg == "-g" = parse_rs_flag_g rest spec pos
        | arg == "-w" = parse_rs_flag_w rest spec pos
        | arg == "-k" = parse_rs_flag_k rest spec pos
        | (str_starts_with arg "-c") or (str_starts_with arg "-s") = parse_rs_inline_c (str_slice arg 2 (str_len arg)) rest spec pos
        | (str_starts_with arg "-C") or (str_starts_with arg "-S") = parse_rs_inline_cap_c (str_slice arg 2 (str_len arg)) rest spec pos
        | str_starts_with arg "-g" = parse_rs_inline_g (str_slice arg 2 (str_len arg)) rest spec pos
        | str_starts_with arg "-w" = parse_rs_inline_w (str_slice arg 2 (str_len arg)) rest spec pos
        | str_starts_with arg "-k" = parse_rs_inline_k (str_slice arg 2 (str_len arg)) rest spec pos
        | otherwise = parse_rs_loop rest spec (pos :: [arg])

fn :: parse_rs_flag_c [String]   RsSpec [String] => RsSpec
      parse_rs_flag_c []         spec   pos      =  parse_rs_inline_c (char_from_code 9) [] spec pos
      parse_rs_flag_c (sep:rest) spec   pos      =  parse_rs_inline_c sep rest spec pos

fn :: parse_rs_flag_cap_c [String]   RsSpec [String] => RsSpec
      parse_rs_flag_cap_c []         spec   pos      =  parse_rs_inline_cap_c (char_from_code 9) [] spec pos
      parse_rs_flag_cap_c (sep:rest) spec   pos      =  parse_rs_inline_cap_c sep rest spec pos

fn :: parse_rs_flag_g [String] RsSpec [String] => RsSpec
      parse_rs_flag_g []       spec   pos      =  assign_rs_pos pos spec
      parse_rs_flag_g (g:rest) spec   pos      =  parse_rs_inline_g g rest spec pos

fn :: parse_rs_inline_g String [String] RsSpec [String] => RsSpec
      parse_rs_inline_g g      rest     spec   pos      =
        parse_rs_loop rest (spec with r_gutter: str_to_int g) pos

fn :: parse_rs_flag_w [String] RsSpec [String] => RsSpec
      parse_rs_flag_w []       spec   pos      =  assign_rs_pos pos spec
      parse_rs_flag_w (w:rest) spec   pos      =  parse_rs_inline_w w rest spec pos

fn :: parse_rs_inline_w String [String] RsSpec [String] => RsSpec
      parse_rs_inline_w w      rest     spec   pos      =
        parse_rs_loop rest (spec with r_width: str_to_int w) pos

fn :: parse_rs_flag_k [String] RsSpec [String] => RsSpec
      parse_rs_flag_k []       spec   pos      =  assign_rs_pos pos spec
      parse_rs_flag_k (k:rest) spec   pos      =  parse_rs_inline_k k rest spec pos

fn :: parse_rs_inline_k String [String] RsSpec [String] => RsSpec
      parse_rs_inline_k k      rest     spec   pos      =
        parse_rs_loop rest (spec with r_skip_lines: str_to_int k) pos

fn :: parse_rs_inline_c String [String] RsSpec [String] => RsSpec
      parse_rs_inline_c sep    rest     spec   pos      =
        parse_rs_loop rest (spec with r_in_sep: sep) pos

fn :: parse_rs_inline_cap_c String [String] RsSpec [String] => RsSpec
      parse_rs_inline_cap_c sep    rest     spec   pos      =
        parse_rs_loop rest (spec with r_out_sep: sep, r_has_out_sep: True) pos

fn :: assign_rs_pos [String]     RsSpec => RsSpec
      assign_rs_pos []           spec   =  spec
      assign_rs_pos (p1:[])      spec   =
        | is_digits_only p1 = spec with r_rows: str_to_int p1, r_cols: 0, r_files: []
        | otherwise = spec with r_files: [p1]
      assign_rs_pos (p1:p2:rest) spec   =
        | (is_digits_only p1) and (is_digits_only p2) = spec with r_rows: str_to_int p1, r_cols: str_to_int p2, r_files: rest
        | is_digits_only p1 = spec with r_rows: str_to_int p1, r_cols: 0, r_files: [p2] :: rest
        | otherwise = spec with r_files: [p1, p2] :: rest

fn :: ceil_div Int Int => Int
      ceil_div n   d   =
        | d <= 0 = 1
        | n <= 0 = 0
        | otherwise = ((n + d - 1) / d)

fn :: max_item_len [String] Int  => Int
      max_item_len []       best =  best
      max_item_len (x:xs)   best =  max_item_len xs (max_int best (str_len x))

fn :: infer_rs_dims [String] Int Int Int Int Int => (Int, Int)
      infer_rs_dims items total r c width gutter =
        | total <= 0 = (0, 0)
        | (r > 0) and (c > 0) = (r, c)
        | (r <= 0) and (c > 0) = (ceil_div total c, c)
        | (r > 0) and (c <= 0) = (r, ceil_div total r)
        | width > 0 = infer_rs_width_dims items total width gutter
        | otherwise = (1, total)

fn :: infer_rs_width_dims [String] Int   Int   Int    => (Int, Int)
      infer_rs_width_dims items    total width gutter =
        m_len = max_int 1 (max_item_len items 0)
        fit_cols = max_int 1 ((width + gutter) / (m_len + gutter))
        cols = min_int total fit_cols
        (ceil_div total cols, cols)

fn :: grid_cell_index Int Int Int Int Bool => Int
      grid_cell_index orig_rows orig_cols r c False = (r * orig_cols) + c
      grid_cell_index orig_rows orig_cols r c True = (c * orig_cols) + r

fn :: grid_cell [String] Int Int Int Int Int Bool Bool => String
      grid_cell items total orig_rows orig_cols r c trans recycle =
        idx = grid_cell_index orig_rows orig_cols r c trans
        resolve_grid_item items total idx recycle

fn :: resolve_grid_item [String] Int   Int Bool  => String
      resolve_grid_item items    total idx False =  str_list_nth items idx
      resolve_grid_item items    total idx True  =
        | total <= 0 = ""
        | otherwise = str_list_nth items (idx % total)

fn :: max_col_width [String] Int Int Int Int Int Int Bool Bool Int => Int
      max_col_width items total orig_rows orig_cols out_rows c row_idx trans recycle best =
        | row_idx >= out_rows = best
        | otherwise =
          cell = grid_cell items total orig_rows orig_cols row_idx c trans recycle
          w = str_len cell
          next_best = max_int best w
          (max_col_width items total orig_rows orig_cols out_rows c (row_idx + 1) trans recycle next_best)

fn :: compute_col_widths [String] Int Int Int Int Int Int Bool Bool [Int] => [Int]
      compute_col_widths items total orig_rows orig_cols out_rows out_cols c trans recycle acc =
        | c >= out_cols = acc
        | otherwise =
          w = max_col_width items total orig_rows orig_cols out_rows c 0 trans recycle 0
          (compute_col_widths items total orig_rows orig_cols out_rows out_cols (c + 1) trans recycle (acc :: [w]))

fn :: row_active_cols Int Int Int Int Int Bool Bool Int Int => Int
      row_active_cols total orig_rows orig_cols r out_cols trans True c last_valid = out_cols
      row_active_cols total orig_rows orig_cols r out_cols trans False c last_valid =
        | c >= out_cols = last_valid + 1
        | (grid_cell_index orig_rows orig_cols r c trans) < total = row_active_cols total orig_rows orig_cols r out_cols trans False (c + 1) c
        | otherwise = row_active_cols total orig_rows orig_cols r out_cols trans False (c + 1) last_valid

fn :: format_rs_cell String Int Bool Bool Bool => String
      format_rs_cell cell width right_align squeeze is_last =
        | squeeze = cell
        | right_align = pad_left cell width
        | is_last = cell
        | otherwise = pad_right cell width

fn :: render_rs_row [String] Int Int Int Int Int Bool [Int] RsSpec Int [String] => String
      render_rs_row items total orig_rows orig_cols r active_cols trans widths spec c acc =
        | c >= active_cols = join_rs_cells acc spec
        | otherwise =
          raw_cell = grid_cell items total orig_rows orig_cols r c trans (r_recycle spec)
          col_w = int_list_nth widths c
          is_last = (c + 1) >= active_cols
          formatted = format_rs_cell raw_cell col_w (r_right_align spec) (r_squeeze spec) (is_last or (r_has_out_sep spec))
          (render_rs_row items total orig_rows orig_cols r active_cols trans widths spec (c + 1) (acc :: [formatted]))

fn :: join_rs_cells [String] RsSpec => String
      join_rs_cells cells    spec   =
        | r_has_out_sep spec = join_with cells (r_out_sep spec)
        | otherwise = join_with cells (repeat_str " " (r_gutter spec))

fn :: render_rs_rows [String] Int Int Int Int Int Int Bool [Int] RsSpec [String] => [String]
      render_rs_rows items total orig_rows orig_cols out_rows out_cols r trans widths spec acc =
        | r >= out_rows = acc
        | otherwise =
          active_cols = row_active_cols total orig_rows orig_cols r out_cols trans (r_recycle spec) 0 (0 - 1)
          row_str = render_rs_row items total orig_rows orig_cols r active_cols trans widths spec 0 []
          (render_rs_rows items total orig_rows orig_cols out_rows out_cols (r + 1) trans widths spec (acc :: [row_str]))

fn :: preprocess_rs_lines String Int    => String
      preprocess_rs_lines raw    skip_n =
        | skip_n <= 0 = raw
        | otherwise = join_with (drop_str_list (split lines raw) skip_n) (char_from_code 10)

fn :: tokenize_rs_input String RsSpec => [String]
      tokenize_rs_input raw    spec   =
        trimmed = preprocess_rs_lines raw (r_skip_lines spec)
        tokenize_rs_trimmed trimmed spec

fn :: tokenize_rs_trimmed String  RsSpec => [String]
      tokenize_rs_trimmed trimmed spec   =
        | r_line_entries spec = split lines trimmed
        | (str_len (r_in_sep spec)) > 0 = split_char_loop trimmed 0 (str_len trimmed) (str_char_at (r_in_sep spec) 0) "" []
        | otherwise = split whitespace trimmed

op :: reshape text String with RsSpec => String
      reshape text raw    with spec   =
        items = tokenize_rs_input raw spec
        total = list_len items
        dims = infer_rs_dims items total (r_rows spec) (r_cols spec) (r_width spec) (r_gutter spec)
        orig_rows = fst dims
        orig_cols = snd dims
        out_dims = select_out_dims orig_rows orig_cols (r_transpose spec)
        out_rows = fst out_dims
        out_cols = snd out_dims
        render_rs_or_shape items total orig_rows orig_cols out_rows out_cols spec

fn :: render_rs_or_shape [String] Int Int Int Int Int RsSpec => String
      render_rs_or_shape items total orig_rows orig_cols out_rows out_cols spec =
        | r_shape_only spec = (int_to_str out_rows) + " by " + (int_to_str out_cols)
        | otherwise = render_rs_full_grid items total orig_rows orig_cols out_rows out_cols spec

fn :: render_rs_full_grid [String] Int Int Int Int Int RsSpec => String
      render_rs_full_grid items total orig_rows orig_cols out_rows out_cols spec =
        widths = compute_col_widths items total orig_rows orig_cols out_rows out_cols 0 (r_transpose spec) (r_recycle spec) []
        rows = render_rs_rows items total orig_rows orig_cols out_rows out_cols 0 (r_transpose spec) widths spec []
        join_with rows (char_from_code 10)

fn :: select_out_dims Int Int Bool  => (Int, Int)
      select_out_dims r   c   False =  (r, c)
      select_out_dims r   c   True  =  (c, r)

-- ============================================================================
-- 3. BSD lam(1) — FR-FILTER-002
-- ============================================================================

fn :: parse_min_max String => (Int, Int)
      parse_min_max spec   =  parse_min_max_loop spec 0 (str_len spec) ""

fn :: parse_min_max_loop String Int Int String => (Int, Int)
      parse_min_max_loop s      idx len left   =
        | idx >= len = (parse_opt_int left, 0)
        | (str_char_at s idx) == 46 = (parse_opt_int left, parse_opt_int (str_slice s (idx + 1) len))
        | otherwise = parse_min_max_loop s (idx + 1) len (left + (str_slice s idx (idx + 1)))

fn :: parse_opt_int String => Int
      parse_opt_int s      =
        | (str_len s) == 0 = 0
        | s == "-" = 0
        | otherwise = parse_signed_int s

fn :: format_lam_cell String Int   Int   => String
      format_lam_cell s      min_w max_w =
        truncated = truncate_lam s max_w
        pad_lam truncated min_w

fn :: truncate_lam String Int   => String
      truncate_lam s      max_w =
        | (max_w > 0) and ((str_len s) > max_w) = str_slice s 0 max_w
        | otherwise = s

fn :: pad_lam String Int   => String
      pad_lam s      min_w =
        | min_w > 0 = pad_left s min_w
        | min_w < 0 = pad_right s (0 - min_w)
        | otherwise = s

fn :: max_lam_lines [LamCol]   Int  => Int
      max_lam_lines []         best =  best
      max_lam_lines (col:rest) best =
        n = list_len (l_lines col)
        max_lam_lines rest (max_int best n)

fn :: render_lam_line [LamCol]   Int     String    => String
      render_lam_line []         row_idx trail_sep =  trail_sep
      render_lam_line (col:rest) row_idx trail_sep =
        in_bounds = row_idx < (list_len (l_lines col))
        raw_line = str_list_nth (l_lines col) row_idx
        cell_body = select_lam_cell_body in_bounds (l_pad_eof col) raw_line (l_min col) (l_max col)
        cell = (l_sep col) + cell_body
        cell + (render_lam_line rest row_idx trail_sep)

fn :: select_lam_cell_body Bool Bool String Int Int => String
      select_lam_cell_body True pad_eof raw_line min_w max_w = format_lam_cell raw_line min_w max_w
      select_lam_cell_body False True raw_line min_w max_w = format_lam_cell "" min_w max_w
      select_lam_cell_body False False raw_line min_w max_w = format_lam_cell "" min_w max_w

fn :: render_lam_all [LamCol] Int Int String [String] => [String]
      render_lam_all cols row_idx total_rows trail_sep acc =
        | row_idx >= total_rows = acc
        | otherwise =
          line = render_lam_line cols row_idx trail_sep
          (render_lam_all cols (row_idx + 1) total_rows trail_sep (acc :: [line]))

op :: laminate columns [LamCol] with String    => String
      laminate columns cols     with trail_sep =
        total_rows = max_lam_lines cols 0
        lines = render_lam_all cols 0 total_rows trail_sep []
        join_with lines (char_from_code 10)

op :: parse lam args [String] => ([LamCol], String)
      parse lam args args     =  parse_lam_loop args "" "" 0 0 0 0 False False 10 10 []

fn :: parse_lam_loop [String] String String Int Int Int Int Bool Bool Int Int [LamCol] => ([LamCol], String)
      parse_lam_loop [] cur_s def_s cur_min cur_max def_min def_max cur_p def_p cur_t def_t acc = (acc, select_nonempty cur_s def_s)
      parse_lam_loop (arg:rest) cur_s def_s cur_min cur_max def_min def_max cur_p def_p cur_t def_t acc =
        | arg == "-s" = parse_lam_flag_s rest False cur_s def_s cur_min cur_max def_min def_max cur_p def_p cur_t def_t acc
        | arg == "-S" = parse_lam_flag_s rest True cur_s def_s cur_min cur_max def_min def_max cur_p def_p cur_t def_t acc
        | arg == "-f" = parse_lam_flag_f rest False False cur_s def_s cur_min cur_max def_min def_max cur_p def_p cur_t def_t acc
        | arg == "-F" = parse_lam_flag_f rest True False cur_s def_s cur_min cur_max def_min def_max cur_p def_p cur_t def_t acc
        | arg == "-p" = parse_lam_flag_f rest False True cur_s def_s cur_min cur_max def_min def_max cur_p def_p cur_t def_t acc
        | arg == "-P" = parse_lam_flag_f rest True True cur_s def_s cur_min cur_max def_min def_max cur_p def_p cur_t def_t acc
        | arg == "-t" = parse_lam_flag_t rest False cur_s def_s cur_min cur_max def_min def_max cur_p def_p cur_t def_t acc
        | arg == "-T" = parse_lam_flag_t rest True cur_s def_s cur_min cur_max def_min def_max cur_p def_p cur_t def_t acc
        | str_starts_with arg "-s" = parse_lam_loop rest (str_slice arg 2 (str_len arg)) def_s cur_min cur_max def_min def_max cur_p def_p cur_t def_t acc
        | str_starts_with arg "-S" = parse_lam_loop rest (str_slice arg 2 (str_len arg)) (str_slice arg 2 (str_len arg)) cur_min cur_max def_min def_max cur_p def_p cur_t def_t acc
        | str_starts_with arg "-f" = parse_lam_inline_f (str_slice arg 2 (str_len arg)) rest False False cur_s def_s cur_min cur_max def_min def_max cur_p def_p cur_t def_t acc
        | str_starts_with arg "-F" = parse_lam_inline_f (str_slice arg 2 (str_len arg)) rest True False cur_s def_s cur_min cur_max def_min def_max cur_p def_p cur_t def_t acc
        | str_starts_with arg "-p" = parse_lam_inline_f (str_slice arg 2 (str_len arg)) rest False True cur_s def_s cur_min cur_max def_min def_max cur_p def_p cur_t def_t acc
        | str_starts_with arg "-P" = parse_lam_inline_f (str_slice arg 2 (str_len arg)) rest True True cur_s def_s cur_min cur_max def_min def_max cur_p def_p cur_t def_t acc
        | str_starts_with arg "-t" = parse_lam_inline_t (str_slice arg 2 (str_len arg)) rest False cur_s def_s cur_min cur_max def_min def_max cur_p def_p cur_t def_t acc
        | str_starts_with arg "-T" = parse_lam_inline_t (str_slice arg 2 (str_len arg)) rest True cur_s def_s cur_min cur_max def_min def_max cur_p def_p cur_t def_t acc
        | otherwise = parse_lam_file arg rest cur_s def_s cur_min cur_max def_min def_max cur_p def_p cur_t def_t acc

fn :: select_nonempty String String => String
      select_nonempty a      b      =
        | (str_len a) > 0 = a
        | otherwise = b

fn :: parse_lam_flag_s [String] Bool String String Int Int Int Int Bool Bool Int Int [LamCol] => ([LamCol], String)
      parse_lam_flag_s [] persist cur_s def_s cur_min cur_max def_min def_max cur_p def_p cur_t def_t acc = (acc, select_nonempty cur_s def_s)
      parse_lam_flag_s (sep:rest) False cur_s def_s cur_min cur_max def_min def_max cur_p def_p cur_t def_t acc =
        parse_lam_loop rest sep def_s cur_min cur_max def_min def_max cur_p def_p cur_t def_t acc
      parse_lam_flag_s (sep:rest) True cur_s def_s cur_min cur_max def_min def_max cur_p def_p cur_t def_t acc =
        parse_lam_loop rest sep sep cur_min cur_max def_min def_max cur_p def_p cur_t def_t acc

fn :: parse_lam_flag_f [String] Bool Bool String String Int Int Int Int Bool Bool Int Int [LamCol] => ([LamCol], String)
      parse_lam_flag_f [] persist pad_eof cur_s def_s cur_min cur_max def_min def_max cur_p def_p cur_t def_t acc = (acc, select_nonempty cur_s def_s)
      parse_lam_flag_f (mm:rest) persist pad_eof cur_s def_s cur_min cur_max def_min def_max cur_p def_p cur_t def_t acc =
        parse_lam_inline_f mm rest persist pad_eof cur_s def_s cur_min cur_max def_min def_max cur_p def_p cur_t def_t acc

fn :: parse_lam_inline_f String [String] Bool Bool String String Int Int Int Int Bool Bool Int Int [LamCol] => ([LamCol], String)
      parse_lam_inline_f mm rest False pad_eof cur_s def_s cur_min cur_max def_min def_max cur_p def_p cur_t def_t acc =
        bounds = parse_min_max mm
        parse_lam_loop rest cur_s def_s (fst bounds) (snd bounds) def_min def_max pad_eof def_p cur_t def_t acc
      parse_lam_inline_f mm rest True pad_eof cur_s def_s cur_min cur_max def_min def_max cur_p def_p cur_t def_t acc =
        bounds = parse_min_max mm
        b1 = fst bounds
        b2 = snd bounds
        parse_lam_loop rest cur_s def_s b1 b2 b1 b2 pad_eof pad_eof cur_t def_t acc

fn :: parse_lam_flag_t [String] Bool String String Int Int Int Int Bool Bool Int Int [LamCol] => ([LamCol], String)
      parse_lam_flag_t [] persist cur_s def_s cur_min cur_max def_min def_max cur_p def_p cur_t def_t acc = (acc, select_nonempty cur_s def_s)
      parse_lam_flag_t (tc:rest) persist cur_s def_s cur_min cur_max def_min def_max cur_p def_p cur_t def_t acc =
        parse_lam_inline_t tc rest persist cur_s def_s cur_min cur_max def_min def_max cur_p def_p cur_t def_t acc

fn :: parse_lam_inline_t String [String] Bool String String Int Int Int Int Bool Bool Int Int [LamCol] => ([LamCol], String)
      parse_lam_inline_t tc rest False cur_s def_s cur_min cur_max def_min def_max cur_p def_p cur_t def_t acc =
        code = char_at_or tc 0 10
        parse_lam_loop rest cur_s def_s cur_min cur_max def_min def_max cur_p def_p code def_t acc
      parse_lam_inline_t tc rest True cur_s def_s cur_min cur_max def_min def_max cur_p def_p cur_t def_t acc =
        code = char_at_or tc 0 10
        parse_lam_loop rest cur_s def_s cur_min cur_max def_min def_max cur_p def_p code code acc

fn :: resolve_lam_path String => String
      resolve_lam_path path   =
        | path == "-" = "/dev/stdin"
        | otherwise = path

fn :: parse_lam_file String [String] String String Int Int Int Int Bool Bool Int Int [LamCol] => ([LamCol], String)
      parse_lam_file path rest cur_s def_s cur_min cur_max def_min def_max cur_p def_p cur_t def_t acc =
        content = read_file (resolve_lam_path path)
        lines = split_lines_delim content 0 (str_len content) cur_t "" []
        col = LamCol cur_s cur_min cur_max cur_p lines
        parse_lam_loop rest def_s def_s def_min def_max def_min def_max def_p def_p def_t def_t (acc :: [col])

-- ============================================================================
-- 4. BSD vis(1) & unvis(1) — FR-FILTER-002
-- ============================================================================

fn :: default_vis_spec => VisSpec
      default_vis_spec =  VisSpec False True False False False False False False 0 []

op :: parse vis args [String] => VisSpec
      parse vis args args     =  parse_vis_loop args default_vis_spec

fn :: apply_vis_flag_char Int VisSpec => VisSpec
      apply_vis_flag_char c   spec    =
        | c == 111 = spec with v_octal: True
        | c == 99 = spec with v_cstyle: True
        | c == 116 = spec with v_tab: True
        | c == 119 = spec with v_tab: True, v_space: True, v_newline: True
        | c == 115 = spec with v_space: True
        | c == 98 = spec with v_nobs: True
        | c == 108 = spec with v_mark_nl: True
        | c == 110 = spec with v_none: True
        | c == 102 = spec with v_fold: 80
        | otherwise = spec

fn :: apply_vis_flags_str String Int Int VisSpec => VisSpec
      apply_vis_flags_str s      idx len spec    =
        | idx >= len = spec
        | otherwise = apply_vis_flags_str s (idx + 1) len (apply_vis_flag_char (str_char_at s idx) spec)

fn :: parse_vis_loop [String]   VisSpec => VisSpec
      parse_vis_loop []         spec    =  spec
      parse_vis_loop (arg:rest) spec    =
        | arg == "-F" = parse_vis_flag_fold rest spec
        | (str_starts_with arg "-F") and ((str_len arg) > 2) = parse_vis_inline_fold (str_slice arg 2 (str_len arg)) rest spec
        | (str_starts_with arg "-") and ((str_len arg) > 1) = parse_vis_loop rest (apply_vis_flags_str arg 1 (str_len arg) spec)
        | otherwise = parse_vis_loop rest (spec with v_files: (v_files spec) :: [arg])

fn :: parse_vis_flag_fold [String] VisSpec => VisSpec
      parse_vis_flag_fold []       spec    =  spec
      parse_vis_flag_fold (w:rest) spec    =  parse_vis_inline_fold w rest spec

fn :: parse_vis_inline_fold String [String] VisSpec => VisSpec
      parse_vis_inline_fold w      rest     spec    =
        parse_vis_loop rest (spec with v_fold: str_to_int w)

fn :: bslash => String
      bslash =  char_from_code 92

fn :: vis_prefix VisSpec => String
      vis_prefix spec    =
        | v_nobs spec = ""
        | otherwise = bslash

fn :: to_octal_3 Int VisSpec => String
      to_octal_3 c   spec    =
        d2 = (c / 64) % 8
        d1 = (c / 8) % 8
        d0 = c % 8
        (vis_prefix spec) + (int_to_str d2) + (int_to_str d1) + (int_to_str d0)

fn :: encode_vis_code Int VisSpec => String
      encode_vis_code c   spec    =
        | (c == 92) and (v_nobs spec) = bslash
        | c == 92 = bslash + bslash
        | v_none spec = char_from_code c
        | (c == 9) and (not (v_tab spec)) = char_from_code 9
        | (c == 9) and (v_octal spec) = to_octal_3 9 spec
        | c == 9 = (vis_prefix spec) + "t"
        | (c == 10) and (v_mark_nl spec) and (not (v_newline spec)) = (vis_prefix spec) + "$" + (char_from_code 10)
        | (c == 10) and (not (v_newline spec)) = char_from_code 10
        | (c == 10) and (v_octal spec) = to_octal_3 10 spec
        | c == 10 = (vis_prefix spec) + "n"
        | (c == 32) and (not (v_space spec)) = " "
        | (c == 32) and (v_octal spec) = to_octal_3 32 spec
        | c == 32 = (vis_prefix spec) + "s"
        | (c == 13) and (v_octal spec) = to_octal_3 13 spec
        | c == 13 = (vis_prefix spec) + "r"
        | (c == 8) and (v_octal spec) = to_octal_3 8 spec
        | c == 8 = (vis_prefix spec) + "b"
        | (c == 7) and (v_octal spec) = to_octal_3 7 spec
        | c == 7 = (vis_prefix spec) + "a"
        | (c == 11) and (v_octal spec) = to_octal_3 11 spec
        | c == 11 = (vis_prefix spec) + "v"
        | (c == 12) and (v_octal spec) = to_octal_3 12 spec
        | c == 12 = (vis_prefix spec) + "f"
        | (c == 127) and (v_octal spec) = to_octal_3 127 spec
        | c == 127 = (vis_prefix spec) + "^?"
        | (c >= 0) and (c < 32) and (v_octal spec) = to_octal_3 c spec
        | (c >= 0) and (c < 32) = (vis_prefix spec) + "^" + (char_from_code (c + 64))
        | c >= 128 = to_octal_3 c spec
        | otherwise = char_from_code c

fn :: vis_encode_loop String Int Int VisSpec String => String
      vis_encode_loop s idx len spec acc =
        | idx >= len = acc
        | otherwise =
          c = str_char_at s idx
          (vis_encode_loop s (idx + 1) len spec (acc + (encode_vis_code c spec)))

fn :: fold_vis_lines String Int Int Int Int String => String
      fold_vis_lines s idx len max_col cur_col acc =
        | idx >= len = acc
        | (str_char_at s idx) == 10 = fold_vis_lines s (idx + 1) len max_col 0 (acc + (char_from_code 10))
        | (cur_col + 1) >= max_col = fold_vis_lines s idx len max_col 0 (acc + bslash + (char_from_code 10))
        | otherwise = fold_vis_lines s (idx + 1) len max_col (cur_col + 1) (acc + (str_slice s idx (idx + 1)))

op :: vis encode String with VisSpec => String
      vis encode s      with spec    =
        encoded = vis_encode_loop s 0 (str_len s) spec ""
        apply_vis_fold encoded (v_fold spec)

fn :: apply_vis_fold String  Int    => String
      apply_vis_fold encoded fold_w =
        | fold_w <= 1 = encoded
        | otherwise = fold_vis_lines encoded 0 (str_len encoded) fold_w 0 ""

fn :: is_octal_digit Int => Bool
      is_octal_digit c   =  (c >= 48) and (c <= 55)

fn :: hex_val Int => Int
      hex_val c   =
        | (c >= 48) and (c <= 57) = c - 48
        | (c >= 65) and (c <= 70) = c - 55
        | (c >= 97) and (c <= 102) = c - 87
        | otherwise = 0 - 1

fn :: is_hex_digit Int => Bool
      is_hex_digit c   =  (hex_val c) >= 0

fn :: decode_c_escape Int => String
      decode_c_escape c   =
        | c == 110 = char_from_code 10
        | c == 116 = char_from_code 9
        | c == 114 = char_from_code 13
        | c == 98 = char_from_code 8
        | c == 97 = char_from_code 7
        | c == 118 = char_from_code 11
        | c == 102 = char_from_code 12
        | c == 115 = " "
        | c == 69 = char_from_code 27
        | c == 92 = char_from_code 92
        | c == 36 = ""
        | c == 10 = ""
        | otherwise = char_from_code c

fn :: decode_ctrl_code Int => Int
      decode_ctrl_code c   =
        | c == 63 = 127
        | otherwise = c - 64

fn :: decode_ctrl_char Int => String
      decode_ctrl_char c   =  char_from_code (decode_ctrl_code c)

fn :: unvis_loop String Int Int String => String
      unvis_loop s      idx len acc    =
        | idx >= len = acc
        | ((str_char_at s idx) == 92) and ((idx + 1) < len) = unvis_after_bslash s idx len acc
        | otherwise = unvis_loop s (idx + 1) len (acc + (str_slice s idx (idx + 1)))

fn :: unvis_after_bslash String Int Int String => String
      unvis_after_bslash s      idx len acc    =
        | ((str_char_at s (idx + 1)) == 94) and ((idx + 2) < len) = unvis_loop s (idx + 3) len (acc + (decode_ctrl_char (str_char_at s (idx + 2))))
        | ((str_char_at s (idx + 1)) == 77) and ((idx + 3) < len) and ((str_char_at s (idx + 2)) == 45) = unvis_loop s (idx + 4) len (acc + (char_from_code (128 + (str_char_at s (idx + 3)))))
        | ((str_char_at s (idx + 1)) == 77) and ((idx + 3) < len) and ((str_char_at s (idx + 2)) == 94) = unvis_loop s (idx + 4) len (acc + (char_from_code (128 + (decode_ctrl_code (str_char_at s (idx + 3))))))
        | ((str_char_at s (idx + 1)) == 120) and ((idx + 3) < len) and (is_hex_digit (str_char_at s (idx + 2))) and (is_hex_digit (str_char_at s (idx + 3))) = unvis_hex_2 s idx len acc
        | is_octal_digit (str_char_at s (idx + 1)) = unvis_octal_var s idx len acc
        | otherwise = unvis_loop s (idx + 2) len (acc + (decode_c_escape (str_char_at s (idx + 1))))

fn :: unvis_hex_2 String Int Int String => String
      unvis_hex_2 s      idx len acc    =
        h1 = hex_val (str_char_at s (idx + 2))
        h0 = hex_val (str_char_at s (idx + 3))
        unvis_loop s (idx + 4) len (acc + (char_from_code ((h1 * 16) + h0)))

fn :: unvis_octal_var String Int Int String => String
      unvis_octal_var s      idx len acc    =
        | ((idx + 3) < len) and (is_octal_digit (str_char_at s (idx + 2))) and (is_octal_digit (str_char_at s (idx + 3))) = unvis_octal_3 s idx len acc
        | ((idx + 2) < len) and (is_octal_digit (str_char_at s (idx + 2))) = unvis_octal_2 s idx len acc
        | otherwise = unvis_octal_1 s idx len acc

fn :: unvis_octal_3 String Int Int String => String
      unvis_octal_3 s      idx len acc    =
        d2 = (str_char_at s (idx + 1)) - 48
        d1 = (str_char_at s (idx + 2)) - 48
        d0 = (str_char_at s (idx + 3)) - 48
        code = (d2 * 64) + (d1 * 8) + d0
        unvis_loop s (idx + 4) len (acc + (char_from_code code))

fn :: unvis_octal_2 String Int Int String => String
      unvis_octal_2 s      idx len acc    =
        d1 = (str_char_at s (idx + 1)) - 48
        d0 = (str_char_at s (idx + 2)) - 48
        code = (d1 * 8) + d0
        unvis_loop s (idx + 3) len (acc + (char_from_code code))

fn :: unvis_octal_1 String Int Int String => String
      unvis_octal_1 s      idx len acc    =
        d0 = (str_char_at s (idx + 1)) - 48
        unvis_loop s (idx + 2) len (acc + (char_from_code d0))

op :: unvis decode String => String
      unvis decode s      =  unvis_loop s 0 (str_len s) ""

fn :: vis_encode_bytes_loop [Int]    VisSpec String => String
      vis_encode_bytes_loop []       spec    acc    =  acc
      vis_encode_bytes_loop (b:rest) spec    acc    =
        vis_encode_bytes_loop rest spec (acc + (encode_vis_code b spec))

op :: vis encode bytes [Int] with VisSpec => String
      vis encode bytes bs    with spec    =
        encoded = vis_encode_bytes_loop bs spec ""
        apply_vis_fold encoded (v_fold spec)

fn :: decode_c_escape_bytes Int => [Int]
      decode_c_escape_bytes c   =
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

fn :: unvis_bytes_loop String Int Int [Int] => [Int]
      unvis_bytes_loop s      idx len acc   =
        | idx >= len = acc
        | ((str_char_at s idx) == 92) and ((idx + 1) < len) = unvis_bytes_after_bslash s idx len acc
        | otherwise = unvis_bytes_loop s (idx + 1) len (acc :: [str_char_at s idx])

fn :: unvis_bytes_after_bslash String Int Int [Int] => [Int]
      unvis_bytes_after_bslash s      idx len acc   =
        | ((str_char_at s (idx + 1)) == 94) and ((idx + 2) < len) = unvis_bytes_loop s (idx + 3) len (acc :: [decode_ctrl_code (str_char_at s (idx + 2))])
        | ((str_char_at s (idx + 1)) == 77) and ((idx + 3) < len) and ((str_char_at s (idx + 2)) == 45) = unvis_bytes_loop s (idx + 4) len (acc :: [128 + (str_char_at s (idx + 3))])
        | ((str_char_at s (idx + 1)) == 77) and ((idx + 3) < len) and ((str_char_at s (idx + 2)) == 94) = unvis_bytes_loop s (idx + 4) len (acc :: [128 + (decode_ctrl_code (str_char_at s (idx + 3)))])
        | ((str_char_at s (idx + 1)) == 120) and ((idx + 3) < len) and (is_hex_digit (str_char_at s (idx + 2))) and (is_hex_digit (str_char_at s (idx + 3))) = unvis_bytes_hex_2 s idx len acc
        | is_octal_digit (str_char_at s (idx + 1)) = unvis_bytes_octal_var s idx len acc
        | otherwise = unvis_bytes_loop s (idx + 2) len (acc :: (decode_c_escape_bytes (str_char_at s (idx + 1))))

fn :: unvis_bytes_hex_2 String Int Int [Int] => [Int]
      unvis_bytes_hex_2 s      idx len acc   =
        h1 = hex_val (str_char_at s (idx + 2))
        h0 = hex_val (str_char_at s (idx + 3))
        unvis_bytes_loop s (idx + 4) len (acc :: [(h1 * 16) + h0])

fn :: unvis_bytes_octal_var String Int Int [Int] => [Int]
      unvis_bytes_octal_var s      idx len acc   =
        | ((idx + 3) < len) and (is_octal_digit (str_char_at s (idx + 2))) and (is_octal_digit (str_char_at s (idx + 3))) = unvis_bytes_octal_3 s idx len acc
        | ((idx + 2) < len) and (is_octal_digit (str_char_at s (idx + 2))) = unvis_bytes_octal_2 s idx len acc
        | otherwise = unvis_bytes_octal_1 s idx len acc

fn :: unvis_bytes_octal_3 String Int Int [Int] => [Int]
      unvis_bytes_octal_3 s      idx len acc   =
        d2 = (str_char_at s (idx + 1)) - 48
        d1 = (str_char_at s (idx + 2)) - 48
        d0 = (str_char_at s (idx + 3)) - 48
        code = (d2 * 64) + (d1 * 8) + d0
        unvis_bytes_loop s (idx + 4) len (acc :: [code])

fn :: unvis_bytes_octal_2 String Int Int [Int] => [Int]
      unvis_bytes_octal_2 s      idx len acc   =
        d1 = (str_char_at s (idx + 1)) - 48
        d0 = (str_char_at s (idx + 2)) - 48
        code = (d1 * 8) + d0
        unvis_bytes_loop s (idx + 3) len (acc :: [code])

fn :: unvis_bytes_octal_1 String Int Int [Int] => [Int]
      unvis_bytes_octal_1 s      idx len acc   =
        d0 = (str_char_at s (idx + 1)) - 48
        unvis_bytes_loop s (idx + 2) len (acc :: [d0])

op :: unvis decode bytes String => [Int]
      unvis decode bytes s      =  unvis_bytes_loop s 0 (str_len s) []
