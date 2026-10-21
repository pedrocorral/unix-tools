-- proc_lib.rg — Pure Ridgun BSD Process & Build Library (apply, make, sh)
-- @implements FR-PROC-001
-- @implements FR-PROC-002

-- ============================================================================
-- Algebraic Data Types (ADTs)
-- ============================================================================

type ApplySpec = a_magic: Int, a_dry_run: Bool, a_group_size: Int, a_has_group: Bool, a_cmd: String, a_args: [String]

type MakeRule = MRule String [String] [String]

type MakeVar = MVar String String

type MakeSpec = mk_file: String, mk_dry_run: Bool, mk_always: Bool, mk_jobs: Int, mk_cli_vars: [MakeVar], mk_targets: [String]

type Redir = RedirIn String | RedirOut String | RedirAppend String | RedirErrToOut

type ShNode = ShSimple [String] [Redir] | ShPipe ShNode ShNode | ShAnd ShNode ShNode | ShOr ShNode ShNode | ShSeq ShNode ShNode

expand type ShNode = ShSubshell ShNode | ShIf ShNode ShNode ShNode | ShWhile ShNode ShNode | ShFor String [String] ShNode | ShAssign String String

type ShEnv = env_vars: [MakeVar], env_pos: [String], env_status: Int, env_out: String, env_exited: Bool

-- ============================================================================
-- Foundational Helpers
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

fn :: join with [String] String => String
      join with []       sep    =  ""
      join with (x:[])   sep    =  x
      join with (x:xs)   sep    =  x + sep + (join with xs sep)

fn :: str list nth [String] Int => String
      str list nth []       n   =  ""
      str list nth (x:xs)   n   =
        | n <= 0    = x
        | otherwise = str list nth xs (n - 1)

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

fn :: take str list [A] Int [A] => [A]
      take str list [] n acc = acc
      take str list (x:xs) n acc =
        | n <= 0    = acc
        | otherwise = take str list xs (n - 1) (acc :: [x])

fn :: drop str list [A]    Int => [A]
      drop str list []     n   =  []
      drop str list (x:xs) n   =
        | n <= 0    = [x] :: xs
        | otherwise = drop str list xs (n - 1)

fn :: list contains str [String] String => Bool
      list contains str []       target =  False
      list contains str (x:xs)   target =
        | x == target = True
        | otherwise   = list contains str xs target

fn :: max int Int Int => Int
      max int a   b   =
        | a >= b    = a
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

-- ============================================================================
-- 1. BSD apply(1) — FR-PROC-001
-- ============================================================================

fn :: default_apply_spec => ApplySpec
      default_apply_spec =  ApplySpec 37 False 1 False "" []

op :: parse apply args [String] => ApplySpec
      parse apply args args     =  parse_apply_loop args default_apply_spec

fn :: parse_apply_loop [String]   ApplySpec => ApplySpec
      parse_apply_loop []         spec      =  spec
      parse_apply_loop (arg:rest) spec      =
        | arg == "-d" = parse_apply_loop rest (spec with a_dry_run: True)
        | arg == "-a" = parse_apply_flag_a rest spec
        | (str_starts_with arg "-a") and ((str_len arg) > 2) = parse_apply_inline_a (str_slice arg 2 (str_len arg)) rest spec
        | (str_starts_with arg "-") and (is_digits_only (str_slice arg 1 (str_len arg))) = parse_apply_loop rest (spec with a_group_size: str_to_int (str_slice arg 1 (str_len arg)), a_has_group: True)
        | otherwise = finalize_apply_cmd arg rest spec

fn :: parse_apply_flag_a [String] ApplySpec => ApplySpec
      parse_apply_flag_a []       spec      =  spec
      parse_apply_flag_a (c:rest) spec      =  parse_apply_inline_a c rest spec

fn :: parse_apply_inline_a String [String] ApplySpec => ApplySpec
      parse_apply_inline_a c      rest     spec      =
        mc = select_magic_char c
        parse_apply_loop rest (spec with a_magic: mc)

fn :: select_magic_char String => Int
      select_magic_char s      =
        | (str_len s) == 0 = 37
        | otherwise = str_char_at s 0

fn :: finalize_apply_cmd String [String] ApplySpec => ApplySpec
      finalize_apply_cmd cmd    rest     spec      =
        max_p = max_magic_placeholder cmd 0 (str_len cmd) (a_magic spec) 0
        inferred_n = resolve_apply_group (a_has_group spec) (a_group_size spec) max_p
        spec with a_group_size: inferred_n, a_has_group: max_p > 0, a_cmd: cmd, a_args: rest

fn :: resolve_apply_group Bool  Int        Int   => Int
      resolve_apply_group True  explicit_n max_p =  explicit_n
      resolve_apply_group False explicit_n max_p =
        | max_p > 0 = max_p
        | otherwise = 1

fn :: max_magic_placeholder String Int Int Int Int => Int
      max_magic_placeholder cmd idx len magic best =
        | (idx + 1) >= len = best
        | ((str_char_at cmd idx) == magic) and ((str_char_at cmd (idx + 1)) >= 49) and ((str_char_at cmd (idx + 1)) <= 57) = max_magic_placeholder cmd (idx + 2) len magic (max_int best ((str_char_at cmd (idx + 1)) - 48))
        | otherwise = max_magic_placeholder cmd (idx + 1) len magic best

fn :: subst_apply_chunk String Int Int Int [String] String => String
      subst_apply_chunk cmd idx len magic chunk acc =
        | idx >= len = acc
        | ((idx + 1) < len) and ((str_char_at cmd idx) == magic) and ((str_char_at cmd (idx + 1)) >= 49) and ((str_char_at cmd (idx + 1)) <= 57) = subst_apply_slot cmd idx len magic chunk acc
        | otherwise = subst_apply_chunk cmd (idx + 1) len magic chunk (acc + (str_slice cmd idx (idx + 1)))

fn :: subst_apply_slot String Int Int Int [String] String => String
      subst_apply_slot cmd idx len magic chunk acc =
        slot = ((str_char_at cmd (idx + 1)) - 49)
        val = str_list_nth chunk slot
        subst_apply_chunk cmd (idx + 2) len magic chunk (acc + val)

fn :: build_apply_cmd_line ApplySpec [String] => String
      build_apply_cmd_line spec      chunk    =
        | a_has_group spec = subst_apply_chunk (a_cmd spec) 0 (str_len (a_cmd spec)) (a_magic spec) chunk ""
        | (list_len chunk) == 0 = a_cmd spec
        | otherwise = (a_cmd spec) + " " + (join_with chunk " ")

fn :: build_apply_commands ApplySpec [String] [String] => [String]
      build_apply_commands spec      []       acc      =  acc
      build_apply_commands spec      rem      acc      =
        gs = max_int 1 (a_group_size spec)
        chunk = take_str_list rem gs []
        rest = drop_str_list rem gs
        cmd_line = build_apply_cmd_line spec chunk
        build_apply_commands spec rest (acc :: [cmd_line])

op :: evaluate apply ApplySpec => ([String], Int)
      evaluate apply spec      =
        cmds = build_apply_commands spec (a_args spec) []
        run_or_dry_apply cmds (a_dry_run spec) 0

fn :: run_or_dry_apply [String] Bool  Int => ([String], Int)
      run_or_dry_apply cmds     True  st  =  (cmds, 0)
      run_or_dry_apply []       False st  =  ([], st)
      run_or_dry_apply (c:rest) False st  =
        rc = shell_exec c
        next_st = select_nonzero_status st rc
        run_or_dry_apply rest False next_st

fn :: select_nonzero_status Int Int => Int
      select_nonzero_status 0   rc  =  rc
      select_nonzero_status st  rc  =  st

-- ============================================================================
-- 2. BSD make(1) — FR-PROC-001
-- ============================================================================

fn :: mv_key MakeVar    => String
      mv_key (MVar k v) =  k

fn :: mv_val MakeVar    => String
      mv_val (MVar k v) =  v

fn :: mr_target MakeRule => String
      mr_target (MRule t deps recs) = t

fn :: mr_deps MakeRule => [String]
      mr_deps (MRule t deps recs) = deps

fn :: mr_recipes MakeRule => [String]
      mr_recipes (MRule t deps recs) = recs

fn :: lookup_var [MakeVar] String => String
      lookup_var []        key    =  sys_getenv key
      lookup_var (v:rest)  key    =
        | (mv_key v) == key = mv_val v
        | otherwise = lookup_var rest key

fn :: has_var [MakeVar] String => Bool
      has_var []        key    =  False
      has_var (v:rest)  key    =
        | (mv_key v) == key = True
        | otherwise = has_var rest key

fn :: set_var [MakeVar] String String [MakeVar] => [MakeVar]
      set_var []        key    val    acc       =  acc :: [MVar key val]
      set_var (v:rest)  key    val    acc       =
        | (mv_key v) == key = acc :: [MVar key val] :: rest
        | otherwise = set_var rest key val (acc :: [v])

fn :: expand_make_vars String [MakeVar] String     String    => String
      expand_make_vars s      vars      cur_target first_dep =
        expand_make_loop s 0 (str_len s) vars cur_target first_dep ""

fn :: expand_make_loop String Int Int [MakeVar] String String String => String
      expand_make_loop s idx len vars cur_target first_dep acc =
        | idx >= len = acc
        | ((str_char_at s idx) == 36) and ((idx + 1) < len) = expand_make_dollar s idx len vars cur_target first_dep acc
        | otherwise = expand_make_loop s (idx + 1) len vars cur_target first_dep (acc + (str_slice s idx (idx + 1)))

fn :: expand_make_dollar String Int Int [MakeVar] String String String => String
      expand_make_dollar s idx len vars cur_target first_dep acc =
        | (str_char_at s (idx + 1)) == 64 = expand_make_loop s (idx + 2) len vars cur_target first_dep (acc + cur_target)
        | (str_char_at s (idx + 1)) == 60 = expand_make_loop s (idx + 2) len vars cur_target first_dep (acc + first_dep)
        | (str_char_at s (idx + 1)) == 36 = expand_make_loop s (idx + 2) len vars cur_target first_dep (acc + "$")
        | (str_char_at s (idx + 1)) == 40 = expand_make_delim s (idx + 2) len 41 vars cur_target first_dep acc
        | (str_char_at s (idx + 1)) == 123 = expand_make_delim s (idx + 2) len 125 vars cur_target first_dep acc
        | otherwise = expand_make_loop s (idx + 1) len vars cur_target first_dep (acc + "$")

fn :: expand_make_delim String Int Int Int [MakeVar] String String String => String
      expand_make_delim s idx len close_ch vars cur_target first_dep acc =
        pair = read_until_char s idx len close_ch ""
        vname = fst pair
        next_idx = snd pair
        val = lookup_var vars vname
        expand_make_loop s next_idx len vars cur_target first_dep (acc + val)

fn :: read_until_char String Int Int Int String => (String, Int)
      read_until_char s idx len close_ch acc =
        | idx >= len = (acc, len)
        | (str_char_at s idx) == close_ch = (acc, idx + 1)
        | otherwise = read_until_char s (idx + 1) len close_ch (acc + (str_slice s idx (idx + 1)))

fn :: find_char_idx String Int Int Int       => Int
      find_char_idx s      idx len target_ch =
        | idx >= len = 0 - 1
        | (str_char_at s idx) == target_ch = idx
        | otherwise = find_char_idx s (idx + 1) len target_ch

op :: parse make args [String] => MakeSpec
      parse make args args     =  parse_make_args_loop args (MakeSpec "Makefile" False False 1 [] [])

fn :: parse_make_args_loop [String]   MakeSpec => MakeSpec
      parse_make_args_loop []         spec     =  spec
      parse_make_args_loop (arg:rest) spec     =
        | arg == "-n" = parse_make_args_loop rest (spec with mk_dry_run: True)
        | arg == "-B" = parse_make_args_loop rest (spec with mk_always: True)
        | arg == "-f" = parse_make_flag_f rest spec
        | arg == "-j" = parse_make_flag_j rest spec
        | (find_char_idx arg 0 (str_len arg) 61) > 0 = parse_make_cli_var arg rest spec
        | otherwise = parse_make_args_loop rest (spec with mk_targets: (mk_targets spec) :: [arg])

fn :: parse_make_flag_f [String] MakeSpec => MakeSpec
      parse_make_flag_f []       spec     =  spec
      parse_make_flag_f (f:rest) spec     =
        parse_make_args_loop rest (spec with mk_file: f)

fn :: parse_make_flag_j [String] MakeSpec => MakeSpec
      parse_make_flag_j []       spec     =  spec
      parse_make_flag_j (j:rest) spec     =
        parse_make_args_loop rest (spec with mk_jobs: str_to_int j)

fn :: parse_make_cli_var String [String] MakeSpec => MakeSpec
      parse_make_cli_var arg    rest     spec     =
        eq_idx = find_char_idx arg 0 (str_len arg) 61
        k = trim_ws (str_slice arg 0 eq_idx)
        v = trim_ws (str_slice arg (eq_idx + 1) (str_len arg))
        next_vars = set_var (mk_cli_vars spec) k v []
        parse_make_args_loop rest (spec with mk_cli_vars: next_vars)

op :: parse makefile String with [MakeVar] => ([MakeRule], [MakeVar], [String])
      parse makefile raw    with cli_vars  =
        lines = split lines raw
        parse_makefile_lines lines cli_vars cli_vars "" [] [] [] []

fn :: flush_current_rule String [String] [String] [MakeRule] => [MakeRule]
      flush_current_rule ""     cur_deps cur_recs rules      =  rules
      flush_current_rule cur_t  cur_deps cur_recs rules      =  rules :: [MRule cur_t cur_deps cur_recs]

fn :: parse_makefile_lines [String] [MakeVar] [MakeVar] String [String] [String] [MakeRule] [String] => ([MakeRule], [MakeVar], [String])
      parse_makefile_lines [] vars cli_vars cur_t cur_deps cur_recs rules phonies =
        (flush_current_rule cur_t cur_deps cur_recs rules, vars, phonies)
      parse_makefile_lines (line:rest) vars cli_vars cur_t cur_deps cur_recs rules phonies =
        | ((str_len line) > 0) and ((str_char_at line 0) == 9) and ((str_len cur_t) > 0) = parse_makefile_lines rest vars cli_vars cur_t cur_deps (cur_recs :: [str_slice line 1 (str_len line)]) rules phonies
        | (str_len (trim_ws line)) == 0 = parse_makefile_lines rest vars cli_vars cur_t cur_deps cur_recs rules phonies
        | str_starts_with (trim_ws line) "#" = parse_makefile_lines rest vars cli_vars cur_t cur_deps cur_recs rules phonies
        | otherwise = parse_makefile_stmt line rest vars cli_vars (flush_current_rule cur_t cur_deps cur_recs rules) phonies

fn :: parse_makefile_stmt String [String] [MakeVar] [MakeVar] [MakeRule] [String] => ([MakeRule], [MakeVar], [String])
      parse_makefile_stmt line rest vars cli_vars rules phonies =
        eq_idx = find_char_idx line 0 (str_len line) 61
        col_idx = find_char_idx line 0 (str_len line) 58
        dispatch_makefile_stmt line rest vars cli_vars rules phonies eq_idx col_idx

fn :: dispatch_makefile_stmt String [String] [MakeVar] [MakeVar] [MakeRule] [String] Int Int => ([MakeRule], [MakeVar], [String])
      dispatch_makefile_stmt line rest vars cli_vars rules phonies eq_idx col_idx =
        | (eq_idx > 0) and ((col_idx < 0) or (eq_idx < col_idx) or ((col_idx + 1) == eq_idx)) = parse_makefile_assign line eq_idx rest vars cli_vars rules phonies
        | col_idx > 0 = parse_makefile_rule line col_idx rest vars cli_vars rules phonies
        | otherwise = parse_makefile_lines rest vars cli_vars "" [] [] rules phonies

fn :: parse_makefile_assign String Int [String] [MakeVar] [MakeVar] [MakeRule] [String] => ([MakeRule], [MakeVar], [String])
      parse_makefile_assign line eq_idx rest vars cli_vars rules phonies =
        prev_ch = str_char_at line (eq_idx - 1)
        is_cond = prev_ch == 63
        is_imm = prev_ch == 58
        key_end = select_assign_key_end eq_idx (is_cond or is_imm)
        key = trim_ws (str_slice line 0 key_end)
        raw_val = trim_ws (str_slice line (eq_idx + 1) (str_len line))
        exp_val = expand_make_vars raw_val vars "" ""
        next_vars = update_make_var vars cli_vars key exp_val is_cond
        parse_makefile_lines rest next_vars cli_vars "" [] [] rules phonies

fn :: select_assign_key_end Int    Bool  => Int
      select_assign_key_end eq_idx False =  eq_idx
      select_assign_key_end eq_idx True  =  eq_idx - 1

fn :: update_make_var [MakeVar] [MakeVar] String String Bool => [MakeVar]
      update_make_var vars cli_vars key val is_cond =
        | has_var cli_vars key = vars
        | is_cond and (has_var vars key) = vars
        | otherwise = set_var vars key val []

fn :: parse_makefile_rule String Int [String] [MakeVar] [MakeVar] [MakeRule] [String] => ([MakeRule], [MakeVar], [String])
      parse_makefile_rule line col_idx rest vars cli_vars rules phonies =
        raw_t = trim_ws (str_slice line 0 col_idx)
        exp_t = expand_make_vars raw_t vars "" ""
        raw_deps = trim_ws (str_slice line (col_idx + 1) (str_len line))
        exp_deps = split whitespace (expand_make_vars raw_deps vars exp_t "")
        record_makefile_rule exp_t exp_deps rest vars cli_vars rules phonies

fn :: record_makefile_rule String [String] [String] [MakeVar] [MakeVar] [MakeRule] [String] => ([MakeRule], [MakeVar], [String])
      record_makefile_rule ".PHONY" deps rest vars cli_vars rules phonies =
        parse_makefile_lines rest vars cli_vars "" [] [] rules (phonies :: deps)
      record_makefile_rule target deps rest vars cli_vars rules phonies =
        parse_makefile_lines rest vars cli_vars target deps [] rules phonies

fn :: find_rule [MakeRule] String => (Bool, MakeRule)
      find_rule []         target =  (False, MRule "" [] [])
      find_rule (r:rest)   target =
        | (mr_target r) == target = (True, r)
        | otherwise = find_rule rest target

fn :: file_mtime_or_neg String => Int
      file_mtime_or_neg path   =
        raw = file_lstat path
        extract_lstat_mtime raw

fn :: extract_lstat_mtime (Bool, String, Int, Int, Int, String, String, Int, String) => Int
      extract_lstat_mtime (False, k, m, nl, sz, ow, gr, mt, lt) = 0 - 1
      extract_lstat_mtime (True, k, m, nl, sz, ow, gr, mt, lt) = mt

fn :: needs_rebuild String [String] [String] [String] Bool => Bool
      needs_rebuild target deps phonies built always =
        | always = True
        | list_contains_str phonies target = True
        | (file_mtime_or_neg target) < 0 = True
        | otherwise = check_deps_newer (file_mtime_or_neg target) deps built

fn :: check_deps_newer Int     [String] [String] => Bool
      check_deps_newer t_mtime []       built    =  False
      check_deps_newer t_mtime (d:rest) built    =
        | list_contains_str built d = True
        | (file_mtime_or_neg d) > t_mtime = True
        | otherwise = check_deps_newer t_mtime rest built

fn :: eval_make_target String [MakeRule] [MakeVar] [String] MakeSpec [String] [String] [String] => (Bool, [String], [String])
      eval_make_target target rules vars phonies spec visiting built out_log =
        | list_contains_str visiting target = (False, built, out_log :: ["make: Graph cycles through " + target])
        | list_contains_str built target = (True, built, out_log)
        | otherwise = eval_make_target_rule target (find_rule rules target) rules vars phonies spec visiting built out_log

fn :: eval_make_target_rule String (Bool, MakeRule) [MakeRule] [MakeVar] [String] MakeSpec [String] [String] [String] => (Bool, [String], [String])
      eval_make_target_rule target (False, dummy) rules vars phonies spec visiting built out_log =
        | (file_mtime_or_neg target) >= 0 = (True, built, out_log)
        | otherwise = (False, built, out_log :: ["make: don't know how to make " + target])
      eval_make_target_rule target (True, rule) rules vars phonies spec visiting built out_log =
        deps = mr_deps rule
        dep_res = eval_make_deps deps rules vars phonies spec (visiting :: [target]) built out_log
        finish_make_target target rule deps dep_res phonies vars spec

fn :: eval_make_deps [String] [MakeRule] [MakeVar] [String] MakeSpec [String] [String] [String] => (Bool, [String], [String])
      eval_make_deps [] rules vars phonies spec visiting built out_log = (True, built, out_log)
      eval_make_deps (d:rest) rules vars phonies spec visiting built out_log =
        r1 = eval_make_target d rules vars phonies spec visiting built out_log
        continue_make_deps r1 rest rules vars phonies spec visiting

fn :: continue_make_deps (Bool, [String], [String]) [String] [MakeRule] [MakeVar] [String] MakeSpec [String] => (Bool, [String], [String])
      continue_make_deps (False, b1, log1) rest rules vars phonies spec visiting = (False, b1, log1)
      continue_make_deps (True, b1, log1) rest rules vars phonies spec visiting =
        eval_make_deps rest rules vars phonies spec visiting b1 log1

fn :: finish_make_target String MakeRule [String] (Bool, [String], [String]) [String] [MakeVar] MakeSpec => (Bool, [String], [String])
      finish_make_target target rule deps (False, b1, log1) phonies vars spec = (False, b1, log1)
      finish_make_target target rule deps (True, b1, log1) phonies vars spec =
        | not (needs_rebuild target deps phonies b1 (mk_always spec)) = (True, b1, log1)
        | otherwise = execute_make_recipes target (mr_recipes rule) (str_list_nth deps 0) vars (mk_dry_run spec) (b1 :: [target]) log1

fn :: execute_make_recipes String [String] String [MakeVar] Bool [String] [String] => (Bool, [String], [String])
      execute_make_recipes target [] first_dep vars dry_run built log_acc = (True, built, log_acc)
      execute_make_recipes target (raw_rec:rest) first_dep vars dry_run built log_acc =
        exp_rec = expand_make_vars raw_rec vars target first_dep
        silent = (str_starts_with exp_rec "@") and (not dry_run)
        clean_cmd = strip_at_prefix exp_rec
        next_log = append_recipe_log log_acc clean_cmd silent
        rc = run_recipe_cmd clean_cmd dry_run
        check_recipe_rc rc target rest first_dep vars dry_run built next_log

fn :: strip_at_prefix String => String
      strip_at_prefix s      =
        | str_starts_with s "@" = str_slice s 1 (str_len s)
        | otherwise = s

fn :: append_recipe_log [String] String Bool  => [String]
      append_recipe_log log_acc  cmd    True  =  log_acc
      append_recipe_log log_acc  cmd    False =  log_acc :: [cmd]

fn :: run_recipe_cmd String Bool  => Int
      run_recipe_cmd cmd    True  =  0
      run_recipe_cmd cmd    False =  shell_exec cmd

fn :: check_recipe_rc Int String [String] String [MakeVar] Bool [String] [String] => (Bool, [String], [String])
      check_recipe_rc 0 target rest first_dep vars dry_run built next_log =
        execute_make_recipes target rest first_dep vars dry_run built next_log
      check_recipe_rc rc target rest first_dep vars dry_run built next_log =
        (False, built, next_log :: ["make: *** [" + target + "] Error " + (int_to_str rc)])

op :: evaluate make String       with MakeSpec => (Bool, [String])
      evaluate make raw_makefile with spec     =
        parsed = parse makefile raw_makefile with (mk_cli_vars spec)
        rules = fst3 parsed
        vars = snd3 parsed
        phonies = trd3 parsed
        targets = select_make_targets (mk_targets spec) rules
        res = eval_make_deps targets rules vars phonies spec [] [] []
        (fst3 res, trd3 res)

fn :: select_make_targets [String] [MakeRule]        => [String]
      select_make_targets []       []                =  []
      select_make_targets []       (first_rule:rest) =  [mr_target first_rule]
      select_make_targets explicit rules             =  explicit

-- ============================================================================
-- 3. BSD sh(1) Almquist-Style Shell — FR-PROC-002
-- ============================================================================

op :: tokenize shell String => [String]
      tokenize shell src    =  lex_sh_loop src 0 (str_len src) "" False []

fn :: push_sh_tok String Bool  [String] => [String]
      push_sh_tok cur    True  acc      =  acc :: [cur]
      push_sh_tok cur    False acc      =
        | (str_len cur) == 0 = acc
        | otherwise = acc :: [cur]

fn :: lex_sh_loop String Int Int String Bool [String] => [String]
      lex_sh_loop s idx len cur quoted acc =
        | idx >= len = push_sh_tok cur quoted acc
        | ((str_char_at s idx) == 35) and ((str_len cur) == 0) and (not quoted) = lex_skip_comment s (idx + 1) len acc
        | (str_char_at s idx) == 39 = lex_single_quote s (idx + 1) len (cur + (char_from_code 1)) acc
        | (str_char_at s idx) == 34 = lex_double_quote s (idx + 1) len cur acc
        | ((str_char_at s idx) == 92) and ((idx + 1) < len) = lex_sh_loop s (idx + 2) len (cur + (str_slice s (idx + 1) (idx + 2))) True acc
        | (str_char_at s idx) == 10 = lex_sh_loop s (idx + 1) len "" False ((push_sh_tok cur quoted acc) :: [";"])
        | is_ws (str_char_at s idx) = lex_sh_loop s (idx + 1) len "" False (push_sh_tok cur quoted acc)
        | ((idx + 3) < len) and ((str_slice s idx (idx + 4)) == "2>&1") = lex_sh_loop s (idx + 4) len "" False ((push_sh_tok cur quoted acc) :: ["2>&1"])
        | ((idx + 1) < len) and (((str_slice s idx (idx + 2)) == "&&") or ((str_slice s idx (idx + 2)) == "||") or ((str_slice s idx (idx + 2)) == ">>")) = lex_sh_loop s (idx + 2) len "" False ((push_sh_tok cur quoted acc) :: [str_slice s idx (idx + 2)])
        | ((str_char_at s idx) == 59) or ((str_char_at s idx) == 124) or ((str_char_at s idx) == 40) or ((str_char_at s idx) == 41) or ((str_char_at s idx) == 60) or ((str_char_at s idx) == 62) = lex_sh_loop s (idx + 1) len "" False ((push_sh_tok cur quoted acc) :: [str_slice s idx (idx + 1)])
        | otherwise = lex_sh_loop s (idx + 1) len (cur + (str_slice s idx (idx + 1))) quoted acc

fn :: lex_skip_comment String Int Int [String] => [String]
      lex_skip_comment s      idx len acc      =
        | idx >= len = acc
        | (str_char_at s idx) == 10 = lex_sh_loop s (idx + 1) len "" False (acc :: [";"])
        | otherwise = lex_skip_comment s (idx + 1) len acc

fn :: lex_single_quote String Int Int String [String] => [String]
      lex_single_quote s idx len cur acc =
        | idx >= len = push_sh_tok (cur + (char_from_code 2)) True acc
        | (str_char_at s idx) == 39 = lex_sh_loop s (idx + 1) len (cur + (char_from_code 2)) True acc
        | otherwise = lex_single_quote s (idx + 1) len (cur + (str_slice s idx (idx + 1))) acc

fn :: is_dq_escapable Int => Bool
      is_dq_escapable c   =  (c == 34) or (c == 92) or (c == 36)

fn :: lex_double_quote String Int Int String [String] => [String]
      lex_double_quote s idx len cur acc =
        | idx >= len = push_sh_tok cur True acc
        | (str_char_at s idx) == 34 = lex_sh_loop s (idx + 1) len cur True acc
        | ((str_char_at s idx) == 92) and ((idx + 1) < len) and (is_dq_escapable (str_char_at s (idx + 1))) = lex_double_quote s (idx + 2) len (cur + (str_slice s (idx + 1) (idx + 2))) acc
        | ((str_char_at s idx) == 92) and ((idx + 1) < len) = lex_double_quote s (idx + 2) len (cur + (str_slice s idx (idx + 2))) acc
        | otherwise = lex_double_quote s (idx + 1) len (cur + (str_slice s idx (idx + 1))) acc

op :: parse shell [String] => ShNode
      parse shell toks     =  fst (parse_sh_list toks [])

fn :: skip_semis [String] => [String]
      skip_semis []       =  []
      skip_semis (t:rest) =
        | t == ";" = skip_semis rest
        | otherwise = [t] :: rest

fn :: is_stop_word String [String] => Bool
      is_stop_word tok    stops    =  list_contains_str stops tok

fn :: parse_sh_list [String] [String] => (ShNode, [String])
      parse_sh_list toks     stops    =
        cleaned = skip_semis toks
        parse_sh_list_nonempty cleaned stops

fn :: parse_sh_list_nonempty [String] [String] => (ShNode, [String])
      parse_sh_list_nonempty []       stops    =  (ShSimple [] [], [])
      parse_sh_list_nonempty (t:rest) stops    =
        | is_stop_word t stops = (ShSimple [] [], [t] :: rest)
        | otherwise =
          first = parse_sh_and_or ([t] :: rest)
          parse_sh_list_tail (fst first) (skip_semis (snd first)) stops

fn :: parse_sh_list_tail ShNode [String] [String] => (ShNode, [String])
      parse_sh_list_tail left   []       stops    =  (left, [])
      parse_sh_list_tail left   (t:rest) stops    =
        | is_stop_word t stops = (left, [t] :: rest)
        | otherwise = combine_sh_seq left (parse_sh_and_or ([t] :: rest)) stops

fn :: combine_sh_seq ShNode (ShNode, [String]) [String] => (ShNode, [String])
      combine_sh_seq left   (right, rem)       stops    =
        parse_sh_list_tail (ShSeq left right) (skip_semis rem) stops

fn :: parse_sh_and_or [String] => (ShNode, [String])
      parse_sh_and_or toks     =
        first = parse_sh_pipeline toks
        parse_sh_and_or_tail (fst first) (snd first)

fn :: parse_sh_and_or_tail ShNode [String] => (ShNode, [String])
      parse_sh_and_or_tail left   []       =  (left, [])
      parse_sh_and_or_tail left   (t:rest) =
        | t == "&&" = combine_sh_and left (parse_sh_pipeline (skip_semis rest))
        | t == "||" = combine_sh_or left (parse_sh_pipeline (skip_semis rest))
        | otherwise = (left, [t] :: rest)

fn :: combine_sh_and ShNode (ShNode, [String]) => (ShNode, [String])
      combine_sh_and left   (right, rem)       =  parse_sh_and_or_tail (ShAnd left right) rem

fn :: combine_sh_or ShNode (ShNode, [String]) => (ShNode, [String])
      combine_sh_or left   (right, rem)       =  parse_sh_and_or_tail (ShOr left right) rem

fn :: parse_sh_pipeline [String] => (ShNode, [String])
      parse_sh_pipeline toks     =
        first = parse_sh_command toks
        parse_sh_pipe_tail (fst first) (snd first)

fn :: parse_sh_pipe_tail ShNode [String] => (ShNode, [String])
      parse_sh_pipe_tail left   []       =  (left, [])
      parse_sh_pipe_tail left   (t:rest) =
        | t == "|" = combine_sh_pipe left (parse_sh_command (skip_semis rest))
        | otherwise = (left, [t] :: rest)

fn :: combine_sh_pipe ShNode (ShNode, [String]) => (ShNode, [String])
      combine_sh_pipe left   (right, rem)       =  parse_sh_pipe_tail (ShPipe left right) rem

fn :: parse_sh_command [String] => (ShNode, [String])
      parse_sh_command []       =  (ShSimple [] [], [])
      parse_sh_command (t:rest) =
        | t == "(" = parse_sh_subshell rest
        | t == "if" = parse_sh_if rest
        | t == "while" = parse_sh_while rest
        | t == "for" = parse_sh_for rest
        | otherwise = parse_sh_simple ([t] :: rest) [] []

fn :: consume_expected String [String] => [String]
      consume_expected exp    []       =  []
      consume_expected exp    (t:rest) =
        | t == exp = rest
        | otherwise = [t] :: rest

fn :: parse_sh_subshell [String] => (ShNode, [String])
      parse_sh_subshell toks     =
        inner = parse_sh_list toks [")"]
        rem = consume_expected ")" (snd inner)
        (ShSubshell (fst inner), rem)

fn :: parse_sh_if [String] => (ShNode, [String])
      parse_sh_if toks     =
        cond_res = parse_sh_list toks ["then"]
        after_then = consume_expected "then" (snd cond_res)
        then_res = parse_sh_list after_then ["elif", "else", "fi"]
        parse_sh_if_else (fst cond_res) (fst then_res) (snd then_res)

fn :: parse_sh_if_else ShNode ShNode [String] => (ShNode, [String])
      parse_sh_if_else cond_n then_n []       =  (ShIf cond_n then_n (ShSimple [] []), [])
      parse_sh_if_else cond_n then_n (t:rest) =
        | t == "fi" = (ShIf cond_n then_n (ShSimple [] []), rest)
        | t == "elif" = wrap_elif cond_n then_n (parse_sh_if rest)
        | t == "else" = wrap_else cond_n then_n (parse_sh_list rest ["fi"])
        | otherwise = (ShIf cond_n then_n (ShSimple [] []), [t] :: rest)

fn :: wrap_elif ShNode ShNode (ShNode, [String]) => (ShNode, [String])
      wrap_elif cond_n then_n (elif_node, rem)   =  (ShIf cond_n then_n elif_node, rem)

fn :: wrap_else ShNode ShNode (ShNode, [String]) => (ShNode, [String])
      wrap_else cond_n then_n (else_node, rem)   =
        after_fi = consume_expected "fi" rem
        (ShIf cond_n then_n else_node, after_fi)

fn :: parse_sh_while [String] => (ShNode, [String])
      parse_sh_while toks     =
        cond_res = parse_sh_list toks ["do"]
        after_do = consume_expected "do" (snd cond_res)
        body_res = parse_sh_list after_do ["done"]
        after_done = consume_expected "done" (snd body_res)
        (ShWhile (fst cond_res) (fst body_res), after_done)

fn :: parse_sh_for [String]     => (ShNode, [String])
      parse_sh_for []           =  (ShSimple [] [], [])
      parse_sh_for (vname:rest) =
        after_in = consume_expected "in" rest
        items_res = collect_for_items after_in []
        items = fst items_res
        after_semi = skip_semis (snd items_res)
        after_do = consume_expected "do" after_semi
        body_res = parse_sh_list after_do ["done"]
        after_done = consume_expected "done" (snd body_res)
        (ShFor vname items (fst body_res), after_done)

fn :: collect_for_items [String] [String] => ([String], [String])
      collect_for_items []       acc      =  (acc, [])
      collect_for_items (t:rest) acc      =
        | (t == ";") or (t == "do") = (acc, [t] :: rest)
        | otherwise = collect_for_items rest (acc :: [t])

fn :: is_cmd_delim String => Bool
      is_cmd_delim t      =
        (t == ";") or (t == "&&") or (t == "||") or (t == "|") or (t == ")") or (t == "then") or (t == "elif") or (t == "else") or (t == "fi") or (t == "do") or (t == "done")

fn :: is_var_assign_tok String => Bool
      is_var_assign_tok s      =
        eq_i = find_char_idx s 0 (str_len s) 61
        (eq_i > 0) and (not (str_starts_with s "="))

fn :: parse_sh_simple [String] [String] [Redir] => (ShNode, [String])
      parse_sh_simple []       words    redirs  =  (finalize_simple_node words redirs, [])
      parse_sh_simple (t:rest) words    redirs  =
        | is_cmd_delim t = (finalize_simple_node words redirs, [t] :: rest)
        | t == "<" = parse_sh_redir 1 rest words redirs
        | t == ">" = parse_sh_redir 2 rest words redirs
        | t == ">>" = parse_sh_redir 3 rest words redirs
        | t == "2>&1" = parse_sh_simple rest words (redirs :: [RedirErrToOut])
        | otherwise = parse_sh_simple rest (words :: [t]) redirs

fn :: parse_sh_redir Int  [String]      [String] [Redir] => (ShNode, [String])
      parse_sh_redir kind []            words    redirs  =  (finalize_simple_node words redirs, [])
      parse_sh_redir kind (target:rest) words    redirs  =
        | kind == 1 = parse_sh_simple rest words (redirs :: [RedirIn target])
        | kind == 2 = parse_sh_simple rest words (redirs :: [RedirOut target])
        | otherwise = parse_sh_simple rest words (redirs :: [RedirAppend target])

fn :: finalize_simple_node [String] [Redir] => ShNode
      finalize_simple_node (w:[])   []      =
        | is_var_assign_tok w = split_assign_node w
        | otherwise = ShSimple [w] []
      finalize_simple_node words    redirs  =  ShSimple words redirs

fn :: split_assign_node String => ShNode
      split_assign_node w      =
        eq_i = find_char_idx w 0 (str_len w) 61
        ShAssign (str_slice w 0 eq_i) (str_slice w (eq_i + 1) (str_len w))

fn :: expand_sh_word String ShEnv => String
      expand_sh_word w      env   =  expand_sh_word_loop w 0 (str_len w) False env ""

fn :: expand_sh_word_loop String Int Int Bool ShEnv String => String
      expand_sh_word_loop s idx len in_sq env acc =
        | idx >= len = acc
        | (str_char_at s idx) == 1 = expand_sh_word_loop s (idx + 1) len True env acc
        | (str_char_at s idx) == 2 = expand_sh_word_loop s (idx + 1) len False env acc
        | (not in_sq) and ((str_char_at s idx) == 36) and ((idx + 1) < len) = expand_sh_dollar s idx len env acc
        | otherwise = expand_sh_word_loop s (idx + 1) len in_sq env (acc + (str_slice s idx (idx + 1)))

fn :: expand_sh_dollar String Int Int ShEnv String => String
      expand_sh_dollar s idx len env acc =
        | (str_char_at s (idx + 1)) == 63 = expand_sh_word_loop s (idx + 2) len False env (acc + (int_to_str (env_status env)))
        | (str_char_at s (idx + 1)) == 35 = expand_sh_word_loop s (idx + 2) len False env (acc + (int_to_str (list_len (env_pos env))))
        | (str_char_at s (idx + 1)) == 64 = expand_sh_word_loop s (idx + 2) len False env (acc + (join_with (env_pos env) " "))
        | (str_char_at s (idx + 1)) == 36 = expand_sh_word_loop s (idx + 2) len False env (acc + "1000")
        | ((str_char_at s (idx + 1)) >= 49) and ((str_char_at s (idx + 1)) <= 57) = expand_sh_word_loop s (idx + 2) len False env (acc + (str_list_nth (env_pos env) ((str_char_at s (idx + 1)) - 49)))
        | (str_char_at s (idx + 1)) == 123 = expand_sh_braced s (idx + 2) len env acc
        | otherwise = expand_sh_plain_var s (idx + 1) len env acc

fn :: expand_sh_braced String Int Int ShEnv String => String
      expand_sh_braced s idx len env acc =
        pair = read_until_char s idx len 125 ""
        expr = fst pair
        next_idx = snd pair
        val = resolve_braced_expr expr env
        expand_sh_word_loop s next_idx len False env (acc + val)

fn :: resolve_braced_expr String ShEnv => String
      resolve_braced_expr expr   env   =
        col_idx = find_char_idx expr 0 (str_len expr) 58
        resolve_braced_colon expr col_idx env

fn :: lookup_sh_param String ShEnv => String
      lookup_sh_param vname  env   =
        | vname == "?" = int_to_str (env_status env)
        | vname == "#" = int_to_str (list_len (env_pos env))
        | vname == "@" = join_with (env_pos env) " "
        | (is_digits_only vname) and ((str_to_int vname) >= 1) = str_list_nth (env_pos env) ((str_to_int vname) - 1)
        | otherwise = lookup_var (env_vars env) vname

fn :: resolve_braced_colon String Int     ShEnv => String
      resolve_braced_colon expr   col_idx env   =
        | (col_idx > 0) and (((col_idx + 1) < (str_len expr)) and ((str_char_at expr (col_idx + 1)) == 45)) = resolve_var_or_default (str_slice expr 0 col_idx) (str_slice expr (col_idx + 2) (str_len expr)) env
        | otherwise = lookup_sh_param expr env

fn :: resolve_var_or_default String String  ShEnv => String
      resolve_var_or_default vname  def_val env   =
        v = lookup_sh_param vname env
        select_nonempty_str v def_val

fn :: select_nonempty_str String String  => String
      select_nonempty_str v      def_val =
        | (str_len v) > 0 = v
        | otherwise = def_val

fn :: is_var_char Int => Bool
      is_var_char c   =
        ((c >= 65) and (c <= 90)) or ((c >= 97) and (c <= 122)) or ((c >= 48) and (c <= 57)) or (c == 95)

fn :: read_var_ident String Int Int String => (String, Int)
      read_var_ident s      idx len acc    =
        | idx >= len = (acc, len)
        | is_var_char (str_char_at s idx) = read_var_ident s (idx + 1) len (acc + (str_slice s idx (idx + 1)))
        | otherwise = (acc, idx)

fn :: expand_sh_plain_var String Int Int ShEnv String => String
      expand_sh_plain_var s idx len env acc =
        pair = read_var_ident s idx len ""
        vname = fst pair
        next_idx = snd pair
        val = lookup_var (env_vars env) vname
        expand_sh_word_loop s next_idx len False env (acc + val)

fn :: expand_sh_words [String] ShEnv [String] => [String]
      expand_sh_words []       env   acc      =  acc
      expand_sh_words (w:rest) env   acc      =
        expand_sh_words rest env (acc :: [expand_sh_word w env])

op :: evaluate shell ShNode in ShEnv => ShEnv
      evaluate shell node   in env   =
        | env_exited env = env
        | otherwise = eval_sh_active node env

fn :: eval_sh_active ShNode           ShEnv => ShEnv
      eval_sh_active (ShAssign k v)   env   =
        exp_v = expand_sh_word v env
        _ = sys_setenv k exp_v
        env with env_vars: set_var (env_vars env) k exp_v [], env_status: 0, env_exited: False
      eval_sh_active (ShSeq a b)      env   =
        env1 = evaluate shell a in env
        evaluate shell b in env1
      eval_sh_active (ShAnd a b)      env   =
        env1 = evaluate shell a in env
        eval_sh_and_rhs b env1
      eval_sh_active (ShOr a b)       env   =
        env1 = evaluate shell a in env
        eval_sh_or_rhs b env1
      eval_sh_active (ShSubshell sub) env   =
        sub_env = evaluate shell sub in env
        env with env_status: env_status sub_env, env_out: env_out sub_env, env_exited: False
      eval_sh_active (ShIf cond_n then_n else_n) env =
        env_c = evaluate shell cond_n in env
        eval_sh_if_branch then_n else_n env_c
      eval_sh_active (ShWhile cond_n body_n) env =
        eval_sh_while_loop cond_n body_n env 0
      eval_sh_active (ShFor vname raw_items body_n) env =
        items = expand_sh_words raw_items env []
        eval_sh_for_loop vname items body_n env
      eval_sh_active (ShPipe left right) env =
        eval_sh_pipe left right env
      eval_sh_active (ShSimple raw_words redirs) env =
        words = expand_sh_words raw_words env []
        eval_sh_simple words redirs env

fn :: eval_sh_and_rhs ShNode ShEnv => ShEnv
      eval_sh_and_rhs b      env1  =
        | (env_status env1) == 0 = evaluate shell b in env1
        | otherwise = env1

fn :: eval_sh_or_rhs ShNode ShEnv => ShEnv
      eval_sh_or_rhs b      env1  =
        | (env_status env1) != 0 = evaluate shell b in env1
        | otherwise = env1

fn :: eval_sh_if_branch ShNode ShNode ShEnv => ShEnv
      eval_sh_if_branch then_n else_n env_c =
        | (env_status env_c) == 0 = evaluate shell then_n in env_c
        | otherwise = evaluate shell else_n in env_c

fn :: eval_sh_while_loop ShNode ShNode ShEnv Int   => ShEnv
      eval_sh_while_loop cond_n body_n env   steps =
        | (env_exited env) or (steps >= 1000) = env
        | otherwise =
          env_c = evaluate shell cond_n in env
          continue_sh_while cond_n body_n env_c steps

fn :: continue_sh_while ShNode ShNode ShEnv Int   => ShEnv
      continue_sh_while cond_n body_n env_c steps =
        | (env_status env_c) != 0 = env_c with env_status: 0
        | otherwise = eval_sh_while_loop cond_n body_n (evaluate shell body_n in env_c) (steps + 1)

fn :: eval_sh_for_loop String [String]    ShNode ShEnv => ShEnv
      eval_sh_for_loop vname  []          body_n env   =  env
      eval_sh_for_loop vname  (item:rest) body_n env   =
        | env_exited env = env
        | otherwise =
          _ = sys_setenv vname item
          env1 = env with env_vars: set_var (env_vars env) vname item [], env_exited: False
          env2 = evaluate shell body_n in env1
          (eval_sh_for_loop vname rest body_n env2)

fn :: quote_sh_words [String] [String] => [String]
      quote_sh_words []       acc      =  acc
      quote_sh_words (w:rest) acc      =
        sq = char_from_code 39
        quote_sh_words rest (acc :: [sq + w + sq])

fn :: render_node_to_sh ShNode       ShEnv => String
      render_node_to_sh (ShSimple raw_words redirs) env =
        words = quote_sh_words (expand_sh_words raw_words env []) []
        (join_with words " ") + (render_redirs redirs env "")
      render_node_to_sh (ShPipe a b) env   =
        (render_node_to_sh a env) + " | " + (render_node_to_sh b env)
      render_node_to_sh other        env   =  "true"

fn :: render_redirs [Redir]            ShEnv String => String
      render_redirs []                 env   acc    =  acc
      render_redirs ((RedirIn f):rest) env   acc    =  render_redirs rest env (acc + " < " + (expand_sh_word f env))
      render_redirs ((RedirOut f):rest) env  acc    =  render_redirs rest env (acc + " > " + (expand_sh_word f env))
      render_redirs ((RedirAppend f):rest) env acc  =  render_redirs rest env (acc + " >> " + (expand_sh_word f env))
      render_redirs (RedirErrToOut:rest) env acc    =  render_redirs rest env (acc + " 2>&1")

fn :: eval_sh_pipe ShNode ShNode ShEnv => ShEnv
      eval_sh_pipe left   right  env   =
        cmd_str = render_node_to_sh (ShPipe left right) env
        run_external_capture cmd_str env

fn :: run_external_capture String  ShEnv => ShEnv
      run_external_capture cmd_str env   =
        rc = shell_exec ("(" + cmd_str + ") > /tmp/.ut_sh_cap.out")
        captured = read_file "/tmp/.ut_sh_cap.out"
        _ = shell_exec "rm -f /tmp/.ut_sh_cap.out"
        env with env_status: rc, env_out: (env_out env) + captured, env_exited: False

fn :: apply_out_redirs String [Redir]      ShEnv => String
      apply_out_redirs text   []           env   =  (env_out env) + text
      apply_out_redirs text   ((RedirOut f):rest) env =
        target = expand_sh_word f env
        _ = write_file target text
        apply_out_redirs "" rest env
      apply_out_redirs text   ((RedirAppend f):rest) env =
        target = expand_sh_word f env
        prev = read_file target
        _ = write_file target (prev + text)
        apply_out_redirs "" rest env
      apply_out_redirs text   (other:rest) env   =  apply_out_redirs text rest env

fn :: eval_sh_simple [String]   [Redir] ShEnv => ShEnv
      eval_sh_simple []         redirs  env   =  env
      eval_sh_simple (cmd:args) redirs  env   =
        | cmd == "true" = env with env_status: 0, env_exited: False
        | cmd == "false" = env with env_status: 1, env_exited: False
        | cmd == "echo" = eval_builtin_echo args redirs env
        | cmd == "cd" = eval_builtin_cd args env
        | cmd == "pwd" = env with env_status: 0, env_out: apply_out_redirs ((sys_getcwd) + (char_from_code 10)) redirs env, env_exited: False
        | cmd == "export" = eval_builtin_export args env
        | cmd == "unset" = eval_builtin_unset args env
        | cmd == "set" = eval_builtin_set args env
        | cmd == "exit" = eval_builtin_exit args env
        | (cmd == "test") or (cmd == "[") = eval_builtin_test cmd args env
        | cmd == "." = eval_builtin_source args env
        | otherwise = run_external_capture ((join_with ([cmd] :: args) " ") + (render_redirs redirs env "")) env

fn :: eval_builtin_echo [String]    [Redir] ShEnv => ShEnv
      eval_builtin_echo ("-n":rest) redirs  env   =
        out = apply_out_redirs (join_with rest " ") redirs env
        env with env_status: 0, env_out: out, env_exited: False
      eval_builtin_echo args        redirs  env   =
        out = apply_out_redirs ((join_with args " ") + (char_from_code 10)) redirs env
        env with env_status: 0, env_out: out, env_exited: False

fn :: eval_builtin_cd [String]   ShEnv => ShEnv
      eval_builtin_cd []         env   =
        rc = sys_chdir (sys_getenv "HOME")
        env with env_status: status_from_sys rc, env_exited: False
      eval_builtin_cd (dir:rest) env   =
        rc = sys_chdir dir
        env with env_status: status_from_sys rc, env_exited: False

fn :: status_from_sys Int   => Int
      status_from_sys 0     =  0
      status_from_sys other =  1

fn :: eval_builtin_export [String]   ShEnv => ShEnv
      eval_builtin_export []         env   =  env
      eval_builtin_export (arg:rest) env   =
        | is_var_assign_tok arg =
          eq_i = find_char_idx arg 0 (str_len arg) 61
          k = str_slice arg 0 eq_i
          v = str_slice arg (eq_i + 1) (str_len arg)
          _ = sys_setenv k v
          (eval_builtin_export rest (env with env_vars: set_var (env_vars env) k v [], env_status: 0, env_exited: False))
        | otherwise =
          _ = sys_setenv arg (lookup_var (env_vars env) arg)
          (eval_builtin_export rest env)

fn :: remove_var [MakeVar] String [MakeVar] => [MakeVar]
      remove_var []        key    acc       =  acc
      remove_var (v:rest)  key    acc       =
        | (mv_key v) == key = remove_var rest key acc
        | otherwise = remove_var rest key (acc :: [v])

fn :: eval_builtin_unset [String] ShEnv => ShEnv
      eval_builtin_unset []       env   =  env
      eval_builtin_unset (k:rest) env   =
        _ = sys_setenv k ""
        eval_builtin_unset rest (env with env_vars: remove_var (env_vars env) k [], env_status: 0, env_exited: False)

fn :: eval_builtin_set [String]    ShEnv => ShEnv
      eval_builtin_set ("--":rest) env   =  env with env_pos: rest, env_status: 0, env_exited: False
      eval_builtin_set args        env   =  env with env_pos: args, env_status: 0, env_exited: False

fn :: eval_builtin_exit [String]    ShEnv => ShEnv
      eval_builtin_exit []          env   =  env with env_exited: True
      eval_builtin_exit (code:rest) env   =  env with env_status: str_to_int code, env_exited: True

fn :: eval_builtin_source [String]    ShEnv => ShEnv
      eval_builtin_source []          env   =  env
      eval_builtin_source (path:rest) env   =
        script = read_file path
        node = parse shell (tokenize shell script)
        evaluate shell node in env

fn :: strip_closing_bracket [String] [String] => [String]
      strip_closing_bracket []       acc      =  acc
      strip_closing_bracket ("]":[]) acc      =  acc
      strip_closing_bracket (x:xs)   acc      =  strip_closing_bracket xs (acc :: [x])

fn :: eval_builtin_test String [String] ShEnv => ShEnv
      eval_builtin_test cmd    raw_args env   =
        args = clean_test_args cmd raw_args
        ok = eval_test_expr args
        env with env_status: bool_to_status ok, env_exited: False

fn :: clean_test_args String [String] => [String]
      clean_test_args "["    raw_args =  strip_closing_bracket raw_args []
      clean_test_args other  raw_args =  raw_args

fn :: bool_to_status Bool  => Int
      bool_to_status True  =  0
      bool_to_status False =  1

fn :: eval_test_expr [String]        => Bool
      eval_test_expr []              =  False
      eval_test_expr ("!":rest)      =  not (eval_test_expr rest)
      eval_test_expr (x:[])          =  (str_len x) > 0
      eval_test_expr (op_s:arg:[])   =
        | op_s == "-z" = (str_len arg) == 0
        | op_s == "-n" = (str_len arg) > 0
        | op_s == "-e" = (file_mtime_or_neg arg) >= 0
        | op_s == "-f" = check_lstat_kind (file_lstat arg) "file"
        | op_s == "-d" = check_lstat_kind (file_lstat arg) "dir"
        | otherwise = False
      eval_test_expr (a:op_s:b:rest) =
        | (op_s == "=") or (op_s == "==") = a == b
        | op_s == "!=" = a != b
        | op_s == "-eq" = (str_to_int a) == (str_to_int b)
        | op_s == "-ne" = (str_to_int a) != (str_to_int b)
        | op_s == "-lt" = (str_to_int a) < (str_to_int b)
        | op_s == "-le" = (str_to_int a) <= (str_to_int b)
        | op_s == "-gt" = (str_to_int a) > (str_to_int b)
        | op_s == "-ge" = (str_to_int a) >= (str_to_int b)
        | otherwise = False

fn :: check_lstat_kind (Bool, String, Int, Int, Int, String, String, Int, String) String => Bool
      check_lstat_kind (False, k, m, nl, sz, ow, gr, mt, lt) exp_k = False
      check_lstat_kind (True, k, m, nl, sz, ow, gr, mt, lt) exp_k = k == exp_k

op :: run shell script String with [String] => (String, Int)
      run shell script src    with pos_args =
        toks = tokenize shell src
        ast = parse shell toks
        init_env = ShEnv [] pos_args 0 "" False
        final_env = evaluate shell ast in init_env
        (env_out final_env, env_status final_env)
