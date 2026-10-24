-- proc_lib.rg — Pure Ridgun BSD Process, Build & Socket Library (apply, make, sh, timeout, nc)
-- @implements FR-PROC-001
-- @implements FR-PROC-002
-- @implements FR-PROC-003

import io
import socket

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

type TimeoutSpec = to_sig: String, to_kill_ms: Int, to_preserve: Bool, to_dur_ms: Int, to_cmd: String, to_args: [String]

type NcSpec = nc_listen: Bool, nc_udp: Bool, nc_unix: Bool, nc_zero: Bool, nc_verbose: Bool, nc_timeout_ms: Int, nc_host: String, nc_port: Int, nc_unix_path: String

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

fn :: is digits only String => Bool
      is digits only s      =
        | (str len s) == 0 = False
        | otherwise        = is digits loop s 0 (str len s)

fn :: is digits loop String Int Int => Bool
      is digits loop s idx len =
        | idx >= len                                                    = True
        | ((str char at s idx) >= 48) and ((str char at s idx) <= 57) = is digits loop s (idx + 1) len
        | otherwise                                                     = False

-- ============================================================================
-- 1. BSD apply(1) — FR-PROC-001
-- ============================================================================

fn :: default apply spec => ApplySpec
      default apply spec =  ApplySpec 37 False 1 False "" []

op :: parse apply args [String] => ApplySpec
      parse apply args args     =  parse apply loop args (default apply spec)

fn :: parse apply loop [String]   ApplySpec => ApplySpec
      parse apply loop []         spec      =  spec
      parse apply loop (arg:rest) spec      =
        | arg == "-d" = parse apply loop rest (spec with a_dry_run = True)
        | arg == "-a" = parse apply flag a rest spec
        | (str starts with arg "-a") and ((str len arg) > 2) = parse apply inline a (str slice arg 2 (str len arg)) rest spec
        | (str starts with arg "-") and (is digits only (str slice arg 1 (str len arg))) = parse apply loop rest (spec with a_group_size = str to int (str slice arg 1 (str len arg)), a_has_group = True)
        | otherwise = finalize apply cmd arg rest spec

fn :: parse apply flag a [String] ApplySpec => ApplySpec
      parse apply flag a [] spec = spec
      parse apply flag a (c:rest) spec = parse apply inline a c rest spec

fn :: parse apply inline a String [String] ApplySpec => ApplySpec
      parse apply inline a c rest spec =
        mc = select magic char c
        res = parse apply loop rest (spec with a_magic = mc)
        res

fn :: select magic char String => Int
      select magic char s      =
        | (str len s) == 0 = 37
        | otherwise        = str char at s 0

fn :: finalize apply cmd String [String] ApplySpec => ApplySpec
      finalize apply cmd cmd rest spec =
        max_p = max magic placeholder cmd 0 (str len cmd) (a_magic spec) 0
        inferred_n = resolve apply group (a_has_group spec) (a_group_size spec) max_p
        res = spec with a_group_size = inferred_n, a_has_group = max_p > 0, a_cmd = cmd, a_args = rest
        res

fn :: resolve apply group Bool Int Int => Int
      resolve apply group True explicit_n max_p = explicit_n
      resolve apply group False explicit_n max_p =
        | max_p > 0 = max_p
        | otherwise = 1

fn :: max magic placeholder String Int Int Int Int => Int
      max magic placeholder cmd idx len magic best =
        | (idx + 1) >= len = best
        | ((str char at cmd idx) == magic) and ((str char at cmd (idx + 1)) >= 49) and ((str char at cmd (idx + 1)) <= 57) = max magic placeholder cmd (idx + 2) len magic (max int best ((str char at cmd (idx + 1)) - 48))
        | otherwise = max magic placeholder cmd (idx + 1) len magic best

fn :: subst apply chunk String Int Int Int [String] String => String
      subst apply chunk cmd idx len magic chunk acc =
        | idx >= len = acc
        | ((idx + 1) < len) and ((str char at cmd idx) == magic) and ((str char at cmd (idx + 1)) >= 49) and ((str char at cmd (idx + 1)) <= 57) = subst apply slot cmd idx len magic chunk acc
        | otherwise = subst apply chunk cmd (idx + 1) len magic chunk (acc + (str slice cmd idx (idx + 1)))

fn :: subst apply slot String Int Int Int [String] String => String
      subst apply slot cmd idx len magic chunk acc =
        slot = ((str char at cmd (idx + 1)) - 49)
        val = str list nth chunk slot
        subst apply chunk cmd (idx + 2) len magic chunk (acc + val)

fn :: build apply cmd line ApplySpec [String] => String
      build apply cmd line spec chunk =
        | a_has_group spec      = subst apply chunk (a_cmd spec) 0 (str len (a_cmd spec)) (a_magic spec) chunk ""
        | (list len chunk) == 0 = a_cmd spec
        | otherwise             = (a_cmd spec) + " " + (join with chunk " ")

fn :: build apply commands ApplySpec [String] [String] => [String]
      build apply commands spec [] acc = acc
      build apply commands spec rem acc =
        gs = max int 1 (a_group_size spec)
        chunk = take str list rem gs []
        rest = drop str list rem gs
        cmd_line = build apply cmd line spec chunk
        build apply commands spec rest (acc :: [cmd_line])

op :: evaluate apply ApplySpec => [String] Int
      evaluate apply spec      =
        cmds = build apply commands spec (a_args spec) []
        run apply cmds cmds (a_dry_run spec) 0

fn :: run apply cmds [String] Bool Int => [String] Int
      run apply cmds cmds True st = (cmds, 0)
      run apply cmds [] False st = ([], st)
      run apply cmds (c:rest) False st =
        rc = shell exec c
        next_st = select nonzero status st rc
        run apply cmds rest False next_st

fn :: select nonzero status Int Int => Int
      select nonzero status 0   rc  =  rc
      select nonzero status st  rc  =  st

-- ============================================================================
-- 2. BSD make(1) — FR-PROC-001
-- ============================================================================

fn :: mv key MakeVar    => String
      mv key (MVar k v) =  k

fn :: mv val MakeVar    => String
      mv val (MVar k v) =  v

fn :: mr target MakeRule => String
      mr target (MRule t deps recs) = t

fn :: mr deps MakeRule => [String]
      mr deps (MRule t deps recs) = deps

fn :: mr recipes MakeRule => [String]
      mr recipes (MRule t deps recs) = recs

fn :: lookup var [MakeVar] String => String
      lookup var []        key    =  sys getenv key
      lookup var (v:rest)  key    =
        | (mv key v) == key = mv val v
        | otherwise         = lookup var rest key

fn :: has var [MakeVar] String => Bool
      has var []        key    =  False
      has var (v:rest)  key    =
        | (mv key v) == key = True
        | otherwise         = has var rest key

fn :: set var [MakeVar] String String [MakeVar] => [MakeVar]
      set var [] key val acc = acc :: [MVar key val]
      set var (v:rest) key val acc =
        | (mv key v) == key = acc :: [MVar key val] :: rest
        | otherwise         = set var rest key val (acc :: [v])

fn :: expand make vars String [MakeVar] String String => String
expand make vars s      vars      cur_target first_dep =
expand make loop s 0 (str len s) vars cur_target first_dep ""

fn :: expand make loop String Int Int [MakeVar] String String String => String
expand make loop s idx len vars cur_target first_dep acc =
  | idx >= len = acc
        | ((str char at s idx) == 36) and ((idx + 1) < len) = expand make dollar s idx len vars cur_target first_dep acc
        | otherwise = expand make loop s (idx + 1) len vars cur_target first_dep (acc + (str slice s idx (idx + 1)))

fn :: expand make dollar String Int Int [MakeVar] String String String => String
expand make dollar s idx len vars cur_target first_dep acc =
  | (str char at s (idx + 1)) == 64  = expand make loop s (idx + 2) len vars cur_target first_dep (acc + cur_target)
        | (str char at s (idx + 1)) == 60  = expand make loop s (idx + 2) len vars cur_target first_dep (acc + first_dep)
        | (str char at s (idx + 1)) == 36  = expand make loop s (idx + 2) len vars cur_target first_dep (acc + "$")
        | (str char at s (idx + 1)) == 40  = expand make delim s (idx + 2) len 41 vars cur_target first_dep acc
        | (str char at s (idx + 1)) == 123 = expand make delim s (idx + 2) len 125 vars cur_target first_dep acc
        | otherwise                        = expand make loop s (idx + 1) len vars cur_target first_dep (acc + "$")

fn :: expand make delim String Int Int Int [MakeVar] String String String => String
expand make delim s idx len close_ch vars cur_target first_dep acc =
  pair = read until char s idx len close_ch ""
  vname = fst pair
  next_idx = snd pair
  val = lookup var vars vname
expand make loop s next_idx len vars cur_target first_dep (acc + val)

fn :: read until char String Int Int Int String => String Int
      read until char s idx len close_ch acc =
        | idx >= len                      = (acc, len)
        | (str char at s idx) == close_ch = (acc, idx + 1)
        | otherwise                       = read until char s (idx + 1) len close_ch (acc + (str slice s idx (idx + 1)))

fn :: find char idx String Int Int Int => Int
      find char idx s idx len target_ch =
        | idx >= len                       = 0 - 1
        | (str char at s idx) == target_ch = idx
        | otherwise                        = find char idx s (idx + 1) len target_ch

op :: parse make args [String] => MakeSpec
      parse make args args     =  parse make args loop args (MakeSpec "Makefile" False False 1 [] [])

fn :: parse make args loop [String] MakeSpec => MakeSpec
      parse make args loop [] spec = spec
      parse make args loop (arg:rest) spec =
        | arg == "-n" = parse make args loop rest (spec with mk_dry_run = True)
        | arg == "-B" = parse make args loop rest (spec with mk_always = True)
        | arg == "-f" = parse make flag f rest spec
        | arg == "-j" = parse make flag j rest spec
        | (find char idx arg 0 (str len arg) 61) > 0 = parse make cli var arg rest spec
        | otherwise = parse make args loop rest (spec with mk_targets = (mk_targets spec) :: [arg])

fn :: parse make flag f [String] MakeSpec => MakeSpec
      parse make flag f [] spec = spec
      parse make flag f (f:rest) spec = parse make args loop rest (spec with mk_file = f)

fn :: parse make flag j [String] MakeSpec => MakeSpec
      parse make flag j [] spec = spec
      parse make flag j (j:rest) spec = parse make args loop rest (spec with mk_jobs = str to int j)

fn :: parse make cli var String [String] MakeSpec => MakeSpec
      parse make cli var arg rest spec =
        eq_idx = find char idx arg 0 (str len arg) 61
        k = trim ws (str slice arg 0 eq_idx)
        v = trim ws (str slice arg (eq_idx + 1) (str len arg))
        next_vars = set var (mk_cli_vars spec) k v []
        res = parse make args loop rest (spec with mk_cli_vars = next_vars)
        res

op :: parse makefile String with [MakeVar] => [MakeRule] [MakeVar] [String]
      parse makefile raw    with cli_vars  =
        lines = split lines raw
        parse makefile lines lines cli_vars cli_vars "" [] [] [] []

fn :: flush current rule String [String] [String] [MakeRule] => [MakeRule]
      flush current rule "" cur_deps cur_recs rules = rules
      flush current rule cur_t cur_deps cur_recs rules = rules :: [MRule cur_t cur_deps cur_recs]

fn :: parse makefile lines [String] [MakeVar] [MakeVar] String [String] [String] [MakeRule] [String] => [MakeRule] [MakeVar] [String]
      parse makefile lines [] vars cli_vars cur_t cur_deps cur_recs rules phonies =
        (flush current rule cur_t cur_deps cur_recs rules, vars, phonies)
      parse makefile lines (line:rest) vars cli_vars cur_t cur_deps cur_recs rules phonies =
        | ((str len line) > 0) and ((str char at line 0) == 9) and ((str len cur_t) > 0) = parse makefile lines rest vars cli_vars cur_t cur_deps (cur_recs :: [str slice line 1 (str len line)]) rules phonies
        | (str len (trim ws line)) == 0 = parse makefile lines rest vars cli_vars cur_t cur_deps cur_recs rules phonies
        | str starts with (trim ws line) "#" = parse makefile lines rest vars cli_vars cur_t cur_deps cur_recs rules phonies
        | otherwise = parse makefile stmt line rest vars cli_vars (flush current rule cur_t cur_deps cur_recs rules) phonies

fn :: parse makefile stmt String [String] [MakeVar] [MakeVar] [MakeRule] [String] => [MakeRule] [MakeVar] [String]
      parse makefile stmt line rest vars cli_vars rules phonies =
        eq_idx = find char idx line 0 (str len line) 61
        col_idx = find char idx line 0 (str len line) 58
        dispatch makefile stmt line rest vars cli_vars rules phonies eq_idx col_idx

fn :: dispatch makefile stmt String [String] [MakeVar] [MakeVar] [MakeRule] [String] Int Int => [MakeRule] [MakeVar] [String]
      dispatch makefile stmt line rest vars cli_vars rules phonies eq_idx col_idx =
        | (eq_idx > 0) and ((col_idx < 0) or (eq_idx < col_idx) or ((col_idx + 1) == eq_idx)) = parse makefile assign line eq_idx rest vars cli_vars rules phonies
        | col_idx > 0 = parse makefile rule line col_idx rest vars cli_vars rules phonies
        | otherwise   = parse makefile lines rest vars cli_vars "" [] [] rules phonies

fn :: parse makefile assign String Int [String] [MakeVar] [MakeVar] [MakeRule] [String] => [MakeRule] [MakeVar] [String]
      parse makefile assign line eq_idx rest vars cli_vars rules phonies =
        prev_ch = str char at line (eq_idx - 1)
        is_cond = prev_ch == 63
        is_imm = prev_ch == 58
        key_end = select assign key end eq_idx (is_cond or is_imm)
        key = trim ws (str slice line 0 key_end)
        raw_val = trim ws (str slice line (eq_idx + 1) (str len line))
        exp_val = expand make vars raw_val vars "" ""
        next_vars = update make var vars cli_vars key exp_val is_cond
        parse makefile lines rest next_vars cli_vars "" [] [] rules phonies

fn :: select assign key end Int Bool => Int
      select assign key end eq_idx False = eq_idx
      select assign key end eq_idx True = eq_idx - 1

fn :: update make var [MakeVar] [MakeVar] String String Bool => [MakeVar]
      update make var vars cli_vars key val is_cond =
        | has var cli_vars key            = vars
        | is_cond and (has var vars key) = vars
        | otherwise                       = set var vars key val []

fn :: parse makefile rule String Int [String] [MakeVar] [MakeVar] [MakeRule] [String] => [MakeRule] [MakeVar] [String]
      parse makefile rule line col_idx rest vars cli_vars rules phonies =
        raw_t = trim ws (str slice line 0 col_idx)
        exp_t = expand make vars raw_t vars "" ""
        raw_deps = trim ws (str slice line (col_idx + 1) (str len line))
        exp_deps = split whitespace (expand make vars raw_deps vars exp_t "")
        record makefile rule exp_t exp_deps rest vars cli_vars rules phonies

fn :: record makefile rule String [String] [String] [MakeVar] [MakeVar] [MakeRule] [String] => [MakeRule] [MakeVar] [String]
      record makefile rule ".PHONY" deps rest vars cli_vars rules phonies =
        parse makefile lines rest vars cli_vars "" [] [] rules (phonies :: deps)
      record makefile rule target deps rest vars cli_vars rules phonies =
        parse makefile lines rest vars cli_vars target deps [] rules phonies

fn :: find rule [MakeRule] String => Bool MakeRule
      find rule []         target =  (False, MRule "" [] [])
      find rule (r:rest)   target =
        | (mr target r) == target = (True, r)
        | otherwise               = find rule rest target

fn :: file mtime neg String => Int
      file mtime neg path   =
        raw = file lstat path
        extract lstat mtime raw

fn :: extract lstat mtime (Bool, String, Int, Int, Int, String, String, Int, String) => Int
      extract lstat mtime (False, k, m, nl, sz, ow, gr, mt, lt) = 0 - 1
      extract lstat mtime (True, k, m, nl, sz, ow, gr, mt, lt) = mt

fn :: needs rebuild String [String] [String] [String] Bool => Bool
      needs rebuild target deps phonies built always =
        | always                             = True
        | list contains str phonies target   = True
        | (file mtime neg target) < 0        = True
        | otherwise                          = check deps newer (file mtime neg target) deps built

fn :: check deps newer Int [String] [String] => Bool
      check deps newer t_mtime [] built = False
      check deps newer t_mtime (d:rest) built =
        | list contains str built d         = True
        | (file mtime neg d) > t_mtime      = True
        | otherwise                         = check deps newer t_mtime rest built

fn :: eval make target String [MakeRule] [MakeVar] [String] MakeSpec [String] [String] [String] => Bool [String] [String]
      eval make target target rules vars phonies spec visiting built out_log =
        | list contains str visiting target = (False, built, out_log :: ["make: Graph cycles through " + target])
        | list contains str built target    = (True, built, out_log)
        | otherwise                         = eval make target rule target (find rule rules target) rules vars phonies spec visiting built out_log

fn :: eval make target rule String (Bool, MakeRule) [MakeRule] [MakeVar] [String] MakeSpec [String] [String] [String] => Bool [String] [String]
      eval make target rule target (False, dummy) rules vars phonies spec visiting built out_log =
        | (file mtime neg target) >= 0 = (True, built, out_log)
        | otherwise                    = (False, built, out_log :: ["make: don't know how to make " + target])
      eval make target rule target (True, rule) rules vars phonies spec visiting built out_log =
        deps = mr deps rule
        dep_res = eval make deps deps rules vars phonies spec (visiting :: [target]) built out_log
        finish make target target rule deps dep_res phonies vars spec

fn :: eval make deps [String] [MakeRule] [MakeVar] [String] MakeSpec [String] [String] [String] => Bool [String] [String]
      eval make deps [] rules vars phonies spec visiting built out_log = (True, built, out_log)
      eval make deps (d:rest) rules vars phonies spec visiting built out_log =
        r1 = eval make target d rules vars phonies spec visiting built out_log
        continue make deps r1 rest rules vars phonies spec visiting

fn :: continue make deps (Bool, [String], [String]) [String] [MakeRule] [MakeVar] [String] MakeSpec [String] => Bool [String] [String]
      continue make deps (False, b1, log1) rest rules vars phonies spec visiting = (False, b1, log1)
      continue make deps (True, b1, log1) rest rules vars phonies spec visiting =
        eval make deps rest rules vars phonies spec visiting b1 log1

fn :: finish make target String MakeRule [String] (Bool, [String], [String]) [String] [MakeVar] MakeSpec => Bool [String] [String]
      finish make target target rule deps (False, b1, log1) phonies vars spec = (False, b1, log1)
      finish make target target rule deps (True, b1, log1) phonies vars spec =
        | not (needs rebuild target deps phonies b1 (mk_always spec)) = (True, b1, log1)
        | otherwise = execute make recipes target (mr recipes rule) (str list nth deps 0) vars (mk_dry_run spec) (b1 :: [target]) log1

fn :: execute make recipes String [String] String [MakeVar] Bool [String] [String] => Bool [String] [String]
      execute make recipes target [] first_dep vars dry_run built log_acc = (True, built, log_acc)
      execute make recipes target (raw_rec:rest) first_dep vars dry_run built log_acc =
        exp_rec = expand make vars raw_rec vars target first_dep
        silent = (str starts with exp_rec "@") and (not dry_run)
        clean_cmd = strip at prefix exp_rec
        next_log = append recipe log log_acc clean_cmd silent
        rc = run recipe cmd clean_cmd dry_run
        check recipe rc rc target rest first_dep vars dry_run built next_log

fn :: strip at prefix String => String
      strip at prefix s      =
        | str starts with s "@" = str slice s 1 (str len s)
        | otherwise             = s

fn :: append recipe log [String] String Bool => [String]
      append recipe log log_acc cmd True = log_acc
      append recipe log log_acc cmd False = log_acc :: [cmd]

fn :: run recipe cmd String Bool  => Int
      run recipe cmd cmd    True  =  0
      run recipe cmd cmd    False =  shell exec cmd

fn :: check recipe rc Int String [String] String [MakeVar] Bool [String] [String] => Bool [String] [String]
      check recipe rc 0 target rest first_dep vars dry_run built next_log =
        execute make recipes target rest first_dep vars dry_run built next_log
      check recipe rc rc target rest first_dep vars dry_run built next_log =
        (False, built, next_log :: ["make: *** [" + target + "] Error " + (int to str rc)])

op :: evaluate make String       with MakeSpec => Bool [String]
      evaluate make raw_makefile with spec     =
        parsed = parse makefile raw_makefile with (mk_cli_vars spec)
        rules = fst3 parsed
        vars = snd3 parsed
        phonies = trd3 parsed
        targets = select make targets (mk_targets spec) rules
        res = eval make deps targets rules vars phonies spec [] [] []
        (fst3 res, trd3 res)

fn :: select make targets [String] [MakeRule]        => [String]
      select make targets []       []                =  []
      select make targets []       (first_rule:rest) =  [mr target first_rule]
      select make targets explicit rules             =  explicit

-- ============================================================================
-- 3. BSD sh(1) Almquist-Style Shell — FR-PROC-002
-- ============================================================================

op :: tokenize shell String => [String]
      tokenize shell src    =  lex sh loop src 0 (str len src) "" False []

fn :: push sh tok String Bool [String] => [String]
      push sh tok cur True acc = acc :: [cur]
      push sh tok cur False acc =
        | (str len cur) == 0 = acc
        | otherwise          = acc :: [cur]

fn :: lex sh loop String Int Int String Bool [String] => [String]
      lex sh loop s idx len cur quoted acc =
        | idx >= len = push sh tok cur quoted acc
        | ((str char at s idx) == 35) and ((str len cur) == 0) and (not quoted) = lex skip comment s (idx + 1) len acc
        | (str char at s idx) == 39 = lex single quote s (idx + 1) len (cur + (char from code 1)) acc
        | (str char at s idx) == 34 = lex double quote s (idx + 1) len cur acc
        | ((str char at s idx) == 92) and ((idx + 1) < len) = lex sh loop s (idx + 2) len (cur + (str slice s (idx + 1) (idx + 2))) True acc
        | (str char at s idx) == 10 = lex sh loop s (idx + 1) len "" False ((push sh tok cur quoted acc) :: [";"])
        | is ws (str char at s idx) = lex sh loop s (idx + 1) len "" False (push sh tok cur quoted acc)
        | ((idx + 3) < len) and ((str slice s idx (idx + 4)) == "2>&1") = lex sh loop s (idx + 4) len "" False ((push sh tok cur quoted acc) :: ["2>&1"])
        | ((idx + 1) < len) and (((str slice s idx (idx + 2)) == "&&") or ((str slice s idx (idx + 2)) == "||") or ((str slice s idx (idx + 2)) == ">>")) = lex sh loop s (idx + 2) len "" False ((push sh tok cur quoted acc) :: [str slice s idx (idx + 2)])
        | ((str char at s idx) == 59) or ((str char at s idx) == 124) or ((str char at s idx) == 40) or ((str char at s idx) == 41) or ((str char at s idx) == 60) or ((str char at s idx) == 62) = lex sh loop s (idx + 1) len "" False ((push sh tok cur quoted acc) :: [str slice s idx (idx + 1)])
        | otherwise = lex sh loop s (idx + 1) len (cur + (str slice s idx (idx + 1))) quoted acc

fn :: lex skip comment String Int Int [String] => [String]
      lex skip comment s idx len acc =
        | idx >= len                = acc
        | (str char at s idx) == 10 = lex sh loop s (idx + 1) len "" False (acc :: [";"])
        | otherwise                 = lex skip comment s (idx + 1) len acc

fn :: lex single quote String Int Int String [String] => [String]
      lex single quote s idx len cur acc =
        | idx >= len                = push sh tok (cur + (char from code 2)) True acc
        | (str char at s idx) == 39 = lex sh loop s (idx + 1) len (cur + (char from code 2)) True acc
        | otherwise                 = lex single quote s (idx + 1) len (cur + (str slice s idx (idx + 1))) acc

fn :: is dq escapable Int => Bool
      is dq escapable c   =  (c == 34) or (c == 92) or (c == 36)

fn :: lex double quote String Int Int String [String] => [String]
      lex double quote s idx len cur acc =
        | idx >= len = push sh tok cur True acc
        | (str char at s idx) == 34 = lex sh loop s (idx + 1) len cur True acc
        | ((str char at s idx) == 92) and ((idx + 1) < len) and (is dq escapable (str char at s (idx + 1))) = lex double quote s (idx + 2) len (cur + (str slice s (idx + 1) (idx + 2))) acc
        | ((str char at s idx) == 92) and ((idx + 1) < len) = lex double quote s (idx + 2) len (cur + (str slice s idx (idx + 2))) acc
        | otherwise = lex double quote s (idx + 1) len (cur + (str slice s idx (idx + 1))) acc

op :: parse shell [String] => ShNode
      parse shell toks     =  fst (parse sh list toks [])

fn :: skip semis [String] => [String]
      skip semis []       =  []
      skip semis (t:rest) =
        | t == ";"  = skip semis rest
        | otherwise = [t] :: rest

fn :: is stop word String [String] => Bool
      is stop word tok    stops    =  list contains str stops tok

fn :: parse sh list [String] [String] => ShNode [String]
      parse sh list toks     stops    =
        cleaned = skip semis toks
        parse sh list nonempty cleaned stops

fn :: parse sh list nonempty [String] [String] => ShNode [String]
      parse sh list nonempty [] stops = (ShSimple [] [], [])
      parse sh list nonempty (t:rest) stops =
        | is stop word t stops = (ShSimple [] [], [t] :: rest)
        | otherwise =
          first = parse sh andor ([t] :: rest)
          parse sh list tail (fst first) (skip semis (snd first)) stops

fn :: parse sh list tail ShNode [String] [String] => ShNode [String]
      parse sh list tail left [] stops = (left, [])
      parse sh list tail left (t:rest) stops =
        | is stop word t stops = (left, [t] :: rest)
        | otherwise            = combine sh seq left (parse sh andor ([t] :: rest)) stops

fn :: combine sh seq ShNode (ShNode, [String]) [String] => ShNode [String]
      combine sh seq left (right, rem) stops =
        parse sh list tail (ShSeq left right) (skip semis rem) stops

fn :: parse sh andor [String] => ShNode [String]
      parse sh andor toks     =
        first = parse sh pipeline toks
        parse sh andor tail (fst first) (snd first)

fn :: parse sh andor tail ShNode [String] => ShNode [String]
      parse sh andor tail left [] = (left, [])
      parse sh andor tail left (t:rest) =
        | t == "&&" = combine sh andnode left (parse sh pipeline (skip semis rest))
        | t == "||" = combine sh ornode left (parse sh pipeline (skip semis rest))
        | otherwise = (left, [t] :: rest)

fn :: combine sh andnode ShNode (ShNode, [String]) => ShNode [String]
      combine sh andnode left   (right, rem)       =  parse sh andor tail (ShAnd left right) rem

fn :: combine sh ornode ShNode (ShNode, [String]) => ShNode [String]
      combine sh ornode left   (right, rem)       =  parse sh andor tail (ShOr left right) rem

fn :: parse sh pipeline [String] => ShNode [String]
      parse sh pipeline toks     =
        first = parse sh command toks
        parse sh pipe tail (fst first) (snd first)

fn :: parse sh pipe tail ShNode [String] => ShNode [String]
      parse sh pipe tail left [] = (left, [])
      parse sh pipe tail left (t:rest) =
        | t == "|"  = combine sh pipe left (parse sh command (skip semis rest))
        | otherwise = (left, [t] :: rest)

fn :: combine sh pipe ShNode (ShNode, [String]) => ShNode [String]
      combine sh pipe left   (right, rem)       =  parse sh pipe tail (ShPipe left right) rem

fn :: parse sh command [String] => ShNode [String]
      parse sh command []       =  (ShSimple [] [], [])
      parse sh command (t:rest) =
        | t == "("     = parse sh subshell rest
        | t == "if"    = parse sh if rest
        | t == "while" = parse sh while rest
        | t == "for"   = parse sh for rest
        | otherwise    = parse sh simple ([t] :: rest) [] []

fn :: consume expected String [String] => [String]
      consume expected exp    []       =  []
      consume expected exp    (t:rest) =
        | t == exp  = rest
        | otherwise = [t] :: rest

fn :: parse sh subshell [String] => ShNode [String]
      parse sh subshell toks     =
        inner = parse sh list toks [")"]
        rem = consume expected ")" (snd inner)
        (ShSubshell (fst inner), rem)

fn :: parse sh if [String] => ShNode [String]
      parse sh if toks     =
        cond_res = parse sh list toks ["then"]
        after_then = consume expected "then" (snd cond_res)
        then_res = parse sh list after_then ["elif", "else", "fi"]
        parse sh if else (fst cond_res) (fst then_res) (snd then_res)

fn :: parse sh if else ShNode ShNode [String] => ShNode [String]
      parse sh if else cond_n then_n [] = (ShIf cond_n then_n (ShSimple [] []), [])
      parse sh if else cond_n then_n (t:rest) =
        | t == "fi"   = (ShIf cond_n then_n (ShSimple [] []), rest)
        | t == "elif" = wrap elif cond_n then_n (parse sh if rest)
        | t == "else" = wrap else cond_n then_n (parse sh list rest ["fi"])
        | otherwise   = (ShIf cond_n then_n (ShSimple [] []), [t] :: rest)

fn :: wrap elif ShNode ShNode (ShNode, [String]) => ShNode [String]
      wrap elif cond_n then_n (elif_node, rem)   =  (ShIf cond_n then_n elif_node, rem)

fn :: wrap else ShNode ShNode (ShNode, [String]) => ShNode [String]
      wrap else cond_n then_n (else_node, rem)   =
        after_fi = consume expected "fi" rem
        (ShIf cond_n then_n else_node, after_fi)

fn :: parse sh while [String] => ShNode [String]
      parse sh while toks     =
        cond_res = parse sh list toks ["do"]
        after_do = consume expected "do" (snd cond_res)
        body_res = parse sh list after_do ["done"]
        after_done = consume expected "done" (snd body_res)
        (ShWhile (fst cond_res) (fst body_res), after_done)

fn :: parse sh for [String]     => ShNode [String]
      parse sh for []           =  (ShSimple [] [], [])
      parse sh for (vname:rest) =
        after_in = consume expected "in" rest
        items_res = collect for items after_in []
        items = fst items_res
        after_semi = skip semis (snd items_res)
        after_do = consume expected "do" after_semi
        body_res = parse sh list after_do ["done"]
        after_done = consume expected "done" (snd body_res)
        (ShFor vname items (fst body_res), after_done)

fn :: collect for items [String] [String] => [String] [String]
      collect for items []       acc      =  (acc, [])
      collect for items (t:rest) acc      =
        | (t == ";") or (t == "do") = (acc, [t] :: rest)
        | otherwise                 = collect for items rest (acc :: [t])

fn :: is cmd delim String => Bool
      is cmd delim t      =
        (t == ";") or (t == "&&") or (t == "||") or (t == "|") or (t == ")") or (t == "then") or (t == "elif") or (t == "else") or (t == "fi") or (t == "do") or (t == "done")

fn :: is var assign tok String => Bool
      is var assign tok s      =
        eq_i = find char idx s 0 (str len s) 61
        (eq_i > 0) and (not (str starts with s "="))

fn :: parse sh simple [String] [String] [Redir] => ShNode [String]
      parse sh simple [] words redirs = (finalize simple node words redirs, [])
      parse sh simple (t:rest) words redirs =
        | is cmd delim t = (finalize simple node words redirs, [t] :: rest)
        | t == "<"       = parse sh redir 1 rest words redirs
        | t == ">"       = parse sh redir 2 rest words redirs
        | t == ">>"      = parse sh redir 3 rest words redirs
        | t == "2>&1"    = parse sh simple rest words (redirs :: [RedirErrToOut])
        | otherwise      = parse sh simple rest (words :: [t]) redirs

fn :: parse sh redir Int [String] [String] [Redir] => ShNode [String]
      parse sh redir kind [] words redirs = (finalize simple node words redirs, [])
      parse sh redir kind (target:rest) words redirs =
        | kind == 1 = parse sh simple rest words (redirs :: [RedirIn target])
        | kind == 2 = parse sh simple rest words (redirs :: [RedirOut target])
        | otherwise = parse sh simple rest words (redirs :: [RedirAppend target])

fn :: finalize simple node [String] [Redir] => ShNode
      finalize simple node (w:[])   []      =
        | is var assign tok w = split assign node w
        | otherwise           = ShSimple [w] []
      finalize simple node words    redirs  =  ShSimple words redirs

fn :: split assign node String => ShNode
      split assign node w      =
        eq_i = find char idx w 0 (str len w) 61
        ShAssign (str slice w 0 eq_i) (str slice w (eq_i + 1) (str len w))

fn :: expand sh word String ShEnv => String
expand sh word w      env   =  expand sh word loop w 0 (str len w) False env ""

fn :: expand sh word loop String Int Int Bool ShEnv String => String
expand sh word loop s idx len in_sq env acc =
  | idx >= len               = acc
        | (str char at s idx) == 1 = expand sh word loop s (idx + 1) len True env acc
        | (str char at s idx) == 2 = expand sh word loop s (idx + 1) len False env acc
        | (not in_sq) and ((str char at s idx) == 36) and ((idx + 1) < len) = expand sh dollar s idx len env acc
        | otherwise                = expand sh word loop s (idx + 1) len in_sq env (acc + (str slice s idx (idx + 1)))

fn :: expand sh dollar String Int Int ShEnv String => String
expand sh dollar s idx len env acc =
  | (str char at s (idx + 1)) == 63 = expand sh word loop s (idx + 2) len False env (acc + (int to str (env_status env)))
        | (str char at s (idx + 1)) == 35 = expand sh word loop s (idx + 2) len False env (acc + (int to str (list len (env_pos env))))
        | (str char at s (idx + 1)) == 64 = expand sh word loop s (idx + 2) len False env (acc + (join with (env_pos env) " "))
        | (str char at s (idx + 1)) == 36 = expand sh word loop s (idx + 2) len False env (acc + "1000")
        | ((str char at s (idx + 1)) >= 49) and ((str char at s (idx + 1)) <= 57) = expand sh word loop s (idx + 2) len False env (acc + (str list nth (env_pos env) ((str char at s (idx + 1)) - 49)))
        | (str char at s (idx + 1)) == 123 = expand sh braced s (idx + 2) len env acc
        | otherwise                        = expand sh plain var s (idx + 1) len env acc

fn :: expand sh braced String Int Int ShEnv String => String
expand sh braced s idx len env acc =
  pair = read until char s idx len 125 ""
  expr = fst pair
  next_idx = snd pair
  val = resolve braced expr expr env
expand sh word loop s next_idx len False env (acc + val)

fn :: resolve braced expr String ShEnv => String
      resolve braced expr expr   env   =
        col_idx = find char idx expr 0 (str len expr) 58
        resolve braced colon expr col_idx env

fn :: lookup sh param String ShEnv => String
      lookup sh param vname  env   =
        | vname == "?" = int to str (env_status env)
        | vname == "#" = int to str (list len (env_pos env))
        | vname == "@" = join with (env_pos env) " "
        | (is digits only vname) and ((str to int vname) >= 1) = str list nth (env_pos env) ((str to int vname) - 1)
        | otherwise    = lookup var (env_vars env) vname

fn :: resolve braced colon String Int ShEnv => String
      resolve braced colon expr col_idx env =
        | (col_idx > 0) and (((col_idx + 1) < (str len expr)) and ((str char at expr (col_idx + 1)) == 45)) = resolve var default (str slice expr 0 col_idx) (str slice expr (col_idx + 2) (str len expr)) env
        | otherwise = lookup sh param expr env

fn :: resolve var default String String ShEnv => String
      resolve var default vname def_val env =
        v = lookup sh param vname env
        select nonempty str v def_val

fn :: select nonempty str String String  => String
      select nonempty str v      def_val =
        | (str len v) > 0 = v
        | otherwise       = def_val

fn :: is var char Int => Bool
      is var char c   =
        ((c >= 65) and (c <= 90)) or ((c >= 97) and (c <= 122)) or ((c >= 48) and (c <= 57)) or (c == 95)

fn :: read var ident String Int Int String => String Int
      read var ident s idx len acc =
        | idx >= len                      = (acc, len)
        | is var char (str char at s idx) = read var ident s (idx + 1) len (acc + (str slice s idx (idx + 1)))
        | otherwise                       = (acc, idx)

fn :: expand sh plain var String Int Int ShEnv String => String
expand sh plain var s idx len env acc =
  pair = read var ident s idx len ""
  vname = fst pair
  next_idx = snd pair
  val = lookup var (env_vars env) vname
expand sh word loop s next_idx len False env (acc + val)

fn :: expand sh words [String] ShEnv [String] => [String]
expand sh words []       env   acc      =  acc
expand sh words (w:rest) env   acc      =
expand sh words rest env (acc :: [expand sh word w env])

op :: evaluate shell ShNode in ShEnv => ShEnv
      evaluate shell node   in env   =
        | env_exited env = env
        | otherwise      = eval sh active node env

fn :: eval sh active ShNode           ShEnv => ShEnv
      eval sh active (ShAssign k v)   env   =
        exp_v = expand sh word v env
        _ = sys setenv k exp_v
        res = env with env_vars = set var (env_vars env) k exp_v [], env_status = 0, env_exited = False
        res
      eval sh active (ShSeq a b)      env   =
        env1 = evaluate shell a in env
        evaluate shell b in env1
      eval sh active (ShAnd a b)      env   =
        env1 = evaluate shell a in env
        eval sh andrhs b env1
      eval sh active (ShOr a b)       env   =
        env1 = evaluate shell a in env
        eval sh orrhs b env1
      eval sh active (ShSubshell sub) env   =
        sub_env = evaluate shell sub in env
        res = env with env_status = env_status sub_env, env_out = env_out sub_env, env_exited = False
        res
      eval sh active (ShIf cond_n then_n else_n) env =
        env_c = evaluate shell cond_n in env
        eval sh if branch then_n else_n env_c
      eval sh active (ShWhile cond_n body_n) env =
        eval sh while loop cond_n body_n env 0
      eval sh active (ShFor vname raw_items body_n) env =
        items = expand sh words raw_items env []
        eval sh for loop vname items body_n env
      eval sh active (ShPipe left right) env =
        eval sh pipe left right env
      eval sh active (ShSimple raw_words redirs) env =
        words = expand sh words raw_words env []
        eval sh simple words redirs env

fn :: eval sh andrhs ShNode ShEnv => ShEnv
      eval sh andrhs b      env1  =
        | (env_status env1) == 0 = evaluate shell b in env1
        | otherwise              = env1

fn :: eval sh orrhs ShNode ShEnv => ShEnv
      eval sh orrhs b      env1  =
        | (env_status env1) != 0 = evaluate shell b in env1
        | otherwise              = env1

fn :: eval sh if branch ShNode ShNode ShEnv => ShEnv
      eval sh if branch then_n else_n env_c =
        | (env_status env_c) == 0 = evaluate shell then_n in env_c
        | otherwise               = evaluate shell else_n in env_c

fn :: eval sh while loop ShNode ShNode ShEnv Int => ShEnv
      eval sh while loop cond_n body_n env steps =
        | (env_exited env) or (steps >= 1000) = env
        | otherwise =
          env_c = evaluate shell cond_n in env
          continue sh while cond_n body_n env_c steps

fn :: continue sh while ShNode ShNode ShEnv Int => ShEnv
      continue sh while cond_n body_n env_c steps =
        | (env_status env_c) != 0 = env_c with env_status = 0
        | otherwise               = eval sh while loop cond_n body_n (evaluate shell body_n in env_c) (steps + 1)

fn :: eval sh for loop String [String] ShNode ShEnv => ShEnv
      eval sh for loop vname [] body_n env = env
      eval sh for loop vname (item:rest) body_n env =
        | env_exited env = env
        | otherwise =
          _ = sys setenv vname item
          env1 = env with env_vars = set var (env_vars env) vname item [], env_exited = False
          env2 = evaluate shell body_n in env1
          (eval sh for loop vname rest body_n env2)

fn :: quote sh words [String] [String] => [String]
      quote sh words []       acc      =  acc
      quote sh words (w:rest) acc      =
        sq = char from code 39
        quote sh words rest (acc :: [sq + w + sq])

fn :: render node to sh ShNode ShEnv => String
      render node to sh (ShSimple raw_words redirs) env =
        words = quote sh words (expand sh words raw_words env []) []
        (join with words " ") + (render redirs redirs env "")
      render node to sh (ShPipe a b) env =
        (render node to sh a env) + " | " + (render node to sh b env)
      render node to sh other env = "true"

fn :: render redirs [Redir]            ShEnv String => String
      render redirs []                 env   acc    =  acc
      render redirs ((RedirIn f):rest) env   acc    =  render redirs rest env (acc + " < " + (expand sh word f env))
      render redirs ((RedirOut f):rest) env  acc    =  render redirs rest env (acc + " > " + (expand sh word f env))
      render redirs ((RedirAppend f):rest) env acc  =  render redirs rest env (acc + " >> " + (expand sh word f env))
      render redirs (RedirErrToOut:rest) env acc    =  render redirs rest env (acc + " 2>&1")

fn :: eval sh pipe ShNode ShNode ShEnv => ShEnv
      eval sh pipe left right env =
        cmd_str = render node to sh (ShPipe left right) env
        run external capture cmd_str env

fn :: run external capture String  ShEnv => ShEnv
      run external capture cmd_str env   =
        pid = sys getpid
        cap_file = "/tmp/.ut_sh_cap_{pid}.out"
        rc = shell exec ("(" + cmd_str + ") > " + cap_file)
        captured = read file cap_file
        _ = sys unlink cap_file
        res = env with env_status = rc, env_out = (env_out env) + captured, env_exited = False
        res

fn :: apply out redirs String [Redir] ShEnv => String
      apply out redirs text [] env = (env_out env) + text
      apply out redirs text ((RedirOut f):rest) env =
        target = expand sh word f env
        _ = write file target text
        apply out redirs "" rest env
      apply out redirs text ((RedirAppend f):rest) env =
        target = expand sh word f env
        _ = append file target text
        apply out redirs "" rest env
      apply out redirs text (other:rest) env = apply out redirs text rest env

fn :: eval sh simple [String] [Redir] ShEnv => ShEnv
      eval sh simple [] redirs env = env
      eval sh simple (cmd:args) redirs env =
        | cmd == "true"                   = env with env_status = 0, env_exited = False
        | cmd == "false"                  = env with env_status = 1, env_exited = False
        | cmd == "echo"                   = eval builtin echo args redirs env
        | cmd == "cd"                     = eval builtin cd args env
        | cmd == "pwd"                    = env with env_status = 0, env_out = apply out redirs ((sys getcwd) + (char from code 10)) redirs env, env_exited = False
        | cmd == "export"                 = eval builtin export args env
        | cmd == "unset"                  = eval builtin unset args env
        | cmd == "set"                    = eval builtin set args env
        | cmd == "exit"                   = eval builtin exit args env
        | (cmd == "test") or (cmd == "[") = eval builtin test cmd args env
        | cmd == "."                      = eval builtin source args env
        | otherwise                       = run external capture ((join with ([cmd] :: args) " ") + (render redirs redirs env "")) env

fn :: eval builtin echo [String] [Redir] ShEnv => ShEnv
      eval builtin echo ("-n":rest) redirs env =
        out = apply out redirs (join with rest " ") redirs env
        res = env with env_status = 0, env_out = out, env_exited = False
        res
      eval builtin echo args redirs env =
        out = apply out redirs ((join with args " ") + (char from code 10)) redirs env
        res = env with env_status = 0, env_out = out, env_exited = False
        res

fn :: eval builtin cd [String]   ShEnv => ShEnv
      eval builtin cd []         env   =
        rc = sys chdir (sys getenv "HOME")
        res = env with env_status = status from sys rc, env_exited = False
        res
      eval builtin cd (dir:rest) env   =
        rc = sys chdir dir
        res = env with env_status = status from sys rc, env_exited = False
        res

fn :: status from sys Int   => Int
      status from sys 0     =  0
      status from sys other =  1

fn :: eval builtin export [String]   ShEnv => ShEnv
      eval builtin export []         env   =  env
      eval builtin export (arg:rest) env   =
        | is var assign tok arg =
          eq_i = find char idx arg 0 (str len arg) 61
          k = str slice arg 0 eq_i
          v = str slice arg (eq_i + 1) (str len arg)
          _ = sys setenv k v
          (eval builtin export rest (env with env_vars = set var (env_vars env) k v [], env_status = 0, env_exited = False))
        | otherwise =
          _ = sys setenv arg (lookup var (env_vars env) arg)
          (eval builtin export rest env)

fn :: remove var [MakeVar] String [MakeVar] => [MakeVar]
      remove var []        key    acc       =  acc
      remove var (v:rest)  key    acc       =
        | (mv key v) == key = remove var rest key acc
        | otherwise         = remove var rest key (acc :: [v])

fn :: eval builtin unset [String] ShEnv => ShEnv
      eval builtin unset []       env   =  env
      eval builtin unset (k:rest) env   =
        _ = sys setenv k ""
        res = eval builtin unset rest (env with env_vars = remove var (env_vars env) k [], env_status = 0, env_exited = False)
        res

fn :: eval builtin set [String]    ShEnv => ShEnv
      eval builtin set ("--":rest) env   =  env with env_pos = rest, env_status = 0, env_exited = False
      eval builtin set args        env   =  env with env_pos = args, env_status = 0, env_exited = False

fn :: eval builtin exit [String]    ShEnv => ShEnv
      eval builtin exit []          env   =  env with env_exited = True
      eval builtin exit (code:rest) env   =  env with env_status = str to int code, env_exited = True

fn :: eval builtin source [String]    ShEnv => ShEnv
      eval builtin source []          env   =  env
      eval builtin source (path:rest) env   =
        script = read file path
        node = parse shell (tokenize shell script)
        evaluate shell node in env

fn :: strip closing bracket [String] [String] => [String]
      strip closing bracket []       acc      =  acc
      strip closing bracket ("]":[]) acc      =  acc
      strip closing bracket (x:xs)   acc      =  strip closing bracket xs (acc :: [x])

fn :: eval builtin test String [String] ShEnv => ShEnv
      eval builtin test cmd raw_args env =
        args = clean test args cmd raw_args
        ok = eval test expr args
        res = env with env_status = bool to status ok, env_exited = False
        res

fn :: clean test args String [String] => [String]
      clean test args "["    raw_args =  strip closing bracket raw_args []
      clean test args other  raw_args =  raw_args

fn :: bool to status Bool  => Int
      bool to status True  =  0
      bool to status False =  1

fn :: eval test expr [String]        => Bool
      eval test expr []              =  False
      eval test expr ("!":rest)      =  not (eval test expr rest)
      eval test expr (x:[])          =  (str len x) > 0
      eval test expr (op_s:arg:[])   =
        | op_s == "-z" = (str len arg) == 0
        | op_s == "-n" = (str len arg) > 0
        | op_s == "-e" = file exists arg
        | op_s == "-f" = is file arg
        | op_s == "-d" = is dir arg
        | otherwise    = False
      eval test expr (a:op_s:b:rest) =
        | (op_s == "=") or (op_s == "==") = a == b
        | op_s == "!="                    = a != b
        | op_s == "-eq"                   = (str to int a) == (str to int b)
        | op_s == "-ne"                   = (str to int a) != (str to int b)
        | op_s == "-lt"                   = (str to int a) < (str to int b)
        | op_s == "-le"                   = (str to int a) <= (str to int b)
        | op_s == "-gt"                   = (str to int a) > (str to int b)
        | op_s == "-ge"                   = (str to int a) >= (str to int b)
        | otherwise                       = False

op :: run shell script String with [String] => String Int
      run shell script src    with pos_args =
        toks = tokenize shell src
        ast = parse shell toks
        init_env = ShEnv [] pos_args 0 "" False
        final_env = evaluate shell ast in init_env
        (env_out final_env, env_status final_env)

-- ============================================================================
-- 4. BSD timeout(1) Implementation (FR-PROC-003)
-- ============================================================================

fn :: parse duration len String Int => Int
      parse duration len raw    len =
        | len == 0 = 0
        | (len > 2) and ((str slice raw (len - 2) len) == "ms") = str to int (str slice raw 0 (len - 2))
        | (len > 1) and ((str slice raw (len - 1) len) == "s")  = (str to int (str slice raw 0 (len - 1))) * 1000
        | (len > 1) and ((str slice raw (len - 1) len) == "m")  = (str to int (str slice raw 0 (len - 1))) * 60000
        | (len > 1) and ((str slice raw (len - 1) len) == "h")  = (str to int (str slice raw 0 (len - 1))) * 3600000
        | (len > 1) and ((str slice raw (len - 1) len) == "d")  = (str to int (str slice raw 0 (len - 1))) * 86400000
        | otherwise = (str to int raw) * 1000

op :: parse duration ms String => Int
      parse duration ms raw    =  parse duration len raw (str len raw)

fn :: default timeout spec => TimeoutSpec
      default timeout spec =  TimeoutSpec "TERM" 10 False 0 "" []

fn :: parse timeout flags [String]           TimeoutSpec => TimeoutSpec
      parse timeout flags []                 spec        =  spec
      parse timeout flags ("--preserve-status":rs) spec  =  parse timeout flags rs (spec with to_preserve = True)
      parse timeout flags ("-s":sig:rs)      spec        =  parse timeout flags rs (spec with to_sig = sig)
      parse timeout flags ("-k":kdur:rs)     spec        =  parse timeout flags rs (spec with to_kill_ms = parse duration ms kdur)
      parse timeout flags (dur:cmd:cmd_args) spec        =  spec with to_dur_ms = (parse duration ms dur), to_cmd = cmd, to_args = cmd_args
      parse timeout flags (dur:[])           spec        =  spec with to_dur_ms = (parse duration ms dur)

op :: parse timeout args [String] => TimeoutSpec
      parse timeout args args     =  parse timeout flags args (default timeout spec)

fn :: map timeout rc Int TimeoutSpec => Int
      map timeout rc rc  spec        =
        | ((rc == 124) or (rc == 137)) and (to_preserve spec) = rc
        | (rc == 124) or (rc == 137)                          = 124
        | rc > 255                                            = (rc / 256) % 256
        | rc < 0                                              = 125
        | otherwise                                           = rc

fn :: quote timeout tok String => String
      quote timeout tok s      =  "'" + s + "'"

fn :: quote timeout list [String] => [String]
      quote timeout list []       =  []
      quote timeout list (x:xs)   =  [quote timeout tok x] :: (quote timeout list xs)

op :: execute timeout TimeoutSpec => String Int
      execute timeout spec        =
        | (str len (to_cmd spec)) == 0 = ("", 125)
        | (to_dur_ms spec) <= 0        = ("", 124)
        | otherwise =
          with timeout (to_dur_ms spec) do
            pid = sys getpid
            cap_file = "/tmp/.ut_timeout_cap_{pid}.out"
            tspec = format timeout sec (to_dur_ms spec)
            kspec = format timeout sec (to_kill_ms spec)
            cmd_line = join with (quote timeout list ([to_cmd spec] :: (to_args spec))) " "
            rc = shell exec ("timeout -s " + (to_sig spec) + " -k " + kspec + " " + tspec + " " + cmd_line + " > " + cap_file + " 2>&1")
            out = read file cap_file
            _ = sys unlink cap_file
            (out, map timeout rc rc spec)

-- ============================================================================
-- 5. BSD nc(1) Netcat Implementation (FR-PROC-003)
-- ============================================================================

fn :: default nc spec => NcSpec
      default nc spec =  NcSpec False False False False False 2000 "127.0.0.1" 0 ""

fn :: parse nc flags [String] [String] NcSpec => NcSpec
      parse nc flags [] pos spec = finalize nc pos pos spec
      parse nc flags ("-l":rest) pos spec = parse nc flags rest pos (spec with nc_listen = True)
      parse nc flags ("-u":rest) pos spec = parse nc flags rest pos (spec with nc_udp = True)
      parse nc flags ("-U":rest) pos spec = parse nc flags rest pos (spec with nc_unix = True)
      parse nc flags ("-z":rest) pos spec = parse nc flags rest pos (spec with nc_zero = True)
      parse nc flags ("-v":rest) pos spec = parse nc flags rest pos (spec with nc_verbose = True)
      parse nc flags ("-zv":rest) pos spec = parse nc flags rest pos (spec with nc_zero = True, nc_verbose = True)
      parse nc flags ("-vz":rest) pos spec = parse nc flags rest pos (spec with nc_zero = True, nc_verbose = True)
      parse nc flags ("-w":sec:rest) pos spec = parse nc flags rest pos (spec with nc_timeout_ms = (str to int sec) * 1000)
      parse nc flags (arg:rest) pos spec = parse nc flags rest (pos :: [arg]) spec

fn :: finalize nc pos [String] NcSpec => NcSpec
      finalize nc pos []       spec   =  spec
      finalize nc pos (p1:[])  spec   =
        | nc_unix spec   = spec with nc_unix_path = p1
        | nc_listen spec = spec with nc_port = str to int p1
        | otherwise      = spec with nc_host = p1
      finalize nc pos (h:p:_)  spec   =  spec with nc_host = h, nc_port = str to int p

op :: parse nc args [String] => NcSpec
      parse nc args args     =  parse nc flags args [] (default nc spec)

fn :: unpack sock ok SockRes     => Socket
      unpack sock ok (SockOk s)  =  s
      unpack sock ok (SockErr _) =  sock tcp

fn :: unpack pair cli SockPairRes        => Socket
      unpack pair cli (SockPairOk cli _) =  cli
      unpack pair cli (SockPairErr _)    =  sock tcp

fn :: unpack pair srv SockPairRes        => Socket
      unpack pair srv (SockPairOk _ srv) =  srv
      unpack pair srv (SockPairErr _)    =  sock tcp

fn :: unpack recv str SockRecvStrRes     => Bool String
      unpack recv str (SockStrOk _ text) =  (True, text)
      unpack recv str (SockStrErr err)   =  (False, err)

op :: nc loopback transfer String  to String port Int  => Bool String
      nc loopback transfer payload to host   port pnum =
        addr_res = sock resolve host host pnum
        nc loopback with addr payload addr_res

fn :: nc loopback with addr String SockAddrRes => Bool String
      nc loopback with addr _ (SockAddrErr msg) = (False, msg)
      nc loopback with addr payload (SockAddrOk addr) =
        srv0 = unpack sock ok (sock bind (sock tcp) addr)
        srv_l = unpack sock ok (sock listen srv0 4)
        pair1 = sock connect pair (sock tcp) srv_l
        cli1 = unpack pair cli pair1
        srv1 = unpack pair srv pair1
        cli2 = unpack sock ok (sock send str cli1 payload)
        pair2 = sock pipe transfer cli2 srv1
        srv2 = unpack pair srv pair2
        unpack recv str (sock recv str srv2 65536)

fn :: format nc probe Int String Int Bool => Int String
      format nc probe 0 target port True =
        (0, "Connection to " + target + " " + (int to str port) + " port [tcp/*] succeeded!" + (char from code 10))
      format nc probe 0 _ _ False = (0, "")
      format nc probe _ target port True =
        (1, "nc: connect to " + target + " port " + (int to str port) + " (tcp) failed" + (char from code 10))
      format nc probe _ _ _ False = (1, "")

fn :: finish nc listen (Int, Int) String  => Int String
      finish nc listen lres       payload =
        | (fst lres) != 0 = (1, "nc: listen failed")
        | otherwise       = (0, payload)

fn :: finish nc zero (Int, Int) NcSpec => Int String
      finish nc zero cres       spec   =
        | (fst cres) == 0 =
          _ = posix close (snd cres)
          format nc probe 0 (nc_host spec) (nc_port spec) (nc_verbose spec)
        | otherwise = format nc probe 1 (nc_host spec) (nc_port spec) (nc_verbose spec)

fn :: finish nc transfer (Bool, String) => Int String
      finish nc transfer t_res          =
        | fst t_res = (0, snd t_res)
        | otherwise = (1, "nc: " + (snd t_res) + (char from code 10))

op :: execute nc NcSpec with String  => Int String
      execute nc spec   with payload =
        | nc_unix spec                                         = execute nc unix spec (nc_unix_path spec) payload
        | (nc_listen spec) and (not (is valid port (nc_port spec))) = (1, "nc: invalid port")
        | nc_listen spec                                       = finish nc listen (sys tcp listen (nc_host spec) (nc_port spec) 4) payload
        | (nc_zero spec) and (not (is valid port (nc_port spec)))   = (1, "nc: invalid port")
        | nc_zero spec                                         = finish nc zero (sys tcp connect (nc_host spec) (nc_port spec) (nc_timeout_ms spec)) spec
        | otherwise                                            = finish nc transfer (nc loopback transfer payload to (nc_host spec) port (nc_port spec))

fn :: finish nc unix conn (Int, Int) String => Int String
      finish nc unix conn cres payload =
        | (fst cres) == 0 =
          _ = posix close (snd cres)
          (0, payload)
        | otherwise = (1, "nc: unix connect failed")

fn :: execute nc unix NcSpec String String => Int String
      execute nc unix spec upath payload =
        | not (is safe unix path upath) = (1, "nc: invalid unix socket path")
        | nc_listen spec                = finish nc listen (sys unix listen upath 4) payload
        | otherwise                     = finish nc unix conn (sys unix connect upath (nc_timeout_ms spec)) payload
