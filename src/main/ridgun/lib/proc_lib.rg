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

type MakeSpec = mk_file: String, mk_dir: String, mk_dry_run: Bool, mk_always: Bool, mk_silent: Bool, mk_ignore_err: Bool, mk_keep_going: Bool, mk_question: Bool, mk_jobs: Int, mk_cli_vars: [MakeVar], mk_targets: [String]

type Redir = RedirIn String | RedirOut String | RedirAppend String | RedirErrToOut | RedirHeredoc String Bool

type ShNode = ShSimple [String] [Redir] | ShPipe ShNode ShNode | ShAnd ShNode ShNode | ShOr ShNode ShNode | ShSeq ShNode ShNode

expand type ShNode = ShSubshell ShNode | ShBrace ShNode | ShNot ShNode | ShIf ShNode ShNode ShNode | ShWhile ShNode ShNode | ShUntil ShNode ShNode | ShFor String [String] Bool ShNode | ShCase String [([String], ShNode)] | ShFuncDef String ShNode | ShAssign String String

type ShFunc = SFunc String ShNode

type ShEnv = env_vars: [MakeVar], env_readonly: [String], env_funcs: [ShFunc], env_locals: [String], env_pos: [String], env_status: Int, env_out: String, env_stdin: String, env_exited: Bool, env_returning: Bool, env_break_cnt: Int, env_cont_cnt: Int, env_errexit: Bool, env_nounset: Bool, env_noglob: Bool, env_noerrexit_ctx: Bool, env_exit_trap: String

type TimeoutSpec = to_sig: String, to_kill_ms: Int, to_preserve: Bool, to_foreground: Bool, to_invalid: Bool, to_dur_ms: Int, to_cmd: String, to_args: [String]

type NcSpec = nc_listen: Bool, nc_udp: Bool, nc_unix: Bool, nc_zero: Bool, nc_verbose: Bool, nc_nodns: Bool, nc_timeout_ms: Int, nc_host: String, nc_port: Int, nc_port_end: Int, nc_unix_path: String

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

fn :: dedup str list [String] [String] => [String]
      dedup str list []       acc      =  acc
      dedup str list (x:rest) acc      =
        | list contains str acc x = dedup str list rest acc
        | otherwise               = dedup str list rest (acc :: [x])

fn :: target stem of String => String
      target stem of t      =
        slice stem at t (find last char idx t ((str len t) - 1) 46)

fn :: slice stem at String Int   => String
      slice stem at t      dot_i =
        | dot_i > 0 = str slice t 0 dot_i
        | otherwise = t

fn :: find last char idx String Int Int => Int
      find last char idx s idx target_ch =
        | idx < 0                          = 0 - 1
        | (str char at s idx) == target_ch = idx
        | otherwise                        = find last char idx s (idx - 1) target_ch

fn :: expand make full String [MakeVar] String String String String Int => String
expand make full s vars cur_target first_dep all_deps stem depth =
  | depth >= 8 = s
        | otherwise  = expand make loop s 0 (str len s) vars cur_target first_dep all_deps stem depth ""

fn :: expand make vars String [MakeVar] String String => String
expand make vars s vars cur_target first_dep =
expand make full s vars cur_target first_dep first_dep (target stem of cur_target) 0

fn :: expand make loop String Int Int [MakeVar] String String String String Int String => String
expand make loop s idx len vars cur_target first_dep all_deps stem depth acc =
  | idx >= len = acc
        | ((str char at s idx) == 36) and ((idx + 1) < len) = expand make dollar s idx len vars cur_target first_dep all_deps stem depth acc
        | otherwise = expand make loop s (idx + 1) len vars cur_target first_dep all_deps stem depth (acc + (str slice s idx (idx + 1)))

fn :: expand make dollar String Int Int [MakeVar] String String String String Int String => String
expand make dollar s idx len vars cur_target first_dep all_deps stem depth acc =
  | (str char at s (idx + 1)) == 64  = expand make loop s (idx + 2) len vars cur_target first_dep all_deps stem depth (acc + cur_target)
        | (str char at s (idx + 1)) == 60  = expand make loop s (idx + 2) len vars cur_target first_dep all_deps stem depth (acc + first_dep)
        | (str char at s (idx + 1)) == 94  = expand make loop s (idx + 2) len vars cur_target first_dep all_deps stem depth (acc + all_deps)
        | (str char at s (idx + 1)) == 63  = expand make loop s (idx + 2) len vars cur_target first_dep all_deps stem depth (acc + all_deps)
        | (str char at s (idx + 1)) == 42  = expand make loop s (idx + 2) len vars cur_target first_dep all_deps stem depth (acc + stem)
        | (str char at s (idx + 1)) == 36  = expand make loop s (idx + 2) len vars cur_target first_dep all_deps stem depth (acc + "$")
        | (str char at s (idx + 1)) == 40  = expand make delim s (idx + 2) len 41 vars cur_target first_dep all_deps stem depth acc
        | (str char at s (idx + 1)) == 123 = expand make delim s (idx + 2) len 125 vars cur_target first_dep all_deps stem depth acc
        | otherwise                        = expand make loop s (idx + 1) len vars cur_target first_dep all_deps stem depth (acc + "$")

fn :: expand make delim String Int Int Int [MakeVar] String String String String Int String => String
expand make delim s idx len close_ch vars cur_target first_dep all_deps stem depth acc =
  pair = read until char s idx len close_ch ""
  vname = fst pair
  next_idx = snd pair
  raw_val = lookup var vars vname
  val = expand make full raw_val vars cur_target first_dep all_deps stem (depth + 1)
expand make loop s next_idx len vars cur_target first_dep all_deps stem depth (acc + val)

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
      parse make args args     =  parse make args loop args (MakeSpec "Makefile" "" False False False False False False 1 [] [])

fn :: parse make args loop [String] MakeSpec => MakeSpec
      parse make args loop [] spec = spec
      parse make args loop (arg:rest) spec =
        | arg == "-n" = parse make args loop rest (spec with mk_dry_run = True)
        | arg == "-B" = parse make args loop rest (spec with mk_always = True)
        | arg == "-s" = parse make args loop rest (spec with mk_silent = True)
        | arg == "-i" = parse make args loop rest (spec with mk_ignore_err = True)
        | arg == "-k" = parse make args loop rest (spec with mk_keep_going = True)
        | arg == "-q" = parse make args loop rest (spec with mk_question = True)
        | arg == "-f" = parse make flag f rest spec
        | arg == "-C" = parse make flag c rest spec
        | arg == "-j" = parse make flag j rest spec
        | (find char idx arg 0 (str len arg) 61) > 0 = parse make cli var arg rest spec
        | otherwise = parse make args loop rest (spec with mk_targets = (mk_targets spec) :: [arg])

fn :: parse make flag f [String] MakeSpec => MakeSpec
      parse make flag f [] spec = spec
      parse make flag f (f:rest) spec = parse make args loop rest (spec with mk_file = f)

fn :: parse make flag c [String] MakeSpec => MakeSpec
      parse make flag c [] spec = spec
      parse make flag c (d:rest) spec = parse make args loop rest (spec with mk_dir = d)

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

fn :: mk ends with bs String => Bool
      mk ends with bs s      =
        | (str len s) == 0                        = False
        | (str char at s ((str len s) - 1)) == 92 = True
        | otherwise                               = False

fn :: join make continuations [String] String [String] => [String]
      join make continuations [] cur acc =
        | (str len cur) == 0 = acc
        | otherwise          = acc :: [cur]
      join make continuations (line:rest) cur acc =
        | mk ends with bs line =
          trimmed = str slice line 0 ((str len line) - 1)
          next_piece = select cont piece cur trimmed
      join make continuations rest next_piece acc
        | otherwise =
          next_line = select cont final cur line
          join make continuations rest "" (acc :: [next_line])

fn :: select cont piece String String  => String
      select cont piece ""     trimmed =  trim right ws trimmed
      select cont piece cur    trimmed =  cur + " " + (trim ws trimmed)

fn :: select cont final String String => String
      select cont final ""     line   =  line
      select cont final cur    line   =  cur + " " + (trim ws line)

fn :: trim right ws String => String
      trim right ws s      =
        right = trim right idx s 0 (str len s)
        str slice s 0 right

fn :: expand make includes [String] [String] => [String]
expand make includes [] acc = acc
expand make includes (line:rest) acc =
expand make include line line (trim ws line) rest acc

fn :: expand make include line String String [String] [String] => [String]
expand make include line line tline rest acc =
  | str starts with tline "include " =
    inc_path = trim ws (str slice tline 8 (str len tline))
    inc_lines = join make continuations (split lines (read file inc_path)) "" []
expand make includes (inc_lines :: rest) acc
| str starts with tline "-include " =
  inc_path = trim ws (str slice tline 9 (str len tline))
  inc_lines = load optional include inc_path
expand make includes (inc_lines :: rest) acc
| otherwise = expand make includes rest (acc :: [line])

fn :: load optional include String => [String]
      load optional include path   =
        | file exists path = join make continuations (split lines (read file path)) "" []
        | otherwise        = []

op :: parse makefile String with [MakeVar] => [MakeRule] [MakeVar] [String]
      parse makefile raw    with cli_vars  =
        lines = expand make includes (join make continuations (split lines raw) "" []) []
        parse makefile lines lines cli_vars cli_vars "" [] [] [] []

fn :: flush current rule String [String] [String] [MakeRule] => [MakeRule]
      flush current rule "" cur_deps cur_recs rules = rules
      flush current rule cur_t cur_deps cur_recs rules = merge rule into list rules (MRule cur_t cur_deps cur_recs)

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
        is_app = prev_ch == 43
        key_end = select assign key end eq_idx (is_cond or is_imm or is_app)
        key = trim ws (str slice line 0 key_end)
        raw_val = trim ws (str slice line (eq_idx + 1) (str len line))
        stored_val = compute make assign val vars key raw_val is_imm is_app
        next_vars = update make var vars cli_vars key stored_val is_cond
        parse makefile lines rest next_vars cli_vars "" [] [] rules phonies

fn :: compute make assign val [MakeVar] String String Bool Bool => String
      compute make assign val vars key raw_val is_imm is_app =
        | is_app =
          prev = lookup var vars key
          append make var val prev raw_val
        | is_imm    = expand make vars raw_val vars "" ""
        | otherwise = raw_val

fn :: append make var val String String => String
      append make var val "" raw_val = raw_val
      append make var val prev raw_val = prev + " " + raw_val

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
      record makefile rule ".SUFFIXES" _ rest vars cli_vars rules phonies =
        parse makefile lines rest vars cli_vars "" [] [] rules phonies
      record makefile rule target deps rest vars cli_vars rules phonies =
        norm_pair = normalize suffix rule target deps
        parse makefile lines rest vars cli_vars (fst norm_pair) (snd norm_pair) [] rules phonies

fn :: normalize suffix rule String [String] => String [String]
      normalize suffix rule target deps     =
        | (str starts with target ".") and ((list len deps) == 0) =
          second_dot = find char idx target 1 (str len target) 46
          convert suffix rule target second_dot
        | otherwise = (target, deps)

fn :: convert suffix rule String Int        => String [String]
      convert suffix rule target second_dot =
        | second_dot > 1 =
          src_ext = str slice target 0 second_dot
          dst_ext = str slice target second_dot (str len target)
          ("%" + dst_ext, ["%" + src_ext])
        | otherwise = (target, [])

fn :: merge rule into list [MakeRule] MakeRule => [MakeRule]
      merge rule into list rules (MRule t deps recs) =
        merge rule step (fst (find exact rule rules t)) rules t deps recs

fn :: merge rule step Bool [MakeRule] String [String] [String] => [MakeRule]
      merge rule step True rules t deps recs = update existing rule rules t deps recs []
      merge rule step False rules t deps recs = rules :: [MRule t deps recs]

fn :: update existing rule [MakeRule] String [String] [String] [MakeRule] => [MakeRule]
      update existing rule [] _ _ _ acc = acc
      update existing rule (r:rest) t deps recs acc =
        | (mr target r) == t =
          merged_deps = (mr deps r) :: deps
          merged_recs = select merged recs (mr recipes r) recs
          acc :: [MRule (mr target r) merged_deps merged_recs] :: rest
        | otherwise = update existing rule rest t deps recs (acc :: [r])

fn :: select merged recs [String] [String] => [String]
      select merged recs rrecs    []       =  rrecs
      select merged recs _        recs     =  recs

fn :: find exact rule [MakeRule] String => Bool MakeRule
      find exact rule []         _      =  (False, MRule "" [] [])
      find exact rule (r:rest)   target =
        | (mr target r) == target = (True, r)
        | otherwise               = find exact rule rest target

fn :: match pattern target String String => Bool String
      match pattern target pat    target =
        match pattern at pat target (find char idx pat 0 (str len pat) 37)

fn :: match pattern at String String Int => Bool String
      match pattern at pat target pct_i =
        | pct_i < 0 = (False, "")
        | otherwise =
          pref = str slice pat 0 pct_i
          suff = str slice pat (pct_i + 1) (str len pat)
          plen = str len pref
          slen = str len suff
          tlen = str len target
          check pattern match target pref suff plen slen tlen

fn :: check pattern match String String String Int Int Int => Bool String
      check pattern match target pref suff plen slen tlen =
        | tlen < (plen + slen) = (False, "")
        | (plen > 0) and (not (str starts with target pref)) = (False, "")
        | (slen > 0) and ((str slice target (tlen - slen) tlen) != suff) = (False, "")
        | otherwise = (True, str slice target plen (tlen - slen))

fn :: subst pattern stem String String => String
      subst pattern stem dep    stem   =
        subst pattern at dep stem (find char idx dep 0 (str len dep) 37)

fn :: subst pattern at String String Int => String
      subst pattern at dep stem pct_i =
        | pct_i < 0 = dep
        | otherwise = (str slice dep 0 pct_i) + stem + (str slice dep (pct_i + 1) (str len dep))

fn :: subst pattern deps [String] String [String] => [String]
      subst pattern deps [] _ acc = acc
      subst pattern deps (d:rest) stem acc =
        subst pattern deps rest stem (acc :: [subst pattern stem d stem])

fn :: find pattern rule [MakeRule] String => Bool MakeRule
      find pattern rule []         _      =  (False, MRule "" [] [])
      find pattern rule (r:rest)   target =
        find pattern step (match pattern target (mr target r) target) r rest target

fn :: find pattern step (Bool, String) MakeRule [MakeRule] String => Bool MakeRule
      find pattern step (True, stem) r _ target =
        (True, MRule target (subst pattern deps (mr deps r) stem []) (mr recipes r))
      find pattern step (False, _) _ rest target =
        find pattern rule rest target

fn :: find rule [MakeRule] String => Bool MakeRule
      find rule rules      target =
        find rule step (find exact rule rules target) rules target

fn :: find rule step (Bool, MakeRule) [MakeRule] String => Bool MakeRule
      find rule step (True, r) rules target = resolve rule with pattern r rules target
      find rule step (False, _) rules target = find pattern rule rules target

fn :: resolve rule with pattern MakeRule [MakeRule] String => Bool MakeRule
      resolve rule with pattern r rules target =
        | (list len (mr recipes r)) > 0 = (True, r)
        | otherwise                     =
          pat = find pattern rule rules target
          merge explicit and pattern (mr target r) (mr deps r) pat

fn :: merge explicit and pattern String [String] (Bool, MakeRule) => Bool MakeRule
      merge explicit and pattern t deps (False, _) = (True, MRule t deps [])
      merge explicit and pattern t deps (True, pr) =
        (True, MRule t (dedup str list (deps :: (mr deps pr)) []) (mr recipes pr))

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
        dep_res = eval make deps deps rules vars phonies spec (visiting :: [target]) built out_log False
        finish make target target rule deps dep_res phonies vars spec

fn :: eval make deps [String] [MakeRule] [MakeVar] [String] MakeSpec [String] [String] [String] Bool => Bool [String] [String]
      eval make deps [] _ _ _ _ _ built out_log any_failed = (not any_failed, built, out_log)
      eval make deps (d:rest) rules vars phonies spec visiting built out_log any_failed =
        r1 = eval make target d rules vars phonies spec visiting built out_log
        continue make deps r1 rest rules vars phonies spec visiting any_failed

fn :: continue make deps (Bool, [String], [String]) [String] [MakeRule] [MakeVar] [String] MakeSpec [String] Bool => Bool [String] [String]
      continue make deps (False, b1, log1) rest rules vars phonies spec visiting _ =
        | mk_keep_going spec = eval make deps rest rules vars phonies spec visiting b1 log1 True
        | otherwise          = (False, b1, log1)
      continue make deps (True, b1, log1) rest rules vars phonies spec visiting any_failed =
        eval make deps rest rules vars phonies spec visiting b1 log1 any_failed

fn :: finish make target String MakeRule [String] (Bool, [String], [String]) [String] [MakeVar] MakeSpec => Bool [String] [String]
      finish make target _ _ _ (False, b1, log1) _ _ _ = (False, b1, log1)
      finish make target target rule deps (True, b1, log1) phonies vars spec =
        | not (needs rebuild target deps phonies b1 (mk_always spec)) = (True, b1, log1)
        | mk_question spec                                            = (False, b1, log1)
        | otherwise                                                   = execute make recipes target (mr recipes rule) deps vars spec (b1 :: [target]) log1

fn :: strip recipe prefixes String Bool Bool => String Bool Bool
      strip recipe prefixes s silent ign =
        | str starts with s "@" = strip recipe prefixes (str slice s 1 (str len s)) True ign
        | str starts with s "-" = strip recipe prefixes (str slice s 1 (str len s)) silent True
        | str starts with s "+" = strip recipe prefixes (str slice s 1 (str len s)) silent ign
        | otherwise             = (s, silent, ign)

fn :: execute make recipes String [String] [String] [MakeVar] MakeSpec [String] [String] => Bool [String] [String]
      execute make recipes _ [] _ _ _ built log_acc = (True, built, log_acc)
      execute make recipes target (raw_rec:rest) deps vars spec built log_acc =
        first_dep = str list nth deps 0
        all_deps = join with (dedup str list deps []) " "
        stem = target stem of target
        exp_rec = expand make full raw_rec vars target first_dep all_deps stem 0
        pref_info = strip recipe prefixes exp_rec False False
        clean_cmd = fst3 pref_info
        rec_silent = snd3 pref_info
        rec_ignore = trd3 pref_info
        should_hide = ((rec_silent or (mk_silent spec)) and (not (mk_dry_run spec)))
        next_log = append recipe log log_acc clean_cmd should_hide
        rc = run recipe cmd clean_cmd (mk_dry_run spec)
        check recipe rc rc rec_ignore target rest deps vars spec built next_log

fn :: append recipe log [String] String Bool => [String]
      append recipe log log_acc _ True = log_acc
      append recipe log log_acc cmd False = log_acc :: [cmd]

fn :: run recipe cmd String Bool  => Int
      run recipe cmd _      True  =  0
      run recipe cmd cmd    False =  shell exec cmd

fn :: check recipe rc Int Bool String [String] [String] [MakeVar] MakeSpec [String] [String] => Bool [String] [String]
      check recipe rc 0 _ target rest deps vars spec built next_log =
        execute make recipes target rest deps vars spec built next_log
      check recipe rc _ True target rest deps vars spec built next_log =
        execute make recipes target rest deps vars spec built next_log
      check recipe rc rc False target rest deps vars spec built next_log =
        | mk_ignore_err spec = execute make recipes target rest deps vars spec built next_log
        | otherwise          = (False, built, next_log :: ["make: *** [" + target + "] Error " + (int to str rc)])

op :: evaluate make String       with MakeSpec => Bool [String]
      evaluate make raw_makefile with spec     =
        parsed = parse makefile raw_makefile with (mk_cli_vars spec)
        rules = fst3 parsed
        vars = snd3 parsed
        phonies = trd3 parsed
        targets = select make targets (mk_targets spec) rules
        res = eval make deps targets rules vars phonies spec [] [] [] False
        (fst3 res, trd3 res)

fn :: select make targets [String] [MakeRule]        => [String]
      select make targets []       []                =  []
      select make targets []       (first_rule:rest) =  [mr target first_rule]
      select make targets explicit rules             =  explicit

-- ============================================================================
-- 3. BSD sh(1) Almquist-Style Shell — FR-PROC-002
-- ============================================================================

fn :: strip sh continuations String Int Int Bool String => String
      strip sh continuations s idx len in_sq acc =
        | idx >= len = acc
        | (not in_sq) and (((str char at s idx) == 92) and (((idx + 1) < len) and ((str char at s (idx + 1)) == 10))) =
      strip sh continuations s (idx + 2) len False acc
        | (not in_sq) and (((str char at s idx) == 92) and ((idx + 1) < len)) =
      strip sh continuations s (idx + 2) len False (acc + (str slice s idx (idx + 2)))
        | (str char at s idx) == 39 =
      strip sh continuations s (idx + 1) len (not in_sq) (acc + "'")
        | otherwise =
          strip sh continuations s (idx + 1) len in_sq (acc + (str slice s idx (idx + 1)))

fn :: read balanced arith String Int Int Int String => String Int
      read balanced arith s idx len depth acc =
        | idx >= len = (acc, len)
        | (((idx + 1) < len) and ((str slice s idx (idx + 2)) == "))")) and (depth == 1) =
          (acc + "))", idx + 2)
        | (str char at s idx) == 40 =
      read balanced arith s (idx + 1) len (depth + 1) (acc + "(")
        | (str char at s idx) == 41 =
      read balanced arith s (idx + 1) len (depth - 1) (acc + ")")
        | otherwise =
          read balanced arith s (idx + 1) len depth (acc + (str slice s idx (idx + 1)))

fn :: read balanced subst String Int Int Int String => String Int
      read balanced subst s idx len depth acc =
        | idx >= len = (acc, len)
        | ((str char at s idx) == 92) and ((idx + 1) < len) =
      read balanced subst s (idx + 2) len depth (acc + (str slice s idx (idx + 2)))
        | ((str char at s idx) == 41) and (depth == 1) =
          (acc + ")", idx + 1)
        | (str char at s idx) == 40 =
      read balanced subst s (idx + 1) len (depth + 1) (acc + "(")
        | (str char at s idx) == 41 =
      read balanced subst s (idx + 1) len (depth - 1) (acc + ")")
        | otherwise =
          read balanced subst s (idx + 1) len depth (acc + (str slice s idx (idx + 1)))

fn :: read balanced brace String Int Int Int String => String Int
      read balanced brace s idx len depth acc =
        | idx >= len = (acc, len)
        | ((str char at s idx) == 92) and ((idx + 1) < len) =
      read balanced brace s (idx + 2) len depth (acc + (str slice s idx (idx + 2)))
        | ((str char at s idx) == 125) and (depth == 1) =
          (acc + "}", idx + 1)
        | (str char at s idx) == 123 =
      read balanced brace s (idx + 1) len (depth + 1) (acc + "{")
        | (str char at s idx) == 125 =
      read balanced brace s (idx + 1) len (depth - 1) (acc + "}")
        | otherwise =
          read balanced brace s (idx + 1) len depth (acc + (str slice s idx (idx + 1)))

fn :: read backtick tok String Int Int String => String Int
      read backtick tok s idx len acc =
        | idx >= len = (acc, len)
        | ((str char at s idx) == 92) and ((idx + 1) < len) =
      read backtick tok s (idx + 2) len (acc + (str slice s idx (idx + 2)))
        | (str char at s idx) == 96 =
          (acc + "`", idx + 1)
        | otherwise =
          read backtick tok s (idx + 1) len (acc + (str slice s idx (idx + 1)))

fn :: has quote char String => Bool
      has quote char s      =
        ((find char idx s 0 (str len s) 39) >= 0) or ((find char idx s 0 (str len s) 34) >= 0)

fn :: strip quote chars String Int Int String => String
      strip quote chars s idx len acc =
        | idx >= len = acc
        | ((str char at s idx) == 39) or ((str char at s idx) == 34) =
      strip quote chars s (idx + 1) len acc
        | otherwise =
          strip quote chars s (idx + 1) len (acc + (str slice s idx (idx + 1)))

fn :: skip inline spaces String Int Int => Int
      skip inline spaces s idx len =
        | idx >= len = len
        | ((str char at s idx) == 32) or ((str char at s idx) == 9) =
      skip inline spaces s (idx + 1) len
        | otherwise = idx

fn :: read hd delim String Int Int String => String Int
      read hd delim s idx len acc =
        | idx >= len = (acc, len)
        | (is ws (str char at s idx)) or ((str char at s idx) == 59) = (acc, idx)
        | otherwise = read hd delim s (idx + 1) len (acc + (str slice s idx (idx + 1)))

fn :: find line end String Int Int => Int
      find line end s idx len =
        | idx >= len                = len
        | (str char at s idx) == 10 = idx
        | otherwise                 = find line end s (idx + 1) len

fn :: strip leading tabs String Int Int => String
      strip leading tabs s idx len =
        | idx >= len               = ""
        | (str char at s idx) == 9 = strip leading tabs s (idx + 1) len
        | otherwise                = str slice s idx len

fn :: read heredoc body String Int Int String Bool String => String Int
      read heredoc body s idx len delim strip_tabs acc =
        | idx >= len = (acc, len)
        | otherwise  =
          read heredoc line s idx len (find line end s idx len) delim strip_tabs acc

fn :: read heredoc line String Int Int Int String Bool String => String Int
      read heredoc line s idx len nl_idx delim strip_tabs acc =
        raw_line = str slice s idx nl_idx
        cmp_line = select hd cmp line raw_line strip_tabs
        finish heredoc line s nl_idx len raw_line cmp_line delim strip_tabs acc

fn :: select hd cmp line String Bool => String
      select hd cmp line line True = strip leading tabs line 0 (str len line)
      select hd cmp line line False = line

fn :: finish heredoc line String Int Int String String String Bool String => String Int
      finish heredoc line s nl_idx len _ cmp_line delim strip_tabs acc =
        | cmp_line == delim = (acc, min int len (nl_idx + 1))
        | otherwise         =
          read heredoc body s (min int len (nl_idx + 1)) len delim strip_tabs (acc + cmp_line + (char from code 10))

fn :: min int Int Int => Int
      min int a   b   =
        | a < b     = a
        | otherwise = b

fn :: select hd tag Bool  => String
      select hd tag True  =  "<<HEREDOC_LIT"
      select hd tag False =  "<<HEREDOC_EXP"

op :: tokenize shell String => [String]
      tokenize shell src    =
        cleaned = strip sh continuations src 0 (str len src) False ""
        lex sh loop cleaned 0 (str len cleaned) "" False []

fn :: push sh tok String Bool [String] => [String]
      push sh tok cur True acc = acc :: [cur]
      push sh tok cur False acc =
        | (str len cur) == 0 = acc
        | otherwise          = acc :: [cur]

fn :: lex sh loop String Int Int String Bool [String] => [String]
      lex sh loop s idx len cur quoted acc =
        | idx >= len = push sh tok cur quoted acc
        | ((str char at s idx) == 35) and (((str len cur) == 0) and (not quoted)) =
          lex skip comment s (idx + 1) len acc
        | (str char at s idx) == 39 =
          lex single quote s (idx + 1) len (cur + (char from code 1)) acc
        | (str char at s idx) == 34 =
          lex double quote s (idx + 1) len (cur + (char from code 3)) acc
        | ((idx + 2) < len) and ((str slice s idx (idx + 3)) == "$((") =
          lex after balanced s len cur acc (read balanced arith s (idx + 3) len 1 "$((")
        | ((idx + 1) < len) and ((str slice s idx (idx + 2)) == "$(") =
          lex after balanced s len cur acc (read balanced subst s (idx + 2) len 1 "$(")
        | ((idx + 1) < len) and ((str slice s idx (idx + 2)) == "$\{") =
          lex after balanced s len cur acc (read balanced brace s (idx + 2) len 1 "$\{")
        | (str char at s idx) == 96 =
          lex after balanced s len cur acc (read backtick tok s (idx + 1) len "`")
        | ((str char at s idx) == 92) and ((idx + 1) < len) =
      lex sh loop s (idx + 2) len (cur + (char from code 1) + (str slice s (idx + 1) (idx + 2)) + (char from code 2)) True acc
        | (str char at s idx) == 10 =
      lex sh loop s (idx + 1) len "" False ((push sh tok cur quoted acc) :: [";"])
        | is ws (str char at s idx) =
      lex sh loop s (idx + 1) len "" False (push sh tok cur quoted acc)
        | ((idx + 3) < len) and ((str slice s idx (idx + 4)) == "2>&1") =
      lex sh loop s (idx + 4) len "" False ((push sh tok cur quoted acc) :: ["2>&1"])
        | ((idx + 1) < len) and ((str slice s idx (idx + 2)) == "<<") =
          lex heredoc s (idx + 2) len (push sh tok cur quoted acc)
        | ((idx + 1) < len) and (((str slice s idx (idx + 2)) == "&&") or (((str slice s idx (idx + 2)) == "||") or (((str slice s idx (idx + 2)) == ">>") or ((str slice s idx (idx + 2)) == ";;")))) =
      lex sh loop s (idx + 2) len "" False ((push sh tok cur quoted acc) :: [str slice s idx (idx + 2)])
        | ((str char at s idx) == 59) or (((str char at s idx) == 124) or (((str char at s idx) == 40) or (((str char at s idx) == 41) or (((str char at s idx) == 60) or ((str char at s idx) == 62))))) =
      lex sh loop s (idx + 1) len "" False ((push sh tok cur quoted acc) :: [str slice s idx (idx + 1)])
        | otherwise =
          lex sh loop s (idx + 1) len (cur + (str slice s idx (idx + 1))) quoted acc

fn :: lex after balanced String Int String [String] (String, Int) => [String]
      lex after balanced s len cur acc (frag, next_idx) =
        lex sh loop s next_idx len (cur + frag) True acc

fn :: lex heredoc String Int Int [String] => [String]
      lex heredoc s idx len acc =
        | (idx < len) and ((str char at s idx) == 45) =
          lex heredoc delim s (skip inline spaces s (idx + 1) len) len True acc
        | otherwise =
          lex heredoc delim s (skip inline spaces s idx len) len False acc

fn :: lex heredoc delim String Int Int Bool [String] => [String]
      lex heredoc delim s idx len strip_tabs acc =
        dp = read hd delim s idx len ""
        raw_delim = fst dp
        is_q = has quote char raw_delim
        clean_delim = strip quote chars raw_delim 0 (str len raw_delim) ""
        nl_idx = find line end s (snd dp) len
        bp = read heredoc body s (min int len (nl_idx + 1)) len clean_delim strip_tabs ""
        hd_tag = select hd tag is_q
        lex sh loop s (snd bp) len "" False (acc :: [hd_tag, fst bp, ";"])

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
      is dq escapable c   =  (c == 34) or ((c == 92) or ((c == 36) or (c == 96)))

fn :: lex double quote String Int Int String [String] => [String]
      lex double quote s idx len cur acc =
        | idx >= len = push sh tok (cur + (char from code 4)) True acc
        | (str char at s idx) == 34 = lex sh loop s (idx + 1) len (cur + (char from code 4)) True acc
        | ((idx + 2) < len) and ((str slice s idx (idx + 3)) == "$((") =
          lex dq after balanced s len cur acc (read balanced arith s (idx + 3) len 1 "$((")
        | ((idx + 1) < len) and ((str slice s idx (idx + 2)) == "$(") =
          lex dq after balanced s len cur acc (read balanced subst s (idx + 2) len 1 "$(")
        | ((idx + 1) < len) and ((str slice s idx (idx + 2)) == "$\{") =
          lex dq after balanced s len cur acc (read balanced brace s (idx + 2) len 1 "$\{")
        | (str char at s idx) == 96 =
          lex dq after balanced s len cur acc (read backtick tok s (idx + 1) len "`")
        | ((str char at s idx) == 92) and (((idx + 1) < len) and (is dq escapable (str char at s (idx + 1)))) =
      lex double quote s (idx + 2) len (cur + (char from code 1) + (str slice s (idx + 1) (idx + 2)) + (char from code 2)) acc
        | ((str char at s idx) == 92) and ((idx + 1) < len) =
      lex double quote s (idx + 2) len (cur + (str slice s idx (idx + 2))) acc
        | otherwise =
          lex double quote s (idx + 1) len (cur + (str slice s idx (idx + 1))) acc

fn :: lex dq after balanced String Int String [String] (String, Int) => [String]
      lex dq after balanced s len cur acc (frag, next_idx) =
        lex double quote s next_idx len (cur + frag) acc

-- ----------------------------------------------------------------------------
-- 3.2 Parser (`parse shell`)
-- ----------------------------------------------------------------------------

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
      parse sh list tail left [] _ = (left, [])
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

fn :: parse sh pipeline [String]   => ShNode [String]
      parse sh pipeline ("!":rest) =
        inner = parse sh pipeline rest
        (ShNot (fst inner), snd inner)
      parse sh pipeline toks       =
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
      parse sh command (fname:"(":")":rest) =
        body_res = parse sh command (skip semis rest)
        (ShFuncDef fname (fst body_res), snd body_res)
      parse sh command (t:rest) =
        | t == "("     = parse sh subshell rest
        | t == "\{"     = parse sh brace rest
        | t == "if"    = parse sh if rest
        | t == "while" = parse sh while rest
        | t == "until" = parse sh until rest
        | t == "for"   = parse sh for rest
        | t == "case"  = parse sh case rest
        | otherwise    = parse sh simple ([t] :: rest) [] []

fn :: consume expected String [String] => [String]
      consume expected _      []       =  []
      consume expected exp    (t:rest) =
        | t == exp  = rest
        | otherwise = [t] :: rest

fn :: parse sh subshell [String] => ShNode [String]
      parse sh subshell toks     =
        inner = parse sh list toks [")"]
        rem = consume expected ")" (snd inner)
        (ShSubshell (fst inner), rem)

fn :: parse sh brace [String] => ShNode [String]
      parse sh brace toks     =
        inner = parse sh list toks ["\}"]
        rem = consume expected "\}" (snd inner)
        (ShBrace (fst inner), rem)

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

fn :: parse sh until [String] => ShNode [String]
      parse sh until toks     =
        cond_res = parse sh list toks ["do"]
        after_do = consume expected "do" (snd cond_res)
        body_res = parse sh list after_do ["done"]
        after_done = consume expected "done" (snd body_res)
        (ShUntil (fst cond_res) (fst body_res), after_done)

fn :: parse sh for [String]     => ShNode [String]
      parse sh for []           =  (ShSimple [] [], [])
      parse sh for (vname:rest) =  parse sh for items vname rest

fn :: parse sh for items String [String] => ShNode [String]
      parse sh for items vname ("in":rest) =
        items_res = collect for items rest []
        items = fst items_res
        after_semi = skip semis (snd items_res)
        after_do = consume expected "do" after_semi
        body_res = parse sh list after_do ["done"]
        after_done = consume expected "done" (snd body_res)
        (ShFor vname items True (fst body_res), after_done)
      parse sh for items vname rest =
        after_semi = skip semis rest
        after_do = consume expected "do" after_semi
        body_res = parse sh list after_do ["done"]
        after_done = consume expected "done" (snd body_res)
        (ShFor vname [] False (fst body_res), after_done)

fn :: collect for items [String] [String] => [String] [String]
      collect for items []       acc      =  (acc, [])
      collect for items (t:rest) acc      =
        | (t == ";") or (t == "do") = (acc, [t] :: rest)
        | otherwise                 = collect for items rest (acc :: [t])

fn :: parse sh case [String] => ShNode [String]
      parse sh case []       =  (ShSimple [] [], [])
      parse sh case (w:rest) =
        after_in = consume expected "in" (skip semis rest)
        arms_res = parse case arms (skip semis after_in) []
        (ShCase w (fst arms_res), snd arms_res)

fn :: skip case semis [String] => [String]
      skip case semis []       =  []
      skip case semis (t:rest) =
        | (t == ";") or (t == ";;") = skip case semis rest
        | otherwise                 = [t] :: rest

fn :: parse case arms [String] [([String], ShNode)] => [([String], ShNode)] [String]
      parse case arms [] acc = (acc, [])
      parse case arms (t:rest) acc =
        | t == "esac"               = (acc, rest)
        | (t == ";") or (t == ";;") = parse case arms rest acc
        | otherwise                 =
          pats_res = collect case pats ([t] :: rest) []
          body_res = parse sh list (snd pats_res) [";;", "esac"]
          after_arm = skip case semis (snd body_res)
          parse case arms after_arm (acc :: [(fst pats_res, fst body_res)])

fn :: collect case pats [String]   [String] => [String] [String]
      collect case pats []         acc      =  (acc, [])
      collect case pats ("(":rest) acc      =  collect case pats rest acc
      collect case pats ("|":rest) acc      =  collect case pats rest acc
      collect case pats (")":rest) acc      =  (acc, rest)
      collect case pats (p:rest)   acc      =  collect case pats rest (acc :: [p])

fn :: is cmd delim String => Bool
      is cmd delim t      =
        (t == ";") or ((t == ";;") or ((t == "&&") or ((t == "||") or ((t == "|") or ((t == ")") or ((t == "\}") or ((t == "then") or ((t == "elif") or ((t == "else") or ((t == "fi") or ((t == "do") or ((t == "done") or (t == "esac")))))))))))))

fn :: is var assign tok String => Bool
      is var assign tok s      =
        eq_i = find char idx s 0 (str len s) 61
        (eq_i > 0) and (not (str starts with s "="))

fn :: parse sh simple [String] [String] [Redir] => ShNode [String]
      parse sh simple [] words redirs = (finalize simple node words redirs, [])
      parse sh simple (t:rest) words redirs =
        | is cmd delim t       = (finalize simple node words redirs, [t] :: rest)
        | t == "<"             = parse sh redir 1 rest words redirs
        | t == ">"             = parse sh redir 2 rest words redirs
        | t == ">>"            = parse sh redir 3 rest words redirs
        | t == "<<HEREDOC_EXP" = parse sh redir 4 rest words redirs
        | t == "<<HEREDOC_LIT" = parse sh redir 5 rest words redirs
        | t == "2>&1"          = parse sh simple rest words (redirs :: [RedirErrToOut])
        | otherwise            = parse sh simple rest (words :: [t]) redirs

fn :: parse sh redir Int [String] [String] [Redir] => ShNode [String]
      parse sh redir _ [] words redirs = (finalize simple node words redirs, [])
      parse sh redir kind (target:rest) words redirs =
        | kind == 1 = parse sh simple rest words (redirs :: [RedirIn target])
        | kind == 2 = parse sh simple rest words (redirs :: [RedirOut target])
        | kind == 3 = parse sh simple rest words (redirs :: [RedirAppend target])
        | kind == 4 = parse sh simple rest words (redirs :: [RedirHeredoc target True])
        | otherwise = parse sh simple rest words (redirs :: [RedirHeredoc target False])

fn :: finalize simple node [String] [Redir] => ShNode
      finalize simple node (w:[])   []      =
        | is var assign tok w = split assign node w
        | otherwise           = ShSimple [w] []
      finalize simple node words    redirs  =  ShSimple words redirs

fn :: split assign node String => ShNode
      split assign node w      =
        eq_i = find char idx w 0 (str len w) 61
        ShAssign (str slice w 0 eq_i) (str slice w (eq_i + 1) (str len w))

-- ----------------------------------------------------------------------------
-- 3.3 Globbing, Prefix/Suffix Stripping & Arithmetic Evaluator `$(( ... ))`
-- ----------------------------------------------------------------------------

fn :: sh glob match String String => Bool
      sh glob match pat    text   =
        sh glob loop pat 0 (str len pat) text 0 (str len text)

fn :: sh glob loop String Int Int String Int Int => Bool
      sh glob loop pat pi plen text ti tlen =
        | (pi >= plen) and (ti >= tlen) = True
        | (pi < plen) and ((str char at pat pi) == 42) =
      sh glob star pat pi plen text ti tlen
        | (pi >= plen) or (ti >= tlen) = False
        | (str char at pat pi) == 63 =
      sh glob loop pat (pi + 1) plen text (ti + 1) tlen
        | (str char at pat pi) == (str char at text ti) =
      sh glob loop pat (pi + 1) plen text (ti + 1) tlen
        | otherwise = False

fn :: sh glob star String Int Int String Int Int => Bool
      sh glob star pat pi plen text ti tlen =
        | sh glob loop pat (pi + 1) plen text ti tlen = True
        | ti < tlen                                   = sh glob loop pat pi plen text (ti + 1) tlen
        | otherwise                                   = False

fn :: strip prefix shortest String String Int Int => String
      strip prefix shortest val pat idx len =
        | idx > len = val
        | sh glob match pat (str slice val 0 idx) = str slice val idx len
        | otherwise = strip prefix shortest val pat (idx + 1) len

fn :: strip prefix longest String String Int Int => String
      strip prefix longest val pat idx len =
        | idx < 0 = val
        | sh glob match pat (str slice val 0 idx) = str slice val idx len
        | otherwise = strip prefix longest val pat (idx - 1) len

fn :: strip suffix shortest String String Int Int => String
      strip suffix shortest val pat idx len =
        | idx < 0 = val
        | sh glob match pat (str slice val idx len) = str slice val 0 idx
        | otherwise = strip suffix shortest val pat (idx - 1) len

fn :: strip suffix longest String String Int Int => String
      strip suffix longest val pat idx len =
        | idx > len = val
        | sh glob match pat (str slice val idx len) = str slice val 0 idx
        | otherwise = strip suffix longest val pat (idx + 1) len

fn :: arith shift left Int Int => Int
      arith shift left a   b   =
        | b <= 0    = a
        | otherwise = arith shift left (a * 2) (b - 1)

fn :: arith shift right Int Int => Int
      arith shift right a   b   =
        | b <= 0    = a
        | otherwise = arith shift right (a / 2) (b - 1)

fn :: bit band Int Int => Int
      bit band a   b   =
        | (a <= 0) or (b <= 0) = 0
        | otherwise            = (((a % 2) * (b % 2)) + (2 * (bit band (a / 2) (b / 2))))

fn :: bit or1 Int Int => Int
      bit or1 0   0   =  0
      bit or1 _   _   =  1

fn :: bit bor Int Int => Int
      bit bor a   b   =
        | (a <= 0) and (b <= 0) = 0
        | otherwise             = ((bit or1 (a % 2) (b % 2)) + (2 * (bit bor (a / 2) (b / 2))))

fn :: bit bxor Int Int => Int
      bit bxor a   b   =
        | (a <= 0) and (b <= 0) = 0
        | otherwise             = ((((a % 2) + (b % 2)) % 2) + (2 * (bit bxor (a / 2) (b / 2))))

fn :: bool to int Bool  => Int
      bool to int True  =  1
      bool to int False =  0

fn :: is var char Int => Bool
      is var char c   =
        ((c >= 65) and (c <= 90)) or (((c >= 97) and (c <= 122)) or (((c >= 48) and (c <= 57)) or (c == 95)))

fn :: read var ident String Int Int String => String Int
      read var ident s idx len acc =
        | idx >= len                      = (acc, len)
        | is var char (str char at s idx) = read var ident s (idx + 1) len (acc + (str slice s idx (idx + 1)))
        | otherwise                       = (acc, idx)

fn :: lex arith String Int Int [String] => [String]
      lex arith s idx len acc =
        | idx >= len                = acc
        | is ws (str char at s idx) = lex arith s (idx + 1) len acc
        | ((idx + 1) < len) and (is two char arith op (str slice s idx (idx + 2))) =
      lex arith s (idx + 2) len (acc :: [str slice s idx (idx + 2)])
        | is var char (str char at s idx) =
          lex arith after ident s len acc (read var ident s idx len "")
        | otherwise =
          lex arith s (idx + 1) len (acc :: [str slice s idx (idx + 1)])

fn :: is two char arith op String => Bool
      is two char arith op op_s =
        (op_s == "<<") or ((op_s == ">>") or ((op_s == "<=") or ((op_s == ">=") or ((op_s == "==") or ((op_s == "!=") or ((op_s == "&&") or (op_s == "||")))))))

fn :: lex arith after ident String Int [String] (String, Int) => [String]
      lex arith after ident s len acc (id_s, next_idx) =
        lex arith s next_idx len (acc :: [id_s])

fn :: eval sh arith String ShEnv => Int
      eval sh arith expr   env   =
        exp_pair = expand sh word expr env
        toks = lex arith (fst exp_pair) 0 (str len (fst exp_pair)) []
        fst (parse arith ternary toks (snd exp_pair))

fn :: parse arith ternary [String] ShEnv => Int [String]
      parse arith ternary toks     env   =
        c_res = parse arith lor toks env
        parse arith ternary tail (fst c_res) (snd c_res) env

fn :: parse arith ternary tail Int [String] ShEnv => Int [String]
      parse arith ternary tail cond_v ("?":rest) env =
        t_res = parse arith ternary rest env
        after_colon = consume expected ":" (snd t_res)
        f_res = parse arith ternary after_colon env
        (select arith ternary cond_v (fst t_res) (fst f_res), snd f_res)
      parse arith ternary tail cond_v rem _ = (cond_v, rem)

fn :: select arith ternary Int Int Int => Int
      select arith ternary 0 _ fv = fv
      select arith ternary _ tv _ = tv

fn :: parse arith lor [String] ShEnv => Int [String]
      parse arith lor toks     env   =
        l_res = parse arith land toks env
        parse arith lor tail (fst l_res) (snd l_res) env

fn :: parse arith lor tail Int [String] ShEnv => Int [String]
      parse arith lor tail lv ("||":rest) env =
        r_res = parse arith land rest env
        parse arith lor tail (bool to int ((lv != 0) or ((fst r_res) != 0))) (snd r_res) env
      parse arith lor tail lv rem _ = (lv, rem)

fn :: parse arith land [String] ShEnv => Int [String]
      parse arith land toks     env   =
        l_res = parse arith bor toks env
        parse arith land tail (fst l_res) (snd l_res) env

fn :: parse arith land tail Int [String] ShEnv => Int [String]
      parse arith land tail lv ("&&":rest) env =
        r_res = parse arith bor rest env
        parse arith land tail (bool to int ((lv != 0) and ((fst r_res) != 0))) (snd r_res) env
      parse arith land tail lv rem _ = (lv, rem)

fn :: parse arith bor [String] ShEnv => Int [String]
      parse arith bor toks     env   =
        l_res = parse arith bxor toks env
        parse arith bor tail (fst l_res) (snd l_res) env

fn :: parse arith bor tail Int [String] ShEnv => Int [String]
      parse arith bor tail lv ("|":rest) env =
        r_res = parse arith bxor rest env
        parse arith bor tail (bit bor lv (fst r_res)) (snd r_res) env
      parse arith bor tail lv rem _ = (lv, rem)

fn :: parse arith bxor [String] ShEnv => Int [String]
      parse arith bxor toks     env   =
        l_res = parse arith band toks env
        parse arith bxor tail (fst l_res) (snd l_res) env

fn :: parse arith bxor tail Int [String] ShEnv => Int [String]
      parse arith bxor tail lv ("^":rest) env =
        r_res = parse arith band rest env
        parse arith bxor tail (bit bxor lv (fst r_res)) (snd r_res) env
      parse arith bxor tail lv rem _ = (lv, rem)

fn :: parse arith band [String] ShEnv => Int [String]
      parse arith band toks     env   =
        l_res = parse arith eq toks env
        parse arith band tail (fst l_res) (snd l_res) env

fn :: parse arith band tail Int [String] ShEnv => Int [String]
      parse arith band tail lv ("&":rest) env =
        r_res = parse arith eq rest env
        parse arith band tail (bit band lv (fst r_res)) (snd r_res) env
      parse arith band tail lv rem _ = (lv, rem)

fn :: parse arith eq [String] ShEnv => Int [String]
      parse arith eq toks     env   =
        l_res = parse arith rel toks env
        parse arith eq tail (fst l_res) (snd l_res) env

fn :: parse arith eq tail Int [String] ShEnv => Int [String]
      parse arith eq tail lv ("==":rest) env =
        r_res = parse arith rel rest env
        parse arith eq tail (bool to int (lv == (fst r_res))) (snd r_res) env
      parse arith eq tail lv ("!=":rest) env =
        r_res = parse arith rel rest env
        parse arith eq tail (bool to int (lv != (fst r_res))) (snd r_res) env
      parse arith eq tail lv rem _ = (lv, rem)

fn :: parse arith rel [String] ShEnv => Int [String]
      parse arith rel toks     env   =
        l_res = parse arith shift toks env
        parse arith rel tail (fst l_res) (snd l_res) env

fn :: parse arith rel tail Int [String] ShEnv => Int [String]
      parse arith rel tail lv ("<":rest) env =
        r_res = parse arith shift rest env
        parse arith rel tail (bool to int (lv < (fst r_res))) (snd r_res) env
      parse arith rel tail lv ("<=":rest) env =
        r_res = parse arith shift rest env
        parse arith rel tail (bool to int (lv <= (fst r_res))) (snd r_res) env
      parse arith rel tail lv (">":rest) env =
        r_res = parse arith shift rest env
        parse arith rel tail (bool to int (lv > (fst r_res))) (snd r_res) env
      parse arith rel tail lv (">=":rest) env =
        r_res = parse arith shift rest env
        parse arith rel tail (bool to int (lv >= (fst r_res))) (snd r_res) env
      parse arith rel tail lv rem _ = (lv, rem)

fn :: parse arith shift [String] ShEnv => Int [String]
      parse arith shift toks     env   =
        l_res = parse arith add toks env
        parse arith shift tail (fst l_res) (snd l_res) env

fn :: parse arith shift tail Int [String] ShEnv => Int [String]
      parse arith shift tail lv ("<<":rest) env =
        r_res = parse arith add rest env
        parse arith shift tail (arith shift left lv (fst r_res)) (snd r_res) env
      parse arith shift tail lv (">>":rest) env =
        r_res = parse arith add rest env
        parse arith shift tail (arith shift right lv (fst r_res)) (snd r_res) env
      parse arith shift tail lv rem _ = (lv, rem)

fn :: parse arith add [String] ShEnv => Int [String]
      parse arith add toks     env   =
        l_res = parse arith mul toks env
        parse arith add tail (fst l_res) (snd l_res) env

fn :: parse arith add tail Int [String] ShEnv => Int [String]
      parse arith add tail lv ("+":rest) env =
        r_res = parse arith mul rest env
        parse arith add tail (lv + (fst r_res)) (snd r_res) env
      parse arith add tail lv ("-":rest) env =
        r_res = parse arith mul rest env
        parse arith add tail (lv - (fst r_res)) (snd r_res) env
      parse arith add tail lv rem _ = (lv, rem)

fn :: parse arith mul [String] ShEnv => Int [String]
      parse arith mul toks     env   =
        l_res = parse arith unary toks env
        parse arith mul tail (fst l_res) (snd l_res) env

fn :: parse arith mul tail Int [String] ShEnv => Int [String]
      parse arith mul tail lv ("*":rest) env =
        r_res = parse arith unary rest env
        parse arith mul tail (lv * (fst r_res)) (snd r_res) env
      parse arith mul tail lv ("/":rest) env =
        r_res = parse arith unary rest env
        parse arith mul tail (safe div lv (fst r_res)) (snd r_res) env
      parse arith mul tail lv ("%":rest) env =
        r_res = parse arith unary rest env
        parse arith mul tail (safe mod lv (fst r_res)) (snd r_res) env
      parse arith mul tail lv rem _ = (lv, rem)

fn :: safe div Int Int => Int
      safe div _   0   =  0
      safe div a   b   =  a / b

fn :: safe mod Int Int => Int
      safe mod _   0   =  0
      safe mod a   b   =  a % b

fn :: parse arith unary [String]   ShEnv => Int [String]
      parse arith unary ("-":rest) env   =
        r_res = parse arith unary rest env
        (0 - (fst r_res), snd r_res)
      parse arith unary ("+":rest) env   =
        parse arith unary rest env
      parse arith unary ("!":rest) env   =
        r_res = parse arith unary rest env
        (bool to int ((fst r_res) == 0), snd r_res)
      parse arith unary toks       env   =
        parse arith primary toks env

fn :: parse arith primary [String]   ShEnv => Int [String]
      parse arith primary []         _     =  (0, [])
      parse arith primary ("(":rest) env   =
        inner = parse arith ternary rest env
        (fst inner, consume expected ")" (snd inner))
      parse arith primary (tok:rest) env   =
        | is digits only tok = (str to int tok, rest)
        | otherwise          = (parse opt digits (lookup sh param tok env), rest)

-- ----------------------------------------------------------------------------
-- 3.4 Parameter Expansion, Command Substitution & Pathname Globbing
-- ----------------------------------------------------------------------------

fn :: is sh param set String ShEnv => Bool
      is sh param set vname env =
        | (vname == "?") or ((vname == "#") or ((vname == "@") or ((vname == "*") or (vname == "$")))) = True
        | (is digits only vname) and ((str to int vname) >= 1) = (str to int vname) <= (list len (env_pos env))
        | otherwise = has var (env_vars env) vname

fn :: lookup sh param String ShEnv => String
      lookup sh param vname  env   =
        | vname == "?" = int to str (env_status env)
        | vname == "#" = int to str (list len (env_pos env))
        | (vname == "@") or (vname == "*") = join with (env_pos env) " "
        | vname == "$" = int to str (sys getpid)
        | (is digits only vname) and ((str to int vname) >= 1) = str list nth (env_pos env) ((str to int vname) - 1)
        | otherwise = lookup var (env_vars env) vname

fn :: assign sh var String String ShEnv => ShEnv
      assign sh var k v env =
        | list contains str (env_readonly env) k =
          env with env_status = 1, env_exited = True
        | otherwise =
          _ = sys setenv k v
          env with env_vars = set var (env_vars env) k v []

fn :: strip trailing newlines String => String
      strip trailing newlines s      =
        | (str len s) == 0                        = ""
        | (str char at s ((str len s) - 1)) == 10 = strip trailing newlines (str slice s 0 ((str len s) - 1))
        | otherwise                               = s

fn :: eval cmd subst String  ShEnv => String ShEnv
      eval cmd subst sub_src env   =
        sub_ast = parse shell (tokenize shell sub_src)
        sub_env = evaluate shell sub_ast in (env with env_out = "", env_exited = False, env_returning = False, env_noerrexit_ctx = False)
        out_str = strip trailing newlines (env_out sub_env)
        (out_str, env with env_status = env_status sub_env)

fn :: expand sh word String ShEnv => String ShEnv
expand sh word w      env   =  expand sh word loop w 0 (str len w) False env ""

fn :: expand sh word loop String Int Int Bool ShEnv String => String ShEnv
expand sh word loop s idx len in_sq env acc =
  | (idx >= len) or (env_exited env) = (acc, env)
        | (str char at s idx) == 1         = expand sh word loop s (idx + 1) len True env acc
        | (str char at s idx) == 2         = expand sh word loop s (idx + 1) len False env acc
        | ((str char at s idx) == 3) or ((str char at s idx) == 4) =
expand sh word loop s (idx + 1) len in_sq env acc
| (not in_sq) and ((str char at s idx) == 96) =
expand sh backtick s (idx + 1) len env acc
| (not in_sq) and (((str char at s idx) == 36) and ((idx + 1) < len)) =
expand sh dollar s idx len env acc
| otherwise =
expand sh word loop s (idx + 1) len in_sq env (acc + (str slice s idx (idx + 1)))

fn :: expand sh backtick String Int Int ShEnv String => String ShEnv
expand sh backtick s idx len env acc =
  bt_pair = read backtick tok s idx len ""
  raw_bt = fst bt_pair
  inner_cmd = str slice raw_bt 0 (max zero ((str len raw_bt) - 1))
  sub_res = eval cmd subst inner_cmd env
expand sh word loop s (snd bt_pair) len False (snd sub_res) (acc + (fst sub_res))

fn :: max zero Int => Int
      max zero n   =
        | n < 0     = 0
        | otherwise = n

fn :: expand sh dollar String Int Int ShEnv String => String ShEnv
expand sh dollar s idx len env acc =
  | ((idx + 2) < len) and ((str slice s idx (idx + 3)) == "$((") =
expand sh arith dollar s (idx + 3) len env acc
| ((idx + 1) < len) and ((str slice s idx (idx + 2)) == "$(") =
expand sh subst dollar s (idx + 2) len env acc
| (str char at s (idx + 1)) == 123 =
expand sh braced s (idx + 2) len env acc
| (str char at s (idx + 1)) == 63 =
expand sh word loop s (idx + 2) len False env (acc + (int to str (env_status env)))
| (str char at s (idx + 1)) == 35 =
expand sh word loop s (idx + 2) len False env (acc + (int to str (list len (env_pos env))))
| ((str char at s (idx + 1)) == 64) or ((str char at s (idx + 1)) == 42) =
expand sh word loop s (idx + 2) len False env (acc + (join with (env_pos env) " "))
| (str char at s (idx + 1)) == 36 =
expand sh word loop s (idx + 2) len False env (acc + (int to str (sys getpid)))
| ((str char at s (idx + 1)) >= 49) and ((str char at s (idx + 1)) <= 57) =
expand sh pos digit s (idx + 2) len ((str char at s (idx + 1)) - 48) env acc
| otherwise =
expand sh plain var s (idx + 1) len env acc

fn :: expand sh arith dollar String Int Int ShEnv String => String ShEnv
expand sh arith dollar s idx len env acc =
  ar_pair = read balanced arith s idx len 1 ""
  raw_ar = fst ar_pair
  inner_ar = str slice raw_ar 0 (max zero ((str len raw_ar) - 2))
  val_i = eval sh arith inner_ar env
expand sh word loop s (snd ar_pair) len False env (acc + (int to str val_i))

fn :: expand sh subst dollar String Int Int ShEnv String => String ShEnv
expand sh subst dollar s idx len env acc =
  sb_pair = read balanced subst s idx len 1 ""
  raw_sb = fst sb_pair
  inner_sb = str slice raw_sb 0 (max zero ((str len raw_sb) - 1))
  sub_res = eval cmd subst inner_sb env
expand sh word loop s (snd sb_pair) len False (snd sub_res) (acc + (fst sub_res))

fn :: expand sh pos digit String Int Int Int ShEnv String => String ShEnv
expand sh pos digit s next_idx len d env acc =
  | (env_nounset env) and (d > (list len (env_pos env))) =
    (acc, env with env_status = 1, env_exited = True)
    | otherwise =
expand sh word loop s next_idx len False env (acc + (str list nth (env_pos env) (d - 1)))

fn :: expand sh plain var String Int Int ShEnv String => String ShEnv
expand sh plain var s idx len env acc =
  pair = read var ident s idx len ""
  vname = fst pair
  next_idx = snd pair
expand sh plain resolved s next_idx len vname env acc

fn :: expand sh plain resolved String Int Int String ShEnv String => String ShEnv
expand sh plain resolved s next_idx len vname env acc =
  | (str len vname) == 0 =
expand sh word loop s next_idx len False env (acc + "$")
| (env_nounset env) and (not (is sh param set vname env)) =
  (acc, env with env_status = 1, env_exited = True)
  | otherwise =
expand sh word loop s next_idx len False env (acc + (lookup sh param vname env))

fn :: expand sh braced String Int Int ShEnv String => String ShEnv
expand sh braced s idx len env acc =
  br_pair = read balanced brace s idx len 1 ""
  raw_br = fst br_pair
  expr = str slice raw_br 0 (max zero ((str len raw_br) - 1))
  res_pair = eval braced expr expr env
expand sh word loop s (snd br_pair) len False (snd res_pair) (acc + (fst res_pair))

fn :: eval braced expr String ShEnv => String ShEnv
      eval braced expr expr   env   =
        | ((str len expr) > 1) and ((str char at expr 0) == 35) =
          vname = str slice expr 1 (str len expr)
          (int to str (str len (lookup sh param vname env)), env)
        | is digits only expr =
          (lookup sh param expr env, env)
        | otherwise =
          id_pair = read var ident expr 0 (str len expr) ""
          eval braced with ident (fst id_pair) (str slice expr (snd id_pair) (str len expr)) env

fn :: eval braced plain ident String ShEnv => String ShEnv
      eval braced plain ident vname env =
        | (env_nounset env) and (not (is sh param set vname env)) =
          ("", env with env_status = 1, env_exited = True)
        | otherwise =
          (lookup sh param vname env, env)

fn :: eval braced with ident String String ShEnv => String ShEnv
      eval braced with ident vname rem env =
        | (str len rem) == 0 =
          eval braced plain ident vname env
        | str starts with rem "##" =
          pat_res = expand sh word (str slice rem 2 (str len rem)) env
          val = lookup sh param vname (snd pat_res)
          (strip prefix longest val (fst pat_res) (str len val) (str len val), snd pat_res)
        | str starts with rem "#" =
          pat_res = expand sh word (str slice rem 1 (str len rem)) env
          val = lookup sh param vname (snd pat_res)
          (strip prefix shortest val (fst pat_res) 0 (str len val), snd pat_res)
        | str starts with rem "%%" =
          pat_res = expand sh word (str slice rem 2 (str len rem)) env
          val = lookup sh param vname (snd pat_res)
          (strip suffix longest val (fst pat_res) 0 (str len val), snd pat_res)
        | str starts with rem "%" =
          pat_res = expand sh word (str slice rem 1 (str len rem)) env
          val = lookup sh param vname (snd pat_res)
          (strip suffix shortest val (fst pat_res) (str len val) (str len val), snd pat_res)
        | str starts with rem ":" =
      eval braced modifier vname True (str slice rem 1 (str len rem)) env
        | otherwise =
          eval braced modifier vname False rem env

fn :: is param active String Bool ShEnv => Bool
      is param active vname True env =
        (is sh param set vname env) and ((str len (lookup sh param vname env)) > 0)
      is param active vname False env =
        is sh param set vname env

fn :: eval braced modifier String Bool String ShEnv => String ShEnv
      eval braced modifier vname colon_mode op_rem env =
        | (str len op_rem) == 0 = (lookup sh param vname env, env)
        | otherwise =
          op_ch = str char at op_rem 0
          w_raw = str slice op_rem 1 (str len op_rem)
          active = is param active vname colon_mode env
          cur_v = lookup sh param vname env
          dispatch braced op vname op_ch w_raw active cur_v env

fn :: dispatch braced op String Int String Bool String ShEnv => String ShEnv
      dispatch braced op _ 45 _ True cur_v env = (cur_v, env)
      dispatch braced op _ 45 w_raw False _ env = expand sh word w_raw env
      dispatch braced op _ 61 _ True cur_v env = (cur_v, env)
      dispatch braced op vname 61 w_raw False _ env =
        w_res = expand sh word w_raw env
        env1 = assign sh var vname (fst w_res) (snd w_res)
        (fst w_res, env1)
      dispatch braced op _ 43 w_raw True _ env = expand sh word w_raw env
      dispatch braced op _ 43 _ False _ env = ("", env)
      dispatch braced op _ 63 _ True cur_v env = (cur_v, env)
      dispatch braced op _ 63 _ False _ env = ("", env with env_status = 1, env_exited = True)
      dispatch braced op _ _ _ _ cur_v env = (cur_v, env)

fn :: is raw token quoted String Int Int => Bool
      is raw token quoted s idx len =
        | idx >= len                   = False
        | ((str char at s idx) >= 1) and ((str char at s idx) <= 4) = True
        | otherwise                    = is raw token quoted s (idx + 1) len

fn :: has glob meta String Int Int => Bool
      has glob meta s idx len =
        | idx >= len = False
        | ((str char at s idx) == 42) or ((str char at s idx) == 63) = True
        | otherwise  = has glob meta s (idx + 1) len

fn :: str lex le String String Int Int Int => Bool
      str lex le a b idx la lb =
        | (idx >= la) and (idx >= lb)               = True
        | idx >= la                                 = True
        | idx >= lb                                 = False
        | (str char at a idx) < (str char at b idx) = True
        | (str char at a idx) > (str char at b idx) = False
        | otherwise                                 = str lex le a b (idx + 1) la lb

fn :: insert sorted str String [String] => [String]
      insert sorted str x      []       =  [x]
      insert sorted str x      (y:ys)   =
        | str lex le x y 0 (str len x) (str len y) = [x, y] :: ys
        | otherwise                                = [y] :: (insert sorted str x ys)

fn :: sort str list [String] [String] => [String]
      sort str list []       acc      =  acc
      sort str list (x:xs)   acc      =  sort str list xs (insert sorted str x acc)

fn :: filter glob entries [String] String [String] => [String]
      filter glob entries [] _ acc = sort str list acc []
      filter glob entries (e:rest) pat acc =
        | (not (str starts with pat ".")) and (str starts with e ".") =
      filter glob entries rest pat acc
        | sh glob match pat e =
      filter glob entries rest pat (acc :: [e])
        | otherwise =
          filter glob entries rest pat acc

fn :: glob expand single String String ShEnv => [String]
      glob expand single raw_w exp_w env =
        | (not (env_noglob env)) and ((not (is raw token quoted raw_w 0 (str len raw_w))) and (has glob meta exp_w 0 (str len exp_w))) =
          select glob matches exp_w (filter glob entries (list dir (sys getcwd)) exp_w [])
        | otherwise = [exp_w]

fn :: select glob matches String [String] => [String]
      select glob matches exp_w  []       =  [exp_w]
      select glob matches _      matches  =  matches

fn :: expand sh words [String] ShEnv [String] => [String] ShEnv
expand sh words [] env acc = (acc, env)
expand sh words (w:rest) env acc =
  | env_exited env = (acc, env)
        | otherwise =
          w_res = expand sh word w env
          globbed = glob expand single w (fst w_res) (snd w_res)
expand sh words rest (snd w_res) (acc :: globbed)

-- ----------------------------------------------------------------------------
-- 3.5 Evaluator, Control Flow, Functions & Built-ins
-- ----------------------------------------------------------------------------

fn :: is sh interrupted ShEnv => Bool
      is sh interrupted env   =
        (env_exited env) or ((env_returning env) or (((env_break_cnt env) > 0) or ((env_cont_cnt env) > 0)))

fn :: apply errexit check ShEnv => ShEnv
      apply errexit check env   =
        | ((env_status env) != 0) and ((env_errexit env) and (not (env_noerrexit_ctx env))) =
          env with env_exited = True
        | otherwise = env

op :: evaluate shell ShNode in ShEnv => ShEnv
      evaluate shell node   in env   =
        | is sh interrupted env = env
        | otherwise             = eval sh active node env

fn :: eval sh active ShNode           ShEnv => ShEnv
      eval sh active (ShAssign k v)   env   =
        v_res = expand sh word v env
        finish sh assign k (fst v_res) (snd v_res)
      eval sh active (ShFuncDef fname body_n) env =
        env with env_funcs = set sh func (env_funcs env) fname body_n [], env_status = 0
      eval sh active (ShSeq a b)      env   =
        env1 = evaluate shell a in env
        evaluate shell b in env1
      eval sh active (ShAnd a b)      env   =
        env1 = evaluate shell a in (env with env_noerrexit_ctx = True)
        env1_r = env1 with env_noerrexit_ctx = env_noerrexit_ctx env
        eval sh andrhs b env1_r
      eval sh active (ShOr a b)       env   =
        env1 = evaluate shell a in (env with env_noerrexit_ctx = True)
        env1_r = env1 with env_noerrexit_ctx = env_noerrexit_ctx env
        eval sh orrhs b env1_r
      eval sh active (ShNot inner)    env   =
        env1 = evaluate shell inner in (env with env_noerrexit_ctx = True)
        inv_st = invert status (env_status env1)
        env1 with env_status = inv_st, env_noerrexit_ctx = env_noerrexit_ctx env
      eval sh active (ShSubshell sub) env   =
        sub_env = evaluate shell sub in env
        apply errexit check (env with env_status = env_status sub_env, env_out = env_out sub_env)
      eval sh active (ShBrace sub)    env   =
        evaluate shell sub in env
      eval sh active (ShIf cond_n then_n else_n) env =
        env_c = evaluate shell cond_n in (env with env_noerrexit_ctx = True)
        env_cr = env_c with env_noerrexit_ctx = env_noerrexit_ctx env
        eval sh if branch then_n else_n env_cr
      eval sh active (ShWhile cond_n body_n) env =
        eval sh while loop cond_n body_n env 0
      eval sh active (ShUntil cond_n body_n) env =
        eval sh until loop cond_n body_n env 0
      eval sh active (ShFor vname raw_items has_in body_n) env =
        eval sh for start vname raw_items has_in body_n env
      eval sh active (ShCase raw_w arms) env =
        w_res = expand sh word raw_w env
        eval sh case arms (fst w_res) arms (snd w_res)
      eval sh active (ShPipe left right) env =
        eval sh pipe left right env
      eval sh active (ShSimple raw_words redirs) env =
        w_res = expand sh words raw_words env []
        eval sh simple expanded (fst w_res) redirs (snd w_res)

fn :: finish sh assign String String ShEnv => ShEnv
      finish sh assign k exp_v env =
        | env_exited env = env
        | otherwise      =
          env1 = assign sh var k exp_v env
          select assign status env1

fn :: select assign status ShEnv => ShEnv
      select assign status env   =
        | env_exited env = env
        | otherwise      = env with env_status = 0

fn :: invert status Int => Int
      invert status 0   =  1
      invert status _   =  0

fn :: set sh func [ShFunc] String ShNode [ShFunc] => [ShFunc]
      set sh func [] fname body_n acc = acc :: [SFunc fname body_n]
      set sh func ((SFunc k b):rest) fname body_n acc =
        | k == fname = (acc :: [SFunc fname body_n]) :: rest
        | otherwise  = set sh func rest fname body_n (acc :: [SFunc k b])

fn :: lookup sh func [ShFunc]           String => Bool ShNode
      lookup sh func []                 _      =  (False, ShSimple [] [])
      lookup sh func ((SFunc k b):rest) fname  =
        | k == fname = (True, b)
        | otherwise  = lookup sh func rest fname

fn :: eval sh andrhs ShNode ShEnv => ShEnv
      eval sh andrhs b      env1  =
        | is sh interrupted env1 = env1
        | (env_status env1) == 0 = evaluate shell b in env1
        | otherwise              = env1

fn :: eval sh orrhs ShNode ShEnv => ShEnv
      eval sh orrhs b      env1  =
        | is sh interrupted env1 = env1
        | (env_status env1) != 0 = evaluate shell b in env1
        | otherwise              = env1

fn :: eval sh if branch ShNode ShNode ShEnv => ShEnv
      eval sh if branch then_n else_n env_c =
        | is sh interrupted env_c = env_c
        | (env_status env_c) == 0 = evaluate shell then_n in env_c
        | otherwise               = evaluate shell else_n in (env_c with env_status = 0)

fn :: eval sh while loop ShNode ShNode ShEnv Int => ShEnv
      eval sh while loop cond_n body_n env steps =
        | (is sh interrupted env) or (steps >= 1000) = env
        | otherwise =
          env_c = evaluate shell cond_n in (env with env_noerrexit_ctx = True)
          env_cr = env_c with env_noerrexit_ctx = env_noerrexit_ctx env
          continue sh while cond_n body_n env_cr steps

fn :: continue sh while ShNode ShNode ShEnv Int => ShEnv
      continue sh while cond_n body_n env_c steps =
        | (env_status env_c) != 0 = env_c with env_status = 0
        | otherwise               =
          env_b = evaluate shell body_n in env_c
          handle loop step True cond_n body_n env_b steps

fn :: eval sh until loop ShNode ShNode ShEnv Int => ShEnv
      eval sh until loop cond_n body_n env steps =
        | (is sh interrupted env) or (steps >= 1000) = env
        | otherwise =
          env_c = evaluate shell cond_n in (env with env_noerrexit_ctx = True)
          env_cr = env_c with env_noerrexit_ctx = env_noerrexit_ctx env
          continue sh until cond_n body_n env_cr steps

fn :: continue sh until ShNode ShNode ShEnv Int => ShEnv
      continue sh until cond_n body_n env_c steps =
        | (env_status env_c) == 0 = env_c with env_status = 0
        | otherwise               =
          env_b = evaluate shell body_n in (env_c with env_status = 0)
          handle loop step False cond_n body_n env_b steps

fn :: handle loop step Bool ShNode ShNode ShEnv Int => ShEnv
      handle loop step is_while cond_n body_n env_b steps =
        | (env_exited env_b) or (env_returning env_b) = env_b
        | (env_break_cnt env_b) > 0                   = env_b with env_break_cnt = (env_break_cnt env_b) - 1
        | (env_cont_cnt env_b) > 1                    = env_b with env_cont_cnt = (env_cont_cnt env_b) - 1
        | (env_cont_cnt env_b) == 1                   =
          next sh loop is_while cond_n body_n (env_b with env_cont_cnt = 0) (steps + 1)
        | otherwise                                   =
          next sh loop is_while cond_n body_n env_b (steps + 1)

fn :: next sh loop Bool ShNode ShNode ShEnv Int => ShEnv
      next sh loop True cond_n body_n env steps = eval sh while loop cond_n body_n env steps
      next sh loop False cond_n body_n env steps = eval sh until loop cond_n body_n env steps

fn :: eval sh for start String [String] Bool ShNode ShEnv => ShEnv
      eval sh for start vname raw_items True body_n env =
        items_res = expand sh words raw_items env []
        eval sh for loop vname (fst items_res) body_n (snd items_res)
      eval sh for start vname _ False body_n env =
        eval sh for loop vname (env_pos env) body_n env

fn :: eval sh for loop String [String] ShNode ShEnv => ShEnv
      eval sh for loop _ [] _ env = env
      eval sh for loop vname (item:rest) body_n env =
        | is sh interrupted env = env
        | otherwise =
          env1 = assign sh var vname item env
          env2 = evaluate shell body_n in env1
          handle for step vname rest body_n env2

fn :: handle for step String [String] ShNode ShEnv => ShEnv
      handle for step vname rest body_n env2 =
        | (env_exited env2) or (env_returning env2) = env2
        | (env_break_cnt env2) > 0                  = env2 with env_break_cnt = (env_break_cnt env2) - 1
        | (env_cont_cnt env2) > 1                   = env2 with env_cont_cnt = (env_cont_cnt env2) - 1
        | (env_cont_cnt env2) == 1                  = eval sh for loop vname rest body_n (env2 with env_cont_cnt = 0)
        | otherwise                                 = eval sh for loop vname rest body_n env2

fn :: eval sh case arms String [([String], ShNode)] ShEnv => ShEnv
      eval sh case arms _ [] env = env with env_status = 0
      eval sh case arms val ((pats, arm_body):rest) env =
        | match any case pat val pats env = evaluate shell arm_body in env
        | otherwise                       = eval sh case arms val rest env

fn :: match any case pat String [String] ShEnv => Bool
      match any case pat _ [] _ = False
      match any case pat val (p:rest) env =
        | sh glob match (fst (expand sh word p env)) val = True
        | otherwise                                      = match any case pat val rest env

fn :: eval sh pipe ShNode ShNode ShEnv => ShEnv
      eval sh pipe left right env =
        left_env = evaluate shell left in (env with env_out = "", env_noerrexit_ctx = True)
        pipe_data = env_out left_env
        right_env = evaluate shell right in (env with env_out = "", env_stdin = pipe_data)
        combined = env with env_out = (env_out env) + (env_out right_env), env_status = env_status right_env, env_vars = env_vars right_env
        apply errexit check combined

fn :: extract input redirs [Redir]            ShEnv => String ShEnv
      extract input redirs []                 env   =  (env_stdin env, env)
      extract input redirs ((RedirIn f):rest) env   =
        f_res = expand sh word f env
        extract input redirs rest ((snd f_res) with env_stdin = read existing or empty (fst f_res))
      extract input redirs ((RedirHeredoc body True):rest) env =
        b_res = expand sh word body env
        extract input redirs rest ((snd b_res) with env_stdin = fst b_res)
      extract input redirs ((RedirHeredoc body False):rest) env =
        extract input redirs rest (env with env_stdin = body)
      extract input redirs (_:rest)           env   =
        extract input redirs rest env

fn :: apply out redirs String [Redir] ShEnv => String
      apply out redirs text [] env = (env_out env) + text
      apply out redirs text ((RedirOut f):rest) env =
        target = fst (expand sh word f env)
        _ = write file target text
        apply out redirs "" rest env
      apply out redirs text ((RedirAppend f):rest) env =
        target = fst (expand sh word f env)
        _ = append file target text
        apply out redirs "" rest env
      apply out redirs text (_:rest) env = apply out redirs text rest env

fn :: eval sh simple expanded [String] [Redir] ShEnv => ShEnv
      eval sh simple expanded words redirs env =
        | env_exited env = env
        | otherwise      =
          in_pair = extract input redirs redirs env
          eval sh simple words redirs (snd in_pair)

fn :: eval sh simple [String] [Redir] ShEnv => ShEnv
      eval sh simple [] _ env = env
      eval sh simple (cmd:args) redirs env =
        | cmd == ":"                      = env with env_status = 0
        | cmd == "true"                   = env with env_status = 0
        | cmd == "false"                  = apply errexit check (env with env_status = 1)
        | cmd == "echo"                   = eval builtin echo args redirs env
        | cmd == "printf"                 = eval builtin printf args redirs env
        | cmd == "read"                   = eval builtin read args env
        | cmd == "cd"                     = apply errexit check (eval builtin cd args env)
        | cmd == "pwd"                    = env with env_status = 0, env_out = apply out redirs ((sys getcwd) + (char from code 10)) redirs env
        | cmd == "export"                 = eval builtin export args env
        | cmd == "readonly"               = eval builtin readonly args env
        | cmd == "local"                  = eval builtin local args env
        | cmd == "unset"                  = eval builtin unset args env
        | cmd == "shift"                  = apply errexit check (eval builtin shift args env)
        | cmd == "set"                    = eval builtin set args env
        | cmd == "return"                 = eval builtin return args env
        | cmd == "break"                  = eval builtin break args env
        | cmd == "continue"               = eval builtin continue args env
        | cmd == "eval"                   = eval builtin eval args env
        | cmd == "trap"                   = eval builtin trap args env
        | cmd == "exit"                   = eval builtin exit args env
        | (cmd == "test") or (cmd == "[") = apply errexit check (eval builtin test cmd args env)
        | cmd == "."                      = eval builtin source args env
        | (cmd == "cat") and ((list len args) == 0) =
          env with env_status = 0, env_out = apply out redirs (env_stdin env) redirs env, env_stdin = ""
        | otherwise                       =
          dispatch func or external (lookup sh func (env_funcs env) cmd) cmd args redirs env

fn :: dispatch func or external (Bool, ShNode) String [String] [Redir] ShEnv => ShEnv
      dispatch func or external (True, fn_body) _ args redirs env =
        call sh function fn_body args redirs env
      dispatch func or external (False, _) cmd args redirs env =
        apply errexit check (run external capture ([cmd] :: args) redirs env)

fn :: restore local vars [String] [MakeVar] [MakeVar] => [MakeVar]
      restore local vars [] _ cur_vars = cur_vars
      restore local vars (loc_k:rest) orig_vars cur_vars =
        | has var orig_vars loc_k =
          old_v = lookup var orig_vars loc_k
          _ = sys setenv loc_k old_v
      restore local vars rest orig_vars (set var cur_vars loc_k old_v [])
        | otherwise =
          _ = sys setenv loc_k ""
          restore local vars rest orig_vars (remove var cur_vars loc_k [])

fn :: call sh function ShNode [String] [Redir] ShEnv => ShEnv
      call sh function fn_body args redirs env =
        orig_vars = env_vars env
        orig_pos = env_pos env
        orig_locals = env_locals env
        callee_env = env with env_pos = args, env_locals = [], env_returning = False, env_out = ""
        after_fn = evaluate shell fn_body in callee_env
        restored_vars = restore local vars (env_locals after_fn) orig_vars (env_vars after_fn)
        out_applied = apply out redirs (env_out after_fn) redirs env
        after_fn with env_vars = restored_vars, env_pos = orig_pos, env_locals = orig_locals, env_returning = False, env_out = out_applied

fn :: quote sh words [String] [String] => [String]
      quote sh words []       acc      =  acc
      quote sh words (w:rest) acc      =
        sq = char from code 39
        quote sh words rest (acc :: [sq + w + sq])

fn :: render external redirs [Redir] ShEnv String => String
      render external redirs [] _ acc = acc
      render external redirs ((RedirIn f):rest) env acc =
        render external redirs rest env (acc + " < " + (fst (expand sh word f env)))
      render external redirs ((RedirOut f):rest) env acc =
        render external redirs rest env (acc + " > " + (fst (expand sh word f env)))
      render external redirs ((RedirAppend f):rest) env acc =
        render external redirs rest env (acc + " >> " + (fst (expand sh word f env)))
      render external redirs (RedirErrToOut:rest) env acc =
        render external redirs rest env (acc + " 2>&1")
      render external redirs (_:rest) env acc =
        render external redirs rest env acc

fn :: run external capture [String] [Redir] ShEnv => ShEnv
      run external capture words redirs env =
        pid = sys getpid
        ns = sys monotonic ns
        cap_file = "/tmp/.ut_sh_cap_" + (int to str pid) + "_" + (int to str (ns % 1000000)) + ".out"
        in_file = cap_file + ".in"
        cmd_base = (join with (quote sh words words []) " ") + (render external redirs redirs env "")
        in_redir = prepare stdin file in_file (env_stdin env)
        rc = shell exec ("(" + cmd_base + in_redir + ") > " + cap_file)
        captured = read existing or empty cap_file
        _ = sys unlink cap_file
        _ = sys unlink in_file
        env with env_status = rc, env_out = apply out redirs captured redirs env, env_stdin = ""

fn :: prepare stdin file String  String => String
      prepare stdin file in_file data   =
        | (str len data) == 0 = ""
        | otherwise =
          _ = write file in_file data
          " < " + in_file

fn :: interpret backslash escapes String Int Int String => String
      interpret backslash escapes s idx len acc =
        | idx >= len = acc
        | ((str char at s idx) == 92) and ((idx + 1) < len) =
          interpret escape code s (idx + 2) len (str char at s (idx + 1)) acc
        | otherwise =
          interpret backslash escapes s (idx + 1) len (acc + (str slice s idx (idx + 1)))

fn :: interpret escape code String Int Int Int String => String
      interpret escape code s next_idx len 110 acc =
        interpret backslash escapes s next_idx len (acc + (char from code 10))
      interpret escape code s next_idx len 116 acc =
        interpret backslash escapes s next_idx len (acc + (char from code 9))
      interpret escape code s next_idx len 92 acc =
        interpret backslash escapes s next_idx len (acc + (char from code 92))
      interpret escape code s next_idx len other acc =
        interpret backslash escapes s next_idx len (acc + (char from code 92) + (char from code other))

fn :: eval builtin echo [String] [Redir] ShEnv => ShEnv
      eval builtin echo ("-n":rest) redirs env =
        out = apply out redirs (join with rest " ") redirs env
        env with env_status = 0, env_out = out
      eval builtin echo ("-e":rest) redirs env =
        raw = join with rest " "
        esc = interpret backslash escapes raw 0 (str len raw) ""
        out = apply out redirs (esc + (char from code 10)) redirs env
        env with env_status = 0, env_out = out
      eval builtin echo args redirs env =
        out = apply out redirs ((join with args " ") + (char from code 10)) redirs env
        env with env_status = 0, env_out = out

fn :: drop sh list Int [String] => [String]
      drop sh list _   []       =  []
      drop sh list n   (x:xs)   =
        | n <= 0    = [x] :: xs
        | otherwise = drop sh list (n - 1) xs

fn :: int to hex lower Int => String
      int to hex lower n   =
        | n == 0    = "0"
        | otherwise = hex digits loop n ""

fn :: hex digits loop Int String => String
      hex digits loop 0   acc    =  acc
      hex digits loop n   acc    =
        d = n % 16
        ch = str slice "0123456789abcdef" d (d + 1)
        hex digits loop (n / 16) (ch + acc)

fn :: pad zeroes String Int   => String
      pad zeroes s      width =
        | (str len s) >= width = s
        | otherwise            = pad zeroes ("0" + s) width

fn :: format printf loop String Int Int [String] String => String
      format printf loop fmt idx len args acc =
        | idx >= len = acc
        | ((str char at fmt idx) == 92) and ((idx + 1) < len) =
          format printf escape fmt (idx + 2) len (str char at fmt (idx + 1)) args acc
        | ((str char at fmt idx) == 37) and ((idx + 1) < len) =
      format printf spec fmt (idx + 1) len args acc
        | otherwise =
          format printf loop fmt (idx + 1) len args (acc + (str slice fmt idx (idx + 1)))

fn :: format printf escape String Int Int Int [String] String => String
      format printf escape fmt next_idx len 110 args acc =
        format printf loop fmt next_idx len args (acc + (char from code 10))
      format printf escape fmt next_idx len 116 args acc =
        format printf loop fmt next_idx len args (acc + (char from code 9))
      format printf escape fmt next_idx len 92 args acc =
        format printf loop fmt next_idx len args (acc + (char from code 92))
      format printf escape fmt next_idx len c args acc =
        format printf loop fmt next_idx len args (acc + (char from code c))

fn :: format printf spec String Int Int [String] String => String
      format printf spec fmt idx len args acc =
        | (str char at fmt idx) == 37 =
      format printf loop fmt (idx + 1) len args (acc + "%")
        | ((str char at fmt idx) == 48) and (((idx + 2) < len) and ((str char at fmt (idx + 2)) == 100)) =
          w = (str char at fmt (idx + 1)) - 48
          arg_s = str list nth args 0
          num_s = pad zeroes (int to str (parse opt digits arg_s)) w
      format printf loop fmt (idx + 3) len (drop sh list 1 args) (acc + num_s)
        | (str char at fmt idx) == 115 =
      format printf loop fmt (idx + 1) len (drop sh list 1 args) (acc + (str list nth args 0))
        | ((str char at fmt idx) == 100) or ((str char at fmt idx) == 105) =
          arg_s = str list nth args 0
      format printf loop fmt (idx + 1) len (drop sh list 1 args) (acc + (int to str (parse opt digits arg_s)))
        | (str char at fmt idx) == 120 =
          arg_s = str list nth args 0
      format printf loop fmt (idx + 1) len (drop sh list 1 args) (acc + (int to hex lower (parse opt digits arg_s)))
        | otherwise =
          format printf loop fmt (idx + 1) len args acc

fn :: eval builtin printf [String] [Redir] ShEnv => ShEnv
      eval builtin printf [] _ env = env with env_status = 0
      eval builtin printf (fmt:args) redirs env =
        rendered = format printf loop fmt 0 (str len fmt) args ""
        out = apply out redirs rendered redirs env
        env with env_status = 0, env_out = out

fn :: eval builtin read [String] ShEnv => ShEnv
      eval builtin read vars     env   =
        | (str len (env_stdin env)) == 0 = env with env_status = 1
        | otherwise =
          nl_i = find line end (env_stdin env) 0 (str len (env_stdin env))
          line = str slice (env_stdin env) 0 nl_i
          rem_in = str slice (env_stdin env) (min int (str len (env_stdin env)) (nl_i + 1)) (str len (env_stdin env))
          words = split whitespace line
          bind read vars vars words (env with env_stdin = rem_in, env_status = 0)

fn :: bind read vars [String] [String] ShEnv => ShEnv
      bind read vars [] _ env = env
      bind read vars (v:[]) words env =
        assign sh var v (join with words " ") env
      bind read vars (v:vrest) [] env =
        bind read vars vrest [] (assign sh var v "" env)
      bind read vars (v:vrest) (w:wrest) env =
        bind read vars vrest wrest (assign sh var v w env)

fn :: eval builtin cd [String] ShEnv => ShEnv
      eval builtin cd []       env   =
        rc = sys chdir (sys getenv "HOME")
        env with env_status = status from sys rc
      eval builtin cd (dir:_)  env   =
        rc = sys chdir dir
        env with env_status = status from sys rc

fn :: status from sys Int => Int
      status from sys 0   =  0
      status from sys _   =  1

fn :: eval builtin export [String]   ShEnv => ShEnv
      eval builtin export []         env   =  env
      eval builtin export (arg:rest) env   =
        | is var assign tok arg =
          eq_i = find char idx arg 0 (str len arg) 61
          k = str slice arg 0 eq_i
          v = str slice arg (eq_i + 1) (str len arg)
      eval builtin export rest       ((assign sh var k v env) with env_status = 0)
        | otherwise =
          _ = sys setenv arg (lookup var (env_vars env) arg)
          eval builtin export rest env

fn :: eval builtin readonly [String]   ShEnv => ShEnv
      eval builtin readonly []         env   =  env with env_status = 0
      eval builtin readonly (arg:rest) env   =
        | is var assign tok arg =
          eq_i = find char idx arg 0 (str len arg) 61
          k = str slice arg 0 eq_i
          v = str slice arg (eq_i + 1) (str len arg)
          env1 = assign sh var k v env
      eval builtin readonly rest       (env1 with env_readonly = (env_readonly env1) :: [k], env_status = 0)
        | otherwise =
          eval builtin readonly rest (env with env_readonly = (env_readonly env) :: [arg], env_status = 0)

fn :: eval builtin local [String]   ShEnv => ShEnv
      eval builtin local []         env   =  env with env_status = 0
      eval builtin local (arg:rest) env   =
        | is var assign tok arg =
          eq_i = find char idx arg 0 (str len arg) 61
          k = str slice arg 0 eq_i
          v = str slice arg (eq_i + 1) (str len arg)
          env1 = assign sh var k v (env with env_locals = (env_locals env) :: [k])
      eval builtin local rest       (env1 with env_status = 0)
        | otherwise =
          env1 = assign sh var arg "" (env with env_locals = (env_locals env) :: [arg])
          eval builtin local rest (env1 with env_status = 0)

fn :: remove var [MakeVar] String [MakeVar] => [MakeVar]
      remove var []        _      acc       =  acc
      remove var (v:rest)  key    acc       =
        | (mv key v) == key = remove var rest key acc
        | otherwise         = remove var rest key (acc :: [v])

fn :: eval builtin unset [String] ShEnv => ShEnv
      eval builtin unset []       env   =  env
      eval builtin unset (k:rest) env   =
        | list contains str (env_readonly env) k =
          env with env_status = 1, env_exited = True
        | otherwise =
          _ = sys setenv k ""
          eval builtin unset rest (env with env_vars = remove var (env_vars env) k [], env_status = 0)

fn :: eval builtin shift [String]  ShEnv => ShEnv
      eval builtin shift []        env   =  eval builtin shift n 1 env
      eval builtin shift (n_str:_) env   =  eval builtin shift n (parse opt digits n_str) env

fn :: eval builtin shift n Int ShEnv => ShEnv
      eval builtin shift n n env =
        | n > (list len (env_pos env)) = env with env_status = 1
        | otherwise                    = env with env_pos = drop sh list n (env_pos env), env_status = 0

fn :: eval builtin set [String]    ShEnv => ShEnv
      eval builtin set []          env   =  env with env_pos = [], env_status = 0
      eval builtin set ("--":rest) env   =  env with env_pos = rest, env_status = 0
      eval builtin set ("-e":rest) env   =  eval builtin set opts rest (env with env_errexit = True, env_status = 0)
      eval builtin set ("+e":rest) env   =  eval builtin set opts rest (env with env_errexit = False, env_status = 0)
      eval builtin set ("-u":rest) env   =  eval builtin set opts rest (env with env_nounset = True, env_status = 0)
      eval builtin set ("+u":rest) env   =  eval builtin set opts rest (env with env_nounset = False, env_status = 0)
      eval builtin set ("-f":rest) env   =  eval builtin set opts rest (env with env_noglob = True, env_status = 0)
      eval builtin set ("+f":rest) env   =  eval builtin set opts rest (env with env_noglob = False, env_status = 0)
      eval builtin set args        env   =  env with env_pos = args, env_status = 0

fn :: eval builtin set opts [String] ShEnv => ShEnv
      eval builtin set opts [] env = env
      eval builtin set opts args env = eval builtin set args env

fn :: eval builtin return [String] ShEnv => ShEnv
      eval builtin return []       env   =  env with env_returning = True
      eval builtin return (code:_) env   =  env with env_status = str to int code, env_returning = True

fn :: eval builtin break [String]  ShEnv => ShEnv
      eval builtin break []        env   =  env with env_break_cnt = 1, env_status = 0
      eval builtin break (n_str:_) env   =  env with env_break_cnt = max one (str to int n_str), env_status = 0

fn :: eval builtin continue [String]  ShEnv => ShEnv
      eval builtin continue []        env   =  env with env_cont_cnt = 1, env_status = 0
      eval builtin continue (n_str:_) env   =  env with env_cont_cnt = max one (str to int n_str), env_status = 0

fn :: max one Int => Int
      max one n   =
        | n < 1     = 1
        | otherwise = n

fn :: eval builtin eval [String] ShEnv => ShEnv
      eval builtin eval args     env   =
        script = join with args " "
        node = parse shell (tokenize shell script)
        evaluate shell node in env

fn :: eval builtin trap [String]      ShEnv => ShEnv
      eval builtin trap []            env   =  env with env_status = 0
      eval builtin trap (action:sigs) env   =
        | (list contains str sigs "EXIT") or (list contains str sigs "0") =
          env with env_exit_trap = action, env_status = 0
        | otherwise =
          env with env_status = 0

fn :: eval builtin exit [String] ShEnv => ShEnv
      eval builtin exit []       env   =  env with env_exited = True
      eval builtin exit (code:_) env   =  env with env_status = str to int code, env_exited = True

fn :: eval builtin source [String] ShEnv => ShEnv
      eval builtin source []       env   =  env
      eval builtin source (path:_) env   =
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
        env with env_status = bool to status ok

fn :: clean test args String [String] => [String]
      clean test args "["    raw_args =  strip closing bracket raw_args []
      clean test args _      raw_args =  raw_args

fn :: bool to status Bool  => Int
      bool to status True  =  0
      bool to status False =  1

fn :: eval test expr [String]      => Bool
      eval test expr []            =  False
      eval test expr ("!":rest)    =  not (eval test expr rest)
      eval test expr (x:[])        =  (str len x) > 0
      eval test expr (op_s:arg:[]) =
        | op_s == "-z" = (str len arg) == 0
        | op_s == "-n" = (str len arg) > 0
        | op_s == "-e" = file exists arg
        | op_s == "-f" = is file arg
        | op_s == "-d" = is dir arg
        | otherwise    = False
      eval test expr (a:op_s:b:_)  =
        | (op_s == "=") or (op_s == "==") = a == b
        | op_s == "!="                    = a != b
        | op_s == "-eq"                   = (str to int a) == (str to int b)
        | op_s == "-ne"                   = (str to int a) != (str to int b)
        | op_s == "-lt"                   = (str to int a) < (str to int b)
        | op_s == "-le"                   = (str to int a) <= (str to int b)
        | op_s == "-gt"                   = (str to int a) > (str to int b)
        | op_s == "-ge"                   = (str to int a) >= (str to int b)
        | otherwise                       = False

fn :: run exit trap if any ShEnv => ShEnv
      run exit trap if any env =
        | (str len (env_exit_trap env)) == 0 = env
        | otherwise =
          saved_rc = env_status env
          trap_node = parse shell (tokenize shell (env_exit_trap env))
          after_trap = evaluate shell trap_node in (env with env_exited = False, env_returning = False, env_exit_trap = "")
          after_trap with env_status = saved_rc

op :: run shell script String with [String] => String Int
      run shell script src    with pos_args =
        toks = tokenize shell src
        ast = parse shell toks
        init_env = ShEnv [] [] [] [] pos_args 0 "" "" False False 0 0 False False False False ""
        final_env = run exit trap if any (evaluate shell ast in init_env)
        (env_out final_env, env_status final_env)

-- ============================================================================
-- 4. BSD timeout(1) Implementation (FR-PROC-003)
-- ============================================================================

fn :: is decimal str String => Bool
      is decimal str s      =
        | (str len s) == 0 = False
        | otherwise        = is decimal loop s 0 (str len s) 0 0

fn :: is decimal loop String Int Int Int Int => Bool
      is decimal loop s idx len digits dots =
        | idx >= len = (digits > 0) and (dots <= 1)
        | (str char at s idx) == 46 = is decimal loop s (idx + 1) len digits (dots + 1)
        | ((str char at s idx) >= 48) and ((str char at s idx) <= 57) = is decimal loop s (idx + 1) len (digits + 1) dots
        | otherwise = False

fn :: parse scaled num String Int     => Int
      parse scaled num num_s  unit_ms =
        | not (is decimal str num_s) = 0 - 1
        | otherwise                  = parse scaled dot num_s (find char idx num_s 0 (str len num_s) 46) unit_ms

fn :: pow10 int Int => Int
      pow10 int n   =
        | n <= 0    = 1
        | otherwise = 10 * (pow10 int (n - 1))

fn :: parse scaled dot String Int Int => Int
      parse scaled dot num_s dot_i unit_ms =
        | dot_i < 0 = (str to int num_s) * unit_ms
        | otherwise =
          w_str = str slice num_s 0 dot_i
          f_raw = str slice num_s (dot_i + 1) (str len num_s)
          f_cut = take max3 str f_raw
          whole = parse opt digits w_str
          frac = parse opt digits f_cut
          denom = pow10 int (str len f_cut)
          (whole * unit_ms) + ((frac * unit_ms) / denom)

fn :: take max3 str String => String
      take max3 str s      =
        | (str len s) > 3 = str slice s 0 3
        | otherwise       = s

fn :: parse opt digits String => Int
      parse opt digits ""     =  0
      parse opt digits s      =  str to int s

fn :: parse duration len String Int => Int
      parse duration len raw    len =
        | len == 0 = 0 - 1
        | (len > 2) and ((str slice raw (len - 2) len) == "ms") = parse scaled num (str slice raw 0 (len - 2)) 1
        | (len > 1) and ((str slice raw (len - 1) len) == "s")  = parse scaled num (str slice raw 0 (len - 1)) 1000
        | (len > 1) and ((str slice raw (len - 1) len) == "m")  = parse scaled num (str slice raw 0 (len - 1)) 60000
        | (len > 1) and ((str slice raw (len - 1) len) == "h")  = parse scaled num (str slice raw 0 (len - 1)) 3600000
        | (len > 1) and ((str slice raw (len - 1) len) == "d")  = parse scaled num (str slice raw 0 (len - 1)) 86400000
        | otherwise                                             = parse scaled num raw 1000

op :: parse duration ms String => Int
      parse duration ms raw    =
        res = parse duration len raw (str len raw)
        clamp duration ms res

fn :: clamp duration ms Int => Int
      clamp duration ms res =
        | res < 0   = 0
        | otherwise = res

fn :: parse signal num String => Int
      parse signal num raw    =
        s = strip sig prefix raw
        match signal name s

fn :: strip sig prefix String => String
      strip sig prefix s      =
        | str starts with s "SIG" = str slice s 3 (str len s)
        | str starts with s "sig" = str slice s 3 (str len s)
        | otherwise               = s

fn :: match signal name String => Int
      match signal name s      =
        | (s == "HUP") or (s == "hup") or (s == "1")   = 1
        | (s == "INT") or (s == "int") or (s == "2")   = 2
        | (s == "QUIT") or (s == "quit") or (s == "3") = 3
        | (s == "KILL") or (s == "kill") or (s == "9") = 9
        | (s == "USR1") or (s == "usr1") or (s == "10") = 10
        | (s == "USR2") or (s == "usr2") or (s == "12") = 12
        | (s == "ALRM") or (s == "alrm") or (s == "14") = 14
        | (s == "TERM") or (s == "term") or (s == "15") = 15
        | otherwise                                    = 0 - 1

fn :: default timeout spec => TimeoutSpec
      default timeout spec =  TimeoutSpec "TERM" 10 False False False 0 "" []

fn :: parse timeout flags [String]       TimeoutSpec => TimeoutSpec
      parse timeout flags []             spec        =  spec
      parse timeout flags ("--preserve-status":rs) spec = parse timeout flags rs (spec with to_preserve = True)
      parse timeout flags ("--foreground":rs) spec   =  parse timeout flags rs (spec with to_foreground = True)
      parse timeout flags ("-s":sig:rs)  spec        =  parse timeout flags rs (spec with to_sig = sig)
      parse timeout flags ("-k":kdur:rs) spec        =
        kd = parse duration len kdur (str len kdur)
        parse timeout flags rs (spec with to_kill_ms = clamp duration ms kd, to_invalid = (to_invalid spec) or (kd < 0))
      parse timeout flags (arg:rs)       spec        =
        | str starts with arg "--signal=" =
          sig = str slice arg 9 (str len arg)
      parse timeout flags rs             (spec with to_sig = sig)
        | str starts with arg "--kill-after=" =
          kstr = str slice arg 13 (str len arg)
          kd = parse duration len kstr (str len kstr)
      parse timeout flags rs             (spec with to_kill_ms = clamp duration ms kd, to_invalid = (to_invalid spec) or (kd < 0))
        | str starts with arg "-" =
          spec with to_invalid = True
        | otherwise = finalize timeout positional ([arg] :: rs) spec

fn :: finalize timeout positional [String]           TimeoutSpec => TimeoutSpec
      finalize timeout positional []                 spec        =  spec with to_invalid = True
      finalize timeout positional (dur:[])           spec        =
        d_ms = parse duration len dur (str len dur)
        spec with to_dur_ms = clamp duration ms d_ms, to_invalid = True
      finalize timeout positional (dur:cmd:cmd_args) spec        =
        d_ms = parse duration len dur (str len dur)
        spec with to_dur_ms = clamp duration ms d_ms, to_invalid = (to_invalid spec) or (d_ms < 0), to_cmd = cmd, to_args = cmd_args

op :: parse timeout args [String] => TimeoutSpec
      parse timeout args args     =  parse timeout flags args (default timeout spec)

fn :: split colon path String => [String]
      split colon path s      =  split by char s 58

fn :: is executable file String => Bool
      is executable file path   =
        st = file lstat path
        (lstat exists st) and ((lstat kind st) != "dir") and ((((lstat mode st) / 64) % 2) == 1)

fn :: check cmd path String => Int
      check cmd path path   =
        check cmd lstat path (file lstat path)

fn :: check cmd lstat String (Bool, String, Int, Int, Int, String, String, Int, String) => Int
      check cmd lstat path st =
        | not (lstat exists st)    = 127
        | (lstat kind st) == "dir" = 126
        | is executable file path  = 0
        | otherwise                = 126

fn :: search cmd in dirs String [String] Int => Int
      search cmd in dirs _ [] best_err = best_err
      search cmd in dirs cmd (d:rest) best_err =
        search cmd step cmd rest best_err (check cmd path (d + "/" + cmd))

fn :: search cmd step String [String] Int Int => Int
      search cmd step _ _ _ 0 = 0
      search cmd step cmd rest _ 126 = search cmd in dirs cmd rest 126
      search cmd step cmd rest best_err _ = search cmd in dirs cmd rest best_err

fn :: resolve timeout cmd status String => Int
      resolve timeout cmd status cmd    =
        | (find char idx cmd 0 (str len cmd) 47) >= 0 = check cmd path cmd
        | otherwise =
          dirs = split colon path (sys getenv "PATH")
          search cmd in dirs cmd dirs 127

fn :: timeout expiry status TimeoutSpec Int     => Int
      timeout expiry status spec        sig_num =
        | to_preserve spec = 128 + sig_num
        | otherwise        = 124

fn :: quote timeout tok String => String
      quote timeout tok s      =  "'" + s + "'"

fn :: quote timeout list [String] => [String]
      quote timeout list []       =  []
      quote timeout list (x:xs)   =  [quote timeout tok x] :: (quote timeout list xs)

fn :: wait child deadline Int Int Int Int TimeoutSpec String => String Int
      wait child deadline pid deadline_ms kill_ms sig_num spec cap_file =
        | not (process exists pid) = finish child completed pid cap_file
        | (sys monotonic ms) >= deadline_ms =
          _ = posix kill pid sig_num
          _ = escalate kill after pid kill_ms
          out = read existing or empty cap_file
          _ = sys unlink cap_file
          (out, timeout expiry status spec sig_num)
        | otherwise =
          _ = sys poll 0 1 5
          wait child deadline pid deadline_ms kill_ms sig_num spec cap_file

fn :: escalate kill after Int Int     => Int
      escalate kill after pid kill_ms =
        | not (process exists pid) = 0
        | kill_ms <= 0             = posix kill pid 9
        | otherwise                =
          _ = sys poll 0 1 (min timeout wait kill_ms)
          posix kill pid 9

fn :: min timeout wait Int => Int
      min timeout wait ms  =
        | ms > 50   = 50
        | otherwise = ms

fn :: finish child completed Int String   => String Int
      finish child completed _   cap_file =
        rc_file = cap_file + ".rc"
        _ = wait for rc file rc_file 0
        rc_str = trim ws (read existing or empty rc_file)
        out = read existing or empty cap_file
        _ = sys unlink cap_file
        _ = sys unlink rc_file
        (out, parse opt digits rc_str)

fn :: wait for rc file String Int => ()
      wait for rc file rc_file tries =
        | (file exists rc_file) or (tries >= 20) = ()
        | otherwise =
          _ = sys poll 0 1 2
          wait for rc file rc_file (tries + 1)

op :: execute timeout TimeoutSpec => String Int
      execute timeout spec        =
        dispatch execute timeout (parse signal num (to_sig spec)) spec

fn :: dispatch execute timeout Int     TimeoutSpec => String Int
      dispatch execute timeout sig_num spec        =
        | (to_invalid spec) or ((str len (to_cmd spec)) == 0) or (sig_num < 0) = ("", 125)
        | (to_dur_ms spec) <= 0                                                = ("", timeout expiry status spec sig_num)
        | otherwise =
          cmd_st = resolve timeout cmd status (to_cmd spec)
          run timeout checked cmd_st sig_num spec

fn :: run timeout checked Int Int TimeoutSpec => String Int
      run timeout checked 126 _ _ = ("", 126)
      run timeout checked 127 _ _ = ("", 127)
      run timeout checked _ sig_num spec =
        my_pid = sys getpid
        now_ns = sys monotonic ns
        cap_file = "/tmp/.ut_to_" + (int to str my_pid) + "_" + (int to str (now_ns % 1000000)) + ".out"
        pid_file = cap_file + ".pid"
        rc_file = cap_file + ".rc"
        cmd_line = join with (quote timeout list ([to_cmd spec] :: (to_args spec))) " "
        bg_script = "(" + cmd_line + " > " + cap_file + " 2>&1; echo $? > " + rc_file + ") & echo $! > " + pid_file
        _ = shell exec bg_script
        child_pid = parse opt digits (trim ws (read existing or empty pid_file))
        _ = sys unlink pid_file
        deadline = (sys monotonic ms) + (to_dur_ms spec)
        wait child deadline child_pid deadline (to_kill_ms spec) sig_num spec cap_file

-- ============================================================================
-- 5. BSD nc(1) Netcat Implementation (FR-PROC-003)
-- ============================================================================

fn :: default nc spec => NcSpec
      default nc spec =  NcSpec False False False False False False 2000 "127.0.0.1" 0 0 ""

fn :: parse nc flags [String] [String] NcSpec => NcSpec
      parse nc flags [] pos spec = finalize nc pos pos spec
      parse nc flags ("-l":rest) pos spec = parse nc flags rest pos (spec with nc_listen = True)
      parse nc flags ("-u":rest) pos spec = parse nc flags rest pos (spec with nc_udp = True)
      parse nc flags ("-U":rest) pos spec = parse nc flags rest pos (spec with nc_unix = True)
      parse nc flags ("-lU":rest) pos spec = parse nc flags rest pos (spec with nc_listen = True, nc_unix = True)
      parse nc flags ("-Ul":rest) pos spec = parse nc flags rest pos (spec with nc_listen = True, nc_unix = True)
      parse nc flags ("-zU":rest) pos spec = parse nc flags rest pos (spec with nc_zero = True, nc_unix = True)
      parse nc flags ("-Uz":rest) pos spec = parse nc flags rest pos (spec with nc_zero = True, nc_unix = True)
      parse nc flags ("-zu":rest) pos spec = parse nc flags rest pos (spec with nc_zero = True, nc_udp = True)
      parse nc flags ("-uz":rest) pos spec = parse nc flags rest pos (spec with nc_zero = True, nc_udp = True)
      parse nc flags ("-z":rest) pos spec = parse nc flags rest pos (spec with nc_zero = True)
      parse nc flags ("-v":rest) pos spec = parse nc flags rest pos (spec with nc_verbose = True)
      parse nc flags ("-n":rest) pos spec = parse nc flags rest pos (spec with nc_nodns = True)
      parse nc flags ("-zv":rest) pos spec = parse nc flags rest pos (spec with nc_zero = True, nc_verbose = True)
      parse nc flags ("-vz":rest) pos spec = parse nc flags rest pos (spec with nc_zero = True, nc_verbose = True)
      parse nc flags ("-nvv":rest) pos spec = parse nc flags rest pos (spec with nc_nodns = True, nc_verbose = True)
      parse nc flags ("-w":sec:rest) pos spec = parse nc flags rest pos (spec with nc_timeout_ms = (str to int sec) * 1000)
      parse nc flags (arg:rest) pos spec = parse nc flags rest (pos :: [arg]) spec

fn :: parse port or range String => Int Int
      parse port or range s      =
        parse port range at s (find char idx s 0 (str len s) 45)

fn :: parse port range at String Int => Int Int
      parse port range at s dash_i =
        | dash_i > 0 =
          p1 = str to int (str slice s 0 dash_i)
          p2 = str to int (str slice s (dash_i + 1) (str len s))
          (p1, p2)
        | otherwise =
          p = str to int s
          (p, p)

fn :: finalize nc pos [String] NcSpec => NcSpec
      finalize nc pos []       spec   =  spec
      finalize nc pos (p1:[])  spec   =
        | nc_unix spec   = spec with nc_unix_path = p1
        | nc_listen spec =
          pr = parse port or range p1
          spec with nc_port = fst pr, nc_port_end = snd pr
        | otherwise      = spec with nc_host = p1
      finalize nc pos (h:p:_)  spec   =
        pr = parse port or range p
        spec with nc_host = h, nc_port = fst pr, nc_port_end = snd pr

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

fn :: nc udp loopback transfer String String Int => Bool String
      nc udp loopback transfer payload host pnum =
        addr_res = sock resolve host host pnum
        nc udp with addr payload addr_res

fn :: nc udp with addr String SockAddrRes => Bool String
      nc udp with addr _ (SockAddrErr msg) = (False, msg)
      nc udp with addr payload (SockAddrOk addr) =
        srv0 = unpack sock ok (sock bind (sock udp) addr)
        cli0 = sock udp
        cli1 = unpack sock ok (sock send to cli0 (str to byte list payload) addr)
        pair1 = sock deliver dgram cli1 srv0
        srv1 = unpack pair srv pair1
        unpack udp dgram (sock recv from srv1 65536)

fn :: unpack udp dgram SockRecvFromRes   => Bool String
      unpack udp dgram (SockFromOk _ bytes _) = (True, byte list to str bytes)
      unpack udp dgram (SockFromErr err) =  (False, err)

fn :: format nc probe Int String Int Bool => Int String
      format nc probe 0 target port True =
        (0, "Connection to " + target + " " + (int to str port) + " port [tcp/*] succeeded!" + (char from code 10))
      format nc probe 0 _ _ False = (0, "")
      format nc probe _ target port True =
        (1, "nc: connect to " + target + " port " + (int to str port) + " (tcp) failed" + (char from code 10))
      format nc probe _ _ _ False = (1, "")

fn :: recv all fd Int Int String => String
      recv all fd fd timeout_ms acc =
        recv all step fd (sys recv fd 65536 timeout_ms) acc

fn :: recv all step Int (Int, String) String => String
      recv all step fd (0, chunk) acc = recv all fd fd 100 (acc + chunk)
      recv all step _ _ acc = acc

fn :: finish nc listen (Int, Int) NcSpec String => Int String
      finish nc listen (rc, lfd) spec payload =
        | rc != 0   = (1, "nc: listen failed")
        | otherwise =
          acc_res = sys accept lfd (nc_timeout_ms spec)
          handle nc accepted lfd acc_res spec payload

fn :: handle nc accepted Int (Int, Int, String) NcSpec String => Int String
      handle nc accepted lfd (arc, sfd, _) spec payload =
        | arc != 0 =
          _ = posix close lfd
          (0, payload)
        | otherwise =
          rx = recv all fd sfd 200 ""
          _ = send if nonempty sfd payload (nc_timeout_ms spec)
          _ = sys shutdown write sfd
          _ = posix close sfd
          _ = posix close lfd
          (0, rx)

fn :: send if nonempty Int String Int => Int
      send if nonempty _ "" _ = 0
      send if nonempty fd payload timeout_ms =
        sres = sys send fd payload timeout_ms
        sock rc sres

fn :: execute nc zero range NcSpec Int Int Int [String] => Int String
      execute nc zero range spec cur_p end_p rc_acc log_acc =
        | cur_p > end_p = (rc_acc, join with log_acc "")
        | otherwise     =
          cres = sys tcp connect (nc_host spec) cur_p (nc_timeout_ms spec)
          one = finish nc zero one cres spec cur_p
          next_rc = select zero rc rc_acc (fst one)
          execute nc zero range spec (cur_p + 1) end_p next_rc (log_acc :: [snd one])

fn :: select zero rc Int Int => Int
      select zero rc 0   0   =  0
      select zero rc _   _   =  1

fn :: finish nc zero one (Int, Int) NcSpec Int => Int String
      finish nc zero one (0, cfd) spec port =
        _ = posix close cfd
        format nc probe 0 (nc_host spec) port (nc_verbose spec)
      finish nc zero one _ spec port =
        format nc probe 1 (nc_host spec) port (nc_verbose spec)

fn :: execute nc tcp client NcSpec String => Int String
      execute nc tcp client spec payload =
        execute nc tcp conn (sys tcp connect (nc_host spec) (nc_port spec) (nc_timeout_ms spec)) spec payload

fn :: execute nc tcp conn (Int, Int) NcSpec String => Int String
      execute nc tcp conn (0, cfd) spec payload =
        _ = send if nonempty cfd payload (nc_timeout_ms spec)
        _ = sys shutdown write cfd
        rx = recv all fd cfd 200 ""
        _ = posix close cfd
        (0, select client rx rx payload)
      execute nc tcp conn _ spec payload =
        finish nc transfer (nc loopback transfer payload to (nc_host spec) port (nc_port spec))

fn :: select client rx String String  => String
      select client rx ""     payload =  payload
      select client rx rx     _       =  rx

fn :: finish nc transfer (Bool, String) => Int String
      finish nc transfer (True, out)    =  (0, out)
      finish nc transfer (False, err)   =  (1, "nc: " + err + (char from code 10))

op :: execute nc NcSpec with String  => Int String
      execute nc spec   with payload =
        | nc_unix spec                                              = execute nc unix spec (nc_unix_path spec) payload
        | (nc_nodns spec) and (not (is valid ipv4 (nc_host spec)))  = (1, "nc: nodns requires numeric IP address")
        | nc_udp spec                                               = finish nc transfer (nc udp loopback transfer payload (nc_host spec) (nc_port spec))
        | (nc_listen spec) and (not (is valid port (nc_port spec))) = (1, "nc: invalid port")
        | nc_listen spec                                            = finish nc listen (sys tcp listen reuseport (nc_host spec) (nc_port spec) 4) spec payload
        | (nc_zero spec) and ((not (is valid port (nc_port spec))) or (not (is valid port (nc_port_end spec)))) = (1, "nc: invalid port")
        | nc_zero spec                                              = execute nc zero range spec (nc_port spec) (nc_port_end spec) 0 []
        | not (is valid port (nc_port spec))                        = (1, "nc: invalid port")
        | otherwise                                                 = execute nc tcp client spec payload

fn :: finish nc unix conn (Int, Int) NcSpec String => Int String
      finish nc unix conn (0, cfd) spec payload =
        | nc_zero spec =
          _ = posix close cfd
          (0, "")
        | otherwise =
          _ = send if nonempty cfd payload (nc_timeout_ms spec)
          _ = sys shutdown write cfd
          rx = recv all fd cfd 200 ""
          _ = posix close cfd
          (0, select client rx rx payload)
      finish nc unix conn _ _ _ = (1, "nc: unix connect failed")

fn :: execute nc unix NcSpec String String => Int String
      execute nc unix spec upath payload =
        | not (is safe unix path upath) = (1, "nc: invalid unix socket path")
        | nc_listen spec                = finish nc listen (sys unix listen upath 4) spec payload
        | otherwise                     = finish nc unix conn (sys unix connect upath (nc_timeout_ms spec)) spec payload
