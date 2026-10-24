-- sec_lib.rg — Pure Ridgun OpenBSD doas(1), doas.conf(5) & BSD sha256(1) Security Library
-- @implements FR-SEC-001
-- @implements FR-SEC-002

import posix
import crypto

-- ============================================================================
-- Algebraic Data Types (ADTs)
-- ============================================================================

type DoasAction = ActPermit | ActDeny

type DoasRule = dr_action: DoasAction, dr_nopass: Bool, dr_nolog: Bool, dr_persist: Bool, dr_keepenv: Bool, dr_setenv: [String], dr_ident: String, dr_target: String, dr_has_cmd: Bool, dr_cmd: String, dr_has_args: Bool, dr_args: [String]

type DoasParseResult = DParseOk [DoasRule] | DParseErr Int String

type DoasDecision = DecDeny | DecPermit Bool DoasRule

type DoasSpec = d_check_conf: String, d_target_user: String, d_non_interactive: Bool, d_clear_persist: Bool, d_shell_mode: Bool, d_cmd: String, d_cmd_args: [String]

type Sha256Spec = sh_quiet: Bool, sh_reverse: Bool, sh_has_str: Bool, sh_str: String, sh_key: String, sh_argon_salt: String, sh_verify_phc: String, sh_files: [String]

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

fn :: is ws Int => Bool
      is ws c   =  (c == 32) or (c == 9) or (c == 10) or (c == 13)

fn :: list len [A]    => Int
      list len []     =  0
      list len (_:xs) =  1 + (list len xs)

fn :: fst (A, B) => A
      fst (a, _) =  a

fn :: snd (A, B) => B
      snd (_, b) =  b

fn :: list contains str [String] String => Bool
      list contains str []       _      =  False
      list contains str (x:xs)   target =
        | x == target = True
        | otherwise   = list contains str xs target

fn :: str list eq [String] [String] => Bool
      str list eq []       []       =  True
      str list eq []       (_:_)    =  False
      str list eq (_:_)    []       =  False
      str list eq (x:xs)   (y:ys)   =
        | x == y    = str list eq xs ys
        | otherwise = False

fn :: join with [String] String => String
      join with []       _      =  ""
      join with (x:[])   _      =  x
      join with (x:xs)   sep    =  x + sep + (join with xs sep)

fn :: find char idx String Int Int Int => Int
      find char idx s idx len target_ch =
        | idx >= len                       = 0 - 1
        | (str char at s idx) == target_ch = idx
        | otherwise                        = find char idx s (idx + 1) len target_ch

-- ============================================================================
-- 1. doas.conf(5) Lexer & Parser — FR-SEC-001
-- ============================================================================

fn :: is permit action DoasAction => Bool
      is permit action ActPermit  =  True
      is permit action ActDeny    =  False

op :: tokenize doas line String => [String]
      tokenize doas line line   =  lex doas line line 0 (str len line) "" False []

fn :: push doas tok String Bool [String] => [String]
      push doas tok cur True acc = acc :: [cur]
      push doas tok cur False acc =
        | (str len cur) == 0 = acc
        | otherwise          = acc :: [cur]

fn :: lex doas line String Int Int String Bool [String] => [String]
      lex doas line s idx len cur quoted acc =
        | idx >= len = push doas tok cur quoted acc
        | ((str char at s idx) == 35) and (not quoted) and ((str len cur) == 0) = acc
        | (str char at s idx) == 34 = lex doas quote s (idx + 1) len cur acc
        | (not quoted) and (is ws (str char at s idx)) = lex doas line s (idx + 1) len "" False (push doas tok cur False acc)
        | (not quoted) and (((str char at s idx) == 123) or ((str char at s idx) == 125)) = lex doas line s (idx + 1) len "" False ((push doas tok cur False acc) :: [str slice s idx (idx + 1)])
        | otherwise = lex doas line s (idx + 1) len (cur + (str slice s idx (idx + 1))) quoted acc

fn :: lex doas quote String Int Int String [String] => [String]
      lex doas quote s idx len cur acc =
        | idx >= len = push doas tok cur True acc
        | (str char at s idx) == 34 = lex doas line s (idx + 1) len "" False (push doas tok cur True acc)
        | ((str char at s idx) == 92) and ((idx + 1) < len) = lex doas quote s (idx + 2) len (cur + (str slice s (idx + 1) (idx + 2))) acc
        | otherwise = lex doas quote s (idx + 1) len (cur + (str slice s idx (idx + 1))) acc

fn :: ends with backslash String => Bool
      ends with backslash s      =
        | (str len s) == 0                        = False
        | (str char at s ((str len s) - 1)) == 92 = True
        | otherwise                               = False

fn :: join doas cont [String] String Int [String] [Int] => [String] [Int]
      join doas cont [] cur start_ln acc_l acc_n =
        | (str len cur) == 0 = (acc_l, acc_n)
        | otherwise          = (acc_l :: [cur], acc_n :: [start_ln])
      join doas cont (line:rest) cur start_ln acc_l acc_n =
        | ends with backslash line =
          trimmed = str slice line 0 ((str len line) - 1)
      join doas cont rest (cur + trimmed + " ") start_ln acc_l acc_n
        | otherwise =
          full = cur + line
          join doas cont rest "" (start_ln + 1) (acc_l :: [full]) (acc_n :: [start_ln])

op :: parse doas config String => DoasParseResult
      parse doas config raw    =
        raw_lines = split lines raw
        joined = join doas cont raw_lines "" 1 [] []
        parse doas lines (fst joined) (snd joined) []

fn :: head int [Int] Int => Int
      head int []    def =  def
      head int (x:_) _   =  x

fn :: tail int [Int]  => [Int]
      tail int []     =  []
      tail int (_:xs) =  xs

fn :: parse doas lines [String] [Int] [DoasRule] => DoasParseResult
      parse doas lines [] _ acc = DParseOk acc
      parse doas lines (line:rest) lnums acc =
        line_no = head int lnums 1
        toks = tokenize doas line line
        parse doas line tokens toks rest (tail int lnums) line_no acc

fn :: dummy rule => DoasRule
      dummy rule =  DoasRule ActDeny False False False False [] "" "root" False "" False []

fn :: parse doas line tokens [String] [String] [Int] Int [DoasRule] => DoasParseResult
      parse doas line tokens [] rest lnums _ acc = parse doas lines rest lnums acc
      parse doas line tokens (act_tok:more) rest lnums line_no acc =
        | act_tok == "permit" = continue doas line (parse rule options more ((dummy rule) with dr_action = ActPermit) line_no) rest lnums line_no acc
        | act_tok == "deny"   = continue doas line (parse rule options more ((dummy rule) with dr_action = ActDeny) line_no) rest lnums line_no acc
        | otherwise           = DParseErr line_no ("doas.conf:" + (int to str line_no) + ": syntax error: expected permit or deny")

fn :: continue doas line (Bool, DoasRule, String) [String] [Int] Int [DoasRule] => DoasParseResult
      continue doas line (False, _, err_msg) _ _ line_no _ = DParseErr line_no err_msg
      continue doas line (True, rule, _) rest lnums _ acc =
        parse doas lines rest lnums (acc :: [rule])

fn :: is reserved kw String => Bool
      is reserved kw s      =
        (s == "as") or (s == "cmd") or (s == "args") or (s == "nopass") or (s == "nolog") or (s == "persist") or (s == "keepenv") or (s == "setenv") or (s == "\{") or (s == "\}")

fn :: parse rule options [String] DoasRule Int => Bool DoasRule String
      parse rule options [] _ line_no =
        (False, (dummy rule), "doas.conf:" + (int to str line_no) + ": syntax error: missing identity")
      parse rule options (t:rest) rule line_no =
        | t == "nopass"  = parse rule options rest (rule with dr_nopass = True) line_no
        | t == "nolog"   = parse rule options rest (rule with dr_nolog = True) line_no
        | t == "persist" = parse rule options rest (rule with dr_persist = True) line_no
        | t == "keepenv" = parse rule options rest (rule with dr_keepenv = True) line_no
        | t == "setenv"  = parse rule setenv rest rule line_no
        | is reserved kw t = (False, (dummy rule), "doas.conf:" + (int to str line_no) + ": syntax error: unexpected keyword '" + t + "'")
        | otherwise      = parse rule after ident rest (rule with dr_ident = t) line_no

fn :: parse rule setenv [String] DoasRule Int => Bool DoasRule String
      parse rule setenv [] _ line_no =
        (False, (dummy rule), "doas.conf:" + (int to str line_no) + ": syntax error: expected '\{' after setenv")
      parse rule setenv (t:rest) rule line_no =
        | t == "\{" = collect setenv block rest rule line_no
        | otherwise = (False, (dummy rule), "doas.conf:" + (int to str line_no) + ": syntax error: expected '\{' after setenv")

fn :: collect setenv block [String] DoasRule Int => Bool DoasRule String
      collect setenv block [] _ line_no =
        (False, (dummy rule), "doas.conf:" + (int to str line_no) + ": syntax error: unclosed '\{' in setenv")
      collect setenv block (t:rest) rule line_no =
        | t == "\}" = parse rule options rest rule line_no
        | otherwise = collect setenv block rest (rule with dr_setenv = ((dr_setenv rule) :: [t])) line_no

fn :: parse rule after ident [String] DoasRule Int => Bool DoasRule String
      parse rule after ident [] rule _ = (True, rule, "")
      parse rule after ident (t:rest) rule line_no =
        | t == "as"  = parse rule as rest rule line_no
        | t == "cmd" = parse rule cmd rest rule line_no
        | otherwise  = (False, (dummy rule), "doas.conf:" + (int to str line_no) + ": syntax error: unexpected token '" + t + "'")

fn :: parse rule as [String] DoasRule Int => Bool DoasRule String
      parse rule as [] _ line_no =
        (False, (dummy rule), "doas.conf:" + (int to str line_no) + ": syntax error: missing target after 'as'")
      parse rule as (target:rest) rule line_no =
        | is reserved kw target = (False, (dummy rule), "doas.conf:" + (int to str line_no) + ": syntax error: invalid target after 'as'")
        | otherwise             = parse rule after as rest (rule with dr_target = target) line_no

fn :: parse rule after as [String] DoasRule Int => Bool DoasRule String
      parse rule after as [] rule _ = (True, rule, "")
      parse rule after as (t:rest) rule line_no =
        | t == "cmd" = parse rule cmd rest rule line_no
        | otherwise  = (False, (dummy rule), "doas.conf:" + (int to str line_no) + ": syntax error: unexpected token '" + t + "'")

fn :: parse rule cmd [String] DoasRule Int => Bool DoasRule String
      parse rule cmd [] _ line_no =
        (False, (dummy rule), "doas.conf:" + (int to str line_no) + ": syntax error: missing command after 'cmd'")
      parse rule cmd (cmd_name:rest) rule line_no = parse rule cmd args rest (rule with dr_has_cmd = True, dr_cmd = cmd_name) line_no

fn :: parse rule cmd args [String] DoasRule Int => Bool DoasRule String
      parse rule cmd args [] rule _ = (True, rule, "")
      parse rule cmd args (t:rest) rule line_no =
        | t == "args" = (True, rule with dr_has_args = True, dr_args = rest, "")
        | otherwise   = (False, (dummy rule), "doas.conf:" + (int to str line_no) + ": syntax error: expected 'args' after command")

-- ============================================================================
-- 2. Policy Engine ("Last Matching Rule Wins") — FR-SEC-001
-- ============================================================================

fn :: match rule ident String String [String] => Bool
      match rule ident rule_id caller_user caller_groups =
        | str starts with rule_id ":" = list contains str caller_groups (str slice rule_id 1 (str len rule_id))
        | otherwise                   = rule_id == caller_user

fn :: match rule cmd DoasRule String [String] => Bool
      match rule cmd rule cmd cmd_args =
        | not (dr_has_cmd rule)  = True
        | (dr_cmd rule) != cmd   = False
        | not (dr_has_args rule) = True
        | otherwise              = str list eq (dr_args rule) cmd_args

fn :: rule matches DoasRule String [String] String String [String] => Bool
      rule matches rule caller_user caller_groups target_user cmd cmd_args =
        id_ok = match rule ident (dr_ident rule) caller_user caller_groups
        tgt_ok = (dr_target rule) == target_user
        cmd_ok = match rule cmd rule cmd cmd_args
        id_ok and tgt_ok and cmd_ok

op :: evaluate doas rules [DoasRule] for String in [String] as String running String with [String] => DoasDecision
      evaluate doas rules rules for caller_user in caller_groups as target_user running cmd with cmd_args =
        eval rules loop rules caller_user caller_groups target_user cmd cmd_args DecDeny

fn :: eval rules loop [DoasRule] String [String] String String [String] DoasDecision => DoasDecision
      eval rules loop [] _ _ _ _ _ current_dec = current_dec
      eval rules loop (r:rest) caller_user caller_groups target_user cmd cmd_args current_dec =
        | rule matches r caller_user caller_groups target_user cmd cmd_args = eval rules loop rest caller_user caller_groups target_user cmd cmd_args (decision from rule r)
        | otherwise                                                         = eval rules loop rest caller_user caller_groups target_user cmd cmd_args current_dec

fn :: decision from rule DoasRule => DoasDecision
      decision from rule r        =
        | is permit action (dr_action r) = DecPermit (dr_nopass r) r
        | otherwise                      = DecDeny

op :: format doas audit DoasDecision       => String
      format doas audit DecDeny            =  "deny"
      format doas audit (DecPermit True _) =  "permit nopass"
      format doas audit (DecPermit False _) = "permit"

-- ============================================================================
-- 3. CLI Parsing, Environment Sanitization & Direct Syscall Execution
-- ============================================================================

op :: parse doas args [String] => DoasSpec
      parse doas args args     =  parse doas cli args (DoasSpec "" "root" False False False "" [])

fn :: parse doas cli [String]   DoasSpec => DoasSpec
      parse doas cli []         spec     =  finalize doas shell spec
      parse doas cli (arg:rest) spec     =
        | arg == "-n" = parse doas cli rest (spec with d_non_interactive = True)
        | arg == "-L" = parse doas cli rest (spec with d_clear_persist = True)
        | arg == "-s" = parse doas cli rest (spec with d_shell_mode = True)
        | arg == "-C" = parse doas flag c rest spec
        | arg == "-u" = parse doas flag u rest spec
        | arg == "--" = finalize doas shell (assign doas cmd rest spec)
        | otherwise   = finalize doas shell (assign doas cmd ([arg] :: rest) spec)

fn :: finalize doas shell DoasSpec => DoasSpec
      finalize doas shell spec     =
        apply doas shell ((d_shell_mode spec) and ((str len (d_cmd spec)) == 0)) spec

fn :: apply doas shell Bool  DoasSpec => DoasSpec
      apply doas shell False spec     =  spec
      apply doas shell True  spec     =
        sh_bin = select default shell (sys getenv "SHELL")
        res = spec with d_cmd = sh_bin
        res

fn :: select default shell String => String
      select default shell ""     =  "/bin/sh"
      select default shell s      =  s

fn :: parse doas flag c [String] DoasSpec => DoasSpec
      parse doas flag c [] spec = spec
      parse doas flag c (conf:rest) spec = parse doas cli rest (spec with d_check_conf = conf)

fn :: parse doas flag u [String] DoasSpec => DoasSpec
      parse doas flag u [] spec = spec
      parse doas flag u (u:rest) spec = parse doas cli rest (spec with d_target_user = u)

fn :: assign doas cmd [String]   DoasSpec => DoasSpec
      assign doas cmd []         spec     =  spec
      assign doas cmd (cmd:args) spec     =  spec with d_cmd = cmd, d_cmd_args = args

op :: build doas env DoasRule for String      as String      => [String]
      build doas env rule     for caller_user as target_user =
        base = ["PATH=/bin:/sbin:/usr/bin:/usr/sbin:/usr/local/bin:/usr/local/sbin", "DOAS_USER=" + caller_user, "USER=" + target_user, "LOGNAME=" + target_user, "HOME=/home/" + target_user]
        kept = append keepenv base (dr_keepenv rule)
        apply setenv specs kept (dr_setenv rule)

fn :: safe keepenv keys => [String]
      safe keepenv keys =  ["TERM", "LANG", "LC_ALL", "LC_CTYPE", "DISPLAY", "SSH_AUTH_SOCK", "TZ", "EDITOR", "PAGER", "SAFE_CUSTOM_VAR"]

fn :: collect safe env [String] [String] => [String]
      collect safe env []       acc      =  acc
      collect safe env (k:rest) acc      =
        collect safe env step k (sys getenv k) rest acc

fn :: collect safe env step String String [String] [String] => [String]
      collect safe env step _ "" rest acc = collect safe env rest acc
      collect safe env step k v rest acc = collect safe env rest (acc :: [k + "=" + v])

fn :: is dangerous env key String => Bool
      is dangerous env key k      =
        (str starts with k "LD_") or (k == "IFS") or (k == "ENV") or (k == "BASH_ENV") or (k == "PYTHONPATH") or (k == "PERL5LIB")

fn :: filter safe env list [String] [String] => [String]
      filter safe env list [] acc = acc
      filter safe env list (kv:rest) acc =
        eq_i = find char idx kv 0 (str len kv) 61
        key = str slice kv 0 eq_i
        filter safe env step key kv rest acc

fn :: filter safe env step String String [String] [String] => [String]
      filter safe env step key kv rest acc =
        | is dangerous env key key = filter safe env list rest acc
        | otherwise                = filter safe env list rest (acc :: [kv])

fn :: append keepenv [String] Bool  => [String]
      append keepenv base     False =  base
      append keepenv base     True  =
        kept = collect safe env (safe keepenv keys) base
        filter safe env list kept []

fn :: resolve setenv ref String => String
      resolve setenv ref item   =
        | str starts with item "$" =
          k = str slice item 1 (str len item)
          k + "=" + (sys getenv k)
        | otherwise = item + "=" + (sys getenv item)

fn :: apply setenv specs [String] [String]    => [String]
      apply setenv specs env_list []          =  filter safe env list env_list []
      apply setenv specs env_list (item:rest) =
        | str starts with item "-"                       = apply setenv specs (remove env key env_list (str slice item 1 (str len item)) []) rest
        | (find char idx item 0 (str len item) 61) > 0   = apply setenv specs (put env kv env_list item) rest
        | otherwise                                      = apply setenv specs (put env kv env_list (resolve setenv ref item)) rest

fn :: remove env key [String] String [String] => [String]
      remove env key [] _ acc = acc
      remove env key (kv:rest) key acc =
        | str starts with kv (key + "=") = remove env key rest key acc
        | otherwise                      = remove env key rest key (acc :: [kv])

fn :: put env kv [String] String => [String]
      put env kv env_list kv     =
        eq_i = find char idx kv 0 (str len kv) 61
        key = str slice kv 0 eq_i
        cleaned = remove env key env_list key []
        cleaned :: [kv]

fn :: execute privileged DoasRule DoasSpec String      => Int
      execute privileged rule     spec     caller_user =
        auth_ok = authenticate doas rule spec caller_user
        run after auth auth_ok rule spec caller_user

fn :: verify doas password String => Bool
      verify doas password pw     =
        | (str len pw) == 0 = False
        | otherwise         = check password against verifiers pw (sys getenv "DOAS_AUTH_PHC") (sys getenv "DOAS_AUTH_SHA256")

fn :: check password against verifiers String String String => Bool
      check password against verifiers pw phc hex =
        | (str len phc) > 0 = argon2id verify pw phc
        | (str len hex) > 0 = ct equal (sha256 hex pw) hex
        | otherwise         = False

fn :: select candidate password String String      => String
      select candidate password ""     caller_user =  sys read pass ("doas (" + caller_user + ") password: ")
      select candidate password env_pw _           =  env_pw

fn :: read candidate password String      => String
      read candidate password caller_user =  select candidate password (sys getenv "DOAS_PASS") caller_user

fn :: authenticate doas DoasRule DoasSpec String      => Bool
      authenticate doas rule     spec     caller_user =
        | dr_nopass rule           = True
        | d_non_interactive spec   = False
        | otherwise                = verify doas password (read candidate password caller_user)

fn :: run after auth Bool DoasRule DoasSpec String => Int
      run after auth False _ _ _ =
        _ = posix write 2 ("doas: Authentication required" + (char from code 10))
        1
      run after auth True rule spec caller_user =
        ug = sys lookup uid gid (d_target_user spec)
        t_uid = fst ug
        t_gid = snd ug
        _ = switch if valid ids t_uid t_gid
        env_list = build doas env rule for caller_user as (d_target_user spec)
        sys execve (d_cmd spec) (d_cmd_args spec) env_list

fn :: switch if valid ids Int Int => Int
      switch if valid ids t_uid t_gid =
        | (t_uid < 0) or (t_gid < 0) = 0
        | (sys getuid) != 0          = 0
        | otherwise                  =
          _ = sys setresgid t_gid
          _ = sys setgroups [t_gid]
          sys setresuid t_uid

-- ============================================================================
-- 5. BSD sha256(1) Cryptographic Digest, HMAC-SHA256 & Argon2id (FR-SEC-002)
-- ============================================================================

fn :: default sha256 spec => Sha256Spec
      default sha256 spec =  Sha256Spec False False False "" "" "" "" []

fn :: parse sha256 loop [String]      Sha256Spec => Sha256Spec
      parse sha256 loop []            spec       =  spec
      parse sha256 loop ("-q":rest)   spec       =  parse sha256 loop rest (spec with sh_quiet = True)
      parse sha256 loop ("-r":rest)   spec       =  parse sha256 loop rest (spec with sh_reverse = True)
      parse sha256 loop ("-qr":rest)  spec       =  parse sha256 loop rest (spec with sh_quiet = True, sh_reverse = True)
      parse sha256 loop ("-rq":rest)  spec       =  parse sha256 loop rest (spec with sh_quiet = True, sh_reverse = True)
      parse sha256 loop ("-s":s:rest) spec       =  parse sha256 loop rest (spec with sh_has_str = True, sh_str = s)
      parse sha256 loop ("-k":k:rest) spec       =  parse sha256 loop rest (spec with sh_key = k)
      parse sha256 loop ("--argon2id":s:rest) spec = parse sha256 loop rest (spec with sh_argon_salt = s)
      parse sha256 loop ("--verify":p:rest) spec =  parse sha256 loop rest (spec with sh_verify_phc = p)
      parse sha256 loop (f:rest)      spec       =  parse sha256 loop rest (spec with sh_files = ((sh_files spec) :: [f]))

op :: parse sha256 args [String] => Sha256Spec
      parse sha256 args args     =  parse sha256 loop args (default sha256 spec)

fn :: compute digest for Sha256Spec String => String
      compute digest for spec       data   =
        | (str len (sh_argon_salt spec)) > 0 = argon2id hash data (sh_argon_salt spec) 64 2 1
        | (str len (sh_key spec)) > 0        = bytes to hex str (hmac sha256 (sh_key spec) data)
        | otherwise                          = sha256 hex data

fn :: algo label Sha256Spec => String
      algo label spec       =
        | (str len (sh_key spec)) > 0 = "HMAC-SHA256"
        | otherwise                   = "SHA256"

fn :: format sha256 entry Sha256Spec String String Bool => String
      format sha256 entry spec label digest is_str =
        | sh_quiet spec                    = digest
        | (sh_reverse spec) and is_str     = digest + " \"" + label + "\""
        | sh_reverse spec                  = digest + "  " + label
        | is_str                           = (algo label spec) + " (\"" + label + "\") = " + digest
        | otherwise                        = (algo label spec) + " (" + label + ") = " + digest

fn :: file readable check String => Bool
      file readable check path   =
        fd = posix open path (o rdonly) 0
        close if valid fd fd

fn :: close if valid fd Int => Bool
      close if valid fd fd  =
        | fd < 0    = False
        | otherwise =
          _ = posix close fd
          True

fn :: eval one sha256 file Sha256Spec String => Int String
      eval one sha256 file spec f =
        | not (file readable check f) = (1, "sha256: " + f + ": No such file or directory")
        | otherwise                   =
          data = read file f
          dig = compute digest for spec data
          (0, format sha256 entry spec f dig False)

fn :: merge sha256 rc Int Int => Int
      merge sha256 rc a   b   =
        | a != 0    = a
        | otherwise = b

fn :: eval sha256 files Sha256Spec [String] Int [String] => Int [String]
      eval sha256 files _ [] rc acc = (rc, acc)
      eval sha256 files spec (f:rest) rc acc =
        one = eval one sha256 file spec f
        eval sha256 files spec rest (merge sha256 rc rc (fst one)) (acc :: [snd one])

fn :: verify sha256 phc Sha256Spec String     => Int String
      verify sha256 phc spec       stdin_data =
        | argon2id verify (select sha256 input spec stdin_data) (sh_verify_phc spec) = (0, "OK")
        | otherwise                                                                  = (1, "FAILED")

fn :: finish sha256 files (Int, [String]) => Int String
      finish sha256 files (rc, lines)     =
        (rc, join sec lines lines (char from code 10))

op :: evaluate sha256 Sha256Spec with String     => Int String
      evaluate sha256 spec       with stdin_data =
        | (str len (sh_verify_phc spec)) > 0     = verify sha256 phc spec stdin_data
        | sh_has_str spec                        = (0, format sha256 entry spec (sh_str spec) (compute digest for spec (sh_str spec)) True)
        | (sec str list len (sh_files spec)) > 0 = finish sha256 files (eval sha256 files spec (sh_files spec) 0 [])
        | otherwise                              = (0, compute digest for spec stdin_data)

fn :: select sha256 input Sha256Spec String     => String
      select sha256 input spec       stdin_data =
        | sh_has_str spec = sh_str spec
        | otherwise       = stdin_data

fn :: sec str list len [String] => Int
      sec str list len []       =  0
      sec str list len (_:rest) =  1 + (sec str list len rest)

fn :: join sec lines [String] String => String
      join sec lines []       _      =  ""
      join sec lines (x:[])   _      =  x
      join sec lines (x:xs)   sep    =  x + sep + (join sec lines xs sep)
