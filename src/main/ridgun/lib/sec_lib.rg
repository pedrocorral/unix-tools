-- sec_lib.rg — Pure Ridgun OpenBSD doas(1) & doas.conf(5) Security Policy Engine
-- @implements FR-SEC-001

import posix

-- ============================================================================
-- Algebraic Data Types (ADTs)
-- ============================================================================

type DoasAction = ActPermit | ActDeny

type DoasRule = dr_action: DoasAction, dr_nopass: Bool, dr_nolog: Bool, dr_persist: Bool, dr_keepenv: Bool, dr_setenv: [String], dr_ident: String, dr_target: String, dr_has_cmd: Bool, dr_cmd: String, dr_has_args: Bool, dr_args: [String]

type DoasParseResult = DParseOk [DoasRule] | DParseErr Int String

type DoasDecision = DecDeny | DecPermit Bool DoasRule

type DoasSpec = d_check_conf: String, d_target_user: String, d_non_interactive: Bool, d_cmd: String, d_cmd_args: [String]

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
        | (str char at s idx) == 34 = lex doas line s (idx + 1) len cur True acc
        | ((str char at s idx) == 92) and ((idx + 1) < len) = lex doas quote s (idx + 2) len (cur + (str slice s (idx + 1) (idx + 2))) acc
        | otherwise = lex doas quote s (idx + 1) len (cur + (str slice s idx (idx + 1))) acc

op :: parse doas config String => DoasParseResult
      parse doas config raw    =
        lines = split lines raw
        parse doas lines lines 1 []

fn :: parse doas lines [String] Int [DoasRule] => DoasParseResult
      parse doas lines [] _ acc = DParseOk acc
      parse doas lines (line:rest) line_no acc =
        toks = tokenize doas line line
        parse doas line tokens toks rest line_no acc

fn :: dummy rule => DoasRule
      dummy rule =  DoasRule ActDeny False False False False [] "" "root" False "" False []

fn :: parse doas line tokens [String] [String] Int [DoasRule] => DoasParseResult
      parse doas line tokens [] rest line_no acc = parse doas lines rest (line_no + 1) acc
      parse doas line tokens (act_tok:more) rest line_no acc =
        | act_tok == "permit" = continue doas line (parse rule options more ((dummy rule) with dr_action: ActPermit) line_no) rest line_no acc
        | act_tok == "deny"   = continue doas line (parse rule options more ((dummy rule) with dr_action: ActDeny) line_no) rest line_no acc
        | otherwise           = DParseErr line_no ("doas.conf:" + (int to str line_no) + ": syntax error: expected permit or deny")

fn :: continue doas line (Bool, DoasRule, String) [String] Int [DoasRule] => DoasParseResult
      continue doas line (False, _, err_msg) _ line_no _ = DParseErr line_no err_msg
      continue doas line (True, rule, _) rest line_no acc =
        parse doas lines rest (line_no + 1) (acc :: [rule])

fn :: is reserved kw String => Bool
      is reserved kw s      =
        (s == "as") or (s == "cmd") or (s == "args") or (s == "nopass") or (s == "nolog") or (s == "persist") or (s == "keepenv") or (s == "setenv") or (s == "\{") or (s == "\}")

fn :: parse rule options [String] DoasRule Int => (Bool, DoasRule, String)
      parse rule options [] _ line_no =
        (False, (dummy rule), "doas.conf:" + (int to str line_no) + ": syntax error: missing identity")
      parse rule options (t:rest) rule line_no =
        | t == "nopass"  = parse rule options rest (rule with dr_nopass: True) line_no
        | t == "nolog"   = parse rule options rest (rule with dr_nolog: True) line_no
        | t == "persist" = parse rule options rest (rule with dr_persist: True) line_no
        | t == "keepenv" = parse rule options rest (rule with dr_keepenv: True) line_no
        | t == "setenv"  = parse rule setenv rest rule line_no
        | is reserved kw t = (False, (dummy rule), "doas.conf:" + (int to str line_no) + ": syntax error: unexpected keyword '" + t + "'")
        | otherwise      = parse rule after ident rest (rule with dr_ident: t) line_no

fn :: parse rule setenv [String] DoasRule Int => (Bool, DoasRule, String)
      parse rule setenv [] _ line_no =
        (False, (dummy rule), "doas.conf:" + (int to str line_no) + ": syntax error: expected '\{' after setenv")
      parse rule setenv (t:rest) rule line_no =
        | t == "\{" = collect setenv block rest rule line_no
        | otherwise = (False, (dummy rule), "doas.conf:" + (int to str line_no) + ": syntax error: expected '\{' after setenv")

fn :: collect setenv block [String] DoasRule Int => (Bool, DoasRule, String)
      collect setenv block [] _ line_no =
        (False, (dummy rule), "doas.conf:" + (int to str line_no) + ": syntax error: unclosed '\{' in setenv")
      collect setenv block (t:rest) rule line_no =
        | t == "\}" = parse rule options rest rule line_no
        | otherwise = collect setenv block rest (rule with dr_setenv: (dr_setenv rule) :: [t]) line_no

fn :: parse rule after ident [String] DoasRule Int => (Bool, DoasRule, String)
      parse rule after ident [] rule _ = (True, rule, "")
      parse rule after ident (t:rest) rule line_no =
        | t == "as"  = parse rule as rest rule line_no
        | t == "cmd" = parse rule cmd rest rule line_no
        | otherwise  = (False, (dummy rule), "doas.conf:" + (int to str line_no) + ": syntax error: unexpected token '" + t + "'")

fn :: parse rule as [String] DoasRule Int => (Bool, DoasRule, String)
      parse rule as [] _ line_no =
        (False, (dummy rule), "doas.conf:" + (int to str line_no) + ": syntax error: missing target after 'as'")
      parse rule as (target:rest) rule line_no =
        | is reserved kw target = (False, (dummy rule), "doas.conf:" + (int to str line_no) + ": syntax error: invalid target after 'as'")
        | otherwise             = parse rule after as rest (rule with dr_target: target) line_no

fn :: parse rule after as [String] DoasRule Int => (Bool, DoasRule, String)
      parse rule after as [] rule _ = (True, rule, "")
      parse rule after as (t:rest) rule line_no =
        | t == "cmd" = parse rule cmd rest rule line_no
        | otherwise  = (False, (dummy rule), "doas.conf:" + (int to str line_no) + ": syntax error: unexpected token '" + t + "'")

fn :: parse rule cmd [String] DoasRule Int => (Bool, DoasRule, String)
      parse rule cmd [] _ line_no =
        (False, (dummy rule), "doas.conf:" + (int to str line_no) + ": syntax error: missing command after 'cmd'")
      parse rule cmd (cmd_name:rest) rule line_no =
        parse rule cmd args rest (rule with dr_has_cmd: True, dr_cmd: cmd_name) line_no

fn :: parse rule cmd args [String] DoasRule Int => (Bool, DoasRule, String)
      parse rule cmd args [] rule _ = (True, rule, "")
      parse rule cmd args (t:rest) rule line_no =
        | t == "args" = (True, rule with dr_has_args: True, dr_args: rest, "")
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
      parse doas args args     =  parse doas cli args (DoasSpec "" "root" False "" [])

fn :: parse doas cli [String]   DoasSpec => DoasSpec
      parse doas cli []         spec     =  spec
      parse doas cli (arg:rest) spec     =
        | arg == "-n" = parse doas cli rest (spec with d_non_interactive: True)
        | arg == "-C" = parse doas flag c rest spec
        | arg == "-u" = parse doas flag u rest spec
        | arg == "--" = assign doas cmd rest spec
        | otherwise   = assign doas cmd ([arg] :: rest) spec

fn :: parse doas flag c [String] DoasSpec => DoasSpec
      parse doas flag c [] spec = spec
      parse doas flag c (conf:rest) spec =
        parse doas cli rest (spec with d_check_conf: conf)

fn :: parse doas flag u [String] DoasSpec => DoasSpec
      parse doas flag u [] spec = spec
      parse doas flag u (u:rest) spec =
        parse doas cli rest (spec with d_target_user: u)

fn :: assign doas cmd [String]   DoasSpec => DoasSpec
      assign doas cmd []         spec     =  spec
      assign doas cmd (cmd:args) spec     =
        spec with d_cmd: cmd, d_cmd_args: args

op :: build doas env DoasRule for String      as String      => [String]
      build doas env rule     for caller_user as target_user =
        base = ["PATH=/bin:/sbin:/usr/bin:/usr/sbin:/usr/local/bin:/usr/local/sbin", "DOAS_USER=" + caller_user, "USER=" + target_user, "LOGNAME=" + target_user, "HOME=/home/" + target_user]
        kept = append keepenv base (dr_keepenv rule)
        apply setenv specs kept (dr_setenv rule)

fn :: append keepenv [String] Bool  => [String]
      append keepenv base     False =  base
      append keepenv base     True  =
        term_v = sys getenv "TERM"
        lang_v = sys getenv "LANG"
        base :: ["TERM=" + term_v, "LANG=" + lang_v]

fn :: apply setenv specs [String] [String]    => [String]
      apply setenv specs env_list []          =  env_list
      apply setenv specs env_list (item:rest) =
        | str starts with item "-"                       = apply setenv specs (remove env key env_list (str slice item 1 (str len item)) []) rest
        | (find char idx item 0 (str len item) 61) > 0   = apply setenv specs (put env kv env_list item) rest
        | otherwise                                      = apply setenv specs (put env kv env_list (item + "=" + (sys getenv item))) rest

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

fn :: authenticate doas DoasRule DoasSpec String      => Bool
      authenticate doas rule     spec     caller_user =
        | dr_nopass rule           = True
        | d_non_interactive spec   = False
        | otherwise                = (str len (sys read pass ("doas (" + caller_user + ") password: "))) >= 0

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
