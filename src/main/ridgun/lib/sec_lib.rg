-- sec_lib.rg — Pure Ridgun OpenBSD doas(1) & doas.conf(5) Security Policy Engine
-- @implements FR-SEC-001

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
        | (str_len s) == 0 = []
        | otherwise = split_lines_loop s 0 (str_len s) "" []

fn :: split_lines_loop String Int Int String [String] => [String]
      split_lines_loop s idx len cur acc =
        | idx >= len = finalize_lines cur acc
        | (str_char_at s idx) == 10 = split_lines_loop s (idx + 1) len "" (acc :: [strip_cr cur])
        | otherwise = split_lines_loop s (idx + 1) len (cur + (str_slice s idx (idx + 1))) acc

fn :: strip_cr String => String
      strip_cr s      =
        | (str_len s) == 0 = s
        | (str_char_at s ((str_len s) - 1)) == 13 = str_slice s 0 ((str_len s) - 1)
        | otherwise = s

fn :: finalize_lines String [String] => [String]
      finalize_lines cur    acc      =
        | (str_len cur) == 0 = acc
        | otherwise = acc :: [strip_cr cur]

fn :: is_ws Int => Bool
      is_ws c   =  (c == 32) or (c == 9) or (c == 10) or (c == 13)

fn :: list_len [A]    => Int
      list_len []     =  0
      list_len (_:xs) =  1 + (list_len xs)

fn :: fst (A, B) => A
      fst (a, _) =  a

fn :: snd (A, B) => B
      snd (_, b) =  b

fn :: list_contains_str [String] String => Bool
      list_contains_str []       target =  False
      list_contains_str (x:xs)   target =
        | x == target = True
        | otherwise = list_contains_str xs target

fn :: str_list_eq [String] [String] => Bool
      str_list_eq []       []       =  True
      str_list_eq []       (y:ys)   =  False
      str_list_eq (x:xs)   []       =  False
      str_list_eq (x:xs)   (y:ys)   =
        | x == y = str_list_eq xs ys
        | otherwise = False

fn :: join_with [String] String => String
      join_with []       sep    =  ""
      join_with (x:[])   sep    =  x
      join_with (x:xs)   sep    =  x + sep + (join_with xs sep)

fn :: find_char_idx String Int Int Int       => Int
      find_char_idx s      idx len target_ch =
        | idx >= len = 0 - 1
        | (str_char_at s idx) == target_ch = idx
        | otherwise = find_char_idx s (idx + 1) len target_ch

-- ============================================================================
-- 1. doas.conf(5) Lexer & Parser — FR-SEC-001
-- ============================================================================

fn :: is_permit_action DoasAction => Bool
      is_permit_action ActPermit  =  True
      is_permit_action ActDeny    =  False

op :: tokenize doas line String => [String]
      tokenize doas line line   =  lex_doas_line line 0 (str_len line) "" False []

fn :: push_doas_tok String Bool  [String] => [String]
      push_doas_tok cur    True  acc      =  acc :: [cur]
      push_doas_tok cur    False acc      =
        | (str_len cur) == 0 = acc
        | otherwise = acc :: [cur]

fn :: lex_doas_line String Int Int String Bool [String] => [String]
      lex_doas_line s idx len cur quoted acc =
        | idx >= len = push_doas_tok cur quoted acc
        | ((str_char_at s idx) == 35) and (not quoted) and ((str_len cur) == 0) = acc
        | (str_char_at s idx) == 34 = lex_doas_quote s (idx + 1) len cur acc
        | (not quoted) and (is_ws (str_char_at s idx)) = lex_doas_line s (idx + 1) len "" False (push_doas_tok cur False acc)
        | (not quoted) and (((str_char_at s idx) == 123) or ((str_char_at s idx) == 125)) = lex_doas_line s (idx + 1) len "" False ((push_doas_tok cur False acc) :: [str_slice s idx (idx + 1)])
        | otherwise = lex_doas_line s (idx + 1) len (cur + (str_slice s idx (idx + 1))) quoted acc

fn :: lex_doas_quote String Int Int String [String] => [String]
      lex_doas_quote s idx len cur acc =
        | idx >= len = push_doas_tok cur True acc
        | (str_char_at s idx) == 34 = lex_doas_line s (idx + 1) len cur True acc
        | ((str_char_at s idx) == 92) and ((idx + 1) < len) = lex_doas_quote s (idx + 2) len (cur + (str_slice s (idx + 1) (idx + 2))) acc
        | otherwise = lex_doas_quote s (idx + 1) len (cur + (str_slice s idx (idx + 1))) acc

op :: parse doas config String => DoasParseResult
      parse doas config raw    =
        lines = split lines raw
        parse_doas_lines lines 1 []

fn :: parse_doas_lines [String]    Int     [DoasRule] => DoasParseResult
      parse_doas_lines []          line_no acc        =  DParseOk acc
      parse_doas_lines (line:rest) line_no acc        =
        toks = tokenize doas line line
        parse_doas_line_tokens toks rest line_no acc

fn :: dummy_rule => DoasRule
      dummy_rule =  DoasRule ActDeny False False False False [] "" "root" False "" False []

fn :: parse_doas_line_tokens [String] [String] Int [DoasRule] => DoasParseResult
      parse_doas_line_tokens [] rest line_no acc = parse_doas_lines rest (line_no + 1) acc
      parse_doas_line_tokens (act_tok:more) rest line_no acc =
        | act_tok == "permit" = continue_doas_line (parse_rule_options more (dummy_rule with dr_action: ActPermit) line_no) rest line_no acc
        | act_tok == "deny" = continue_doas_line (parse_rule_options more (dummy_rule with dr_action: ActDeny) line_no) rest line_no acc
        | otherwise = DParseErr line_no ("doas.conf:" + (int_to_str line_no) + ": syntax error: expected permit or deny")

fn :: continue_doas_line (Bool, DoasRule, String) [String] Int [DoasRule] => DoasParseResult
      continue_doas_line (False, dummy, err_msg) rest line_no acc = DParseErr line_no err_msg
      continue_doas_line (True, rule, ok_msg) rest line_no acc =
        parse_doas_lines rest (line_no + 1) (acc :: [rule])

fn :: is_reserved_kw String => Bool
      is_reserved_kw s      =
        (s == "as") or (s == "cmd") or (s == "args") or (s == "nopass") or (s == "nolog") or (s == "persist") or (s == "keepenv") or (s == "setenv") or (s == "\{") or (s == "\}")

fn :: parse_rule_options [String] DoasRule Int     => (Bool, DoasRule, String)
      parse_rule_options []       rule     line_no =
        (False, dummy_rule, "doas.conf:" + (int_to_str line_no) + ": syntax error: missing identity")
      parse_rule_options (t:rest) rule     line_no =
        | t == "nopass" = parse_rule_options rest (rule with dr_nopass: True) line_no
        | t == "nolog" = parse_rule_options rest (rule with dr_nolog: True) line_no
        | t == "persist" = parse_rule_options rest (rule with dr_persist: True) line_no
        | t == "keepenv" = parse_rule_options rest (rule with dr_keepenv: True) line_no
        | t == "setenv" = parse_rule_setenv rest rule line_no
        | is_reserved_kw t = (False, dummy_rule, "doas.conf:" + (int_to_str line_no) + ": syntax error: unexpected keyword '" + t + "'")
        | otherwise = parse_rule_after_ident rest (rule with dr_ident: t) line_no

fn :: parse_rule_setenv [String] DoasRule Int     => (Bool, DoasRule, String)
      parse_rule_setenv []       rule     line_no =
        (False, dummy_rule, "doas.conf:" + (int_to_str line_no) + ": syntax error: expected '\{' after setenv")
      parse_rule_setenv (t:rest) rule     line_no =
        | t == "\{" = collect_setenv_block rest rule line_no
        | otherwise = (False, dummy_rule, "doas.conf:" + (int_to_str line_no) + ": syntax error: expected '\{' after setenv")

fn :: collect_setenv_block [String] DoasRule Int     => (Bool, DoasRule, String)
      collect_setenv_block []       rule     line_no =
        (False, dummy_rule, "doas.conf:" + (int_to_str line_no) + ": syntax error: unclosed '\{' in setenv")
      collect_setenv_block (t:rest) rule     line_no =
        | t == "\}" = parse_rule_options rest rule line_no
        | otherwise = collect_setenv_block rest (rule with dr_setenv: (dr_setenv rule) :: [t]) line_no

fn :: parse_rule_after_ident [String] DoasRule Int => (Bool, DoasRule, String)
      parse_rule_after_ident [] rule line_no = (True, rule, "")
      parse_rule_after_ident (t:rest) rule line_no =
        | t == "as" = parse_rule_as rest rule line_no
        | t == "cmd" = parse_rule_cmd rest rule line_no
        | otherwise = (False, dummy_rule, "doas.conf:" + (int_to_str line_no) + ": syntax error: unexpected token '" + t + "'")

fn :: parse_rule_as [String]      DoasRule Int     => (Bool, DoasRule, String)
      parse_rule_as []            rule     line_no =
        (False, dummy_rule, "doas.conf:" + (int_to_str line_no) + ": syntax error: missing target after 'as'")
      parse_rule_as (target:rest) rule     line_no =
        | is_reserved_kw target = (False, dummy_rule, "doas.conf:" + (int_to_str line_no) + ": syntax error: invalid target after 'as'")
        | otherwise = parse_rule_after_as rest (rule with dr_target: target) line_no

fn :: parse_rule_after_as [String] DoasRule Int     => (Bool, DoasRule, String)
      parse_rule_after_as []       rule     line_no =  (True, rule, "")
      parse_rule_after_as (t:rest) rule     line_no =
        | t == "cmd" = parse_rule_cmd rest rule line_no
        | otherwise = (False, dummy_rule, "doas.conf:" + (int_to_str line_no) + ": syntax error: unexpected token '" + t + "'")

fn :: parse_rule_cmd [String] DoasRule Int => (Bool, DoasRule, String)
      parse_rule_cmd [] rule line_no =
        (False, dummy_rule, "doas.conf:" + (int_to_str line_no) + ": syntax error: missing command after 'cmd'")
      parse_rule_cmd (cmd_name:rest) rule line_no =
        parse_rule_cmd_args rest (rule with dr_has_cmd: True, dr_cmd: cmd_name) line_no

fn :: parse_rule_cmd_args [String] DoasRule Int     => (Bool, DoasRule, String)
      parse_rule_cmd_args []       rule     line_no =  (True, rule, "")
      parse_rule_cmd_args (t:rest) rule     line_no =
        | t == "args" = (True, rule with dr_has_args: True, dr_args: rest, "")
        | otherwise = (False, dummy_rule, "doas.conf:" + (int_to_str line_no) + ": syntax error: expected 'args' after command")

-- ============================================================================
-- 2. Policy Engine ("Last Matching Rule Wins") — FR-SEC-001
-- ============================================================================

fn :: match_rule_ident String  String      [String]      => Bool
      match_rule_ident rule_id caller_user caller_groups =
        | str_starts_with rule_id ":" = list_contains_str caller_groups (str_slice rule_id 1 (str_len rule_id))
        | otherwise = rule_id == caller_user

fn :: match_rule_cmd DoasRule String [String] => Bool
      match_rule_cmd rule     cmd    cmd_args =
        | not (dr_has_cmd rule) = True
        | (dr_cmd rule) != cmd = False
        | not (dr_has_args rule) = True
        | otherwise = str_list_eq (dr_args rule) cmd_args

fn :: rule_matches DoasRule String [String] String String [String] => Bool
      rule_matches rule caller_user caller_groups target_user cmd cmd_args =
        id_ok = match_rule_ident (dr_ident rule) caller_user caller_groups
        tgt_ok = (dr_target rule) == target_user
        cmd_ok = match_rule_cmd rule cmd cmd_args
        id_ok and tgt_ok and cmd_ok

op :: evaluate doas rules [DoasRule] for String in [String] as String running String with [String] => DoasDecision
      evaluate doas rules rules for caller_user in caller_groups as target_user running cmd with cmd_args =
        eval_rules_loop rules caller_user caller_groups target_user cmd cmd_args DecDeny

fn :: eval_rules_loop [DoasRule] String [String] String String [String] DoasDecision => DoasDecision
      eval_rules_loop [] caller_user caller_groups target_user cmd cmd_args current_dec = current_dec
      eval_rules_loop (r:rest) caller_user caller_groups target_user cmd cmd_args current_dec =
        | rule_matches r caller_user caller_groups target_user cmd cmd_args = eval_rules_loop rest caller_user caller_groups target_user cmd cmd_args (decision_from_rule r)
        | otherwise = eval_rules_loop rest caller_user caller_groups target_user cmd cmd_args current_dec

fn :: decision_from_rule DoasRule => DoasDecision
      decision_from_rule r        =
        | is_permit_action (dr_action r) = DecPermit (dr_nopass r) r
        | otherwise = DecDeny

op :: format doas audit DoasDecision       => String
      format doas audit DecDeny            =  "deny"
      format doas audit (DecPermit True r) =  "permit nopass"
      format doas audit (DecPermit False r) = "permit"

-- ============================================================================
-- 3. CLI Parsing, Environment Sanitization & Direct Syscall Execution
-- ============================================================================

op :: parse doas args [String] => DoasSpec
      parse doas args args     =  parse_doas_cli args (DoasSpec "" "root" False "" [])

fn :: parse_doas_cli [String]   DoasSpec => DoasSpec
      parse_doas_cli []         spec     =  spec
      parse_doas_cli (arg:rest) spec     =
        | arg == "-n" = parse_doas_cli rest (spec with d_non_interactive: True)
        | arg == "-C" = parse_doas_flag_c rest spec
        | arg == "-u" = parse_doas_flag_u rest spec
        | arg == "--" = assign_doas_cmd rest spec
        | otherwise = assign_doas_cmd ([arg] :: rest) spec

fn :: parse_doas_flag_c [String]    DoasSpec => DoasSpec
      parse_doas_flag_c []          spec     =  spec
      parse_doas_flag_c (conf:rest) spec     =
        parse_doas_cli rest (spec with d_check_conf: conf)

fn :: parse_doas_flag_u [String] DoasSpec => DoasSpec
      parse_doas_flag_u []       spec     =  spec
      parse_doas_flag_u (u:rest) spec     =
        parse_doas_cli rest (spec with d_target_user: u)

fn :: assign_doas_cmd [String]   DoasSpec => DoasSpec
      assign_doas_cmd []         spec     =  spec
      assign_doas_cmd (cmd:args) spec     =
        spec with d_cmd: cmd, d_cmd_args: args

op :: build doas env DoasRule for String      as String      => [String]
      build doas env rule     for caller_user as target_user =
        base = ["PATH=/bin:/sbin:/usr/bin:/usr/sbin:/usr/local/bin:/usr/local/sbin", "DOAS_USER=" + caller_user, "USER=" + target_user, "LOGNAME=" + target_user, "HOME=/home/" + target_user]
        kept = append_keepenv base (dr_keepenv rule)
        apply_setenv_specs kept (dr_setenv rule)

fn :: append_keepenv [String] Bool  => [String]
      append_keepenv base     False =  base
      append_keepenv base     True  =
        term_v = sys_getenv "TERM"
        lang_v = sys_getenv "LANG"
        base :: ["TERM=" + term_v, "LANG=" + lang_v]

fn :: apply_setenv_specs [String] [String]    => [String]
      apply_setenv_specs env_list []          =  env_list
      apply_setenv_specs env_list (item:rest) =
        | str_starts_with item "-" = apply_setenv_specs (remove_env_key env_list (str_slice item 1 (str_len item)) []) rest
        | (find_char_idx item 0 (str_len item) 61) > 0 = apply_setenv_specs (put_env_kv env_list item) rest
        | otherwise = apply_setenv_specs (put_env_kv env_list (item + "=" + (sys_getenv item))) rest

fn :: remove_env_key [String]  String [String] => [String]
      remove_env_key []        key    acc      =  acc
      remove_env_key (kv:rest) key    acc      =
        | str_starts_with kv (key + "=") = remove_env_key rest key acc
        | otherwise = remove_env_key rest key (acc :: [kv])

fn :: put_env_kv [String] String => [String]
      put_env_kv env_list kv     =
        eq_i = find_char_idx kv 0 (str_len kv) 61
        key = str_slice kv 0 eq_i
        cleaned = remove_env_key env_list key []
        cleaned :: [kv]

fn :: execute_privileged DoasRule DoasSpec String      => Int
      execute_privileged rule     spec     caller_user =
        auth_ok = authenticate_doas rule spec caller_user
        run_after_auth auth_ok rule spec caller_user

fn :: authenticate_doas DoasRule DoasSpec String      => Bool
      authenticate_doas rule     spec     caller_user =
        | dr_nopass rule = True
        | d_non_interactive spec = False
        | otherwise = (str_len (sys_read_pass ("doas (" + caller_user + ") password: "))) >= 0

fn :: run_after_auth Bool  DoasRule DoasSpec String      => Int
      run_after_auth False rule     spec     caller_user =
        _ = write_file "/dev/stderr" ("doas: Authentication required" + (char_from_code 10))
        1
      run_after_auth True  rule     spec     caller_user =
        ug = sys_lookup_uid_gid (d_target_user spec)
        t_uid = fst ug
        t_gid = snd ug
        _ = switch_if_valid_ids t_uid t_gid
        env_list = build doas env rule for caller_user as (d_target_user spec)
        sys_execve (d_cmd spec) (d_cmd_args spec) env_list

fn :: switch_if_valid_ids Int   Int   => Int
      switch_if_valid_ids t_uid t_gid =
        | (t_uid < 0) or (t_gid < 0) = 0
        | (sys_getuid) != 0 = 0
        | otherwise =
          _ = sys_setresgid t_gid
          _ = sys_setgroups [t_gid]
          sys_setresuid t_uid
