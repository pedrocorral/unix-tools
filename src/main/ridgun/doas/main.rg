-- doas/main.rg — OpenBSD doas(1) dedicated privilege escalation & config auditor in Pure Ridgun
-- @implements FR-SEC-001

import sec_lib

fn :: drop_prog [String]    => [String]
      drop_prog []          =  []
      drop_prog (prog:rest) =  rest

fn :: resolve_conf_path String => String
      resolve_conf_path ""     =  "/etc/doas.conf"
      resolve_conf_path p      =  p

fn :: handle_parse_result DoasParseResult  DoasSpec => ()
      handle_parse_result (DParseErr line_no msg) spec =
        _ = echo msg
        sys_exit 1
      handle_parse_result (DParseOk rules) spec     =
        | ((str_len (d_check_conf spec)) > 0) and ((str_len (d_cmd spec)) == 0) = ()
        | (str_len (d_check_conf spec)) > 0                                     = run_audit_query rules spec
        | (str_len (d_cmd spec)) == 0                                           = sys_exit 1
        | otherwise                                                             = run_live_doas rules spec

fn :: current_caller_user => String
      current_caller_user =
        env_u = sys_getenv "DOAS_CALLER"
        select_caller_user env_u

fn :: select_caller_user String => String
      select_caller_user env_u  =
        | (str_len env_u) > 0 = env_u
        | otherwise           = sys_username (sys_getuid)

fn :: current_caller_groups String => [String]
      current_caller_groups caller =
        env_g = sys_getenv "DOAS_GROUPS"
        select_caller_groups env_g caller

fn :: select_caller_groups String String => [String]
      select_caller_groups env_g  caller =
        | (str_len env_g) > 0 = [env_g]
        | otherwise           = sys_user_groups caller

fn :: run_audit_query [DoasRule] DoasSpec => ()
      run_audit_query rules      spec     =
        caller = current_caller_user
        groups = current_caller_groups caller
        dec = evaluate doas rules rules for caller in groups as (d_target_user spec) running (d_cmd spec) with (d_cmd_args spec)
        echo (format doas audit dec)

fn :: run_live_doas [DoasRule] DoasSpec => ()
      run_live_doas rules      spec     =
        caller = current_caller_user
        groups = current_caller_groups caller
        dec = evaluate doas rules rules for caller in groups as (d_target_user spec) running (d_cmd spec) with (d_cmd_args spec)
        dispatch_live_decision dec spec caller

fn :: dispatch_live_decision DoasDecision DoasSpec String => ()
      dispatch_live_decision DecDeny      spec     caller =
        _ = write_file "/dev/stderr" ("doas: Operation not permitted" + (char_from_code 10))
        sys_exit 1
      dispatch_live_decision (DecPermit nopass rule) spec caller =
        rc = execute_privileged rule spec caller
        exit_if_fail rc

fn :: exit_if_fail Int => ()
      exit_if_fail 0   =  ()
      exit_if_fail rc  =  sys_exit rc

fn :: main => ()
      main =
        args = drop_prog cli_args
        spec = parse doas args args
        raw_conf = read_file (resolve_conf_path (d_check_conf spec))
        parsed = parse doas config raw_conf
        handle_parse_result parsed spec
