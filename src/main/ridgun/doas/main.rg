-- doas/main.rg — OpenBSD doas(1) dedicated privilege escalation & config auditor in Pure Ridgun
-- @implements FR-SEC-001

import sec_lib

fn :: drop prog [String]    => [String]
      drop prog []          =  []
      drop prog (prog:rest) =  rest

fn :: resolve conf path String => String
      resolve conf path ""     =  "/etc/doas.conf"
      resolve conf path p      =  p

fn :: handle parse result DoasParseResult  DoasSpec => ()
      handle parse result (DParseErr line_no msg) spec =
        _ = echo msg
        sys exit 1
      handle parse result (DParseOk rules) spec     =
        | ((str len (d_check_conf spec)) > 0) and ((str len (d_cmd spec)) == 0) = ()
        | (str len (d_check_conf spec)) > 0                                     = run audit query rules spec
        | (str len (d_cmd spec)) == 0                                           = sys exit 1
        | otherwise                                                             = run live doas rules spec

fn :: current caller user => String
      current caller user =
        env_u = sys getenv "DOAS_CALLER"
        select caller user env_u

fn :: select caller user String => String
      select caller user env_u  =
        | (str len env_u) > 0 = env_u
        | otherwise           = sys username (sys getuid)

fn :: current caller groups String => [String]
      current caller groups caller =
        env_g = sys getenv "DOAS_GROUPS"
        select caller groups env_g caller

fn :: select caller groups String String => [String]
      select caller groups env_g  caller =
        | (str len env_g) > 0 = [env_g]
        | otherwise           = sys user groups caller

fn :: run audit query [DoasRule] DoasSpec => ()
      run audit query rules      spec     =
        caller = current caller user
        groups = current caller groups caller
        dec = evaluate doas rules rules for caller in groups as (d_target_user spec) running (d_cmd spec) with (d_cmd_args spec)
        echo (format doas audit dec)

fn :: run live doas [DoasRule] DoasSpec => ()
      run live doas rules      spec     =
        caller = current caller user
        groups = current caller groups caller
        dec = evaluate doas rules rules for caller in groups as (d_target_user spec) running (d_cmd spec) with (d_cmd_args spec)
        dispatch live decision dec spec caller

fn :: dispatch live decision DoasDecision DoasSpec String => ()
      dispatch live decision DecDeny spec caller =
        _ = posix write 2 ("doas: Operation not permitted" + (char from code 10))
        sys exit 1
      dispatch live decision (DecPermit nopass rule) spec caller =
        rc = execute privileged rule spec caller
        exit if fail rc

fn :: exit if fail Int => ()
      exit if fail 0   =  ()
      exit if fail rc  =  sys exit rc

fn :: main => ()
      main =
        args = drop prog (cli args)
        spec = parse doas args args
        raw_conf = read file (resolve conf path (d_check_conf spec))
        parsed = parse doas config raw_conf
        handle parse result parsed spec
