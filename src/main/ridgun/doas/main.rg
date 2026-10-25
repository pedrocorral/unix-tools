-- doas/main.rg — OpenBSD doas(1) dedicated privilege escalation & config auditor in Pure Ridgun
-- @implements FR-SEC-001

import sec_lib

fn :: drop prog [String]    => [String]
      drop prog []          =  []
      drop prog (prog:rest) =  rest

fn :: resolve conf path String => String
      resolve conf path ""     =  "/etc/doas.conf"
      resolve conf path p      =  p

fn :: split csv groups String Int Int String [String] => [String]
      split csv groups s idx len cur acc =
        | idx >= len                                                  = push csv group cur acc
        | ((str char at s idx) == 44) or ((str char at s idx) == 58)  = split csv groups s (idx + 1) len "" (push csv group cur acc)
        | otherwise                                                   = split csv groups s (idx + 1) len (cur + (str slice s idx (idx + 1))) acc

fn :: push csv group String [String] => [String]
      push csv group ""     acc      =  acc
      push csv group cur    acc      =  acc :: [cur]

fn :: handle parse result DoasParseResult  DoasSpec => ()
      handle parse result (DParseErr line_no msg) spec =
        _ = echo msg
        sys exit 1
      handle parse result (DParseOk rules) spec     =
        | ((str len (d_check_conf spec)) > 0) and ((str len (d_cmd spec)) == 0) = ()
        | (str len (d_check_conf spec)) > 0                                     = run audit query rules spec
        | (str len (d_cmd spec)) == 0                                           = sys exit 1
        | otherwise                                                             = run live doas rules spec

fn :: select audit user String => String
      select audit user ""     =  sys username (sys getuid)
      select audit user env_u  =  env_u

fn :: audit caller user => String
      audit caller user =  select audit user (sys getenv "DOAS_CALLER")

fn :: select audit groups String String => [String]
      select audit groups ""     caller =  sys user groups caller
      select audit groups env_g  _      =  split csv groups env_g 0 (str len env_g) "" []

fn :: audit caller groups String => [String]
      audit caller groups caller =  select audit groups (sys getenv "DOAS_GROUPS") caller

fn :: run audit query [DoasRule] DoasSpec => ()
      run audit query rules      spec     =
        caller = audit caller user
        groups = audit caller groups caller
        dec = evaluate doas rules rules for caller in groups as (d_target_user spec) running (d_cmd spec) with (d_cmd_args spec)
        echo (format doas audit dec)

fn :: run live doas [DoasRule] DoasSpec => ()
      run live doas rules      spec     =
        caller = sys username (sys getuid)
        groups = sys user groups caller
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

fn :: dispatch doas spec DoasSpec => ()
      dispatch doas spec spec     =
        dispatch doas clear (d_clear_persist spec) spec

fn :: dispatch doas clear Bool  DoasSpec => ()
      dispatch doas clear True  _        =
        _ = clear doas persist
        ()
      dispatch doas clear False spec     =
        raw_conf = read file (resolve conf path (d_check_conf spec))
        parsed = parse doas config raw_conf
        handle parse result parsed spec

fn :: main => ()
      main =
        args = drop prog (cli args)
        spec = parse doas args args
        dispatch doas spec spec
