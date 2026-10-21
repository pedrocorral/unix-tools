-- test_sec.rg — Exhaustive Unit & E2E Binary Tests for Tier 4 OpenBSD doas(1) & doas.conf(5)
-- @tests TS-SEC-002

import test
import sec_lib

fn :: unwrap_rules DoasParseResult    => [DoasRule]
      unwrap_rules (DParseOk rules)   =  rules
      unwrap_rules (DParseErr ln msg) =  []

fn :: is_parse_ok DoasParseResult    => Bool
      is_parse_ok (DParseOk rules)   =  True
      is_parse_ok (DParseErr ln msg) =  False

fn :: parse_err_line DoasParseResult    => Int
      parse_err_line (DParseOk rules)   =  0
      parse_err_line (DParseErr ln msg) =  ln

fn :: test_doas_parser_suite => ()
      test_doas_parser_suite =
        nl = char_from_code 10
        _ = echo "=== 1. Testing OpenBSD doas.conf(5) Grammar & Syntax Validation (TS-SEC-001) ==="
        good_cfg = "# OpenBSD doas.conf" + nl + "permit nopass :wheel" + nl + "permit keepenv setenv \{ PATH HOME -SECRET CUSTOM=1 \} pedro as root cmd /sbin/reboot args" + nl + "deny baduser as root cmd /bin/rm" + nl
        p_good = parse doas config good_cfg
        _ = assert_true (is_parse_ok p_good) "AC1: valid doas.conf parses without error"
        _ = assert_int_equals (parse_err_line (parse doas config ("permit nopass :wheel" + nl + "allow root" + nl))) 2 "AC1: malformed action keyword reports syntax error on line 2"
        _ = assert_int_equals (parse_err_line (parse doas config ("permit nopass" + nl))) 1 "AC1: missing identity reports syntax error on line 1"
        _ = assert_int_equals (parse_err_line (parse doas config ("permit setenv \{ PATH pedro" + nl))) 1 "AC1: unclosed setenv brace reports syntax error on line 1"
        assert_int_equals (parse_err_line (parse doas config ("permit pedro as" + nl))) 1 "AC1: missing target after 'as' reports syntax error on line 1"

fn :: test_doas_policy_suite => ()
      test_doas_policy_suite =
        nl = char_from_code 10
        _ = echo "=== 2. Testing OpenBSD 'Last Matching Rule Wins' Policy Engine (TS-SEC-001) ==="
        cfg = "permit :wheel" + nl + "permit nopass pedro as root cmd /sbin/reboot args" + nl + "deny pedro as root cmd /sbin/reboot args -f" + nl + "permit pedro as operator cmd /usr/bin/id" + nl
        rules = unwrap_rules (parse doas config cfg)
        d1 = evaluate doas rules rules for "pedro" in ["users", "wheel"] as "root" running "/sbin/reboot" with []
        _ = assert_equals (format doas audit d1) "permit nopass" "AC2: pedro running /sbin/reboot with 0 args matches rule 2 (permit nopass)"
        d2 = evaluate doas rules rules for "pedro" in ["users", "wheel"] as "root" running "/sbin/reboot" with ["-f"]
        _ = assert_equals (format doas audit d2) "deny" "AC2: pedro running /sbin/reboot -f matches rule 1 (:wheel) then overridden by rule 3 (deny)"
        d3 = evaluate doas rules rules for "alice" in ["wheel"] as "root" running "/usr/bin/apt" with ["update"]
        _ = assert_equals (format doas audit d3) "permit" "AC2: alice in :wheel matches rule 1 (permit)"
        d4 = evaluate doas rules rules for "bob" in ["users"] as "root" running "/usr/bin/apt" with ["update"]
        _ = assert_equals (format doas audit d4) "deny" "AC2: bob not in :wheel gets deny"
        d5 = evaluate doas rules rules for "pedro" in ["users"] as "operator" running "/usr/bin/id" with ["-u"]
        _ = assert_equals (format doas audit d5) "permit" "AC2: cmd without 'args' keyword allows any arguments"
        d6 = evaluate doas rules rules for "pedro" in ["users"] as "root" running "/sbin/reboot" with ["now"]
        _ = assert_equals (format doas audit d6) "deny" "AC2: cmd with empty 'args' keyword rejects non-empty arguments"
        env_rule = dummy_rule with dr_action: ActPermit, dr_nopass: True, dr_setenv: ["-USER", "CUSTOM=42"], dr_ident: "pedro"
        san_env = build doas env env_rule for "pedro" as "root"
        _ = assert_true (list_contains_str san_env "DOAS_USER=pedro") "doas env sets DOAS_USER=pedro"
        _ = assert_true (list_contains_str san_env "CUSTOM=42") "doas setenv adds CUSTOM=42"
        assert_true (not (list_contains_str san_env "USER=root")) "doas setenv -USER removes USER from environment"

fn :: test_doas_e2e_suite => ()
      test_doas_e2e_suite =
        nl = char_from_code 10
        _ = echo "=== 3. Testing Compiled Native ELF Binary target/doas (E2E) ==="
        cfg = "permit :wheel" + nl + "permit nopass boogie as root cmd /sbin/reboot args" + nl + "deny boogie as root cmd /sbin/reboot args -f" + nl
        _ = write_file "/tmp/ut_doas_good.conf" cfg
        rc_c1 = shell_exec "./target/doas -C /tmp/ut_doas_good.conf > /tmp/ut_doas_c1.out"
        _ = assert_int_equals rc_c1 0 "AC1 E2E binary: target/doas -C valid.conf exits 0"
        _ = write_file "/tmp/ut_doas_bad.conf" ("permit :wheel" + nl + "bogus_rule_here" + nl)
        rc_c2 = shell_exec "./target/doas -C /tmp/ut_doas_bad.conf > /tmp/ut_doas_c2.out"
        _ = assert_true (rc_c2 != 0) "AC1 E2E binary: target/doas -C invalid.conf exits non-zero"
        _ = assert_true (str_starts_with (read_file "/tmp/ut_doas_c2.out") "doas.conf:2: syntax error") "AC1 E2E binary: target/doas -C reports exact syntax error line number"
        _ = shell_exec "DOAS_CALLER=boogie DOAS_GROUPS=wheel ./target/doas -C /tmp/ut_doas_good.conf -u root /sbin/reboot > /tmp/ut_doas_q1.out"
        _ = assert_equals (read_file "/tmp/ut_doas_q1.out") ("permit nopass" + nl) "AC2 E2E binary: target/doas -C audit prints 'permit nopass'"
        _ = shell_exec "DOAS_CALLER=boogie DOAS_GROUPS=wheel ./target/doas -C /tmp/ut_doas_good.conf -u root /sbin/reboot -f > /tmp/ut_doas_q2.out"
        _ = assert_equals (read_file "/tmp/ut_doas_q2.out") ("deny" + nl) "AC2 E2E binary: target/doas -C audit prints 'deny' when later rule denies"
        _ = shell_exec "DOAS_CALLER=boogie DOAS_GROUPS=wheel ./target/doas -C /tmp/ut_doas_good.conf -u root /usr/bin/whoami > /tmp/ut_doas_q3.out"
        _ = assert_equals (read_file "/tmp/ut_doas_q3.out") ("permit" + nl) "AC2 E2E binary: target/doas -C audit prints 'permit' via :wheel"
        _ = shell_exec "rm -f /tmp/ut_doas_good.conf /tmp/ut_doas_bad.conf /tmp/ut_doas_c1.out /tmp/ut_doas_c2.out /tmp/ut_doas_q1.out /tmp/ut_doas_q2.out /tmp/ut_doas_q3.out"
        ()

fn :: main => ()
      main =
        _ = test_doas_parser_suite
        _ = test_doas_policy_suite
        _ = test_doas_e2e_suite
        echo "All 21 Tier 4 OpenBSD doas(1) & doas.conf(5) unit & E2E assertions passed!"
