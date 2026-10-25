-- test_sec.rg — Exhaustive Unit & E2E Binary Tests for Tier 4 OpenBSD doas(1), doas.conf(5) & BSD sha256(1)
-- @tests TS-SEC-002
-- @tests TS-SEC-003

import test
import sec_lib

fn :: unwrap rules DoasParseResult  => [DoasRule]
      unwrap rules (DParseOk rules) =  rules
      unwrap rules (DParseErr _ _)  =  []

fn :: is parse ok DoasParseResult => Bool
      is parse ok (DParseOk _)    =  True
      is parse ok (DParseErr _ _) =  False

fn :: parse err line DoasParseResult  => Int
      parse err line (DParseOk _)     =  0
      parse err line (DParseErr ln _) =  ln

fn :: test doas parser suite => ()
      test doas parser suite =
        nl = char from code 10
        _ = echo "=== 1. Testing OpenBSD doas.conf(5) Grammar & Syntax Validation (TS-SEC-001) ==="
        good_cfg = "# OpenBSD doas.conf" + nl + "permit nopass :wheel" + nl + "permit keepenv setenv \{ PATH HOME -SECRET CUSTOM=1 \} pedro as root cmd /sbin/reboot args" + nl + "deny baduser as root cmd /bin/rm" + nl
        p_good = parse doas config good_cfg
        _ = assert true (is parse ok p_good) "AC1: valid doas.conf parses without error"
        _ = assert int equals (parse err line (parse doas config ("permit nopass :wheel" + nl + "allow root" + nl))) 2 "AC1: malformed action keyword reports syntax error on line 2"
        _ = assert int equals (parse err line (parse doas config ("permit nopass" + nl))) 1 "AC1: missing identity reports syntax error on line 1"
        _ = assert int equals (parse err line (parse doas config ("permit setenv \{ PATH pedro" + nl))) 1 "AC1: unclosed setenv brace reports syntax error on line 1"
        _ = assert int equals (parse err line (parse doas config ("permit pedro as" + nl))) 1 "AC1: missing target after 'as' reports syntax error on line 1"
        _ = assert int equals (parse err line (parse doas config ("permit pedro cmd" + nl))) 1 "AC1: missing command after 'cmd' reports syntax error on line 1"
        _ = assert int equals (parse err line (parse doas config ("permit badopt pedro" + nl))) 1 "AC1: unknown keyword after action reports syntax error on line 1"
        empty_cfg = parse doas config ("# only comments" + nl + "   " + nl)
        _ = assert true (is parse ok empty_cfg) "AC1: comments-only doas.conf parses cleanly"
        assert int equals (list len (unwrap rules empty_cfg)) 0 "AC1: comments-only doas.conf yields 0 rules"

fn :: test doas policy suite => ()
      test doas policy suite =
        nl = char from code 10
        _ = echo "=== 2. Testing OpenBSD 'Last Matching Rule Wins' Policy Engine (TS-SEC-001) ==="
        cfg = "permit :wheel" + nl + "permit nopass pedro as root cmd /sbin/reboot args" + nl + "deny pedro as root cmd /sbin/reboot args -f" + nl + "permit pedro as operator cmd /usr/bin/id" + nl
        rules = unwrap rules (parse doas config cfg)
        d_empty = evaluate doas rules [] for "root" in ["wheel"] as "root" running "/bin/sh" with []
        _ = assert equals (format doas audit d_empty) "deny" "AC2 security: empty rule list defaults to deny even for root"
        d1 = evaluate doas rules rules for "pedro" in ["users", "wheel"] as "root" running "/sbin/reboot" with []
        _ = assert equals (format doas audit d1) "permit nopass" "AC2: pedro running /sbin/reboot with 0 args matches rule 2 (permit nopass)"
        d2 = evaluate doas rules rules for "pedro" in ["users", "wheel"] as "root" running "/sbin/reboot" with ["-f"]
        _ = assert equals (format doas audit d2) "deny" "AC2: pedro running /sbin/reboot -f matches rule 1 (:wheel) then overridden by rule 3 (deny)"
        d3 = evaluate doas rules rules for "alice" in ["wheel"] as "root" running "/usr/bin/apt" with ["update"]
        _ = assert equals (format doas audit d3) "permit" "AC2: alice in :wheel matches rule 1 (permit)"
        d4 = evaluate doas rules rules for "bob" in ["users"] as "root" running "/usr/bin/apt" with ["update"]
        _ = assert equals (format doas audit d4) "deny" "AC2: bob not in :wheel gets deny"
        d5 = evaluate doas rules rules for "pedro" in ["users"] as "operator" running "/usr/bin/id" with ["-u"]
        _ = assert equals (format doas audit d5) "permit" "AC2: cmd without 'args' keyword allows any arguments"
        d6 = evaluate doas rules rules for "pedro" in ["users"] as "root" running "/sbin/reboot" with ["now"]
        _ = assert equals (format doas audit d6) "deny" "AC2: cmd with empty 'args' keyword rejects non-empty arguments"
        env_rule = (dummy rule) with dr_action = ActPermit, dr_nopass = True, dr_setenv = ["-USER", "CUSTOM=42"], dr_ident = "pedro"
        san_env = build doas env env_rule for "pedro" as "root"
        _ = assert true (list contains str san_env "DOAS_USER=pedro") "doas env sets DOAS_USER=pedro"
        _ = assert true (list contains str san_env "CUSTOM=42") "doas setenv adds CUSTOM=42"
        _ = assert true (list contains str san_env "LOGNAME=root") "doas env sets LOGNAME to target user"
        assert true (not (list contains str san_env "USER=root")) "doas setenv -USER removes USER from environment"

fn :: test doas e2e suite => ()
      test doas e2e suite =
        nl = char from code 10
        _ = echo "=== 3. Testing Compiled Native ELF Binary target/doas (E2E) ==="
        cfg = "permit :wheel" + nl + "permit nopass boogie as root cmd /sbin/reboot args" + nl + "deny boogie as root cmd /sbin/reboot args -f" + nl
        _ = write file "/tmp/ut_doas_good.conf" cfg
        rc_c1 = shell exec "./target/doas -C /tmp/ut_doas_good.conf > /tmp/ut_doas_c1.out"
        _ = assert int equals rc_c1 0 "AC1 E2E binary: target/doas -C valid.conf exits 0"
        _ = write file "/tmp/ut_doas_bad.conf" ("permit :wheel" + nl + "bogus_rule_here" + nl)
        rc_c2 = shell exec "./target/doas -C /tmp/ut_doas_bad.conf > /tmp/ut_doas_c2.out"
        _ = assert true (rc_c2 != 0) "AC1 E2E binary: target/doas -C invalid.conf exits non-zero"
        _ = assert true (str starts with (read file "/tmp/ut_doas_c2.out") "doas.conf:2: syntax error") "AC1 E2E binary: target/doas -C reports exact syntax error line number"
        _ = shell exec "DOAS_CALLER=boogie DOAS_GROUPS=wheel ./target/doas -C /tmp/ut_doas_good.conf -u root /sbin/reboot > /tmp/ut_doas_q1.out"
        _ = assert equals (read file "/tmp/ut_doas_q1.out") ("permit nopass" + nl) "AC2 E2E binary: target/doas -C audit prints 'permit nopass'"
        _ = shell exec "DOAS_CALLER=boogie DOAS_GROUPS=wheel ./target/doas -C /tmp/ut_doas_good.conf -u root /sbin/reboot -f > /tmp/ut_doas_q2.out"
        _ = assert equals (read file "/tmp/ut_doas_q2.out") ("deny" + nl) "AC2 E2E binary: target/doas -C audit prints 'deny' when later rule denies"
        _ = shell exec "DOAS_CALLER=boogie DOAS_GROUPS=wheel ./target/doas -C /tmp/ut_doas_good.conf -u root /usr/bin/whoami > /tmp/ut_doas_q3.out"
        _ = assert equals (read file "/tmp/ut_doas_q3.out") ("permit" + nl) "AC2 E2E binary: target/doas -C audit prints 'permit' via :wheel"
        bs = char from code 92
        quoted_cfg = "permit nopass boogie as root cmd /bin/echo " + bs + nl + "  args \"hello world\" \"with spaces\"" + nl
        q_rules = unwrap rules (parse doas config quoted_cfg)
        q_dec_ok = evaluate doas rules q_rules for "boogie" in ["wheel"] as "root" running "/bin/echo" with ["hello world", "with spaces"]
        _ = assert equals (format doas audit q_dec_ok) "permit nopass" "AC2: doas policy matches multi-word quoted args after backslash line continuation"
        q_dec_split = evaluate doas rules q_rules for "boogie" in ["wheel"] as "root" running "/bin/echo" with ["hello", "world", "with spaces"]
        _ = assert equals (format doas audit q_dec_split) "deny" "AC2: doas policy rejects split tokens when quoted token is required"
        cont_cfg = "permit nopass boogie as root cmd /bin/echo " + bs + nl + "  args alpha_arg beta_arg" + nl
        _ = write file "/tmp/ut_doas_quoted.conf" cont_cfg
        rc_qc = shell exec "./target/doas -C /tmp/ut_doas_quoted.conf"
        _ = assert int equals rc_qc 0 "AC1 E2E binary: target/doas -C accepts backslash line continuation"
        _ = shell exec "DOAS_CALLER=boogie ./target/doas -C /tmp/ut_doas_quoted.conf -u root /bin/echo alpha_arg beta_arg > /tmp/ut_doas_q4.out"
        _ = assert equals (read file "/tmp/ut_doas_q4.out") ("permit nopass" + nl) "AC2 E2E binary: target/doas -C matches args across backslash line continuation"
        _ = shell exec "DOAS_CALLER=boogie ./target/doas -C /tmp/ut_doas_quoted.conf -u root /bin/echo alpha_arg wrong_arg > /tmp/ut_doas_q5.out"
        _ = assert equals (read file "/tmp/ut_doas_q5.out") ("deny" + nl) "AC2 E2E binary: target/doas -C rejects mismatched args across line continuation"
        rc_clear_l = shell exec "./target/doas -L"
        _ = assert int equals rc_clear_l 0 "AC2 E2E binary: target/doas -L clears persisted token and exits 0"
        _ = sys setenv "LD_PRELOAD" "/tmp/evil.so"
        _ = sys setenv "LD_LIBRARY_PATH" "/tmp/evil_lib"
        _ = sys setenv "SAFE_CUSTOM_VAR" "preserved_ok"
        keep_rule = (dummy rule) with dr_action = ActPermit, dr_nopass = True, dr_keepenv = True, dr_ident = "boogie"
        kept_env = build doas env keep_rule for "boogie" as "root"
        _ = sys setenv "LD_PRELOAD" ""
        _ = sys setenv "LD_LIBRARY_PATH" ""
        _ = assert false (list contains str kept_env "LD_PRELOAD=/tmp/evil.so") "AC2 security (VUL-INJ-001): doas keepenv strips dangerous LD_PRELOAD"
        _ = assert false (list contains str kept_env "LD_LIBRARY_PATH=/tmp/evil_lib") "AC2 security (VUL-INJ-001): doas keepenv strips dangerous LD_LIBRARY_PATH"
        _ = assert true (list contains str kept_env "SAFE_CUSTOM_VAR=preserved_ok") "AC2: doas keepenv preserves safe caller environment variables"
        pass_rule = (dummy rule) with dr_action = ActPermit, dr_nopass = False, dr_ident = "boogie"
        ni_spec = (parse doas args ["-n", "-u", "root", "/bin/id"])
        rc_ni = execute privileged pass_rule ni_spec "boogie"
        _ = assert false (check eq rc_ni 0) "AC2 security: doas -n non-interactive fails when rule requires password"
        int_spec = (parse doas args ["-u", "root", "/bin/id"])
        _ = sys setenv "DOAS_PASS" "wrong_password"
        _ = sys setenv "DOAS_AUTH_SHA256" "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
        rc_wrong_pw = execute privileged pass_rule int_spec "boogie"
        _ = assert false (check eq rc_wrong_pw 0) "AC2 security (VUL-AUTHN-001): doas rejects wrong password when nopass is False"
        _ = sys setenv "DOAS_PASS" "abc"
        _ = assert true (authenticate doas pass_rule int_spec "boogie") "AC2 security (VUL-AUTHN-001): doas accepts password matching SHA-256 verifier"
        _ = sys setenv "DOAS_PASS" ""
        _ = sys setenv "DOAS_AUTH_SHA256" ""
        _ = shell exec "rm -f /tmp/ut_doas_good.conf /tmp/ut_doas_bad.conf /tmp/ut_doas_quoted.conf /tmp/ut_doas_c1.out /tmp/ut_doas_c2.out /tmp/ut_doas_q1.out /tmp/ut_doas_q2.out /tmp/ut_doas_q3.out /tmp/ut_doas_q4.out /tmp/ut_doas_q5.out"
        ()

fn :: test sha256 suite => ()
      test sha256 suite =
        nl = char from code 10
        _ = echo "=== 4. Testing BSD sha256(1) Digest, HMAC-SHA256 & Argon2id with crypto.rglib (TS-SEC-003) ==="
        r1 = evaluate sha256 (parse sha256 args ["-s", "abc"]) with ""
        _ = assert str eq (snd r1) "SHA256 (\"abc\") = ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad" "AC1: sha256 -s abc BSD tag format"
        r_q = evaluate sha256 (parse sha256 args ["-q", "-s", "abc"]) with ""
        _ = assert str eq (snd r_q) "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad" "AC1: sha256 -q -s abc quiet hex digest"
        r_rev = evaluate sha256 (parse sha256 args ["-r", "-s", "abc"]) with ""
        _ = assert str eq (snd r_rev) "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad \"abc\"" "AC1: sha256 -r -s abc reversed output format"
        r_empty = evaluate sha256 (parse sha256 args ["-q", "-s", ""]) with ""
        _ = assert str eq (snd r_empty) "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855" "AC1: sha256 NIST FIPS 180-4 empty string vector"
        r_multiblock = evaluate sha256 (parse sha256 args ["-q", "-s", "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"]) with ""
        _ = assert str eq (snd r_multiblock) "248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1" "AC1: sha256 NIST multi-block (>512-bit boundary) vector"
        r_missing = evaluate sha256 (parse sha256 args ["/tmp/nonexistent_ut_sha256_file.bin"]) with ""
        _ = assert int equals (fst r_missing) 1 "AC1: sha256 returns exit code 1 when input file is missing"
        _ = assert true (str contains (snd r_missing) "No such file or directory") "AC1: sha256 reports missing file diagnostic"
        r_hmac = evaluate sha256 (parse sha256 args ["-q", "-k", "key", "-s", "The quick brown fox jumps over the lazy dog"]) with ""
        _ = assert str eq (snd r_hmac) "f7bc83f430538424b13298e6aa6fb143ef4d59a14946175997479dbc2d1a3cd8" "AC2: sha256 -q -k key -s '...' RFC HMAC-SHA256 vector"
        r_hmac_rfc2 = evaluate sha256 (parse sha256 args ["-q", "-k", "Jefe", "-s", "what do ya want for nothing?"]) with ""
        _ = assert str eq (snd r_hmac_rfc2) "5bdcc146bf60754e6a042426089575c75a003f089d2739839dec58b964ec3843" "AC2: sha256 RFC 4231 Test Case 2 (Jefe) HMAC-SHA256 vector"
        long_key = "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef_extra_key_bytes"
        r_hmac_long = evaluate sha256 (parse sha256 args ["-q", "-k", long_key, "-s", "test payload"]) with ""
        _ = assert int equals (str len (snd r_hmac_long)) 64 "AC2: sha256 HMAC with >64-byte key pre-hashes key and emits 64-char hex digest"
        r_arg = evaluate sha256 (parse sha256 args ["-q", "--argon2id", "saltsalt", "-s", "correcthorse"]) with ""
        phc = snd r_arg
        _ = assert true (str starts with phc "$argon2id$v=19$m=64,t=2,p=1$") "AC2: sha256 --argon2id emits Argon2id PHC string"
        r_ver_ok = evaluate sha256 (parse sha256 args ["--verify", phc, "-s", "correcthorse"]) with ""
        _ = assert int equals (fst r_ver_ok) 0 "AC2: sha256 --verify accepts matching password"
        r_ver_bad = evaluate sha256 (parse sha256 args ["--verify", phc, "-s", "wrongpass"]) with ""
        _ = assert int equals (fst r_ver_bad) 1 "AC2: sha256 --verify rejects wrong password"
        tampered_phc = (str slice phc 0 ((str len phc) - 1)) + "X"
        r_ver_tampered = evaluate sha256 (parse sha256 args ["--verify", tampered_phc, "-s", "correcthorse"]) with ""
        _ = assert false (check eq (fst r_ver_tampered) 0) "AC2: sha256 --verify rejects tampered Argon2id PHC hash"
        r_ver_malformed = evaluate sha256 (parse sha256 args ["--verify", "$invalid$phc", "-s", "correcthorse"]) with ""
        _ = assert false (check eq (fst r_ver_malformed) 0) "AC2 security: sha256 --verify rejects malformed PHC string"
        rc_sha_bin = shell exec "./target/sha256 -q -s abc > /tmp/ut_sha256_bin.out"
        _ = assert int equals rc_sha_bin 0 "E2E binary: target/sha256 -q -s abc exits 0"
        _ = assert str eq (read file "/tmp/ut_sha256_bin.out") ("ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad" + nl) "E2E binary: target/sha256 outputs exact SHA-256 hex"
        rc_sha_pipe = shell exec "printf 'abc' | ./target/sha256 -q > /tmp/ut_sha256_pipe.out"
        _ = assert int equals rc_sha_pipe 0 "E2E binary: printf 'abc' | target/sha256 -q reads fd 0 via posix read"
        _ = assert str eq (read file "/tmp/ut_sha256_pipe.out") ("ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad" + nl) "E2E binary: stdin pipeline SHA-256 matches -s abc"
        rc_hmac_bin = shell exec "./target/sha256 -q -k Jefe -s 'what do ya want for nothing?' > /tmp/ut_sha256_hmac.out"
        _ = assert int equals rc_hmac_bin 0 "E2E binary: target/sha256 -q -k Jefe exits 0"
        _ = assert str eq (read file "/tmp/ut_sha256_hmac.out") ("5bdcc146bf60754e6a042426089575c75a003f089d2739839dec58b964ec3843" + nl) "E2E binary: target/sha256 -q -k Jefe matches RFC 4231 vector"
        _ = sys unlink "/tmp/ut_sha256_bin.out"
        _ = sys unlink "/tmp/ut_sha256_pipe.out"
        _ = sys unlink "/tmp/ut_sha256_hmac.out"
        ()

fn :: test doas upstream parse y => ()
      test doas upstream parse y =
        nl = char from code 10
        bs = char from code 92
        dq = char from code 34
        _ = echo "=== 5. Upstream OpenBSD usr.bin/doas/parse.y Grammar & Lexer Regression Suite (TS-SEC-002) ==="
        c_inline = "permit nopass :wheel # inline comment after rule" + nl + "permit alice as root cmd /bin/echo args " + dq + "hash # inside quotes" + dq + nl
        p_inline = parse doas config c_inline
        _ = assert true (is parse ok p_inline) "upstream parse.y: inline # comment after rule and literal # inside double quotes parse cleanly"
        r_inline = unwrap rules p_inline
        _ = assert int equals (list len r_inline) 2 "upstream parse.y: 2 rules extracted with inline comments"
        d_hash = evaluate doas rules r_inline for "alice" in ["users"] as "root" running "/bin/echo" with ["hash # inside quotes"]
        _ = assert str eq (format doas audit d_hash) "permit" "upstream parse.y: # inside double-quoted arg is preserved as part of argument"
        c_esc = "permit nopass alice as root cmd /bin/echo args " + dq + "say " + bs + dq + "hi" + bs + dq + " " + bs + bs + " end" + dq + " " + dq + dq + nl
        p_esc = parse doas config c_esc
        _ = assert true (is parse ok p_esc) "upstream parse.y: escaped quotes \\\", escaped backslash \\\\, and empty quotes \"\" in args parse cleanly"
        d_esc = evaluate doas rules (unwrap rules p_esc) for "alice" in [] as "root" running "/bin/echo" with ["say \"hi\" \\ end", ""]
        _ = assert str eq (format doas audit d_esc) "permit nopass" "upstream parse.y: escaped quotes and empty string \"\" arg match exact argv"
        d_esc_miss = evaluate doas rules (unwrap rules p_esc) for "alice" in [] as "root" running "/bin/echo" with ["say \"hi\" \\ end"]
        _ = assert str eq (format doas audit d_esc_miss) "deny" "upstream parse.y: omitting trailing empty string \"\" arg is rejected"
        c_kw_args = "permit nopass alice as root cmd /bin/echo args permit deny nopass as cmd args keepenv" + nl
        p_kw_args = parse doas config c_kw_args
        _ = assert true (is parse ok p_kw_args) "upstream parse.y: reserved keywords after 'args' are accepted as literal command arguments"
        d_kw_args = evaluate doas rules (unwrap rules p_kw_args) for "alice" in [] as "root" running "/bin/echo" with ["permit", "deny", "nopass", "as", "cmd", "args", "keepenv"]
        _ = assert str eq (format doas audit d_kw_args) "permit nopass" "upstream parse.y: command arguments containing reserved words match accurately"
        c_multiline_err = "permit nopass " + bs + nl + "  keepenv " + bs + nl + "  :wheel" + nl + "invalid_directive_on_line_4" + nl
        _ = assert int equals (parse err line (parse doas config c_multiline_err)) 4 "upstream parse.y: syntax error after 3-line backslash continuation reports physical line 4"
        _ = assert int equals (parse err line (parse doas config ("permit setenv PATH alice" + nl))) 1 "upstream parse.y: missing '{' after setenv is rejected on line 1"
        _ = assert int equals (parse err line (parse doas config ("permit alice as root as operator" + nl))) 1 "upstream parse.y: duplicate 'as' target clause is rejected"
        _ = assert int equals (parse err line (parse doas config ("permit alice cmd /bin/ls as root" + nl))) 1 "upstream parse.y: 'as' clause after 'cmd' is rejected (must precede cmd)"
        _ = assert int equals (parse err line (parse doas config ("permit alice cmd /bin/ls -l" + nl))) 1 "upstream parse.y: arguments after 'cmd <prog>' without 'args' keyword are rejected"
        _ = assert int equals (parse err line (parse doas config ("permit as root" + nl))) 1 "upstream parse.y: reserved keyword 'as' in place of identity is rejected"
        assert int equals (parse err line (parse doas config ("permit alice as cmd" + nl))) 1 "upstream parse.y: reserved keyword 'cmd' in place of target user is rejected"

fn :: test doas upstream policy and env => ()
      test doas upstream policy and env =
        nl = char from code 10
        _ = echo "=== 6. Upstream OpenBSD usr.bin/doas/ (doas.c, env.c) Policy, Env & Auth Regression Suite (TS-SEC-002) ==="
        c_all_opts = "permit persist keepenv nolog nopass setenv \{ A=1 B=2 \} :wheel as operator cmd /usr/bin/id" + nl
        p_all = parse doas config c_all_opts
        _ = assert true (is parse ok p_all) "upstream parse.y: all options (persist keepenv nolog nopass setenv) in arbitrary order parse cleanly"
        r0 = head rule (unwrap rules p_all)
        _ = assert true ((dr_persist r0) and ((dr_keepenv r0) and ((dr_nolog r0) and (dr_nopass r0)))) "upstream parse.y: all boolean option flags set on parsed DoasRule"
        cascade_cfg = "permit :wheel" + nl + "deny :builders as root" + nl + "permit nopass alice as root cmd /usr/bin/make" + nl + "deny alice as root cmd /usr/bin/make args clean" + nl + "permit nopass alice as root cmd /usr/bin/make args clean FORCE=1" + nl
        c_rules = unwrap rules (parse doas config cascade_cfg)
        _ = assert str eq (format doas audit (evaluate doas rules c_rules for "alice" in ["wheel", "builders"] as "root" running "/bin/ls" with [])) "deny" "upstream doas.c cascade: rule 2 (deny :builders) overrides rule 1 (permit :wheel)"
        _ = assert str eq (format doas audit (evaluate doas rules c_rules for "alice" in ["wheel", "builders"] as "root" running "/usr/bin/make" with ["all"])) "permit nopass" "upstream doas.c cascade: rule 3 permits alice running /usr/bin/make all"
        _ = assert str eq (format doas audit (evaluate doas rules c_rules for "alice" in ["wheel", "builders"] as "root" running "/usr/bin/make" with ["clean"])) "deny" "upstream doas.c cascade: rule 4 denies alice running /usr/bin/make clean"
        _ = assert str eq (format doas audit (evaluate doas rules c_rules for "alice" in ["wheel", "builders"] as "root" running "/usr/bin/make" with ["clean", "FORCE=1"])) "permit nopass" "upstream doas.c cascade: rule 5 permits /usr/bin/make clean FORCE=1"
        _ = sys setenv "CALLER_EXPORTED" "from_caller_env"
        _ = sys setenv "SRC_TOKEN" "copied_via_dollar"
        _ = sys setenv "UNSET_CALLER_VAR" ""
        _ = sys setenv "IFS" " \t\n"
        env_r = (dummy rule) with dr_action = ActPermit, dr_nopass = True, dr_setenv = ["CALLER_EXPORTED", "$SRC_TOKEN", "MAPPED=$SRC_TOKEN", "UNSET_CALLER_VAR", "DROPPED=$UNSET_CALLER_VAR", "-LOGNAME", "PATH=/custom/bin", "LD_PRELOAD=/evil.so", "IFS=:"], dr_ident = "alice"
        built_env = build doas env env_r for "alice" as "operator"
        _ = sys setenv "CALLER_EXPORTED" ""
        _ = sys setenv "SRC_TOKEN" ""
        _ = assert true (list contains str built_env "CALLER_EXPORTED=from_caller_env") "upstream env.c: bare VAR in setenv copies caller environment variable"
        _ = assert true (list contains str built_env "SRC_TOKEN=copied_via_dollar") "upstream env.c: $VAR in setenv copies caller environment variable"
        _ = assert true (list contains str built_env "MAPPED=copied_via_dollar") "upstream env.c: DST=$SRC in setenv assigns caller's SRC value to DST"
        _ = assert false (list contains str built_env "UNSET_CALLER_VAR=") "upstream env.c: unset caller variable in setenv is omitted (not added as empty)"
        _ = assert false (list contains str built_env "DROPPED=") "upstream env.c: DST=$UNSET in setenv omits/removes DST when source is unset"
        _ = assert false (list contains str built_env "LOGNAME=operator") "upstream env.c: -LOGNAME in setenv removes default LOGNAME"
        _ = assert true (list contains str built_env "PATH=/custom/bin") "upstream env.c: PATH=... in setenv overrides default PATH"
        _ = assert false (list contains str built_env "LD_PRELOAD=/evil.so") "upstream env.c security (VUL-INJ-001): explicit LD_PRELOAD in setenv is stripped"
        _ = assert false (list contains str built_env "IFS=:") "upstream env.c security (VUL-INJ-001): explicit IFS in setenv is stripped"
        _ = sys setenv "SHELL" "/bin/zsh"
        sp_sh1 = parse doas args ["-s", "-u", "operator"]
        _ = assert str eq (d_cmd sp_sh1) "/bin/zsh" "upstream doas.c: doas -s selects $SHELL from environment when set"
        _ = sys setenv "SHELL" ""
        sp_sh2 = parse doas args ["-s"]
        _ = assert str eq (d_cmd sp_sh2) "/bin/sh" "upstream doas.c: doas -s falls back to /bin/sh when $SHELL is empty"
        sp_ddash = parse doas args ["-u", "root", "--", "-cmd_starting_with_dash", "-x"]
        _ = assert str eq (d_cmd sp_ddash) "-cmd_starting_with_dash" "upstream doas.c: '--' stops option parsing and treats '-cmd...' as command"
        r_arg_phc = evaluate sha256 (parse sha256 args ["-q", "--argon2id", "doassalt", "-s", "openbsd_pw"]) with ""
        doas_phc = snd r_arg_phc
        pw_rule = (dummy rule) with dr_action = ActPermit, dr_nopass = False, dr_ident = "alice"
        pw_spec = parse doas args ["-u", "root", "/usr/bin/id"]
        _ = sys setenv "DOAS_AUTH_PHC" doas_phc
        _ = sys setenv "DOAS_PASS" "openbsd_pw"
        _ = assert true (authenticate doas pw_rule pw_spec "alice") "upstream doas.c auth: Argon2id PHC verifier accepts matching password"
        _ = sys setenv "DOAS_PASS" "wrong_openbsd_pw"
        _ = assert false (authenticate doas pw_rule pw_spec "alice") "upstream doas.c auth: Argon2id PHC verifier rejects wrong password"
        _ = sys setenv "DOAS_AUTH_PHC" ""
        _ = sys setenv "DOAS_PASS" ""
        ()

fn :: test doas persist log path and sha256 c => ()
      test doas persist log path and sha256 c =
        nl = char from code 10
        _ = echo "=== 7. Testing doas persist/nolog/PATH & sha256 -c Checklist Verification (TS-SEC-002, TS-SEC-003) ==="
        -- 1. doas bare command PATH resolution
        _ = write file "/tmp/ut_doas_path.conf" ("permit nopass boogie as root cmd /bin/echo" + nl + "permit nopass boogie as root cmd /usr/bin/echo" + nl)
        rc_bare = shell exec "DOAS_CALLER=boogie PATH=/bin:/usr/bin ./target/doas -C /tmp/ut_doas_path.conf -u root echo hello_path > /tmp/ut_doas_bare.out"
        _ = assert int equals rc_bare 0 "doas bare command 'echo' resolved via PATH exits 0 in -C audit"
        _ = assert str eq (read file "/tmp/ut_doas_bare.out") ("permit nopass" + nl) "doas bare command 'echo' resolves to /bin/echo and matches rule"
        -- 2. doas persist session caching & -L ticket clearing
        _ = shell exec "./target/doas -L"
        persist_rule = (dummy rule) with dr_action = ActPermit, dr_nopass = False, dr_persist = True, dr_ident = "boogie"
        ni_echo = parse doas args ["-n", "-u", "root", "/bin/true"]
        rc_pre = execute privileged persist_rule ni_echo "boogie"
        _ = assert false (check eq rc_pre 0) "doas persist: before authentication, -n non-interactive fails"
        int_echo = parse doas args ["-u", "root", "/bin/true"]
        _ = sys setenv "DOAS_PASS" "abc"
        _ = sys setenv "DOAS_AUTH_SHA256" "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
        rc_auth = execute privileged persist_rule int_echo "boogie"
        _ = sys setenv "DOAS_PASS" ""
        _ = assert int equals rc_auth 0 "doas persist: interactive authentication with valid password succeeds and writes persist ticket"
        rc_cached = execute privileged persist_rule ni_echo "boogie"
        _ = assert int equals rc_cached 0 "doas persist: subsequent -n non-interactive execution succeeds using cached persist ticket without password"
        _ = shell exec "./target/doas -L"
        rc_cleared = execute privileged persist_rule ni_echo "boogie"
        _ = sys setenv "DOAS_AUTH_SHA256" ""
        _ = assert false (check eq rc_cleared 0) "doas -L: clearing persist ticket causes subsequent -n execution to fail again"
        -- 3. doas audit logging vs nolog suppression
        _ = sys unlink "/tmp/ut_doas_audit.log"
        _ = sys setenv "DOAS_LOG_FILE" "/tmp/ut_doas_audit.log"
        log_rule = (dummy rule) with dr_action = ActPermit, dr_nopass = True, dr_nolog = False, dr_ident = "boogie"
        nolog_rule = (dummy rule) with dr_action = ActPermit, dr_nopass = True, dr_nolog = True, dr_ident = "boogie"
        _ = execute privileged log_rule (parse doas args ["-u", "root", "/bin/true"]) "boogie"
        log_txt1 = read file "/tmp/ut_doas_audit.log"
        _ = assert true (str contains log_txt1 "boogie") "doas logging: rule without nolog appends caller to DOAS_LOG_FILE"
        _ = assert true (str contains log_txt1 "/bin/true") "doas logging: rule without nolog appends command to DOAS_LOG_FILE"
        _ = sys unlink "/tmp/ut_doas_audit.log"
        _ = execute privileged nolog_rule (parse doas args ["-u", "root", "/bin/true"]) "boogie"
        _ = assert false (lstat exists (file lstat "/tmp/ut_doas_audit.log")) "doas nolog: rule with nolog suppresses writing to DOAS_LOG_FILE"
        _ = sys setenv "DOAS_LOG_FILE" ""
        -- 4. sha256 -c checklist verification (BSD tag format and reversed format, pass & fail)
        _ = write file "/tmp/ut_sha_f1.txt" "abc"
        _ = write file "/tmp/ut_sha_f2.txt" ""
        chk_bsd = "SHA256 (/tmp/ut_sha_f1.txt) = ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad" + nl + "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855  /tmp/ut_sha_f2.txt" + nl
        _ = write file "/tmp/ut_sha_list.sums" chk_bsd
        r_chk_ok = evaluate sha256 (parse sha256 args ["-c", "/tmp/ut_sha_list.sums"]) with ""
        _ = assert int equals (fst r_chk_ok) 0 "sha256 -c: valid BSD + reversed checklist exits 0"
        _ = assert true (str contains (snd r_chk_ok) "(SHA256) /tmp/ut_sha_f1.txt: OK") "sha256 -c: outputs (SHA256) <file>: OK for BSD line"
        _ = assert true (str contains (snd r_chk_ok) "(SHA256) /tmp/ut_sha_f2.txt: OK") "sha256 -c: outputs (SHA256) <file>: OK for reversed line"
        _ = write file "/tmp/ut_sha_f1.txt" "tampered_abc"
        r_chk_fail = evaluate sha256 (parse sha256 args ["-c", "/tmp/ut_sha_list.sums"]) with ""
        _ = assert false (check eq (fst r_chk_fail) 0) "sha256 -c: tampered file in checklist returns non-zero exit code"
        _ = assert true (str contains (snd r_chk_fail) "(SHA256) /tmp/ut_sha_f1.txt: FAILED") "sha256 -c: reports FAILED for tampered file"
        rc_bin_chk = shell exec "./target/sha256 -c /tmp/ut_sha_list.sums > /tmp/ut_sha_chk_bin.out"
        _ = assert false (check eq rc_bin_chk 0) "E2E binary: target/sha256 -c exits non-zero on tampered file"
        _ = shell exec "rm -f /tmp/ut_doas_path.conf /tmp/ut_doas_bare.out /tmp/ut_doas_audit.log /tmp/ut_sha_f1.txt /tmp/ut_sha_f2.txt /tmp/ut_sha_list.sums /tmp/ut_sha_chk_bin.out"
        ()

fn :: head rule [DoasRule] => DoasRule
      head rule []         =  dummy rule
      head rule (r:_)      =  r

fn :: main => ()
      main =
        _ = test doas parser suite
        _ = test doas policy suite
        _ = test doas e2e suite
        _ = test sha256 suite
        _ = test doas upstream parse y
        _ = test doas upstream policy and env
        _ = test doas persist log path and sha256 c
        echo "All 101 Tier 4 & Upstream OpenBSD Security (doas, doas.conf, sha256) assertions passed!"
