-- test_sec.rg — Exhaustive Unit & E2E Binary Tests for Tier 4 OpenBSD doas(1), doas.conf(5) & BSD sha256(1)
-- @tests TS-SEC-002
-- @tests TS-SEC-003

import test
import posix
import crypto
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
        assert int equals (parse err line (parse doas config ("permit pedro as" + nl))) 1 "AC1: missing target after 'as' reports syntax error on line 1"

fn :: test doas policy suite => ()
      test doas policy suite =
        nl = char from code 10
        _ = echo "=== 2. Testing OpenBSD 'Last Matching Rule Wins' Policy Engine (TS-SEC-001) ==="
        cfg = "permit :wheel" + nl + "permit nopass pedro as root cmd /sbin/reboot args" + nl + "deny pedro as root cmd /sbin/reboot args -f" + nl + "permit pedro as operator cmd /usr/bin/id" + nl
        rules = unwrap rules (parse doas config cfg)
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
        env_rule = (dummy rule) with dr_action: ActPermit, dr_nopass: True, dr_setenv: ["-USER", "CUSTOM=42"], dr_ident: "pedro"
        san_env = build doas env env_rule for "pedro" as "root"
        _ = assert true (list contains str san_env "DOAS_USER=pedro") "doas env sets DOAS_USER=pedro"
        _ = assert true (list contains str san_env "CUSTOM=42") "doas setenv adds CUSTOM=42"
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
        _ = shell exec "rm -f /tmp/ut_doas_good.conf /tmp/ut_doas_bad.conf /tmp/ut_doas_c1.out /tmp/ut_doas_c2.out /tmp/ut_doas_q1.out /tmp/ut_doas_q2.out /tmp/ut_doas_q3.out"
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
        rc_sha_bin = shell exec "./target/sha256 -q -s abc > /tmp/ut_sha256_bin.out"
        _ = assert int equals rc_sha_bin 0 "E2E binary: target/sha256 -q -s abc exits 0"
        _ = assert str eq (read file "/tmp/ut_sha256_bin.out") ("ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad" + nl) "E2E binary: target/sha256 outputs exact SHA-256 hex"
        rc_sha_pipe = shell exec "printf 'abc' | ./target/sha256 -q > /tmp/ut_sha256_pipe.out"
        _ = assert int equals rc_sha_pipe 0 "E2E binary: printf 'abc' | target/sha256 -q reads fd 0 via posix read"
        _ = assert str eq (read file "/tmp/ut_sha256_pipe.out") ("ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad" + nl) "E2E binary: stdin pipeline SHA-256 matches -s abc"
        _ = sys unlink "/tmp/ut_sha256_bin.out"
        _ = sys unlink "/tmp/ut_sha256_pipe.out"
        ()

fn :: main => ()
      main =
        _ = test doas parser suite
        _ = test doas policy suite
        _ = test doas e2e suite
        _ = test sha256 suite
        echo "All 38 Tier 4 BSD Security (doas, doas.conf, sha256) unit & E2E assertions passed!"
