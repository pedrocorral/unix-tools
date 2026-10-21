-- test_filter.rg — Exhaustive unit & E2E binary test suite for Tier 1 BSD Filters
-- @tests TS-FILTER-001
-- @tests TS-FILTER-002

import test
import filter_lib

fn :: build ascii range Int Int String => String
      build ascii range code end_code acc =
        | code > end_code = acc
        | otherwise       = build ascii range (code + 1) end_code (acc + (char from code code))

fn :: test jot suite => ()
      test jot suite =
        nl = char from code 10
        _ = echo "=== 1. Testing BSD jot(1) Unit & Edge Cases (TS-FILTER-001) ==="
        _ = assert equals (run jot ["5", "10", "50"]) ("10" + nl + "20" + nl + "30" + nl + "40" + nl + "50") "AC1: jot 5 10 50"
        _ = assert equals (run jot ["-s", ",", "4", "1", "4"]) "1,2,3,4" "AC1: jot -s , 4 1 4"
        _ = assert equals (run jot ["-b", "ok", "3"]) ("ok" + nl + "ok" + nl + "ok") "AC2: jot -b ok 3"
        _ = assert equals (run jot ["-s,", "4"]) "1,2,3,4" "jot 1-arg (reps only) + attached -s,"
        _ = assert equals (run jot ["-s,", "4", "10"]) "10,11,12,13" "jot 2-arg (reps, begin)"
        _ = assert equals (run jot ["-s,", "5", "50", "10"]) "50,40,30,20,10" "jot descending sequence (5 50 10 -> step -10)"
        _ = assert equals (run jot ["-s,", "4", "2", "100", "3"]) "2,5,8,11" "jot 4 explicit args (reps, begin, end, step)"
        _ = assert equals (run jot ["-s", ":", "-", "10", "30", "10"]) "10:20:30" "jot omitted reps (- 10 30 10)"
        _ = assert equals (run jot ["-s,", "-", "5", "8"]) "5,6,7,8" "jot omitted reps & step ascending (- 5 8)"
        _ = assert equals (run jot ["-s,", "-", "8", "5"]) "8,7,6,5" "jot omitted reps & step descending (- 8 5)"
        _ = assert equals (run jot ["-s,", "4", "-", "10"]) "7,8,9,10" "jot omitted begin & step (4 - 10)"
        _ = assert equals (run jot ["-s,", "4", "-", "10", "2"]) "4,6,8,10" "jot omitted begin with step (4 - 10 2)"
        _ = assert equals (run jot ["-s,", "4", "5", "-", "3"]) "5,8,11,14" "jot omitted end (4 5 - 3)"
        _ = assert equals (run jot ["-s,", "4", "-10", "-22", "-4"]) "-10,-14,-18,-22" "jot negative numbers (-10 to -22 step -4)"
        _ = assert equals (run jot ["-s,", "--", "3", "-5"]) "-5,-4,-3" "jot -- end-of-options with negative begin"
        _ = assert equals (run jot ["1", "42", "99"]) "42" "jot single repetition (reps = 1)"
        _ = assert equals (run jot ["-w", "item_%d", "-s", " ", "3", "1"]) "item_1 item_2 item_3" "jot -w item_%d"
        _ = assert equals (run jot ["-w", "%04d", "-s,", "3", "8"]) "0008,0009,0010" "jot -w %04d zero-padded width"
        _ = assert equals (run jot ["-w", "%4d", "-s,", "2", "9"]) "   9,  10" "jot -w %4d right-aligned width"
        _ = assert equals (run jot ["-w", "%-4d", "-s|", "2", "9"]) "9   |10  " "jot -w %-4d left-aligned width"
        _ = assert equals (run jot ["-w", "0x%02x", "-s,", "4", "10"]) "0x0a,0x0b,0x0c,0x0d" "jot -w 0x%02x lowercase hex"
        _ = assert equals (run jot ["-w", "%X", "-s,", "4", "10"]) "A,B,C,D" "jot -w %X uppercase hex"
        _ = assert equals (run jot ["-w", "%03o", "-s,", "3", "7"]) "007,010,011" "jot -w %03o octal format"
        _ = assert equals (run jot ["-w", "100%%_%d", "-s,", "2", "1"]) "100%_1,100%_2" "jot -w escaped %% specifier"
        _ = assert equals (run jot ["-w[#%i]", "-s", "", "3", "1"]) "[#1][#2][#3]" "jot attached -w[#%i]"
        _ = assert equals (run jot ["-b!", "-s", "", "4"]) "!!!!" "jot attached -b!"
        _ = assert equals (run jot ["-p", "2", "-s,", "5", "0.00", "1.00"]) "0.00,0.25,0.50,0.75,1.00" "jot -p 2 fixed-point decimal sequence"
        _ = assert equals (run jot ["-s,", "3", "1.5", "3.5"]) "1.5,2.5,3.5" "jot auto-inferred decimal precision from 1.5..3.5"
        _ = assert equals (run jot ["-w", "%.2f", "-s,", "3", "1", "2"]) "1.00,1.50,2.00" "jot -w %.2f precision formatting"
        _ = assert equals (run jot ["-c", "-s", "", "4", "A"]) "ABCD" "jot -c character mode from 'A'"
        _ = assert equals (run jot ["-cn", "-s", "", "-", "a", "f"]) "abcdef" "jot -cn combined flag char range a..f"
        _ = assert equals (run jot ["-c", "-s", ",", "3", "65"]) "A,B,C" "jot -c with numeric ASCII code 65"
        rand_a = run jot ["-r", "-s,", "5", "10", "20", "42"]
        rand_b = run jot ["-r", "-s,", "5", "10", "20", "42"]
        _ = assert equals rand_a rand_b "jot -r deterministic PRNG with explicit seed 42"
        assert true ((str len rand_a) >= 14) "jot -r generates 5 two-digit values in [10..20]"

fn :: test rs suite => ()
      test rs suite =
        nl = char from code 10
        _ = echo "=== 2. Testing BSD rs(1) Unit & Edge Cases (TS-FILTER-001) ==="
        _ = assert equals (reshape text "a b c d e f" with (parse rs args ["2", "3"])) ("a  b  c" + nl + "d  e  f") "AC3: rs 2 3"
        _ = assert equals (reshape text "a b c d e f" with (parse rs args ["-t", "2", "3"])) ("a  d" + nl + "b  e" + nl + "c  f") "AC3: rs -t 2 3 transpose"
        _ = assert equals (reshape text "a b c d e" with (parse rs args ["2", "3"])) ("a  b  c" + nl + "d  e") "rs 2 3 ragged last row (5 items, no trailing spaces)"
        _ = assert equals (reshape text "a b c d e" with (parse rs args ["-t", "2", "3"])) ("a  d" + nl + "b  e" + nl + "c") "rs -t 2 3 ragged transpose (5 items)"
        _ = assert equals (reshape text "a b c d e f" with (parse rs args ["0", "3"])) ("a  b  c" + nl + "d  e  f") "rs 0 3 auto-infer rows"
        _ = assert equals (reshape text "a b c d e f g" with (parse rs args ["0", "3"])) ("a  b  c" + nl + "d  e  f" + nl + "g") "rs 0 3 auto-infer rows with ragged 7th item"
        _ = assert equals (reshape text "a b c d e f" with (parse rs args ["2"])) ("a  b  c" + nl + "d  e  f") "rs 2 auto-infer cols"
        _ = assert equals (reshape text "a b c d" with (parse rs args [])) "a  b  c  d" "rs default 1-row output"
        _ = assert equals (reshape text "hello world alpha bravo" with (parse rs args ["-w", "14"])) ("hello  world" + nl + "alpha  bravo") "rs -w 14 auto-calculates 2x2 grid from display width"
        _ = assert equals (reshape text ("first line" + nl + "second line" + nl + "third line" + nl + "fourth line") with (parse rs args ["-e", "-C|", "2", "2"])) ("first line|second line" + nl + "third line|fourth line") "rs -e treats each line as an entry"
        _ = assert equals (reshape text ("HEADER1" + nl + "HEADER2" + nl + "a b c d") with (parse rs args ["-k", "2", "2", "2"])) ("a  b" + nl + "c  d") "rs -k 2 skips first 2 header lines"
        _ = assert equals (reshape text "x y" with (parse rs args ["-y", "2", "3"])) ("x  y  x" + nl + "y  x  y") "rs -y recycles entries to fill 2x3 grid"
        _ = assert equals (reshape text "a b c d e f" with (parse rs args ["-h", "2", "3"])) "2 by 3" "rs -h prints shape '2 by 3'"
        _ = assert equals (reshape text "a b c d e f" with (parse rs args ["-th", "2", "3"])) "3 by 2" "rs -th prints transposed shape '3 by 2'"
        _ = assert equals (reshape text "long a b short" with (parse rs args ["-z", "2", "2"])) ("long  a" + nl + "b  short") "rs -z squeezes column padding"
        _ = assert equals (reshape text "a:b:c:d" with (parse rs args ["-c:", "2", "2"])) ("a  b" + nl + "c  d") "rs -c: custom input delimiter"
        _ = assert equals (reshape text "1 2 3 4 5 6" with (parse rs args ["-C,", "2", "3"])) ("1,2,3" + nl + "4,5,6") "rs -C, custom output delimiter"
        _ = assert equals (reshape text "a b c d" with (parse rs args ["-g4", "2", "2"])) ("a    b" + nl + "c    d") "rs -g4 custom gutter width"
        _ = assert equals (reshape text "1 100 20 3" with (parse rs args ["-j", "2", "2"])) (" 1  100" + nl + "20    3") "rs -j right-aligned columns"
        _ = assert equals (reshape text "1 100 20 3" with (parse rs args ["-tj", "2", "2"])) ("  1  20" + nl + "100   3") "rs -tj combined transpose + right-align"
        assert equals (reshape text "" with (parse rs args ["2", "2"])) "" "rs on empty input returns empty string"

fn :: test lam suite => ()
      test lam suite =
        nl = char from code 10
        _ = echo "=== 3. Testing BSD lam(1) Unit & Edge Cases (TS-FILTER-002) ==="
        col_a = LamCol "" 0 0 False ["alpha", "beta", "gamma"]
        col_b = LamCol " -> " 0 0 False ["1", "2", "3"]
        _ = assert equals (laminate columns [col_a, col_b] with "") ("alpha -> 1" + nl + "beta -> 2" + nl + "gamma -> 3") "AC1: lam a.txt -s ' -> ' b.txt"
        col_pad = LamCol "" (0 - 6) 6 False ["a", "abcdefg"]
        col_num = LamCol ":" 3 0 False ["7", "42"]
        _ = assert equals (laminate columns [col_pad, col_num] with "!") ("a     :  7!" + nl + "abcdef: 42!") "lam -f min.max padding, truncation, and trailing -s"
        col_short = LamCol "[" (0 - 4) 0 True ["x"]
        col_long = LamCol "|" 2 0 False ["1", "2", "3"]
        _ = assert equals (laminate columns [col_short, col_long] with "]") ("[x   | 1]" + nl + "[    | 2]" + nl + "[    | 3]") "lam uneven line counts with padding preserved on empty lines"
        assert equals (laminate columns [] with "") "" "lam with zero columns"

fn :: test vis unvis suite => ()
      test vis unvis suite =
        _ = echo "=== 4. Testing BSD vis(1) & unvis(1) Unit & Edge Cases (TS-FILTER-002) ==="
        bs = char from code 92
        tab = char from code 9
        nl = char from code 10
        cr = char from code 13
        bel = char from code 7
        bs_ch = char from code 8
        vt = char from code 11
        ff = char from code 12
        esc = char from code 27
        ctrl_a = char from code 1
        ctrl_z = char from code 26
        del_ch = char from code 127
        sample = "hello" + tab + "world" + bs + "path" + bel + bs_ch + vt + ff + cr + ctrl_a + ctrl_z + del_ch + nl
        spec_def = parse vis args []
        enc_def = vis encode sample with spec_def
        _ = assert equals (unvis decode enc_def) sample "AC2: unvis(vis(S)) == S default mode"
        spec_w = parse vis args ["-w"]
        enc_w = vis encode sample with spec_w
        _ = assert equals (unvis decode enc_w) sample "AC2: unvis(vis -w(S)) == S whitespace-encoded mode"
        spec_o = parse vis args ["-o", "-w"]
        enc_o = vis encode sample with spec_o
        _ = assert equals (unvis decode enc_o) sample "AC2: unvis(vis -o -w(S)) == S octal mode"
        spec_l = parse vis args ["-l"]
        enc_l = vis encode ("line1" + nl + "line2" + nl) with spec_l
        _ = assert true (str contains enc_l (bs + "$" + nl)) "vis -l marks newlines with \\$"
        _ = assert equals (unvis decode enc_l) ("line1" + nl + "line2" + nl) "unvis decodes vis -l (\\$ newline markers) losslessly"
        spec_fold = parse vis args ["-F", "10"]
        long_txt = "0123456789abcdefghij012345"
        enc_fold = vis encode long_txt with spec_fold
        _ = assert true (str contains enc_fold (bs + nl)) "vis -F 10 inserts soft line continuation (\\ + newline)"
        _ = assert equals (unvis decode enc_fold) long_txt "unvis strips soft line continuations from vis -F 10 losslessly"
        spec_n = parse vis args ["-n"]
        enc_n = vis encode ("a" + bs + "b" + ctrl_a) with spec_n
        _ = assert equals enc_n ("a" + bs + bs + "b" + ctrl_a) "vis -n encodes only backslashes"
        _ = assert equals (unvis decode (bs + "x41" + bs + "x7a" + bs + "E")) ("Az" + esc) "unvis decodes \\xHH hex escapes and \\E (ESC)"
        _ = assert equals (unvis decode (bs + "7" + bs + "12")) (bel + nl) "unvis decodes 1-digit and 2-digit octal escapes"
        adversarial = "literal " + bs + "n and " + bs + "^A and " + bs + "040 and " + bs + bs + " end"
        enc_adv = vis encode adversarial with (parse vis args ["-w"])
        _ = assert equals (unvis decode enc_adv) adversarial "AC2: adversarial literal escape sequences survive vis/unvis roundtrip"
        all_ascii = build ascii range 1 127 ""
        enc_ascii_c = vis encode all_ascii with (parse vis args ["-ctw"])
        _ = assert equals (unvis decode enc_ascii_c) all_ascii "AC2: all 127 ASCII chars (1..127) roundtrip in -ctw mode"
        enc_ascii_o = vis encode all_ascii with (parse vis args ["-ow"])
        _ = assert equals (unvis decode enc_ascii_o) all_ascii "AC2: all 127 ASCII chars (1..127) roundtrip in -ow octal mode"
        spec_b = parse vis args ["-b"]
        enc_b = vis encode ("a" + bs + "b") with spec_b
        assert equals enc_b ("a" + bs + "b") "vis -b does not double backslashes"

fn :: test e2e binaries suite => ()
      test e2e binaries suite =
        nl = char from code 10
        tab = char from code 9
        bs = char from code 92
        ctrl_a = char from code 1
        del_ch = char from code 127
        _ = echo "=== 5. Testing Compiled Native ELF Binaries in target/ (E2E) ==="
        _ = shell exec "./target/jot 5 10 50 > /tmp/ut_jot1.out"
        _ = assert equals (read file "/tmp/ut_jot1.out") ("10" + nl + "20" + nl + "30" + nl + "40" + nl + "50" + nl) "E2E binary: target/jot 5 10 50"
        rc_jot_n = shell exec "test $(./target/jot -n -s , 4 1 4 | wc -c) -eq 7"
        _ = assert int equals rc_jot_n 0 "E2E binary: target/jot -n -s , 4 1 4 emits exact 7 bytes (no trailing newline)"
        _ = write file "/tmp/ut_rs.in" ("a b c d e f" + nl)
        _ = shell exec "./target/rs -t 2 3 /tmp/ut_rs.in > /tmp/ut_rs.out"
        _ = assert equals (read file "/tmp/ut_rs.out") ("a  d" + nl + "b  e" + nl + "c  f" + nl) "E2E binary: target/rs -t 2 3"
        _ = write file "/tmp/ut_lam_a.in" ("alpha" + nl + "beta" + nl + "gamma" + nl)
        _ = write file "/tmp/ut_lam_b.in" ("1" + nl + "2" + nl + "3" + nl)
        _ = shell exec "./target/lam /tmp/ut_lam_a.in -s ' -> ' /tmp/ut_lam_b.in > /tmp/ut_lam.out"
        _ = assert equals (read file "/tmp/ut_lam.out") ("alpha -> 1" + nl + "beta -> 2" + nl + "gamma -> 3" + nl) "E2E binary: target/lam with -s ' -> '"
        _ = shell exec "./target/lam -S '|' -F -4.4 /tmp/ut_lam_a.in /tmp/ut_lam_b.in > /tmp/ut_lam_cap.out"
        _ = assert equals (read file "/tmp/ut_lam_cap.out") ("|alph|1   |" + nl + "|beta|2   |" + nl + "|gamm|3   |" + nl) "E2E binary: target/lam persistent -S and -F flags"
        _ = write file "/tmp/ut_lam_colon.in" "u1:u2:u3:"
        _ = shell exec "./target/lam -t : /tmp/ut_lam_colon.in -s '=' /tmp/ut_lam_b.in > /tmp/ut_lam_t.out"
        _ = assert equals (read file "/tmp/ut_lam_t.out") ("u1=1" + nl + "u2=2" + nl + "u3=3" + nl) "E2E binary: target/lam -t : custom input line terminator"
        raw_payload = "linea 1" + tab + "con" + bs + "barra" + ctrl_a + del_ch + nl + "linea 2 con salto final" + nl
        _ = write file "/tmp/ut_vis.in" raw_payload
        _ = shell exec "./target/vis -t /tmp/ut_vis.in > /tmp/ut_vis1.enc && ./target/unvis /tmp/ut_vis1.enc > /tmp/ut_vis1.dec"
        rc_cmp1 = shell exec "cmp -s /tmp/ut_vis.in /tmp/ut_vis1.dec"
        _ = assert int equals rc_cmp1 0 "E2E binary: target/vis -t | target/unvis byte-for-byte identical via cmp(1)"
        _ = shell exec "./target/vis -tw /tmp/ut_vis.in > /tmp/ut_vis2.enc && ./target/unvis /tmp/ut_vis2.enc > /tmp/ut_vis2.dec"
        rc_cmp2 = shell exec "cmp -s /tmp/ut_vis.in /tmp/ut_vis2.dec"
        _ = assert int equals rc_cmp2 0 "E2E binary: target/vis -tw | target/unvis byte-for-byte identical via cmp(1)"
        _ = shell exec "./target/vis -F 12 -t /tmp/ut_vis.in > /tmp/ut_vis3.enc && ./target/unvis /tmp/ut_vis3.enc > /tmp/ut_vis3.dec"
        rc_cmp3 = shell exec "cmp -s /tmp/ut_vis.in /tmp/ut_vis3.dec"
        _ = assert int equals rc_cmp3 0 "E2E binary: target/vis -F 12 -t | target/unvis folded lines byte-for-byte identical"
        _ = write file "/tmp/ut_import.in" ("import foo" + nl + "export bar" + nl)
        _ = shell exec "./target/vis /tmp/ut_import.in > /tmp/ut_import.out"
        _ = assert equals (read file "/tmp/ut_import.out") ("import foo" + nl + "export bar" + nl) "E2E regression: read file preserves lines starting with 'import ' verbatim"
        _ = write file "/tmp/ut_no_nl.in" "sin_salto"
        rc_no_nl = shell exec "test $(./target/vis /tmp/ut_no_nl.in | wc -c) -eq 9"
        _ = assert int equals rc_no_nl 0 "E2E regression: read file does not inject synthetic trailing newline"
        cr = char from code 13
        _ = write file "/tmp/ut_crlf.in" ("uno" + cr + nl + "dos")
        _ = shell exec "./target/vis -w /tmp/ut_crlf.in > /tmp/ut_crlf.out"
        _ = assert equals (read file "/tmp/ut_crlf.out") ("uno" + bs + "r" + bs + "ndos") "E2E regression: read file preserves CR (\\r) in CRLF input"
        _ = write bytes "/tmp/ut_bin256.in" [0, 1, 7, 8, 9, 10, 13, 27, 32, 65, 92, 127, 128, 129, 200, 255]
        _ = shell exec "./target/vis -w /tmp/ut_bin256.in > /tmp/ut_bin256.enc && ./target/unvis /tmp/ut_bin256.enc > /tmp/ut_bin256.dec"
        rc_bin256 = shell exec "cmp -s /tmp/ut_bin256.in /tmp/ut_bin256.dec"
        _ = assert int equals rc_bin256 0 "E2E binary: target/vis -w | target/unvis 0..255 raw binary roundtrip (NUL + high bytes)"
        _ = shell exec "rm -f /tmp/ut_jot1.out /tmp/ut_rs.in /tmp/ut_rs.out /tmp/ut_lam_a.in /tmp/ut_lam_b.in /tmp/ut_lam.out /tmp/ut_lam_cap.out /tmp/ut_lam_colon.in /tmp/ut_lam_t.out /tmp/ut_vis.in /tmp/ut_vis1.enc /tmp/ut_vis1.dec /tmp/ut_vis2.enc /tmp/ut_vis2.dec /tmp/ut_vis3.enc /tmp/ut_vis3.dec /tmp/ut_import.in /tmp/ut_import.out /tmp/ut_no_nl.in /tmp/ut_crlf.in /tmp/ut_crlf.out /tmp/ut_bin256.in /tmp/ut_bin256.enc /tmp/ut_bin256.dec"
        ()

fn :: main => ()
      main =
        _ = test jot suite
        _ = test rs suite
        _ = test lam suite
        _ = test vis unvis suite
        _ = test e2e binaries suite
        echo "All 81 Tier 1 BSD Filter unit & E2E binary assertions passed!"
