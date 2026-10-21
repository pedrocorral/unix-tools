-- test_proc.rg — Exhaustive Unit & E2E Binary Tests for Tier 3 BSD Process, Build & Socket Utilities
-- @tests TS-PROC-001
-- @tests TS-PROC-002
-- @tests TS-PROC-003

import test
import io
import socket
import proc_lib

fn :: test apply suite => ()
      test apply suite =
        _ = echo "=== 1. Testing BSD apply(1) Unit & Edge Cases (TS-PROC-001) ==="
        spec1 = parse apply args ["-d", "-2", "diff %1 %2", "a1", "b1", "a2", "b2"]
        res1 = evaluate apply spec1
        _ = assert equals (join with (fst res1) " | ") "diff a1 b1 | diff a2 b2" "AC1: apply -d -2 'diff %1 %2' a1 b1 a2 b2"
        spec2 = parse apply args ["-d", "mv %1 %2.bak", "f1", "g1", "f2", "g2"]
        res2 = evaluate apply spec2
        _ = assert equals (join with (fst res2) " | ") "mv f1 g1.bak | mv f2 g2.bak" "apply auto-infers group size 2 from %2 in command"
        spec3 = parse apply args ["-d", "-a", "@", "cp @1 @2", "src1", "dst1"]
        res3 = evaluate apply spec3
        _ = assert equals (join with (fst res3) " | ") "cp src1 dst1" "apply -a @ custom magic character"
        spec4 = parse apply args ["-d", "-3", "echo", "x", "y", "z", "u", "v", "w"]
        res4 = evaluate apply spec4
        assert equals (join with (fst res4) " | ") "echo x y z | echo u v w" "apply -3 without %N appends 3 arguments per command"

fn :: test make suite => ()
      test make suite =
        nl = char from code 10
        tab = char from code 9
        _ = echo "=== 2. Testing BSD make(1) Unit & Edge Cases (TS-PROC-001) ==="
        mk_src = "CC = cc" + nl + "CFLAGS ?= -O2" + nl + ".PHONY: all clean" + nl + "all: app" + nl + tab + "echo done $(CC)" + nl + "app: main.o util.o" + nl + tab + "$(CC) $(CFLAGS) -o $@ main.o util.o" + nl + "main.o:" + nl + tab + "$(CC) -c main.c" + nl + "util.o:" + nl + tab + "$\{CC\} -c util.c" + nl
        mspec1 = parse make args ["-n"]
        mres1 = evaluate make mk_src with mspec1
        _ = assert true (fst mres1) "AC2: make -n succeeds on valid DAG"
        _ = assert equals (join with (snd mres1) " ; ") "cc -c main.c ; cc -c util.c ; cc -O2 -o app main.o util.o ; echo done cc" "AC2: make -n prints recipes in topological dependency order"
        mspec_cli = parse make args ["-n", "CC=clang", "CFLAGS=-g"]
        mres_cli = evaluate make mk_src with mspec_cli
        _ = assert equals (join with (snd mres_cli) " ; ") "clang -c main.c ; clang -c util.c ; clang -g -o app main.o util.o ; echo done clang" "make CLI variable overrides and ?= default"
        mk_cycle = "a: b" + nl + tab + "echo a" + nl + "b: c" + nl + tab + "echo b" + nl + "c: a" + nl + tab + "echo c" + nl
        mres_cyc = evaluate make mk_cycle with mspec1
        _ = assert true (not (fst mres_cyc)) "AC2: make rejects circular dependency graph"
        assert equals (str list nth (snd mres_cyc) 0) "make: Graph cycles through a" "AC2: make reports cycle target name"

fn :: test sh suite => ()
      test sh suite =
        nl = char from code 10
        _ = echo "=== 3. Testing BSD sh(1) Unit & Edge Cases (TS-PROC-002) ==="
        r1 = run shell script "X=hello; echo \"$X world\"" with []
        _ = assert equals (fst r1) ("hello world" + nl) "AC1: sh -c 'X=hello; echo \"$X world\"' output"
        _ = assert int equals (snd r1) 0 "AC1: sh exits with 0"
        r_sq = run shell script "X=hello; echo '$X world'" with []
        _ = assert equals (fst r_sq) ("$X world" + nl) "sh single quotes preserve literal $X"
        r_def = run shell script "echo $\{UNSET_VAR:-fallback_val\}" with []
        _ = assert equals (fst r_def) ("fallback_val" + nl) "sh $VAR:-default parameter expansion"
        r_pos = run shell script "echo \"$#:$1:$2\"" with ["alpha", "beta"]
        _ = assert equals (fst r_pos) ("2:alpha:beta" + nl) "sh positional parameters $#, $1, $2"
        r_and = run shell script "true && echo ok; false && echo nope; false || echo fallback" with []
        _ = assert equals (fst r_and) ("ok" + nl + "fallback" + nl) "AC2: sh && and || short-circuit lists"
        r_if = run shell script "X=5; if [ $X -gt 3 ]; then echo big; else echo small; fi" with []
        _ = assert equals (fst r_if) ("big" + nl) "AC2: sh if / [ -gt ] / then / else / fi"
        r_elif = run shell script "X=2; if [ $X -eq 1 ]; then echo one; elif [ $X -eq 2 ]; then echo two; else echo other; fi" with []
        _ = assert equals (fst r_elif) ("two" + nl) "AC2: sh if / elif / else / fi"
        r_for = run shell script "for item in a b c; do echo -n \"$item,\"; done" with []
        _ = assert equals (fst r_for) "a,b,c," "AC2: sh for item in a b c; do ...; done"
        r_while = run shell script "set a b; while [ $# -gt 0 ]; do echo -n $1; set; done" with []
        _ = assert equals (fst r_while) "a" "AC2: sh while loop with set and [ $# -gt 0 ]"
        r_sub = run shell script "V=outer; (V=inner; echo -n $V); echo \":$V\"" with []
        _ = assert equals (fst r_sub) ("inner:outer" + nl) "sh subshell ( ... ) isolates variable changes"
        r_exp = run shell script "export K=v1; echo -n $K; unset K; echo \":$K\"" with []
        _ = assert equals (fst r_exp) ("v1:" + nl) "sh export and unset built-ins"
        r_test = run shell script "test -n non_empty && test -z '' && [ abc = abc ] && [ abc != def ] && [ 10 -le 10 ] && echo ok_test" with []
        _ = assert equals (fst r_test) ("ok_test" + nl) "sh test / [ built-in string and numeric operators"
        r_exit = run shell script "echo before; exit 7; echo after" with []
        _ = assert equals (fst r_exit) ("before" + nl) "sh exit stops script execution immediately"
        assert int equals (snd r_exit) 7 "sh exit propagates exit code 7"

fn :: test proc e2e suite => ()
      test proc e2e suite =
        nl = char from code 10
        tab = char from code 9
        _ = echo "=== 4. Testing Compiled Native ELF Binaries target/apply, target/make, target/sh (E2E) ==="
        _ = shell exec "./target/apply -d -2 'diff %1 %2' a1 b1 a2 b2 > /tmp/ut_apply.out"
        _ = assert equals (read file "/tmp/ut_apply.out") ("diff a1 b1" + nl + "diff a2 b2" + nl) "E2E binary: target/apply -d -2"
        _ = shell exec "rm -f /tmp/ut_apply_live.txt && ./target/apply -2 'echo %1=%2 >> /tmp/ut_apply_live.txt' k1 v1 k2 v2"
        _ = assert equals (read file "/tmp/ut_apply_live.txt") ("k1=v1" + nl + "k2=v2" + nl) "E2E binary: target/apply live command execution"
        _ = write file "/tmp/ut_Makefile" ("OUT = /tmp/ut_mk_built.txt" + nl + "all: step1" + nl + tab + "echo final >> $(OUT)" + nl + "step1:" + nl + tab + "echo first > $(OUT)" + nl)
        rc_mk = shell exec "./target/make -f /tmp/ut_Makefile > /tmp/ut_make.out"
        _ = assert int equals rc_mk 0 "E2E binary: target/make executes DAG recipes"
        _ = assert equals (read file "/tmp/ut_mk_built.txt") ("first" + nl + "final" + nl) "E2E binary: target/make ran step1 before all"
        _ = write file "/tmp/ut_MakeCycle" ("x: y" + nl + tab + "echo x" + nl + "y: x" + nl + tab + "echo y" + nl)
        rc_cyc = shell exec "./target/make -n -f /tmp/ut_MakeCycle > /tmp/ut_mk_cyc.out"
        _ = assert true (rc_cyc != 0) "E2E binary: target/make exits non-zero on circular dependency"
        _ = shell exec "./target/sh -c 'X=hello; echo \"$X world\"' > /tmp/ut_sh1.out"
        _ = assert equals (read file "/tmp/ut_sh1.out") ("hello world" + nl) "E2E binary: target/sh -c variable assignment & expansion"
        _ = shell exec "./target/sh -c 'printf \"c\\na\\nb\\n\" | sort' > /tmp/ut_sh_pipe.out"
        _ = assert equals (read file "/tmp/ut_sh_pipe.out") ("a" + nl + "b" + nl + "c" + nl) "E2E binary: target/sh pipeline execution"
        _ = shell exec "./target/sh -c 'echo l1 > /tmp/ut_sh_red.txt; echo l2 >> /tmp/ut_sh_red.txt'"
        _ = assert equals (read file "/tmp/ut_sh_red.txt") ("l1" + nl + "l2" + nl) "E2E binary: target/sh > and >> redirections"
        _ = write file "/tmp/ut_script.sh" ("# sample script" + nl + "GREET=$\{1:-hola\}" + nl + "for n in 1 2 3; do" + nl + "  echo \"$GREET $n\"" + nl + "done" + nl)
        _ = shell exec "./target/sh /tmp/ut_script.sh buenas > /tmp/ut_sh_script.out"
        _ = assert equals (read file "/tmp/ut_sh_script.out") ("buenas 1" + nl + "buenas 2" + nl + "buenas 3" + nl) "E2E binary: target/sh script file execution with positional arg"
        _ = shell exec "./target/sh /tmp/ut_script.sh > /tmp/ut_sh_script_def.out"
        _ = assert equals (read file "/tmp/ut_sh_script_def.out") ("hola 1" + nl + "hola 2" + nl + "hola 3" + nl) "E2E binary: target/sh script file default fallback when $1 is unset"
        _ = shell exec "rm -f /tmp/ut_apply.out /tmp/ut_apply_live.txt /tmp/ut_Makefile /tmp/ut_mk_built.txt /tmp/ut_make.out /tmp/ut_MakeCycle /tmp/ut_mk_cyc.out /tmp/ut_sh1.out /tmp/ut_sh_pipe.out /tmp/ut_sh_red.txt /tmp/ut_script.sh /tmp/ut_sh_script.out /tmp/ut_sh_script_def.out"
        ()

fn :: test pal io suite => ()
      test pal io suite =
        _ = echo "=== 5. Testing Ridgun v0.9.5 PAL & posix/io Library Integration (TS-PROC-002) ==="
        _ = assert equals (null device) "/dev/null" "PAL posix: null device on Linux is /dev/null"
        _ = assert equals (path separator (sys os)) "/" "PAL posix: path separator on Linux is /"
        _ = assert true (process exists (sys getpid)) "PAL posix: process exists for current PID"
        _ = write file "/tmp/ut_pal_io.txt" "hello_pal"
        res_io = io read with timeout "/tmp/ut_pal_io.txt" 500
        _ = assert equals (format io result res_io) "ok:str:hello_pal" "PAL io: io read with timeout reads file within deadline"
        _ = append file "/tmp/ut_pal_io.txt" "_appended"
        _ = assert equals (read file "/tmp/ut_pal_io.txt") "hello_pal_appended" "PAL io: append file appends content cleanly"
        _ = assert true (is file "/tmp/ut_pal_io.txt") "PAL io: is file predicate"
        _ = sys unlink "/tmp/ut_pal_io.txt"
        assert true (not (file exists "/tmp/ut_pal_io.txt")) "PAL posix/io: sys unlink removes file"

fn :: test timeout and nc suite => ()
      test timeout and nc suite =
        nl = char from code 10
        tab = char from code 9
        _ = echo "=== 6. Testing BSD timeout(1) & nc(1) with io/socket Libraries (TS-PROC-003) ==="
        _ = assert int equals (parse duration ms "250ms") 250 "timeout duration parser: 250ms"
        _ = assert int equals (parse duration ms "2s") 2000 "timeout duration parser: 2s"
        _ = assert int equals (parse duration ms "1m") 60000 "timeout duration parser: 1m"
        _ = assert int equals (parse duration ms "1h") 3600000 "timeout duration parser: 1h"
        ts_flags = parse timeout args ["-k", "120ms", "-s", "TERM", "--preserve-status", "50ms", "sleep", "2"]
        _ = assert int equals (to_kill_ms ts_flags) 120 "AC1: timeout -k 120ms parses grace period"
        _ = assert str eq (to_sig ts_flags) "TERM" "AC1: timeout -s TERM parses signal name"
        _ = assert true (to_preserve ts_flags) "AC1: timeout --preserve-status sets preserve flag"
        to_ok = execute timeout (parse timeout args ["1s", "echo", "fast_ok"])
        _ = assert int equals (snd to_ok) 0 "AC1: timeout 1s echo fast_ok exits with 0"
        _ = assert str eq (fst to_ok) ("fast_ok" + nl) "AC1: timeout 1s echo fast_ok captures output"
        to_nonzero = execute timeout (parse timeout args ["2s", "sh", "-c", "exit 42"])
        _ = assert int equals (snd to_nonzero) 42 "AC1: command exiting 42 before deadline propagates exact exit status 42 (not 124)"
        to_exp = execute timeout (parse timeout args ["50ms", "sleep", "2"])
        _ = assert int equals (snd to_exp) 124 "AC1: timeout 50ms sleep 2 times out with status 124"
        nc_tr = nc loopback transfer "ping_ridgun" to "127.0.0.1" port 44123
        _ = assert true (fst nc_tr) "AC2: nc loopback transfer over socket.rglib succeeds"
        _ = assert str eq (snd nc_tr) "ping_ridgun" "AC2: nc loopback transfer preserves payload"
        multi_payload = "hdr:1" + tab + "val" + nl + "line2:ok" + nl
        nc_tr2 = nc loopback transfer multi_payload to "127.0.0.1" port 44125
        _ = assert true (fst nc_tr2) "AC2: nc loopback transfer handles multi-line + tab payload"
        _ = assert str eq (snd nc_tr2) multi_payload "AC2: nc loopback transfer preserves multi-line payload byte-for-byte"
        _ = sys tcp listen "127.0.0.1" 44124 4
        nc_z = execute nc (parse nc args ["-zv", "127.0.0.1", "44124"]) with ""
        _ = assert int equals (fst nc_z) 0 "AC2: nc -zv 127.0.0.1 44124 succeeds against active listener"
        nc_closed = execute nc (parse nc args ["-zv", "-w", "1", "127.0.0.1", "44199"]) with ""
        _ = assert false (check eq (fst nc_closed) 0) "AC2: nc -zv against closed port 44199 returns non-zero"
        _ = assert true (str contains (snd nc_closed) "failed") "AC2: nc -zv against closed port emits failure diagnostic"
        nc_bad_port = execute nc (parse nc args ["-z", "127.0.0.1", "0"]) with ""
        _ = assert int equals (fst nc_bad_port) 1 "AC2: nc rejects invalid port 0 with status 1"
        rc_to_bin = shell exec "./target/timeout 1s echo bin_ok > /tmp/ut_to_bin.out"
        _ = assert int equals rc_to_bin 0 "E2E binary: target/timeout 1s echo bin_ok exits 0"
        _ = assert str eq (read file "/tmp/ut_to_bin.out") ("bin_ok" + nl) "E2E binary: target/timeout emits stdout"
        _ = sys unlink "/tmp/ut_to_bin.out"
        rc_nc_bin = shell exec "./target/nc -zv 127.0.0.1 44124 > /tmp/ut_nc_bin.out"
        _ = assert int equals rc_nc_bin 0 "E2E binary: target/nc -zv 127.0.0.1 44124 exits 0"
        _ = sys unlink "/tmp/ut_nc_bin.out"
        ()

fn :: main => ()
      main =
        _ = test apply suite
        _ = test make suite
        _ = test sh suite
        _ = test proc e2e suite
        _ = test pal io suite
        _ = test timeout and nc suite
        echo "All 63 Tier 3 BSD Process, Build, PAL I/O & Socket (apply, make, sh, timeout, nc) assertions passed!"
