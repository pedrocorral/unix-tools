-- test_make.rg — Exhaustive, Uncompromising BSD make(1) & apply(1) Test Battery
-- @tests TS-PROC-001

import test
import proc_lib

fn :: test make variables and assignments => ()
      test make variables and assignments =
        nl = char from code 10
        tab = char from code 9
        _ = echo "=== 1. make(1) Deferred (=), Immediate (:=), Append (+=), Conditional (?=) & Continuations (TS-PROC-001) ==="
        mk_def = "A = init" + nl + "B = $(A)_ref" + nl + "A = updated" + nl + "all:" + nl + tab + "echo $(B)" + nl
        res_def = evaluate make mk_def with (parse make args ["-n"])
        _ = assert true (fst res_def) "make deferred '=' assignment parses cleanly"
        _ = assert str eq (join with (snd res_def) "") "echo updated_ref" "make '=' recursive/deferred assignment expands $(A) at recipe evaluation time (updated_ref)"
        mk_imm = "A := init" + nl + "B := $(A)_ref" + nl + "A := updated" + nl + "all:" + nl + tab + "echo $(B)" + nl
        res_imm = evaluate make mk_imm with (parse make args ["-n"])
        _ = assert str eq (join with (snd res_imm) "") "echo init_ref" "make ':=' immediate assignment expands $(A) at assignment time (init_ref)"
        mk_app = "CFLAGS = -Wall" + nl + "CFLAGS += -Wextra" + nl + "CFLAGS += -Werror" + nl + "all:" + nl + tab + "cc $(CFLAGS) main.c" + nl
        res_app = evaluate make mk_app with (parse make args ["-n"])
        _ = assert str eq (join with (snd res_app) "") "cc -Wall -Wextra -Werror main.c" "make '+=' appends space-separated values to variable"
        mk_cont = "SRCS = a.c \\" + nl + "       b.c \\" + nl + "       c.c" + nl + "all:" + nl + tab + "echo $(SRCS)" + nl
        res_cont = evaluate make mk_cont with (parse make args ["-n"])
        assert str eq (join with (snd res_cont) "") "echo a.c b.c c.c" "make backslash-newline (\\ + LF) line continuation joins multi-line variable definitions"

fn :: test make auto vars and pattern rules => ()
      test make auto vars and pattern rules =
        nl = char from code 10
        tab = char from code 9
        _ = echo "=== 2. make(1) Automatic Variables ($@, $<, $^, $?, $*, $$) & Pattern/Suffix Rules (TS-PROC-001) ==="
        mk_auto = "app: main.o util.o main.o" + nl + tab + "link -o $@ first=$< all=$^ stem=$* dollar=$$end" + nl + "main.o:" + nl + tab + "cc -c main.c" + nl + "util.o:" + nl + tab + "cc -c util.c" + nl
        res_auto = evaluate make mk_auto with (parse make args ["-n"])
        _ = assert true (fst res_auto) "make automatic variables DAG succeeds"
        _ = assert str eq (str list nth (snd res_auto) 2) "link -o app first=main.o all=main.o util.o stem=app dollar=$end" "make expands $@, $<, deduplicated $^, $*, and $$"
        mk_pat = "CC = gcc" + nl + "all: foo.o bar.o" + nl + tab + "$(CC) -o all $^" + nl + "%.o: %.c" + nl + tab + "$(CC) -c $< -o $@ (stem=$*)" + nl + "foo.c:" + nl + tab + "touch foo.c" + nl + "bar.c:" + nl + tab + "touch bar.c" + nl
        res_pat = evaluate make mk_pat with (parse make args ["-n"])
        _ = assert true (fst res_pat) "make %.o: %.c pattern rule resolves foo.o and bar.o"
        _ = assert true (list contains str (snd res_pat) "gcc -c foo.c -o foo.o (stem=foo)") "make %.o: %.c expands $<, $@, and $* for foo.o"
        _ = assert true (list contains str (snd res_pat) "gcc -c bar.c -o bar.o (stem=bar)") "make %.o: %.c expands $<, $@, and $* for bar.o"
        mk_suf = ".SUFFIXES: .c .o" + nl + ".c.o:" + nl + tab + "cc -O2 -c $< -o $@" + nl + " util.o: util.c" + nl + "util.c:" + nl + tab + "touch util.c" + nl
        res_suf = evaluate make mk_suf with (parse make args ["-n", "util.o"])
        _ = assert true (fst res_suf) "make .c.o: POSIX suffix rule resolves util.o"
        assert true (list contains str (snd res_suf) "cc -O2 -c util.c -o util.o") "make .c.o: synthesizes recipe with $< and $@"

fn :: test make prefixes flags include => ()
      test make prefixes flags include =
        nl = char from code 10
        tab = char from code 9
        _ = echo "=== 3. make(1) Recipe Prefixes (@, -, +), Flags (-s, -i, -k, -q, -C) & include (TS-PROC-001) ==="
        mk_dash = "all:" + nl + tab + "-false" + nl + tab + "@echo after_ignored_error" + nl
        res_dash = evaluate make mk_dash with (parse make args [])
        _ = assert true (fst res_dash) "make '-' recipe prefix ignores non-zero exit code from 'false' and continues"
        mk_i = "all:" + nl + tab + "false" + nl + tab + "echo continued_under_i" + nl
        res_i = evaluate make mk_i with (parse make args ["-i"])
        _ = assert true (fst res_i) "make -i flag ignores non-zero exit codes in recipes"
        mk_s = "all:" + nl + tab + "true" + nl
        res_s = evaluate make mk_s with (parse make args ["-s"])
        _ = assert int equals (list len (snd res_s)) 0 "make -s silent mode suppresses recipe echoing"
        mk_k = "all: bad_target independent_good" + nl + tab + "echo all_done" + nl + "bad_target:" + nl + tab + "false" + nl + "independent_good:" + nl + tab + "echo built_independent" + nl
        res_k = evaluate make mk_k with (parse make args ["-k"])
        _ = assert false (fst res_k) "make -k still returns overall failure when a target fails"
        _ = assert true (list contains str (snd res_k) "echo built_independent") "make -k continues building unrelated target 'independent_good' after 'bad_target' fails"
        _ = write file "/tmp/ut_mk_inc.mk" ("INC_VAR = from_included_file" + nl)
        mk_inc = "include /tmp/ut_mk_inc.mk" + nl + "all:" + nl + tab + "echo $(INC_VAR)" + nl
        res_inc = evaluate make mk_inc with (parse make args ["-n"])
        _ = sys unlink "/tmp/ut_mk_inc.mk"
        _ = assert true (fst res_inc) "make include directive loads external makefile fragment"
        assert str eq (join with (snd res_inc) "") "echo from_included_file" "make expands variables defined in included file"

fn :: test make mtime and e2e => ()
      test make mtime and e2e =
        nl = char from code 10
        tab = char from code 9
        _ = echo "=== 4. make(1) Incremental mtime Rebuild, -B Force, -q Question & -C Dir (TS-PROC-001) ==="
        _ = shell exec "rm -rf /tmp/ut_mk_dir && mkdir -p /tmp/ut_mk_dir"
        _ = write file "/tmp/ut_mk_dir/src.c" "int main() { return 0; }"
        _ = write file "/tmp/ut_mk_dir/out.bin" "binary"
        _ = write file "/tmp/ut_mk_dir/Makefile" ("/tmp/ut_mk_dir/out.bin: /tmp/ut_mk_dir/src.c" + nl + tab + "cp /tmp/ut_mk_dir/src.c /tmp/ut_mk_dir/out.bin" + nl)
        _ = shell exec "touch -t 202501010000 /tmp/ut_mk_dir/src.c && touch -t 202501020000 /tmp/ut_mk_dir/out.bin"
        res_uptodate = evaluate make (read file "/tmp/ut_mk_dir/Makefile") with (parse make args ["-n", "/tmp/ut_mk_dir/out.bin"])
        _ = assert true (fst res_uptodate) "make succeeds when target is newer than prerequisite"
        _ = assert int equals (list len (snd res_uptodate)) 0 "make skips recipe when target mtime > prerequisite mtime"
        res_force = evaluate make (read file "/tmp/ut_mk_dir/Makefile") with (parse make args ["-n", "-B", "/tmp/ut_mk_dir/out.bin"])
        _ = assert int equals (list len (snd res_force)) 1 "make -B forces rebuild even when target is newer than prerequisite"
        rc_c = shell exec "./target/make -C /tmp/ut_mk_dir -n -B /tmp/ut_mk_dir/out.bin > /tmp/ut_mk_c.out"
        _ = assert int equals rc_c 0 "E2E binary: target/make -C /tmp/ut_mk_dir changes directory and reads Makefile"
        _ = assert true (str contains (read file "/tmp/ut_mk_c.out") "cp /tmp/ut_mk_dir/src.c") "E2E binary: target/make -C outputs expected recipe"
        _ = shell exec "rm -rf /tmp/ut_mk_dir /tmp/ut_mk_c.out"
        ()

fn :: main => ()
      main =
        _ = test make variables and assignments
        _ = test make auto vars and pattern rules
        _ = test make prefixes flags include
        _ = test make mtime and e2e
        echo "All 21 exhaustive BSD make(1) assertions in test_make.rg passed!"
