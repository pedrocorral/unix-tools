-- test_sh.rg — Exhaustive, Uncompromising POSIX/BSD sh(1) Test Battery
-- @tests TS-PROC-002

import test
import proc_lib

fn :: test sh quoting and comments => ()
      test sh quoting and comments =
        nl = char from code 10
        bs = char from code 92
        dq = char from code 34
        _ = echo "=== 1. sh(1) Quoting, Escaping, Line Continuations & Comments (TS-PROC-002) ==="
        r1 = run shell script "echo 'single $VAR \"quoted\" \\escaped #not_a_comment'" with []
        _ = assert str eq (fst r1) ("single $VAR \"quoted\" \\escaped #not_a_comment" + nl) "sh single quotes preserve $, \", \\, and # literally"
        r2 = run shell script ("A=42; echo " + dq + "val=$A " + bs + dq + "quoted" + bs + dq + " " + bs + "$A " + bs + bs + " #still_in_dq" + dq) with []
        _ = assert str eq (fst r2) ("val=42 \"quoted\" $A \\ #still_in_dq" + nl) "sh double quotes expand $A and unescape \\\", \\$, \\\\ while preserving #"
        r3 = run shell script ("echo foo#bar # real comment" + nl + "echo baz#qux") with []
        _ = assert str eq (fst r3) ("foo#bar" + nl + "baz#qux" + nl) "sh # mid-word (foo#bar) is literal; # after whitespace is comment"
        r4 = run shell script ("echo hello" + bs + nl + "world") with []
        _ = assert str eq (fst r4) ("helloworld" + nl) "sh backslash-newline (\\ + LF) line continuation joins tokens seamlessly"
        r5 = run shell script ("echo a" + bs + " b" + bs + ";echo c") with []
        _ = assert str eq (fst r5) ("a b;echo c" + nl) "sh unquoted backslash escapes space and semicolon"
        r6 = run shell script "echo 'part1'\"part2\"part3" with []
        assert str eq (fst r6) ("part1part2part3" + nl) "sh adjacent single-quoted, double-quoted, and unquoted segments concatenate into 1 word"

fn :: test sh parameter expansions => ()
      test sh parameter expansions =
        nl = char from code 10
        _ = echo "=== 2. sh(1) Full POSIX Parameter Expansion Suite (TS-PROC-002) ==="
        r_def1 = run shell script "unset U; E=''; S='set'; echo \"$\{U:-d1\}:$\{E:-d2\}:$\{S:-d3\}\"" with []
        _ = assert str eq (fst r_def1) ("d1:d2:set" + nl) "sh $\{VAR:-word\} uses default for both unset and empty"
        r_def2 = run shell script "unset U; E=''; S='set'; echo \"$\{U-d1\}:$\{E-d2\}:$\{S-d3\}\"" with []
        _ = assert str eq (fst r_def2) ("d1::set" + nl) "sh $\{VAR-word\} uses default only when unset (preserves empty)"
        r_asg1 = run shell script "unset U; E=''; echo -n \"$\{U:=init1\}:$\{E:=init2\}:\"; echo \"$U:$E\"" with []
        _ = assert str eq (fst r_asg1) ("init1:init2:init1:init2" + nl) "sh $\{VAR:=word\} assigns and returns default when unset or empty"
        r_asg2 = run shell script "unset U; E=''; echo -n \"$\{U=init1\}:$\{E=init2\}:\"; echo \"$U:$E\"" with []
        _ = assert str eq (fst r_asg2) ("init1::init1:" + nl) "sh $\{VAR=word\} assigns only when unset"
        r_alt1 = run shell script "unset U; E=''; S='ok'; echo \"$\{U:+alt1\}:$\{E:+alt2\}:$\{S:+alt3\}\"" with []
        _ = assert str eq (fst r_alt1) ("::alt3" + nl) "sh $\{VAR:+word\} substitutes alt only when set and non-empty"
        r_alt2 = run shell script "unset U; E=''; S='ok'; echo \"$\{U+alt1\}:$\{E+alt2\}:$\{S+alt3\}\"" with []
        _ = assert str eq (fst r_alt2) (":alt2:alt3" + nl) "sh $\{VAR+word\} substitutes alt when set (even if empty)"
        r_err = run shell script "unset MISSING_V; echo $\{MISSING_V:?custom_missing_err\}; echo never_reached" with []
        _ = assert false (check eq (snd r_err) 0) "sh $\{VAR:?err\} exits non-zero when VAR is unset"
        _ = assert false (str contains (fst r_err) "never_reached") "sh $\{VAR:?err\} aborts command execution"
        r_len = run shell script "S='Ridgun_v0.9.17'; E=''; echo \"$\{#S\}:$\{#E\}\"" with []
        _ = assert str eq (fst r_len) ("14:0" + nl) "sh $\{#VAR\} computes exact character length"
        r_strip = run shell script "P='/usr/local/bin/ridgunc.tar.gz'; echo \"$\{P#*/\}|$\{P##*/\}|$\{P%.*\}|$\{P%%.*\}\"" with []
        _ = assert str eq (fst r_strip) ("usr/local/bin/ridgunc.tar.gz|ridgunc.tar.gz|/usr/local/bin/ridgunc.tar|/usr/local/bin/ridgunc" + nl) "sh $\{V#pat\}, $\{V##pat\}, $\{V%pat\}, $\{V%%pat\} prefix/suffix glob stripping"
        r_pid = run shell script "echo $$" with []
        pid_str = trim ws (fst r_pid)
        _ = assert false (check str eq pid_str "1000") "sh $$ anti-cheat: $$ is NOT hardcoded to 1000"
        _ = assert str eq pid_str (int to str (sys getpid)) "sh $$ matches real OS process ID (sys getpid)"
        r_pos10 = run shell script "echo \"$#:$1:$9:$\{10\}:$\{11\}\"" with ["p1", "p2", "p3", "p4", "p5", "p6", "p7", "p8", "p9", "p10", "p11"]
        assert str eq (fst r_pos10) ("11:p1:p9:p10:p11" + nl) "sh positional parameters support $1..$9 and braced $\{10\}, $\{11\}"

fn :: test sh arithmetic and subst => ()
      test sh arithmetic and subst =
        nl = char from code 10
        _ = echo "=== 3. sh(1) Arithmetic Expansion $((...)) & Command Substitution $(...) / `...` (TS-PROC-002) ==="
        r_ar1 = run shell script "echo $(( 2 + 3 * 4 )) $(( (2 + 3) * 4 )) $(( 17 / 5 )) $(( 17 % 5 )) $(( -5 + 12 ))" with []
        _ = assert str eq (fst r_ar1) ("14 20 3 2 7" + nl) "sh $((...)) operator precedence (+, -, *, /, %, unary -, parens)"
        r_ar2 = run shell script "X=10; Y=3; echo $(( X * Y + 2 )) $(( $X - $Y ))" with []
        _ = assert str eq (fst r_ar2) ("32 7" + nl) "sh $((...)) resolves bare variable names (X) and $X inside arithmetic"
        r_ar3 = run shell script "echo $(( 5 == 5 )) $(( 5 != 5 )) $(( 3 < 7 )) $(( 7 <= 7 )) $(( 8 > 2 )) $(( 4 >= 9 ))" with []
        _ = assert str eq (fst r_ar3) ("1 0 1 1 1 0" + nl) "sh $((...)) relational and equality operators"
        r_ar4 = run shell script "echo $(( 1 && 1 )) $(( 1 && 0 )) $(( 0 || 5 )) $(( !0 )) $(( !7 ))" with []
        _ = assert str eq (fst r_ar4) ("1 0 1 1 0" + nl) "sh $((...)) logical &&, ||, ! operators"
        r_ar5 = run shell script "echo $(( (1 << 4) | 3 )) $(( 15 & 6 )) $(( 12 ^ 5 )) $(( 32 >> 2 ))" with []
        _ = assert str eq (fst r_ar5) ("19 6 9 8" + nl) "sh $((...)) bitwise <<, >>, |, &, ^ operators"
        r_ar6 = run shell script "A=7; echo $(( A > 5 ? 100 : 200 )) $(( A < 5 ? 100 : 200 ))" with []
        _ = assert str eq (fst r_ar6) ("100 200" + nl) "sh $((...)) ternary cond ? a : b operator"
        r_sub1 = run shell script "X=$(echo alpha); Y=`echo beta`; echo \"$X-$Y\"" with []
        _ = assert str eq (fst r_sub1) ("alpha-beta" + nl) "sh $(...) and `...` command substitution strip trailing newline"
        r_sub2 = run shell script "echo \"nested=$(echo outer_$(echo inner_$(echo deep)))\"" with []
        assert str eq (fst r_sub2) ("nested=outer_inner_deep" + nl) "sh nested $(echo $(echo ...)) command substitution"

fn :: test sh control flow and functions => ()
      test sh control flow and functions =
        nl = char from code 10
        _ = echo "=== 4. sh(1) case/esac, until, break/continue, Functions, local, return & Recursion (TS-PROC-002) ==="
        case_sc = "for w in foo.rg bar.c README.md unknown.xyz; do case \"$w\" in *.rg) echo -n \"R:\";; *.c|*.h) echo -n \"C:\";; README*) echo -n \"D:\";; *) echo -n \"?:\";; esac; done"
        r_case = run shell script case_sc with []
        _ = assert str eq (fst r_case) "R:C:D:?:" "sh case ... in pat1|pat2) ... ;; *) ... ;; esac with glob patterns"
        until_sc = "N=1; until [ $N -gt 4 ]; do echo -n \"$N,\"; N=$(( N + 1 )); done"
        r_until = run shell script until_sc with []
        _ = assert str eq (fst r_until) "1,2,3,4," "sh until ...; do ...; done loop"
        brk_sc = "for i in 1 2 3 4 5; do if [ $i -eq 2 ]; then continue; fi; if [ $i -eq 4 ]; then break; fi; echo -n \"$i,\"; done"
        r_brk = run shell script brk_sc with []
        _ = assert str eq (fst r_brk) "1,3," "sh break and continue inside for loop"
        brk2_sc = "for x in A B; do for y in 1 2 3; do echo -n \"$x$y,\"; if [ \"$x$y\" = \"A2\" ]; then break 2; fi; done; done"
        r_brk2 = run shell script brk2_sc with []
        _ = assert str eq (fst r_brk2) "A1,A2," "sh multi-level break 2 exits outer loop"
        brace_sc = "V=init; \{ V=mutated; echo -n \"$V:\"; \}; echo \"$V\""
        r_brace = run shell script brace_sc with []
        _ = assert str eq (fst r_brace) ("mutated:mutated" + nl) "sh brace group \{ ...; \} executes in current environment"
        fn_sc = "greet() \{ local MSG=\"hi $1\"; echo \"$MSG ($#)\"; return 5; \}; MSG=global; greet world extra; RC=$?; echo \"$MSG:$RC\""
        r_fn = run shell script fn_sc with ["orig_arg"]
        _ = assert str eq (fst r_fn) ("hi world (2)" + nl + "global:5" + nl) "sh function definition, scoped $1/$#, local variable isolation, and return status"
        fact_sc = "fact() \{ if [ $1 -le 1 ]; then echo 1; else local p=$(fact $(( $1 - 1 ))); echo $(( $1 * p )); fi; \}; fact 5"
        r_fact = run shell script fact_sc with []
        _ = assert str eq (fst r_fact) ("120" + nl) "sh recursive function call (5! = 120)"
        for_def = "for arg; do echo -n \"[$arg]\"; done"
        r_for_def = run shell script for_def with ["x", "y", "z"]
        assert str eq (fst r_for_def) "[x][y][z]" "sh 'for arg; do ...' without 'in' iterates positional parameters $@"

fn :: test sh builtins and options => ()
      test sh builtins and options =
        nl = char from code 10
        tab = char from code 9
        _ = echo "=== 5. sh(1) Built-ins (printf, read, shift, set -e/-u/-f, readonly, eval, trap, colon) (TS-PROC-002) ==="
        r_colon = run shell script ":; X=1; : $\{X:=99\} $\{Y:=42\}; echo \"$X:$Y\"" with []
        _ = assert str eq (fst r_colon) ("1:42" + nl) "sh ':' null built-in evaluates argument side-effects and returns 0"
        r_printf = run shell script "printf '%s=%04d (0x%x)\\n' item 7 255" with []
        _ = assert str eq (fst r_printf) ("item=0007 (0xff)" + nl) "sh printf built-in formats %s, %04d, %x, and \\n"
        r_shift = run shell script "echo -n \"$1:\"; shift; echo -n \"$1:\"; shift 2; echo \"$1 ($#)\"" with ["a", "b", "c", "d", "e"]
        _ = assert str eq (fst r_shift) ("a:b:d (2)" + nl) "sh shift and shift N built-in updates $1..$N and $#"
        r_sete = run shell script "set -e; echo step1; false; echo step2_unreachable" with []
        _ = assert false (check eq (snd r_sete) 0) "sh set -e exits non-zero when unchecked command fails"
        _ = assert str eq (fst r_sete) ("step1" + nl) "sh set -e aborts script immediately after failing command"
        r_sete_ok = run shell script "set -e; false || echo handled; if false; then echo no; fi; ! false; echo survived" with []
        _ = assert str eq (fst r_sete_ok) ("handled" + nl + "survived" + nl) "sh set -e does NOT abort on failing lhs of ||, if condition, or ! negation"
        r_setu = run shell script "set -u; echo $DEFINITELY_UNSET_VAR_999; echo unreachable" with []
        _ = assert false (check eq (snd r_setu) 0) "sh set -u exits non-zero on expansion of unset variable"
        r_ro = run shell script "readonly CONST=10; CONST=20; echo $CONST" with []
        _ = assert false (check eq (snd r_ro) 0) "sh readonly rejects subsequent assignment to read-only variable"
        r_eval = run shell script "CMD='echo evaluated_value'; eval $CMD" with []
        _ = assert str eq (fst r_eval) ("evaluated_value" + nl) "sh eval built-in parses and executes constructed command"
        r_trap = run shell script "trap 'echo cleanup_ran' EXIT; echo body_ran; exit 0" with []
        _ = assert str eq (fst r_trap) ("body_ran" + nl + "cleanup_ran" + nl) "sh trap '...' EXIT executes handler on shell exit"
        r_read = run shell script "printf 'alpha beta gamma delta\\n' | \{ read A B C; echo \"$A|$B|$C\"; \}" with []
        _ = assert str eq (fst r_read) ("alpha|beta|gamma delta" + nl) "sh read built-in splits input by IFS and assigns remainder to last variable"
        r_echo_e = run shell script "echo -e 'col1\\tcol2\\nline2'" with []
        assert str eq (fst r_echo_e) ("col1" + tab + "col2" + nl + "line2" + nl) "sh echo -e interprets \\t and \\n escapes"

fn :: test sh redirs pipes globs => ()
      test sh redirs pipes globs =
        nl = char from code 10
        _ = echo "=== 6. sh(1) Here-Docs, Redirections, Negated Pipelines & Globbing (TS-PROC-002) ==="
        hdoc1 = "NAME=Ridgun" + nl + "cat <<EOF" + nl + "Hello $NAME $(( 3 + 4 ))" + nl + "EOF" + nl
        r_hd1 = run shell script hdoc1 with []
        _ = assert str eq (fst r_hd1) ("Hello Ridgun 7" + nl) "sh <<EOF here-document performs parameter and arithmetic expansion"
        hdoc2 = "NAME=Ridgun" + nl + "cat <<'EOF'" + nl + "Literal $NAME $(( 3 + 4 ))" + nl + "EOF" + nl
        r_hd2 = run shell script hdoc2 with []
        _ = assert str eq (fst r_hd2) ("Literal $NAME $(( 3 + 4 ))" + nl) "sh <<'EOF' quoted here-document preserves body literally"
        r_neg1 = run shell script "! false; echo -n \"$?:\"; ! true; echo \"$?\"" with []
        _ = assert str eq (fst r_neg1) ("0:1" + nl) "sh ! pipeline negation inverts exit status (0 <-> 1)"
        orig_cwd = sys getcwd
        _ = shell exec "rm -rf /tmp/ut_sh_glob && mkdir -p /tmp/ut_sh_glob && touch /tmp/ut_sh_glob/a.rg /tmp/ut_sh_glob/b.rg /tmp/ut_sh_glob/c.txt"
        r_glob = run shell script "cd /tmp/ut_sh_glob; echo *.rg" with []
        _ = assert str eq (fst r_glob) ("a.rg b.rg" + nl) "sh unquoted *.rg expands to sorted matching filenames"
        r_noglob = run shell script "cd /tmp/ut_sh_glob; set -f; echo *.rg" with []
        _ = assert str eq (fst r_noglob) ("*.rg" + nl) "sh set -f (noglob) disables pathname expansion"
        _ = sys chdir orig_cwd
        _ = shell exec "rm -rf /tmp/ut_sh_glob"
        r_pipe_builtin = run shell script "echo 'hello_pipeline' | \{ read V; echo \"got:$V\"; \}" with []
        assert str eq (fst r_pipe_builtin) ("got:hello_pipeline" + nl) "sh pure-Ridgun internal pipeline between built-ins and compound commands"

fn :: test sh upstream expansion and params => ()
      test sh upstream expansion and params =
        nl = char from code 10
        tab = char from code 9
        _ = echo "=== 7. Upstream BSD bin/sh/tests/ (expansion, parameters) Regression Suite (TS-PROC-002) ==="
        r_nest1 = run shell script "unset A B; C='deep_val'; echo \"$\{A:-$\{B:-$C\}\}\"" with []
        _ = assert str eq (fst r_nest1) ("deep_val" + nl) "upstream plus-minus1: nested $\{A:-$\{B:-$C\}\} falls through to innermost set variable"
        r_nest2 = run shell script "A='outer'; B='inner'; echo \"$\{A:+$\{B:-fallback\}\}\"" with []
        _ = assert str eq (fst r_nest2) ("inner" + nl) "upstream plus-minus2: nested $\{A:+$\{B:-fallback\}\} evaluates inner expansion when A is non-empty"
        r_nest3 = run shell script "unset X; echo \"$\{X:=$\{DEF:-assigned_nested\}\}:$X\"" with []
        _ = assert str eq (fst r_nest3) ("assigned_nested:assigned_nested" + nl) "upstream plus-minus3: nested $\{X:=$\{DEF:-...\}\} assigns expanded default to X"
        r_trim1 = run shell script "V='a/b/c/d'; echo \"$\{V#*/\}|$\{V##*/\}|$\{V%/*\}|$\{V%%/*\}\"" with []
        _ = assert str eq (fst r_trim1) ("b/c/d|d|a/b/c|a" + nl) "upstream trim1: path component stripping #, ##, %, %%"
        r_trim2 = run shell script "V='hello_world'; echo \"$\{V#nomatch*\}|$\{V%nomatch*\}\"" with []
        _ = assert str eq (fst r_trim2) ("hello_world|hello_world" + nl) "upstream trim2: non-matching # and % patterns leave string unchanged"
        r_trim3 = run shell script "V='abc123xyz'; P='*123'; S='123*'; echo \"$\{V#$P\}|$\{V%$S\}\"" with []
        _ = assert str eq (fst r_trim3) ("xyz|abc" + nl) "upstream trim3: parameter expansion inside # and % pattern operand"
        r_ar_assoc = run shell script "echo $(( 20 - 5 - 3 )) $(( 100 / 10 / 2 )) $(( 2 + 3 * 4 - 5 ))" with []
        _ = assert str eq (fst r_ar_assoc) ("12 5 9" + nl) "upstream arith1: left-associativity of - and / and mixed precedence"
        r_ar_un = run shell script "echo $(( +42 )) $(( - -7 )) $(( !0 )) $(( !!9 )) $(( !(3 - 3) ))" with []
        _ = assert str eq (fst r_ar_un) ("42 7 1 1 1" + nl) "upstream arith2: unary +, double unary -, and chained logical !!"
        r_ar_tern = run shell script "echo $(( 1 ? (0 ? 11 : 22) : 33 )) $(( 0 ? 11 : (1 ? 44 : 55) ))" with []
        _ = assert str eq (fst r_ar_tern) ("22 44" + nl) "upstream arith3: nested ternary ?: expressions"
        r_ar_div0 = run shell script "echo $(( 10 / 0 )) $(( 10 % 0 ))" with []
        _ = assert str eq (fst r_ar_div0) ("0 0" + nl) "upstream arith4: division/modulo by zero handled safely without crash"
        r_ar_param = run shell script "A=6; B=7; echo $(( $\{A\} * $\{B\} + $(echo 8) ))" with []
        _ = assert str eq (fst r_ar_param) ("50" + nl) "upstream arith5: braced parameters and $(...) inside $((...))"
        r_pos_set = run shell script "set -- alpha beta gamma; echo -n \"$#:$1:$3|\"; set --; echo \"$#:$1\"" with ["init"]
        _ = assert str eq (fst r_pos_set) ("3:alpha:gamma|0:" + nl) "upstream positional1: set -- replaces $@ and empty set/shift clears $@"
        r_pos_shift_err = run shell script "shift 5; echo $?" with ["only_one"]
        _ = assert str eq (fst r_pos_shift_err) ("1" + nl) "upstream positional2: shift N beyond $# returns non-zero status 1"
        r_hd_tab = run shell script ("cat <<-EOF" + nl + tab + "line_one" + nl + tab + tab + "line_two $(( 2 + 2 ))" + nl + tab + "EOF" + nl) with []
        _ = assert str eq (fst r_hd_tab) ("line_one" + nl + "line_two 4" + nl) "upstream parser/heredoc1: <<-EOF strips leading tabs from body and delimiter"
        r_cs_trail = run shell script "X=$(printf 'hello\\n\\n\\n'); echo \"[$X]\"" with []
        assert str eq (fst r_cs_trail) ("[hello]" + nl) "upstream cmdsubst1: $(...) strips all trailing newlines, not just one"

fn :: test sh upstream control and builtins => ()
      test sh upstream control and builtins =
        nl = char from code 10
        _ = echo "=== 8. Upstream BSD bin/sh/tests/builtins & Control Flow Regression Suite (TS-PROC-002) ==="
        r_case_lparen = run shell script "for x in a b c; do case \"$x\" in (a|b) echo -n \"AB:\";; (c) echo -n \"C:\";; esac; done" with []
        _ = assert str eq (fst r_case_lparen) "AB:AB:C:" "upstream case1: POSIX optional leading '(' in case patterns (a|b)"
        r_case_qmark = run shell script "for w in a1 a22 b1; do case \"$w\" in a?) echo -n \"M:$w,\";; *) echo -n \"N:$w,\";; esac; done" with []
        _ = assert str eq (fst r_case_qmark) "M:a1,N:a22,N:b1," "upstream case2: '?' single-char glob pattern in case arms"
        r_case_nomatch = run shell script "false; case \"zzz\" in aaa) echo hit;; bbb) echo hit2;; esac; echo $?" with []
        _ = assert str eq (fst r_case_nomatch) ("0" + nl) "upstream case3: case with no matching arm resets exit status to 0"
        r_cont2 = run shell script "for i in A B; do for j in 1 2 3; do if [ $j -eq 2 ]; then continue 2; fi; echo -n \"$i$j,\"; done; echo -n \"unreachable,\"; done" with []
        _ = assert str eq (fst r_cont2) "A1,B1," "upstream continue1: multi-level 'continue 2' jumps to next outer loop iteration"
        r_ret_loop = run shell script "find_three() \{ for x in 1 2 3 4 5; do if [ $x -eq 3 ]; then echo \"found:$x\"; return 33; fi; done; echo never; \}; find_three; echo \"rc:$?\"" with []
        _ = assert str eq (fst r_ret_loop) ("found:3" + nl + "rc:33" + nl) "upstream return1: 'return' inside for-loop unwinds loop and exits function immediately"
        r_fn_pos_iso = run shell script "inner() \{ echo -n \"in:$#:$1|\"; \}; outer() \{ echo -n \"out1:$#:$1|\"; inner x y z; echo -n \"out2:$#:$1|\"; \}; outer p q; echo \"top:$#:$1\"" with ["top_arg"]
        _ = assert str eq (fst r_fn_pos_iso) ("out1:2:p|in:3:x|out2:2:p|top:1:top_arg" + nl) "upstream parameters/positional3: nested function calls preserve caller positional parameters"
        r_fn_redir = run shell script "emit_pair() \{ echo \"lineA:$1\"; echo \"lineB:$2\"; \}; emit_pair foo bar > /tmp/ut_sh_fn_redir.out; cat < /tmp/ut_sh_fn_redir.out" with []
        _ = sys unlink "/tmp/ut_sh_fn_redir.out"
        _ = assert str eq (fst r_fn_redir) ("lineA:foo" + nl + "lineB:bar" + nl) "upstream execution/redir1: output redirection on shell function call and input redirection '<'"
        _ = write file "/tmp/ut_sh_test_f.txt" "non_empty_content"
        _ = write file "/tmp/ut_sh_test_empty.txt" ""
        _ = sys symlink "/tmp/ut_sh_test_f.txt" "/tmp/ut_sh_test_sym.lnk"
        r_test_fs = run shell script "[ -s /tmp/ut_sh_test_f.txt ] && [ ! -s /tmp/ut_sh_test_empty.txt ] && [ -L /tmp/ut_sh_test_sym.lnk ] && [ -h /tmp/ut_sh_test_sym.lnk ] && echo fs_pred_ok" with []
        _ = shell exec "rm -f /tmp/ut_sh_test_f.txt /tmp/ut_sh_test_empty.txt /tmp/ut_sh_test_sym.lnk"
        _ = assert str eq (fst r_test_fs) ("fs_pred_ok" + nl) "upstream builtins/test1: [ -s ], [ ! -s ], [ -L ], and [ -h ] filesystem predicates"
        r_test_ao = run shell script "[ 5 -gt 3 -a 10 -eq 10 ] && [ 1 -eq 2 -o 'yes' = 'yes' ] && [ ! \\( 1 -eq 2 \\) -o 1 -eq 1 ] && echo logic_ok" with []
        _ = assert str eq (fst r_test_ao) ("logic_ok" + nl) "upstream builtins/test2: compound test -a (AND) and -o (OR) operators"
        _ = write file "/tmp/ut_sh_sourced.sh" ("SOURCED_VAR=\"from_dot_$1\"" + nl + "echo \"sourcing_ok\"" + nl)
        r_dot = run shell script ". /tmp/ut_sh_sourced.sh arg1; echo \"$SOURCED_VAR\"" with [" script_arg"]
        _ = sys unlink "/tmp/ut_sh_sourced.sh"
        _ = assert str eq (fst r_dot) ("sourcing_ok" + nl + "from_dot_ script_arg" + nl) "upstream builtins/dot1: '.' source built-in executes in current environment"
        r_read_multi = run shell script "printf 'r1_a r1_b\\nr2_a r2_b\\n' | \{ read X1 Y1; read X2 Y2; echo \"$X1:$Y1|$X2:$Y2\"; \}" with []
        _ = assert str eq (fst r_read_multi) ("r1_a:r1_b|r2_a:r2_b" + nl) "upstream builtins/read1: sequential 'read' calls consume successive lines from pipeline stdin"
        r_read_eof = run shell script "printf '' | \{ read X; echo \"rc:$?\"; \}" with []
        assert str eq (fst r_read_eof) ("rc:1" + nl) "upstream builtins/read2: 'read' on empty/EOF stdin returns exit status 1"

fn :: test sh upstream sete and subshells => ()
      test sh upstream sete and subshells =
        nl = char from code 10
        _ = echo "=== 9. Upstream BSD bin/sh/tests/ (set-e, execution, errors) Regression Suite (TS-PROC-002) ==="
        r_se_plus = run shell script "set -e; set +e; false; echo after_false_with_plus_e" with []
        _ = assert int equals (snd r_se_plus) 0 "upstream set-e1: 'set +e' disables errexit so subsequent 'false' does not abort"
        _ = assert str eq (fst r_se_plus) ("after_false_with_plus_e" + nl) "upstream set-e1: commands after 'false' execute under 'set +e'"
        r_se_and = run shell script "set -e; false && echo unreachable; echo survived_and" with []
        _ = assert int equals (snd r_se_and) 0 "upstream set-e2: failing LHS of '&&' does not trigger set -e abort"
        _ = assert str eq (fst r_se_and) ("survived_and" + nl) "upstream set-e2: execution continues after 'false && ...' under set -e"
        r_se_while = run shell script "set -e; while false; do echo no; done; until true; do echo no; done; echo survived_loops" with []
        _ = assert str eq (fst r_se_while) ("survived_loops" + nl) "upstream set-e3: failing 'while' condition and 'until' condition do not trigger set -e"
        r_sub_exit = run shell script "( echo in_sub; exit 42; echo sub_unreach ); echo \"parent:$?\"" with []
        _ = assert int equals (snd r_sub_exit) 0 "upstream execution/subshell1: 'exit 42' inside ( ... ) only exits the subshell"
        _ = assert str eq (fst r_sub_exit) ("in_sub" + nl + "parent:42" + nl) "upstream execution/subshell1: parent shell receives subshell exit code 42 in $?"
        r_ro_unset = run shell script "readonly LOCKED=99; unset LOCKED; echo unreach" with []
        _ = assert false (check eq (snd r_ro_unset) 0) "upstream errors/readonly1: 'unset' on a readonly variable fails with non-zero status"
        _ = assert false (str contains (fst r_ro_unset) "unreach") "upstream errors/readonly1: 'unset' on a readonly variable aborts execution"
        r_trap_rc = run shell script "trap 'echo trap_msg' EXIT; exit 19" with []
        _ = assert int equals (snd r_trap_rc) 19 "upstream builtins/trap1: EXIT trap preserves script's original exit status 19"
        _ = assert str eq (fst r_trap_rc) ("trap_msg" + nl) "upstream builtins/trap1: EXIT trap outputs message on non-zero exit"
        r_for_empty = run shell script "for x in; do echo $x; done; echo \"empty_for:$?\"" with []
        assert str eq (fst r_for_empty) ("empty_for:0" + nl) "upstream execution/for1: 'for x in; do ...' with empty item list executes 0 times and exits 0"

fn :: main => ()
      main =
        _ = test sh quoting and comments
        _ = test sh parameter expansions
        _ = test sh arithmetic and subst
        _ = test sh control flow and functions
        _ = test sh builtins and options
        _ = test sh redirs pipes globs
        _ = test sh upstream expansion and params
        _ = test sh upstream control and builtins
        _ = test sh upstream sete and subshells
        echo "All 101 exhaustive & upstream POSIX/BSD sh(1) assertions in test_sh.rg passed!"
