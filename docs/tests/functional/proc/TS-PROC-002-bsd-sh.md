---
id: TS-PROC-002
title: BSD sh(1) Almquist-style command interpreter, POSIX expansions, >1000 loops, subshell isolation, globs, built-ins, background jobs, and traps
level: functional
covers: [FR-PROC-002]
status: automated
automated_in: [src/test/ridgun/test_proc.rg, src/test/ridgun/test_sh.rg]
---
# TS-PROC-002 — BSD `sh(1)` Verification

## Preconditions / fixtures
- Pure Ridgun `sh` interpreter (`src/main/ridgun/sh/main.rg` and `src/main/ridgun/lib/proc_lib.rg`) and test suites `src/test/ridgun/test_proc.rg` and `src/test/ridgun/test_sh.rg`.

## Steps / inputs
1. Test quoting (`'...'`, `"..."`, `\`, line continuations, `#` comments), special parameters (`$?`, `$$`, `$!`, `$#`, `$0`, `$1`..`${10}`, `$*`, `$@`), parameter expansions (`:-`, `-`, `:=`, `=`, `:+`, `+`, `:?`, `${#var}`, `#`, `##`, `%`, `%%`), `~` tilde expansion, custom `$IFS` field splitting, command substitution `$(...)` and `` `...` ``, arithmetic expansion `$((...))` (including `~`, `=`, `+=`, `-=`, `*=`, `/=`, `%=`, `++x`, `--x`, `x++`, `x--`), and pathname globbing (`*`, `?`, `[abc]`, `[a-z]`, `[!a-z]`, `dir/*.ext`, `set -f`).
2. Test redirections (`>`, `>>`, `<`, `2>file`, `2>>file`, `2>&1`, `>&2`, `<<EOF`, `<<'EOF'`, `<<-EOF`), pipelines (`|`, `!`), short-circuit lists (`&&`, `||`, `;`), `if/elif/else/fi`, `case/in/esac`, `for`, `while` and `until` (> 2,500 iterations without artificial cap), `break [n]`, `continue [n]`, subshells `( cd /tmp; umask 077; VAR=x )` (verifying parent CWD, umask, and vars are untouched), brace groups `{ ...; }`, per-command prefix assignments (`VAR=val cmd`), background jobs (`cmd &`, `$!`, `wait`), and recursive functions with `local` and `return`.
3. Test all built-ins (`:`, `true`, `false`, `echo`, `printf`, `cd`, `pwd`, `export`, `unset`, `readonly`, `shift`, `set -e/-u/-f/--`, `read`, `test`/`[`, `eval`, `exec`, `command [-v]`, `type`, `umask`, `getopts`, `wait`, `trap` for `EXIT`/`INT`/`TERM`/`HUP`/`-`, `exit`, `.`) and verify `cat` executes as a real external process.

## Expected results (map to acceptance criteria)
- AC1 → All quoting, expansions, tilde `~`, custom `IFS`, `$((...))` arithmetic with assignments/increments/bitwise `~`, `$(...)`, and globs (`[a-z]`, `[!a-z]`, `dir/*.ext`) evaluate to exact POSIX outputs.
- AC2 → All control-flow constructs (including >1,000 iteration loops), subshell CWD/umask/env isolation, prefix assignments, stderr redirections, background jobs (`&`, `$!`, `wait`), signal traps, and built-ins (`command`, `type`, `umask`, `getopts`, `exec`) execute with exact POSIX/BSD semantics.

## Not covered here (to avoid duplication)
- `apply` and `make` are covered in `TS-PROC-001`.
