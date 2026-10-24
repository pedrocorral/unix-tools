---
id: TS-PROC-002
title: BSD sh(1) Almquist-style command interpreter and shell AST evaluator
level: functional
covers: [FR-PROC-002]
status: automated
automated_in: [src/test/ridgun/test_proc.rg, src/test/ridgun/test_sh.rg]
---
# TS-PROC-002 — BSD `sh(1)` Verification

## Preconditions / fixtures
- Pure Ridgun `sh` interpreter (`src/main/ridgun/sh/main.rg` and `src/main/ridgun/lib/proc_lib.rg`) and exhaustive test suites in `src/test/ridgun/test_proc.rg` and `src/test/ridgun/test_sh.rg`.

## Steps / inputs
1. Test quoting (`'...'`, `"..."`, `\`, line continuations, `#` comments vs mid-word `#`), special parameters (`$?`, real `$$` via `sys getpid`, `$#`, `$0`, `$1`..`${10}`, `$*`, `$@`), parameter expansions (`:-`, `-`, `:=`, `=`, `:+`, `+`, `:?`, `${#var}`, `#`, `##`, `%`, `%%`), nested command substitution `$(...)` and `` `...` ``, arithmetic expansion `$((...))` (including bitwise, logical, ternary, comparisons, and variable assignments), `$IFS` field splitting, and pathname globbing (`*`, `?`, `[...]`, `set -f`).
2. Test redirections (`>`, `>|`, `>>`, `<`, `2>`, `2>>`, `2>&1`, `>&2`, `<<EOF`, `<<'EOF'`, `<<-EOF`), pipelines (`|`, `!`), short-circuit lists (`&&`, `||`, `;`), `if/elif/else/fi`, `case/in/esac`, `for`, `while`, `until`, `break [n]`, `continue [n]`, subshells `(...)`, brace groups `{ ...; }`, and recursive shell functions with `local` and `return`.
3. Test all built-in commands (`:`, `true`, `false`, `echo`, `printf`, `cd`, `pwd`, `export`, `unset`, `readonly`, `shift`, `set -e/-u/-f/-x/-C/--`, `read`, `test`/`[`, `eval`, `exec`, `command -v`, `type`, `alias`, `umask`, `trap`, `exit`, `.`) in Pure Ridgun without `/bin/sh` delegation.

## Expected results (map to acceptance criteria)
- AC1 → All quoting, parameter expansions, real `$$` PID, command substitutions, arithmetic expressions, `$IFS` splitting, and globs evaluate to exact POSIX outputs.
- AC2 → All control-flow constructs, pipelines, redirections, here-documents, functions, traps, options (`-e`, `-u`, `-f`), and built-ins execute in Pure Ridgun with exact POSIX/BSD semantics and exit codes.

## Not covered here (to avoid duplication)
- `apply` and `make` are covered in `TS-PROC-001`.
