---
id: FR-PROC-002
title: BSD sh(1) Almquist-style command interpreter in Pure Ridgun
status: implemented
priority: should
created: 2026-10-17
updated: 2026-10-24
depends_on: [ADR-0001, ADR-0002, FR-PROC-001]
verified_by: [TS-PROC-002]
---
# FR-PROC-002 — BSD `sh(1)` (Almquist-Style Shell) in Pure Ridgun

## Statement
The project SHALL provide a pure Ridgun implementation of a POSIX / BSD `sh(1)` command interpreter (`src/main/ridgun/sh/main.rg` and `src/main/ridgun/lib/proc_lib.rg`) with zero in-memory command shortcuts and zero artificial loop iteration caps.

## Functional Specification
1. **Lexing, Quoting, Comments & Here-Documents**:
   - Single quotes `'...'`, double quotes `"..."`, backslash escaping `\`, line continuation `\ + LF`, `#` comments, and `<<EOF`, `<<'EOF'`, `<<-EOF` here-documents.
2. **Expansions, Arithmetic, Tilde, `IFS` & Globbing**:
   - Special parameters `$?`, `$$` (`sys getpid`), `$!`, `$#`, `$0`, `$1`..`${10}`, `$*`, `$@`.
   - Braced modifiers `:-`, `-`, `:=`, `=`, `:+`, `+`, `:?`, `${#var}`, `#`, `##`, `%`, `%%`.
   - Command substitution `$(...)` and `` `...` ``.
   - Arithmetic `$((...))` with full precedence, ternary `?:`, bitwise `& | ^ ~ << >>`, logical `&& || !`, comparisons, variable assignments (`=`, `+=`, `-=`, `*=`, `/=`, `%=`), and `++x`, `--x`, `x++`, `x--`.
   - Leading `~` / `~/path` tilde expansion to `$HOME`.
   - Unquoted `$VAR` field splitting using `$IFS` characters.
   - Pathname globbing (`*`, `?`, `[abc]`, `[a-z]`, `[!a-z]`/`[^a-z]`, and subdirectory globs `dir/*.ext`) with `set -f` (`noglob`) control.
3. **Control Flow, Subshell Isolation, Redirections, Background Jobs & Built-ins**:
   - `;`, `&&`, `||`, `!`, `|`, `( ... )` (with full working directory `cwd`, `umask`, and variable isolation), `{ ...; }`, `if/elif/else/fi`, `case/in/esac`, `for`, `while` and `until` (with no 1,000-iteration cap), `break [N]`, `continue [N]`, functions with `local` and `return`, and per-command prefix assignments (`VAR=val cmd`).
   - Redirections `<`, `>`, `>>`, `2>file`, `2>>file`, `2>&1`, `>&2` / `1>&2`.
   - Background jobs `cmd &`, `$!`, and `wait [pid]`.
   - Built-ins `:`, `true`, `false`, `echo`, `printf`, `read`, `cd`, `pwd`, `export`, `readonly`, `local`, `unset`, `shift`, `set`, `return`, `break`, `continue`, `eval`, `exec`, `command [-v]`, `type`, `umask`, `getopts`, `wait`, `trap` (`EXIT`, `INT`, `TERM`, `HUP`, `QUIT`, `-`), `exit`, `.`, `test` / `[`.
   - All external commands (including `cat`) execute as real child processes via `posix pipe` + `posix fork` + `posix dup2` + `posix execve` + `posix waitpid`.

## Acceptance Criteria
- AC1: Quoting, parameter expansions, tilde `~`, custom `IFS` splitting, `$((...))` arithmetic (including assignments, `++`/`--`, and `~`), `$(...)`, and globbing (`*`, `?`, `[a-z]`, `[!a-z]`, `dir/*.ext`) evaluate to exact POSIX results.
- AC2: Control flow (including `while`/`until` > 1,000 iterations), subshell `( cd ... )` isolation, prefix assignments `VAR=val cmd`, redirections (`2>`, `2>>`, `>&2`, `2>&1`), background jobs (`cmd &`, `$!`, `wait`), signal `trap`s, and all built-ins (`command -v`, `type`, `umask`, `getopts`, `exec`, etc.) execute with exact POSIX/BSD semantics.
