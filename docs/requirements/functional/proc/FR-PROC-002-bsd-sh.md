---
id: FR-PROC-002
title: BSD sh(1) Almquist-style command interpreter in Pure Ridgun
status: implemented
priority: should
created: 2026-10-17
updated: 2026-10-24
depends_on: [ADR-0001, FR-PROC-001]
verified_by: [TS-PROC-002]
---
# FR-PROC-002 — BSD `sh(1)` (Almquist-Style Shell) in Pure Ridgun

## Statement
The project SHALL provide a pure Ridgun implementation of a POSIX / BSD `sh(1)` (Almquist-style) command interpreter (`src/main/ridgun/sh/main.rg` and `src/main/ridgun/lib/proc_lib.rg`) supporting `-c` command execution, `.sh` script execution, `-s` stdin execution, and interactive REPL usage, without delegating shell grammar, pipelines, or built-in execution to `/bin/sh`.

## Functional Specification

### 1. Lexing, Quoting, Comments & Line Continuations
- **Single quotes (`'...'`)**: Preserve all characters literally without any expansion.
- **Double quotes (`"..."`)**: Preserve whitespace and glob characters while performing `$VAR`, `${...}`, `$(...)`, `` `...` ``, and `$((...))` expansions, plus `\"`, `\\`, `\$`, and `` \` `` backslash escapes.
- **Unquoted backslash (`\c`)**: Escapes the next character `c`; a backslash followed immediately by newline (`\` + `\n`) acts as a line continuation and is removed before tokenization.
- **Comments (`# ...`)**: A `#` at the beginning of a token (start of line or preceded by whitespace/operator) starts a comment to end-of-line; a `#` inside a word (e.g., `echo foo#bar`) is a literal `#` character.

### 2. Expansions (Parameter, Command Substitution, Arithmetic, Field Splitting, Globbing)
- **Special Parameters**:
  - `$?`: Exit status of the most recent pipeline.
  - `$$`: Real OS process ID of the shell via `sys getpid` (MUST NOT be hardcoded).
  - `$#`: Decimal number of positional parameters.
  - `$0`: Shell or script name.
  - `$1`..`$9` and `${10}`..`${N}`: Positional parameters.
  - `$*` and `$@`: All positional parameters (with `"$@"` preserving distinct arguments).
- **Parameter Expansion Forms**:
  - `$VAR` and `${VAR}`: Value of `VAR` (or error when `set -u` / `nounset` is active and `VAR` is unset).
  - `${VAR:-word}` (use default if unset or null) and `${VAR-word}` (use default if unset).
  - `${VAR:=word}` (assign default if unset or null) and `${VAR=word}` (assign default if unset).
  - `${VAR:+word}` (use alternate if set and non-null) and `${VAR+word}` (use alternate if set).
  - `${VAR:?word}` (print diagnostic error and exit non-zero if unset or null).
  - `${#VAR}`: String length in characters of the value of `VAR`.
  - `${VAR#pattern}` (strip shortest leading match), `${VAR##pattern}` (strip longest leading match), `${VAR%pattern}` (strip shortest trailing match), and `${VAR%%pattern}` (strip longest trailing match) using shell glob patterns.
- **Command Substitution**:
  - `$(command)` (including nested `$(echo $(...))`) and backticks `` `command` `` execute `command` inside the Pure Ridgun shell evaluator and substitute its standard output with trailing newlines stripped.
- **Arithmetic Expansion (`$(( expression ))`)**:
  - Evaluates integer arithmetic expressions in Pure Ridgun supporting:
    - Unary `+`, `-`, `!` (logical NOT), `~` (bitwise NOT).
    - Multiplicative `*`, `/`, `%`, additive `+`, `-`.
    - Bitwise shifts `<<`, `>>`, bitwise `&`, `^`, `|`.
    - Relational `<`, `<=`, `>`, `>=` and equality `==`, `!=`.
    - Logical `&&`, `||` and ternary conditional `cond ? expr1 : expr2`.
    - Parentheses `( ... )`, shell variable identifiers (`X` or `$X`), and assignment (`X = expr`, `X += expr`, `X -= expr`).
- **Field Splitting (`$IFS`) & Pathname Expansion (Globbing)**:
  - Unquoted expansion results are split by `$IFS` (default space, tab, newline).
  - Unquoted words containing `*`, `?`, or `[...]` expand against matching filesystem paths in sorted order (or remain literal if no path matches), unless disabled via `set -f` (`noglob`).

### 3. Redirections & Here-Documents
- Output redirections: `> file`, `>| file` (clobber even with `set -C`), `>> file` (append), `2> file`, `2>> file`, `2>&1`, `1>&2` / `>&2`, `&> file`.
- Input redirections: `< file`, `<> file`, and Here-Documents `<<EOF ... EOF` (with parameter/command/arithmetic expansion), `<<'EOF' ... EOF` / `<<"EOF"` (literal, no expansion), and `<<-EOF` (leading tabs stripped).

### 4. Compound Commands, Functions & Control Flow
- **Pipelines (`cmd1 | cmd2 | ...`) & Negation (`! pipeline`)**:
  - Executes multi-stage pipelines inside the Pure Ridgun interpreter, piping stdout of stage $i$ into stdin of stage $i+1$, and negating the exit status (`0` ↔ `1`) when prefixed with `!`.
- **Lists (`;`, `&`, `&&`, `||`)**: Left-associative short-circuit evaluation for `&&` and `||`.
- **Conditionals**: `if list; then list; [elif list; then list; ...] [else list;] fi`.
- **Pattern Matching (`case`)**: `case word in [ [(] pat1 [| pat2 ...] ) list ;; ] ... esac` supporting glob patterns (`*`, `?`, `[...]`) and alternation `|`.
- **Loops**:
  - `for name [in word ...]; do list; done` (defaults to `in "$@"` when `in ...` is omitted).
  - `while list; do list; done` and `until list; do list; done`.
  - `break [n]` and `continue [n]` supporting single and multi-level (`n >= 1`) loop control.
- **Grouping**:
  - Subshell `( list )`: Executes `list` in an isolated copy of the shell state (`vars`, `cwd`, `positional`, `functions`, `options` do not leak to parent).
  - Brace group `{ list; }`: Executes `list` in the current shell environment (mutations persist).
- **Shell Functions**:
  - `fname() { list; }` (and `function fname { list; }`): Defines a callable function with scoped positional parameters (`$1`..`$9`, `$#`, `$@`), `local var[=val]` local variable scoping, `return [n]`, and full support for recursive calls.

### 5. Built-in Commands (Pure Ridgun Execution)
- `:`, `true`, `false`
- `echo` (`-n`, `-e` escape sequences `\n`, `\t`, `\r`, `\\`, `\c`)
- `printf` (`%s`, `%d`, `%i`, `%x`, `%X`, `%o`, `%c`, `%%`, width/zero-padding, escape sequences, and format reuse across extra arguments)
- `cd` (updates `$PWD` and `$OLDPWD`, supports `cd` to `$HOME` and `cd -` to `$OLDPWD`), `pwd`
- `export` (`export VAR=val`, `export VAR`, `export -p`), `unset` (`-v`, `-f`), `readonly` (`readonly VAR=val`, `readonly VAR`, `readonly -p`)
- `shift [n]`, `set` (`-e` errexit, `-u` nounset, `-f` noglob, `-x` xtrace, `-C` noclobber, `+e`, `+u`, `+f`, `+x`, `--` positional parameter replacement)
- `read` (`-r` raw mode, multiple variable names split by `$IFS` with remainder assigned to last variable)
- `test` and `[` (`-e`, `-f`, `-d`, `-r`, `-w`, `-x`, `-s`, `-L`/`-h`, `-p`, `-S`, `-z`, `-n`, `=`, `==`, `!=`, `-eq`, `-ne`, `-lt`, `-le`, `-gt`, `-ge`, `-nt`, `-ot`, `!`, `-a`, `-o`, `( ... )`)
- `eval`, `exec`, `command` (`-v`, `-V`, `-p`), `type`, `alias`, `unalias`, `umask`, `trap` (`trap 'cmd' EXIT INT TERM 0` and reset `trap - EXIT`), `wait`, `times`, `exit [n]`, `return [n]`, `break [n]`, `continue [n]`, `.` / `source`.

## Acceptance Criteria
- AC1: `sh` accurately tokenizes and expands single/double quotes, backslash escapes, line continuations, all POSIX parameter expansions (`:-`, `:=`, `:+`, `:?`, `${#var}`, `#`, `##`, `%`, `%%`), real `$$` (`sys getpid`), command substitutions `$(...)` / `` `...` ``, arithmetic `$((...))`, `$IFS` splitting, and pathname globbing.
- AC2: `sh` executes pipelines (`|`, `!`), redirections (`>`, `>>`, `<`, `2>`, `2>&1`, `<<EOF`, `<<-EOF`), conditionals (`if`/`elif`/`else`/`fi`, `case`/`in`/`esac`), loops (`for`, `while`, `until`, `break [n]`, `continue [n]`), subshells `(...)`, brace groups `{ ...; }`, recursive shell functions with `local` and `return`, `set -e`/`-u`/`-f`, `trap ... EXIT`, and all POSIX built-ins in Pure Ridgun without delegating to `/bin/sh`.
