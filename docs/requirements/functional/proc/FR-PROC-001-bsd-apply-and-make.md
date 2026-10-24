---
id: FR-PROC-001
title: BSD apply(1) and make(1) utilities in Pure Ridgun
status: implemented
priority: should
created: 2026-10-17
updated: 2026-10-24
depends_on: [ADR-0001]
verified_by: [TS-PROC-001]
---
# FR-PROC-001 — BSD `apply(1)` and `make(1)` Utilities in Pure Ridgun

## Statement
The project SHALL provide pure Ridgun implementations of BSD `apply(1)` (`src/main/ridgun/apply/main.rg`) and BSD `make(1)` (`src/main/ridgun/make/main.rg`).

## Functional Specification

### 1. `apply` — Apply a Command to a Set of Arguments (`apply(1)`)
- **Synopsis**: `apply [-a c] [-d] [-#] command args ...`
- **Behaviour**:
  - Divides `args ...` into groups of size `#` (default `1`, or inferred from the highest `%1`..`%9` placeholder in `command`; `-0` passes 0 arguments per invocation or appends 1 if no `-0` placeholder).
  - Substitutes `%1`..`%9` (or `-a c` magic character) with the current chunk of arguments and executes `command` for each group.
  - `-d`: Display the constructed commands on stdout without executing them (dry-run).

### 2. `make` — Maintain Program Dependencies (`make(1)`)
- **Synopsis**: `make [-f makefile] [-C dir] [-n] [-B] [-s] [-i] [-k] [-q] [-j jobs] [VAR=val ...] [target ...]`
- **Variable Assignment & Expansion**:
  - `VAR = val`: Recursive (deferred) assignment — `$(OTHER)` references inside `val` are expanded when `$(VAR)` is referenced, reflecting later updates to `OTHER`.
  - `VAR := val`: Immediate (simple) assignment — `$(OTHER)` references inside `val` are expanded immediately at assignment time.
  - `VAR ?= val`: Conditional assignment — assigns `val` only if `VAR` is not yet defined.
  - `VAR += val`: Appends `val` (separated by a space) to the existing value of `VAR`.
  - Command-line overrides (`make CC=clang`) take precedence over in-file assignments.
  - Supports backslash-newline (`\` + `\n`) line continuations and `include <file>` / `-include <file>` directives.
- **Automatic Variables**:
  - `$@`: Target name of the current rule.
  - `$<`: First prerequisite of the current rule.
  - `$^`: Space-separated list of all unique prerequisites of the current rule.
  - `$?`: Space-separated list of prerequisites newer than the target (or all prerequisites when target does not exist).
  - `$*`: The stem matched by a pattern/suffix rule (`%.o: %.c` or `.c.o:`), or target basename without suffix.
  - `$$`: Literal `$` character.
- **Pattern & Suffix Inference Rules**:
  - Supports pattern rules (`%.o: %.c`) and classic POSIX suffix rules (`.c.o:`) to synthesize rules for targets lacking explicit recipes.
- **Recipe Modifiers & Execution Flags**:
  - `@` recipe prefix: Suppress echoing of that recipe line.
  - `-` recipe prefix: Ignore non-zero exit status of that recipe line.
  - `+` recipe prefix: Execute recipe line even when `-n` is specified (or strip `+` cleanly during recipe normalization).
  - `-n`: Dry-run — print recipes without executing them.
  - `-s`: Silent mode — do not echo any recipe lines before executing.
  - `-B`: Unconditionally rebuild all targets regardless of `mtime`.
  - `-i`: Ignore non-zero exit codes across all recipes.
  - `-k`: Keep going when a target fails, continuing to build independent targets that do not depend on the failed target.
  - `-q`: Question mode — exit `0` if target is up-to-date, `1` if target needs rebuilding, without executing recipes.
  - `-C <dir>`: Change working directory to `<dir>` before reading the Makefile or executing targets.
  - `-j <jobs>`: Parallel job limit specification.

## Acceptance Criteria
- AC1: `apply -d -2 "diff %1 %2" a1 b1 a2 b2` prints `diff a1 b1` and `diff a2 b2`, and `apply` supports `-a <char>`, `-#`, `%1`..`%9` inference, and live execution.
- AC2: `make` distinguishes deferred (`=`) vs immediate (`:=`) vs append (`+=`) vs default (`?=`) variables, expands automatic variables (`$@`, `$<`, `$^`, `$*`, `$$`), resolves pattern/suffix rules (`%.o: %.c`), handles `\` line continuations and `include`, honors `@`/`-`/`+` recipe prefixes and `-n`/`-s`/`-B`/`-i`/`-k`/`-q`/`-C` flags, executes recipes in topological DAG order, and rejects circular dependencies.
