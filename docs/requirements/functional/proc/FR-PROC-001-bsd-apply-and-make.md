---
id: FR-PROC-001
title: BSD apply(1) and make(1) utilities in Pure Ridgun
status: implemented
priority: should
created: 2026-10-17
updated: 2026-10-24
depends_on: [ADR-0001, ADR-0002]
verified_by: [TS-PROC-001]
---
# FR-PROC-001 — BSD `apply(1)` and `make(1)` Utilities in Pure Ridgun

## Statement
The project SHALL provide pure Ridgun implementations of BSD `apply(1)` (`src/main/ridgun/apply/main.rg`) and BSD `make(1)` (`src/main/ridgun/make/main.rg`).

## Functional Specification

### 1. `apply` — Apply a Command to a Set of Arguments (`apply(1)`)
- **Synopsis**: `apply [-a c] [-d] [-#] command args ...`
- **Behaviour**:
  - Divides `args ...` into groups of size `#` (default `1`, or inferred from the highest `%1`..`%9` placeholder in `command`; `-0` clamps to 1).
  - Substitutes `%1`..`%9` (or `-a c` magic character) with the current chunk of arguments and executes `command` for each group via the Pure Ridgun shell engine.
  - `-d`: Display the constructed commands on stdout without executing them (dry-run).

### 2. `make` — Maintain Program Dependencies (`make(1)`)
- **Synopsis**: `make [-f makefile] [-C dir] [-n] [-B] [-s] [-i] [-k] [-q] [-e] [-j jobs] [VAR=val ...] [target ...]`
- **Variable Assignment & Expansion**:
  - `VAR = val`: Recursive (deferred) assignment.
  - `VAR := val`: Immediate (simple) assignment.
  - `VAR ?= val`: Conditional default assignment.
  - `VAR += val`: Append assignment.
  - `VAR != cmd`: BSD shell assignment operator — executes `cmd` via Pure Ridgun `sh`, strips trailing newlines, and assigns the output to `VAR`.
  - Built-in functions: `$(shell cmd)`, `$(wildcard pattern)`, `$(patsubst pattern,replacement,text)`, and `$(subst from,to,text)`.
  - `-e` flag: Environment variables override standard Makefile variable assignments.
  - Command-line overrides (`make CC=clang`) take highest precedence.
  - Supports backslash-newline (`\` + `\n`) line continuations and `include <file>` / `-include <file>` directives.
- **Conditional Directives**:
  - Supports BSD conditionals (`.if defined(VAR)` / `.if !defined(VAR)` / `.if $(A) == "val"` / `.if $(A) != "val"`, `.else`, `.endif`) and GNU conditionals (`ifdef VAR`, `ifndef VAR`, `ifeq (a,b)`, `ifneq (a,b)`, `else`, `endif`).
- **Automatic Variables, Pattern/Suffix Rules, and Flags**:
  - `$@`, `$<`, `$^`, `$?`, `$*`, `$$`; `%.o: %.c` and `.c.o:`; `@`, `-`, `+` recipe prefixes; `-n`, `-s`, `-B`, `-i`, `-k`, `-q`, `-e`, `-C <dir>`, `-j <jobs>`.

## Acceptance Criteria
- AC1: `apply -d -2 "diff %1 %2" a1 b1 a2 b2` prints `diff a1 b1` and `diff a2 b2`, and `apply` supports `-a <char>`, `-#`, `%1`..`%9` inference, and live execution.
- AC2: `make` evaluates `=`, `:=`, `+=`, `?=`, and `!=` shell assignments, `$(shell ...)`, `$(wildcard ...)`, `$(patsubst ...)`, `$(subst ...)`, BSD `.if`/`.else`/`.endif` and GNU `ifdef`/`ifndef`/`ifeq`/`ifneq` conditionals, `-e` environment precedence, automatic variables (`$@`, `$<`, `$^`, `$*`, `$$`), pattern/suffix rules, `include`, `@`/`-`/`+` prefixes, and `-n`/`-s`/`-B`/`-i`/`-k`/`-q`/`-C` flags in topological DAG order.
