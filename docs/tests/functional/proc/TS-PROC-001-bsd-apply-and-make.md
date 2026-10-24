---
id: TS-PROC-001
title: BSD apply(1) command applicator and make(1) DAG dependency build tool
level: functional
covers: [FR-PROC-001]
status: automated
automated_in: [src/test/ridgun/test_proc.rg, src/test/ridgun/test_make.rg]
---
# TS-PROC-001 — BSD `apply(1)` and `make(1)` Verification

## Preconditions / fixtures
- Pure Ridgun `apply` and `make` implementations and `Makefile` fixtures exercising `=`, `:=`, `+=`, `?=`, `$@`, `$<`, `$^`, `$?`, `$*`, `$$`, pattern rules (`%.o: %.c`), suffix rules (`.c.o:`), backslash line continuations, `include`, `@`/`-`/`+` recipe prefixes, `-n`/`-s`/`-B`/`-i`/`-k`/`-q`/`-C`/`-j`, `.PHONY`, `mtime` up-to-date skipping, and circular dependency graphs.

## Steps / inputs
1. Run `apply -d -2 "diff %1 %2" a1 b1 a2 b2`, `%1`..`%9` arity inference, custom `-a` magic character, `-0`..`-9` chunking, and live execution.
2. Run `make` across unit and E2E fixtures testing deferred (`=`) vs immediate (`:=`) vs append (`+=`) vs conditional (`?=`) variables, CLI overrides, automatic variables (`$@`, `$<`, `$^`, `$?`, `$*`, `$$`), pattern/suffix rules, `\` line continuations, `include`, `@`/`-`/`+` recipe prefixes, `-n`/`-s`/`-B`/`-i`/`-k`/`-q`/`-C`, `mtime` incremental builds, and cycle rejection.

## Expected results (map to acceptance criteria)
- AC1 → `apply -d -2 "diff %1 %2" a1 b1 a2 b2` prints `diff a1 b1` and `diff a2 b2`, and handles all chunking and `%N` substitutions accurately.
- AC2 → `make` evaluates all variable flavors (`=`, `:=`, `+=`, `?=`), automatic variables (`$@`, `$<`, `$^`, `$?`, `$*`, `$$`), pattern rules, line continuations, `include`, recipe prefixes (`@`, `-`, `+`), and flags (`-n`, `-s`, `-B`, `-i`, `-k`, `-q`, `-C`), builds targets in topological DAG order, and rejects cycles.

## Not covered here (to avoid duplication)
- `sh` is covered in `TS-PROC-002`.
