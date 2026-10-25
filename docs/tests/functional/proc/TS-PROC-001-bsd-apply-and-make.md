---
id: TS-PROC-001
title: BSD apply(1) argument chunking and make(1) DAG, conditionals, != shell assignment, functions, and flags
level: unit
covers: [FR-PROC-001]
status: automated
automated_in: [src/test/ridgun/test_proc.rg, src/test/ridgun/test_make.rg]
---
# TS-PROC-001 — BSD `apply(1)` and `make(1)` Verification

## Preconditions / fixtures
- Pure Ridgun `apply` and `make` engines in `src/main/ridgun/lib/proc_lib.rg` and binaries `target/apply` and `target/make`.

## Steps / inputs
1. Test `apply` with `-d`, `-#`, `-0`, `-a <char>`, `%1`..`%9` inference, and live command execution.
2. Test `make` variable assignments (`=`, `:=`, `+=`, `?=`, `!=` shell assignment), built-in functions (`$(shell ...)`, `$(wildcard ...)`, `$(patsubst ...)`, `$(subst ...)`), BSD `.if`/`.else`/`.endif` and GNU `ifdef`/`ifndef`/`ifeq`/`ifneq` conditionals, `-e` environment variable override, automatic variables (`$@`, `$<`, `$^`, `$?`, `$*`, `$$`), pattern/suffix rules (`%.o: %.c`, `.c.o:`), `include`, `@`/`-`/`+` recipe prefixes, `-n`/`-s`/`-B`/`-i`/`-k`/`-q`/`-C`, `mtime` incremental builds, and cycle rejection.

## Expected results (map to acceptance criteria)
- AC1 → `apply` constructs and executes grouped commands accurately.
- AC2 → `make` evaluates all variable flavors (`=`, `:=`, `+=`, `?=`, `!=`), functions, BSD/GNU conditionals, `-e` overrides, automatic variables, pattern/suffix rules, and DAG dependencies accurately.

## Not covered here (to avoid duplication)
- `sh` is covered in `TS-PROC-002`.
