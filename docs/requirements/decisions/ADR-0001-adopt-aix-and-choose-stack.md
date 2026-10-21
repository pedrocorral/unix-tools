---
id: ADR-0001
title: Adopt AIX and build BSD/Unix userland tools in 100% Pure Ridgun
status: accepted
date: 2026-10-17
supersedes: []
affects: [docs/requirements, src/main/ridgun, src/test/ridgun, build.rg]
---
# ADR-0001 — Adopt AIX and build BSD/Unix userland tools in 100% Pure Ridgun

## Context
This repository (`unix-tools`) ports classic **BSD (`FreeBSD`/`OpenBSD`/`NetBSD`)** and **POSIX/Unix** userland utilities to **100% pure Ridgun (`.rg`)**, compiling each utility into a standalone native Linux ELF executable via `ridgunc` and `stable`.

Many BSD base-system utilities (`jot`, `rs`, `lam`, `vis`/`unvis`, `mtree`, `apply`, `doas`, as well as BSD-flavored `ls`, `find`, `make`, and `sh`) are either absent from standard GNU/Linux (`coreutils`/`util-linux`) or offer cleaner, more orthogonal specifications than their GNU counterparts.

## Options considered
1. **Pure Ridgun (`.rg`) with `stable` build layout** (`build.rg`, shared libraries in `src/main/ridgun/lib/`, each executable in `src/main/ridgun/<tool>/main.rg`, tests in `src/test/ridgun/test_*.rg`).
2. Hybrid host-language wrappers around existing C/Rust libraries.

## Decision
We choose **Option 1: 100% Pure Ridgun (`.rg`)** with zero host-language shortcuts:
- **Language & Compiler**: Ridgun (`v0.9.5+`), using the toolchain (`ridgunc` and `stable`) and consuming the system PAL library bundles (`posix.rglib`, `io.rglib`) from `/home/boogie/ridgun-libs` via `.stable/deps/`.
- **Idiomatic Style**:
  - Use **natural-language mixfix operators (`op :: ...`)** for domain logic, CLI flag predicates, rule evaluation, and data transformations.
  - Use **algebraic data types (`type ...`)** and exhaustive pattern matching with guards (`| cond = expr`) for ASTs, options, filesystem nodes, and rules.
  - Use **parallel `where`-DAGs (`||`)** where independent file/row computations can run concurrently.
  - Format all `.rg` files with `stable format` (canonical aesthetic alignment: 6-space clause column, 18-char outlier cap, 80-col / 4-param compact fallback, 2-space block indentation).
- **Project Layout**:
  - `build.rg` at repository root
  - `src/main/ridgun/lib/` for shared modules (`cli.rg`, `fmt_grid.rg`, `fs.rg`, etc.)
  - `src/main/ridgun/<tool>/main.rg` for each standalone executable (`jot`, `rs`, `lam`, `vis`, `ls`, `find`, `mtree`, `apply`, `make`, `sh`, `doas`)
  - `src/test/ridgun/test_<tool>.rg` for automated test suites

## Consequences
- Every utility compiles to a native Linux binary under `target/<tool>` via `stable built <tool>`.
- When a higher-tier tool (Tier 2–4) requires low-level OS syscalls not yet exposed by `ridgunc` (e.g., `lstat`/`readdir` without shell, direct `execve`, or `uid`/`gid`/`termios` for `doas`), those primitives must be added cleanly as built-ins in `/home/boogie/ridgun` rather than worked around with unsafe `/bin/sh` wrappers.
