---
id: PRODUCT-VISION
title: BSD and Unix Userland Tools in Pure Ridgun
status: approved
---
# Vision — BSD & Unix Userland Tools in Pure Ridgun (`unix-tools`)

## 1. Purpose
Build a suite of classic **BSD (`FreeBSD`/`OpenBSD`)** and **Unix** command-line utilities written in **100% pure Ridgun (`.rg`)**, bringing BSD tools that are missing on standard Linux (`jot`, `rs`, `lam`, `vis`/`unvis`, `mtree`, `apply`, `doas`) as well as clean BSD implementations of core tools (`ls`, `find`, `make`, `sh`) into standalone native ELF executables.

## 2. Why BSD Tools in Ridgun?
1. **Genuine Utility on Linux**: Standard GNU/Linux distributions lack BSD base-system tools like `mtree(8)`, `rs(1)`, `jot(1)`, `lam(1)`, `apply(1)`, `vis(1)`/`unvis(1)`, and `doas(1)`.
2. **Showcasing Ridgun's Core Strengths**:
   - **Natural-language mixfix operators (`op ::`)** make rule engines (`doas.conf`, `mtree` specs, `find` predicates) read like specifications.
   - **ADTs & Pattern Matching** model shell ASTs (`sh`), dependency graphs (`make`), and boolean filter trees (`find`).
   - **Column/Grid Geometry & Parallel `where`-DAGs (`||`)** power tabular reshaping (`rs`, `ls` multi-column layout) and parallel file/tree verification (`mtree`, `find`).

## 3. Staged 4-Tier Implementation Roadmap

Agents implementing this project MUST follow this 4-tier progression from pure text/table filters up to privileged system tools:

| Tier | Domain | Tools | Ridgun Runtime Dependencies |
|---|---|---|---|
| **Tier 1** | `FILTER` | `jot(1)`, `rs(1)`, `lam(1)`, `vis(1)`, `unvis(1)` | Works out-of-the-box with current Ridgun v0.9.3 (`args`, `read_file`, `write_file`, `echo`, string/list primitives). |
| **Tier 2** | `FS` | `ls(1)` (BSD), `find(1)` (BSD), `mtree(8)` | Requires fast directory listing & metadata (`list_dir`, `file_stat` / `lstat`, `sha256`). |
| **Tier 3** | `PROC` | `apply(1)`, `make(1)`, `sh(1)` (BSD Almquist-style shell) | Requires process spawning, pipes, file descriptors, and exit-status waiting (`fork`/`execve`/`pipe`/`waitpid`). |
| **Tier 4** | `SEC` | `doas(1)` & `doas.conf(5)` | **Phase A (`doas -C` config parser & rule evaluator)**: works purely in Ridgun today. **Phase B (`setuid` execution)**: requires direct POSIX `getuid`/`setresuid`/`setgroups`/`execve` and `termios`/PAM without `/bin/sh`. |

## 4. Toolchain Reference for Agents
- Compiler binary: `/home/boogie/ridgun/target/ridgunc`
- Build & format tool: `/home/boogie/ridgun/target/stable`
- Language specification: `/home/boogie/ridgun/docs/lang/spec.md`
- Ridgun coding rules: `/home/boogie/ridgun/.aix/custom/instructions/languages/ridgun.md`
