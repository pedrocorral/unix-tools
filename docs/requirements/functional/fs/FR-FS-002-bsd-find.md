---
id: FR-FS-002
title: BSD find(1) directory tree boolean evaluator in Pure Ridgun
status: implemented
priority: must
created: 2026-10-17
updated: 2026-10-24
depends_on: [ADR-0001, ADR-0002, FR-FS-001]
verified_by: [TS-FS-002]
---
# FR-FS-002 — BSD `find(1)` Directory Tree Boolean Evaluator in Pure Ridgun

## Statement
The project SHALL provide a pure Ridgun implementation of BSD `find(1)` (`src/main/ridgun/find/main.rg`) that parses command-line flags (`-L`, `-P`), primaries, operators, and actions into an algebraic Boolean expression AST and evaluates it recursively across directory hierarchies.

## Functional Specification

### 1. Primaries, Options, Operators, and Actions
- **Traversal Flags**:
  - `-P` (default): Do not follow symbolic links.
  - `-L`: Follow symbolic links to directories and stat link targets.
- **Primaries**:
  - `-name pattern` / `-iname pattern`: Shell glob matching (`*`, `?`, `[...]`, `[!...]`) against the basename.
  - `-path pattern`: Glob matching against the full path.
  - `-regex pattern`: Regular expression / anchored pattern match against the full path.
  - `-type [f|d|l|p|s|b|c]`: Match file type.
  - `-size [+|-]N[c|k|M|G]`: Match file size.
  - `-perm mode` / `-perm -mode` / `-perm /mode`: Match exact octal permissions, all permission bits set (`-mode`), or any permission bit set (`/mode`).
  - `-mtime [+|-]N`: Match modification time in 24-hour periods relative to current time.
  - `-newer file`: Match files whose `mtime` (seconds/nanoseconds) is strictly newer than `file`.
  - `-user name_or_uid` / `-group name_or_gid`: Match file owner or group.
  - `-links [+|-]N`: Match hardlink count (`st_nlink`).
  - `-inum N`: Match inode number (`st_ino`).
  - `-empty`: Match 0-byte regular files or empty directories.
  - `-maxdepth N` / `-mindepth N`: Control traversal depth.
  - `-prune`: Always evaluates to true; stops `find` from descending into the current directory.
- **Operators**:
  - `( expr )`, `! expr` / `-not expr`, `expr1 -a expr2` / `expr1 -and expr2` (or implicit juxtaposition), `expr1 -o expr2` / `expr1 -or expr2`.
- **Actions**:
  - `-print` (default), `-print0`, `-delete`, `-exec cmd {} ;` (per-file execution), and `-exec cmd {} +` (batched single-process execution with all matched paths).

## Acceptance Criteria
- AC1: `find src -type f -name "*.rg"` lists all `.rg` files under `src` in deterministic traversal order, and supports `-L` symlink following, `-newer`, `-perm` (exact/`-`/`/`), `-links`, `-inum`, `-regex`, `-mtime`, `-user`, `-group`, and `-delete`.
- AC2: Compound boolean expressions with `-or`, `-and`, `-not`, parentheses, `-prune` directory exclusion, `-exec cmd {} ;`, and batched `-exec cmd {} +` evaluate according to POSIX/BSD semantics.
