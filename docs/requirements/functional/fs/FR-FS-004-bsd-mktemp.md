---
id: FR-FS-004
title: BSD mktemp(1) temporary file and directory creator in Pure Ridgun
status: implemented
priority: should
created: 2026-10-21
updated: 2026-10-24
depends_on: [ADR-0001, ADR-0002]
verified_by: [TS-FS-004]
---
# FR-FS-004 — BSD `mktemp(1)` in Pure Ridgun

## Statement
The project SHALL provide a pure Ridgun implementation of the BSD `mktemp(1)` utility (`src/main/ridgun/mktemp/main.rg`) using `posix.rglib` and `io.rglib` (`sys random bytes list`, `sys getpid`, `posix open`, `posix close`, `sys mkdir`) to atomically generate unique temporary files and directories.

## Functional Specification
- **Template Substitution**: Replaces trailing `X` characters in the template (default `tmp.XXXXXXXXXX` in `/tmp` or `-p <tmpdir>`) with random alphanumeric characters (`0-9A-Za-z`) drawn from `sys random bytes list`.
- **Flags**:
  - `-d`: Atomically create a directory with mode `0700` (`448`) via `sys mkdir path 448` in a single syscall (no `0755` -> `0700` race window) instead of a regular file (`posix open` with `O_CREAT | O_EXCL`, mode `0600` / `384`).
  - `-u`: Unsafe / dry-run mode — print the generated path without creating the file or directory on disk.
  - `-q`: Quiet mode — suppress error messages on failure.
  - `-p <dir>`: Use `<dir>` as the base directory when `-t` or a relative template is given.
  - `-t <prefix>`: Generate a template `<dir>/<prefix>.XXXXXXXX`.

## Acceptance Criteria
- AC1: `mktemp /tmp/ut_mktemp.XXXXXX` atomically creates a regular file with mode `0600` matching the template prefix and prints its path.
- AC2: `mktemp -d /tmp/ut_mktemp_dir.XXXXXX` atomically creates a directory with mode `0700` (`sys mkdir path 448`) and `mktemp -u /tmp/ut_mktemp_dry.XXXXXX` prints a unique candidate path without creating it on disk.
