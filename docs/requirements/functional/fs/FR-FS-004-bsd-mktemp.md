---
id: FR-FS-004
title: BSD mktemp(1) temporary file and directory creator in Pure Ridgun
status: implemented
priority: should
created: 2026-10-21
updated: 2026-10-21
depends_on: [ADR-0001]
verified_by: [TS-FS-004]
---
# FR-FS-004 — BSD `mktemp(1)` in Pure Ridgun

## Statement
The project SHALL provide a pure Ridgun implementation of the BSD `mktemp(1)` utility (`src/main/ridgun/mktemp/main.rg`) using `posix.rglib` and `io.rglib` (`sys random bytes list`, `sys getpid`, `posix open`, `posix close`, `posix mkdir`, `posix chmod`) to safely generate unique temporary files and directories.

## Functional Specification
- **Template Substitution**: Replaces trailing `X` characters in the template (default `tmp.XXXXXXXXXX` in `/tmp` or `-p <tmpdir>`) with random alphanumeric characters (`0-9A-Za-z`) seeded from `sys random bytes list` and `sys getpid`.
- **Flags**:
  - `-d`: Create a directory (mode `0700` / `448`) instead of a regular file (mode `0600` / `384`).
  - `-u`: Unsafe / dry-run mode — print the generated path without creating the file or directory on disk.
  - `-q`: Quiet mode — suppress error messages on failure.
  - `-p <dir>`: Use `<dir>` as the base directory when `-t` or a relative template is given.
  - `-t <prefix>`: Generate a template `<dir>/<prefix>.XXXXXXXX`.

## Acceptance Criteria
- AC1: `mktemp /tmp/ut_mktemp.XXXXXX` creates a regular file with mode `0600` matching the template prefix and prints its path.
- AC2: `mktemp -d /tmp/ut_mktemp_dir.XXXXXX` creates a directory and `mktemp -u /tmp/ut_mktemp_dry.XXXXXX` prints a unique candidate path without creating it on disk.
