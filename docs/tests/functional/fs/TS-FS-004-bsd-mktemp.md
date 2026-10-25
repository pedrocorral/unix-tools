---
id: TS-FS-004
title: BSD mktemp(1) atomic 0600 file and 0700 directory creation verification
level: functional
covers: [FR-FS-004]
status: automated
automated_in: [src/test/ridgun/test_fs.rg]
---
# TS-FS-004 — BSD `mktemp(1)` Verification

## Preconditions / fixtures
- Pure Ridgun `mktemp` utility (`src/main/ridgun/mktemp/main.rg`) and `fs_lib.rg` helpers.

## Steps / inputs
1. Run `mktemp /tmp/ut_mktemp.XXXXXX` and verify the printed path exists as a regular file with exact octal mode `0600` (`384`).
2. Run `mktemp -d /tmp/ut_mktemp_d.XXXXXX` and verify the printed path is atomically created as a directory with exact octal mode `0700` (`448`); run `mktemp -u`, `-p`, `-t`, and `-q` combinations.

## Expected results (map to acceptance criteria)
- AC1 → `mktemp /tmp/ut_mktemp.XXXXXX` creates a regular file with mode `0600` matching the template prefix and prints its path.
- AC2 → `mktemp -d /tmp/ut_mktemp_dir.XXXXXX` atomically creates a directory with mode `0700` and `mktemp -u /tmp/ut_mktemp_dry.XXXXXX` prints a unique candidate path without creating it on disk.

## Not covered here (to avoid duplication)
- `ls`, `find`, and `mtree` are covered in `TS-FS-001`..`TS-FS-003`.
