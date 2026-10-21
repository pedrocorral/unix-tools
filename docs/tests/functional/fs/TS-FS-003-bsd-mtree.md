---
id: TS-FS-003
title: BSD mtree(8) specification generation (-c), auditing (-f), and remediation (-u)
level: functional
covers: [FR-FS-003]
status: automated
automated_in: [src/test/ridgun/test_fs.rg]
---
# TS-FS-003 — BSD `mtree(8)` Verification

## Preconditions / fixtures
- Temporary directory tree with subdirectories, regular files, permissions, and SHA-256 digests.

## Steps / inputs
1. Generate specification via `mtree -c -p <dir> > spec.mtree` and immediately verify via `mtree -f spec.mtree -p <dir>`.
2. Modify a file's contents/size, permissions, add an extra file, and remove a file; verify `mtree -f` reports `size`, `permissions`, `sha256`, `extra`, and `missing` discrepancies and exits non-zero.
3. Run `mtree -u -f spec.mtree -p <dir>` to restore missing directories and permissions.

## Expected results (map to acceptance criteria)
- AC1 → `mtree -c -p <dir>` followed by `mtree -f spec.mtree -p <dir>` exits with `0` and zero discrepancies.
- AC2 → Modifying file contents or permissions causes `mtree -f` to report the exact modified file and attribute mismatch and exit non-zero.

## Not covered here (to avoid duplication)
- `ls` and `find` are covered in `TS-FS-001` and `TS-FS-002`.
