---
id: TS-FS-003
title: BSD mtree(8) specification generation (-c), /set & /unset directives, .. blocks, auditing (-f, -e), and repair (-u, -r)
level: functional
covers: [FR-FS-003]
status: automated
automated_in: [src/test/ridgun/test_fs.rg]
---
# TS-FS-003 — BSD `mtree(8)` Verification

## Preconditions / fixtures
- Temporary directory tree with subdirectories, regular files, symlinks, permissions, modification times, and SHA-256 digests.

## Steps / inputs
1. Generate specification via `mtree -c -p <dir> > spec.mtree` and immediately verify via `mtree -f spec.mtree -p <dir>`.
2. Test specifications using `/set` default attributes, `/unset` attribute resets, and hierarchical directory blocks closed by `..`, as well as `time=<epoch>` keyword checks.
3. Modify a file's contents/size, permissions, `time`, add an extra file, and remove a file; verify `mtree -f` reports `size`, `permissions`, `time`, `sha256`, `extra`, and `missing` discrepancies, `-e` ignores `extra` files, `-r` removes `extra` files from disk, and `-u` restores missing directories and permissions.

## Expected results (map to acceptance criteria)
- AC1 → `mtree -c -p <dir>` followed by `mtree -f spec.mtree -p <dir>` exits with `0` and zero discrepancies, and `/set`, `/unset`, and `..` directory blocks are parsed and applied accurately.
- AC2 → Attribute mismatches (`size`, `permissions`, `time`, `sha256`, `missing`, `extra`) are reported accurately, `-e` suppresses extra-file errors, `-r` deletes extra files, and `-u` repairs permissions and missing directories.

## Not covered here (to avoid duplication)
- `ls` and `find` are covered in `TS-FS-001` and `TS-FS-002`.
