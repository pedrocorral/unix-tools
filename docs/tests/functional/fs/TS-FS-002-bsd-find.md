---
id: TS-FS-002
title: BSD find(1) Boolean AST, -L symlink traversal, -prune, -newer, -perm, -links, -inum, -regex, -delete, and batched -exec +
level: functional
covers: [FR-FS-002]
status: automated
automated_in: [src/test/ridgun/test_fs.rg]
---
# TS-FS-002 — BSD `find(1)` Verification

## Preconditions / fixtures
- Temporary test hierarchy with nested directories, symlinks to directories, hardlinked files, varying permissions (`0755`, `0644`, `0600`), timestamps, and sizes.

## Steps / inputs
1. Evaluate `find` with `-name`, `-iname`, `-path`, `-regex`, `-type`, `-size`, `-empty`, `-maxdepth`, `-mindepth`, `-perm` (exact `0644`, `-0111`, `/0222`), `-newer`, `-mtime`, `-user`, `-group`, `-links`, `-inum`, and `-L` (following symlinks).
2. Evaluate compound boolean expressions combining `-or`, `-and`, `-not`/`!`, parentheses `( ... )`, `-prune` (skipping pruned subdirectories), `-delete`, `-print0`, per-file `-exec cmd {} ;`, and batched `-exec cmd {} +`.

## Expected results (map to acceptance criteria)
- AC1 → `find` accurately filters by `-name` (including `[...]` character classes), `-regex`, `-type`, `-size`, `-perm`, `-newer`, `-links`, `-inum`, `-mtime`, `-user`, `-group`, `-L`, and `-delete`.
- AC2 → Compound boolean expressions, `-prune` subtree exclusion, per-file `-exec ... {} ;`, and single-batch `-exec ... {} +` evaluate according to POSIX/BSD semantics.

## Not covered here (to avoid duplication)
- `ls` and `mtree` are covered in `TS-FS-001` and `TS-FS-003`.
