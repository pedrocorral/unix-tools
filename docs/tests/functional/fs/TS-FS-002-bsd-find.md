---
id: TS-FS-002
title: BSD find(1) Boolean expression AST parser and directory tree evaluator
level: functional
covers: [FR-FS-002]
status: automated
automated_in: [src/test/ridgun/test_fs.rg]
---
# TS-FS-002 — BSD `find(1)` Verification

## Preconditions / fixtures
- Project `src/` tree and temporary test hierarchy with nested directories and files of varying sizes and types.

## Steps / inputs
1. Evaluate `find src -type f -name "*.rg"`.
2. Evaluate compound boolean expressions combining `-or`, `-and`, `-not`/`!`, parentheses `( ... )`, `-iname`, `-path`, `-size`, `-empty`, `-maxdepth`, `-mindepth`, `-print0`, and `-exec`.

## Expected results (map to acceptance criteria)
- AC1 → `find src -type f -name "*.rg"` lists all `.rg` files under `src` in deterministic traversal order.
- AC2 → Compound boolean expressions with `-or`, `-and`, `-not`, and parentheses evaluate according to POSIX/BSD precedence.

## Not covered here (to avoid duplication)
- `ls` and `mtree` are covered in `TS-FS-001` and `TS-FS-003`.
