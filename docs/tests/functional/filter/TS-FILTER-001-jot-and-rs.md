---
id: TS-FILTER-001
title: BSD jot(1) 4-tuple sequence generation and rs(1) 2D array reshaping
level: unit
covers: [FR-FILTER-001]
status: automated
automated_in: [src/test/ridgun/test_filter.rg]
---
# TS-FILTER-001 — BSD `jot(1)` and `rs(1)` Verification

## Preconditions / fixtures
- Pure Ridgun implementations of `jot` and `rs` logic accessible in `src/test/ridgun/test_filter.rg`.

## Steps / inputs
1. Invoke `jot` evaluation with `5 10 50` and `-s "," 4 1 4`, as well as omitted parameters (`-` or fewer args).
2. Invoke `jot` evaluation with `-b "ok" 3`, `-w "item_%d"`, and `-c` character mode.
3. Invoke `rs` evaluation on input `"a b c d e f"` with dimensions `2 3` and transposed `-t 2 3`, plus `-j` right-alignment and `-C` custom output separator.

## Expected results (map to acceptance criteria)
- AC1 → `jot 5 10 50` produces `10\n20\n30\n40\n50`; `jot -s "," 4 1 4` produces `1,2,3,4`.
- AC2 → `jot -b "ok" 3` produces `ok\nok\nok`.
- AC3 → `rs 2 3` on `a b c d e f` formats a 2-row by 3-column aligned grid (`a  b  c` / `d  e  f`); `rs -t 2 3` formats the transposed 3-row by 2-column grid (`a  d` / `b  e` / `c  f`).

## Not covered here (to avoid duplication)
- `lam`, `vis`, and `unvis` are covered in `TS-FILTER-002`.
