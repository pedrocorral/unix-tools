---
id: TS-FILTER-001
title: BSD jot(1) 4-tuple sequence generation, entropy random mode, and rs(1) 2D array reshaping
level: unit
covers: [FR-FILTER-001]
status: automated
automated_in: [src/test/ridgun/test_filter.rg]
---
# TS-FILTER-001 — BSD `jot(1)` and `rs(1)` Verification

## Preconditions / fixtures
- Pure Ridgun implementations of `jot` and `rs` logic accessible in `src/test/ridgun/test_filter.rg` and compiled binaries `target/jot` and `target/rs`.

## Steps / inputs
1. Invoke `jot` evaluation with `5 10 50` and `-s "," 4 1 4`, as well as omitted parameters (`-` or fewer args), negative steps, and fixed-point precision (`-p N` and `%.Nf`).
2. Invoke `jot` evaluation with `-b "ok" 3`, `-w` printf specifiers (`%d`, `%i`, `%o`, `%x`, `%X`, `%c`, `%%`), and `-c` character mode.
3. Invoke `jot -r` both with an explicit 4th seed argument (`jot -r 5 10 99 42` deterministic) and without a seed (`jot -r 20 1 1000000` twice), verifying that unseeded invocations draw real kernel entropy (`sys random bytes list`) and produce distinct sequences while staying within `[begin..end]`.
4. Invoke `rs` evaluation on input `"a b c d e f"` with dimensions `2 3`, transposed `-t 2 3`, `-e`, `-k`, `-y`, `-h`, `-H`, `-z`, `-w`, `-g`, `-j`, `-c`, and `-C`.

## Expected results (map to acceptance criteria)
- AC1 → `jot 5 10 50` produces `10\n20\n30\n40\n50`; `jot -s "," 4 1 4` produces `1,2,3,4`.
- AC2 → `jot -b "ok" 3` produces `ok\nok\nok`; `jot -r` is deterministic when seeded and non-deterministic (real OS entropy) when unseeded.
- AC3 → `rs 2 3` on `a b c d e f` formats a 2-row by 3-column aligned grid (`a  b  c` / `d  e  f`); `rs -t 2 3` formats the transposed 3-row by 2-column grid (`a  d` / `b  e` / `c  f`).

## Not covered here (to avoid duplication)
- `lam`, `vis`, and `unvis` are covered in `TS-FILTER-002`.
