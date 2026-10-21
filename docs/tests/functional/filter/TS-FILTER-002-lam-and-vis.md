---
id: TS-FILTER-002
title: BSD lam(1) line lamination and vis(1)/unvis(1) lossless encoding
level: unit
covers: [FR-FILTER-002]
status: automated
automated_in: [src/test/ridgun/test_filter.rg]
---
# TS-FILTER-002 — BSD `lam(1)` and `vis(1)` / `unvis(1)` Verification

## Preconditions / fixtures
- Pure Ridgun implementations of `lam`, `vis`, and `unvis` logic accessible in `src/test/ridgun/test_filter.rg`.

## Steps / inputs
1. Laminate multi-line inputs with `lam` using per-column `-s " -> "` separator and `-f min.max` width padding.
2. Encode strings containing printable characters, tabs, newlines, backslashes, control characters (`< 32`, `127`), and octal mode (`-o`) with `vis`, then decode the result with `unvis`.

## Expected results (map to acceptance criteria)
- AC1 → `lam` joins corresponding lines from each input with `" -> "` and respects `-f min.max` padding/truncation.
- AC2 → For all tested strings $S$ (including escapes, control characters, backslashes, and octal mode), `unvis(vis(S)) == S` holds 100% losslessly.

## Not covered here (to avoid duplication)
- `jot` and `rs` are covered in `TS-FILTER-001`.
