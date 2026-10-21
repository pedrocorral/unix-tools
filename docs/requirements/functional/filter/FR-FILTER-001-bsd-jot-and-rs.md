---
id: FR-FILTER-001
title: BSD jot(1) and rs(1) utilities in Pure Ridgun
status: implemented
priority: must
created: 2026-10-17
updated: 2026-10-17
depends_on: [ADR-0001]
verified_by: [TS-FILTER-001]
---
# FR-FILTER-001 — BSD `jot(1)` and `rs(1)` Utilities in Pure Ridgun

## Statement
The project SHALL provide pure Ridgun implementations of the BSD `jot(1)` sequential/repetitive data generator (`src/main/ridgun/jot/main.rg`) and the BSD `rs(1)` data array reshaper (`src/main/ridgun/rs/main.rg`).

## Functional Specification

### 1. `jot` — Print Sequential or Repetitive Data (`jot(1)`)
- **Synopsis**: `jot [-cnr] [-b word] [-w word] [-s string] [reps [begin [end [s]]]]`
- **4-Tuple Parameter Inference**:
  - `jot` accepts up to 4 numeric arguments `(reps, begin, end, step)`. Any omitted argument (or `-` placeholder) is inferred mathematically from the remaining arguments using defaults `reps = 100`, `begin = 1`, `end = 100`, `step = 1`:
    - If `reps`, `begin`, and `end` are given, `step = (end - begin) / (reps - 1)`.
    - If `begin`, `end`, and `step` are given, `reps = ((end - begin) / step) + 1`.
    - If `reps`, `begin`, and `step` are given, `end = begin + step * (reps - 1)`.
- **Flags**:
  - `-b word`: Print `word` repeatedly `reps` times without numeric calculation.
  - `-w fmt`: Format output values using `fmt` prefix/suffix or `%d`/`%c`.
  - `-c`: ASCII character output mode (interpret `begin`/`end` as character codes or single-char strings and print characters).
  - `-s sep`: Separate output items with `sep` instead of newline (`\n`).
  - `-n`: Do not print the final trailing newline.
- **Ridgun Idioms**: Represent omitted parameters as an ADT (`type JotParam = Omitted | Given Int`) and resolve the 4-tuple via pattern matching and `op :: resolve jot tuple ...`.

### 2. `rs` — Reshape a Data Array (`rs(1)`)
- **Synopsis**: `rs [-t] [-c[ch]] [-C[ch]] [-w width] [-g gutter] [-j] [rows [cols]]`
- **Behaviour**:
  - Reads whitespace-separated (or `-c<ch>` delimited) entries from a file or stdin into a flat list of strings.
  - Reshapes the list into a 2D grid of `rows x cols` (if `rows == 0` or omitted, computes `rows = ceil(N / cols)`).
  - `-t`: **Transpose** rows and columns before output.
  - Column formatting: computes the maximum string width of each output column and pads cells with spaces (separated by `gutter` spaces, default `2`), or right-aligns cells when `-j` is passed, or joins with separator `<ch>` when `-C<ch>` is passed.

## Acceptance Criteria
- AC1: `jot 5 10 50` prints `10`, `20`, `30`, `40`, `50` on separate lines; `jot -s "," 4 1 4` prints `1,2,3,4`.
- AC2: `jot -b "ok" 3` prints `ok` three times.
- AC3: Given 6 input words `a b c d e f`, `rs 2 3` outputs a 2-row by 3-column aligned table (`a  b  c` / `d  e  f`), and `rs -t 2 3` outputs the transposed 3-row by 2-column table.
