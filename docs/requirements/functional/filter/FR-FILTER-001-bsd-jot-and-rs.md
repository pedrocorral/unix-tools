---
id: FR-FILTER-001
title: BSD jot(1) and rs(1) utilities in Pure Ridgun
status: implemented
priority: must
created: 2026-10-17
updated: 2026-10-24
depends_on: [ADR-0001, ADR-0002]
verified_by: [TS-FILTER-001]
---
# FR-FILTER-001 — BSD `jot(1)` and `rs(1)` Utilities in Pure Ridgun

## Statement
The project SHALL provide pure Ridgun implementations of the BSD `jot(1)` sequential/repetitive/random data generator (`src/main/ridgun/jot/main.rg`) and the BSD `rs(1)` data array reshaper (`src/main/ridgun/rs/main.rg`).

## Functional Specification

### 1. `jot` — Print Sequential, Repetitive, or Random Data (`jot(1)`)
- **Synopsis**: `jot [-cnr] [-b word] [-w word] [-s string] [-p precision] [reps [begin [end [s]]]]`
- **4-Tuple Parameter Inference**:
  - `jot` accepts up to 4 numeric arguments `(reps, begin, end, step)`. Any omitted argument (or `-` placeholder) is inferred mathematically from the remaining arguments using defaults `reps = 100`, `begin = 1`, `end = 100`, `step = 1`:
    - If `reps`, `begin`, and `end` are given, `step = (end - begin) / (reps - 1)`.
    - If `begin`, `end`, and `step` are given, `reps = ((end - begin) / step) + 1`.
    - If `reps`, `begin`, and `step` are given, `end = begin + step * (reps - 1)`.
- **Flags**:
  - `-b word`: Print `word` repeatedly `reps` times without numeric calculation.
  - `-w fmt`: Format output values using `fmt` (`%d`, `%i`, `%u`, `%o`, `%x`, `%X`, `%c`, `%.Nf`, width/zero-padding, left/right alignment, and `%%`).
  - `-c`: ASCII character output mode.
  - `-s sep`: Separate output items with `sep` instead of newline (`\n`).
  - `-n`: Do not print the final trailing newline.
  - `-p precision`: Explicit decimal precision for fixed-point sequences.
  - `-r`: Random number generation mode in `[begin..end]`. When an explicit seed is passed as the 4th positional argument, `jot -r` is deterministic for that seed; when no seed is passed, `jot -r` MUST draw real OS entropy via `sys random bytes list 4` (`getrandom` syscall) so independent invocations produce different random sequences.

### 2. `rs` — Reshape a Data Array (`rs(1)`)
- **Synopsis**: `rs [-t] [-e] [-k N] [-y] [-h] [-H] [-z] [-c[ch]] [-C[ch]] [-w width] [-g gutter] [-j] [rows [cols]]`
- **Behaviour**:
  - Reads whitespace-separated (or `-c<ch>` delimited, or `-e` line-per-entry) entries from a file or stdin into a flat list of strings, skipping `-k N` leading lines if specified.
  - Reshapes the list into a 2D grid of `rows x cols` (auto-inferring omitted dimensions or recycling entries when `-y` is passed).
  - `-t`: **Transpose** rows and columns before output.
  - `-h` / `-H`: Print array shape (`R by C`) only (`-h`) or header before grid (`-H`).
  - `-z`: Squeeze column widths to fit actual entries without uniform padding.
  - `-j`: Right-align cells within each column.

## Acceptance Criteria
- AC1: `jot 5 10 50` prints `10`, `20`, `30`, `40`, `50` on separate lines; `jot -s "," 4 1 4` prints `1,2,3,4`.
- AC2: `jot -b "ok" 3` prints `ok` three times; `jot -r` uses deterministic seeding when the 4th seed argument is provided and real kernel entropy (`sys random bytes list`) when omitted so unseeded runs differ.
- AC3: Given 6 input words `a b c d e f`, `rs 2 3` outputs a 2-row by 3-column aligned table (`a  b  c` / `d  e  f`), and `rs -t 2 3` outputs the transposed 3-row by 2-column table.
