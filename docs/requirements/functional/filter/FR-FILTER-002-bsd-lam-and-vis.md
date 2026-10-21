---
id: FR-FILTER-002
title: BSD lam(1) and vis(1)/unvis(1) utilities in Pure Ridgun
status: implemented
priority: should
created: 2026-10-17
updated: 2026-10-17
depends_on: [ADR-0001]
verified_by: [TS-FILTER-002]
---
# FR-FILTER-002 — BSD `lam(1)` and `vis(1)` / `unvis(1)` Utilities in Pure Ridgun

## Statement
The project SHALL provide pure Ridgun implementations of the BSD `lam(1)` file laminator (`src/main/ridgun/lam/main.rg`) and the BSD `vis(1)` / `unvis(1)` reversible visual character encoders (`src/main/ridgun/vis/main.rg`, `src/main/ridgun/unvis/main.rg`).

## Functional Specification

### 1. `lam` — Laminate Files Line by Line (`lam(1)`)
- **Synopsis**: `lam [-s sepstring] [-f min.max] file ...`
- **Behaviour**:
  - Reads corresponding lines from multiple files in parallel and concatenates them into a single output line until all input files are exhausted.
  - `-s sep`: Prints literal string `sep` before the next file's line (unlike POSIX `paste`, `sep` can be an arbitrary multi-character string and can vary per file position).
  - `-f min.max`: Pads or truncates the next file's line to minimum width `min` and maximum width `max` (left-padded with spaces if `min` is positive, right-padded if `-min`).

### 2. `vis` and `unvis` — Reversible Visual Encoding (`vis(1)`, `unvis(1)`)
- **Synopsis**: `vis [-cbostw] [file ...]` and `unvis [file ...]`
- **Behaviour**:
  - `vis` converts non-printable characters (ASCII `< 32` except space/tab/newline unless `-t`/`-w`/`-s` are specified, `127` DEL, and `>= 128`) and backslashes (`\` $\rightarrow$ `\\`) into unambiguous visual sequences:
    - C-style escapes (`-c` or standard): `\n`, `\t`, `\r`, `\b`, `\a`, `\v`, `\f`, `\\`.
    - Control characters: `\^A`..`\^_` and `\^?` (DEL).
    - Octal escapes (`-o`): `\040`, `\177`, etc.
  - `unvis` is the exact lossless inverse of `vis`: decoding any `vis`-encoded stream back into the original raw string.

## Acceptance Criteria
- AC1: Given two 3-line files `a.txt` and `b.txt`, `lam a.txt -s " -> " b.txt` joins each line pair with `" -> "`.
- AC2: For any input string $S$, `unvis(vis(S)) == S` holds 100% losslessly.
