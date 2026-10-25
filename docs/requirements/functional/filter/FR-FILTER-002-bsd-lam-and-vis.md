---
id: FR-FILTER-002
title: BSD lam(1) and vis(1)/unvis(1) utilities in Pure Ridgun
status: implemented
priority: should
created: 2026-10-17
updated: 2026-10-24
depends_on: [ADR-0001, ADR-0002]
verified_by: [TS-FILTER-002]
---
# FR-FILTER-002 — BSD `lam(1)` and `vis(1)` / `unvis(1)` Utilities in Pure Ridgun

## Statement
The project SHALL provide pure Ridgun implementations of the BSD `lam(1)` file laminator (`src/main/ridgun/lam/main.rg`) and the BSD `vis(1)` / `unvis(1)` reversible visual character encoders (`src/main/ridgun/vis/main.rg`, `src/main/ridgun/unvis/main.rg`).

## Functional Specification

### 1. `lam` — Laminate Files Line by Line (`lam(1)`)
- **Synopsis**: `lam [-s sepstring] [-S sepstring] [-f min.max] [-F min.max] [-p min.max] [-P min.max] [-t char] [-T char] file ...`
- **Behaviour**:
  - Reads corresponding lines from multiple files (and `-` for `stdin`) in parallel and concatenates them into a single output line until all input files are exhausted.
  - `-s sep` / `-S sep`: Prints literal string `sep` before the next file's line (`-s` one-shot, `-S` persistent).
  - `-f min.max` / `-F min.max`: Pads or truncates the next file's line to minimum width `min` and maximum width `max`.
  - `-p min.max` / `-P min.max`: Pads empty lines from exhausted files when other files still have lines.
  - `-t char` / `-T char`: Uses custom line terminator character `char` instead of `\n`.

### 2. `vis` and `unvis` — Reversible Visual Encoding (`vis(1)`, `unvis(1)`)
- **Synopsis**: `vis [-cbnostwlhm] [-F foldwidth] [file ...]` and `unvis [-hm] [file ...]`
- **Behaviour**:
  - `vis` converts non-printable characters and backslashes (`\` $\rightarrow$ `\\`) into unambiguous visual sequences:
    - C-style escapes (`-c` or standard): `\n`, `\t`, `\r`, `\b`, `\a`, `\v`, `\f`, `\\`, `\s` (`-w`), `\$` (`-l`).
    - Control characters: `\^A`..`\^_` and `\^?` (DEL).
    - Octal escapes (`-o`): `\040`, `\177`, etc.
    - HTTP URI RFC 3986 encoding (`-h`): encodes non-alphanumeric URI-unsafe bytes as `%xx` lowercase hexadecimal (`unvis -h` decodes `%xx` sequences).
    - MIME Quoted-Printable encoding (`-m`): encodes non-printable bytes and `=` as `=XX` uppercase hexadecimal (`unvis -m` decodes `=XX` sequences and `=\n` soft line breaks).
    - Folded lines (`-F width`): wraps output lines with `\` + `\n` soft line continuation.
  - `unvis` is the exact lossless inverse of `vis`: decoding any `vis`-encoded stream (including `-c`, `-o`, `-w`, `-l`, `-F`, `-h`, `-m`, and raw binary 0..255) back into the original byte stream.

## Acceptance Criteria
- AC1: Given two 3-line files `a.txt` and `b.txt`, `lam a.txt -s " -> " b.txt` joins each line pair with `" -> "`.
- AC2: For any input string $S$, `unvis(vis(S)) == S` holds 100% losslessly across default, `-ctw`, `-ow`, `-l`, `-F`, `-h` (HTTP URI `%xx`), and `-m` (MIME Quoted-Printable `=XX`) modes.
