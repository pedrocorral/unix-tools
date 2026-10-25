---
id: TS-FILTER-002
title: BSD lam(1) line lamination and vis(1)/unvis(1) lossless encoding including HTTP (-h) and MIME (-m)
level: unit
covers: [FR-FILTER-002]
status: automated
automated_in: [src/test/ridgun/test_filter.rg]
---
# TS-FILTER-002 — BSD `lam(1)` and `vis(1)` / `unvis(1)` Verification

## Preconditions / fixtures
- Pure Ridgun implementations of `lam`, `vis`, and `unvis` logic accessible in `src/test/ridgun/test_filter.rg` and compiled binaries `target/lam`, `target/vis`, and `target/unvis`.

## Steps / inputs
1. Laminate multi-line inputs with `lam` using per-column `-s` / `-S` separator, `-f` / `-F` width padding/truncation, `-p` / `-P` exhausted-file padding, and `-t` / `-T` custom line terminators.
2. Encode strings containing printable characters, tabs, newlines, backslashes, control characters (`< 32`, `127`), raw bytes (`0..255`), octal mode (`-o`), newline marker (`-l`), line folding (`-F`), HTTP URI mode (`-h`, `%xx`), and MIME Quoted-Printable mode (`-m`, `=XX` and soft `=\n` breaks) with `vis`, then decode with `unvis` (including `unvis -h` and `unvis -m`).

## Expected results (map to acceptance criteria)
- AC1 → `lam` joins corresponding lines from each input with `" -> "` and respects `-s`/`-S`, `-f`/`-F`, `-p`/`-P`, and `-t`/`-T`.
- AC2 → For all tested strings $S$ across default, `-ctw`, `-ow`, `-l`, `-F`, `-h` (HTTP URI `%xx`), and `-m` (MIME Quoted-Printable `=XX`), `unvis(vis(S)) == S` holds 100% losslessly.

## Not covered here (to avoid duplication)
- `jot` and `rs` are covered in `TS-FILTER-001`.
