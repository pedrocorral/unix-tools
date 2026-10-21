---
id: TS-FILTER-003
title: mdrr(1) terminal Markdown parser, renderer, GFM tables, alerts, TOC, and section filter
level: unit
covers: [FR-FILTER-003]
status: automated
automated_in: [src/test/ridgun/test_md.rg]
---
# TS-FILTER-003 — `mdrr(1)` Terminal Markdown Formatter & Reader Verification

## Preconditions / fixtures
- Pure Ridgun implementation of `md_lib` (`src/main/ridgun/lib/md_lib.rg`) and compiled binary `target/mdrr` (`src/main/ridgun/mdrr/main.rg`), tested in `src/test/ridgun/test_md.rg`.

## Steps / inputs
1. Render Markdown documents containing YAML front-matter, `#`–`###` headings, inline formatting (`**bold**`, `*italic*`, `~~strike~~`, `` `code` ``, `[link](https://...)`), unordered/ordered/task lists (`- [ ]`, `- [x]`), blockquotes, and GitHub alerts (`> [!NOTE]`, `> [!WARNING]`) in both default ANSI mode and `-p` plain mode.
2. Render GFM tables with left (`:---`), center (`:---:`), and right (`---:`) column alignments, and fenced code blocks with `-n` line numbers and `-a` ASCII box mode.
3. Run `mdrr -t` to generate a Table of Contents and `mdrr -s` to extract a specific heading section, both via unit calls and E2E execution of `./target/mdrr`.

## Expected results (map to acceptance criteria)
- AC1 → Inline spans, headings, task lists, blockquotes, and GitHub alerts render with ANSI SGR sequences by default and zero ESC (`\x1b`) bytes when `-p` is passed, while wrapping paragraphs to `-w` visible width.
- AC2 → GFM tables align cell text according to column specifiers (`:---`, `:---:`, `---:`), and fenced code blocks render language headers and 1-based line numbers (`-n`) in Unicode or ASCII (`-a`) frames.
- AC3 → `mdrr -t` lists all document headings with level indentation, and `mdrr -s` outputs only the matching section (and its nested subheadings) until the next peer/higher heading.

## Not covered here (to avoid duplication)
- `jot`, `rs`, `lam`, `vis`, and `unvis` are covered in `TS-FILTER-001` and `TS-FILTER-002`.
