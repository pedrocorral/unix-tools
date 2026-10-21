---
id: TASK-0005
title: Implement mdrr(1) Terminal Markdown Reader in Pure Ridgun
status: completed
created: 2026-10-21
completed: 2026-10-21
requirements: [FR-FILTER-003]
tests: [TS-FILTER-003]
security: []
context_files:
  - docs/requirements/functional/filter/FR-FILTER-003-mdrr-markdown-reader.md
  - docs/tests/functional/filter/TS-FILTER-003-mdrr-markdown-reader.md
  - src/main/ridgun/lib/md_lib.rg
  - src/main/ridgun/mdrr/main.rg
  - src/test/ridgun/test_md.rg
scope:
  - src/main/ridgun/lib/md_lib.rg
  - src/main/ridgun/mdrr/main.rg
  - src/test/ridgun/test_md.rg
owner: agent-001
claimed: 2026-10-21T08:56:31Z
claimed_by: agent gemini@solarfall
---
# TASK-0005 — Implement `mdrr(1)` Terminal Markdown Reader in Pure Ridgun

## Goal (one sentence)
Build `mdrr` (**MD Ridgun Reader**), a pure Ridgun terminal Markdown parser, formatter, GFM table/code/alert renderer, TOC extractor, and section reader (`target/mdrr`).

## Plan
- [x] Specify `FR-FILTER-003` and `TS-FILTER-003` and update domain `INDEX.md` files.
- [x] Implement `src/main/ridgun/lib/md_lib.rg` (AST parser, inline formatter, ANSI/ASCII/plain renderer, GFM table alignment, code syntax highlighter, GitHub alerts, TOC `-t`, section filter `-s`, visible-width word wrapper).
- [x] Implement `src/main/ridgun/mdrr/main.rg` CLI entrypoint.
- [x] Implement `src/test/ridgun/test_md.rg` unit and E2E binary test suite (`TS-FILTER-003`).
- [x] Verify with `stable check`, `stable built`, `stable test`, `aix docs validate`, and `aix docs coverage`.

## Progress log (append-only, newest last)
- 2026-10-21: Created `FR-FILTER-003` and `TS-FILTER-003`; starting implementation of `md_lib.rg`, `mdrr/main.rg`, and `test_md.rg`.
- 2026-10-21: Implemented `src/main/ridgun/lib/md_lib.rg`, `src/main/ridgun/mdrr/main.rg`, and `src/test/ridgun/test_md.rg`; compiled `target/mdrr` and verified all 5 test suites (216/216 assertions passing).

## Files changed (keep current)
- `docs/requirements/functional/filter/FR-FILTER-003-mdrr-markdown-reader.md`
- `docs/requirements/functional/filter/INDEX.md`
- `docs/requirements/functional/INDEX.md`
- `docs/tests/functional/filter/TS-FILTER-003-mdrr-markdown-reader.md`
- `docs/tests/functional/filter/INDEX.md`
- `docs/tests/functional/INDEX.md`
- `src/main/ridgun/lib/md_lib.rg`
- `src/main/ridgun/mdrr/main.rg`
- `src/test/ridgun/test_md.rg`

## Verification performed (command → result, newest last)
- `stable check` → `Checked 19 module(s) — 0 errors.`
- `stable built` → `Built native ELF executable: target/mdrr` (13/13 binaries built)
- `stable test` → `Passed 5/5 test suite(s) — 0 failures` (30/30 assertions in `test_md.rg`, 216/216 total)

## Exact next actions
1. None — task completed.

## Decisions / open questions
- None.

## Definition of done checklist
- [x] requirements referenced are `implemented`  - [x] tests automated & green
- [x] security entries updated  - [x] docs INDEX updated  - [x] coverage matrix regenerated
