---
id: TASK-0002
title: Implement Tier 2 BSD filesystem tools (ls, find, mtree)
status: completed
priority: high
created: 2026-10-17
updated: 2026-10-17
implements: [FR-FS-001, FR-FS-002, FR-FS-003]
tests: []
mitigates: []
depends_on: [TASK-0001]
scope: [src/main/ridgun, src/test/ridgun]
context_files:
  - docs/requirements/decisions/ADR-0001-adopt-aix-and-choose-stack.md
  - docs/requirements/functional/fs/FR-FS-001-bsd-ls.md
  - docs/requirements/functional/fs/FR-FS-002-bsd-find.md
  - docs/requirements/functional/fs/FR-FS-003-bsd-mtree.md
owner: agent-001
claimed: 2026-10-18T14:10:39Z
claimed_by: agent gemini@solarfall
---
# TASK-0002 — Implement Tier 2 BSD Filesystem Tools (`ls`, `find`, `mtree`)

## Goal
Implement BSD `ls(1)`, BSD `find(1)`, and BSD `mtree(8)` in 100% pure Ridgun.

## Steps
1. Implement shared filesystem metadata & directory entry helpers in `src/main/ridgun/lib/fs.rg` (extending `/home/boogie/ridgun` built-ins with `list_dir` / `file_stat` if needed).
2. Implement `src/main/ridgun/ls/main.rg` (`@implements FR-FS-001`) with multi-column grid, `-l`, `-F`, `-a`, `-A`, `-h`, `-S`, `-t`, `-r`, `-R`, and `-G` (`LSCOLORS`).
3. Implement `src/main/ridgun/find/main.rg` (`@implements FR-FS-002`) with Boolean AST parsing and evaluation.
4. Implement `src/main/ridgun/mtree/main.rg` (`@implements FR-FS-003`) with `-c` specification generation and `-f` verification.
