---
id: TASK-0003
title: Implement Tier 3 BSD process, DAG build & shell tools (apply, make, sh)
status: completed
priority: medium
created: 2026-10-17
updated: 2026-10-17
implements: [FR-PROC-001, FR-PROC-002]
tests: []
mitigates: []
depends_on: [TASK-0002]
scope: [src/main/ridgun, src/test/ridgun]
context_files:
  - docs/requirements/decisions/ADR-0001-adopt-aix-and-choose-stack.md
  - docs/requirements/functional/proc/FR-PROC-001-bsd-apply-and-make.md
  - docs/requirements/functional/proc/FR-PROC-002-bsd-sh.md
owner: agent-001
claimed: 2026-10-18T14:10:40Z
claimed_by: agent gemini@solarfall
---
# TASK-0003 — Implement Tier 3 BSD Process, DAG Build & Shell Tools (`apply`, `make`, `sh`)

## Goal
Implement BSD `apply(1)`, BSD `make(1)`, and BSD `sh(1)` (Almquist-style shell) in 100% pure Ridgun.

## Steps
1. Implement `src/main/ridgun/apply/main.rg` and `src/main/ridgun/make/main.rg` (`@implements FR-PROC-001`).
2. Implement `src/main/ridgun/sh/main.rg` (`@implements FR-PROC-002`) with AST parsing, variable expansion, control flow, pipelines, redirections, built-ins, and interactive REPL.
3. Add test suites in `src/test/ridgun/` and format with `stable format`.
