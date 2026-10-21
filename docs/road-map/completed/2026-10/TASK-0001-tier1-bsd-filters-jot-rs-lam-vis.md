---
id: TASK-0001
title: Implement Tier 1 BSD text & table filters (jot, rs, lam, vis, unvis)
status: completed
priority: high
created: 2026-10-17
updated: 2026-10-17
implements: [FR-FILTER-001, FR-FILTER-002]
tests: [TS-FILTER-001, TS-FILTER-002]
mitigates: []
depends_on: []
scope: [build.rg, src/main/ridgun, src/test/ridgun]
context_files:
  - docs/requirements/decisions/ADR-0001-adopt-aix-and-choose-stack.md
  - docs/requirements/functional/filter/FR-FILTER-001-bsd-jot-and-rs.md
  - docs/requirements/functional/filter/FR-FILTER-002-bsd-lam-and-vis.md
  - docs/tests/functional/filter/TS-FILTER-001-jot-and-rs.md
  - docs/tests/functional/filter/TS-FILTER-002-lam-and-vis.md
owner: agent-001
claimed: 2026-10-17T19:52:00Z
claimed_by: agent gemini@solarfall
---
# TASK-0001 — Implement Tier 1 BSD Text & Table Filters (`jot`, `rs`, `lam`, `vis`, `unvis`)

## Goal
Scaffold `build.rg` and implement the Tier 1 BSD utilities (`jot`, `rs`, `lam`, `vis`, `unvis`) in 100% pure Ridgun using `op ::` operators, ADTs, and `stable format`.

## Steps
1. Create `build.rg` at the repository root and shared CLI/string helpers in `src/main/ridgun/lib/`.
2. Implement `src/main/ridgun/jot/main.rg` and `src/main/ridgun/rs/main.rg` (`@implements FR-FILTER-001`).
3. Implement `src/main/ridgun/lam/main.rg`, `src/main/ridgun/vis/main.rg`, and `src/main/ridgun/unvis/main.rg` (`@implements FR-FILTER-002`).
4. Add automated Ridgun test suites under `src/test/ridgun/` and verify with `/home/boogie/ridgun/target/stable test` and `/home/boogie/ridgun/target/stable format`.
