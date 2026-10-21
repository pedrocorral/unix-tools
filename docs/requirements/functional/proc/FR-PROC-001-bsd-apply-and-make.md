---
id: FR-PROC-001
title: BSD apply(1) and make(1) utilities in Pure Ridgun
status: implemented
priority: should
created: 2026-10-17
updated: 2026-10-18
depends_on: [ADR-0001]
verified_by: [TS-PROC-001]
---
# FR-PROC-001 — BSD `apply(1)` and `make(1)` Utilities in Pure Ridgun

## Statement
The project SHALL provide pure Ridgun implementations of BSD `apply(1)` (`src/main/ridgun/apply/main.rg`) and BSD `make(1)` (`src/main/ridgun/make/main.rg`).

## Functional Specification

### 1. `apply` — Apply a Command to a Set of Arguments (`apply(1)`)
- **Synopsis**: `apply [-a c] [-d] [-#] command args ...`
- **Behaviour**:
  - Divides `args ...` into groups of size `#` (default `1`, or inferred from the highest `%1`..`%9` placeholder in `command`).
  - Substitutes `%1`..`%9` (or `-a c` magic character) with the current chunk of arguments and executes `command` for each group.
  - `-d`: Display the constructed commands on stdout without executing them (dry-run).

### 2. `make` — Maintain Program Dependencies (`make(1)`)
- **Synopsis**: `make [-f makefile] [-n] [-B] [-j jobs] [VAR=val ...] [target ...]`
- **Behaviour**:
  - Parses `Makefile` variable assignments (`VAR = val`, `VAR ?= val`, `$(VAR)` / `${VAR}` expansion) and target rules (`target: dep1 dep2 ...` followed by tab-indented recipe lines).
  - Builds a Directed Acyclic Graph (DAG) of targets, detects dependency cycles, compares modification timestamps (`mtime`) of targets vs prerequisites, and executes recipes for out-of-date targets.
  - Supports `-n` (dry-run: print recipes without executing), `-B` (unconditionally rebuild all targets), and `.PHONY` targets.

## Acceptance Criteria
- AC1: `apply -d -2 "diff %1 %2" a1 b1 a2 b2` prints `diff a1 b1` and `diff a2 b2`.
- AC2: `make -n` on a multi-target `Makefile` prints recipes in topological dependency order and rejects circular dependencies with an error.
