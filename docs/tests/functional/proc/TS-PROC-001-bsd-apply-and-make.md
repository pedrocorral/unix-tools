---
id: TS-PROC-001
title: BSD apply(1) command applicator and make(1) DAG dependency build tool
level: functional
covers: [FR-PROC-001]
status: automated
automated_in: [src/test/ridgun/test_proc.rg]
---
# TS-PROC-001 — BSD `apply(1)` and `make(1)` Verification

## Preconditions / fixtures
- Pure Ridgun `apply` and `make` implementations and sample `Makefile` definitions with variables, multi-target DAG dependencies, `.PHONY`, and intentional circular dependency cases.

## Steps / inputs
1. Run `apply -d -2 "diff %1 %2" a1 b1 a2 b2`, automatic `%N` arity inference, custom `-a` magic character, and live command execution.
2. Run `make -n` and live `make` on multi-target DAG `Makefile`s, variable expansions (`$(VAR)`, `${VAR}`, `?=`, CLI overrides), `.PHONY`, `-B`, and circular dependency graphs.

## Expected results (map to acceptance criteria)
- AC1 → `apply -d -2 "diff %1 %2" a1 b1 a2 b2` prints `diff a1 b1` and `diff a2 b2`.
- AC2 → `make -n` on a multi-target `Makefile` prints recipes in topological dependency order and rejects circular dependencies with an error.

## Not covered here (to avoid duplication)
- `sh` is covered in `TS-PROC-002`.
