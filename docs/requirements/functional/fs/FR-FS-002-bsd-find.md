---
id: FR-FS-002
title: BSD find(1) directory tree boolean evaluator in Pure Ridgun
status: implemented
priority: must
created: 2026-10-17
updated: 2026-10-18
depends_on: [ADR-0001, FR-FS-001]
verified_by: [TS-FS-002]
---
# FR-FS-002 — BSD `find(1)` Directory Tree Boolean Evaluator in Pure Ridgun

## Statement
The project SHALL provide a pure Ridgun implementation of BSD `find(1)` (`src/main/ridgun/find/main.rg`) that parses command-line primaries and operators into an algebraic Boolean expression AST and evaluates it recursively across directory hierarchies.

## Functional Specification

### 1. Expression AST (`type FindExpr`)
```ridgun
type FindExpr = ETrue
              | EName String
              | EIname String
              | EPath String
              | EType String
              | ESize Int Int
              | EMaxDepth Int
              | EMinDepth Int
              | EEmpty
              | ENot FindExpr
              | EAnd FindExpr FindExpr
              | EOr FindExpr FindExpr
```
- **Primaries**:
  - `-name pattern` / `-iname pattern`: Shell glob matching (`*`, `?`, `[...]`) against the basename.
  - `-path pattern`: Glob matching against the full path.
  - `-type [f|d|l|p|s]`: Match file type (regular file, directory, symlink, FIFO, socket).
  - `-size [+|-]N[c|k|M|G]`: Match file size (greater than `+N`, less than `-N`, or exact `N`).
  - `-empty`: Match 0-byte regular files or empty directories.
  - `-maxdepth N` / `-mindepth N`: Control traversal depth.
- **Operators**:
  - `( expr )`, `! expr` / `-not expr`, `expr1 -and expr2` (or implicit juxtaposition), `expr1 -or expr2`.
- **Actions**:
  - `-print` (default when no action is specified), `-print0`, `-exec cmd {} ;`.

## Acceptance Criteria
- AC1: `find src -type f -name "*.rg"` lists all `.rg` files under `src` in deterministic traversal order.
- AC2: Compound boolean expressions with `-or`, `-and`, `-not`, and parentheses evaluate according to POSIX/BSD precedence.
