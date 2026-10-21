---
id: TS-PROC-002
title: BSD sh(1) Almquist-style command interpreter and shell AST evaluator
level: functional
covers: [FR-PROC-002]
status: automated
automated_in: [src/test/ridgun/test_proc.rg]
---
# TS-PROC-002 — BSD `sh(1)` Verification

## Preconditions / fixtures
- Pure Ridgun `sh` interpreter (`src/main/ridgun/sh/main.rg`) and shell scripts testing quoting, expansions, control flow, pipelines, redirections, and built-ins.

## Steps / inputs
1. Execute `sh -c 'X=hello; echo "$X world"'` and parameter expansions (`${VAR}`, `${VAR:-default}`, `$?`, `$#`, `$1`).
2. Execute conditional lists (`&&`, `||`, `;`), pipelines (`|`), redirections (`>`, `>>`, `<`, `2>&1`), `if/elif/else/fi`, `for/in/do/done`, `while/do/done`, and built-ins (`cd`, `pwd`, `export`, `unset`, `test`/`[`, `true`, `false`, `exit`, `.`).

## Expected results (map to acceptance criteria)
- AC1 → `sh -c 'X=hello; echo "$X world"'` prints `hello world` and exits with `0`.
- AC2 → Conditional lists (`true && echo ok`, `false || echo fallback`), pipelines, and `if`/`for`/`while` loops execute according to POSIX/BSD `sh` semantics.

## Not covered here (to avoid duplication)
- `apply` and `make` are covered in `TS-PROC-001`.
