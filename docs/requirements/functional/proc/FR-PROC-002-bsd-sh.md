---
id: FR-PROC-002
title: BSD sh(1) Almquist-style command interpreter in Pure Ridgun
status: implemented
priority: should
created: 2026-10-17
updated: 2026-10-18
depends_on: [ADR-0001, FR-PROC-001]
verified_by: [TS-PROC-002]
---
# FR-PROC-002 — BSD `sh(1)` (Almquist-Style Shell) in Pure Ridgun

## Statement
The project SHALL provide a pure Ridgun implementation of a BSD `sh(1)` (Almquist-style) command interpreter (`src/main/ridgun/sh/main.rg`) supporting interactive REPL usage, `-c` command execution, and `.sh` script execution.

## Functional Specification

### 1. Shell AST (`type ShNode`)
```ridgun
type Redir = RedirIn String | RedirOut String | RedirAppend String | RedirErrToOut
type ShNode = ShSimple [String] [Redir]
            | ShPipe ShNode ShNode
            | ShAnd ShNode ShNode
            | ShOr ShNode ShNode
            | ShSeq ShNode ShNode
            | ShSubshell ShNode
            | ShIf ShNode ShNode ShNode
            | ShWhile ShNode ShNode
            | ShFor String [String] ShNode
            | ShAssign String String
```

### 2. Core Features
- **Lexing & Quoting**: Single quotes (`'...'` literal), double quotes (`"..."` with `$VAR` expansion), backslash escaping, comments (`# ...`).
- **Variable & Parameter Expansion**: `$VAR`, `${VAR}`, `${VAR:-default}`, `$?` (last exit status), `$$`, positional parameters `$1`..`$9`, `$#`, `$@`.
- **Operators & Control Flow**:
  - Sequential execution (`;` and newline), short-circuit boolean lists (`&&`, `||`), pipelines (`cmd1 | cmd2`), and I/O redirections (`<`, `>`, `>>`, `2>&1`).
  - `if ...; then ...; [elif ...; then ...;] [else ...;] fi`, `for x in ...; do ...; done`, `while ...; do ...; done`.
- **Built-in Commands**: `cd`, `pwd`, `echo`, `export`, `unset`, `set`, `true`, `false`, `test` / `[`, `exit`, `.` (source).

## Acceptance Criteria
- AC1: `sh -c 'X=hello; echo "$X world"'` prints `hello world` and exits with `0`.
- AC2: Conditional lists (`true && echo ok`, `false || echo fallback`), pipelines, and `if`/`for`/`while` loops execute according to POSIX/BSD `sh` semantics.
