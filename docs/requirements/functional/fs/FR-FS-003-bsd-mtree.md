---
id: FR-FS-003
title: BSD mtree(8) directory hierarchy mapper and verifier in Pure Ridgun
status: implemented
priority: must
created: 2026-10-17
updated: 2026-10-18
depends_on: [ADR-0001, FR-FS-001]
verified_by: [TS-FS-003]
---
# FR-FS-003 — BSD `mtree(8)` Directory Hierarchy Mapper & Verifier in Pure Ridgun

## Statement
The project SHALL provide a pure Ridgun implementation of BSD `mtree(8)` (`src/main/ridgun/mtree/main.rg`) capable of generating declarative directory hierarchy specifications (`-c`) and auditing live directory trees against a specification (`-f`).

## Functional Specification

### 1. Modes of Operation
- **Create Specification (`mtree -c [-p path] [-K keywords]`)**:
  - Walks the directory tree rooted at `path` (default `.`) and emits a canonical BSD `mtree` specification to stdout.
  - Supports `/set` defaults and per-entry keywords: `type=(dir|file|link)`, `mode=<octal>`, `size=<bytes>`, `sha256=<hex>`, `link=<target>`.
- **Verify Hierarchy (`mtree [-f specfile] [-p path] [-u]`)**:
  - Parses `specfile` (supporting `/set`, `/unset`, relative entries, and directory blocks) into a `MtreeSpec` ADT.
  - Compares the live filesystem at `path` against `specfile` and reports discrepancies:
    - `<path>: missing` (present in spec, absent on disk)
    - `<path>: extra` (present on disk, absent in spec)
    - `<path>: size (<expected>, <actual>)`
    - `<path>: permissions (<expected>, <actual>)`
    - `<path>: sha256 (<expected>, <actual>)`
  - Returns exit code `0` if the tree matches 100%, or non-zero if any mismatch is found.
  - With `-u`, creates missing directories and updates permissions to match the specification.

### 2. Parallel Verification with Ridgun `where`-DAGs
- File checksum (`sha256`) and attribute verification across independent subdirectories SHALL use Ridgun's `op ::` operators and `where` blocks.

## Acceptance Criteria
- AC1: Running `mtree -c -p src > spec.mtree` followed immediately by `mtree -f spec.mtree -p src` exits with `0` and zero discrepancies.
- AC2: Modifying a file's contents or permissions under `src` causes `mtree -f spec.mtree -p src` to report the exact modified file and attribute mismatch.
