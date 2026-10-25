---
id: FR-FS-003
title: BSD mtree(8) directory hierarchy mapper and verifier in Pure Ridgun
status: implemented
priority: must
created: 2026-10-17
updated: 2026-10-24
depends_on: [ADR-0001, ADR-0002, FR-FS-001]
verified_by: [TS-FS-003]
---
# FR-FS-003 — BSD `mtree(8)` Directory Hierarchy Mapper & Verifier in Pure Ridgun

## Statement
The project SHALL provide a pure Ridgun implementation of BSD `mtree(8)` (`src/main/ridgun/mtree/main.rg`) capable of generating declarative directory hierarchy specifications (`-c`) and auditing or repairing live directory trees against a specification (`-f`, `-u`, `-e`, `-r`).

## Functional Specification

### 1. Modes of Operation
- **Create Specification (`mtree -c [-p path] [-K keywords] [-k keywords]`)**:
  - Walks the directory tree rooted at `path` (default `.`) and emits a canonical BSD `mtree` specification to stdout.
  - Supports `/set` defaults and per-entry keywords: `type=(dir|file|link)`, `mode=<octal>`, `size=<bytes>`, `time=<epoch>`, `uid=<uid>`, `uname=<user>`, `gid=<gid>`, `gname=<group>`, `sha256=<hex>` / `sha256digest=<hex>`, `link=<target>`.
- **Verify & Repair Hierarchy (`mtree [-f specfile] [-p path] [-u] [-e] [-r]`)**:
  - Parses `specfile` supporting `/set` default inheritance, `/unset` attribute clearing, relative path entries, and hierarchical directory blocks closed by `..`.
  - Compares the live filesystem at `path` against `specfile` and reports discrepancies (`missing`, `extra`, `type`, `size`, `permissions`, `time`, `sha256`, `link`).
  - `-u`: Creates missing directories and updates permissions to match the specification.
  - `-e`: Do not complain about extra files on disk that are not listed in the specification.
  - `-r`: Remove extra files and directories on disk that are not listed in the specification.

## Acceptance Criteria
- AC1: Running `mtree -c -p src > spec.mtree` followed immediately by `mtree -f spec.mtree -p src` exits with `0` and zero discrepancies, and `mtree` accurately parses `/set`, `/unset`, and `..` directory blocks.
- AC2: Modifying a file's contents, permissions, or `time`, or adding extra files under `path`, causes `mtree -f` to report the exact mismatch, `-e` suppresses extra-file warnings, `-r` removes extra files, and `-u` restores missing directories and permissions.
