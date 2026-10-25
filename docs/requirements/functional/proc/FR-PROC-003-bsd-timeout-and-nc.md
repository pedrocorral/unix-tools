---
id: FR-PROC-003
title: BSD timeout(1) and nc(1) utilities in Pure Ridgun
status: implemented
priority: should
created: 2026-10-21
updated: 2026-10-24
depends_on: [ADR-0001, ADR-0002]
verified_by: [TS-PROC-003]
---
# FR-PROC-003 — BSD `timeout(1)` and `nc(1)` Utilities in Pure Ridgun

## Statement
The project SHALL provide pure Ridgun implementations of BSD `timeout(1)` (`src/main/ridgun/timeout/main.rg`) and BSD `nc(1)` (`src/main/ridgun/nc/main.rg`) using real kernel process groups, signals, and TCP/UDP/Unix sockets.

## Functional Specification
1. **`timeout(1)`**:
   - Fractional durations (`ms`, `s`, `m`, `h`, `d`), `-s sig`, `-k kill_dur`, `--preserve-status`, `--foreground`.
   - When `--foreground` is not specified, places the child into a dedicated process group (`setpgid(0, 0)`) and signals the process group (`kill(-pid, sig)`) on timeout so child subprocesses are also terminated.
   - Exit codes: `124` on timeout, `128+sig` with `--preserve-status`, `125` on invalid usage, `126` on non-executable command, `127` on missing command.
2. **`nc(1)`**:
   - Real TCP client and `-l` server bidirectional stream transfer (`sys tcp listen`, `sys accept`, `sys tcp connect`, `sys send`, `sys recv`).
   - Real UDP (`-u` and `-lu`) datagram transfer over kernel `AF_INET` / `SOCK_DGRAM` sockets (never echoing payload in memory when no peer is present).
   - Real Unix domain socket (`-U` and `-lU`) bidirectional stream transfer over kernel `AF_UNIX` sockets, plus `-zU` zero-I/O probing.
   - Port scanning (`-z`, `-zv`) over single ports and port ranges (`start-end`), `-n` numeric IP enforcement, and `-w` timeout.

## Acceptance Criteria
- AC1: `timeout` enforces process deadlines and process-group termination in Pure Ridgun without `/usr/bin/timeout` or `/bin/sh`.
- AC2: `nc` performs real kernel TCP, UDP (`-u` / `-lu`), and Unix domain socket (`-U` / `-lU`) transfers and `-z` / `-zv` port range scans.
