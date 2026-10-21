---
id: FR-PROC-003
title: BSD timeout(1) and nc(1) process deadline and socket utilities in Pure Ridgun
status: implemented
priority: should
created: 2026-10-21
updated: 2026-10-21
depends_on: [ADR-0001]
verified_by: [TS-PROC-003]
---
# FR-PROC-003 — BSD `timeout(1)` and `nc(1)` in Pure Ridgun

## Statement
The project SHALL provide pure Ridgun implementations of BSD `timeout(1)` (`src/main/ridgun/timeout/main.rg`) using `io.rglib`/`posix.rglib` and BSD `nc(1)` (`src/main/ridgun/nc/main.rg`) using `socket.rglib`/`posix.rglib`.

## Functional Specification
### 1. BSD `timeout(1)`
- Synopsis: `timeout [-s sig] [-k kill_dur] [--preserve-status] <duration> <command> [args...]`
- Parses durations with optional `ms`, `s`, `m`, `h`, `d` suffixes into milliseconds and executes `<command> [args...]` bounded by `io exec timeout` / `with timeout`.
- Exits with `124` when the command times out, or with the command's exit status when it completes within the deadline.

### 2. BSD `nc(1)` (netcat)
- Synopsis: `nc [-l] [-u] [-U] [-z] [-v] [-w timeout_s] [host] [port]`
- Uses `socket.rglib` (`sock parse addr`, `sock resolve host`, `is valid port`, `sys tcp listen`, `sys tcp connect`, `sys unix listen`, `sys unix connect`, `sys send`, `sys recv`, `sock tcp`, `sock connect pair`, `sock send str`, `sock recv str`) to support:
  - Port scanning / zero-I/O probe (`-z [-v] <host> <port>`), reporting connection readiness or active listener status.
  - TCP and Unix-domain (`-U`) stream client/server transfer (`-l` listen mode and client mode).

## Acceptance Criteria
- AC1: `timeout 1s echo ok` prints `ok` and exits with `0`, whereas `timeout 50ms sleep 2` terminates the command and exits with `124`.
- AC2: `nc -z 127.0.0.1 <port>` and `nc` stream transfers over `socket.rglib` validate endpoints, connect/listen, and transfer payload data accurately.
