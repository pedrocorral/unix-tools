---
id: TS-PROC-003
title: BSD timeout(1) deadline & process-group runner and nc(1) real TCP, UDP, and Unix socket transfer verification
level: functional
covers: [FR-PROC-003]
status: automated
automated_in: [src/test/ridgun/test_proc.rg, src/test/ridgun/test_nc_timeout.rg]
---
# TS-PROC-003 — BSD `timeout(1)` and `nc(1)` Verification

## Preconditions / fixtures
- Pure Ridgun `timeout` (`src/main/ridgun/timeout/main.rg`) and `nc` (`src/main/ridgun/nc/main.rg`) binaries and `proc_lib.rg` helpers.

## Steps / inputs
1. Test `timeout` fractional durations (`ms`, `s`, `m`, `h`, `d`), signals (`-s TERM`, `-s 9`), `-k` kill-after escalation, `--preserve-status`, `--foreground`, process-group termination of spawned child/grandchild processes (`setpgid`), exit codes (`0`, child exit code, `124`, `125`, `126`, `127`, `128+sig`), and operation without `/usr/bin/timeout` or `/bin/sh`.
2. Test `nc` real two-process TCP client/server transfer (`-l` and client), real two-process UDP client/server datagram transfer (`-lu` and `-u`, plus verifying `-u` to an closed port does not fake an in-memory echo), real two-process Unix domain socket stream transfer (`-lU` server and `-U` client), `-zU` Unix probe, single-port and port-range `-zv` scanning, and `-n` numeric IP enforcement.

## Expected results (map to acceptance criteria)
- AC1 → `timeout` enforces process deadlines and process-group signaling in Pure Ridgun with exact exit codes.
- AC2 → `nc` performs real kernel TCP, UDP (`-u` / `-lu`), and Unix domain socket (`-U` / `-lU`) transfers and `-z` / `-zv` port range scans.

## Not covered here (to avoid duplication)
- `apply`, `make`, and `sh` are covered in `TS-PROC-001` and `TS-PROC-002`.
