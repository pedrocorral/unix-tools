---
id: TS-PROC-003
title: BSD timeout(1) deadline runner and nc(1) socket utility verification
level: functional
covers: [FR-PROC-003]
status: automated
automated_in: [src/test/ridgun/test_proc.rg]
---
# TS-PROC-003 — BSD `timeout(1)` and `nc(1)` Verification

## Preconditions / fixtures
- Pure Ridgun `timeout` (`src/main/ridgun/timeout/main.rg`) and `nc` (`src/main/ridgun/nc/main.rg`) binaries and `proc_lib.rg` helpers consuming `io.rglib`, `posix.rglib`, and `socket.rglib`.

## Steps / inputs
1. Parse duration strings (`50ms`, `2s`, `1m`) and execute `timeout 1s echo hello` (completes with `0`) and `timeout 50ms sleep 1` (times out with `124`).
2. Test `nc` option parsing, `socket.rglib` TCP/Unix listener and client connection (`-z` probe and payload transfer).

## Expected results (map to acceptance criteria)
- AC1 → `timeout 1s echo ok` prints `ok` and exits with `0`, whereas `timeout 50ms sleep 2` terminates the command and exits with `124`.
- AC2 → `nc -z 127.0.0.1 <port>` and `nc` stream transfers over `socket.rglib` validate endpoints, connect/listen, and transfer payload data accurately.

## Not covered here (to avoid duplication)
- `apply`, `make`, and `sh` are covered in `TS-PROC-001` and `TS-PROC-002`.
