---
id: TS-PROC-003
title: BSD timeout(1) deadline runner and nc(1) socket utility verification
level: functional
covers: [FR-PROC-003]
status: automated
automated_in: [src/test/ridgun/test_proc.rg, src/test/ridgun/test_nc_timeout.rg]
---
# TS-PROC-003 — BSD `timeout(1)` and `nc(1)` Verification

## Preconditions / fixtures
- Pure Ridgun `timeout` (`src/main/ridgun/timeout/main.rg`) and `nc` (`src/main/ridgun/nc/main.rg`) binaries and `proc_lib.rg` helpers consuming `io.rglib`, `posix.rglib`, and `socket.rglib`.

## Steps / inputs
1. Test `timeout` duration parsing (`250ms`, `0.1s`, `1.5s`, `2s`, `1m`, `1h`, `1d`), signal parsing (`-s TERM`, `-s SIGKILL`, `-s 9`, `-s 15`, invalid signal -> `125`), `-k` grace period, `--preserve-status` (`143` for `SIGTERM`, `137` for `SIGKILL`), `--foreground`, exit codes (`0`, child status `42`, timeout `124`, internal error `125`, non-executable `126`, command not found `127`), and verify it works when `PATH` has no `/usr/bin/timeout`.
2. Test `nc` option parsing, TCP client/server bidirectional stream transfer (`-l` and client mode with `stdin` payload), Unix domain socket (`-U` and `-lU`) stream transfer and `-zU` probe, UDP (`-u`) datagram transfer, single-port and port-range (`44120-44125`) `-zv` scanning, `-n` numeric-only enforcement, `-w` timeout, and security rejection of invalid ports and `../` Unix socket paths.

## Expected results (map to acceptance criteria)
- AC1 → `timeout` enforces process deadlines in Pure Ridgun without `/usr/bin/timeout`, returning exact POSIX/BSD exit codes (`0`, child exit code, `124`, `125`, `126`, `127`, `128+sig`).
- AC2 → `nc` performs real TCP, UDP (`-u`), and Unix socket (`-U`) transfers and `-z`/`-zv` port/range probes over `socket.rglib`.

## Not covered here (to avoid duplication)
- `apply`, `make`, and `sh` are covered in `TS-PROC-001` and `TS-PROC-002`.
