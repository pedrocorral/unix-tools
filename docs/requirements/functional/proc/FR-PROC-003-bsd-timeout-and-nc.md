---
id: FR-PROC-003
title: BSD timeout(1) and nc(1) process deadline and socket utilities in Pure Ridgun
status: implemented
priority: should
created: 2026-10-21
updated: 2026-10-24
depends_on: [ADR-0001]
verified_by: [TS-PROC-003]
---
# FR-PROC-003 — BSD `timeout(1)` and `nc(1)` in Pure Ridgun

## Statement
The project SHALL provide pure Ridgun implementations of BSD `timeout(1)` (`src/main/ridgun/timeout/main.rg`) using `io.rglib`/`posix.rglib` and BSD `nc(1)` (`src/main/ridgun/nc/main.rg`) using `socket.rglib`/`posix.rglib`, without delegating `timeout` execution to `/usr/bin/timeout`.

## Functional Specification
### 1. BSD `timeout(1)` (Pure Ridgun Process Deadline Supervisor)
- **Synopsis**: `timeout [-s sig | --signal=sig] [-k kill_dur | --kill-after=kill_dur] [--preserve-status] [--foreground] <duration> <command> [args...]`
- **Duration Parsing**:
  - Supports integer and floating-point / fractional durations (e.g., `0.1`, `0.25s`, `1.5s`, `250ms`, `2s`, `1m`, `1h`, `1d`).
  - A duration of `0` or `0s` disables the timeout (or immediately expires when negative/invalid; invalid duration string exits with `125`).
- **Signal Parsing (`-s` / `--signal`)**:
  - Accepts symbolic signal names with or without `SIG` prefix (`TERM`, `SIGTERM`, `KILL`, `SIGKILL`, `INT`, `SIGINT`, `HUP`, `SIGHUP`, `QUIT`, `SIGQUIT`, `ALRM`, `SIGALRM`, `USR1`, `SIGUSR1`, `USR2`, `SIGUSR2`) and numeric signal numbers (`1`, `2`, `3`, `9`, `10`, `12`, `14`, `15`).
  - Rejects unknown or out-of-range signal specifications with exit status `125`.
- **Exit Status Codes**:
  - `124`: Command timed out and `--preserve-status` was not specified.
  - `128 + sig_num` (e.g., `143` for `SIGTERM=15`, `137` for `SIGKILL=9`, `130` for `SIGINT=2`): Command timed out and `--preserve-status` was specified (or killed by `-k` grace escalation).
  - `125`: `timeout` itself failed (missing command, invalid option, invalid duration, invalid signal).
  - `126`: `<command>` was found on disk / `PATH` but is not executable (or is a directory).
  - `127`: `<command>` was not found on disk / `PATH`.
  - Otherwise: Exact exit status (`0`..`255`) of `<command>`.
- **Anti-Cheat Requirement**: `timeout` MUST execute and bound `<command>` using Ridgun `io.rglib`/`posix.rglib` process and deadline primitives (`with timeout`, `sys fork`/`sys kill` or `io` primitives) and MUST NEVER invoke `/usr/bin/timeout`.

### 2. BSD `nc(1)` (netcat)
- **Synopsis**: `nc [-l] [-u] [-U] [-z] [-v] [-n] [-k] [-w timeout_s] [-p source_port] [host] [port | port_start-port_end]`
- **Modes & Flags**:
  - **Zero-I/O Port & Socket Probe (`-z [-v]`)**:
    - Probes single TCP port (`nc -zv 127.0.0.1 44124`), inclusive port ranges (`nc -zv 127.0.0.1 44120-44125`), UDP endpoints (`-zu`), or Unix domain sockets (`nc -zU /tmp/test.sock`), reporting `succeeded!` or `failed` diagnostics when `-v` is enabled.
  - **TCP Client & Listen Mode (`-l`)**:
    - Client mode connects to `host:port` via `sys tcp connect`, sends `stdin` payload via `sys send`, shuts down write (`sys shutdown write`), and receives response data via `sys recv`.
    - Listen mode (`nc -l [host] <port>`) binds via `sys tcp listen reuseport`, accepts an incoming connection (`sys accept` bounded by `-w`), receives client data (`sys recv`), and sends `stdin` response payload (`sys send`).
  - **Unix Domain Stream Sockets (`-U`)**:
    - Supports both `-lU <socket_path>` server listen/accept/transfer and `-U <socket_path>` client connect/transfer via `sys unix listen` and `sys unix connect`, rejecting unsafe/traversal socket paths (`../`).
  - **UDP Mode (`-u`)**:
    - Supports UDP datagram binding, sending, and receiving via `socket.rglib` (`sock udp`, `sock bind`, `sock send to`, `sock deliver dgram`, `sock recv from`).
  - **`-n` (No DNS Lookup)**:
    - Rejects symbolic hostnames when `-n` is set, accepting only numeric IPv4/IPv6 addresses (or Unix socket paths with `-U`).
  - **Standard Input Integration**:
    - `nc/main.rg` reads non-blocking / EOF-bounded `stdin` (`fd 0`) when invoked in client or server data transfer mode.

## Acceptance Criteria
- AC1: `timeout` parses integer and fractional durations (`250ms`, `0.1s`, `1.5s`, `2s`, `1m`, `1h`, `1d`) and signal names/numbers (`-s TERM`, `-s 9`, `--signal=USR1`), returns `125` on invalid flags/signals/durations, `126` on non-executable targets, `127` on missing commands, `124` on timeout (or `128 + sig` with `--preserve-status`), and propagates the child's exact exit code when completed within the deadline without invoking `/usr/bin/timeout`.
- AC2: `nc` performs real TCP, UDP (`-u`), and Unix domain socket (`-U`) client/server bidirectional transfers and `-z` / `-zv` single-port and port-range (`start-end`) probes, honors `-n` (numeric-only IPs) and `-w` timeouts, and rejects invalid ports and path traversal socket paths.
