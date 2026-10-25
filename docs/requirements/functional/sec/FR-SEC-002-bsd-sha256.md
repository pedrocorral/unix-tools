---
id: FR-SEC-002
title: BSD sha256(1) cryptographic digest, HMAC-SHA256, checklist verifier, and Argon2id utility in Pure Ridgun
status: implemented
priority: should
created: 2026-10-21
updated: 2026-10-24
depends_on: [ADR-0001, ADR-0002]
verified_by: [TS-SEC-003]
---
# FR-SEC-002 — BSD `sha256(1)` Cryptographic Digest, HMAC, Checklist Verifier & Argon2id Utility

## Statement
The project SHALL provide a pure Ridgun implementation of BSD `sha256(1)` (`src/main/ridgun/sha256/main.rg`) backed by `crypto.rglib` and `posix.rglib` (`sha256 hex`, `hmac sha256`, `bytes to hex str`, `argon2id hash`, `argon2id verify`, `ct equal`).

## Functional Specification
- **Synopsis**: `sha256 [-q] [-r] [-x] [-t] [-c checklist] [-s string] [-k key] [--argon2id salt] [--verify phc] [file ...]`
- **Modes**:
  - Default BSD output: `SHA256 (<target>) = <digest>` (or `HMAC-SHA256 (<target>) = <digest>` when `-k <key>` is given).
  - `-r`: Reversed output format (`<digest>  <filename>`).
  - `-q`: Quiet mode — outputs only the hex digest (or PHC string).
  - `-s <string>`: Computes the digest or HMAC of `<string>` instead of a file.
  - `-c <checklist>`: Reads a file of BSD (`SHA256 (path) = hex`) or reversed (`hex  path` / `hex *path`) checksum lines, verifies each file on disk in constant time (`ct equal`), prints `<path>: OK` or `<path>: FAILED`, and exits `0` only if all files match (`1` if any file fails or is missing).
  - `-x`: Runs the built-in FIPS-180-4 SHA-256 self-test vector suite.
  - `-t`: Runs a throughput time-trial benchmark.
  - `-k <key>` / `--hmac <key>`: Computes RFC 2104 `HMAC-SHA256` using `hmac sha256` from `crypto.rglib`.
  - `--argon2id <salt>`: Computes an Argon2id PHC hash (`$argon2id$v=19$m=64,t=2,p=1$...`) of the input using `argon2id hash` from `crypto.rglib`.
  - `--verify <phc>`: Verifies the input against an Argon2id PHC string using `argon2id verify` (`0` on match, `1` on mismatch).

## Acceptance Criteria
- AC1: `sha256 -s "abc"` outputs `SHA256 ("abc") = ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad`, `-q` / `-r` / `-x` / `-t` format and verify digests accurately, and `sha256 -c checklist` validates both BSD and reversed checksum files (reporting `OK` / `FAILED` with non-zero exit on mismatch).
- AC2: `sha256 -q -k "secret" -s "msg"` computes the exact HMAC-SHA256 hex digest via `crypto.rglib`, and `sha256 --argon2id <salt> -s <pwd>` / `sha256 --verify <phc> -s <pwd>` hash and verify Argon2id PHC strings.
