---
id: TS-SEC-003
title: BSD sha256(1) cryptographic digest, HMAC-SHA256, and Argon2id verification
level: functional
covers: [FR-SEC-002]
status: automated
automated_in: [src/test/ridgun/test_sec.rg]
---
# TS-SEC-003 — BSD `sha256(1)` Verification

## Preconditions / fixtures
- Pure Ridgun `sha256` utility (`src/main/ridgun/sha256/main.rg`) and `sec_lib.rg` helpers backed by `crypto.rglib`.

## Steps / inputs
1. Evaluate `sha256 -s "abc"`, `sha256 -q -s "abc"`, `sha256 -r <file>`, and stdin mode.
2. Evaluate `sha256 -q -k "key" -s "The quick brown fox jumps over the lazy dog"` (RFC 4231 / standard HMAC-SHA256 vector) and Argon2id `--argon2id` / `--verify` roundtrip.

## Expected results (map to acceptance criteria)
- AC1 → `sha256 -s "abc"` outputs `SHA256 ("abc") = ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad`, and `-q` / `-r` format the digest accordingly.
- AC2 → `sha256 -q -k "secret" -s "msg"` computes the exact HMAC-SHA256 hex digest via `crypto.rglib`, and `sha256 --argon2id <salt> -s <pwd>` / `sha256 --verify <phc> -s <pwd>` hash and verify Argon2id PHC strings.

## Not covered here (to avoid duplication)
- `doas` and `doas.conf` are covered in `TS-SEC-002`.
