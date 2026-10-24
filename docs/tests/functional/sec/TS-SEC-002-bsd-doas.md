---
id: TS-SEC-002
title: OpenBSD doas(1) and doas.conf(5) rule parser, -C audit mode, password auth, and anti-spoofing
level: functional
covers: [FR-SEC-001]
status: automated
automated_in: [src/test/ridgun/test_sec.rg]
---
# TS-SEC-002 — OpenBSD `doas(1)` and `doas.conf(5)` Verification

## Preconditions / fixtures
- Valid and malformed `doas.conf` files exercising `permit`/`deny`, `nopass`, `nolog`, `persist`, `keepenv`, `setenv { ... }`, user and `:group` identities, `as <target>`, quoted `cmd` arguments, line continuations, Argon2id/SHA-256 password verifiers, and environment sanitization.

## Steps / inputs
1. Run `doas -C doas.conf` on valid and invalid configuration files (including quoted strings, backslash line continuations, and malformed syntax) and verify exact syntax error line reporting.
2. Run `doas -C doas.conf [-u target] cmd [args ...]` across multi-rule configurations to verify OpenBSD *"last matching rule wins"* semantics, exact `cmd args` matching, multi-group `:wheel` matching, and default-deny behavior.
3. Verify security invariants: password verification rejects empty/wrong passwords and accepts valid Argon2id/SHA-256 passwords, `-n` non-interactive mode rejects rules requiring a password, live execution mode ignores spoofed `DOAS_CALLER`/`DOAS_GROUPS` environment variables, and `keepenv`/`setenv` strips `LD_PRELOAD`, `LD_LIBRARY_PATH`, and `IFS`.

## Expected results (map to acceptance criteria)
- AC1 → `doas -C doas.conf` validates rule syntax and reports exact line numbers on malformed rules.
- AC2 → `doas` evaluates OpenBSD *"last matching rule wins"* policy, enforces cryptographic password verification and `-n` non-interactive checks, blocks caller env-var spoofing in live mode, and sanitizes the execution environment.

## Not covered here (to avoid duplication)
- `sha256` is covered in `TS-SEC-003`.
