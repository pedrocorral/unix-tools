---
id: TS-SEC-002
title: OpenBSD doas(1) and doas.conf(5) rule parser, -C audit mode, and direct execution
level: functional
covers: [FR-SEC-001]
status: automated
automated_in: [src/test/ridgun/test_sec.rg]
---
# TS-SEC-002 — OpenBSD `doas(1)` and `doas.conf(5)` Verification

## Preconditions / fixtures
- Valid and malformed `doas.conf` files exercising `permit`/`deny`, `nopass`, `nolog`, `persist`, `keepenv`, `setenv { ... }`, user and `:group` identities, `as <target>`, and `cmd <command> [args ...]`.

## Steps / inputs
1. Run `doas -C doas.conf` on valid and invalid configuration files and verify syntax error line reporting.
2. Run `doas -C doas.conf [-u target] cmd [args ...]` across multi-rule configurations to verify OpenBSD *"last matching rule wins"* semantics, exact `cmd args` matching, group `:wheel` matching, and environment sanitization.

## Expected results (map to acceptance criteria)
- AC1 → `doas -C doas.conf` validates rule syntax and reports line numbers on malformed rules.
- AC2 → `doas -C doas.conf -u root /sbin/reboot` prints `permit`, `permit nopass`, or `deny` according to OpenBSD *"last matching rule wins"* semantics.

## Not covered here (to avoid duplication)
- Non-privileged filters, filesystem tools, and process tools are covered in `TS-FILTER-*`, `TS-FS-*`, and `TS-PROC-*`.
