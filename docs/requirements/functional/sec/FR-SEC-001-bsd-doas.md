---
id: FR-SEC-001
title: OpenBSD doas(1) and doas.conf(5) in Pure Ridgun
status: implemented
priority: could
created: 2026-10-17
updated: 2026-10-24
depends_on: [ADR-0001]
verified_by: [TS-SEC-002]
---
# FR-SEC-001 — OpenBSD `doas(1)` and `doas.conf(5)` in Pure Ridgun

## Statement
The project SHALL provide a pure Ridgun implementation of OpenBSD `doas(1)` (`src/main/ridgun/doas/main.rg` and `src/main/ridgun/lib/sec_lib.rg`) implementing the full `doas.conf(5)` grammar, the *"last matching rule wins"* policy engine, the `-C config` configuration audit mode, cryptographic password verification (`crypto.rglib`), and secure privilege execution.

## Functional Specification

### 1. `doas.conf(5)` Grammar & Policy Engine
- **Rule Syntax**:
  ```text
  permit|deny [nopass] [nolog] [persist] [keepenv] [setenv { [VAR ...] [VAR=val ...] [-VAR ...] [$VAR ...] }] <identity> [as <target>] [cmd <command> [args ...]]
  ```
  - `<identity>`: Username (e.g. `pedro`) or colon-prefixed group name (e.g. `:wheel`), supporting numeric UID/GID or symbolic names.
  - `as <target>`: Target user (default `root`).
  - `cmd <command>`: Command path; if `args ...` is specified, the invoked arguments must match `args ...` exactly (`args` with no tokens means the command may only be run with zero arguments). Supports double-quoted tokens (`"arg with spaces"`) and backslash escapes in `doas.conf`.
- **Evaluation Semantics**:
  - All rules in `doas.conf` are evaluated in order; the **last matching rule** determines whether execution is permitted or denied.
  - Comments start with `#`; blank lines and backslash-continued lines (`\` + `\n`) are supported; syntax errors cause immediate rejection of the configuration file with exact 1-based line numbers.

### 2. CLI Flags & Modes (`doas [-Lns] [-C config] [-u user] [cmd [args ...]]`)
- **`-C config` (Configuration Audit Mode)**:
  - With `-C config` only: Parses and validates `config`; exits `0` if valid, or prints `doas.conf:<line>: syntax error: <reason>` and exits `1`.
  - With `-C config [-u user] cmd [args ...]`: Evaluates the rules for the caller, target user (`-u user`, default `root`), `cmd`, and `args...`, printing:
    - `permit` (when permitted and password required)
    - `permit nopass` (when permitted without password)
    - `deny` (when no rule matches or last matching rule is `deny`)
- **`-n` (Non-Interactive Mode)**:
  - Fails immediately (`doas: Authentication required`) if the matching rule requires a password (i.e., `nopass` is not set).
- **`-s` (Shell Mode)**:
  - Executes the shell from `SHELL` or `/bin/sh` when `-s` is passed instead of an explicit command.
- **`-L` (Clear Persist Token)**:
  - Clears any persisted authentication token and exits `0`.

### 3. Security Invariants (Anti-Spoofing, Password Verification & Environment Sanitization)
- **Caller Identity Anti-Spoofing (`VUL-AUTHZ-001`)**:
  - In `-C` audit mode, `DOAS_CALLER` and `DOAS_GROUPS` may be inspected for non-destructive policy simulation.
  - In live execution mode (`without -C`), `doas` MUST resolve the caller identity strictly from the OS kernel (`sys getuid`, `/etc/passwd`, `/etc/group`) and MUST NEVER trust `DOAS_CALLER` or `DOAS_GROUPS` environment variables.
- **Cryptographic Password Verification (`VUL-AUTHN-001`)**:
  - When a matching `permit` rule does not specify `nopass`, `doas` MUST verify the supplied password against an Argon2id PHC hash or SHA-256 verifier (via `crypto.rglib` `argon2id verify` / `ct equal`) and MUST reject empty or incorrect passwords.
- **Environment Sanitization (`keepenv` & `setenv`, `VUL-INJ-001`)**:
  - Strips dangerous dynamic linker and shell injection variables (`LD_PRELOAD`, `LD_LIBRARY_PATH`, `LD_AUDIT`, `IFS`, `ENV`, `BASH_ENV`) even when `keepenv` is enabled, while preserving safe caller variables under `keepenv` and applying `setenv { VAR -REMOVE KEY=val }` mutations.
- **Direct Syscall Execution**:
  - Privileged command execution MUST use `sys setresgid`, `sys setresuid`, and `sys execve` directly and MUST NEVER pass user arguments to `/bin/sh -c`.

## Acceptance Criteria
- AC1: `doas -C doas.conf` validates rule syntax (including quoted arguments, line continuations, and `setenv` blocks) and reports exact line numbers on malformed rules.
- AC2: `doas -C doas.conf -u root /sbin/reboot` prints `permit`, `permit nopass`, or `deny` according to OpenBSD *"last matching rule wins"* semantics, and `doas` enforces `-n` non-interactive rejection, cryptographic password verification, caller identity anti-spoofing in live mode, and environment sanitization (`LD_PRELOAD` stripping).
