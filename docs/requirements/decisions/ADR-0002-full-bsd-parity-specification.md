---
id: ADR-0002
title: Full BSD/POSIX Parity Specification and Elimination of All In-Memory Shortcuts Across All 17 Utilities
status: accepted
date: 2026-10-24
deciders: [pedro, agent]
affects:
  - FR-FILTER-001
  - FR-FILTER-002
  - FR-FILTER-003
  - FR-FS-001
  - FR-FS-002
  - FR-FS-003
  - FR-FS-004
  - FR-PROC-001
  - FR-PROC-002
  - FR-PROC-003
  - FR-SEC-001
  - FR-SEC-002
  - TS-FILTER-001
  - TS-FILTER-002
  - TS-FILTER-003
  - TS-FS-001
  - TS-FS-002
  - TS-FS-003
  - TS-FS-004
  - TS-PROC-001
  - TS-PROC-002
  - TS-PROC-003
  - TS-SEC-002
  - TS-SEC-003
supersedes: []
---
# ADR-0002 — Full BSD/POSIX Parity Specification and Elimination of All In-Memory Shortcuts Across All 17 Utilities

## Context
An exhaustive audit of the 17 Pure Ridgun BSD/OpenBSD utilities (`jot`, `rs`, `lam`, `vis`, `unvis`, `mdrr`, `ls`, `find`, `mtree`, `mktemp`, `apply`, `make`, `sh`, `timeout`, `nc`, `doas`, `sha256`) identified specific shortcuts, artificial caps, simulated in-memory behaviors, and missing BSD/POSIX features that must be documented in full detail, covered by multi-case unit, E2E, and combinational tests, and implemented in Pure Ridgun.

## Complete Audit & Detailed Requirements Specification

### 1. `nc(1)` — Netcat (`FR-PROC-003`, `TS-PROC-003`)
- **Gap 1.1 (UDP Transfer)**: `nc -u` previously echoed the payload back in memory `(0, payload)` when not in `-z` scan mode.
  - **Requirement**: `nc -u` MUST use real Linux kernel `AF_INET` / `SOCK_DGRAM` sockets (`sys udp bind`, `sys call 41 2 2 0` `socket`, `sys call 42` `connect`, `sys send` / `sys recv` / `sys poll`) to transmit and receive real UDP datagrams over the kernel network stack. Connecting/sending to an closed/unbound UDP port without a peer MUST NOT synthesize an in-memory echo.
- **Gap 1.2 (Unix Domain Socket Stream Transfer)**: `nc -U` previously only supported `-zU` zero-I/O scanning.
  - **Requirement**: `nc -lU <path>` (server) and `nc -U <path>` (client) MUST perform real bidirectional stream transfer over kernel `AF_UNIX` (`SOCK_STREAM`) sockets (`sys unix listen`, `sys accept`, `sys unix connect`, `sys send`, `sys recv`).
- **Gap 1.3 (Multi-Chunk Streaming Receive)**:
  - **Requirement**: `nc` MUST read incoming stream data in a loop until EOF or poll timeout, rather than stopping after a single `sys recv` call.

### 2. `sh(1)` — Almquist POSIX/BSD Shell (`FR-PROC-002`, `TS-PROC-002`)
- **Gap 2.1 (Hardcoded `cat` Shortcut)**:
  - **Requirement**: `sh` MUST NOT intercept `cat` as an internal built-in; `cat` (with or without arguments) MUST execute as a real external binary via `posix pipe` + `posix fork` + `posix dup2` + `posix execve` + `posix waitpid`.
- **Gap 2.2 (1,000-Iteration Loop Cap)**:
  - **Requirement**: `while` and `until` loops MUST NOT have an artificial 1,000-iteration cap (`steps >= 1000`); loops of 2,500+ iterations MUST execute until their condition terminates or `break`/`return`/`exit` is invoked.
- **Gap 2.3 (Subshell `( ... )` Process & CWD Isolation)**:
  - **Requirement**: Executing `( cd /tmp; VAR=inner; umask 077 )` MUST isolate the parent shell's working directory (`sys getcwd`), file mode creation mask (`umask`), variables, functions, and shell options (`set -e/-u/-f`).
- **Gap 2.4 (Bracket `[...]` Globbing & Subdirectory Globbing `dir/*.ext`)**:
  - **Requirement**: `sh glob match` MUST support character classes `[abc]`, inclusive ranges `[a-z]` / `[0-9]`, and negated classes `[!a-z]` / `[^a-z]` across pathname expansion, `case` patterns, and `${var#pat}` / `${var%pat}` stripping. Pathname globbing MUST support directory prefixes (`dir/*.txt`, `/tmp/sub/[a-c]?.rg`).
- **Gap 2.5 (Arithmetic `$((...))` Assignments, Increment/Decrement & Bitwise `~`)**:
  - **Requirement**: `$((...))` MUST support bitwise complement `~x`, variable assignments (`x = 5`, `x += 3`, `x -= 2`, `x *= 4`, `x /= 2`, `x %= 3`), and pre/post increment/decrement (`++x`, `--x`, `x++`, `x--`), persisting updated variable values in `ShEnv`.
- **Gap 2.6 (Per-Command Prefix Assignments `VAR=val cmd args...`)**:
  - **Requirement**: Prefix assignments before a command (`A=1 B=2 cmd`) MUST export `A=1` and `B=2` to `cmd`'s execution environment without mutating the caller shell's variable bindings after an external command completes, and multiple bare assignments on one line (`A=1 B=2`) MUST set all variables in `ShEnv`.
- **Gap 2.7 (Stderr Redirections `2>file`, `2>>file`, `>&2`, `1>&2`)**:
  - **Requirement**: `sh` MUST support redirecting file descriptor `2` to a file (`2>file` truncate, `2>>file` append) and duplicating `stdout` to `stderr` (`>&2` / `1>&2`) in both built-ins and external commands.
- **Gap 2.8 (Tilde `~` Expansion & Custom `IFS` Field Splitting)**:
  - **Requirement**: Unquoted leading `~` (`~` or `~/path`) MUST expand to `$HOME`. Unquoted parameter expansions `$VAR` MUST split into words using the characters in `$IFS` (default space/tab/newline; custom e.g. `IFS=:` or `IFS=,`).
- **Gap 2.9 (Built-ins `command`, `type`, `umask`, `getopts`, `exec`, Background `cmd &`, `$!`, `wait`, and Signal `trap`)**:
  - **Requirement**:
    - `command [-v] name [args...]`: resolves or runs `name` bypassing functions.
    - `type name...`: reports whether `name` is a shell builtin, function, or external path.
    - `umask [octal]`: queries or sets the OS process umask via Linux syscall `95` (`__NR_umask`).
    - `getopts optstring var [args...]`: parses short options (including clustered `-ab`, argument-taking `-f val` / `-fval`, unknown `?`, and missing `:`), updating `OPTIND` and `OPTARG`.
    - `exec [cmd args...]`: executes `cmd` and exits shell with its status (or applies descriptor redirections).
    - `cmd &`, `$!`, and `wait [pid]`: launches background jobs with `posix fork`, stores the child PID in `$!`, and reaps background jobs with `posix waitpid`.
    - `trap 'action' SIG...` / `trap - SIG...`: supports `EXIT`/`0`, `INT`/`2`, `TERM`/`15`, `HUP`/`1`, `QUIT`/`3` (including `kill -INT $$` self-signaling and `trap -` reset).

### 3. `doas(1)` — OpenBSD Privilege Escalation (`FR-SEC-001`, `TS-SEC-002`)
- **Gap 3.1 (`PATH` Resolution for Bare Commands)**:
  - **Requirement**: When `cmd` does not contain `/`, `doas` MUST resolve `cmd` across the target `PATH` (`/bin:/sbin:/usr/bin:/usr/sbin:/usr/local/bin:/usr/local/sbin` or `PATH` in `setenv`/`keepenv`) for execution.
- **Gap 3.2 (Real `persist` Timestamp Ticket & `-L` Clearing)**:
  - **Requirement**: When a rule with `persist` is authenticated, `doas` MUST record a timestamped session ticket bound to the caller UID (`/tmp/.doas_persist_<uid>`) valid for 300 seconds. Subsequent `doas` calls matching a `persist` rule within 300s succeed without re-prompting; `doas -L` MUST delete the ticket on disk so subsequent calls require authentication again.
- **Gap 3.3 (Audit Logging & `nolog` Suppression)**:
  - **Requirement**: Permitted executions without `nolog` MUST append a structured OpenBSD audit entry (`doas: <caller> ran command <cmd> as <target> from <cwd>`) to the audit log sink (`DOAS_LOG_FILE`), while rules with `nolog` MUST suppress audit logging completely.

### 4. `ls(1)` — Directory Listing (`FR-FS-001`, `TS-FS-001`)
- **Gap 4.1 (Real Kernel `lstat` Metadata: `st_ino`, `st_nlink`, `st_uid`, `st_gid`, `st_blocks`)**:
  - **Requirement**: `ls` MUST read the real Linux x86_64 `struct stat` via raw `lstat` (`sys call 6`) to populate true hardlink counts (`st_nlink`), inode numbers (`st_ino`), numeric UID/GID (`st_uid`, `st_gid`), and 512-byte/1K block counts (`st_blocks`).
- **Gap 4.2 (Flags `-i`, `-n`, `-s`)**:
  - **Requirement**: `ls -i` MUST prepend each entry's real inode number; `ls -n` MUST format long listings with numeric UID and GID instead of user/group names; `ls -s` MUST prepend allocated block counts.

### 5. `find(1)` — Directory Tree Boolean Evaluator (`FR-FS-002`, `TS-FS-002`)
- **Gap 5.1 (Batched `-exec cmd {} +`, `-prune`, `-newer`, `-perm`, `-mtime`, `-links`, `-inum`, `-regex`, `-delete`, `-L`)**:
  - **Requirement**:
    - `-exec cmd {} +`: batches all matched paths into a single invocation of `cmd`.
    - `-prune`: prevents descending into the matched directory while returning true.
    - `-newer file`: matches entries with `mtime > mtime(file)`.
    - `-perm mode` / `-perm -mode` / `-perm /mode`: exact, all-bits, and any-bit permission matching.
    - `-mtime [+|-]N`, `-links [+|-]N`, `-inum N`, `-regex pat`, `-delete`, and `-L` (follow symlinks during directory walk).

### 6. `mtree(8)` — Hierarchy Mapper & Verifier (`FR-FS-003`, `TS-FS-003`)
- **Gap 6.1 (`/set`, `/unset`, `..` Relative Directory Blocks, `time`, `-e`, `-r`)**:
  - **Requirement**: `mtree` MUST parse `/set` and `/unset` default attribute directives, hierarchical directory blocks terminated by `..`, `time=<epoch>` modification time checks, `-e` (ignore extra files on disk), and `-r` (remove extra files/directories on disk not present in the specification).

### 7. `make(1)` — Build DAG Utility (`FR-PROC-001`, `TS-PROC-001`)
- **Gap 7.1 (Conditionals, `!=` Shell Assignment, Text Functions & `-e` Flag)**:
  - **Requirement**: `make` MUST support BSD `.if`/`.else`/`.endif` and GNU `ifdef`/`ifndef`/`ifeq`/`ifneq`/`else`/`endif` conditionals, `VAR != cmd` shell output assignment, built-in functions `$(shell cmd)`, `$(wildcard pat)`, `$(patsubst pat,repl,text)`, `$(subst from,to,text)`, and `-e` (environment variable precedence over Makefile assignments).

### 8. `timeout(1)` — Process Deadline Utility (`FR-PROC-003`, `TS-PROC-003`)
- **Gap 8.1 (Process Group Signaling `setpgid` vs `--foreground`)**:
  - **Requirement**: When `--foreground` is not set, `timeout` MUST place the child process into its own process group (`setpgid(0, 0)`, syscall `109`) and signal the process group (`kill(-pid, sig)`) upon deadline expiry so grandchild processes are also terminated.

### 9. `jot(1)`, `vis(1)`, `unvis(1)`, `mktemp(1)`, `sha256(1)` (`FR-FILTER-001`, `FR-FILTER-002`, `FR-FS-004`, `FR-SEC-002`)
- **Gap 9.1 (`jot -r` True Random Seed)**: When `-r` is passed without an explicit seed, `jot` MUST draw entropy from `sys random bytes list 4` (`getrandom` syscall) so consecutive runs produce independent random sequences.
- **Gap 9.2 (`vis`/`unvis` `-h` HTTP URI and `-m` MIME Quoted-Printable)**: `vis -h` / `unvis -h` MUST encode/decode RFC 3986 `%xx` hex escapes; `vis -m` / `unvis -m` MUST encode/decode MIME Quoted-Printable `=XX` escapes and soft `=\n` line breaks.
- **Gap 9.3 (`mktemp -d` Atomic `0700` Creation)**: `mktemp -d` MUST invoke `sys mkdir path 448` (`0700`) atomically in a single syscall.
- **Gap 9.4 (`sha256 -c` Checklist Verification)**: `sha256 -c file` MUST parse BSD (`SHA256 (file) = hex`) and GNU/reversed (`hex  file`) checksum lists, verify each file on disk, report `OK` or `FAILED`, and exit non-zero if any digest mismatches.

## Consequences
- Every utility's requirement specification (`FR-*`) and test specification (`TS-*`) is updated with explicit acceptance criteria and multi-case test requirements before code changes are made.
