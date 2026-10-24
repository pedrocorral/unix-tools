# unix-tools

A collection of **17 classic BSD and Unix userland utilities** written in 100% pure **Ridgun** (`v0.9.17` PAL) and compiled directly to standalone native Linux `x86_64` ELF64 executables with zero C, Rust, or `libc` runtime dependencies.

All utilities operate over POSIX file descriptors and system calls provided by `/home/boogie/ridgun/libs` (`posix.rglib`, `io.rglib`, `crypto.rglib`, and `socket.rglib`).

---

## Included Utilities (17 Tools)

| Domain | Utility | Description | Source |
| :--- | :--- | :--- | :--- |
| **FILTER** | `jot(1)` | Sequential, repetitive, character, and pseudo-random data generator | `src/main/ridgun/jot/main.rg` |
| **FILTER** | `rs(1)` | Array and stream reshaping, transposition, and column formatting filter | `src/main/ridgun/rs/main.rg` |
| **FILTER** | `lam(1)` | Multi-file line lamination tool with per-stream padding and separators | `src/main/ridgun/lam/main.rg` |
| **FILTER** | `vis(1)` | Lossless visual encoding of non-printable and binary characters | `src/main/ridgun/vis/main.rg` |
| **FILTER** | `unvis(1)` | Lossless decoder restoring `vis(1)`-encoded streams to raw bytes | `src/main/ridgun/unvis/main.rg` |
| **FILTER** | `mdrr(1)` | Terminal Markdown formatter and reader with GFM tables, alerts, and TOC filter | `src/main/ridgun/mdrr/main.rg` |
| **FS** | `ls(1)` | Directory contents listing with classification, sorting, and ANSI color | `src/main/ridgun/ls/main.rg` |
| **FS** | `find(1)` | Recursive directory hierarchy walker and boolean expression evaluator | `src/main/ridgun/find/main.rg` |
| **FS** | `mtree(8)` | Directory hierarchy specification generator, SHA-256 auditor, and healer | `src/main/ridgun/mtree/main.rg` |
| **FS** | `mktemp(1)` | Atomic temporary file (`0600`) and directory (`0700`) generator | `src/main/ridgun/mktemp/main.rg` |
| **PROC** | `apply(1)` | Fixed-arity argument batching and command runner | `src/main/ridgun/apply/main.rg` |
| **PROC** | `make(1)` | Directed Acyclic Graph (DAG) build tool with topological sort and cycle checks | `src/main/ridgun/make/main.rg` |
| **PROC** | `sh(1)` | POSIX/BSD command interpreter (pipelines, redirections, control flow, built-ins) | `src/main/ridgun/sh/main.rg` |
| **PROC** | `timeout(1)` | Time-bounded command execution with signal escalation | `src/main/ridgun/timeout/main.rg` |
| **PROC** | `nc(1)` | TCP, UDP, and Unix-domain socket Swiss army knife and port scanner | `src/main/ridgun/nc/main.rg` |
| **SEC** | `doas(1)` | OpenBSD rule-based privilege escalation utility and `doas.conf(5)` auditor | `src/main/ridgun/doas/main.rg` |
| **SEC** | `sha256(1)` | BSD SHA-256 digest, HMAC-SHA256 (`-k`), and Argon2id (`--argon2id`) utility | `src/main/ridgun/sha256/main.rg` |

---

## Detailed Utility Reference

### Tier 1 — BSD Text, Table & Markdown Filters (`FILTER`)

#### 1. `jot(1)` — Sequential & Random Data Generator
Generates sequences of numbers, repeated strings, ASCII characters, or deterministic pseudo-random numbers.
- **Flags**:
  - `-r`: Generate pseudo-random values (`-S seed` for deterministic seeding).
  - `-b word`: Repeat `word` verbatim.
  - `-w word`: Format output using `printf`-style specifiers (`%d`, `%i`, `%04d`, `%-4d`, `%x`, `%X`, `%o`, `%c`, `%.2f`, `%%`).
  - `-s string`: Join items with `string` instead of newline.
  - `-n`: Omit the final trailing newline.
  - `-p precision`: Set fixed-point decimal precision.
  - `-c`: Character output mode.
- **Examples**:
  ```sh
  ./target/jot 5 10 50            # 10, 20, 30, 40, 50
  ./target/jot -n -s , 4 1 4      # 1,2,3,4
  ./target/jot -w '0x%02x' 4 0 3  # 0x00, 0x01, 0x02, 0x03
  ```

#### 2. `rs(1)` — Reshape Data Array
Reads whitespace- or delimiter-separated items from files or descriptor `0` (`stdin`) and arranges them into a 2D matrix.
- **Flags**:
  - `[rows [cols]]`: Target dimensions (`0` auto-infers from input length).
  - `-t`: Transpose rows and columns.
  - `-c<ch>` / `-C<ch>`: Custom input / output column delimiter.
  - `-g<n>`: Set inter-column gutter width.
  - `-j`: Right-align columns within their computed widths.
  - `-w<width>`: Auto-calculate column count to fit display width.
  - `-e`: Treat each input line as a single entry.
  - `-k<n>`: Ignore the first `n` lines of input.
  - `-y`: Recycle elements when the last row is incomplete.
  - `-h`: Print matrix dimensions (`rows by cols`) only.
  - `-z`: Squeeze column widths to exact item lengths.
- **Example**:
  ```sh
  ./target/jot 6 1 6 | ./target/rs -t 2 3
  # 1  3  5
  # 2  4  6
  ```

#### 3. `lam(1)` — Laminate Multiple Files Side-by-Side
Concatenates corresponding lines from multiple input files or `-` (`stdin`) simultaneously, supporting both per-file lowercase flags and persistent uppercase flags.
- **Flags**:
  - `-s sep` / `-S sep`: Print separator string before the next file column (reset per file vs. persistent).
  - `-f min.max` / `-F min.max`: Pad or truncate column width (`-min.max` left-aligns; `min.max` right-aligns).
  - `-p min.max` / `-P min.max`: Continue padding when an input file reaches EOF early.
  - `-t char`: Use `char` instead of newline as the input line terminator.
- **Example**:
  ```sh
  ./target/lam users.txt -s ' -> ' roles.txt
  ```

#### 4. `vis(1)` & `unvis(1)` — Visual Byte Encoding & Lossless Decoding
Encodes non-printable, control, whitespace, and arbitrary binary bytes (`0x00..0xFF`) into unambiguous printable ASCII sequences (`\^A`, `\M-x`, `\040`, `\n`, `\t`, `\\`), and restores them byte-for-byte with `unvis(1)`.
- **Flags (`vis`)**:
  - `-c`: Backslash-escape C-style control sequences (`\n`, `\t`, `\r`, `\b`, `\a`, `\v`, `\f`).
  - `-o`: Encode all non-printable bytes in 3-digit octal (`\ddd`).
  - `-w`: Encode spaces, tabs, and newlines visually.
  - `-t`: Encode tabs and newlines visually.
  - `-l`: Mark newlines with `\$` before the line break.
  - `-F width`: Fold output lines at `width` columns using soft `\` line continuations.
  - `-n`: Encode only backslashes.
  - `-b`: Suppress doubling of backslashes.
- **Example**:
  ```sh
  ./target/vis -tw binary.dat | ./target/unvis > restored.dat
  ```

#### 5. `mdrr(1)` — Terminal Markdown Formatter & Reader
Parses and renders Markdown documents for terminal viewing with ANSI colors, Unicode box-drawing tables, GitHub alerts (`[!NOTE]`, `[!TIP]`, `[!IMPORTANT]`, `[!WARNING]`, `[!CAUTION]`), task lists (`☐`/`☑`), and fenced code blocks.
- **Flags**:
  - `-w width`: Wrap paragraphs and blocks to `width` visible columns (ignoring zero-width ANSI SGR codes).
  - `-p`: Plain mode (strip ANSI color and style sequences).
  - `-a`: ASCII-only borders and checkboxes (`+---+`, `[ ]`, `[x]`).
  - `-t`: Extract and print an indented Table of Contents from document headings.
  - `-s heading`: Filter and render only the section matching `heading` (and its nested subsections).
  - `-n`: Display line numbers inside fenced code blocks.
  - `-H`: Hide YAML front-matter blocks.
- **Example**:
  ```sh
  ./target/mdrr -n README.md
  ./target/mdrr -p -a -t README.md
  ```

---

### Tier 2 — BSD Filesystem & Hierarchy Tools (`FS`)

#### 6. `ls(1)` — List Directory Contents
Lists files and directories with BSD mode formatting (`drwxr-xr-x`, sticky `t`/`T`, setuid/setgid `s`/`S`), type classification, and sorting.
- **Flags**:
  - `-l`: Long listing format (permissions, link count, owner, group, size, timestamp, symlink target).
  - `-a` / `-A`: Include dot-entries (`.` and `..` included with `-a`, excluded with `-A`).
  - `-F`: Append file type indicator (`/` dir, `*` executable, `@` symlink, `|` FIFO, `=` socket).
  - `-G`: Enable ANSI colorized output by file type.
  - `-h`: Format sizes with human-readable binary suffixes (`B`, `K`, `M`, `G`).
  - `-R`: Recursively list subdirectories.
  - `-S` / `-t` / `-r`: Sort by descending size (`-S`), modification time (`-t`), or reverse order (`-r`).
  - `-1` / `-d`: Force single-column output (`-1`) or list directory entries themselves (`-d`).

#### 7. `find(1)` — Walk a File Hierarchy
Recursively traverses one or more root paths and evaluates compound boolean expressions on every entry.
- **Primaries & Operators**:
  - `-name pattern` / `-iname pattern` / `-path pattern`: Glob matching (`*`, `?`, `[a-z]`, `[!a-z]`).
  - `-type [f|d|l|p|s|b|c]`: Match file type.
  - `-size [+|-]N[c|k|M|G]`: Match file size (greater than, less than, or exact).
  - `-empty`: Match 0-byte regular files or empty directories.
  - `-maxdepth d` / `-mindepth d`: Constrain traversal depth.
  - `! expr`, `expr -a expr`, `expr -o expr`, `( expr )`: Full boolean precedence.
  - `-print0`: Delimit results with NUL (`\0`) bytes.
  - `-exec prog args ... {} ;`: Execute a command per matched entry.
- **Example**:
  ```sh
  ./target/find src -type f -name '*.rg' ! -empty
  ```

#### 8. `mtree(8)` — Map & Audit Directory Hierarchies
Generates declarative hierarchy specifications (`type`, `mode`, `size`, `sha256`, `link`) and audits live directory trees against saved specifications.
- **Flags**:
  - `-c`: Generate a specification from the target root directory.
  - `-f specfile`: Verify the live tree against `specfile` (exits `0` if clean, `2` on mismatch).
  - `-p path`: Root path to generate or verify (default `.`).
  - `-u`: Heal missing directories and restore mismatched octal file permissions automatically.
  - `-K keywords`: Add comma-separated keywords to the specification.
- **Example**:
  ```sh
  ./target/mtree -c -p src > src.mtree
  ./target/mtree -f src.mtree -p src
  ```

#### 9. `mktemp(1)` — Make Temporary File or Directory
Safely creates unique temporary files (`0600`) or directories (`0700`) using `O_CREAT | O_EXCL` and cryptographic randomness.
- **Flags**:
  - `-d`: Create a directory instead of a regular file.
  - `-u`: Dry-run ("unsafe") mode — print the generated unique path without creating it on disk.
  - `-q`: Suppress diagnostic output on failure.
  - `-p dir`: Base directory for `-t` templates (defaults to `/tmp`).
  - `-t prefix`: Construct template `<dir>/<prefix>.XXXXXXXX`.
- **Example**:
  ```sh
  TMP_DIR=$(./target/mktemp -d /tmp/build.XXXXXX)
  ```

---

### Tier 3 — BSD Process, Build, Shell & Network Tools (`PROC`)

#### 10. `apply(1)` — Apply a Command to Groups of Arguments
Partitions a flat argument list into fixed-size tuples and executes a command template for each group.
- **Flags**:
  - `-N`: Consume `N` arguments per invocation (`-0` runs the command once per argument without appending it).
  - `%1..%9`: Positional placeholders inside the command string (auto-infers `N` from the highest `%k`).
  - `-a char`: Change the placeholder magic character from `%` to `char`.
  - `-d`: Dry-run mode — print constructed commands without executing them.
- **Example**:
  ```sh
  ./target/apply -2 'cmp -s %1 %2' a.old a.new b.old b.new
  ```

#### 11. `make(1)` — Maintain Program Dependencies
Parses a `Makefile`, resolves macro definitions (`=`, `:=`, `?=`) and CLI overrides (`VAR=val`), checks topological DAG dependencies, detects dependency cycles, and runs target recipes.
- **Flags**:
  - `-f makefile`: Read rules from `makefile`.
  - `-n`: Dry-run — print recipe commands in topological order without executing them.
  - `-B`: Unconditionally build all targets.
  - `-j jobs`: Specify concurrency level.

#### 12. `sh(1)` — Command Interpreter (Shell)
Executes POSIX/BSD shell scripts or `-c` command strings.
- **Supported Grammar & Features**:
  - Pipelines (`cmd1 | cmd2`), sequential lists (`;`), and short-circuit boolean lists (`&&`, `||`).
  - Redirections (`>`, `>>`, `<`, `2>&1`).
  - Control structures: `if ...; then ...; elif ...; then ...; else ...; fi`, `for var in ...; do ...; done`, `while ...; do ...; done`, and isolated subshells `( ... )`.
  - Quoting & Expansion: single quotes `'...'`, double quotes `"..."`, `$VAR`, `${VAR}`, `${VAR:-default}`, `$#`, `$?`, `$1..$9`.
  - Built-ins: `cd`, `export`, `unset`, `set`, `echo`, `exit`, and `test` / `[ ... ]` (`=`, `!=`, `-z`, `-n`, `-eq`, `-ne`, `-lt`, `-le`, `-gt`, `-ge`, `-f`, `-d`, `-e`).

#### 13. `timeout(1)` — Run a Command with a Time Limit
Executes a utility under a strict deadline and terminates it if the time limit expires.
- **Flags**:
  - `DURATION`: Supports `ms`, `s`, `m`, `h`, and `d` suffixes (e.g., `250ms`, `5s`, `1m`).
  - `-s SIGNAL`: Signal to send on timeout (default `TERM`).
  - `-k DURATION`: Send `KILL` if the process is still running `DURATION` after the initial signal.
  - `--preserve-status`: Exit with the child's signal status instead of `124` on timeout.
- **Example**:
  ```sh
  ./target/timeout -k 100ms 2s ./target/sh -c 'echo ok'
  ```

#### 14. `nc(1)` — Arbitrary TCP, UDP & Unix Socket Connections
Establishes outbound connections, listens for inbound connections, transfers data streams, and probes open ports via `socket.rglib`.
- **Flags**:
  - `-l`: Listen for an incoming connection on the specified port or Unix socket path.
  - `-z`: Zero-I/O port scanning mode (check connectivity and close immediately).
  - `-v`: Verbose connection diagnostics.
  - `-w sec`: Connection timeout in seconds.
  - `-u`: Use UDP sockets instead of TCP.
  - `-U`: Use Unix-domain stream sockets (`AF_UNIX`).
- **Example**:
  ```sh
  ./target/nc -zv -w 1 127.0.0.1 8080
  ```

---

### Tier 4 — BSD Security, Privilege & Cryptography Tools (`SEC`)

#### 15. `doas(1)` — Execute Commands as Another User
Implements the OpenBSD `doas.conf(5)` parser, "last matching rule wins" policy engine, environment sanitization (`keepenv`, `setenv`), and configuration auditing.
- **Flags**:
  - `-C config [cmd args...]`: Parse and validate `config`; if a command is supplied, print the policy verdict (`permit`, `permit nopass`, or `deny`) without executing the command.
  - `-u user`: Target user account (default `root`).
  - `-n`: Non-interactive mode — fail immediately if a rule requires a password.
  - `-s`: Execute the user's shell.
- **Example**:
  ```sh
  ./target/doas -C /etc/doas.conf -u root /sbin/reboot
  ```

#### 16. `sha256(1)` — Cryptographic SHA-256, HMAC-SHA256 & Argon2id
Computes FIPS 180-4 SHA-256 digests, RFC 2104 HMAC-SHA256 authentication tags, and RFC 9106 Argon2id password hashes via `crypto.rglib`.
- **Flags**:
  - `-s string`: Digest the literal `string`.
  - `-q`: Quiet mode — output only the raw hex digest or PHC string.
  - `-r`: Reverse output format (`<digest> <filename>`).
  - `-k key`: Compute `HMAC-SHA256` using `key`.
  - `--argon2id salt`: Derive an Argon2id PHC string (`$argon2id$v=19$m=64,t=2,p=1$...`).
  - `--verify phc`: Verify input password against an Argon2id `phc` string in constant time (exits `0` on match, `1` on mismatch).
- **Examples**:
  ```sh
  ./target/sha256 -s "abc"
  # SHA256 ("abc") = ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad

  ./target/sha256 -q -k "secret" -s "message"
  # HMAC-SHA256 hex digest

  PHC=$(./target/sha256 -q --argon2id "saltsalt" -s "correcthorse")
  ./target/sha256 --verify "$PHC" -s "correcthorse"
  ```

---

## Building & Testing with `stable`

This project is managed by **`stable`** (`build.rg`), Ridgun's self-hosted build orchestrator, dependency manager, parallel type-checker, canonical tabular formatter, and test runner.

### Prerequisites
- `stable` and `ridgunc` (`v0.9.17` PAL) in `PATH`.
- Ridgun standard libraries available at `/home/boogie/ridgun/libs`.

### 1. Inspect Project & Custom Tasks
```sh
stable info
stable tasks
```

### 2. Sync `.rglib` Dependencies (`posix`, `io`, `crypto`, `socket`)
Run the custom `sync libs` task defined in `build.rg` to compile and install the `.rglib` bundles into `.stable/deps/` and `target/`:
```sh
stable sync libs
```

### 3. Compile All 17 Native ELF64 Binaries (Strict Mode)
Build every discovered utility in `src/main/ridgun/<tool>/main.rg` into `target/` using `ridgunc build --strict` via the custom `strict binaries` task (or use `stable built` / `stable built mktemp`):
```sh
stable strict binaries
stable built
```

### 4. Type-Check, Format & Run the Automated Test Suites (283 Assertions)
`stable` executes type-checking, formatting, and all 5 test suites (`test_filter.rg`, `test_md.rg`, `test_fs.rg`, `test_proc.rg`, `test_sec.rg`) concurrently across cores via Ridgun's `||map`:
```sh
stable check
stable format
stable test
```

### 5. Install Executables Locally (Optional)
Build and install the ELF64 binaries into `~/.local/bin/` and `./bin/`:
```sh
stable install
```
