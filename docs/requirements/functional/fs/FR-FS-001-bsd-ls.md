---
id: FR-FS-001
title: BSD ls(1) directory listing utility in Pure Ridgun
status: implemented
priority: must
created: 2026-10-17
updated: 2026-10-24
depends_on: [ADR-0001, ADR-0002]
verified_by: [TS-FS-001]
---
# FR-FS-001 — BSD `ls(1)` Directory Listing Utility in Pure Ridgun

## Statement
The project SHALL provide a pure Ridgun implementation of BSD `ls(1)` (`src/main/ridgun/ls/main.rg`) featuring BSD type suffixes (`-F`, `-p`), multi-column vertical/horizontal/stream grid layouts (`-C`, `-x`, `-m`), long format (`-l`, `-n`) with real kernel `lstat` link counts (`st_nlink`), inode numbers (`-i`, `st_ino`), allocated block counts (`-s`, `st_blocks`), human-readable sizes (`-h`), directory-entry listing (`-d`), non-printable replacement (`-q`), sorting (`-S`, `-t`, `-r`, `-f`), recursive traversal (`-R`), and BSD `CLICOLOR` / `LSCOLORS` colorization (`-G`).

## Functional Specification

### 1. Supported BSD Flags
- `-1`: Force one entry per line.
- `-C`: Multi-column output sorted down columns (target width 80 cols).
- `-x`: Multi-column output sorted across rows rather than down columns.
- `-m`: Stream output format, listing files across the page separated by `, `.
- `-l`: Long listing format (`<mode_str> <nlink> <owner> <group> <size> <date> <name>[ -> <symlink_target>]`) using real `st_nlink` from kernel `lstat`.
- `-n`: Long listing format like `-l`, but displaying numeric UID (`st_uid`) and GID (`st_gid`) instead of user and group names.
- `-i`: For each file, print the file's real filesystem inode serial number (`st_ino`).
- `-s`: Display the number of filesystem blocks actually used by each file (`st_blocks`).
- `-d`: Directories are listed as plain files (not searched recursively or expanded).
- `-a`: Include directory entries whose names begin with a dot (`.` and `..` included).
- `-A`: Include dot-entries except `.` and `..`.
- `-f`: Output is not sorted; implicitly turns on `-a`.
- `-F`: Append BSD file-type indicator character to each name (`/`, `*`, `@`, `|`, `=`).
- `-p`: Write a slash (`/`) after each filename if that file is a directory.
- `-q`: Force printing of non-graphic characters in file names as `?`.
- `-h`: With `-l`/`-n`, use unit suffixes (`B`, `K`, `M`, `G`, `T`).
- `-S`: Sort by file size (largest first; ties broken lexicographically).
- `-t`: Sort by modification timestamp (`mtime` seconds and nanoseconds, newest first; ties broken lexicographically).
- `-r`: Reverse sort order.
- `-R`: Recursively list subdirectories encountered.
- `-G`: Enable BSD colorized output using the 22-character BSD `LSCOLORS` specification.

### 2. Kernel `struct stat` Inspection
- Reads the 144-byte Linux x86_64 `struct stat` via `__NR_lstat` (`sys call 6`) to extract real `st_ino`, `st_nlink`, `st_mode`, `st_uid`, `st_gid`, `st_size`, `st_blocks`, `st_mtime`, and `st_mtime_nsec`.

## Acceptance Criteria
- AC1: `ls -F` appends `/` to directories, `*` to executables, `@` to symlinks, `|` to FIFOs, and `=` to sockets; `ls -p` appends `/` only to directories; `ls -d` lists directory paths themselves; `ls -q` replaces control characters with `?`.
- AC2: `ls -lh` and `ls -ln` print real kernel hardlink counts (`st_nlink`, e.g., `2` or `3` for hardlinked files and `2 + subdirs` for directories), real numeric UID/GID under `-n`, real inode numbers under `-i`, and real block allocations under `-s`, while `-m`, `-x`, `-C`, `-f`, `-S`, `-t`, `-r`, and `-R` format and sort entries accurately.
- AC3: `ls -G` emits ANSI color escapes derived from BSD `LSCOLORS` (including custom `LSCOLORS` environment overrides).
