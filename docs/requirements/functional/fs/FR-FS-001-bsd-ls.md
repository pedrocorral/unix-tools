---
id: FR-FS-001
title: BSD ls(1) directory listing utility in Pure Ridgun
status: implemented
priority: must
created: 2026-10-17
updated: 2026-10-18
depends_on: [ADR-0001]
verified_by: [TS-FS-001]
---
# FR-FS-001 — BSD `ls(1)` Directory Listing Utility in Pure Ridgun

## Statement
The project SHALL provide a pure Ridgun implementation of BSD `ls(1)` (`src/main/ridgun/ls/main.rg`) featuring BSD type suffixes (`-F`), multi-column vertical grid layout (`-C`), long format (`-l`), human-readable sizes (`-h`), sorting (`-S`, `-t`, `-r`), recursive traversal (`-R`), and BSD `CLICOLOR` / `LSCOLORS` colorization (`-G`).

## Functional Specification

### 1. Supported BSD Flags
- `-1`: Force one entry per line.
- `-C`: Multi-column output sorted down columns (default when formatting for terminal display, target width 80 cols).
- `-l`: Long listing format (`<mode_str> <nlink> <owner> <group> <size> <date> <name>[ -> <symlink_target>]`).
- `-a`: Include directory entries whose names begin with a dot (`.` and `..` included).
- `-A`: Include dot-entries except `.` and `..`.
- `-F`: Append BSD file-type indicator character to each name:
  - `/` for directories
  - `*` for executable regular files
  - `@` for symbolic links
  - `|` for FIFOs
  - `=` for sockets
- `-h`: With `-l`, use unit suffixes (`B`, `K`, `M`, `G`, `T`) to keep size strings to 4 characters or fewer.
- `-S`: Sort by file size (largest first) instead of lexicographical order.
- `-t`: Sort by modification timestamp (`mtime`, newest first).
- `-r`: Reverse sort order.
- `-R`: Recursively list subdirectories encountered.
- `-G`: Enable BSD colorized output using the 22-character BSD `LSCOLORS` specification (default `exfxcxdxbxegedabagacad`, where each 2-char pair specifies foreground/background for: 1. directory, 2. symlink, 3. socket, 4. pipe, 5. executable, 6. block special, 7. char special, 8. setuid exec, 9. setgid exec, 10. sticky+writable dir, 11. non-sticky writable dir).

### 2. Ridgun Data Model & `op ::` Design
- Define `type FileKind = FReg | FDir | FSymLink | FFifo | FSock | FBlock | FChar` and `type FileEntry = Entry String String FileKind Int Int Int String String Int String` (`name`, `full_path`, `kind`, `mode`, `nlink`, `size`, `owner`, `group`, `mtime`, `link_target`).
- Express sorting, filtering, permission formatting (`rwxr-xr-x`), and grid column layout via `op ::` declarations.

## Acceptance Criteria
- AC1: `ls -F` appends `/` to directories, `*` to executables, and `@` to symlinks.
- AC2: `ls -lh` prints aligned columns for mode, links, owner, group, human-readable size, date, and entry name.
- AC3: `ls -G` emits ANSI color escapes derived from BSD `LSCOLORS`.
