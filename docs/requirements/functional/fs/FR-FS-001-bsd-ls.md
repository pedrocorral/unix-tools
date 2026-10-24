---
id: FR-FS-001
title: BSD ls(1) directory listing utility in Pure Ridgun
status: implemented
priority: must
created: 2026-10-17
updated: 2026-10-24
depends_on: [ADR-0001]
verified_by: [TS-FS-001]
---
# FR-FS-001 — BSD `ls(1)` Directory Listing Utility in Pure Ridgun

## Statement
The project SHALL provide a pure Ridgun implementation of BSD `ls(1)` (`src/main/ridgun/ls/main.rg`) featuring BSD type suffixes (`-F`), multi-column vertical grid layout (`-C`), long format (`-l`), human-readable sizes (`-h`), directory-entry listing (`-d`), non-printable replacement (`-q`), sorting (`-S`, `-t`, `-r`, `-f`), recursive traversal (`-R`), and BSD `CLICOLOR` / `LSCOLORS` colorization (`-G`).

## Functional Specification

### 1. Supported BSD Flags
- `-1`: Force one entry per line.
- `-C`: Multi-column output sorted down columns (default when formatting for terminal display, target width 80 cols).
- `-x`: Multi-column output sorted across rows rather than down columns.
- `-m`: Stream output format, listing files across the page separated by `, `.
- `-l`: Long listing format (`<mode_str> <nlink> <owner> <group> <size> <date> <name>[ -> <symlink_target>]`).
- `-d`: Directories are listed as plain files (not searched recursively or expanded), and symbolic links in the argument list are not indirected through.
- `-a`: Include directory entries whose names begin with a dot (`.` and `..` included).
- `-A`: Include dot-entries except `.` and `..`.
- `-f`: Output is not sorted; implicitly turns on `-a`.
- `-F`: Append BSD file-type indicator character to each name:
  - `/` for directories
  - `*` for executable regular files
  - `@` for symbolic links
  - `|` for FIFOs
  - `=` for sockets
- `-p`: Write a slash (`/`) after each filename if that file is a directory.
- `-q`: Force printing of non-graphic characters in file names as the character `?`.
- `-h`: With `-l`, use unit suffixes (`B`, `K`, `M`, `G`, `T`) to keep size strings to 4 characters or fewer.
- `-S`: Sort by file size (largest first) instead of lexicographical order (ties broken lexicographically).
- `-t`: Sort by modification timestamp (`mtime`, newest first; ties broken lexicographically).
- `-r`: Reverse sort order.
- `-R`: Recursively list subdirectories encountered.
- `-G`: Enable BSD colorized output using the 22-character BSD `LSCOLORS` specification (default `exfxcxdxbxegedabagacad`, where each 2-char pair specifies foreground/background for: 1. directory, 2. symlink, 3. socket, 4. pipe, 5. executable, 6. block special, 7. char special, 8. setuid exec, 9. setgid exec, 10. sticky+writable dir, 11. non-sticky writable dir).

### 2. Ridgun Data Model & `op ::` Design
- Define `type FileKind = FReg | FDir | FSymLink | FFifo | FSock | FBlock | FChar` and `type FileEntry = Entry String String FileKind Int Int Int String String Int String` (`name`, `full_path`, `kind`, `mode`, `nlink`, `size`, `owner`, `group`, `mtime`, `link_target`).
- Express sorting, filtering, permission formatting (`rwxr-xr-x`, `rwsr-xr-x`, `rwxr-sr-x`, `rwxrwxrwt`), and grid column layout via `op ::` declarations.

## Acceptance Criteria
- AC1: `ls -F` appends `/` to directories, `*` to executables, `@` to symlinks, `|` to FIFOs, and `=` to sockets; `ls -p` appends `/` only to directories; `ls -d` lists directory paths themselves without expanding their contents; `ls -q` replaces control characters with `?`.
- AC2: `ls -lh` prints aligned columns for mode, links, owner, group, human-readable size, date, and entry name (including `-> target` for symlinks), while `-m`, `-x`, `-C`, `-f`, `-S`, `-t`, `-r`, and `-R` format and sort entries accurately.
- AC3: `ls -G` emits ANSI color escapes derived from BSD `LSCOLORS` (including custom `LSCOLORS` environment overrides).
