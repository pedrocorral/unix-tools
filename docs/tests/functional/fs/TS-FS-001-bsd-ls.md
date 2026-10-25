---
id: TS-FS-001
title: BSD ls(1) directory listing, real kernel stat metadata (-i, -n, -s, st_nlink), sorting, and LSCOLORS
level: functional
covers: [FR-FS-001]
status: automated
automated_in: [src/test/ridgun/test_fs.rg]
---
# TS-FS-001 — BSD `ls(1)` Verification

## Preconditions / fixtures
- Temporary directory hierarchy with regular files, multiple hardlinks (`ln`), executable files, setuid/setgid/sticky files, subdirectories, dotfiles, FIFOs, sockets, control-character filenames, and symbolic links.

## Steps / inputs
1. Run `ls` with `-F`, `-p`, `-d`, `-q`, `-m`, `-x`, `-f`, `-l`, `-lh`, `-n`, `-i`, `-s`, `-a`, `-A`, `-S`, `-t`, `-r`, `-R`, `-C`, and `-G` (with default and custom `LSCOLORS`), plus combined flags (`-lni`, `-lsF`, `-lR`).
2. Create hardlinks (`ln f1 f2`, `ln f1 f3`) and nested subdirectories, verifying `ls -l` reports the real kernel `st_nlink` (`3` for the triple-linked file, `2 + K` for a directory with `K` subdirectories), `ls -i` reports identical non-zero `st_ino` for hardlinked files, `ls -n` reports numeric UID/GID (`sys getuid` / `sys getgid`), and `ls -s` reports allocated blocks.

## Expected results (map to acceptance criteria)
- AC1 → `ls -F` appends `/` to directories, `*` to executables, `@` to symlinks, `|` to FIFOs, and `=` to sockets; `ls -p` appends `/` only to directories; `ls -d` lists directory arguments themselves; `ls -q` replaces non-printable characters with `?`.
- AC2 → `ls -lh` and `ls -ln` print real kernel `st_nlink`, numeric UID/GID under `-n`, real inode numbers under `-i`, and real block allocations under `-s`, while `-m`, `-x`, `-C`, `-f`, `-S`, `-t`, `-r`, and `-R` format and sort entries accurately.
- AC3 → `ls -G` emits ANSI color escapes derived from BSD `LSCOLORS`.

## Not covered here (to avoid duplication)
- `find` and `mtree` are covered in `TS-FS-002` and `TS-FS-003`.
