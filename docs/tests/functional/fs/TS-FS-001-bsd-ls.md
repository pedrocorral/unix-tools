---
id: TS-FS-001
title: BSD ls(1) directory listing, formatting, sorting, -d/-p/-q/-m/-x/-f, and LSCOLORS
level: functional
covers: [FR-FS-001]
status: automated
automated_in: [src/test/ridgun/test_fs.rg]
---
# TS-FS-001 — BSD `ls(1)` Verification

## Preconditions / fixtures
- Temporary directory hierarchy with regular files, executable files, setuid/setgid/sticky files, subdirectories, dotfiles, FIFOs, sockets, control-character filenames, and symbolic links.

## Steps / inputs
1. Run `ls` with `-F`, `-p`, `-d`, `-q`, `-m`, `-x`, `-f`, `-l`, `-lh`, `-a`, `-A`, `-S`, `-t`, `-r`, `-R`, `-C`, and `-G` (with default and custom `LSCOLORS`).

## Expected results (map to acceptance criteria)
- AC1 → `ls -F` appends `/` to directories, `*` to executables, `@` to symlinks, `|` to FIFOs, and `=` to sockets; `ls -p` appends `/` only to directories; `ls -d` lists directory arguments themselves without expanding contents; `ls -q` replaces non-printable characters with `?`.
- AC2 → `ls -lh` prints aligned columns for mode, links, owner, group, human-readable size, date, and entry name, while `-m`, `-x`, `-C`, `-f`, `-S`, `-t`, `-r`, and `-R` format and sort entries accurately.
- AC3 → `ls -G` emits ANSI color escapes derived from BSD `LSCOLORS`.

## Not covered here (to avoid duplication)
- `find` and `mtree` are covered in `TS-FS-002` and `TS-FS-003`.
