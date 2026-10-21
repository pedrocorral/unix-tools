---
id: TS-FS-001
title: BSD ls(1) directory listing, formatting, sorting, and LSCOLORS
level: functional
covers: [FR-FS-001]
status: automated
automated_in: [src/test/ridgun/test_fs.rg]
---
# TS-FS-001 — BSD `ls(1)` Verification

## Preconditions / fixtures
- Temporary directory hierarchy with regular files, executable files, subdirectories, dotfiles, and symbolic links.

## Steps / inputs
1. Run `ls` with `-F`, `-l`, `-lh`, `-a`, `-A`, `-S`, `-t`, `-r`, `-R`, `-C`, and `-G`.

## Expected results (map to acceptance criteria)
- AC1 → `ls -F` appends `/` to directories, `*` to executables, and `@` to symlinks.
- AC2 → `ls -lh` prints aligned columns for mode, links, owner, group, human-readable size, date, and entry name.
- AC3 → `ls -G` emits ANSI color escapes derived from BSD `LSCOLORS`.

## Not covered here (to avoid duplication)
- `find` and `mtree` are covered in `TS-FS-002` and `TS-FS-003`.
