-- test_fs.rg — Exhaustive Unit & E2E Binary Tests for Tier 2 BSD Filesystem Utilities
-- @tests TS-FS-001
-- @tests TS-FS-002
-- @tests TS-FS-003

import test
import fs_lib

fn :: test_ls_unit_suite => ()
      test_ls_unit_suite =
        esc = char_from_code 27
        nl = char_from_code 10
        _ = echo "=== 1. Testing BSD ls(1) Unit & Edge Cases (TS-FS-001) ==="
        dir_ent = Entry "sub" "/tmp/sub" FDir 493 2 4096 "root" "root" 1000 ""
        exec_ent = Entry "run.sh" "/tmp/run.sh" FReg 493 1 2048 "root" "root" 2000 ""
        link_ent = Entry "latest" "/tmp/latest" FSymLink 511 1 6 "root" "root" 3000 "run.sh"
        fifo_ent = Entry "pipe0" "/tmp/pipe0" FFifo 420 1 0 "root" "root" 1500 ""
        sock_ent = Entry "sock0" "/tmp/sock0" FSock 493 1 0 "root" "root" 1500 ""
        reg_ent = Entry "notes.txt" "/tmp/notes.txt" FReg 420 1 512 "root" "root" 500 ""
        _ = assert_equals (classify suffix dir_ent) "/" "AC1: ls -F appends / to directories"
        _ = assert_equals (classify suffix exec_ent) "*" "AC1: ls -F appends * to executable regular files"
        _ = assert_equals (classify suffix link_ent) "@" "AC1: ls -F appends @ to symbolic links"
        _ = assert_equals (classify suffix fifo_ent) "|" "AC1: ls -F appends | to FIFOs"
        _ = assert_equals (classify suffix sock_ent) "=" "AC1: ls -F appends = to sockets"
        _ = assert_equals (classify suffix reg_ent) "" "AC1: ls -F appends nothing to non-executable regular files"
        _ = assert_equals (format mode string FDir 493) "drwxr-xr-x" "AC2: mode 0755 dir -> drwxr-xr-x"
        _ = assert_equals (format mode string FReg 420) "-rw-r--r--" "AC2: mode 0644 file -> -rw-r--r--"
        _ = assert_equals (format mode string FSymLink 511) "lrwxrwxrwx" "AC2: mode 0777 symlink -> lrwxrwxrwx"
        _ = assert_equals (format mode string FReg 2541) "-rwsr-xr-x" "AC2: mode 4755 setuid exec -> -rwsr-xr-x"
        _ = assert_equals (format mode string FDir 1023) "drwxrwxrwt" "AC2: mode 1777 sticky dir -> drwxrwxrwt"
        _ = assert_equals (format human size 512) "512B" "AC2: human size 512 -> 512B"
        _ = assert_equals (format human size 2048) "2.0K" "AC2: human size 2048 -> 2.0K"
        _ = assert_equals (format human size 1572864) "1.5M" "AC2: human size 1.5 MiB -> 1.5M"
        spec_s = parse ls args ["-1S"]
        sorted_s = format ls entries (sort entries [reg_ent, dir_ent, exec_ent] with spec_s) with spec_s
        _ = assert_equals (join_with sorted_s ",") "sub,run.sh,notes.txt" "ls -S sorts by descending file size"
        spec_sr = parse ls args ["-1Sr"]
        sorted_sr = format ls entries (sort entries [reg_ent, dir_ent, exec_ent] with spec_sr) with spec_sr
        _ = assert_equals (join_with sorted_sr ",") "notes.txt,run.sh,sub" "ls -Sr reverses size sort"
        spec_t = parse ls args ["-1t"]
        sorted_t = format ls entries (sort entries [reg_ent, dir_ent, exec_ent] with spec_t) with spec_t
        _ = assert_equals (join_with sorted_t ",") "run.sh,sub,notes.txt" "ls -t sorts by descending mtime"
        spec_g = parse ls args ["-1GF"]
        colored_lines = format ls entries [dir_ent, link_ent, exec_ent] with spec_g
        _ = assert_equals (str_list_nth colored_lines 0) (esc + "[34msub" + esc + "[0m/") "AC3: ls -GF emits ANSI blue for directory + / suffix"
        _ = assert_equals (str_list_nth colored_lines 1) (esc + "[35mlatest" + esc + "[0m@") "AC3: ls -GF emits ANSI magenta for symlink + @ suffix"
        assert_equals (str_list_nth colored_lines 2) (esc + "[31mrun.sh" + esc + "[0m*") "AC3: ls -GF emits ANSI red for executable + * suffix"

fn :: test_find_unit_suite => ()
      test_find_unit_suite =
        _ = echo "=== 2. Testing BSD find(1) Unit & Edge Cases (TS-FS-002) ==="
        _ = assert_true (glob match "*.rg" against "main.rg") "glob *.rg matches main.rg"
        _ = assert_true (not (glob match "*.rg" against "main.rs")) "glob *.rg rejects main.rs"
        _ = assert_true (glob match "test_?.rg" against "test_a.rg") "glob ? matches single char"
        _ = assert_true (glob match "[a-c]*.rg" against "beta.rg") "glob [a-c] character range"
        _ = assert_true (glob match "[!a-c]*.rg" against "zeta.rg") "glob [!a-c] negated character class"
        e_rg = Entry "main.rg" "src/main.rg" FReg 420 1 1200 "root" "root" 1000 ""
        e_md = Entry "README.md" "docs/README.md" FReg 420 1 0 "root" "root" 1000 ""
        e_dir = Entry "src" "src" FDir 493 2 4096 "root" "root" 1000 ""
        fspec1 = parse find args ["src", "-type", "f", "-name", "*.rg"]
        _ = assert_true (evaluate find expr (f_expr fspec1) on e_rg at 1) "AC1: find -type f -name *.rg matches main.rg"
        _ = assert_true (not (evaluate find expr (f_expr fspec1) on e_dir at 0)) "AC1: find -type f rejects directory"
        fspec2 = parse find args ["src", "(", "-name", "*.rg", "-or", "-iname", "readme*", ")", "-and", "!", "-size", "+2000c"]
        _ = assert_true (evaluate find expr (f_expr fspec2) on e_rg at 1) "AC2: compound boolean expr matches main.rg (1200c <= 2000c)"
        _ = assert_true (evaluate find expr (f_expr fspec2) on e_md at 1) "AC2: compound boolean expr with -iname matches README.md"
        e_big = Entry "big.rg" "src/big.rg" FReg 420 1 5000 "root" "root" 1000 ""
        assert_true (not (evaluate find expr (f_expr fspec2) on e_big at 1)) "AC2: compound boolean expr with ! -size +2000c rejects 5000B file"

fn :: test_fs_e2e_suite => ()
      test_fs_e2e_suite =
        nl = char_from_code 10
        _ = echo "=== 3. Testing Compiled Native ELF Binaries target/ls, target/find, target/mtree (E2E) ==="
        _ = shell_exec "rm -rf /tmp/ut_fs_tree /tmp/ut_mtree.spec && mkdir -p /tmp/ut_fs_tree/sub /tmp/ut_fs_tree/empty_dir"
        _ = write_file "/tmp/ut_fs_tree/alpha.rg" ("fn :: main => ()" + nl)
        _ = write_file "/tmp/ut_fs_tree/sub/beta.rg" ("-- beta" + nl)
        _ = write_file "/tmp/ut_fs_tree/notes.txt" ""
        _ = sys_chmod "/tmp/ut_fs_tree/alpha.rg" 493
        _ = sys_chmod "/tmp/ut_fs_tree/sub/beta.rg" 420
        _ = sys_chmod "/tmp/ut_fs_tree/notes.txt" 420
        _ = shell_exec "ln -s alpha.rg /tmp/ut_fs_tree/link_alpha"
        _ = write_file "/tmp/ut_fs_tree/.hidden" "dot"
        _ = sys_chmod "/tmp/ut_fs_tree/.hidden" 420
        _ = shell_exec "./target/ls -1 /tmp/ut_fs_tree > /tmp/ut_ls_def.out"
        _ = assert_equals (read_file "/tmp/ut_ls_def.out") ("alpha.rg" + nl + "empty_dir" + nl + "link_alpha" + nl + "notes.txt" + nl + "sub" + nl) "E2E binary: target/ls hides dot-entries by default"
        _ = shell_exec "./target/ls -1A /tmp/ut_fs_tree > /tmp/ut_ls_cap_a.out"
        _ = assert_equals (read_file "/tmp/ut_ls_cap_a.out") (".hidden" + nl + "alpha.rg" + nl + "empty_dir" + nl + "link_alpha" + nl + "notes.txt" + nl + "sub" + nl) "E2E binary: target/ls -A includes .hidden but excludes . and .."
        _ = shell_exec "./target/ls -1a /tmp/ut_fs_tree > /tmp/ut_ls_a.out"
        _ = assert_true (str_starts_with (read_file "/tmp/ut_ls_a.out") ("." + nl + ".." + nl + ".hidden" + nl)) "E2E binary: target/ls -a includes ., .., and .hidden"
        _ = shell_exec "./target/ls -1R /tmp/ut_fs_tree > /tmp/ut_ls_r.out"
        _ = assert_true (glob match "*/tmp/ut_fs_tree/sub:*beta.rg*" against (read_file "/tmp/ut_ls_r.out")) "E2E binary: target/ls -R recursively traverses subdirectories"
        _ = shell_exec "rm -f /tmp/ut_fs_tree/.hidden"
        _ = shell_exec "./target/ls -1F /tmp/ut_fs_tree > /tmp/ut_ls_f.out"
        _ = assert_equals (read_file "/tmp/ut_ls_f.out") ("alpha.rg*" + nl + "empty_dir/" + nl + "link_alpha@" + nl + "notes.txt" + nl + "sub/" + nl) "E2E binary: target/ls -1F classifies *, /, @"
        _ = shell_exec "./target/ls -lh /tmp/ut_fs_tree/alpha.rg > /tmp/ut_ls_lh.out"
        lh_out = read_file "/tmp/ut_ls_lh.out"
        _ = assert_true (str_starts_with lh_out "-rwxr-xr-x") "E2E binary: target/ls -lh prints mode string -rwxr-xr-x"
        _ = shell_exec "./target/find /tmp/ut_fs_tree -type f -name '*.rg' > /tmp/ut_find1.out"
        _ = assert_equals (read_file "/tmp/ut_find1.out") ("/tmp/ut_fs_tree/alpha.rg" + nl + "/tmp/ut_fs_tree/sub/beta.rg" + nl) "E2E binary: target/find -type f -name '*.rg'"
        _ = shell_exec "./target/find /tmp/ut_fs_tree -maxdepth 1 -type f -name '*.rg' > /tmp/ut_find_d1.out"
        _ = assert_equals (read_file "/tmp/ut_find_d1.out") ("/tmp/ut_fs_tree/alpha.rg" + nl) "E2E binary: target/find -maxdepth 1"
        _ = shell_exec "./target/find /tmp/ut_fs_tree -mindepth 2 -type f > /tmp/ut_find_min2.out"
        _ = assert_equals (read_file "/tmp/ut_find_min2.out") ("/tmp/ut_fs_tree/sub/beta.rg" + nl) "E2E binary: target/find -mindepth 2"
        _ = shell_exec "./target/find /tmp/ut_fs_tree -path '*/sub/*' > /tmp/ut_find_path.out"
        _ = assert_equals (read_file "/tmp/ut_find_path.out") ("/tmp/ut_fs_tree/sub/beta.rg" + nl) "E2E binary: target/find -path '*/sub/*'"
        _ = shell_exec "./target/find /tmp/ut_fs_tree -empty > /tmp/ut_find_emp.out"
        _ = assert_equals (read_file "/tmp/ut_find_emp.out") ("/tmp/ut_fs_tree/empty_dir" + nl + "/tmp/ut_fs_tree/notes.txt" + nl) "E2E binary: target/find -empty finds 0-byte file and empty dir"
        _ = shell_exec "rm -f /tmp/ut_fs_tree/link_alpha"
        rc_mtree_c = shell_exec "./target/mtree -c -p /tmp/ut_fs_tree > /tmp/ut_mtree.spec"
        _ = assert_int_equals rc_mtree_c 0 "E2E binary: target/mtree -c exits 0"
        rc_mtree_v1 = shell_exec "./target/mtree -f /tmp/ut_mtree.spec -p /tmp/ut_fs_tree > /tmp/ut_mtree_v1.out"
        _ = assert_int_equals rc_mtree_v1 0 "AC1 E2E binary: target/mtree -f verifies clean tree with exit code 0"
        _ = assert_equals (read_file "/tmp/ut_mtree_v1.out") "" "AC1 E2E binary: target/mtree -f emits zero discrepancies on clean tree"
        _ = write_file "/tmp/ut_fs_tree/sub/beta.rg" ("-- modified content!" + nl)
        _ = sys_chmod "/tmp/ut_fs_tree/alpha.rg" 420
        _ = write_file "/tmp/ut_fs_tree/rogue.txt" "extra"
        _ = shell_exec "rm -f /tmp/ut_fs_tree/notes.txt"
        rc_mtree_v2 = shell_exec "./target/mtree -f /tmp/ut_mtree.spec -p /tmp/ut_fs_tree > /tmp/ut_mtree_v2.out"
        _ = assert_true (rc_mtree_v2 != 0) "AC2 E2E binary: target/mtree -f exits non-zero on modified tree"
        v2_lines = split lines (read_file "/tmp/ut_mtree_v2.out")
        _ = assert_equals (str_list_nth v2_lines 0) "alpha.rg: permissions (0755, 0644)" "AC2 E2E binary: mtree reports permissions mismatch"
        _ = assert_equals (str_list_nth v2_lines 1) "notes.txt: missing" "AC2 E2E binary: mtree reports missing file"
        _ = assert_equals (str_list_nth v2_lines 2) "sub/beta.rg: size (8, 21)" "AC2 E2E binary: mtree reports size mismatch"
        _ = assert_true (str_starts_with (str_list_nth v2_lines 3) "sub/beta.rg: sha256 (") "AC2 E2E binary: mtree reports sha256 mismatch"
        _ = assert_equals (str_list_nth v2_lines 4) "rogue.txt: extra" "AC2 E2E binary: mtree reports extra file"
        _ = write_file "/tmp/ut_fs_tree/notes.txt" ""
        _ = sys_chmod "/tmp/ut_fs_tree/notes.txt" 420
        _ = shell_exec "rm -rf /tmp/ut_fs_tree/empty_dir /tmp/ut_fs_tree/rogue.txt && cp /tmp/ut_mtree.spec /tmp/ut_mtree2.spec"
        _ = write_file "/tmp/ut_fs_tree/sub/beta.rg" ("-- beta" + nl)
        _ = shell_exec "./target/mtree -f /tmp/ut_mtree.spec -p /tmp/ut_fs_tree -u > /tmp/ut_mtree_u.out || true"
        rc_mtree_v3 = shell_exec "./target/mtree -f /tmp/ut_mtree.spec -p /tmp/ut_fs_tree > /tmp/ut_mtree_v3.out"
        _ = assert_int_equals rc_mtree_v3 0 "E2E binary: target/mtree -u heals missing directory and restores 0755 permissions"
        _ = shell_exec "rm -rf /tmp/ut_fs_tree /tmp/ut_mtree.spec /tmp/ut_mtree2.spec /tmp/ut_ls_def.out /tmp/ut_ls_cap_a.out /tmp/ut_ls_a.out /tmp/ut_ls_r.out /tmp/ut_ls_f.out /tmp/ut_ls_lh.out /tmp/ut_find1.out /tmp/ut_find_d1.out /tmp/ut_find_min2.out /tmp/ut_find_path.out /tmp/ut_find_emp.out /tmp/ut_mtree_v1.out /tmp/ut_mtree_v2.out /tmp/ut_mtree_u.out /tmp/ut_mtree_v3.out"
        ()

fn :: main => ()
      main =
        _ = test_ls_unit_suite
        _ = test_find_unit_suite
        _ = test_fs_e2e_suite
        echo "All 50 Tier 2 BSD Filesystem (ls, find, mtree) unit & E2E assertions passed!"
