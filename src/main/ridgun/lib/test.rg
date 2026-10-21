-- test.rg — Unit and functional assertion helpers for unix-tools test suites

fn :: assert_true Bool  String => ()
      assert_true True  label  =
        echo "  [PASS] {label}"
      assert_true False label  =
        _ = echo "  [FAIL] {label}: expected True, got False"
        sys_exit 1

fn :: assert_equals String String   String => ()
      assert_equals actual expected label  =
        | actual == expected = echo "  [PASS] {label}"
        | otherwise = fail_eq actual expected label

fn :: assert_int_equals Int    Int      String => ()
      assert_int_equals actual expected label  =
        | actual == expected = echo "  [PASS] {label}"
        | otherwise = fail_eq (int_to_str actual) (int_to_str expected) label

fn :: fail_eq String String   String => ()
      fail_eq actual expected label  =
        _ = echo "  [FAIL] {label}: expected '{expected}', got '{actual}'"
        sys_exit 1
