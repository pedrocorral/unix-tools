-- test.rg — Unit and functional assertion helpers for unix-tools test suites

fn :: assert true Bool  String => ()
      assert true True  label  =
        echo "  [PASS] {label}"
      assert true False label  =
        _ = echo "  [FAIL] {label}: expected True, got False"
        sys exit 1

fn :: assert equals String String   String => ()
      assert equals actual expected label  =
        | actual == expected = echo "  [PASS] {label}"
        | otherwise = fail eq actual expected label

fn :: assert int equals Int Int String => ()
      assert int equals actual expected label =
        | actual == expected = echo "  [PASS] {label}"
        | otherwise = fail eq (int to str actual) (int to str expected) label

fn :: fail eq String String   String => ()
      fail eq actual expected label  =
        _ = echo "  [FAIL] {label}: expected '{expected}', got '{actual}'"
        sys exit 1
