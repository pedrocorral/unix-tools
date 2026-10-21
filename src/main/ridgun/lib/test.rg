-- test.rg — Pure-Ridgun Unit & Functional Test Assertion Library for unix-tools

fn :: fail test String => ()
      fail test msg    =
        _ = echo "  [FAIL] {msg}"
        sys exit 1

fn :: check eq A      A        => Bool
      check eq actual expected =  actual == expected

fn :: check str eq String String   => Bool
      check str eq actual expected =  actual == expected

fn :: check true Bool => Bool
      check true cond =  cond

fn :: assert true Bool  String => ()
      assert true True  label  =
        echo "  [PASS] {label}"
      assert true False label  =
        fail test "assert_true failed: {label}"

fn :: assert false Bool  String => ()
      assert false False label  =
        echo "  [PASS] {label}"
      assert false True  label  =
        fail test "assert_false failed: {label}"

fn :: assert equals A      A        String => ()
      assert equals actual expected label  =
        | actual == expected = echo "  [PASS] {label}"
        | otherwise          = fail test "assert_equals failed: {label}"

fn :: assert str eq String String String => ()
      assert str eq actual expected label =
        | actual == expected = echo "  [PASS] {label}"
        | otherwise          = fail eq actual expected label

fn :: assert int equals Int Int String => ()
      assert int equals actual expected label =
        | actual == expected = echo "  [PASS] {label}"
        | otherwise          = fail eq (int to str actual) (int to str expected) label

fn :: assert ok Bool => ()
      assert ok cond =  assert true cond "assert_ok"

fn :: assert not Bool => ()
      assert not cond =  assert false cond "assert_not"

fn :: assert eq A      A        => ()
      assert eq actual expected =  assert equals actual expected "assert_eq"

fn :: fail eq String String   String => ()
      fail eq actual expected label  =
        _ = echo "  [FAIL] {label}: expected '{expected}', got '{actual}'"
        sys exit 1
