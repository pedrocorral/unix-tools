-- test_md.rg — Exhaustive Unit & E2E Binary Tests for mdrr(1) Terminal Markdown Reader
-- @tests TS-FILTER-003

import test
import md_lib

fn :: test inline and wrap suite => ()
      test inline and wrap suite =
        esc = char from code 27
        _ = echo "=== 1. Testing mdrr(1) Inline Formatting & Visible-Width Wrapping (TS-FILTER-003) ==="
        in_ansi = format inline "Hello **world**, *italic*, `fn :: main`, and [Ridgun](https://ridgun.org)!" plain False
        _ = assert true (str contains ci in_ansi (esc + "[1mworld")) "AC1: **world** emits ANSI bold SGR"
        _ = assert true (str contains ci in_ansi (esc + "[3mitalic")) "AC1: *italic* emits ANSI italic SGR"
        _ = assert true (str contains ci in_ansi (esc + "[36;1mfn :: main")) "AC1: `code` emits ANSI cyan bold SGR"
        _ = assert true (str contains ci in_ansi "(https://ridgun.org)") "AC1: [label](url) includes link target URL"
        in_plain = format inline "Hello **world**, *italic*, `code`, and [Ridgun](https://ridgun.org)!" plain True
        _ = assert equals in_plain "Hello world, italic, `code`, and Ridgun (https://ridgun.org)!" "AC1: -p plain mode strips ANSI codes and preserves readable text"
        _ = assert int equals (visible len ((esc + "[1;36mSolstice") + (esc + "[0m"))) 8 "AC1: visible len ignores zero-width ANSI SGR escape sequences"
        wrapped = wrap formatted text "alpha beta gamma delta epsilon" 12 "" ""
        _ = assert equals (join with wrapped "|") "alpha beta|gamma delta|epsilon" "AC1: word wrapper wraps at max width 12"
        img_plain = format inline "Logo: ![Solstice Logo](/img/logo.png)" plain True
        assert equals img_plain "Logo: [IMAGE: Solstice Logo (/img/logo.png)]" "AC1: ![alt](src) formats image badge"

fn :: test blocks alerts lists suite => ()
      test blocks alerts lists suite =
        nl = char from code 10
        _ = echo "=== 2. Testing mdrr(1) Headings, Front-Matter, Lists, Task Lists & GitHub Alerts (TS-FILTER-003) ==="
        doc = "---" + nl + "id: FR-001" + nl + "status: implemented" + nl + "---" + nl + "# Main Title" + nl + nl + "> [!NOTE]" + nl + "> Keep requirements as ground truth." + nl + nl + "- First bullet" + nl + "- [ ] Pending task" + nl + "- [x] Completed task" + nl + "1. Step one" + nl
        spec_pa = parse mdrr args ["-p", "-a"]
        out_pa = render markdown doc with spec_pa
        _ = assert true (str contains ci out_pa "front-matter") "AC1: YAML front-matter header box is rendered"
        _ = assert true (str contains ci out_pa ("Main Title" + nl + "==========")) "AC1: H1 renders title and '=' underline in -a ASCII mode"
        _ = assert true (str contains ci out_pa "[i] NOTE") "AC1: > [!NOTE] renders GitHub alert header"
        _ = assert true (str contains ci out_pa "* First bullet") "AC1: unordered list item renders bullet"
        _ = assert true (str contains ci out_pa "[ ] Pending task") "AC1: - [ ] renders unchecked task box"
        _ = assert true (str contains ci out_pa "[x] Completed task") "AC1: - [x] renders completed task box"
        _ = assert true (str contains ci out_pa "1. Step one") "AC1: ordered list item preserves number marker"
        spec_no_fm = parse mdrr args ["-p", "-H"]
        out_no_fm = render markdown doc with spec_no_fm
        _ = assert true (not (str contains ci out_no_fm "front-matter")) "mdrr -H suppresses YAML front-matter block"
        assert true (str contains ci out_no_fm "☐ Pending task") "AC1: Unicode mode renders ☐ and ☑ task checkboxes"

fn :: test tables and code suite => ()
      test tables and code suite =
        nl = char from code 10
        _ = echo "=== 3. Testing mdrr(1) GFM Tables & Fenced Code Blocks (TS-FILTER-003) ==="
        tbl_md = "| Tool | Align | Size |" + nl + "| :--- | :---: | ---: |" + nl + "| jot | mid | 415K |" + nl + "| mdrr | ok | 60K |" + nl
        spec_tbl = parse mdrr args ["-p", "-a"]
        out_tbl = render markdown tbl_md with spec_tbl
        _ = assert true (str contains ci out_tbl "+------+-------+------+") "AC2: GFM table computes exact column border widths"
        _ = assert true (str contains ci out_tbl "| jot  |  mid  | 415K |") "AC2: GFM table aligns left, center, and right cells"
        _ = assert true (str contains ci out_tbl "| mdrr |  ok   |  60K |") "AC2: GFM table right-aligns 60K in 4-char column"
        code_md = "```ridgun" + nl + "-- sample comment" + nl + "fn :: main => ()" + nl + "```" + nl
        spec_code = parse mdrr args ["-p", "-a", "-n"]
        out_code = render markdown code_md with spec_code
        _ = assert true (str contains ci out_code "+- ridgun ") "AC2: fenced code block header displays language tag"
        _ = assert true (str contains ci out_code "|   1 │ -- sample comment") "AC2: fenced code block with -n renders line 1"
        assert true (str contains ci out_code "|   2 │ fn :: main => ()") "AC2: fenced code block with -n renders line 2"

fn :: test toc and section suite => ()
      test toc and section suite =
        nl = char from code 10
        _ = echo "=== 4. Testing mdrr(1) Table of Contents (-t) & Section Filter (-s) (TS-FILTER-003) ==="
        doc = "# Guide" + nl + "Intro text." + nl + "## Installation" + nl + "Run stable built." + nl + "### Linux" + nl + "Use target/mdrr." + nl + "## Usage" + nl + "Pass -t or -s." + nl
        spec_toc = parse mdrr args ["-p", "-a", "-t"]
        out_toc = render markdown doc with spec_toc
        _ = assert equals out_toc ("* Guide" + nl + "  * Installation" + nl + "    * Linux" + nl + "  * Usage" + nl) "AC3: mdrr -t extracts indented Table of Contents"
        spec_sec = parse mdrr args ["-p", "-a", "-s", "Installation"]
        out_sec = render markdown doc with spec_sec
        _ = assert true (str contains ci out_sec "Run stable built.") "AC3: mdrr -s Installation includes Installation body"
        _ = assert true (str contains ci out_sec "### Linux") "AC3: mdrr -s Installation includes nested ### Linux subsection"
        assert true (not (str contains ci out_sec "Pass -t or -s.")) "AC3: mdrr -s Installation stops before peer ## Usage section"

fn :: test mdrr e2e suite => ()
      test mdrr e2e suite =
        nl = char from code 10
        esc = char from code 27
        _ = echo "=== 5. Testing Compiled Native ELF Binary target/mdrr (E2E) ==="
        sample = "# Solstice Docs" + nl + nl + "> [!TIP]" + nl + "> Built in **Pure Ridgun**." + nl + nl + "## Features" + nl + "- [x] Fast compilation" + nl + "- [ ] Zero bugs" + nl + nl + "## Benchmarks" + nl + "| Metric | Value |" + nl + "| :--- | ---: |" + nl + "| Check | 0.45s |" + nl
        _ = write file "/tmp/ut_mdrr_sample.md" sample
        rc1 = shell exec "./target/mdrr /tmp/ut_mdrr_sample.md > /tmp/ut_mdrr_ansi.out"
        _ = assert int equals rc1 0 "E2E binary: target/mdrr exits 0 on valid Markdown file"
        ansi_out = read file "/tmp/ut_mdrr_ansi.out"
        _ = assert true (str contains ci ansi_out (esc + "[1;36mSolstice Docs")) "E2E binary: target/mdrr emits ANSI color codes by default"
        _ = assert true (str contains ci ansi_out "💡 TIP") "E2E binary: target/mdrr renders Unicode 💡 TIP alert"
        _ = assert true (str contains ci ansi_out "☑") "E2E binary: target/mdrr renders Unicode ☑ checkbox"
        _ = shell exec "./target/mdrr -p -a -t /tmp/ut_mdrr_sample.md > /tmp/ut_mdrr_toc.out"
        _ = assert equals (read file "/tmp/ut_mdrr_toc.out") ("* Solstice Docs" + nl + "  * Features" + nl + "  * Benchmarks" + nl) "AC3 E2E binary: target/mdrr -p -a -t outputs exact TOC"
        _ = shell exec "./target/mdrr -p -a -s Benchmarks /tmp/ut_mdrr_sample.md > /tmp/ut_mdrr_sec.out"
        sec_out = read file "/tmp/ut_mdrr_sec.out"
        _ = assert true (str contains ci sec_out "| Check  | 0.45s |") "AC2/AC3 E2E binary: target/mdrr -s Benchmarks renders aligned table"
        _ = assert false (str contains ci sec_out "Fast compilation") "AC3 E2E binary: target/mdrr -s Benchmarks excludes Features section"
        _ = shell exec "cat /tmp/ut_mdrr_sample.md | ./target/mdrr -p -a -t > /tmp/ut_mdrr_stdin.out"
        _ = assert str eq (read file "/tmp/ut_mdrr_stdin.out") ("* Solstice Docs" + nl + "  * Features" + nl + "  * Benchmarks" + nl) "E2E pipeline: cat | target/mdrr -p -a -t reads fd 0 via posix read"
        _ = shell exec "rm -f /tmp/ut_mdrr_sample.md /tmp/ut_mdrr_ansi.out /tmp/ut_mdrr_toc.out /tmp/ut_mdrr_sec.out /tmp/ut_mdrr_stdin.out"
        ()

fn :: main => ()
      main =
        _ = test inline and wrap suite
        _ = test blocks alerts lists suite
        _ = test tables and code suite
        _ = test toc and section suite
        _ = test mdrr e2e suite
        echo "All 31 mdrr(1) Terminal Markdown Reader unit & E2E assertions passed!"
