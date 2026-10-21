-- build.rg — Project build definition for unix-tools
-- @implements ADR-0001

fn :: project name => String
      project name =  "unix-tools"

fn :: project version => String
      project version =  "0.1.0"

fn :: main entry => String
      main entry =  "src/main/ridgun/jot/main.rg"

fn :: source dir => String
      source dir =  "src/main/ridgun"

fn :: test dir => String
      test dir =  "src/test/ridgun"

fn :: target dir => String
      target dir =  "target"

fn :: dependencies => [String]
      dependencies =  ["/home/boogie/ridgun-libs/src/main/ridgun/lib/posix.rg", "/home/boogie/ridgun-libs/src/main/ridgun/lib/io.rg"]

fn :: task sync libs => String
      task sync libs =  "mkdir -p .stable/deps target && ridgunc build --lib /home/boogie/ridgun-libs/src/main/ridgun/lib/posix.rg -o .stable/deps/posix.rglib && cp -f .stable/deps/posix.rglib target/posix.rglib && ridgunc build --lib /home/boogie/ridgun-libs/src/main/ridgun/lib/io.rg -o .stable/deps/io.rglib && cp -f .stable/deps/io.rglib target/io.rglib"
