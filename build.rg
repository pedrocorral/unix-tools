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
      dependencies =  ["/home/boogie/ridgun/libs/src/main/ridgun/lib/posix.rg", "/home/boogie/ridgun/libs/src/main/ridgun/lib/io.rg", "/home/boogie/ridgun/libs/src/main/ridgun/lib/crypto.rg", "/home/boogie/ridgun/libs/src/main/ridgun/lib/socket.rg"]

fn :: task sync libs => String
      task sync libs =  "mkdir -p .stable/deps target && for l in posix io crypto argon2id socket; do cp -f /home/boogie/ridgun/libs/target/bundles/$l.rglib .stable/deps/$l.rglib && cp -f .stable/deps/$l.rglib target/$l.rglib; done"

fn :: task strict binaries => String
      task strict binaries =  "mkdir -p target && for u in jot rs lam vis unvis mdrr ls find mtree mktemp apply make sh timeout nc doas sha256; do ridgunc build --strict src/main/ridgun/$u/main.rg -o target/$u || exit 1; done"
