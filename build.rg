-- build.rg — Project build definition for unix-tools
-- @implements ADR-0001

fn :: project_name => String
project_name = "unix-tools"

fn :: project_version => String
project_version = "0.1.0"

fn :: main_entry => String
main_entry = "src/main/ridgun/jot/main.rg"

fn :: source_dir => String
source_dir = "src/main/ridgun"

fn :: test_dir => String
test_dir = "src/test/ridgun"

fn :: target_dir => String
target_dir = "target"
