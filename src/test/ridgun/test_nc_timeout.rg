-- test_nc_timeout.rg — Exhaustive, Uncompromising BSD timeout(1) & nc(1) Test Battery
-- @tests TS-PROC-003

import test
import proc_lib

fn :: test timeout exhaustive => ()
      test timeout exhaustive =
        nl = char from code 10
        _ = echo "=== 1. timeout(1) Fractional Durations, Signals, Exit Codes (124/125/126/127/128+sig) & Anti-Cheat (TS-PROC-003) ==="
        _ = assert int equals (parse duration ms "0.1s") 100 "timeout fractional duration: 0.1s -> 100ms"
        _ = assert int equals (parse duration ms "1.5s") 1500 "timeout fractional duration: 1.5s -> 1500ms"
        _ = assert int equals (parse duration ms "0.5m") 30000 "timeout fractional duration: 0.5m -> 30000ms"
        _ = assert int equals (parse duration ms "0.25") 250 "timeout fractional duration without suffix: 0.25 -> 250ms"
        to_bad_dur = execute timeout (parse timeout args ["not_a_number", "echo", "hi"])
        _ = assert int equals (snd to_bad_dur) 125 "timeout with invalid duration string returns 125"
        to_bad_sig = execute timeout (parse timeout args ["-s", "NOTASIGNAL", "1s", "echo", "hi"])
        _ = assert int equals (snd to_bad_sig) 125 "timeout with invalid signal name returns 125"
        to_bad_flag = execute timeout (parse timeout args ["--unknown-flag", "1s", "echo", "hi"])
        _ = assert int equals (snd to_bad_flag) 125 "timeout with unknown flag returns 125"
        to_not_found = execute timeout (parse timeout args ["1s", "/nonexistent_ut_cmd_99999"])
        _ = assert int equals (snd to_not_found) 127 "timeout on non-existent command returns 127"
        _ = write file "/tmp/ut_nonexec.sh" "#!/bin/sh\necho hi\n"
        _ = sys chmod "/tmp/ut_nonexec.sh" 420
        to_nonexec = execute timeout (parse timeout args ["1s", "/tmp/ut_nonexec.sh"])
        _ = sys unlink "/tmp/ut_nonexec.sh"
        _ = assert int equals (snd to_nonexec) 126 "timeout on non-executable file (mode 0644) returns 126"
        to_pres_term = execute timeout (parse timeout args ["--preserve-status", "-s", "TERM", "50ms", "sleep", "2"])
        _ = assert int equals (snd to_pres_term) 143 "timeout --preserve-status -s TERM returns 143 (128 + 15)"
        to_pres_kill = execute timeout (parse timeout args ["--preserve-status", "-s", "9", "50ms", "sleep", "2"])
        _ = assert int equals (snd to_pres_kill) 137 "timeout --preserve-status -s 9 (SIGKILL) returns 137 (128 + 9)"
        to_fg = execute timeout (parse timeout args ["--foreground", "--signal=SIGTERM", "--kill-after=100ms", "50ms", "sleep", "2"])
        _ = assert int equals (snd to_fg) 124 "timeout --foreground --signal=SIGTERM --kill-after=100ms times out with 124"
        _ = shell exec "rm -rf /tmp/ut_no_host_timeout && mkdir -p /tmp/ut_no_host_timeout && ln -s /bin/echo /tmp/ut_no_host_timeout/echo && ln -s /bin/sleep /tmp/ut_no_host_timeout/sleep"
        rc_anticheat_ok = shell exec "PATH=/tmp/ut_no_host_timeout ./target/timeout 1s echo pure_ridgun_ok > /tmp/ut_to_ac.out"
        _ = assert int equals rc_anticheat_ok 0 "timeout anti-cheat: target/timeout succeeds when /usr/bin/timeout is absent from PATH"
        _ = assert str eq (read file "/tmp/ut_to_ac.out") ("pure_ridgun_ok" + nl) "timeout anti-cheat: target/timeout captures stdout without /usr/bin/timeout"
        rc_anticheat_exp = shell exec "PATH=/tmp/ut_no_host_timeout ./target/timeout 50ms sleep 2; test $? -eq 124"
        _ = assert int equals rc_anticheat_exp 0 "timeout anti-cheat: target/timeout enforces 50ms deadline and exits 124 without /usr/bin/timeout"
        _ = shell exec "rm -rf /tmp/ut_no_host_timeout /tmp/ut_to_ac.out"
        ()

fn :: test nc exhaustive => ()
      test nc exhaustive =
        nl = char from code 10
        _ = echo "=== 2. nc(1) Real TCP/UDP/Unix Transfer, Port Ranges, -n Flag & Stdin Piping (TS-PROC-003) ==="
        l1 = sys tcp listen reuseport "127.0.0.1" 44210 4
        l2 = sys tcp listen reuseport "127.0.0.1" 44211 4
        _ = assert int equals (sock rc l1) 0 "setup listener on 44210"
        _ = assert int equals (sock rc l2) 0 "setup listener on 44211"
        nc_range = execute nc (parse nc args ["-zv", "127.0.0.1", "44210-44211"]) with ""
        _ = posix close (sock fd l1)
        _ = posix close (sock fd l2)
        _ = assert int equals (fst nc_range) 0 "nc -zv 127.0.0.1 44210-44211 port range scan succeeds when ports in range are open"
        _ = assert true (str contains (snd nc_range) "44210") "nc -zv port range scan reports port 44210"
        _ = assert true (str contains (snd nc_range) "44211") "nc -zv port range scan reports port 44211"
        nc_nodns = execute nc (parse nc args ["-n", "-zv", "localhost", "44210"]) with ""
        _ = assert false (check eq (fst nc_nodns) 0) "nc -n rejects non-numeric hostname 'localhost'"
        nc_udp_unb = execute nc (parse nc args ["-u", "-w", "1", "127.0.0.1", "44299"]) with "udp_datagram_payload"
        _ = assert str eq (snd nc_udp_unb) "" "ADR-0002 anti-fake: nc -u to unbound port 44299 does NOT fake an in-memory echo"
        rc_udp_cs = shell exec "(./target/nc -lu 127.0.0.1 44216 > /tmp/ut_nc_udp_srv.out) & UPID=$!; sleep 0.1; printf 'real_udp_kernel_dgram' | ./target/nc -u 127.0.0.1 44216; wait $UPID"
        _ = assert int equals rc_udp_cs 0 "ADR-0002 E2E binary: real two-process target/nc -lu server <-> target/nc -u client UDP transfer exits 0"
        _ = assert str eq (read file "/tmp/ut_nc_udp_srv.out") "real_udp_kernel_dgram" "ADR-0002 E2E binary: target/nc -lu server received 'real_udp_kernel_dgram' over real kernel AF_INET SOCK_DGRAM socket"
        _ = sys unlink "/tmp/ut_nc_udp_srv.out"
        upath = "/tmp/ut_nc_stream.sock"
        _ = sys unlink upath
        ul = sys unix listen upath 4
        _ = assert int equals (sock rc ul) 0 "setup unix domain socket listener at /tmp/ut_nc_stream.sock"
        nc_uz = execute nc (parse nc args ["-zU", upath]) with ""
        _ = assert int equals (fst nc_uz) 0 "nc -zU /tmp/ut_nc_stream.sock zero-I/O probe succeeds against active unix listener"
        _ = posix close (sock fd ul)
        _ = sys unlink upath
        u_stream_path = "/tmp/ut_nc_u_stream.sock"
        _ = sys unlink u_stream_path
        rc_unix_cs = shell exec "(printf 'unix_pong_srv' | ./target/nc -lU /tmp/ut_nc_u_stream.sock > /tmp/ut_nc_u_srv.out) & USPID=$!; sleep 0.1; printf 'unix_ping_cli' | ./target/nc -U /tmp/ut_nc_u_stream.sock > /tmp/ut_nc_u_cli.out; wait $USPID"
        _ = assert int equals rc_unix_cs 0 "ADR-0002 E2E binary: real two-process target/nc -lU <-> target/nc -U Unix domain stream transfer exits 0"
        _ = assert str eq (read file "/tmp/ut_nc_u_srv.out") "unix_ping_cli" "ADR-0002 E2E binary: target/nc -lU server received 'unix_ping_cli' over kernel AF_UNIX socket"
        _ = assert str eq (read file "/tmp/ut_nc_u_cli.out") "unix_pong_srv" "ADR-0002 E2E binary: target/nc -U client received 'unix_pong_srv' over kernel AF_UNIX socket"
        _ = shell exec "rm -f /tmp/ut_nc_u_srv.out /tmp/ut_nc_u_cli.out /tmp/ut_nc_u_stream.sock"
        rc_nc_stdin = shell exec "printf 'stdin_net_payload' | ./target/nc 127.0.0.1 44220 > /tmp/ut_nc_stdin.out"
        _ = assert int equals rc_nc_stdin 0 "E2E binary: printf | target/nc reads payload from stdin (fd 0)"
        _ = assert str eq (read file "/tmp/ut_nc_stdin.out") "stdin_net_payload" "E2E binary: target/nc transfers exact stdin payload (not empty string)"
        _ = sys unlink "/tmp/ut_nc_stdin.out"
        rc_nc_cs = shell exec "(printf 'pong_from_server' | ./target/nc -l 127.0.0.1 44225 > /tmp/ut_nc_srv_rx.out) & SRV_PID=$!; sleep 0.1; printf 'ping_from_client' | ./target/nc 127.0.0.1 44225 > /tmp/ut_nc_cli_rx.out; wait $SRV_PID"
        _ = assert int equals rc_nc_cs 0 "E2E binary: real two-process target/nc -l server <-> target/nc client TCP exchange exits 0"
        _ = assert str eq (read file "/tmp/ut_nc_srv_rx.out") "ping_from_client" "E2E binary: target/nc -l server received 'ping_from_client' over real kernel TCP socket"
        _ = assert str eq (read file "/tmp/ut_nc_cli_rx.out") "pong_from_server" "E2E binary: target/nc client received 'pong_from_server' over real kernel TCP socket"
        _ = shell exec "rm -f /tmp/ut_nc_srv_rx.out /tmp/ut_nc_cli_rx.out"
        to_pgrp = execute timeout (parse timeout args ["80ms", "/bin/sh", "-c", "sleep 2 & echo $! > /tmp/ut_to_gchild.pid; wait"])
        _ = assert int equals (snd to_pgrp) 124 "ADR-0002 timeout process group: times out shell + background grandchild with 124"
        gchild_pid = str to int (trim ws (read existing or empty "/tmp/ut_to_gchild.pid"))
        _ = sys unlink "/tmp/ut_to_gchild.pid"
        _ = assert false (process exists gchild_pid) "ADR-0002 timeout setpgid: grandchild process in process group was terminated on timeout"
        ()

fn :: main => ()
      main =
        _ = test timeout exhaustive
        _ = test nc exhaustive
        echo "All 34 exhaustive BSD timeout(1) & nc(1) assertions in test_nc_timeout.rg passed!"
