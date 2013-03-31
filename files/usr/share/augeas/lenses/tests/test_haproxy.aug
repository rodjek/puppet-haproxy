module Test_haproxy =
    let global = "global\n    daemon\nlog 10.0.0.1 local0\n"

    test Haproxy.global get global = 
        { "global"
            { "daemon" }
            { "log"
                { "address" = "10.0.0.1" }
                { "facility" = "local0" }
            }
        }

    (*
     * LOG
     *)

    let basic_log = "log 127.0.0.1 local0\n"
    let log_with_max = "log 127.0.0.1 local0 max emerg\n"
    let log_with_max_min = "log 127.0.0.1 local0 max emerg min warning\n"

    test Haproxy.log_opt get basic_log =
        { "log"
            { "address" = "127.0.0.1" }
            { "facility" = "local0" }
        }

    test Haproxy.log_opt get log_with_max =
        { "log"
            { "address" = "127.0.0.1" }
            { "facility" = "local0" }
            { "max" = "emerg" }
        }

    test Haproxy.log_opt get log_with_max_min =
        { "log"
            { "address" = "127.0.0.1" }
            { "facility" = "local0" }
            { "max" = "emerg" }
            { "min" = "warning" }
        }

    (*
     * STATS SOCKET
     *)

    let basic_stats_socket = "stats socket /tmp/foo.sock\n"
    let stats_socket_uid = "stats socket /tmp/foo.sock uid 1000\n"
    let stats_socket_gid = "stats socket /tmp/foo.sock gid 1234\n"
    let stats_socket_mode = "stats socket /tmp/foo.sock mode 0444\n"
    let stats_socket_level = "stats socket /tmp/foo.sock level operator\n"
    let stats_socket_everything = "stats socket /tmp/foo.sock uid 1000 gid 1234 mode 0444 level operator\n"

    test Haproxy.stats_socket get basic_stats_socket =
        { "stats_socket"
            { "path" = "/tmp/foo.sock" }
        }

    test Haproxy.stats_socket get stats_socket_uid =
        { "stats_socket"
            { "path" = "/tmp/foo.sock" }
            { "uid" = "1000" }
        }

    test Haproxy.stats_socket get stats_socket_gid =
        { "stats_socket"
            { "path" = "/tmp/foo.sock" }
            { "gid" = "1234" }
        }

    test Haproxy.stats_socket get stats_socket_mode =
        { "stats_socket"
            { "path" = "/tmp/foo.sock" }
            { "mode" = "0444" }
        }

    test Haproxy.stats_socket get stats_socket_level =
        { "stats_socket"
            { "path" = "/tmp/foo.sock" }
            { "level" = "operator" }
        }

    test Haproxy.stats_socket get stats_socket_everything =
        { "stats_socket"
            { "path" = "/tmp/foo.sock" }
            { "uid" = "1000" }
            { "gid" = "1234" }
            { "mode" = "0444" }
            { "level" = "operator" }
        }

    (*
     * STATS TIMEOUT
     *)

    let stats_timeout = "stats timeout 100000\n"
    let stats_timeout_unit = "stats timeout 10s\n"

    test Haproxy.stats_timeout get stats_timeout =
        { "stats_timeout" = "100000" }

    test Haproxy.stats_timeout get stats_timeout_unit =
        { "stats_timeout" = "10s" }

    (*
     * BIND
     *)

    let bind_addr_list = ":443,10.0.0.1:80,*:22,::ff:80"

    test Haproxy.bind_address_list get bind_addr_list =
        { "1"
            { "port" = "443" }
        }
        { "2"
            { "address" = "10.0.0.1" }
            { "port" = "80" }
        }
        { "3"
            { "address" = "*" }
            { "port" = "22" }
        }
        { "4"
            { "address" = "::ff" }
            { "port" = "80" }
        }

    test Haproxy.abortonclose get "no option abortonclose\n" = { "abortonclose" = "false" }
    test Haproxy.persist_rdp_cookie get "persist rdp-cookie\n" = { "persist-rdp-cookie" }
    test Haproxy.persist_rdp_cookie get "persist rdp-cookie(foo)\n" = { "persist-rdp-cookie" = "foo" }

    test Haproxy.rate_limit_sessions get "rate-limit sessions 100\n" = { "rate-limit-sessions" = "100" }
