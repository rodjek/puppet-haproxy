module Haproxy =
    autoload xfm

    let eol = Util.eol
    let indent = del /[ \t]{0,4}/ "    "
    let ws = del /[ \t]+/ " "
    let store_to_eol = store /[^ \t\n]+/
    let store_to_ws = store /[^ \t]+/
    let store_time = store /[0-9]+(us|ms|s|m|h|d)?/

    let bool_option (r:regexp) = [ indent . key r . eol ]
    let kv_option (r:regexp) = [ indent . key r . ws . store_to_eol . eol ]

    (*************************************************************************
      LOG OPTION
     *************************************************************************)
    let log_facility = "kern" | "user" | "mail" | "daemon" | "auth" | "syslog"
                     | "lpr" | "news" | "uucp" | "cron" | "auth2" | "ftp"
                     | "ntp" | "audit" | "alert" | "cron2" | "local0"
                     | "local1" | "local2" | "local3" | "local4" | "local5"
                     | "local6" | "local7"
    let log_level = "emerg" | "alert" | "crit" | "err" | "warning" | "notice"
                  | "info" | "debug"

    let log_opt = [ indent . key "log" .
        ws . [ label "address" . store_to_ws ] .
        ws . [ label "facility" . store log_facility ] .
        ( 
            ws . [ key "max" . ws . store log_level ] . 
            ( ws . [ key "min" . ws . store log_level ] )?
        )? ] . eol

    (*************************************************************************
      STATS OPTION
     *************************************************************************)
    let stats_level = "user" | "operator" | "admin"
    let stats_uid = [ key /(uid|user)/ . ws . store_to_eol ]
    let stats_gid = [ key /(gid|group)/ . ws . store_to_eol ]
    let stats_mode = [ key "mode" . ws . store_to_eol ]
    let stats_socket = [ indent . Util.del_str "stats socket" .
        label "stats_socket" . [ ws . label "path" . store_to_eol ] .
        ( [ ws . key /(uid|user)/ . ws . store_to_eol ] )? .
        ( [ ws . key /(gid|group)/ . ws . store_to_eol ] )? .
        ( [ ws . key "mode" . ws . store_to_eol ] )? .
        ( [ ws . key "level" . ws . store stats_level ] )?
        ] . eol
    let stats_timeout = [ indent . Util.del_str "stats timeout" .
        label "stats_timeout" . ws . store_time ] . eol
    let stats_maxconn =  [ indent . Util.del_str "stats maxconn" .
        label "stats_maxconn" . ws . store /[0-9]+/ ] . eol
    let stats = ( stats_socket | stats_timeout | stats_maxconn )

    (*************************************************************************
      GLOBAL SECTION
     *************************************************************************)
    let global_bool_opts = "daemon" | "noepoll" | "nokqueue" | "noepoll"
                         | "nosepoll" | "nosplice" | "debug" | "quiet"
    let global_kv_opts = "chroot" | "gid" | "group" | "log-send-hostname"
                       | "nbproc" | "pidfile" | "uid" | "ulimit-n" | "user"
                       | "node" | "description" | "maxconn" | "maxpipes"
                       | "spread-checks" | "tune.bufsize" | "tune.chksize"
                       | "tune.maxaccept" | "tune.maxpollevents" 
                       | "tune.maxrewrite" | "tune.rcvbuf.client"
                       | "tune.rcvbuf.server" | "tune.sndbuf.client"
                       | "tune.sndbuf.server"

    let global = [ key "global" . eol .
        (bool_option global_bool_opts|kv_option global_kv_opts|stats|log_opt)*
        ]

    (*************************************************************************
      USER LISTS

      TODO!
     *************************************************************************)

    (*************************************************************************
      PROXY OPTIONS
     *************************************************************************)

    let lns = global

    let xfm = transform lns (incl "/etc/haproxy/haproxy.cfg")
