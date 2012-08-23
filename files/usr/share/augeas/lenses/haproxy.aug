module Haproxy =
    autoload xfm

    let eol = Util.eol
    let hard_eol = del "\n" "\n"
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
    let acl = indent . [ key "acl" . ws
        . [ label "name" . store_to_ws ] . ws
        . [ label "value" . store /[^ \t][^\n]+/ ]
        ] . hard_eol

    let appsession = indent . [ key "appsession" . ws
        . [ label "cookie" . store_to_ws ] . ws
        . [ key "len" . store_to_ws ] . ws
        . [ key "timeout" . store_time ]
        . ( ws . [ key "request-learn" ] )?
        . ( ws . [ key "prefix" ] )?
        . ( ws . [ key "mode" . store /(path-parameters|query-string)/ ] )?
        ] . eol

    let backlog = kv_option "backlog"

    let balance = indent . [ key "balance" . ws
        . [ label "algorithm" . store_to_ws ]
        . ( ws . [ label "params" . store /[^ \t][^\n]+/ ] )?
        ] . hard_eol

    let bind_address = [ seq "bind_addr" 
        . ( [ label "address" . store /[^ \t,]+/ ] )?
        . Util.del_str ":" . [ label "port" . store /[0-9-]+/ ] ]
    let bind_address_list = bind_address . ( Util.del_str "," . bind_address)*
    let bind = indent . [ key "bind" . ws
        . [ label "bind_addr" . bind_address_list ]
        . ( ws . [ key "interface" . store_to_ws ] )?
        . ( ws . [ key "mss" . store_to_ws ] )?
        . ( ws . [ key "transparent" ] )?
        . ( ws . [ key "id" . store_to_ws ] )?
        . ( ws . [ key "name" . store_to_ws ] )?
        . ( ws . [ key "defer-accept" ] )?
        ] . eol

    let bind_process_id = [ key /[0-9]+/ ]
    let bind_process_id_list = [ label "number"
        . bind_process_id . ( ws . bind_process_id )*
        ]
    let bind_process = indent . [ key "bind-process" . ws
        . (store /(all|odd|even)/|bind_process_id_list)
        ] . eol

    let block = indent . [ key "block" . ws
        . [ label "condition" . store /[^ \t][^\n]+/ ]
        ] . hard_eol

    let capture_cookie = indent . Util.del_str "capture cookie" . ws
        . [ label "capture_cookie"
            . [ label "name" . store_to_ws ] . ws
            . [ label "len" . store /[0-9]+/ ]
        ] . eol

    let capture_request_header = indent 
        . Util.del_str "capture request header" . ws
        . [ label "capture_request_header"
            . [ label "name" . store_to_ws ] . ws
            . [ label "len" . store /[0-9]+/ ]
        ] . eol

    let capture_response_header = indent
        . Util.del_str "capture response header" . ws
        . [ label "capture_response_header"
            . [ label "name" . store_to_ws ] . ws
            . [ label "len" . store /[0-9]+/ ]
        ] . eol

    let clitimeout = kv_option "clitimeout"

    let contimeout = kv_option "contimeout"

    let cookie = indent . [ key "cookie" . ws
        . [ label "name" . store_to_ws ]
        . ( ws . [ label "method" . store /(rewrite|insert|prefix)/ ] )?
        . ( ws . [ key "indirect" ] )?
        . ( ws . [ key "nocache" ] )?
        . ( ws . [ key "postonly" ] )?
        . ( ws . [ key "preserve" ] )?
        . ( ws . [ key "httponly" ] )?
        . ( ws . [ key "secure" ] )?
        . ( ws . [ key "domain" . store_to_ws ] )?
        . ( ws . [ key "maxidle" . store_time ] )?
        . ( ws . [ key "maxlife" . store_time ] )?
        ] . eol
    
    (* #XXX default-server *)

    let default_backend = kv_option "default_backend"

    let disabled = bool_option "disabled"

    let dispatch = indent . [ key "dispatch" . ws
        . [ label "address" . store /[^ \t,]+/ ]
        . Util.del_str ":" . [ label "port" . store /[0-9-]+/ ] ]

    let enabled = bool_option "enabled"

    let errorfile = indent . [ key "errorfile" . ws
        . [ label "code" . store /[0-9]+/ ] . ws
        . [ label "file" . store_to_eol ]
        ] . eol

    let error_redir (keyword:string) = indent . [ key keyword . ws
        . [ label "code" . store /[0-9]+/ ] . ws
        . [ label "url" . store_to_eol ]
        ] . eol

    let errorloc = error_redir "errorloc"
    let errorloc302 = error_redir "errorloc302"
    let errorloc303 = error_redir "errorloc303"

    let force_persist = indent . [ key "force-persist" . ws
        . [ label "condition" . store /[^ \t][^\n]+/ ]
        ] . hard_eol

    let fullconn = kv_option "fullconn"

    let grace = kv_option "grace"

    let hash_type = kv_option "hash-type"

    let http_check_disable_on_404 = indent
        . Util.del_str "http-check disable-on-404"
        . [ label "http_check_disable_on_404" ] . eol

    let http_check_expect = indent . Util.del_str "http-check expect"
        . [ label "http_check_expect"
            . ( ws . [ Util.del_str "!" . label "not" ] )?
            . ws . [ label "match" . store /(status|rstatus|string|rstring)/ ]
            . ws . [ label "pattern" . store /[^ \t][^\n]+/ ]
        ] . hard_eol

    let http_check_send_state = indent . Util.del_str "http-check send-state"
        . [ label "http_check_keep_state" ] . eol

    (* #XXX http-request *)

    let http_send_name_header = kv_option "http-send-name-header"

    let id = kv_option "id"

    let ignore_persist = indent . [ key "ignore-persist" . ws
        . [ label "condition" . store /[^ \t][^\n]+/ ]
        ] . hard_eol

    let log = (indent . [ key "log" . store "global" ] . eol ) | log_opt

    let maxconn = kv_option "maxconn"

    let mode = kv_option "mode"

    let monitor_fail = indent . Util.del_str "monitor fail"
        . [ key "monitor_fail" . ws
            . [ label "condition" . store /[^ \t][^\n]+/ ]
        ] . hard_eol

    let monitor_net = kv_option "monitor-net"

    let monitor_uri = kv_option "monitor-uri"

    let lns = global

    let xfm = transform lns (incl "/etc/haproxy/haproxy.cfg")
