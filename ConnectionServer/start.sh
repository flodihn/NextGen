erl +K true +P 1000000 -pa ebin -env ERL_MAX_PORTS 65535 -sname connsrv -s connsrv_app start
