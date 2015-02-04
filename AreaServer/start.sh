erl +K true +P 10000000 -env ERL_MAX_PORTS 65535 -pa ebin -sname $1 -eval "application:start(areasrv)" $2
