erl -pa ebin -pa deps/riak-erlang-client/ebin -pa deps/riak-erlang-client/deps/*/ebin +K true +P 1000000 -env ERL_MAX_PORTS 65535 -sname accsrv -s accsrv_app start $1
