erl -pa deps/riak-erlang-client/ebin -pa deps/riak-erlang-client/deps/*/ebin +K true +P 1000000 -env ERL_MAX_PORTS 65535 -sname charsrv -pa ebin -s charsrv_app start $1
